{@abstract(The main purpose of @name is to define @link(TScreenObject)
  which extends @link(TScreenObject) by adding boundary conditions and
  the ability to handle PHAST-style interpolation.  @link(TScreenObject)
  represents the objects that the user draws on the screen.  @name also
  defines a variety of related classes.)
  @name also defines @link(TCustomPhastBoundaryCondition) and a series of
  descendants.  The descendants represent boundary conditions in GoPhast.

  Another class defined here is @link(TMultiValueScreenObject) which is used
  when importing zones from an existing PHAST model.

  Finally @link(TScreenObjectItem) and @link(TScreenObjectCollection)
  are used to save @link(TScreenObject)s to a file or read them from
  a file.

  See @link(TPhastInterpolationValues) for an explanation of
  PHAST-style interpolation.

@author(Richard B. Winston <rbwinst@usgs.gov>)}
unit ScreenObjectUnit;

interface

uses
  ZLib, PhastDataSets,
  Windows, // Windows is included to allow AnsiCompareText to be inlined.
  GR32, // defines TBitmap32.
  GR32_Polygons, Types, SysUtils, Classes, SyncObjs, Graphics, Dialogs,
  Controls, OpenGL12x, ZoomBox2, AbstractGridUnit, PhastGridUnit, SelectUnit,
  UndoItems, DataSetUnit, Contnrs, RbwParser, FastGEO, GoPhastTypes,
  SubscriptionUnit, SparseDataSets, ModflowConstantHeadBoundaryUnit,
  ModflowGhbUnit, ModflowWellUnit, OrderedCollectionUnit, ModflowBoundaryUnit,
  ModflowRivUnit, ModflowDrnUnit, ModflowDrtUnit, ModflowCellUnit,
  ModflowGridUnit, RealListUnit, IntervalTree, ModflowRchUnit, ModflowEvtUnit,
  ModflowEtsUnit, ModflowResUnit, ModflowLakUnit, ModflowSfrUnit,
  ModflowUzfUnit, ModflowHobUnit, ValueArrayStorageUnit, QuadTreeClass,
  ModflowHfbUnit, ModpathParticleUnit, GPC_Classes, ModflowGageUnit,
  FormulaManagerUnit, ModflowMnw2Unit;

type
  //
  T2DRealPointArray6 = array[0..5] of TPoint2D;
  // @name is used when a @link(TScreenObject) to specify which one
  // is being handled in the current operation.
  TWhichElevation = (weHigher, weLower);

  // @abstract(@name is the class of errors related to @link(TScreenObject)s.)
  EScreenObjectError = class(Exception);

  // @abstract(@name stores information about where
  // a @link(TScreenObject) is intersecting
  // the grid.)
  //
  // See @link(TEdgePoint) and @link(TCellElementSegment).
  TEdgePosition = (epFirst, epMiddle, epLast);

  {@abstract(@name represents a point of intersection between a
   @link(TScreenObject) and a grid element or grid cell.)

  @longcode(#
  TEdgePoint = record
    Position: TEdgePosition;
    X: real;
    Y: real;
  end;
  #)
  }
  TEdgePoint = record
    {If @name = epFirst, this is a vertex of the @link(TScreenObject)
     or this point is at the edge of the grid and
     this is the first spot where the @link(TScreenObject)
     intersects the grid (or else the contour has left the grid and is just
     reentering it)

     If @name = epLast, this is a vertex of the @link(TScreenObject)
     or this point is at the edge of the grid
     and this is where the @link(TScreenObject) is exiting the grid.

     If @name = epMiddle, this point is in the interior of the grid
     and this point is at the edge of an element or cell.

     See @link(TEdgePosition).
     }
    Position: TEdgePosition;
    // @name is the X-coordinate of the point.
    X: real;
    // @name is the Y-coordinate of the point.
    Y: real;
  end;

  // @name is a pointer to @link(TEdgePoint);
  PEdgePoint = ^TEdgePoint;

  // @name is an array of @link(TEdgePoint)s.
  TEdgePointArray = array of TEdgePoint;

  TLineSegment = class(TObject)
    Point1: TPoint2D;
    Point2: TPoint2D;
    function IsNextSegment(Seg: TLineSegment): boolean;
  end;

  TSelectedCells = class(TObject)
  private
    FLayerCount: integer;
    FRowCount: integer;
    FColCount: integer;
    FSelected: array of array of array of boolean;
    function GetSelected(Layer, Row, Col: integer): boolean;
    procedure SetSelected(Layer, Row, Col: integer; const Value: boolean);
    procedure ValidateIndicies(Col: Integer; Row: Integer; Layer: Integer);
    function GetHasCells: boolean;
  public
    procedure SetExtents(LayerCount, RowCount, ColCount: integer);
    property Selected[Layer, Row, Col: integer]: boolean read GetSelected
      write SetSelected; default;
    property HasCells: boolean read GetHasCells;
    procedure Clear;
  end;

  // @abstract(@name represents the 2D intersection of
  // one segment of a @link(TScreenObject)
  // with a cell or element in the grid.)
  TCellElementSegment = class(TObject)
  private
    // See @link(Col).
    FCol: integer;
    // See @link(EndPosition).
    FEndPosition: TEdgePosition;
    // See @link(Layer).
    FLayer: integer;
    // See @link(VertexIndex).
    FVertexIndex: integer;
    // See @link(Row).
    FRow: integer;
    // See @link(StartPosition).
    FStartPosition: TEdgePosition;
    // See @link(X1).
    FX1: double;
    // See @link(X2).
    FX2: double;
    // See @link(Y1).
    FY1: double;
    // See @link(Y2).
    FY2: double;
    FSectionIndex: integer;
    procedure Store(Stream: TCompressionStream);
    procedure Restore(Stream: TDecompressionStream);
  public
    // @name is the column of the grid which this @classname intersects.
    // In this context, column can represent a column of elements or a column
    // of cells depending on which this @classname is intersecting.
    property Col: integer read FCol write FCol;
    // If the end of this @classname is at the edge of the
    // cell or element, @name is epLast.  If it is a vertex on the
    // @link(TScreenObject) so that the point is in the interior of the
    // cell or element, it is epMiddle.
    property EndPosition: TEdgePosition read FEndPosition write FEndPosition;
    // @name is the layer of the grid which this @classname intersects.
    // In this context, layer can represent a layer of elements or a layer
    // of cells depending on which this @classname is intersecting.
    property Layer: integer read FLayer write FLayer;
    // @name returns the length of this @classname.
    function Length: double;
    // @name is the index of the point in @link(TScreenObject.Points)
    // that comes before this @classname.
    property VertexIndex: integer read FVertexIndex write FVertexIndex;
    // @name is the row of the grid which this @classname intersects.
    // In this context, row can represent a row of elements or a row
    // of cells depending on which this @classname is intersecting.
    property Row: integer read FRow write FRow;
    // If the start of this @classname is at the edge of the
    // cell or element, @name is epFirst.  If it is a vertex on the
    // @link(TScreenObject) so that the point is in the interior of the
    // cell or element, it is epMiddle.
    property StartPosition: TEdgePosition read FStartPosition
      write FStartPosition;
    // @name is the X-coordinate of the start of this @classname.
    // @name is in the coordinate system of the grid rather
    // than global coordinate system.
    property X1: double read FX1 write FX1;
    // @name is the X-coordinate of the end of this @classname.
    // @name is in the coordinate system of the grid rather
    // than global coordinate system.
    property X2: double read FX2 write FX2;
    // @name is the Y-coordinate of the start of this @classname.
    // @name is in the coordinate system of the grid rather
    // than global coordinate system.
    property Y1: double read FY1 write FY1;
    // @name is the Y-coordinate of the end of this @classname.
    // @name is in the coordinate system of the grid rather
    // than global coordinate system.
    property Y2: double read FY2 write FY2;
    function FirstPointRealCoord(ViewDirection: TViewDirection): TPoint2D;
    function SecondPointRealCoord(ViewDirection: TViewDirection): TPoint2D;
    property SectionIndex: integer read FSectionIndex write FSectionIndex;
  end;

  TCellElementLeaf = class(TRangeTreeLeaf)
  private
    FX1, FX2, FY1, FY2: double;
    FSegment: TCellElementSegment;
  public
    Constructor Create(Segment: TCellElementSegment;
      ViewDirection: TViewDirection);
    function GetCoordinate(Depth: integer): double; override;
  end;


  TCellElementLeafList = class(TRangeTreeLeafList)
  public
    function CoordinateCount: integer; override;
    Constructor Create;
  end;

  TIntersectEdge = class(TObject)
    StartPoint: TPoint2D;
    EndPoint: TPoint2D;
    procedure  Assign(Segment: TCellElementSegment); overload;
    procedure  Assign(P1, P2: TPoint2D); overload;
    procedure Reverse;
  end;

  TScreenObject = class;

  // @abstract(@name is a list of the @link(TCellElementSegment)s of a
  // @link(TScreenObject). Each segment is the 2D
  // intersection of one segment of the @link(TScreenObject)
  // with an individual cell or element in the grid.)
  TCellElementSegmentList = class(TObjectList)
  private
    FStartPoints: TRbwQuadTree;
    FEndPoints: TRbwQuadTree;
    FScreenObject: TScreenObject;
    // @name is a @link(TPhastModel) or nil.
    FModel: TComponent;
    // See @link(UpToDate).
    FUpToDate: boolean;
//    FTempFileName: string;
    FTempMemoryStream: TMemoryStream;
    FCached: Boolean;
    FCleared: Boolean;
    // @name is set and used in @link(TCellElementSegmentList.ClosestSegment)
    // to help find the closest segment to the point of interest.
    FMinDistance: double;
//    FAnisotropy: double;
    FRangeTree: TRbwRangeTree;
    // See @link(Items).
    // @param(Index indicates the position of the @link(TCellElementSegment).)
    function GetSegment(Index: Integer): TCellElementSegment;
    // See @link(Items).
    // @param(Index indicates the position of the @link(TCellElementSegment).)
    // @param(Value is the @link(TCellElementSegment) to be stored.)
    procedure SetSegment(Index: Integer; const Value: TCellElementSegment);
    // See @link(UpToDate).
    procedure SetUpToDate(const Value: boolean);
    procedure RestoreData;
//    procedure GetExpandedXInterval(Subject: TObject;
//      out LowerBoundary, UpperBoundary: double);
//    procedure GetExpandedYInterval(Subject: TObject;
//      out LowerBoundary, UpperBoundary: double);
//    procedure CheckSegment(Subject: TObject; Point: TOneDRealArray;
//      var Accept: boolean);
  public
    procedure CacheData;
    // @name indicates that the @link(TCellElementSegment)s in this
    // class name are up-to-date and do not need to be recalculated.
    property UpToDate: boolean read FUpToDate write SetUpToDate;
    // @name adds a @link(TCellElementSegment) to the current @classname
    // and returns the position of it in the @classname.
    // @param(ASegment is the @link(TCellElementSegment) to be added.)
    function Add(ASegment: TCellElementSegment): Integer;
    // @name is used to access the @link(TCellElementSegment)s in the list.
    property Items[Index: Integer]: TCellElementSegment read GetSegment
      write SetSegment; default;
    // @name creates an instance of @classname and assigns Model
    // to @link(FModel).
    constructor Create(Model: TComponent; ScreenObject: TScreenObject);
    destructor Destroy; override;
    // @name returns the TCellElementSegment that is closes to
    // Location.
    //
    // Location is in real-world coordinates;
    function ClosestSegment(Location: TPoint2D; Anisotropy: double):
      TCellElementSegment;
  end;

  TIntersectionLocation = record
    // @name is the position in @link(TSubPolygon.FPoints)
    // before which the associated intersection point should
    // be inserted.
    Position: integer;
    // If @name is @true, the associated intersection point
    // must be inserted into @link(TSubPolygon.FPoints) as
    // @link(Position). Otherwise, it is a duplicate of a
    // point already at @link(Position).
    New: boolean;
  end;

  TIntersection = record
    Point: TPoint2D;
    Location1: TIntersectionLocation;
    Location2: TIntersectionLocation;
  end;

  TIntersectionArray = array of TIntersection;

  { TODO : Make TScreenObject.EvaluateSubPolygon a method of TSubPolygon. }

  // @abstract(@name is used to make determining whether a point is inside
  // a @link(TScreenObject) faster and to make finding the nearest point
  // on a line faster.)
  // @Seealso(TScreenObject.EvaluateSubPolygon)
  // @Seealso(TScreenObject.IsAnyPointCloser)
  TSubPolygon = class(TObject)
//  strict private
//    constructor Copy(Source: TSubPolygon; New: boolean); overload;
  private
    FOriginalCount: integer;
    // @name is the number of points used by the @classname.
    FCount: integer;
    // @name is the maximum X value of any
    // of the points used by the @classname.
    FMaxX: real;
    // @name is the maximum Y value of any
    // of the points used by the @classname.
    FMaxY: real;
    // @name is the minimum X value of any
    // of the points used by the @classname.
    FMinX: real;
    // @name is the minimum Y value of any
    // of the points used by the @classname.
    FMinY: real;
    // @name is the index of the first point used by the @classname.
    FStart: integer;
    // @name represents the @classname used to
    // process the first half of the points if
    // the number of points exceeds a threshold.
    FSubPolygon1: TSubPolygon;
    // @name represents the @classname used to
    // process the second half of the points if
    // the number of points exceeds a threshold.
    FSubPolygon2: TSubPolygon;
    FPoints: TRealPointArray;
    FSectionIndex: integer;
    // @name creates and instance of TSubPolygon.
    // If Count is large enough, it will create @link(FSubPolygon1)
    // and @link(FSubPolygon2) to handle what it needs to do.
    // @param(Points is the array of TPoint2Ds
    // to be used by @classname.)
    // @param(Count is the number of TPoint2Ds
    // in Points to be used by @classname.)
    // @param(Count is the index of the  first TPoint2Ds
    // in Points to be used by @classname.)

    constructor Create(const Points: TRealPointArray;
      const Count, Start, Section: integer);  
    procedure CreateSubPolygons(const Points: TRealPointArray;
      const Count, Start, Section: Integer);
    procedure SetMaxAndMinWhenNoSubPolygons(const Count, Start: Integer;
      const Points: TRealPointArray);
    procedure SetMaxAndMinFromSubPolygons;
    procedure InternalBoxIntersect(SubPolygons: TList;
      const BoxMinX, BoxMaxX, BoxMinY, BoxMaxY: Double);
  public
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    procedure GrowByOne;
    Procedure BoxIntersect(const Point1, Point2: TPoint2D; SubPolygons: TList);
    property SectionIndex: integer read FSectionIndex;
//    constructor Copy(Source: TSubPolygon); overload;
  end;
              
  TAssignmentMethod = (amEnclose, amIntersect);

  TCellAssignment = class(TObject)
  strict private
    FAnnotation: string;
    FAssignmentMethod: TAssignmentMethod;
    FOwnedSegment: TCellElementSegment;
    FSegment: TCellElementSegment;
    FLayer: integer;
    FSection: integer;
    FRow: integer;
    FColumn: integer;
  private
    procedure Assign(Cell: TCellAssignment);
    function GetSection: integer;
    procedure Store(Stream: TCompressionStream);
    procedure Restore(Stream: TDecompressionStream;
      const EncloseAnnotation, IntersectAnnotation: string);
    constructor CreateFromStream(Stream: TDecompressionStream;
      const EncloseAnnotation, IntersectAnnotation: string);
    constructor CreateFromCell(Cell: TCellAssignment);
  public
    property AssignmentMethod: TAssignmentMethod read FAssignmentMethod;
    property Layer: integer read FLayer;
    property Row: integer read FRow;
    property Column: integer read FColumn;
    property Segment: TCellElementSegment read FSegment;
    property Section: integer read GetSection;
    property Annotation: string read FAnnotation;
    Constructor Create(ALayer, ARow, ACol: integer;
      ASegment: TCellElementSegment; ASection: integer;
      const AnAnnotation: string; AnAssignmentMethod: TAssignmentMethod);
    Destructor Destroy; override;
  end;

  // @name is used to provide limited access to @link(TCellAssignment);
  // @name owns the @link(TCellAssignment)s stored in it.
  TCellAssignmentList = class(TObject)
  private
    FList : TList;
    FCached: Boolean;
    FMemoryStream: TMemoryStream;
    FCleared: Boolean;
    function GetItem(Index: integer): TCellAssignment;
    function GetCount: integer;
    procedure Restore(const EncloseAnnotation, IntersectAnnotation: string);
  public
    procedure Add(Item: TCellAssignment);
    procedure Assign(CachedList: TCellAssignmentList;
      const EncloseAnnotation, IntersectAnnotation: string);
    procedure Cache;
    property Count: integer read GetCount;
    property Items[Index: integer]: TCellAssignment read GetItem; default;
    Constructor Create;
    Destructor Destroy; override;
    procedure Clear;
    procedure Delete(Index: integer);
  end;

  TCachedCells = class(TObject)
  private
    FCachedLists: array[Low(TAssignmentLocation)..High(TAssignmentLocation)] of TCellAssignmentList;
    FEvalAt: TEvaluatedAt;
    FOrientation: TDataSetOrientation;
    FAssignmentLocation: TAssignmentLocation;
//    FMinRow: integer;
//    FMaxLayer: integer;
//    FMinLayer: integer;
//    FMaxCol: integer;
//    FMaxRow: integer;
//    FMinCol: integer;
  public
    function RestoreFromCache(CellList: TCellAssignmentList;
      EvalAt: TEvaluatedAt; Orientation: TDataSetOrientation;
      AssignmentLocation: TAssignmentLocation; const EncloseAnnotation,
      IntersectAnnotation: string): boolean;
    procedure UpdateCache(CellList: TCellAssignmentList;
      EvalAt: TEvaluatedAt; Orientation: TDataSetOrientation;
      AssignmentLocation: TAssignmentLocation);
    Destructor Destroy; override;
    procedure Invalidate;
//    property MinLayer: integer read FMinLayer;
//    property MaxLayer: integer read FMaxLayer;
//    property MinRow: integer read FMinRow;
//    property MaxRow: integer read FMaxRow;
//    property MinCol: integer read FMinCol;
//    property MaxCol: integer read FMaxCol;
  end;


  // @abstract(@name is an abstract base class.  Descendants are used for
  // assigning properties to @link(TDataArray)s from the front and side views.)
  TCustomScreenObjectDelegate = class abstract (TPersistent)
  strict protected
    FSelectedCells: TSelectedCells;
  protected
    // See @link(ModelSelection).
    FModelSelection: TModelSelection;
    // @name is the @link(TPhastModel) that is being used.
    FModel: TComponent;
    // @name is the @link(TScreenObject) for which this
    // @classname will be used.
    FScreenObject: TScreenObject;
    procedure AssignValuesToFrontDataSet(const Grid: TCustomGrid;
      const DataSet: TDataArray; OtherData: TObject;
      AssignmentLocation: TAssignmentLocation = alAll); virtual; abstract;
    procedure AssignValuesToSideDataSet(const Grid: TCustomGrid;
      const DataSet: TDataArray; OtherData: TObject;
      AssignmentLocation: TAssignmentLocation = alAll); virtual; abstract;
    procedure AssignValuesToTopDataSet(const Grid: TCustomGrid;
      const DataSet: TDataArray; OtherData: TObject;
      AssignmentLocation: TAssignmentLocation = alAll); virtual; abstract;
    procedure UpdateFrontSegments(const Grid: TCustomGrid;
      const EvaluatedAt: TEvaluatedAt);virtual; abstract;
    procedure UpdateSideSegments(const Grid: TCustomGrid;
      const EvaluatedAt: TEvaluatedAt); virtual;abstract;
    function GetCompiler(const Orientation: TDataSetOrientation): TRbwParser;
    procedure GetFrontCellsToAssign(const Grid: TCustomGrid;
      const DataSetFunction: string; OtherData: TObject;
      const DataSet: TDataArray; CellList: TCellAssignmentList;
      AssignmentLocation: TAssignmentLocation); virtual; abstract;
    procedure GetSideCellsToAssign(const Grid: TCustomGrid;
      const DataSetFunction: string; OtherData: TObject;
      const DataSet: TDataArray; CellList: TCellAssignmentList;
      AssignmentLocation: TAssignmentLocation); virtual; abstract;
    procedure GetTopCellsToAssign(const Grid: TCustomGrid;
      const DataSetFunction: string; OtherData: TObject;
      const DataSet: TDataArray; CellList: TCellAssignmentList;
      AssignmentLocation: TAssignmentLocation); virtual; abstract;
    {@name assigns a value to a particular cell in DataSet.

      In @name:

      (1) If OtherData = nil
      or not @Link(TInterpValuesItem)(OtherData).Values.UsePHAST_Interpolation,
      the inherited AssignCellValue is called.

      (2) Otherwise,
      values are assigned to DataSet at the location
      LayerIndex, RowIndex, ColIndex using PHAST-style interpolation.
      (See @link(TPhastInterpolationValues).)
      Expression and Compiler are not used in this case and the variables
      listed in UsedVariables are not updated.

      OtherData is set in @Link(TScreenObject.IsBoundaryTimeDataSetUsed).
    }
    procedure AssignCellValue(const UsedVariables: TStringList;
      const DataSet: TDataArray;
      LayerIndex, RowIndex, ColIndex: integer;
      const Compiler: TRbwParser; const Annotation: string;
      var Expression: TExpression; const OtherData: TObject;
      SectionIndex: integer); virtual;
    // @name returns an integer that indicates what
    // type of boundary condition,
    // if any, are specified by this @classname.
    function BoundaryType: integer; virtual;
    // @name returns @true if DataSet is a
    // @link(TSparseArrayPhastInterpolationDataSet)
    // @name also returns @true if DataSet is a @link(TCustomPhastDataSet)
    // and DataSet is specified by this @classname. OtherData may be
    // changed DataSet is a @link(TCustomPhastDataSet).
    function DataSetUsed(const DataSet: TDataArray;
      var OtherData: TObject): boolean; virtual;
    // @name returns a string that indicates that a location
    // was specified by being enclosed in this @classname and how the value
    // at that location was determined.
    function EncloseAnnotation(const DataSetFormula: string;
      const OtherData: TObject): string; virtual;
  protected
    // {@name (1) gets the proper DataSetFormula to apply to DataSet,
    // (2) gets the proper TRbwParser for DataSet, and (3) compiles
    // DataSetFormula to get Expression.  However, it doesn't need to do
    // any of that is PHAST-style interpolation is used.
    // See @link(TPhastInterpolationValues).}
    procedure InitializeExpression(out Compiler: TRbwParser;
      out DataSetFunction: string; out Expression: TExpression;
      const DataSet: TDataArray; const OtherData: TObject); virtual;
    procedure InitializeVariables(const UsedVariables: TStringList;
      const DataSet: TDataArray; const Expression: TExpression;
      const Compiler: TRbwParser);
    // @name returns a string that indicates that a location
    // was specified by being intersected by this @classname and how the value
    // at that location was determined.
    function IntersectAnnotation(const DataSetFormula: string;
      const OtherData: TObject): string; virtual;
    // The purpose of @name is to get First and Last.  They are
    // the indices of the first and last layer, row, or column
    // perpendicular to the plain of @link(TViewDirection) that are
    // enclosed or intersected by the @classname.
    //
    // In commented-out code, First and Last and changed to
    // frmGoPhast.PhastGrid.@link(TCustomGrid.LayerCount) for river
    // data sets.  It has been commented-out because it
    // messes up the display of the river
    // data on the status bar.
    //
    // It would be good to find a way around
    // this problem. As it is, all @name really
    // does is call the inherited @name.
    procedure OtherIndex(const LayerOrRow, RowOrColumn: integer;
      out First, Last: integer; const DataSet: TDataArray); virtual;
    { In preparation for evaluating a formula, @name updates the
      @link(TCustomValue)s whose names are listed in
      UsedVariables with their values at locations specifed by Layer, Row
      and Column.  DataSet is the @link(TDataArray) for which the
      @link(TCustomValue)s will be used. Compiler is the @link(TRbwParser)
      which will be used to evaluate a formula.}
    procedure UpdateVariables(const UsedVariables: TStringList;
      const DataSet: TDataArray; Layer, Row, Column: integer;
      const Compiler: TRbwParser);
    // @name creates an instance of @classname.
    Constructor Create(ScreenObject: TScreenObject); virtual;
    // @name indicates the model for which this delegate will be used.
    property ModelSelection: TModelSelection read FModelSelection;
    property SelectedCells: TSelectedCells read FSelectedCells;
    procedure AssignSelectedCells(const Grid: TCustomGrid); virtual; abstract;
  end;

  TCustomScreenObjectDelegateClass = class of TCustomScreenObjectDelegate;

  TDelegateItem = class(TCollectionItem)
  private
    FDelegate: TCustomScreenObjectDelegate;
    function GetDelegateClass: string;
    procedure SetDelegate(const Value: TCustomScreenObjectDelegate);
    procedure SetDelegateClass(const Value: string);
  protected
    function ScreenObject: TScreenObject;
  public
    destructor Destroy; override;
  published
    // @name indicates the class of @link(Delegate).  @name
    // must be declared before @link(Delegate) in order for
    // @link(Delegate) to be read from a stream correctly.
    property DelegateClass: string read GetDelegateClass
      write SetDelegateClass;
    // @name is the @link(TCustomScreenObjectDelegate) stored in
    // an instance of @classname.
    property Delegate: TCustomScreenObjectDelegate read FDelegate
      write SetDelegate;
  end;

  TDelegateCollection = class(TCollection)
  private
    FOwnerScreenObject: TScreenObject;
    FCachedDelegate: TCustomScreenObjectDelegate;
  public
    Constructor Create(OwnerScreenObject: TScreenObject);
    function Delegate(
      ModelSelection: TModelSelection): TCustomScreenObjectDelegate;
  end;

  {@abstract(@name is an abstract base class.  Its descendants
    store a series of @link(TCustomPhastBoundaryCondition)s.
    The series define how one aspect of a boundary condition changes
    with time.)}
  TCustomPhastBoundaryCollection = class(TPhastCollection)
  private
    // See @link(ScreenObject).
    FScreenObject: TScreenObject;
    // See @link(TimeList).
    FTimeList: TPhastTimeList;
    // See @link(TimeList).
    procedure SetTimeList(const Value: TPhastTimeList);
  protected
    // @name specifies the type of data (real number, integer, boolean,
    // or string) stored in the @classname.  However, at the time this
    // was written only real numbers and integers were supported because
    // those were the only types supported in descendants of
    // @link(TSparseArrayPhastInterpolationDataSet).
    // See TCustomPhastBoundaryCondition.@link(
    // TCustomPhastBoundaryCondition.GetDataSet).
    function GetDatatype: TRbwDataType; virtual; abstract;
    // @name is the @link(TPhastTimeList) that stores the @link(TDataArray)s
    // for this boundary condition.
    property TimeList: TPhastTimeList read FTimeList write SetTimeList;
    // @name calls TCustomPhastBoundaryCondition.@link(
    // TCustomPhastBoundaryCondition.UpdateMixtureExpression) for each
    // of its Items.
    procedure UpdateMixtureExpression;
    procedure UpdateFormulaExpression;
  public
    // @name removes all of the @classname items and invalidates
    // @link(TimeList).
    procedure Clear;
    // @name gets the @link(TSparseArrayPhastInterpolationDataSet) from
    // @link(TimeList)
    // that starts at ATime.  If there isn't such a data set already,
    // one is created.
    function GetDataSet(const ATime: double):
      TSparseArrayPhastInterpolationDataSet;
    // @name is the @link(TScreenObject) that owns this @classname.
    property ScreenObject: TScreenObject read FScreenObject write
      FScreenObject;
  end;

  {@abstract(@name stores a collection of
   @link(TRealPhastBoundaryCondition)s.)}
  TRealPhastBoundaries = class(TCustomPhastBoundaryCollection)
  protected
    // @name indicates that this boundary condition
    // represents a real number.
    function GetDatatype: TRbwDataType; override;
  public
    // @name creates an instance of @classname and sets the type of
    // @link(TCustomPhastBoundaryCondition) used by the @classname to be
    // @link(TRealPhastBoundaryCondition).
    constructor Create(Model: TComponent);
  end;

  {@abstract(@name stores a collection of
  // @link(TIntegerPhastBoundaryCondition)s.)}
  TIntegerPhastBoundaries = class(TCustomPhastBoundaryCollection)
  protected
    // @name indicates that this boundary condition
    // represents an integer.
    function GetDatatype: TRbwDataType; override;
  public
    // @name creates an instance of @classname and sets the type of
    // @link(TCustomPhastBoundaryCondition) used by the @classname to be
    // @link(TIntegerPhastBoundaryCondition).
    constructor Create(Model: TComponent);
  end;

  {@abstract(@name is an abstract base class.  Its descendants are used to
    define all aspects of one type of boundary condition for one
    @link(TScreenObject) including those aspects that vary with time.)}
  TCustomPhastBoundary = class(TPersistent)
  private
    // See @link(ScreenObject).
    FScreenObject: TScreenObject;
    procedure SetModel(const Value: TComponent);
  protected
    FModel: TComponent;
    procedure InvalidateModel;
    // @name clears all the @link(TCustomPhastBoundaryCollection)s that are
    // part of the @classname.
    procedure Clear; virtual; abstract;
    // See @link(ScreenObject).
    procedure SetScreenObject(const Value: TScreenObject); virtual;
    // For boundary condition data sets that do not vary with time,
    // @name is used to set the formula to be applied to the data set
    // for this @classname.  If Formula = '', the data set specified
    // by DataSetName will no long be affected by the @link(ScreenObject).
    procedure UpdateBoundaryDataSet(const DataSetName: string; var Formula: string);
      overload;
    procedure UpdateBoundaryDataSet(const DataArray: TDataArray;
      var Formula: string); overload;
    // @name calls TCustomPhastBoundaryCollection.@link(
    // TCustomPhastBoundaryCollection.UpdateMixtureExpression) for each of
    // the @link(TCustomPhastBoundaryCollection) it owns.
    procedure UpdateMixtureExpression; virtual; abstract;
    procedure UpdateFormulaExpression; virtual; abstract;
  public
    property Model: TComponent read FModel write SetModel;
    constructor Create(ScreenObject: TScreenObject; Model: TComponent);
    // @name is the @link(TScreenObject) that owns this @classname.
    property ScreenObject: TScreenObject read FScreenObject write
      SetScreenObject;
  end;

  TCustomInterpolatedBoundary = class(TCustomPhastBoundary)
  strict protected
    // See @link(BoundaryValue).
    FBoundaryValue: TRealPhastBoundaries;
    // See @link(Solution).
    FSolution: TIntegerPhastBoundaries;
    // See @link(BoundaryValue).
    procedure SetBoundaryValue(const Value: TRealPhastBoundaries);
    // See @link(Solution).
    procedure SetSolution(const Value: TIntegerPhastBoundaries);
  protected
    // See @link(TCustomPhastBoundary.ScreenObject).
    procedure SetScreenObject(const Value: TScreenObject); override;
    // @name calls @link(Solution).@link(
    // TCustomPhastBoundaryCollection.UpdateMixtureExpression) and
    // @link(BoundaryValue).@link(
    // TCustomPhastBoundaryCollection.UpdateMixtureExpression).
    procedure UpdateMixtureExpression; override;
    procedure UpdateFormulaExpression;  override;
    function BoundaryDataSetFormula(DataSetName: string): string;

  public
    // If Source is a @classname, @name copies @link(Solution) and
    // @link(BoundaryValue)
    procedure Assign(Source: TPersistent); override;
    // @name clears @link(Solution) and
    // @link(BoundaryValue).
    procedure Clear; override;
    // @name creates an instance of @classname.
    constructor Create(ScreenObject: TScreenObject; Model: TComponent);
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
  published
    // @name defines how the solution for this @classname
    // varies through time for its @link(TCustomPhastBoundary.ScreenObject).
    property Solution: TIntegerPhastBoundaries
      read FSolution write SetSolution;
    // @name defines how the main value for this @classname
    // varies through time for its @link(TCustomPhastBoundary.ScreenObject).
    property BoundaryValue: TRealPhastBoundaries read FBoundaryValue
      write SetBoundaryValue;
  end;

  {@abstract(@name represents a boundary condition that only applies to
    one @link(TViewDirection).)}
  TCustomOrientedPhastBoundary = class(TCustomInterpolatedBoundary)
  private
    // See @link(Orientation).
    FOrientation: TViewDirection;
    // See @link(Orientation).
    procedure SetOrientation(const Value: TViewDirection); virtual;
  protected
    // See @link(TCustomPhastBoundary.ScreenObject).
    procedure SetScreenObject(const Value: TScreenObject); override;
  public
    // If Source is a @classname, @name copies the @link(Orientation)
    // of Source.
    procedure Assign(Source: TPersistent); override;
  published
    // @name is the @link(TViewDirection) for this @classname.
    property Orientation: TViewDirection read FOrientation write SetOrientation
      stored False;
  end;

  {@abstract(@name represents a flux boundary in PHAST for one
    @link(TScreenObject).)}
  TFluxBoundary = class(TCustomOrientedPhastBoundary)
  private
    // See TCustomOrientedPhastBoundary.@link(
    // TCustomOrientedPhastBoundary.Orientation).
    // @name sets AssociatedSolution.@link(
    // TCustomPhastBoundaryCollection.TimeList) and
    // Flux.@link(TCustomPhastBoundaryCollection.TimeList).
    procedure SetOrientation(const Value: TViewDirection); override;
  protected
    // See @link(TCustomPhastBoundary.ScreenObject).
    procedure SetScreenObject(const Value: TScreenObject); override;
  public
    // @name creates an instance of @classname.
    constructor Create(ScreenObject: TScreenObject; Model: TComponent);
  published
    // @exclude
    // AssociatedSolution is used only for backwards compatibility.
    // AssociatedSolution represents how the solution associated with the
    // flux varies
    // through time in the current @link(TCustomPhastBoundary.ScreenObject).
    // See @link(Solution).
    property AssociatedSolution: TIntegerPhastBoundaries
      read FSolution write SetSolution stored False;
    // @exclude
    // @name is used only for backwards compatibility.
    // @name represents how the flux varies
    // through time in the current @link(TCustomPhastBoundary.ScreenObject).
    // See @link(BoundaryValue).
    property Flux: TRealPhastBoundaries read FBoundaryValue
      write SetBoundaryValue stored False;
  end;

  {@abstract(@name represents a leaky boundary in PHAST for one
    @link(TScreenObject).)}
  TLeakyBoundary = class(TCustomOrientedPhastBoundary)
  private
    // See @link(HydraulicConductivity).
    FHydraulicConductivity: string;
    // See @link(Thickness).
    FThickness: string;
    // See @link(HydraulicConductivity).
    function GetHydraulicConductivity: string;
    // See TCustomOrientedPhastBoundary.@link(
    // TCustomOrientedPhastBoundary.Orientation).
    // @name sets AssociatedSolution.@link(
    // TCustomPhastBoundaryCollection.TimeList) and
    // Head.@link(TCustomPhastBoundaryCollection.TimeList).
    procedure SetOrientation(const Value: TViewDirection); override;
    // See @link(HydraulicConductivity).
    procedure SetHydraulicConductivity(const Value: string);
    // See @link(Thickness).
    procedure SetThickness(const Value: string);
    function GetThickness: string;
    procedure Loaded;
  public
    // If Source is a @classname, @name copies @link(Solution),
    // @link(BoundaryValue), @link(HydraulicConductivity), and @link(Thickness).
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create(ScreenObject: TScreenObject; Model: TComponent);
    // If Source is a @classname, @name copies @link(Solution), and
    // @link(BoundaryValue) but not @link(HydraulicConductivity),
    // and @link(Thickness).
    // @name is used in @link(TUndoSetScreenObjectProperties).
    procedure PartialAssign(Source: TPersistent);
    // @name resets the formulas for @link(HydraulicConductivity) and
    // @link(Thickness) to '' if
    // (@link(BoundaryValue).Count = 0) and (@link(Solution).Count = 0).
    procedure Reset;
  published

    // @name is the formula for specifying the leaky boundary
    // hydraulic conductivity
    // for this @link(TCustomPhastBoundary.ScreenObject).
    property HydraulicConductivity: string read GetHydraulicConductivity
      write SetHydraulicConductivity;
    // @name is the formula for specifying the leaky boundary
    // thickness
    // for this @link(TCustomPhastBoundary.ScreenObject).
    property Thickness: string read GetThickness write SetThickness;
    // @exclude
    // @name is used only for backwards compatibility.
    // @name defines how the head for this @classname
    // varies through time for its @link(TCustomPhastBoundary.ScreenObject).
    // See @link(BoundaryValue).
    property Head: TRealPhastBoundaries read FBoundaryValue
      write SetBoundaryValue stored False;
    // @exclude
    // @name is used only for backwards compatibility.
    // @name defines how the associated solution for this @classname
    // varies through time for its @link(TCustomPhastBoundary.ScreenObject).
    // See @link(Solution).
    property AssociatedSolution: TIntegerPhastBoundaries
      read FSolution write SetSolution stored False;
  end;

  {@abstract(@name represents a river boundary in PHAST for one
    @link(TScreenObject).)}
  TRiverBoundary = class(TCustomInterpolatedBoundary)
  private
    // @name: string;
    // See @link(BedHydraulicConductivity).
    FBedHydraulicConductivity: string;
    // @name: string;
    // See @link(BedThickness).
    FBedThickness: string;
    // @name: string;
    // See @link(Depth).
    FDepth: string;
    // @name: string;
    // See @link(Description).
    FDescription: string;
    // @name: string;
    // See @link(Width).
    FWidth: string;
    // See @link(BedHydraulicConductivity).
    procedure SetBedHydraulicConductivity(const Value: string);
    // See @link(BedThickness).
    procedure SetBedThickness(const Value: string);
    // See @link(Depth).
    procedure SetDepth(const Value: string);
    // See @link(Description).
    procedure SetDescription(const Value: string);
    // See @link(Width).
    procedure SetWidth(const Value: string);
    function GetBedHydraulicConductivity: string;
    function GetBedThickness: string;
    function GetDepth: string;
    function GetWidth: string;
    procedure Loaded;
  public
    // If Source is a @classname, @name copies @link(AssociatedSolution),
    // @link(BedHydraulicConductivity), @link(BedThickness), @link(Depth),
    // @link(Description), @link(Head), and @link(Width).
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create(ScreenObject: TScreenObject; Model: TComponent);
    // @name returns true if all the published properties have been set
    // to values that indicate that a river boundary is present.
    function IsBoundary: boolean;
    // If Source is a @classname, @name copies @link(AssociatedSolution),
    // and @link(Head) but not @link(BedHydraulicConductivity),
    // @link(BedThickness), @link(Depth),
    // @link(Description), and @link(Width).
    // @name is used in @link(TUndoSetScreenObjectProperties).
    procedure PartialAssign(Source: TPersistent);
  published
    // @name is the formula for specifying the river bed
    // hydraulic conductivity
    // for this @link(TCustomPhastBoundary.ScreenObject).
    property BedHydraulicConductivity: string read GetBedHydraulicConductivity
      write SetBedHydraulicConductivity;
    // @name is the formula for specifying the river bed thickness
    // for this @link(TCustomPhastBoundary.ScreenObject).
    property BedThickness: string read GetBedThickness write SetBedThickness;
    // @name is the formula for specifying the river depth
    // for this @link(TCustomPhastBoundary.ScreenObject).
    property Depth: string read GetDepth write SetDepth;
    // @name is the name of the river
    // for this @link(TCustomPhastBoundary.ScreenObject).
    property Description: string read FDescription write SetDescription;
    // @name is the formula for specifying the river width
    // for this @link(TCustomPhastBoundary.ScreenObject).
    property Width: string read GetWidth write SetWidth;
    // @name defines how the associated solution for this @classname
    // varies through time for its @link(TCustomPhastBoundary.ScreenObject).
    property AssociatedSolution: TIntegerPhastBoundaries
      read FSolution write SetSolution;
    // @name defines how the head for this @classname
    // varies through time for its @link(TCustomPhastBoundary.ScreenObject).
    property Head: TRealPhastBoundaries read FBoundaryValue
      write SetBoundaryValue;
  end;

  // @name specifies the type of solution in a specified head
  // boundary condition. stAssociated means that the solution
  // is merely associated with the head.  stSpecified specifies
  /// that the solution also has a fixed value.
  TSolutionType = (stAssociated, stSpecified);

  {@abstract(@name represents a specified head boundary in PHAST for one
    @link(TScreenObject).)}
  TSpecifiedHeadBoundary = class(TCustomInterpolatedBoundary)
  private
    // See @link(Solution).
    // See @link(SolutionType).
    FSolutionType: TSolutionType;

    // See @link(Solution).
    // See @link(SolutionType).
    procedure SetSolutionType(const Value: TSolutionType);
    procedure Loaded;
  public
    // If Source is a @classname, @name copies @link(Solution),
    // @link(BoundaryValue), and @link(SolutionType).
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create(ScreenObject: TScreenObject; Model: TComponent);
  published
    // @name specifies whether @link(Solution) represents
    // a specified solution or an associated solution.
    property SolutionType: TSolutionType read FSolutionType write
      SetSolutionType;
    // @exclude
    // @name is used only for backwards compatibility.
    // @name defines how the solution for this @classname
    // varies through time for its @link(TCustomPhastBoundary.ScreenObject).
    property AssociatedSolution: TIntegerPhastBoundaries
      read FSolution write SetSolution stored False;
    // @exclude
    // @name is used only for backwards compatibility.
    // @name defines how the head for this @classname
    // varies through time for its @link(TCustomPhastBoundary.ScreenObject).
    property Head: TRealPhastBoundaries read FBoundaryValue
      write SetBoundaryValue stored False;
  end;

  {@exclude @abstract(@name is retained only for backwards compatibility.)}
  TSpecifiedSolutionBoundary = class(TCustomPhastBoundary)
  private
    FSolution: TIntegerPhastBoundaries;
    procedure SetSolution(const Value: TIntegerPhastBoundaries);
  protected
    procedure SetScreenObject(const Value: TScreenObject); override;
    procedure UpdateMixtureExpression; override;
    procedure UpdateFormulaExpression;  override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    constructor Create(ScreenObject: TScreenObject; Model: TComponent);
    destructor Destroy; override;
  published
    property Solution: TIntegerPhastBoundaries read FSolution
      write SetSolution;
  end;

  {@abstract(@name an open interval in a well boundary in PHAST.)}
  TWellInterval = class(TCollectionItem)
  private
    // @name: double;
    // See @link(FirstElevation).
    FFirstElevation: double;
    // @name: double;
    // See @link(SecondElevation).
    FSecondElevation: double;
    // See @link(FirstElevation).
    procedure SetFirstElevation(const Value: double);
    // See @link(SecondElevation).
    procedure SetSecondElevation(const Value: double);
    procedure InvalidateModel;
  public
    // If Source is a @classname, @name copies @link(FirstElevation) and
    // @link(SecondElevation) from Source.
    procedure Assign(Source: TPersistent); override;
  published
    // @name represents elevation or depth of one end of the well
    // screen in the open interval.
    property FirstElevation: double read FFirstElevation write
      SetFirstElevation;
    // @name represents elevation or depth of the end of the well
    // screen opposite @link(FirstElevation) in the open interval.
    property SecondElevation: double read FSecondElevation write
      SetSecondElevation;
  end;

  {@abstract(@name represents a series of @link(TWellInterval)s.)}
  TWellIntervals = class(TPhastCollection)
  private
    FScreenObject: TScreenObject;
  public
    // @name creates an instance of @classname and the type of the
    // Items to be @link(TWellInterval).
    constructor Create(ScreenObject: TScreenObject; Model: TComponent);
    property ScreenObject: TScreenObject read FScreenObject;
  end;

  {@abstract(@name represents a well in PHAST for one
    @link(TScreenObject).)}
  TWellBoundary = class(TCustomInterpolatedBoundary)
  private
    // See @link(AllocateByPressureAndMobility).
    FAllocateByPressureAndMobility: boolean;
    // See @link(Description).
    FDescription: string;
    // @name: double;
    // See @link(Diameter).
    FDiameter: double;
    // @name: @link(TWellIntervals);
    // See @link(Intervals).
    FIntervals: TWellIntervals;
    // @name: double;
    // See @link(LandSurfaceDatum).
    FLandSurfaceDatum: double;
    // @name: @link(TWellElevationFormat);
    // See @link(WellElevationFormat).
    FWellElevationFormat: TWellElevationFormat;
    // See @link(AllocateByPressureAndMobility).
    procedure SetAllocateByPressureAndMobility(const Value: boolean);
    // See @link(Description).
    procedure SetDescription(const Value: string);
    // See @link(Diameter).
    procedure SetDiameter(const Value: double);
    // See @link(Intervals).
    procedure SetIntervals(const Value: TWellIntervals);
    // See @link(LandSurfaceDatum).
    procedure SetLandSurfaceDatum(const Value: double);
    // See @link(WellElevationFormat).
    procedure SetWellElevationFormat(const Value: TWellElevationFormat);
  public
    // If Source is a @classname, @name copies
    // @link(AllocateByPressureAndMobility), @link(Description),
    // @link(Diameter), @link(InjectionOrPumpingRate), @link(Intervals),
    // @link(LandSurfaceDatum), @link(Solution), and @link(WellElevationFormat).
    procedure Assign(Source: TPersistent); override;
    // @name clears @link(InjectionOrPumpingRate) and
    // @link(Solution).
    procedure Clear; override;
    // @name creates an instance of @classname.
    constructor Create(ScreenObject: TScreenObject; Model: TComponent);
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    // @name returns true if all the published properties have been set
    // to values that indicate that a well boundary is present.
    function IsBoundary: boolean;
    // If Source is a @classname, @name copies @link(Solution) and
    // @link(InjectionOrPumpingRate) but not
    // @link(AllocateByPressureAndMobility), @link(Description),
    // @link(Diameter), @link(Intervals),
    // @link(LandSurfaceDatum), and @link(WellElevationFormat).
    // @name is used in @link(TUndoSetScreenObjectProperties).
    procedure PartialAssign(Source: TPersistent);
  published
    // If @name is @true, PHAST will distribute the pumping amount
    // among layers by taking into account the pressure and permeability in
    // the layers in which the well has an open screen interval.
    // If @name is @false, PHAST will not consider the pressure
    // and permeability in the layers when distributing the pumping
    // amount among layers.
    property AllocateByPressureAndMobility: boolean read
      FAllocateByPressureAndMobility write SetAllocateByPressureAndMobility
      default True;
    // @name is the name of the well.
    property Description: string read FDescription write SetDescription;
    // @name is the diameter of the well.
    property Diameter: double read FDiameter write SetDiameter;
    // @name defines the open well intervals.
    // See @link(WellElevationFormat) and @link(LandSurfaceDatum).
    property Intervals: TWellIntervals read FIntervals write SetIntervals;
    // If @link(WellElevationFormat) = wefDepth, @name represents the datum
    // from which the elevations (really depths) in @link(Intervals) are
    // measured. If @link(WellElevationFormat) <> wefDepth. @name is ignored.
    property LandSurfaceDatum: double read FLandSurfaceDatum write
      SetLandSurfaceDatum;
    // @name determines whether elevations in @link(Intervals) represent
    // true elevations or depths below a datum. (see @link(LandSurfaceDatum).)
    property WellElevationFormat: TWellElevationFormat read FWellElevationFormat
      write SetWellElevationFormat;
    // @name is the rate at which water is pumped into or out of the porous
    // medium.
    // A positive @name means flow out of the porous medium.
    property InjectionOrPumpingRate: TRealPhastBoundaries read
      FBoundaryValue write SetBoundaryValue;
  end;

  TIntegerItem = class(TCollectionItem)
  private
    FValue: integer;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Value: integer read FValue write FValue;
  end;

  TIntegerCollection = class(TPhastCollection)
  private
    function GetValue(Index: integer): integer;
    procedure SetValue(Index: integer; const Value: integer);
  published
  public
    constructor Create(Model: TComponent);
    property Values[Index: integer]: integer read GetValue write SetValue;
  end;

  { TODO : Allow the user to control visibility of TScreenObject via selected
  col, row, layer. }

  { TODO :
Replace the commands that do things on one view of the TScreenObject
a separate "strategy" object that is delegated with responsibility for one
view. }

  {
  @abstract(@name is the internal representation of an object on the screen.)
  @name has a series of 0 or more points that defines its location in two
  dimensions.  It may also have zero, one, or two associated elevation
  formulas that define its position in the third dimension.
  (See @link(TScreenObject.ElevationFormula),
  @link(TScreenObject.HigherElevationFormula),
  and @link(TScreenObject.LowerElevationFormula).)
  It can be used
  to set the values of zero or more @link(TDataArray)s.  For each
  such @link(TDataArray),
  it has a formula (See @link(TScreenObject.DataSetFormulas))
  that describes how it sets the values for them.
  The @link(TScreenObject.ViewDirection) of the @link(TScreenObject)
  determine whether the 2D location of
  the object is with respect to the top, front, or side view of the model.
  It has properties that define boundary conditions in
  PHAST and it has methods to deal with PHAST-style interpolation.
  (See the following list.)
  @unorderedList(
    @item(@link(TScreenObject.InterpValues))
    @item(FluxBoundary)
    @item(LeakyBoundary)
    @item(RiverBoundary)
    @item(SpecifiedHeadBoundary)
    @item(WellBoundary)
  )

  @seealso(TPhastInterpolationValues)
  }

  TModflowBoundaries = class(TObject)
  private
    // @name represents a Time-Variant Specified Head boundary.
    // @name is only created if needed.
    // @seealso(TScreenObject.CreateChdBoundary)
    FModflowChdBoundary: TChdBoundary;
    // @name represents an Evapotranspiration Segments boundary.
    // @name is only created if needed.
    // @seealso(TScreenObject.CreateEtsBoundary)
    FModflowEtsBoundary: TEtsBoundary;
    // @name represents an Evapotranspiration boundary.
    // @name is only created if needed.
    // @seealso(TScreenObject.CreateEvtBoundary)
    FModflowEvtBoundary: TEvtBoundary;
    // @name represents an Drain boundary.
    // @name is only created if needed.
    // @seealso(TScreenObject.CreateDrnBoundary)
    FModflowDrnBoundary: TDrnBoundary;
    // @name represents an Drain Return boundary.
    // @name is only created if needed.
    // @seealso(TScreenObject.CreateDrtBoundary)
    FModflowDrtBoundary: TDrtBoundary;
    // @name represents an General Head boundary.
    // @name is only created if needed.
    // @seealso(TScreenObject.CreateGhbBoundary)
    FModflowGhbBoundary: TGhbBoundary;
    // @name represents an Lake boundary.
    // @name is only created if needed.
    // @seealso(TScreenObject.CreateLakBoundary)
    FModflowLakBoundary: TLakBoundary;
    // @name represents an Recharge boundary.
    // @name is only created if needed.
    // @seealso(TScreenObject.CreateRchBoundary)
    FModflowRchBoundary: TRchBoundary;
    // @name represents an Reservoir boundary.
    // @name is only created if needed.
    // @seealso(TScreenObject.CreateResBoundary)
    FModflowResBoundary: TResBoundary;
    // @name represents an River boundary.
    // @name is only created if needed.
    // @seealso(TScreenObject.CreateRivBoundary)
    FModflowRivBoundary: TRivBoundary;
    // @name represents an Stream-Flow Routing boundary.
    // @name is only created if needed.
    // @seealso(TScreenObject.CreateSfrBoundary)
    FModflowSfrBoundary: TSfrBoundary;
    // @name represents an Well boundary.
    // @name is only created if needed.
    // @seealso(TScreenObject.CreateWelBoundary)
    FModflowWellBoundary: TMfWellBoundary;
    // @name represents an UZF boundary.
    // @name is only created if needed.
    // @seealso(TScreenObject.CreateUzfBoundary)
    FModflowUzfBoundary: TUzfBoundary;
    FModflowHeadObservations: THobBoundary;
    FModflowHfbBoundary: THfbBoundary;
    FModflowGage: TStreamGage;
    FModflowMnw2Boundary: TMnw2Boundary;
  public
    Destructor Destroy; override;
    property ModflowChdBoundary: TChdBoundary read FModflowChdBoundary
      write FModflowChdBoundary;
    property ModflowGhbBoundary: TGhbBoundary read FModflowGhbBoundary
      write FModflowGhbBoundary;
    property ModflowWellBoundary: TMfWellBoundary read FModflowWellBoundary
      write FModflowWellBoundary;
    property ModflowRivBoundary: TRivBoundary read FModflowRivBoundary
      write FModflowRivBoundary;
    property ModflowDrnBoundary: TDrnBoundary read FModflowDrnBoundary
      write FModflowDrnBoundary;
    property ModflowDrtBoundary: TDrtBoundary read FModflowDrtBoundary
      write FModflowDrtBoundary;
    property ModflowRchBoundary: TRchBoundary read FModflowRchBoundary
      write FModflowRchBoundary;
    property ModflowEvtBoundary: TEvtBoundary read FModflowEvtBoundary
      write FModflowEvtBoundary;
    property ModflowEtsBoundary: TEtsBoundary read FModflowEtsBoundary
      write FModflowEtsBoundary;
    property ModflowResBoundary: TResBoundary read FModflowResBoundary
      write FModflowResBoundary;
    property ModflowLakBoundary: TLakBoundary read FModflowLakBoundary
      write FModflowLakBoundary;
    property ModflowSfrBoundary: TSfrBoundary read FModflowSfrBoundary
      write FModflowSfrBoundary;
    property ModflowUzfBoundary: TUzfBoundary read FModflowUzfBoundary
      write FModflowUzfBoundary;
    property ModflowHeadObservations: THobBoundary read FModflowHeadObservations
      write FModflowHeadObservations;
    property ModflowHfbBoundary: THfbBoundary read FModflowHfbBoundary
      write FModflowHfbBoundary;
    property ModflowGage: TStreamGage read FModflowGage Write FModflowGage;
    property ModflowMnw2Boundary: TMnw2Boundary read FModflowMnw2Boundary
      write FModflowMnw2Boundary;
  end;

  TModflowDelegate = class;

  TScreenObject = class(TObserver)
  strict private
    FCachedCells: TCachedCells;
    FIntervalTree: TRbwIntervalTree;
    FDelegateCollection: TDelegateCollection;
    // @name returns @true if Location is outside a box surrounding
    // the @classname plus a buffer distance.
    // @seealso(IsOutsideSubPolygonBoxPlusBuffer)
    function IsOutsideBoxPlusBuffer(const Location: TPoint2D;
      const BufferDistance: real; const Anisotropy: real): boolean;
    // @name tests whether any point on the two @link(TSubPolygon)s
    // is closer to Location than Distance.  If so, @name returns @true
    // and Distance is set to the distance to the closest point on
    // The @classname and ClosestLocation
    // is set to the location whose point was closest to Location.
    function IsAnyPointInSubPolygonCloser(const Location: TPoint2D;
      var Distance: real; out ClosestLocation: TPoint2D;
      const Anisotropy: real; const ASubPolygon: TSubPolygon): boolean;
    // @name returns @true if Location is outside SubPolygon
    // plus a buffer distance.
    // @seealso(IsOutsideSubPolygonBoxPlusBuffer)
    function IsOutsideSubPolygonBoxPlusBuffer(const Location: TPoint2D;
      const BufferDistance: real; const Anisotropy: real;
      const SubPolygon: TSubPolygon): boolean;
  private
    FCanInvalidateModel: boolean;
    FModel: TComponent;
    // @name holds all the subscriptions related to
    // mixtures for @link(TDataArray)s.
    FDataSetMixtureSubscriptions: TObjectList;
    // See @link(InterpValues).
    FInterpValues: TInterpValuesCollection;
    // @name is assigned in @link(AssignValuesToPhastDataSet).
    FMixtureCompiler: TRbwParser;
    // @name is assigned in @link(AssignValuesToPhastDataSet).
    FMixtureExpression: TExpression;
    // @name is assigned and Freed in @link(AssignValuesToPhastDataSet).
    FMixtureVariables: TStringList;
    // See @link(FluxBoundary).
    FFluxBoundary: TFluxBoundary;
    // See @link(LeakyBoundary).
    FLeakyBoundary: TLeakyBoundary;
    // See @link(RiverBoundary).
    FRiverBoundary: TRiverBoundary;
    // See @link(SpecifiedHeadBoundary).
    FSpecifiedHeadBoundary: TSpecifiedHeadBoundary;
    // @exclude
    // See @link(SpecifiedSolutionBoundary).
    FSpecifiedSolutionBoundary: TSpecifiedSolutionBoundary;
    // See @link(WellBoundary).
    FWellBoundary: TWellBoundary;
    {
      @name is used to temporarily store the lower elevation
      of the @classname in current cell when the @classname has
      an upper and a lower elevation.
    }
    FBottomElevation: real;
    {
      @name is a @link(SubscriptionUnit.TObserver) that responds to changes
      in any of the data sets referenced in @link(LowerElevationFormula)
      by calling @link(Changed).}
    FBottomElevSubscription: TObserver;
    // See @link(BoundaryDataSetFormulas).
    FBoundaryDataSetFormulas: TList;
    // See @link(BoundaryDataSets).
    FBoundaryDataSets: TList;
    {
     @name holds a list of @link(SubscriptionUnit.TObserver)s
     related to boundary conditions set by this @classname.
     If this @classname changes, the @link(SubscriptionUnit.TObserver)s
     in @name will ensure that those boundary conditions
     get updated properly when needed.}
    FBoundaryDataSetSubscriptions: TObjectList;
    // See @link(CanvasCoordinates).
    FCanvasCoordinates: TPointArray;
    // See @link(Capacity).
    FCapacity: integer;
    // See @link(CellSize).
    FCellSize: real;
    // See @link(CellSizeUsed).
    FCellSizeUsed: boolean;
    // See @link(ColorLine).
    FColorLine: boolean;
    // See @link(Count).
    FCount: integer;
    // See @link(DataSetFormulas).
    FDataSetFormulas: TList;
    // See @link(DataSets).
    FDataSets: TList;
    {
     @name holds a list of @link(SubscriptionUnit.TObserver)s
     related to @link(DataSetUnit.TDataArray)s set by this @classname.
     If this @classname changes, the @link(SubscriptionUnit.TObserver)s
     in @name will ensure that those @link(DataSetUnit.TDataArray)s
     get updated properly when needed.}
    FDataSetSubscriptions: TObjectList;
    // See @link(Deleted).
    FDeleted: boolean;
    // See @link(ElevationCount) and @link(TElevationCount).
    FElevationCount: TElevationCount;
    {
      @name is a @link(SubscriptionUnit.TObserver) that responds to changes
      in any of the data sets referenced in @link(ElevationFormula)
      by calling @link(Changed).}
    FElevSubscription: TObserver;
    // See @link(EvaluatedAt) and @link(TEvaluatedAt).
    FEvaluatedAt: TEvaluatedAt;
    // See @link(FillColor).
    FFillColor: TColor;
    // See @link(FillScreenObject).
    FFillScreenObject: boolean;
    FHigher3DElevations: T3DSparseRealArray;
    // @name indicates whether or not @link(FHigher3DElevations) are
    // up-to-date.  If they aren't, they will be recalculated when needed.
    FHigher3DElevationsNeedsUpdating: boolean;
    // See @link(LineColor).
    FLineColor: TColor;
    FLower3DElevations: T3DSparseRealArray;
    // @name indicates whether or not @link(FLower3DElevations) are
    // up-to-date.  If they aren't, they will be recalculated when needed.
    FLower3DElevationsNeedsUpdating: boolean;
    // See @link(MaxX).
    FMaxX: real;
    // See @link(MaxY).
    FMaxY: real;
    // See @link(MinX).
    FMinX: real;
    // See @link(MinY).
    FMinY: real;
    {
      @name indicates whether or not @link(SelectLines) is
      up-to-date.  If it isn't, it will be recalculated when needed.}
    FNeedToUpdateLine: boolean;
    // @name: @link(TRealPointArray);
    // See @link(Points) and @link(GoPhastTypes.TRealPointArray).
    FPoints: TRealPointArray;
    {
      @name indicates whether or not @link(FScreenObjectArea) is
      up-to-date.  If it isn't, it will be recalculated when needed.
      See @link(ScreenObjectArea).}
    FRecalculateArea: boolean;
    {
      @name indicates whether or not @link(MinX), @link(MaxX),
      @link(MinY), and @link(MaxY) are
      up-to-date.  If the aren't, they will be recalculated when needed.}
    FRecalculateBox: boolean;
    {
      @name indicates whether or not @link(FCanvasCoordinates) is
      up-to-date.  If it isn't, it will be recalculated when needed.
      See @link(ScreenObjectArea).}
    FRecalculateCoordinates: boolean;
    {
      @name indicates whether or not @link(FScreenObjectLength) is
      up-to-date.  If it isn't, it will be recalculated when needed.
      See @link(ScreenObjectLength).}
    FRecalculateLength: boolean;
    // See @link(ScreenObjectArea).
    FScreenObjectArea: real;
    // See @link(ScreenObjectLength).
    FScreenObjectLength: real;
    // See @link(Segments) and @link(TCellElementSegmentList).
    FSegments: TCellElementSegmentList;
    // See @link(Selected).
    FSelected: boolean;
    // See @link(SelectedVertexCount).
    FSelectedVertexCount: integer;
    // See @link(SelectedVertices).
    FSelectedVertices: TBooleanDynArray;
    // See @link(SelectLines) and @link(SelectUnit.TLine).
    FSelectLines: TLines;
    // See @link(SetValuesByInterpolation).
    FSetValuesByInterpolation: boolean;
    // See @link(SetValuesOfEnclosedCells).
    FSetValuesOfEnclosedCells: boolean;
    // See @link(SetValuesOfIntersectedCells).
    FSetValuesOfIntersectedCells: boolean;
    // @name is used to determine whether adding
    // a point to @classname will cause it to intersect an existing Segment.
    FLastSubPolygon: TSubPolygon;
    {
      @name is used to temporarily store the upper elevation
      of the @classname in current cell when the @classname has
      an upper and a lower elevation.}
    FTopElevation: real;
    {
      @name is a @link(SubscriptionUnit.TObserver) that responds to changes
      in any of the data sets referenced in @link(HigherElevationFormula)
      by calling @link(Changed).}
    FTopElevSubscription: TObserver;
    // See @link(ViewDirection) and @link(TViewDirection).
    FViewDirection: TViewDirection;
    // See @link(Visible).
    FVisible: boolean;
    // See @link(NeedToResetSelectedVertexCount).
    FNeedToResetSelectedVertexCount: boolean;
    // @name is actually a TObjectList.
    // @name holds instances of @link(TSubPolygon).
    // @seealso(CreateSubPolygons)
    // @seealso(SubPolygonCount)
    // @seealso(SubPolygons)
    // @seealso(IsAnyPointCloser)
    FSubPolygons: TList;
    FModflowBoundaries: TModflowBoundaries;
    FSectionStarts: TValueArrayStorage;
    FImportedSectionElevations: TValueArrayStorage;
    FCurrentValues: TValueArrayStorage;
    FImportedHigherSectionElevations: TValueArrayStorage;
    FImportedLowerSectionElevations: TValueArrayStorage;
    FImportedValues: TValueCollection;
    FIsClosedCached: Boolean;
    FCachedClosed: boolean;
    FGlListCreated: Boolean;
    FGlScreenObjectList: TGLuint;
    FListUpToDate: Boolean;
    FIFACE: TIface;
    FModpathParticles: TParticleStorage;
    FUpdateCount: Integer;
    FGpcPolygons: TGpcPolygonClass;
    FPriorObjectIntersectLengthCol: Integer;
    FPriorObjectIntersectLengthRow: Integer;
    FPriorObjectIntersectLengthLayer: Integer;
    FPriorObjectIntersectLengthResult: Real;
    FCachedDataSetIndex: integer;
    FElevationFormulaObject: TFormulaObject;
    FHigherElevationFormulaObject: TFormulaObject;
    FLowerElevationFormulaObject: TFormulaObject;
    FPriorObjectSectionIntersectLengthCol: integer;
    FPriorObjectSectionIntersectLengthRow: integer;
    FPriorObjectSectionIntersectLengthLayer: integer;
    FPriorObjectSectionIntersectLengthResult: real;
    FPriorObjectSectionIntersectLengthSection: integer;
    procedure CreateLastSubPolygon;
    procedure DestroyLastSubPolygon;
    function GetSubPolygonCount: integer;
    function GetSubPolygon(Index: integer): TSubPolygon;
    // See @link(FluxBoundary).
    procedure SetFluxBoundary(const Value: TFluxBoundary);
    // See @link(InterpValues).
    procedure SetInterpValues(const Value: TInterpValuesCollection);
    // See @link(LeakyBoundary).
    procedure SetLeakyBoundary(const Value: TLeakyBoundary);
    // See @link(RiverBoundary).
    procedure SetRiverBoundary(const Value: TRiverBoundary);
    // @name determines whether @link(FluxBoundary) is stored.
    function StoreFlux: boolean;
    // @name determines whether @link(LeakyBoundary) is stored.
    function StoreLeaky: boolean;
    // @name determines whether @link(RiverBoundary) is stored.
    function StoreRiver: boolean;
    // See @link(SpecifiedHeadBoundary).
    procedure SetSpecifiedHeadBoundary(
      const Value: TSpecifiedHeadBoundary);
    // @exclude
    // See @link(SpecifiedSolutionBoundary).
    procedure SetSpecifiedSolutionBoundary(
      const Value: TSpecifiedSolutionBoundary);
    // See @link(WellBoundary).
    procedure SetWellBoundary(const Value: TWellBoundary);
    // @name determines whether @link(SpecifiedHeadBoundary) is stored.
    function StoreSpecifiedHead: boolean;
    // @name determines whether @link(WellBoundary) is stored.
    function StoreWell: boolean;
    // AddPointFromColumn creates a @link(TEdgePoint)
    // at the column boundary on Grid
    // indicated by ColIndex on the line defined by PreviousPoint and APoint.
    // If the new @link(TEdgePoint) is between PreviousPoint and APoint, it is
    // added at TempPoints1[Count] and then Count is incremented.  The Position
    // of the new @link(TEdgePoint) is set to Position.
    // AddPointFromColumn is called when updating @Link(Segments).
    // @param(ColIndex indicates the column boundary on the grid.  The new
    // point will be where the line intersects this column.)
    // @param(Grid is the @link(TPhastGrid) from which the point is being
    // added.)
    // @param(PreviousPoint is one of the points defining the line.)
    // @param(APoint is the other point defining the line.)
    // @param(TempPoints1 is the array to which the point is added.)
    // @param(Count is the new number of points in TempPoints1.)
    // @param(Position is the @link(TEdgePosition) for the new point.)
    procedure AddPointFromColumn(const ColIndex: integer;
      const Grid: TCustomGrid; const PreviousPoint, APoint: TEdgePoint;
      var TempPoints1: TEdgePointArray; var Count: integer;
      const Position: TEdgePosition);
    // AddPointFromLayer creates a @link(TEdgePoint)
    // at the layer boundary on Grid
    // indicated by LayerIndex on the line defined by PreviousPoint and APoint.
    // If the new @link(TEdgePoint) is between PreviousPoint and APoint, it is
    // added at TempPoints1[Count] and then Count is incremented.  The Position
    // of the new @link(TEdgePoint) is set to Position.
    // AddPointFromLayer is called when updating @Link(Segments).
    // @param(LayerIndex indicates the layer boundary on the grid.  The new
    // point will be where the line intersects this layer.)
    // @param(Grid is the @link(TPhastGrid) from which the point is being
    // added.)
    // @param(PreviousPoint is one of the points defining the line.)
    // @param(APoint is the other point defining the line.)
    // @param(TempPoints1 is the array to which the point is added.)
    // @param(Count is the new number of points in TempPoints1.)
    // @param(Position is the @link(TEdgePosition) for the new point.)
    procedure AddPointFromLayer(const LayerIndex: integer;
      const Grid: TCustomGrid; const PreviousPoint, APoint: TEdgePoint;
      var TempPoints1: TEdgePointArray; var Count: integer;
      const Position: TEdgePosition);
    // AddPointFromRow creates a @link(TEdgePoint) at the row boundary on Grid
    // indicated by RowIndex on the line defined by PreviousPoint and APoint.
    // If the new @link(TEdgePoint) is between PreviousPoint and APoint, it is
    // added at TempPoints1[Count] and then Count is incremented.  The Position
    // of the new @link(TEdgePoint) is set to Position.
    // AddPointFromRow is called when updating @Link(Segments).
    // @param(RowIndex indicates the row boundary on the grid.  The new
    // point will be where the line intersects this row.)
    // @param(Grid is the @link(TPhastGrid) from which the point is being
    // added.)
    // @param(PreviousPoint is one of the points defining the line.)
    // @param(APoint is the other point defining the line.)
    // @param(TempPoints1 is the array to which the point is added.)
    // @param(Count is the new number of points in TempPoints1.)
    // @param(Position is the @link(TEdgePosition) for the new point.)
    procedure AddPointFromRow(const RowIndex: integer;
      const Grid: TCustomGrid; const PreviousPoint, APoint: TEdgePoint;
      var TempPoints1: TEdgePointArray; var Count: integer;
      const Position: TEdgePosition);
    procedure Assign3DElevations(Formula: string;
      const SparseArray: T3DSparseRealArray);
    {@name fills @link(T3DSparseRealArray SparseArray)  with values based on
     Compiler.CurrentExpression.  Only cells
     whose values ought to be set by the @classname will be set.
     This procedure is only called if @link(ViewDirection) =
     @link(TViewDirection vdFront).
     @param(Compiler is the TRbwParser whose CurrentExpression will
     be used to assign values.)
     @param(SparseArray is a @link(T3DSparseRealArray) whose values
     will be set.)
     }
    procedure Assign3DElevationsFromFront(const Compiler: TRbwParser;
      const SparseArray: T3DSparseRealArray);
    {@name fills @link(T3DSparseRealArray SparseArray)  with values based on
     Compiler.CurrentExpression.  Only cells
     whose values ought to be set by the @classname will be set.
     This procedure is only called if @link(ViewDirection)
     = @link(TViewDirection vdSide).
     @param(Compiler is the TRbwParser whose CurrentExpression will
     be used to assign values.)
     @param(SparseArray is a @link(T3DSparseRealArray) whose values
     will be set.)}
    procedure Assign3DElevationsFromSide(const Compiler: TRbwParser;
      const SparseArray: T3DSparseRealArray);
    {@name fills @link(FHigher3DElevations) with values based on either
    @link(HigherElevationFormula) or @link(ElevationFormula) depending on
    @link(ElevationCount).}
    procedure AssignHigher3DElevations;
    {@name fills @link(FLower3DElevations) with values based on
    @link(LowerElevationFormula).}
    procedure AssignLower3DElevations;
    // CalculateCanvasCoordinates takes all the TPoint2Ds in
    // @Link(Points) and calculates the corresponding TPoints.  These are
    // stored in FCanvasCoordinates.  The TPoints are used when drawing
    // the ScreenObject on the screen.
    procedure CalculateCanvasCoordinates;
    // @name first calls @link(ClearSubPolygons) to get rid of existing
    // @link(TSubPolygon)s.  It then creates one @link(TSubPolygon) for
    // each section of the @classname. (See @link(SectionCount).)
    // Each @link(TSubPolygon) will have access to
    // the TPoint2Ds in its section @Link(Points).  Each @link(TSubPolygon)
    // will divide itself into two additional subcountours until the
    // number of points each has access to is small enough.
    // The @link(TSubPolygon)s are used in speeding up evaluation of whether a
    // point is inside the @classname.
    procedure CreateSubPolygons;
    // @name destroys @link(TSubPolygon)s in @link(FSubPolygons).
    procedure ClearSubPolygons;
    {@name is used to draw the @classname when Direction =
     @Link(ViewDirection).
     The elevation functions (if any) are not used.
     (To facilitated fast selection
     of objects, the selected objects are redrawn in an Image32
     after being in the normal fashion in a bitmap.
     @param(Bitmap32 is the TBitmap32 on which the @classname will be drawn.)
     @param(Direction is the @link(TViewDirection) for Bitmap32.)
     @param(DrawAsSelected is used to determine whether
     to draw the screen object in the
     normal fashion or as a selected screen object.)}
    procedure Draw0Elev(Const Bitmap32: TBitmap32;
      const Direction: TViewDirection;
      const DrawAsSelected: Boolean = False);
     {@name is used to draw the @classname when @Link(ElevationCount) =
     ecOne.  if Direction = @Link(ViewDirection), @link(Draw0Elev) is
     called.  Otherwise @Link(ElevationFormula) is used to determine
     either the X or Y coordinates.
     @param(Bitmap32 is the TCanvas on which the @classname will be drawn.)
     @param(Direction is the @link(TViewDirection) for Bitmap32.)
     @param(DrawAsSelected is used to determine whether
     to draw the screen object in the
     normal fashion or as a selected screen object.
     (To facilitate fast selection
     of objects, the selected objects are redrawn in an Image32
     after being in the normal fashion in a bitmap.)) }
    procedure Draw1Elev(
      Const Bitmap32: TBitmap32;
      const Direction: TViewDirection;
      const DrawAsSelected: Boolean = False);
    {@name is used to draw the @classname when @Link(ElevationCount) =
    ecTwo.  if Direction = @Link(ViewDirection), @Link(Draw0Elev) is
    called.  Otherwise @Link(HigherElevationFormula) and
    @Link(LowerElevationFormula) is used to determine either the
    X or Y coordinates.
     @param(Bitmap32 is the TBitmap32 on which the @classname will be drawn.)
     @param(Direction is the @link(TViewDirection) for Bitmap32.)
     @param(DrawAsSelected is used to determine whether
     to draw the screen object in the
     normal fashion or as a selected screen object.
     (To facilitate fast selection
     of objects, the selected objects are redrawn in an Image32
     after being in the normal fashion in a bitmap.)) }
    procedure Draw2Elev(Const Bitmap32: TBitmap32;
      const Direction: TViewDirection;
      const DrawAsSelected: Boolean = False);
    // @name is used to help determine whether the point at
    // X, Y is inside the @classname. (See @Link(IsPointInside).)
    // IsInside is updated to reflect the effect of ASubPolygon on the final
    // result.  IsInside is not the final result.
    // @name calls itself recursively.
    // @param(ASubPolygon is the @link(TSubPolygon) which will be evaluated.)
    // @param(X is the X-coordinate of the location being tested.)
    // @param(Y is the Y-coordinate of the location being tested.)
    // @param(IsInside represents the effects of ASubPolygon on whether
    // the point (X, Y) is inside the polygon.)
    procedure EvaluateSubPolygon(const ASubPolygon: TSubPolygon;
      const X, Y: real; var IsInside: boolean);
    { TODO : Compare and possibly combine GetACol and GetColumn. }
    // If @Link(EvaluatedAt) = eaNodes, @name returns the
    // column whose center is closest to X.
    // except that it can return Grid.ColumnCount
    // for EvaluatedAt = eaBlocks.
    // If @Link(EvaluatedAt) = eaBlocks @name returns
    // the index of the column center that is closest to X.
    // @name is similar to @link(GetColumn).
    // (X is in a coordinates system parallel to the grid.)
    // @name is called by @link(UpdateTopSegments)
    // and @link(UpdateFrontSegments).
    // @param(Grid is the @link(TPhastGrid) from which the column boundary
    // will be extracted.)
    // @param(X is the X coordinate (in a coordinates system parallel
    // to the grid) of the location of interest.)
    function GetACol(const Grid: TCustomGrid; const X: double): integer;
    { TODO : Compare and possibly combine GetAPhastLayer and GetLayer. }
    // If @Link(EvaluatedAt) = eaNodes, @name returns the
    // layer boundary that is closest to Z.
    // If @Link(EvaluatedAt) = eaBlocks @name returns
    // the index of the layer center that is closest to Z
    // except that it can return Grid.LayerCount
    // for EvaluatedAt = eaBlocks.
    // @name is called by @link(UpdateFrontSegments)
    // and @link(UpdateSideSegments).
    // @param(Grid is the @link(TPhastGrid) from which the layer boundary
    // will be extracted.)
    // @param(Z is the Z coordinate (in a coordinates system parallel
    // to the grid) of the location of interest.)
    function GetAPhastLayer(const Grid: TCustomGrid; const Z: double): integer;
    { TODO : Compare and possibly combine GetARow and GetRow. }
    // If @Link(EvaluatedAt) = eaNodes, @name returns the
    // row boundary that is closest to Y (
    // except that it can return Grid.RowCount
    // for EvaluatedAt = eaBlocks.
    // If @Link(EvaluatedAt) = eaBlocks @name returns
    // the index of the row center that is closest to Y.
    // (Y is in a coordinates system parallel to the grid.)
    // @name is called by @link(UpdateTopSegments)
    // and @link(UpdateSideSegments).
    // @param(Grid is the @link(TPhastGrid) from which the layer boundary
    // will be extracted.)
    // @param(Y is the Y coordinate (in a coordinates system parallel
    // to the grid) of the location of interest.)
    function GetARow(const Grid: TCustomGrid; const Y: double): integer;
    // @name is the Read function for the
    // @Link(BoundaryDataSetFormulas) property.
    function GetBoundaryDataSetFormulas(const Index: integer): string;
    // @name is the Read function for the @Link(BoundaryDataSets) property.
    function GetBoundaryDataSets(const Index: integer): TDataArray;
    // @name is the Read function for the @Link(CanvasCoordinates) property.
    // It recalculates the coordinates when needed.
    function GetCanvasCoordinates: TPointArray;
    // @name returns the column number that contains X.
    // @name is similar to @link(GetACol).
    // @param(Grid is the @link(TPhastGrid) from which the column boundary
    // will be extracted.)
    // @param(X is the X coordinate (in a coordinates system parallel
    // to the grid) of the location of interest.)
    function GetColumn(const Grid: TCustomGrid; const X: real): integer;

    { @name sets FirstCol and LastCol to the range of columns intersected by
      the @classname.
      @param(Grid is the grid being tested)
      @param(TempMinX is the minimum X value in the grid coordinate system.)
      @param(TempMaxX is the maximum X value in the grid coordinate system.)
      @param(FirstCol is the first column intersected by TempMinX to TempMaxX.)
      @param(LastCol is the last column intersected by TempMinX to TempMaxX.)
      }
    procedure GetColumns(const Grid: TCustomGrid; TempMinX, TempMaxX:
      real; out FirstCol, LastCol: integer);
    // See @link(Count).
    function GetCount: integer;
    // See @link(DataSetCapacity).
    function GetDataSetCapacity: integer;
    // See @link(DataSetCount).
    function GetDataSetCount: integer;
    // See @link(DataSetFormulas).
    function GetDataSetFormulas(const Index: integer): string;
    // See @link(DataSets).
    function GetDataSets(const Index: integer): TDataArray;
    // See @link(EdgePoints).
    function GetEdgePoints(const Index: integer): TEdgePoint;
    // See @link(FillColor).
    function GetFillColor: TColor;
    // @name sets XMin, XMax, YMin, and YMax to be the positions
    // of the grid cell or element specified by Col, Row, and Layer.
    // Which is used depends on @link(EvaluatedAt).
    procedure GetGridCellOrElementLimits(const Col, Row, Layer: integer;
      out XMin, XMax, YMin, YMax: double);
    // @name  calls @link(UpdateHigher3DElevations).  Then it returns
    // @link(FHigher3DElevations).
    function GetHigher3DElevations: T3DSparseRealArray;
    // @name returns the layer number that contains Z.
    // @name is similar to @link(GetAPhastLayer).
    // @param(Grid is the @link(TPhastGrid) from which the layer boundary
    // will be extracted.)
    // @param(Z is the Z coordinate (in a coordinates system parallel
    // to the grid) of the location of interest.)
    function GetLayer(const Grid: TCustomGrid; const Z: real): integer;

    { @name sets FirstLayer and LastLayer to the range of layers intersected by
      the @classname.
      @param(Grid is the grid being tested)
      @param(TempMinZ is the minimum Z value in the grid coordinate system.)
      @param(TempMaxZ is the maximum Z value in the grid coordinate system.)
      @param(FirstLayer is the first layer intersected by TempMinZ to TempMaxZ.)
      @param(LastRow is the last layer intersected by TempMinZ to TempMaxZ.)
      @param(LastLayer specifies how to interpret the columns.)
      }
    procedure GetLayers(const Grid: TCustomGrid; TempMinZ, TempMaxZ: real;
      out FirstLayer, LastLayer: integer);
    // See @link(LineColor).
    function GetLineColor: TColor;
    // @name calls @link(UpdateLower3DElevations).  Then it returns
    // @link(FLower3DElevations).
    function GetLower3DElevations: T3DSparseRealArray;
    // See @link(MaxX).
    function GetMaxX: real;
    // See @link(MaxY).
    function GetMaxY: real;
    // See @link(MinX).
    function GetMinX: real;
    // See @link(MinY).
    function GetMinY: real;
    // See @link(Points).
    function GetPoints(const Index: integer): TPoint2D;
    // @name returns the row number that contains Y.
    // @name is similar to @link(GetARow).
    // @param(Grid is the @link(TPhastGrid) from which the row boundary
    // will be extracted.)
    // @param(Y is the Y coordinate (in a coordinates system parallel
    // to the grid) of the location of interest.)
    function GetRow(const Grid: TCustomGrid; const Y: real): integer;
      
    { @name sets FirstRow and LastRow to the range of rows intersected by
      the @classname.
      @param(Grid is the grid being tested)
      @param(TempMinY is the minimum Y value in the grid coordinate system.)
      @param(TempMaxY is the maximum Y value in the grid coordinate system.)
      @param(FirstRow is the first row intersected by TempMinY to TempMaxY.)
      @param(LastRow is the last row intersected by TempMinY to TempMaxY.)
      @param(EvaluatedAt specifies how to interpret the columns.)
      }
    procedure GetRows(const Grid: TCustomGrid; TempMinY, TempMaxY: real;
      out FirstRow, LastRow: integer);
    // See @link(SelectedVertexCount).
    function GetSelectedVertexCount: integer;
    // See @link(SelectedVertices).
    function GetSelectedVertices(const index: integer): boolean;
    // See @link(SelectLines).
    function GetSelectLines: TLines;
    // @name increases @link(Capacity).
    procedure Grow;
    // @name increases @link(Capacity) by Amount.
    procedure GrowBy(Amount: integer);
    // @name returns true if the point (X,Y) is inside the bounding
    // box for the @classname.
    function InsideBox(const X, Y: real): boolean;
    {@name calculates the distance between a point and a line segment.
     @param(P is the point.)
     @param(A is one of the points defining the line segment.)
     @param(B is the other point defining the line segment.)
     @param(Closest is the closest point on the line segment to P.)
     @param(Anisotropy is the Anisotropy used in calculating the distance.)}
    function MinDistPointLine(const P, A, B: TPoint2D;
      out Closest: TPoint2D; const Anisotropy: real): real;
    // @name returns the distance between points A and B taking into account
    // Anisotropy.
    function PointToPointDist(const A, B: TPoint2D;
      const Anisotropy: real): real;
    // @name is used if @link(LowerElevationFormula) has an error
    // to reset it to a default valid value.
    procedure ResetBottomElevationFormula(const Compiler: TRbwParser;
      const ErrorMessage: string);
    // @name is used if @link(ElevationFormula) has an error
    // to reset it to a default valid value.
    procedure ResetElevationFormula(const Compiler: TRbwParser;
      const ErrorMessage: string);
    // @name deselects all vertices in the @classname.
    procedure ResetSelectedVertexCount;
    // @name is used if @link(HigherElevationFormula) has an error
    // to reset it to a default valid value.
    procedure ResetTopElevationFormula(const Compiler: TRbwParser;
      const ErrorMessage: string);
    // See @link(BoundaryDataSetFormulas).
    procedure SetBoundaryDataSetFormulas(const Index: integer;
      const Value: string);
    // See @link(BoundaryDataSets).
    procedure SetBoundaryDataSets(const Index: integer;
      const DataSet: TDataArray);
    // See @link(Capacity).
    procedure SetCapacity(Value: integer);
    // See @link(CellSize).
    procedure SetCellSize(const Value: real);
    // See @link(CellSizeUsed).
    procedure SetCellSizeUsed(const Value: boolean);
    // See @link(ColorLine).
    procedure SetColorLine(const Value: boolean);
    // See @link(Count).
    procedure SetCount(const Value: integer);
    // See @link(DataSetCapacity).
    procedure SetDataSetCapacity(const Value: integer);
    // See @link(DataSetFormulas).
    procedure SetDataSetFormulas(const Index: integer; Value: string);
    // See @link(DataSets).
    procedure SetDataSets(const Index: integer; const DataSet: TDataArray);
    // See @link(Deleted).
    procedure SetDeleted(const Value: boolean);
    // See @link(EdgePoints).
    procedure SetEdgePoints(const Index: integer; const Value: TEdgePoint);
    // See @link(ElevationCount).
    procedure SetElevationCount(const Value: TElevationCount);
    // See @link(EvaluatedAt).
    procedure SetEvaluatedAt(const Value: TEvaluatedAt);
    // See @link(FillColor).
    procedure SetFillColor(const Value: TColor);
    // See @link(FillScreenObject).
    procedure SetFillScreenObject(const Value: boolean);
    // See @link(LineColor).
    procedure SetLineColor(const Value: TColor);
    // See @link(Points).
    procedure SetPoints(const Index: integer; const Value: TPoint2D);
    // See @link(Selected).
    procedure SetSelected(const Value: boolean);
    // See @link(SelectedVertices).
    procedure SetSelectedVertices(const index: integer; const Value: boolean);
    // See @link(SetValuesByInterpolation).
    procedure Set_SetValuesByInterpolation(const Value: boolean);
    // See @link(SetValuesOfEnclosedCells).
    procedure Set_SetValuesOfEnclosedCells(const Value: boolean);
    // See @link(SetValuesOfIntersectedCells).
    procedure Set_SetValuesOfIntersectedCells(const Value: boolean);
    // See @link(Visible).
    procedure SetVisible(const Value: boolean);
    {@name sorts the points in Input and returns the
     sorted points in SortedPoints. The points in Input lie along the line
     (PreviousPoint,APoint).  They are sorted so that they occur in order
     along that line.  Duplicate points are eliminated.
     @param(Input is an array of @link(TEdgePoint)s to be sorted.)
     @param(SortedPoints is an array of @link(TEdgePoint)s obtained by
     sorting Input.)
     @param(APoint is the end point of a line along
     which are all the points in Input.)
     @param(PreviousPoint is the beginning point of a line along
     which are all the points in Input.)
     @param(MaxLength is the number of points in Input to be sorted.)
     }
    procedure SortPoints(const Input: TEdgePointArray;
      out SortedPoints: TEdgePointArray; const APoint,
      PreviousPoint: TEdgePoint; const MaxLength: integer;
      Const EpsilonX: real = 0; Const EpsilonY: real = 0);
    // @name updates @link(MinX), @link(MaxX), @link(MinY), and @link(MaxY).  
    procedure UpdateBox;
    //@name updates the contents of @link(Segments) when
    // @link(ViewDirection) = @link(TViewDirection vdFront).
    procedure UpdateFrontSegments(const Grid: TCustomGrid;
      const EvaluatedAt: TEvaluatedAt);
    //@name updates the contents of @link(Segments) when
    // @link(ViewDirection) = @link(TViewDirection vdSide).
    procedure UpdateSideSegments(const Grid: TCustomGrid;
      const EvaluatedAt: TEvaluatedAt);
    //@name updates the contents of @link(Segments) when
    // @link(ViewDirection) = @link(TViewDirection vdTop).
    procedure UpdateTopSegments(const Grid: TCustomGrid;
      const EvaluatedAt: TEvaluatedAt; const PointsRotated: boolean;
      var RotatedPoints: TEdgePointArray);
    // @name is used when accessing vertices to raise an exception if
    // Index is invalid.
    procedure ValidateIndex(const Index: integer); inline;
    // @name returns false if Col, Row, Layer refer to an invalid
    // cell.  Only two of the three are checked.  The ones that are
    // checked depend on @link(ViewDirection)
    function ValidCell(const Col, Row, Layer: integer): boolean;
    // See @link(SetValuesOfEnclosedCells).
    function Get_SetValuesOfEnclosedCells: boolean;
    // On input, Sorter contains a series of pointers to TPoint2Ds
    // that are on a straight line and
    // Point1 and Point2 are points defining the endpoints of the line.
    // On exit the contents of Sorter will be rearranged so that they
    // are in order from Point1 toward Point2.
//    procedure SortPointsInCorrectDirection(Sorter: TList;
//      const Point1, Point2: TPoint2D);
    // @name is used to specify the intersection of a segment of @classname
    // with the box defined by XMin, XMax, YMin, YMax.
    //
    // On exit, Point1 and Point2 are the points of the segment;
    // TempPoints will contain the intersection points; and
    // PointCount will contain the number of points of intersection.
//    procedure GetPointsOnLineSegment(const RotatedPoints: TRealPointArray;
//      const XMin, XMax, YMin, YMax: Double; const Index: Integer;
//      out Point1, Point2: TPoint2D; out TempPoints: T2DRealPointArray6;
//      out PointCount: Integer);

    // @name returns the proper @link(TCustomScreenObjectDelegate) for
    // the active @link(TPhastModel.ModelSelection TPhastModel.ModelSelection).
    procedure UpdateHigher3DElevations;
    procedure UpdateLower3DElevations;
    // @name is used to assign data to a particular cell when PHAST-style
    // interpolation is used  and the data is integer data.
    // See @link(TPhastInterpolationValues).
    procedure AssignIntegerDataWithPhastInterpolation(const DataSet: TDataArray;
      const LayerIndex, RowIndex, ColIndex: integer; const Comment: string;
      const InterpValue: TInterpValuesItem);
    // @name is used to assign data to a particular cell when PHAST-style
    // interpolation is used  and the data is real-number data.
    // See @link(TPhastInterpolationValues).
    procedure AssignRealDataWithPhastInterpolation(const DataSet: TDataArray;
      const LayerIndex, RowIndex, ColIndex: integer; const Comment: string;
      const InterpValue: TInterpValuesItem);
    // See @link(MixtureDataSetFormula).
    function GetMixtureDataSetFormula(const Index: integer): string;
    // See @link(MixtureDataSetFormula).
    procedure SetMixtureDataSetFormula(const Index: integer;
      const Value: string);
    // The purpose of @name is to (1) determine the formula for
    // a mixture to use with using PHAST-style
    // interpolation, (2) determine the TRbwParser to use with that formula,
    // and (3) compile that formula into a
    // TExpression.  See @link(TPhastInterpolationValues).
    // @param(Compiler is set in @name to the correct TRbwParser to use
    // with the mixture formula.)
    // @param(MixtureFormula is set in @name to the formula for the mixture.)
    // @param(Expression is set in @name to the TExpression that results
    // from compiling MixtureFormula.)
    // @param(DataSet is the @link(TDataArray) to which the mixture formula
    // applies.)
    // @param(OtherData if DataSet is a normal data set, OtherData is
    // a @link(TInterpValuesItem). if DataSet represents a boundary condition,
    // OtherData is a @link(TCustomPhastBoundaryCondition).)
    procedure InitializeMixtureExpression(out Compiler: TRbwParser;
      out MixtureFormula: string; out Expression: TExpression;
      const DataSet: TDataArray; const OtherData: TObject);
    // @name returns a formula that can be used to identify the type of
    // 3D boundary condition (if any) that is assigned with this @classname.
    function ThreeDBoundaryFormula: string;
    // @name returns a formula that can be used to identify the type of
    // 2D boundary condition (if any) that is assigned with this @classname.
    function TwoDBoundaryFormula: string;
    function ZoomBox(VD: TViewDirection): TQrbwZoomBox2;
    function FrameScreenObject(VD: TViewDirection): TScreenObject;
    function IAmACurrentScreenObject: Boolean;
    // @name calls @link(TfrmGoPhast.ScreenObjectsChanged).
    procedure NotifyGuiOfChange(Sender: TObject);
    procedure SetModflowChdBoundary(const Value: TChdBoundary);
    procedure RefreshGui(Sender: TObject);
    procedure SetModflowGhbBoundary(const Value: TGhbBoundary);
    procedure SetModflowWellBoundary(const Value: TMfWellBoundary);
    procedure SetModflowRivBoundary(const Value: TRivBoundary);
    procedure SetModflowDrnBoundary(const Value: TDrnBoundary);
    procedure SetModflowDrtBoundary(const Value: TDrtBoundary);
    function StoreModflowChdBoundary: Boolean;
    function StoreModflowDrnBoundary: Boolean;
    function StoreModflowDrtBoundary: Boolean;
    function StoreModflowGhbBoundary: Boolean;
    function StoreModflowRivBoundary: Boolean;
    function StoreModflowWellBoundary: Boolean;
    procedure SetModflowRchBoundary(const Value: TRchBoundary);
    function StoreModflowRchBoundary: Boolean;
    procedure SetModflowEvtBoundary(const Value: TEvtBoundary);
    function StoreModflowEvtBoundary: Boolean;
    procedure SetModflowEtsBoundary(const Value: TEtsBoundary);
    function StoreModflowEtsBoundary: Boolean;
    procedure SetModflowResBoundary(const Value: TResBoundary);
    function StoreModflowResBoundary: Boolean;
    procedure SetModflowLakBoundary(const Value: TLakBoundary);
    function StoreModflowLakBoundary: Boolean;
    procedure GetInterpDistance(const InterpValue: TInterpValuesItem;
      var Distance: Double; const DataSet: TDataArray;
      const LayerIndex, RowIndex, ColIndex: Integer);
    procedure AssignTopDataSetValues(const Grid: TCustomGrid;
      Expression: TExpression; const DataSetFunction: string;
      Compiler: TRbwParser; UsedVariables: TStringList;
      OtherData: TObject; const DataSet: TDataArray;
      AssignmentLocation: TAssignmentLocation = alAll);
    function GetSegments: TCellElementSegmentList;
    function GetSectionCount: integer;
    function GetSectionEnd(const Index: integer): integer;
    function GetSectionStart(const Index: integer): integer;
    function GetSectionClosed(const Index: integer): boolean;
    function GetSectionLength(const Index: integer): integer;
    procedure SetModflowSfrBoundary(const Value: TSfrBoundary);
    function StoreModflowSfrBoundary: Boolean;
    function GetModflowSfrBoundary: TSfrBoundary;
    function GetModflowLakBoundary: TLakBoundary;
    function GetModflowResBoundary: TResBoundary;
    function GetModflowEtsBoundary: TEtsBoundary;
    function GetModflowEvtBoundary: TEvtBoundary;
    function GetModflowRchBoundary: TRchBoundary;
    function GetModflowDrtBoundary: TDrtBoundary;
    function GetModflowDrnBoundary: TDrnBoundary;
    function GetModflowRivBoundary: TRivBoundary;
    function GetModflowWellBoundary: TMfWellBoundary;
    function GetModflowGhbBoundary: TGhbBoundary;
    function GetModflowChdBoundary: TChdBoundary;
    function GetFluxBoundary: TFluxBoundary;
    function GetLeakyBoundary: TLeakyBoundary;
    function GetRiverBoundary: TRiverBoundary;
    function GetSpecifiedHeadBoundary: TSpecifiedHeadBoundary;
    function GetSpecifiedSolutionBoundary: TSpecifiedSolutionBoundary;
    function GetWellBoundary: TWellBoundary;
    function GetModflowBoundaries: TModflowBoundaries;
    procedure SubPolygonXLimits(Subject: TObject; out LowerBoundary,
      UpperBoundary: double);
    procedure SubPolygonYLimits(Subject: TObject; out LowerBoundary,
      UpperBoundary: double);
    function GetModflowUzfBoundary: TUzfBoundary;
    procedure SetModflowUzfBoundary(const Value: TUzfBoundary);
    function StoreModflowUzfBoundary: Boolean;
    procedure Draw1ElevModflow(const Direction: TViewDirection;
      const Bitmap32: TBitmap32; const DrawAsSelected: Boolean);
    function GetModflowHeadObservations: THobBoundary;
    procedure SetModflowHeadObservations(const Value: THobBoundary);
    function StoreModflowHeadObservations: Boolean;
    procedure SetSectionStarts(const Value: TValueArrayStorage);
    function GetSectionStarts: TValueArrayStorage;
    procedure CreateSectionStarts;
    procedure SetImportedSectionElevations(const Value: TValueArrayStorage);
    procedure SetImportedHigherSectionElevations(
      const Value: TValueArrayStorage);
    procedure SetImportedLowerSectionElevations(
      const Value: TValueArrayStorage);
    function GetImportedHigherSectionElevations: TValueArrayStorage;
    function GetImportedLowerSectionElevations: TValueArrayStorage;
    function GetImportedSectionElevations: TValueArrayStorage;
    {@name fills @link(T2DSparseRealArray SparseArray) with values based on
     Compiler.CurrentExpression.  Only cells
     whose values ought to be set by the @classname will be set.
     This procedure is only called if @link(ViewDirection)
     = @link(TViewDirection vdTop).
     @param(Compiler is the TRbwParser whose CurrentExpression will
     be used to assign values.)
     @param(SparseArray is a @link(T3DSparseRealArray) whose values
     will be set.)}
    procedure Assign3DElevationsFromTop(const Compiler: TRbwParser;
      const SparseArray: T3DSparseRealArray);
    procedure Draw1ElevPhast(const Direction: TViewDirection;
      const Bitmap32: TBitmap32; const DrawAsSelected: Boolean);
    procedure SetImportedValues(const Value: TValueCollection);
    procedure SetModflowHfbBoundary(const Value: THfbBoundary);
    function GetModflowHfbBoundary: THfbBoundary;
    function GetElevSubscription: TObserver;
    function GetTopElevSubscription: TObserver;
    function GetBottomElevSubscription: TObserver;
    procedure SetIFACE(const Value: TIface);
    procedure SetModpathParticles(const Value: TParticleStorage);
    function StoreImportedHigherSectionElevations: Boolean;
    function StoreImportedLowerSectionElevations: Boolean;
    function StoreImportedSectionElevations: Boolean;
    function StoreImportedValues: Boolean;
    function GetModflowStreamGage: TStreamGage;
    procedure SetModflowStreamGage(const Value: TStreamGage);
    function StoreModflowStreamGage: Boolean;
    procedure RemoveElevationSubscription(Sender: TObject; const AName: string);
    procedure RemoveHigherElevationSubscription(Sender: TObject;
      const AName: string);
    procedure RemoveLowerElevationSubscription(Sender: TObject;
      const AName: string);
    procedure RestoreElevationSubscription(Sender: TObject;
      const AName: string);
    procedure RestoreHigherElevationSubscription(Sender: TObject;
      const AName: string);
    procedure RestoreLowerElevationSubscription(Sender: TObject;
      const AName: string);
    procedure RemoveDataArraySubscription(Sender: TObject;
      const AName: string);
    procedure RestoreDataArraySubscription(Sender: TObject;
      const AName: string);
    function GetElevationFormula: string;
    function GetHigherElevationFormula: string;
    function GetLowerElevationFormula: string;
    procedure CreateOrRetrieveBoundaryFormulaObject(const Index: Integer;
      ADataSet: TDataArray; var FormulaObject: TFormulaObject);
    procedure RestoreBoundaryDataArraySubscription(Sender: TObject;
      const AName: string);
    procedure RemoveBoundaryDataArraySubscription(Sender: TObject;
      const AName: string);
    function GetModflowMnw2Boundary: TMnw2Boundary;
    procedure SetModflowMnw2Boundary(const Value: TMnw2Boundary);
    procedure CacheElevationArrays;
    property SubPolygonCount: integer read GetSubPolygonCount;
    property SubPolygons[Index: integer]: TSubPolygon read GetSubPolygon;
    procedure DeleteExtraSections;
    procedure CreatePhastFluxBoundary;
    procedure CreatePhastLeakyBoundary;
    procedure CreatePhastRiverBoundary;
    procedure CreatePhastSpecifiedHeadBoundary;
    procedure CreatePhastSpecifiedSolutionBoundary;
    procedure CreatePhastWellBoundary;
    procedure CreateElevationSubscription;
    procedure CreateTopElevationSubscription;
    procedure CreateBottomElevationSubscription;
    procedure CreateBoundaryDataSetFormulas;
    procedure CreateBoundaryDataSetSubscriptions;
    procedure CreateBoundaryDataSets;
    procedure Draw2ElevModflow(const Direction: TViewDirection;
      const Bitmap32: TBitmap32);
    procedure Draw2ElevPhast(const Direction: TViewDirection;
      const Bitmap32: TBitmap32);
    procedure SetValueStorageField(const Value: TValueArrayStorage;
      var StoredValues: TValueArrayStorage);
    procedure CreateValueArrayStorage(var StoredValues: TValueArrayStorage);
    procedure DrawModflowSideSegmentRightOutsideAbove(
      SidePoints: T2DRealPointArray; LocalDelegate: TModflowDelegate;
      ModflowGrid: TModflowGrid;
      LayerIndex: Integer; ColIndex: Integer; RowIndex: Integer;
      SegmentList: TList);
    procedure DrawModflowSideSegmentAboveOutsideRight(
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
      SidePoints: T2DRealPointArray; SegmentList: TList);
    procedure DrawModflowSideSegmentAboveOutsideLeft(
      SidePoints: T2DRealPointArray;
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      LocalDelegate: TModflowDelegate; SegmentList: TList);
    procedure DrawModflowSideSegmentLeftOutsideAbove(
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      LocalDelegate: TModflowDelegate; SidePoints: T2DRealPointArray;
      SegmentList: TList);
    procedure DrawModflowSideSegmentAboveRight(SidePoints: T2DRealPointArray;
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
      SegmentList: TList);
    procedure DrawModflowSideSegmentAboveLeft(
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      LocalDelegate: TModflowDelegate; SidePoints: T2DRealPointArray;
      SegmentList: TList);
    procedure DrawModflowSideSegmentBottomLeft(SidePoints: T2DRealPointArray;
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
      SegmentList: TList);
    procedure DrawModflowSideSegmentTopLeft(
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      LocalDelegate: TModflowDelegate; SidePoints: T2DRealPointArray;
      SegmentList: TList);
    procedure DrawModflowSideSegmentInsideTopRight(LayerIndex: Integer;
      ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
      SidePoints: T2DRealPointArray;
      RowIndex: Integer; ColIndex: Integer; SegmentList: TList);
    procedure DrawModflowSideSegmentInsideBottomRight(
      SidePoints: T2DRealPointArray;
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
      SegmentList: TList);
    procedure DrawModflowSideSegmentInsideBottomLeft(
      SidePoints: T2DRealPointArray;
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
      SegmentList: TList);
    procedure DrawModflowSideSegmentInsideTopLeft(ModflowGrid: TModflowGrid;
      LocalDelegate: TModflowDelegate; var SidePoints: T2DRealPointArray;
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      SegmentList: TList);
    procedure DrawModflowFrontSegmentRightOutsideAbove(
      FrontPoints: T2DRealPointArray;
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
      SegmentList: TList);
    procedure DrawModflowFrontSegmentAboveOutsideRight(
      LocalDelegate: TModflowDelegate;
      FrontPoints: T2DRealPointArray;
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      ModflowGrid: TModflowGrid; SegmentList: TList);
    procedure DrawModflowFrontSegmentAboveOutsideLeft(
      FrontPoints: T2DRealPointArray;
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      LocalDelegate: TModflowDelegate; SegmentList: TList);
    procedure DrawModflowFrontSegmentLeftOutsideAbove(
      FrontPoints: T2DRealPointArray;
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      LocalDelegate: TModflowDelegate; SegmentList: TList);
    procedure DrawModflowFrontSegmentAboveRight(
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
      FrontPoints: T2DRealPointArray; SegmentList: TList);
    procedure DrawModflowFrontSegmentAboveLeft(
      FrontPoints: T2DRealPointArray;
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      LocalDelegate: TModflowDelegate; SegmentList: TList);
    procedure DrawModflowFrontSegmentBottomLeft(ModflowGrid: TModflowGrid;
      LocalDelegate: TModflowDelegate; FrontPoints: T2DRealPointArray;
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      SegmentList: TList);
    procedure DrawModflowFrontSegmentTopLeft(
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      LocalDelegate: TModflowDelegate; FrontPoints: T2DRealPointArray;
      SegmentList: TList);
    procedure DrawModflowFrontSegmentInsideTopRight(
      FrontPoints: T2DRealPointArray;
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
      SegmentList: TList);
    procedure DrawModflowFrontSegmentInsideBottomRight(
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
      FrontPoints: T2DRealPointArray; SegmentList: TList);
    procedure DrawModflowFrontSegmentInsideBottomLeft(
      FrontPoints: T2DRealPointArray;
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
      SegmentList: TList);
    procedure DrawModflowFrontSegmentInsideTopLeft(
      var FrontPoints: T2DRealPointArray;
      RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
      ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
      SegmentList: TList);
    procedure DrawModflowTopSegmentRightOutsideAbove(
      RowIndex: Integer; ColIndex: Integer; SegmentList: TList;
      LayerIndex: Integer; ModflowGrid: TModflowGrid;
      LocalDelegate: TModflowDelegate);
    procedure DrawModflowTopSegmentAboveOutsideRight(
      RowIndex: Integer; ColIndex: Integer; SegmentList: TList;
      LayerIndex: Integer; ModflowGrid: TModflowGrid;
      LocalDelegate: TModflowDelegate);
    procedure DrawModflowTopSegmentAboveOutsideLeft(
      RowIndex: Integer; ColIndex: Integer; SegmentList: TList;
      LayerIndex: Integer; ModflowGrid: TModflowGrid;
      LocalDelegate: TModflowDelegate);
    procedure DrawModflowTopSegmentLeftOutsideAbove(
      RowIndex: Integer; ColIndex: Integer; SegmentList: TList;
      LayerIndex: Integer; ModflowGrid: TModflowGrid;
      LocalDelegate: TModflowDelegate);
    procedure DrawModflowTopSegmentAboveRight(
      RowIndex: Integer; ColIndex: Integer; SegmentList: TList;
      LayerIndex: Integer; ModflowGrid: TModflowGrid;
      LocalDelegate: TModflowDelegate);
    procedure DrawModflowTopSegmentAboveLeft(
      RowIndex: Integer; ColIndex: Integer; SegmentList: TList;
      LayerIndex: Integer; ModflowGrid: TModflowGrid;
      LocalDelegate: TModflowDelegate);
    procedure DrawModflowTopSegmentBottomLeft(
      RowIndex: Integer; ColIndex: Integer; SegmentList: TList;
      LayerIndex: Integer; ModflowGrid: TModflowGrid;
      LocalDelegate: TModflowDelegate);
    procedure DrawModflowTopSegmentTopLeft(
      RowIndex: Integer; ColIndex: Integer; SegmentList: TList;
      LayerIndex: Integer; ModflowGrid: TModflowGrid;
      LocalDelegate: TModflowDelegate);
    procedure DrawModflowTopSegmentInsideTopRight(ModflowGrid: TModflowGrid;
      LocalDelegate: TModflowDelegate; RowIndex: Integer; ColIndex: Integer;
      SegmentList: TList; LayerIndex: Integer);
    procedure DrawModflowTopSegmentInsideBottomRight(RowIndex: Integer;
      ColIndex: Integer; SegmentList: TList; LayerIndex: Integer;
      ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate);
    procedure DrawModflowTopSegmentInsideBottomLeft(SegmentList: TList;
      LayerIndex: Integer; ModflowGrid: TModflowGrid;
      LocalDelegate: TModflowDelegate; RowIndex: Integer; ColIndex: Integer);
    procedure DrawModflowTopSegmentInsideTopLeft(RowIndex: Integer;
      ColIndex: Integer; SegmentList: TList; LayerIndex: Integer;
      ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate);
    procedure DrawSegmentList(const Direction: TViewDirection;
      const Bitmap32: TBitmap32; SegmentList: TList);
    procedure UpdateHigherElevationSubscriptions(var NewFormula: string;
      OldFormula: string);
    procedure UpdateLowerElevationSubscriptions(var NewFormula: string;
      OldFormula: string);
    procedure UpdateElevationSubscriptions(var NewFormula: string; OldFormula: string);
    procedure CreateGpcPolygon;
    procedure EvaluateDataArrayExpression(const DataSet: TDataArray; var Expression: TExpression; const Compiler: TRbwParser);
    procedure DrawPointMarker(LineColor32: TColor32; FillColor32: TColor32;
      const Bitmap32: TBitmap32; IntPoint: TPoint; LineWidth: Single);
    procedure SetElevationFormulaParser;
    function GetElevationCompiler: TRbwParser;
    procedure SetHigherElevationFormulaParser;
    procedure SetLowerElevationFormulaParser;
    procedure CreateElevationFormulaObject;
    procedure CreateHigherElevationFormulaObject;
    procedure CreateLowerElevationFormulaObject;
    procedure CreateFormulaObjects;
    procedure SetFormulaParsers;
    procedure CreateOrRetrieveFormulaObject(const Index: Integer;
      ADataSet: TDataArray; var FormulaObject: TFormulaObject);
    procedure RemoveSubscriptionFromList(const AName: string; Sender: TObject;
      List: TList; SubscriptionList: TObjectList);
    procedure RestoreSubscriptionToList(List: TList; const AName: string;
      Sender: TObject; Subscriptions: TObjectList);
  protected
    {
     @name is true during @link(TScreenObjectItem.UpdateScreenObject).}
    FIsUpdating: boolean;
    procedure UpdateModel(Model: TComponent);
    procedure InvalidateModel;
    // @name indicates what sort of PHAST boundary (if any) is specified
    // by this @classname.
    function PhastBoundaryType: TBoundaryTypes;
    {
    @name assigns a value to a particular cell in DataSet.

      In @name:

      (1) If OtherData = nil
      or not @Link(TInterpValuesItem)(OtherData).Values.UsePHAST_Interpolation,
      @name evaluates Expression using Compiler and assigns
      the result to DataSet
      at the location (LayerIndex, RowIndex, ColIndex).  The variables
      listed in UsedVariables are updated before Expression is evaluated.
      OtherData can be used when overriding AssignCellValue to pass additional
      data to the procedure. DataSet.Annotation at location
      (LayerIndex, RowIndex, ColIndex) is assigned the value of Annotation

      (2) Otherwise,
      values are assigned to DataSet at the location
      LayerIndex, RowIndex, ColIndex using PHAST-style interpolation.
      (See @link(TPhastInterpolationValues).)
      Expression and Compiler are not used in this case and the variables
      listed in UsedVariables are not updated.

      OtherData is set in @Link(IsBoundaryTimeDataSetUsed).

    }
    procedure AssignCellValue(const UsedVariables: TStringList;
      const DataSet: TDataArray; LayerIndex, RowIndex, ColIndex: integer;
      const Compiler: TRbwParser; const Annotation: string;
      var Expression: TExpression; const OtherData: TObject);
    // @name initializes variables needed by the formula for the
    // @link(TDataArray) and then assigns values to the data set based
    // on that formula for intersected and enclosed cells or elements.
    // @name is only used when @link(ViewDirection)
    // = @link(TViewDirection vdFront).
    // See @link(AssignValuesToPhastDataSet).
    procedure AssignValuesToFrontPhastDataSet(const Grid: TCustomGrid;
      const DataSet: TDataArray; OtherData: TObject);
    // @name initializes variables needed by the formula for the
    // @link(TDataArray) and then assigns values to the data set based
    // on that formula for intersected and enclosed cells or elements.
    // @name is only used when @link(ViewDirection)
    // = @link(TViewDirection vdSide).
    // See @link(AssignValuesToPhastDataSet).
    procedure AssignValuesToSidePhastDataSet(const Grid: TCustomGrid;
      const DataSet: TDataArray; OtherData: TObject);
    // @name initializes variables needed by the formula for the
    // @link(TDataArray) and then assigns values to the data set based
    // on that formula for intersected and enclosed cells or elements.
    // @name is only used when @link(ViewDirection)
    // = @link(TViewDirection vdTop).
    // See @link(AssignValuesToPhastDataSet).
    procedure AssignValuesToTopPhastDataSet(const Grid: TCustomGrid;
      const DataSet: TDataArray; OtherData: TObject);
    // @name returns an integer that indicates what type of PHAST boundary
    // condition, if any, is specified by this @classname.
    function BoundaryType: integer;
    // @name returns @True if DataSet can be added to the @classname.
    function CanAddDataSet(const DataSet: TDataArray): boolean;
    {
     @name returns @true if

     @orderedlist(
     @item(DataSet is a @link(TSparseArrayPhastInterpolationDataSet),)

     @item(DataSet is a @link(TCustomPhastDataSet)
     and DataSet is specified by this @classname, or)

     @item(the value of DataSet is set by this @classname
     or if DataSet equals @link(TPhastModel.TopBoundaryType),
     @link(TPhastModel.FrontBoundaryType),
     @link(TPhastModel.SideBoundaryType),
     or @link(TPhastModel.Top2DBoundaryType).)
     )

     OtherData may be changed in @name.
    }

    function DataSetUsed(const DataSet: TDataArray; var OtherData: TObject):
      boolean;
    // @name provides a way of accessing @link(Points) using @link(TEdgePoint)s
    // instead of  TPoint2Ds.
    property EdgePoints[const Index: integer]: TEdgePoint read GetEdgePoints
      write SetEdgePoints;
    // @name returns a string that indicates that the value of a cell or element
    // was specified by being enclosed in this @classname and how the value
    // at that location was determined.
    function EncloseAnnotation(const DataSetFormula: string;
      const OtherData: TObject): string;
    // @name gets a TRbwParser consistent with Orientation and
    // @link(EvaluatedAt).
    function GetCompiler(const Orientation: TDataSetOrientation): TRbwParser;
      overload;
    function GetCompiler(const Orientation: TDataSetOrientation;
      const EvaluatedAt: TEvaluatedAt): TRbwParser; overload;
    function Get1DCompiler: TRbwParser;
    // @name sets RotatedPoints to be an array of
    // TPoint2Ds in the coordinate system of the grid.
    procedure GetRotatedPoints(out RotatedPoints: TRealPointArray);
    // @name contains a set of higher elevations for points that
    // could be in the @classname.  It is indexed by [Layer, Row, Column].
    property Higher3DElevations: T3DSparseRealArray read GetHigher3DElevations;
    function IsHigher3DElevationAssigned(Col, Row, Layer: integer): boolean;

    {@name
     @orderedlist(
     @item(gets the proper DataSetFormula to apply to DataSet,)
     @item(gets the proper TRbwParser for DataSet, and)
     @item(compiles DataSetFormula to get Expression.)
     )
     However, it doesn't need to do
     any of that is PHAST-style interpolation is used.
     See @link(TPhastInterpolationValues).

     @param(Compiler is the correct TRbwParser to use with DataSet.)
     @param(DataSetFormula is the formula that will be used to assign
     values to DataSet.)
     @param(Expression is the TExpression that will be used to assign
     values to DataSet.)
     @param(DataSet is the @link(TDataArray) to which values will be assigned.)
     @param(OtherData can be used to pass other data to @name.
       It is an TInterpValuesItem)}
    procedure InitializeExpression(out Compiler: TRbwParser;
      out DataSetFormula: string; out Expression: TExpression;
      const DataSet: TDataArray; const OtherData: TObject);
    // @name fills UsedVariables with the names of the variables
    // use by Expression.  It initializes the @link(TDataArray)s associated
    // with those variables.
    procedure InitializeVariables(const UsedVariables: TStringList;
      const DataSet: TDataArray; const Expression: TExpression;
      const Compiler: TRbwParser); virtual;
    // @name provides a string that can be used in annotations to indicate
    // that the value of a cell or element has been set by virtue of being
    // intersected by a @classname and how the value
    // at that location was determined.
    function IntersectAnnotation(const DataSetFormula: string;
      const OtherData: TObject): string;
    // Intersection is used when determining whether adding a point to a
    // @classname will cause it to intersect itself.
    // Point1 and Point2 define a line segment.
    // IntersectPoint is the point of intersection (if one exists) between
    // the last section of @classname and the line segment.
    // @name returns True if the line segment intersects the @classname.
    function Intersection(const Point1, Point2: TPoint2D; StartIndex: Integer;
      out IntersectPoint: TPoint2D): TIntersectResult;
    // @name returns @true if DataSet is specified by a boundary condition and,
    // if so, sets OtherData to the relevant
    // @link(TCustomPhastBoundaryCondition).
    // if @name returns @false, OtherData is set to nil.
    function IsBoundaryTimeDataSetUsed(const DataSet: TDataArray;
      out OtherData: TObject): boolean;
    procedure Loaded; override;
    // @name is an array of elevations for the @classname.
    // If @link(ElevationCount) = ecTwo, @name is set using
    // @link(LowerElevationFormula).
    // If @link(ElevationCount) = ecOne or ecZero, @name is not used.
//    property LowerElevations: T2DSparseRealArray read GetLowerElevations;
    // @name contains a set of lower elevations for points that
    // could be in the @classname.  It is indexed by [Layer, Row, Column].
    property Lower3DElevations: T3DSparseRealArray read GetLower3DElevations;
    function IsLower3DElevationAssigned(Col, Row, Layer: integer): boolean;
    // The purpose of @name is to get First and Last.  They are
    // the indices of the first and last layer, row, or column
    // perpendicular to the plain of @link(TViewDirection) that are
    // enclosed or intersected by the @classname.
    //
    // In commented-out code, First and Last and changed to
    // frmGoPhast.PhastGrid.@link(TCustomGrid.LayerCount) for river
    // data sets.  It has been commented-out because it
    // messes up the display of the river
    // data on the status bar.
    //
    // It would be good to find a way around
    // this problem.
    procedure OtherIndex(const LayerOrRow, RowOrColumn: integer;
      out First, Last: integer; const DataSet: TDataArray);
    // @name sets RotatedPoints to be Points rotated to the grid coordinate
    // system. TempMinX, TempMinY, TempMaxX, TempMaxY are set to the minimum
    // and maximum X and Y coordinates of any of the points in the @classname
    //  in the Grid coordinate system.
    procedure RotatePoints(const Grid: TCustomGrid;
      out RotatedPoints: TEdgePointArray;
      out TempMinX, TempMinY, TempMaxX, TempMaxY: double);
    // See @link(ElevationFormula).
    // @name makes changes in the @link(TObserver)s as required.
    procedure SetElevationFormula(NewFormula: string); virtual;
    // @name sets @link(TObserver.UpToDate) to true as well as
    // @link(FElevSubscription), @link(FTopElevSubscription), and
    // @link(FBottomElevSubscription).
    procedure SetGeometryUpToDate; virtual;
    // See @link(HigherElevationFormula).
    // @name makes changes in the @link(TObserver)s as required.
    procedure SetHigherElevationFormula(NewFormula: string); virtual;
    // See @link(LowerElevationFormula).
    // @name makes changes in the @link(TObserver)s as required.
    procedure SetLowerElevationFormula(NewFormula: string); virtual;
    // See @link(SubscriptionUnit.TObserver.Name).
    procedure SetName(const Value: TComponentName); override;
    // @name calls inherited.  Then if Value is @True,
    // @unorderedList(
    // @item(@name sets all the TObserver.@link(TObserver.UpToDate)
    //   to @true for all @link(TObserver)s in @link(FDataSetSubscriptions).)
    // @item(@name sets all the TObserver.@link(TObserver.UpToDate)
    //   to @true for all @link(TObserver)s in
    //   @link(FDataSetMixtureSubscriptions).))
    procedure SetUpToDate(const Value: boolean); override;
    // See @link(ViewDirection).
    procedure SetViewDirection(const Value: TViewDirection); virtual;
    { @name gets the variables names from UsedVariables and gets the
      corresponding TCustomValues from Compiler.  It sets each TCustomValue
      to have the value of its corresponding data set at the location specified
      by Layer, Row, Column.  However, if the corresponding @link(TDataArray),
      is a 2D data set, either Layer, Row, or Column is changed to zero.}
    procedure UpdateVariables(const UsedVariables: TStringList;
      const DataSet: TDataArray; Layer, Row, Column: integer;
      const Compiler: TRbwParser);
    // @name is used to access the formula for the Mixture specified by
    // Index when PHAST style interpolation is used with a mixture.
    // See @link(TPhastInterpolationValues).
    property MixtureDataSetFormula[const Index: integer]: string
      read GetMixtureDataSetFormula write SetMixtureDataSetFormula;
    // @name calls TCustomPhastBoundary.@link(
    // TCustomPhastBoundary.UpdateMixtureExpression)
    // for @link(FluxBoundary), @link(LeakyBoundary),
    // @link(RiverBoundary), @link(SpecifiedHeadBoundary),
    // and @link(WellBoundary).
    procedure UpdateMixtureExpression;
    procedure UpdateFormulaExpression;
    property ModflowBoundaries: TModflowBoundaries read GetModflowBoundaries;
  public
    procedure GetCellsToAssign(const Grid: TCustomGrid;
      const DataSetFunction: string; OtherData: TObject;
      const DataSet: TDataArray; CellList: TCellAssignmentList;
      AssignmentLocation: TAssignmentLocation);
  public
    procedure CacheSegments;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure GetModpathCellList(CellList: TCellAssignmentList);
    property ElevSubscription: TObserver read GetElevSubscription;
    property TopElevSubscription: TObserver read GetTopElevSubscription;
    property BottomElevSubscription: TObserver read GetBottomElevSubscription;

    function Delegate: TCustomScreenObjectDelegate;
    procedure AssignNumericValueToDataSet(const Grid: TCustomGrid;
      const DataSet: TDataArray; Value: double);
    procedure CreateChdBoundary;
    procedure CreateDrnBoundary;
    procedure CreateDrtBoundary;
    procedure CreateEtsBoundary;
    procedure CreateEvtBoundary;
    procedure CreateGhbBoundary;
    procedure CreateLakBoundary;
    procedure CreateRchBoundary;
    procedure CreateResBoundary;
    procedure CreateRivBoundary;
    procedure CreateSfrBoundary;
    procedure CreateWelBoundary;
    procedure CreateUzfBoundary;
    procedure CreateHfbBoundary;
    procedure CreateHeadObservations;
    procedure CreateGagBoundary;
    procedure CreateMnw2Boundary;
    function ModflowDataSetUsed(DataArray: TDataArray): boolean;
    property SectionCount: integer read GetSectionCount;
    property SectionStart[const Index: integer]: integer read GetSectionStart;
    property SectionEnd[const Index: integer]: integer read GetSectionEnd;
    property SectionClosed[const Index: integer]: boolean read GetSectionClosed;
    property SectionLength[const Index: integer]: integer read GetSectionLength;
    procedure InvalidateSegments;
    procedure MovePointsWhenCreatingScreenObjectByDeletingEdge(
      DeletedEdge: integer; ExistingObject: TScreenObject);
    function AllSectionsClosed: boolean;
    {
      @name indicates whether or not @link(SelectedVertexCount) is
      up-to-date.  If it isn't, it will be recalculated when needed.}
    property NeedToResetSelectedVertexCount: boolean
      read FNeedToResetSelectedVertexCount
      write FNeedToResetSelectedVertexCount;
    Procedure DeleteEdge(EdgeToDelete: integer);
    // @name calls TCustomPhastBoundaryCondition.@link(
    // TCustomPhastBoundaryCondition.ResetMixtureSubscription)
    // for each solution boundary condition.
    procedure ResetBoundaryMixtureSubscriptions;
    // @name sets UpToDate to @true for all members of
    // @link(FDataSetMixtureSubscriptions)
    procedure ResetMixtureSubscriptions;
    procedure MovePoints(var Dest: TRealPointArray);
    procedure MoveToPoints(const SourcePoints: TRealPointArray);
    procedure MoveSelectedPoints(var Dest: TBooleanDynArray);
    procedure MoveToSelectedPoints(var Source: TBooleanDynArray);
    property BottomElevation: real read FBottomElevation;
    property TopElevation: real read FTopElevation;
    // @Link(BoundaryDataSets) are data sets associated with a boundary
    // condition whose values are set by the object.
    // An example is the hydraulic conductivity of a leaky boundary.
    // @name adds a boundary data set to @Link(BoundaryDataSets).
    // The position of the boundary data sets in @Link(BoundaryDataSets) is
    // the result of the function.  This result may then be used to set a
    // formula for the data set in @Link(BoundaryDataSetFormulas).
    // @SeeAlso(AddDataSet) @SeeAlso(DeleteBoundaryDataSet)
    function AddBoundaryDataSet(const DataSet: TDataArray): integer;
    // @Link(DataSets) are data sets whose values are set by the object.
    // An Example is Kx.
    // @name adds a data set to @Link(DataSets).
    // The position of the data sets in @Link(DataSets) is
    // the result of the function.  This result may then be used to set a
    // formula for the data set in @Link(DataSetFormulas).
    //
    // @name also does what it takes to ensure that
    // DataSet will be notified when there is a change to this
    // @classname.
    // @SeeAlso(AddBoundaryDataSet) @SeeAlso(DeleteDataSet)
    function AddDataSet(const DataSet: TDataArray): Integer; virtual;
    // @name adds a point to @Link(Points).  If CheckPoint is true,
    // each point is checked for
    // validity when added.  A point is invalid if it causes the @classname to
    // cross itself.
    procedure AddPoint(const APoint: TPoint2D; NewPart: boolean);
    // In @name, if Source is a @classname, its properties are copied into
    // the current @classname.
    procedure Assign(Source: TPersistent); override;
    procedure AssignValuesToModflowDataSet(const Grid: TCustomGrid;
      const DataSet: TDataArray; const Formula: string;
      AssignmentLocation: TAssignmentLocation = alAll);
    // @name is a key method of @classname.  It is used
    // to assign values to a data set based on the function for that data
    // set. @name first checks whether it should set the values
    // DataSet and if so, does.  It calls one of the protected
    // procedures  @Link(AssignValuesToTopPhastDataSet),
    // @Link(AssignValuesToFrontPhastDataSet),
    // or @Link(AssignValuesToSidePhastDataSet)
    // to do most of the work.
    procedure AssignValuesToPhastDataSet(const Grid: TCustomGrid; const DataSet:
      TDataArray); virtual;
    // @name is the number of boundary @link(BoundaryDataSets) affected
    // by the @classname.
    function BoundaryDataSetCount: integer;
    // @name are data sets associated with a boundary
    // condition whose values are set by the object.
    // An example is the hydraulic conductivity of a leaky boundary.
    // @SeeAlso(AddBoundaryDataSet) @SeeAlso(BoundaryDataSetFormulas)
    // @SeeAlso(IndexOfBoundaryDataSet) @SeeAlso(InsertBoundaryDataSet)
    // @SeeAlso(RemoveDataSet)
    property BoundaryDataSets[const Index: integer]: TDataArray
      read GetBoundaryDataSets write SetBoundaryDataSets;
    // @name are the formulas associated with each
    // member of @Link(BoundaryDataSets).
    property BoundaryDataSetFormulas[const Index: integer]: string
      read GetBoundaryDataSetFormulas write SetBoundaryDataSetFormulas;
    // @name returns @True if this @classname
    // affects a boundary condition.
    function BoundaryTypeUsed: TBoundaryTypes;
    property CanInvalidateModel: boolean read FCanInvalidateModel;
    // @name is an array of TPoints that represent the object.
    // These coordinates are recalculate as needed when the magnification or
    // view of the model changes.  For example, panning causes them to be
    // recalculated.
    property CanvasCoordinates: TPointArray read GetCanvasCoordinates;
    // @name is the number of points that the @classname can hold without
    // resizing the array that holds them.
    property Capacity: integer read FCapacity write SetCapacity;
    // @name is an event handler for TObserver.OnUpToDateSet for
    // @Link(FElevSubscription), @Link(FTopElevSubscription), and
    // @Link(FBottomElevSubscription). If Sender
    // ( = @link(SubscriptionUnit.TObserver)) isn't up to date, the
    // @classname is invalidated.  (See @link(Invalidate).)
    procedure Changed(Sender: TObject);
    // @name removes all the members from
    // @Link(BoundaryDataSets).
    procedure ClearBoundaryDataSets; virtual;
    // @name clears @link(InterpValues).
    // @name also unsubscribes to everything in
    // @link(FDataSetMixtureSubscriptions) and clears
    // @link(FDataSetMixtureSubscriptions).
    // @name removes all the members from
    // @Link(DataSets).
    // This involves unsubscribing to all the subscriptions related to
    // all members of @Link(DataSets)
    procedure ClearDataSets; virtual;
    // @name removes all the members from @Link(Points).
    procedure ClearPoints;
    // If any individual vertices in an object are selected, @name
    // unselects them.
    procedure ClearSelectedVertices;
    // @name returns true if an object is closed.
    function Closed: boolean;
    // @name copies TPoint2Ds from Points into Destination.
    // Position is the location in Destination where the first of the
    // Points will be copied.  Start is the location of the first point
    // in Points to be copied.  Count is the number of points to be copied.
    // However, the number of points actually copied may be less than Count
    // if there are not enough points in Count
    procedure CopyPoints(var Destination: TRealPointArray;
      const Position, Start: integer; var Count: integer);
    // @name is the number of vertices in an object.  Normally setting Count
    // to a value larger than the current value means that new vertices
    // will be written into the FPoints array by some mechanism other than
    // the @Link(AddPoint) method.  In such cases, it is important to make
    // sure that the added vertices are valid.
    property Count: integer read GetCount write SetCount;
    // @name creates a @classname.
    constructor Create(AnOwner: TComponent); override;
    // @name creates a @classname, assigns it a name and if
    // UndoAble is true, creates a TUndoCreateScreenObject that may be used to
    // undo or redo creation of the @classname.
    constructor CreateWithViewDirection(const Model: TComponent;
      const AViewDirection: TViewDirection;
      out UndoCreateScreenObject: TCustomUndo; const UndoAble: boolean = True);
      virtual;
    // @name is the number of @link(TDataArray TDataArrays)
    // (and their associated
    // formulas) that can be held without increasing the size of the TList
    // that holds them.  See @Link(DataSets).
    property DataSetCapacity: integer read GetDataSetCapacity
      write SetDataSetCapacity;
    // @name is the number of @link(TDataArray TDataArrays)
    // (and their associated
    // formulas) whose properties will be set by the @classname.
    // See @Link(DataSets).
    property DataSetCount: integer read GetDataSetCount;
    // @name accesses the @link(TDataArray TDataArrays)
    // whose values will be set by the
    // @classname.  The number of members in DataSets is @Link(DataSetCount).
    // The number of @link(TDataArray TDataArrays)
    // that can be held in DataSets is
    // @Link(DataSetCapacity).
    // @SeeAlso(AddDataSet) @SeeAlso(DataSetFormulas)
    // @SeeAlso(IndexOfDataSet) @SeeAlso(RemoveDataSet)
    property DataSets[const Index: integer]: TDataArray read GetDataSets
      write SetDataSets;

    { TODO : 
Consider converting formulas into objects and 
having them take care of the subscriptions. }

    // @name contains the formulas used to set the values of
    // the corresponding member of @Link(DataSets).
    property DataSetFormulas[const Index: integer]: string
      read GetDataSetFormulas write SetDataSetFormulas;
    // @name removes a @link(TDataArray) from @Link(BoundaryDataSets)
    // and the corresponding formula from @Link(BoundaryDataSetFormulas).
    // @SeeAlso(AddBoundaryDataSet) @SeeAlso(DeleteDataSet)
    procedure DeleteBoundaryDataSet(const Index: Integer);
    // @name indicates whether or not the user has deleted the @classname.
    // Deleting a screen object does
    // not cause it to be destroyed.  It is merely
    // put in a state where it is not displayed and does not affect the values
    // of any @link(DataSetUnit.TDataArray).
    // This is accomplished by exiting immediately from
    // @Link(Draw) and @Link(AssignValuesToPhastDataSet) if Deleted is true.
    property Deleted: boolean read FDeleted write SetDeleted;
    // @abstract(@name deletes the @link(TDataArray) specified by Index from
    // the list of @link(TDataArray)s affected by this @classname.)
    // @name removes a @link(DataSetUnit.TDataArray)
    // from @Link(DataSets) and the
    // corresponding formula from @Link(DataSetFormulas).
    // @SeeAlso(AddDataSet) @SeeAlso(DeleteBoundaryDataSet)
    procedure DeleteDataSet(const Index: Integer); virtual;
    // @name removes the point at Index from @Link(Points).
    // It does not check that removing that point will still keep the Points
    // all valid.  For example, it does
    // not check that the screen object does not
    // cross itself after deleting a point.
    procedure DeletePoint(const Index: Integer);
    // @name destroys the @classname.  @name should not be called directly.
    // Call Free instead.
    destructor Destroy; override;
    // @name returns the distance from Location to the closest
    // point on the @classname.
    // ClosestLocation will be set to that location.
    // Anisotropy is taken into account when computing the distance.
    function DistanceToScreenObject(const Location: TPoint2D;
      out ClosestLocation: TPoint2D; const Anisotropy: real): real;
    // @name is the primary routine for drawing a @classname.  Bitmap32 is the
    // TBitmap32 on which the @link(TScreenObject) should be drawn.  Direction
    // is the direction from which the @classname will be viewed.
    // DrawAsSelected indicates whether the screen object should be drawn as a
    // selected @classname. (See @Link(DrawSelected).)
    procedure Draw(Const Bitmap32: TBitmap32;
      const Direction: TViewDirection;
      const DrawAsSelected: Boolean = False); virtual;
    procedure Draw3D;
    // If a @classname is @Link(Selected), @name calls @Link(Draw) with
    // DrawAsSelected = True.  DrawSelected is used for drawing just the
    // selected @classname so that the entire view of the model doesn't need
    // to be redrawn when just the selected screen objects have changed.
    procedure DrawSelected(
      const BitMap32: TBitmap32;
      const Direction: TViewDirection);
    // @name returns the @link(TCustomPhastBoundaryCollection)
    // one of whose @link(TPhastTimeList)s is TimeList.
    function GetBoundaryCollection(const TimeList: TPhastTimeList):
      TCustomPhastBoundaryCollection;
    // @name returns the position of DataSet in
    // @Link(BoundaryDataSets).
    function IndexOfBoundaryDataSet(const DataSet: TDataArray): integer;
    // IndexOfDataSet returns the position of DataSet in
    // @Link(DataSets).
    function IndexOfDataSet(const DataSet: TDataArray): integer;
    // @name returns the first position of APoint in Points.
    // An exact match is required.
    function IndexOfPoint(const APoint: TPoint2D): integer;
    // @name inserts DataSet in @Link(BoundaryDataSets) at
    // Index if it is not already in @Link(BoundaryDataSets).
    procedure InsertBoundaryDataSet(const Index: Integer;
      const DataSet: TDataArray); virtual;
    // InsertDataSet inserts DataSet in @Link(DataSets) at
    // Index if it is not already in @Link(DataSets).
    procedure InsertDataSet(const Index: Integer;
      const DataSet: TDataArray); virtual;
    // @name inserts APoint in @Link(Points) at Index. It does not
    // check that the point that is inserted will leave the @classname in
    // a valid state.  For instance, the @classname might cross itself after
    // @name.
    procedure InsertPoint(const Index: Integer; const APoint: TPoint2D);
    // @name is called when the @classname has been changed.  Everything
    // that depends on the @classname gets notified of the change
    procedure Invalidate;
    // @name is called when the @Link(CanvasCoordinates) need
    // to be recalculated.  The recalculation does not occur until the
    // next time the @Link(CanvasCoordinates) are needed.
    procedure InvalidateCoordinates;
    // @name returns true if (X, Y) is inside the @classname.
    function IsPointInside(const X, Y: real; out SectionIndex: integer):
      boolean; overload;
    // @name returns true if APoint is inside the @classname.
    function IsPointInside(const APoint: TPoint2D; out SectionIndex: integer):
      boolean; overload;
    // @name is the largest X coordinate of any of the points in @Link(Points).
    property MaxX: real read GetMaxX;
    // @name is the largest Y coordinate of any of the points in @Link(Points).
    property MaxY: real read GetMaxY;
    // @name returns the methods that the screen object uses to set the
    // properties of @link(TDataArray)s.
    function Methods: string;
    // @name is the smallest X coordinate of any of the points in @Link(Points).
    property MinX: real read GetMinX;
    // @name is the smallest Y coordinate of any of the points in @Link(Points).
    property MinY: real read GetMinY;
    // @name is nil or the @link(TPhastModel) that owns this @classname.
    Property Model: TComponent read FModel;
    // @name returns the 2D area of intersection between an
    // object and an individual cell or element.
    function ObjectIntersectArea(const Col, Row, Layer: integer): real;
    // @name returns the 2D length of intersection between an
    // object and an individual cell or element.
    function ObjectIntersectLength(const Col, Row, Layer: integer): real;
    function ObjectSectionIntersectLength(const Col, Row, Layer, Section: integer): real;
    // @name gives the real-world coordinates of the vertices of the
    // @classname.
    // For a @classname whose @Link(ViewDirection)
    // = @link(TViewDirection vdTop),
    // @name gives the X and Y coordinates of the @classname.
    // For a @classname whose @Link(ViewDirection) =
    // @link(TViewDirection vdFront),
    // @name gives the X and Z coordinates of the @classname (with the Y of
    // each TPoint2D representing the Z coordinate).
    // For a @classname whose @Link(ViewDirection)
    // = @link(TViewDirection vdSide),
    // @name gives the Y and Z coordinates of the @classname (with the X of
    // each TPoint2D representing the Z coordinate).
    // @SeeAlso(SelectedVertices)
    property Points[const Index: integer]: TPoint2D read GetPoints
      write SetPoints;
    // @name removes DataSet from either @Link(DataSets) or
    // @Link(BoundaryDataSets).
    function RemoveDataSet(const DataSet: TDataArray): Integer; virtual;
    // @name sets the UpToDate property of
    // all subscriptions related to members of @Link(BoundaryDataSets) to @True;
    procedure ResetBoundaryDataSetSubscriptions;
    // @name sets the UpToDate property of
    // all subscriptions related to members of @Link(DataSets) to True;
    procedure ResetDataSetSubscriptions;
    {
     @name calls
     @unorderedlist(
     @item(@link(ResetBoundaryDataSetSubscriptions),)
     @item(@link(ResetDataSetSubscriptions),)
     @item(@link(ResetMixtureSubscriptions), and)
     @item(@link(ResetBoundaryMixtureSubscriptions)))
    }
    procedure ResetSubscriptions; 
    // @name is the area of the object.  The value is stored and only
    // recalculated when it is no longer up-to-date.
    function ScreenObjectArea: real;
    // @name is the 2D length of the object.  Its value is stored and
    // is only recalculated when it is no longer up-to-date.
    function ScreenObjectLength: real;
    // @name is a list of the cell or element segments of the object.
    // Each segment is the intersection of one segment of the object with an
    // individual cell or element in the grid.
    // @Link(EvaluatedAt) determines whether the segments in Segments is
    // a cell segment or an element segment.
    property Segments: TCellElementSegmentList read GetSegments;
    // If the mouse coordinates (X, Y) are inside the @classname, @name
    // returns True.  However, the @Link(Selected) property is not changed
    // by calling Select.
    function Select(const XScreenCoordinate, YScreenCoordinate: integer):
      boolean;
    // @name returns -1 if the mouse coordinates X,Y are not on any
    // edge of the @classname.  Otherwise it returns the index of the
    // edge on which the point X,Y lies.
    function SelectEdge(const X, Y: integer): integer;
    // @name tells how many points in @Link(Points) are selected.
    property SelectedVertexCount: integer read GetSelectedVertexCount;
    // For each member of @Link(Points), @name tells whether or not
    // that vertex is selected.
    property SelectedVertices[const Index: integer]: boolean
      read GetSelectedVertices write SetSelectedVertices;
    // @name is a representation of the @classname in canvas coordinates.
    // It is used when determining whether a "lasso" drawn by the user
    // intersects the @classname.
    property SelectLines: TLines read GetSelectLines;
    // @name sets the @link(SubscriptionUnit.TObserver.Name)
    // and @link(ViewDirection) of the @classname.
    // If UndoAble is true, it also creates a @link(TUndoCreateScreenObject).
    // It sets the elevation formulas to default values.
    procedure UpdateScreenObjectWithName(const AName: string;
      const AViewDirection: TViewDirection;
      out UndoCreateScreenObject: TCustomUndo; const UndoAble: boolean = True);
      reintroduce; virtual;
    // @name converts OriginalName to a name that is valid as a name for
    // a @classname. It does not ensure that the resulting name is unique.
    class function ValidName(const OriginalName: string): string;
    // @name tests whether any point in the @classname is closer to
    // Location than Distance.  If so, ClosestLocation and Distance
    // are updated to reflect the values for the @classname.
    // Distance and ClosestLocation are changed if and only
    // if @name returns @True.
    function IsAnyPointCloser(const Location: TPoint2D;
      var Distance: real; out ClosestLocation: TPoint2D;
      const Anisotropy: real): boolean;
    function GetMfBoundary(ParamType: TParameterType): TModflowParamBoundary;
    // @name returns a MODFLOW cell where @classname is located.
    // The cell numbers in the @link(TCellLocation) will be 1 based.
    // If the @link(TCellLocation.Layer TCellLocation.Layer) = 0,
    // the @name does not intersect the MODFLOW grid.
    function SingleCellLocation: TCellLocation;
    procedure UpdateModflowTimes(ModflowTimes: TRealList);
    property CurrentValues: TValueArrayStorage read FCurrentValues;
    procedure UpdateImportedValues(DataArray: TDataArray);
    procedure ReverseDirection;
    function RestoreCellsFromCache(CellList: TCellAssignmentList;
      EvalAt: TEvaluatedAt; Orientation: TDataSetOrientation;
      AssignmentLocation: TAssignmentLocation; const EncloseAnnotation,
      IntersectAnnotation: string): boolean;
    procedure UpdateCellCache(CellList: TCellAssignmentList;
      EvalAt: TEvaluatedAt; Orientation: TDataSetOrientation;
      AssignmentLocation: TAssignmentLocation);
  published
    // If @Link(CellSizeUsed) is true, @name is the size of the cells
    // to be created in the vicinity of the object.
    property CellSize: real read FCellSize write SetCellSize;
    // During automatic generation of a grid, any objects for which
    // @name is true will be used to set the cell size of the grid.
    property CellSizeUsed: boolean read FCellSizeUsed write SetCellSizeUsed;
    // @name determines whether or not the line outlining the object
    // will be drawn in color.  See @Link(LineColor).
    property ColorLine: boolean read FColorLine write SetColorLine;
    // @name indicates the number of associated elevations of the @classname.
    // See @link(TElevationCount).
    property ElevationCount: TElevationCount read FElevationCount
      write SetElevationCount;
    // If @Link(ElevationCount) = ecOne, @name is the
    // formula used to define the position of the object in the dimension
    // perpendicular to the plane of the @Link(ViewDirection).
    property ElevationFormula: string read GetElevationFormula write
      SetElevationFormula;
    // @name is only used for backwards compatibility.
    // See @link(ElevationFormula).
    property ElevationFunction: string read GetElevationFormula write
      SetElevationFormula stored False;
    // @name determines whether a data set is evaluated at nodes or
    // cell centers.  This also controls which data sets it can set the
    // values of. See @link(TEvaluatedAt).
    property EvaluatedAt: TEvaluatedAt read FEvaluatedAt write SetEvaluatedAt;
    // @name is the color used to draw the interior of objects.  It
    // only has an effect for closed objects.
    //@Seealso(FillScreenObject)
    //@Seealso(LineColor)
    property FillColor: TColor read GetFillColor write SetFillColor;
    // @exclude
    // @name is used for backwards compatibility only.
    property FillContour: boolean write SetFillScreenObject stored False;
    // @name determines whether the interior of an object is drawn in
    // color.  It only has an effect for closed objects.  See @Link(FillColor).
    property FillScreenObject: boolean read FFillScreenObject write
      SetFillScreenObject;
    // If @Link(ElevationCount) = ecTwo, @name is the
    // formula used to define the higher
    // position of the object in the dimension
    // perpendicular to the plane of the @Link(ViewDirection).
    property HigherElevationFormula: string read GetHigherElevationFormula
      write SetHigherElevationFormula;
    // @name is only used for backwards compatibility.
    // See @link(HigherElevationFormula).
    property HigherElevationFunction: string read GetHigherElevationFormula
      write SetHigherElevationFormula stored False;
    // @name is the color used to draw the line defining the object.
    // @Seealso(ColorLine)
    // @Seealso(FillColor)
    property LineColor: TColor read GetLineColor write SetLineColor;
    // If @Link(ElevationCount) = ecTwo, @name is the
    // formula used to define the lower position of the object in the dimension
    // perpendicular to the plane of the @Link(ViewDirection).
    property LowerElevationFormula: string read GetLowerElevationFormula
      write SetLowerElevationFormula;
    // @name is only used for backwards compatibility.
    // See @link(LowerElevationFormula).
    property LowerElevationFunction: string read GetLowerElevationFormula
      write SetLowerElevationFormula stored False;
    // @name indicates whether the object is currently selected or not.
    property Selected: boolean read FSelected write SetSelected;

    // If @name is true, the formula of the object for a
    // particular data set will set the values of elements or nodes
    // in the data according to the algorithm defined by the interpolator for
    // the data set.
    property SetValuesByInterpolation: boolean
      read FSetValuesByInterpolation write Set_SetValuesByInterpolation;

    // @exclude
    // SetPropertiesByInterpolation is maintained only for backwards
    // compatiblilty
    property SetPropertiesByInterpolation: boolean
      read FSetValuesByInterpolation write Set_SetValuesByInterpolation
      stored false;

    // If @name, the formula of the object for a
    // particular data set will set the values of all elements or nodes
    // in the data set that are located inside the object.
    // An object must be closed for this to have any effect.
    property SetValuesOfEnclosedCells: boolean
      read Get_SetValuesOfEnclosedCells write Set_SetValuesOfEnclosedCells;

    // @exclude
    // SetPropertiesOfEnclosedCells is maintained only for backwards
    // compatiblilty
    property SetPropertiesOfEnclosedCells: boolean
      read Get_SetValuesOfEnclosedCells write Set_SetValuesOfEnclosedCells
      stored false;

    // If @name is true, the formula of the object for a
    // particular data set will set the values of all elements or nodes
    // in the data set that are intersected by the object.
    property SetValuesOfIntersectedCells: boolean
      read FSetValuesOfIntersectedCells
      write Set_SetValuesOfIntersectedCells;

    // @exclude
    // SetPropertiesOfIntersectedCells is maintained only for backwards
    // compatiblilty
    property SetPropertiesOfIntersectedCells: boolean
      read FSetValuesOfIntersectedCells
      write Set_SetValuesOfIntersectedCells stored false;

    // @name indicates the direction from which the object is to be
    // viewed.
    // setting also sets the @link(TCustomOrientedPhastBoundary.Orientation)
    // of @link(FluxBoundary) and @link(LeakyBoundary).
    property ViewDirection: TViewDirection read FViewDirection write
      SetViewDirection;
    // @name indicates whether the @classname is visible to the user.
    property Visible: boolean read FVisible write SetVisible default True;
    // @name represents the flux boundary (if any) in this @classname.
    property FluxBoundary: TFluxBoundary read GetFluxBoundary write
      SetFluxBoundary stored StoreFlux;
    // @name represents the PHAST style interpolation parameters
    // for the data sets affected by this @classname.
    // See @link(TPhastInterpolationValues).
    property InterpValues: TInterpValuesCollection read FInterpValues write
      SetInterpValues;
    // @name represents the leaky boundary (if any) in this @classname.
    property LeakyBoundary: TLeakyBoundary read GetLeakyBoundary write
      SetLeakyBoundary stored StoreLeaky;
    // @name represents the river boundary (if any) in this @classname.
    property RiverBoundary: TRiverBoundary read GetRiverBoundary write
      SetRiverBoundary stored StoreRiver;
    // @name represents the specified head boundary (if any) in this @classname.
    property SpecifiedHeadBoundary: TSpecifiedHeadBoundary
      read GetSpecifiedHeadBoundary write SetSpecifiedHeadBoundary
      stored StoreSpecifiedHead;
    // @exclude
    // @name is retained only for backwards compatibility.
    property SpecifiedSolutionBoundary: TSpecifiedSolutionBoundary
      read GetSpecifiedSolutionBoundary write SetSpecifiedSolutionBoundary
      stored False;
    // @name represents the well boundary (if any) in this @classname.
    property WellBoundary: TWellBoundary read GetWellBoundary write
      SetWellBoundary stored StoreWell;
    // MODFLOW boundary conditions
    property ModflowChdBoundary: TChdBoundary read GetModflowChdBoundary
      write SetModflowChdBoundary stored StoreModflowChdBoundary;
    property ModflowGhbBoundary: TGhbBoundary read GetModflowGhbBoundary
      write SetModflowGhbBoundary stored StoreModflowGhbBoundary;
    property ModflowWellBoundary: TMfWellBoundary read GetModflowWellBoundary
      write SetModflowWellBoundary stored StoreModflowWellBoundary;
    property ModflowRivBoundary: TRivBoundary read GetModflowRivBoundary
      write SetModflowRivBoundary stored StoreModflowRivBoundary;
    property ModflowDrnBoundary: TDrnBoundary read GetModflowDrnBoundary
      write SetModflowDrnBoundary stored StoreModflowDrnBoundary;
    property ModflowDrtBoundary: TDrtBoundary read GetModflowDrtBoundary
      write SetModflowDrtBoundary stored StoreModflowDrtBoundary;
    property ModflowRchBoundary: TRchBoundary read GetModflowRchBoundary
      write SetModflowRchBoundary stored StoreModflowRchBoundary;
    property ModflowEvtBoundary: TEvtBoundary read GetModflowEvtBoundary
      write SetModflowEvtBoundary stored StoreModflowEvtBoundary;
    property ModflowEtsBoundary: TEtsBoundary read GetModflowEtsBoundary
      write SetModflowEtsBoundary stored StoreModflowEtsBoundary;
    property ModflowResBoundary: TResBoundary read GetModflowResBoundary
      write SetModflowResBoundary stored StoreModflowResBoundary;
    property ModflowLakBoundary: TLakBoundary read GetModflowLakBoundary
      write SetModflowLakBoundary stored StoreModflowLakBoundary;
    property ModflowSfrBoundary: TSfrBoundary read GetModflowSfrBoundary
      write SetModflowSfrBoundary stored StoreModflowSfrBoundary;
    property ModflowUzfBoundary: TUzfBoundary read GetModflowUzfBoundary
      write SetModflowUzfBoundary stored StoreModflowUzfBoundary;
    property ModflowHeadObservations: THobBoundary
      read GetModflowHeadObservations write SetModflowHeadObservations
      stored StoreModflowHeadObservations;
    property ModflowStreamGage: TStreamGage read GetModflowStreamGage
      write SetModflowStreamGage stored StoreModflowStreamGage;
    // @name is used only for backwards compatibility with versions of
    // ModelMuse predating the initial release of ModelMuse.
    property ModflowHfbBoundary: THfbBoundary read GetModflowHfbBoundary
      write SetModflowHfbBoundary;
    property ModflowMnw2Boundary: TMnw2Boundary read GetModflowMnw2Boundary
      write SetModflowMnw2Boundary;
    { TODO :
Consider making SectionStarts private and only exposing SectionStart,
SectionEnd etc. DefineProperties could be used to store and retrieve
SectionStarts.}

    // If a @classname has N parts, @name will have N-1 integer values.
    // Each i represents the index of a TPoint2D in
    // @link(Points). The starting points of each part of the @classname
    // except the first part will be represented in @name. (The starting point
    // of the first part is implicitly 0.
    // The values in @name must be sorted in ascending order.
    // before they are used.
    property SectionStarts: TValueArrayStorage read GetSectionStarts
      write SetSectionStarts;
    property ImportedSectionElevations: TValueArrayStorage
      read GetImportedSectionElevations write SetImportedSectionElevations
      stored StoreImportedSectionElevations;
    property ImportedHigherSectionElevations: TValueArrayStorage
      read GetImportedHigherSectionElevations
      write SetImportedHigherSectionElevations
      stored StoreImportedHigherSectionElevations;
    property ImportedLowerSectionElevations: TValueArrayStorage
      read GetImportedLowerSectionElevations
      write SetImportedLowerSectionElevations
      stored StoreImportedLowerSectionElevations;
    property ImportedValues: TValueCollection read FImportedValues
      write SetImportedValues stored StoreImportedValues;
    property IFACE: TIface read FIFACE write SetIFACE default iInternal;
    property ModpathParticles: TParticleStorage read FModpathParticles write SetModpathParticles;
  end;

  TScreenObjectList = class(TObject)
  private
    FList: TList;
    function GetCount: integer;
    function GetItems(Index: integer): TScreenObject;
    function GetCapacity: integer;
    procedure SetCapacity(const Value: integer);
  public
    function  Add(ScreenObject: TScreenObject): integer;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount;
    property Items[Index: integer]: TScreenObject read GetItems; default;
    constructor Create;
    destructor Destroy; override;
  end;

  // @abstract(@name provides functionality used in storing multiple values
  // associated with a single screen object (a @Link(TMultiValueScreenObject));
  // It is used as an ancestor of @link(TRealDataListItem)
  // and @link(TIntegerDataListItem) which actually hold data.)
  TCustomDataListItem = class(TCollectionItem)
  private
    // See @link(DataSetName).
    FDataSetName: string;
    // See @link(FirstCol).
    FFirstCol: integer;
    // See @link(FirstLay).
    FFirstLay: integer;
    // See @link(FirstRow).
    FFirstRow: integer;
    // See @link(LastCol).
    FLastCol: integer;
    // See @link(LastLay).
    FLastLay: integer;
    // See @link(LastRow).
    FLastRow: integer;
  protected
    // @name retrieves the number of items stored in @classname.
    function GetValuesLength: integer; virtual; abstract;
    // @name sets the number of items stored in @classname.
    procedure SetValuesLength(const Value: integer); virtual; abstract;
  public
    // @name is the number of items stored in @classname.
    property Length: integer read GetValuesLength write SetValuesLength;
    // @name copies all the values from Source
    // (which must be a @classname)
    // to the object calling Assign
    procedure Assign(Source: TPersistent); override;
  published
    // @name is the name of the data set for which values are being
    // stored
    property DataSetName: string read FDataSetName write FDataSetName;
    // @name is the first column for which data is stored;
    property FirstCol: integer read FFirstCol write FFirstCol;
    // @name is the first layer for which data is stored;
    property FirstLay: integer read FFirstLay write FFirstLay;
    // @name is the first row for which data is stored;
    property FirstRow: integer read FFirstRow write FFirstRow;
    // @name is the last column for which data is stored;
    property LastCol: integer read FLastCol write FLastCol;
    // @name is the last layer for which data is stored;
    property LastLay: integer read FLastLay write FLastLay;
    // @name is the last row for which data is stored;
    property LastRow: integer read FLastRow write FLastRow;
    // @name is a function used to determine the position or a particular
    // data item within the array of data
    // items in descendants of TCustomDataListItem.
    function ValueIndex(const Col, Row, Lay: integer): integer;
  end;

  // @abstract(@name is a @link(TCustomDataListItem) specialized for
  // holding real numbers.)
  TRealDataListItem = class(TCustomDataListItem)
  private
    // @name: @link(GoPhastTypes.TOneDRealArray);
    // @name is the raw data stored by the @classname.  Normally,
    // these values are access through the @link(Values) property which
    // provides error checking.
    FValues: TOneDRealArray;
    // See @link(Values).
    function GetValues(const Index: Integer): double;
    // See @link(Values).
    procedure SetValues(const Index: Integer; const Value: double);
  protected
    // @name retrieves the number of items stored in @classname.
    function GetValuesLength: integer; Override;
    // @name is used to read the contents of @link(FValues) from a stream.
    procedure ReadValues(Reader: TReader);
    // @name sets the number of items stored in @classname.
    procedure SetValuesLength(const Value: integer); Override;
    // @name is used to write the contents of @link(FValues) to a stream.
    procedure WriteValues(Writer: TWriter);
    // @name causes @link(FValues) to be written to and read from a stream.
    procedure DefineProperties(Filer: TFiler); override;
  public
    // @name copies all the values from Source
    // (which must be a @link(TCustomDataListItem))
    // to the object calling Assign
    procedure Assign(Source: TPersistent); override;
    // @name is used to access the data stored in the @classname.
    // @SeeAlso(TCustomDataListItem.ValueIndex)
    property Values[const Index: Integer]: double read GetValues write
      SetValues;
  end;

  // @abstract(@name is a @link(TCustomDataListItem) specialized for
  // specialized for holding integers.)
  TIntegerDataListItem = class(TCustomDataListItem)
  private
    // @name: @link(GoPhastTypes.TOneDIntegerArray);
    // @name is the raw data stored by the @classname.  Normally,
    // these values are access through the @link(Values) property which
    // provides error checking.
    FValues: TOneDIntegerArray;
    // See @link(Values).
    function GetValues(const Index: Integer): Integer;
    // See @link(Values).
    procedure SetValues(const Index, Value: Integer);
  protected
    // @name retrieves the number of items stored in @classname.
    function GetValuesLength: integer; override;
    // @name is used to read the contents of @link(FValues) from a stream.
    procedure ReadValues(Reader: TReader);
    // @name sets the number of items stored in @classname.
    procedure SetValuesLength(const Value: integer); override;
    // @name is used to write the contents of @link(FValues) to a stream.
    procedure WriteValues(Writer: TWriter);
    // @name causes @link(FValues) to be written to and read from a stream.
    procedure DefineProperties(Filer: TFiler); override;
  public
    // Assign copies all the values from Source
    // (which must be a @link(TCustomDataListItem))
    // to the object calling Assign
    procedure Assign(Source: TPersistent); override;
    // Values is used to access the data stored in the @classname.
    // @SeeAlso(TCustomDataListItem.ValueIndex)
    property Values[const Index: Integer]: Integer read GetValues write
      SetValues;
  end;

  // @abstract(@name is a collection of @Link(TCustomDataListItem)'s.
  // It is used to store data by zones in a @Link(TMultiValueScreenObject).)
  // Normally, only the descendants of TDataListCollection are created.
  TDataListCollection = class(TPhastCollection)
  private
    // @name is the @link(TScreenObject) with which the data is associated.
    FScreenObject: TScreenObject;
    // @name is used to store the result of @link(GetItemByName).
    // Often, the next time @link(GetItemByName) is called, it will be with the
    // same name so storing the result can speed things up.
    PriorIndex: integer;
  public
    // @name creates and instance of @classname.
    constructor Create(ItemClass: TCollectionItemClass; const ScreenObject:
      TScreenObject; Model: TComponent);
    // @name is the TScreenObject with which the data is associated.
    property ScreenObject: TScreenObject read FScreenObject;
    // @name returns the TCustomDataListItem
    // whose name matches ADataSetName.
    function GetItemByName(ADataSetName: string): TCustomDataListItem;
  end;

  // @abstract(@name is a descendant of @link(TDataListCollection)
  // specialized for storing real numbers.)
  TRealDataListCollection = class(TDataListCollection)
    // @name creates an instance of @classname.
    // See @link(TRealDataListItem).
    constructor Create(const ScreenObject: TScreenObject; Model: TComponent);
  end;

  // @abstract(@name is a descendant of @link(TDataListCollection)
  // specialized for storing integers.)
  TIntegerDataListCollection = class(TDataListCollection)
    // @name creates an instance of @classname.
    // See @link(TIntegerDataListItem).
    constructor Create(const ScreenObject: TScreenObject; Model: TComponent);
  end;

  //@abstract(@name is used in creating descendants of @link(TScreenObject)
  // when reading a model from a file and when the user creates a new
  // @link(TScreenObject).)
  TScreenObjectClass = class of TScreenObject;

  {@abstract(@name is used to store or read whether
   a vertex in a @link(TScreenObject) is selected or not.)}
  TSelectedVertexItem = class(TCollectionItem)
  private
    // @name stores whether a vertex is selected.
    // See @link(VertexSelected).
    FVertexSelected: boolean;
  published
    // @name stores whether a vertex is selected.
    property VertexSelected: boolean read FVertexSelected
      write FVertexSelected;
    // @exclude
    // @name is an alias for @link(VertexSelected).  It is maintained for
    // backwards compatibility only.
    property NodeSelected: boolean read FVertexSelected write FVertexSelected
      stored False;
  end;

  {@abstract(@name is used to store or read which vertices
    in a @link(TScreenObject) are selected.)}
  TSelectedVertexCollection = class(TCollection)
    // @name creates an instance of @classname.
    // See @link(TSelectedVertexItem).
    constructor Create;
  end;

  // @exclude
  // @name is an alias of @link(TSelectedVertexCollection)
  // maintained for backwards compatibility.
  TSelectedNodeCollection = TSelectedVertexCollection;

  {@abstract(@name is used to store a TPoint2D.)}
  TPointItem = class(TCollectionItem)
  private
    // See @link(X).
    FX: double;
    // See @link(Y).
    FY: double;
  published
    // @name is the X coordinate of the point.
    property X: double read FX write FX;
    // @name is the Y coordinate of the point.
    property Y: double read FY write FY;
  end;

  {@abstract(@name is used to store a series of TPoint2Ds.)}
  TPointCollection = class(TCollection)
    // @name creates an instance of @classname.
    // See @link(TPointItem).
    constructor Create;
  end;

  {@abstract(@name is used in reading a @link(TScreenObject)
    from or writing it to a stream.)
   When saving data to a stream, the data is read from
   @link(TScreenObjectItem.FScreenObject).  When reading data from a stream,
   the data is stored in private fields.  It is transferred to a
   @link(TScreenObject) in @link(TScreenObjectItem.UpdateScreenObject).}
  TScreenObjectItem = class(TCollectionItem)
  private
    // See @link(DataSetNames).
    FDataSetNames: TStrings;
    // See @link(DataSetFormulas).
    FDataSetFormulas: TStrings;
    // See @link(ElevationFormula).
    FElevationFormula: string;
    // See @link(HigherElevationFormula).
    FHigherElevationFormula: string;
    // See @link(LowerElevationFormula).
    FLowerElevationFormula: string;
    // @name stores the formulas for mixtures in the @link(TScreenObject).
    FMixtureFormulas: TStrings;
    // See @link(Points).
    FPoints: TPointCollection;
    // See @link(ScreenObject).
    FScreenObject: TScreenObject;
    // See @link(SelectedVertices).
    FSelectedVertexCollection: TSelectedVertexCollection;
    // See @link(ClassTypeName).
    function GetClassType: string;
    // See @link(DataSetFormulas).
    function GetDataSetFormulas: TStrings;
    // See @link(DataSetNames).
    function GetDataSetNames: TStrings;
    // See @link(ElevationFormula).
    function GetElevationFormula: string;
    // See @link(HigherElevationFormula).
    function GetHigherElevationFormula: string;
    // See @link(LowerElevationFormula).
    function GetLowerElevationFormula: string;
    // @name fills @link(FMixtureFormulas) with the formulas for mixtures
    // in the @link(TScreenObject) and then
    // returns @link(FMixtureFormulas).
    function GetMixtureFormulas: TStrings;
    // See @link(ClassTypeName).
    procedure SetClassType(Value: string);
  protected
    // @name updates @link(ScreenObject) with the values read from
    // the stream and stored in private data fields.
    // After a @link(TScreenObject) has been read,
    // @name is called to update its formulas for mixtures.
    procedure UpdateScreenObject;
  public
    // @name creates an instance of @classname.
    constructor Create(Collection: TCollection); override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    // see @link(ScreenObject).
    procedure SetScreenObject(const Value: TScreenObject);
  published
    // @name must be before @link(ScreenObject) or Contour.
    // Setting @name causes @link(ScreenObject) to be created.
    property ClassTypeName: string read GetClassType write SetClassType;
    // @exclude
    // @name is used for backwards compatibility only.
    // See @link(ScreenObject).
    property Contour: TScreenObject read FScreenObject stored False;
    // @name is the data sets whose values are set by @link(ScreenObject).
    property DataSetNames: TStrings read GetDataSetNames write FDataSetNames;
    // @name has the formulas for the data sets
    // whose values are set by @link(ScreenObject).
    property DataSetFormulas: TStrings read GetDataSetFormulas write
      FDataSetFormulas;
    // @name is the @link(TScreenObject.ElevationFormula)
    // of @link(ScreenObject).
    property ElevationFunction: string read GetElevationFormula write
      FElevationFormula stored False;
    property ElevationFormula: string read GetElevationFormula write
      FElevationFormula;
    // @name is the @link(TScreenObject.HigherElevationFormula)
    // formula of @link(ScreenObject).
    property HigherElevationFunction: string read GetHigherElevationFormula
      write FHigherElevationFormula stored False;
    property HigherElevationFormula: string read GetHigherElevationFormula
      write FHigherElevationFormula;
    // @name is the @link(TScreenObject.LowerElevationFormula)
    // formula of @link(ScreenObject).
    property LowerElevationFunction: string read GetLowerElevationFormula
      write FLowerElevationFormula stored False;
    property LowerElevationFormula: string read GetLowerElevationFormula
      write FLowerElevationFormula;
    // @name stores the @link(TScreenObject.Points) of @link(ScreenObject).
    property Points: TPointCollection read FPoints write FPoints;
    // @name is set to a subcomponent in @link(SetScreenObject).
    // This is required
    // to have the properties of @link(TScreenObject) saved to file.
    // It can be set through @link(SetScreenObject).
    property ScreenObject: TScreenObject read FScreenObject;
    // @name stores the @link(TScreenObject.SelectedVertices)
    // of @link(ScreenObject).
    property SelectedVertices: TSelectedVertexCollection
      read FSelectedVertexCollection write FSelectedVertexCollection;
    // @exclude
    // @name is the same as @link(SelectedVertices).  It is used only
    // for backwards compatibility.
    property SelectedNodes: TSelectedVertexCollection
      read FSelectedVertexCollection write FSelectedVertexCollection
      stored False;
    // @name is used for reading and storing formulas for mixtures.
    // When it is being read, the value is read from the associated
    // @link(TScreenObject).  When it is being written,
    // it is stored in the private field (FMixtureFormulas) and is
    // later transfered to the @link(TScreenObject) in the protected
    // method @link(UpdateScreenObject).
    property MixtureFormulas: TStrings read GetMixtureFormulas write
      FMixtureFormulas;
  end;

  TPhastDelegate = class(TCustomScreenObjectDelegate)
  strict private
    FMixtureVariables: TStringList;
    FMixtureCompiler: TRbwParser;
    FMixtureExpression: TExpression;
    function PhastBoundaryType: TBoundaryTypes;
    function ThreeDBoundaryFormula: string;
    function TwoDBoundaryFormula: string;
    procedure AssignIntegerDataWithPhastInterpolation(const DataSet: TDataArray;
      const LayerIndex, RowIndex, ColIndex: integer; const Comment: string;
      const InterpValue: TInterpValuesItem);
    procedure AssignRealDataWithPhastInterpolation(const DataSet: TDataArray;
      const LayerIndex, RowIndex, ColIndex: integer; const Comment: string;
      const InterpValue: TInterpValuesItem);
  strict protected
    procedure AssignCellValue(const UsedVariables: TStringList;
      const DataSet: TDataArray;
      LayerIndex, RowIndex, ColIndex: integer;
      const Compiler: TRbwParser; const Annotation: string;
      var Expression: TExpression; const OtherData: TObject;
      SectionIndex: integer); override;
    procedure AssignValuesToFrontDataSet(const Grid: TCustomGrid;
      const DataSet: TDataArray; OtherData: TObject;
      AssignmentLocation: TAssignmentLocation = alAll);override;
    procedure AssignValuesToSideDataSet(const Grid: TCustomGrid;
      const DataSet: TDataArray; OtherData: TObject;
      AssignmentLocation: TAssignmentLocation = alAll); override;
    procedure AssignValuesToTopDataSet(const Grid: TCustomGrid;
      const DataSet: TDataArray; OtherData: TObject;
      AssignmentLocation: TAssignmentLocation = alAll); override;
    function BoundaryType: integer; override;
    function DataSetUsed(const DataSet: TDataArray;
      var OtherData: TObject): boolean; override;
    function EncloseAnnotation(const DataSetFormula: string;
      const OtherData: TObject): string; override;
    function IntersectAnnotation(const DataSetFormula: string;
      const OtherData: TObject): string; override;
    procedure OtherIndex(const LayerOrRow, RowOrColumn: integer;
      out First, Last: integer; const DataSet: TDataArray); override;
    procedure UpdateFrontSegments(const Grid: TCustomGrid;
      const EvaluatedAt: TEvaluatedAt);override;
    procedure UpdateSideSegments(const Grid: TCustomGrid;
      const EvaluatedAt: TEvaluatedAt);override;
  protected
    procedure GetSideCellsToAssign(const Grid: TCustomGrid;
      const DataSetFunction: string; OtherData: TObject;
      const DataSet: TDataArray; CellList: TCellAssignmentList;
      AssignmentLocation: TAssignmentLocation); override;
    procedure GetFrontCellsToAssign(const Grid: TCustomGrid;
      const DataSetFunction: string; OtherData: TObject;
      const DataSet: TDataArray; CellList: TCellAssignmentList;
      AssignmentLocation: TAssignmentLocation); override;
    procedure GetTopCellsToAssign(const Grid: TCustomGrid;
      const DataSetFunction: string; OtherData: TObject;
      const DataSet: TDataArray; CellList: TCellAssignmentList;
      AssignmentLocation: TAssignmentLocation); override;
    procedure GetCellsToAssign(const Grid: TCustomGrid;
      const DataSetFunction: string; OtherData: TObject;
      const EvaluatedAt: TEvaluatedAt; CellList: TCellAssignmentList;
      AssignmentLocation: TAssignmentLocation);
  public
    procedure AssignSelectedCells(const Grid: TCustomGrid); override;
    procedure InitializeExpression(out Compiler: TRbwParser;
      out DataSetFormula: string; out Expression: TExpression;
      const DataSet: TDataArray; const OtherData: TObject); override;
    constructor Create(ScreenObject: TScreenObject); override;
    destructor Destroy; override;
  end;

  TModflowDataObject = class(TObject)
    Compiler: TRbwParser;
    DataSetFunction: string;
    AlternateName: string;
    AlternateDataType: TRbwDataType;
  end;

  TModflowDelegate = class(TCustomScreenObjectDelegate)
  strict private
    procedure AssignColAndRowIndicies(
      var ColIndex, RowIndex, LayerIndex : Integer;
  const HorizontalIndex1, HorizontalIndex2, PerpendicularIndex: Integer);
    procedure AssignParallellLimits(const Grid: TCustomGrid;
      out FirstParallelIndexA, LastParallelIndexA,
      FirstParallelIndexB, LastParallelIndexB: Integer);
    function FindLayer(const ColOrRow: integer; const Location: TEdgePoint;
      const CellOutlines: T2DRealPointArray; const MovingUp: boolean): integer;
    function CountIntersectPointsOnEdge(const ColOrRow, Layer,
      EdgeIndex: integer; const FirstPoint, SecondPoint: TPoint2D;
      const CellOutlines: T2DRealPointArray;
      out PointsOnEdge: TRealPointArray): integer;
    function FindIntersectionSurface(const FirstCol, LastCol,
      LayerIndex: integer; const PreviousPoint, APoint: TEdgePoint;
      var IntersectionLocation: TEdgePoint;
      const CellOutlines: T2DRealPointArray; out Col: integer): boolean;
    function FindLayerOnEdge(const APoint, PreviousPoint: TEdgePoint;
      var IntersectionLocation: TEdgePoint;
      const CellOutline: T2DRealPointArray; const EdgeIndex: integer): integer;
    function OutlineVertex(const ColOrRow, Layer, PointIndex: integer;
      const CellOutlines: T2DRealPointArray): TPoint2D;
    function InCell(const ColOrRow, Layer: integer; const TestPoint: TEdgePoint;
      const CellOutlines: T2DRealPointArray): boolean;
    procedure GetHorizontalLimitsOfGrid(const Grid: TCustomGrid;
      out GridMinHorizontal, GridMaxHorizontal: real);
    procedure GetVerticalLimitsOfGrid(const Grid: TCustomGrid;
      out GridMinZ, GridMaxZ: Real);
    function GetColOrRow(const Grid: TCustomGrid;
      const APoint: TEdgePoint;
      const GridMinHorizontal, GridMaxHorizontal: Real): integer;
    procedure GetHorizontalLimits(const Grid: TCustomGrid;
      var HorizontalLimit: Integer; var PerpendicularLimit: Integer);
    procedure GetRotatedPoints(var RotatedPoints: TEdgePointArray);
    function GetHorizontalIndexFromLocation(const X: Real;
      const Grid: TCustomGrid): integer;
    procedure CreateSegment(const Point1,Point2: TEdgePoint;
      const LayerIndex, PerpendicularIndex, HorizontalIndex,
      VertexIndex, SectionIndex: Integer);
    // @name creates a set of segments for each cell intersected by
    // @link(FScreenObject) in front or side views.
    function IsPointInside(const CellLocation3D: T3DRealPoint;
      Grid: TCustomGrid;
      out SectionIndex: integer): boolean;
    function GetPerpendiularLimit(const Grid: TCustomGrid): integer;
    procedure AssignValuesToDataSet(const Grid: TCustomGrid;
      const DataSet: TDataArray; OtherData: TObject;
      AssignmentLocation: TAssignmentLocation = alAll);
    procedure UpdateHorizontalRangeOfCellsToCheck(
      var FirstHorizontalIndex, LastHorizontalIndex: Integer;
      const HorizontalIndex, HorizontalLimit: Integer;
      const APoint, PreviousPoint: TEdgePoint);
  private
    // @name assigns values to FTopElevation and FBottomElevation
    // based on the cell specified by ColIndex, RowIndex, and LayerIndex.
    function AssignElevations(Const ColIndex, RowIndex,
      LayerIndex: integer): boolean;
    function ElevationOk(const Grid: TCustomGrid;
      const PerpendicularIndex: integer; const ColIndex: integer;
      const RowIndex: integer): boolean;
    function GetCellOutlines(const Grid: TCustomGrid;
      const RowOrCol: integer): T2DRealPointArray;
    procedure UpdateSegments(const Grid: TCustomGrid;
      const EvaluatedAt: TEvaluatedAt);
    procedure GetCellsToAssign(const Grid: TCustomGrid;
      const DataSetFunction: string; OtherData: TObject;
      const EvaluatedAt: TEvaluatedAt; CellList: TCellAssignmentList;
      AssignmentLocation: TAssignmentLocation; Orientation: TDataSetOrientation);
  strict protected
    function DataSetUsed(const DataSet: TDataArray;
      var OtherData: TObject): boolean; override;
  protected
    procedure GetFrontCellsToAssign(const Grid: TCustomGrid;
      const DataSetFunction: string; OtherData: TObject;
      const DataSet: TDataArray; CellList: TCellAssignmentList;
      AssignmentLocation: TAssignmentLocation); override;
    procedure GetSideCellsToAssign(const Grid: TCustomGrid;
      const DataSetFunction: string; OtherData: TObject;
      const DataSet: TDataArray; CellList: TCellAssignmentList;
      AssignmentLocation: TAssignmentLocation); override;
    procedure GetTopCellsToAssign(const Grid: TCustomGrid;
      const DataSetFunction: string; OtherData: TObject;
      const DataSet: TDataArray; CellList: TCellAssignmentList;
      AssignmentLocation: TAssignmentLocation); override;
  public
    procedure InitializeExpression(out Compiler: TRbwParser;
      out DataSetFunction: string; out Expression: TExpression;
      const DataSet: TDataArray; const OtherData: TObject); override;
    procedure AssignSelectedCells(const Grid: TCustomGrid); override;
    procedure AssignValuesToFrontDataSet(const Grid: TCustomGrid;
      const DataSet: TDataArray; OtherData: TObject;
      AssignmentLocation: TAssignmentLocation = alAll);override;
    procedure AssignValuesToSideDataSet(const Grid: TCustomGrid;
      const DataSet: TDataArray; OtherData: TObject;
      AssignmentLocation: TAssignmentLocation = alAll); override;
    procedure AssignValuesToTopDataSet(const Grid: TCustomGrid;
      const DataSet: TDataArray; OtherData: TObject;
      AssignmentLocation: TAssignmentLocation = alAll); override;
    constructor Create(ScreenObject: TScreenObject); override;
    procedure UpdateFrontSegments(const Grid: TCustomGrid;
      const EvaluatedAt: TEvaluatedAt); override;
    procedure UpdateSideSegments(const Grid: TCustomGrid;
      const EvaluatedAt: TEvaluatedAt);override;
    destructor Destroy; override;
  end;

  TResetProcedure = procedure(Compiler: TRbwParser) of object;

  {@abstract(@name is and abstract base class.  Its descendants,
    @link(TIntegerPhastBoundaryCondition) and
    @link(TRealPhastBoundaryCondition), are
    used to define the value of one aspect of a boundary
    condition for one time period.)}
  TCustomPhastBoundaryCondition = class(TInterpValuesItem)
  private
    // See @link(Formula).
    FFormula: string;
    // @name is a list of the @link(TObserver)s that are directly used in
    // @link(MixtureFormula).
    FMixtureDataSetList: TList;
    FFormulaDataSetList: TList;
    // See @link(MixtureFormula).
    FMixtureFormula: string;
    // @name is used to monitor changes to the @link(TDataArray)s that are
    // used in @link(MixtureFormula).
    FMixtureObserver: TObserver;
    // See @link(Time).
    FTime: double;
    FFormulaObserver: TObserver;
    FFormulaObject: TFormulaObject;
    // @name determines the @link(TDataArray)s that are used in
    // @link(MixtureFormula) and has @link(FMixtureObserver) listen to
    // each of them.  See (TObserver.TalksTo).
    procedure AddMixtureSubscriptions;
    // See @link(Distance1).
    function GetDistance1: double;
    // See @link(Distance2).
    function GetDistance2: double;
    // See @link(InterpolationDirection).
    function GetInterpolationDirection: TInterpolationDirection;
    // See @link(MixtureExpression).
    function GetMixtureFormula: string;
    // See @link(ScreenObject).
    function GetScreenObject: TScreenObject;
    // See @link(UsePHAST_Interpolation).
    function GetUsePHAST_Interpolation: boolean;
    // @name causes @link(FMixtureObserver) to stop listening to
    // every @link(TDataArray) that it is observing.
    // See @link(TObserver.StopsTalkingTo).
    procedure RemoveMixtureSubscriptions;
    // See @link(Distance1).
    procedure SetDistance1(const Value: double);
    // See @link(Distance2).
    procedure SetDistance2(const Value: double);
    // See @link(Expression).
    procedure SetFormula(const Value: string);
    // See @link(InterpolationDirection).
    procedure SetInterpolationDirection(const Value: TInterpolationDirection);
    // See @link(MixtureExpression).
    procedure SetMixtureExpression(const Value: string);
    // See @link(MixtureFormula).
    procedure SetMixtureFormula(const Value: string);
    // See @link(Time).
    procedure SetTime(const Value: double);
    // See @link(UsePHAST_Interpolation).
    procedure SetUsePHAST_Interpolation(const Value: boolean);
    function GetFormulaExpression: string;
    procedure SetFormulaExpression(const Value: string);
    procedure RemoveDataSetSubscriptions(DataSetList: TList; Observer: TObserver);
    procedure AddDataSetSubscriptions(DataSetList: TList; Observer: TObserver;
      Reset: TResetProcedure; NewFormula: string);
    procedure RemoveFormulaSubscriptions;
    procedure AddFormulaSubscriptions;
    procedure RemoveSubscription(Sender: TObject;
      const AName: string);
    procedure RestoreSubscription(Sender: TObject;
      const AName: string);
    procedure ResetMixtureFormula(Compiler: TRbwParser);
    procedure ResetFormulaExpression(Compiler: TRbwParser);
  protected
    // See @link(Datatype).
    function GetDatatype: TRbwDataType; virtual; abstract;
    // @name sets @link(FMixtureObserver).UpToDate := True;
    procedure ResetMixtureSubscription;
    // @name transfers the value stored in @link(FMixtureFormula) to
    // @link(MixtureExpression).  See @link(MixtureFormula).
    procedure UpdateMixtureExpression;
    procedure UpdateFormulaExpression;
    procedure InvalidateModel;
  public
    // @name copies values from @classname or @link(TInterpValuesItem)
    // to the object calling @name.
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create(Collection: TCollection); override;
    // @name destroys the current instance of @classname.
    destructor Destroy; override;
    // @name gets the @link(TSparseArrayPhastInterpolationDataSet) to which
    // this @classname applies.
    function GetDataSet: TSparseArrayPhastInterpolationDataSet;
    // See TInterpValuesItem.Values.@link(
    // TPhastInterpolationValues.MixtureFormula).
    // Mixture Expression can not be set until the entire model has
    // been loaded.  See @link(MixtureFormula).
    // See @link(TPhastInterpolationValues).
    property MixtureExpression: string read GetMixtureFormula write
      SetMixtureExpression;
    // @name is used to read the @link(TScreenObject) to which
    // this @classname applies.
    property ScreenObject: TScreenObject read GetScreenObject;
    property FormulaExpression: string read GetFormulaExpression
      write SetFormulaExpression;
  published
    // @name specifies the type of data (real number, integer, boolean,
    // or string) stored in the @classname.  However, at the time this
    // was written only real numbers and integers were supported because
    // those were the only types supported in descendants of
    // @link(TSparseArrayPhastInterpolationDataSet). See @link(GetDataSet).
    property DataType: TRbwDataType read GetDatatype;
    // See TInterpValuesItem.Values.@link(TPhastInterpolationValues.Distance1).
    // See @link(TPhastInterpolationValues).
    property Distance1: double read GetDistance1 write SetDistance1;
    // See TInterpValuesItem.Values.@link(TPhastInterpolationValues.Distance2).
    // See @link(TPhastInterpolationValues).
    property Distance2: double read GetDistance2 write SetDistance2;
    // @name is only for backwards compatibility.
    property Expression: string read GetFormulaExpression write SetFormula stored False;
    // @name is the formula used to set the value of the @classname when
    // PHAST style interpolation is not used (that is:
    // @link(UsePHAST_Interpolation) = @false).
    // See @link(TPhastInterpolationValues).

    property Formula: string read GetFormulaExpression write SetFormula;
    // See TInterpValuesItem.Values.@link(
    // TPhastInterpolationValues.InterpolationDirection).
    property InterpolationDirection: TInterpolationDirection
      read GetInterpolationDirection write SetInterpolationDirection;
    // See TInterpValuesItem.Values.@link(
    // TPhastInterpolationValues.MixtureFormula).
    // @name is used to read and write @link(MixtureExpression).
    // After the model is loaded, @link(UpdateMixtureExpression) is called
    // to transfer the value saved in @link(FMixtureFormula) to
    // @link(MixtureExpression).
    // See @link(TPhastInterpolationValues).
    property MixtureFormula: string read GetMixtureFormula write
      SetMixtureFormula;
    // @name is the time at which this @classname takes effect.
    property Time: double read FTime write SetTime;
    // @name indicates whether or not PHAST-style interpolation will be used.
    // See @link(TPhastInterpolationValues).
    property UsePHAST_Interpolation: boolean read GetUsePHAST_Interpolation
      write SetUsePHAST_Interpolation;
  end;

  {@abstract(@name is used to define the value of one aspect of a boundary
    condition for one time period.  The aspect of the boundary condition
    must be a real number.  An example is the head in a specified head
    boundary.)}
  TRealPhastBoundaryCondition = class(TCustomPhastBoundaryCondition)
  private
    // See @link(Value1).
    function GetValue1: double;
    // See @link(Value2).
    function GetValue2: double;
    // See @link(Value1).
    procedure SetValue1(const Value: double);
    // See @link(Value2).
    procedure SetValue2(const Value: double);
  protected
    // @name indicates that this boundary condition
    // represents a real number.
    function GetDatatype: TRbwDataType; override;
  public
    // If PHAST-style interpolation will be used, @name indicates the first
    // value for the interpolation.
    // See @link(TCustomPhastBoundaryCondition.UsePHAST_Interpolation)
    // and @link(TPhastInterpolationValues).
    property Value1: double read GetValue1 write SetValue1;
    // If PHAST-style interpolation will be used, @name indicates the second
    // value for the interpolation.
    // See @link(TCustomPhastBoundaryCondition.UsePHAST_Interpolation)
    // and @link(TPhastInterpolationValues).
    property Value2: double read GetValue2 write SetValue2;
  end;

  {@abstract(@name is used to define the value of one aspect of a boundary
    condition for one time period.  The aspect of the boundary condition
    must be an integer.  An example is the associated solution
    in a specified head boundary.)}
  TIntegerPhastBoundaryCondition = class(TCustomPhastBoundaryCondition)
  private
    // See @link(Value1).
    function GetValue1: integer;
    // See @link(Value2).
    function GetValue2: integer;
    // See @link(Value1).
    procedure SetValue1(const Value: integer);
    // See @link(Value2).
    procedure SetValue2(const Value: integer);
  protected
    // @name indicates that this boundary condition
    // represents an integer.
    function GetDatatype: TRbwDataType; override;
  public
    // If PHAST-style interpolation will be used, @name indicates the first
    // value for the interpolation.
    // See @link(TCustomPhastBoundaryCondition.UsePHAST_Interpolation)
    // and @link(TPhastInterpolationValues).
    property Value1: integer read GetValue1 write SetValue1;
    // If PHAST-style interpolation will be used, @name indicates the second
    // value for the interpolation.
    // See @link(TCustomPhastBoundaryCondition.UsePHAST_Interpolation)
    // and @link(TPhastInterpolationValues).
    property Value2: integer read GetValue2 write SetValue2;
  end;




  { TODO : Replace TScreenObject with a generic property of
    TScreenObject that can hold everything that is currently in
    TScreenObject and can be replaced with something else for a different
    model. }


  {@abstract(@name is a @link(TScreenObject) that can store a
    series of integer or real number values associated with a data set.
    @name is used when importing a zone from an existing PHAST model.)}
  TMultiValueScreenObject = class(TScreenObject)
  private
    // See @link(IntegerValues).
    FIntegerValues: TIntegerDataListCollection;
    // See @link(RealValues).
    FRealValues: TRealDataListCollection;
    // See @link(IntegerValues).
    procedure SetIntegerValues(const Value: TIntegerDataListCollection);
    // See @link(RealValues).
    procedure SetRealValues(const Value: TRealDataListCollection);
  public
    procedure Assign(Source: TPersistent); override;
   // @name creates an instance of @classname.
    constructor Create(AnOwner: TComponent); override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
  published
    // @name stores integer values associated with one or more data sets.
    property IntegerValues: TIntegerDataListCollection read FIntegerValues
      write SetIntegerValues;
    // @name stores real number values associated with one or more data sets.
    property RealValues: TRealDataListCollection read FRealValues write
      SetRealValues;
  end;

  {@abstract(@name is used for reading a series of @link(TScreenObject)s
    from or writing them to a file or the clipboard.)}
  TScreenObjectCollection = class(TCollection)
  private
    FModel: TComponent;
  public
    // @name creates an instance of @classname.
    constructor Create(Model: TComponent);
    // @name calls @link(TScreenObjectItem.UpdateScreenObject) for
    // each @link(TScreenObjectItem).
    procedure UpdateScreenObjects;
  end;

  TScreenObjectClipboard = class(TComponent)
  private
    FScreenObjects: TScreenObjectCollection;
    procedure SetScreenObjects(const Value: TScreenObjectCollection);
  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ScreenObjects: TScreenObjectCollection read FScreenObjects
      write SetScreenObjects;
    procedure UpdateModel(Model: TComponent);
  end;

{@abstract(If the formula for a @link(TDataArray) in a @link(TScreenObject)
  is incorrect, @name is used to change it to a default value.)}
procedure ResetScreenObjectFunction(const DataSetIndex: integer;
  const ScreenObject: TScreenObject; const Compiler: TRbwParser;
  const DataSetDataType: TRbwDataType; const ErrorMessage: string;
  const IsBoundary: boolean);

  // @abstract(Point1 and Point2 define a non-horizontal line.
  // Given Y, @name returns the
  // X value for this point along the line.)
function XIntersection(const Point1, Point2: TEdgePoint;
  const Y: real): real;

  // @abstract(Point1 and Point2 define a non-vertical line.
  // Given X, @name returns the
  // Y value for this point.)
function YIntersection(const Point1, Point2: TEdgePoint;
  const X: real): real; overload;
function YIntersection(const Point1, Point2: TPoint2D;
  const X: real): real; overload;

// @name is a comparison function for List.Sort.  It ensures that
// the @link(TScreenObject)s in the list will be sorted propertly.
// The @link(TScreenObject)s are sorted first by their root name
// (without case sensitivity) and then by the number following the
// root name.
function ScreenObjectCompare(Item1, Item2: Pointer): Integer;

// result.Column and result.Row must have already been set when @name is called.
procedure GetLayerFromZ(Z: Double; var CellLocation: TCellLocation;
  Grid: TModflowGrid; Model: TObject);

procedure SelectAScreenObject(ScreenObject: TScreenObject);

function FindIntersectionPoints(Poly1, Poly2: TSubPolygon;
  var Intersections: TIntersectionArray; 
  var Count: integer): boolean;

procedure GlobalRemoveScreenObjectDataArraySubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure GlobalRestoreDataArraySubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure GlobalRemoveElevationSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
procedure GlobalRestoreElevationSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
procedure GlobalRemoveHigherElevationSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
procedure GlobalRestoreHigherElevationSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
procedure GlobalRemoveLowerElevationSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
procedure GlobalRestoreLowerElevationSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
procedure GlobalRemoveBoundaryDataArraySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
procedure GlobalRestoreBoundaryDataArraySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
procedure GlobalRemovePhastBoundarySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
procedure GlobalRestorePhastBoundarySubscription(Sender: TObject; Subject: TObject;
  const AName: string);

const
  ObjectPrefix = 'Object';
  StrInvalidScreenObjec = 'Invalid screen object vertex index.';

implementation

uses Math, UndoItemsScreenObjects, BigCanvasMethods,
  GIS_Functions, frmFormulaErrorsUnit, SparseArrayUnit, ModelMuseUtilities,
  InteractiveTools, PhastModelUnit, CountObjectsUnit, GlobalVariablesUnit,
  IntListUnit, frmGoPhastUnit, IsosurfaceUnit, TempFiles, LayerStructureUnit, 
  gpc;


const
  SquareSize = 3;
  MaxPointsInSubPolygon = 4;

type
  // @name is used when intersecting @link(TScreenObject)s with a
  // @link(TModflowGrid) in front or side views.
  TSegmentDirection = (sdUpRight, sdUpLeft, sdUp, sdDownRight,
    sdDownLeft, sdDown);


procedure GetLayerFromZ(Z: Double; var CellLocation: TCellLocation;
  Grid: TModflowGrid; Model: TObject);
var
  LayerIndex: Integer;
  LocalLayer: integer;
begin
  // result.Column and result.Row must have already been set when
  // GetLayerFromZ is called.
  CellLocation.Layer := 0;
  if Grid.LayerElevations[CellLocation.Column, CellLocation.Row, 0] < Z then
  begin
    CellLocation.Layer := 0;
    CellLocation.Row := 1;
    CellLocation.Column := 1;
  end
  else if Grid.LayerElevations[CellLocation.Column, CellLocation.Row, 0] = Z
    then
  begin
    CellLocation.Layer := 1;
    Inc(CellLocation.Column);
    Inc(CellLocation.Row);
  end
  else
  begin
    for LayerIndex := 1 to Grid.LayerCount do
    begin
      if Grid.LayerElevations[CellLocation.Column, CellLocation.Row,
        LayerIndex] < Z then
      begin
        LocalLayer := (Model as TPhastModel).LayerStructure.
          DataSetLayerToModflowLayer(LayerIndex-1);
        CellLocation.Layer := LocalLayer;
        Inc(CellLocation.Column);
        Inc(CellLocation.Row);
        break;
      end;
    end;
  end;
end;


function ScreenObjectCompare(Item1, Item2: Pointer): Integer;
var
  ScreenObject1: TScreenObject;
  ScreenObject2: TScreenObject;
  Name1, Name2: string;
  Number1, Number2: string;
  procedure ProcessName(var AName, ANumber: string);
  const
    Digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
  var
    Index: integer;
    LastDigit: integer;
  begin
    LastDigit := Length(AName) + 1;
    for Index := Length(AName) downto 1 do
    begin
      if (AName[Index] in Digits) then
      begin
        LastDigit := Index;
      end
      else
      begin
        break;
      end;
    end;
    ANumber := Copy(AName, LastDigit, MAXINT);
    SetLength(AName, LastDigit-1);
  end;
begin
  ScreenObject1 := Item1;
  ScreenObject2 := Item2;
  Name1 := ScreenObject1.Name;
  Name2 := ScreenObject2.Name;
  ProcessName(Name1, Number1);
  ProcessName(Name2, Number2);
  result := AnsiCompareText(Name1, Name2);
  if result = 0 then
  begin
    if Number1 = Number2 then
    begin
      Exit;
    end
    else if Number1 = '' then
    begin
      result := -1
    end
    else if Number2 = '' then
    begin
      result := 1
    end
    else
    begin
      result := StrToInt(Number1) - StrToInt(Number2);
    end;
  end;
end;

procedure ResetScreenObjectFunction(const DataSetIndex: integer; const
  ScreenObject:
  TScreenObject; const Compiler: TRbwParser; const DataSetDataType:
    TRbwDataType;
  const ErrorMessage: string; const IsBoundary: boolean);
var
  ScreenObjectFunction: string;
begin
  if IsBoundary then
  begin
    ScreenObjectFunction :=
      ScreenObject.BoundaryDataSetFormulas[DataSetIndex];
    frmFormulaErrors.AddError(ScreenObject.Name,
      ScreenObject.BoundaryDataSets[DataSetIndex].Name,
      ScreenObjectFunction, ErrorMessage);
  end
  else
  begin
    ScreenObjectFunction := ScreenObject.DataSetFormulas[DataSetIndex];
    frmFormulaErrors.AddError(ScreenObject.Name,
      ScreenObject.DataSets[DataSetIndex].Name,
      ScreenObjectFunction, ErrorMessage);
  end;

  case DataSetDataType of
    rdtDouble, rdtInteger:
      begin
        ScreenObjectFunction := '0';
      end;
    rdtBoolean:
      begin
        ScreenObjectFunction := 'False';
      end;
    rdtString:
      begin
        ScreenObjectFunction := '"0"';
      end;
  else
    Assert(False);
  end;
  if IsBoundary then
  begin
    ScreenObject.BoundaryDataSetFormulas[DataSetIndex] :=
      ScreenObjectFunction;
  end
  else
  begin
    ScreenObject.DataSetFormulas[DataSetIndex] := ScreenObjectFunction;
  end;

  Compiler.Compile(ScreenObjectFunction);
end;

// @abstract(@name returns true if Middle is between First and Last.
// First and Last do not need to be in order.  @name will
// also return true if it is just barely outside First to Last.)
function IsValueInside(const First, Middle, Last: real): boolean;
var
  Epsilon: double;
begin
  Epsilon := Abs(First - Last) / 1E8;
  // This function returns True if Middle is between First and Last.
  result := ((First - Epsilon <= Middle) and (Middle <= Last + Epsilon))
    or ((First + Epsilon >= Middle) and (Middle >= Last - Epsilon));
end;

// @abstract(@name is used in sorting TPoint2Ds
// along a line segment so that the
// points are arranged in order from lower left to upper right.)
function SortPointsRightUp(Item1, Item2: Pointer): Integer;
  // This function is used in sorting points along a line segment so that the
  // points are arranged in order from lower left to upper right.
var
  Point1, Point2: TPoint2D;
  doubleResult: real;
begin
  Point1 := P2DRealPoint(Item1)^;
  Point2 := P2DRealPoint(Item2)^;
  doubleResult := Point1.X - Point2.X;
  if doubleResult > 0 then
  begin
    result := 1;
  end
  else if doubleResult < 0 then
  begin
    result := -1
  end
  else
  begin
    doubleResult := Point1.Y - Point2.Y;
    if doubleResult > 0 then
    begin
      result := 1;
    end
    else if doubleResult < 0 then
    begin
      result := -1
    end
    else
    begin
      result := 0;
    end;
  end;
end;

// @abstract(@name is used in sorting TPoint2Ds
// along a line segment so that the
// points are arranged in order from upper left to lower right.)
function SortPointsRightDown(Item1, Item2: Pointer): Integer;
  // This function is used in sorting points along a line segment so that the
  // points are arranged in order from upper left to lower right.
var
  Point1, Point2: TPoint2D;
  doubleResult: real;
begin
  Point1 := P2DRealPoint(Item1)^;
  Point2 := P2DRealPoint(Item2)^;
  doubleResult := Point1.X - Point2.X;
  if doubleResult > 0 then
  begin
    result := 1;
  end
  else if doubleResult < 0 then
  begin
    result := -1
  end
  else
  begin
    doubleResult := Point2.Y - Point1.Y;
    if doubleResult > 0 then
    begin
      result := 1;
    end
    else if doubleResult < 0 then
    begin
      result := -1
    end
    else
    begin
      result := 0;
    end;
  end;
end;

// @abstract(@name is used in sorting TPoint2Ds
// along a line segment so that the
// points are arranged in order from lower right to upper left.)
function SortPointsLeftUp(Item1, Item2: Pointer): Integer;
  // This function is used in sorting points along a line segment so that the
  // points are arranged in order from lower right to upper left.
var
  Point1, Point2: TPoint2D;
  doubleResult: real;
begin
  Point1 := P2DRealPoint(Item1)^;
  Point2 := P2DRealPoint(Item2)^;
  doubleResult := Point2.X - Point1.X;
  if doubleResult > 0 then
  begin
    result := 1;
  end
  else if doubleResult < 0 then
  begin
    result := -1
  end
  else
  begin
    doubleResult := Point1.Y - Point2.Y;
    if doubleResult > 0 then
    begin
      result := 1;
    end
    else if doubleResult < 0 then
    begin
      result := -1
    end
    else
    begin
      result := 0;
    end;
  end;
end;

// @abstract(@name is used in sorting TPoint2Ds
// along a line segment so that the
// points are arranged in order from upper right to lower left.)
function SortPointsLeftDown(Item1, Item2: Pointer): Integer;
  // This function is used in sorting points along a line segment so that the
  // points are arranged in order from upper right to lower left.
var
  Point1, Point2: TPoint2D;
  doubleResult: real;
begin
  Point1 := P2DRealPoint(Item1)^;
  Point2 := P2DRealPoint(Item2)^;
  doubleResult := Point2.X - Point1.X;
  if doubleResult > 0 then
  begin
    result := 1;
  end
  else if doubleResult < 0 then
  begin
    result := -1
  end
  else
  begin
    doubleResult := Point2.Y - Point1.Y;
    if doubleResult > 0 then
    begin
      result := 1;
    end
    else if doubleResult < 0 then
    begin
      result := -1
    end
    else
    begin
      result := 0;
    end;
  end;
end;

function SortByEdgePostion(const Point1, Point2: TEdgePoint): integer;
begin
  result := 0;
  if Point1.Position = Point2.Position then
  begin
    result := 0;
  end
  else if Point1.Position = epFirst then
  begin
    result := -1;
  end
  else if Point2.Position = epFirst then
  begin
    result := 1;
  end
  else if Point1.Position = epLast then
  begin
    result := -1;
  end
  else if Point2.Position = epLast then
  begin
    result := 1;
  end
  else Assert(False);
end;



function SortEdgePointsRightUp(Item1, Item2: Pointer): Integer;
  // This function is used in sorting points along a line segment so that the
  // points are arranged in order from lower left to upper right.
var
  Point1, Point2: TEdgePoint;
  doubleResult: real;
begin
  Point1 := PEdgePoint(Item1)^;
  Point2 := PEdgePoint(Item2)^;
  doubleResult := Point1.X - Point2.X;
  if doubleResult > 0 then
  begin
    result := 1;
  end
  else if doubleResult < 0 then
  begin
    result := -1
  end
  else
  begin
    doubleResult := Point1.Y - Point2.Y;
    if doubleResult > 0 then
    begin
      result := 1;
    end
    else if doubleResult < 0 then
    begin
      result := -1
    end
    else
    begin
      result := SortByEdgePostion(Point1, Point2);
    end;
  end;
end;

function SortEdgePointsRightDown(Item1, Item2: Pointer): Integer;
  // This function is used in sorting points along a line segment so that the
  // points are arranged in order from upper left to lower right.
var
  Point1, Point2: TEdgePoint;
  doubleResult: real;
begin
  Point1 := PEdgePoint(Item1)^;
  Point2 := PEdgePoint(Item2)^;
  doubleResult := Point1.X - Point2.X;
  if doubleResult > 0 then
  begin
    result := 1;
  end
  else if doubleResult < 0 then
  begin
    result := -1
  end
  else
  begin
    doubleResult := Point2.Y - Point1.Y;
    if doubleResult > 0 then
    begin
      result := 1;
    end
    else if doubleResult < 0 then
    begin
      result := -1
    end
    else
    begin
      result := SortByEdgePostion(Point1, Point2);
    end;
  end;
end;

function SortEdgePointsLeftUp(Item1, Item2: Pointer): Integer;
  // This function is used in sorting points along a line segment so that the
  // points are arranged in order from lower right to upper left.
var
  Point1, Point2: TEdgePoint;
  doubleResult: real;
begin
  Point1 := PEdgePoint(Item1)^;
  Point2 := PEdgePoint(Item2)^;
  doubleResult := Point2.X - Point1.X;
  if doubleResult > 0 then
  begin
    result := 1;
  end
  else if doubleResult < 0 then
  begin
    result := -1
  end
  else
  begin
    doubleResult := Point1.Y - Point2.Y;
    if doubleResult > 0 then
    begin
      result := 1;
    end
    else if doubleResult < 0 then
    begin
      result := -1
    end
    else
    begin
      result := SortByEdgePostion(Point1, Point2);
    end;
  end;
end;

function SortEdgePointsLeftDown(Item1, Item2: Pointer): Integer;
  // This function is used in sorting points along a line segment so that the
  // points are arranged in order from upper right to lower left.
var
  Point1, Point2: TEdgePoint;
  doubleResult: real;
begin
  Point1 := PEdgePoint(Item1)^;
  Point2 := PEdgePoint(Item2)^;
  doubleResult := Point2.X - Point1.X;
  if doubleResult > 0 then
  begin
    result := 1;
  end
  else if doubleResult < 0 then
  begin
    result := -1
  end
  else
  begin
    doubleResult := Point2.Y - Point1.Y;
    if doubleResult > 0 then
    begin
      result := 1;
    end
    else if doubleResult < 0 then
    begin
      result := -1
    end
    else
    begin
      result := SortByEdgePostion(Point1, Point2);
    end;
  end;
end;

function Distance(const Point1, Point2: TEdgePoint): real; inline;
begin
  result := Sqrt(sqr(Point1.X - Point2.X) + sqr(Point1.Y - Point2.Y));
end;

function MidPoint(const Point1, Point2: TEdgePoint): TEdgePoint; inline;
begin
  result.X := (Point1.X + Point2.X)/2;
  result.Y := (Point1.Y + Point2.Y)/2;
end;

function YIntersection(const Point1, Point2: TEdgePoint; const X: real):
  real;
var
  Slope: real;
  Intercept: real;
begin
  // Point1 and Point2 define a non-vertical line.  Given X, return the
  // Y value for this point.
  Assert(Point1.X <> Point2.X);
  Slope := (Point2.Y - Point1.Y) / (Point2.X - Point1.X);
  Intercept := Point1.Y - Slope * Point1.X;
  result := Slope * X + Intercept;
end;

function YIntersection(const Point1, Point2: TPoint2D;
  const X: real): real; overload;
var
  Slope: real;
  Intercept: real;
begin
  // Point1 and Point2 define a non-vertical line.  Given X, return the
  // Y value for this point.
  Assert(Point1.X <> Point2.X);
  Slope := (Point2.Y - Point1.Y) / (Point2.X - Point1.X);
  Intercept := Point1.Y - Slope * Point1.X;
  result := Slope * X + Intercept;
end;

function XIntersection(const Point1, Point2: TEdgePoint; const Y: real):
  real;
var
  // Point1 and Point2 define a non-horizontal line.  Given Y, return the
  // X value for this point along the line.
  Slope: real;
  Intercept: real;
begin
  Assert(Point1.Y <> Point2.Y);
  Slope := (Point2.X - Point1.X) / (Point2.Y - Point1.Y);
  Intercept := Point1.X - Slope * Point1.Y;
  result := Slope * Y + Intercept;
end;

procedure UpdateUsedVariables(VariableList, DataSetList: TList;
  LayerIndex, RowIndex, ColIndex: Integer);
var
  VarIndex: Integer;
  AnotherDataSet: TDataArray;
  Variable: TCustomValue;
begin
  for VarIndex := 0 to VariableList.Count - 1 do
  begin
    Variable := VariableList[VarIndex];
    AnotherDataSet := DataSetList[VarIndex];
    if AnotherDataSet <> nil then
    begin
      case AnotherDataSet.Orientation of
        dsoTop:
          begin
            LayerIndex := 0;
          end;
        dsoFront:
          begin
            RowIndex := 0;
          end;
        dsoSide:
          begin
            ColIndex := 0;
          end;
        dso3D:
          begin
          end;
      else
        Assert(False);
      end;
      case Variable.ResultType of
        rdtDouble:
          begin
            TRealVariable(Variable).Value := AnotherDataSet.RealData[LayerIndex, RowIndex, ColIndex];
          end;
        rdtInteger:
          begin
            TIntegerVariable(Variable).Value := AnotherDataSet.IntegerData[LayerIndex, RowIndex, ColIndex];
          end;
        rdtBoolean:
          begin
            TBooleanVariable(Variable).Value := AnotherDataSet.BooleanData[LayerIndex, RowIndex, ColIndex];
          end;
        rdtString:
          begin
            TStringVariable(Variable).Value := AnotherDataSet.StringData[LayerIndex, RowIndex, ColIndex];
          end;
      else
        Assert(False);
      end;
    end;
  end;
end;

procedure InitializeUsedDataSets(Model: TPhastModel; Compiler: TRbwParser;
  const Expression: TExpression; VariableList, DataSetList: TList);
var
  VarIndex: integer;
  VarName: string;
  VarPosition: integer;
  Variable: TCustomValue;
  AnotherDataSet: TDataArray;
  UsedVariables: TStringList;
begin
  UsedVariables := TStringList.Create;
  try
    UsedVariables.Assign(Expression.VariablesUsed);

    for VarIndex := 0 to UsedVariables.Count - 1 do
    begin
      VarName := UsedVariables[VarIndex];
      VarPosition := Compiler.IndexOfVariable(VarName);
      Variable := Compiler.Variables[VarPosition];
      VariableList.Add(Variable);
      AnotherDataSet := Model.GetDataSetByName(VarName);
      if AnotherDataSet = nil then
      begin
        DataSetList.Add(nil);
      end
      else
      begin
        DataSetList.Add(AnotherDataSet);
        Assert(AnotherDataSet.DataType = Variable.ResultType);
        AnotherDataSet.Initialize;
        Model.AddDataSetToCache(AnotherDataSet);
      end;
    end;
  finally
    UsedVariables.Free;
  end;
end;

{ TSubPolygon }

procedure TSubPolygon.BoxIntersect(const Point1, Point2: TPoint2D;
  SubPolygons: TList);
var
  BoxMaxX, BoxMinX, BoxMaxY, BoxMinY: double;
begin
  if Point1.x > Point2.x then
  begin
    BoxMaxX := Point1.x;
    BoxMinX := Point2.x;
  end
  else
  begin
    BoxMaxX := Point2.x;
    BoxMinX := Point1.x;
  end;
  if Point1.Y > Point2.Y then
  begin
    BoxMaxY := Point1.Y;
    BoxMinY := Point2.Y;
  end
  else
  begin
    BoxMaxY := Point2.Y;
    BoxMinY := Point1.Y;
  end;
  InternalBoxIntersect(SubPolygons, BoxMinX, BoxMaxX, BoxMinY, BoxMaxY);
end;

procedure TSubPolygon.InternalBoxIntersect(SubPolygons: TList;
  const BoxMinX, BoxMaxX, BoxMinY, BoxMaxY: Double);
begin
  if (BoxMaxX >= FMinX) and (BoxMinX <= FMaxX)
    and (BoxMaxY >= FMinY) and (BoxMinY <= FMaxY) then
  begin
    if FSubPolygon1 = nil then
    begin
      SubPolygons.Add(self);
    end
    else
    begin
      FSubPolygon1.InternalBoxIntersect(SubPolygons,
        BoxMinX, BoxMaxX, BoxMinY, BoxMaxY);
      FSubPolygon2.InternalBoxIntersect(SubPolygons,
        BoxMinX, BoxMaxX, BoxMinY, BoxMaxY);
    end;
  end;
end;

procedure TSubPolygon.SetMaxAndMinFromSubPolygons;
begin
  // Determine the Maximum Y value, Minimum Y value and
  // Maximum X value by comparing those of the two subpolygons.
  FMaxY := Max(FSubPolygon1.FMaxY, FSubPolygon2.FMaxY);
  FMinY := Min(FSubPolygon1.FMinY, FSubPolygon2.FMinY);
  FMaxX := Max(FSubPolygon1.FMaxX, FSubPolygon2.FMaxX);
  FMinX := Min(FSubPolygon1.FMinX, FSubPolygon2.FMinX);
end;

//constructor TSubPolygon.Copy(Source: TSubPolygon);
//begin
//  Copy(Source, True);
//end;

//constructor TSubPolygon.Copy(Source: TSubPolygon; New: boolean);
//begin
//  inherited Create;
//  FOriginalCount := Source.FOriginalCount;
//  FCount := Source.FCount;
//  FMaxX := Source.FMaxX;
//  FMaxY := Source.FMaxY;
//  FMinX := Source.FMinX;
//  FMinY := Source.FMinY;
//  FStart := Source.FStart;
//  FPoints := Source.FPoints;
//  if New then
//  begin
//    SetLength(FPoints, Length(FPoints));
//  end;
//  FSectionIndex := Source.FSectionIndex;
//  if Source.FSubPolygon1 <> nil then
//  begin
//    FSubPolygon1 := Copy(Source.FSubPolygon1, False);
//    FSubPolygon2 := Copy(Source.FSubPolygon2, False);
//  end;
//end;

constructor TSubPolygon.Create(const Points: TRealPointArray;
  const Count, Start, Section: integer);
begin
  FSectionIndex := Section;
  FOriginalCount := Count;
  Assert(Start + Count -1 < Length(Points));
  // Each subpolygon must store where in the array its data starts,
  // how many points it has and the Maximum Y value, Minimum Y value and
  // Maximum X value.
  // Store it's starting point and count.
  FStart := Start;
  FCount := Count;
  FPoints := Points;
  if Count > MaxPointsInSubPolygon then
  begin
    CreateSubPolygons(Points, Count, Start, Section);
  end
  else
  begin
    SetMaxAndMinWhenNoSubPolygons(Count, Start, Points);
  end;
end;

destructor TSubPolygon.Destroy;
begin
  // Destroy the subpolygons if they exist.
  FSubPolygon1.Free;
  FSubPolygon2.Free;
  inherited;
end;

procedure TSubPolygon.GrowByOne;
begin
  Inc(FCount);
  if (FCount > MaxPointsInSubPolygon) then
  begin
    if FSubPolygon2 = nil then
    begin
      FOriginalCount := FCount;
      CreateSubPolygons(FPoints, FCount, FStart, FSectionIndex);
    end
    else if (FCount > 2*FOriginalCount)
      or (FSubPolygon1.FCount*2 < FSubPolygon2.FCount) then
    begin
      FSubPolygon1.Free;
      FSubPolygon2.Free;
      FOriginalCount := FCount;
      CreateSubPolygons(FPoints, FCount, FStart, FSectionIndex);
    end
    else
    begin
      FSubPolygon2.GrowByOne;
      SetMaxAndMinFromSubPolygons;
    end;
  end
  else
  begin
    SetMaxAndMinWhenNoSubPolygons(FCount, FStart, FPoints);
  end;
end;

procedure TSubPolygon.SetMaxAndMinWhenNoSubPolygons(const Count, Start: Integer;
  const Points: TRealPointArray);
var
  Index: Integer;
  Temp: Real;
begin
  // If the subpolygons are small enough, determine the Maximum Y value,
  // Minimum Y value and Maximum X values directly.
  FMaxX := Points[Start].X;
  FMinX := FMaxX;
  FMaxY := Points[Start].Y;
  FMinY := FMaxY;
  for Index := 1 to Count - 1 do
  begin
    Temp := Points[Start + Index].Y;
    if Temp > FMaxY then
    begin
      FMaxY := Temp;
    end
    else if Temp < FMinY then
    begin
      FMinY := Temp;
    end;
    Temp := Points[Start + Index].X;
    if Temp > FMaxX then
    begin
      FMaxX := Temp;
    end
    else if Temp < FMinX then
    begin
      FMinX := Temp;
    end;
  end;
end;

procedure TSubPolygon.CreateSubPolygons(const Points: TRealPointArray;
  const Count, Start, Section: Integer);
begin
  // If the number of points is too big, create additional subpolygons
  // that each store half the points.  The two subpolygons overlap at
  // one vertext.
  FSubPolygon1 := TSubPolygon.Create(Points, Count div 2 + 1, Start, Section);
  FSubPolygon2 := TSubPolygon.Create(Points, Count - FSubPolygon1.FCount + 1,
    Start + FSubPolygon1.FCount - 1, Section);
  SetMaxAndMinFromSubPolygons;
end;

{ TScreenObject }

function TScreenObject.AddBoundaryDataSet(const DataSet: TDataArray): integer;
var
  Subscription: TObserver;
begin
  CreateBoundaryDataSets;
  result := FBoundaryDataSets.IndexOf(DataSet);
  if result < 0 then
  begin
    Subscription := nil;
    if FCanInvalidateModel then
    begin

      InvalidateModel;
      Subscription := TObserver.Create(nil);
      Subscription.UpdateWithName(DataSet.Name + Name);
      CreateBoundaryDataSetSubscriptions;
      FBoundaryDataSetSubscriptions.Add(Subscription);
    end;
    result := FBoundaryDataSets.Add(DataSet);
    CreateBoundaryDataSetFormulas;
    FBoundaryDataSetFormulas.Add(nil);
    if FCanInvalidateModel then
    begin
      self.TalksTo(DataSet);
      Subscription.TalksTo(DataSet);
      self.TalksTo(Subscription);
      case ElevationCount of
        ecZero:
          begin
            // do nothing
          end;
        ecOne:
          begin
            if not (csReading in FModel.ComponentState) then
            begin
              if FElevSubscription = nil then
              begin
                CreateElevationSubscription;
              end;
              FElevSubscription.TalksTo(DataSet);
            end;
          end;
        ecTwo:
          begin
            if not (csReading in FModel.ComponentState) then
            begin
              if FTopElevSubscription = nil then
              begin
                CreateTopElevationSubscription
              end;
              FTopElevSubscription.TalksTo(DataSet);

              if FBottomElevSubscription = nil then
              begin
                CreateBottomElevationSubscription
              end;
              FBottomElevSubscription.TalksTo(DataSet);
            end;
          end;
      else
        Assert(False);
      end;
      DataSet.Invalidate;
    end;
  end;
end;

function TScreenObject.CanAddDataSet(const DataSet: TDataArray): boolean;
begin
  result := True;
  if DataSet.Orientation <> dso3D then
  begin
    case ViewDirection of
      vdTop:
        begin
          if DataSet.Orientation <> dsoTop then
          begin
            result := False;
          end;
        end;
      vdFront:
        begin
          if DataSet.Orientation <> dsoFront then
          begin
            result := False;
          end;
        end;
      vdSide:
        begin
          if DataSet.Orientation <> dsoSide then
          begin
            result := False;
          end;
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TScreenObject.AddPoint(const APoint: TPoint2D; NewPart: boolean);
var
  IntersectPoint: TPoint2D;
  Epsilon: real;
  SubPolygonList: TList;
  SubIndex: Integer;
  SubPolygon: TSubPolygon;
  PointIndex: Integer;
  StartIndex: Integer;
  function NearlyTheSame(const A, B: real): boolean;
  begin
    result := A = B;
    if not result then
    begin
      result := Abs(A - B) / (Abs(A) + Abs(B)) < Epsilon;
    end;
  end;
  function PointsNearlyTheSame(Point1, Point2: TPoint2D): boolean;
  begin
    result := NearlyTheSame(Point1.X, Point2.X) and
      NearlyTheSame(Point1.Y, Point2.Y);
  end;
begin
  BeginUpdate;
  try
    FIsClosedCached := False;
    if NewPart and (Count > 0) then
    begin
      SectionStarts.Add;
      SectionStarts.IntValues[SectionStarts.Count -1] := Count;
      DestroyLastSubPolygon;
    end;
    // Check to make sure that this won't
    // cause the screen object to intersect itself.
    if SectionLength[SectionCount-1] >= 2 then
    begin
      if FLastSubPolygon = nil then
      begin
        CreateLastSubPolygon;
      end;
      SubPolygonList := TList.Create;
      try
        Epsilon := Abs(FMaxX);
        Epsilon := Max(Epsilon, Abs(FMinX));
        Epsilon := Max(Epsilon, Abs(FMaxY));
        Epsilon := Max(Epsilon, Abs(FMinY));
        Epsilon := Epsilon / 1E14;

        FLastSubPolygon.BoxIntersect(Points[Count - 1], APoint, SubPolygonList);
        for SubIndex := 0 to SubPolygonList.Count - 1 do
        begin
          SubPolygon := SubPolygonList[SubIndex];
          for PointIndex := 0 to SubPolygon.FCount - 2 do
          begin
            StartIndex := SubPolygon.FStart + PointIndex;
            if StartIndex >= Count-2 then
            begin
              break;
            end;
            if Intersection(Points[Count - 1], APoint, StartIndex,
              IntersectPoint) = irDoIntersect then
            begin
              if not PointsNearlyTheSame(IntersectPoint, Points[Count - 1])
                and not PointsNearlyTheSame(IntersectPoint,
                Points[FLastSubPolygon.FStart]) then
              begin
                raise EScreenObjectError.Create('Invalid vertex (#'
                  + IntToStr(Count+1)
                  + ') in "' + Name + '".');
              end;
            end;
          end;
        end;
      finally
        SubPolygonList.Free;
      end;
    end;
    // Check to make sure that this won't
    // cause the screen object to intersect itself.
  //  if (SectionLength[SectionCount-1] >= 2) then
  //  begin
  //    if Intersection(Points[Count - 1], APoint, IntersectPoint) = irDoIntersect
  //      then
  //    begin
  //      Epsilon := 0;
  //      for Index := 0 to Count - 1 do
  //      begin
  //        TempPoint := Points[Index];
  //        Epsilon := Max(Epsilon, Abs(TempPoint.X));
  //        Epsilon := Max(Epsilon, Abs(TempPoint.Y));
  //      end;
  //      Epsilon := Epsilon / 1E14;
  //
  //      if not PointsNearlyTheSame(IntersectPoint, Points[Count - 1])
  //        and not PointsNearlyTheSame(IntersectPoint, Points[SectionStart[SectionCount-1]]) then
  //      begin
  //        raise EScreenObjectError.Create('Invalid vertex (#' + IntToStr(Count+1)
  //          + ') in "' + Name + '".');
  //      end;
  //    end;
  //  end;

    // Check that the array is big enough.  If it isn't, make it bigger.
    if FCount = FCapacity then
    begin
      Grow;
    end;
    FPoints[FCount] := APoint;
    FSelectedVertices[FCount] := False;
    Inc(FCount);
    if FLastSubPolygon <> nil then
    begin
      FLastSubPolygon.GrowByOne;
    end;

    // update the variables that stores the extent of the screen object.
    if FCount = 1 then
    begin
      FMaxX := APoint.X;
      FMinX := APoint.X;
      FMaxY := APoint.Y;
      FMinY := APoint.Y;
      FRecalculateBox := False
    end
    else if not FRecalculateBox then
    begin
      // there is no point in doing this if RecalculateBox indicates
      // that the values are invalid anyway.  Instead, defer recalculating them
      // until they are needed.
      if APoint.X > FMaxX then
      begin
        FMaxX := APoint.X;
      end
      else if APoint.X < FMinX then
      begin
        FMinX := APoint.X;
      end;

      if APoint.Y > FMaxY then
      begin
        FMaxY := APoint.Y;
      end
      else if APoint.Y < FMinY then
      begin
        FMinY := APoint.Y;
      end;
    end;

    // indicate that the screen object has
    // changed so internal data will be updated
    // when it is needed.
  //  Invalidate;
  finally
    EndUpdate;
  end;

end;

procedure TScreenObject.Assign(Source: TPersistent);
var
  Index: integer;
  AScreenObject: TScreenObject;
  DataSetIndex: Integer;
  DataArray: TDataArray;
  DSIndex: Integer;
  AssignModelToBoundary: boolean;
  DS_Index: Integer;
  DataSet: TDataArray;
  BoundarArray: TDataArray;
begin
  if Source is TScreenObject then
  begin
    AScreenObject := TScreenObject(Source);
  end
  else
  begin
    inherited Assign(Source);
    Exit;
  end;
  if not FCanInvalidateModel then
  begin
    FModel := AScreenObject.FModel;
  end;

  // copy the data of the other screen object.
  CellSize := AScreenObject.CellSize;
  CellSizeUsed := AScreenObject.CellSizeUsed;
  EvaluatedAt := AScreenObject.EvaluatedAt;
  Visible := AScreenObject.Visible;
  Capacity := AScreenObject.Capacity;
  Count := AScreenObject.Count;
  Name := AScreenObject.Name;
  FViewDirection := AScreenObject.FViewDirection;
  MoveToPoints(AScreenObject.FPoints);
  MoveToSelectedPoints(AScreenObject.FSelectedVertices);

  FillColor := AScreenObject.FillColor;
  LineColor := AScreenObject.LineColor;
  SetValuesOfEnclosedCells := AScreenObject.SetValuesOfEnclosedCells;
  SetValuesOfIntersectedCells :=
    AScreenObject.SetValuesOfIntersectedCells;
  SetValuesByInterpolation := AScreenObject.SetValuesByInterpolation;
  ColorLine := AScreenObject.ColorLine;
  FillScreenObject := AScreenObject.FillScreenObject;
  ImportedValues := AScreenObject.ImportedValues;

  for Index := DataSetCount - 1 downto 0 do
  begin
    DataSet := DataSets[Index];
    if AScreenObject.IndexOfDataSet(DataSet) < 0 then
    begin
      DeleteDataSet(Index);
    end;
  end;

  for Index := 0 to AScreenObject.DataSetCount - 1 do
  begin
    DataSet := AScreenObject.DataSets[Index];
    DS_Index := IndexOfDataSet(DataSet);
    if DS_Index < 0 then
    begin
      DS_Index := AddDataSet(DataSet);
    end;
    DataSetFormulas[DS_Index] := AScreenObject.DataSetFormulas[Index];
  end;

  ElevationCount := AScreenObject.ElevationCount;
  ElevationFormula := AScreenObject.ElevationFormula;
  HigherElevationFormula := AScreenObject.HigherElevationFormula;
  LowerElevationFormula := AScreenObject.LowerElevationFormula;

  Selected := AScreenObject.Selected;
  if Selected and (AScreenObject.SelectedVertexCount > 0) then
  begin
    for Index := 0 to Count - 1 do
    begin
      SelectedVertices[Index] := AScreenObject.SelectedVertices[Index];
    end;
  end
  else
  begin
    ClearSelectedVertices;
  end;

  // All PHAST boundaries are evaluated at nodes.
  InterpValues := AScreenObject.InterpValues;
  if AScreenObject.EvaluatedAt = eaNodes then
  begin
    FluxBoundary := AScreenObject.FluxBoundary;
  end
  else
  begin
    FluxBoundary.Clear;
  end;

  AssignModelToBoundary := (LeakyBoundary.FModel = nil);
  if AssignModelToBoundary then
  begin
    LeakyBoundary.FModel := AScreenObject.LeakyBoundary.FModel;
  end;
  if AScreenObject.EvaluatedAt = eaNodes then
  begin
    LeakyBoundary := AScreenObject.LeakyBoundary;
  end
  else
  begin
    LeakyBoundary.Clear
  end;
  if AssignModelToBoundary then
  begin
    LeakyBoundary.FModel := nil;
  end;

  AssignModelToBoundary := (RiverBoundary.FModel = nil);
  if AssignModelToBoundary then
  begin
    RiverBoundary.FModel := AScreenObject.RiverBoundary.FModel;
  end;
  if AScreenObject.EvaluatedAt = eaNodes then
  begin
    RiverBoundary := AScreenObject.RiverBoundary;
  end
  else
  begin
    RiverBoundary.Clear;
  end;
  if AssignModelToBoundary then
  begin
    RiverBoundary.FModel := nil;
  end;

  AssignModelToBoundary := (SpecifiedHeadBoundary.FModel = nil);
  if AssignModelToBoundary then
  begin
    SpecifiedHeadBoundary.FModel := AScreenObject.SpecifiedHeadBoundary.FModel;
  end;
  if AScreenObject.EvaluatedAt = eaNodes then
  begin
    SpecifiedHeadBoundary := AScreenObject.SpecifiedHeadBoundary;
  end
  else
  begin
    SpecifiedHeadBoundary.Clear;
  end;
  if AssignModelToBoundary then
  begin
    SpecifiedHeadBoundary.FModel := nil;
  end;

  if AScreenObject.EvaluatedAt = eaNodes then
  begin
    SpecifiedSolutionBoundary := AScreenObject.SpecifiedSolutionBoundary;
  end
  else
  begin
    SpecifiedSolutionBoundary.Clear;
  end;

  if AScreenObject.EvaluatedAt = eaNodes then
  begin
    WellBoundary := AScreenObject.WellBoundary;
  end
  else
  begin
    WellBoundary.Clear;
  end;

  if not FCanInvalidateModel then
  begin
    FModel := nil;
  end;

  ModflowChdBoundary := AScreenObject.ModflowChdBoundary;
  ModflowGhbBoundary := AScreenObject.ModflowGhbBoundary;
  ModflowWellBoundary := AScreenObject.ModflowWellBoundary;
  ModflowRivBoundary := AScreenObject.ModflowRivBoundary;
  ModflowDrnBoundary := AScreenObject.ModflowDrnBoundary;
  ModflowDrtBoundary := AScreenObject.ModflowDrtBoundary;
  ModflowRchBoundary := AScreenObject.ModflowRchBoundary;
  ModflowEvtBoundary := AScreenObject.ModflowEvtBoundary;
  ModflowEtsBoundary := AScreenObject.ModflowEtsBoundary;
  ModflowResBoundary := AScreenObject.ModflowResBoundary;
  ModflowLakBoundary := AScreenObject.ModflowLakBoundary;
  ModflowSfrBoundary := AScreenObject.ModflowSfrBoundary;
  ModflowUzfBoundary := AScreenObject.ModflowUzfBoundary;
  ModflowHeadObservations := AScreenObject.ModflowHeadObservations;
  ModflowHfbBoundary := AScreenObject.ModflowHfbBoundary;
  ModflowStreamGage := AScreenObject.ModflowStreamGage;
  ModflowMnw2Boundary := AScreenObject.ModflowMnw2Boundary;

  if not FCanInvalidateModel then
  begin
    FModel := AScreenObject.FModel;
  end;
  
  SectionStarts := AScreenObject.SectionStarts;

  FRecalculateBox := True;
  Invalidate;

  IFACE := AScreenObject.IFACE;
  ModpathParticles := AScreenObject.ModpathParticles;

  for DataSetIndex := DataSetCount -1 downto 0 do
  begin
    DataArray := DataSets[DataSetIndex];
    if AScreenObject.IndexOfDataSet(DataArray) < 0 then
    begin
      DeleteDataSet(DataSetIndex);
    end;
  end;

  for DataSetIndex := 0 to AScreenObject.DataSetCount - 1 do
  begin
    DataArray := AScreenObject.DataSets[DataSetIndex];
    DSIndex := IndexOfDataSet(DataArray);
    if DSIndex < 0 then
    begin
      DSIndex := AddDataSet(DataArray);
    end;
    DataSetFormulas[DSIndex] := AScreenObject.DataSetFormulas[DataSetIndex];
  end;

  for DataSetIndex := BoundaryDataSetCount -1 downto 0 do
  begin
    DataArray := BoundaryDataSets[DataSetIndex];
    if AScreenObject.IndexOfBoundaryDataSet(DataArray) < 0 then
    begin
      DeleteBoundaryDataSet(DataSetIndex);
    end;
  end;

  for DataSetIndex := 0 to AScreenObject.BoundaryDataSetCount - 1 do
  begin
    DataArray := AScreenObject.BoundaryDataSets[DataSetIndex];
    DSIndex := IndexOfBoundaryDataSet(DataArray);
    if DSIndex < 0 then
    begin
      DSIndex := AddBoundaryDataSet(DataArray);
    end;
    BoundaryDataSetFormulas[DSIndex] :=
      AScreenObject.BoundaryDataSetFormulas[DataSetIndex];
  end;

  BoundarArray :=
    frmGoPhast.PhastModel.GetDataSetByName(rsModflowSpecifiedHead);
  if (ModflowChdBoundary <> nil)
    and ModflowChdBoundary.Used then
  begin
    DS_Index := IndexOfDataSet(BoundarArray);
    if DS_Index < 0 then
    begin
      DS_Index := AddDataSet(BoundarArray);
    end;
    DataSetFormulas[DS_Index] := 'True';
  end
  else
  begin
    DS_Index := IndexOfDataSet(BoundarArray);
    if DS_Index >= 0 then
    begin
      DeleteDataSet(DS_Index);
    end;
  end;

  BoundarArray :=
    frmGoPhast.PhastModel.GetDataSetByName(StrUzfGage_1_and_2);
  if (ModflowUzfBoundary <> nil)
    and ModflowUzfBoundary.Used
    and (ModflowUzfBoundary.GageOption1 <> 0) then
  begin
    DS_Index := IndexOfDataSet(BoundarArray);
    if DS_Index < 0 then
    begin
      DS_Index := AddDataSet(BoundarArray);
    end;
    DataSetFormulas[DS_Index] := IntToStr(ModflowUzfBoundary.GageOption1);
  end
  else
  begin
    DS_Index := IndexOfDataSet(BoundarArray);
    if DS_Index >= 0 then
    begin
      DeleteDataSet(DS_Index);
    end;
  end;

  BoundarArray :=
    frmGoPhast.PhastModel.GetDataSetByName(StrUzfGage3);
  if (ModflowUzfBoundary <> nil)
    and ModflowUzfBoundary.Used
    and (ModflowUzfBoundary.GageOption2 <> 0) then
  begin
    DS_Index := IndexOfDataSet(BoundarArray);
    if DS_Index < 0 then
    begin
      DS_Index := AddDataSet(BoundarArray);
    end;
    DataSetFormulas[DS_Index] := IntToStr(ModflowUzfBoundary.GageOption2);
  end
  else
  begin
    DS_Index := IndexOfDataSet(BoundarArray);
    if DS_Index >= 0 then
    begin
      DeleteDataSet(DS_Index);
    end;
  end;

  if not FCanInvalidateModel then
  begin
    FModel := nil;
  end;
end;

procedure TScreenObject.Assign3DElevationsFromFront(const Compiler: TRbwParser;
  const SparseArray: T3DSparseRealArray);
var
  TempMinX, TempMaxX: double;
  FirstLayer, LastLayer, FirstCol, LastCol, FirstRow, LastRow: integer;
  ColIndex, LayerIndex: integer;
  CellLocation3D: T3DRealPoint;
  Expression: TExpression;
  SegmentIndex: integer;
  ASegment: TCellElementSegment;
  RowIndex: Integer;
  Grid: TCustomGrid;
  SectionIndex: integer;
  TempImportedElevations : TValueArrayStorage;
  VariableList, DataSetList: TList;
  Temp: TValueArrayStorage;
begin
  VariableList := TList.Create;
  DataSetList := TList.Create;
  TempImportedElevations := CurrentValues;
  try
  //  Assert(EvaluatedAt = eaBlocks);
    Expression := Compiler.CurrentExpression;
    Temp := FCurrentValues;
    try
      InitializeUsedDataSets(FModel as TPhastModel, Compiler, Expression,
        VariableList, DataSetList);
    finally
      FCurrentValues := Temp;
    end;
    if (SetValuesOfEnclosedCells or SetValuesOfIntersectedCells)
      and Closed then
    begin

      // Get the coordinates of the points.
      TempMinX := MinX;
      TempMaxX := MaxX;

      Grid := (FModel as TPhastModel).Grid;
      GetColumns(Grid, TempMinX, TempMaxX,
        FirstCol, LastCol);

      if FirstCol > Grid.ColumnCount then
      begin
        FirstCol := Grid.ColumnCount;
      end;
      if LastCol > Grid.ColumnCount then
      begin
        LastCol := Grid.ColumnCount;
      end;

      FirstLayer := -1;
      LastLayer := -1;
      case EvaluatedAt of
        eaBlocks:
          begin
            FirstLayer := 0;
            LastLayer := Grid.LayerCount -1;
          end;
        eaNodes:
          begin
            FirstLayer := 0;
            LastLayer := Grid.LayerCount;
          end;
        else Assert(False);
      end;
      FirstRow := -1;
      LastRow := -1;
      case EvaluatedAt of
        eaBlocks:
          begin
            FirstRow := 0;
            LastRow := Grid.RowCount -1;
          end;
        eaNodes:
          begin
            FirstRow := 0;
            LastRow := Grid.RowCount;
          end;
        else Assert(False);
      end;

      // Find the cells inside the screen object and assign values to them.
      Assert((FirstCol >= 0) and (LastCol >= 0)
        and (FirstLayer >= 0) and (LastLayer >= 0));
      for RowIndex := FirstRow to LastRow do
      begin
        for ColIndex := FirstCol to LastCol do
        begin
          for LayerIndex := FirstLayer to LastLayer do
          begin
            case EvaluatedAt of
              eaBlocks:
                begin
                  CellLocation3D := Grid.ThreeDElementCenter(
                    ColIndex, RowIndex, LayerIndex);
                end;
              eaNodes:
                begin
                  CellLocation3D := Grid.ThreeDElementCorner(
                    ColIndex, RowIndex, LayerIndex);
                end;
            else
              Assert(False);
            end;

            if IsPointInside(CellLocation3D.X, CellLocation3D.Z,
              SectionIndex) then
            begin
              Temp := FCurrentValues;
              try
                UpdateUsedVariables(VariableList, DataSetList,
                  LayerIndex, RowIndex, ColIndex);
              finally
                FCurrentValues := Temp;
              end;
              UpdateCurrentScreenObject(self);
              UpdateGlobalLocations(ColIndex, RowIndex,
                LayerIndex, EvaluatedAt);
              UpdateCurrentSection(SectionIndex);
              FCurrentValues := TempImportedElevations;
              Expression.Evaluate;

              SparseArray.Items[LayerIndex, RowIndex, ColIndex] :=
                Expression.DoubleResult;
            end;
          end;
        end
      end;
    end;
    if SetValuesOfIntersectedCells then
    begin
      if not Segments.UpToDate then
      begin
        UpdateFrontSegments((FModel as TPhastModel).Grid, EvaluatedAt);
      end;
      // Assign values here.
      for SegmentIndex := 0 to Segments.Count - 1 do
      begin
        ASegment := Segments[SegmentIndex];
        begin
          UpdateCurrentSegment(ASegment);

          if not SparseArray.IsValue[ASegment.Layer, ASegment.Row,
            ASegment.Col] then
          begin
            Temp := FCurrentValues;
            try
              UpdateUsedVariables(VariableList, DataSetList,
                ASegment.Layer, ASegment.Row, ASegment.Col);
            finally
              FCurrentValues := Temp;
            end;
            UpdateCurrentScreenObject(self);
            UpdateCurrentSegment(ASegment);
            UpdateGlobalLocations(ASegment.Col, ASegment.Row, ASegment.Layer,
              EvaluatedAt);
            UpdateCurrentSection(ASegment.SectionIndex);
            FCurrentValues := TempImportedElevations;
            Expression.Evaluate;

            SparseArray.Items[ASegment.Layer, ASegment.Row, ASegment.Col] :=
              Expression.DoubleResult;
          end;
        end;
      end;
    end;
  finally
    FCurrentValues := TempImportedElevations;
    VariableList.Free;
    DataSetList.Free;
  end;
end;

procedure TScreenObject.Assign3DElevationsFromTop(const Compiler: TRbwParser;
  const SparseArray: T3DSparseRealArray);
var
  TempMinX, TempMaxX: double;
  FirstLayer, LastLayer, FirstCol, LastCol, FirstRow, LastRow: integer;
  ColIndex, LayerIndex: integer;
  CellLocation3D: T3DRealPoint;
  Expression: TExpression;
  SegmentIndex: integer;
  ASegment: TCellElementSegment;
  RowIndex: Integer;
  Grid: TCustomGrid;
  SectionIndex: integer;
  TempMinY: double;
  TempMaxY: double;
  RotatedPoints: TEdgePointArray;
  APoint: TPoint2D;
  VariableList, DataSetList: TList;
  Temp: TValueArrayStorage;
begin
  VariableList := TList.Create;
  DataSetList := TList.Create;
  try
  Expression := Compiler.CurrentExpression;
  Temp := FCurrentValues;
  try
    InitializeUsedDataSets(FModel as TPhastModel, Compiler, Expression,
      VariableList, DataSetList);
  finally
    FCurrentValues := Temp;
  end;
  if (SetValuesOfEnclosedCells or SetValuesOfIntersectedCells)
    and Closed then
  begin
    // Get the coordinates of the points.

    Grid := (FModel as TPhastModel).Grid;
    RotatePoints(Grid, RotatedPoints, TempMinX, TempMinY, TempMaxX, TempMaxY);
    GetColumns(Grid, TempMinX, TempMaxX,
      FirstCol, LastCol);

    if FirstCol > Grid.ColumnCount then
    begin
      FirstCol := Grid.ColumnCount;
    end;
    if LastCol > Grid.ColumnCount then
    begin
      LastCol := Grid.ColumnCount;
    end;

    FirstLayer := -1;
    LastLayer := -1;
    case EvaluatedAt of
      eaBlocks:
        begin
          FirstLayer := 0;
          LastLayer := Grid.LayerCount -1;
        end;
      eaNodes:
        begin
          FirstLayer := 0;
          LastLayer := Grid.LayerCount;
        end;
      else Assert(False);
    end;

    GetRows(Grid, TempMinY, TempMaxY, FirstRow, LastRow);
    if FirstRow > Grid.RowCount then
    begin
      FirstRow := Grid.RowCount;
    end;
    if LastRow > Grid.RowCount then
    begin
      LastRow := Grid.RowCount;
    end;

    // Find the cells inside the screen object and assign values to them.
    Assert((FirstCol >= 0) and (LastCol >= 0)
      and (FirstLayer >= 0) and (LastLayer >= 0));
    for RowIndex := FirstRow to LastRow do
    begin
      for ColIndex := FirstCol to LastCol do
      begin
        for LayerIndex := FirstLayer to LastLayer do
        begin
          case EvaluatedAt of
            eaBlocks:
              begin
                CellLocation3D := Grid.ThreeDElementCenter(
                  ColIndex, RowIndex, LayerIndex);
              end;
            eaNodes:
              begin
                CellLocation3D := Grid.ThreeDElementCorner(
                  ColIndex, RowIndex, LayerIndex);
              end;
          else
            Assert(False);
          end;

          APoint.x := CellLocation3D.X;
          APoint.y := CellLocation3D.Y;
          APoint := Grid.
            RotateFromGridCoordinatesToRealWorldCoordinates(APoint);

          if IsPointInside(APoint.x, APoint.y,
            SectionIndex) then
          begin
            Temp := FCurrentValues;
            try
              UpdateUsedVariables(VariableList, DataSetList,
                LayerIndex, RowIndex, ColIndex);
            finally
              FCurrentValues := Temp;
            end;
            UpdateCurrentScreenObject(self);
            UpdateGlobalLocations(ColIndex, RowIndex, LayerIndex, EvaluatedAt);
            UpdateCurrentSection(SectionIndex);
            Expression.Evaluate;

            SparseArray.Items[LayerIndex, RowIndex, ColIndex] :=
              Expression.DoubleResult;
          end;
        end;
      end
    end;
  end;
  if SetValuesOfIntersectedCells then
  begin
    if not Segments.UpToDate then
    begin
      UpdateTopSegments((FModel as TPhastModel).Grid, EvaluatedAt, False,
        RotatedPoints);
    end;
    // Assign values here.
    for SegmentIndex := 0 to Segments.Count - 1 do
    begin
      ASegment := Segments[SegmentIndex];
      begin
        UpdateCurrentSegment(ASegment);

        if not SparseArray.IsValue[ASegment.Layer, ASegment.Row,
          ASegment.Col] then
        begin
          Temp := FCurrentValues;
          try
            UpdateUsedVariables(VariableList, DataSetList,
              ASegment.Layer, ASegment.Row, ASegment.Col);
          finally
            FCurrentValues := Temp;
          end;
          UpdateCurrentScreenObject(self);
          UpdateCurrentSegment(ASegment);
          UpdateGlobalLocations(ASegment.Col, ASegment.Row, ASegment.Layer,
            EvaluatedAt);
          Expression.Evaluate;

          SparseArray.Items[ASegment.Layer, ASegment.Row, ASegment.Col] :=
            Expression.DoubleResult;
        end;
      end;
    end;
  end;
  finally
    VariableList.Free;
    DataSetList.Free;
  end;
end;

function TScreenObject.ZoomBox(VD: TViewDirection): TQrbwZoomBox2;
begin
  // use the correct zoombox depending on which way you are viewing the
  // screen object from.
  Assert(FModel <> nil);
  result := (FModel as TPhastModel).ZoomBox(VD);
end;

procedure TScreenObject.CalculateCanvasCoordinates;
var
  AZoomBox: TQrbwZoomBox2;
  PointIndex: integer;
  APoint: TPoint2D;
begin
  if not FRecalculateCoordinates then
    Exit;
  // use the correct zoombox depending on which way you are viewing the
  // screen object from.
  AZoomBox := ZoomBox(ViewDirection);
  SetLength(FCanvasCoordinates, Count);
  // calculate the coordinates.
  with AZoomBox do
  begin
    for PointIndex := 0 to Count - 1 do
    begin
      APoint := Points[PointIndex];
      with FCanvasCoordinates[PointIndex] do
      begin
        X := XCoord(APoint.X);
        Y := YCoord(APoint.Y);
      end;
    end;
  end;
  // Next time, we won't have to do this again.
  FRecalculateCoordinates := False;
end;

procedure GlobalRemoveScreenObjectDataArraySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TScreenObject).RemoveDataArraySubscription(Sender, AName);
end;

procedure GlobalRestoreDataArraySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TScreenObject).RestoreDataArraySubscription(Sender, AName);
end;  

procedure TScreenObject.ClearDataSets;
var
  Index: integer;
  ADataSet: TDataArray;
  Observer: TObserver;
  FormulaIndex: Integer;
  FormulaObject: TFormulaObject;
begin
  Assert(FDataSetMixtureSubscriptions.Count = DataSetCount);
  for Index := 0 to FDataSetMixtureSubscriptions.Count -1 do
  begin
    MixtureDataSetFormula[Index] := '0';
    ADataSet := DataSets[Index];
    Observer := FDataSetMixtureSubscriptions[Index] as TObserver;
    StopsTalkingTo(Observer);
    Observer.StopsTalkingTo(ADataSet);
  end;
  if not FIsUpdating then
  begin
    FInterpValues.Clear;
  end;

  FDataSetMixtureSubscriptions.Clear;

  Assert(FDataSets.Count = FDataSetSubscriptions.Count);
  for Index := 0 to FDataSets.Count - 1 do
  begin
    // Get rid of any subscriptions due to the formula.
    DataSetFormulas[Index] := '0';
    ADataSet := FDataSets[Index];
    if FElevSubscription <> nil then
    begin
      FElevSubscription.StopsTalkingTo(ADataSet);
    end;
    if FTopElevSubscription <> nil then
    begin
      FTopElevSubscription.StopsTalkingTo(ADataSet);
    end;
    if FBottomElevSubscription <> nil then
    begin
      FBottomElevSubscription.StopsTalkingTo(ADataSet);
    end;
    self.StopsTalkingTo(ADataSet);
    Observer := FDataSetSubscriptions[Index] as TObserver;
    Observer.StopsTalkingTo(ADataSet);
  end;

  UpToDate := False;
  FDataSets.Clear;
  for FormulaIndex := 0 to FDataSetFormulas.Count - 1 do
  begin
    FormulaObject := FDataSetFormulas[FormulaIndex];
    frmGoPhast.PhastModel.FormulaManager.Remove(FormulaObject,
      GlobalRemoveScreenObjectDataArraySubscription, GlobalRestoreDataArraySubscription, self);
  end;
  FDataSetFormulas.Clear;
  for Index := 0 to FDataSetSubscriptions.Count -1 do
  begin
    Observer := FDataSetSubscriptions[Index] as TObserver;
    self.StopsTalkingTo(Observer);
  end;

  FDataSetSubscriptions.Clear;
  InvalidateModel;
end;

procedure TScreenObject.ClearPoints;
begin
  // setting the capacity to 0 also sets the count to 0.
  // It also releases memory.
  FIsClosedCached := False;
  Capacity := 0;
  FRecalculateBox := True;
  Invalidate;
end;

procedure TScreenObject.UpdateScreenObjectWithName(const AName: string;
  const AViewDirection: TViewDirection;
  out UndoCreateScreenObject: TCustomUndo;
  const UndoAble: boolean = True);
begin
  inherited UpdateWithName(AName);
  // initialize the object.
  if UndoAble then
  begin
    UndoCreateScreenObject := TUndoCreateScreenObject.Create(self);
  end
  else
  begin
    UndoCreateScreenObject := nil;
  end;
  FRecalculateLength := True;
  FRecalculateArea := True;
  IncrementObjectsCreated;
  SetViewDirection(AViewDirection);
  Invalidate;

  // 1 elev
  if ElevationFormula = '' then
  begin
    ElevationFormula := '0';
  end;

  // 2 elev
  if HigherElevationFormula = '' then
  begin
    HigherElevationFormula := '0';
  end;
  if LowerElevationFormula = '' then
  begin
    LowerElevationFormula := '0';
  end;
end;

procedure TScreenObject.Changed(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    Invalidate;
  end;
  NotifyGuiOfChange(Sender);
end;

constructor TScreenObject.CreateWithViewDirection(const Model: TComponent;
  const AViewDirection: TViewDirection;
  out UndoCreateScreenObject: TCustomUndo;
  const UndoAble: boolean = True);
var
  AName: string;
begin
  AName := ObjectPrefix + IntToStr(ObjectsCreated);
  Name := AName;
  Create(Model);
  Name := '';
  UpdateScreenObjectWithName(AName, AViewDirection,
    UndoCreateScreenObject, UndoAble);
  if Model <> nil then
  begin
    (FModel as TPhastModel).ResetSelectedScreenObjects;
  end;
  Selected := True;
end;

procedure GlobalRemoveBoundaryDataArraySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TScreenObject).RemoveBoundaryDataArraySubscription(Sender, AName);
end;

procedure GlobalRestoreBoundaryDataArraySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TScreenObject).RestoreBoundaryDataArraySubscription(Sender, AName);
end;

procedure TScreenObject.DeleteBoundaryDataSet(const Index: Integer);
var
  DataSet: TDataArray;
  Subscription: TObserver;
  FormulaObject: TFormulaObject;
begin
  // Get rid of any subscriptions due to the formula.
  BoundaryDataSetFormulas[Index] := '0';

  DataSet := nil;
  if FCanInvalidateModel then
  begin
    Assert(FBoundaryDataSetSubscriptions <> nil);
    Subscription := FBoundaryDataSetSubscriptions[Index] as TObserver;
    DataSet := BoundaryDataSets[Index];
    self.StopsTalkingTo(DataSet);
    Subscription.StopsTalkingTo(DataSet);
    self.StopsTalkingTo(Subscription);
    FBoundaryDataSetSubscriptions.Delete(Index);
    DataSet.Invalidate;
  end;
  FBoundaryDataSets.Delete(Index);
  Assert(FBoundaryDataSetFormulas <> nil);
  FormulaObject := FBoundaryDataSetFormulas[Index];
  frmGoPhast.PhastModel.FormulaManager.Remove(FormulaObject,
    GlobalRemoveBoundaryDataArraySubscription, GlobalRestoreBoundaryDataArraySubscription, self);
  FBoundaryDataSetFormulas.Delete(Index);
  if FCanInvalidateModel then
  begin
    if FElevSubscription <> nil then
    begin
      FElevSubscription.StopsTalkingTo(DataSet);
    end;
    if FTopElevSubscription <> nil then
    begin
      FTopElevSubscription.StopsTalkingTo(DataSet);
    end;
    if FBottomElevSubscription <> nil then
    begin
      FBottomElevSubscription.StopsTalkingTo(DataSet);
    end;
  end;
  InvalidateModel;
end;

procedure TScreenObject.DeletePoint(const Index: Integer);
var
  VertexIsSelected: boolean;
  SectionIndex: Integer;
  Value: integer;
  NeedToDestroyLastSubPolygon: boolean;
begin
  FIsClosedCached := False;
  // make sure that the index is valid.  If it isn't, an exception will
  // be raised.
  ValidateIndex(Index);

  SectionIndex := SectionCount -1;
  NeedToDestroyLastSubPolygon := Index >= SectionStart[SectionIndex];

  // if we know the box around the screen object is invalid, there is no
  // point in doing checking whether deleting this point will make
  // the box invalid.
  if not FRecalculateBox then
  begin
    // see if this invalidates the range of the box around the screen object.
    if FPoints[Index].X = FMaxX then
    begin
      FRecalculateBox := True;
    end
    else if FPoints[Index].X = FMinX then
    begin
      FRecalculateBox := True;
    end;

    if FPoints[Index].Y = FMaxY then
    begin
      FRecalculateBox := True;
    end
    else if FPoints[Index].Y = FMinY then
    begin
      FRecalculateBox := True;
    end;
  end;
  InvalidateModel;

  if Index = Pred(FCount) then
  begin
    // If the point to be deleted is the last point, just reduce
    // the count by 1.
    Count := Pred(FCount);
  end
  else
  begin
    // Otherwise, you need to move the points to fill the space left
    // by the deleted point.
    Dec(FCount);
    Move(FPoints[Index + 1], FPoints[Index],
      (FCount - Index) * SizeOf(TPoint2D));
    // If the vertex that is being deleted is a selected vertex, then
    // it is also necessary to decrement the number of selected vertices.
    VertexIsSelected := False;
    if SelectedVertices[Index] then
    begin
      Dec(FSelectedVertexCount);
      VertexIsSelected := True;
    end;
    // Move the selected vertex information to fill the space left
    // by the deleted vertex.
    // However if nothing was selected, this can be skipped.
    if VertexIsSelected or (FSelectedVertexCount > 0) then
    begin
      Move(FSelectedVertices[Index + 1], FSelectedVertices[Index],
        (FCount - Index) * SizeOf(boolean));
    end;
  end;
  for SectionIndex := 0 to SectionStarts.Count - 1 do
  begin
    Value := SectionStarts.IntValues[SectionIndex];
    if Value > Index  then
    begin
      SectionStarts.IntValues[SectionIndex] := Value -1;
    end;
  end;
  DeleteExtraSections;
  if NeedToDestroyLastSubPolygon then
  begin
    DestroyLastSubPolygon;
  end;
  Invalidate;
end;

procedure TScreenObject.Draw3D;
var
  Red, Green, Blue: TGLubyte;
  Colors: array[0..3] of TGLfloat;
  GlGridValues: TGrid;
  LocalModel: TPhastModel;
  CellsToDraw: TSelectedCells;
  ColLimit: Integer;
  RowLimit: Integer;
  LayerLimit: Integer;
  ShouldDraw: Boolean;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
begin
  if Deleted or not Visible or (ElevationCount = ecZero) then
    Exit;

  LocalModel := Model as TPhastModel;
  With LocalModel.Grid do
  begin
    if (LayerCount <= 0) or (RowCount <= 0) or (ColumnCount <= 0) then
    begin
      Exit;
    end;
  end;

  if not FGlListCreated then
  begin
    FGlScreenObjectList := glGenLists(1);
    if FGlScreenObjectList <> 0 then
    begin
      FGlListCreated := True;
    end
    else
    begin
      Exit;
    end;
  end;
  if not FListUpToDate then
  begin
    glNewList(FGlScreenObjectList, GL_COMPILE);
    try
      glPushMatrix;
      try
        glEnable(GL_LINE_SMOOTH);
        glShadeModel(GL_SMOOTH);

        if FillScreenObject then
        begin
          ExtractColorComponents(FillColor, Red, Green, Blue);
        end
        else if ColorLine then
        begin
          ExtractColorComponents(LineColor, Red, Green, Blue);
        end
        else
        begin
          Red := 64;
          Green := 64;
          Blue := 64;
        end;

        Colors[0] := Red / 255;
        Colors[1] := Green / 255;
        Colors[2] := Blue / 255;
        Colors[3] := 1;

        glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, @Colors);
        glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 0.7);
        glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @Colors);

        glLineWidth(2);
        glPointSize(5);
        //  glColor3ub(Red, Green, Blue);

        GlGridValues := LocalModel.Grid.GlGrid(EvaluatedAt,
          LocalModel.ModelSelection);
        if GlGridValues = nil then
        begin
          Exit;
        end;

        try
          Delegate.AssignSelectedCells(LocalModel.Grid);
          CellsToDraw := Delegate.SelectedCells;
          if CellsToDraw.HasCells then
          begin
            ColLimit:= -1;
            RowLimit:= -1;
            LayerLimit:= -1;
            case EvaluatedAt of
              eaBlocks:
                begin
                  ColLimit := LocalModel.Grid.ColumnCount -1;
                  RowLimit := LocalModel.Grid.RowCount -1;
                  LayerLimit := LocalModel.Grid.LayerCount -1;
                end;
              eaNodes:
                begin
                  ColLimit := LocalModel.Grid.ColumnCount;
                  RowLimit := LocalModel.Grid.RowCount;
                  LayerLimit := LocalModel.Grid.LayerCount;
                end;
              else Assert(False);
            end;
            ShouldDraw := False;
            for ColIndex := 0 to ColLimit do
            begin
              for RowIndex := 0 to RowLimit do
              begin
                for LayerIndex := 0 to LayerLimit do
                begin
                  if CellsToDraw.Selected[LayerIndex, RowIndex, ColIndex] then
                  begin
                    GlGridValues[ColIndex+1, RowIndex+1, LayerIndex+1].Value := 1;
                    ShouldDraw := True;
                  end
                  else
                  begin
                    GlGridValues[ColIndex+1, RowIndex+1, LayerIndex+1].Value := -1;
                  end;
                end;
              end;
            end;
            if ShouldDraw then
            begin
              DrawIsoSurface(GlGridValues, 0.0);
            end;
          end;
        finally
          Delegate.SelectedCells.Clear;
        end;
      finally
        glPopMatrix;
      end;
    finally
      glEndList;
    end;
    FListUpToDate := True;
  end;
  glCallList(FGlScreenObjectList);
end;

procedure TScreenObject.Draw(Const Bitmap32: TBitmap32;
  const Direction: TViewDirection; const DrawAsSelected: Boolean = False);
begin
  if Deleted or not Visible then
    Exit;
  if not frmGoPhast.tb3DObjects.Down
    and (Direction <> ViewDirection) then
  begin
      Exit;
  end;
  case ElevationCount of
    ecZero:
      begin
        Draw0Elev(Bitmap32, Direction, DrawAsSelected);
      end;
    ecOne:
      begin
        Draw1Elev(Bitmap32, Direction, DrawAsSelected);
      end;
    ecTwo:
      begin
        Draw2Elev(Bitmap32, Direction, DrawAsSelected);
      end;
  else
    Assert(False);
  end;
end;

procedure TScreenObject.DrawSelected(
  const BitMap32: TBitmap32; const Direction:
  TViewDirection);
begin
  if not Selected or not Visible then
  begin
    Exit;
  end;
  Draw(BitMap32, Direction, True);
end;

procedure TScreenObject.Draw1Elev(Const Bitmap32: TBitmap32;
  const Direction: TViewDirection; const DrawAsSelected: Boolean = False);
begin
  if Deleted then
    Exit;
  if Direction = ViewDirection then
  begin
    Draw0Elev(Bitmap32, Direction, DrawAsSelected);
  end
  else
  begin
    if (Model = nil)
      or ((Model as TPhastModel).ModelSelection = msModflow) then
    begin
      Draw1ElevModflow(Direction, Bitmap32, DrawAsSelected);
      Exit;
    end;
    if (Model as TPhastModel).ModelSelection = msPhast then
    begin
      Draw1ElevPhast(Direction, Bitmap32, DrawAsSelected);
      Exit;
    end;
    Assert(False);
  end;
end;

procedure TScreenObject.Draw0Elev(
  Const Bitmap32: TBitmap32; const Direction:
  TViewDirection; const DrawAsSelected: Boolean = False);
var
  IntPoint: TPoint;
  Index: integer;
  FillColor32, LineColor32: TColor32;
  LineWidth: single;
  SectionIndex: Integer;
  SectionPoints: TPointArray;
  SLength: integer;
  P: TPolygon32;
  MultiplePolygons: boolean;
  LastClosedSection: integer;
begin
  P := nil;
  if Deleted or (ViewDirection <> Direction) then
    Exit;

  // initialize the variables.
  FillColor32 := Color32(FillColor);
  if ColorLine then
  begin
    LineColor32 := Color32(LineColor);
  end
  else
  begin
    LineColor32 := clBlack32;
  end;
  if (Count <> 1) then
  begin
    if FillScreenObject then
    begin
      if DrawAsSelected then
      begin
        FillColor32 := SetAlpha(FillColor32, 192);
      end
      else
      begin
        FillColor32 := SetAlpha(FillColor32, 128);
      end;
    end
    else
    begin
      if DrawAsSelected then
      begin
        FillColor32 := Color32(clSilver);
        FillColor32 := SetAlpha(FillColor32, 128);
      end
      else
      begin
        FillColor32 := clTransparent32;
      end;
    end;
  end;

  if DrawAsSelected then
  begin
    LineWidth := 3;
  end
  else
  begin
    LineWidth := 2;
  end;
  if (Count = 1) then
  begin
    // draw a point
    if DrawAsSelected then
    begin
      if ColorLine then
      begin
        FillColor32 := LineColor32;
      end
      else
      begin
        FillColor32 := clBlack32;
      end;
    end
    else
    begin
      FillColor32 := clTransparent32;
    end;
    for Index := 0 to Count - 1 do
    begin
      IntPoint := CanvasCoordinates[Index];
      DrawPointMarker(LineColor32, FillColor32, Bitmap32, IntPoint, LineWidth);
    end;
  end
  else
  begin
    if Count > 0 then
    begin
      if IAmACurrentScreenObject then
      begin
        // make the current screen object look a little different while it is
        // being drawn.
        LineWidth := 1;
      end;

      MultiplePolygons := SectionCount > 1;
      try
        LastClosedSection := -1;
        for SectionIndex := SectionCount - 1 downto 0 do
        begin
          if SectionClosed[SectionIndex] then
          begin
            LastClosedSection := SectionIndex;
            break;
          end;
        end;
        for SectionIndex := 0 to SectionCount - 1 do
        begin
          if SectionCount = 1 then
          begin
            SectionPoints := CanvasCoordinates;
          end
          else
          begin
            SLength := SectionLength[SectionIndex];
            SetLength(SectionPoints, SLength);
            if SLength > 0 then
            begin
              Move(CanvasCoordinates[SectionStart[SectionIndex]],
                SectionPoints[0], SLength*SizeOf(TPoint));
            end;
          end;
          if SectionClosed[SectionIndex] then
          begin
            // draw a closed screen object
            DrawBigPolygon32(Bitmap32, LineColor32, FillColor32, LineWidth,
              SectionPoints, P, MultiplePolygons,
              (SectionIndex = LastClosedSection));
          end
          else if SectionLength[SectionIndex] = 1 then
          begin
            IntPoint := CanvasCoordinates[SectionStart[SectionIndex]];
            DrawPointMarker(LineColor32, FillColor32, BitMap32, IntPoint, LineWidth);
          end
          else
          begin
            // draw an open screen object.
            DrawBigPolyline32(Bitmap32, LineColor32, LineWidth,
              SectionPoints, True);
          end;

        end;
      finally
        P.Free;
      end;
        // for screen objects that are selected, draw a square at each vertex.
      if DrawAsSelected and not IAmACurrentScreenObject then
      begin
        LineWidth := 1;
        for Index := 0 to Count - 1 do
        begin
          IntPoint := CanvasCoordinates[Index];
          if SelectedVertices[Index] then
          begin
            FillColor32 := clWhite32;
          end
          else
          begin
            if ColorLine then
            begin
              FillColor32 := LineColor32;
            end
            else
            begin
              FillColor32 := clBlack32;
            end;
          end;
          DrawPointMarker(LineColor32, FillColor32, BitMap32, IntPoint, LineWidth);
        end;
      end;
    end;
  end;
end;

function TScreenObject.GetCanvasCoordinates: TPointArray;
begin
  // CalculateCanvasCoordinates exits quickly if nothing needs to be
  // done.  Otherwise, it recalculates the coordinates at which the
  // screen object should be drawn on the screeen.
  CalculateCanvasCoordinates;
  result := FCanvasCoordinates;
end;

function TScreenObject.GetPoints(const Index: integer): TPoint2D;
begin
  // Test that the index is valid.  Raise an exception if it isn't.
  ValidateIndex(Index);
  // Get the point.
  result := FPoints[Index];
end;

procedure TScreenObject.Grow;
begin
  // For just a few points, don't allocate a lot of space.
  if FCapacity < 16 then
  begin
    GrowBy(4);
  end
  else
  begin
    // for larger numbers of points, allocate a larger block of data.
    GrowBy(FCapacity div 4);
  end;
end;

procedure TScreenObject.GrowBy(Amount: integer);
begin
  // never make the capacity bigger by too small an amount.
  if Amount < FCapacity div 4 then
  begin
    Amount := FCapacity div 4
  end;
  Capacity := FCapacity + Amount;
end;

function TScreenObject.IsHigher3DElevationAssigned(Col, Row,
  Layer: integer): boolean;
begin
  UpdateHigher3DElevations;
  result := FHigher3DElevations.IsValue[Layer,Row,Col];
end;

function TScreenObject.IsLower3DElevationAssigned(Col, Row,
  Layer: integer): boolean;
begin
  UpdateLower3DElevations;
  result := FLower3DElevations.IsValue[Layer, Row, Col];
end;

function TScreenObject.IndexOfBoundaryDataSet(const DataSet: TDataArray):
  integer;
begin
  if FBoundaryDataSets = nil then
  begin
    result := -1;
  end
  else
  begin
    result := FBoundaryDataSets.IndexOf(DataSet);
  end;
end;

function TScreenObject.IndexOfDataSet(const DataSet: TDataArray): integer;
begin
  if (FCachedDataSetIndex >= 0)
    and (FCachedDataSetIndex < FDataSets.Count)
    and (FDataSets[FCachedDataSetIndex] = DataSet) then
  begin
    result := FCachedDataSetIndex;
    Exit;
  end;
  result := FDataSets.IndexOf(DataSet);
  FCachedDataSetIndex := result;
end;

function TScreenObject.IndexOfPoint(const APoint: TPoint2D): integer;
var
  Index: integer;
  Point: TPoint2D;
begin
  // find a point that matches APoint if there is one.
  // Otherwise, return -1.
  result := -1;
  for Index := 0 to FCount - 1 do
  begin
    Point := FPoints[Index];
    if (APoint.X = Point.X) and (APoint.Y = Point.Y) then
    begin
      result := Index;
      Exit;
    end;
  end;
end;

procedure TScreenObject.InsertDataSet(const Index: Integer; const DataSet:
  TDataArray);
var
  Subscription: TObserver;
  Item: TInterpValuesItem;
begin
  if IndexOfDataSet(DataSet) < 0 then
  begin
    if DataSet is TCustomPhastDataSet then
    begin
      Item := FInterpValues.Insert(Index) as TInterpValuesItem;
      Item.Values.Assign(DataSet);
    end;

    InvalidateModel;
    Subscription := TObserver.Create(nil);
    Subscription.UpdateWithName(DataSet.Name + Name);
    FDataSetSubscriptions.Insert(Index, Subscription);

    FDataSets.Insert(Index, DataSet);
    FDataSetFormulas.Insert(Index, nil);
    self.TalksTo(DataSet);
    Subscription.TalksTo(DataSet);
    self.TalksTo(Subscription);
    case ElevationCount of
      ecZero:
        begin
          // do nothing
        end;
      ecOne:
        begin
          if FElevSubscription = nil then
          begin
            CreateElevationSubscription;
          end;
          FElevSubscription.TalksTo(DataSet);
        end;
      ecTwo:
        begin
          if FTopElevSubscription = nil then
          begin
            CreateTopElevationSubscription;
          end;
          FTopElevSubscription.TalksTo(DataSet);

          if FBottomElevSubscription = nil then
          begin
            CreateBottomElevationSubscription;
          end;
          FBottomElevSubscription.TalksTo(DataSet);
        end;
    else
      Assert(False);
    end;
    DataSet.Invalidate;
  end;
end;

procedure TScreenObject.InsertPoint(const Index: Integer;
  const APoint: TPoint2D);
var
  SectionIndex: Integer;
  Value: integer;
begin
  FIsClosedCached := False;
  InvalidateModel;
  if Index = FCount then
  begin
    // If you are inserting a new point at the end of the array,
    // just make the array bigger.
    AddPoint(APoint, False);
  end
  else
  begin
    // Otherwise check the index and see if it is OK.
    ValidateIndex(Index);
    if not FRecalculateBox then
    begin
      // there is no point in doing this if the values are invalid anyway.
      if APoint.X > FMaxX then
      begin
        FMaxX := APoint.X;
      end
      else if APoint.X < FMinX then
      begin
        FMinX := APoint.X;
      end;

      if APoint.Y > FMaxY then
      begin
        FMaxY := APoint.Y;
      end
      else if APoint.Y < FMinY then
      begin
        FMinY := APoint.Y;
      end;
    end;
    // If required, make the array bigger.
    if FCount = FCapacity then
    begin
      Grow;
    end;
    // Move subsequent points out of the way.
    Move(FPoints[Index], FPoints[Index + 1],
      (FCount - Index) * SizeOf(TPoint2D));
    // move the selection information too.
    if SelectedVertexCount > 0 then
    begin
      Move(FSelectedVertices[Index], FSelectedVertices[Index + 1],
        (FCount - Index) * SizeOf(boolean));
      FSelectedVertices[Index] := False;
    end
    else
    begin
      FSelectedVertices[FCount] := False;
    end;
    for SectionIndex := 0 to SectionStarts.Count - 1 do
    begin
      Value := SectionStarts.IntValues[SectionIndex];
      if Value >= Index then
      begin
        SectionStarts.IntValues[SectionIndex] := Value + 1;
      end;
    end;

    // Update the count.
    Inc(FCount);
    // Insert the new point.
    FPoints[Index] := APoint;
  end;
  Invalidate;
end;

procedure TScreenObject.InvalidateModel;
begin
  if FCanInvalidateModel and (FModel <> nil) then
  begin
    (FModel as TPhastModel).Invalidate;
  end;
end;

procedure TScreenObject.InvalidateSegments;
begin
  FSegments.UpToDate := False;
end;

procedure TScreenObject.InvalidateCoordinates;
begin
  FRecalculateCoordinates := True;
end;

function TScreenObject.FrameScreenObject(VD: TViewDirection): TScreenObject;
begin
  if FModel = nil then
  begin
    result := nil;
  end
  else
  begin
    result := (FModel as TPhastModel).GetCurrentScreenObject(VD)
  end;
end;

procedure TScreenObject.Invalidate;
var
  DataSet: TDataArray;
  Index: integer;
begin
  if csDestroying in ComponentState then
  begin
    Exit;
  end;
  FPriorObjectIntersectLengthCol := -1;
  FPriorObjectIntersectLengthRow := -1;
  FPriorObjectIntersectLengthLayer := -1;

  FreeAndNil(FGpcPolygons);
  FListUpToDate := False;
  ClearSubPolygons;
  FRecalculateCoordinates := True;
  FRecalculateLength := True;
  FRecalculateArea := True;
  FNeedToUpdateLine := True;
  FreeAndNil(FSelectLines);
  FSegments.UpToDate := False;
  FHigher3DElevationsNeedsUpdating := True;
  FLower3DElevationsNeedsUpdating := True;

  UpToDate := False;
  NotifyGuiOfChange(self);

  if FrameScreenObject(ViewDirection) = self then
  begin
    ZoomBox(ViewDirection).Image32.Invalidate;
  end;

  if (FModel <> nil) then
  begin
    if ((FModel as TPhastModel).Grid <> nil) then
    begin
      if ((FModel as TPhastModel).Grid.TopDataSet <> nil) and
        not (FModel as TPhastModel).Grid.TopDataSet.UpToDate then
      begin
        (FModel as TPhastModel).Grid.NeedToRecalculateTopCellColors := True;
        ZoomBox(vdTop).Image32.Invalidate;
      end;

      if ((FModel as TPhastModel).Grid.FrontDataSet <> nil) and
        not (FModel as TPhastModel).Grid.FrontDataSet.UpToDate then
      begin
        (FModel as TPhastModel).Grid.NeedToRecalculateFrontCellColors := True;
        ZoomBox(vdFront).Image32.Invalidate;
      end;

      if ((FModel as TPhastModel).Grid.SideDataSet <> nil) and
        not (FModel as TPhastModel).Grid.SideDataSet.UpToDate then
      begin
        (FModel as TPhastModel).Grid.NeedToRecalculateSideCellColors := True;
        ZoomBox(vdSide).Image32.Invalidate;
      end;

      if ((FModel as TPhastModel).Grid.ThreeDDataSet <> nil) and
        not (FModel as TPhastModel).Grid.ThreeDDataSet.UpToDate then
      begin
        (FModel as TPhastModel).Grid.NeedToRecalculate3DCellColors := True;
        (FModel as TPhastModel).Grid.GridChanged;
      end;
    end;

    if FModel <> nil then
    begin
      (FModel as TPhastModel).Notify3DViewChanged;
    end;
    InvalidateModel;
  end;

  if (FFluxBoundary <> nil)
    and (FluxBoundary.BoundaryValue.Count > 0) and FCanInvalidateModel then
  begin
    FluxBoundary.BoundaryValue.TimeList.Invalidate
  end;
  if (FFluxBoundary <> nil)
    and (FluxBoundary.Solution.Count > 0) and FCanInvalidateModel then
  begin
    FluxBoundary.Solution.TimeList.Invalidate
  end;
  if (FLeakyBoundary <> nil)
    and (LeakyBoundary.BoundaryValue.Count > 0) and FCanInvalidateModel then
  begin
    LeakyBoundary.BoundaryValue.TimeList.Invalidate;
  end;
  if (FLeakyBoundary <> nil)
    and (LeakyBoundary.Solution.Count > 0) and FCanInvalidateModel then
  begin
    LeakyBoundary.Solution.TimeList.Invalidate;
  end;
  if FCanInvalidateModel and (FLeakyBoundary <> nil) then
  begin
    if (LeakyBoundary.BoundaryValue.Count > 0)
      or (LeakyBoundary.Solution.Count > 0) then
    begin
      Index := -1;
      case ViewDirection of
        vdTop:
          begin
            Index := (FModel as TPhastModel).IndexOfBoundaryDataSet(
              rsTopLeakyHydraulicConductivity);
          end;
        vdFront:
          begin
            Index := (FModel as TPhastModel).IndexOfBoundaryDataSet(
              rsFrontLeakyHydraulicConductivity);
          end;
        vdSide:
          begin
            Index := (FModel as TPhastModel).IndexOfBoundaryDataSet(
              rsSideLeakyHydraulicConductivity);
          end;
      else
        Assert(False);
      end;
      if Index >= 0 then
      begin
        DataSet := (FModel as TPhastModel).BoundaryDataSets[Index];
        DataSet.Invalidate;
      end;
      case ViewDirection of
        vdTop:
          begin
            Index := (FModel as TPhastModel).IndexOfBoundaryDataSet(
              rsTopLeakyThickness);
          end;
        vdFront:
          begin
            Index := (FModel as TPhastModel).IndexOfBoundaryDataSet(
              rsFrontLeakyThickness);
          end;
        vdSide:
          begin
            Index := (FModel as TPhastModel).IndexOfBoundaryDataSet(
              rsSideLeakyThickness);
          end;
      else
        Assert(False);
      end;
      if Index >= 0 then
      begin
        DataSet := (FModel as TPhastModel).BoundaryDataSets[Index];
        DataSet.Invalidate;
      end;
    end;
  end;

  if (FRiverBoundary <> nil)
    and (RiverBoundary.BoundaryValue.Count > 0) and FCanInvalidateModel then
  begin
    RiverBoundary.BoundaryValue.TimeList.Invalidate;
  end;
  if (FRiverBoundary <> nil)
    and (RiverBoundary.Solution.Count > 0) and FCanInvalidateModel then
  begin
    RiverBoundary.Solution.TimeList.Invalidate;
  end;
  if FCanInvalidateModel and (FRiverBoundary <> nil) then
  begin
    if (RiverBoundary.BoundaryValue.Count > 0)
      or (RiverBoundary.Solution.Count > 0) then
    begin
      Index := (FModel as TPhastModel).IndexOfBoundaryDataSet(
        rsRiverHydraulicConductivity);
      if Index >= 0 then
      begin
        DataSet := (FModel as TPhastModel).BoundaryDataSets[Index];
        DataSet.Invalidate;
      end;
      Index := (FModel as TPhastModel).IndexOfBoundaryDataSet(rsRiverWidth);
      if Index >= 0 then
      begin
        DataSet := (FModel as TPhastModel).BoundaryDataSets[Index];
        DataSet.Invalidate;
      end;
      Index := (FModel as TPhastModel).IndexOfBoundaryDataSet(rsRiverDepth);
      if Index >= 0 then
      begin
        DataSet := (FModel as TPhastModel).BoundaryDataSets[Index];
        DataSet.Invalidate;
      end;
      Index := (FModel as TPhastModel).IndexOfBoundaryDataSet(
        rsRiverBedThickness);
      if Index >= 0 then
      begin
        DataSet := (FModel as TPhastModel).BoundaryDataSets[Index];
        DataSet.Invalidate;
      end;
    end;
  end;

  if (FSpecifiedHeadBoundary <> nil)
    and (SpecifiedHeadBoundary.BoundaryValue.Count > 0)
    and FCanInvalidateModel then
  begin
    SpecifiedHeadBoundary.BoundaryValue.TimeList.Invalidate;
  end;
  if (FSpecifiedHeadBoundary <> nil)
    and (SpecifiedHeadBoundary.Solution.Count > 0) and FCanInvalidateModel then
  begin
    SpecifiedHeadBoundary.Solution.TimeList.Invalidate;
  end;
  {if SpecifiedSolutionBoundary.Solution.Count > 0 then
  begin
    SpecifiedSolutionBoundary.Solution.TimeList.Invalidate;
  end;}
  if (FWellBoundary <> nil)
    and (WellBoundary.BoundaryValue.Count > 0) and FCanInvalidateModel then
  begin
    WellBoundary.BoundaryValue.TimeList.Invalidate
  end;
  if (FWellBoundary <> nil)
    and (WellBoundary.Solution.Count > 0) and FCanInvalidateModel then
  begin
    WellBoundary.Solution.TimeList.Invalidate;
  end;

  if (FModflowBoundaries <> nil) and CanInvalidateModel then
  begin
    if ModflowChdBoundary <> nil then
    begin
      ModflowChdBoundary.InvalidateDisplay;
    end;
    if ModflowGhbBoundary <> nil then
    begin
      ModflowGhbBoundary.InvalidateDisplay;
    end;
    if ModflowWellBoundary <> nil then
    begin
      ModflowWellBoundary.InvalidateDisplay;
    end;
    if ModflowRivBoundary <> nil then
    begin
      ModflowRivBoundary.InvalidateDisplay;
    end;
    if ModflowDrnBoundary <> nil then
    begin
      ModflowDrnBoundary.InvalidateDisplay;
    end;
    if (ModflowDrtBoundary <> nil) then
    begin
      ModflowDrtBoundary.InvalidateDisplay;
    end;
    if ModflowRchBoundary <> nil then
    begin
      ModflowRchBoundary.InvalidateDisplay;
    end;
    if ModflowEvtBoundary <> nil then
    begin
      ModflowEvtBoundary.InvalidateDisplay;
    end;
    if ModflowEtsBoundary <> nil then
    begin
      ModflowEtsBoundary.InvalidateDisplay;
    end;
    if ModflowResBoundary <> nil then
    begin
      ModflowResBoundary.InvalidateDisplay;
    end;
    if ModflowLakBoundary <> nil then
    begin
      ModflowLakBoundary.InvalidateDisplay;
    end;
    if ModflowSfrBoundary <> nil then
    begin
      ModflowSfrBoundary.InvalidateDisplay;
    end;
    if ModflowUzfBoundary <> nil then
    begin
      ModflowUzfBoundary.InvalidateDisplay;
    end;
  end;
end;

procedure TScreenObject.SetCapacity(Value: integer);
begin
  if Value < 0 then
  begin
    Value := 0;
  end;
  if FCapacity <> Value then
  begin
    DestroyLastSubPolygon;
    FCapacity := Value;
    SetLength(FPoints, FCapacity);
    SetLength(FSelectedVertices, FCapacity);
    if FCapacity < FCount then
    begin
      FCount := FCapacity;
      ResetSelectedVertexCount;
    end;
  end;
end;

procedure TScreenObject.ResetSelectedVertexCount;
var
  Index: integer;
begin
  FSelectedVertexCount := 0;
  for Index := 0 to FCount - 1 do
  begin
    if SelectedVertices[Index] then
    begin
      Inc(FSelectedVertexCount);
    end;
  end;
end;

procedure TScreenObject.SetCount(const Value: integer);
var
  Index: integer;
  OldCount: integer;
begin
  if FCount <> Value then
  begin
    FIsClosedCached := False;
    if (FCount > Value) and (FSelectedVertexCount > 0) then
    begin
      FNeedToResetSelectedVertexCount := True;
    end;
    if Capacity < Value then
    begin
      Capacity := Value;
    end;
    OldCount := FCount;
    FCount := Value;
    for Index := OldCount to Count - 1 do
    begin
      SelectedVertices[Index] := False;
    end;
    if FNeedToResetSelectedVertexCount then
    begin
      ResetSelectedVertexCount;
    end;
    DeleteExtraSections;
    if OldCount > FCount then
    begin
      DestroyLastSubPolygon;
    end;
    FRecalculateBox := True;
    Invalidate;
  end;
end;

procedure TScreenObject.SetDataSetCapacity(const Value: integer);
begin
  FDataSets.Capacity := Value;
  FDataSetFormulas.Capacity := Value;
end;

procedure TScreenObject.SetDataSetFormulas(const Index: integer; Value:
  string);
var
  Observer: TObserver;
  OldUseList: TStringList;
  NewUseList: TStringList;
  UseIndex: integer;
  OtherIndex: integer;
  AFunction, OldFunction: string;
  Compiler: TRbwParser;
  ADataSet: TDataArray;
  DS: TObserver;
  AFormulaObject: TFormulaObject;
  FormulaObject: TFormulaObject;
begin
  if (Value = rsObjectImportedValuesR)
    or (Value = rsObjectImportedValuesI)
    or (Value = rsObjectImportedValuesB)
    or (Value = rsObjectImportedValuesT)
    then
  begin
    ADataSet := self.DataSets[Index];
    Value := Value + '("' + ADataSet.Name + '")';
  end;
  AFormulaObject := FDataSetFormulas[Index];
  if AFormulaObject = nil then
  begin
    AFunction := '';
  end
  else
  begin
    AFunction := AFormulaObject.Formula;
  end;
  Observer := nil;
  if (AFunction <> Value) then
  begin
    if FCanInvalidateModel then
    begin
      InvalidateModel;
      OldFunction := AFunction;
      try
        ADataSet := DataSets[Index];
        Observer := FDataSetSubscriptions[Index] as TObserver;
        OldUseList := TStringList.Create;
        NewUseList := TStringList.Create;
        try
          Compiler := GetCompiler(ADataSet.Orientation,
            ADataSet.EvaluatedAt);
          if AFunction = '' then
          begin
            AFunction := '0'
          end;
          try
            Compiler.Compile(AFunction);
            OldUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
          except on E: ERbwParserError do
              OldUseList.Clear;
          end;

          AFunction := Value;
          Compiler.Compile(AFunction);
          NewUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
          CreateOrRetrieveFormulaObject(Index, ADataSet, FormulaObject);
          frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
            FormulaObject, Compiler.CurrentExpression.Decompile, Compiler,
            GlobalRemoveScreenObjectDataArraySubscription, GlobalRestoreDataArraySubscription, self);
          FDataSetFormulas[Index] := FormulaObject;
          for UseIndex := OldUseList.Count - 1 downto 0 do
          begin
            OtherIndex := NewUseList.IndexOf(OldUseList[UseIndex]);
            if OtherIndex >= 0 then
            begin
              OldUseList.Delete(UseIndex);
              NewUseList.Delete(OtherIndex);
            end;
          end;
          for UseIndex := 0 to OldUseList.Count - 1 do
          begin
            DS := (FModel as TPhastModel).GetObserverByName(
              OldUseList[UseIndex]);
            Assert(DS <> nil);
            DS.StopsTalkingTo(Observer);
          end;
          for UseIndex := 0 to NewUseList.Count - 1 do
          begin
            DS := (FModel as TPhastModel).GetObserverByName(
              NewUseList[UseIndex]);
            Assert(DS <> nil);
            DS.TalksTo(Observer);
          end;
          Invalidate;
        finally
          OldUseList.Free;
          NewUseList.Free;
        end;

        if not (csDestroying in Model.ComponentState) then
        begin
          Observer.UpToDate := True;
          Observer.UpToDate := False;
          ADataSet.Invalidate;
          Observer.UpToDate := True;
        end;
      finally
        if not (csDestroying in Model.ComponentState) then
        begin
          if Observer.IsRecursive then
          begin
            DataSetFormulas[Index] := OldFunction;
          end;
        end;
      end;
    end
    else
    begin
      ADataSet := DataSets[Index];
      Compiler := frmGoPhast.PhastModel.GetCompiler(ADataSet.Orientation,
        ADataSet.EvaluatedAt);
      CreateOrRetrieveFormulaObject(Index, ADataSet, FormulaObject);
      frmGoPhast.PhastModel.FormulaManager.ChangeFormula(FormulaObject, Value,
        Compiler, GlobalRemoveScreenObjectDataArraySubscription, GlobalRestoreDataArraySubscription, self);
      FDataSetFormulas[Index] := FormulaObject;
    end;
  end;
end;

procedure TScreenObject.SetDataSets(const Index: integer; const DataSet:
  TDataArray);
var
  OldIndex: integer;
  FormulaObject: TFormulaObject;
begin
  // Determine the position of DataSet in FDataSets.
  // If OldIndex < 0, DataSet is not in FDataSets.
  OldIndex := FDataSets.IndexOf(DataSet);

  // Store DataSet.
  FDataSets[Index] := DataSet;
  if (OldIndex >= 0) and (OldIndex <> Index) then
  begin
    // If DataSet was already in FDataSets, move it's funciton
    // to the new position and delete the old copy of both the
    // data set and the function.
    FDataSetFormulas[Index] := FDataSetFormulas[OldIndex];
    FDataSets.Delete(OldIndex);
    FormulaObject := FDataSetFormulas[OldIndex];
    frmGoPhast.PhastModel.FormulaManager.Remove(FormulaObject,
      GlobalRemoveScreenObjectDataArraySubscription, GlobalRestoreDataArraySubscription, self);
    FDataSetFormulas.Delete(OldIndex);
  end;
  InvalidateModel;
end;

procedure TScreenObject.SetPoints(const Index: integer;
  const Value: TPoint2D);
begin
  ValidateIndex(Index);
  if (FPoints[Index].X <> Value.X) or (FPoints[Index].Y <> Value.Y) then
  begin
    BeginUpdate;
    try
    FIsClosedCached := False;
//    InvalidateModel;
//    NotifyGuiOfChange(self);

    FRecalculateLength := True;
    FRecalculateArea := True;
    if not FRecalculateBox then
    begin
      // There is no point in doing this if the values are
      // invalid anyway.
      // if you are replacing a point that is at the edge of the box
      // surrounding the screen object, the box may need to be updated.
      if FPoints[Index].X = FMaxX then
      begin
        FRecalculateBox := True;
      end
      else if FPoints[Index].X = FMinX then
      begin
        FRecalculateBox := True;
      end;

      if FPoints[Index].Y = FMaxY then
      begin
        FRecalculateBox := True;
      end
      else if FPoints[Index].Y = FMinY then
      begin
        FRecalculateBox := True;
      end;
    end;
    FPoints[Index] := Value;
    if not FRecalculateBox then
    begin
      // There is no point in doing this if the box is invalid anyway.
      if Value.X > FMaxX then
      begin
        FMaxX := Value.X;
      end
      else if Value.X < FMinX then
      begin
        FMinX := Value.X;
      end;

      if Value.Y > FMaxY then
      begin
        FMaxY := Value.Y;
      end
      else if Value.Y < FMinY then
      begin
        FMinY := Value.Y;
      end;
    end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TScreenObject.SetViewDirection(const Value: TViewDirection);
var
  OldViewDirection: TViewDirection;
begin
  OldViewDirection := FViewDirection;
  FViewDirection := Value;
  if OldViewDirection <> Value then
  begin
    InvalidateModel;
    if FrameScreenObject(OldViewDirection) = self then
    begin
      ZoomBox(OldViewDirection).Image32.Invalidate;
    end;
  end;
  if FFluxBoundary <> nil then
  begin
    FluxBoundary.Orientation := Value;
  end;
  if FLeakyBoundary <> nil then
  begin
    LeakyBoundary.Orientation := Value;
  end;
  SetFormulaParsers;
  Invalidate;
end;

procedure TScreenObject.ValidateIndex(const Index: integer);
begin
  if (Index < 0) or (Index >= FCount) then
  begin
    raise EScreenObjectError.Create(StrInvalidScreenObjec);
  end;
end;

// based partly on CACM 112

procedure TScreenObject.EvaluateSubPolygon(const ASubPolygon: TSubPolygon;
  const X, Y: real; var IsInside: boolean);
var
  VertexIndex: integer;
  APoint, AnotherPoint: TPoint2D;
begin
  if (Y < ASubPolygon.FMinY) or (Y > ASubPolygon.FMaxY)
    or (X > ASubPolygon.FMaxX) then
    Exit;
  if ASubPolygon.FSubPolygon1 <> nil then
  begin
    EvaluateSubPolygon(ASubPolygon.FSubPolygon1, X, Y, IsInside);
    EvaluateSubPolygon(ASubPolygon.FSubPolygon2, X, Y, IsInside);
  end
  else
  begin
    for VertexIndex := 0 to ASubPolygon.FCount - 2 do
    begin
      APoint := Points[ASubPolygon.FStart + VertexIndex];
      AnotherPoint := Points[ASubPolygon.FStart + VertexIndex + 1];
      if ((Y <= APoint.Y) = (Y > AnotherPoint.Y)) and
        (X - APoint.X - (Y - APoint.Y) *
        (AnotherPoint.X - APoint.X) /
        (AnotherPoint.Y - APoint.Y) < 0) then
      begin
        IsInside := not IsInside;
      end;
    end;
  end;
end;

procedure TScreenObject.SetGeometryUpToDate;
begin
  UpToDate := True;
  if FElevSubscription <> nil then
  begin
    FElevSubscription.UpToDate := True;
  end;
  if FTopElevSubscription <> nil then
  begin
    FTopElevSubscription.UpToDate := True;
  end;
  if FBottomElevSubscription <> nil then
  begin
    FBottomElevSubscription.UpToDate := True;
  end;
end;

function TScreenObject.ScreenObjectLength: real;
var
  Index: integer;
  First, Second: TPoint2D;
  SectionIndex: integer;
begin
  if FRecalculateLength then
  begin
    SectionIndex := 1;
    FScreenObjectLength := 0;
    if Count > 1 then
    begin
      First := Points[0];
      for Index := 1 to Count - 1 do
      begin
        Second := Points[Index];
        if (SectionCount > 1) and (SectionIndex < SectionCount) then
        begin
          if SectionStart[SectionIndex] = Index then
          begin
            Inc(SectionIndex);
            Continue;
          end;
        end;
        FScreenObjectLength := FScreenObjectLength
          + Sqrt(Sqr(Second.X - First.X)
          + Sqr(Second.Y - First.Y));
        First := Second;
      end;
    end;
    FRecalculateLength := false;
  end;
  result := FScreenObjectLength;
end;

function TScreenObject.ScreenObjectArea: real;
var
  EmptyPolygon: TGpcPolygonClass;
  IntersectionPolygon: TGpcPolygonClass;
  ContourIndex: Integer;
begin
  if FRecalculateArea then
  begin
    FScreenObjectArea := 0;
    if Closed then
    begin
      if FGpcPolygons = nil then
      begin
        CreateGpcPolygon;
      end;

      if FGpcPolygons.NumberOfContours = 1 then
      begin
        FScreenObjectArea := Abs(FGpcPolygons.ContourArea(0));
      end
      else
      begin
        EmptyPolygon := TGpcPolygonClass.Create;
        try
          EmptyPolygon.NumberOfContours := 0;
          IntersectionPolygon := TGpcPolygonClass.CreateFromOperation(
            GPC_DIFF, FGpcPolygons, EmptyPolygon);
          try
            for ContourIndex := 0 to IntersectionPolygon.NumberOfContours - 1 do
            begin
              if IntersectionPolygon.Holes[ContourIndex] then
              begin
                FScreenObjectArea := FScreenObjectArea
                  - Abs(IntersectionPolygon.ContourArea(ContourIndex));
              end
              else
              begin
                FScreenObjectArea := FScreenObjectArea
                  + Abs(IntersectionPolygon.ContourArea(ContourIndex));
              end;
            end;
          finally
            IntersectionPolygon.Free;
          end;
        finally
          EmptyPolygon.Free;
        end;
      end;

    end;
    FRecalculateArea := False;
  end;
  result := FScreenObjectArea;
end;

function TScreenObject.GetRiverBoundary: TRiverBoundary;
begin
  CreatePhastRiverBoundary;
  result := FRiverBoundary
end;

procedure TScreenObject.GetRotatedPoints(out RotatedPoints: TRealPointArray);
var
  Index: integer;
begin
  RotatedPoints := FPoints;
  SetLength(RotatedPoints, Count);
  if (ViewDirection = vdTop) and ((FModel as TPhastModel).
    Grid.GridAngle <> 0) then
  begin
    for Index := 0 to Count - 1 do
    begin
      RotatedPoints[Index] := (FModel as TPhastModel).Grid.
        RotateFromRealWorldCoordinatesToGridCoordinates(RotatedPoints[Index]);
    end;
  end;
end;

function TScreenObject.ValidCell(const Col, Row, Layer: integer): boolean;
begin
  Result := True;
  case ViewDirection of
    vdTop:
      begin
        if (Col < 0) or (Row < 0)
          or (Col > (FModel as TPhastModel).Grid.ColumnCount)
          or (Row > (FModel as TPhastModel).Grid.RowCount) then
        begin
          Result := False;
        end;
      end;
    vdFront:
      begin
        if (Col < 0) or (Layer < 0)
          or (Col > (FModel as TPhastModel).Grid.ColumnCount)
          or (Layer > (FModel as TPhastModel).Grid.LayerCount) then
        begin
          Result := False;
        end;
      end;
    vdSide:
      begin
        if (Row < 0) or (Layer < 0)
          or (Row > (FModel as TPhastModel).Grid.RowCount)
          or (Layer > (FModel as TPhastModel).Grid.LayerCount) then
        begin
          Result := False;
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TScreenObject.GetGridCellOrElementLimits(const Col, Row, Layer:
  integer; out XMin, XMax, YMin, YMax: double);
var
  PhastGrid: TPhastGrid;
  Grid: TCustomGrid;
begin
  PhastGrid := (FModel as TPhastModel).PhastGrid;
  Grid := (FModel as TPhastModel).Grid;
  case EvaluatedAt of
    eaBlocks:
      begin
        case ViewDirection of
          vdTop:
            begin
              case Grid.ColumnDirection of
                cdWestToEast:
                  begin
                    XMin := Grid.ColumnPosition[Col];
                    XMax := Grid.ColumnPosition[Col + 1];
                  end;
                cdEastToWest:
                  begin
                    XMin := Grid.ColumnPosition[Col + 1];
                    XMax := Grid.ColumnPosition[Col];
                  end;
              end;
              case Grid.RowDirection of
                rdSouthToNorth:
                  begin
                    YMin := Grid.RowPosition[Row];
                    YMax := Grid.RowPosition[Row + 1];
                  end;
                rdNorthToSouth:
                  begin
                    YMin := Grid.RowPosition[Row + 1];
                    YMax := Grid.RowPosition[Row];
                  end;
              end;
            end;
          vdFront:
            begin
              XMin := PhastGrid.ColumnPosition[Col];
              XMax := PhastGrid.ColumnPosition[Col + 1];
              YMin := PhastGrid.LayerElevation[Layer];
              YMax := PhastGrid.LayerElevation[Layer + 1];
            end;
          vdSide:
            begin
              XMin := PhastGrid.RowPosition[Row];
              XMax := PhastGrid.RowPosition[Row + 1];
              YMin := PhastGrid.LayerElevation[Layer];
              YMax := PhastGrid.LayerElevation[Layer + 1];
            end;
        else
          Assert(False);
        end;
      end;
    eaNodes:
      begin
        case ViewDirection of
          vdTop:
            begin
              if Col = 0 then
              begin
                XMin := PhastGrid.ColumnPosition[Col];
              end
              else
              begin
                XMin := (PhastGrid.ColumnPosition[Col - 1]
                  + PhastGrid.ColumnPosition[Col]) / 2;
              end;
              if Col = PhastGrid.ColumnCount then
              begin
                XMax := PhastGrid.ColumnPosition[Col];
              end
              else
              begin
                XMax := (PhastGrid.ColumnPosition[Col + 1]
                  + PhastGrid.ColumnPosition[Col]) / 2;
              end;
              if Row = 0 then
              begin
                YMin := PhastGrid.RowPosition[Row];
              end
              else
              begin
                YMin := (PhastGrid.RowPosition[Row - 1]
                  + PhastGrid.RowPosition[Row]) / 2;
              end;
              if Row = PhastGrid.RowCount then
              begin
                YMax := PhastGrid.RowPosition[Row];
              end
              else
              begin
                YMax := (PhastGrid.RowPosition[Row + 1]
                  + PhastGrid.RowPosition[Row]) / 2;
              end;
            end;
          vdFront:
            begin
              if Col = 0 then
              begin
                XMin := PhastGrid.ColumnPosition[Col];
              end
              else
              begin
                XMin := (PhastGrid.ColumnPosition[Col - 1]
                  + PhastGrid.ColumnPosition[Col]) / 2;
              end;
              if Col = PhastGrid.ColumnCount then
              begin
                XMax := PhastGrid.ColumnPosition[Col];
              end
              else
              begin
                XMax := (PhastGrid.ColumnPosition[Col + 1]
                  + PhastGrid.ColumnPosition[Col]) / 2;
              end;
              if Layer = 0 then
              begin
                YMin := PhastGrid.LayerElevation[Layer];
              end
              else
              begin
                YMin := (PhastGrid.LayerElevation[Layer - 1]
                  + PhastGrid.LayerElevation[Layer]) / 2;
              end;
              if Layer = PhastGrid.LayerCount then
              begin
                YMax := PhastGrid.LayerElevation[Layer];
              end
              else
              begin
                YMax := (PhastGrid.LayerElevation[Layer + 1]
                  + PhastGrid.LayerElevation[Layer]) / 2;
              end;
            end;
          vdSide:
            begin
              if Row = 0 then
              begin
                XMin := PhastGrid.RowPosition[Row];
              end
              else
              begin
                XMin := (PhastGrid.RowPosition[Row - 1]
                  + PhastGrid.RowPosition[Row]) / 2;
              end;
              if Row = PhastGrid.RowCount then
              begin
                XMax := PhastGrid.RowPosition[Row];
              end
              else
              begin
                XMax := (PhastGrid.RowPosition[Row + 1]
                  + PhastGrid.RowPosition[Row]) / 2;
              end;
              if Layer = 0 then
              begin
                YMin := PhastGrid.LayerElevation[Layer];
              end
              else
              begin
                YMin := (PhastGrid.LayerElevation[Layer - 1]
                  + PhastGrid.LayerElevation[Layer]) / 2;
              end;
              if Layer = PhastGrid.LayerCount then
              begin
                YMax := PhastGrid.LayerElevation[Layer];
              end
              else
              begin
                YMax := (PhastGrid.LayerElevation[Layer + 1]
                  + PhastGrid.LayerElevation[Layer]) / 2;
              end;
            end;
        else
          Assert(False);
        end;
      end;
  else
    Assert(False);
  end;
end;

function TScreenObject.ObjectIntersectArea(const Col, Row,
  Layer: integer): real;
var
  XMin, XMax, YMin, YMax: double;
  TempPoints: TRealPointArray;
  ModflowGrid: TModflowGrid;
  MFDelegate: TModflowDelegate;
  CellOutlines: T2DRealPointArray;
  PointIndex: Integer;
  CellOutline: TGpcPolygonClass;
  AVertex: Tgpc_vertex;
  IntersectionContours: TGpcPolygonClass;
  ContourIndex: Integer;
  LocalGrid: TCustomGrid;
  Temp: TFloat;
begin
  if not Closed then
  begin
    result := 0;
    Exit;
  end;

  if not ValidCell(Col, Row, Layer) then
  begin
    Result := 0;
    Exit;
  end;

  Assert(Model <> nil);
  if ((Model as TPhastModel).ModelSelection <> msPhast)
    and (ViewDirection <> vdTop) then
  begin
    ModflowGrid := (Model as TPhastModel).Grid as TModflowGrid;
    CellOutlines := nil;
    MFDelegate := Delegate as TModflowDelegate;
    SetLength(TempPoints,6);
    case ViewDirection of
      vdTop: Assert(False);
      vdFront:
        begin
          CellOutlines := MFDelegate.GetCellOutlines(ModflowGrid, Row);
          TempPoints[0] := CellOutlines[Col*2,Layer];
          TempPoints[1] := CellOutlines[Col*2+1,Layer];
          TempPoints[2] := CellOutlines[Col*2+2,Layer];
          TempPoints[3] := CellOutlines[Col*2+2,Layer+1];
          TempPoints[4] := CellOutlines[Col*2+1,Layer+1];
          TempPoints[5] := CellOutlines[Col*2,Layer+1];
        end;
      vdSide:
        begin
          CellOutlines := MFDelegate.GetCellOutlines(ModflowGrid, Col);
          TempPoints[0] := CellOutlines[Row*2,Layer];
          TempPoints[1] := CellOutlines[Row*2+1,Layer];
          TempPoints[2] := CellOutlines[Row*2+2,Layer];
          TempPoints[3] := CellOutlines[Row*2+2,Layer+1];
          TempPoints[4] := CellOutlines[Row*2+1,Layer+1];
          TempPoints[5] := CellOutlines[Row*2,Layer+1];
          for PointIndex := 0 to Length(TempPoints) - 1 do
          begin
            Temp := TempPoints[PointIndex].x;
            TempPoints[PointIndex].x := TempPoints[PointIndex].y;
            TempPoints[PointIndex].y := Temp;
          end;
        end;
      else Assert(False);
    end;
  end
  else
  begin
    GetGridCellOrElementLimits(Col, Row, Layer, XMin, XMax, YMin, YMax);

    SetLength(TempPoints, 4);
    case ViewDirection of
      vdTop:
        begin
          TempPoints[0].x := XMin;
          TempPoints[0].y := YMin;
          TempPoints[1].x := XMin;
          TempPoints[1].y := YMax;
          TempPoints[2].x := XMax;
          TempPoints[2].y := YMax;
          TempPoints[3].x := XMax;
          TempPoints[3].y := YMin;
          LocalGrid := (FModel as TPhastModel).Grid;
          if LocalGrid.GridAngle <> 0 then
          begin
            for PointIndex := 0 to 3 do
            begin
              TempPoints[PointIndex] := LocalGrid.
                RotateFromGridCoordinatesToRealWorldCoordinates(
                TempPoints[PointIndex])
            end;
          end;
        end;
      vdFront:
        begin
          TempPoints[0].x := XMin;
          TempPoints[0].y := YMin;
          TempPoints[1].x := XMin;
          TempPoints[1].y := YMax;
          TempPoints[2].x := XMax;
          TempPoints[2].y := YMax;
          TempPoints[3].x := XMax;
          TempPoints[3].y := YMin;
        end;
      vdSide:
        begin
          TempPoints[0].y := XMin;
          TempPoints[0].x := YMin;
          TempPoints[1].y := XMin;
          TempPoints[1].x := YMax;
          TempPoints[2].y := XMax;
          TempPoints[2].x := YMax;
          TempPoints[3].y := XMax;
          TempPoints[3].x := YMin;
        end;
      else Assert(False);
    end;
  end;

  CellOutline := TGpcPolygonClass.Create;
  try
    CellOutline.NumberOfContours := 1;
    CellOutline.VertexCount[0] := Length(TempPoints);
    for PointIndex := 0 to Length(TempPoints) - 1 do
    begin
      AVertex.x := TempPoints[PointIndex].x;
      AVertex.y := TempPoints[PointIndex].y;
      CellOutline.Vertices[0,PointIndex] := AVertex;
    end;

    if FGpcPolygons = nil then
    begin
      CreateGpcPolygon;
    end;

    IntersectionContours := TGpcPolygonClass.CreateFromOperation(
      GPC_INT, CellOutline, FGpcPolygons);
    try
      result := 0;
      for ContourIndex := 0 to IntersectionContours.NumberOfContours - 1 do
      begin
        if IntersectionContours.Holes[ContourIndex] then
        begin
          result := result
            - Abs(IntersectionContours.ContourArea(ContourIndex));
        end
        else
        begin
          result := result
            + Abs(IntersectionContours.ContourArea(ContourIndex));
        end;
      end;
    finally
      IntersectionContours.Free;
    end;
  finally
    CellOutline.Free;
  end  
end;

function TScreenObject.ObjectSectionIntersectLength(const Col, Row,
  Layer, Section: integer): real;
var
  LocalSegments: TCellElementSegmentList;
  SegmentIndex: Integer;
  Segment: TCellElementSegment;
begin

  Result := 0;
  if not ValidCell(Col, Row, Layer) then
  begin
    Exit;
  end;

  if (FPriorObjectSectionIntersectLengthCol = Col)
    and (FPriorObjectSectionIntersectLengthRow = Row)
    and (FPriorObjectSectionIntersectLengthLayer = Layer)
    and (FPriorObjectSectionIntersectLengthSection = Section)
    then
  begin
    result := FPriorObjectSectionIntersectLengthResult;
    Exit;
  end;

  LocalSegments := Segments;
  for SegmentIndex := 0 to LocalSegments.Count - 1 do
  begin
    Segment := LocalSegments[SegmentIndex];
    if (Segment.Col = Col)
      and (Segment.Row = Row)
      and (Segment.Layer = Layer)
      and (Segment.SectionIndex = Section) then
    begin
      result := result + Segment.Length;
    end;
  end;

  FPriorObjectSectionIntersectLengthCol := Col;
  FPriorObjectSectionIntersectLengthRow := Row;
  FPriorObjectSectionIntersectLengthLayer := Layer;
  FPriorObjectSectionIntersectLengthSection := Section;
  FPriorObjectSectionIntersectLengthResult := result;

end;

function TScreenObject.ObjectIntersectLength(const Col, Row,
  Layer: integer): real;
var
//  RotatedPoints: TRealPointArray;
//  Index, InnerIndex: integer;
//  XMin, XMax, YMin, YMax: double;
//  TempPoints: T2DRealPointArray6;
//  Point1, Point2: TPoint2D;
//  PointCount: integer;
//  Sorter: TList;
  SegmentIndex: Integer;
  Segment: TCellElementSegment;
  LocalSegments: TCellElementSegmentList;
begin

  Result := 0;
  if not ValidCell(Col, Row, Layer) then
  begin
    Exit;
  end;

  if (FPriorObjectIntersectLengthCol = Col)
    and (FPriorObjectIntersectLengthRow = Row)
    and (FPriorObjectIntersectLengthLayer = Layer)
    then
  begin
    result := FPriorObjectIntersectLengthResult;
    Exit;
  end;

  LocalSegments := Segments;
  for SegmentIndex := 0 to LocalSegments.Count - 1 do
  begin
    Segment := LocalSegments[SegmentIndex];
    if (Segment.Col = Col)
      and (Segment.Row = Row)
      and (Segment.Layer = Layer) then
    begin
      result := result + Segment.Length;
    end;
  end;

  FPriorObjectIntersectLengthCol := Col;
  FPriorObjectIntersectLengthRow := Row;
  FPriorObjectIntersectLengthLayer := Layer;
  FPriorObjectIntersectLengthResult := result;

{
  Exit;


  Assert(Model <> nil);
  if ((Model as TPhastModel).ModelSelection <> msPhast)
    and (ViewDirection <> vdTop) then
  begin
    if not Segments.UpToDate then
    begin
      case ViewDirection of
        vdTop: Assert(False);
        vdFront: UpdateFrontSegments((Model as TPhastModel).Grid, EvaluatedAt);
        vdSide: UpdateSideSegments((Model as TPhastModel).Grid, EvaluatedAt);
        else Assert(False);
      end;
    end;
    for SegmentIndex := 0 to Segments.Count - 1 do
    begin
      Segment := Segments[SegmentIndex];
      if (Segment.Col = Col)
        and (Segment.Row = Row)
        and (Segment.Layer = Layer) then
      begin
        result := result + Segment.Length;
      end;
    end;
  end
  else
  begin
    GetRotatedPoints(RotatedPoints);

    GetGridCellOrElementLimits(Col, Row, Layer, XMin, XMax, YMin, YMax);

    Sorter := TList.Create;
    try
      Sorter.Capacity := 6;
      for Index := 0 to Count - 2 do
      begin
        GetPointsOnLineSegment(RotatedPoints, XMin, XMax, YMin, YMax, Index,
          Point1, Point2, TempPoints, PointCount);

        TempPoints[PointCount] := Point2;
        Inc(PointCount);

        Sorter.Count := 0;
        for InnerIndex := 0 to PointCount - 1 do
        begin
          Sorter.Add(@TempPoints[InnerIndex]);
        end;

        if PointCount > 3 then
        begin
          SortPointsInCorrectDirection(Sorter, Point1, Point2);
        end;
        for InnerIndex := 0 to Sorter.Count - 2 do
        begin
          Point1 := P2DRealPoint(Sorter[InnerIndex])^;
          Point2 := P2DRealPoint(Sorter[InnerIndex + 1])^;
          if (Point1.X >= XMin) and
            (Point1.X <= XMax) and
            (Point2.X >= XMin) and
            (Point2.X <= XMax) and
            (Point1.Y >= YMin) and
            (Point1.Y <= YMax) and
            (Point2.Y >= YMin) and
            (Point2.Y <= YMax) then
          begin
            result := result + Sqrt(Sqr(Point1.X - Point2.X)
              + Sqr(Point1.Y - Point2.Y));
          end;
        end;
      end;
    finally
      Sorter.Free;
    end;
  end;
  }
end;

constructor TScreenObject.Create(AnOwner: TComponent);
begin
  inherited Create(nil);
  FPriorObjectIntersectLengthCol := -1;
  FPriorObjectIntersectLengthRow := -1;
  FPriorObjectIntersectLengthLayer := -1;
  FCachedCells:= TCachedCells.Create;
  Assert((AnOwner = nil) or (AnOwner is TPhastModel));
  FModel := AnOwner;
  FCanInvalidateModel := (FModel <> nil);

  FGlListCreated := False;

  FImportedValues := TValueCollection.Create;
  FVisible := True;
  FSegments := TCellElementSegmentList.Create(AnOwner, self);
  FCellSize := 1;
  if FDataSets = nil then
  begin
    FDataSets := TList.Create;
  end;
  if FDataSetFormulas = nil then
  begin
    FDataSetFormulas := TList.Create;
  end;

  if FDataSetSubscriptions = nil then
  begin
    FDataSetSubscriptions := TObjectList.Create;
  end;
  ElevationCount := ecTwo;

  if FModel <> nil then
  begin
    if ((FModel as TPhastModel).PhastGrid <> nil) then
    begin
      with (FModel as TPhastModel).PhastGrid do
      begin
        if LayerCount > 0 then
        begin
          HigherElevationFormula := FloatToStr(LayerElevations[LayerCount]);
          LowerElevationFormula := FloatToStr(LayerElevations[0]);
        end
        else
        begin
          HigherElevationFormula := '0';
          LowerElevationFormula := '0';
        end;
      end;
    end;
  end;
  FHigher3DElevationsNeedsUpdating := True;
  FLower3DElevationsNeedsUpdating := True;

  FDelegateCollection:= TDelegateCollection.Create(self);

  FDataSetMixtureSubscriptions := TObjectList.Create;

  FInterpValues := TInterpValuesCollection.Create(AnOwner);
  FIFACE := iInternal;
  FModpathParticles := TParticleStorage.Create(Model);
end;

procedure TScreenObject.CreateLastSubPolygon;
var
  SectionIndex: Integer;
begin
  DestroyLastSubPolygon;
  for SectionIndex := SectionCount -1 downto 0 do
  begin
    if SectionStart[SectionIndex] < Count then
    begin
      FLastSubPolygon := TSubPolygon.Create(FPoints,
        SectionLength[SectionIndex], SectionStart[SectionIndex], SectionIndex);
      break;
    end;
  end;
end;

function TScreenObject.Intersection(const Point1, Point2: TPoint2D;
  StartIndex: integer; out IntersectPoint: TPoint2D): TIntersectResult;
  {
  Modified from
  http://www1.acm.org/pubs/tog/GraphicsGems/gemsii/xlines.c
  AUTHOR: Mukesh Prasad
  }
  function SameSign(const A, B: real): boolean;
  begin
    result := (A >= 0) = (B >= 0);
  end;
var
  x1, y1, x2, y2, x3, y3, x4, y4: real;
  a1, a2, b1, b2, c1, c2: real; { Coefficients of line eqns. }
  r1, r2, r3, r4: real; { 'Sign' values }
  denom, offset, num: real; { Intermediate values }
  ScreenObjectPoint1, ScreenObjectPoint2: TPoint2D;
  Epsilon: double;
  function NearlyTheSame(const A, B: real): boolean;
  begin
    result := A = B;
    if not result then
    begin
      result := Abs(A - B) / (Abs(A) + Abs(B)) < Epsilon;
    end;
  end;
begin
  x1 := Point1.X;
  y1 := Point1.Y;
  x2 := Point2.X;
  y2 := Point2.Y;

  ScreenObjectPoint1 := Points[StartIndex];
  ScreenObjectPoint2 := Points[StartIndex + 1];

  x3 := ScreenObjectPoint1.X;
  y3 := ScreenObjectPoint1.Y;
  x4 := ScreenObjectPoint2.X;
  y4 := ScreenObjectPoint2.Y;

  Epsilon := Max(Abs(x1), Abs(x2));
  Epsilon := Max(Epsilon, Abs(x3));
  Epsilon := Max(Epsilon, Abs(x4));
  Epsilon := Max(Epsilon, Abs(y1));
  Epsilon := Max(Epsilon, Abs(y2));
  Epsilon := Max(Epsilon, Abs(y3));
  Epsilon := Max(Epsilon, Abs(y4));
  Epsilon := Epsilon / 1E14;

  if (Max(x1,x2) + Epsilon) < Min(x3,x4) then
  begin
    result := irDontIntersect;
    Exit;
  end;
  if (Min(x1,x2) - Epsilon) > Max(x3,x4) then
  begin
    result := irDontIntersect;
    Exit;
  end;
  if (Max(y1,y2) + Epsilon) < Min(y3,y4) then
  begin
    result := irDontIntersect;
    Exit;
  end;
  if (Min(y1,y2) - Epsilon) > Max(y3,y4) then
  begin
    result := irDontIntersect;
    Exit;
  end;

  if (NearlyTheSame(x1, x3) and NearlyTheSame(y1,y3))
    or (NearlyTheSame(x1, x4) and NearlyTheSame(y1,y4)) then
  begin
    IntersectPoint.X := x1;
    IntersectPoint.Y := y1;
    result := irDoIntersect;
    Exit;
  end
  else if (NearlyTheSame(x2, x3) and NearlyTheSame(y2,y3))
    or (NearlyTheSame(x2, x4) and NearlyTheSame(y2,y4)) then
  begin
    IntersectPoint.X := x2;
    IntersectPoint.Y := y2;
    result := irDoIntersect;
    Exit;
  end;

  { Compute a1, b1, c1, where line joining points 1 and 2
   * is "a1 x  +  b1 y  +  c1  =  0".
   }

  a1 := y2 - y1;
  b1 := x1 - x2;
  c1 := x2 * y1 - x1 * y2;

  { Compute r3 and r4.
   }

  r3 := a1 * x3 + b1 * y3 + c1;
  r4 := a1 * x4 + b1 * y4 + c1;

  { Check signs of r3 and r4.  If both point 3 and point 4 lie on
    same side of line 1, the line segments do not intersect.
   }

  if (r3 <> 0) and
    (r4 <> 0) and
    SameSign(r3, r4) then
  begin
    result := irDontIntersect;
    Exit;
  end;

  { Compute a2, b2, c2 }

  a2 := y4 - y3;
  b2 := x3 - x4;
  c2 := x4 * y3 - x3 * y4;

  { Compute r1 and r2 }

  r1 := a2 * x1 + b2 * y1 + c2;
  r2 := a2 * x2 + b2 * y2 + c2;

  { Check signs of r1 and r2.  If both point 1 and point 2 lie
    on same side of second line segment, the line segments do
    not intersect.
   }

  if (r1 <> 0) and
    (r2 <> 0) and
    SameSign(r1, r2) then
  begin
    result := irDontIntersect;
    Exit;
  end;

  { Line segments intersect: compute intersection point.}

  denom := a1 * b2 - a2 * b1;
  if (denom = 0) then
  begin
    result := irColinear;
    Exit;
  end;

  {if denom < 0 then
  begin
    offset := -denom / 2;
  end
  else
  begin
    offset := denom / 2;
  end;  }
  offset := 0;
  { The denom/2 is to get rounding instead of truncating.  It
    is added or subtracted to the numerator, depending upon the
    sign of the numerator.
   }

  num := b1 * c2 - b2 * c1;
  if num < 0 then
  begin
    IntersectPoint.X := (num - offset) / denom;
  end
  else
  begin
    IntersectPoint.X := (num + offset) / denom;
  end;

  num := a2 * c1 - a1 * c2;

  if num < 0 then
  begin
    IntersectPoint.Y := (num - offset) / denom;
  end
  else
  begin
    IntersectPoint.Y := (num + offset) / denom;
  end;
  result := irDoIntersect;
end;

procedure TScreenObject.DeleteEdge(EdgeToDelete: integer);
var
  SectionIndex: integer;
  TempPoints: TRealPointArray;
  TempSelectedVertices: TBooleanDynArray;
  PointsBefore: integer;
  PointsAfter: integer;
  InnerSectionIndex: Integer;
  Value: integer;
begin
  FIsClosedCached := False;
  for SectionIndex := 0 to SectionCount - 1 do
  begin
    if (EdgeToDelete >= SectionStart[SectionIndex])
      and (EdgeToDelete <= SectionEnd[SectionIndex]) then
    begin
      Assert(EdgeToDelete <> SectionEnd[SectionIndex]);

      if not SectionClosed[SectionIndex] then
      begin
        SectionStarts.Insert(SectionIndex);
        SectionStarts.IntValues[SectionIndex] := EdgeToDelete+1;
      end
      else
      begin
        SetLength(TempPoints, SectionLength[SectionIndex]-1);
        SetLength(TempSelectedVertices, SectionLength[SectionIndex]-1);
        PointsBefore := EdgeToDelete - SectionStart[SectionIndex];
        PointsAfter := SectionLength[SectionIndex] - PointsBefore -1;

        if PointsAfter > 0 then
        begin
          Move(FPoints[EdgeToDelete + 1], TempPoints[0],
            PointsAfter * SizeOf(TPoint2D));
        end;
        if PointsBefore > 0 then
        begin
          Move(FPoints[SectionStart[SectionIndex]+1],
            TempPoints[PointsAfter],
            PointsBefore * SizeOf(TPoint2D));
        end;
        Move(TempPoints[0], FPoints[SectionStart[SectionIndex]],
          (SectionLength[SectionIndex]-1)  * SizeOf(TPoint2D));
        if SectionIndex < SectionCount - 1 then
        begin
          Move(FPoints[SectionEnd[SectionIndex]+1],
            FPoints[SectionEnd[SectionIndex]],
            (Count - SectionEnd[SectionIndex] - 1)  * SizeOf(TPoint2D));
        end;

        if PointsAfter > 0 then
        begin
          Move(FSelectedVertices[EdgeToDelete + 1], TempSelectedVertices[0],
            PointsAfter * SizeOf(boolean));
        end;
        if PointsBefore > 0 then
        begin
          Move(FSelectedVertices[SectionStart[SectionIndex]+1],
            TempSelectedVertices[PointsAfter],
            PointsBefore * SizeOf(boolean));
        end;
        Move(TempSelectedVertices[0],
          FSelectedVertices[SectionStart[SectionIndex]],
          (SectionLength[SectionIndex]-1)  * SizeOf(boolean));
        if SectionIndex < SectionCount - 1 then
        begin
          Move(FSelectedVertices[SectionEnd[SectionIndex]+1],
            FSelectedVertices[SectionEnd[SectionIndex]],
            (Count - SectionEnd[SectionIndex] - 1)  * SizeOf(boolean));
        end;
        for InnerSectionIndex := SectionIndex to SectionStarts.Count - 1 do
        begin
          Value := SectionStarts.IntValues[InnerSectionIndex];
          SectionStarts.IntValues[InnerSectionIndex] := Value-1;
        end;
        DeleteExtraSections;
        Count := Count -1;
        FNeedToResetSelectedVertexCount := True;
      end;

      break;
    end;
  end;

  Invalidate;
end;

procedure TScreenObject.CopyPoints(var Destination: TRealPointArray;
  const Position, Start: integer; var Count: integer);
begin
  if Count > self.Count - Start then
  begin
    Count := self.Count - Start;
  end;
  Assert(Length(Destination) >= Position + Count);
  Move(FPoints[Start], Destination[Position], Count * SizeOf(TPoint2D));
end;

procedure TScreenObject.RemoveElevationSubscription(Sender: TObject;
  const AName: string);
var
  Observer: TObserver;
begin
  if ElevationCount = ecOne then
  begin
    Assert(FElevSubscription <> nil);
    Observer := (FModel as TPhastModel).GetObserverByName(AName);
    Assert(Observer <> nil);
    Observer.StopsTalkingTo(FElevSubscription);
    Observer.StopsTalkingTo(self);
  end;
end;

procedure TScreenObject.RestoreElevationSubscription(Sender: TObject;
  const AName: string);
var
  Observer: TObserver;
begin
  if ElevationCount = ecOne then
  begin
    Assert(FElevSubscription <> nil);
    Observer := (FModel as TPhastModel).GetObserverByName(AName);
    Assert(Observer <> nil);
    Observer.TalksTo(FElevSubscription);
    Observer.TalksTo(self);
  end;
end;

procedure TScreenObject.RemoveHigherElevationSubscription(Sender: TObject;
  const AName: string);
var
  Observer: TObserver;
begin
  if ElevationCount = ecTwo then
  begin
    Assert(FTopElevSubscription <> nil);
    Observer := (FModel as TPhastModel).GetObserverByName(AName);
    Assert(Observer <> nil);
    Observer.StopsTalkingTo(FTopElevSubscription);
    Observer.StopsTalkingTo(self);
  end;
end;

procedure TScreenObject.RestoreHigherElevationSubscription(Sender: TObject;
  const AName: string);
var
  Observer: TObserver;
begin
  if ElevationCount = ecTwo then
  begin
    Assert(FTopElevSubscription <> nil);
    Observer := (FModel as TPhastModel).GetObserverByName(AName);
    Assert(Observer <> nil);
    Observer.TalksTo(FTopElevSubscription);
    Observer.TalksTo(self);
  end;
end;

procedure TScreenObject.RemoveLowerElevationSubscription(Sender: TObject;
  const AName: string);
var
  Observer: TObserver;
begin
  if ElevationCount = ecTwo then
  begin
    Assert(FBottomElevSubscription <> nil);
    Observer := (FModel as TPhastModel).GetObserverByName(AName);
    Assert(Observer <> nil);
    Observer.StopsTalkingTo(FBottomElevSubscription);
    Observer.StopsTalkingTo(self);
  end;
end;

procedure TScreenObject.RestoreLowerElevationSubscription(Sender: TObject;
  const AName: string);
var
  Observer: TObserver;
begin
  if ElevationCount = ecTwo then
  begin
    Assert(FBottomElevSubscription <> nil);
    Observer := (FModel as TPhastModel).GetObserverByName(AName);
    Assert(Observer <> nil);
    Observer.TalksTo(FBottomElevSubscription);
    Observer.TalksTo(self);
  end;
end;

procedure GlobalRemoveElevationSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TScreenObject).RemoveElevationSubscription(Sender, AName);
end;

procedure GlobalRestoreElevationSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TScreenObject).RestoreElevationSubscription(Sender, AName);
end;

procedure GlobalRemoveHigherElevationSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TScreenObject).RemoveHigherElevationSubscription(Sender, AName);
end;

procedure GlobalRestoreHigherElevationSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TScreenObject).RestoreHigherElevationSubscription(Sender, AName);
end;

procedure GlobalRemoveLowerElevationSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TScreenObject).RemoveLowerElevationSubscription(Sender, AName);
end;

procedure GlobalRestoreLowerElevationSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TScreenObject).RestoreLowerElevationSubscription(Sender, AName);
end;

procedure TScreenObject.SetElevationCount(const Value: TElevationCount);
var
  Index: integer;
  DataSet: TDataArray;
  OldElevationCount: TElevationCount;
  LocalModel: TPhastModel;
  List: TList;
  Dummy: string;
  TempElevationFormula: string;
  OldElevationFormula: string;
  OldHigherElevationFormula: string;
  OldLowerElevationFormula: string;
begin
  if FElevationCount <> Value then
  begin
    InvalidateModel;
    OldElevationFormula := ElevationFormula;
    OldLowerElevationFormula := LowerElevationFormula;
    OldHigherElevationFormula := HigherElevationFormula;
    OldElevationCount := FElevationCount;
    FElevationCount := Value;
    if (FModel <> nil) and not (csReading in FModel.ComponentState) then
    begin
      LocalModel := Model as TPhastModel;
      case OldElevationCount of
        ecZero:
          begin
            // do nothing
          end;
        ecOne:
          begin
            for Index := 0 to DataSetCount - 1 do
            begin
              DataSet := DataSets[Index];
              if FElevSubscription <> nil then
              begin
                FElevSubscription.StopsTalkingTo(DataSet);
              end;
            end;
            Dummy := '0';
            UpdateElevationSubscriptions(Dummy, OldElevationFormula);
            frmGoPhast.PhastModel.FormulaManager.Remove(FElevationFormulaObject,
              GlobalRemoveElevationSubscription,
              GlobalRestoreElevationSubscription, self);
            FElevationFormulaObject := nil;
          end;
        ecTwo:
          begin
            for Index := 0 to DataSetCount - 1 do
            begin
              DataSet := DataSets[Index];
              if FTopElevSubscription <> nil then
              begin
                FTopElevSubscription.StopsTalkingTo(DataSet);
              end;

              if FBottomElevSubscription <> nil then
              begin
                FBottomElevSubscription.StopsTalkingTo(DataSet);
              end;
            end;
            Dummy := '0';
            UpdateHigherElevationSubscriptions(Dummy, OldHigherElevationFormula);
            Dummy := '0';
            UpdateLowerElevationSubscriptions(Dummy, OldLowerElevationFormula);
            frmGoPhast.PhastModel.FormulaManager.Remove(FHigherElevationFormulaObject,
              GlobalRemoveHigherElevationSubscription,
              GlobalRestoreHigherElevationSubscription, self);
            FHigherElevationFormulaObject := nil;
            frmGoPhast.PhastModel.FormulaManager.Remove(FLowerElevationFormulaObject,
              GlobalRemoveLowerElevationSubscription,
              GlobalRestoreLowerElevationSubscription, self);
            FLowerElevationFormulaObject := nil;
          end;
      else
        Assert(False);
      end;
      case Value of
        ecZero:
          begin
            // do nothing
          end;
        ecOne:
          begin
            for Index := 0 to DataSetCount - 1 do
            begin
              DataSet := DataSets[Index];
              if FElevSubscription = nil then
              begin
                CreateElevationSubscription;
              end;
              FElevSubscription.TalksTo(DataSet);
            end;
            TempElevationFormula := ElevationFormula;
            UpdateElevationSubscriptions(TempElevationFormula, '0');
            ElevationFormula := TempElevationFormula;
          end;
        ecTwo:
          begin
            for Index := 0 to DataSetCount - 1 do
            begin
              DataSet := DataSets[Index];
              if FTopElevSubscription = nil then
              begin
                CreateTopElevationSubscription;
              end;
              FTopElevSubscription.TalksTo(DataSet);


              if FBottomElevSubscription = nil then
              begin
                CreateBottomElevationSubscription;
              end;
              FBottomElevSubscription.TalksTo(DataSet);
            end;
            TempElevationFormula := HigherElevationFormula;
            UpdateHigherElevationSubscriptions(TempElevationFormula, '0');
            HigherElevationFormula := TempElevationFormula;
            TempElevationFormula := LowerElevationFormula;
            UpdateLowerElevationSubscriptions(TempElevationFormula, '0');
            LowerElevationFormula := TempElevationFormula;
          end;
      else
        Assert(False);
      end;

      if (OldElevationCount in [ecOne, ecTwo])
        <> (Value in [ecOne, ecTwo]) then
      begin
        List := TList.Create;
        try
          LocalModel.GetLayerGroupDataSets(List);
          case OldElevationCount of
            ecZero:
              begin
                // do nothing
              end;
            ecOne,ecTwo:
              begin
                for Index := 0 to List.Count - 1 do
                begin
                  DataSet := List[Index];
                  DataSet.StopsTalkingTo(self);
                end;
              end;
            else Assert(False);
          end;
          case Value of
            ecZero:
              begin
                // do nothing
              end;
            ecOne,ecTwo:
              begin
                for Index := 0 to List.Count - 1 do
                begin
                  DataSet := List[Index];
                  DataSet.TalksTo(self);
                end;
              end;
            else Assert(False);
          end;
        finally
          List.Free;
        end;
      end;
    end;
    FElevationCount := Value;
    CreateFormulaObjects;
    Invalidate;
  end;
end;

procedure TScreenObject.SetElevationFormula(NewFormula: string);
var
  OldFormula: string;
  Dummy: string;
begin
  OldFormula := ElevationFormula;
  if OldFormula <> NewFormula then
  begin
    InvalidateModel;
    if not FCanInvalidateModel or (csReading in FModel.ComponentState) then
    begin
      if ElevationCount = ecOne then
      begin
        frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
          FElevationFormulaObject, NewFormula, Get1DCompiler,
          GlobalRemoveElevationSubscription,
          GlobalRestoreElevationSubscription, self);
      end;
      Exit;
    end;
    if ElevationCount = ecOne then
    begin
      UpdateElevationSubscriptions(NewFormula, OldFormula);
      frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
        FElevationFormulaObject, NewFormula, Get1DCompiler,
        GlobalRemoveElevationSubscription,
        GlobalRestoreElevationSubscription, self);
    end
    else
    begin
      Dummy:= '0';
      UpdateElevationSubscriptions(Dummy, OldFormula);
    end;
  end;
end;

procedure TScreenObject.ResetElevationFormula(const Compiler: TRbwParser;
  const ErrorMessage: string);
var
  ScreenObjectFunction: string;
begin
  ScreenObjectFunction := ElevationFormula;
  frmFormulaErrors.AddError(Name, '(Elevation Formula)',
    ScreenObjectFunction, ErrorMessage);
  ScreenObjectFunction := '0';
  ElevationFormula := '1';
  ElevationFormula := ScreenObjectFunction;
  if Compiler <> nil then
  begin
    Compiler.Compile(ScreenObjectFunction);
  end;
end;

procedure TScreenObject.ResetBottomElevationFormula(
  const Compiler: TRbwParser; const ErrorMessage: string);
var
  ScreenObjectFunction: string;
begin
  ScreenObjectFunction := LowerElevationFormula;
  frmFormulaErrors.AddError(Name, '(Lower Elevation Formula)',
    ScreenObjectFunction, ErrorMessage);
  ScreenObjectFunction := '0';
  LowerElevationFormula := '1';
  LowerElevationFormula := ScreenObjectFunction;
  if Compiler <> nil then
  begin
    Compiler.Compile(ScreenObjectFunction);
  end;
end;

procedure TScreenObject.ResetTopElevationFormula(const Compiler: TRbwParser;
  const ErrorMessage: string);
var
  ScreenObjectFunction: string;
begin
  ScreenObjectFunction := HigherElevationFormula;
  frmFormulaErrors.AddError(Name, '(Higher Elevation Formula)',
    ScreenObjectFunction, ErrorMessage);
  ScreenObjectFunction := '0';
  HigherElevationFormula := '1';
  HigherElevationFormula := ScreenObjectFunction;
  if Compiler <> nil then
  begin
    Compiler.Compile(ScreenObjectFunction);
  end;
end;

function TScreenObject.RestoreCellsFromCache(CellList: TCellAssignmentList;
  EvalAt: TEvaluatedAt; Orientation: TDataSetOrientation;
  AssignmentLocation: TAssignmentLocation; const EncloseAnnotation,
  IntersectAnnotation: string): boolean;
begin
//  result := False;
//  Exit;
  result := FCachedCells.RestoreFromCache(CellList, EvalAt,
    Orientation, AssignmentLocation, EncloseAnnotation, IntersectAnnotation);
  UpdateCurrentScreenObject(self);
end;

procedure TScreenObject.RestoreDataArraySubscription(Sender: TObject;
  const AName: string);
begin
  RestoreSubscriptionToList(FDataSetFormulas, AName, Sender,
    FDataSetSubscriptions);
end;

procedure TScreenObject.RestoreBoundaryDataArraySubscription(Sender: TObject;
  const AName: string);
begin
  RestoreSubscriptionToList(FBoundaryDataSetFormulas, AName, Sender,
    FBoundaryDataSetSubscriptions);
end;

procedure TScreenObject.ReverseDirection;
var
  Temp: TPoint2D;
  Index: integer;
  PointCount: integer;
  NewSectionStarts: array of integer;
begin
  PointCount := Count;
  for Index := 0 to PointCount div 2 - 1 do
  begin
    Temp := FPoints[Index];
    FPoints[Index] := FPoints[PointCount-Index-1];
    FPoints[PointCount-Index-1] := Temp;
  end;
  SetLength(NewSectionStarts, SectionStarts.Count);
  for Index := 0 to SectionStarts.Count - 1 do
  begin
    NewSectionStarts[Index] := PointCount - SectionStarts.IntValues[Index];
  end;
  for Index := 0 to SectionStarts.Count - 1 do
  begin
    SectionStarts.IntValues[Index] :=
      NewSectionStarts[SectionStarts.Count - Index -1]
  end;
  Invalidate;
end;

procedure TScreenObject.UpdateElevationSubscriptions(var NewFormula: string; OldFormula: string);
var
  OldUseList: TStringList;
  DS: TObserver;
  Index: Integer;
  Compiler: TRbwParser;
  NewUseList: TStringList;
  OtherIndex: Integer;
begin
  OldUseList := TStringList.Create;
  NewUseList := TStringList.Create;
  try
    Compiler := nil;
    case ViewDirection of
      vdTop:
        begin
          Compiler := GetCompiler(dsoTop, EvaluatedAt);
        end;
      vdFront:
        begin
          Compiler := GetCompiler(dsoFront, EvaluatedAt);
        end;
      vdSide:
        begin
          Compiler := GetCompiler(dsoSide, EvaluatedAt);
        end;
    else
      Assert(False);
    end;
    if OldFormula = '' then
    begin
      OldFormula := '0';
    end;
    Compiler.Compile(OldFormula);
    OldUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
    if NewFormula = '' then
    begin
      NewFormula := '0';
    end;
    OldFormula := NewFormula;
    try
      Compiler.Compile(OldFormula);
    except on E: ERbwParserError do
      begin
        OldFormula := '0';
        Compiler.Compile(OldFormula);
      end;
    end;
    NewUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
    NewFormula := Compiler.CurrentExpression.Decompile;
    for Index := OldUseList.Count - 1 downto 0 do
    begin
      OtherIndex := NewUseList.IndexOf(OldUseList[Index]);
      if OtherIndex >= 0 then
      begin
        OldUseList.Delete(Index);
        NewUseList.Delete(OtherIndex);
      end;
    end;
    for Index := 0 to OldUseList.Count - 1 do
    begin
      if FElevSubscription <> nil then
      begin
        DS := (FModel as TPhastModel).GetObserverByName(OldUseList[Index]);
        Assert(DS <> nil);
        DS.StopsTalkingTo(FElevSubscription);
        DS.StopsTalkingTo(self);
      end;
    end;
    for Index := 0 to NewUseList.Count - 1 do
    begin
      if FElevSubscription = nil then
      begin
        CreateElevationSubscription;
      end;
      DS := (FModel as TPhastModel).GetObserverByName(NewUseList[Index]);
      Assert(DS <> nil);
      DS.TalksTo(FElevSubscription);
      DS.TalksTo(self);
    end;
    Invalidate;
  finally
    OldUseList.Free;
    NewUseList.Free;
  end;
  if FElevSubscription <> nil then
  begin
    FElevSubscription.UpToDate := False;
  end;
  Invalidate;
end;

procedure TScreenObject.UpdateLowerElevationSubscriptions(
  var NewFormula: string; OldFormula: string);
var
  DS: TObserver;
  OtherIndex: Integer;
  Index: Integer;
  Compiler: TRbwParser;
  NewUseList: TStringList;
  OldUseList: TStringList;
begin
  OldUseList := TStringList.Create;
  NewUseList := TStringList.Create;
  try
    Compiler := nil;
    case ViewDirection of
      vdTop:
        begin
          Compiler := GetCompiler(dsoTop, EvaluatedAt);
        end;
      vdFront:
        begin
          Compiler := GetCompiler(dsoFront, EvaluatedAt);
        end;
      vdSide:
        begin
          Compiler := GetCompiler(dsoSide, EvaluatedAt);
        end;
    else
      Assert(False);
    end;
    if OldFormula = '' then
    begin
      OldFormula := '0';
    end;
    Compiler.Compile(OldFormula);
    OldUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
    if NewFormula = '' then
    begin
      NewFormula := '0';
    end;
    OldFormula := NewFormula;
    try
      Compiler.Compile(OldFormula);
    except on E: ERbwParserError do
      begin
        OldFormula := '0';
        Compiler.Compile(OldFormula);
      end;
    end;
    NewUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
    NewFormula := Compiler.CurrentExpression.Decompile;
    for Index := OldUseList.Count - 1 downto 0 do
    begin
      OtherIndex := NewUseList.IndexOf(OldUseList[Index]);
      if OtherIndex >= 0 then
      begin
        OldUseList.Delete(Index);
        NewUseList.Delete(OtherIndex);
      end;
    end;
    for Index := 0 to OldUseList.Count - 1 do
    begin
      if FBottomElevSubscription <> nil then
      begin
        DS := (FModel as TPhastModel).GetObserverByName(OldUseList[Index]);
        Assert(DS <> nil);
        DS.StopsTalkingTo(FBottomElevSubscription);
        DS.StopsTalkingTo(self);
      end;
    end;
    for Index := 0 to NewUseList.Count - 1 do
    begin
      if FBottomElevSubscription = nil then
      begin
        CreateBottomElevationSubscription;
      end;
      DS := (FModel as TPhastModel).GetObserverByName(NewUseList[Index]);
      Assert(DS <> nil);
      DS.TalksTo(FBottomElevSubscription);
      DS.TalksTo(self);
    end;
    Invalidate;
  finally
    OldUseList.Free;
    NewUseList.Free;
  end;
  if FBottomElevSubscription <> nil then
  begin
    FBottomElevSubscription.UpToDate := False;
  end;
  Invalidate;
end;

procedure TScreenObject.UpdateHigherElevationSubscriptions(
  var NewFormula: string; OldFormula: string);
var
  DS: TObserver;
  OtherIndex: Integer;
  Index: Integer;
  Compiler: TRbwParser;
  NewUseList: TStringList;
  OldUseList: TStringList;
begin
  OldUseList := TStringList.Create;
  NewUseList := TStringList.Create;
  try
    Compiler := nil;
    case ViewDirection of
      vdTop:
        begin
          Compiler := GetCompiler(dsoTop, EvaluatedAt);
        end;
      vdFront:
        begin
          Compiler := GetCompiler(dsoFront, EvaluatedAt);
        end;
      vdSide:
        begin
          Compiler := GetCompiler(dsoSide, EvaluatedAt);
        end;
    else
      Assert(False);
    end;
    if OldFormula = '' then
    begin
      OldFormula := '0';
    end;
    Compiler.Compile(OldFormula);
    OldUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
    if NewFormula = '' then
    begin
      NewFormula := '0';
    end;
    OldFormula := NewFormula;
    try
      Compiler.Compile(OldFormula);
    except on E: ERbwParserError do
      begin
        OldFormula := '0';
        Compiler.Compile(OldFormula);
      end;
    end;
    NewUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
    NewFormula := Compiler.CurrentExpression.Decompile;
    for Index := OldUseList.Count - 1 downto 0 do
    begin
      OtherIndex := NewUseList.IndexOf(OldUseList[Index]);
      if OtherIndex >= 0 then
      begin
        OldUseList.Delete(Index);
        NewUseList.Delete(OtherIndex);
      end;
    end;
    for Index := 0 to OldUseList.Count - 1 do
    begin
      if FTopElevSubscription <> nil then
      begin
        DS := (FModel as TPhastModel).GetObserverByName(OldUseList[Index]);
        Assert(DS <> nil);
        DS.StopsTalkingTo(FTopElevSubscription);
        DS.StopsTalkingTo(self);
      end;
    end;
    for Index := 0 to NewUseList.Count - 1 do
    begin
      if FTopElevSubscription = nil then
      begin
        CreateTopElevationSubscription;
      end;
      DS := (FModel as TPhastModel).GetObserverByName(NewUseList[Index]);
      Assert(DS <> nil);
      DS.TalksTo(FTopElevSubscription);
      DS.TalksTo(self);
    end;
    Invalidate;
  finally
    OldUseList.Free;
    NewUseList.Free;
  end;
  if FTopElevSubscription <> nil then
  begin
    FTopElevSubscription.UpToDate := False;
  end;
  Invalidate;
end;

procedure TScreenObject.SetHigherElevationFormula(NewFormula: string);
var
  OldFunction: string;
  Dummy: string;
begin
  OldFunction := HigherElevationFormula;
  if OldFunction <> NewFormula then
  begin
    InvalidateModel;
    if  not FCanInvalidateModel or (csReading in FModel.ComponentState) then
    begin
      if ElevationCount = ecTwo then
      begin
        frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
          FHigherElevationFormulaObject, NewFormula, Get1DCompiler,
          GlobalRemoveHigherElevationSubscription,
          GlobalRestoreHigherElevationSubscription, self);
      end;
      Exit;
    end;
    if ElevationCount = ecTwo then
    begin
      UpdateHigherElevationSubscriptions(NewFormula, OldFunction);
      frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
        FHigherElevationFormulaObject, NewFormula, Get1DCompiler,
        GlobalRemoveHigherElevationSubscription,
        GlobalRestoreHigherElevationSubscription, self);
    end
    else
    begin
      Dummy := '0';
      UpdateHigherElevationSubscriptions(Dummy, OldFunction);
    end;
  end;
end;

procedure TScreenObject.SetLowerElevationFormula(NewFormula: string);
var
  OldFormula: string;
  Dummy: string;
begin
  OldFormula := LowerElevationFormula;
  if OldFormula <> NewFormula then
  begin
    InvalidateModel;
    if  not FCanInvalidateModel or (csReading in FModel.ComponentState) then
    begin
      if ElevationCount = ecTwo then
      begin
        frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
          FLowerElevationFormulaObject, NewFormula, Get1DCompiler,
          GlobalRemoveLowerElevationSubscription,
          GlobalRestoreLowerElevationSubscription, self);
      end;
      Exit;
    end;

    if ElevationCount = ecTwo then
    begin
      UpdateLowerElevationSubscriptions(NewFormula, OldFormula);
      frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
        FLowerElevationFormulaObject, NewFormula, Get1DCompiler,
        GlobalRemoveLowerElevationSubscription,
        GlobalRestoreLowerElevationSubscription, self);
    end
    else
    begin
      Dummy := '0';
      UpdateLowerElevationSubscriptions(Dummy, OldFormula);
    end;
  end;
end;

procedure TScreenObject.SetModflowChdBoundary(const Value: TChdBoundary);
begin
  if (Value = nil) or not Value.Used then
  begin
    if ModflowBoundaries.FModflowChdBoundary <> nil then
    begin
      InvalidateModel;
    end;
    FreeAndNil(ModflowBoundaries.FModflowChdBoundary);
  end
  else
  begin
    CreateChdBoundary;
    ModflowBoundaries.FModflowChdBoundary.Assign(Value);
  end;
end;

procedure TScreenObject.SetModflowDrnBoundary(const Value: TDrnBoundary);
begin
  if (Value = nil) or not Value.Used then
  begin
    if ModflowBoundaries.FModflowDrnBoundary <> nil then
    begin
      InvalidateModel;
    end;
    FreeAndNil(ModflowBoundaries.FModflowDrnBoundary);
  end
  else
  begin
    CreateDrnBoundary;
    ModflowBoundaries.FModflowDrnBoundary.Assign(Value);
  end;
end;

procedure TScreenObject.SetModflowDrtBoundary(const Value: TDrtBoundary);
begin
  if (Value = nil) or not Value.Used then
  begin
    if ModflowBoundaries.FModflowDrtBoundary <> nil then
    begin
      InvalidateModel;
    end;
    FreeAndNil(ModflowBoundaries.FModflowDrtBoundary);
  end
  else
  begin
    CreateDrtBoundary;
    ModflowBoundaries.FModflowDrtBoundary.Assign(Value);
  end;
end;

procedure TScreenObject.SetModflowEtsBoundary(const Value: TEtsBoundary);
begin
  if (Value = nil) or not Value.Used then
  begin
    if ModflowBoundaries.FModflowEtsBoundary <> nil then
    begin
      InvalidateModel;
    end;
    FreeAndNil(ModflowBoundaries.FModflowEtsBoundary);
  end
  else
  begin
    CreateEtsBoundary;
    ModflowBoundaries.FModflowEtsBoundary.Assign(Value);
  end;
end;

procedure TScreenObject.SetModflowEvtBoundary(const Value: TEvtBoundary);
begin
  if (Value = nil) or not Value.Used then
  begin
    if ModflowBoundaries.FModflowEvtBoundary <> nil then
    begin
      InvalidateModel;
    end;
    FreeAndNil(ModflowBoundaries.FModflowEvtBoundary);
  end
  else
  begin
    CreateEvtBoundary;
    ModflowBoundaries.FModflowEvtBoundary.Assign(Value);
  end;
end;

procedure TScreenObject.SetModflowGhbBoundary(const Value: TGhbBoundary);
begin
  if (Value = nil) or not Value.Used then
  begin
    if ModflowBoundaries.FModflowGhbBoundary <> nil then
    begin
      InvalidateModel;
    end;
    FreeAndNil(ModflowBoundaries.FModflowGhbBoundary);
  end
  else
  begin
    CreateGhbBoundary;
    ModflowBoundaries.FModflowGhbBoundary.Assign(Value);
  end;
end;

procedure TScreenObject.SetModflowHeadObservations(const Value: THobBoundary);
begin
  if (Value = nil) or not Value.Used then
  begin
    if ModflowBoundaries.FModflowHeadObservations <> nil then
    begin
      InvalidateModel;
    end;
    FreeAndNil(ModflowBoundaries.FModflowHeadObservations);
  end
  else
  begin
    CreateHeadObservations;
    ModflowBoundaries.FModflowHeadObservations.Assign(Value);
  end;
end;

procedure TScreenObject.SetModflowHfbBoundary(const Value: THfbBoundary);
begin
  if (Value = nil) or not Value.Used then
  begin
    if ModflowBoundaries.FModflowHfbBoundary <> nil then
    begin
      InvalidateModel;
    end;
    FreeAndNil(ModflowBoundaries.FModflowHfbBoundary);
  end
  else
  begin
    CreateHfbBoundary;
    ModflowBoundaries.FModflowHfbBoundary.Assign(Value);
  end;
end;

procedure TScreenObject.SetModflowLakBoundary(const Value: TLakBoundary);
begin
  if (Value = nil) or not Value.Used then
  begin
    if ModflowBoundaries.FModflowLakBoundary <> nil then
    begin
      InvalidateModel;
    end;
    FreeAndNil(ModflowBoundaries.FModflowLakBoundary);
  end
  else
  begin
    CreateLakBoundary;
    ModflowBoundaries.FModflowLakBoundary.Assign(Value);
  end;
end;

procedure TScreenObject.SetModflowMnw2Boundary(const Value: TMnw2Boundary);
begin
  if (Value = nil) or not Value.Used then
  begin
    if ModflowBoundaries.FModflowMnw2Boundary <> nil then
    begin
      InvalidateModel;
    end;
    FreeAndNil(ModflowBoundaries.FModflowMnw2Boundary);
  end
  else
  begin
    CreateMnw2Boundary;
    ModflowBoundaries.FModflowMnw2Boundary.Assign(Value);
  end;
end;

procedure TScreenObject.SetModflowRchBoundary(const Value: TRchBoundary);
begin
  if (Value = nil) or not Value.Used then
  begin
    if ModflowBoundaries.FModflowRchBoundary <> nil then
    begin
      InvalidateModel;
    end;
    FreeAndNil(ModflowBoundaries.FModflowRchBoundary);
  end
  else
  begin
    CreateRchBoundary;
    ModflowBoundaries.FModflowRchBoundary.Assign(Value);
  end;
end;

procedure TScreenObject.SetModflowResBoundary(const Value: TResBoundary);
begin
  if (Value = nil) or not Value.Used then
  begin
    if ModflowBoundaries.FModflowResBoundary <> nil then
    begin
      InvalidateModel;
    end;
    FreeAndNil(ModflowBoundaries.FModflowResBoundary);
  end
  else
  begin
    CreateResBoundary;
    ModflowBoundaries.FModflowResBoundary.Assign(Value);
  end;
end;

procedure TScreenObject.SetModflowRivBoundary(const Value: TRivBoundary);
begin
  if (Value = nil) or not Value.Used then
  begin
    if ModflowBoundaries.FModflowRivBoundary <> nil then
    begin
      InvalidateModel;
    end;
    FreeAndNil(ModflowBoundaries.FModflowRivBoundary);
  end
  else
  begin
    CreateRivBoundary;
    ModflowBoundaries.FModflowRivBoundary.Assign(Value);
  end;
end;

procedure TScreenObject.SetModflowSfrBoundary(const Value: TSfrBoundary);
begin
  if (Value = nil) or not Value.Used then
  begin
    if ModflowBoundaries.FModflowSfrBoundary <> nil then
    begin
      InvalidateModel;
    end;
    FreeAndNil(ModflowBoundaries.FModflowSfrBoundary);
  end
  else
  begin
    CreateSfrBoundary;
    ModflowBoundaries.FModflowSfrBoundary.Assign(Value);
  end;
end;

procedure TScreenObject.SetModflowStreamGage(const Value: TStreamGage);
begin
  if (Value = nil) or not Value.Used then
  begin
    if ModflowBoundaries.FModflowGage <> nil then
    begin
      InvalidateModel;
    end;
    FreeAndNil(ModflowBoundaries.FModflowGage);
  end
  else
  begin
    CreateGagBoundary;
    ModflowBoundaries.FModflowGage.Assign(Value);
  end;
end;

procedure TScreenObject.SetModflowUzfBoundary(const Value: TUzfBoundary);
begin
  if (Value = nil) or not Value.Used then
  begin
    if ModflowBoundaries.FModflowUzfBoundary <> nil then
    begin
      InvalidateModel;
    end;
    FreeAndNil(ModflowBoundaries.FModflowUzfBoundary);
  end
  else
  begin
    CreateUzfBoundary;
    ModflowBoundaries.FModflowUzfBoundary.Assign(Value);
  end;
end;

procedure TScreenObject.SetModflowWellBoundary(const Value: TMfWellBoundary);
begin
  if (Value = nil) or not Value.Used then
  begin
    if ModflowBoundaries.FModflowWellBoundary <> nil then
    begin
      InvalidateModel;
    end;
    FreeAndNil(ModflowBoundaries.FModflowWellBoundary);
  end
  else
  begin
    CreateWelBoundary;
    ModflowBoundaries.FModflowWellBoundary.Assign(Value);
  end;
end;

procedure TScreenObject.SetModpathParticles(const Value: TParticleStorage);
begin
  FModpathParticles.Assign(Value);
end;

procedure TScreenObject.Draw2Elev(
  Const Bitmap32: TBitmap32;
  const Direction: TViewDirection; const DrawAsSelected: Boolean);
var
  HighPoints: T2DRealPointArray;
  LowPoints: T2DRealPointArray;
  OuterPointIndex, InnerPointIndex: integer;
  RealPoint: TPoint2D;
  function UsePoint: boolean;
  var
    HighPoint, LowPoint: TPoint2D;
  begin
    if ViewDirection = vdSide then
    begin
      if Direction = vdTop then
      begin
        HighPoint := HighPoints[OuterPointIndex, InnerPointIndex];
        LowPoint := LowPoints[OuterPointIndex, InnerPointIndex];
        HighPoint :=
          (FModel as TPhastModel).Grid.
          RotateFromRealWorldCoordinatesToGridCoordinates(HighPoint);
        LowPoint :=
          (FModel as TPhastModel).Grid.
          RotateFromRealWorldCoordinatesToGridCoordinates(LowPoint);
        result := HighPoint.X >= LowPoint.X;
      end
      else
      begin
        result := HighPoints[OuterPointIndex, InnerPointIndex].X
          >= LowPoints[OuterPointIndex, InnerPointIndex].X
      end;
    end
    else
    begin
      if Direction = vdTop then
      begin
        HighPoint := HighPoints[OuterPointIndex, InnerPointIndex];
        LowPoint := LowPoints[OuterPointIndex, InnerPointIndex];
        HighPoint :=
          (FModel as TPhastModel).Grid.
          RotateFromRealWorldCoordinatesToGridCoordinates(HighPoint);
        LowPoint :=
          (FModel as TPhastModel).Grid.
          RotateFromRealWorldCoordinatesToGridCoordinates(LowPoint);
        result := HighPoint.Y >= LowPoint.Y;
      end
      else
      begin
        result := HighPoints[OuterPointIndex, InnerPointIndex].Y
          >= LowPoints[OuterPointIndex, InnerPointIndex].Y
      end;
    end;

  end;
  function ConvertPoint: TPoint;
  begin
    Assert(FModel <> nil);
    result := (FModel as TPhastModel).ConvertPoint(Direction, RealPoint);
  end;
begin
  if Deleted then
    Exit;
  if Direction = ViewDirection then
  begin
    Draw0Elev(Bitmap32, Direction, DrawAsSelected);
  end
  else
  begin
    if (Model = nil)
      or ((Model as TPhastModel).ModelSelection = msModflow) then
    begin
      Draw2ElevModflow(Direction, Bitmap32);
      Exit;
    end
    else if ((Model as TPhastModel).ModelSelection = msPhast) then
    begin
      Draw2ElevPhast(Direction, Bitmap32);
      Exit;
    end;
    Assert(False);
  end;
end;

procedure TScreenObject.SetCellSize(const Value: real);
begin
  if Value > 0 then
  begin
    if FCellSize <> Value then
    begin
      FCellSize := Value;
      InvalidateModel;
    end;
  end;
end;

procedure TScreenObject.SetCellSizeUsed(const Value: boolean);
begin
  if FCellSizeUsed <> Value then
  begin
    FCellSizeUsed := Value;
    InvalidateModel;
  end;
end;

procedure TScreenObject.SetEvaluatedAt(const Value: TEvaluatedAt);
begin
  if FEvaluatedAt <> Value then
  begin
    FEvaluatedAt := Value;
    InvalidateModel;
    SetFormulaParsers;
    Invalidate;
  end;
end;

procedure TScreenObject.ResetDataSetSubscriptions;
var
  Index: integer;
  Subscription: TObserver;
begin
  for Index := 0 to FDataSetSubscriptions.Count - 1 do
  begin
    Subscription := FDataSetSubscriptions[Index] as TObserver;
    Subscription.UpToDate := True;
  end;
end;

function TScreenObject.Get1DCompiler: TRbwParser;
begin
  result := nil;
  case ViewDirection of
    vdTop: result := frmGoPhast.PhastModel.GetCompiler(dsoTop, EvaluatedAt);
    vdFront: result := frmGoPhast.PhastModel.GetCompiler(dsoFront, EvaluatedAt);
    vdSide: result := frmGoPhast.PhastModel.GetCompiler(dsoSide, EvaluatedAt);
    else Assert(False);
  end;
end;

function TScreenObject.GetACol(const Grid: TCustomGrid; const X: double):
  integer;
begin
  result := Grid.NearestColumnPosition(X);

  if result < 0 then
  begin
    result := 0;
  end;
  case EvaluatedAt of
    eaBlocks:
      begin
        if ((result > Grid.ColumnCount) or (Grid.ColumnPosition[result] > X))
          and (result > 0) then
        begin
          Dec(result);
        end;
        // the following TODO may be incorrect.
        if result > Grid.ColumnCount then
        begin
          result := Grid.ColumnCount;
        end;

        { TODO : Can this be added here? }
        // The following would allow combinging this function
        // with GetCol.  However, does it work for other cases?
        {if (result >= Grid.ColumnCount) then
        begin
          result := Grid.ColumnCount - 1;
        end;} 
      end;
    eaNodes:
      begin
        // do nothing
      end;
  else
    Assert(False);
  end;
//  if result > Grid.ColumnCount then
//  begin
//    result := Grid.ColumnCount;
//  end;
end;

function TScreenObject.GetAPhastLayer(const Grid: TCustomGrid; const Z: double):
  integer;
begin
  result := (Grid as TPhastGrid).NearestLayerPosition(Z);
  if result < 0 then
  begin
    result := 0;
  end;
  case EvaluatedAt of
    eaBlocks:
      begin
        if ((result > Grid.LayerCount)
          or ((Grid as TPhastGrid).LayerElevation[result] > Z))
          and (result > 0) then
        begin
          Dec(result);
        end;
        // This  is inconsistent with the following comment
        if result > Grid.LayerCount then
        begin
          result := Grid.LayerCount
        end;

        { TODO : Can this be added here? }
        // The following would allow combinging this function
        // with GetLayer.  However, does it work for other cases?
        {if (result >= Grid.LayerCount) then
        begin
          result := Grid.LayerCount - 1
        end;}
      end;
    eaNodes:
      begin
        // do nothing
      end;
  else
    Assert(False);
  end;
//  if result > Grid.LayerCount then
//  begin
//    result := Grid.LayerCount
//  end;
end;

function TScreenObject.GetARow(const Grid: TCustomGrid; const Y: double):
  integer;
begin
  result := Grid.NearestRowPosition(Y);
  if result < 0 then
  begin
    result := 0;
  end;
  case EvaluatedAt of
    eaBlocks:
      begin
        case Grid.RowDirection of
          rdSouthToNorth:
            begin
              if ((result > Grid.RowCount) or (Grid.RowPosition[result] > Y))
                and (result > 0) then
              begin
                Dec(result);
              end;
            end;
          rdNorthToSouth:
            begin
              if ((result > Grid.RowCount) or (Grid.RowPosition[result] <= Y))
                and (result > 0) then
              begin
                Dec(result);
              end;
            end;
          else Assert(False);
        end;
        // the following TODO may be incorrect
        if result > Grid.RowCount then
        begin
          result := Grid.RowCount
        end;

        { TODO : Can this be added here? }
        // The following would allow combinging this function
        // with GetRow.  However, does it work for other cases?
        {if (result >= Grid.RowCount) then
        begin
          result := Grid.RowCount - 1
        end; }
      end;
    eaNodes:
      begin
      end;
  else
    Assert(False);
  end;
//  if result > Grid.RowCount then
//  begin
//    result := Grid.RowCount
//  end;
end;

function TScreenObject.DataSetUsed(const DataSet: TDataArray;
  var OtherData: TObject): boolean;
var
  DataSetIndex: integer;
  CouldBeBoundary: boolean;
begin
  if DataSet is TSparseArrayPhastInterpolationDataSet then
  begin
    result := True;
  end
  else if DataSet is TCustomPhastDataSet then
  begin
    OtherData := nil;
    CouldBeBoundary := False;
    DataSetIndex := IndexOfDataSet(DataSet);
    if DataSetIndex < 0 then
    begin
      CouldBeBoundary := True;
      DataSetIndex := IndexOfBoundaryDataSet(DataSet);
    end;
    result := DataSetIndex >= 0;
    if result and not CouldBeBoundary then
    begin
      OtherData := FInterpValues.Items[DataSetIndex] as TInterpValuesItem;
    end;
  end
  else
  begin
    OtherData := nil;
    result := (IndexOfDataSet(DataSet) >= 0)
      or (IndexOfBoundaryDataSet(DataSet) >= 0)
      or (DataSet = (FModel as TPhastModel).TopBoundaryType)
      or (DataSet = (FModel as TPhastModel).FrontBoundaryType)
      or (DataSet = (FModel as TPhastModel).SideBoundaryType)
      or (DataSet = (FModel as TPhastModel).Top2DBoundaryType);
    if result then
    begin
      if (DataSet = (FModel as TPhastModel).TopBoundaryType)
        or (DataSet = (FModel as TPhastModel).FrontBoundaryType)
        or (DataSet = (FModel as TPhastModel).SideBoundaryType) then
      begin
        result := PhastBoundaryType in [btSpecifiedHead, btFlux, btLeaky]
      end
      else if (DataSet = (FModel as TPhastModel).Top2DBoundaryType) then
      begin
        result := PhastBoundaryType in [btRiver, btWell];
      end;
    end;
  end;
end;

function TScreenObject.GetBoundaryDataSetFormulas(
  const Index: integer): string;
var
  FormulaObject: TFormulaObject;
begin
  Assert(FBoundaryDataSetFormulas <> nil);
  FormulaObject := FBoundaryDataSetFormulas[Index];
  if FormulaObject = nil then
  begin
    result := '0';
  end
  else
  begin
    result := FormulaObject.Formula;
  end;
//  result := FBoundaryDataSetFormulas[Index];
end;

procedure TScreenObject.SetBoundaryDataSetFormulas(const Index: integer;
  const Value: string);
var
  Observer: TObserver;
  OldUseList: TStringList;
  NewUseList: TStringList;
  UseIndex: integer;
  OtherIndex: integer;
  AFunction, OldFunction: string;
  Compiler: TRbwParser;
  ADataSet: TDataArray;
  DS: TObserver;
  AFormulaObject: TFormulaObject;
begin
  Assert(FBoundaryDataSetFormulas <> nil);
  AFormulaObject := FBoundaryDataSetFormulas[Index];
  if AFormulaObject = nil then
  begin
    AFunction := '';
  end
  else
  begin
    AFunction := AFormulaObject.Formula;
  end;
//  AFunction := FBoundaryDataSetFormulas[Index];
  Observer := nil;
  if AFunction <> Value then
  begin
    if FCanInvalidateModel then
    begin
      InvalidateModel;
      OldFunction := AFunction;
      try
        ADataSet := FBoundaryDataSets[Index];
        Assert(FBoundaryDataSetSubscriptions <> nil);
        Observer := FBoundaryDataSetSubscriptions[Index] as TObserver;
        OldUseList := TStringList.Create;
        NewUseList := TStringList.Create;
        try
          Compiler := GetCompiler(ADataSet.Orientation,
            ADataSet.EvaluatedAt);
          if AFunction = '' then
          begin
            AFunction := '0'
          end;
          try
            Compiler.Compile(AFunction);
            OldUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
          except on E: ERbwParserError do
              OldUseList.Clear;
          end;

          AFunction := Value;
          Compiler.Compile(AFunction);
          NewUseList.Assign(Compiler.CurrentExpression.VariablesUsed);

          CreateOrRetrieveBoundaryFormulaObject(Index, ADataSet, AFormulaObject);
          frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
            AFormulaObject, Compiler.CurrentExpression.Decompile, Compiler,
            GlobalRemoveBoundaryDataArraySubscription,
            GlobalRestoreBoundaryDataArraySubscription, self);
          FBoundaryDataSetFormulas[Index]:= AFormulaObject;

          for UseIndex := OldUseList.Count - 1 downto 0 do
          begin
            OtherIndex := NewUseList.IndexOf(OldUseList[UseIndex]);
            if OtherIndex >= 0 then
            begin
              OldUseList.Delete(UseIndex);
              NewUseList.Delete(OtherIndex);
            end;
          end;
          for UseIndex := 0 to OldUseList.Count - 1 do
          begin
            DS := (FModel as TPhastModel).GetObserverByName(
              OldUseList[UseIndex]);
            Assert(DS <> nil);
            DS.StopsTalkingTo(Observer);
          end;
          for UseIndex := 0 to NewUseList.Count - 1 do
          begin
            DS := (FModel as TPhastModel).GetObserverByName(
              NewUseList[UseIndex]);
            Assert(DS <> nil);
            DS.TalksTo(Observer);
          end;
          Invalidate;
        finally
          OldUseList.Free;
          NewUseList.Free;
        end;

        if not (csDestroying in Model.ComponentState) then
        begin
          Observer.UpToDate := True;
          Observer.UpToDate := False;
          BoundaryDataSets[Index].Invalidate;
          Observer.UpToDate := True;
        end;
      finally
        if not (csDestroying in Model.ComponentState) then
        begin
          if Observer.IsRecursive then
          begin
            BoundaryDataSetFormulas[Index] := OldFunction;
          end;
        end;
      end;
    end
    else
    begin
      ADataSet := BoundaryDataSets[Index];
      Compiler := frmGoPhast.PhastModel.GetCompiler(ADataSet.Orientation,
        ADataSet.EvaluatedAt);
      CreateOrRetrieveBoundaryFormulaObject(Index, ADataSet, AFormulaObject);
      frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
        AFormulaObject, Value, Compiler,
        GlobalRemoveBoundaryDataArraySubscription,
        GlobalRestoreBoundaryDataArraySubscription, self);
      FBoundaryDataSetFormulas[Index] := AFormulaObject;
    end;
  end;
end;

function TScreenObject.GetBoundaryDataSets(const Index: integer): TDataArray;
begin
  Assert(FBoundaryDataSets <> nil);
  result := FBoundaryDataSets[Index];
end;

procedure TScreenObject.SetBoundaryDataSets(const Index: integer;
  const DataSet: TDataArray);
var
  OldIndex: integer;
  FormulaObject: TFormulaObject;
begin
  CreateBoundaryDataSets;
  // Determine the position of DataSet in FDataSets.
  // If OldIndex < 0, DataSet is not in FDataSets.
  OldIndex := FBoundaryDataSets.IndexOf(DataSet);

  // Store DataSet.
  FBoundaryDataSets[Index] := DataSet;
  if (OldIndex >= 0) and (OldIndex <> Index) then
  begin
    // If DataSet was already in FDataSets, move it's funciton
    // to the new position and delete the old copy of both the
    // data set and the function.
    Assert(FBoundaryDataSetFormulas <> nil);
    FBoundaryDataSetFormulas[Index] := FBoundaryDataSetFormulas[OldIndex];
    FBoundaryDataSets.Delete(OldIndex);
    FormulaObject := FBoundaryDataSetFormulas[OldIndex];
    frmGoPhast.PhastModel.FormulaManager.Remove(FormulaObject,
      GlobalRemoveBoundaryDataArraySubscription,
      GlobalRestoreBoundaryDataArraySubscription, self);
    FBoundaryDataSetFormulas.Delete(OldIndex);
  end;
  InvalidateModel;
end;

procedure TScreenObject.ResetBoundaryDataSetSubscriptions;
var
  Index: integer;
  Subscription: TObserver;
begin
  if FBoundaryDataSetSubscriptions <> nil then
  begin
    for Index := 0 to FBoundaryDataSetSubscriptions.Count - 1 do
    begin
      Subscription := FBoundaryDataSetSubscriptions[Index] as TObserver;
      Subscription.UpToDate := True;
    end;
  end;
end;

procedure TScreenObject.ClearBoundaryDataSets;
var
  Index: integer;
  ADataSet: TDataArray;
  Observer: TObserver;
  FormulaObject: TFormulaObject;
begin
  CreateBoundaryDataSetSubscriptions;
  CreateBoundaryDataSets;
  Assert(FBoundaryDataSetSubscriptions.Count = FBoundaryDataSets.Count);
  for Index := 0 to FBoundaryDataSets.Count - 1 do
  begin
    // Get rid of any subscriptions due to the formula.
    BoundaryDataSetFormulas[Index] := '0';
    ADataSet := FBoundaryDataSets[Index];
    Observer := FBoundaryDataSetSubscriptions[Index] as TObserver;
    if FElevSubscription <> nil then
    begin
      FElevSubscription.StopsTalkingTo(ADataSet);
    end;
    if FTopElevSubscription <> nil then
    begin
      FTopElevSubscription.StopsTalkingTo(ADataSet);
    end;
    if FBottomElevSubscription <> nil then
    begin
      FBottomElevSubscription.StopsTalkingTo(ADataSet);
    end;
    self.StopsTalkingTo(ADataSet);
    Observer.StopsTalkingTo(ADataSet);
  end;

  UpToDate := False;
  FBoundaryDataSets.Clear;
  if FBoundaryDataSetFormulas <> nil then
  begin
    for Index := 0 to FBoundaryDataSetFormulas.Count - 1 do
    begin
      FormulaObject := FBoundaryDataSetFormulas[Index];
      frmGoPhast.PhastModel.FormulaManager.Remove(FormulaObject,
        GlobalRemoveBoundaryDataArraySubscription,
        GlobalRestoreBoundaryDataArraySubscription, self);
    end;
    FBoundaryDataSetFormulas.Clear;
  end;
  for Index := 0 to FBoundaryDataSetSubscriptions.Count -1 do
  begin
    Observer := FBoundaryDataSetSubscriptions[Index] as TObserver;
    self.StopsTalkingTo(Observer);
  end;
  FBoundaryDataSetSubscriptions.Clear;
  InvalidateModel;
end;

procedure TScreenObject.InsertBoundaryDataSet(const Index: Integer;
  const DataSet: TDataArray);
var
  Subscription: TObserver;
begin
  CreateBoundaryDataSets;
  if FBoundaryDataSets.IndexOf(DataSet) < 0 then
  begin
    InvalidateModel;
    Subscription := TObserver.Create(nil);
    Subscription.UpdateWithName(DataSet.Name + Name);
    CreateBoundaryDataSetSubscriptions;
    FBoundaryDataSetSubscriptions.Insert(Index, Subscription);

    FBoundaryDataSets.Insert(Index, DataSet);
    CreateBoundaryDataSetFormulas;
    FBoundaryDataSetFormulas.Insert(Index, nil);
    self.TalksTo(DataSet);
    Subscription.TalksTo(DataSet);
    self.TalksTo(Subscription);
    case ElevationCount of
      ecZero:
        begin
          // do nothing
        end;
      ecOne:
        begin
          if FElevSubscription = nil then
          begin
            CreateElevationSubscription;
          end;
          FElevSubscription.TalksTo(DataSet);
        end;
      ecTwo:
        begin
          if FTopElevSubscription = nil then
          begin
            CreateTopElevationSubscription;
          end;
          FTopElevSubscription.TalksTo(DataSet);

          if FBottomElevSubscription = nil then
          begin
            CreateBottomElevationSubscription;
          end;
          FBottomElevSubscription.TalksTo(DataSet);
        end;
    else
      Assert(False);
    end;
    DataSet.Invalidate;
  end;
end;

function TScreenObject.GetEdgePoints(const Index: integer): TEdgePoint;
var
  RealPoint: TPoint2D;
begin
  RealPoint := Points[Index];
  result.X := RealPoint.X;
  result.Y := RealPoint.Y;
  result.Position := epMiddle;
end;

function TScreenObject.GetElevSubscription: TObserver;
begin
  if FElevSubscription = nil then
  begin
    CreateElevationSubscription;
  end;
  result := FElevSubscription;
end;

procedure TScreenObject.SetEdgePoints(const Index: integer;
  const Value: TEdgePoint);
var
  RealPoint: TPoint2D;
begin
  RealPoint.X := Value.X;
  RealPoint.Y := Value.Y;
  Points[Index] := RealPoint;
end;

procedure TScreenObject.SetName(const Value: TComponentName);
begin
  inherited;
  InvalidateModel;
end;

procedure TScreenObject.ResetSubscriptions;
begin
  ResetBoundaryDataSetSubscriptions;
  ResetDataSetSubscriptions;

  ResetMixtureSubscriptions;
  ResetBoundaryMixtureSubscriptions;
end;

procedure TScreenObject.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if not Value then
    begin
      Selected := False;
    end;
    RefreshGui(self);
  end;
end;

procedure TScreenObject.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

function TScreenObject.BoundaryDataSetCount: integer;
begin
  if FBoundaryDataSets = nil then
  begin
    result := 0;
  end
  else
  begin
    result := FBoundaryDataSets.Count;
  end;
end;

procedure TScreenObject.Assign3DElevationsFromSide(const Compiler: TRbwParser;
  const SparseArray: T3DSparseRealArray);
var
  TempMinX, TempMaxX: double;
  FirstRow, LastRow, FirstLayer, LastLayer: integer;
  LayerIndex, RowIndex: integer;
  CellLocation3D: T3DRealPoint;
  Expression: TExpression;
  SegmentIndex: integer;
  ASegment: TCellElementSegment;
  FirstColumn, LastColumn, ColIndex: integer;
  Grid: TCustomGrid;
  SectionIndex: Integer;
  TempImportedElevations : TValueArrayStorage;
  VariableList, DataSetList: TList;
  Temp: TValueArrayStorage;
begin
  VariableList := TList.Create;
  DataSetList := TList.Create;
  TempImportedElevations := CurrentValues;
  try
    Grid := (FModel as TPhastModel).Grid;
    Expression := Compiler.CurrentExpression;
    Temp := FCurrentValues;
    try
      InitializeUsedDataSets(FModel as TPhastModel, Compiler, Expression,
        VariableList, DataSetList);
    finally
      FCurrentValues := Temp;
    end;
    if (SetValuesOfEnclosedCells or SetValuesOfIntersectedCells)
      and Closed then
    begin

      // Get the coordinates of the points after rotating them to the grid
      // coordinate system.
      // Even though the last point and first point are identical,
      // the last point still has to be transformed for use with
      // the intersected points.

      TempMinX := MinY;
      TempMaxX := MaxY;

      GetRows(Grid, TempMinX, TempMaxX, FirstRow, LastRow);
      if FirstRow > Grid.RowCount then
      begin
        FirstRow := Grid.RowCount;
      end;
      if LastRow > Grid.RowCount then
      begin
        LastRow := Grid.RowCount;
      end;

      FirstLayer := -1;
      LastLayer := -1;
      case EvaluatedAt of
        eaBlocks:
          begin
            FirstLayer := 0;
            LastLayer := Grid.LayerCount -1;
          end;
        eaNodes:
          begin
            FirstLayer := 0;
            LastLayer := Grid.LayerCount;
          end;
        else Assert(False);
      end;
      FirstColumn := -1;
      LastColumn := -1;
      case EvaluatedAt of
        eaBlocks:
          begin
            FirstColumn := 0;
            LastColumn := Grid.ColumnCount -1;
          end;
        eaNodes:
          begin
            FirstColumn := 0;
            LastColumn := Grid.ColumnCount;
          end;
        else Assert(False);
      end;

      Assert((FirstRow >= 0) and (LastRow >= 0) and (FirstLayer >= 0)
        and (LastLayer >= 0));
      // Find the cells inside the screen object and color them.
      for ColIndex := FirstColumn to LastColumn do
      begin
        for RowIndex := FirstRow to LastRow do
        begin
          for LayerIndex := FirstLayer to LastLayer do
          begin
            case EvaluatedAt of
              eaBlocks:
                begin
                  CellLocation3D := Grid.ThreeDElementCenter(
                    ColIndex, RowIndex, LayerIndex);
                end;
              eaNodes:
                begin
                  CellLocation3D := Grid.ThreeDElementCorner(
                    ColIndex, RowIndex, LayerIndex);
                end;
            else
              Assert(False);
            end;

            if IsPointInside(CellLocation3D.Z, CellLocation3D.Y,
              SectionIndex) then
            begin
              Temp := FCurrentValues;
              try
                UpdateUsedVariables(VariableList, DataSetList,
                  LayerIndex, RowIndex, ColIndex);
              finally
                FCurrentValues := Temp;
              end;
              UpdateCurrentScreenObject(self);
              UpdateGlobalLocations(ColIndex, RowIndex, LayerIndex, EvaluatedAt);
              UpdateCurrentSection(SectionIndex);
              FCurrentValues := TempImportedElevations;
              Expression.Evaluate;
        
              SparseArray.Items[LayerIndex, RowIndex, ColIndex] :=
                Expression.DoubleResult;
            end;
          end;
        end
      end;
    end;
    if SetValuesOfIntersectedCells then
    begin
      if not Segments.UpToDate then
      begin
        UpdateSideSegments(Grid, EvaluatedAt);
      end;
      // set values here
      for SegmentIndex := 0 to Segments.Count - 1 do
      begin
        ASegment := Segments[SegmentIndex];
        Temp := FCurrentValues;
        try
          UpdateUsedVariables(VariableList, DataSetList,
            ASegment.Layer, ASegment.Row, ASegment.Col);
        finally
          FCurrentValues := Temp;
        end;
        UpdateCurrentScreenObject(self);
        UpdateCurrentSegment(ASegment);
        UpdateGlobalLocations(ASegment.Col, ASegment.Row, ASegment.Layer,
          EvaluatedAt);
        UpdateCurrentSection(ASegment.SectionIndex);
        FCurrentValues := TempImportedElevations;
        Expression.Evaluate;

        SparseArray.Items[ASegment.Layer, ASegment.Row, ASegment.Col] :=
          Expression.DoubleResult;
      end;
    end;
  finally
    FCurrentValues := TempImportedElevations;
    VariableList.Free;
    DataSetList.Free;
  end;
end;

procedure TScreenObject.Assign3DElevations(Formula: string;
  const SparseArray: T3DSparseRealArray);
var
  Orientation: TDataSetOrientation;
  Compiler: TRbwParser;
begin
  Orientation := dsoTop;
  case ViewDirection of
    vdTop: Orientation := dsoTop;
    vdFront: Orientation := dsoFront;
    vdSide: Orientation := dsoSide;
  else
    Assert(False);
  end;
  Compiler := GetCompiler(Orientation);

  Compiler.Compile(Formula);

  case ViewDirection of
    vdTop: Assign3DElevationsFromTop(Compiler, SparseArray);
    vdFront: Assign3DElevationsFromFront(Compiler, SparseArray);
    vdSide: Assign3DElevationsFromSide(Compiler, SparseArray);
  else
    Assert(False);
  end;
end;

procedure TScreenObject.AssignHigher3DElevations;
var
  Formula: string;
  TempValues: TValueArrayStorage;
begin
  Assert(ElevationCount <> ecZero);
  if FHigher3DElevations = nil then
  begin
    FHigher3DElevations := T3DSparseRealArray.Create(SPASmall);
  end;

  FHigher3DElevations.Clear;
  TempValues := FCurrentValues;
  try
    case ElevationCount of
      ecOne:
        begin
          Formula := ElevationFormula;
          FCurrentValues := ImportedSectionElevations;
        end;
      ecTwo:
        begin
          Formula := HigherElevationFormula;
          FCurrentValues := ImportedHigherSectionElevations;
        end
    else
      Assert(False);
    end;
    try
      Assign3DElevations(Formula, FHigher3DElevations);
    except on E: ERbwParserError do
      begin
        case ElevationCount of
          ecOne:
            begin
              ResetElevationFormula(nil, E.Message);
              Formula := ElevationFormula;
            end;
          ecTwo:
            begin
              ResetTopElevationFormula(nil, E.Message);
              Formula := HigherElevationFormula;
            end;
        else
          Assert(False);
        end;
        Assign3DElevations(Formula, FHigher3DElevations);
      end;
    end;
  finally
    FCurrentValues := TempValues;
  end;
  FHigher3DElevationsNeedsUpdating := False;
end;

procedure TScreenObject.AssignLower3DElevations;
var
  Formula: string;
  TempValues: TValueArrayStorage;
begin
  Assert(ElevationCount = ecTwo);
  if FLower3DElevations = nil then
  begin
    FLower3DElevations := T3DSparseRealArray.Create(SPASmall);
  end;
  FLower3DElevations.Clear;
  Formula := LowerElevationFormula;
  TempValues := FCurrentValues;
  try
    try
      FCurrentValues := ImportedLowerSectionElevations;
      Assign3DElevations(Formula, FLower3DElevations);
    except on E: ERbwParserError do
      begin
        ResetBottomElevationFormula(nil, E.Message);
        Formula := LowerElevationFormula;
        Assign3DElevations(Formula, FLower3DElevations);
      end;
    end;
  finally
    FCurrentValues := TempValues;
  end;
  FLower3DElevationsNeedsUpdating := False;
end;

function TScreenObject.GetHigher3DElevations: T3DSparseRealArray;
begin
  UpdateHigher3DElevations;
  result := FHigher3DElevations;
end;

function TScreenObject.GetHigherElevationFormula: string;
begin
  if ElevationCount = ecTwo then
  begin
    if FHigherElevationFormulaObject = nil then
    begin
      CreateHigherElevationFormulaObject;
    end;
    result := FHigherElevationFormulaObject.Formula
  end
  else
  begin
    result := '0.'
  end;
end;

function TScreenObject.GetLower3DElevations: T3DSparseRealArray;
begin
  UpdateLower3DElevations;
  result := FLower3DElevations;
end;

function TScreenObject.GetLowerElevationFormula: string;
begin
  if ElevationCount = ecTwo then
  begin
    if FLowerElevationFormulaObject = nil then
    begin
      CreateLowerElevationFormulaObject
    end;
    result := FLowerElevationFormulaObject.Formula
  end
  else
  begin
    result := '0.'
  end;
end;

procedure TScreenObject.SubPolygonXLimits(Subject: TObject;
  out LowerBoundary, UpperBoundary: double);
var
  SubPolygon: TSubPolygon;
begin
  SubPolygon := TSubPolygon(Subject);
  LowerBoundary := SubPolygon.FMinX;
  UpperBoundary := SubPolygon.FMaxX;
end;

procedure TScreenObject.SubPolygonYLimits(Subject: TObject;
  out LowerBoundary, UpperBoundary: double);
var
  SubPolygon: TSubPolygon;
begin
  SubPolygon := TSubPolygon(Subject);
  LowerBoundary := SubPolygon.FMinY;
  UpperBoundary := SubPolygon.FMaxY;
end;

function TScreenObject.IsPointInside(const X, Y: real;
  out SectionIndex: integer): boolean;
var
  Index: Integer;
  SubPolygon: TSubPolygon;
  ExistingResult: boolean;
  IntDefArray: TIntDefArray;
  List: TList;
  PointArray: TOneDRealArray;
begin
  result := false;
  ExistingResult := False;
  if Deleted then
  begin
    Exit;
  end;
  if not Closed then
  begin
    // If the screen object is not closed, nothing can be inside it.
    Exit;
  end;

  // Make a quick check to make sure it might possibly be inside
  // the screen object.
  if not InsideBox(X, Y) then
  begin
    Exit;
  end;

  if SubPolygonCount = 0 then
  begin
    CreateSubPolygons;
  end;
  Assert(SubPolygonCount = SectionCount);

  SectionIndex := -1;
  if SectionCount > 25 then
  begin
    if FIntervalTree = nil then
    begin
      SetLength(IntDefArray, 2);
      IntDefArray[0].LowerBoundary := MinX;
      IntDefArray[0].UpperBoundary := MaxX;
      IntDefArray[0].OnFindObjectInterval := SubPolygonXLimits;
      IntDefArray[1].LowerBoundary := MinY;
      IntDefArray[1].UpperBoundary := MaxY;
      IntDefArray[1].OnFindObjectInterval := SubPolygonYLimits;
      FIntervalTree := TRbwIntervalTree.Create(IntDefArray);
      for Index := 0 to SubPolygonCount - 1 do
      begin
        if SectionClosed[Index] then
        begin
          FIntervalTree.Add(SubPolygons[Index]);
        end;
      end;
    end;
    List := TList.Create;
    try
      SetLength(PointArray, 2);
      PointArray[0] := X;
      PointArray[1] := Y;
      FIntervalTree.FindContainingObjects(PointArray, List);
      for Index := 0 to List.Count - 1 do
      begin
        SubPolygon := List[Index];
        Assert( SectionClosed[SubPolygon.SectionIndex]);
        EvaluateSubPolygon(SubPolygon, X, Y, result);
        if result and (ExistingResult <> result) then
        begin
          SectionIndex := SubPolygon.SectionIndex;
        end;
        ExistingResult := result;
      end;
    finally
      List.Free;
    end;
  end
  else
  begin
    for Index := 0 to SubPolygonCount - 1 do
    begin
      if SectionClosed[Index] then
      begin
        SubPolygon := SubPolygons[Index];
        EvaluateSubPolygon(SubPolygon, X, Y, result);
        if result and (ExistingResult <> result) then
        begin
          SectionIndex := Index;
        end;
        ExistingResult := result;
      end;
    end;
  end;
end;

procedure TScreenObject.CreateSubPolygons;
var
  Index: Integer;  
  SubPolygon : TSubPolygon;
begin
  ClearSubPolygons;
  if SectionCount > 0 then
  begin
    if FSubPolygons = nil then
    begin
      FSubPolygons:= TObjectList.Create;
    end;
  end;
  for Index := 0 to SectionCount - 1 do
  begin
    SubPolygon := TSubPolygon.Create(FPoints, SectionLength[Index],
      SectionStart[Index], Index);
    FSubPolygons.Add(SubPolygon);
  end;
end;

procedure TScreenObject.ClearSubPolygons;
begin
  FreeAndNil(FSubPolygons);
  FreeAndNil(FIntervalTree);
end;

function TScreenObject.GetSelectLines: TLines;
var
  MaxP, MinP: TPoint;
  Index: Integer;
  SelectLine: TLine;
  MaxX: double;
  MaxY: double;
  MinX: double;
  MinY: double;
  APoint: TPoint2D;
  Start: integer;
  PointIndex: Integer;
begin
  if FNeedToUpdateLine then
  begin
    FreeAndNil(FSelectLines);
    FSelectLines := TLines.Create;

    for Index := 0 to SectionCount - 1 do
    begin
      SelectLine := TLine.Create(0);
      FSelectLines.Add(SelectLine);
      SelectLine.AssignPoints(CanvasCoordinates);
      Start := SectionStart[Index];
      SelectLine.Start := Start;
      SelectLine.Count := SectionLength[Index];
      APoint := Points[Start];
      MaxX := APoint.X;
      MaxY := APoint.Y;
      MinX := MaxX;
      MinY := MaxY;
      for PointIndex := 1 to SectionLength[Index] - 1 do
      begin
        APoint := Points[Start+PointIndex];
        if MaxX < APoint.X then
        begin
          MaxX := APoint.X;
        end;
        if MinX > APoint.X then
        begin
          MinX := APoint.X;
        end;
        if MaxY < APoint.Y then
        begin
          MaxY := APoint.Y;
        end;
        if MinY > APoint.Y then
        begin
          MinY := APoint.Y;
        end;
      end;
      with ZoomBox(ViewDirection) do
      begin
        MaxP.X := XCoord(MaxX);
        MaxP.Y := YCoord(MaxY);
        MinP.X := XCoord(MinX);
        MinP.Y := YCoord(MinY);
      end;
      SelectLine.SetBox(MaxP, MinP);
    end;

    FNeedToUpdateLine := False;
  end;
  result := FSelectLines;
end;

function TScreenObject.GetSpecifiedHeadBoundary: TSpecifiedHeadBoundary;
begin
  CreatePhastSpecifiedHeadBoundary;
  result := FSpecifiedHeadBoundary;
end;

function TScreenObject.GetSpecifiedSolutionBoundary: TSpecifiedSolutionBoundary;
begin
  CreatePhastSpecifiedSolutionBoundary;
  result := FSpecifiedSolutionBoundary;
end;

function TScreenObject.Select(const XScreenCoordinate,
  YScreenCoordinate: integer): boolean;
var
  Index: integer;
  RealX, RealY: real;
  SectionIndex: Integer;
begin
  result := False;
  if Deleted or not Visible then
  begin
    Exit;
  end;

  for Index := 0 to Count - 1 do
  begin
    if (Abs(XScreenCoordinate - CanvasCoordinates[Index].X) <= SelectEpsilon)
      and (Abs(YScreenCoordinate - CanvasCoordinates[Index].Y) <= SelectEpsilon)
      then
    begin
      result := True;
      Exit;
    end;
  end;

  if Closed then
  begin
    with ZoomBox(ViewDirection) do
    begin
      RealX := X(XScreenCoordinate);
      RealY := Y(YScreenCoordinate);
    end;

    if IsPointInside(RealX, RealY, SectionIndex) then
    begin
      result := True;
      Exit;
    end;
  end;

  if SelectEdge(XScreenCoordinate, YScreenCoordinate) >= 0 then
    result := True;
end;

procedure TScreenObject.Set_SetValuesOfEnclosedCells(const Value: boolean);
begin
  if FSetValuesOfEnclosedCells <> Value then
  begin
    FSetValuesOfEnclosedCells := Value;
    InvalidateModel;
    Invalidate;
  end;
end;

procedure TScreenObject.Set_SetValuesOfIntersectedCells(const Value:
  boolean);
begin
  if FSetValuesOfIntersectedCells <> Value then
  begin
    FSetValuesOfIntersectedCells := Value;
    InvalidateModel;
    Invalidate;
  end;
end;

function TScreenObject.SingleCellLocation: TCellLocation;
var
  PhastModel: TPhastModel;
  Grid: TModflowGrid;
  CellList: TCellAssignmentList;
  Cell: TCellAssignment;
  LocalLayer: integer;
begin
  result.Layer := 0;
  result.Row := 1;
  result.Column := 1;
  if (Count <> 1) or (ElevationCount <> ecOne) or Deleted then
  begin
    Exit;
  end;
  PhastModel := Model as TPhastModel;
  Grid := PhastModel.ModflowGrid;
  CellList := TCellAssignmentList.Create;
  try
    GetCellsToAssign(Grid, '', nil, nil, CellList, alAll);
    if CellList.Count = 1 then
    begin
      Cell := CellList[0];
      LocalLayer := PhastModel.LayerStructure.
        DataSetLayerToModflowLayer(Cell.Layer);
      result.Layer := LocalLayer;
      result.Row := Cell.Row+1;
      result.Column := Cell.Column+1;
    end;
  finally
    CellList.Free;
  end;
end;

procedure TScreenObject.SetColorLine(const Value: boolean);
begin
  if FColorLine <> Value then
  begin
    FColorLine := Value;
    InvalidateModel;
  end;
end;

procedure TScreenObject.SetFillScreenObject(const Value: boolean);
begin
  if FFillScreenObject <> Value then
  begin
    FFillScreenObject := Value;
    InvalidateModel;
  end;
end;

function TScreenObject.GetLineColor: TColor;
begin
  Result := FLineColor;
end;

procedure TScreenObject.SetLineColor(const Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    InvalidateModel;
  end;
end;

procedure TScreenObject.RotatePoints(const Grid: TCustomGrid;
  out RotatedPoints: TEdgePointArray;
  out TempMinX, TempMinY, TempMaxX, TempMaxY: double);
var
  Index: integer;
  APoint: TPoint2D;
  EdgePoint: TEdgePoint;
begin
  SetLength(RotatedPoints, Count);
  TempMinX := 0;
  TempMinY := 0;
  TempMaxX := 0;
  TempMaxY := 0;
  for Index := 0 to Count - 1 do
  begin
    APoint := Grid.
      RotateFromRealWorldCoordinatesToGridCoordinates(Points[Index]);
    EdgePoint.X := APoint.X;
    EdgePoint.Y := APoint.Y;
    RotatedPoints[Index] := EdgePoint;

    if Index = 0 then
    begin
      TempMinX := APoint.X;
      TempMaxX := TempMinX;
      TempMinY := APoint.Y;
      TempMaxY := TempMinY;
    end
    else
    begin
      if APoint.X > TempMaxX then
      begin
        TempMaxX := APoint.X
      end
      else if APoint.X < TempMinX then
      begin
        TempMinX := APoint.X;
      end;

      if APoint.Y > TempMaxY then
      begin
        TempMaxY := APoint.Y
      end
      else if APoint.Y < TempMinY then
      begin
        TempMinY := APoint.Y;
      end;
    end;
  end;
end;

function TScreenObject.GetLayer(const Grid: TCustomGrid; const Z: real):
  integer;
begin
  Assert(EvaluatedAt = eaBlocks);
  result := (Grid as TPhastGrid).NearestLayerPosition(Z);

  if result < 0 then
  begin
    result := 0;
  end;
  if (result >= Grid.LayerCount) then
  begin
    result := Grid.LayerCount - 1
  end;
  if (result > 0) and
    ((Grid as TPhastGrid).LayerElevation[result] > Z) then
  begin
    Dec(result);
  end;
end;

procedure TScreenObject.GetLayers(const Grid: TCustomGrid;
  TempMinZ, TempMaxZ: real; out FirstLayer, LastLayer: integer);
var
  TempI: integer;
  Temp: real;
begin
  if (TempMinZ > TempMaxZ) then
  begin
    Temp := TempMaxZ;
    TempMaxZ := TempMinZ;
    TempMinZ := Temp;
  end;

  case EvaluatedAt of
    eaBlocks:
      begin
        FirstLayer := GetLayer(Grid, TempMinZ);
        LastLayer := GetLayer(Grid, TempMaxZ);
      end;
    eaNodes:
      begin
        FirstLayer := GetAPhastLayer(Grid, TempMinZ);
        LastLayer := GetAPhastLayer(Grid, TempMaxZ);
      end;
  else
    Assert(False);
  end;

  if (LastLayer < FirstLayer) then
  begin
    TempI := LastLayer;
    LastLayer := FirstLayer;
    FirstLayer := TempI;
  end;
end;

function TScreenObject.GetLeakyBoundary: TLeakyBoundary;
begin
  CreatePhastLeakyBoundary;
  result := FLeakyBoundary
end;

function TScreenObject.GetColumn(const Grid: TCustomGrid; const X: real):
  integer;
begin
  Assert(EvaluatedAt = eaBlocks);
  result := Grid.NearestColumnPosition(X);

  if result < 0 then
  begin
    result := 0;
  end;
  if (result >= Grid.ColumnCount) then
  begin
    result := Grid.ColumnCount - 1;
  end;
  if (result > 0) and
    (Grid.ColumnPosition[result] > X) then
  begin
    Dec(result);
  end;
end;

procedure TScreenObject.GetColumns(const Grid: TCustomGrid;
  TempMinX, TempMaxX: real; out FirstCol, LastCol: integer);
var
  TempI: integer;
  Temp: real;
begin
  if (TempMinX > TempMaxX) then
  begin
    Temp := TempMaxX;
    TempMaxX := TempMinX;
    TempMinX := Temp;
  end;

  case EvaluatedAt of
    eaBlocks:
      begin
        FirstCol := GetColumn(Grid, TempMinX);
        LastCol := GetColumn(Grid, TempMaxX);
      end;
    eaNodes:
      begin
        FirstCol := GetACol(Grid, TempMinX);
        LastCol := GetACol(Grid, TempMaxX);
      end;
  else
    Assert(False);
  end;

  if (LastCol < FirstCol) then
  begin
    TempI := LastCol;
    LastCol := FirstCol;
    FirstCol := TempI;
  end;
end;

function TScreenObject.GetCompiler(const Orientation: TDataSetOrientation;
  const EvaluatedAt: TEvaluatedAt): TRbwParser;
begin
  result := (FModel as TPhastModel).GetCompiler(Orientation, EvaluatedAt);
end;

function TScreenObject.GetRow(const Grid: TCustomGrid; const Y: real): integer;
begin
  Assert(EvaluatedAt = eaBlocks);
  result := Grid.NearestRowPosition(Y);

  if result < 0 then
  begin
    result := 0;
  end;
  if (result >= Grid.RowCount) then
  begin
    result := Grid.RowCount - 1
  end;
  case Grid.RowDirection of
    rdSouthToNorth:
      begin
        if (result > 0) and
          (Grid.RowPosition[result] > Y) then
        begin
          Dec(result);
        end;
      end;
    rdNorthToSouth:
      begin
        if (result > 0) and
          (Grid.RowPosition[result] < Y) then
        begin
          Dec(result);
        end;
      end;
      else Assert(False);
  end;
end;

procedure TScreenObject.GetRows(const Grid: TCustomGrid;
  TempMinY, TempMaxY: real; out FirstRow, LastRow: integer);
var
  TempI: integer;
  Temp: real;
begin
  if (TempMinY > TempMaxY) then
  begin
    Temp := TempMaxY;
    TempMaxY := TempMinY;
    TempMinY := Temp;
  end;

  case EvaluatedAt of
    eaBlocks:
      begin
        FirstRow := GetRow(Grid, TempMinY);
        LastRow := GetRow(Grid, TempMaxY);
      end;
    eaNodes:
      begin
        FirstRow := GetARow(Grid, TempMinY);
        LastRow := GetARow(Grid, TempMaxY);
      end;
  else
    Assert(False);
  end;

  if (LastRow < FirstRow) then
  begin
    TempI := LastRow;
    LastRow := FirstRow;
    FirstRow := TempI;
  end;
end;

procedure TScreenObject.UpdateTopSegments(const Grid: TCustomGrid;
  const EvaluatedAt: TEvaluatedAt; const PointsRotated: boolean;
  var RotatedPoints: TEdgePointArray);
var
  GridMaxX, GridMinX, GridMaxY, GridMinY, Temp: double;
  Index: integer;
  APoint, PreviousPoint, Point1, Point2, NewPoint: TEdgePoint;
  ColIndex, RowIndex: integer;
  FirstElevationIndex: integer;
  LastElevationIndex: integer;
  ElevationIndex: integer;
  ASegment: TCellElementSegment;
  FirstRow, LastRow, FirstCol, LastCol: integer;
  TempPoints1, TempPoints2: TEdgePointArray;
  Position: TEdgePosition;
  ACell: T2DTopCell;
  PointIndex: integer;
  Col1, Col2, Row1, Row2: integer;
  RealPoint: TPoint2D;
  LayerLimit: integer;
  TempMinX, TempMinY, TempMaxX, TempMaxY: double;
  IncreasingX, IncreasingY: boolean;
  SectionIndex: integer;
  EndSection: boolean;
  Expression1: TExpression;
  Expression2: TExpression;
  TempFormula: string;
  Compiler: TRbwParser;
  Elevation1: double;
  Elevation2: double;
  LocalModel: TPhastModel;
  BlockTop: double;
  BlockBottom: double;
  VariableList1, DataSetList1: TList;
  VariableList2, DataSetList2: TList;
  Procedure AddSomeSegments;
  begin
    BlockBottom := 0;
    BlockTop := 0;
    if ElevationCount in [ecOne, ecTwo] then
    begin
      case LocalModel.ModelSelection of
        msPhast:
          begin
            case EvaluatedAt of
              eaBlocks:
                begin
                  BlockBottom := LocalModel.
                    PhastGrid.LayerElevation[ElevationIndex];
                  BlockTop := LocalModel.
                    PhastGrid.LayerElevation[ElevationIndex+1];
                end;
              eaNodes:
                begin
                  if ElevationIndex > 0 then
                  begin
                    BlockBottom := LocalModel.
                      PhastGrid.LayerCenter(ElevationIndex-1);
                  end
                  else
                  begin
                    BlockBottom := LocalModel.
                      PhastGrid.LayerElevation[ElevationIndex];
                  end;
                  if ElevationIndex < LocalModel.
                      PhastGrid.LayerCount then
                  begin
                    BlockTop := LocalModel.
                      PhastGrid.LayerCenter(ElevationIndex);
                  end
                  else
                  begin
                    BlockTop := LocalModel.
                      PhastGrid.LayerElevation[ElevationIndex];
                  end;
                end;
              else Assert(False);
            end;
          end;
        msModflow:
          begin
            Assert(EvaluatedAt = eaBlocks);
            BlockTop := LocalModel.ModflowGrid.
              LayerElevations[ASegment.Col, ASegment.Row, ElevationIndex];
            BlockBottom := LocalModel.ModflowGrid.
              LayerElevations[ASegment.Col, ASegment.Row, ElevationIndex+1];
          end;
        else Assert(False);
      end;
    end;

    if ElevationCount in [ecOne, ecTwo] then
    begin
      UpdateUsedVariables(VariableList1, DataSetList1,
        ElevationIndex, ASegment.Row, ASegment.Col);
      UpdateCurrentScreenObject(self);
      UpdateGlobalLocations(ASegment.Col, ASegment.Row, ElevationIndex,
        EvaluatedAt);
      UpdateCurrentSegment(ASegment);
      UpdateCurrentSection(ASegment.SectionIndex);
      if ElevationCount = ecOne then
      begin
        FCurrentValues := ImportedSectionElevations;
      end
      else
      begin
        FCurrentValues := ImportedHigherSectionElevations;
      end;
      Expression1.Evaluate;
      Elevation1 := Expression1.DoubleResult;
    end;

    if ElevationCount = ecTwo then
    begin
      UpdateUsedVariables(VariableList2, DataSetList2,
        ElevationIndex, ASegment.Row, ASegment.Col);

      UpdateCurrentScreenObject(self);
      UpdateGlobalLocations(ASegment.Col, ASegment.Row, ElevationIndex,
        EvaluatedAt);
      UpdateCurrentSegment(ASegment);
      UpdateCurrentSection(ASegment.SectionIndex);
      FCurrentValues := ImportedLowerSectionElevations;
      Expression2.Evaluate;
      Elevation2 := Expression2.DoubleResult;
    end;

    case ElevationCount of
      ecZero:
        begin
            FSegments.Add(ASegment);
        end;
      ecOne:
        begin
          if (BlockTop >= Elevation1) and (BlockBottom <= Elevation1) then
          begin
            FSegments.Add(ASegment);
          end
          else
          begin
            ASegment.Free;
          end;
        end;
      ecTwo:
        begin
          if ((BlockTop > Elevation2)
            or ((BlockTop = Elevation2) and (ASegment.Layer = 0)))
            and (BlockBottom < Elevation1) then
          begin
            FSegments.Add(ASegment);
          end
          else
          begin
            ASegment.Free;
          end;
        end;
      else Assert(false);
    end;
  end;
begin
  VariableList1 := TList.Create;
  DataSetList1 := TList.Create;
  VariableList2 := TList.Create;
  DataSetList2 := TList.Create;
  try
    LocalModel:= Model as TPhastModel;
    Expression1 := nil;
    Expression2 := nil;
    case ElevationCount of
      ecZero:
        begin
        end;
      ecOne:
        begin
          TempFormula := ElevationFormula;
          Compiler := GetCompiler(dso3D);
          Compiler.Compile(TempFormula);
          Expression1 := Compiler.CurrentExpression;
          InitializeUsedDataSets(LocalModel, Compiler, Expression1,
            VariableList1, DataSetList1);
        end;
      ecTwo:
        begin
          TempFormula := HigherElevationFormula;
          Compiler := GetCompiler(dso3D);
          Compiler.Compile(TempFormula);
          Expression1 := Compiler.CurrentExpression;
          InitializeUsedDataSets(LocalModel, Compiler, Expression1,
            VariableList1, DataSetList1);

          TempFormula := LowerElevationFormula;
          Compiler := GetCompiler(dso3D);
          Compiler.Compile(TempFormula);
          Expression2 := Compiler.CurrentExpression;
          InitializeUsedDataSets(LocalModel, Compiler, Expression2,
            VariableList2, DataSetList2);
        end;
      else Assert(False);
    end;
    // get rid of the existing TCellElementSegments.
    FSegments.Clear;

    // set the maximum layer to be tested.
    LayerLimit := -1;
    case EvaluatedAt of
      eaBlocks:
        begin
          LayerLimit := Grid.LayerCount - 1;
        end;
      eaNodes:
        begin
          LayerLimit := Grid.LayerCount;
        end;
    else
      Assert(False);
    end;

    if not PointsRotated then
    begin
      // Get the coordinates of the points after rotating them to the grid
      // coordinate system.
      RotatePoints(Grid, RotatedPoints, TempMinX, TempMinY, TempMaxX,
        TempMaxY);
    end;

    // Get the box outlining the grid.
    if Grid.ColumnCount >= 0 then
    begin
      GridMaxX := Grid.ColumnPosition[Grid.ColumnCount];
      GridMinX := Grid.ColumnPosition[0];
    end
    else
    begin
      GridMaxX := 0;
      GridMinX := 0;
    end;
    if GridMinX > GridMaxX then
    begin
      Temp := GridMinX;
      GridMinX := GridMaxX;
      GridMaxX := Temp;
    end;

    if Grid.RowCount >= 0 then
    begin
      GridMaxY := Grid.RowPosition[Grid.RowCount];
      GridMinY := Grid.RowPosition[0];
    end
    else
    begin
      GridMaxY := 0;
      GridMinY := 0;
    end;
    if GridMinY > GridMaxY then
    begin
      Temp := GridMinY;
      GridMinY := GridMaxY;
      GridMaxY := Temp;
    end;

    SectionIndex := 0;
    // loop over vertices in the TScreenObject
    for Index := 0 to Count - 1 do
    begin
      Assert(SectionIndex < SectionCount);
      if (Index > SectionStart[SectionIndex]) then
      begin
        PreviousPoint := APoint;
      end;
      APoint := RotatedPoints[Index];
      if (SectionLength[SectionIndex] = 1) then
      begin
        Inc(SectionIndex);
        // store the value of the cell at the first vertex.
        if (APoint.X >= GridMinX) and (APoint.X <= GridMaxX)
          and (APoint.Y >= GridMinY) and (APoint.Y <= GridMaxY) then
        begin
          ColIndex := GetACol(Grid, APoint.X);
          if (EvaluatedAt = eaBlocks) and (ColIndex = Grid.ColumnCount) then
          begin
            Dec(ColIndex);
          end;
          RowIndex := GetARow(Grid, APoint.Y);
          if (EvaluatedAt = eaBlocks) and (RowIndex = Grid.RowCount) then
          begin
            Dec(RowIndex);
          end;

          FirstElevationIndex := 0;
          LastElevationIndex := LayerLimit;
          for ElevationIndex := FirstElevationIndex to LastElevationIndex do
          begin
            ASegment := TCellElementSegment.Create;
            ASegment.X1 := APoint.X;
            ASegment.X2 := APoint.X;
            ASegment.Y1 := APoint.Y;
            ASegment.Y2 := APoint.Y;
            ASegment.Col := ColIndex;
            ASegment.Row := RowIndex;
            ASegment.Layer := ElevationIndex;
            ASegment.VertexIndex := Index;
            ASegment.StartPosition := epFirst;
            ASegment.EndPosition := epLast;
            ASegment.SectionIndex := SectionIndex -1;

            AddSomeSegments;
          end;
        end;
      end
      else
      begin
        EndSection := False;
        if (Index = SectionStart[SectionIndex]) then
        begin
          Continue;
        end;
        if (Index = SectionEnd[SectionIndex]) then
        begin
          Inc(SectionIndex);
          EndSection := True;
        end;
        // get a line segment and store the values of cells along it.
        FirstCol := GetACol(Grid, PreviousPoint.X);
        LastCol := GetACol(Grid, APoint.X);

        FirstRow := GetARow(Grid, PreviousPoint.Y);
        LastRow := GetARow(Grid, APoint.Y);

        PointIndex := 0;
        // get points on line segment that intersect the column lines.
        SetLength(TempPoints1, Abs(LastCol - FirstCol) + Abs(LastRow -
          FirstRow) + 4);
        PreviousPoint.Position := epFirst;
        TempPoints1[PointIndex] := PreviousPoint;
        Inc(PointIndex);
        if (PreviousPoint.X <> APoint.X) then
        begin
          IncreasingX := APoint.X > PreviousPoint.X;
          if LastCol >= FirstCol then
          begin
            Col1 := FirstCol;
            Col2 := LastCol;
          end
          else
          begin
            Col1 := LastCol;
            Col2 := FirstCol;
          end;
          if (Col1 >= 0) and (Col2 >= 0) then
          begin
            for ColIndex := Col1 to Col2 do
            begin
              if ((ColIndex = 0) or (ColIndex = Grid.ColumnCount+1)) then
              begin
                if (ColIndex = 0) then
                begin
                  if IncreasingX then
                  begin
                    Position := epFirst;
                  end
                  else
                  begin
                    Position := epLast;
                  end;
                end
                else
                begin
                  if IncreasingX then
                  begin
                    Position := epLast;
                  end
                  else
                  begin
                    Position := epFirst;
                  end;
                end;
              end
              else
              begin
                Position := epMiddle;
              end;

              AddPointFromColumn(ColIndex, Grid,
                PreviousPoint, APoint, TempPoints1, PointIndex, Position);
            end;
          end;
        end
        else
        begin
          Col1 := FirstCol;
          Col2 := LastCol;
        end;

        // get points on line segment that intersect the row lines.
        if (PreviousPoint.Y <> APoint.Y) then
        begin
          IncreasingY := APoint.Y > PreviousPoint.Y;
          if LastRow >= FirstRow then
          begin
            Row1 := FirstRow;
            Row2 := LastRow;
          end
          else
          begin
            Row1 := LastRow;
            Row2 := FirstRow;
          end;
          if (Row1 >= 0) and (Row2 >= 0) then
          begin
            for RowIndex := Row1 to Row2 do
            begin
              if ((RowIndex = 0) or (RowIndex = Grid.RowCount+1)) then
              begin
                if RowIndex = 0 then
                begin
                  if IncreasingY then
                  begin
                    Position := epFirst;
                  end
                  else
                  begin
                    Position := epLast;
                  end;
                end
                else
                begin
                  if IncreasingY then
                  begin
                    Position := epLast;
                  end
                  else
                  begin
                    Position := epFirst;
                  end;
                end;
              end
              else
              begin
                Position := epMiddle;
              end;

              AddPointFromRow(RowIndex, Grid, PreviousPoint,
                APoint, TempPoints1, PointIndex, Position);
            end;
          end;
        end
        else
        begin
          Row1 := FirstRow;
          Row2 := LastRow;
        end;
        APoint.Position := epLast;
        TempPoints1[PointIndex] := APoint;
        Inc(PointIndex);

        // Sort the points.
        SortPoints(TempPoints1, TempPoints2, APoint, PreviousPoint,
          PointIndex);

        // Get cells.
        case EvaluatedAt of
          eaBlocks:
            begin
              for PointIndex := 1 to Length(TempPoints2) - 1 do
              begin
                Point2 := TempPoints2[PointIndex];
                Point1 := TempPoints2[PointIndex - 1];
                NewPoint.X := (Point1.X + Point2.X) / 2;
                NewPoint.Y := (Point1.Y + Point2.Y) / 2;
                if (NewPoint.X >= GridMinX) and (NewPoint.X <= GridMaxX)
                  and (NewPoint.Y >= GridMinY) and (NewPoint.Y <= GridMaxY) then
                begin
                  RealPoint.X := NewPoint.X;
                  RealPoint.Y := NewPoint.Y;
                  ACell := Grid.TopContainingCell(RealPoint, EvaluatedAt,
                    False, Col1, Col2, Row1, Row2);

                  FirstElevationIndex := 0;
                  LastElevationIndex := LayerLimit;
                  for ElevationIndex := FirstElevationIndex to
                    LastElevationIndex do
                  begin
                    ASegment := TCellElementSegment.Create;
                    ASegment.X2 := Point2.X;
                    ASegment.X1 := Point1.X;
                    ASegment.Y2 := Point2.Y;
                    ASegment.Y1 := Point1.Y;
                    ASegment.Col := ACell.Col;
                    ASegment.Row := ACell.Row;
                    ASegment.Layer := ElevationIndex;
                    ASegment.VertexIndex := Index - 1;
                    ASegment.StartPosition := Point1.Position;
                    ASegment.EndPosition := Point2.Position;
                    if PointIndex = 1 then
                    begin
                      ASegment.StartPosition := epFirst;
                    end;
                    if PointIndex = Length(TempPoints2) - 1 then
                    begin
                      ASegment.StartPosition := epLast;
                    end;
                    if EndSection then
                    begin
                      ASegment.SectionIndex := SectionIndex-1;
                    end
                    else
                    begin
                      ASegment.SectionIndex := SectionIndex;
                    end;
                    AddSomeSegments;
                  end;
                end;
              end;
            end;
          eaNodes:
            begin
              for PointIndex := 1 to Length(TempPoints2) - 1 do
              begin
                Point2 := TempPoints2[PointIndex];
                Point1 := TempPoints2[PointIndex - 1];
                NewPoint.X := (Point1.X + Point2.X) / 2;
                NewPoint.Y := (Point1.Y + Point2.Y) / 2;

                if (NewPoint.X >= GridMinX) and (NewPoint.X <= GridMaxX)
                  and (NewPoint.Y >= GridMinY) and (NewPoint.Y <= GridMaxY) then
                begin
                  RealPoint.X := NewPoint.X;
                  RealPoint.Y := NewPoint.Y;
                  ACell := Grid.TopContainingCell(RealPoint, EvaluatedAt,
                    False, Col1, Col2, Row1, Row2);

                  FirstElevationIndex := 0;
                  LastElevationIndex := LayerLimit;
                  for ElevationIndex := FirstElevationIndex to
                    LastElevationIndex do
                  begin
                    ASegment := TCellElementSegment.Create;
                    ASegment.X2 := Point2.X;
                    ASegment.X1 := Point1.X;
                    ASegment.Y2 := Point2.Y;
                    ASegment.Y1 := Point1.Y;
                    ASegment.Col := ACell.Col;
                    ASegment.Row := ACell.Row;
                    ASegment.Layer := ElevationIndex;
                    ASegment.VertexIndex := Index - 1;
                    ASegment.StartPosition := Point1.Position;
                    ASegment.EndPosition := Point2.Position;
                    if PointIndex = 0 then
                    begin
                      ASegment.StartPosition := epFirst;
                    end;
                    if PointIndex = Length(TempPoints2) - 1 then
                    begin
                      ASegment.StartPosition := epLast;
                    end;
                    if EndSection then
                    begin
                      ASegment.SectionIndex := SectionIndex-1;
                    end
                    else
                    begin
                      ASegment.SectionIndex := SectionIndex;
                    end;
                    AddSomeSegments;
                  end;
                end;
              end;
            end;
        else
          Assert(False);
        end;
      end;
    end;
    FSegments.UpToDate := True;
  finally
    VariableList1.Free;
    DataSetList1.Free;
    VariableList2.Free;
    DataSetList2.Free;
  end;
end;

procedure TScreenObject.AssignValuesToTopPhastDataSet(const Grid: TCustomGrid;
  const DataSet: TDataArray; OtherData: TObject);
var
  DataSetFunction: string;
  Compiler: TRbwParser;
  Expression: TExpression;
  UsedVariables: TStringList;
begin
  Assert(DataSet <> nil);
  if not (DataSet.Orientation in [dsoTop, dso3D]) then
  begin
    RemoveDataSet(DataSet);
    Exit;
  end;
  if DataSetUsed(DataSet, OtherData) then
  begin
    UsedVariables := TStringList.Create;
    try
      InitializeExpression(Compiler, DataSetFunction, Expression, DataSet,
        OtherData);
      AssignTopDataSetValues(Grid, Expression, DataSetFunction, Compiler,
        UsedVariables, OtherData, DataSet);
    finally
      UsedVariables.Free;
    end;
  end;
end;

procedure TScreenObject.RemoveDataArraySubscription(Sender: TObject;
  const AName: string);
begin
  RemoveSubscriptionFromList(AName, Sender, FDataSetFormulas,
    FDataSetSubscriptions);
end;

procedure TScreenObject.RemoveBoundaryDataArraySubscription(Sender: TObject;
  const AName: string);
begin
  RemoveSubscriptionFromList(AName, Sender, FBoundaryDataSetFormulas,
    FBoundaryDataSetSubscriptions);
end;

function TScreenObject.RemoveDataSet(const DataSet: TDataArray): Integer;
begin
  if (Model <> nil) and
    ((csDestroying in Model.ComponentState)
    or (Model as TPhastModel).Clearing) then
  begin
    result := -1;
    Exit;
  end;
  result := IndexOfDataSet(DataSet);
  if result >= 0 then
  begin
    DeleteDataSet(result);
  end
  else
  begin
    result := IndexOfBoundaryDataSet(DataSet);
    if result >= 0 then
    begin
      DeleteBoundaryDataSet(result);
    end;
  end;
  if FElevSubscription <> nil then
  begin
    FElevSubscription.StopsTalkingTo(DataSet);
  end;
  if FTopElevSubscription <> nil then
  begin
    FTopElevSubscription.StopsTalkingTo(DataSet);
  end;
  if FBottomElevSubscription <> nil then
  begin
    FBottomElevSubscription.StopsTalkingTo(DataSet);
  end;
end;

function TScreenObject.GetCompiler(const Orientation: TDataSetOrientation):
  TRbwParser;
begin
  result := GetCompiler(Orientation, EvaluatedAt);
end;

procedure TScreenObject.UpdateVariables(const UsedVariables: TStringList;
  const DataSet: TDataArray; Layer, Row, Column: integer;
  const Compiler: TRbwParser);
var
  VarIndex: integer;
  VarName: string;
  VarPosition: integer;
  Variable: TCustomValue;
//  DataSetIndex: integer;
  AnotherDataSet: TDataArray;
begin
  UpdateGlobalLocations(Column, Row, Layer, DataSet.EvaluatedAt);

  for VarIndex := 0 to UsedVariables.Count - 1 do
  begin
    VarName := UsedVariables[VarIndex];
    VarPosition := Compiler.IndexOfVariable(VarName);
    Variable := Compiler.Variables[VarPosition];
    AnotherDataSet := (FModel as TPhastModel).GetDataSetByName(VarName);
    if AnotherDataSet <> nil then
    begin
//      AnotherDataSet := (FModel as TPhastModel).DataSets[DataSetIndex];
      Assert(AnotherDataSet <> DataSet);
      Assert(AnotherDataSet.DataType = Variable.ResultType);
      if AnotherDataSet.Orientation = dsoTop then
      begin
        Layer := 0;
      end;
      if AnotherDataSet.Orientation = dsoFront then
      begin
        Row := 0;
      end;
      if AnotherDataSet.Orientation = dsoSide then
      begin
        Column := 0;
      end;
      case Variable.ResultType of
        rdtDouble:
          begin
            TRealVariable(Variable).Value :=
              AnotherDataSet.RealData[Layer, Row, Column];
          end;
        rdtInteger:
          begin
            TIntegerVariable(Variable).Value :=
              AnotherDataSet.IntegerData[Layer, Row, Column];
          end;
        rdtBoolean:
          begin
            TBooleanVariable(Variable).Value :=
              AnotherDataSet.BooleanData[Layer, Row, Column];
          end;
        rdtString:
          begin
            TStringVariable(Variable).Value :=
              AnotherDataSet.StringData[Layer, Row, Column];
          end;
      else
        Assert(False);
      end;
    end;
  end;
end;

procedure TScreenObject.InitializeVariables(const UsedVariables: TStringList;
  const DataSet: TDataArray; const Expression: TExpression;
  const Compiler: TRbwParser);
var
  VarIndex: integer;
  VarName: string;
  VarPosition: integer;
  Variable: TCustomValue;
  AnotherDataSet: TDataArray;
  Model: TPhastModel;
  GlobalVariable: TGlobalVariable;
begin
  if Expression = nil then
    Exit;
  UsedVariables.Assign(Expression.VariablesUsed);
  Model := FModel as TPhastModel;
  for VarIndex := 0 to UsedVariables.Count - 1 do
  begin
    VarName := UsedVariables[VarIndex];
    VarPosition := Compiler.IndexOfVariable(VarName);
    Variable := Compiler.Variables[VarPosition];
    AnotherDataSet := Model.GetDataSetByName(VarName);
    if AnotherDataSet <> nil then
    begin
      Assert(AnotherDataSet <> DataSet);
      Assert(AnotherDataSet.DataType = Variable.ResultType);
      AnotherDataSet.Initialize;
      Model.AddDataSetToCache(AnotherDataSet);
    end
    else
    begin
      GlobalVariable := Model.GlobalVariables.GetVariableByName(VarName);
      Assert(GlobalVariable <> nil);
      Assert(Variable.ResultType = GlobalVariable.Format);
    end;
  end;
end;

procedure TScreenObject.AssignCellValue(const UsedVariables: TStringList;
  const DataSet: TDataArray; LayerIndex, RowIndex, ColIndex: integer;
  const Compiler: TRbwParser; const Annotation: string;
  var Expression: TExpression; const OtherData: TObject);
var
  InterpValue: TInterpValuesItem;
begin
  case DataSet.Orientation of
    dsoTop:
      begin
        LayerIndex := 0;
      end;
    dsoFront:
      begin
        RowIndex := 0;
      end;
    dsoSide:
      begin
        ColIndex := 0;
      end;
    dso3D:
      begin
        // do nothing.
      end;
    else Assert(False);
  end;
  InterpValue := OtherData as TInterpValuesItem;
  if (InterpValue = nil) or not InterpValue.Values.UsePHAST_Interpolation then
  begin
    UpdateVariables(UsedVariables, DataSet,
      LayerIndex, RowIndex, ColIndex, Compiler);
    EvaluateDataArrayExpression(DataSet, Expression, Compiler);

    case DataSet.Datatype of
      rdtDouble:
        begin
          DataSet.RealData[LayerIndex, RowIndex, ColIndex] :=
            Expression.DoubleResult;
        end;
      rdtInteger:
        begin
          DataSet.IntegerData[LayerIndex, RowIndex, ColIndex] :=
            Expression.IntegerResult;
        end;
      rdtBoolean:
        begin
          DataSet.BooleanData[LayerIndex, RowIndex, ColIndex] :=
            Expression.BooleanResult;
        end;
      rdtString:
        begin
          DataSet.StringData[LayerIndex, RowIndex, ColIndex] :=
            Expression.StringResult;
        end;
    else
      Assert(False);
    end;
    DataSet.Annotation[LayerIndex, RowIndex, ColIndex] := Annotation;
    if (InterpValue <> nil) and (DataSet is TCustomPhastDataSet) then
    begin
      TCustomPhastDataSet(DataSet).IsInterpolatedCell[
        LayerIndex, RowIndex, ColIndex] := False;
    end
  end
  else
  begin
    UpdateGlobalLocations(ColIndex, RowIndex, LayerIndex, DataSet.EvaluatedAt);

    case DataSet.Datatype of
      rdtDouble:
        begin
          AssignRealDataWithPhastInterpolation(DataSet,
            LayerIndex, RowIndex, ColIndex, Annotation, InterpValue);
        end;
      rdtInteger:
        begin
          AssignIntegerDataWithPhastInterpolation(DataSet,
            LayerIndex, RowIndex, ColIndex, Annotation, InterpValue);
        end;
      rdtBoolean:
        begin
          Assert(False);
        end;
      rdtString:
        begin
          Assert(False);
        end;
    else
      Assert(False);
    end;
  end;
end;

function TScreenObject.IntersectAnnotation(const DataSetFormula: string;
  const OtherData: TObject): string;
var
  InterpValue: TInterpValuesItem;
begin
  InterpValue := OtherData as TInterpValuesItem;
  if (InterpValue = nil) or not InterpValue.Values.UsePHAST_Interpolation then
  begin
    if Length(DataSetFormula) > 1000 then
    begin
      result := 'Intersected by ' + Name + ' with a formula '
        + 'that is too long to show. '
    end
    else
    begin
      result := 'Intersected by ' + Name + ' with formula: '
        + DataSetFormula;
    end;
  end
  else
  begin
    result := 'Intersected by ' + Name
      + ': Set by PHAST-style interpolation';
  end;
end;

function TScreenObject.EncloseAnnotation(const DataSetFormula: string;
  const OtherData: TObject): string;
var
  InterpValue: TInterpValuesItem;
begin
  InterpValue := OtherData as TInterpValuesItem;
  if (InterpValue = nil) or not InterpValue.Values.UsePHAST_Interpolation then
  begin
    if Length(DataSetFormula) > 1000 then
    begin
      result := 'Enclosed by ' + Name + ' with a formula '
        + 'that is too long to show. '
    end
    else
    begin
      result := 'Enclosed by ' + Name + ' with formula: '
        + DataSetFormula;
    end;
  end
  else
  begin
    result := 'Enclosed by ' + Name
      + ': Set by PHAST-style interpolation';
  end;
end;

procedure TScreenObject.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    InvalidateModel;
    NotifyGuiOfChange(self);
    Invalidate;
  end;
end;

procedure TScreenObject.SortPoints(const Input: TEdgePointArray;
  out SortedPoints: TEdgePointArray; const APoint, PreviousPoint: TEdgePoint;
  const MaxLength: integer; Const EpsilonX: real = 0;
  Const EpsilonY: real = 0);
var
  PointList: TList;
  PointIndex: integer;
  Point1, Point2: TEdgePoint;
begin
  // Sort the points.
  PointList := TList.Create;
  try
    PointList.Capacity := MaxLength;

    for PointIndex := 0 to PointList.Capacity - 1 do
    begin
      PointList.Add(Addr(Input[PointIndex]));
    end;
    if APoint.X >= PreviousPoint.X then
    begin
      if APoint.Y >= PreviousPoint.Y then
      begin
        PointList.Sort(SortEdgePointsRightUp);
      end
      else
      begin
        PointList.Sort(SortEdgePointsRightDown);
      end;
    end
    else
    begin
      if APoint.Y >= PreviousPoint.Y then
      begin
        PointList.Sort(SortEdgePointsLeftUp);
      end
      else
      begin
        PointList.Sort(SortEdgePointsLeftDown);
      end;
    end;

    // delete duplicated points.
    for PointIndex := PointList.Count - 1 downto 1 do
    begin
      Point1 := PEdgePoint(PointList[PointIndex])^;
      Point2 := PEdgePoint(PointList[PointIndex - 1])^;
      if IsEqual(Point1.X ,Point2.X, EpsilonX)
        and IsEqual(Point1.Y, Point2.Y, EpsilonY) then
      begin
        PointList.Delete(PointIndex);
      end
    end;

    // transfer points to an array.
    SetLength(SortedPoints, PointList.Count);
    for PointIndex := 0 to PointList.Count - 1 do
    begin
      Point1 := PEdgePoint(PointList[PointIndex])^;
      SortedPoints[PointIndex] := Point1;
    end;

  finally
    PointList.Free;
  end;
end;

procedure TScreenObject.AddPointFromColumn(const ColIndex: integer;
  const Grid: TCustomGrid; const PreviousPoint, APoint: TEdgePoint;
  var TempPoints1: TEdgePointArray; var Count: integer;
  const Position: TEdgePosition);
var
  NewPoint: TEdgePoint;
begin
  case EvaluatedAt of
    eaBlocks:
      begin
        NewPoint.X := Grid.ColumnPosition[ColIndex];
      end;
    eaNodes:
      begin
        if (ColIndex = 0) then
        begin
          NewPoint.X := Grid.ColumnPosition[ColIndex];
        end
        else if (ColIndex = Grid.ColumnCount+1)  then
        begin
          NewPoint.X := Grid.ColumnPosition[ColIndex-1];
        end
        else
        begin
          NewPoint.X := (Grid.ColumnPosition[ColIndex]
            + Grid.ColumnPosition[ColIndex-1])/2;
        end;
      end;
    else Assert(False);
  end;
  if IsValueInside(PreviousPoint.X, NewPoint.X, APoint.X) then
  begin
    NewPoint.Y := YIntersection(PreviousPoint, APoint,
      NewPoint.X);
    if IsValueInside(PreviousPoint.Y, NewPoint.Y, APoint.Y) then
    begin
      NewPoint.Position := Position;
      TempPoints1[Count] := NewPoint;
      Inc(Count);
    end;
  end;
end;

procedure TScreenObject.AddPointFromRow(const RowIndex: integer;
  const Grid: TCustomGrid; const PreviousPoint, APoint: TEdgePoint;
  var TempPoints1: TEdgePointArray; var Count: integer;
  const Position: TEdgePosition);
var
  NewPoint: TEdgePoint;
begin
  if ViewDirection = vdSide then
  begin
    case EvaluatedAt of
      eaBlocks:
        begin
          NewPoint.X := Grid.RowPosition[RowIndex];
        end;
      eaNodes:
        begin
          if (RowIndex = 0)  then
          begin
            NewPoint.X := Grid.RowPosition[RowIndex];
          end
          else if (RowIndex = Grid.RowCount+1)  then
          begin
            NewPoint.X := Grid.RowPosition[RowIndex-1];
          end
          else
          begin
            NewPoint.X := (Grid.RowPosition[RowIndex]
              + Grid.RowPosition[RowIndex-1])/2;
          end;
        end;
      else Assert(False);
    end;
    if IsValueInside(PreviousPoint.X, NewPoint.X, APoint.X) then
    begin
      NewPoint.Y := YIntersection(PreviousPoint, APoint,
        NewPoint.X);
      if IsValueInside(PreviousPoint.Y, NewPoint.Y, APoint.Y) then
      begin
        NewPoint.Position := Position;
        TempPoints1[Count] := NewPoint;
        Inc(Count);
      end;
    end;
  end
  else
  begin
    Assert(ViewDirection = vdTop);
    case EvaluatedAt of
      eaBlocks:
        begin
          NewPoint.Y := Grid.RowPosition[RowIndex];
        end;
      eaNodes:
        begin
          if (RowIndex = 0)  then
          begin
            NewPoint.Y := Grid.RowPosition[RowIndex];
          end
          else if (RowIndex = Grid.RowCount+1)  then
          begin
            NewPoint.Y := Grid.RowPosition[RowIndex-1];
          end
          else
          begin
            NewPoint.Y := (Grid.RowPosition[RowIndex]
              + Grid.RowPosition[RowIndex-1])/2;
          end;
        end;
      else Assert(False);
    end;
    if IsValueInside(PreviousPoint.Y, NewPoint.Y, APoint.Y) then
    begin
      NewPoint.X := XIntersection(PreviousPoint, APoint,
        NewPoint.Y);
      if IsValueInside(PreviousPoint.X, NewPoint.X, APoint.X) then
      begin
        NewPoint.Position := Position;
        TempPoints1[Count] := NewPoint;
        Inc(Count);
      end;
    end;
  end;
end;

function TScreenObject.AllSectionsClosed: boolean;
var
  Index: Integer;
begin
  result := True;
  for Index := 0 to SectionCount - 1 do
  begin
    if not SectionClosed[Index] then
    begin
      result := False;
      Exit;
    end;
  end;
end;

procedure TScreenObject.AddPointFromLayer(const LayerIndex: integer;
  const Grid: TCustomGrid;
  const PreviousPoint, APoint: TEdgePoint;
  var TempPoints1: TEdgePointArray;
  var Count: integer; const Position: TEdgePosition);
var
  NewPoint: TEdgePoint;
begin
  case EvaluatedAt of
    eaBlocks:
      begin
        NewPoint.Y := (Grid as TPhastGrid).LayerElevation[LayerIndex];
      end;
    eaNodes:
      begin
        if (LayerIndex = 0)  then
        begin
          NewPoint.Y := (Grid as TPhastGrid).LayerElevation[LayerIndex];
        end
        else if (LayerIndex = Grid.LayerCount+1)  then
        begin
          NewPoint.Y := (Grid as TPhastGrid).LayerElevation[LayerIndex-1];
        end
        else
        begin
          NewPoint.Y := ((Grid as TPhastGrid).LayerElevation[LayerIndex]
            + (Grid as TPhastGrid).LayerElevation[LayerIndex-1])/2;
        end;
      end;
    else Assert(False);
  end;
  if IsValueInside(PreviousPoint.Y, NewPoint.Y, APoint.Y) then
  begin
    NewPoint.X := XIntersection(PreviousPoint, APoint,
      NewPoint.Y);
    if IsValueInside(PreviousPoint.X, NewPoint.X, APoint.X) then
    begin
      NewPoint.Position := Position;
      TempPoints1[Count] := NewPoint;
      Inc(Count);
    end;
  end;
end;

procedure TScreenObject.UpdateFormulaExpression;
begin
  if FFluxBoundary <> nil then
  begin
    FluxBoundary.UpdateFormulaExpression;
  end;
  if FLeakyBoundary <> nil then
  begin
    LeakyBoundary.UpdateFormulaExpression;
  end;
  if FRiverBoundary <> nil then
  begin
    RiverBoundary.UpdateFormulaExpression;
  end;
  if FSpecifiedHeadBoundary <> nil then
  begin
    SpecifiedHeadBoundary.UpdateFormulaExpression;
  end;
  if FWellBoundary <> nil then
  begin
    WellBoundary.UpdateFormulaExpression;
  end;
end;

procedure TScreenObject.UpdateFrontSegments(const Grid: TCustomGrid;
  const EvaluatedAt: TEvaluatedAt);
begin
  Delegate.UpdateFrontSegments(Grid, EvaluatedAt);
end;

procedure TScreenObject.AssignValuesToFrontPhastDataSet(
  const Grid: TCustomGrid;
  const DataSet: TDataArray; OtherData: TObject);
begin
  Delegate.AssignValuesToFrontDataSet(Grid, DataSet, OtherData);
end;

procedure TScreenObject.UpdateSideSegments(const Grid: TCustomGrid;
  const EvaluatedAt: TEvaluatedAt);
begin
  Delegate.UpdateSideSegments(Grid, EvaluatedAt);
end;

procedure TScreenObject.AssignValuesToSidePhastDataSet(const Grid: TCustomGrid;
  const DataSet: TDataArray; OtherData: TObject);
begin
  Delegate.AssignValuesToSideDataSet(Grid, DataSet, OtherData);
end;

procedure TScreenObject.UpdateBox;
var
  Index: integer;
  APoint: TPoint2D;
begin
  if FRecalculateBox and (Count > 0) then
  begin
    FMaxX := FPoints[0].X;
    FMinX := FMaxX;
    FMaxY := FPoints[0].Y;
    FMinY := FMaxY;
    for Index := 1 to Count - 1 do
    begin
      APoint := FPoints[Index];
      if APoint.X > FMaxX then
      begin
        FMaxX := APoint.X;
      end
      else if APoint.X < FMinX then
      begin
        FMinX := APoint.X;
      end;

      if APoint.Y > FMaxY then
      begin
        FMaxY := APoint.Y;
      end
      else if APoint.Y < FMinY then
      begin
        FMinY := APoint.Y;
      end;
    end;
    FRecalculateBox := False;
  end;
end;

procedure TScreenObject.CacheElevationArrays;
begin
  if FHigher3DElevations <> nil then
  begin
    FHigher3DElevations.Cache;
  end;
  if FLower3DElevations <> nil then
  begin
    FLower3DElevations.Cache;
  end;
end;

procedure TScreenObject.UpdateCellCache(CellList: TCellAssignmentList;
  EvalAt: TEvaluatedAt; Orientation: TDataSetOrientation;
  AssignmentLocation: TAssignmentLocation);
begin
//  Exit;
  FCachedCells.UpdateCache(CellList, EvalAt, Orientation, AssignmentLocation);
end;

procedure TScreenObject.RestoreSubscriptionToList(List: TList;
  const AName: string; Sender: TObject; Subscriptions: TObjectList);
var
  DS: TObserver;
  Observer: TObserver;
  Index: Integer;
begin
  if FCanInvalidateModel then
  begin
    Index := List.IndexOf(Sender);
    Assert(Index >= 0);
    Observer := Subscriptions[Index] as TObserver;
    DS := (FModel as TPhastModel).GetObserverByName(AName);
    Assert(DS <> nil);
    DS.TalksTo(Observer);
    Observer.UpToDate := False;
  end;
end;

procedure TScreenObject.RemoveSubscriptionFromList(const AName: string;
  Sender: TObject; List: TList; SubscriptionList: TObjectList);
var
  DS: TObserver;
  Observer: TObserver;
  Index: Integer;
begin
  if FCanInvalidateModel then
  begin
    Index := List.IndexOf(Sender);
    Assert(Index >= 0);
    Observer := SubscriptionList[Index] as TObserver;
    DS := (FModel as TPhastModel).GetObserverByName(AName);
    Assert(DS <> nil);
    DS.StopsTalkingTo(Observer);
  end;
end;

procedure TScreenObject.CreateOrRetrieveBoundaryFormulaObject(const Index: Integer;
  ADataSet: TDataArray; var FormulaObject: TFormulaObject);
begin
  if FBoundaryDataSetFormulas[Index] = nil then
  begin
    FormulaObject := frmGoPhast.PhastModel.FormulaManager.Add;
    FBoundaryDataSetFormulas[Index] := FormulaObject;
    FormulaObject.Parser := frmGoPhast.PhastModel.GetCompiler(
      ADataSet.Orientation, ADataSet.EvaluatedAt);
    FormulaObject.AddSubscriptionEvents(
      GlobalRemoveBoundaryDataArraySubscription,
      GlobalRestoreBoundaryDataArraySubscription, self);
  end
  else
  begin
    FormulaObject := FBoundaryDataSetFormulas[Index];
  end;
end;

procedure TScreenObject.CreateOrRetrieveFormulaObject(const Index: Integer;
  ADataSet: TDataArray; var FormulaObject: TFormulaObject);
begin
  if FDataSetFormulas[Index] = nil then
  begin
    FormulaObject := frmGoPhast.PhastModel.FormulaManager.Add;
    FDataSetFormulas[Index] := FormulaObject;
    FormulaObject.Parser := frmGoPhast.PhastModel.GetCompiler(ADataSet.Orientation, ADataSet.EvaluatedAt);
    FormulaObject.AddSubscriptionEvents(
      GlobalRemoveScreenObjectDataArraySubscription,
      GlobalRestoreDataArraySubscription, self);
  end
  else
  begin
    FormulaObject := FDataSetFormulas[Index];
  end;
end;

procedure TScreenObject.SetFormulaParsers;
var
  Index: Integer;
  FormulaObject: TFormulaObject;
  ADataArray: TDataArray;
begin
  SetElevationFormulaParser;
  SetHigherElevationFormulaParser;
  SetLowerElevationFormulaParser;
  for Index := 0 to FDataSetFormulas.Count - 1 do
  begin
    FormulaObject := FDataSetFormulas[Index];
    ADataArray := DataSets[Index];
    FormulaObject.Parser := frmGoPhast.PhastModel.GetCompiler(
      ADataArray.Orientation, ADataArray.EvaluatedAt);

  end;
end;

procedure TScreenObject.CreateFormulaObjects;
begin
  CreateElevationFormulaObject;
  CreateHigherElevationFormulaObject;
  CreateLowerElevationFormulaObject;
end;

procedure TScreenObject.CreateLowerElevationFormulaObject;
begin
  FLowerElevationFormulaObject := frmGoPhast.PhastModel.FormulaManager.Add;
  SetLowerElevationFormulaParser;
  FLowerElevationFormulaObject.AddSubscriptionEvents(
    GlobalRemoveLowerElevationSubscription,
    GlobalRestoreLowerElevationSubscription, self);
end;

procedure TScreenObject.CreateHigherElevationFormulaObject;
begin
  FHigherElevationFormulaObject := frmGoPhast.PhastModel.FormulaManager.Add;
  SetHigherElevationFormulaParser;
  FHigherElevationFormulaObject.AddSubscriptionEvents(
    GlobalRemoveHigherElevationSubscription,
    GlobalRestoreHigherElevationSubscription, self);
end;

procedure TScreenObject.CreateElevationFormulaObject;
begin
  if ElevationCount = ecOne then
  begin
    FElevationFormulaObject := frmGoPhast.PhastModel.FormulaManager.Add;
    SetElevationFormulaParser;
    FElevationFormulaObject.AddSubscriptionEvents(
      GlobalRemoveElevationSubscription,
      GlobalRestoreElevationSubscription, self);
  end;
end;

procedure TScreenObject.SetLowerElevationFormulaParser;
begin
  if FLowerElevationFormulaObject <> nil then
  begin
    FLowerElevationFormulaObject.Parser := GetElevationCompiler;
  end;
end;

procedure TScreenObject.SetHigherElevationFormulaParser;
begin
  if FHigherElevationFormulaObject <> nil then
  begin
    FHigherElevationFormulaObject.Parser := GetElevationCompiler;
  end;
end;

function TScreenObject.GetElevationCompiler: TRbwParser;
begin
  result := nil;
  if FModel <> nil then
  begin
    case ViewDirection of
      vdTop:
        begin
          result := GetCompiler(dsoTop, EvaluatedAt);
        end;
      vdFront:
        begin
          result := GetCompiler(dsoFront, EvaluatedAt);
        end;
      vdSide:
        begin
          result := GetCompiler(dsoSide, EvaluatedAt);
        end;
    else
      Assert(False);
    end;
  end;
end;

function TScreenObject.GetElevationFormula: string;
begin
  if ElevationCount = ecOne then
  begin
    if FElevationFormulaObject = nil then
    begin
      CreateElevationFormulaObject
    end;
    result := FElevationFormulaObject.Formula;
  end
  else
  begin
    result := '0.'
  end;
end;

procedure TScreenObject.SetElevationFormulaParser;
begin
  if FElevationFormulaObject <> nil then
  begin
    FElevationFormulaObject.Parser := GetElevationCompiler;
  end;
end;

procedure TScreenObject.CacheSegments;
begin
  if FSegments.UpToDate and not FSegments.FCleared then
  begin
    FSegments.CacheData;
  end;
  FreeAndNil(FGpcPolygons);
end;

procedure TScreenObject.DrawPointMarker(LineColor32: TColor32;
  FillColor32: TColor32; const Bitmap32: TBitmap32;
  IntPoint: TPoint; LineWidth: Single);
  function GetLow(AnInt: integer): integer;
  begin
    if AnInt < -MAXINT + SquareSize  then
    begin
      result := -MAXINT
    end
    else
    begin
      result := AnInt - SquareSize;
    end;
  end;
  function GetHigh(AnInt: integer): integer;
  begin
    if AnInt > MAXINT - SquareSize -1 then
    begin
      result := MAXINT
    end
    else
    begin
      result := AnInt + SquareSize + 1;
    end;
  end;
begin
  DrawBigRectangle32(BitMap32, LineColor32, FillColor32, LineWidth,
    GetLow(IntPoint.X), GetLow(IntPoint.Y),
    GetHigh(IntPoint.X), GetHigh(IntPoint.Y));
end;

procedure TScreenObject.EvaluateDataArrayExpression(const DataSet: TDataArray; var Expression: TExpression; const Compiler: TRbwParser);
var
  IsBoundary: Boolean;
  DI: Integer;
begin
  try
    Expression.Evaluate;
  except
    on E: ERbwParserError do
    begin
      DI := IndexOfDataSet(DataSet);
      if DI >= 0 then
      begin
        IsBoundary := False;
      end
      else
      begin
        IsBoundary := True;
        DI := IndexOfBoundaryDataSet(DataSet);
        Assert(DI >= 0);
      end;
      ResetScreenObjectFunction(DI, self, Compiler, DataSet.DataType, E.Message, IsBoundary);
      Expression := Compiler.CurrentExpression;
      Expression.Evaluate;
    end;
  end;
end;

procedure TScreenObject.CreateGpcPolygon;
var
//  SubPolygon: TSubPolygon;
  SectionIndex: Integer;
  VertexIndex: Integer;
  ClosedSectionCount: Integer;
  AVertex: Tgpc_vertex;
begin
  ClosedSectionCount := 0;
  for SectionIndex := 0 to SectionCount - 1 do
  begin
    if SectionClosed[SectionIndex] then
    begin
      Inc(ClosedSectionCount);
    end;
  end;
  FGpcPolygons := TGpcPolygonClass.Create;
  FGpcPolygons.NumberOfContours := ClosedSectionCount;
  ClosedSectionCount := 0;
//  if SubPolygonCount = 0 then
//  begin
//    CreateSubPolygons;
//  end;
  for SectionIndex := 0 to SectionCount - 1 do
  begin
    if SectionClosed[SectionIndex] then
    begin
//      SubPolygon := SubPolygons[SectionIndex];
      FGpcPolygons.VertexCount[ClosedSectionCount] := SectionLength[SectionIndex] - 1;
      for VertexIndex := SectionStart[SectionIndex] to
        SectionStart[SectionIndex] + SectionLength[SectionIndex] - 2 do
      begin
        AVertex.x := Points[VertexIndex].x;
        AVertex.y := Points[VertexIndex].y;
        FGpcPolygons.Vertices[ClosedSectionCount,
          VertexIndex - SectionStart[SectionIndex]] := AVertex;
      end;
      Inc(ClosedSectionCount);
    end;
  end;
end;

function TScreenObject.InsideBox(const X, Y: real): boolean;
begin
  // This function returns True if X,Y are inside the box
  // that outlines the screen object.
  UpdateBox;

  result := (X <= FMaxX) and (X >= FMinX) and (Y <= FMaxY) and (Y >= FMinY);
end;

destructor TScreenObject.Destroy;
var
  Model: TPhastModel;
  Index: Integer;
  DataArray: TDataArray;
  FormulaIndex: Integer;
  FormulaObject: TFormulaObject;
begin
  FGpcPolygons.Free;
  Selected := False;
  ElevationCount := ecZero;
  FModpathParticles.Free;
  FImportedSectionElevations.Free;
  FImportedHigherSectionElevations.Free;
  FImportedLowerSectionElevations.Free;
  FSectionStarts.Free;
  DestroyLastSubPolygon;

  for Index := DataSetCount - 1 downto 0 do
  begin
    DataArray := DataSets[Index];
    RemoveDataSet(DataArray);
  end;
  for Index := BoundaryDataSetCount - 1 downto 0 do
  begin
    DataArray := BoundaryDataSets[Index];
    RemoveDataSet(DataArray);
  end;

  if FModel <> nil then
  begin
    Model := FModel as TPhastModel;
    if not Model.Clearing and not (csDestroying in Model.ComponentState) then
    begin
      ElevationFormula := '0';
      HigherElevationFormula := '0';
      LowerElevationFormula := '0';
    end;
  end;

  FModflowBoundaries.Free;

  FInterpValues.Free;
  FFluxBoundary.Free;
  FLeakyBoundary.Free;
  FRiverBoundary.Free;
  FSpecifiedHeadBoundary.Free;
  FSpecifiedSolutionBoundary.Free;
  FWellBoundary.Free;

  FDelegateCollection.Free;

  if FElevSubscription <> nil then
  begin
    FElevSubscription.StopsTalkingTo(self);
  end;
  FElevSubscription.Free;

  if FTopElevSubscription <> nil then
  begin
    FTopElevSubscription.StopsTalkingTo(self);
  end;
  FTopElevSubscription.Free;

  if FBottomElevSubscription <> nil then
  begin
    FBottomElevSubscription.StopsTalkingTo(self);
  end;
  FBottomElevSubscription.Free;
  FDataSets.Free;
  FBoundaryDataSets.Free;
  ClearSubPolygons;
  for FormulaIndex := 0 to FDataSetFormulas.Count - 1 do
  begin
    FormulaObject := FDataSetFormulas[FormulaIndex];
    if frmGoPhast.PhastModel <> nil then
    begin
      frmGoPhast.PhastModel.FormulaManager.Remove(FormulaObject,
        GlobalRemoveScreenObjectDataArraySubscription,
        GlobalRestoreDataArraySubscription, self);
    end;
  end;
  FDataSetFormulas.Free;
  if FBoundaryDataSetFormulas <> nil then
  begin
    for Index := 0 to FBoundaryDataSetFormulas.Count - 1 do
    begin
      FormulaObject := FBoundaryDataSetFormulas[Index];
      if frmGoPhast.PhastModel <> nil then
      begin
        frmGoPhast.PhastModel.FormulaManager.Remove(FormulaObject,
          GlobalRemoveBoundaryDataArraySubscription,
          GlobalRestoreBoundaryDataArraySubscription, self);
      end;
    end;
  end;
  FBoundaryDataSetFormulas.Free;
  FSelectLines.Free;
  FSegments.Free;
  inherited;
  FDataSetSubscriptions.Free;
  FBoundaryDataSetSubscriptions.Free;
  FHigher3DElevations.Free;
  FLower3DElevations.Free;

  FDataSetMixtureSubscriptions.Free;
  FImportedValues.Free;
  FCachedCells.Free;
end;

procedure TScreenObject.DestroyLastSubPolygon;
begin
  FreeAndNil(FLastSubPolygon);
end;

function TScreenObject.GetFillColor: TColor;
begin
  Result := FFillColor;
end;

function TScreenObject.GetFluxBoundary: TFluxBoundary;
begin
  CreatePhastFluxBoundary;
  result := FFluxBoundary
end;

function TScreenObject.GetDataSetCapacity: integer;
begin
  result := FDataSets.Capacity;
end;

function TScreenObject.GetDataSetCount: integer;
begin
  result := FDataSets.Count;
end;

function TScreenObject.GetDataSetFormulas(const Index: integer): string;
var
  FormulaObject: TFormulaObject;
begin
  FormulaObject := FDataSetFormulas[Index];
  if FormulaObject = nil then
  begin
    result := '';
  end
  else
  begin
    result := FormulaObject.Formula;
  end;
end;

function TScreenObject.GetDataSets(const Index: integer): TDataArray;
begin
  result := FDataSets[Index];
end;

function TScreenObject.GetCount: integer;
begin
  Result := FCount;
end;

procedure TScreenObject.SetFillColor(const Value: TColor);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    InvalidateModel;
  end;
end;

function TScreenObject.IsPointInside(const APoint: TPoint2D;
  out SectionIndex: integer): boolean;
begin
  result := IsPointInside(APoint.X, APoint.Y, SectionIndex);
end;

procedure TScreenObject.Loaded;
begin
  inherited;
  if FLeakyBoundary <> nil then
  begin
    LeakyBoundary.Loaded;
  end;

  if FRiverBoundary <> nil then
  begin
    RiverBoundary.Loaded;
  end;

  if FSpecifiedHeadBoundary <> nil then
  begin
    SpecifiedHeadBoundary.Loaded
  end;

  if (ModflowBoundaries.FModflowChdBoundary <> nil)
    and not ModflowBoundaries.FModflowChdBoundary.Used then
  begin
    FreeAndNil(ModflowBoundaries.FModflowChdBoundary);
  end;
  if (ModflowBoundaries.FModflowGhbBoundary <> nil)
    and not ModflowBoundaries.FModflowGhbBoundary.Used then
  begin
    FreeAndNil(ModflowBoundaries.FModflowGhbBoundary);
  end;
  if (ModflowBoundaries.FModflowWellBoundary <> nil)
    and not ModflowBoundaries.FModflowWellBoundary.Used then
  begin
    FreeAndNil(ModflowBoundaries.FModflowWellBoundary);
  end;
  if (ModflowBoundaries.FModflowRivBoundary <> nil)
    and not ModflowBoundaries.FModflowRivBoundary.Used then
  begin
    FreeAndNil(ModflowBoundaries.FModflowRivBoundary);
  end;
  if (ModflowBoundaries.FModflowDrnBoundary <> nil)
    and not ModflowBoundaries.FModflowDrnBoundary.Used then
  begin
    FreeAndNil(ModflowBoundaries.FModflowDrnBoundary);
  end;
  if (ModflowBoundaries.FModflowDrtBoundary <> nil)
    and not ModflowBoundaries.FModflowDrtBoundary.Used then
  begin
    FreeAndNil(ModflowBoundaries.FModflowDrtBoundary);
  end;
  if (ModflowBoundaries.FModflowRchBoundary <> nil)
    and not ModflowBoundaries.FModflowRchBoundary.Used then
  begin
    FreeAndNil(ModflowBoundaries.FModflowRchBoundary);
  end;
  if (ModflowBoundaries.FModflowEvtBoundary <> nil)
    and not ModflowBoundaries.FModflowEvtBoundary.Used then
  begin
    FreeAndNil(ModflowBoundaries.FModflowEvtBoundary);
  end;
  if (ModflowBoundaries.FModflowEtsBoundary <> nil)
    and not ModflowBoundaries.FModflowEtsBoundary.Used then
  begin
    FreeAndNil(ModflowBoundaries.FModflowEtsBoundary);
  end;
  if (ModflowBoundaries.FModflowResBoundary <> nil)
    and not ModflowBoundaries.FModflowResBoundary.Used then
  begin
    FreeAndNil(ModflowBoundaries.FModflowResBoundary);
  end;
  if (ModflowBoundaries.FModflowLakBoundary <> nil)
    and not ModflowBoundaries.FModflowLakBoundary.Used then
  begin
    FreeAndNil(ModflowBoundaries.FModflowLakBoundary);
  end;
  if (ModflowBoundaries.FModflowSfrBoundary <> nil)
    and not ModflowBoundaries.FModflowSfrBoundary.Used then
  begin
    FreeAndNil(ModflowBoundaries.FModflowSfrBoundary);
  end;
  if (ModflowBoundaries.FModflowUzfBoundary <> nil)
    and not ModflowBoundaries.FModflowUzfBoundary.Used then
  begin
    FreeAndNil(ModflowBoundaries.FModflowUzfBoundary);
  end;
  if (ModflowBoundaries.FModflowHeadObservations <> nil)
    and not ModflowBoundaries.FModflowHeadObservations.Used then
  begin
    FreeAndNil(ModflowBoundaries.FModflowHeadObservations);
  end;
  if (ModflowBoundaries.FModflowHfbBoundary <> nil)
    and not ModflowBoundaries.FModflowHfbBoundary.Used then
  begin
    FreeAndNil(ModflowBoundaries.FModflowHfbBoundary);
  end;
end;

function TScreenObject.GetSelectedVertices(const index: integer): boolean;
begin
  ValidateIndex(Index);
  result := FSelectedVertices[index];
end;

procedure TScreenObject.SetSelectedVertices(const index: integer; const Value:
  boolean);
begin
  ValidateIndex(Index);
  if FSelectedVertices[index] <> Value then
  begin
    InvalidateModel;
    if Value then
    begin
      Inc(FSelectedVertexCount);
    end
    else
    begin
      Dec(FSelectedVertexCount);
    end;
    FSelectedVertices[index] := Value;
  end;
end;

procedure TScreenObject.SetSectionStarts(const Value: TValueArrayStorage);
begin
  FIsClosedCached := False;
  if (Value = nil) or (Value.Count = 0) then
  begin
    FreeAndNil(FSectionStarts);
  end
  else
  begin
    CreateSectionStarts;
    FSectionStarts.Assign(Value);
  end;
end;

procedure TScreenObject.SetSelected(const Value: boolean);
var
  Index: integer;
begin
  // You can't select a deleted object.
  if Value and Deleted then
    Exit;
  if FSelected <> Value then
  begin
    FSelected := Value;
    if FSelected then
    begin
      Visible := True;
      if (FModel <> nil) and FCanInvalidateModel then
      begin
        (FModel as TPhastModel).ScreenObjectSelected;
      end;
    end
    else
    begin
      if (FModel <> nil) and FCanInvalidateModel then
      begin
        (FModel as TPhastModel).ScreenObjectUnSelected;
      end;
    end;
    InvalidateModel;
  end;

  // If the object is not selected, it can't have any selected vertices.
  if not FSelected then
  begin
    if (FSelectedVertexCount > 0) or FNeedToResetSelectedVertexCount then
    begin
      for Index := 0 to Length(FSelectedVertices) - 1 do
      begin
        FSelectedVertices[Index] := False;
      end;
    end;
    FSelectedVertexCount := 0;
    FNeedToResetSelectedVertexCount := False;
  end;
end;

function TScreenObject.GetMaxX: real;
begin
  UpdateBox;
  result := FMaxX;
end;

function TScreenObject.GetMaxY: real;
begin
  UpdateBox;
  result := FMaxY;
end;

function TScreenObject.GetMfBoundary(
  ParamType: TParameterType): TModflowParamBoundary;
begin
  result := nil;
  case ParamType of
    ptUndefined..ptLPF_VKCB: Assert(False);
    ptCHD: result := ModflowChdBoundary;
    ptGHB: result := ModflowGhbBoundary;
    ptQ: result := ModflowWellBoundary;
    ptRIV: result := ModflowRivBoundary;
    ptDRN: result := ModflowDrnBoundary;
    ptDRT: result := ModflowDrtBoundary;
    ptRCH: result := ModflowRchBoundary;
    ptEVT: result := ModflowEvtBoundary;
    ptETS: result := ModflowEtsBoundary;
    else Assert(False);
  end;
end;

function TScreenObject.GetMinX: real;
begin
  UpdateBox;
  result := FMinX;
end;

function TScreenObject.GetMinY: real;
begin
  UpdateBox;
  result := FMinY;
end;

function TScreenObject.GetSectionClosed(const Index: integer): boolean;
var
  StartPoint, EndPoint: TPoint2D;
begin
  Assert(Index >= 0);
  Assert(Index <= SectionCount);
  result := SectionEnd[Index] - SectionStart[Index] >= 3;
  if result then
  begin
    StartPoint := Points[SectionStart[Index]];
    EndPoint := Points[SectionEnd[Index]];
    result := (StartPoint.X = EndPoint.X) and (StartPoint.Y = EndPoint.Y);
  end;
end;

function TScreenObject.GetSectionCount: integer;
begin
  if (FSectionStarts = nil) then
  begin
    result := 1;
  end
  else
  begin
    result := FSectionStarts.Count + 1;
  end;
end;

function TScreenObject.GetSectionEnd(const Index: integer): integer;
var
  SC: integer;
begin
  Assert(Index >= 0);
  SC := SectionCount;
  Assert(Index < SC);
  if Index = SC-1 then
  begin
    result := Count-1
  end
  else
  begin
    result := Min(Count-1, SectionStart[Index+1] -1);
  end;
end;

function TScreenObject.GetSectionLength(const Index: integer): integer;
begin
  Result := SectionEnd[Index] - SectionStart[Index] + 1;
end;

function TScreenObject.GetSegments: TCellElementSegmentList;
var
  RotatedPoints: TEdgePointArray;
  TempMinX, TempMinY, TempMaxX, TempMaxY: double;
begin
  if not FSegments.UpToDate then
  begin
    FCachedCells.Invalidate;
    case ViewDirection of
      vdTop:
        begin
          RotatePoints((Model as TPhastModel).Grid, RotatedPoints,
            TempMinX, TempMinY, TempMaxX, TempMaxY);
          UpdateTopSegments(
            (Model as TPhastModel).Grid,
            EvaluatedAt, True, RotatedPoints);
        end;
      vdFront:
        begin
          UpdateFrontSegments(
            (Model as TPhastModel).Grid,
            EvaluatedAt);
        end;
      vdSide:
        begin
          UpdateSideSegments(
            (Model as TPhastModel).Grid,
            EvaluatedAt);
        end;
    end;
  end
  else if FSegments.FCached and FSegments.FCleared then
  begin
    FSegments.RestoreData;
  end;
  result := FSegments;
end;

function TScreenObject.GetSectionStart(const Index: integer): integer;
begin
  Assert(Index >= 0);
  Assert(Index <= SectionCount);
  if Index = 0 then
  begin
    result := 0
  end
  else
  begin
    result := FSectionStarts.IntValues[Index-1];
  end;
end;

function TScreenObject.GetSectionStarts: TValueArrayStorage;
begin
  CreateSectionStarts;
  result := FSectionStarts;
end;

function TScreenObject.GetSelectedVertexCount: integer;
begin
  if FNeedToResetSelectedVertexCount then
  begin
    ResetSelectedVertexCount;
  end;
  result := FSelectedVertexCount;
end;

function TScreenObject.Closed: boolean;
var
  Index: integer;
begin
  result := Count >= 4;
  if not result then
  begin
    Exit;
    // The first and last points must be at the same location for a
    // screen object to be closed.  Thus a triangle would have four points;
    // not three.
  end;

  if FIsClosedCached then
  begin
    result := FCachedClosed;
  end
  else
  begin
    try
      result := False;
      for Index := 0 to SectionCount - 1 do
      begin
        result := SectionClosed[Index];
        if result then
        begin
          Exit;
        end;
      end;
    finally
      FCachedClosed := result;
      FIsClosedCached := True;
    end;
  end;
end;

procedure TScreenObject.ClearSelectedVertices;
var
  Index: integer;
begin
  if FSelectedVertexCount > 0 then
  begin
    for Index := 0 to Length(FSelectedVertices) - 1 do
    begin
      FSelectedVertices[Index] := False;
    end;
    FSelectedVertexCount := 0;
    InvalidateModel;
  end;
end;

procedure TScreenObject.SetDeleted(const Value: boolean);
begin
  if FDeleted <> Value then
  begin
    InvalidateModel;
    FDeleted := Value;
    Selected := False;
    (FModel as TPhastModel).Grid.NeedToRecalculateCellColors;
    UpToDate := False;
    NotifyGuiOfChange(self);
  end;
end;

function TScreenObject.SelectEdge(const X, Y: integer): integer;
var
  Index: integer;
  X1, X2, Y1, Y2, MinX, MaxX, MinY, MaxY: integer;
  SectionIndex: integer;
  StartIndex: integer;
begin
  result := -1;

  if Deleted or not Visible then
  begin
    Exit;
  end;

  SectionIndex := 1;
  for Index := 1 to Count - 1 do
  begin
    if SectionIndex < SectionCount then
    begin
      StartIndex := SectionStart[SectionIndex];
      if Index = StartIndex then
      begin
        Inc(SectionIndex);
        Continue;
      end;
    end;
    X1 := CanvasCoordinates[Index - 1].X;
    X2 := CanvasCoordinates[Index].X;
    Y1 := CanvasCoordinates[Index - 1].Y;
    Y2 := CanvasCoordinates[Index].Y;
    MinX := Min(X1, X2) - SelectEpsilon;
    MaxX := Max(X1, X2) + SelectEpsilon;
    MinY := Min(Y1, Y2) - SelectEpsilon;
    MaxY := Max(Y1, Y2) + SelectEpsilon;

    if IsValueInside(MinX, X, MaxX) and IsValueInside(MinY, Y, MaxY) then
    begin
      if (CanvasCoordinates[Index - 1].X = CanvasCoordinates[Index].X)
        or (CanvasCoordinates[Index - 1].Y = CanvasCoordinates[Index].Y) then
      begin
        result := Index - 1;
        Exit;
      end
      else
      begin

        if Abs(X2 - X1) > Abs(Y2 - Y1) then
        begin
          if Abs((X - X1) / (X2 - X1) * (Y2 - Y1) + Y1 - Y) < SelectEpsilon then
          begin
            result := Index - 1;
            Exit;
          end
        end
        else
        begin
          if Abs((Y - Y1) / (Y2 - Y1) * (X2 - X1) + X1 - X) < SelectEpsilon then
          begin
            result := Index - 1;
            Exit;
          end
        end;
      end;
    end;
  end;
end;

function TScreenObject.Delegate: TCustomScreenObjectDelegate;
begin
  result := FDelegateCollection.Delegate(
    (FModel as TPhastModel).ModelSelection);
end;

function TScreenObject.IsOutsideBoxPlusBuffer(const Location: TPoint2D;
  const BufferDistance: real; const Anisotropy: real): boolean;
begin
  Assert(BufferDistance >= 0);
  result :=
    ((Location.X - BufferDistance) >= MaxX) or
    ((Location.X + BufferDistance) <= MinX) or
    ((Location.Y*Anisotropy - BufferDistance) >= MaxY*Anisotropy) or
    ((Location.Y*Anisotropy + BufferDistance) <= MinY*Anisotropy);
end;

function TScreenObject.IsOutsideSubPolygonBoxPlusBuffer(
  const Location: TPoint2D; const BufferDistance: real; const Anisotropy: real;
  const SubPolygon: TSubPolygon): boolean;
begin
  Assert(BufferDistance >= 0);
  result :=
    ((Location.X - BufferDistance) >= SubPolygon.FMaxX) or
    ((Location.X + BufferDistance) <= SubPolygon.FMinX) or
    ((Location.Y*Anisotropy - BufferDistance) >= SubPolygon.FMaxY*Anisotropy) or
    ((Location.Y*Anisotropy + BufferDistance) <= SubPolygon.FMinY*Anisotropy);
end;

function TScreenObject.IsAnyPointInSubPolygonCloser(const Location: TPoint2D;
  var Distance: real; out ClosestLocation: TPoint2D;
  const Anisotropy: real; const ASubPolygon: TSubPolygon): boolean;
var
  TempLocation: TPoint2D;
  VertexIndex: Integer;
  FirstPoint: TPoint2D;
  SecondPoint: TPoint2D;
  temp: Real;
begin
  result := False;
  if IsOutsideSubPolygonBoxPlusBuffer(Location, Distance,
    Anisotropy, ASubPolygon) then
  begin
    Exit;
  end;

  if ASubPolygon.FSubPolygon1 <> nil then
  begin
    result := IsAnyPointInSubPolygonCloser(Location, Distance,
      ClosestLocation, Anisotropy, ASubPolygon.FSubPolygon1);

  // We want to know not only if any point is closer but, if so, what is the
  // closest point and how close is it.  Therefore we need to be sure to
  // evaluate both ASubPolygon.FSubPolygon1 and ASubPolygon.FSubPolygon2.
    result := IsAnyPointInSubPolygonCloser(Location, Distance,
      ClosestLocation, Anisotropy, ASubPolygon.FSubPolygon2) or result;
  end
  else
  begin
    if ASubPolygon.FCount = 1 then
    begin
      TempLocation := Points[ASubPolygon.FStart];
      temp := PointToPointDist(Location, TempLocation, Anisotropy);
      if temp < Distance then
      begin
        Distance := temp;
        ClosestLocation := TempLocation;
        result := true;
      end;
    end
    else
    begin
      for VertexIndex := 0 to ASubPolygon.FCount - 2 do
      begin
        FirstPoint := Points[ASubPolygon.FStart + VertexIndex];
        SecondPoint := Points[ASubPolygon.FStart + VertexIndex + 1];
        temp := MinDistPointLine(Location, FirstPoint, SecondPoint,
          TempLocation, Anisotropy);
        if temp < Distance then
        begin
          Distance := temp;
          ClosestLocation := TempLocation;
          result := true;
        end;
      end;
    end;
  end;
end;

function TScreenObject.IsAnyPointCloser(const Location: TPoint2D;
  var Distance: real; out ClosestLocation: TPoint2D;
  const Anisotropy: real): boolean;
var
  SubPolygon: TSubPolygon;
  Index: Integer;
begin
  result := False;
  if IsOutsideBoxPlusBuffer(Location, Distance, Anisotropy) then
  begin
    Exit;
  end;
  if SubPolygonCount = 0 then
  begin
    CreateSubPolygons;
  end;

  for Index := 0 to SubPolygonCount - 1 do
  begin
    SubPolygon := SubPolygons[Index]   ;
    result := IsAnyPointInSubPolygonCloser(Location, Distance,
      ClosestLocation, Anisotropy, SubPolygon);
  end;
end;

procedure TScreenObject.NotifyGuiOfChange(Sender: TObject);
begin
  if FModel <> nil then
  begin
    (FModel as TPhastModel).ScreenObjectsChanged(Sender);
  end;
end;

procedure TScreenObject.RefreshGui(Sender: TObject);
begin
  if FModel <> nil then
  begin
    (FModel as TPhastModel).RefreshScreenObjects(Sender);
  end;
end;

function TScreenObject.IAmACurrentScreenObject: boolean;
begin
  Assert(Assigned(FModel));
  result := (FModel as TPhastModel).IsCurrentScreenObject(self);
end;

procedure TScreenObject.UpdateLower3DElevations;
begin
  if FLower3DElevationsNeedsUpdating then
  begin
    AssignLower3DElevations;
  end;
  FLower3DElevations.CheckRestore;
end;

procedure TScreenObject.UpdateHigher3DElevations;
begin
  if FHigher3DElevationsNeedsUpdating then
  begin
    AssignHigher3DElevations;
  end;
  FHigher3DElevations.CheckRestore;
end;

procedure TScreenObject.UpdateImportedValues(DataArray: TDataArray);
begin
  FCurrentValues := ImportedValues.ValuesByName(DataArray.Name);
end;

procedure TScreenObject.DrawSegmentList(const Direction: TViewDirection;
  const Bitmap32: TBitmap32; SegmentList: TList);
var
  FillColor32: TColor32;
  LineColor32: TColor32;
  ObjectIndex: Integer;
  RealPoint: TPoint2D;
  PointCount: Integer;
  AnotherSegment: TLineSegment;
  SegmentIndex: Integer;
  SegmentFound: Boolean;
  Segment: TLineSegment;
  NewSegmentList: TList;
  MultiplePolygons: Boolean;
  P: TPolygon32;
  PointCapacity: Integer;
  ThreeDPoints: TPointArray;
  function ConvertPoint: TPoint;
  begin
    Assert(FModel <> nil);
    result := (FModel as TPhastModel).ConvertPoint(Direction, RealPoint);
  end;
begin
  PointCapacity := 5;
  SetLength(ThreeDPoints, PointCapacity);
  P := nil;
  MultiplePolygons := false;
  NewSegmentList := TObjectList.Create;
  try
    while SegmentList.Count > 0 do
    begin
      NewSegmentList.Clear;
      Segment := SegmentList[SegmentList.Count - 1];
      NewSegmentList.Add(Segment);
      SegmentList.Delete(SegmentList.Count - 1);
      SegmentFound := True;
      while SegmentFound do
      begin
        SegmentFound := False;
        for SegmentIndex := SegmentList.Count - 1 downto 0 do
        begin
          AnotherSegment := SegmentList[SegmentIndex];
          if Segment.IsNextSegment(AnotherSegment) then
          begin
            Segment := AnotherSegment;
            NewSegmentList.Add(Segment);
            SegmentList.Delete(SegmentIndex);
            SegmentFound := True;
          end;
        end;
      end;
      PointCount := NewSegmentList.Count + 1;
      SetLength(ThreeDPoints, PointCount);
      if NewSegmentList.Count > 0 then
      begin
        Segment := NewSegmentList[0];
        RealPoint := Segment.Point1;
        ThreeDPoints[0] := ConvertPoint;
        for ObjectIndex := 0 to NewSegmentList.Count - 1 do
        begin
          Segment := NewSegmentList[ObjectIndex];
          RealPoint := Segment.Point2;
          ThreeDPoints[ObjectIndex + 1] := ConvertPoint;
        end;
        if ColorLine then
        begin
          LineColor32 := Color32(LineColor);
        end
        else
        begin
          LineColor32 := clBlack32;
        end;
        if FillScreenObject then
        begin
          FillColor32 := Color32(FillColor);
          FillColor32 := SetAlpha(FillColor32, 128);
        end
        else
        begin
          FillColor32 := clTransparent32;
        end;
        DrawBigPolygon32(Bitmap32, LineColor32, FillColor32, 1, ThreeDPoints,
          P, MultiplePolygons, True, True);
      end;
    end;
  finally
    NewSegmentList.Free;
  end;
end;

procedure TScreenObject.DrawModflowTopSegmentInsideTopLeft(
  RowIndex: Integer; ColIndex: Integer;
  SegmentList: TList; LayerIndex: Integer; ModflowGrid: TModflowGrid;
  LocalDelegate: TModflowDelegate);
var
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := True;
  if (ColIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex > 0) and (RowIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    //   --------
    //   |  /   |
    //   | /    |
    //   |/     |
    //   |      |
    //   |      |
    //   |      |
    //   --------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := ModflowGrid.TwoDColumnEdgeCenter(ColIndex, RowIndex);
    Segment.Point2 := ModflowGrid.TwoDRowEdgeCenter(ColIndex, RowIndex);
  end;
end;

procedure TScreenObject.DrawModflowTopSegmentInsideBottomLeft(
  SegmentList: TList; LayerIndex: Integer; ModflowGrid: TModflowGrid;
  LocalDelegate: TModflowDelegate;
  RowIndex: Integer; ColIndex: Integer);
var
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := True;
  if (ColIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex < ModflowGrid.RowCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex + 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex > 0) and (RowIndex < ModflowGrid.RowCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex + 1, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    //  --------
    //  |      |
    //  |      |
    //  |      |
    //  |\     |
    //  | \    |
    //  |  \   |
    //  --------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := ModflowGrid.TwoDRowEdgeCenter(ColIndex, RowIndex + 1);
    Segment.Point2 := ModflowGrid.TwoDColumnEdgeCenter(ColIndex, RowIndex);
  end;
end;

procedure TScreenObject.DrawModflowTopSegmentInsideBottomRight(
  RowIndex: Integer; ColIndex: Integer; SegmentList: TList;
  LayerIndex: Integer; ModflowGrid: TModflowGrid;
  LocalDelegate: TModflowDelegate);
var
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := True;
  if (ColIndex < ModflowGrid.ColumnCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex < ModflowGrid.RowCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex + 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex < ModflowGrid.ColumnCount - 1)
    and (RowIndex < ModflowGrid.RowCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex + 1, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    // --------
    // |      |
    // |      |
    // |      |
    // |     /|
    // |    / |
    // |   /  |
    // --------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := ModflowGrid.TwoDColumnEdgeCenter(ColIndex + 1, RowIndex);
    Segment.Point2 := ModflowGrid.TwoDRowEdgeCenter(ColIndex, RowIndex + 1);
  end;
end;

procedure TScreenObject.DrawModflowTopSegmentInsideTopRight(
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
  RowIndex: Integer; ColIndex: Integer; SegmentList: TList; LayerIndex: Integer);
var
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := True;
  if (ColIndex < ModflowGrid.ColumnCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex < ModflowGrid.ColumnCount - 1) and (RowIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    // ---------
    // |    \  |
    // |     \ |
    // |      \|
    // |   +   |
    // |       |
    // |       |
    // |       |
    // ---------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := ModflowGrid.TwoDRowEdgeCenter(ColIndex, RowIndex);
    Segment.Point2 := ModflowGrid.TwoDColumnEdgeCenter(ColIndex + 1, RowIndex);
  end;
end;

procedure TScreenObject.DrawModflowTopSegmentTopLeft(
  RowIndex: Integer; ColIndex: Integer; SegmentList: TList;
  LayerIndex: Integer; ModflowGrid: TModflowGrid;
  LocalDelegate: TModflowDelegate);
var
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := ColIndex > 0;
  if (ColIndex > 0)
    and not LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex > 0) and (RowIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    // -----=======-----
    // |       |       |
    // |       |       |
    // |       |       |
    // |       |   +   |
    // |       |       |
    // |       |       |
    // |       |       |
    // -----------------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := ModflowGrid.TwoDRowEdgeCenter(ColIndex - 1, RowIndex);
    Segment.Point2 := ModflowGrid.TwoDRowEdgeCenter(ColIndex, RowIndex);
  end;
end;

procedure TScreenObject.DrawModflowTopSegmentBottomLeft(
  RowIndex: Integer; ColIndex: Integer; SegmentList: TList; LayerIndex: Integer;
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate);
var
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := (ColIndex > 0);
  if (ColIndex > 0)
    and not LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex < ModflowGrid.RowCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex + 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex > 0) and (RowIndex < ModflowGrid.RowCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex + 1, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    // -----------------
    // |       |       |
    // |       |       |
    // |       |       |
    // |       |   +   |
    // |       |       |
    // |       |       |
    // |       |       |
    // -----=======-----
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := ModflowGrid.TwoDRowEdgeCenter(ColIndex, RowIndex + 1);
    Segment.Point2 := ModflowGrid.TwoDRowEdgeCenter(ColIndex - 1, RowIndex + 1);
  end;
end;

procedure TScreenObject.DrawModflowTopSegmentAboveLeft(
  RowIndex: Integer; ColIndex: Integer; SegmentList: TList;
  LayerIndex: Integer; ModflowGrid: TModflowGrid;
  LocalDelegate: TModflowDelegate);
var
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := RowIndex > 0;
  if (ColIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex > 0)
    and not LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex > 0) and (RowIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    //  ---------
    //  |       |
    //  |       |
    //  |       |
    //  |       |
    //  +       |
    //  +       |
    //  +       |
    //  +--------
    //  +       |
    //  +       |
    //  +       |
    //  |   +   |
    //  |       |
    //  |       |
    //  |       |
    //  ---------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := ModflowGrid.TwoDColumnEdgeCenter(ColIndex, RowIndex);
    Segment.Point2 := ModflowGrid.TwoDColumnEdgeCenter(ColIndex, RowIndex - 1);
  end;
end;

procedure TScreenObject.DrawModflowTopSegmentAboveRight(
  RowIndex: Integer; ColIndex: Integer; SegmentList: TList; LayerIndex: Integer;
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate);
var
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := (RowIndex > 0);
  if (ColIndex < ModflowGrid.ColumnCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex > 0)
    and not LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex < ModflowGrid.ColumnCount - 1) and (RowIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    //  ---------
    //  |       |
    //  |       |
    //  |       |
    //  |       |
    //  |       +
    //  |       +
    //  |       +
    //  --------+
    //  |       +
    //  |       +
    //  |       +
    //  |   +   |
    //  |       |
    //  |       |
    //  |       |
    //  ---------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := ModflowGrid.
      TwoDColumnEdgeCenter(ColIndex + 1, RowIndex - 1);
    Segment.Point2 := ModflowGrid.TwoDColumnEdgeCenter(ColIndex + 1, RowIndex);
  end;
end;

procedure TScreenObject.DrawModflowTopSegmentLeftOutsideAbove(
  RowIndex: Integer; ColIndex: Integer; SegmentList: TList;
  LayerIndex: Integer; ModflowGrid: TModflowGrid;
  LocalDelegate: TModflowDelegate);
var
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := (ColIndex > 0) and (RowIndex > 0);
  if (ColIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex > 0) and (RowIndex > 0)
    and not LocalDelegate.SelectedCells[
    LayerIndex, RowIndex - 1, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    // ---------
    // |       |
    // |       |
    // |       |
    // |       |
    // |       |
    // |       |
    // |       |
    // -----------------
    //      \  |       |
    //       \ |       |
    //        \|       |
    //         |   +   |
    //         |       |
    //         |       |
    //         |       |
    //         ---------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := ModflowGrid.TwoDColumnEdgeCenter(ColIndex, RowIndex);
    Segment.Point2 := ModflowGrid.TwoDRowEdgeCenter(ColIndex - 1, RowIndex);
  end;
end;

procedure TScreenObject.DrawModflowTopSegmentAboveOutsideLeft(
  RowIndex: Integer; ColIndex: Integer; SegmentList: TList;
  LayerIndex: Integer; ModflowGrid: TModflowGrid;
  LocalDelegate: TModflowDelegate);
var
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := (ColIndex > 0) and (RowIndex > 0);
  if (RowIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex > 0) and (RowIndex > 0)
    and not LocalDelegate.SelectedCells[
    LayerIndex, RowIndex - 1, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    //  ---------
    //  |       |
    //  |       |
    //  |       |
    //  |       |
    //  |       |\
    //  |       | \
    //  |       |  \
    //  -----------------
    //          |       |
    //          |       |
    //          |       |
    //          |   +   |
    //          |       |
    //          |       |
    //          |       |
    //          ---------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := ModflowGrid.TwoDColumnEdgeCenter(ColIndex, RowIndex - 1);
    Segment.Point2 := ModflowGrid.TwoDRowEdgeCenter(ColIndex, RowIndex);
  end;
end;

procedure TScreenObject.DrawModflowTopSegmentAboveOutsideRight(
  RowIndex: Integer; ColIndex: Integer; SegmentList: TList; LayerIndex: Integer;
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate);
var
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := (ColIndex < ModflowGrid.ColumnCount - 1) and (RowIndex > 0);
  if (RowIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex < ModflowGrid.ColumnCount - 1) and (RowIndex > 0)
    and not LocalDelegate.SelectedCells[
    LayerIndex, RowIndex - 1, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    //          ---------
    //          |       |
    //          |       |
    //          |       |
    //          |       |
    //         /|       |
    //        / |       |
    //       /  |       |
    //  -----------------
    //  |       |
    //  |       |
    //  |       |
    //  |   +   |
    //  |       |
    //  |       |
    //  |       |
    //  ---------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := ModflowGrid.TwoDRowEdgeCenter(ColIndex, RowIndex);
    Segment.Point2 := ModflowGrid.
      TwoDColumnEdgeCenter(ColIndex + 1, RowIndex - 1);
  end;
end;

procedure TScreenObject.DrawModflowTopSegmentRightOutsideAbove(
  RowIndex: Integer; ColIndex: Integer; SegmentList: TList; LayerIndex: Integer;
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate);
var
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := (RowIndex > 0) and (ColIndex < ModflowGrid.ColumnCount - 1);
  if (ColIndex < ModflowGrid.ColumnCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex < ModflowGrid.ColumnCount - 1) and (RowIndex > 0)
    and not LocalDelegate.SelectedCells[
    LayerIndex, RowIndex - 1, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    //          ---------
    //          |       |
    //          |       |
    //          |       |
    //          |       |
    //          |       |
    //          |       |
    //          |       |
    //  -----------------
    //  |       |  /
    //  |       | /
    //  |       |/
    //  |   +   |
    //  |       |
    //  |       |
    //  |       |
    //  ---------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := ModflowGrid.TwoDRowEdgeCenter(ColIndex + 1, RowIndex);
    Segment.Point2 := ModflowGrid.TwoDColumnEdgeCenter(ColIndex + 1, RowIndex);
  end;
end;

procedure TScreenObject.DrawModflowFrontSegmentInsideTopLeft(
  var FrontPoints: T2DRealPointArray;
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
  SegmentList: TList);
var
  Point2: TPoint2D;
  Point1: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := True;
  if FrontPoints = nil then
  begin
    FrontPoints := ModflowGrid.FrontCellPoints(RowIndex);
  end;
  if (ColIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if (LayerIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex > 0) and (LayerIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    Point1 := FrontPoints[ColIndex * 2, LayerIndex];
    Point2 := FrontPoints[ColIndex * 2, LayerIndex + 1];
    //   --------
    //   |  /   |
    //   | /    |
    //   |/     |
    //   |      |
    //   |      |
    //   |      |
    //   --------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1.x := (Point1.x + Point2.x) / 2;
    Segment.Point1.y := (Point1.y + Point2.y) / 2;
    Segment.Point2 := FrontPoints[ColIndex * 2 + 1, LayerIndex];
  end;
end;

procedure TScreenObject.DrawModflowFrontSegmentInsideBottomLeft(
  FrontPoints: T2DRealPointArray;
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
  SegmentList: TList);
var
  Point2: TPoint2D;
  Point1: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := True;
  if (ColIndex > 0) and
    LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if (LayerIndex < ModflowGrid.LayerCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex + 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex > 0) and (LayerIndex < ModflowGrid.LayerCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex + 1, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    Point1 := FrontPoints[ColIndex * 2, LayerIndex];
    Point2 := FrontPoints[ColIndex * 2, LayerIndex + 1];
    //  --------
    //  |      |
    //  |      |
    //  |      |
    //  |\     |
    //  | \    |
    //  |  \   |
    //  --------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := FrontPoints[ColIndex * 2 + 1, LayerIndex + 1];
    Segment.Point2.x := (Point1.x + Point2.x) / 2;
    Segment.Point2.y := (Point1.y + Point2.y) / 2;
  end;
end;

procedure TScreenObject.DrawModflowFrontSegmentInsideBottomRight(
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
  FrontPoints: T2DRealPointArray; SegmentList: TList);
var
  Point2: TPoint2D;
  Point1: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := True;
  if (ColIndex < ModflowGrid.ColumnCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if (LayerIndex < ModflowGrid.LayerCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex + 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex < ModflowGrid.ColumnCount - 1)
    and (LayerIndex < ModflowGrid.LayerCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex + 1, RowIndex, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    Point1 := FrontPoints[ColIndex * 2 + 2, LayerIndex];
    Point2 := FrontPoints[ColIndex * 2 + 2, LayerIndex + 1];
    // --------
    // |      |
    // |      |
    // |      |
    // |     /|
    // |    / |
    // |   /  |
    // --------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1.x := (Point1.x + Point2.x) / 2;
    Segment.Point1.y := (Point1.y + Point2.y) / 2;
    Segment.Point2 := FrontPoints[ColIndex * 2 + 1, LayerIndex + 1];
  end;
end;

procedure TScreenObject.DrawModflowFrontSegmentInsideTopRight(
  FrontPoints: T2DRealPointArray;
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
  SegmentList: TList);
var
  Point2: TPoint2D;
  Point1: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := True;
  if (ColIndex < ModflowGrid.ColumnCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if (LayerIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex < ModflowGrid.ColumnCount - 1) and (LayerIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    Point1 := FrontPoints[ColIndex * 2 + 2, LayerIndex];
    Point2 := FrontPoints[ColIndex * 2 + 2, LayerIndex + 1];
    // ---------
    // |    \  |
    // |     \ |
    // |      \|
    // |   +   |
    // |       |
    // |       |
    // |       |
    // ---------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := FrontPoints[ColIndex * 2 + 1, LayerIndex];
    Segment.Point2.x := (Point1.x + Point2.x) / 2;
    Segment.Point2.y := (Point1.y + Point2.y) / 2;
  end;
end;

procedure TScreenObject.DrawModflowFrontSegmentTopLeft(
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  LocalDelegate: TModflowDelegate; FrontPoints: T2DRealPointArray;
  SegmentList: TList);
var
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := ColIndex > 0;
  if (ColIndex > 0) and not LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if (LayerIndex > 0) and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex > 0) and (LayerIndex > 0) and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    // -----=======-----
    // |       |       |
    // |       |       |
    // |       |       |
    // |       |   +   |
    // |       |       |
    // |       |       |
    // |       |       |
    // -----------------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := FrontPoints[ColIndex * 2 - 1, LayerIndex];
    Segment.Point2 := FrontPoints[ColIndex * 2, LayerIndex];
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := FrontPoints[ColIndex * 2, LayerIndex];
    Segment.Point2 := FrontPoints[ColIndex * 2 + 1, LayerIndex];
  end;
end;

procedure TScreenObject.DrawModflowFrontSegmentBottomLeft(
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
  FrontPoints: T2DRealPointArray;
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  SegmentList: TList);
var
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := (ColIndex > 0);
  if (ColIndex > 0)
    and not LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if (LayerIndex < ModflowGrid.LayerCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex + 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex > 0) and (LayerIndex < ModflowGrid.LayerCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex + 1, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    // -----------------
    // |       |       |
    // |       |       |
    // |       |       |
    // |       |   +   |
    // |       |       |
    // |       |       |
    // |       |       |
    // -----=======-----
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := FrontPoints[ColIndex * 2 + 1, LayerIndex + 1];
    Segment.Point2 := FrontPoints[ColIndex * 2, LayerIndex + 1];
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := FrontPoints[ColIndex * 2, LayerIndex + 1];
    Segment.Point2 := FrontPoints[ColIndex * 2 - 1, LayerIndex + 1];
  end;
end;

procedure TScreenObject.DrawModflowFrontSegmentAboveLeft(
  FrontPoints: T2DRealPointArray;
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  LocalDelegate: TModflowDelegate; SegmentList: TList);
var
  Point2: TPoint2D;
  Point1: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := LayerIndex > 0;
  if (ColIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if (LayerIndex > 0)
    and not LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex > 0) and (LayerIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    //  ---------
    //  |       |
    //  |       |
    //  |       |
    //  |       |
    //  +       |
    //  +       |
    //  +       |
    //  +--------
    //  +       |
    //  +       |
    //  +       |
    //  |   +   |
    //  |       |
    //  |       |
    //  |       |
    //  ---------
    Point1 := FrontPoints[ColIndex * 2, LayerIndex];
    Point2 := FrontPoints[ColIndex * 2, LayerIndex + 1];
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1.x := (Point1.x + Point2.x) / 2;
    Segment.Point1.y := (Point1.y + Point2.y) / 2;
    Point2 := FrontPoints[ColIndex * 2, LayerIndex - 1];
    Segment.Point2.x := (Point1.x + Point2.x) / 2;
    Segment.Point2.y := (Point1.y + Point2.y) / 2;
  end;
end;

procedure TScreenObject.DrawModflowFrontSegmentAboveRight(
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
  FrontPoints: T2DRealPointArray; SegmentList: TList);
var
  Point2: TPoint2D;
  Point1: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := (LayerIndex > 0);
  if (ColIndex < ModflowGrid.ColumnCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if (LayerIndex > 0)
    and not LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex < ModflowGrid.ColumnCount - 1) and (LayerIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    //  ---------
    //  |       |
    //  |       |
    //  |       |
    //  |       |
    //  |       +
    //  |       +
    //  |       +
    //  --------+
    //  |       +
    //  |       +
    //  |       +
    //  |   +   |
    //  |       |
    //  |       |
    //  |       |
    //  ---------
    Point1 := FrontPoints[ColIndex * 2 + 2, LayerIndex];
    Point2 := FrontPoints[ColIndex * 2 + 2, LayerIndex - 1];
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1.x := (Point1.x + Point2.x) / 2;
    Segment.Point1.y := (Point1.y + Point2.y) / 2;
    Point2 := FrontPoints[ColIndex * 2 + 2, LayerIndex + 1];
    Segment.Point2.x := (Point1.x + Point2.x) / 2;
    Segment.Point2.y := (Point1.y + Point2.y) / 2;
  end;
end;

procedure TScreenObject.DrawModflowFrontSegmentLeftOutsideAbove(
  FrontPoints: T2DRealPointArray;
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  LocalDelegate: TModflowDelegate; SegmentList: TList);
var
  Point1: TPoint2D;
  Point2: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := (ColIndex > 0) and (LayerIndex > 0);
  if (ColIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex > 0) and (LayerIndex > 0)
    and not LocalDelegate.SelectedCells[
    LayerIndex - 1, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    Point1 := FrontPoints[ColIndex * 2, LayerIndex];
    Point2 := FrontPoints[ColIndex * 2, LayerIndex + 1];
    // ---------
    // |       |
    // |       |
    // |       |
    // |       |
    // |       |
    // |       |
    // |       |
    // -----------------
    //      \  |       |
    //       \ |       |
    //        \|       |
    //         |   +   |
    //         |       |
    //         |       |
    //         |       |
    //         ---------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1.x := (Point1.x + Point2.x) / 2;
    Segment.Point1.y := (Point1.y + Point2.y) / 2;
    Segment.Point2 := FrontPoints[ColIndex * 2 - 1, LayerIndex];
  end;
end;

procedure TScreenObject.DrawModflowFrontSegmentAboveOutsideLeft(
  FrontPoints: T2DRealPointArray;
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  LocalDelegate: TModflowDelegate; SegmentList: TList);
var
  Point2: TPoint2D;
  Point1: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := (ColIndex > 0) and (LayerIndex > 0);
  if (LayerIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex > 0) and (LayerIndex > 0)
    and not LocalDelegate.SelectedCells[
    LayerIndex - 1, RowIndex, ColIndex - 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    Point1 := FrontPoints[ColIndex * 2, LayerIndex];
    Point2 := FrontPoints[ColIndex * 2, LayerIndex - 1];
    //  ---------
    //  |       |
    //  |       |
    //  |       |
    //  |       |
    //  |       |\
    //  |       | \
    //  |       |  \
    //  -----------------
    //          |       |
    //          |       |
    //          |       |
    //          |   +   |
    //          |       |
    //          |       |
    //          |       |
    //          ---------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1.x := (Point1.x + Point2.x) / 2;
    Segment.Point1.y := (Point1.y + Point2.y) / 2;
    Segment.Point2 := FrontPoints[ColIndex * 2 + 1, LayerIndex];
  end;
end;

procedure TScreenObject.DrawModflowFrontSegmentAboveOutsideRight(
  LocalDelegate: TModflowDelegate;
  FrontPoints: T2DRealPointArray;
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  ModflowGrid: TModflowGrid; SegmentList: TList);
var
  Point2: TPoint2D;
  Point1: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := (ColIndex < ModflowGrid.ColumnCount - 1) and (LayerIndex > 0);
  if (LayerIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex < ModflowGrid.ColumnCount - 1) and (LayerIndex > 0)
    and not LocalDelegate.SelectedCells[
    LayerIndex - 1, RowIndex, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    Point1 := FrontPoints[ColIndex * 2 + 2, LayerIndex];
    Point2 := FrontPoints[ColIndex * 2 + 2, LayerIndex - 1];
    //          ---------
    //          |       |
    //          |       |
    //          |       |
    //          |       |
    //         /|       |
    //        / |       |
    //       /  |       |
    //  -----------------
    //  |       |
    //  |       |
    //  |       |
    //  |   +   |
    //  |       |
    //  |       |
    //  |       |
    //  ---------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := FrontPoints[ColIndex * 2 + 1, LayerIndex];
    Segment.Point2.x := (Point1.x + Point2.x) / 2;
    Segment.Point2.y := (Point1.y + Point2.y) / 2;
  end;
end;

procedure TScreenObject.DrawModflowFrontSegmentRightOutsideAbove(
  FrontPoints: T2DRealPointArray;
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
  SegmentList: TList);
var
  Point2: TPoint2D;
  Point1: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := (LayerIndex > 0) and (ColIndex < ModflowGrid.ColumnCount - 1);
  if (ColIndex < ModflowGrid.ColumnCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if (ColIndex < ModflowGrid.ColumnCount - 1) and (LayerIndex > 0)
    and not LocalDelegate.SelectedCells[
    LayerIndex - 1, RowIndex, ColIndex + 1] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    Point1 := FrontPoints[ColIndex * 2 + 2, LayerIndex];
    Point2 := FrontPoints[ColIndex * 2 + 2, LayerIndex + 1];
    //          ---------
    //          |       |
    //          |       |
    //          |       |
    //          |       |
    //          |       |
    //          |       |
    //          |       |
    //  -----------------
    //  |       |  /
    //  |       | /
    //  |       |/
    //  |   +   |
    //  |       |
    //  |       |
    //  |       |
    //  ---------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := FrontPoints[ColIndex * 2 + 3, LayerIndex];
    Segment.Point2.x := (Point1.x + Point2.x) / 2;
    Segment.Point2.y := (Point1.y + Point2.y) / 2;
  end;
end;

procedure TScreenObject.DrawModflowSideSegmentInsideTopLeft(
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
  var SidePoints: T2DRealPointArray;
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  SegmentList: TList);
var
  Point2: TPoint2D;
  Point1: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := True;
  if SidePoints = nil then
  begin
    SidePoints := ModflowGrid.SideCellPoints(ColIndex);
  end;
  if (RowIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (LayerIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex > 0) and (LayerIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    Point1 := SidePoints[RowIndex * 2, LayerIndex];
    Point2 := SidePoints[RowIndex * 2, LayerIndex + 1];
    //   --------
    //   |  /   |
    //   | /    |
    //   |/     |
    //   |      |
    //   |      |
    //   |      |
    //   --------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1.x := (Point1.x + Point2.x) / 2;
    Segment.Point1.y := (Point1.y + Point2.y) / 2;
    Segment.Point2 := SidePoints[RowIndex * 2 + 1, LayerIndex];
  end;
end;

procedure TScreenObject.DrawModflowSideSegmentInsideBottomLeft(
  SidePoints: T2DRealPointArray;
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate; SegmentList: TList);
var
  Point2: TPoint2D;
  Point1: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := True;
  if (RowIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (LayerIndex < ModflowGrid.LayerCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex + 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex > 0) and (LayerIndex < ModflowGrid.LayerCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex + 1, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    Point1 := SidePoints[RowIndex * 2, LayerIndex];
    Point2 := SidePoints[RowIndex * 2, LayerIndex + 1];
    //  --------
    //  |      |
    //  |      |
    //  |      |
    //  |\     |
    //  | \    |
    //  |  \   |
    //  --------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := SidePoints[RowIndex * 2 + 1, LayerIndex + 1];
    Segment.Point2.x := (Point1.x + Point2.x) / 2;
    Segment.Point2.y := (Point1.y + Point2.y) / 2;
  end;
end;

procedure TScreenObject.DrawModflowSideSegmentInsideBottomRight(
  SidePoints: T2DRealPointArray;
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
  SegmentList: TList);
var
  Point2: TPoint2D;
  Point1: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := True;
  if (RowIndex < ModflowGrid.RowCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex + 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (LayerIndex < ModflowGrid.LayerCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex + 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex < ModflowGrid.RowCount - 1)
    and (LayerIndex < ModflowGrid.LayerCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex + 1, RowIndex + 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    Point1 := SidePoints[RowIndex * 2 + 2, LayerIndex];
    Point2 := SidePoints[RowIndex * 2 + 2, LayerIndex + 1];
    // --------
    // |      |
    // |      |
    // |      |
    // |     /|
    // |    / |
    // |   /  |
    // --------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1.x := (Point1.x + Point2.x) / 2;
    Segment.Point1.y := (Point1.y + Point2.y) / 2;
    Segment.Point2 := SidePoints[RowIndex * 2 + 1, LayerIndex + 1];
  end;
end;

procedure TScreenObject.DrawModflowSideSegmentInsideTopRight(
  LayerIndex: Integer; ModflowGrid: TModflowGrid;
  LocalDelegate: TModflowDelegate; SidePoints: T2DRealPointArray;
  RowIndex: Integer; ColIndex: Integer; SegmentList: TList);
var
  Point2: TPoint2D;
  Point1: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := True;
  if (RowIndex < ModflowGrid.RowCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex + 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (LayerIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex < ModflowGrid.RowCount - 1) and (LayerIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex + 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    Point1 := SidePoints[RowIndex * 2 + 2, LayerIndex];
    Point2 := SidePoints[RowIndex * 2 + 2, LayerIndex + 1];
    // ---------
    // |    \  |
    // |     \ |
    // |      \|
    // |   +   |
    // |       |
    // |       |
    // |       |
    // ---------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := SidePoints[RowIndex * 2 + 1, LayerIndex];
    Segment.Point2.x := (Point1.x + Point2.x) / 2;
    Segment.Point2.y := (Point1.y + Point2.y) / 2;
  end;
end;

procedure TScreenObject.DrawModflowSideSegmentTopLeft(

  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  LocalDelegate: TModflowDelegate; SidePoints: T2DRealPointArray;
  SegmentList: TList);
var
  Segment: TLineSegment;
  ShouldDraw: Boolean;  
begin
  ShouldDraw := RowIndex > 0;
  if (RowIndex > 0)
    and not LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (LayerIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex > 0) and (LayerIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    // -----=======-----
    // |       |       |
    // |       |       |
    // |       |       |
    // |       |   +   |
    // |       |       |
    // |       |       |
    // |       |       |
    // -----------------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := SidePoints[RowIndex * 2 - 1, LayerIndex];
    Segment.Point2 := SidePoints[RowIndex * 2, LayerIndex];
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := SidePoints[RowIndex * 2, LayerIndex];
    Segment.Point2 := SidePoints[RowIndex * 2 + 1, LayerIndex];
  end;
end;

procedure TScreenObject.DrawModflowSideSegmentBottomLeft(
  SidePoints: T2DRealPointArray;
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
  SegmentList: TList);
var
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := (RowIndex > 0);
  if (RowIndex > 0)
    and not LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (LayerIndex < ModflowGrid.LayerCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex + 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex > 0) and (LayerIndex < ModflowGrid.LayerCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex + 1, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    // -----------------
    // |       |       |
    // |       |       |
    // |       |       |
    // |       |   +   |
    // |       |       |
    // |       |       |
    // |       |       |
    // -----=======-----
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := SidePoints[RowIndex * 2 + 1, LayerIndex + 1];
    Segment.Point2 := SidePoints[RowIndex * 2, LayerIndex + 1];
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := SidePoints[RowIndex * 2, LayerIndex + 1];
    Segment.Point2 := SidePoints[RowIndex * 2 - 1, LayerIndex + 1];
  end;
end;

procedure TScreenObject.DrawModflowSideSegmentAboveLeft(
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  LocalDelegate: TModflowDelegate; SidePoints: T2DRealPointArray;
  SegmentList: TList);
var
  Point2: TPoint2D;
  Point1: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := LayerIndex > 0;
  if (RowIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (LayerIndex > 0)
    and not LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex > 0) and (LayerIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    //  ---------
    //  |       |
    //  |       |
    //  |       |
    //  |       |
    //  +       |
    //  +       |
    //  +       |
    //  +--------
    //  +       |
    //  +       |
    //  +       |
    //  |   +   |
    //  |       |
    //  |       |
    //  |       |
    //  ---------
    Point1 := SidePoints[RowIndex * 2, LayerIndex];
    Point2 := SidePoints[RowIndex * 2, LayerIndex + 1];
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1.x := (Point1.x + Point2.x) / 2;
    Segment.Point1.y := (Point1.y + Point2.y) / 2;
    Point2 := SidePoints[RowIndex * 2, LayerIndex - 1];
    Segment.Point2.x := (Point1.x + Point2.x) / 2;
    Segment.Point2.y := (Point1.y + Point2.y) / 2;
  end;
end;

procedure TScreenObject.DrawModflowSideSegmentAboveRight(
  SidePoints: T2DRealPointArray;
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
  SegmentList: TList);
var Point2: TPoint2D;
  Point1: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := (LayerIndex > 0);
  if (RowIndex < ModflowGrid.RowCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex + 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (LayerIndex > 0)
    and not LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex < ModflowGrid.RowCount - 1) and (LayerIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex + 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    //  ---------
    //  |       |
    //  |       |
    //  |       |
    //  |       |
    //  |       +
    //  |       +
    //  |       +
    //  --------+
    //  |       +
    //  |       +
    //  |       +
    //  |   +   |
    //  |       |
    //  |       |
    //  |       |
    //  ---------
    Point1 := SidePoints[RowIndex * 2 + 2, LayerIndex];
    Point2 := SidePoints[RowIndex * 2 + 2, LayerIndex - 1];
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1.x := (Point1.x + Point2.x) / 2;
    Segment.Point1.y := (Point1.y + Point2.y) / 2;
    Point2 := SidePoints[RowIndex * 2 + 2, LayerIndex + 1];
    Segment.Point2.x := (Point1.x + Point2.x) / 2;
    Segment.Point2.y := (Point1.y + Point2.y) / 2;
  end;
end;

procedure TScreenObject.DrawModflowSideSegmentLeftOutsideAbove(
  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  LocalDelegate: TModflowDelegate; SidePoints: T2DRealPointArray;
  SegmentList: TList);
var
  Point2: TPoint2D;
  Point1: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := (RowIndex > 0) and (LayerIndex > 0);
  if (RowIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex > 0) and (LayerIndex > 0)
    and not LocalDelegate.SelectedCells[
    LayerIndex - 1, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    Point1 := SidePoints[RowIndex * 2, LayerIndex];
    Point2 := SidePoints[RowIndex * 2, LayerIndex + 1];
    // ---------
    // |       |
    // |       |
    // |       |
    // |       |
    // |       |
    // |       |
    // |       |
    // -----------------
    //      \  |       |
    //       \ |       |
    //        \|       |
    //         |   +   |
    //         |       |
    //         |       |
    //         |       |
    //         ---------

    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1.x := (Point1.x + Point2.x) / 2;
    Segment.Point1.y := (Point1.y + Point2.y) / 2;
    Segment.Point2 := SidePoints[RowIndex * 2 - 1, LayerIndex];
  end;
end;

procedure TScreenObject.DrawModflowSideSegmentAboveOutsideLeft(
  SidePoints: T2DRealPointArray;

  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  LocalDelegate: TModflowDelegate; SegmentList: TList);
var
  Point2: TPoint2D;
  Point1: TPoint2D;
  Segment: TLineSegment;
  ShouldDraw: Boolean;
begin
  ShouldDraw := (RowIndex > 0) and (LayerIndex > 0);
  if (LayerIndex > 0) and
    LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex > 0) and (LayerIndex > 0)
    and not LocalDelegate.SelectedCells[
    LayerIndex - 1, RowIndex - 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    Point1 := SidePoints[RowIndex * 2, LayerIndex];
    Point2 := SidePoints[RowIndex * 2, LayerIndex - 1];
    //  ---------
    //  |       |
    //  |       |
    //  |       |
    //  |       |
    //  |       |\
    //  |       | \
    //  |       |  \
    //  -----------------
    //          |       |
    //          |       |
    //          |       |
    //          |   +   |
    //          |       |
    //          |       |
    //          |       |
    //          ---------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1.x := (Point1.x + Point2.x) / 2;
    Segment.Point1.y := (Point1.y + Point2.y) / 2;
    Segment.Point2 := SidePoints[RowIndex * 2 + 1, LayerIndex];
  end;
end;

procedure TScreenObject.DrawModflowSideSegmentAboveOutsideRight(

  RowIndex: Integer; ColIndex: Integer; LayerIndex: Integer;
  ModflowGrid: TModflowGrid; LocalDelegate: TModflowDelegate;
  SidePoints: T2DRealPointArray;
  SegmentList: TList);
var
  ShouldDraw: Boolean;
  Point1: TPoint2D;
  Point2: TPoint2D;
  Segment: TLineSegment;
begin
  ShouldDraw := (RowIndex < ModflowGrid.RowCount - 1) and (LayerIndex > 0);
  if (LayerIndex > 0)
    and LocalDelegate.SelectedCells[LayerIndex - 1, RowIndex, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex < ModflowGrid.RowCount - 1)
    and (LayerIndex > 0) and not LocalDelegate.SelectedCells[
    LayerIndex - 1, RowIndex + 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    Point1 := SidePoints[RowIndex * 2 + 2, LayerIndex];
    Point2 := SidePoints[RowIndex * 2 + 2, LayerIndex - 1];
    //          ---------
    //          |       |
    //          |       |
    //          |       |
    //          |       |
    //         /|       |
    //        / |       |
    //       /  |       |
    //  -----------------
    //  |       |
    //  |       |
    //  |       |
    //  |   +   |
    //  |       |
    //  |       |
    //  |       |
    //  ---------
    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := SidePoints[RowIndex * 2 + 1, LayerIndex];
    Segment.Point2.x := (Point1.x + Point2.x) / 2;
    Segment.Point2.y := (Point1.y + Point2.y) / 2;
  end;
end;

procedure TScreenObject.DrawModflowSideSegmentRightOutsideAbove(
  SidePoints: T2DRealPointArray; LocalDelegate: TModflowDelegate;
  ModflowGrid: TModflowGrid;
  LayerIndex: Integer; ColIndex: Integer; RowIndex: Integer;
  SegmentList: TList);
var
  ShouldDraw: Boolean;
  Point1: TPoint2D;
  Point2: TPoint2D;
  Segment: TLineSegment;
begin
  ShouldDraw := (LayerIndex > 0) and (RowIndex < ModflowGrid.RowCount - 1);
  if (RowIndex < ModflowGrid.RowCount - 1)
    and LocalDelegate.SelectedCells[LayerIndex, RowIndex + 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if (RowIndex < ModflowGrid.RowCount - 1)
    and (LayerIndex > 0)
    and not LocalDelegate.SelectedCells[
    LayerIndex - 1, RowIndex + 1, ColIndex] then
  begin
    ShouldDraw := False;
  end;
  if ShouldDraw then
  begin
    Point1 := SidePoints[RowIndex * 2 + 2, LayerIndex];
    Point2 := SidePoints[RowIndex * 2 + 2, LayerIndex + 1];
    //          ---------
    //          |       |
    //          |       |
    //          |       |
    //          |       |
    //          |       |
    //          |       |
    //          |       |
    //  -----------------
    //  |       |  /
    //  |       | /
    //  |       |/
    //  |   +   |
    //  |       |
    //  |       |
    //  |       |
    //  ---------

    Segment := TLineSegment.Create;
    SegmentList.Add(Segment);
    Segment.Point1 := SidePoints[RowIndex * 2 + 3, LayerIndex];
    Segment.Point2.x := (Point1.x + Point2.x) / 2;
    Segment.Point2.y := (Point1.y + Point2.y) / 2;
  end;
end;

//procedure TScreenObject.GetPointsOnLineSegment(
//  const RotatedPoints: TRealPointArray; const XMin, XMax, YMin, YMax: Double;
//  const Index: Integer; out Point1, Point2: TPoint2D;
//  out TempPoints: T2DRealPointArray6; out PointCount: Integer);
//var
//  X1: Real;
//  X2: Real;
//  Y1: Real;
//  Y2: Real;
//  Temp: TPoint2D;
//  function IsSectionStart(Position: integer): boolean;
//  var
//    NextStart: integer;
//    Index: integer;
//  begin
//    result := False;
//    for Index := 0 to SectionCount - 1 do
//    begin
//      NextStart := SectionStart[Index];
//      if NextStart = Position then
//      begin
//        result := True;
//        Exit;
//      end
//      else if NextStart > Position then
//      begin
//        Exit;
//      end;
//    end;
//  end;
//begin
//  Point1 := RotatedPoints[Index];
//  if IsSectionStart(Index+1) then
//  begin
//    TempPoints[0] := Point1;
//    Point2 := Point1;
//    PointCount := 1;
//    Exit;
//  end;
//
//  Point2 := RotatedPoints[Index + 1];
//  TempPoints[0] := Point1;
//  PointCount := 1;
//  X1 := Point1.X;
//  X2 := Point2.X;
//  Y1 := Point1.Y;
//  Y2 := Point2.Y;
//  if ((Point1.X > XMax) and (Point2.X < XMax))
//    or ((Point1.X < XMax) and (Point2.X > XMax)) then
//  begin
//    // The line segment defined by Point1 and Point2
//    // can intersect a line parallel to the right side of the block.
//    Temp.X := XMax;
//    Temp.Y := (XMax - X1) / (X2 - X1) * (Y2 - Y1) + Y1;
//    TempPoints[PointCount] := Temp;
//    Inc(PointCount);
//  end;
//  if ((Point1.X > XMin) and (Point2.X < XMin))
//    or ((Point1.X < XMin) and (Point2.X > XMin)) then
//  begin
//    // The line segment defined by Point1 and Point2
//    // can intersect a line parallel to the left side of the block.
//    Temp.X := XMin;
//    Temp.Y := (XMin - X1) / (X2 - X1) * (Y2 - Y1) + Y1;
//    TempPoints[PointCount] := Temp;
//    Inc(PointCount);
//  end;
//  if ((Point1.Y > YMax) and (Point2.Y < YMax))
//    or ((Point1.Y < YMax) and (Point2.Y > YMax)) then
//  begin
//    // The line segment defined by Point1 and Point2
//    // can intersect a line parallel to the top side of the block.
//    Temp.Y := YMax;
//    Temp.X := (YMax - Y1) / (Y2 - Y1) * (X2 - X1) + X1;
//    TempPoints[PointCount] := Temp;
//    Inc(PointCount);
//  end;
//  if ((Point1.Y > YMin) and (Point2.Y < YMin))
//    or ((Point1.Y < YMin) and (Point2.Y > YMin)) then
//  begin
//    // The line segment defined by Point1 and Point2
//    // can intersect a line parallel to the bottom side of the block.
//    Temp.Y := YMin;
//    Temp.X := (YMin - Y1) / (Y2 - Y1) * (X2 - X1) + X1;
//    TempPoints[PointCount] := Temp;
//    Inc(PointCount);
//  end;
//end;

//procedure TScreenObject.SortPointsInCorrectDirection(Sorter: TList;
//  const Point1, Point2: TPoint2D);
//begin
//  if Point2.X > Point1.X then
//  begin
//    if Point2.Y > Point1.Y then
//    begin
//      Sorter.Sort(SortPointsRightUp);
//    end
//    else
//    begin
//      Sorter.Sort(SortPointsRightDown);
//    end;
//  end
//  else
//  begin
//    if Point2.Y > Point1.Y then
//    begin
//      Sorter.Sort(SortPointsLeftUp);
//    end
//    else
//    begin
//      Sorter.Sort(SortPointsLeftDown);
//    end;
//  end;
//end;

function TScreenObject.DistanceToScreenObject(const Location: TPoint2D;
  out ClosestLocation: TPoint2D; const Anisotropy: real): real;
var
  Index: integer;
  FirstPoint: TPoint2D;
  SecondPoint: TPoint2D;
  temp: real;
  TempLocation: TPoint2D;
begin
  { TODO :
TScreenObject.DistanceToScreenObject is a bottleneck.
Try using a range tree or other data structure to increase speed. }
  Assert(Count >= 1);
  if Count = 1 then
  begin
    ClosestLocation := Points[0];
    result := PointToPointDist(ClosestLocation, Location, Anisotropy);
  end
  else if (Count > MaxPointsInSubPolygon) then
  begin
    ClosestLocation := Points[0];
    result := PointToPointDist(ClosestLocation, Location, Anisotropy);
    IsAnyPointCloser(Location, result, ClosestLocation, Anisotropy);
  end
  else
  begin
    FirstPoint := Points[0];
    SecondPoint := Points[1];
    result := MinDistPointLine(Location, FirstPoint,
      SecondPoint, ClosestLocation, Anisotropy);
    for Index := 1 to Count - 2 do
    begin
      FirstPoint := Points[Index];
      SecondPoint := Points[Index + 1];
      temp := MinDistPointLine(Location, FirstPoint, SecondPoint,
        TempLocation, Anisotropy);
      if temp < result then
      begin
        result := temp;
        ClosestLocation := TempLocation;
      end;
    end;
  end;
end;

function TScreenObject.PointToPointDist(const A, B: TPoint2D;
  const Anisotropy: real): real;
begin
  result := sqrt(sqr(B.x - A.x) + sqr(B.y * Anisotropy - A.y * Anisotropy));
end;

function TScreenObject.Methods: string;
var
  None: boolean;
begin
  None := True;
  result := '';
  if SetValuesOfEnclosedCells then
  begin
    result := 'Enclosed, ';
    None := False;
  end;
  if SetValuesOfIntersectedCells then
  begin
    result := result + 'Intersected, ';
    None := False;
  end;
  if SetValuesByInterpolation then
  begin
    result := result + 'Interpolation, ';
    None := False;
  end;
  if None then
  begin
    result := 'None';
  end
  else
  begin
    setLength(Result, Length(Result) -2);
  end;
end;

function TScreenObject.MinDistPointLine(const P, A, B: TPoint2D;
  out Closest: TPoint2D; const Anisotropy: real): real;
  {
  http://www.simdesign.nl/tips/tip001.html

  Tip #1: Minimum distance between a point and a line
  Added: 15Nov2002
  Author: Nils Haeck
  Category: Geometry

  Question:
  How can I calculate the distance between a point and a line?

  Applicable:
  You can use this code when you need to detect whether the mouse click of the
  user is near or on a line segment or not, like in GetHitTestInfo events.

  Answer:
  We'll use some optimisation theory: the minimum distance between the point
  and the line (which can be expressed as a function) will be at the exact
  location where the derivative of this function is zero.

  Perhaps you remember this from school; the minimum or maximum in a parabola
  is where its gradient is zero.

    * We parametrise the distance of point P to the line between A and B as
      the distance of point P to a point Q on the line:

      point Q = (1-q)A+qB where 0 <= q <= 1

    * The distance PQ is:

      |PQ| = sqrt( ((1-q)Ax + qBx - Px)^2 + (... Y term) )

    * Differentiating gives dPQ/dq = 2((Bx-Ax)q
       + (Ax-Px))(Bx - Ax) + (... Y term).

    * dPQ/dq must be zero for minimum so:

      q = (Px-Ax)(Bx-Ax)+(Py-Ay)(By-Ay) / ((Bx-Ax)^2+(By-Ay)^2)

  Code Sample:
  Note that this code also takes into account situations where your line is
  actually not a line (A=B) and situations where the point P is past any of the
  two endpoints. In this case, the distance to the
  closest endpoint is calculated.
  }
var
  q: real;
begin
  if (A.x = B.x) and (A.y = B.y) then
  begin

    // Point to point
    Result := PointToPointDist(P, A, Anisotropy);
    Closest := A;
  end
  else
  begin

    // Minimum
    q := ((P.x - A.x) * (B.x - A.x) + (P.y - A.y) * Anisotropy
      * (B.y - A.y) * Anisotropy)
      / (sqr(B.x - A.x) + sqr(B.y - A.y) * sqr(Anisotropy));

    // Limit q to 0 <= q <= 1
    if q < 0 then
      q := 0
    else if q > 1 then
      q := 1;

    // Distance
    Closest.X := (1 - q) * A.x + q * B.x;
    Closest.Y := (1 - q) * A.y + q * B.y;
    Result := PointToPointDist(P, Closest, Anisotropy);
  end;
end;

function TScreenObject.ModflowDataSetUsed(DataArray: TDataArray): boolean;
begin
  result :=
    ((ModflowChdBoundary <> nil)
      and ModflowChdBoundary.DataSetUsed(DataArray))
    or ((ModflowGhbBoundary <> nil)
      and ModflowGhbBoundary.DataSetUsed(DataArray))
    or ((ModflowWellBoundary <> nil)
      and ModflowWellBoundary.DataSetUsed(DataArray))
    or ((ModflowRivBoundary <> nil)
      and ModflowRivBoundary.DataSetUsed(DataArray))
    or ((ModflowDrnBoundary <> nil)
      and ModflowDrnBoundary.DataSetUsed(DataArray))
    or ((ModflowDrtBoundary <> nil)
     and ModflowDrtBoundary.DataSetUsed(DataArray))
    or ((ModflowRchBoundary <> nil)
      and ModflowRchBoundary.DataSetUsed(DataArray))
    or ((ModflowEvtBoundary <> nil)
      and ModflowEvtBoundary.DataSetUsed(DataArray))
    or ((ModflowEtsBoundary <> nil)
      and ModflowEtsBoundary.DataSetUsed(DataArray))
    or ((ModflowResBoundary <> nil)
       and ModflowResBoundary.DataSetUsed(DataArray))
    or ((ModflowLakBoundary <> nil)
      and ModflowLakBoundary.DataSetUsed(DataArray))
    or ((ModflowSfrBoundary <> nil)
      and ModflowSfrBoundary.DataSetUsed(DataArray))
end;

procedure TScreenObject.MovePoints(var Dest: TRealPointArray);
begin
  SetLength(Dest, Count);
  Move(FPoints[0], Dest[0], Count * SizeOf(TPoint2D));
end;

procedure TScreenObject.MovePointsWhenCreatingScreenObjectByDeletingEdge(
  DeletedEdge: integer; ExistingObject: TScreenObject);
var
  PhastModel: TPhastModel;
begin
  FIsClosedCached := False;
  Move(FPoints[DeletedEdge + 1], FPoints[0],
    (Count - DeletedEdge - 1) * SizeOf(TPoint2D));
  Move(FSelectedVertices[DeletedEdge + 1], FSelectedVertices[0],
    (Count - DeletedEdge - 1) * SizeOf(boolean));
  Count := Count - DeletedEdge - 1;
  FNeedToResetSelectedVertexCount := True;
  PhastModel := FModel as TPhastModel;
  PhastModel.InsertScreenObject(
    PhastModel.IndexOfScreenObject(ExistingObject) + 1, self);
end;

procedure TScreenObject.MoveSelectedPoints(var Dest: TBooleanDynArray);
begin
  SetLength(Dest, Count);
  Move(FSelectedVertices[0], Dest[0], Count * SizeOf(boolean));
end;

procedure TScreenObject.MoveToPoints(const SourcePoints: TRealPointArray);
begin
  if Length(SourcePoints) = 0 then
  begin
    Exit;
  end;
  FIsClosedCached := False;
  if Capacity < Length(SourcePoints) then
  begin
    Capacity := Length(SourcePoints);
  end;
  Move(SourcePoints[0], FPoints[0], Length(SourcePoints) * SizeOf(TPoint2D));
  FRecalculateBox := True;
end;

procedure TScreenObject.MoveToSelectedPoints(var Source: TBooleanDynArray);
begin
  if Length(Source) = 0 then
  begin
    Exit;
  end;
  if Capacity < Length(Source) then
  begin
    Capacity := Length(Source);
  end;
  Move(Source[0], FSelectedVertices[0], Length(Source) * SizeOf(boolean));
end;

procedure TScreenObject.Set_SetValuesByInterpolation(const Value: boolean);
begin
  if FSetValuesByInterpolation <> Value then
  begin
    FSetValuesByInterpolation := Value;
    InvalidateModel;
    Invalidate;
  end;
end;

class function TScreenObject.ValidName(const OriginalName: string): string;
var
  Index: integer;
  AChar: Char;
begin
  result :=  Trim(OriginalName);
  for Index := 1 to Length(result) do
  begin
    AChar := result[Index];
    if Index = 1 then
    begin
      if not (AChar in ['_', 'a'..'z', 'A'..'Z']) then
      begin
        result[Index] := '_';
      end;
    end
    else
    begin
      if not (AChar in ['_', 'a'..'z', 'A'..'Z', '0'..'9']) then
      begin
        result[Index] := '_';
      end;
    end;
  end;
  if result = '' then
  begin
    result := '_';
  end
end;

procedure TScreenObject.SetUpToDate(const Value: boolean);
var
  Index: integer;
  Observer: TObserver;
begin
  inherited;
  if Value then
  begin
    for Index := 0 to FDataSetSubscriptions.Count -1 do
    begin
      Observer := FDataSetSubscriptions[Index] as TObserver;
      Observer.UpToDate := True;
    end;

    for Index := 0 to FDataSetMixtureSubscriptions.Count -1 do
    begin
      Observer := FDataSetMixtureSubscriptions[Index] as TObserver;
      Observer.UpToDate := True;
    end;
  end
  else
  begin
    if (ModflowHeadObservations <> nil) and (Model <> nil) then
    begin
      (Model as TPhastModel).InvalidateMfHobHeads (self);
    end;
    FListUpToDate := False;
    FHigher3DElevationsNeedsUpdating := True;
    FLower3DElevationsNeedsUpdating := True;
    FCachedCells.Invalidate;
  end;
end;

function TScreenObject.Get_SetValuesOfEnclosedCells: boolean;
begin
  result := FSetValuesOfEnclosedCells;
  if result and (FModel <> nil) then
  begin
    result := Closed;
  end;
end;

{ TCustomScreenObjectCollection }

procedure TScreenObjectCollection.UpdateScreenObjects;
var
  Index: integer;
  Item: TScreenObjectItem;
begin
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TScreenObjectItem;
    Item.UpdateScreenObject;
  end;
end;

{ TScreenObjectItem }

constructor TScreenObjectItem.Create(Collection: TCollection);
begin
  inherited;
  FDataSetNames := TStringList.Create;
  FSelectedVertexCollection := TSelectedVertexCollection.Create;
  FPoints := TPointCollection.Create;
  FDataSetFormulas := TStringList.Create;
  FMixtureFormulas := TStringList.Create;
end;

destructor TScreenObjectItem.Destroy;
begin
  FMixtureFormulas.Free;
  FDataSetNames.Free;
  FSelectedVertexCollection.Free;
  FPoints.Free;
  FDataSetFormulas.Free;
  inherited;
end;

function TScreenObjectItem.GetClassType: string;
begin
  result := ScreenObject.ClassName;
end;

function TScreenObjectItem.GetDataSetFormulas: TStrings;
var
  Index: integer;
begin
  FDataSetFormulas.Clear;
  FDataSetFormulas.Capacity := ScreenObject.DataSetCount;
  for Index := 0 to ScreenObject.DataSetCount - 1 do
  begin
    FDataSetFormulas.Add(ScreenObject.DataSetFormulas[Index])
  end;
  Result := FDataSetFormulas;
end;

function TScreenObjectItem.GetDataSetNames: TStrings;
var
  Index: integer;
  ADataSet: TDataArray;
begin
  FDataSetNames.Clear;
  FDataSetNames.Capacity := ScreenObject.DataSetCount;
  for Index := 0 to ScreenObject.DataSetCount - 1 do
  begin
    ADataSet := ScreenObject.DataSets[Index];
    FDataSetNames.Add(ADataSet.Name)
  end;
  result := FDataSetNames;
end;

function TScreenObjectItem.GetElevationFormula: string;
begin
  if (ScreenObject.ElevationCount = ecOne) then
  begin
    result := ScreenObject.ElevationFormula;
  end
  else
  begin
    result := ''
  end;
end;

function TScreenObjectItem.GetHigherElevationFormula: string;
begin
  if (ScreenObject.ElevationCount = ecTwo) then
  begin
    result := ScreenObject.HigherElevationFormula;
  end
  else
  begin
    result := '';
  end;
end;

function TScreenObjectItem.GetLowerElevationFormula: string;
begin
  if (ScreenObject.ElevationCount = ecTwo) then
  begin
    result := ScreenObject.LowerElevationFormula;
  end
  else
  begin
    result := '';
  end;
end;

procedure TScreenObjectItem.SetClassType(Value: string);
var
  Model: TComponent;
begin
  if (Value = 'TPhastContour') or (Value = 'TPhastScreenObject') then
  begin
    Value := 'TScreenObject'
  end
  else if Value = 'TMultiValueContour' then
  begin
    Value := 'TMultiValueScreenObject'
  end;

  if FScreenObject = nil then
  begin
    Model := (Collection as TScreenObjectCollection).FModel;
    FScreenObject := TScreenObjectClass(GetClass(Value)).Create(Model);
  end;
end;

procedure TScreenObjectItem.SetScreenObject(const Value: TScreenObject);
var
  Index: integer;
  Item: TSelectedVertexItem;
  PointItem: TPointItem;
begin
  FScreenObject := Value;
  if FScreenObject.SelectedVertexCount > 0 then
  begin
    for Index := 0 to FScreenObject.Count - 1 do
    begin
      Item := SelectedVertices.Add as TSelectedVertexItem;
      Item.VertexSelected := FScreenObject.SelectedVertices[Index];
    end;
  end;
  for Index := 0 to FScreenObject.Count - 1 do
  begin
    PointItem := Points.Add as TPointItem;
    PointItem.X := FScreenObject.Points[Index].X;
    PointItem.Y := FScreenObject.Points[Index].Y;
  end;
  FScreenObject.SetSubComponent(True);
end;

procedure TScreenObjectItem.UpdateScreenObject;
var
  ADataSet: TDataArray;
  Index: integer;
  Position: integer;
  PointItem: TPointItem;
  APoint: TPoint2D;
  SelectedItem: TSelectedVertexItem;
  UndoCreate2DScreenObject: TCustomUndo;
  UndoAble: boolean;
  PhastModel: TPhastModel;
  SectionStarts: TValueArrayStorage;
  NewPart: Boolean;
  SectionIndex: Integer;
  NextStart: Integer;
begin
  PhastModel := (Collection as TScreenObjectCollection).FModel as TPhastModel;
  ScreenObject.FIsUpdating := True;
  try
    UndoAble := False;
    // UndoCreate2DScreenObject will be set to nil in
    // UpdateScreenObjectWithName because UndoAble is "False".
    ScreenObject.UpdateScreenObjectWithName(ScreenObject.Name,
      ScreenObject.ViewDirection, UndoCreate2DScreenObject, UndoAble);
    if (ScreenObject.ElevationCount = ecOne) then
    begin
      ScreenObject.ElevationFormula := '0';
      ScreenObject.ElevationFormula := FElevationFormula;
    end;

    if (ScreenObject.ElevationCount = ecTwo) then
    begin
      ScreenObject.HigherElevationFormula := '0';
      ScreenObject.HigherElevationFormula :=
        FHigherElevationFormula;

      ScreenObject.LowerElevationFormula := '0';
      ScreenObject.LowerElevationFormula :=
        FLowerElevationFormula;
    end;

    ScreenObject.ClearDataSets;
    for Index := 0 to FDataSetNames.Count - 1 do
    begin
      ADataSet := PhastModel.GetDataSetByName(FDataSetNames[Index]);
      if ADataSet <> nil then
      begin
//        ADataSet := PhastModel.DataSets[Position];
        Position := ScreenObject.AddDataSet(ADataSet);
        if Position >= 0 then
        begin
          try
          ScreenObject.DataSetFormulas[Position] := FDataSetFormulas[Index];
          except on E: ERbwParserError do
            begin
              case ADataSet.DataType of
                rdtDouble:
                  begin
                    ScreenObject.DataSetFormulas[Position] := '0.';
                  end;
                rdtInteger:
                  begin
                    ScreenObject.DataSetFormulas[Position] := '0';
                  end;
                rdtBoolean:
                  begin
                    ScreenObject.DataSetFormulas[Position] := 'False';
                  end;
                rdtString:
                  begin
                    ScreenObject.DataSetFormulas[Position] := '""';
                  end;
                else Assert(False);
              end;
            
            end;
          end;
        end;
      end;
    end;
    SectionStarts := TValueArrayStorage.Create;
    try
      SectionStarts.DataType := rdtInteger;
      for SectionIndex := 0 to ScreenObject.SectionCount - 1 do
      begin
        SectionStarts.Add(ScreenObject.SectionStart[SectionIndex]);
      end;
      ScreenObject.SectionStarts.Clear;
      ScreenObject.ClearPoints;
      SectionIndex := 0;
      NextStart := 0;
      ScreenObject.BeginUpdate;
      try
        for Index := 0 to Points.Count - 1 do
        begin
          NewPart := Index = NextStart;
          if NewPart then
          begin
            Inc(SectionIndex);
            if SectionIndex < SectionStarts.Count then
            begin
              NextStart := SectionStarts.IntValues[SectionIndex];
            end;
          end;
          PointItem := Points.Items[Index] as TPointItem;
          APoint.X := PointItem.X;
          APoint.Y := PointItem.Y;
          ScreenObject.AddPoint(APoint, NewPart);
        end;
      finally
        ScreenObject.EndUpdate;
      end;
    finally
      SectionStarts.Free;
    end;
    for Index := 0 to SelectedVertices.Count - 1 do
    begin
      SelectedItem := SelectedVertices.Items[Index] as TSelectedVertexItem;
      ScreenObject.SelectedVertices[Index] := SelectedItem.VertexSelected;
    end;
    if PhastModel.FileVersion = '1.0.8.0' then
    begin
      if (ScreenObject.ModflowDrnBoundary <> nil)
        and (ScreenObject.ModflowDrnBoundary.FormulaInterpretation = fiTotal) then
      begin
        ScreenObject.ModflowDrnBoundary.FormulaInterpretation := fiDirect
      end;
      if (ScreenObject.ModflowDrtBoundary <> nil)
        and (ScreenObject.ModflowDrtBoundary.FormulaInterpretation = fiTotal) then
      begin
        ScreenObject.ModflowDrtBoundary.FormulaInterpretation := fiDirect
      end;
      if (ScreenObject.ModflowGhbBoundary <> nil)
        and (ScreenObject.ModflowGhbBoundary.FormulaInterpretation = fiTotal) then
      begin
        ScreenObject.ModflowGhbBoundary.FormulaInterpretation := fiDirect
      end;
      if (ScreenObject.ModflowWellBoundary <> nil)
        and (ScreenObject.ModflowWellBoundary.FormulaInterpretation = fiTotal) then
      begin
        ScreenObject.ModflowWellBoundary.FormulaInterpretation := fiDirect
      end;
      if (ScreenObject.ModflowRivBoundary <> nil)
        and (ScreenObject.ModflowRivBoundary.FormulaInterpretation = fiTotal) then
      begin
        ScreenObject.ModflowRivBoundary.FormulaInterpretation := fiDirect
      end;
    end;
    if PhastModel.IndexOfScreenObject(ScreenObject) < 0 then
    begin
      PhastModel.AddScreenObject(ScreenObject);
    end;

    for Index := 0 to ScreenObject.DataSetCount - 1 do
    begin
      if Index = ScreenObject.InterpValues.Count then
      begin
        ScreenObject.InterpValues.Add;
      end;

      if FMixtureFormulas.Count > Index then
      begin
        ScreenObject.MixtureDataSetFormula[Index] :=
          FMixtureFormulas[Index];
      end
      else
      begin
        ScreenObject.MixtureDataSetFormula[Index] := '0.5';
      end;
    end;
    ScreenObject.UpdateMixtureExpression;
    ScreenObject.UpdateFormulaExpression;
  finally
    ScreenObject.FIsUpdating := False;
  end;
end;

{ TSelectedVertexCollection }

constructor TSelectedVertexCollection.Create;
begin
  inherited Create(TSelectedVertexItem);
end;

{ TPointCollection }

constructor TPointCollection.Create;
begin
  inherited Create(TPointItem);
end;

{ TCellElementSegmentList }

function TCellElementSegmentList.Add(ASegment: TCellElementSegment): Integer;
begin
  result := inherited Add(ASegment);
end;

procedure TCellElementSegmentList.CacheData;
var
//  TempFile: TFileStream;
  Compressor: TCompressionStream;
  LocalCount: integer;
  Index: Integer;
  Segment: TCellElementSegment;
begin
  if UpToDate then
  begin
    if not FCached then
    begin
      if FTempMemoryStream = nil then
      begin
        FTempMemoryStream := TMemoryStream.Create;
      end;
      FTempMemoryStream.Position := 0;
//      TempFile := TFileStream.Create(FTempFileName, fmCreate or fmShareDenyWrite,
//        ReadWritePermissions);
      Compressor := TCompressionStream.Create(ZLib.clDefault, FTempMemoryStream);
      try
        LocalCount := Count;
        Compressor.Write(LocalCount, SizeOf(LocalCount));
        for Index := 0 to Count - 1 do
        begin
          Segment := Items[Index];
          Compressor.Write(Segment.FCol, SizeOf(Segment.FCol));
          Compressor.Write(Segment.FEndPosition, SizeOf(Segment.FEndPosition));
          Compressor.Write(Segment.FLayer, SizeOf(Segment.FLayer));
          Compressor.Write(Segment.FVertexIndex, SizeOf(Segment.FVertexIndex));
          Compressor.Write(Segment.FRow, SizeOf(Segment.FRow));
          Compressor.Write(Segment.FStartPosition, SizeOf(Segment.FStartPosition));
          Compressor.Write(Segment.FX1, SizeOf(Segment.FX1));
          Compressor.Write(Segment.FX2, SizeOf(Segment.FX2));
          Compressor.Write(Segment.FY1, SizeOf(Segment.FY1));
          Compressor.Write(Segment.FY2, SizeOf(Segment.FY2));
          Compressor.Write(Segment.FSectionIndex, SizeOf(Segment.FSectionIndex));
        end;
      finally
        Compressor.Free;
//        TempFile.Free;
      end;
      FCached := True;
    end;
    Clear;
    FreeAndNil(FStartPoints);
    FreeAndNil(FEndPoints);
    FreeAndNil(FRangeTree);
    FCleared := True;
  end;
end;

function TCellElementSegmentList.ClosestSegment(Location: TPoint2D;
  Anisotropy: double): TCellElementSegment;
var
  Distance, TempDistance: double;
  Segment: TCellElementSegment;
  Index: Integer;
  Closest: TPoint2D;
  StartPosition, EndPostion: TPoint2D;
  MinX: double;
  MinY: double;
  MaxX: double;
  MaxY: double;
  Position: Integer;
  IntervalDefinitions: TSearchArray;
  InIntervalList: TList;
  PointArray: TOneDRealArray;
  LeafList: TCellElementLeafList;
  SectionList: TList;
  SegmentIndex: Integer;
  Leaf: TCellElementLeaf;
  Grid: TCustomGrid;
  Limits: TLimit;
  RotatedLocation: TPoint2D;
  FirstPoint, SecondPoint: TPoint2D;
  GridOutline: TPolygon2D;
  GridEdgeSegment: TSegment2D;
  SearchCircle: TCircle;
  ICnt: Integer;
  I1: TPoint2D;
  I2: TPoint2D;
  PolyCount: Integer;
  IntersectionArray: TPolyLine2D;
  PolyIndex: Integer;
  Epsilon: double;
  procedure ProcessSegment;
  begin
    if MinX > Segment.X1  then
    begin
      MinX := Segment.X1;
    end;
    if MinY > Segment.Y1  then
    begin
      MinY := Segment.Y1;
    end;
    if MaxX < Segment.X1  then
    begin
      MaxX := Segment.X1;
    end;
    if MaxY < Segment.Y1  then
    begin
      MaxY := Segment.Y1;
    end;
  end;
  function DistanceToSegment(Segment: TCellElementSegment): double;
  var
    StartPosition, EndPosition: TPoint2D;
  begin
    StartPosition := Segment.FirstPointRealCoord(FScreenObject.ViewDirection);
    EndPosition := Segment.SecondPointRealCoord(FScreenObject.ViewDirection);

    result := FScreenObject.MinDistPointLine(Location, StartPosition,
      EndPosition, Closest, Anisotropy);
  end;
begin
  // Location is in real-world coordinates;
  Assert(UpToDate);
  if Count = 0 then
  begin
    result := nil;
    Exit;
  end;

  FMinDistance := 0;
  if Count > 500 then
  begin
    if FStartPoints = nil then
    begin
      FStartPoints := TRbwQuadTree.Create(nil);
      FEndPoints := TRbwQuadTree.Create(nil);
      FStartPoints.MaxPoints := Max(100, Count div 1000);
      FEndPoints.MaxPoints := FStartPoints.MaxPoints;
      Segment := Items[0];
      MinX := Segment.X1;
      MinY := Segment.Y1;
      ProcessSegment;
      Segment := Items[Count-1];
      ProcessSegment;
      for Index := 0 to 50 do
      begin
        Position := Random(Count);
        Segment := Items[Position];
        ProcessSegment;
      end;
      FStartPoints.XMax := MaxX;
      FStartPoints.XMin := MinX;
      FStartPoints.YMax := MaxY;
      FStartPoints.YMin := MinY;
      FEndPoints.XMax := MaxX;
      FEndPoints.XMin := MinX;
      FEndPoints.YMax := MaxY;
      FEndPoints.YMin := MinY;

      for Index := 0 to Count - 1 do
      begin
        Segment := Items[Index];
        FStartPoints.AddPoint(Segment.X1, Segment.Y1*Anisotropy, Segment);
        FEndPoints.AddPoint(Segment.X2, Segment.Y2*Anisotropy, Segment);
      end;
    end
    else
    begin
      Assert(FEndPoints <> nil);
    end;

    if FScreenObject.ViewDirection = vdTop then
    begin
      RotatedLocation := frmGoPhast.Grid.
        RotateFromRealWorldCoordinatesToGridCoordinates(Location);
    end
    else
    begin
      RotatedLocation := Location;
    end;
    RotatedLocation.y := RotatedLocation.y * Anisotropy;
    Segment := FStartPoints.NearestPointsFirstData(
      RotatedLocation.x, RotatedLocation.y);
    // If the TScreenObject has only point data then the segment with the
    // nearest point must be the nearest segment.
    // If the the Segment location is the same as the location we are looking
    // for, it is the nearest segment.
    FirstPoint :=Segment.FirstPointRealCoord(FScreenObject.ViewDirection);
    if (FScreenObject.Count = FScreenObject.SectionCount) or
      ((FirstPoint.x = Location.x) and (FirstPoint.Y = Location.y)) then
    begin
      result := Segment;
      Exit;
    end;
    Distance := Sqrt(Sqr(FirstPoint.x - Location.x)
      + Sqr(Anisotropy*(FirstPoint.y - Location.y)));
    SecondPoint :=Segment.SecondPointRealCoord(FScreenObject.ViewDirection);
    TempDistance := Sqrt(Sqr(SecondPoint.x - Location.x)
      + Sqr(Anisotropy*(SecondPoint.y - Location.y)));
    FMinDistance := Max(Distance, TempDistance);

    Segment := FEndPoints.NearestPointsFirstData(
      RotatedLocation.x, RotatedLocation.y);
    SecondPoint :=Segment.SecondPointRealCoord(FScreenObject.ViewDirection);
    if (SecondPoint.x = Location.x) and (SecondPoint.y = Location.y) then
    begin
      result := Segment;
      Exit;
    end;
    FirstPoint :=Segment.FirstPointRealCoord(FScreenObject.ViewDirection);
    Distance := Sqrt(Sqr(FirstPoint.x - Location.x)
      + Sqr(Anisotropy*(FirstPoint.y - Location.y)));
    TempDistance := Sqrt(Sqr(SecondPoint.x - Location.x)
      + Sqr(Anisotropy*(SecondPoint.y - Location.y)));
    Distance := Max(Distance, TempDistance);

    FMinDistance := Min(FMinDistance, Distance);
  end
  else if Count > 25 then
  begin
    Segment := Items[0];
    SecondPoint :=Segment.SecondPointRealCoord(FScreenObject.ViewDirection);
    if (SecondPoint.x = Location.x) and (SecondPoint.y = Location.y) then
    begin
      result := Segment;
      Exit;
    end;
    FirstPoint :=Segment.FirstPointRealCoord(FScreenObject.ViewDirection);
    Distance := Sqrt(Sqr(FirstPoint.x - Location.x)
      + Sqr(Anisotropy*(FirstPoint.y - Location.y)));
    TempDistance := Sqrt(Sqr(SecondPoint.x - Location.x)
      + Sqr(Anisotropy*(SecondPoint.y - Location.y)));
    FMinDistance := Max(Distance, TempDistance);

    if FMinDistance = 0 then
    begin
      result := Segment;
      Exit;
    end;
    for Index := 0 to 24 do
    begin
      Segment := Items[Random(Count)];

      SecondPoint :=Segment.SecondPointRealCoord(FScreenObject.ViewDirection);
      if (SecondPoint.x = Location.x) and (SecondPoint.y = Location.y) then
      begin
        result := Segment;
        Exit;
      end;
      FirstPoint :=Segment.FirstPointRealCoord(FScreenObject.ViewDirection);
      Distance := Sqrt(Sqr(FirstPoint.x - Location.x)
        + Sqr(Anisotropy*(FirstPoint.y - Location.y)));
      TempDistance := Sqrt(Sqr(SecondPoint.x - Location.x)
        + Sqr(Anisotropy*(SecondPoint.y - Location.y)));
      Distance := Max(Distance, TempDistance);
      FMinDistance := Min(FMinDistance, Distance);
      if FMinDistance = 0 then
      begin
        result := Segment;
        Exit;
      end;
    end;
  end;

  FMinDistance := FMinDistance * 1.00001;

  if Count > 25 then
  begin
    if FRangeTree = nil then
    begin
      LeafList := TCellElementLeafList.Create;
      LeafList.Capacity := Count;
      for Index := 0 to Count - 1 do
      begin
        Segment := Items[Index];
        LeafList.Add(TCellElementLeaf.Create(Segment,
          FScreenObject.ViewDirection));
      end;
      FRangeTree := TRbwRangeTree.Create(LeafList);
    end;

    Limits := frmGoPhast.Grid.GridLimits(FScreenObject.ViewDirection);
    case FScreenObject.ViewDirection of
      vdTop:
        begin
          MinX := Limits.MinX;
          MinY := Limits.MinY;
          MaxX := Limits.MaxX;
          MaxY := Limits.MaxY;
        end;
      vdFront:
        begin
          MinX := Limits.MinX;
          MinY := Limits.MinZ;
          MaxX := Limits.MaxX;
          MaxY := Limits.MaxZ;
        end;
      vdSide:
        begin
          MinX := Limits.MinZ;
          MinY := Limits.MinY;
          MaxX := Limits.MaxX;
          MaxY := Limits.MaxY;
        end;
    end;

    SetLength(IntervalDefinitions, 4);
    IntervalDefinitions[0].LowerBoundary := MinX;
    IntervalDefinitions[0].UpperBoundary := Location.x + FMinDistance;
    IntervalDefinitions[1].LowerBoundary := Location.x - FMinDistance;
    IntervalDefinitions[1].UpperBoundary := MaxX;

    IntervalDefinitions[2].LowerBoundary := MinY{*Anisotropy};
    IntervalDefinitions[2].UpperBoundary := Location.y{*Anisotropy} + FMinDistance;
    IntervalDefinitions[3].LowerBoundary := Location.y{*Anisotropy} - FMinDistance;
    IntervalDefinitions[3].UpperBoundary := MaxY{*Anisotropy};

    // When searching for the nearest segment for a point outside
    // the grid, the distance between the point and the closest
    // segment may be large which results in a large search interval.
    // To decrease the search interval, find the area of intersection
    // between a circle centered at the search point and the grid
    // outline.
    GridOutline := frmGoPhast.Grid.GridOutline(FScreenObject.ViewDirection);
    if not PointInConvexPolygon(Location, GridOutline) then
    begin
      // define the search circle.
      SearchCircle.x := Location.x;
      SearchCircle.y := Location.y*Anisotropy;
      SearchCircle.Radius := FMinDistance;
      // interesect the last edge with the circle.
      GridEdgeSegment[1] := GridOutline[3];
      GridEdgeSegment[2] := GridOutline[0];
      GridEdgeSegment[1].y := GridEdgeSegment[1].y*Anisotropy;
      GridEdgeSegment[2].y := GridEdgeSegment[2].y*Anisotropy;
      SetLength(IntersectionArray,8);
      PolyCount := 0;
      IntersectionPoint(GridEdgeSegment,SearchCircle, ICnt, I1,I2);
      if ICnt >= 1 then
      begin
        Inc(PolyCount);
        IntersectionArray[PolyCount-1] := I1;
      end;
      if ICnt >= 2 then
      begin
        Inc(PolyCount);
        IntersectionArray[PolyCount-1] := I2;
      end;
      // intersect the remaining edges with the circle.
      for PolyIndex := 1 to Length(GridOutline) - 1 do
      begin
        GridEdgeSegment[1] := GridOutline[PolyIndex-1];
        GridEdgeSegment[2] := GridOutline[PolyIndex];
        GridEdgeSegment[1].y := GridEdgeSegment[1].y*Anisotropy;
        GridEdgeSegment[2].y := GridEdgeSegment[2].y*Anisotropy;
        IntersectionPoint(GridEdgeSegment,SearchCircle, ICnt, I1,I2);
        if ICnt >= 1 then
        begin
          Inc(PolyCount);
          IntersectionArray[PolyCount-1] := I1;
        end;
        if ICnt >= 2 then
        begin
          Inc(PolyCount);
          IntersectionArray[PolyCount-1] := I2;
        end;
      end;
      if PolyCount >= 2 then
      begin
        // use the intersection points to define a search rectangle.
        MinX := IntersectionArray[0].x;
        MaxX := IntersectionArray[0].x;
        MinY := IntersectionArray[0].y;
        MaxY := IntersectionArray[0].y;
        for PolyIndex := 1 to PolyCount - 1 do
        begin
          MinX := Min(MinX,IntersectionArray[PolyIndex].x);
          MaxX := Max(MaxX,IntersectionArray[PolyIndex].x);
          MinY := Min(MinY,IntersectionArray[PolyIndex].y);
          MaxY := Max(MaxY,IntersectionArray[PolyIndex].y);
        end;
        // If the center of the circle is directly above, below,
        // to the left, or to the right of the search rectangle,
        // modify the rectangle to include the rest of the area
        // of the circle.
        if (MinX <= SearchCircle.x) and (SearchCircle.x <= MaxX) then
        begin
          if (MinY <= SearchCircle.y) or (MaxY <= SearchCircle.y) then
          begin
            MinY := SearchCircle.y - SearchCircle.Radius;
          end;
          if (MinY >= SearchCircle.y) or (MaxY >= SearchCircle.y) then
          begin
            MaxY := SearchCircle.y + SearchCircle.Radius;
          end;
        end;
        if (MinY <= SearchCircle.y) and (SearchCircle.y <= MaxY) then
        begin
          if (MinX <= SearchCircle.x) or (MaxX <= SearchCircle.x) then
          begin
            MinX := SearchCircle.x - SearchCircle.Radius;
          end;
          if (MinX >= SearchCircle.x) or (MaxX >= SearchCircle.x) then
          begin
            MaxX := SearchCircle.x + SearchCircle.Radius;
          end;
        end;

        // Expand the rectangle slightly.
        Epsilon := (MaxX - MinX)/1e5;
        MinX := MinX-Epsilon;
        MaxX := MaxX+Epsilon;
        Epsilon := (MaxY - MinY)/1e5;
        MinY := MinY-Epsilon;
        MaxY := MaxY+Epsilon;
      end;

      IntervalDefinitions[0].LowerBoundary :=
        Max(MinX, IntervalDefinitions[0].LowerBoundary);
      IntervalDefinitions[1].UpperBoundary :=
        Min(MaxX, IntervalDefinitions[1].UpperBoundary);

      IntervalDefinitions[2].LowerBoundary :=
        Max(MinY, IntervalDefinitions[2].LowerBoundary);
      IntervalDefinitions[3].UpperBoundary :=
        Min(MaxY, IntervalDefinitions[3].UpperBoundary);
    end;

    SectionList := FRangeTree.Search(IntervalDefinitions);
    if SectionList.Count = 0 then
    begin
      result := nil;
      Assert(False);
      Exit;
    end;

    Leaf := SectionList[0];
    Segment := Leaf.FSegment;
    Distance := DistanceToSegment(Segment);
    result := Segment;
    for SegmentIndex := 0 to SectionList.Count - 1 do
    begin
      Leaf := SectionList[SegmentIndex];
      Segment := Leaf.FSegment;
      TempDistance := DistanceToSegment(Segment);
      if TempDistance < Distance then
      begin
        Distance := TempDistance;
        result := Segment;
      end;
    end;
    Exit;
  end;

  Segment := Items[0];

  Distance := DistanceToSegment(Segment);
  result := Segment;
  if Distance = 0 then
  begin
    Exit;
  end;
  for Index := 0 to Count - 1 do
  begin
    Segment := Items[Index];

    FirstPoint := Segment.FirstPointRealCoord(FScreenObject.ViewDirection);
    SecondPoint := Segment.SecondPointRealCoord(FScreenObject.ViewDirection);

    if (Location.x < FirstPoint.x)
      = (Location.x < SecondPoint.x) then
    begin
      if Distance < Min(Abs(Location.x - FirstPoint.x),
        Abs(Location.x - SecondPoint.x)) then
      begin
        Continue;
      end;
    end;
    if (Location.Y < FirstPoint.y)
      = (Location.Y < SecondPoint.y) then
    begin
      if Distance < Anisotropy*Min(Abs(Location.Y - FirstPoint.y),
        Abs(Location.Y - SecondPoint.y)) then
      begin
        Continue;
      end;
    end;

    TempDistance := DistanceToSegment(Segment);
    if TempDistance < Distance then
    begin
      result := Segment;
      Distance := TempDistance;
      if Distance = 0 then
      begin
        Exit;
      end;
    end;
  end;
end;

constructor TCellElementSegmentList.Create(Model: TComponent;
  ScreenObject: TScreenObject);
begin
  inherited Create;
  FCached := False;
  FCleared := False;
  Assert((Model = nil) or (Model is TPhastModel));
  FModel := Model;
  Assert(ScreenObject <> nil);
  FScreenObject := ScreenObject;
end;

destructor TCellElementSegmentList.Destroy;
begin
  FRangeTree.Free;
  FStartPoints.Free;
  FEndPoints.Free;
  FTempMemoryStream.Free;
//  if FileExists(FTempFileName) then
//  begin
//    DeleteFile(FTempFileName);
//  end;
  inherited;
end;

//procedure TCellElementSegmentList.GetExpandedXInterval(Subject: TObject;
//  out LowerBoundary, UpperBoundary: double);
//var
//  Segment: TCellElementSegment;
//begin
//  Segment := Subject as TCellElementSegment;
//  if Segment.X1 < Segment.X2 then
//  begin
//    LowerBoundary := Segment.X1-FMinDistance;
//    UpperBoundary := Segment.X2+FMinDistance;
//  end
//  else
//  begin
//    LowerBoundary := Segment.X2-FMinDistance;
//    UpperBoundary := Segment.X1+FMinDistance;
//  end;
//end;
//
//procedure TCellElementSegmentList.GetExpandedYInterval(Subject: TObject;
//  out LowerBoundary, UpperBoundary: double);
//var
//  Segment: TCellElementSegment;
//begin
//  Segment := Subject as TCellElementSegment;
//  if Segment.Y1 < Segment.Y2 then
//  begin
//    LowerBoundary := Segment.Y1-FMinDistance;
//    UpperBoundary := Segment.Y2+FMinDistance;
//  end
//  else
//  begin
//    LowerBoundary := Segment.Y2-FMinDistance;
//    UpperBoundary := Segment.Y1+FMinDistance;
//  end;
//end;

function TCellElementSegmentList.GetSegment(Index: Integer):
  TCellElementSegment;
begin
  result := TCellElementSegment(inherited Items[Index]); 
end;

procedure TCellElementSegmentList.RestoreData;
var
//  TempFile: TFileStream;
  DeCompressor: TDecompressionStream;
  LocalCount: integer;
  Index: Integer;
  Segment: TCellElementSegment;
begin
  Assert(UpToDate);
  Assert(FCached);
  Assert(FCleared);
  Assert(FTempMemoryStream <> nil);
  FTempMemoryStream.Position := 0;
//  TempFile := TFileStream.Create(FTempFileName, fmOpenRead or fmShareDenyWrite,
//    ReadWritePermissions);
  DeCompressor := TDecompressionStream.Create(FTempMemoryStream);
  try
    DeCompressor.Read(LocalCount, SizeOf(LocalCount));
    Capacity := LocalCount;
    for Index := 0 to LocalCount - 1 do
    begin
      Segment := TCellElementSegment.Create;
      Add(Segment);
      DeCompressor.Read(Segment.FCol, SizeOf(Segment.FCol));
      DeCompressor.Read(Segment.FEndPosition, SizeOf(Segment.FEndPosition));
      DeCompressor.Read(Segment.FLayer, SizeOf(Segment.FLayer));
      DeCompressor.Read(Segment.FVertexIndex, SizeOf(Segment.FVertexIndex));
      DeCompressor.Read(Segment.FRow, SizeOf(Segment.FRow));
      DeCompressor.Read(Segment.FStartPosition, SizeOf(Segment.FStartPosition));
      DeCompressor.Read(Segment.FX1, SizeOf(Segment.FX1));
      DeCompressor.Read(Segment.FX2, SizeOf(Segment.FX2));
      DeCompressor.Read(Segment.FY1, SizeOf(Segment.FY1));
      DeCompressor.Read(Segment.FY2, SizeOf(Segment.FY2));
      DeCompressor.Read(Segment.FSectionIndex, SizeOf(Segment.FSectionIndex));
    end;
  finally
    DeCompressor.Free;
//    TempFile.Free;
  end;
  FCleared := False;
end;

procedure TCellElementSegmentList.SetSegment(Index: Integer;
  const Value: TCellElementSegment);
begin
  inherited Items[Index] := Value;
end;

procedure TCellElementSegmentList.SetUpToDate(const Value: boolean);
begin
  FUpToDate := Value;
  if Value then
  begin
    (FModel as TPhastModel).SomeSegmentsUpToDate := True;
  end
  else
  begin
    FCached := False;
    FCleared := False;
    FreeAndNil(FStartPoints);
    FreeAndNil(FEndPoints);
    FreeAndNil(FRangeTree);
  end;
end;

{ TCellElementSegment }

function TCellElementSegment.FirstPointRealCoord(ViewDirection: TViewDirection): TPoint2D;
begin
  result.x := X1;
  result.y := Y1;
  if ViewDirection = vdTop then
  begin
    result := frmGoPhast.Grid.
      RotateFromGridCoordinatesToRealWorldCoordinates(result);
  end;
end;

function TCellElementSegment.Length: double;
begin
  result := Sqrt(Sqr(X1 - X2) + Sqr(Y1 - Y2));
end;

procedure TCellElementSegment.Restore(Stream: TDecompressionStream);
begin
  FCol := ReadCompInt(Stream);
  Stream.Read(FEndPosition, SizeOf(FEndPosition));
  FLayer := ReadCompInt(Stream);
  FVertexIndex := ReadCompInt(Stream);
  FRow := ReadCompInt(Stream);
  Stream.Read(FStartPosition, SizeOf(FStartPosition));
  FX1 := ReadCompReal(Stream);
  FX2 := ReadCompReal(Stream);
  FY1 := ReadCompReal(Stream);
  FY2 := ReadCompReal(Stream);
  FSectionIndex := ReadCompInt(Stream);
end;

function TCellElementSegment.SecondPointRealCoord(ViewDirection: TViewDirection): TPoint2D;
begin
  result.x := X2;
  result.y := Y2;
  if ViewDirection = vdTop then
  begin
    result := frmGoPhast.Grid.
      RotateFromGridCoordinatesToRealWorldCoordinates(result);
  end;
end;

procedure TCellElementSegment.Store(Stream: TCompressionStream);
begin
  WriteCompInt(Stream, FCol);
  Stream.Write(FEndPosition, SizeOf(FEndPosition));
  WriteCompInt(Stream, FLayer);
  WriteCompInt(Stream, FVertexIndex);
  WriteCompInt(Stream, FRow);
  Stream.Write(FStartPosition, SizeOf(FStartPosition));
  WriteCompReal(Stream,FX1);
  WriteCompReal(Stream,FX2);
  WriteCompReal(Stream,FY1);
  WriteCompReal(Stream,FY2);
  WriteCompInt(Stream, FSectionIndex);
end;

{ TRealDataListItem }

procedure TRealDataListItem.Assign(Source: TPersistent);
begin
  if Source is TRealDataListItem then
  begin
    FValues := TRealDataListItem(Source).FValues;
    SetLength(FValues, System.Length(FValues));
  end;
  inherited;
end;

procedure TRealDataListItem.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('RealValues', ReadValues,
    WriteValues, (Length > 0));
end;

function TRealDataListItem.GetValuesLength: integer;
begin
  result := System.Length(FValues);
end;

function TRealDataListItem.GetValues(const Index: Integer): double;
begin
  Assert((Index >= 0) and (Index < Length));
  result := FValues[Index];
end;

procedure TRealDataListItem.ReadValues(Reader: TReader);
var
  Positions: TOneDRealArray;
begin
  ReadRealArray(Reader, Positions,
    (Collection as TDataListCollection).ScreenObject.Count);
  FValues := Positions;
end;

procedure TRealDataListItem.SetValuesLength(const Value: integer);
begin
  SetLength(FValues, Value)
end;

procedure TRealDataListItem.SetValues(const Index: Integer;
  const Value: double);
begin
  Assert((Index >= 0) and (Index < Length));
  FValues[Index] := Value;
end;

procedure TRealDataListItem.WriteValues(Writer: TWriter);
begin
  WriteRealArray(Writer, FValues);
end;

{ TIntegerDataListItem }

procedure TIntegerDataListItem.Assign(Source: TPersistent);
begin
  if Source is TIntegerDataListItem then
  begin
    FValues := TIntegerDataListItem(Source).FValues;
    SetLength(FValues, Length);
  end;
  inherited;
end;

procedure TIntegerDataListItem.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('IntegerValues', ReadValues,
    WriteValues, (Length > 0));
end;

function TIntegerDataListItem.GetValues(const Index: Integer): Integer;
begin
  Assert((Index >= 0) and (Index < Length));
  result := FValues[Index];
end;

function TIntegerDataListItem.GetValuesLength: integer;
begin
  result := System.Length(FValues);
end;

procedure TIntegerDataListItem.ReadValues(Reader: TReader);
var
  Positions: TOneDIntegerArray;
begin
  ReadIntegerArray(Reader, Positions,
    (Collection as TDataListCollection).ScreenObject.Count);
  FValues := Positions;
end;

procedure TIntegerDataListItem.SetValues(const Index, Value: Integer);
begin
  Assert((Index >= 0) and (Index < Length));
  FValues[Index] := Value;
end;

procedure TIntegerDataListItem.SetValuesLength(const Value: integer);
begin
  SetLength(FValues, Value);
end;

procedure TIntegerDataListItem.WriteValues(Writer: TWriter);
begin
  WriteIntegerArray(Writer, FValues);
end;

{ TDataListCollection }

constructor TDataListCollection.Create(ItemClass: TCollectionItemClass; const
  ScreenObject: TScreenObject; Model: TComponent);
begin
  inherited create(ItemClass, Model);
  FScreenObject := ScreenObject;
  PriorIndex := -1;
end;

function TDataListCollection.GetItemByName(
  ADataSetName: string): TCustomDataListItem;
var
  Index: integer;
  Item: TCustomDataListItem;
begin
  // If the function has already been called, see if the
  // result the previous time should still be the result
  // this time.
  if (PriorIndex >= 0) and (PriorIndex < Count) then
  begin
    Item := Items[PriorIndex] as TCustomDataListItem;
    if ADataSetName = UpperCase(Item.DataSetName) then
    begin
      result := Item;
      Exit;
    end;
  end;
  result := nil;
  ADataSetName := UpperCase(ADataSetName);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TCustomDataListItem;
    if ADataSetName = UpperCase(Item.DataSetName) then
    begin
      result := Item;
      // store the result for the next time the function
      // is called.
      PriorIndex := Index;
      Exit;
    end;
  end;
end;

{ TRealDataListCollection }

constructor TRealDataListCollection.Create(const ScreenObject: TScreenObject;
  Model: TComponent);
begin
  inherited Create(TRealDataListItem, ScreenObject, Model);
end;

{ TIntegerDataListCollection }

constructor TIntegerDataListCollection.Create(const ScreenObject:
  TScreenObject; Model: TComponent);
begin
  inherited Create(TIntegerDataListItem, ScreenObject, Model);
end;

{ TCustomDataListItem }

procedure TCustomDataListItem.Assign(Source: TPersistent);
begin
  if Source is TCustomDataListItem then
  begin
    with TCustomDataListItem(Source) do
    begin
      self.FirstCol := FirstCol;
      self.FirstRow := FirstRow;
      self.FirstLay := FirstLay;
      self.LastCol := LastCol;
      self.LastRow := LastRow;
      self.LastLay := LastLay;
      self.DataSetName := DataSetName;
    end;
  end
  else
  begin
    inherited;
  end;
end;

function TCustomDataListItem.ValueIndex(const Col, Row, Lay: integer): integer;
var
  RowCount: integer;
  ColCount: integer;
begin
  if (Col < FirstCol) or (Col > LastCol)
    or (Row < FirstRow) or (Row > LastRow)
    or (Lay < FirstLay) or (Lay > LastLay) then
  begin
    result := -1;
  end
  else
  begin
    RowCount := LastRow - FirstRow + 1;
    ColCount := LastCol - FirstCol + 1;
    result := (Lay - FirstLay) * RowCount * ColCount
      + (Row - FirstRow) * ColCount
      + (Col - FirstCol);
  end;
end;

{ TDelegateItem }

destructor TDelegateItem.Destroy;
begin
  FDelegate.Free;
  inherited;
end;

function TDelegateItem.GetDelegateClass: string;
begin
  if FDelegate = nil then
  begin
    result := '';
  end
  else
  begin
    result := FDelegate.ClassName;
  end;
end;

procedure TDelegateItem.SetDelegate(const Value: TCustomScreenObjectDelegate);
begin
  FDelegate.Assign(Value);
end;

function TDelegateItem.ScreenObject: TScreenObject;
begin
  result := (Collection as TDelegateCollection).FOwnerScreenObject;
end;

procedure TDelegateItem.SetDelegateClass(const Value: string);
var
  DelegateClass: TCustomScreenObjectDelegateClass;
begin
  FDelegate.Free;
  DelegateClass := TCustomScreenObjectDelegateClass(FindClass(Value));
  FDelegate := DelegateClass.Create(ScreenObject);
end;

{ TDelegateCollection }

constructor TDelegateCollection.Create(OwnerScreenObject: TScreenObject);
begin
  inherited Create(TDelegateItem);
  FOwnerScreenObject := OwnerScreenObject;
end;

function TDelegateCollection.Delegate(
  ModelSelection: TModelSelection): TCustomScreenObjectDelegate;
var
  Index: Integer;
  Item: TDelegateItem;
begin
  if (FCachedDelegate = nil) or
    (FCachedDelegate.ModelSelection <> ModelSelection) then
  begin
    FCachedDelegate := nil;
    for Index := 0 to Count - 1 do
    begin
      Item := Items[Index] as TDelegateItem;
      if (Item.Delegate <> nil) and
        (Item.Delegate.ModelSelection = ModelSelection) then
      begin
        FCachedDelegate := Item.Delegate;
        break;
      end;
    end;
    if FCachedDelegate = nil then
    begin
      Item := Add as TDelegateItem;
      case ModelSelection of
        msUndefined:
          begin
            Assert(False);
          end;
        msPhast:
          begin
            Item.DelegateClass := TPhastDelegate.ClassName;
          end;
        msModflow:
          begin
            Item.DelegateClass := TModflowDelegate.ClassName;
          end;
        else
          begin
            Assert(False);
          end;
      end;
      FCachedDelegate := Item.Delegate;
    end;
  end;
  result := FCachedDelegate;
end;

{ TCustomScreenObjectDelegate }

procedure TCustomScreenObjectDelegate.AssignCellValue(
  const UsedVariables: TStringList; const DataSet: TDataArray; LayerIndex,
  RowIndex, ColIndex: integer; const Compiler: TRbwParser;
  const Annotation: string; var Expression: TExpression;
  const OtherData: TObject; SectionIndex: integer);
begin
  case DataSet.Orientation of
    dsoTop:
      begin
        LayerIndex := 0;
      end;
    dsoFront:
      begin
        RowIndex := 0;
      end;
    dsoSide:
      begin
        ColIndex := 0;
      end;
    dso3D:
      begin
        // do nothing.
      end;
    else Assert(False);
  end;
  UpdateVariables(UsedVariables, DataSet,
    LayerIndex, RowIndex, ColIndex, Compiler);
  UpdateCurrentSection(SectionIndex);

  FScreenObject.EvaluateDataArrayExpression(DataSet, Expression, Compiler);

  case DataSet.Datatype of
    rdtDouble:
      begin
        DataSet.RealData[LayerIndex, RowIndex, ColIndex] :=
          Expression.DoubleResult;
      end;
    rdtInteger:
      begin
        DataSet.IntegerData[LayerIndex, RowIndex, ColIndex] :=
          Expression.IntegerResult;
      end;
    rdtBoolean:
      begin
        DataSet.BooleanData[LayerIndex, RowIndex, ColIndex] :=
          Expression.BooleanResult;
      end;
    rdtString:
      begin
        DataSet.StringData[LayerIndex, RowIndex, ColIndex] :=
          Expression.StringResult;
      end;
  else
    Assert(False);
  end;
  DataSet.Annotation[LayerIndex, RowIndex, ColIndex] := Annotation;
end;

function TCustomScreenObjectDelegate.BoundaryType: integer;
begin
  result := 0;
end;

constructor TCustomScreenObjectDelegate.Create(ScreenObject: TScreenObject);
begin
  inherited Create;
  FScreenObject := ScreenObject;
  FModel := FScreenObject.FModel;
end;

function TCustomScreenObjectDelegate.DataSetUsed(const DataSet: TDataArray;
  var OtherData: TObject): boolean;
begin
  OtherData := nil;
  result := (FScreenObject.IndexOfDataSet(DataSet) >= 0)
    or (FScreenObject.IndexOfBoundaryDataSet(DataSet) >= 0)
    or (DataSet = (FModel as TPhastModel).TopBoundaryType)
    or (DataSet = (FModel as TPhastModel).FrontBoundaryType)
    or (DataSet = (FModel as TPhastModel).SideBoundaryType)
    or (DataSet = (FModel as TPhastModel).Top2DBoundaryType);
end;

function TCustomScreenObjectDelegate.EncloseAnnotation(
  const DataSetFormula: string; const OtherData: TObject): string;
begin
  if Length(DataSetFormula) > 1000 then
  begin
    result := 'Enclosed by ' + FScreenObject.Name + ' with a formula '
      + 'that is too long to show.';
  end
  else
  begin
    result := 'Enclosed by ' + FScreenObject.Name + ' with formula: '
      + DataSetFormula;
  end;
end;

function TCustomScreenObjectDelegate.GetCompiler(
  const Orientation: TDataSetOrientation): TRbwParser;
begin
  result := FScreenObject.GetCompiler(
    Orientation, FScreenObject.EvaluatedAt);
end;

procedure TCustomScreenObjectDelegate.InitializeExpression(
  out Compiler: TRbwParser; out DataSetFunction: string;
  out Expression: TExpression; const DataSet: TDataArray;
  const OtherData: TObject);
var
  ResultTypeOK: boolean;
  DI: integer;
  IsBoundary: boolean;
begin
  Compiler := GetCompiler(DataSet.Orientation);
  DI := FScreenObject.IndexOfDataSet(DataSet);
  if DI >= 0 then
  begin
    DataSetFunction := FScreenObject.DataSetFormulas[DI];
    IsBoundary := False;
  end
  else
  begin
    IsBoundary := True;
    DI := FScreenObject.IndexOfBoundaryDataSet(DataSet);
    Assert(DI >= 0);
    DataSetFunction := FScreenObject.BoundaryDataSetFormulas[DI];
  end;

  try
    Compiler.Compile(DataSetFunction);
  except on E: ERbwParserError do
    begin
      ResetScreenObjectFunction(DI, FScreenObject, Compiler,
        DataSet.DataType, E.Message, IsBoundary);
    end;
  end;
  Expression := Compiler.CurrentExpression;
  ResultTypeOK := (Expression.ResultType = DataSet.Datatype)
    or ((Expression.ResultType = rdtInteger)
    and (DataSet.Datatype = rdtDouble));
  if not ResultTypeOK then
  begin
    raise EInvalidDataType.Create('Invalid data type.');
  end;
end;

procedure TCustomScreenObjectDelegate.InitializeVariables(
  const UsedVariables: TStringList; const DataSet: TDataArray;
  const Expression: TExpression; const Compiler: TRbwParser);
var
  VarIndex: integer;
  VarName: string;
  VarPosition: integer;
  Variable: TCustomValue;
  AnotherDataSet: TDataArray;
  Model: TPhastModel;
begin
  if Expression = nil then
    Exit;
  Model := FModel as TPhastModel;
  UsedVariables.Assign(Expression.VariablesUsed);
  for VarIndex := 0 to UsedVariables.Count - 1 do
  begin
    VarName := UsedVariables[VarIndex];
    VarPosition := Compiler.IndexOfVariable(VarName);
    Variable := Compiler.Variables[VarPosition];
    AnotherDataSet := Model.GetDataSetByName(VarName);
    if AnotherDataSet <> nil then
    begin
      Assert(AnotherDataSet <> DataSet);
      Assert(AnotherDataSet.DataType = Variable.ResultType);
      AnotherDataSet.Initialize;
      Model.AddDataSetToCache(AnotherDataSet);
    end;
  end;
end;

function TCustomScreenObjectDelegate.IntersectAnnotation(
  const DataSetFormula: string; const OtherData: TObject): string;
begin
  if Length(DataSetFormula) > 1000 then
  begin
    result := 'Intersected by ' + FScreenObject.Name + ' with a formula '
      + 'that is too long to show.';
  end
  else
  begin
    result := 'Intersected by ' + FScreenObject.Name + ' with formula: '
      + DataSetFormula;
  end;
end;

procedure TCustomScreenObjectDelegate.OtherIndex(const LayerOrRow,
  RowOrColumn: integer; out First, Last: integer; const DataSet: TDataArray);
var
  Grid: TCustomGrid;
begin
  if (DataSet <> nil) and (DataSet.Orientation <> dso3D) then
  begin
    First := 0;
    Last := 0;
  end
  else
  begin
    case FScreenObject.ElevationCount of
      ecZero:
        begin
          First := 0;
          Last := 0;
        end;
      ecOne, ecTwo:
        begin
          First := 0;
          Grid := (FModel as TPhastModel).Grid;
          case FScreenObject.ViewDirection of
            vdTop:
              begin
                case FScreenObject.EvaluatedAt of
                  eaBlocks: Last := Grid.LayerCount-1;
                  eaNodes: Last := Grid.LayerCount;
                  else Assert(False);
                end;
              end;
            vdFront:
              begin
                case FScreenObject.EvaluatedAt of
                  eaBlocks: Last := Grid.RowCount-1;
                  eaNodes: Last := Grid.RowCount;
                  else Assert(False);
                end;
              end;
            vdSide:
              begin
                case FScreenObject.EvaluatedAt of
                  eaBlocks: Last := Grid.ColumnCount-1;
                  eaNodes: Last := Grid.ColumnCount;
                  else Assert(False);
                end;
              end;
            else Assert(False);
          end;
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TCustomScreenObjectDelegate.UpdateVariables(
  const UsedVariables: TStringList; const DataSet: TDataArray; Layer, Row,
  Column: integer; const Compiler: TRbwParser);
var
  VarIndex: integer;
  VarName: string;
  VarPosition: integer;
  Variable: TCustomValue;
//  DataSetIndex: integer;
  AnotherDataSet: TDataArray;
  Model: TPhastModel;
begin
  UpdateGlobalLocations(Column, Row, Layer, DataSet.EvaluatedAt);
  Model := FModel as TPhastModel;
  for VarIndex := 0 to UsedVariables.Count - 1 do
  begin
    VarName := UsedVariables[VarIndex];
    VarPosition := Compiler.IndexOfVariable(VarName);
    Variable := Compiler.Variables[VarPosition];
    AnotherDataSet := Model.GetDataSetByName(VarName);
    if AnotherDataSet <> nil then
    begin

//      AnotherDataSet := Model.DataSets[DataSetIndex];
      Assert(AnotherDataSet <> DataSet);
      Assert(AnotherDataSet.DataType = Variable.ResultType);
      if AnotherDataSet.Orientation = dsoTop then
      begin
        Layer := 0;
      end;
      if AnotherDataSet.Orientation = dsoFront then
      begin
        Row := 0;
      end;
      if AnotherDataSet.Orientation = dsoSide then
      begin
        Column := 0;
      end;
      case Variable.ResultType of
        rdtDouble:
          begin
            TRealVariable(Variable).Value :=
              AnotherDataSet.RealData[Layer, Row, Column];
          end;
        rdtInteger:
          begin
            TIntegerVariable(Variable).Value :=
              AnotherDataSet.IntegerData[Layer, Row, Column];
          end;
        rdtBoolean:
          begin
            TBooleanVariable(Variable).Value :=
              AnotherDataSet.BooleanData[Layer, Row, Column];
          end;
        rdtString:
          begin
            TStringVariable(Variable).Value :=
              AnotherDataSet.StringData[Layer, Row, Column];
          end;
      else
        Assert(False);
      end;
    end;
  end;
end;

{ TPhastDelegate }

function TPhastDelegate.ThreeDBoundaryFormula: string;
begin
  if PhastBoundaryType in  [btNone, btSpecifiedHead, btFlux, btLeaky] then
  begin
    result := IntToStr(BoundaryType);
  end
  else
  begin
    result := '0';
  end;
end;

function TPhastDelegate.TwoDBoundaryFormula: string;
begin
  if PhastBoundaryType in  [btNone, btRiver, btWell] then
  begin
    result := IntToStr(BoundaryType);
  end
  else
  begin
    result := '0';
  end;
end;

procedure TPhastDelegate.AssignCellValue(const UsedVariables: TStringList;
  const DataSet: TDataArray; LayerIndex, RowIndex, ColIndex: integer;
  const Compiler: TRbwParser; const Annotation: string;
  var Expression: TExpression; const OtherData: TObject;
  SectionIndex: integer);
var
  InterpValue: TInterpValuesItem;
begin
  case DataSet.Orientation of
    dsoTop:
      begin
        LayerIndex := 0;
      end;
    dsoFront:
      begin
        RowIndex := 0;
      end;
    dsoSide:
      begin
        ColIndex := 0;
      end;
    dso3D:
      begin
        // do nothing.
      end;
    else Assert(False);
  end;
  InterpValue := OtherData as TInterpValuesItem;
  if (InterpValue = nil) or not InterpValue.Values.UsePHAST_Interpolation then
  begin
    inherited;
    if (InterpValue <> nil) and (DataSet is TCustomPhastDataSet) then
    begin
      TCustomPhastDataSet(DataSet).IsInterpolatedCell[
        LayerIndex, RowIndex, ColIndex] := False;
    end
  end
  else
  begin
    UpdateGlobalLocations(ColIndex, RowIndex, LayerIndex, DataSet.EvaluatedAt);
    UpdateCurrentSection(SectionIndex);

    case DataSet.Datatype of
      rdtDouble:
        begin
          AssignRealDataWithPhastInterpolation(DataSet,
            LayerIndex, RowIndex, ColIndex, Annotation, InterpValue);
        end;
      rdtInteger:
        begin
          AssignIntegerDataWithPhastInterpolation(DataSet,
            LayerIndex, RowIndex, ColIndex, Annotation, InterpValue);
        end;
      rdtBoolean:
        begin
          Assert(False);
        end;
      rdtString:
        begin
          Assert(False);
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TPhastDelegate.AssignIntegerDataWithPhastInterpolation(
  const DataSet: TDataArray;
  const LayerIndex, RowIndex, ColIndex: integer; const Comment: string;
  const InterpValue: TInterpValuesItem);
var
  Distance, Fraction: double;
  RealValue: double;
  Model: TPhastModel;
begin
  Model:= FModel as TPhastModel;
  if DataSet is TIntegerPhastDataSet then
  begin
    with TIntegerPhastDataSet(DataSet) do
    begin
      CellDistance1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance1;
      CellDistance2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance2;
      CellInterpolationDirection[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.InterpolationDirection;
      IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.UsePHAST_Interpolation;
      Annotation[LayerIndex, RowIndex, ColIndex] := Comment;
      CellValue1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.IntValue1;
      CellValue2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.IntValue2;
    end;
  end
  else if DataSet is TSparseIntegerPhastDataSet then
  begin
    with TSparseIntegerPhastDataSet(DataSet) do
    begin
      CellDistance1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance1;
      CellDistance2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance2;
      CellInterpolationDirection[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.InterpolationDirection;
      IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.UsePHAST_Interpolation;
      Annotation[LayerIndex, RowIndex, ColIndex] := Comment;
      CellValue1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.IntValue1;
      CellValue2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.IntValue2;
    end;
  end
  else
  begin
    Assert(false);
  end;

  Distance := 0;
  case InterpValue.Values.InterpolationDirection of
    pidX:
      begin
        case FScreenObject.EvaluatedAt of
          eaBlocks:
            begin
              Distance := (Model.PhastGrid.ColumnPosition[ColIndex]
                + Model.PhastGrid.ColumnPosition[ColIndex + 1]) / 2;
            end;
          eaNodes:
            begin
              Distance := Model.PhastGrid.ColumnPosition[ColIndex];
            end;
        else
          Assert(False);
        end;
      end;
    pidY:
      begin
        case FScreenObject.EvaluatedAt of
          eaBlocks:
            begin
              Distance := (Model.PhastGrid.RowPosition[RowIndex]
                + Model.PhastGrid.RowPosition[RowIndex + 1]) / 2;
            end;
          eaNodes:
            begin
              Distance := Model.PhastGrid.RowPosition[RowIndex];
            end;
        else
          Assert(False);
        end;
      end;
    pidZ:
      begin
        case FScreenObject.EvaluatedAt of
          eaBlocks:
            begin
              Distance := (Model.PhastGrid.LayerElevation[LayerIndex]
                + Model.PhastGrid.LayerElevation[LayerIndex + 1]) / 2;
            end;
          eaNodes:
            begin
              Distance := Model.PhastGrid.LayerElevation[LayerIndex];
            end;
        else
          Assert(False);
        end;
      end;
    pidMix:
      begin
        UpdateVariables(FMixtureVariables, DataSet,
          LayerIndex, RowIndex, ColIndex, FMixtureCompiler);

        FMixtureExpression.Evaluate;

        Distance := 1 - FMixtureExpression.DoubleResult;
      end;
    else Assert(False);
  end;
  if Distance <= InterpValue.Values.Distance1 then
  begin
    DataSet.IntegerData[LayerIndex, RowIndex, ColIndex] :=
      InterpValue.Values.IntValue1;
    Fraction := 1;
  end
  else if Distance >= InterpValue.Values.Distance2 then
  begin
    DataSet.IntegerData[LayerIndex, RowIndex, ColIndex] :=
      InterpValue.Values.IntValue2;
    Fraction := 0;
  end
  else
  begin
    Fraction := 1 - (Distance - InterpValue.Values.Distance1) /
      (InterpValue.Values.Distance2 - InterpValue.Values.Distance1);
    RealValue := Fraction * InterpValue.Values.IntValue1 + (1 - Fraction) *
      InterpValue.Values.IntValue2;
    DataSet.IntegerData[LayerIndex, RowIndex, ColIndex] :=
      Round(RealValue);
  end;
  // Fraction is needed in all cases in order to read
  // TSparseIntegerPhastInterpolationDataSet.RealValue
  // or TIntegerPhastDataSet.RealValue
  if DataSet is TIntegerPhastDataSet then
  begin
    TIntegerPhastDataSet(DataSet).Fraction[
      LayerIndex, RowIndex, ColIndex] := Fraction;
  end
  else if DataSet is TSparseIntegerPhastDataSet then
  begin
    TSparseIntegerPhastDataSet(DataSet).Fraction[
      LayerIndex, RowIndex, ColIndex] := Fraction
  end;
end;

procedure TPhastDelegate.AssignRealDataWithPhastInterpolation(
  const DataSet: TDataArray;
  const LayerIndex, RowIndex, ColIndex: integer; const Comment: string;
  const InterpValue: TInterpValuesItem);
var
  Distance, Fraction: double;
  Model: TPhastModel;
begin
  Model := FModel as TPhastModel;
  if DataSet is TRealPhastDataSet then
  begin
    with TRealPhastDataSet(DataSet) do
    begin
      CellDistance1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance1;
      CellDistance2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance2;
      CellInterpolationDirection[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.InterpolationDirection;
      IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.UsePHAST_Interpolation;
      Annotation[LayerIndex, RowIndex, ColIndex] := Comment;
      //'Set by PHAST-style interpolation: ' + Name;
      CellValue1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.RealValue1;
      CellValue2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.RealValue2;
    end;
  end
  else if DataSet is TSparseRealPhastDataSet then
  begin
    with TSparseRealPhastDataSet(DataSet) do
    begin
      CellDistance1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance1;
      CellDistance2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance2;
      CellInterpolationDirection[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.InterpolationDirection;
      IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.UsePHAST_Interpolation;
      Annotation[LayerIndex, RowIndex, ColIndex] := Comment;
      //'Set by PHAST-style interpolation: ' + Name;
      CellValue1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.RealValue1;
      CellValue2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.RealValue2;
    end;
  end
  else
  begin
    Assert(False);
  end;

  Distance := 0;
  case InterpValue.Values.InterpolationDirection of
    pidX:
      begin
        case FScreenObject.EvaluatedAt of
          eaBlocks:
            begin
              Distance := (Model.PhastGrid.ColumnPosition[ColIndex]
                + Model.PhastGrid.ColumnPosition[ColIndex + 1]) / 2;
            end;
          eaNodes:
            begin
              Distance := Model.PhastGrid.ColumnPosition[ColIndex];
            end;
        else
          Assert(False);
        end;
      end;
    pidY:
      begin
        case FScreenObject.EvaluatedAt of
          eaBlocks:
            begin
              Distance := (Model.PhastGrid.RowPosition[RowIndex]
                + Model.PhastGrid.RowPosition[RowIndex + 1]) / 2;
            end;
          eaNodes:
            begin
              Distance := Model.PhastGrid.RowPosition[RowIndex];
            end;
        else
          Assert(False);
        end;
      end;
    pidZ:
      begin
        case FScreenObject.EvaluatedAt of
          eaBlocks:
            begin
              Distance := (Model.PhastGrid.LayerElevation[LayerIndex]
                + Model.PhastGrid.LayerElevation[LayerIndex + 1]) / 2;
            end;
          eaNodes:
            begin
              Distance := Model.PhastGrid.LayerElevation[LayerIndex];
            end;
        else
          Assert(False);
        end;
      end;
  else
    Assert(False);
  end;
  if Distance <= InterpValue.Values.Distance1 then
  begin
    DataSet.RealData[LayerIndex, RowIndex, ColIndex] :=
      InterpValue.Values.RealValue1;
  end
  else if Distance >= InterpValue.Values.Distance2 then
  begin
    DataSet.RealData[LayerIndex, RowIndex, ColIndex] :=
      InterpValue.Values.RealValue2;
  end
  else
  begin
    Fraction := 1 - (Distance - InterpValue.Values.Distance1) /
      (InterpValue.Values.Distance2 - InterpValue.Values.Distance1);
    DataSet.RealData[LayerIndex, RowIndex, ColIndex] :=
      Fraction * InterpValue.Values.RealValue1 + (1 - Fraction) *
      InterpValue.Values.RealValue2;
  end;
end;

procedure TPhastDelegate.AssignSelectedCells(const Grid: TCustomGrid);
var
  CellList: TCellAssignmentList;
  Index: Integer;
  Cell: TCellAssignment;
begin
  CellList := TCellAssignmentList.Create;
  try
    case FScreenObject.EvaluatedAt of
      eaBlocks:
        begin
          FSelectedCells.SetExtents(Grid.LayerCount,
            Grid.RowCount, Grid.ColumnCount);
        end;
      eaNodes:
        begin
          FSelectedCells.SetExtents(Grid.LayerCount+1,
            Grid.RowCount+1, Grid.ColumnCount+1);
        end;
      else Assert(False);
    end;
    GetCellsToAssign(Grid, '', nil, FScreenObject.EvaluatedAt, CellList, alAll);
    for Index := 0 to CellList.Count - 1 do
    begin
      Cell := CellList[Index];
      FSelectedCells[Cell.Layer, Cell.Row, Cell.Column] := True;
    end;

  finally
    CellList.Free;
  end;
end;

procedure TPhastDelegate.AssignValuesToFrontDataSet(const Grid: TCustomGrid;
  const DataSet: TDataArray; OtherData: TObject;
  AssignmentLocation: TAssignmentLocation = alAll);
var
  DataSetFunction: string;
  Compiler: TRbwParser;
  Expression: TExpression;
  UsedVariables: TStringList;
  CellList: TCellAssignmentList;
  CellAssignment: TCellAssignment;
  AssignmentIndex: Integer;
begin
  CellList := TCellAssignmentList.Create;
  try
    Assert(DataSet <> nil);
    Assert(AssignmentLocation = alAll);
    if not (DataSet.Orientation in [dsoFront, dso3D]) then
    begin
      FScreenObject.RemoveDataSet(DataSet);
      Exit;
    end;

    if DataSetUsed(DataSet, OtherData) then
    begin
      UsedVariables := TStringList.Create;
      try
        InitializeExpression(Compiler, DataSetFunction, Expression, DataSet,
          OtherData);

        InitializeVariables(UsedVariables, DataSet, Expression, Compiler);
        GetFrontCellsToAssign(Grid, DataSetFunction, OtherData, DataSet,
          CellList, alAll);
        FScreenObject.UpdateImportedValues(DataSet);
        for AssignmentIndex := 0 to CellList.Count - 1 do
        begin
          CellAssignment := CellList[AssignmentIndex];
          UpdateCurrentSegment(CellAssignment.Segment);
          UpdateCurrentSection(CellAssignment.Section);
          AssignCellValue(UsedVariables, DataSet, CellAssignment.Layer,
            CellAssignment.Row, CellAssignment.Column, Compiler,
            CellAssignment.Annotation, Expression, OtherData,
            CellAssignment.Section);
        end;
        (FModel as TPhastModel).CacheDataArrays;
      finally
        UsedVariables.Free;
      end;
    end;
  finally
    CellList.Free;
  end;
end;

procedure TPhastDelegate.AssignValuesToSideDataSet(const Grid: TCustomGrid;
  const DataSet: TDataArray; OtherData: TObject;
  AssignmentLocation: TAssignmentLocation = alAll);
var
  DataSetFunction: string;
  Compiler: TRbwParser;
  Expression: TExpression;
  UsedVariables: TStringList;
  CellList: TCellAssignmentList;
  CellAssignment: TCellAssignment;
  AssignmentIndex: Integer;
begin
  CellList := TCellAssignmentList.Create;
  try
    Assert(DataSet <> nil);
    Assert(AssignmentLocation = alAll);
    if not (DataSet.Orientation in [dsoSide, dso3D]) then
    begin
      FScreenObject.RemoveDataSet(DataSet);
      Exit;
    end;
    if DataSetUsed(DataSet, OtherData) then
    begin
      UsedVariables := TStringList.Create;
      try
        InitializeExpression(Compiler, DataSetFunction, Expression, DataSet,
          OtherData);

        InitializeVariables(UsedVariables, DataSet, Expression, Compiler);
        GetSideCellsToAssign(Grid, DataSetFunction, OtherData, DataSet,
          CellList, alAll);
        FScreenObject.UpdateImportedValues(DataSet);
        for AssignmentIndex := 0 to CellList.Count - 1 do
        begin
          CellAssignment := CellList[AssignmentIndex];
          UpdateCurrentSegment(CellAssignment.Segment);
          UpdateCurrentSection(CellAssignment.Section);
          AssignCellValue(UsedVariables, DataSet, CellAssignment.Layer,
            CellAssignment.Row, CellAssignment.Column, Compiler,
            CellAssignment.Annotation, Expression, OtherData,
            CellAssignment.Section);
        end;
      finally
        UsedVariables.Free;
        (FModel as TPhastModel).CacheDataArrays;
      end;
    end;
  finally
    CellList.Free;
  end;
end;

procedure TPhastDelegate.AssignValuesToTopDataSet(const Grid: TCustomGrid;
  const DataSet: TDataArray; OtherData: TObject;
  AssignmentLocation: TAssignmentLocation = alAll);
var
  DataSetFunction: string;
  Compiler: TRbwParser;
  Expression: TExpression;
  UsedVariables: TStringList;
  CellList: TCellAssignmentList;
  CellAssignment: TCellAssignment;
  AssignmentIndex: Integer;
begin
  CellList := TCellAssignmentList.Create;
  try
    Assert(DataSet <> nil);
    Assert(AssignmentLocation = alAll);
    if not (DataSet.Orientation in [dsoTop, dso3D]) then
    begin
      FScreenObject.RemoveDataSet(DataSet);
      Exit;
    end;
    if DataSetUsed(DataSet, OtherData) then
    begin
      UsedVariables := TStringList.Create;
      try
        InitializeExpression(Compiler, DataSetFunction, Expression, DataSet,
          OtherData);

        InitializeVariables(UsedVariables, DataSet, Expression, Compiler);
        GetTopCellsToAssign(Grid, DataSetFunction, OtherData, DataSet,
          CellList, alAll);
        FScreenObject.UpdateImportedValues(DataSet);
        for AssignmentIndex := 0 to CellList.Count - 1 do
        begin
          CellAssignment := CellList[AssignmentIndex];
          UpdateCurrentSegment(CellAssignment.Segment);
          UpdateCurrentSection(CellAssignment.Section);
          AssignCellValue(UsedVariables, DataSet, CellAssignment.Layer,
            CellAssignment.Row, CellAssignment.Column, Compiler,
            CellAssignment.Annotation, Expression, OtherData,
            CellAssignment.Section);
        end;
      finally
        UsedVariables.Free;
        (FModel as TPhastModel).CacheDataArrays;
      end;
    end;
  finally
    CellList.Free;
  end;
end;

function TPhastDelegate.BoundaryType: integer;
begin
  result := Ord(PhastBoundaryType);
end;

constructor TPhastDelegate.Create(ScreenObject: TScreenObject);
begin
  inherited;
  FModelSelection := msPhast;
  FSelectedCells := TSelectedCells.Create;
end;

procedure TPhastDelegate.GetCellsToAssign(const Grid: TCustomGrid;
  const DataSetFunction: string; OtherData: TObject;
  const EvaluatedAt: TEvaluatedAt; CellList: TCellAssignmentList;
  AssignmentLocation: TAssignmentLocation);
begin
  Assert(CellList.Count = 0);
  if (Grid.ColumnCount <= 0) or (Grid.RowCount <= 0)
    or (Grid.LayerCount <= 0) then
  begin
    Exit;
  end;
  case FScreenObject.ViewDirection of
    vdTop: GetTopCellsToAssign(Grid, DataSetFunction, OtherData,
      nil, CellList, AssignmentLocation);
    vdFront: GetFrontCellsToAssign(Grid, DataSetFunction, OtherData,
      nil, CellList, AssignmentLocation);
    vdSide: GetSideCellsToAssign(Grid, DataSetFunction, OtherData,
      nil, CellList, AssignmentLocation);
    else Assert(False);
  end;
end;

procedure TPhastDelegate.GetFrontCellsToAssign(const Grid: TCustomGrid;
  const DataSetFunction: string; OtherData: TObject;
  const DataSet: TDataArray; CellList: TCellAssignmentList;
  AssignmentLocation: TAssignmentLocation);
var
  TempMaxY: Real;
  TempMaxX: Real;
  TempMinY: Real;
  TempMinX: Real;
  RowLimit: Integer;
  EAnnotation: string;
  IAnnotation: string;
  ASegment: TCellElementSegment;
  SegmentIndex: Integer;
  PriorLayer: Integer;
  PriorCol: Integer;
  Annotation: string;
  ElevationIndex: Integer;
  LastElevationIndex: Integer;
  FirstElevationIndex: Integer;
  SectionIndex: Integer;
  CellLocation3D: T3DRealPoint;
  LayerIndex: Integer;
  ColIndex: Integer;
  LastLayer: Integer;
  FirstLayer: Integer;
  LastCol: Integer;
  FirstCol: Integer;
  LocalGrid: TPhastGrid;
  LowerBound: Double;
  UpperBound: Double;
  Middle: Double;
  EvalAt: TEvaluatedAt;
  Orientation: TDataSetOrientation;
  AssignmentMethod: TAssignmentMethod;
begin
  Assert(CellList.Count = 0);
  LocalGrid := Grid as TPhastGrid;
  IAnnotation := IntersectAnnotation(DataSetFunction, OtherData);
  EAnnotation := EncloseAnnotation(DataSetFunction, OtherData);
  UpdateCurrentScreenObject(FScreenObject);
  RowLimit := -1;
  if DataSet = nil then
  begin
    EvalAt := FScreenObject.EvaluatedAt;
    Orientation := dso3D;
  end
  else
  begin
    EvalAt := DataSet.EvaluatedAt;
    Orientation := DataSet.Orientation;
  end;
  if FScreenObject.RestoreCellsFromCache(CellList, EvalAt,
    Orientation, AssignmentLocation, EAnnotation, IAnnotation) then
  begin
    Exit;
  end;
  try
    case EvalAt of
      eaBlocks:
        begin
          RowLimit := Grid.RowCount - 1;
        end;
      eaNodes:
        begin
          RowLimit := Grid.RowCount;
        end;
    else
      Assert(False);
    end;
    if (FScreenObject.SetValuesOfEnclosedCells
      or (FScreenObject.SetValuesOfIntersectedCells
      and (FScreenObject.ElevationCount <> ecZero)))
      and FScreenObject.Closed then
    begin
      // Get the coordinates of the points.
      TempMinX := FScreenObject.MinX;
      TempMinY := FScreenObject.MinY;
      TempMaxX := FScreenObject.MaxX;
      TempMaxY := FScreenObject.MaxY;
      FScreenObject.GetColumns(Grid, TempMinX, TempMaxX, FirstCol, LastCol);
      if FirstCol > Grid.ColumnCount then
      begin
        FirstCol := Grid.ColumnCount;
      end;
      if LastCol > Grid.ColumnCount then
      begin
        LastCol := Grid.ColumnCount;
      end;
      FScreenObject.GetLayers(Grid, TempMinY, TempMaxY, FirstLayer, LastLayer);
      if FirstLayer > Grid.LayerCount then
      begin
        FirstLayer := Grid.LayerCount;
      end;
      if LastLayer > Grid.LayerCount then
      begin
        LastLayer := Grid.LayerCount;
      end;
      // Find the cells inside the screen object and assign values to them.
      for ColIndex := FirstCol to LastCol do
      begin
        for LayerIndex := FirstLayer to LastLayer do
        begin
          case FScreenObject.EvaluatedAt of
            eaBlocks:
              begin
                CellLocation3D := Grid.ThreeDElementCenter(
                  ColIndex, 0, LayerIndex);
              end;
            eaNodes:
              begin
                CellLocation3D := Grid.ThreeDElementCorner(
                   ColIndex, 0, LayerIndex);
              end;
          else
            Assert(False);
          end;
          if FScreenObject.IsPointInside(CellLocation3D.X,
            CellLocation3D.Z, SectionIndex) then
          begin
            OtherIndex(LayerIndex, ColIndex, FirstElevationIndex,
              LastElevationIndex, DataSet);
            if (FirstElevationIndex >= 0)
              and (LastElevationIndex <= RowLimit) then
            begin
              for ElevationIndex := FirstElevationIndex to LastElevationIndex do
              begin
                case FScreenObject.ElevationCount of
                  ecZero: ; // do nothing
                  ecOne:
                    begin
                      FScreenObject.FTopElevation :=
                        FScreenObject.Higher3DElevations[
                          LayerIndex, ElevationIndex, ColIndex];
                      FScreenObject.FBottomElevation :=
                        FScreenObject.TopElevation
                    end;
                  ecTwo:
                    begin
                      FScreenObject.FTopElevation :=
                        FScreenObject.Higher3DElevations[
                          LayerIndex, ElevationIndex, ColIndex];
                      FScreenObject.FBottomElevation :=
                        FScreenObject.Lower3DElevations[
                          LayerIndex, ElevationIndex, ColIndex];
                    end;
                  else Assert(False);
                end;

                if FScreenObject.SetValuesOfIntersectedCells
                  and (FScreenObject.ElevationCount <> ecZero)
                  and ((ElevationIndex = FirstElevationIndex)
                  or (ElevationIndex = LastElevationIndex)) then
                begin
                  AssignmentMethod := amIntersect;
                  if IAnnotation = '' then
                  begin
                    IAnnotation := IntersectAnnotation(DataSetFunction,
                      OtherData);
                  end;
                  Annotation := IAnnotation;
                end
                else
                begin
                  AssignmentMethod := amEnclose;
                  if EAnnotation = '' then
                  begin
                    EAnnotation := EncloseAnnotation(DataSetFunction, OtherData);
                  end;
                  Annotation := EAnnotation;
                end;
                UpdateCurrentSection(SectionIndex);
                LocalGrid.BlockExtents(vdFront, FScreenObject.EvaluatedAt,
                  ElevationIndex, LowerBound, UpperBound);
                case FScreenObject.ElevationCount of
                  ecZero:
                    begin
                      CellList.Add(TCellAssignment.Create(LayerIndex,
                        ElevationIndex, ColIndex, nil, SectionIndex, Annotation,
                        AssignmentMethod));
                    end;
                  ecOne:
                    begin
                      if (Orientation = dsoFront) or
                        ((FScreenObject.TopElevation >= LowerBound)
                        and ((FScreenObject.BottomElevation < UpperBound)
                        or ((FScreenObject.BottomElevation = UpperBound)
                        and (ElevationIndex = RowLimit)))) then
                      begin
                        CellList.Add(TCellAssignment.Create(LayerIndex,
                          ElevationIndex, ColIndex, nil,
                          SectionIndex, Annotation, AssignmentMethod));
                      end;
                    end;
                  ecTwo:
                    begin
                      if not FScreenObject.SetValuesOfEnclosedCells then
                      begin
                        if (Orientation = dsoFront)
                          or ((UpperBound >= FScreenObject.TopElevation)
                          and (LowerBound <= FScreenObject.TopElevation))
                          or ((UpperBound >= FScreenObject.BottomElevation)
                          and (LowerBound <= FScreenObject.BottomElevation)) then
                        begin
                          CellList.Add(TCellAssignment.Create(LayerIndex,
                            ElevationIndex, ColIndex, nil, SectionIndex,
                            Annotation, AssignmentMethod));
                        end;
                      end
                      else
                      begin
                        case FScreenObject.EvaluatedAt of
                          eaBlocks:
                            begin
                              Middle := (LowerBound + UpperBound)/2;
                              if (Orientation = dsoFront) or
                                ((FScreenObject.TopElevation >= Middle)
                                and (FScreenObject.BottomElevation
                                <= Middle)) then
                              begin
                                CellList.Add(TCellAssignment.Create(LayerIndex,
                                  ElevationIndex, ColIndex, nil, SectionIndex,
                                  Annotation, AssignmentMethod));
                              end;
                            end;
                          eaNodes:
                            begin
                              if (Orientation = dsoFront) or
                                ((FScreenObject.TopElevation >= LowerBound)
                                and (FScreenObject.BottomElevation
                                <= UpperBound)) then
                              begin
                                CellList.Add(TCellAssignment.Create(LayerIndex,
                                  ElevationIndex, ColIndex, nil,
                                  SectionIndex, Annotation, AssignmentMethod));
                              end;
                            end;
                          else Assert(False);
                        end;
                      end;
                    end;
                  else Assert(False);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    if FScreenObject.SetValuesOfIntersectedCells then
    begin
      if not FScreenObject.Segments.UpToDate then
      begin
        UpdateFrontSegments(Grid, DataSet.EvaluatedAt);
      end;
      // Assign values here.
      PriorCol := -1;
      PriorLayer := -1;
      FirstElevationIndex := 0;
      LastElevationIndex := -1;
      for SegmentIndex := 0 to FScreenObject.Segments.Count - 1 do
      begin
        ASegment := FScreenObject.Segments[SegmentIndex];
        if (ASegment.Col <> PriorCol) or (ASegment.Layer <> PriorLayer) then
        begin
          UpdateCurrentSegment(ASegment);
          OtherIndex(ASegment.Layer, ASegment.Col, FirstElevationIndex,
            LastElevationIndex, DataSet);
          PriorCol := ASegment.Col;
          PriorLayer := ASegment.Layer;
        end;
        if (FirstElevationIndex >= 0) and (LastElevationIndex <= RowLimit)
          and (ASegment.Row >= FirstElevationIndex)
          and (ASegment.Row <= LastElevationIndex) then
        begin
          UpdateCurrentSegment(ASegment);
          if IAnnotation = '' then
          begin
            IAnnotation := IntersectAnnotation(DataSetFunction, OtherData);
          end;
          Annotation := IAnnotation;
          LocalGrid.BlockExtents(vdFront, FScreenObject.EvaluatedAt,
            ASegment.Row, LowerBound, UpperBound);
          case FScreenObject.ElevationCount of
            ecZero: ; // do nothing
            ecOne:
              begin
                FScreenObject.FTopElevation :=
                  FScreenObject.Higher3DElevations[
                    ASegment.Layer, ASegment.Row, ASegment.Col];
                FScreenObject.FBottomElevation :=
                  FScreenObject.TopElevation
              end;
            ecTwo:
              begin
                FScreenObject.FTopElevation :=
                  FScreenObject.Higher3DElevations[
                    ASegment.Layer, ASegment.Row, ASegment.Col];
                FScreenObject.FBottomElevation :=
                  FScreenObject.Lower3DElevations[
                    ASegment.Layer, ASegment.Row, ASegment.Col];
              end;
            else Assert(False);
          end;
          case FScreenObject.ElevationCount of
            ecZero:
              begin
                CellList.Add(TCellAssignment.Create(ASegment.Layer,
                  ASegment.Row, ASegment.Col, ASegment,
                  ASegment.SectionIndex, Annotation, amIntersect));
              end;
            ecOne:
              begin
                if (Orientation = dsoFront) or
                  ((FScreenObject.TopElevation >= LowerBound)
                  and ((FScreenObject.BottomElevation < UpperBound)
                  or ((FScreenObject.BottomElevation = UpperBound)
                  and (ASegment.Row = RowLimit)))) then
                begin
                  CellList.Add(TCellAssignment.Create(ASegment.Layer,
                    ASegment.Row, ASegment.Col, ASegment,
                    ASegment.SectionIndex, Annotation, amIntersect));
                end;
              end;
            ecTwo:
              begin
                case FScreenObject.EvaluatedAt of
                  eaBlocks:
                    begin
                      Middle := (LowerBound + UpperBound)/ 2;
                      if (Orientation = dsoFront) or
                        ((FScreenObject.TopElevation >= Middle)
                        and (FScreenObject.BottomElevation <= Middle)) then
                      begin
                        CellList.Add(TCellAssignment.Create(ASegment.Layer,
                          ASegment.Row, ASegment.Col, ASegment,
                          ASegment.SectionIndex, Annotation, amIntersect));
                      end;
                    end;
                  eaNodes:
                    begin
                      if (Orientation = dsoFront) or
                        ((FScreenObject.TopElevation >= LowerBound)
                        and (FScreenObject.BottomElevation <= UpperBound)) then
                      begin
                        CellList.Add(TCellAssignment.Create(ASegment.Layer,
                          ASegment.Row, ASegment.Col, ASegment,
                          ASegment.SectionIndex, Annotation, amIntersect));
                      end;
                    end;
                  else Assert(False);
                end;
              end;
            else Assert(False);
          end;
        end;
      end;
    end;
  finally
    FScreenObject.UpdateCellCache(CellList, EvalAt,
    Orientation, AssignmentLocation);
  end;
end;

procedure TPhastDelegate.GetSideCellsToAssign(const Grid: TCustomGrid;
  const DataSetFunction: string; OtherData: TObject; const DataSet: TDataArray;
  CellList: TCellAssignmentList; AssignmentLocation: TAssignmentLocation);
var
  ASegment: TCellElementSegment;
  SegmentIndex: Integer;
  PriorLayer: Integer;
  PriorRow: Integer;
  Annotation: string;
  ElevationIndex: Integer;
  LastElevationIndex: Integer;
  FirstElevationIndex: Integer;
  SectionIndex: Integer;
  CellLocation3D: T3DRealPoint;
  LayerIndex: Integer;
  RowIndex: Integer;
  LastLayer: Integer;
  FirstLayer: Integer;
  LastRow: Integer;
  FirstRow: Integer;
  TempMaxY: Real;
  TempMaxX: Real;
  TempMinY: Real;
  TempMinX: Real;
  ColumnLimit: Integer;
  EAnnotation: string;
  IAnnotation: string;
  LocalGrid: TPhastGrid;
  LowerBound: Double;
  UpperBound: Double;
  Middle: Double;
  EvalAt: TEvaluatedAt;
  Orientation: TDataSetOrientation;
  AssignmentMethod: TAssignmentMethod;
begin
  Assert(CellList.Count = 0);
  LocalGrid := Grid as TPhastGrid;
  IAnnotation := IntersectAnnotation(DataSetFunction, OtherData);
  EAnnotation := EncloseAnnotation(DataSetFunction, OtherData);
  UpdateCurrentScreenObject(FScreenObject);
  ColumnLimit := -1;
  if DataSet = nil then
  begin
    EvalAt := FScreenObject.EvaluatedAt;
    Orientation := dso3D;
  end
  else
  begin
    EvalAt := DataSet.EvaluatedAt;
    Orientation := DataSet.Orientation;
  end;
  if FScreenObject.RestoreCellsFromCache(CellList, EvalAt,
    Orientation, AssignmentLocation, EAnnotation, IAnnotation) then
  begin
    Exit;
  end;
  try
    case EvalAt of
      eaBlocks:
        begin
          ColumnLimit := Grid.ColumnCount - 1;
        end;
      eaNodes:
        begin
          ColumnLimit := Grid.ColumnCount;
        end;
    else
      Assert(False);
    end;
    if (FScreenObject.SetValuesOfEnclosedCells
      or (FScreenObject.SetValuesOfIntersectedCells
      and (FScreenObject.ElevationCount <> ecZero)))
      and FScreenObject.Closed then
    begin
      // Get the coordinates of the points after rotating them to the grid
      // coordinate system.
      // Even though the last point and first point are identical,
      // the last point still has to be transformed for use with
      // the intersected points.
      TempMinX := FScreenObject.MinY;
      TempMinY := FScreenObject.MinX;
      TempMaxX := FScreenObject.MaxY;
      TempMaxY := FScreenObject.MaxX;
      FScreenObject.GetRows(Grid, TempMinX, TempMaxX, FirstRow, LastRow);
      if FirstRow > Grid.RowCount then
      begin
        FirstRow := Grid.RowCount;
      end;
      if LastRow > Grid.RowCount then
      begin
        LastRow := Grid.RowCount;
      end;
      FScreenObject.GetLayers(Grid, TempMinY, TempMaxY, FirstLayer, LastLayer);
      if FirstLayer > Grid.LayerCount then
      begin
        FirstLayer := Grid.LayerCount;
      end;
      if LastLayer > Grid.LayerCount then
      begin
        LastLayer := Grid.LayerCount;
      end;
      //
      // Find the cells inside the screen object and color them.
      for RowIndex := FirstRow to LastRow do
      begin
        for LayerIndex := FirstLayer to LastLayer do
        begin
          case FScreenObject.EvaluatedAt of
            eaBlocks:
              begin
                CellLocation3D := Grid.ThreeDElementCenter(
                  0, RowIndex, LayerIndex);
              end;
            eaNodes:
              begin
                CellLocation3D := Grid.ThreeDElementCorner(
                  0, RowIndex, LayerIndex);
              end;
          else
            Assert(False);
          end;
          if FScreenObject.IsPointInside(
            CellLocation3D.Z, CellLocation3D.Y, SectionIndex) then
          begin
            OtherIndex(LayerIndex, RowIndex, FirstElevationIndex,
              LastElevationIndex, DataSet);
            if (FirstElevationIndex >= 0)
              and (LastElevationIndex <= ColumnLimit) then
            begin
              for ElevationIndex := FirstElevationIndex to LastElevationIndex do
              begin
                case FScreenObject.ElevationCount of
                  ecZero: ; // do nothing
                  ecOne:
                    begin
                      FScreenObject.FTopElevation :=
                        FScreenObject.Higher3DElevations[
                          LayerIndex, RowIndex, ElevationIndex];
                      FScreenObject.FBottomElevation :=
                        FScreenObject.TopElevation
                    end;
                  ecTwo:
                    begin
                      FScreenObject.FTopElevation :=
                        FScreenObject.Higher3DElevations[
                          LayerIndex, RowIndex, ElevationIndex];
                      FScreenObject.FBottomElevation :=
                        FScreenObject.Lower3DElevations[
                          LayerIndex, RowIndex, ElevationIndex];
                    end;
                  else Assert(False);
                end;

                if FScreenObject.SetValuesOfIntersectedCells
                  and (FScreenObject.ElevationCount <> ecZero)
                  and ((ElevationIndex = FirstElevationIndex)
                  or (ElevationIndex = LastElevationIndex)) then
                begin
                  AssignmentMethod := amIntersect;
                  if IAnnotation = '' then
                  begin
                    IAnnotation := IntersectAnnotation(
                      DataSetFunction, OtherData);
                  end;
                  Annotation := IAnnotation;
                end
                else
                begin
                  AssignmentMethod := amEnclose;
                  if EAnnotation = '' then
                  begin
                    EAnnotation := EncloseAnnotation(DataSetFunction, OtherData);
                  end;
                  Annotation := EAnnotation;
                end;
                UpdateCurrentSection(SectionIndex);
                LocalGrid.BlockExtents(vdSide, FScreenObject.EvaluatedAt,
                  ElevationIndex, LowerBound, UpperBound);
                case FScreenObject.ElevationCount of
                  ecZero:
                    begin
                      CellList.Add(TCellAssignment.Create(LayerIndex, RowIndex,
                        ElevationIndex, nil, SectionIndex, Annotation,
                        AssignmentMethod));
                    end;
                  ecOne:
                    begin
                      if (Orientation = dsoSide) or
                        ((FScreenObject.TopElevation >= LowerBound)
                        and ((FScreenObject.BottomElevation < UpperBound)
                        or ((FScreenObject.BottomElevation = UpperBound)
                        and (ElevationIndex = ColumnLimit)))) then
                      begin
                        CellList.Add(TCellAssignment.Create(LayerIndex, RowIndex,
                          ElevationIndex, nil, SectionIndex, Annotation,
                          AssignmentMethod));
                      end;
                    end;
                  ecTwo:
                    begin
                      if not FScreenObject.SetValuesOfEnclosedCells then
                      begin
                        if (Orientation = dsoSide)
                          or ((UpperBound >= FScreenObject.TopElevation)
                          and (LowerBound <= FScreenObject.TopElevation))
                          or ((UpperBound >= FScreenObject.BottomElevation)
                          and (LowerBound <= FScreenObject.BottomElevation)) then
                        begin
                          CellList.Add(TCellAssignment.Create(LayerIndex,
                            RowIndex, ElevationIndex, nil,
                            SectionIndex, Annotation, AssignmentMethod));
                        end;
                      end
                      else
                      begin
                        case FScreenObject.EvaluatedAt of
                          eaBlocks:
                            begin
                              Middle := (UpperBound + LowerBound)/2;
                              if (Orientation = dsoSide) or
                                ((FScreenObject.TopElevation >= Middle)
                                and (FScreenObject.BottomElevation <= Middle))
                                then
                              begin
                                CellList.Add(TCellAssignment.Create(LayerIndex,
                                  RowIndex, ElevationIndex, nil,
                                  SectionIndex, Annotation, AssignmentMethod));
                              end;
                            end;
                          eaNodes:
                            begin
                              if (Orientation = dsoSide) or
                                ((FScreenObject.TopElevation >= LowerBound)
                                and (FScreenObject.BottomElevation <= UpperBound))
                                then
                              begin
                                CellList.Add(TCellAssignment.Create(LayerIndex,
                                  RowIndex, ElevationIndex, nil,
                                  SectionIndex, Annotation, AssignmentMethod));
                              end;
                            end;
                          else Assert(False);
                        end;
                      end;
                    end;
                  else Assert(False);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    if FScreenObject.SetValuesOfIntersectedCells then
    begin
      if not FScreenObject.Segments.UpToDate then
      begin
        UpdateSideSegments(Grid, DataSet.EvaluatedAt);
      end;
      // set values here
      PriorRow := -1;
      PriorLayer := -1;
      FirstElevationIndex := 0;
      LastElevationIndex := -1;
      for SegmentIndex := 0 to FScreenObject.Segments.Count - 1 do
      begin
        ASegment := FScreenObject.Segments[SegmentIndex];
        if (ASegment.Row <> PriorRow) or (ASegment.Layer <> PriorLayer) then
        begin
          UpdateCurrentSegment(ASegment);
          OtherIndex(ASegment.Layer, ASegment.Row, FirstElevationIndex,
            LastElevationIndex, DataSet);
          PriorRow := ASegment.Row;
          PriorLayer := ASegment.Layer;
        end;
        if (FirstElevationIndex >= 0) and (LastElevationIndex <= ColumnLimit)
          and (ASegment.Col >= FirstElevationIndex)
          and (ASegment.Col <= LastElevationIndex) then
        begin
          UpdateCurrentSegment(ASegment);
          if IAnnotation = '' then
          begin
            IAnnotation := IntersectAnnotation(DataSetFunction, OtherData);
          end;
          Annotation := IAnnotation;
          LocalGrid.BlockExtents(vdSide, FScreenObject.EvaluatedAt,
          ASegment.Col, LowerBound, UpperBound);
          case FScreenObject.ElevationCount of
            ecZero: ; // do nothing
            ecOne:
              begin
                FScreenObject.FTopElevation :=
                  FScreenObject.Higher3DElevations[
                    ASegment.Layer, ASegment.Row, ASegment.Col];
                FScreenObject.FBottomElevation :=
                  FScreenObject.TopElevation
              end;
            ecTwo:
              begin
                FScreenObject.FTopElevation :=
                  FScreenObject.Higher3DElevations[
                    ASegment.Layer, ASegment.Row, ASegment.Col];
                FScreenObject.FBottomElevation :=
                  FScreenObject.Lower3DElevations[
                    ASegment.Layer, ASegment.Row, ASegment.Col];
              end;
            else Assert(False);
          end;
          case FScreenObject.ElevationCount of
            ecZero:
              begin
                CellList.Add(TCellAssignment.Create(ASegment.Layer,
                  ASegment.Row, ASegment.Col, ASegment,
                  ASegment.SectionIndex, Annotation, amIntersect));
              end;
            ecOne:
              begin
                if (Orientation = dsoSide) or
                  ((FScreenObject.TopElevation >= LowerBound)
                  and ((FScreenObject.BottomElevation < UpperBound)
                  or ((FScreenObject.BottomElevation = UpperBound)
                  and (ASegment.Col = ColumnLimit)))) then
                begin
                  CellList.Add(TCellAssignment.Create(ASegment.Layer,
                    ASegment.Row, ASegment.Col, ASegment,
                    ASegment.SectionIndex, Annotation, amIntersect));
                end;
              end;
            ecTwo:
              begin
                case FScreenObject.EvaluatedAt of
                  eaBlocks:
                    begin
                      Middle := (UpperBound + LowerBound)/2;
                      if (Orientation = dsoSide) or
                        ((FScreenObject.TopElevation >= Middle)
                        and (FScreenObject.BottomElevation <= Middle)) then
                      begin
                        CellList.Add(TCellAssignment.Create(ASegment.Layer,
                          ASegment.Row, ASegment.Col, ASegment,
                          ASegment.SectionIndex, Annotation, amIntersect));
                      end;
                    end;
                  eaNodes:
                    begin
                      if (Orientation = dsoSide) or
                        ((FScreenObject.TopElevation >= LowerBound)
                        and (FScreenObject.BottomElevation <= UpperBound)) then
                      begin
                        CellList.Add(TCellAssignment.Create(ASegment.Layer,
                          ASegment.Row, ASegment.Col, ASegment,
                          ASegment.SectionIndex, Annotation, amIntersect));
                      end;
                    end;
                  else Assert(False);
                end;
              end;
            else Assert(False);
          end;
        end;
      end;
    end;
  finally
    FScreenObject.UpdateCellCache(CellList, EvalAt,
    Orientation, AssignmentLocation);
  end;
end;

procedure TPhastDelegate.GetTopCellsToAssign(const Grid: TCustomGrid;
  const DataSetFunction: string; OtherData: TObject; const DataSet: TDataArray;
  CellList: TCellAssignmentList; AssignmentLocation: TAssignmentLocation);
var
  TempMaxY: double;
  TempMaxX: double;
  TempMinY: double;
  TempMinX: double;
  LayerLimit: Integer;
  EAnnotation: string;
  IAnnotation: string;
  ASegment: TCellElementSegment;
  SegmentIndex: Integer;
  PriorRow: Integer;
  PriorCol: Integer;
  Annotation: string;
  ElevationIndex: Integer;
  LastElevationIndex: Integer;
  FirstElevationIndex: Integer;
  SectionIndex: Integer;
  CellLocation3D: T3DRealPoint;
  RowIndex: Integer;
  ColIndex: Integer;
  LastRow: Integer;
  FirstRow: Integer;
  LastCol: Integer;
  FirstCol: Integer;
  LocalGrid: TPhastGrid;
  LowerBound: Double;
  UpperBound: Double;
  Middle: double;
  RotatedPoints: TEdgePointArray;
  PointsRotated: boolean;
  APoint: TPoint2D;
  EvalAt: TEvaluatedAt;
  Orientation: TDataSetOrientation;
  AssignmentMethod: TAssignmentMethod;
begin
  Assert(CellList <> nil);
  Assert(CellList.Count = 0);
  LocalGrid := Grid as TPhastGrid;
  IAnnotation := IntersectAnnotation(DataSetFunction, OtherData);
  EAnnotation := EncloseAnnotation(DataSetFunction, OtherData);
  UpdateCurrentScreenObject(FScreenObject);
  LayerLimit := -1;
  if DataSet = nil then
  begin
    EvalAt := FScreenObject.EvaluatedAt;
    Orientation := dso3D;
  end
  else
  begin
    EvalAt := DataSet.EvaluatedAt;
    Orientation := DataSet.Orientation;
  end;
  if FScreenObject.RestoreCellsFromCache(CellList, EvalAt,
    Orientation, AssignmentLocation, EAnnotation, IAnnotation) then
  begin
    Exit;
  end;
  try
    case EvalAt of
      eaBlocks:
        begin
          LayerLimit := LocalGrid.LayerCount - 1;
        end;
      eaNodes:
        begin
          LayerLimit := LocalGrid.LayerCount;
        end;
    else
      Assert(False);
    end;
    PointsRotated := False;
    if (FScreenObject.SetValuesOfEnclosedCells
      or (FScreenObject.SetValuesOfIntersectedCells
      and (FScreenObject.ElevationCount <> ecZero)))
      and FScreenObject.Closed then
    begin
      FScreenObject.RotatePoints(Grid, RotatedPoints,
        TempMinX, TempMinY, TempMaxX, TempMaxY);
      PointsRotated := True;


      // Get the coordinates of the points.
      FScreenObject.GetColumns(LocalGrid, TempMinX, TempMaxX, FirstCol, LastCol);
      if FirstCol > LocalGrid.ColumnCount then
      begin
        FirstCol := LocalGrid.ColumnCount;
      end;
      if LastCol > LocalGrid.ColumnCount then
      begin
        LastCol := LocalGrid.ColumnCount;
      end;
      FScreenObject.GetRows(LocalGrid, TempMinY, TempMaxY, FirstRow, LastRow);
      if FirstRow > LocalGrid.RowCount then
      begin
        FirstRow := LocalGrid.RowCount;
      end;
      if LastRow > LocalGrid.RowCount then
      begin
        LastRow := LocalGrid.RowCount;
      end;
      // Find the cells inside the screen object and assign values to them.
      for ColIndex := FirstCol to LastCol do
      begin
        for RowIndex := FirstRow to LastRow do
        begin
          case FScreenObject.EvaluatedAt of
            eaBlocks:
              begin
                CellLocation3D := LocalGrid.ThreeDElementCenter(
                  ColIndex, RowIndex, 0);
              end;
            eaNodes:
              begin
                CellLocation3D := LocalGrid.ThreeDElementCorner(
                   ColIndex, RowIndex, 0);
              end;
          else
            Assert(False);
          end;
          APoint.x := CellLocation3D.X;
          APoint.y := CellLocation3D.Y;
          APoint := Grid.
            RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
          if FScreenObject.IsPointInside(APoint.X,
            APoint.Y, SectionIndex) then
          begin
            OtherIndex(RowIndex, ColIndex, FirstElevationIndex,
              LastElevationIndex, DataSet);
            if (FirstElevationIndex >= 0)
              and (LastElevationIndex <= LayerLimit) then
            begin
              for ElevationIndex := FirstElevationIndex to LastElevationIndex do
              begin
                case FScreenObject.ElevationCount of
                  ecZero: ; // do nothing
                  ecOne:
                    begin
                      FScreenObject.FTopElevation :=
                        FScreenObject.Higher3DElevations[
                          ElevationIndex, RowIndex, ColIndex];
                      FScreenObject.FBottomElevation :=
                        FScreenObject.TopElevation
                    end;
                  ecTwo:
                    begin
                      FScreenObject.FTopElevation :=
                        FScreenObject.Higher3DElevations[
                          ElevationIndex, RowIndex, ColIndex];
                      FScreenObject.FBottomElevation :=
                        FScreenObject.Lower3DElevations[
                          ElevationIndex, RowIndex, ColIndex];
                    end;
                  else Assert(False);
                end;

                if not FScreenObject.SetValuesOfEnclosedCells then
                begin
                  AssignmentMethod := amIntersect;
                  if IAnnotation = '' then
                  begin
                    IAnnotation := IntersectAnnotation(
                      DataSetFunction, OtherData);
                  end;
                  Annotation := IAnnotation;
                end
                else
                begin
                  AssignmentMethod := amEnclose;
                  if EAnnotation = '' then
                  begin
                    EAnnotation := EncloseAnnotation(DataSetFunction, OtherData);
                  end;
                  Annotation := EAnnotation;
                end;
                UpdateCurrentSection(SectionIndex);
                LocalGrid.BlockExtents(vdTop, FScreenObject.EvaluatedAt,
                  ElevationIndex, LowerBound, UpperBound);
                case FScreenObject.ElevationCount of
                  ecZero:
                    begin
                      CellList.Add(TCellAssignment.Create(ElevationIndex,
                        RowIndex, ColIndex, nil, SectionIndex, Annotation,
                        AssignmentMethod));
                    end;
                  ecOne:
                    begin
                      if (Orientation = dsoTop) or
                        ((FScreenObject.TopElevation >= LowerBound)
                        and ((FScreenObject.BottomElevation < UpperBound)
                        or ((FScreenObject.BottomElevation = UpperBound)
                        and (ElevationIndex = LayerLimit)))) then
                      begin
                        CellList.Add(TCellAssignment.Create(ElevationIndex,
                          RowIndex, ColIndex, nil, SectionIndex, Annotation,
                          AssignmentMethod));
                      end;
                    end;
                  ecTwo:
                    begin
                      if not FScreenObject.SetValuesOfEnclosedCells then
                      begin
                        if (Orientation = dsoTop)
                          or ((UpperBound >= FScreenObject.TopElevation)
                          and (LowerBound <= FScreenObject.TopElevation))
                          or ((UpperBound >= FScreenObject.BottomElevation)
                          and (LowerBound <= FScreenObject.BottomElevation)) then
                        begin
                          CellList.Add(TCellAssignment.Create(ElevationIndex,
                            RowIndex, ColIndex, nil, SectionIndex, Annotation,
                            AssignmentMethod));
                        end;
                      end
                      else
                      begin
                        case FScreenObject.EvaluatedAt of
                          eaBlocks:
                            begin
                              Middle := (LowerBound + UpperBound)/2;
                              if (Orientation = dsoTop) or
                                ((FScreenObject.TopElevation >= Middle)
                                and (FScreenObject.BottomElevation <= Middle)) then
                              begin
                                CellList.Add(TCellAssignment.Create(
                                  ElevationIndex, RowIndex,
                                  ColIndex, nil, SectionIndex, Annotation,
                                  AssignmentMethod));
                              end;
                            end;
                          eaNodes:
                            begin
                              if (Orientation = dsoTop) or
                                ((FScreenObject.TopElevation >= LowerBound)
                                and (FScreenObject.BottomElevation <= UpperBound))
                                then
                              begin
                                CellList.Add(TCellAssignment.Create(
                                  ElevationIndex, RowIndex,
                                  ColIndex, nil, SectionIndex, Annotation,
                                  AssignmentMethod));
                              end;
                            end;
                          else Assert(False);
                        end;
                      end;
                    end;
                  else Assert(False);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    if FScreenObject.SetValuesOfIntersectedCells then
    begin
      if not FScreenObject.Segments.UpToDate then
      begin
        FScreenObject.UpdateTopSegments(LocalGrid, DataSet.EvaluatedAt,
          PointsRotated, RotatedPoints);
      end;
      // Assign values here.
      PriorCol := -1;
      PriorRow := -1;
      FirstElevationIndex := 0;
      LastElevationIndex := -1;
      for SegmentIndex := 0 to FScreenObject.Segments.Count - 1 do
      begin
        ASegment := FScreenObject.Segments[SegmentIndex];
        if (ASegment.Col <> PriorCol) or (ASegment.Row <> PriorRow) then
        begin
          UpdateCurrentSegment(ASegment);
          OtherIndex(ASegment.Row, ASegment.Col, FirstElevationIndex,
            LastElevationIndex, DataSet);
          PriorCol := ASegment.Col;
          PriorRow := ASegment.Row;
        end;
        if (FirstElevationIndex >= 0) and (LastElevationIndex <= LayerLimit)
          and (ASegment.Layer >= FirstElevationIndex)
          and (ASegment.Layer <= LastElevationIndex) then
        begin
          UpdateCurrentSegment(ASegment);
          if IAnnotation = '' then
          begin
            IAnnotation := IntersectAnnotation(DataSetFunction, OtherData);
          end;
          Annotation := IAnnotation;
          LocalGrid.BlockExtents(vdTop, FScreenObject.EvaluatedAt,
            ASegment.Layer, LowerBound, UpperBound);
          case FScreenObject.ElevationCount of
            ecZero: ; // do nothing
            ecOne:
              begin
                FScreenObject.FTopElevation :=
                  FScreenObject.Higher3DElevations[
                    ASegment.Layer, ASegment.Row, ASegment.Col];
                FScreenObject.FBottomElevation :=
                  FScreenObject.TopElevation
              end;
            ecTwo:
              begin
                FScreenObject.FTopElevation :=
                  FScreenObject.Higher3DElevations[
                    ASegment.Layer, ASegment.Row, ASegment.Col];
                FScreenObject.FBottomElevation :=
                  FScreenObject.Lower3DElevations[
                    ASegment.Layer, ASegment.Row, ASegment.Col];
              end;
            else Assert(False);
          end;
          case FScreenObject.ElevationCount of
            ecZero:
              begin
                CellList.Add(TCellAssignment.Create(ASegment.Layer,
                  ASegment.Row, ASegment.Col, ASegment,
                  ASegment.SectionIndex, Annotation, amIntersect));
              end;
            ecOne:
              begin
                case FScreenObject.EvaluatedAt of
                  eaBlocks:
                    begin
                      if (Orientation = dsoTop) or
                        ((FScreenObject.TopElevation >= LowerBound)
                        and ((FScreenObject.BottomElevation < UpperBound)
                        or  ((FScreenObject.BottomElevation = UpperBound)
                        and (ASegment.Layer = Grid.LayerCount-1)))) then
                      begin
                        CellList.Add(TCellAssignment.Create(ASegment.Layer,
                          ASegment.Row, ASegment.Col, ASegment,
                          ASegment.SectionIndex, Annotation, amIntersect));
                      end;
                    end;
                  eaNodes:
                    begin
                      if (Orientation = dsoTop) or
                        ((FScreenObject.TopElevation >= LowerBound)
                        and ((FScreenObject.BottomElevation < UpperBound)
                        or  ((FScreenObject.BottomElevation = UpperBound)
                        and (ASegment.Layer = Grid.LayerCount)))) then
                      begin
                        CellList.Add(TCellAssignment.Create(ASegment.Layer,
                          ASegment.Row, ASegment.Col, ASegment,
                          ASegment.SectionIndex, Annotation, amIntersect));
                      end;
                    end;
                  else Assert(False);
                end;
              end;
            ecTwo:
              begin
                case FScreenObject.EvaluatedAt of
                  eaBlocks:
                    begin
                      Middle := (LowerBound + UpperBound)/2;
                      if (Orientation = dsoTop) or
                        ((FScreenObject.TopElevation >= Middle)
                        and (FScreenObject.BottomElevation <= Middle)) then
                      begin
                        CellList.Add(TCellAssignment.Create(ASegment.Layer,
                          ASegment.Row, ASegment.Col, ASegment,
                          ASegment.SectionIndex, Annotation, amIntersect));
                      end;
                    end;
                  eaNodes:
                    begin
                      if (Orientation = dsoTop) or
                        ((FScreenObject.TopElevation >= LowerBound)
                        and (FScreenObject.BottomElevation <= UpperBound)) then
                      begin
                        CellList.Add(TCellAssignment.Create(ASegment.Layer,
                          ASegment.Row, ASegment.Col, ASegment,
                          ASegment.SectionIndex, Annotation, amIntersect));
                      end;
                    end;
                  else Assert(False);
                end;
              end;
            else Assert(False);
          end;
        end;
      end;
    end;
  finally
    FScreenObject.UpdateCellCache(CellList, EvalAt,
      Orientation, AssignmentLocation);
  end;
end;

function TPhastDelegate.DataSetUsed(const DataSet: TDataArray;
  var OtherData: TObject): boolean;
var
  DataSetIndex: integer;
  CouldBeBoundary: boolean;
begin
  if DataSet is TSparseArrayPhastInterpolationDataSet then
  begin
    result := True;
  end
  else if DataSet is TCustomPhastDataSet then
  begin
    OtherData := nil;
    CouldBeBoundary := False;
    DataSetIndex := FScreenObject.IndexOfDataSet(DataSet);
    if DataSetIndex < 0 then
    begin
      CouldBeBoundary := True;
      DataSetIndex := FScreenObject.IndexOfBoundaryDataSet(DataSet);
    end;
    result := DataSetIndex >= 0;
    if result and not CouldBeBoundary then
    begin
      OtherData := FScreenObject.InterpValues.Items[DataSetIndex]
        as TInterpValuesItem;
    end;
  end
  else
  begin
    result := inherited DataSetUsed(DataSet, OtherData);
    if result then
    begin
      if (DataSet = (FModel as TPhastModel).TopBoundaryType)
        or (DataSet = (FModel as TPhastModel).FrontBoundaryType)
        or (DataSet = (FModel as TPhastModel).SideBoundaryType) then
      begin
        result := PhastBoundaryType in [btSpecifiedHead, btFlux, btLeaky]
      end
      else if (DataSet = (FModel as TPhastModel).Top2DBoundaryType) then
      begin
        result := PhastBoundaryType in [btRiver, btWell];
      end;
    end;
  end;
end;

destructor TPhastDelegate.Destroy;
begin
  FSelectedCells.Free;
  inherited;
end;

function TPhastDelegate.EncloseAnnotation(const DataSetFormula: string;
  const OtherData: TObject): string;
var
  InterpValue: TInterpValuesItem;
begin
  InterpValue := OtherData as TInterpValuesItem;
  if (InterpValue = nil) or not InterpValue.Values.UsePHAST_Interpolation then
  begin
    result := inherited EncloseAnnotation(DataSetFormula, OtherData);
  end
  else
  begin
    result := 'Enclosed by ' + FScreenObject.Name
      + ': Set by PHAST-style interpolation';
  end;
end;

procedure TPhastDelegate.InitializeExpression(out Compiler: TRbwParser;
  out DataSetFormula: string; out Expression: TExpression;
  const DataSet: TDataArray; const OtherData: TObject);
var
  InterpValue: TInterpValuesItem;
  LocalExpression: string;
  ResultTypeOK: boolean;
begin
  if (DataSet = (FModel as TPhastModel).TopBoundaryType)
    or (DataSet = (FModel as TPhastModel).FrontBoundaryType)
    or (DataSet = (FModel as TPhastModel).SideBoundaryType) then
  begin
    Compiler := GetCompiler(DataSet.Orientation);
    DataSetFormula := ThreeDBoundaryFormula;
    Compiler.Compile(DataSetFormula);
    Expression := Compiler.CurrentExpression;
    Exit;
  end;

  if (DataSet = (FModel as TPhastModel).Top2DBoundaryType) then
  begin
    Compiler := GetCompiler(DataSet.Orientation);
    DataSetFormula := TwoDBoundaryFormula;
    Compiler.Compile(DataSetFormula);
    Expression := Compiler.CurrentExpression;
    Exit;
  end;

  InterpValue := OtherData as TInterpValuesItem;
  if (InterpValue = nil) then
  begin
    inherited;
  end
  else if InterpValue is TCustomPhastBoundaryCondition then
  begin
    if TCustomPhastBoundaryCondition(InterpValue).UsePHAST_Interpolation then
    begin
      Compiler := nil;
      Expression := nil;
      DataSetFormula := '';
    end
    else
    begin
      Compiler := GetCompiler(DataSet.Orientation);
      try
        LocalExpression := TCustomPhastBoundaryCondition(InterpValue).
          FormulaExpression;
        Compiler.Compile(LocalExpression);
      except on E: ERbwParserError do
        begin
          LocalExpression := '0';
          TCustomPhastBoundaryCondition(InterpValue).FormulaExpression :=
            LocalExpression;
          Compiler.Compile(LocalExpression);
        end;
      end;
      DataSetFormula := LocalExpression;
      Expression := Compiler.CurrentExpression;
      ResultTypeOK := (Expression.ResultType = DataSet.Datatype)
        or ((Expression.ResultType = rdtInteger) and (DataSet.Datatype =
        rdtDouble));
      if not ResultTypeOK then
      begin
        raise EInvalidDataType.Create('Invalid data type.');
      end;
    end;
  end
  else if not InterpValue.Values.UsePHAST_Interpolation then
  begin
    inherited;
  end
  else
  begin
    Compiler := nil;
    Expression := nil;
    DataSetFormula := '';
  end;
end;

function TPhastDelegate.IntersectAnnotation(const DataSetFormula: string;
  const OtherData: TObject): string;
var
  InterpValue: TInterpValuesItem;
begin
  InterpValue := OtherData as TInterpValuesItem;
  if (InterpValue = nil) or not InterpValue.Values.UsePHAST_Interpolation then
  begin
    result := inherited IntersectAnnotation(DataSetFormula, OtherData);
  end
  else
  begin
    result := 'Intersected by ' + FScreenObject.Name
      + ': Set by PHAST-style interpolation';
  end;
end;

procedure TPhastDelegate.OtherIndex(const LayerOrRow, RowOrColumn: integer;
  out First, Last: integer; const DataSet: TDataArray);
var
  IsRiverDataSet: boolean;
begin
  IsRiverDataSet := (DataSet <> nil)
    and ((FScreenObject.RiverBoundary.BoundaryValue.Count > 0)
    or (FScreenObject.RiverBoundary.Solution.Count > 0));
  if IsRiverDataSet then
  begin
    IsRiverDataSet := (DataSet = (FModel as TPhastModel).Top2DBoundaryType);

    if not IsRiverDataSet then
    begin
      if (DataSet is TSparseArrayPhastInterpolationDataSet) then
      begin
        IsRiverDataSet :=
          ((FModel as TPhastModel).RiverHead.IndexOfDataSet
          (TSparseArrayPhastInterpolationDataSet(DataSet)) >= 0)
          or
          ((FModel as TPhastModel).RiverAssociatedSolution.IndexOfDataSet
          (TSparseArrayPhastInterpolationDataSet(DataSet)) >= 0);
      end;
      if not IsRiverDataSet then
      begin
        IsRiverDataSet := ((FModel as TPhastModel).
          RiverDataSets.IndexOf(DataSet) >= 0);
      end;
      if not IsRiverDataSet then
      begin
        if DataSet is TIntegerSparseDataSet then
        begin
          IsRiverDataSet :=
            TIntegerSparseDataSet(DataSet).IsBoundaryTypeDataSet;
        end;
      end;
    end;
  end;
  { TODO :
Changing the layer as woiuld be done here prevents the river data from
being displayed on the status bar.  Find a way around this problem. }

  {if IsRiverDataSet then
  begin
    First := frmGoPhast.PhastGrid.LayerCount;
    Last := First;
  end
  else
  begin  }
    inherited;
//  end;
end;

function TPhastDelegate.PhastBoundaryType: TBoundaryTypes;
begin
  result := btNone;
  if (FScreenObject.SpecifiedHeadBoundary.BoundaryValue.Count > 0)
    or (FScreenObject.SpecifiedHeadBoundary.Solution.Count > 0) then
  begin
    result := btSpecifiedHead;
  end;

  if (FScreenObject.FluxBoundary.BoundaryValue.Count > 0)
    or (FScreenObject.FluxBoundary.Solution.Count > 0) then
  begin
    Assert(result = btNone);
    result := btFlux;
  end;

  if (FScreenObject.LeakyBoundary.BoundaryValue.Count > 0)
    or (FScreenObject.LeakyBoundary.Solution.Count > 0) then
  begin
    Assert(result = btNone);
    result := btLeaky;
  end;

  if (FScreenObject.RiverBoundary.BoundaryValue.Count > 0)
    or (FScreenObject.RiverBoundary.Solution.Count > 0) then
  begin
    Assert(result = btNone);
    result := btRiver;
  end;

  if (FScreenObject.WellBoundary.BoundaryValue.Count > 0)
    or (FScreenObject.WellBoundary.Solution.Count > 0) then
  begin
    Assert(result = btNone);
    result := btWell;
  end;
end;

procedure TPhastDelegate.UpdateFrontSegments(const Grid: TCustomGrid;
  const EvaluatedAt: TEvaluatedAt);
var
  RotatedPoints: TEdgePointArray;
  GridMaxX, GridMinX, GridMaxZ, GridMinZ, Temp: double;
  Index: integer;
  APoint, PreviousPoint, NewPoint: TEdgePoint;
  ColIndex, LayerIndex: integer;
  FirstRow: integer;
  LastRow: integer;
  RowIndex: integer;
  ASegment: TCellElementSegment;
  RowLimit: integer;
  FirstLayer, LastLayer, FirstCol, LastCol, TempI: integer;
  TempPoints1, TempPoints2: TEdgePointArray;
  PointIndex: integer;
  Col1, Col2, Layer1, Layer2: integer;
  Position: TEdgePosition;
  Point1, Point2: TEdgePoint;
  RealPoint: TPoint2D;
  ACell: T2DFrontCell;
  IncreasingX, IncreasingY: boolean;
  SectionIndex: integer;
  EndSection: Boolean;
begin
  FScreenObject.FSegments.Clear;
  RowLimit := -1;
  case EvaluatedAt of
    eaBlocks:
      begin
        RowLimit := Grid.RowCount - 1;
      end;
    eaNodes:
      begin
        RowLimit := Grid.RowCount;
      end;
  else
    Assert(False);
  end;
  // Get the coordinates of the points.
  SetLength(RotatedPoints, FScreenObject.Count);
  for Index := 0 to FScreenObject.Count - 1 do
  begin
    RotatedPoints[Index] := FScreenObject.EdgePoints[Index];
  end;

  // Get the box outlining the grid.
  if Grid.ColumnCount >= 0 then
  begin
    GridMaxX := Grid.ColumnPosition[Grid.ColumnCount];
    GridMinX := Grid.ColumnPosition[0];
  end
  else
  begin
    GridMaxX := 0;
    GridMinX := 0;
  end;
  if GridMinX > GridMaxX then
  begin
    Temp := GridMinX;
    GridMinX := GridMaxX;
    GridMaxX := Temp;
  end;

  if Grid.LayerCount >= 0 then
  begin
    GridMaxZ := Grid.HighestElevation;
    GridMinZ := Grid.LowestElevation;
  end
  else
  begin
    GridMaxZ := 0;
    GridMinZ := 0;
  end;
  if GridMinZ > GridMaxZ then
  begin
    Temp := GridMinZ;
    GridMinZ := GridMaxZ;
    GridMaxZ := Temp;
  end;

  SectionIndex := 0;
  // loop over vertices
  for Index := 0 to FScreenObject.Count - 1 do
  begin
    Assert(SectionIndex < FScreenObject.SectionCount);
    if (Index > FScreenObject.SectionStart[SectionIndex]) then
    begin
      PreviousPoint := APoint;
    end;
    // color the cells at the vertices.
    APoint := RotatedPoints[Index];
    if (FScreenObject.SectionLength[SectionIndex] = 1) then
    begin
      Inc(SectionIndex);
      if (APoint.X >= GridMinX) and (APoint.X <= GridMaxX)
        and (APoint.Y >= GridMinZ) and (APoint.Y <= GridMaxZ) then
      begin
        ColIndex := FScreenObject.GetACol(Grid, APoint.X);
        if (EvaluatedAt = eaBlocks) and (ColIndex = Grid.ColumnCount) then
        begin
          Dec(ColIndex);
        end;
        LayerIndex := FScreenObject.GetAPhastLayer(Grid, APoint.Y);
        if (EvaluatedAt = eaBlocks) and (LayerIndex = Grid.LayerCount) then
        begin
          Dec(LayerIndex);
        end;

        FirstRow := 0;
        LastRow := RowLimit;
        for RowIndex := FirstRow to LastRow do
        begin
          ASegment := TCellElementSegment.Create;
          FScreenObject.FSegments.Add(ASegment);
          ASegment.X1 := APoint.X;
          ASegment.X2 := APoint.X;
          ASegment.Y1 := APoint.Y;
          ASegment.Y2 := APoint.Y;
          ASegment.Col := ColIndex;
          ASegment.Row := RowIndex;
          ASegment.Layer := LayerIndex;
          ASegment.VertexIndex := Index;
          ASegment.StartPosition := epFirst;
          ASegment.EndPosition := epLast;
          ASegment.SectionIndex := SectionIndex-1;
        end;
      end;
    end
    else
    begin
      EndSection := False;
      // get a line segment and color cells along it.
      if (Index = FScreenObject.SectionStart[SectionIndex]) then
      begin
        Continue;
      end;
      if (Index = FScreenObject.SectionEnd[SectionIndex]) then
      begin
        Inc(SectionIndex);
        EndSection := True;
      end;

      // get a line segment and store the values of cells along it.
      FirstCol := FScreenObject.GetACol(Grid, PreviousPoint.X);
      LastCol := FScreenObject.GetACol(Grid, APoint.X);

      FirstLayer := FScreenObject.GetAPhastLayer(Grid, PreviousPoint.Y);
      LastLayer := FScreenObject.GetAPhastLayer(Grid, APoint.Y);

      PointIndex := 0;
      // get points on line segment that intersect the column lines.
      SetLength(TempPoints1, Abs(LastCol - FirstCol) + Abs(LastLayer -
        FirstLayer) + 4);
      PreviousPoint.Position := epFirst;
      TempPoints1[PointIndex] := PreviousPoint;
      Inc(PointIndex);
      if (PreviousPoint.X <> APoint.X) then
      begin
        IncreasingX := APoint.X > PreviousPoint.X;
        if LastCol >= FirstCol then
        begin
          Col1 := FirstCol;
          Col2 := LastCol;
        end
        else
        begin
          Col1 := LastCol;
          Col2 := FirstCol;
        end;
        if (Col1 >= 0) and (Col2 >= 0) then
        begin
          for ColIndex := Col1 to Col2 do
          begin
            if ((ColIndex = 0) or (ColIndex = Grid.ColumnCount+1)) then
            begin
              if (ColIndex = 0) then
              begin
                if IncreasingX then
                begin
                  Position := epFirst;
                end
                else
                begin
                  Position := epLast;
                end;
              end
              else
              begin
                if IncreasingX then
                begin
                  Position := epLast;
                end
                else
                begin
                  Position := epFirst;
                end;
              end;
            end
            else
            begin
              Position := epMiddle;
            end;
            FScreenObject.AddPointFromColumn(ColIndex, Grid, PreviousPoint,
              APoint, TempPoints1, PointIndex, Position);
          end;
        end;
      end
      else
      begin
        Col1 := FirstCol;
        Col2 := LastCol;
      end;

      // get points on line segment that intersect the row lines.
      if (PreviousPoint.Y <> APoint.Y) then
      begin
        IncreasingY := APoint.Y > PreviousPoint.Y;
        if LastLayer >= FirstLayer then
        begin
          Layer1 := FirstLayer;
          Layer2 := LastLayer;
        end
        else
        begin
          Layer1 := LastLayer;
          Layer2 := FirstLayer;
        end;
        if (Layer1 >= 0) and (Layer2 >= 0) then
        begin
          for LayerIndex := Layer1 to Layer2 do
          begin
            if ((LayerIndex = 0) or (LayerIndex = Grid.LayerCount+1)) then
            begin
              if LayerIndex = 0 then
              begin
                if IncreasingY then
                begin
                  Position := epFirst;
                end
                else
                begin
                  Position := epLast;
                end;
              end
              else
              begin
                if IncreasingY then
                begin
                  Position := epLast;
                end
                else
                begin
                  Position := epFirst;
                end;
              end;
            end
            else
            begin
              Position := epMiddle;
            end;
            FScreenObject.AddPointFromLayer(LayerIndex, Grid, PreviousPoint,
              APoint, TempPoints1, PointIndex, Position);
          end;
        end;
      end
      else
      begin
        Layer1 := FirstLayer;
        Layer2 := LastLayer;
      end;
      APoint.Position := epLast;
      TempPoints1[PointIndex] := APoint;
      Inc(PointIndex);

      // Sort the points.
      FScreenObject.SortPoints(TempPoints1, TempPoints2, APoint, PreviousPoint,
        PointIndex);

      case EvaluatedAt of
        eaBlocks:
          begin
            // Get cells and color them.
            for PointIndex := 1 to Length(TempPoints2) - 1 do
            begin
              Point2 := TempPoints2[PointIndex];
              Point1 := TempPoints2[PointIndex - 1];
              NewPoint.X := (Point1.X + Point2.X) / 2;
              NewPoint.Y := (Point1.Y + Point2.Y) / 2;
              if (NewPoint.X >= GridMinX) and (NewPoint.X <= GridMaxX)
                and (NewPoint.Y >= GridMinZ) and (NewPoint.Y <= GridMaxZ) then
              begin
                RealPoint.X := NewPoint.X;
                RealPoint.Y := NewPoint.Y;
                ACell := (Grid as TPhastGrid).FrontContainingCell(RealPoint,
                  EvaluatedAt, Col1, Col2, Layer1, Layer2);

                FirstRow := 0;
                LastRow := RowLimit;
                for RowIndex := FirstRow to LastRow
                  do
                begin
                  ASegment := TCellElementSegment.Create;
                  FScreenObject.FSegments.Add(ASegment);
                  ASegment.X1 := Point1.X;
                  ASegment.X2 := Point2.X;
                  ASegment.Y1 := Point1.Y;
                  ASegment.Y2 := Point2.Y;
                  ASegment.Col := ACell.Col;
                  ASegment.Row := RowIndex;
                  ASegment.Layer := ACell.Lay;
                  ASegment.VertexIndex := Index - 1;
                  ASegment.StartPosition := Point1.Position;
                  ASegment.EndPosition := Point2.Position;
                  if PointIndex = 0 then
                  begin
                    ASegment.StartPosition := epFirst;
                  end;
                  if PointIndex = Length(TempPoints2) - 1 then
                  begin
                    ASegment.StartPosition := epLast;
                  end;
                  if EndSection then
                  begin
                    ASegment.SectionIndex := SectionIndex-1;
                  end
                  else
                  begin
                    ASegment.SectionIndex := SectionIndex;
                  end;
                end;
              end;
            end;
          end;
        eaNodes:
          begin
            // Get cells and color them.
            for PointIndex := 1 to Length(TempPoints2) - 1 do
            begin
              Point2 := TempPoints2[PointIndex];
              Point1 := TempPoints2[PointIndex - 1];
              NewPoint.X := (Point1.X + Point2.X) / 2;
              NewPoint.Y := (Point1.Y + Point2.Y) / 2;
              if (NewPoint.X >= GridMinX) and (NewPoint.X <= GridMaxX)
                and (NewPoint.Y >= GridMinZ) and (NewPoint.Y <= GridMaxZ) then
              begin
                RealPoint.X := NewPoint.X;
                RealPoint.Y := NewPoint.Y;
                ACell := (Grid as TPhastGrid).FrontContainingCell(RealPoint,
                  EvaluatedAt, Col1, Col2, Layer1, Layer2);

                FirstRow := 0;
                LastRow := RowLimit;
                for RowIndex := FirstRow to LastRow
                  do
                begin
                  ASegment := TCellElementSegment.Create;
                  FScreenObject.FSegments.Add(ASegment);
                  ASegment.X1 := Point1.X;
                  ASegment.X2 := Point2.X;
                  ASegment.Y1 := Point1.Y;
                  ASegment.Y2 := Point2.Y;
                  ASegment.Col := ACell.Col;
                  ASegment.Row := RowIndex;
                  ASegment.Layer := ACell.Lay;
                  ASegment.VertexIndex := Index - 1;
                  ASegment.StartPosition := Point1.Position;
                  ASegment.EndPosition := Point2.Position;
                  if PointIndex = 0 then
                  begin
                    ASegment.StartPosition := epFirst;
                  end;
                  if PointIndex = Length(TempPoints2) - 1 then
                  begin
                    ASegment.StartPosition := epLast;
                  end;
                  if EndSection then
                  begin
                    ASegment.SectionIndex := SectionIndex-1;
                  end
                  else
                  begin
                    ASegment.SectionIndex := SectionIndex;
                  end;
                end;
              end;
            end;
          end;
      else
        Assert(False);
      end;

    end;
  end;
  FScreenObject.FSegments.UpToDate := True;
end;

procedure TPhastDelegate.UpdateSideSegments(const Grid: TCustomGrid;
  const EvaluatedAt: TEvaluatedAt);
var
  RotatedPoints: TEdgePointArray;
  APoint, PreviousPoint, AnotherPoint, NewPoint: TEdgePoint;
  GridMaxY, GridMinY, GridMaxZ, GridMinZ, Temp: double;
  Index: integer;
  RowIndex, LayerIndex: integer;
  FirstElevationIndex: integer;
  LastElevationIndex: integer;
  ElevationIndex: integer;
  ColumnLimit: integer;
  ASegment: TCellElementSegment;
  FirstLayer, LastLayer, FirstRow, LastRow, TempI: integer;
  PointIndex: integer;
  TempPoints1, TempPoints2: TEdgePointArray;
  Row1, Row2, Layer1, Layer2: integer;
  Position: TEdgePosition;
  Point1, Point2: TEdgePoint;
  RealPoint: TPoint2D;
  ACell: T2DSideCell;
  IncreasingX, IncreasingY: boolean;
  SectionIndex: integer;
  EndSection: boolean;
begin
  FScreenObject.FSegments.Clear;
  ColumnLimit := -1;
  case EvaluatedAt of
    eaBlocks:
      begin
        ColumnLimit := Grid.ColumnCount - 1;
      end;
    eaNodes:
      begin
        ColumnLimit := Grid.ColumnCount;
      end;
  else
    Assert(False);
  end;

  // Get the coordinates of the points after rotating them to the grid
  // coordinate system.
  SetLength(RotatedPoints, FScreenObject.Count);
  for Index := 0 to FScreenObject.Count - 1 do
  begin
    APoint := FScreenObject.EdgePoints[Index];
    AnotherPoint.X := APoint.Y;
    AnotherPoint.Y := APoint.X;
    RotatedPoints[Index] := AnotherPoint;
  end;

  // Get the box outlining the grid.
  if Grid.RowCount >= 0 then
  begin
    GridMaxY := Grid.RowPosition[Grid.RowCount];
    GridMinY := Grid.RowPosition[0];
  end
  else
  begin
    GridMaxY := 0;
    GridMinY := 0;
  end;
  if GridMinY > GridMaxY then
  begin
    Temp := GridMinY;
    GridMinY := GridMaxY;
    GridMaxY := Temp;
  end;

  if Grid.LayerCount >= 0 then
  begin
    GridMaxZ := Grid.HighestElevation;
    GridMinZ := Grid.LowestElevation;
  end
  else
  begin
    GridMaxZ := 0;
    GridMinZ := 0;
  end;
  if GridMinZ > GridMaxZ then
  begin
    Temp := GridMinZ;
    GridMinZ := GridMaxZ;
    GridMaxZ := Temp;
  end;

  SectionIndex := 0;
  // loop over vertices
  for Index := 0 to FScreenObject.Count - 1 do
  begin
    Assert(SectionIndex < FScreenObject.SectionCount);
    if (Index > FScreenObject.SectionStart[SectionIndex]) then
    begin
      PreviousPoint := APoint;
    end;
    // color the cells at the vertices.
    APoint := RotatedPoints[Index];
    if (SectionIndex < FScreenObject.SectionCount)
      and (FScreenObject.SectionLength[SectionIndex] = 1) then
    begin
      Inc(SectionIndex);
      if (APoint.X >= GridMinY) and (APoint.X <= GridMaxY)
        and (APoint.Y >= GridMinZ) and (APoint.Y <= GridMaxZ) then
      begin
        RowIndex := FScreenObject.GetARow(Grid, APoint.X);
        if (EvaluatedAt = eaBlocks) and (RowIndex = Grid.RowCount) then
        begin
          Dec(RowIndex);
        end;
        LayerIndex := FScreenObject.GetAPhastLayer(Grid, APoint.Y);
        if (EvaluatedAt = eaBlocks) and (LayerIndex = Grid.LayerCount) then
        begin
          Dec(LayerIndex);
        end;

        FirstElevationIndex := 0;
        LastElevationIndex := ColumnLimit;
        for ElevationIndex := FirstElevationIndex to LastElevationIndex do
        begin
          ASegment := TCellElementSegment.Create;
          FScreenObject.FSegments.Add(ASegment);
          ASegment.X1 := APoint.X;
          ASegment.X2 := APoint.X;
          ASegment.Y1 := APoint.Y;
          ASegment.Y2 := APoint.Y;
          ASegment.Col := ElevationIndex;
          ASegment.Row := RowIndex;
          ASegment.Layer := LayerIndex;
          ASegment.VertexIndex := Index;
          ASegment.StartPosition := epFirst;
          ASegment.EndPosition := epLast;
          ASegment.SectionIndex := SectionIndex -1;
        end;
      end;
    end
    else
    begin
      EndSection := False;
      if (Index = FScreenObject.SectionStart[SectionIndex]) then
      begin
        Continue;
      end;
      if (Index = FScreenObject.SectionEnd[SectionIndex]) then
      begin
        Inc(SectionIndex);
        EndSection := True;
      end;
      // get a line segment and color cells along it.
      FirstRow := FScreenObject.GetARow(Grid, PreviousPoint.X);
      LastRow := FScreenObject.GetARow(Grid, APoint.X);

      FirstLayer := FScreenObject.GetAPhastLayer(Grid, PreviousPoint.Y);
      LastLayer := FScreenObject.GetAPhastLayer(Grid, APoint.Y);

      PointIndex := 0;
      // get points on line segment that intersect the column lines.
      SetLength(TempPoints1, Abs(LastRow - FirstRow) + Abs(LastLayer -
        FirstLayer) + 4);
      PreviousPoint.Position := epFirst;
      TempPoints1[PointIndex] := PreviousPoint;
      Inc(PointIndex);
      if (PreviousPoint.X <> APoint.X) then
      begin
        IncreasingX := APoint.X > PreviousPoint.X;
        if LastRow >= FirstRow then
        begin
          Row1 := FirstRow;
          Row2 := LastRow;
        end
        else
        begin
          Row1 := LastRow;
          Row2 := FirstRow;
        end;
        if (Row1 >= 0) and (Row2 >= 0) then
        begin
          for RowIndex := Row1 to Row2 do
          begin
            if ((RowIndex = 0) or (RowIndex = Grid.RowCount+1)) then
            begin
              if RowIndex = 0 then
              begin
                if IncreasingX then
                begin
                  Position := epFirst;
                end
                else
                begin
                  Position := epLast;
                end;
              end
              else
              begin
                if IncreasingX then
                begin
                  Position := epLast;
                end
                else
                begin
                  Position := epFirst;
                end;
              end;
            end
            else
            begin
              Position := epMiddle;
            end;
            FScreenObject.AddPointFromRow(RowIndex, Grid, PreviousPoint,
              APoint, TempPoints1, PointIndex, Position);
          end;
        end;
      end
      else
      begin
        Row1 := FirstRow;
        Row2 := LastRow;
      end;

      // get points on line segment that intersect the row lines.
      if (PreviousPoint.Y <> APoint.Y) then
      begin
        IncreasingY := APoint.Y > PreviousPoint.Y;
        if LastLayer >= FirstLayer then
        begin
          Layer1 := FirstLayer;
          Layer2 := LastLayer;
        end
        else
        begin
          Layer1 := LastLayer;
          Layer2 := FirstLayer;
        end;
        if (Layer1 >= 0) and (Layer2 >= 0) then
        begin
          for LayerIndex := Layer1 to Layer2 do
          begin
            if ((LayerIndex = 0) or (LayerIndex = Grid.LayerCount+1)) then
            begin
              if LayerIndex = 0 then
              begin
                if IncreasingY then
                begin
                  Position := epFirst;
                end
                else
                begin
                  Position := epLast;
                end;
              end
              else
              begin
                if IncreasingY then
                begin
                  Position := epLast;
                end
                else
                begin
                  Position := epFirst;
                end;
              end;
            end
            else
            begin
              Position := epMiddle;
            end;
            FScreenObject.AddPointFromLayer(LayerIndex, Grid, PreviousPoint,
              APoint, TempPoints1, PointIndex, Position);
          end;
        end;
      end
      else
      begin
        Layer1 := LastLayer;
        Layer2 := FirstLayer;
      end;
      APoint.Position := epLast;
      TempPoints1[PointIndex] := APoint;
      Inc(PointIndex);

      // Sort the points.
      FScreenObject.SortPoints(TempPoints1, TempPoints2, APoint, PreviousPoint,
        PointIndex);

      case EvaluatedAt of
        eaBlocks:
          begin
            // Get cells and color them.
            for PointIndex := 1 to Length(TempPoints2) - 1 do
            begin
              Point2 := TempPoints2[PointIndex];
              Point1 := TempPoints2[PointIndex - 1];
              NewPoint.X := (Point1.X + Point2.X) / 2;
              NewPoint.Y := (Point1.Y + Point2.Y) / 2;
              if (NewPoint.X >= GridMinY) and (NewPoint.X <= GridMaxY)
                and (NewPoint.Y >= GridMinZ) and (NewPoint.Y <= GridMaxZ) then
              begin
                RealPoint.X := NewPoint.X;
                RealPoint.Y := NewPoint.Y;

                ACell := (Grid as TPhastGrid).SideContainingCell(RealPoint,
                  EvaluatedAt, Row1, Row2, Layer1, Layer2);

                FirstElevationIndex := 0;
                LastElevationIndex := ColumnLimit;
                for ElevationIndex := FirstElevationIndex to
                  LastElevationIndex do
                begin

                  ASegment := TCellElementSegment.Create;
                  FScreenObject.FSegments.Add(ASegment);
                  ASegment.X1 := Point1.X;
                  ASegment.X2 := Point2.X;
                  ASegment.Y1 := Point1.Y;
                  ASegment.Y2 := Point2.Y;
                  ASegment.Col := ElevationIndex;
                  ASegment.Row := ACell.Row;
                  ASegment.Layer := ACell.Lay;
                  ASegment.VertexIndex := Index - 1;
                  ASegment.StartPosition := Point1.Position;
                  ASegment.EndPosition := Point2.Position;
                  if PointIndex = 0 then
                  begin
                    ASegment.StartPosition := epFirst;
                  end;
                  if PointIndex = Length(TempPoints2) - 1 then
                  begin
                    ASegment.StartPosition := epLast;
                  end;
                  if EndSection then
                  begin
                    ASegment.SectionIndex := SectionIndex-1;
                  end
                  else
                  begin
                    ASegment.SectionIndex := SectionIndex;
                  end;
                end;
              end;
            end;
          end;
        eaNodes:
          begin
            // Get cells and color them.
            for PointIndex := 1 to Length(TempPoints2) - 1 do
            begin
              Point2 := TempPoints2[PointIndex];
              Point1 := TempPoints2[PointIndex - 1];
              NewPoint.X := (Point1.X + Point2.X) / 2;
              NewPoint.Y := (Point1.Y + Point2.Y) / 2;

              if (NewPoint.X >= GridMinY) and (NewPoint.X <= GridMaxY)
                and (NewPoint.Y >= GridMinZ) and (NewPoint.Y <= GridMaxZ) then
              begin
                RealPoint.X := NewPoint.X;
                RealPoint.Y := NewPoint.Y;
                ACell := (Grid as TPhastGrid).SideContainingCell(RealPoint,
                  EvaluatedAt, Row1, Row2, Layer1, Layer2);

                FirstElevationIndex := 0;
                LastElevationIndex := ColumnLimit;
                for ElevationIndex := FirstElevationIndex to
                  LastElevationIndex do
                begin

                  ASegment := TCellElementSegment.Create;
                  FScreenObject.FSegments.Add(ASegment);
                  ASegment.X2 := Point2.X;
                  ASegment.X1 := Point1.X;
                  ASegment.Y2 := Point2.Y;
                  ASegment.Y1 := Point1.Y;
                  ASegment.Col := ElevationIndex;
                  ASegment.Row := ACell.Row;
                  ASegment.Layer := ACell.Lay;
                  ASegment.VertexIndex := Index - 1;
                  ASegment.StartPosition := Point1.Position;
                  ASegment.EndPosition := Point2.Position;
                  if PointIndex = 0 then
                  begin
                    ASegment.StartPosition := epFirst;
                  end;
                  if PointIndex = Length(TempPoints2) - 1 then
                  begin
                    ASegment.StartPosition := epLast;
                  end;
                  if EndSection then
                  begin
                    ASegment.SectionIndex := SectionIndex-1;
                  end
                  else
                  begin
                    ASegment.SectionIndex := SectionIndex;
                  end;
                end;
              end;
            end;
          end;
      else
        Assert(False);
      end;
    end;
  end;
  FScreenObject.FSegments.UpToDate := True;
end;

{ TModflowDelegate }

function TModflowDelegate.ElevationOk(const Grid: TCustomGrid;
  const PerpendicularIndex: integer; const ColIndex: integer;
  const RowIndex: integer): boolean;
var
  MFGrid: TModflowGrid;
begin
  result := False;
  case FScreenObject.ViewDirection of
    vdTop:
      begin
        case FScreenObject.ElevationCount of
          ecZero:
            begin
              result := PerpendicularIndex = 0;
            end;
          ecOne:
            begin
              MFGrid := Grid as TModflowGrid;
              result :=
                (FScreenObject.BottomElevation <=
                  MFGrid.LayerElevations[
                  ColIndex, RowIndex,PerpendicularIndex])
                and (FScreenObject.TopElevation >=
                  MFGrid.LayerElevations[
                  ColIndex, RowIndex,PerpendicularIndex+1]);
            end;
          ecTwo:
            begin
              MFGrid := Grid as TModflowGrid;
              result :=
                (FScreenObject.BottomElevation <
                  MFGrid.LayerElevations[
                  ColIndex, RowIndex,PerpendicularIndex])
                and (FScreenObject.TopElevation >
                  MFGrid.LayerElevations[
                  ColIndex, RowIndex,PerpendicularIndex+1]);
            end;
          else Assert(False);
        end;
      end;
    vdFront:
      begin
        case FScreenObject.ElevationCount of
          ecZero:
            begin
              result := PerpendicularIndex = 0;
            end;
          ecOne:
            begin
              result := (FScreenObject.TopElevation >=
                Grid.RowPosition[PerpendicularIndex+1])
                and (FScreenObject.BottomElevation <=
                  Grid.RowPosition[PerpendicularIndex]);
            end;
          ecTwo:
            begin
              result := (FScreenObject.TopElevation >
                Grid.RowPosition[PerpendicularIndex+1])
                and (FScreenObject.BottomElevation <
                  Grid.RowPosition[PerpendicularIndex]);
            end;
          else Assert(False);
        end;
      end;
    vdSide:
      begin
        case FScreenObject.ElevationCount of
          ecZero:
            begin
              result := PerpendicularIndex = 0;
            end;
          ecOne:
            begin
              result := (FScreenObject.TopElevation >=
                Grid.ColumnPosition[PerpendicularIndex])
                and (FScreenObject.BottomElevation <=
                Grid.ColumnPosition[PerpendicularIndex+1]);
            end;
          ecTwo:
            begin
              result := (FScreenObject.TopElevation >
                Grid.ColumnPosition[PerpendicularIndex])
                and (FScreenObject.BottomElevation <
                Grid.ColumnPosition[PerpendicularIndex+1]);
            end;
          else Assert(False);
        end;
      end;
    else Assert(False);
  end;
//  result := not result;
end;

procedure TModflowDelegate.AssignValuesToFrontDataSet(const Grid: TCustomGrid;
  const DataSet: TDataArray; OtherData: TObject;
  AssignmentLocation: TAssignmentLocation = alAll);
begin
  AssignValuesToDataSet(Grid, DataSet, OtherData, AssignmentLocation);
end;

procedure TModflowDelegate.AssignValuesToSideDataSet(const Grid: TCustomGrid;
  const DataSet: TDataArray; OtherData: TObject;
  AssignmentLocation: TAssignmentLocation = alAll);
begin
  AssignValuesToDataSet(Grid, DataSet, OtherData, AssignmentLocation);
end;

procedure TModflowDelegate.AssignValuesToTopDataSet(const Grid: TCustomGrid;
  const DataSet: TDataArray; OtherData: TObject;
  AssignmentLocation: TAssignmentLocation = alAll);
begin
  AssignValuesToDataSet(Grid, DataSet, OtherData, AssignmentLocation);
end;

function TModflowDelegate.IsPointInside(
  const CellLocation3D: T3DRealPoint;
  Grid: TCustomGrid; out SectionIndex: integer): boolean;
var
  APoint: TPoint2D;
begin
  result := False;
  case FScreenObject.ViewDirection of
    vdTop:
      begin
        APoint.x := CellLocation3D.X;
        APoint.Y := CellLocation3D.Y;
        APoint := Grid.
          RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
        result := FScreenObject.IsPointInside(
          APoint.X, APoint.Y, SectionIndex);
      end;
    vdFront: result := FScreenObject.IsPointInside(
      CellLocation3D.X, CellLocation3D.Z, SectionIndex);
    vdSide: result := FScreenObject.IsPointInside(
      CellLocation3D.Z, CellLocation3D.Y, SectionIndex);
    else Assert(False);
  end;
end;

function TModflowDelegate.GetPerpendiularLimit(
  const Grid: TCustomGrid): integer;
begin
  result := -1;
  case FScreenObject.ViewDirection of
    vdTop: result := Grid.LayerCount -1;
    vdFront: result := Grid.RowCount - 1;
    vdSide: result := Grid.ColumnCount - 1;
    else Assert(False);
  end;
end;

function TModflowDelegate.AssignElevations(Const ColIndex, RowIndex,
  LayerIndex: integer): boolean;
begin
  result := True;
  case FScreenObject.ElevationCount of
    ecZero:
      begin
        FScreenObject.FTopElevation := 0;
        FScreenObject.FBottomElevation := 0;
      end;
    ecOne:
      begin
        if not FScreenObject.IsHigher3DElevationAssigned(
          ColIndex, RowIndex, LayerIndex) then
        begin
          result := False;
        end;
        if result then
        begin
          FScreenObject.FTopElevation := FScreenObject.
            Higher3DElevations[LayerIndex, RowIndex, ColIndex];
          FScreenObject.FBottomElevation := FScreenObject.FTopElevation;
        end;
      end;
    ecTwo:
      begin
        if not FScreenObject.IsHigher3DElevationAssigned(
          ColIndex, RowIndex, LayerIndex) then
        begin
          result := False;
        end;
        if result then
        begin
          FScreenObject.FTopElevation := FScreenObject.
            Higher3DElevations[LayerIndex, RowIndex, ColIndex];
          FScreenObject.FBottomElevation := FScreenObject.
            Lower3DElevations[LayerIndex, RowIndex, ColIndex];
        end;
      end;
    else Assert(False);
  end;

end;

procedure TModflowDelegate.AssignSelectedCells(const Grid: TCustomGrid);
var
  CellList: TCellAssignmentList;
  Index: Integer;
  Cell: TCellAssignment;
begin
  if FScreenObject.EvaluatedAt = eaNodes then
  begin
    FSelectedCells.SetExtents(0,0,0);
    Exit;
  end;
  CellList := TCellAssignmentList.Create;
  try
    case FScreenObject.EvaluatedAt of
      eaBlocks:
        begin
          FSelectedCells.SetExtents(Grid.LayerCount,
            Grid.RowCount, Grid.ColumnCount);
        end;
      eaNodes:
        begin
          FSelectedCells.SetExtents(Grid.LayerCount+1,
            Grid.RowCount+1, Grid.ColumnCount+1);
        end;
      else Assert(False);
    end;
//    FSelectedCells.SetExtents(Grid.LayerCount,
//      Grid.RowCount, Grid.ColumnCount);
    GetCellsToAssign(Grid, '', nil, eaBlocks, CellList, alAll, dso3D);
    for Index := 0 to CellList.Count - 1 do
    begin
      Cell := CellList[Index];
      FSelectedCells[Cell.Layer, Cell.Row, Cell.Column] := True;
    end;

  finally
    CellList.Free;
  end;
end;



procedure TModflowDelegate.AssignValuesToDataSet(const Grid: TCustomGrid;
  const DataSet: TDataArray; OtherData: TObject;
  AssignmentLocation: TAssignmentLocation = alAll);
var
  DataSetFunction: string;
  Compiler: TRbwParser;
  Expression: TExpression;
  UsedVariables: TStringList;
  CellList: TCellAssignmentList;
  CellAssignment: TCellAssignment;
  AssignmentIndex: Integer;
begin
  CellList := TCellAssignmentList.Create;
  try
    Assert(DataSet <> nil);
    Assert((AssignmentLocation = alAll) or
      FScreenObject.SetValuesOfIntersectedCells);
    if not (DataSet.Orientation in [dsoTop, dso3D]) then
    begin
      // don't remove data set, it may be needed for a PHAST model.
//      FScreenObject.RemoveDataSet(DataSet);
      Exit;
    end;

    if DataSet.EvaluatedAt = eaNodes then
    begin
      Exit;
    end;

  // DataSetUsed doesn't test for transient data sets. 
//  if DataSetUsed(DataSet, OtherData) then
//  begin
    UsedVariables := TStringList.Create;
    try
      InitializeExpression(Compiler, DataSetFunction, Expression, DataSet,
        OtherData);

      InitializeVariables(UsedVariables, DataSet, Expression, Compiler);
      GetCellsToAssign(Grid, DataSetFunction, OtherData,
        DataSet.EvaluatedAt, CellList, AssignmentLocation, DataSet.Orientation);
      FScreenObject.UpdateImportedValues(DataSet);
      for AssignmentIndex := 0 to CellList.Count - 1 do
      begin
        CellAssignment := CellList[AssignmentIndex];
        UpdateCurrentSegment(CellAssignment.Segment);
        UpdateCurrentSection(CellAssignment.Section);
        AssignCellValue(UsedVariables, DataSet, CellAssignment.Layer,
          CellAssignment.Row, CellAssignment.Column, Compiler,
          CellAssignment.Annotation, Expression, OtherData,
          CellAssignment.Section);
      end;
    finally
      (FModel as TPhastModel).CacheDataArrays;
      UsedVariables.Free;
    end;
//  end;
  finally
    CellList.Free;
  end;
end;

constructor TModflowDelegate.Create(ScreenObject: TScreenObject);
begin
  inherited;
  FModelSelection := msModflow;
  FSelectedCells := TSelectedCells.Create;
end;

procedure TModflowDelegate.UpdateHorizontalRangeOfCellsToCheck(
  var FirstHorizontalIndex, LastHorizontalIndex: Integer;
  const HorizontalIndex, HorizontalLimit: Integer;
  const APoint, PreviousPoint: TEdgePoint);
begin
  // It doesn't intersect the other half of the same cell
  // so test the rest of the upper or lower surface.
  case FScreenObject.ViewDirection of
    vdTop:
      Assert(False);
    vdFront:
      begin
        if APoint.X < PreviousPoint.X then
        begin
          FirstHorizontalIndex := HorizontalIndex - 1;
          LastHorizontalIndex := 0;
        end
        else
        begin
          FirstHorizontalIndex := HorizontalIndex + 1;
          LastHorizontalIndex := HorizontalLimit - 1;
        end;
      end;
    vdSide:
      begin
        if APoint.X < PreviousPoint.X then
        begin
          FirstHorizontalIndex := HorizontalIndex + 1;
          LastHorizontalIndex := HorizontalLimit - 1;
        end
        else
        begin
          FirstHorizontalIndex := HorizontalIndex - 1;
          LastHorizontalIndex := 0;
        end;
      end;
  end;
end;

procedure TModflowDelegate.AssignParallellLimits(const Grid: TCustomGrid;
  out FirstParallelIndexA, LastParallelIndexA,
  FirstParallelIndexB, LastParallelIndexB: Integer);
var
  TempMaxX: double;
  TempMinX: double;
  TempMinY: double;
  TempMaxY: double;
  RotatedPoints: TEdgePointArray;
begin
  case FScreenObject.ViewDirection of
    vdTop:
      begin
        FScreenObject.RotatePoints(Grid, RotatedPoints,
          TempMinX, TempMinY, TempMaxX, TempMaxY);
        FScreenObject.GetColumns(Grid, TempMinX, TempMaxX,
          FirstParallelIndexA, LastParallelIndexA);
        if FirstParallelIndexA > Grid.ColumnCount then
        begin
          FirstParallelIndexA := Grid.ColumnCount;
        end;
        if LastParallelIndexA > Grid.ColumnCount then
        begin
          LastParallelIndexA := Grid.ColumnCount;
        end;

        FScreenObject.GetRows(Grid, TempMinY, TempMaxY,
          FirstParallelIndexB, LastParallelIndexB);
        if FirstParallelIndexB > Grid.RowCount then
        begin
          FirstParallelIndexB := Grid.RowCount;
        end;
        if LastParallelIndexB > Grid.RowCount then
        begin
          LastParallelIndexB := Grid.RowCount;
        end;
      end;
    vdFront:
      begin
        TempMinX := FScreenObject.MinX;
        TempMaxX := FScreenObject.MaxX;
        FScreenObject.GetColumns(Grid, TempMinX, TempMaxX,
          FirstParallelIndexA, LastParallelIndexA);
        if FirstParallelIndexA > Grid.ColumnCount then
        begin
          FirstParallelIndexA := Grid.ColumnCount;
        end;
        if LastParallelIndexA > Grid.ColumnCount then
        begin
          LastParallelIndexA := Grid.ColumnCount;
        end;

        FirstParallelIndexB := 0;
        LastParallelIndexB := Grid.LayerCount-1;
      end;
    vdSide:
      begin
        TempMinY := FScreenObject.MinY;
        TempMaxY := FScreenObject.MaxY;
        FScreenObject.GetRows(Grid, TempMinY, TempMaxY,
          FirstParallelIndexA, LastParallelIndexA);
        if FirstParallelIndexA > Grid.RowCount then
        begin
          FirstParallelIndexA := Grid.RowCount;
        end;
        if LastParallelIndexA > Grid.RowCount then
        begin
          LastParallelIndexA := Grid.RowCount;
        end;
        FirstParallelIndexB := 0;
        LastParallelIndexB := Grid.LayerCount-1;
      end;
    else Assert(False);
  end;
end;

procedure TModflowDelegate.AssignColAndRowIndicies(
  var ColIndex, RowIndex, LayerIndex : Integer;
  const HorizontalIndex1, HorizontalIndex2, PerpendicularIndex: Integer);
begin
  case FScreenObject.ViewDirection of
    vdTop:
      begin
        ColIndex := HorizontalIndex1;
        RowIndex := HorizontalIndex2;
        LayerIndex := PerpendicularIndex;
      end;
    vdFront:
      begin
        ColIndex := HorizontalIndex1;
        RowIndex := PerpendicularIndex;
        LayerIndex := HorizontalIndex2;
      end;
    vdSide:
      begin
        ColIndex := PerpendicularIndex;
        RowIndex := HorizontalIndex1;
        LayerIndex := HorizontalIndex2;
      end;
    else Assert(False);
  end;
end;

procedure TModflowDelegate.CreateSegment(const Point1,Point2: TEdgePoint;
  const LayerIndex, PerpendicularIndex, HorizontalIndex, VertexIndex,
  SectionIndex: Integer);
var
  ASegment: TCellElementSegment; 
begin
  ASegment := TCellElementSegment.Create;
  FScreenObject.FSegments.Add(ASegment);
  ASegment.X1 := Point1.X;
  ASegment.X2 := Point2.X;
  ASegment.Y1 := Point1.Y;
  ASegment.Y2 := Point2.Y;
  case FScreenObject.ViewDirection of
    vdTop: Assert(False);
    vdFront:
      begin
        ASegment.Col := HorizontalIndex;
        ASegment.Row := PerpendicularIndex;
      end;
    vdSide:
      begin
        ASegment.Col := PerpendicularIndex;
        ASegment.Row := HorizontalIndex;
      end;
  end;
  ASegment.Layer := LayerIndex;
  ASegment.VertexIndex := VertexIndex;
  ASegment.SectionIndex := SectionIndex;
end;

function TModflowDelegate.DataSetUsed(const DataSet: TDataArray;
  var OtherData: TObject): boolean;
begin
  result := (FScreenObject.IndexOfDataSet(DataSet) >= 0)
    or (FScreenObject.IndexOfBoundaryDataSet(DataSet) >= 0);
  if not result then
  begin
    result := FScreenObject.ModflowDataSetUsed(DataSet);
  end;
end;

destructor TModflowDelegate.Destroy;
begin
  FSelectedCells.Free;
  inherited;
end;

procedure TModflowDelegate.GetCellsToAssign(const Grid: TCustomGrid;
  const DataSetFunction: string; OtherData: TObject;
  const EvaluatedAt: TEvaluatedAt; CellList: TCellAssignmentList;
  AssignmentLocation: TAssignmentLocation; Orientation: TDataSetOrientation);
var
  SegmentIndex: Integer;
  ASegment: TCellElementSegment;
  SelectY: Double;
  SelectX: Double;
  Annotation: string;
  SectionIndex: Integer;
  CellLocation3D: T3DRealPoint;
  ParallelIndex2: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  ParallelIndex1: Integer;
  PerpendicularIndex: Integer;
  FirstParallelIndexA: Integer;
  LastParallelIndexA: Integer;
  FirstParallelIndexB: Integer;
  LastParallelIndexB: Integer;
  PerpendicularLimit: Integer;
  EAnnotation: string;
  IAnnotation: string;
  LayerIndex: Integer;
  LowerLimit, UpperLimit: double;
  AssignmentMethod: TAssignmentMethod;
  Segments: TCellElementSegmentList;
  procedure GetCellBounds;
  begin
    case FScreenObject.ViewDirection of
      vdTop:
        begin
          UpperLimit := Grid.CellElevation[ParallelIndex1,
            ParallelIndex2, PerpendicularIndex];
          LowerLimit := Grid.CellElevation[ParallelIndex1,
            ParallelIndex2, PerpendicularIndex+1];
        end;
      vdFront:
        begin
          UpperLimit := Grid.RowPosition[PerpendicularIndex];
          LowerLimit := Grid.RowPosition[PerpendicularIndex+1];
        end;
      vdSide:
        begin
          LowerLimit := Grid.ColumnPosition[PerpendicularIndex];
          UpperLimit := Grid.ColumnPosition[PerpendicularIndex+1];
        end;
      else Assert(False);
    end;
  end;
begin
  Assert(CellList.Count = 0);
  if (Grid.ColumnCount <= 0) or (Grid.RowCount <= 0)
    or (Grid.LayerCount <=0) then
  begin
    Exit;
  end;
  IAnnotation := IntersectAnnotation(DataSetFunction, OtherData);
  EAnnotation := EncloseAnnotation(DataSetFunction, OtherData);
  if FScreenObject.RestoreCellsFromCache(CellList, EvaluatedAt,
    Orientation, AssignmentLocation, EAnnotation, IAnnotation) then
  begin
    Exit;
  end;
  try
    UpdateCurrentScreenObject(FScreenObject);
    Assert(EvaluatedAt = eaBlocks);
    PerpendicularLimit := GetPerpendiularLimit(Grid);
    if Orientation = dsoTop then
    begin
      PerpendicularLimit := 0
    end
    else
    begin
      Assert(Orientation = dso3D);
    end;
    if (FScreenObject.SetValuesOfEnclosedCells
      or (FScreenObject.SetValuesOfIntersectedCells
      and (FScreenObject.ElevationCount <> ecZero)))
      and FScreenObject.Closed then
    begin
      AssignParallellLimits(Grid,
        FirstParallelIndexA, LastParallelIndexA,
        FirstParallelIndexB, LastParallelIndexB);
      // Find the cells inside the screen object and assign values to them.
      for PerpendicularIndex := 0 to PerpendicularLimit do
      begin
        for ParallelIndex1 := FirstParallelIndexA to LastParallelIndexA do
        begin
          for ParallelIndex2 := FirstParallelIndexB to LastParallelIndexB do
          begin
            AssignColAndRowIndicies(ColIndex, RowIndex, LayerIndex,
              ParallelIndex1, ParallelIndex2, PerpendicularIndex);
            case FScreenObject.EvaluatedAt of
              eaBlocks:
                begin
                  CellLocation3D := Grid.ThreeDElementCenter(
                    ColIndex, RowIndex, LayerIndex);
                end;
              eaNodes:
                begin
                  Assert(False);
                  CellLocation3D := Grid.ThreeDElementCorner(
                    ColIndex, RowIndex, LayerIndex);
                end;
            else
              Assert(False);
            end;
            if IsPointInside(CellLocation3D, Grid, SectionIndex) then
            begin
              if Orientation = dso3D then
              begin

                if not AssignElevations(ColIndex, RowIndex, LayerIndex) then
                begin
                  Continue;
                end;
                if (FScreenObject.ElevationCount = ecTwo)
                  and  not FScreenObject.SetValuesOfEnclosedCells then
                begin
                  GetCellBounds;
                  if not (((UpperLimit >= FScreenObject.FTopElevation)
                    and (LowerLimit <= FScreenObject.FTopElevation))
                    or ((UpperLimit >= FScreenObject.FBottomElevation)
                    and (LowerLimit <= FScreenObject.FBottomElevation))) then
                  begin
                    Continue;
                  end;
                end
                else
                begin
                  if not ElevationOk(Grid, PerpendicularIndex,
                    ColIndex, RowIndex) then
                  begin
                    Continue;
                  end;
                end;
              end;
              if FScreenObject.SetValuesOfIntersectedCells
                and (FScreenObject.ElevationCount <> ecZero) then
              begin
                AssignmentMethod := amIntersect;
                if IAnnotation = '' then
                begin
                  IAnnotation := IntersectAnnotation(DataSetFunction, OtherData);
                end;
                Annotation := IAnnotation;
              end
              else
              begin
                AssignmentMethod := amEnclose;
                if EAnnotation = '' then
                begin
                  EAnnotation := EncloseAnnotation(DataSetFunction, OtherData);
                end;
                Annotation := EAnnotation;
              end;
              UpdateCurrentSection(SectionIndex);
              CellList.Add(TCellAssignment.Create(LayerIndex, RowIndex, ColIndex,
                nil, SectionIndex, Annotation, AssignmentMethod));
            end;
          end;
        end;
      end;
    end;
    if FScreenObject.SetValuesOfIntersectedCells then
    begin
      if not FScreenObject.Segments.UpToDate then
      begin
        UpdateSegments(Grid, EvaluatedAt);
      end;
      Segments := FScreenObject.Segments;
      SelectX := 0;
      SelectY := 0;
      case AssignmentLocation of
        alAll:
          begin
          end;
        alFirstVertex:
          begin
            if Segments.Count > 0 then
            begin
              ASegment := Segments[0];
              SelectX := ASegment.FX1;
              SelectY := ASegment.FY1;
            end
            else
            begin
              SelectX := 0;
              SelectY := 0;
            end;
          end;
        alLastVertex:
          begin
            if Segments.Count > 0 then
            begin
              ASegment := Segments[
                Segments.Count - 1];
              SelectX := ASegment.FX2;
              SelectY := ASegment.FY2;
            end
            else
            begin
              SelectX := 0;
              SelectY := 0;
            end;
          end;
      else
        Assert(False);
      end;
      // Assign values here.
      for SegmentIndex := 0 to Segments.Count - 1 do
      begin
        ASegment := Segments[SegmentIndex];
        case AssignmentLocation of
          alAll:
            begin
            end;
          alFirstVertex:
            begin
              if (SelectX <> ASegment.FX1) or (SelectY <> ASegment.FY1) then
              begin
                Continue;
              end;
            end;
          alLastVertex:
            begin
              if (SelectX <> ASegment.FX2) or (SelectY <> ASegment.FY2) then
              begin
                Continue;
              end;
            end;
        else
          Assert(False);
        end;
        PerpendicularIndex := -1;
        case FScreenObject.ViewDirection of
          vdTop:
            PerpendicularIndex := ASegment.Layer;
          vdFront:
            PerpendicularIndex := ASegment.Row;
          vdSide:
            PerpendicularIndex := ASegment.Col;
        else
          Assert(False);
        end;
        if Orientation = dso3D then
        begin
          UpdateCurrentSegment(ASegment);
          if not AssignElevations(ASegment.Col, ASegment.Row, ASegment.Layer) then
          begin
            Continue;
          end;
          if not ElevationOK(Grid, PerpendicularIndex,
            ASegment.Col, ASegment.Row) then
          begin
            Continue;
          end;
        end;
        UpdateCurrentSegment(ASegment);
        if IAnnotation = '' then
        begin
          IAnnotation := IntersectAnnotation(DataSetFunction, OtherData);
        end;
        Annotation := IAnnotation;
        if Orientation = dso3D then
        begin
          CellList.Add(TCellAssignment.Create(ASegment.Layer, ASegment.Row,
            ASegment.Col, ASegment, ASegment.SectionIndex, Annotation, amIntersect));
        end
        else
        begin
          CellList.Add(TCellAssignment.Create(0, ASegment.Row,
            ASegment.Col, ASegment, ASegment.SectionIndex, Annotation, amIntersect));
        end;
      end;
    end;
  finally
    FScreenObject.CacheElevationArrays;
    FScreenObject.UpdateCellCache(CellList, EvaluatedAt, Orientation,
      AssignmentLocation);
  end;
end;

function TModflowDelegate.GetHorizontalIndexFromLocation(const X: Real;
  const Grid: TCustomGrid): integer;
begin
  result := -1;
  case FScreenObject.ViewDirection of
    vdTop: Assert(False);
    vdFront: result := FScreenObject.GetACol(Grid, X);
    vdSide: result := FScreenObject.GetARow(Grid, X);
    else Assert(False);
  end;
end;

procedure TModflowDelegate.GetRotatedPoints(var RotatedPoints: TEdgePointArray);
var
  Index: Integer;
begin

  // Get the coordinates of the points.
  SetLength(RotatedPoints, FScreenObject.Count);
  case FScreenObject.ViewDirection of
    vdTop: Assert(False);
    vdFront:
      begin
        for Index := 0 to FScreenObject.Count - 1 do
        begin
          RotatedPoints[Index] := FScreenObject.EdgePoints[Index];
        end;
      end;
    vdSide:
      begin
        for Index := 0 to FScreenObject.Count - 1 do
        begin
          RotatedPoints[Index].X := FScreenObject.EdgePoints[Index].Y;
          RotatedPoints[Index].Y := FScreenObject.EdgePoints[Index].X;
          RotatedPoints[Index].Position :=
            FScreenObject.EdgePoints[Index].Position;
        end;
      end;
    else Assert(False);
  end;
end;

procedure TModflowDelegate.GetSideCellsToAssign(const Grid: TCustomGrid;
  const DataSetFunction: string; OtherData: TObject; const DataSet: TDataArray;
  CellList: TCellAssignmentList; AssignmentLocation: TAssignmentLocation);
var
  EvalAt: TEvaluatedAt;
  Orientation: TDataSetOrientation;
begin
  Assert(CellList.Count = 0);
  if DataSet = nil then
  begin
    EvalAt := eaBlocks;
    Orientation := dso3D;
  end
  else
  begin
    EvalAt := DataSet.EvaluatedAt;
    Orientation := DataSet.Orientation;
  end;
  GetCellsToAssign(Grid, DataSetFunction, OtherData, EvalAt, CellList,
    AssignmentLocation, Orientation);
end;

procedure TModflowDelegate.GetTopCellsToAssign(const Grid: TCustomGrid;
  const DataSetFunction: string; OtherData: TObject; const DataSet: TDataArray;
  CellList: TCellAssignmentList; AssignmentLocation: TAssignmentLocation);
var
  EvalAt: TEvaluatedAt;
  Orientation: TDataSetOrientation;
begin
  Assert(CellList <> nil);
  Assert(CellList.Count = 0);
  if DataSet = nil then
  begin
    EvalAt := eaBlocks;
    Orientation := dso3D;
  end
  else
  begin
    EvalAt := DataSet.EvaluatedAt;
    Orientation := DataSet.Orientation;
  end;
  GetCellsToAssign(Grid, DataSetFunction, OtherData, EvalAt, CellList,
    AssignmentLocation, Orientation);
end;

procedure TModflowDelegate.GetHorizontalLimits(const Grid: TCustomGrid;
  var HorizontalLimit: Integer; var PerpendicularLimit: Integer);
begin
  case FScreenObject.ViewDirection of
    vdTop: Assert(False);
    vdFront:
      begin
        PerpendicularLimit := Grid.RowCount - 1;
        HorizontalLimit := Grid.ColumnCount;
      end;
    vdSide:
      begin
        PerpendicularLimit := Grid.ColumnCount - 1;
        HorizontalLimit := Grid.RowCount;
      end;
  end;
end;

function TModflowDelegate.GetColOrRow(const Grid: TCustomGrid;
  const APoint: TEdgePoint;
  const GridMinHorizontal, GridMaxHorizontal: Real): integer;
begin
  result := -1;
  if APoint.X < GridMinHorizontal then
  begin

    case FScreenObject.ViewDirection of
      vdTop: Assert(False);
      vdFront: result := -1;
      vdSide: result := Grid.RowCount;
      else Assert(false);
    end;
  end
  else if APoint.X > GridMaxHorizontal then
  begin
    case FScreenObject.ViewDirection of
      vdTop: Assert(False);
      vdFront: result := Grid.ColumnCount;
      vdSide: result := -1;
      else Assert(false);
    end;
  end
  else
  begin
    case FScreenObject.ViewDirection of
      vdTop: Assert(False);
      vdFront: result := FScreenObject.GetACol(Grid, APoint.X);
      vdSide: result := FScreenObject.GetARow(Grid, APoint.X);
      else Assert(false);
    end;
  end;
end;

procedure TModflowDelegate.GetFrontCellsToAssign(const Grid: TCustomGrid;
  const DataSetFunction: string; OtherData: TObject; const DataSet: TDataArray;
  CellList: TCellAssignmentList; AssignmentLocation: TAssignmentLocation);
var
  EvalAt: TEvaluatedAt;
  Orientation: TDataSetOrientation;
begin
  Assert(CellList.Count = 0);
  if DataSet = nil then
  begin
    EvalAt := eaBlocks;
    Orientation := dso3D;
  end
  else
  begin
    EvalAt := DataSet.EvaluatedAt;
    Orientation := DataSet.Orientation;
  end;
  GetCellsToAssign(Grid, DataSetFunction, OtherData, EvalAt, CellList,
    AssignmentLocation, Orientation);
end;

procedure TModflowDelegate.GetVerticalLimitsOfGrid(const Grid: TCustomGrid;
  out GridMinZ, GridMaxZ: Real);
var
  Temp: real;
begin
  if Grid.LayerCount >= 0 then
  begin
    GridMaxZ := Grid.HighestElevation;
    GridMinZ := Grid.LowestElevation;
  end
  else
  begin
    GridMaxZ := 0;
    GridMinZ := 0;
  end;
  if GridMinZ > GridMaxZ then
  begin
    Temp := GridMinZ;
    GridMinZ := GridMaxZ;
    GridMaxZ := Temp;
  end;
end;

procedure TModflowDelegate.GetHorizontalLimitsOfGrid(const Grid: TCustomGrid;
  out GridMinHorizontal, GridMaxHorizontal: real);
var
  Temp: real;
begin
  // Get the box outlining the grid.
  case FScreenObject.ViewDirection of
    vdTop: Assert(False);
    vdFront:
      begin
        if Grid.ColumnCount >= 0 then
        begin
          GridMaxHorizontal := Grid.ColumnPosition[Grid.ColumnCount];
          GridMinHorizontal := Grid.ColumnPosition[0];
        end
        else
        begin
          GridMaxHorizontal := 0;
          GridMinHorizontal := 0;
        end;
      end;
    vdSide:
      begin
        if Grid.RowCount >= 0 then
        begin
          GridMaxHorizontal := Grid.RowPosition[0];
          GridMinHorizontal := Grid.RowPosition[Grid.RowCount];
        end
        else
        begin
          GridMaxHorizontal := 0;
          GridMinHorizontal := 0;
        end;
      end;
  end;

  if GridMinHorizontal > GridMaxHorizontal then
  begin
    Temp := GridMinHorizontal;
    GridMinHorizontal := GridMaxHorizontal;
    GridMaxHorizontal := Temp;
  end;
end;

// CellOutlines is a 2D array of TPoint2D.
// The outline for a cell will be the points at
// @orderedList(
//   @item([Column*2, Layer])
//   @item([Column*2+1, Layer])
//   @item([Column*2+2, Layer])
//   @item([Column*2, Layer+1])
//   @item([Column*2+1, Layer+1])
//   @item([Column*2+2, Layer+1])
// )
function TModflowDelegate.FindLayer(const ColOrRow: integer;
  const Location: TEdgePoint; const CellOutlines: T2DRealPointArray;
  const MovingUp: boolean): integer;
var
  LeftPoint, RightPoint, CenterPoint: TPoint2D;
  Indicator: integer;
  LayerLimit: integer;
  Y: real;
  FirstIndex, LastIndex, MiddleIndex: integer;
begin
  Assert(CellOutlines <> nil);
  Assert(ColOrRow*2+2 < Length(CellOutlines));
  Assert(ColOrRow >= 0);
  Assert(Length(CellOutlines) > 0);
  LayerLimit := Length(CellOutlines[0]);
  Assert(LayerLimit > 0);
  case FScreenObject.ViewDirection of
    vdTop: Assert(False);
    vdFront:
      begin
        LeftPoint := CellOutlines[ColOrRow*2,0];
        RightPoint := CellOutlines[ColOrRow*2+2,0];
      end;
    vdSide:
      begin
        LeftPoint := CellOutlines[ColOrRow*2+2,0];
        RightPoint := CellOutlines[ColOrRow*2,0];
      end;
    else Assert(False);
  end;
  Assert((Location.X >= LeftPoint.x) and (Location.X <= RightPoint.x));
  CenterPoint := CellOutlines[ColOrRow*2+1,0];
  Indicator := -1;
  // Indicator indicates which half of the cell outline X is in.
  case FScreenObject.ViewDirection of
    vdTop: Assert(False);
    vdFront:
      begin
        if Location.X >= CenterPoint.x then
        begin
          Indicator := ColOrRow*2+1;
        end
        else
        begin
          Indicator := ColOrRow*2;
        end;
      end;
    vdSide:
      begin
        if Location.X >= CenterPoint.x then
        begin
          Indicator := ColOrRow*2;
        end
        else
        begin
          Indicator := ColOrRow*2+1;
        end;
      end;
    else Assert(False);
  end;
  // Check if Location is above the top of the grid.
  LeftPoint := CellOutlines[Indicator,0];
  RightPoint := CellOutlines[Indicator+1,0];
  Y := YIntersection(LeftPoint, RightPoint, Location.X);
  if Y < Location.Y then
  begin
    result := -1;
    Exit;
  end;
  // Check if Location is below the bottom of the grid.
  LeftPoint := CellOutlines[Indicator,LayerLimit-1];
  RightPoint := CellOutlines[Indicator+1,LayerLimit-1];
  Y := YIntersection(LeftPoint, RightPoint, Location.X);
  if Y > Location.Y then
  begin
    result := LayerLimit;
    Exit;
  end;
  FirstIndex := 0;
  LastIndex := LayerLimit-1;
  While (LastIndex - FirstIndex > 1) do
  begin
    MiddleIndex := (FirstIndex + LastIndex) div 2;
    LeftPoint := CellOutlines[Indicator,MiddleIndex];
    RightPoint := CellOutlines[Indicator+1,MiddleIndex];
    Y := YIntersection(LeftPoint, RightPoint, Location.X);

    if Y < Location.Y then
    begin
      LastIndex := MiddleIndex;
    end
    else if Y > Location.Y then
    begin
      FirstIndex := MiddleIndex;
    end
    else if MovingUp then
    begin
      LastIndex := MiddleIndex;
    end
    else
    begin
      FirstIndex := MiddleIndex;
    end;
  end;
  result := FirstIndex;
end;

function TModflowDelegate.OutlineVertex(const ColOrRow, Layer,
  PointIndex: integer; const CellOutlines: T2DRealPointArray): TPoint2D;
begin
  case PointIndex of
    0,6:
      begin
        result :=CellOutlines[ColOrRow*2,Layer];
      end;
    1:
      begin
        result :=CellOutlines[ColOrRow*2+1,Layer];
      end;
    2:
      begin
        result :=CellOutlines[ColOrRow*2+2,Layer];
      end;
    3:
      begin
        result :=CellOutlines[ColOrRow*2+2,Layer+1];
      end;
    4:
      begin
        result :=CellOutlines[ColOrRow*2+1,Layer+1];
      end;
    5:
      begin
        result :=CellOutlines[ColOrRow*2,Layer+1];
      end;
    else Assert(False);
  end;
end;

function TModflowDelegate.CountIntersectPointsOnEdge(
  const ColOrRow, Layer, EdgeIndex: integer;
  const FirstPoint, SecondPoint: TPoint2D;
  const CellOutlines: T2DRealPointArray;
  out PointsOnEdge: TRealPointArray): integer;
var
  EdgeSegment: TSegment2D;
  ObjectSegment: TSegment2D;
  PointIndex: integer;
  function Inside(X, X1, X2: real): boolean;
  begin
    result := ((X < X1) and (X > X2)) or ((X > X1) and (X < X2))
  end;
begin
  ObjectSegment := EquateSegment(FirstPoint, SecondPoint);
  EdgeSegment := EquateSegment(OutlineVertex(ColOrRow, Layer, EdgeIndex,
    CellOutlines), OutlineVertex(ColOrRow, Layer, EdgeIndex+1, CellOutlines));
  if not Intersect(ObjectSegment, EdgeSegment) then
  begin
    result := 0;
  end
  else if not Parallel(ObjectSegment, EdgeSegment) then
  begin
    result := 1;
    SetLength(PointsOnEdge, Result);
    PointsOnEdge[0] := IntersectionPoint(ObjectSegment, EdgeSegment);
  end
  else
  begin
    result := 2;
    PointIndex := -1;
    SetLength(PointsOnEdge, Result);
    if IsEqual(ObjectSegment[1], ObjectSegment[2]) then
    begin
      PointsOnEdge[0] := ObjectSegment[1];
      SetLength(PointsOnEdge, 1);
      result := 1;
      Exit;
    end
    else if IsEqual(EdgeSegment[1], EdgeSegment[2]) then
    begin
      PointsOnEdge[0] := EdgeSegment[1];
      result := 1;
      Exit;
    end;
    // At this point we know that neither segment is degenerate.
    if IsEqual(EdgeSegment[1], ObjectSegment[1]) or
      IsEqual(EdgeSegment[1], ObjectSegment[2]) then
    begin
      Inc(PointIndex);
      PointsOnEdge[PointIndex] := EdgeSegment[1];
    end;
    if IsEqual(EdgeSegment[2], ObjectSegment[1]) or
      IsEqual(EdgeSegment[2], ObjectSegment[2]) then
    begin
      Inc(PointIndex);
      PointsOnEdge[PointIndex] := EdgeSegment[2];
      if PointIndex = 1 then
      begin
        Exit;
      end;
    end;
    if Inside(EdgeSegment[1].X, ObjectSegment[1].x, ObjectSegment[2].x)
      or Inside(EdgeSegment[1].Y, ObjectSegment[1].Y, ObjectSegment[2].Y) then
    begin
      Inc(PointIndex);
      Assert(PointIndex < 2);
      PointsOnEdge[PointIndex] := EdgeSegment[1];
    end;
    if Inside(EdgeSegment[2].X, ObjectSegment[1].x, ObjectSegment[2].x)
      or Inside(EdgeSegment[2].Y, ObjectSegment[1].Y, ObjectSegment[2].Y) then
    begin
      Inc(PointIndex);
      Assert(PointIndex < 2);
      PointsOnEdge[PointIndex] := EdgeSegment[2];
    end;
    if Inside(ObjectSegment[1].X, EdgeSegment[1].x, EdgeSegment[2].x)
      or Inside(ObjectSegment[1].Y, EdgeSegment[1].Y, EdgeSegment[2].Y) then
    begin
      Inc(PointIndex);
      Assert(PointIndex < 2);
      PointsOnEdge[PointIndex] := ObjectSegment[1];
    end;
    if Inside(ObjectSegment[2].X, EdgeSegment[1].x, EdgeSegment[2].x)
      or Inside(ObjectSegment[2].Y, EdgeSegment[1].Y, EdgeSegment[2].Y) then
    begin
      Inc(PointIndex);
      Assert(PointIndex < 2);
      PointsOnEdge[PointIndex] := ObjectSegment[2];
    end;
    if PointIndex = 0 then 
    begin
      SetLength(PointsOnEdge, 1);
      result := 1;
    end;
    Assert(PointIndex in [0,1]);
  end;
end;

function TModflowDelegate.FindIntersectionSurface(
  const FirstCol, LastCol, LayerIndex: integer;
  const PreviousPoint, APoint: TEdgePoint; var IntersectionLocation: TEdgePoint;
  const CellOutlines: T2DRealPointArray;  Out Col: integer): boolean;
var
  ObjectSegment, EdgeSegment: TSegment2D;
  ColIndex: integer;
  function Intersects: boolean;
    procedure SetOutput;
    var
      Int: TPoint2D;
    begin
      Int := IntersectionPoint(ObjectSegment, EdgeSegment);
      IntersectionLocation.X := Int.x;
      IntersectionLocation.Y := Int.y;
      Col := ColIndex;
      result := True;
    end;
  begin
    result := False;
    EdgeSegment := EquateSegment(CellOutlines[ColIndex*2,LayerIndex],
      CellOutlines[ColIndex*2+1,LayerIndex]);
    if Intersect(ObjectSegment, EdgeSegment) then
    begin
      SetOutput;
      Exit;
    end;
    EdgeSegment := EquateSegment(CellOutlines[ColIndex*2+1,LayerIndex],
      CellOutlines[ColIndex*2+2,LayerIndex]);
    if Intersect(ObjectSegment, EdgeSegment) then
    begin
      SetOutput;
      Exit;
    end;
  end;
begin
  ObjectSegment := EquateSegment(PreviousPoint.X, PreviousPoint.Y,
    APoint.X, APoint.Y);
  result := False;
  if FirstCol < LastCol then
  begin
    for ColIndex := FirstCol to LastCol do
    begin
      result := Intersects;
      if result then Exit;
    end;
  end
  else
  begin
    for ColIndex := FirstCol downto LastCol do
    begin
      result := Intersects;
      if result then Exit;
    end;
  end;
end;

function TModflowDelegate.FindLayerOnEdge(const APoint,
  PreviousPoint: TEdgePoint; var IntersectionLocation: TEdgePoint;
  const CellOutline: T2DRealPointArray; const EdgeIndex: integer): integer;
var
  FirstIndex, LastIndex, MiddleIndex: integer;
  ObjectSegment, EdgeSegment: TSegment2D;
  Int: TPoint2D;
  Candidates: array[0..3] of TPoint2D;
  CandidateCount: integer;
  CandidateIndex: integer;
  Separation, TestSeparation: real;
  function IsBetween(const X, X1, X2: Real): boolean;
  begin
    result := ((X1 >= X) and (X >= X2)) or ((X1 <= X) and (X <= X2));
  end;
begin
  FirstIndex := 0;
  LastIndex := Length(CellOutline[0]) -1;

  EdgeSegment := EquateSegment(CellOutline[EdgeIndex,0],
    CellOutline[EdgeIndex,LastIndex]);
  ObjectSegment := EquateSegment(PreviousPoint.X, PreviousPoint.Y,
    APoint.X, APoint.Y);

  if Parallel(EdgeSegment, ObjectSegment) then
  begin
    CandidateCount := 0;
    if IsBetween(ObjectSegment[1].Y, EdgeSegment[1].Y, EdgeSegment[2].Y) then
    begin
      Candidates[CandidateCount] := ObjectSegment[1];
      Inc(CandidateCount);
    end;
    if IsBetween(ObjectSegment[2].Y, EdgeSegment[1].Y, EdgeSegment[2].Y) then
    begin
      Candidates[CandidateCount] := ObjectSegment[2];
      Inc(CandidateCount);
    end;
    if IsBetween(EdgeSegment[1].Y, ObjectSegment[1].Y, ObjectSegment[2].Y) then
    begin
      Candidates[CandidateCount] := EdgeSegment[1];
      Inc(CandidateCount);
    end;
    if IsBetween(EdgeSegment[2].Y, ObjectSegment[1].Y, ObjectSegment[2].Y) then
    begin
      Candidates[CandidateCount] := EdgeSegment[2];
      Inc(CandidateCount);
    end;
    if CandidateCount > 0 then
    begin
      Int := Candidates[0];
      Separation := FastGeo.Distance(ObjectSegment[1], Int);
      for CandidateIndex := 1 to CandidateCount - 1 do
      begin
        TestSeparation := FastGeo.Distance(ObjectSegment[1],
          Candidates[CandidateIndex]);
        if TestSeparation < Separation then
        begin
          Separation := TestSeparation;
          Int := Candidates[CandidateIndex];
        end;
      end;
    end
    else
    begin
      Int := ObjectSegment[1];
    end;
  end
  else
  begin
    Int := IntersectionPoint(EdgeSegment, ObjectSegment);
  end;
  IntersectionLocation.X := Int.X;
  IntersectionLocation.Y := Int.Y;

  while LastIndex - FirstIndex > 1 do
  begin
    MiddleIndex := (FirstIndex + LastIndex) div 2;
    if IntersectionLocation.Y > CellOutline[EdgeIndex,MiddleIndex].Y then
    begin
      LastIndex := MiddleIndex;
    end
    else
    begin
      FirstIndex := MiddleIndex;
    end;
  end;
  result := FirstIndex;
end;

function TModflowDelegate.InCell(const ColOrRow, Layer: integer;
  const TestPoint: TEdgePoint; Const CellOutlines: T2DRealPointArray): boolean;
var
  VertexIndex: integer;
  APoint, AnotherPoint: TPoint2D;
begin
  result := false;

  for VertexIndex := 0 to 5 do
  begin
    APoint := OutlineVertex(ColOrRow, Layer, VertexIndex, CellOutlines);
    AnotherPoint := OutlineVertex(ColOrRow, Layer, VertexIndex+1, CellOutlines);
    if ((TestPoint.Y <= APoint.Y) = (TestPoint.Y > AnotherPoint.Y)) and
      (TestPoint.X - APoint.X - (TestPoint.Y - APoint.Y) *
      (AnotherPoint.X - APoint.X) /
      (AnotherPoint.Y - APoint.Y) < 0) then
    begin
      result := not result;
    end;
  end;
end;

procedure TModflowDelegate.InitializeExpression(out Compiler: TRbwParser;
  out DataSetFunction: string; out Expression: TExpression;
  const DataSet: TDataArray; const OtherData: TObject);
var
  DataObject: TModflowDataObject;
  ResultTypeOK: Boolean;
  NameToDisplay: string;
  TypeToCheck: TRbwDataType;
begin
  if OtherData = nil then
  begin
    inherited;
  end
  else
  begin
    DataObject := OtherData as TModflowDataObject;
    Compiler := DataObject.Compiler;
    DataSetFunction := DataObject.DataSetFunction;

    try
      Compiler.Compile(DataSetFunction);
    except on E: ERbwParserError do
      begin
        if DataSet <> nil then
        begin
          NameToDisplay := DataSet.Name;
        end
        else
        begin
          NameToDisplay := DataObject.AlternateName;
        end;
        frmFormulaErrors.AddError(FScreenObject.Name,
          NameToDisplay, DataSetFunction, 'Invalid Formula');
        DataSetFunction := '0';
        Compiler.Compile(DataSetFunction);
      end;
    end;
    Expression := Compiler.CurrentExpression;
    if DataSet <> nil then
    begin
      TypeToCheck := DataSet.Datatype;
    end
    else
    begin
      TypeToCheck := DataObject.AlternateDataType;
    end;
    ResultTypeOK := (Expression.ResultType = TypeToCheck)
      or ((Expression.ResultType = rdtInteger)
      and (TypeToCheck = rdtDouble));
    if not ResultTypeOK then
    begin
      raise EInvalidDataType.Create('Invalid data type.');
    end; 
  end;
end;

function TModflowDelegate.GetCellOutlines(const Grid: TCustomGrid;
  const RowOrCol: integer): T2DRealPointArray;
begin
  case FScreenObject.ViewDirection of
    vdTop: Assert(False);
    vdFront: result := (Grid as TModflowGrid).FrontCellPoints(RowOrCol);
    vdSide: result := (Grid as TModflowGrid).SideCellPoints(RowOrCol);
    else Assert(False);
  end;
end;

procedure TModflowDelegate.UpdateFrontSegments(const Grid: TCustomGrid;
      const EvaluatedAt: TEvaluatedAt);
begin
  UpdateSegments(Grid, EvaluatedAt);
end;

procedure TModflowDelegate.UpdateSegments(const Grid: TCustomGrid;
  const EvaluatedAt: TEvaluatedAt);
var
  RotatedPoints: TEdgePointArray;
  GridMaxHorizontal, GridMinHorizontal, GridMaxZ, GridMinZ, Temp: real;
  Index: integer;
  APoint, PreviousPoint, NewPoint: TEdgePoint;
  HorizontalIndex, LayerIndex: integer;
  FirstCol: integer;
  LastCol: integer;
  PerpendicularIndex: integer;
  ASegment: TCellElementSegment;
  PerpendicularLimit: integer;
  FirstLayer, LastLayer, FirstHorizontalIndex, LastHorizontalIndex: integer;
  TempI: integer;
  TempPoints1, TempPoints2: TEdgePointArray;
  NextHorizontalIndex, NextLayer: array [0..11] of integer;
  PointIndex: integer;
  Row1, Row2, Layer1, Layer2: integer;
  Position: TEdgePosition;
  Point1, Point2: TEdgePoint;
  RealPoint: TPoint2D;
  ACell: T2DFrontCell;
  IncreasingX, IncreasingY: boolean;
  CellOutlines: T2DRealPointArray;
  MaxXIndex, MaxYIndex: integer;
  Y: real;
  FirstPoint, SecondPoint: TPoint2D;
  PointsOnEdge: TRealPointArray;
  EdgeIndex: Integer;
  PointCount: Integer;
  FirstPointInCell, TestPoint: TEdgePoint;
  MinDistance: real;
  FoundEdgePoint: boolean;
  LayerIndicator: integer;
  ObjectSegment, EdgeSegment: TSegment2D;
  Int: TPoint2D;
  EpsilonX, EpsilonY: Real;
  OtherEdgeAssigned: Boolean;
  CornerPoint: TPoint2D;
  SegmentDirection: TSegmentDirection;
  HorizontalLimit: Integer;
  PointDistance: Real;
  RotatedPointIndex: Integer;
  ASegment2: TCellElementSegment;
  SectionIndex: integer;
  EndSection: boolean;
  SecIndex: integer;
  IncrementedSectionIndex: Boolean;
  procedure AssignEpsilon;
  var
    MinX, MaxX, MinY, MaxY: Real;
    PointIndex: integer;
  begin
    if PointCount > 0 then
    begin
      MinX := TempPoints1[0].X;
      MaxX := TempPoints1[0].X;
      MinY := TempPoints1[0].Y;
      MaxY := TempPoints1[0].Y;
      for PointIndex := 1 to PointCount - 1 do
      begin
        if MinX > TempPoints1[PointIndex].X then
        begin
          MinX := TempPoints1[PointIndex].X;
        end
        else if MaxX < TempPoints1[PointIndex].X then
        begin
          MaxX := TempPoints1[PointIndex].X;
        end;
        if MinY > TempPoints1[PointIndex].Y then
        begin
          MinY := TempPoints1[PointIndex].Y;
        end
        else if MaxY < TempPoints1[PointIndex].Y then
        begin
          MaxY := TempPoints1[PointIndex].Y;
        end;
      end;
      EpsilonX := (MaxX-MinX)/10000;
      EpsilonY := (MaxY-MinY)/10000;
    end
    else
    begin
      EpsilonX := 0.0;
      EpsilonY := 0.0;
    end;
  end;
  function HandleTopLeftCorner: boolean;
  begin
    CornerPoint := OutlineVertex(HorizontalIndex, LayerIndex, 0,
      CellOutlines);
    result := (TempPoints1[PointCount].X = CornerPoint.X)
      and (TempPoints1[PointCount].Y = CornerPoint.Y);
    if result then
    begin
      Case SegmentDirection of
        sdUpRight:
          begin
            NextHorizontalIndex[PointCount] := HorizontalIndex;
            NextLayer[PointCount] := LayerIndex -1;
          end;
        sdUpLeft:
          begin
            case FScreenObject.ViewDirection of
              vdTop: Assert(False);
              vdFront: NextHorizontalIndex[PointCount] := HorizontalIndex-1;
              vdSide: NextHorizontalIndex[PointCount] := HorizontalIndex+1;
              else Assert(False);
            end;
            NextLayer[PointCount] := LayerIndex -1;
          end;
        sdUp:
          begin
            NextHorizontalIndex[PointCount] := HorizontalIndex;
            NextLayer[PointCount] := LayerIndex -1;
          end;
        sdDownRight:
          begin
            // special case
            case FScreenObject.ViewDirection of
              vdTop: Assert(False);
              vdFront: NextHorizontalIndex[PointCount] := HorizontalIndex+1;
              vdSide: NextHorizontalIndex[PointCount] := HorizontalIndex-1;
              else Assert(False);
            end;
            NextLayer[PointCount] := LayerIndex;
          end;
        sdDownLeft:
          begin
            case FScreenObject.ViewDirection of
              vdTop: Assert(False);
              vdFront: NextHorizontalIndex[PointCount] := HorizontalIndex-1;
              vdSide: NextHorizontalIndex[PointCount] := HorizontalIndex+1;
              else Assert(False);
            end;
            NextLayer[PointCount] := LayerIndex;
          end;
        sdDown:
          begin
            NextHorizontalIndex[PointCount] := HorizontalIndex;
            NextLayer[PointCount] := LayerIndex+1;
          end
      End;
    end
  end;
  function HandleTopRightCorner: boolean;
  begin
    // Top right corner
    CornerPoint := OutlineVertex(HorizontalIndex, LayerIndex, 2,
      CellOutlines);
    result := (TempPoints1[PointCount].X = CornerPoint.X)
      and (TempPoints1[PointCount].Y = CornerPoint.Y);
    if result then
    begin
      Case SegmentDirection of
        sdUpRight:
          begin
            case FScreenObject.ViewDirection of
              vdTop: Assert(False);
              vdFront: NextHorizontalIndex[PointCount] := HorizontalIndex+1;
              vdSide: NextHorizontalIndex[PointCount] := HorizontalIndex-1;
              else Assert(False);
            end;
            NextLayer[PointCount] := LayerIndex -1;
          end;
        sdUpLeft:
          begin
            NextHorizontalIndex[PointCount] := HorizontalIndex;
            NextLayer[PointCount] := LayerIndex -1;
          end;
        sdUp:
          begin
            NextHorizontalIndex[PointCount] := HorizontalIndex;
            NextLayer[PointCount] := LayerIndex -1;
          end;
        sdDownRight:
          begin
            case FScreenObject.ViewDirection of
              vdTop: Assert(False);
              vdFront: NextHorizontalIndex[PointCount] := HorizontalIndex+1;
              vdSide: NextHorizontalIndex[PointCount] := HorizontalIndex-1;
              else Assert(False);
            end;
            NextLayer[PointCount] := LayerIndex;
          end;
        sdDownLeft:
          begin
            // special case
            case FScreenObject.ViewDirection of
              vdTop: Assert(False);
              vdFront: NextHorizontalIndex[PointCount] := HorizontalIndex-1;
              vdSide: NextHorizontalIndex[PointCount] := HorizontalIndex+1;
              else Assert(False);
            end;
            NextLayer[PointCount] := LayerIndex;
          end;
        sdDown:
          begin
            NextHorizontalIndex[PointCount] := HorizontalIndex;
            NextLayer[PointCount] := LayerIndex+1;
          end
      End;
    end
  end;
  function  HandleBottomRightCorner: boolean;
  begin
    // bottom right corner
    CornerPoint := OutlineVertex(HorizontalIndex, LayerIndex, 3,
      CellOutlines);
    result := (TempPoints1[PointCount].X = CornerPoint.X)
      and (TempPoints1[PointCount].Y = CornerPoint.Y);
    if result then
    begin
      Case SegmentDirection of
        sdUpRight:
          begin
            case FScreenObject.ViewDirection of
              vdTop: Assert(False);
              vdFront: NextHorizontalIndex[PointCount] := HorizontalIndex+1;
              vdSide: NextHorizontalIndex[PointCount] := HorizontalIndex-1;
              else Assert(False);
            end;
            NextLayer[PointCount] := LayerIndex;
          end;
        sdUpLeft:
          begin
            // special case
            case FScreenObject.ViewDirection of
              vdTop: Assert(False);
              vdFront: NextHorizontalIndex[PointCount] := HorizontalIndex-1;
              vdSide: NextHorizontalIndex[PointCount] := HorizontalIndex+1;
              else Assert(False);
            end;
            NextLayer[PointCount] := LayerIndex;
          end;
        sdUp:
          begin
            NextHorizontalIndex[PointCount] := HorizontalIndex;
            NextLayer[PointCount] := LayerIndex-1;
          end;
        sdDownRight:
          begin
            case FScreenObject.ViewDirection of
              vdTop: Assert(False);
              vdFront: NextHorizontalIndex[PointCount] := HorizontalIndex+1;
              vdSide: NextHorizontalIndex[PointCount] := HorizontalIndex-1;
              else Assert(False);
            end;
            NextLayer[PointCount] := LayerIndex+1;
          end;
        sdDownLeft:
          begin
            NextHorizontalIndex[PointCount] := HorizontalIndex;
            NextLayer[PointCount] := LayerIndex+1;
          end;
        sdDown:
          begin
            NextHorizontalIndex[PointCount] := HorizontalIndex;
            NextLayer[PointCount] := LayerIndex+1;
          end
      End;
    end
  end;
  function HandleBottomLeftCorner: boolean;
  begin
    // bottom left corner
    CornerPoint := OutlineVertex(HorizontalIndex, LayerIndex, 5,
      CellOutlines);
    result := (TempPoints1[PointCount].X = CornerPoint.X)
      and (TempPoints1[PointCount].Y = CornerPoint.Y);
    if result then
    begin
      Case SegmentDirection of
        sdUpRight:
          begin
            // special case
            case FScreenObject.ViewDirection of
              vdTop: Assert(False);
              vdFront: NextHorizontalIndex[PointCount] := HorizontalIndex+1;
              vdSide: NextHorizontalIndex[PointCount] := HorizontalIndex-1;
              else Assert(False);
            end;
            NextLayer[PointCount] := LayerIndex;
          end;
        sdUpLeft:
          begin
            case FScreenObject.ViewDirection of
              vdTop: Assert(False);
              vdFront: NextHorizontalIndex[PointCount] := HorizontalIndex-1;
              vdSide: NextHorizontalIndex[PointCount] := HorizontalIndex+1;
              else Assert(False);
            end;
            NextLayer[PointCount] := LayerIndex;
          end;
        sdUp:
          begin
            NextHorizontalIndex[PointCount] := HorizontalIndex;
            NextLayer[PointCount] := LayerIndex-1;
          end;
        sdDownRight:
          begin
            NextHorizontalIndex[PointCount] := HorizontalIndex;
            NextLayer[PointCount] := LayerIndex+1;
          end;
        sdDownLeft:
          begin
            case FScreenObject.ViewDirection of
              vdTop: Assert(False);
              vdFront: NextHorizontalIndex[PointCount] := HorizontalIndex-1;
              vdSide: NextHorizontalIndex[PointCount] := HorizontalIndex+1;
              else Assert(False);
            end;
            NextLayer[PointCount] := LayerIndex+1;
          end;
        sdDown:
          begin
            NextHorizontalIndex[PointCount] := HorizontalIndex;
            NextLayer[PointCount] := LayerIndex+1;
          end
      End;
    end
  end;
begin
  FScreenObject.FSegments.Clear;
  Assert(EvaluatedAt = eaBlocks);

  GetHorizontalLimits(Grid, HorizontalLimit, PerpendicularLimit);
  SetLength(TempPoints1, 12);

  GetRotatedPoints(RotatedPoints);

  GetHorizontalLimitsOfGrid(Grid, GridMinHorizontal, GridMaxHorizontal);
  GetVerticalLimitsOfGrid(Grid, GridMinZ, GridMaxZ);

  // loop over vertices
  SectionIndex := 0;
  for Index := 0 to FScreenObject.Count - 1 do
  begin
    if Index > FScreenObject.SectionStart[SectionIndex] then
    begin
      PreviousPoint := APoint;
    end;
    for PerpendicularIndex := 0 to PerpendicularLimit do
    begin
      CellOutlines := GetCellOutlines(Grid, PerpendicularIndex);
      Assert(SectionIndex < FScreenObject.SectionCount);

      APoint := RotatedPoints[Index];
      if (SectionIndex < FScreenObject.SectionCount)
        and (FScreenObject.SectionLength[SectionIndex] = 1) then
      begin
        if (APoint.X >= GridMinHorizontal) and (APoint.X <= GridMaxHorizontal)
          and (APoint.Y >= GridMinZ) and (APoint.Y <= GridMaxZ) then
        begin
          HorizontalIndex := GetHorizontalIndexFromLocation(APoint.X, Grid);

          LayerIndex := FindLayer(HorizontalIndex, APoint, CellOutlines, False);
          if (LayerIndex >= 0) and (LayerIndex < Grid.LayerCount) then
          begin
            CreateSegment(APoint, APoint, LayerIndex, PerpendicularIndex,
              HorizontalIndex, Index, SectionIndex);
          end;
        end;
        if PerpendicularIndex = PerpendicularLimit then
        begin
          Inc(SectionIndex);
        end;
      end
      else
      begin
        EndSection := False;
        // get a line segment and identify the cells along it.
        if (Index = FScreenObject.SectionStart[SectionIndex]) then
        begin
          Continue;
        end;
        IncrementedSectionIndex := False;
        if (Index = FScreenObject.SectionEnd[SectionIndex]) then
        begin
          Inc(SectionIndex);
          EndSection := True;
          IncrementedSectionIndex := True;
        end;

        FirstPointInCell := PreviousPoint;
        FirstHorizontalIndex := GetColOrRow(Grid, PreviousPoint,
          GridMinHorizontal, GridMaxHorizontal);
        LastHorizontalIndex := GetColOrRow(Grid, APoint,
          GridMinHorizontal, GridMaxHorizontal);

        // If the line segment is completely outside the grid,
        // skip this segment.
        if (FirstHorizontalIndex < 0) and (LastHorizontalIndex < 0) then
        begin
          Continue;
        end;
        if (FirstHorizontalIndex >= HorizontalLimit)
          and (LastHorizontalIndex >= HorizontalLimit) then
        begin
          Continue;
        end;

        MaxXIndex := Length(CellOutlines)-1;
        MaxYIndex := Length(CellOutlines[0])-1;
        // Identify first cell intersected by segment - if there is one.
        if FirstHorizontalIndex < 0 then
        begin
          FirstHorizontalIndex := 0;
          if LastHorizontalIndex >= HorizontalLimit then
          begin
            LastHorizontalIndex := HorizontalLimit -1;
          end;
          // Check if the segment intersects the edge and HorizontalIndex = 0.
          // If if doesn't, check the
          // top or bottom surface (depending on whether the segment passes
          // above or below the edge.
          EdgeSegment := EquateSegment(CellOutlines[0,0],
            CellOutlines[0,MaxYIndex]);
          ObjectSegment := EquateSegment(PreviousPoint.X, PreviousPoint.Y,
            APoint.X, APoint.Y);
          Y := IntersectionPoint(EdgeSegment, ObjectSegment).Y;
          if Y > CellOutlines[0,0].Y then
          begin
            // Segment goes above the top of the edge
            if not FindIntersectionSurface(FirstHorizontalIndex,
              LastHorizontalIndex, 0, PreviousPoint, APoint,
              FirstPointInCell, CellOutlines, HorizontalIndex) then
            begin
              // It never intersects the top surface so go on to the
              // next segment in the TScreenObject
              Continue;
            end;
            LayerIndex := 0;
          end
          else if Y < CellOutlines[0,MaxYIndex].Y then
          begin
            // Segment goes below the bottom of the edge
            if not FindIntersectionSurface(FirstHorizontalIndex,
              LastHorizontalIndex, MaxYIndex, PreviousPoint, APoint,
              FirstPointInCell, CellOutlines, HorizontalIndex) then
            begin
              // It never intersects the bottom surface so go on to the
              // next segment in the TScreenObject
              Continue;
            end;
            LayerIndex := Grid.LayerCount -1;
          end
          else
          begin
            // Segment intersects the edge
            HorizontalIndex := 0;
            LayerIndex := FindLayerOnEdge(APoint, PreviousPoint,
              FirstPointInCell, CellOutlines, 0);
          end;
        end
        else if FirstHorizontalIndex >= HorizontalLimit then
        begin
          FirstHorizontalIndex := HorizontalLimit -1;
          if LastHorizontalIndex < 0 then
          begin
            LastHorizontalIndex := 0;
          end;
          // Check if the segment intersects the edge
          // at HorizontalIndex = HorizontalLimit-1.
          // If if doesn't, check the
          // top or bottom surface (depending on whether the segment passes
          // above or below the edge.
          EdgeSegment := EquateSegment(CellOutlines[MaxXIndex,0],
            CellOutlines[MaxXIndex,MaxYIndex]);
          ObjectSegment := EquateSegment(PreviousPoint.X, PreviousPoint.Y,
            APoint.X, APoint.Y);
          Y := IntersectionPoint(EdgeSegment, ObjectSegment).Y;
          if Y > CellOutlines[MaxXIndex,0].Y then
          begin
            // Segment goes above the top of the edge
            if not FindIntersectionSurface(FirstHorizontalIndex,
              LastHorizontalIndex, 0, PreviousPoint, APoint,
              FirstPointInCell, CellOutlines, HorizontalIndex) then
            begin
              // It never intersects the top surface so go on to the
              // next segment in the TScreenObject
              Continue;
            end;
            LayerIndex := 0;
          end
          else if Y < CellOutlines[MaxXIndex,MaxYIndex].Y then
          begin
            // Segment goes below the bottom of the edge
            if not FindIntersectionSurface(FirstHorizontalIndex,
              LastHorizontalIndex, MaxYIndex, PreviousPoint, APoint,
              FirstPointInCell, CellOutlines, HorizontalIndex) then
            begin
              // It never intersects the bottom surface so go on to the
              // next segment in the TScreenObject
              Continue;
            end;
            LayerIndex := Grid.LayerCount -1;
          end
          else
          begin
            // Segment intersects the edge
            HorizontalIndex := Grid.ColumnCount-1;
            LayerIndex := FindLayerOnEdge(APoint, PreviousPoint,
              FirstPointInCell, CellOutlines, HorizontalIndex*2+2);
          end;
        end
        else
        begin
          // The first point is inside the left and right boundaries
          // of the grid.  However, it may be above or below the grid.
          if LastHorizontalIndex >= HorizontalLimit then
          begin
            LastHorizontalIndex := HorizontalLimit -1;
          end
          else if LastHorizontalIndex < 0 then
          begin
            LastHorizontalIndex := 0;
          end;
          // identify the layer for the column containg the first point
          LayerIndex := FindLayer(FirstHorizontalIndex, PreviousPoint,
            CellOutlines, APoint.Y > PreviousPoint.Y);
          if LayerIndex < 0 then
          begin
            // First point is above the top of the grid.
            if not FindIntersectionSurface(FirstHorizontalIndex,
              LastHorizontalIndex, 0, PreviousPoint, APoint,
              FirstPointInCell, CellOutlines, HorizontalIndex) then
            begin
              // It never intersects the top surface so go on to the
              // next segment in the TScreenObject
              Continue;
            end;
            LayerIndex := 0;
          end
          else if LayerIndex >= Grid.LayerCount then
          begin
            // First point is below the bottom of the grid.
            if not FindIntersectionSurface(FirstHorizontalIndex,
              LastHorizontalIndex, MaxYIndex, PreviousPoint, APoint,
              FirstPointInCell, CellOutlines, HorizontalIndex) then
            begin
              // It never intersects the bottom surface so go on to the
              // next segment in the TScreenObject
              Continue;
            end;
            LayerIndex := Grid.LayerCount -1;
          end
          else
          begin
            // The first point is inside the grid.
              HorizontalIndex := FirstHorizontalIndex;
          end;
        end;

        if APoint.Y > PreviousPoint.Y then
        begin
          if APoint.X > PreviousPoint.X then
          begin
            SegmentDirection := sdUpRight;
          end
          else if APoint.X < PreviousPoint.X then
          begin
            SegmentDirection := sdUpLeft;
          end
          else
          begin
            SegmentDirection := sdUp;
          end;
        end
        else
        begin
          if APoint.X > PreviousPoint.X then
          begin
            SegmentDirection := sdDownRight;
          end
          else if APoint.X < PreviousPoint.X then
          begin
            SegmentDirection := sdDownLeft;
          end
          else
          begin
            SegmentDirection := sdDown;
          end;
        end;

        // HorizontalIndex and LayerIndex now indicate the first cell intersected by
        // the segment;  (If no cell intersects the segment, you don't get here.
        // FirstPointInCell is the location where the segment intersects the
        // cell.
        repeat
          for PointIndex := 0 to 11 do
          begin
            NextHorizontalIndex[PointIndex] := -1;
            NextLayer[PointIndex] := -1;
          end;
          PointCount := 0;
          TempPoints1[0] := FirstPointInCell;
          HandleTopLeftCorner;
          HandleTopRightCorner;
          HandleBottomRightCorner;
          HandleBottomLeftCorner;
          Inc(PointCount);

          TempPoints1[1] := APoint;
          HandleTopLeftCorner;
          HandleTopRightCorner;
          HandleBottomRightCorner;
          HandleBottomLeftCorner;
          Inc(PointCount);

          FirstPoint := EquatePoint(PreviousPoint.X, PreviousPoint.Y);
          SecondPoint := EquatePoint(APoint.X, APoint.Y);
          for EdgeIndex := 0 to 5 do
          begin
            if CountIntersectPointsOnEdge(HorizontalIndex, LayerIndex,
              EdgeIndex, FirstPoint, SecondPoint, CellOutlines,
              PointsOnEdge) > 0 then
            begin
              for PointIndex := 0 to Length(PointsOnEdge) - 1 do
              begin
                Assert(PointCount < 12);
                TempPoints1[PointCount].X := PointsOnEdge[PointIndex].X;
                TempPoints1[PointCount].Y := PointsOnEdge[PointIndex].Y;
                // One of these points will be the last point in the
                // current cell.  Store the indices of the neighboring cell
                // so it can be easily identified when we get to it.
                case EdgeIndex of
                  0, 1: // Top edge
                    begin
                      if not HandleTopLeftCorner
                        and not HandleTopRightCorner then
                      begin
                        NextHorizontalIndex[PointCount] := HorizontalIndex;
                        NextLayer[PointCount] := LayerIndex -1;
                      end;
                    end;
                  2: // left edge
                    begin
                      if not HandleTopRightCorner
                        and not HandleBottomRightCorner then
                      begin
                        NextHorizontalIndex[PointCount] := HorizontalIndex+1;
                        NextLayer[PointCount] := LayerIndex;
                      end;
                    end;
                  3,4: // bottom edge
                    begin
                      if not HandleBottomRightCorner
                        and not HandleBottomLeftCorner then
                      begin
                        NextHorizontalIndex[PointCount] := HorizontalIndex;
                        NextLayer[PointCount] := LayerIndex +1;
                      end;
                    end;
                  5: // right edge
                    begin
                      if not HandleBottomLeftCorner
                        and not HandleTopLeftCorner then
                      begin
                        NextHorizontalIndex[PointCount] := HorizontalIndex-1;
                        NextLayer[PointCount] := LayerIndex;
                      end;
                    end;
                  else Assert(False);
                end;
                Inc(PointCount);
              end;
            end;
          end;

          // First get appropriate epsilons (margin of error).
          AssignEpsilon;

          FScreenObject.SortPoints(TempPoints1, TempPoints2,
            APoint, PreviousPoint, PointCount, EpsilonX, EpsilonY);
          // Look at each segment defined by a pair of points in TempPoints2.
          // Skip segments that occur before FirstPointInCell.
          // Start checking the midpoints of the remainder of the segments.
          // If the midepont is in the cell, include that segment.
          // Continue until you either reach the end
          // of the array or have reached a segment that isn't in the cell.
          MinDistance := Distance(PreviousPoint, FirstPointInCell);
          for PointIndex := 0 to Length(TempPoints2) - 2 do
          begin
            Point1 := TempPoints2[PointIndex];
            PointDistance := Distance(PreviousPoint, Point1);
            if (PointDistance >= MinDistance) or
              IsEqual(MinDistance, PointDistance, Max(EpsilonX, EpsilonY)) then
            begin
              Point2 := TempPoints2[PointIndex+1];
              TestPoint := MidPoint(Point1,Point2);
              if InCell(HorizontalIndex, LayerIndex, TestPoint,
                CellOutlines) then
              begin
                if EndSection then
                begin
                  SecIndex := SectionIndex-1;
                end
                else
                begin
                  SecIndex := SectionIndex;
                end;
                CreateSegment(Point1, Point2, LayerIndex,
                  PerpendicularIndex, HorizontalIndex, Index - 1, SecIndex);
                FirstPointInCell := Point2;
              end
              else
              begin
                break;
              end;
            end;
          end;
          // At this point, the TCellElementSegment's for the current cell
          // intersected by this segment of the TScreenObject have been created.
          // The next thing to do is to identify the next cell intersected by
          // this segment of the TScreenObject (if there is one).
          // FirstPointInCell has been updated to be the last point in the
          // previous cell.  It may either be the last point in this segment
          // of the TScreenObject or a point of intersection between
          // the cell outline and the segment of the  TScreenObject.
          // The intersection points have been stored in TempPoints1.
          // For each intersection point in TempPoints1,
          // the next cell has been indicated by
          // the corresponding values in NextHorizontalIndex and NextLayer.

          // If FirstPointInCell is the last point of this segment of the
          // TScreenObject, go to the next segment.
          if IsEqual(FirstPointInCell.X, APoint.X, 0)
            and IsEqual(FirstPointInCell.Y, APoint.Y, 0) then
          begin
            break;
          end;

          // The last point in the cell must be on the edge of the cell.
          // Identify the next cell.
          FoundEdgePoint := False;
          for PointIndex := 0 to PointCount - 1 do
          begin
            if IsEqual(FirstPointInCell.X, TempPoints1[PointIndex].X, EpsilonX)
              and IsEqual(FirstPointInCell.Y, TempPoints1[PointIndex].Y,
              EpsilonY) then
            begin
              FoundEdgePoint := True;
              HorizontalIndex := NextHorizontalIndex[PointIndex];
              LayerIndex := NextLayer[PointIndex];
              break;
            end;
          end;
          Assert(FoundEdgePoint);

          if (HorizontalIndex < 0) or (HorizontalIndex >= HorizontalLimit) then
          begin
            // The segment is extending outside the grid and can not re-enter it
            // so go on to the next segment.
            break;
          end;
          if (LayerIndex < 0) or (LayerIndex >= Grid.LayerCount) then
          begin
            // Segment is extending outside the grid.  It might or might not
            // re-enter the grid.  It might re-enter the grid in the
            // other half of the same cell or in a different cell.

            // Indicate top or bottom surface.
            if LayerIndex < 0 then
            begin
              LayerIndicator := 0;
            end
            else
            begin
              LayerIndicator := Grid.LayerCount;
            end;

            // Test if it re-enters the other half of the same cell.
            ObjectSegment := EquateSegment(PreviousPoint.X, PreviousPoint.Y,
              APoint.X, APoint.Y);
            OtherEdgeAssigned := False;
            if (FirstPointInCell.X >
              CellOutlines[HorizontalIndex*2+1,LayerIndicator].X)
             and (APoint.X < PreviousPoint.X) then
            begin
              EdgeSegment :=
                EquateSegment(CellOutlines[HorizontalIndex*2+1,LayerIndicator],
                CellOutlines[HorizontalIndex*2+2,LayerIndicator]);
              OtherEdgeAssigned := True;
            end
            else if (FirstPointInCell.X <
              CellOutlines[HorizontalIndex*2+1,LayerIndicator].X)
             and (APoint.X > PreviousPoint.X) then
            begin
              EdgeSegment :=
                EquateSegment(CellOutlines[HorizontalIndex*2+1,LayerIndicator],
                CellOutlines[HorizontalIndex*2,LayerIndicator]);
              OtherEdgeAssigned := True;
            end;
            if OtherEdgeAssigned and Intersect(ObjectSegment,EdgeSegment) then
            begin
              Int := IntersectionPoint(ObjectSegment,EdgeSegment);
              FirstPointInCell.X := Int.X;
              FirstPointInCell.Y := Int.Y;
              // The segment of the TScreenObject has re-entered the
              // other half of the same cell so you don't need to
              // update HorizontalIndex.
            end
            else
            begin
              UpdateHorizontalRangeOfCellsToCheck(FirstHorizontalIndex,
                LastHorizontalIndex, HorizontalIndex, HorizontalLimit,
                APoint, PreviousPoint);
              // Don't try to test beyond the edge of the grid
              if (FirstHorizontalIndex < 0)
                or (FirstHorizontalIndex >= HorizontalLimit) then
              begin
                break;
              end;

              // If it doesn't re-enter the grid, go on to next segment
              // of the TScreenObject.
              if not FindIntersectionSurface(FirstHorizontalIndex,
                LastHorizontalIndex, LayerIndicator, PreviousPoint, APoint,
                FirstPointInCell, CellOutlines, HorizontalIndex) then
              begin
                break;
              end;
            end;
            // At this point LayerIndex is outside the valid range.
            // it indicates a a layer above the top layer or below
            // the bottom layer.  Fix it so it is within the grid.
            if LayerIndex < 0 then
            begin
              LayerIndex := 0;
            end
            else
            begin
              LayerIndex := Grid.LayerCount-1;
            end;
            // If you get here, the current segment of TScreenObject
            // has reenterd the grid.
            // FirstPointInCell, ColIndex and LayerIndex have
            // all been updated to the values they will have for the next cell.
          end;

          // At this point, the next cell intersected by
          // the current segment of TScreenObject is identified by
          // HorizontalIndex and LayerIndex
          // The first point at which the segment intersects
          // that cell is indicated by FirstPointInCell.
        until (False);
        if PerpendicularIndex <> PerpendicularLimit then
        begin
          if IncrementedSectionIndex then
          begin
            Dec(SectionIndex);
          end;
        end;
      end;
    end;
  end;
  FScreenObject.FSegments.UpToDate := True;
  case FScreenObject.ElevationCount of
    ecZero:
      begin
        // do nothing
      end;
    ecOne:
      begin
        FScreenObject.FHigher3DElevationsNeedsUpdating := True;
      end;
    ecTwo:
      begin
        FScreenObject.FHigher3DElevationsNeedsUpdating := True;
        FScreenObject.FLower3DElevationsNeedsUpdating := True;
      end;
    else Assert(False);
  end;
end;

procedure TModflowDelegate.UpdateSideSegments(const Grid: TCustomGrid;
  const EvaluatedAt: TEvaluatedAt);
begin
  UpdateSegments(Grid, EvaluatedAt);
end;

{ TScreenObject }

function TScreenObject.AddDataSet(const DataSet: TDataArray): Integer;
var
  Item: TInterpValuesItem;
  MixtureSubscription: TObserver;
  FormulaSubscription: TObserver;
begin
  result := IndexOfDataSet(DataSet);
  if (result < 0) and CanAddDataSet(DataSet) then
  begin
    MixtureSubscription := nil;
    FormulaSubscription := nil;
    if FCanInvalidateModel then
    begin
      MixtureSubscription := TObserver.Create(nil);
      MixtureSubscription.UpdateWithName(DataSet.Name + Name + 'Mixture');
      FDataSetMixtureSubscriptions.Add(MixtureSubscription);

      InvalidateModel;
      FormulaSubscription := TObserver.Create(nil);
      FormulaSubscription.UpdateWithName(DataSet.Name + Name);
      FDataSetSubscriptions.Add(FormulaSubscription);
    end;
    result := FDataSets.Add(DataSet);
    FDataSetFormulas.Add(nil);
    // DataSet is notified if the TScreenObject changes.
    if FCanInvalidateModel then
    begin
      self.TalksTo(DataSet);
      // DataSet is notified if the formula for it changes.
      FormulaSubscription.TalksTo(DataSet);
      // The formula is notified if the TScreenObject changes.
      self.TalksTo(FormulaSubscription);
      case ElevationCount of
        ecZero:
          begin
            // do nothing
          end;
        ecOne:
          begin
            // DataSet is notified if the elevation formula changes.
            if FElevSubscription = nil then
            begin
              CreateElevationSubscription;
            end;
            FElevSubscription.TalksTo(DataSet);
          end;
        ecTwo:
          begin
            // DataSet is notified if the elevation formulas changes.
            if FTopElevSubscription = nil then
            begin
              CreateTopElevationSubscription;
            end;
            FTopElevSubscription.TalksTo(DataSet);

            if FBottomElevSubscription = nil then
            begin
              CreateBottomElevationSubscription;
            end;
            FBottomElevSubscription.TalksTo(DataSet);
          end;
      else
        Assert(False);
      end;
      DataSet.Invalidate;

      MixtureSubscription.TalksTo(DataSet);
      self.TalksTo(MixtureSubscription);
    end;

    if FIsUpdating then
    begin
      if DataSet is TCustomPhastDataSet then
      begin
        Item := FInterpValues.Items[result] as TInterpValuesItem;
        Item.Values.DataSet := TCustomPhastDataSet(DataSet);
        Item.Values.MixtureFormula := '0.5';
      end;
    end
    else
    begin
      Item := FInterpValues.Add as TInterpValuesItem;
      if DataSet is TCustomPhastDataSet then
      begin
        Item.Values.Assign(DataSet);
      end;
    end;
  end;
end;

procedure TScreenObject.AssignRealDataWithPhastInterpolation(
  const DataSet: TDataArray; const LayerIndex, RowIndex, ColIndex: integer;
  const Comment: string; const InterpValue: TInterpValuesItem);
var
  Distance, Fraction: double;
begin
  if DataSet is TRealPhastDataSet then
  begin
    with TRealPhastDataSet(DataSet) do
    begin
      CellDistance1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance1;
      CellDistance2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance2;
      CellInterpolationDirection[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.InterpolationDirection;
      IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.UsePHAST_Interpolation;
      Annotation[LayerIndex, RowIndex, ColIndex] := Comment;
      //'Set by PHAST-style interpolation: ' + Name;
      CellValue1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.RealValue1;
      CellValue2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.RealValue2;
    end;
  end
  else if DataSet is TSparseRealPhastDataSet then
  begin
    with TSparseRealPhastDataSet(DataSet) do
    begin
      CellDistance1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance1;
      CellDistance2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance2;
      CellInterpolationDirection[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.InterpolationDirection;
      IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.UsePHAST_Interpolation;
      Annotation[LayerIndex, RowIndex, ColIndex] := Comment;
      //'Set by PHAST-style interpolation: ' + Name;
      CellValue1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.RealValue1;
      CellValue2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.RealValue2;
    end;
  end
  else
  begin
    Assert(False);
  end;

  Assert(InterpValue.Values.InterpolationDirection in [pidX, pidY, pidZ]);
  GetInterpDistance(InterpValue, Distance, DataSet,
    LayerIndex, RowIndex, ColIndex);

  if Distance <= InterpValue.Values.Distance1 then
  begin
    DataSet.RealData[LayerIndex, RowIndex, ColIndex] :=
      InterpValue.Values.RealValue1;
  end
  else if Distance >= InterpValue.Values.Distance2 then
  begin
    DataSet.RealData[LayerIndex, RowIndex, ColIndex] :=
      InterpValue.Values.RealValue2;
  end
  else
  begin
    Fraction := 1 - (Distance - InterpValue.Values.Distance1) /
      (InterpValue.Values.Distance2 - InterpValue.Values.Distance1);
    DataSet.RealData[LayerIndex, RowIndex, ColIndex] :=
      Fraction * InterpValue.Values.RealValue1 + (1 - Fraction) *
      InterpValue.Values.RealValue2;
  end;
end;

procedure TScreenObject.AssignIntegerDataWithPhastInterpolation(
  const DataSet: TDataArray; const LayerIndex, RowIndex, ColIndex: integer;
  const Comment: string; const InterpValue: TInterpValuesItem);
var
  Distance, Fraction: double;
  RealValue: double;
begin
  if DataSet is TIntegerPhastDataSet then
  begin
    with TIntegerPhastDataSet(DataSet) do
    begin
      CellDistance1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance1;
      CellDistance2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance2;
      CellInterpolationDirection[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.InterpolationDirection;
      IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.UsePHAST_Interpolation;
      Annotation[LayerIndex, RowIndex, ColIndex] := Comment;
      CellValue1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.IntValue1;
      CellValue2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.IntValue2;
    end;
  end
  else if DataSet is TSparseIntegerPhastDataSet then
  begin
    with TSparseIntegerPhastDataSet(DataSet) do
    begin
      CellDistance1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance1;
      CellDistance2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.Distance2;
      CellInterpolationDirection[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.InterpolationDirection;
      IsInterpolatedCell[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.UsePHAST_Interpolation;
      Annotation[LayerIndex, RowIndex, ColIndex] := Comment;
      CellValue1[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.IntValue1;
      CellValue2[LayerIndex, RowIndex, ColIndex] :=
        InterpValue.Values.IntValue2;
    end;
  end
  else
  begin
    Assert(false);
  end;
  GetInterpDistance(InterpValue, Distance, DataSet,
    LayerIndex, RowIndex, ColIndex);

  if Distance <= InterpValue.Values.Distance1 then
  begin
    DataSet.IntegerData[LayerIndex, RowIndex, ColIndex] :=
      InterpValue.Values.IntValue1;
    Fraction := 1;
  end
  else if Distance >= InterpValue.Values.Distance2 then
  begin
    DataSet.IntegerData[LayerIndex, RowIndex, ColIndex] :=
      InterpValue.Values.IntValue2;
    Fraction := 0;
  end
  else
  begin
    Fraction := 1 - (Distance - InterpValue.Values.Distance1) /
      (InterpValue.Values.Distance2 - InterpValue.Values.Distance1);
    RealValue := Fraction * InterpValue.Values.IntValue1 + (1 - Fraction) *
      InterpValue.Values.IntValue2;
    DataSet.IntegerData[LayerIndex, RowIndex, ColIndex] :=
      Round(RealValue);
  end;
  // Fraction is needed in all cases in order to read
  // TSparseIntegerPhastInterpolationDataSet.RealValue
  // or TIntegerPhastDataSet.RealValue
  if DataSet is TIntegerPhastDataSet then
  begin
    TIntegerPhastDataSet(DataSet).Fraction[
      LayerIndex, RowIndex, ColIndex] := Fraction;
  end
  else if DataSet is TSparseIntegerPhastDataSet then
  begin
    TSparseIntegerPhastDataSet(DataSet).Fraction[
      LayerIndex, RowIndex, ColIndex] := Fraction
  end;
end;

procedure TScreenObject.DeleteDataSet(const Index: Integer);
var
  FormulaSubscription, MixtureSubscription: TObserver;
  DataSet: TDataArray;
  FormulaObject: TFormulaObject;
begin
  // Get rid of any subscriptions due to the formula.
  MixtureDataSetFormula[Index] := '0';

  if FCanInvalidateModel then
  begin
    MixtureSubscription := FDataSetMixtureSubscriptions[Index] as TObserver;
    DataSet := DataSets[Index];
    self.StopsTalkingTo(MixtureSubscription);
    MixtureSubscription.StopsTalkingTo(DataSet);
    FDataSetMixtureSubscriptions.Delete(Index);
  end;

  // Get rid of any subscriptions due to the formula.
  DataSetFormulas[Index] := '0';

  DataSet := nil;
  if FCanInvalidateModel then
  begin
    FormulaSubscription := FDataSetSubscriptions[Index] as TObserver;
    DataSet := DataSets[Index];
    self.StopsTalkingTo(DataSet);
    FormulaSubscription.StopsTalkingTo(DataSet);
    self.StopsTalkingTo(FormulaSubscription);
    FDataSetSubscriptions.Delete(Index);
    DataSet.Invalidate;
  end;
  FDataSets.Delete(Index);
  FormulaObject := FDataSetFormulas[Index];
  frmGoPhast.PhastModel.FormulaManager.Remove(FormulaObject,
    GlobalRemoveScreenObjectDataArraySubscription,
    GlobalRestoreDataArraySubscription, self);
  FDataSetFormulas.Delete(Index);
  if FCanInvalidateModel then
  begin
    if FElevSubscription <> nil then
    begin
      FElevSubscription.StopsTalkingTo(DataSet);
    end;
    if FTopElevSubscription <> nil then
    begin
      FTopElevSubscription.StopsTalkingTo(DataSet);
    end;
    if FBottomElevSubscription <> nil then
    begin
      FBottomElevSubscription.StopsTalkingTo(DataSet);
    end;
  end;
  InvalidateModel;

  FInterpValues.Delete(Index);
end;

procedure TScreenObject.SetIFACE(const Value: TIface);
begin
  if FIFACE <> Value then
  begin
    InvalidateModel;
    FIFACE := Value;
  end;
end;

procedure TScreenObject.SetImportedHigherSectionElevations(
  const Value: TValueArrayStorage);
begin
  Assert((Value = nil) or (Value.DataType = rdtDouble));
  SetValueStorageField(Value, FImportedHigherSectionElevations);
end;

procedure TScreenObject.SetImportedLowerSectionElevations(
  const Value: TValueArrayStorage);
begin
  Assert((Value = nil) or (Value.DataType = rdtDouble));
  SetValueStorageField(Value, FImportedLowerSectionElevations);
end;

procedure TScreenObject.SetImportedSectionElevations(
  const Value: TValueArrayStorage);
begin
  Assert((Value = nil) or (Value.DataType = rdtDouble));
  SetValueStorageField(Value, FImportedSectionElevations);
end;

procedure TScreenObject.SetImportedValues(const Value: TValueCollection);
begin
  FImportedValues.Assign(Value);
  InvalidateModel;
end;

procedure TScreenObject.SetInterpValues(
  const Value: TInterpValuesCollection);
begin
  FInterpValues.Assign(Value);
  InvalidateModel;
end;

procedure TScreenObject.InitializeExpression(out Compiler: TRbwParser;
  out DataSetFormula: string; out Expression: TExpression;
  const DataSet: TDataArray; const OtherData: TObject);
  procedure NormalHandling;
  var
    ResultTypeOK: boolean;
    DI: integer;
    IsBoundary: boolean;
  begin
    Compiler := GetCompiler(DataSet.Orientation);
    DI := IndexOfDataSet(DataSet);
    if DI >= 0 then
    begin
      DataSetFormula := DataSetFormulas[DI];
      IsBoundary := False;
    end
    else
    begin
      IsBoundary := True;
      DI := IndexOfBoundaryDataSet(DataSet);
      Assert(DI >= 0);
      DataSetFormula := BoundaryDataSetFormulas[DI];
    end;

    try
      Compiler.Compile(DataSetFormula);
    except on E: ERbwParserError do
      begin
        ResetScreenObjectFunction(DI, self, Compiler,
          DataSet.DataType, E.Message, IsBoundary);
      end;
    end;
    Expression := Compiler.CurrentExpression;
    ResultTypeOK := (Expression.ResultType = DataSet.Datatype)
      or ((Expression.ResultType = rdtInteger)
      and (DataSet.Datatype = rdtDouble));
    if not ResultTypeOK then
    begin
      raise EInvalidDataType.Create('Invalid data type.');
    end;
  end;
var
  InterpValue: TInterpValuesItem;
  LocalExpression: string;
  ResultTypeOK: boolean;
  Model: TPhastModel;
begin
  Model := FModel as TPhastModel;
  if (DataSet = Model.TopBoundaryType)
    or (DataSet = Model.FrontBoundaryType)
    or (DataSet = Model.SideBoundaryType) then
  begin
    Compiler := GetCompiler(DataSet.Orientation);
    DataSetFormula := ThreeDBoundaryFormula;
    Compiler.Compile(DataSetFormula);
    Expression := Compiler.CurrentExpression;
    Exit;
  end;

  if (DataSet = Model.Top2DBoundaryType) then
  begin
    Compiler := GetCompiler(DataSet.Orientation);
    DataSetFormula := TwoDBoundaryFormula;
    Compiler.Compile(DataSetFormula);
    Expression := Compiler.CurrentExpression;
    Exit;
  end;

  InterpValue := OtherData as TInterpValuesItem;
  if (InterpValue = nil) then
  begin
    NormalHandling;
  end
  else if InterpValue is TCustomPhastBoundaryCondition then
  begin
    if TCustomPhastBoundaryCondition(InterpValue).UsePHAST_Interpolation then
    begin
      Compiler := nil;
      Expression := nil;
      DataSetFormula := '';
    end
    else
    begin
      Compiler := GetCompiler(DataSet.Orientation);
      try
        LocalExpression := TCustomPhastBoundaryCondition(InterpValue).
          FormulaExpression;
        Compiler.Compile(LocalExpression);
      except on E: ERbwParserError do
        begin
          LocalExpression := '0';
          TCustomPhastBoundaryCondition(InterpValue).FormulaExpression :=
            LocalExpression;
          Compiler.Compile(LocalExpression);
        end;
      end;
      DataSetFormula := LocalExpression;
      Expression := Compiler.CurrentExpression;
      ResultTypeOK := (Expression.ResultType = DataSet.Datatype)
        or ((Expression.ResultType = rdtInteger) and (DataSet.Datatype =
        rdtDouble));
      if not ResultTypeOK then
      begin
        raise EInvalidDataType.Create('Invalid data type.');
      end;
    end;
  end
  else if not InterpValue.Values.UsePHAST_Interpolation then
  begin
    NormalHandling;
  end
  else
  begin
    Compiler := nil;
    Expression := nil;
    DataSetFormula := '';
  end;
end;

function TScreenObject.GetSubPolygonCount: integer;
begin
  if FSubPolygons = nil then
  begin
    result := 0;
  end
  else
  begin
    result := FSubPolygons.Count;
  end;
end;

function TScreenObject.GetTopElevSubscription: TObserver;
begin
  if FTopElevSubscription = nil then
  begin
    CreateTopElevationSubscription;
  end;
  result := FTopElevSubscription
end;

function TScreenObject.GetWellBoundary: TWellBoundary;
begin
  CreatePhastWellBoundary;
  result := FWellBoundary;
end;

function TScreenObject.GetSubPolygon(Index: integer): TSubPolygon;
begin
  Assert(FSubPolygons <> nil);
  result := FSubPolygons[Index];
end;

procedure TScreenObject.SetFluxBoundary(const Value: TFluxBoundary);
begin
  if Value = nil then
  begin
    FreeAndNil(FFluxBoundary);
  end
  else
  begin
    CreatePhastFluxBoundary;
    FFluxBoundary.Assign(Value);
  end;
  InvalidateModel;
end;

procedure TScreenObject.SetLeakyBoundary(const Value: TLeakyBoundary);
begin
  if Value = nil then
  begin
    FreeAndNil(FLeakyBoundary);
  end
  else
  begin
    CreatePhastLeakyBoundary;
    FLeakyBoundary.Assign(Value);
  end;
  InvalidateModel;
end;

procedure TScreenObject.SetRiverBoundary(const Value: TRiverBoundary);
begin
  if Value = nil then
  begin
    FreeAndNil(FRiverBoundary);
  end
  else
  begin
    CreatePhastRiverBoundary;
    FRiverBoundary.Assign(Value);
  end;
  InvalidateModel;
end;

procedure TScreenObject.SetSpecifiedSolutionBoundary(
  const Value: TSpecifiedSolutionBoundary);
begin
  CreatePhastSpecifiedSolutionBoundary;
  FSpecifiedSolutionBoundary.Assign(Value);
  InvalidateModel;
end;

procedure TScreenObject.SetWellBoundary(const Value: TWellBoundary);
begin
  if Value = nil then
  begin
    FreeAndNil(FWellBoundary);
  end
  else
  begin
    CreatePhastWellBoundary;
    FWellBoundary.Assign(Value);
  end;
  InvalidateModel;
end;

procedure TScreenObject.SetSpecifiedHeadBoundary(
  const Value: TSpecifiedHeadBoundary);
begin
  if Value = nil then
  begin
    FreeAndNil(FSpecifiedHeadBoundary);
  end
  else
  begin
    CreatePhastSpecifiedHeadBoundary;
    FSpecifiedHeadBoundary.Assign(Value);
  end;
  InvalidateModel;
end;

function TScreenObject.GetBottomElevSubscription: TObserver;
begin
  if FBottomElevSubscription = nil then
  begin
    CreateBottomElevationSubscription;
  end;
  result := FBottomElevSubscription;
end;

function TScreenObject.GetBoundaryCollection(
  const TimeList: TPhastTimeList): TCustomPhastBoundaryCollection;
var
  List: TList;
  Index: integer;
  Boundary: TCustomPhastBoundaryCollection;
begin
  result := nil;
  List := TList.Create;
  try
    List.Add(FluxBoundary.BoundaryValue);
    List.Add(FluxBoundary.Solution);
    List.Add(LeakyBoundary.BoundaryValue);
    List.Add(LeakyBoundary.Solution);
    List.Add(RiverBoundary.BoundaryValue);
    List.Add(RiverBoundary.Solution);
    List.Add(SpecifiedHeadBoundary.BoundaryValue);
    List.Add(SpecifiedHeadBoundary.Solution);
    List.Add(SpecifiedSolutionBoundary.Solution);
    List.Add(WellBoundary.BoundaryValue);
    List.Add(WellBoundary.Solution);
    for Index := 0 to List.Count - 1 do
    begin
      Boundary := List[Index];
      if Boundary.TimeList = TimeList then
      begin
        result := Boundary;
        Exit;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TScreenObject.ResetBoundaryMixtureSubscriptions;
var
  Index: integer;
  BoundaryCondition: TCustomPhastBoundaryCondition;
  List: TList;
  BoundaryIndex: integer;
  BoundaryCollection: TCustomPhastBoundaryCollection;
begin
  List := TList.Create;
  try
    List.Add(FluxBoundary.Solution);
    List.Add(LeakyBoundary.Solution);
    List.Add(RiverBoundary.Solution);
    List.Add(SpecifiedHeadBoundary.Solution);
    List.Add(SpecifiedSolutionBoundary.Solution);
    List.Add(WellBoundary.Solution);
    for BoundaryIndex := 0 to List.Count - 1 do
    begin
      BoundaryCollection := List[BoundaryIndex];
      for Index := 0 to BoundaryCollection.Count - 1 do
      begin
        BoundaryCondition := BoundaryCollection.Items[Index] as
          TCustomPhastBoundaryCondition;
        BoundaryCondition.ResetMixtureSubscription;
      end;
    end;
  finally
    List.Free;
  end;
end;

function TScreenObject.IsBoundaryTimeDataSetUsed(
  const DataSet: TDataArray; out OtherData: TObject): boolean;
var
  Index: integer;
  BoundaryCondition: TCustomPhastBoundaryCondition;
  TimeIndex: integer;
  List: TList;
  BoundaryIndex: integer;
  BoundaryCollection: TCustomPhastBoundaryCollection;
begin
  OtherData := nil;
  result := False;
  if DataSet = nil then
    Exit;
  if DataSet.DataType in [rdtBoolean, rdtString] then
    Exit;
  List := TList.Create;
  try
    case DataSet.DataType of
      rdtDouble:
        begin
          if FFluxBoundary <> nil then
          begin
            List.Add(FluxBoundary.BoundaryValue);
          end;
          if FLeakyBoundary <> nil then
          begin
            List.Add(LeakyBoundary.BoundaryValue);
          end;
          if FRiverBoundary <> nil then
          begin
            List.Add(RiverBoundary.BoundaryValue);
          end;
          if FSpecifiedHeadBoundary <> nil then
          begin
            List.Add(SpecifiedHeadBoundary.BoundaryValue);
          end;
          if FWellBoundary <> nil then
          begin
            List.Add(WellBoundary.BoundaryValue);
          end;
        end;
      rdtInteger:
        begin
          if FFluxBoundary <> nil then
          begin
            List.Add(FluxBoundary.Solution);
          end;
          if FLeakyBoundary <> nil then
          begin
            List.Add(LeakyBoundary.Solution);
          end;
          if FRiverBoundary <> nil then
          begin
            List.Add(RiverBoundary.Solution);
          end;
          if FSpecifiedHeadBoundary <> nil then
          begin
            List.Add(SpecifiedHeadBoundary.Solution);
          end;
          if FWellBoundary <> nil then
          begin
            List.Add(WellBoundary.Solution);
          end;
        end;
    else
      Assert(False);
    end;

    for BoundaryIndex := 0 to List.Count - 1 do
    begin
      BoundaryCollection := List[BoundaryIndex];
      for Index := 0 to BoundaryCollection.Count - 1 do
      begin
        BoundaryCondition := BoundaryCollection.Items[Index] as
          TCustomPhastBoundaryCondition;
        TimeIndex :=
          BoundaryCollection.TimeList.IndexOf(BoundaryCondition.Time);
        if TimeIndex >= 0 then
        begin
          if BoundaryCollection.TimeList.Items[TimeIndex] = DataSet then
          begin
            result := True;
            OtherData := BoundaryCondition;
            Exit;
          end;
        end;
      end;
    end;
  finally
    List.Free;
  end;
end;


function TScreenObject.StoreFlux: boolean;
begin
  result := (FluxBoundary.BoundaryValue.Count > 0)
    or (FluxBoundary.Solution.Count > 0);
end;

function TScreenObject.StoreImportedHigherSectionElevations: Boolean;
begin
  result := ImportedHigherSectionElevations.Count > 0;
end;

function TScreenObject.StoreImportedLowerSectionElevations: Boolean;
begin
  result := ImportedLowerSectionElevations.Count > 0;
end;

function TScreenObject.StoreImportedSectionElevations: Boolean;
begin
  result := ImportedSectionElevations.Count > 0;
end;

function TScreenObject.StoreImportedValues: Boolean;
begin
  result := ImportedValues.Count > 0;
end;

function TScreenObject.StoreLeaky: boolean;
begin
  result := (LeakyBoundary.BoundaryValue.Count > 0)
    or (LeakyBoundary.Solution.Count > 0);
end;

function TScreenObject.StoreModflowChdBoundary: Boolean;
begin
  result := (FModflowBoundaries <> nil)
    and (ModflowChdBoundary <> nil)
    and ModflowChdBoundary.Used;
end;

function TScreenObject.StoreModflowDrnBoundary: Boolean;
begin
  result := (FModflowBoundaries <> nil)
    and (ModflowDrnBoundary <> nil) and ModflowDrnBoundary.Used;
end;

function TScreenObject.StoreModflowDrtBoundary: Boolean;
begin
  result := (FModflowBoundaries <> nil)
    and (ModflowDrtBoundary <> nil) and ModflowDrtBoundary.Used;
end;

function TScreenObject.StoreModflowEtsBoundary: Boolean;
begin
  result := (FModflowBoundaries <> nil)
    and (ModflowEtsBoundary <> nil) and ModflowEtsBoundary.Used;
end;

function TScreenObject.StoreModflowEvtBoundary: Boolean;
begin
  result := (FModflowBoundaries <> nil)
    and (ModflowEvtBoundary <> nil) and ModflowEvtBoundary.Used;
end;

function TScreenObject.StoreModflowGhbBoundary: Boolean;
begin
  result := (FModflowBoundaries <> nil)
    and (ModflowGhbBoundary <> nil) and ModflowGhbBoundary.Used;
end;

function TScreenObject.StoreModflowHeadObservations: Boolean;
begin
  result := (FModflowBoundaries <> nil)
    and (ModflowHeadObservations <> nil) and ModflowHeadObservations.Used;
end;

function TScreenObject.StoreModflowLakBoundary: Boolean;
begin
  result := (FModflowBoundaries <> nil)
    and (ModflowLakBoundary <> nil) and ModflowLakBoundary.Used;
end;

function TScreenObject.StoreModflowRchBoundary: Boolean;
begin
  result := (FModflowBoundaries <> nil)
    and (ModflowRchBoundary <> nil) and ModflowRchBoundary.Used;
end;

function TScreenObject.StoreModflowResBoundary: Boolean;
begin
  result := (FModflowBoundaries <> nil)
    and (ModflowResBoundary <> nil) and ModflowResBoundary.Used;
end;

function TScreenObject.StoreModflowRivBoundary: Boolean;
begin
  result := (FModflowBoundaries <> nil)
    and (ModflowRivBoundary <> nil) and ModflowRivBoundary.Used;
end;

function TScreenObject.StoreModflowSfrBoundary: Boolean;
begin
  result := (FModflowBoundaries <> nil)
    and (ModflowSfrBoundary <> nil) and ModflowSfrBoundary.Used;
end;

function TScreenObject.StoreModflowStreamGage: Boolean;
begin
  result := (FModflowBoundaries <> nil)
    and (ModflowStreamGage <> nil) and ModflowStreamGage.Used;
end;

function TScreenObject.StoreModflowUzfBoundary: Boolean;
begin
  result := (FModflowBoundaries <> nil)
    and (ModflowUzfBoundary <> nil) and ModflowUzfBoundary.Used;
end;

function TScreenObject.StoreModflowWellBoundary: Boolean;
begin
  result := (FModflowBoundaries <> nil)
    and (ModflowWellBoundary <> nil) and ModflowWellBoundary.Used;
end;

function TScreenObject.StoreRiver: boolean;
begin
  result := (RiverBoundary.BoundaryValue.Count > 0)
    or (RiverBoundary.Solution.Count > 0);
end;

function TScreenObject.StoreSpecifiedHead: boolean;
begin
  result := (SpecifiedHeadBoundary.BoundaryValue.Count > 0)
    or (SpecifiedHeadBoundary.Solution.Count > 0);
end;

function TScreenObject.StoreWell: boolean;
begin
  result := (WellBoundary.BoundaryValue.Count > 0)
    or (WellBoundary.Solution.Count > 0);
end;

function TScreenObject.ThreeDBoundaryFormula: string;
begin
  if PhastBoundaryType in  [btNone, btSpecifiedHead, btFlux, btLeaky] then
  begin
    result := IntToStr(BoundaryType);
  end
  else
  begin
    result := '0';
  end;
end;

function TScreenObject.TwoDBoundaryFormula: string;
begin
  if PhastBoundaryType in  [btNone, btRiver, btWell] then
  begin
    result := IntToStr(BoundaryType);
  end
  else
  begin
    result := '0';
  end;
end;

procedure ResetScreenObjectMixtureFunction(const DataSetIndex: integer;
  const AScreenObject: TScreenObject; const Compiler: TRbwParser;
  const DataSetDataType: TRbwDataType; const ErrorMessage: string;
  const IsBoundary: boolean);
var
  ScreenObjectFunction: string;
begin
  if IsBoundary then
  begin
    Assert(False);
  end
  else
  begin
    ScreenObjectFunction :=
      AScreenObject.MixtureDataSetFormula[DataSetIndex];
    frmFormulaErrors.AddError(AScreenObject.Name,
      AScreenObject.DataSets[DataSetIndex].Name,
      ScreenObjectFunction, ErrorMessage);
  end;

  Assert(DataSetDataType = rdtInteger);
  ScreenObjectFunction := '0.5';
  AScreenObject.MixtureDataSetFormula[DataSetIndex] := ScreenObjectFunction;

  Compiler.Compile(ScreenObjectFunction);
end;

procedure TScreenObject.InitializeMixtureExpression(out Compiler:
  TRbwParser; out MixtureFormula: string; out Expression: TExpression;
  const DataSet: TDataArray; const OtherData: TObject);
var
  ResultTypeOK: boolean;
  DI: integer;
  IsBoundary: boolean;
  InterpValue: TInterpValuesItem;
  BoundaryCondition: TCustomPhastBoundaryCondition;
begin
  Compiler := GetCompiler(DataSet.Orientation);
  DI := IndexOfDataSet(DataSet);
  if DI >= 0 then
  begin
    InterpValue := OtherData as TInterpValuesItem;
    MixtureFormula := InterpValue.Values.MixtureFormula;
    IsBoundary := False;
  end
  else
  begin
    IsBoundary := True;
    BoundaryCondition := OtherData as TCustomPhastBoundaryCondition;
    MixtureFormula := BoundaryCondition.MixtureExpression;
    BoundaryCondition.FMixtureObserver.UpToDate := True;
  end;

  try
    Compiler.Compile(MixtureFormula);
  except on E: ERbwParserError do
    begin
      ResetScreenObjectMixtureFunction(DI, self, Compiler,
        DataSet.DataType, E.Message, IsBoundary);
    end;
  end;
  Expression := Compiler.CurrentExpression;
  ResultTypeOK := Expression.ResultType in [rdtInteger, rdtDouble];
  if not ResultTypeOK then
  begin
    raise EInvalidDataType.Create('Invalid data type.');
  end;
end;

procedure TScreenObject.AssignNumericValueToDataSet(const Grid: TCustomGrid;
  const DataSet: TDataArray; Value: double);
var
  CellList: TCellAssignmentList;
  Index: Integer;
  Cell: TCellAssignment;
begin
  CellList := TCellAssignmentList.Create;
  try
    GetCellsToAssign(Grid, FloatToStr(Value), nil, DataSet, CellList,
      alAll);
    for Index := 0 to CellList.Count - 1 do
    begin
      Cell := CellList[Index];
      DataSet.RealData[Cell.Layer, Cell.Row, Cell.Column] := Value;
      DataSet.Annotation[Cell.Layer, Cell.Row, Cell.Column] := Cell.Annotation;
    end;
  finally
    CellList.Free;
  end;
end;  

procedure TScreenObject.AssignValuesToModflowDataSet(const Grid: TCustomGrid;
  const DataSet: TDataArray; const Formula: string;
  AssignmentLocation: TAssignmentLocation = alAll);
var
  UsedVariables: TStringList;
  Compiler: TRbwParser;
  DataSetFunction: string;
  OtherData: TModflowDataObject;
begin
  UsedVariables := TStringList.Create;
  try
    Compiler := GetCompiler(DataSet.Orientation);
    DataSetFunction := Formula;
    case ViewDirection of
      vdTop:
        begin
          OtherData := TModflowDataObject.Create;
          try
            OtherData.Compiler := Compiler;
            OtherData.DataSetFunction := DataSetFunction;
            Delegate.AssignValuesToTopDataSet(Grid, DataSet, OtherData,
              AssignmentLocation);
          finally
            OtherData.Free;
          end;
        end;
      vdFront:
        begin
          OtherData := TModflowDataObject.Create;
          try
            OtherData.Compiler := Compiler;
            OtherData.DataSetFunction := DataSetFunction;
            Delegate.AssignValuesToFrontDataSet(Grid, DataSet, OtherData,
              AssignmentLocation);
          finally
            OtherData.Free;
          end;
        end;
      vdSide:
        begin
          OtherData := TModflowDataObject.Create;
          try
            OtherData.Compiler := Compiler;
            OtherData.DataSetFunction := DataSetFunction;
            Delegate.AssignValuesToSideDataSet(Grid, DataSet, OtherData,
              AssignmentLocation);
          finally
            OtherData.Free;
          end;
        end;
      else Assert(False);
    end;
  finally
    UsedVariables.Free;
    if FSegments.UpToDate and not FSegments.FCleared then
    begin
      FSegments.CacheData;
    end;
  end
end;

procedure TScreenObject.AssignValuesToPhastDataSet(const Grid: TCustomGrid;
  const DataSet: TDataArray);
var
  OtherData: TObject;
  InterpValue: TInterpValuesItem;
  Compiler: TRbwParser;
  MixtureFormula: string;
  Expression: TExpression;
  UsedVariables: TStringList;
  BoundaryDataSet: boolean;
  IsBoundaryDataSet: boolean;
  Model: TPhastModel;
  BoundaryCondition: TCustomPhastBoundaryCondition;
  BoundaryPosition: Integer;
  AnObserver: TObserver;
begin
  if not SetValuesOfIntersectedCells and not SetValuesOfEnclosedCells then
  begin
    Exit;
  end;
  OtherData := nil;
  Model := FModel as TPhastModel;
  // The boundary data sets should contain an integer value
  // that represents what type of boundary is being assiged.
  BoundaryDataSet := (DataSet = Model.TopBoundaryType)
    or (DataSet = Model.FrontBoundaryType)
    or (DataSet = Model.SideBoundaryType)
    or (DataSet = Model.Top2DBoundaryType);

  // if the screen object is deleted, or the data set is not used, quit.
  if Deleted or (DataSet = nil) or
    ((IndexOfDataSet(DataSet) < 0) and (IndexOfBoundaryDataSet(DataSet) < 0)
    and not IsBoundaryTimeDataSetUsed(DataSet, OtherData)
    and not BoundaryDataSet) then
  begin
    Exit;
  end;
  if BoundaryDataSet and (BoundaryType = 0) then
  begin
    Exit;
  end;
  if not DataSetUsed(DataSet, OtherData) then
    Exit;

  FMixtureCompiler := nil;
  FMixtureExpression := nil;
  UsedVariables := nil;
  try
    if (OtherData <> nil) then
    begin
      InterpValue := OtherData as TInterpValuesItem;
      if InterpValue.Values.UsePHAST_Interpolation
        and (InterpValue.Values.InterpolationDirection = pidMix) then
      begin
        InitializeMixtureExpression(Compiler, MixtureFormula,
          Expression, DataSet, OtherData);

        UsedVariables := TStringList.Create;
        InitializeVariables(UsedVariables, DataSet, Expression, Compiler);

        UpdateCurrentScreenObject(self);
        FMixtureCompiler := Compiler;
        FMixtureExpression := Expression;
      end
      else if not InterpValue.Values.UsePHAST_Interpolation then
      begin
        if InterpValue is TCustomPhastBoundaryCondition then
        begin
          BoundaryCondition := TCustomPhastBoundaryCondition(InterpValue);
          BoundaryCondition.FFormulaObserver.UpToDate := True;
        end;
      end;
    end;
    FMixtureVariables := UsedVariables;

    OtherData := nil;
    IsBoundaryDataSet := (DataSet = Model.TopBoundaryType)
      or (DataSet = Model.FrontBoundaryType)
      or (DataSet = Model.SideBoundaryType)
      or (DataSet = Model.Top2DBoundaryType);
    // if the screen object is deleted, or the data set is not used, quit.
    if Deleted or (DataSet = nil) or
      ((IndexOfDataSet(DataSet) < 0) and (IndexOfBoundaryDataSet(DataSet) < 0)
      and not IsBoundaryTimeDataSetUsed(DataSet, OtherData)
      and not IsBoundaryDataSet) then
    begin
      Exit;
    end;

    if IsBoundaryDataSet and (BoundaryType = 0) then
    begin
      Exit;
    end;

    BoundaryPosition := IndexOfBoundaryDataSet(DataSet);
    if BoundaryPosition >= 0 then
    begin
      AnObserver := FBoundaryDataSetSubscriptions[BoundaryPosition] as TObserver;
      AnObserver.UpToDate := True;
    end;

    case ViewDirection of
      vdTop: AssignValuesToTopPhastDataSet(Grid, DataSet, OtherData);
      vdFront: AssignValuesToFrontPhastDataSet(Grid, DataSet, OtherData);
      vdSide: AssignValuesToSidePhastDataSet(Grid, DataSet, OtherData);
      else
        begin
          Assert(False);
        end;
    end;
    SetGeometryUpToDate;
  finally
    UsedVariables.Free;
    CacheSegments;
  end;
end;

function TScreenObject.GetMixtureDataSetFormula(
  const Index: integer): string;
var
  Item: TInterpValuesItem;
begin
  if Index < InterpValues.Count then
  begin
    Item := InterpValues.Items[Index] as TInterpValuesItem;
    result := Item.Values.MixtureFormula;
  end
  else
  begin
    result := ''
  end;
end;

function TScreenObject.GetModflowBoundaries: TModflowBoundaries;
begin
  if FModflowBoundaries = nil then
  begin
    FModflowBoundaries := TModflowBoundaries.Create;
  end;
  result := FModflowBoundaries
end;

function TScreenObject.GetModflowChdBoundary: TChdBoundary;
begin
  if (FModel = nil)
    or ((FModel <> nil) and (csLoading in FModel.ComponentState)) then
  begin
    CreateChdBoundary;
  end;
  if FModflowBoundaries = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ModflowBoundaries.FModflowChdBoundary;
  end;
end;

function TScreenObject.GetModflowDrnBoundary: TDrnBoundary;
begin
  if (FModel = nil)
    or ((FModel <> nil) and (csLoading in FModel.ComponentState)) then
  begin
    CreateDrnBoundary;
  end;
  if FModflowBoundaries = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ModflowBoundaries.FModflowDrnBoundary;
  end;
end;

function TScreenObject.GetModflowDrtBoundary: TDrtBoundary;
begin
  if (FModel = nil)
    or ((FModel <> nil) and (csLoading in FModel.ComponentState)) then
  begin
    CreateDrtBoundary;
  end;
  if FModflowBoundaries = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ModflowBoundaries.FModflowDrtBoundary;
  end;
end;

function TScreenObject.GetModflowEtsBoundary: TEtsBoundary;
begin
  if (FModel = nil)
    or ((FModel <> nil) and (csLoading in FModel.ComponentState)) then
  begin
    CreateEtsBoundary;
  end;
  if FModflowBoundaries = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ModflowBoundaries.FModflowEtsBoundary;
  end;
end;

function TScreenObject.GetModflowEvtBoundary: TEvtBoundary;
begin
  if (FModel = nil)
    or ((FModel <> nil) and (csLoading in FModel.ComponentState)) then
  begin
    CreateEvtBoundary;
  end;
  if FModflowBoundaries = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ModflowBoundaries.FModflowEvtBoundary;
  end;
end;

function TScreenObject.GetModflowGhbBoundary: TGhbBoundary;
begin
  if (FModel = nil)
    or ((FModel <> nil) and (csLoading in FModel.ComponentState)) then
  begin
    CreateGhbBoundary;
  end;
  if FModflowBoundaries = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ModflowBoundaries.FModflowGhbBoundary;
  end;
end;

procedure TScreenObject.CreateHeadObservations;
begin
  if (ModflowBoundaries.FModflowHeadObservations = nil) then
  begin
    ModflowBoundaries.FModflowHeadObservations :=
      THobBoundary.Create(FModel, self);
  end;
end;

function TScreenObject.GetModflowHeadObservations: THobBoundary;
begin
  if (FModel = nil)
    or ((FModel <> nil) and (csLoading in FModel.ComponentState)) then
  begin
    CreateHeadObservations;
  end;
  if FModflowBoundaries = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ModflowBoundaries.FModflowHeadObservations;
  end;
end;

function TScreenObject.GetModflowHfbBoundary: THfbBoundary;
begin
  if (FModel = nil)
    or ((FModel <> nil) and (csLoading in FModel.ComponentState)) then
  begin
    CreateHfbBoundary;
  end;
  if FModflowBoundaries = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ModflowBoundaries.FModflowHfbBoundary;
  end;
end;

function TScreenObject.GetModflowLakBoundary: TLakBoundary;
begin
  if (FModel = nil)
    or ((FModel <> nil) and (csLoading in FModel.ComponentState)) then
  begin
    CreateLakBoundary;
  end;
  if FModflowBoundaries = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ModflowBoundaries.FModflowLakBoundary;
  end;
end;

function TScreenObject.GetModflowMnw2Boundary: TMnw2Boundary;
begin
  if (FModel = nil)
    or ((FModel <> nil) and (csLoading in FModel.ComponentState)) then
  begin
    CreateMnw2Boundary;
  end;
  if FModflowBoundaries = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ModflowBoundaries.FModflowMnw2Boundary;
  end;
end;

function TScreenObject.GetModflowRchBoundary: TRchBoundary;
begin
  if (FModel = nil)
    or ((FModel <> nil) and (csLoading in FModel.ComponentState)) then
  begin
    CreateRchBoundary;
  end;
  if FModflowBoundaries = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ModflowBoundaries.FModflowRchBoundary;
  end;
end;

function TScreenObject.GetModflowResBoundary: TResBoundary;
begin
  if (FModel = nil)
    or ((FModel <> nil) and (csLoading in FModel.ComponentState)) then
  begin
    CreateResBoundary;
  end;
  if FModflowBoundaries = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ModflowBoundaries.FModflowResBoundary;
  end;
end;

function TScreenObject.GetModflowRivBoundary: TRivBoundary;
begin
  if (FModel = nil)
    or ((FModel <> nil) and (csLoading in FModel.ComponentState)) then
  begin
    CreateRivBoundary;
  end;
  if FModflowBoundaries = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ModflowBoundaries.FModflowRivBoundary;
  end;
end;

function TScreenObject.GetModflowSfrBoundary: TSfrBoundary;
begin
  if (FModel = nil)
    or ((FModel <> nil) and (csLoading in FModel.ComponentState)) then
  begin
    CreateSfrBoundary;
  end;
  if FModflowBoundaries = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ModflowBoundaries.FModflowSfrBoundary;
  end;
end;

function TScreenObject.GetModflowStreamGage: TStreamGage;
begin
  if (FModel = nil)
    or ((FModel <> nil) and (csLoading in FModel.ComponentState)) then
  begin
    CreateGagBoundary;
  end;
  if FModflowBoundaries = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ModflowBoundaries.FModflowGage;
  end;
end;

function TScreenObject.GetModflowUzfBoundary: TUzfBoundary;
begin
  if (FModel = nil)
    or ((FModel <> nil) and (csLoading in FModel.ComponentState)) then
  begin
    CreateUzfBoundary;
  end;
  if FModflowBoundaries = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ModflowBoundaries.FModflowUzfBoundary;
  end;
end;

function TScreenObject.GetModflowWellBoundary: TMfWellBoundary;
begin
  if (FModel = nil)
    or ((FModel <> nil) and (csLoading in FModel.ComponentState)) then
  begin
    CreateWelBoundary;
  end;
  if FModflowBoundaries = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ModflowBoundaries.FModflowWellBoundary;
  end;
end;

procedure TScreenObject.GetModpathCellList(CellList: TCellAssignmentList);
var
  PhastModel: TPhastModel;
begin
  Assert(CellList.Count = 0);
  PhastModel := Model as TPhastModel;
  GetCellsToAssign(PhastModel.Grid, '0', nil, nil, CellList, alAll);
end;

procedure TScreenObject.SetMixtureDataSetFormula(const Index: integer;
  const Value: string);
var
  Observer: TObserver;
  OldUseList: TStringList;
  NewUseList: TStringList;
  UseIndex: integer;
  OtherIndex: integer;
  AFunction, OldFunction: string;
  Compiler: TRbwParser;
  ADataSet: TDataArray;
  DS: TObserver;
  Item: TInterpValuesItem;
  Model: TPhastModel;
begin
  Model := FModel as TPhastModel;
  Item := InterpValues.Items[Index] as TInterpValuesItem;
  AFunction := Item.Values.MixtureFormula;
  Observer := nil;
  if AFunction <> Value then
  begin
    if FCanInvalidateModel then
  begin
      InvalidateModel;
      OldFunction := AFunction;
      try
        ADataSet := DataSets[Index];
        Observer := FDataSetMixtureSubscriptions[Index] as TObserver;
        OldUseList := TStringList.Create;
        NewUseList := TStringList.Create;
        try
          Compiler := GetCompiler(ADataSet.Orientation,
            ADataSet.EvaluatedAt);
          if AFunction = '' then
          begin
            AFunction := '0.5';
          end;
          try
            Compiler.Compile(AFunction);
            OldUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
          except on E: ERbwParserError do
              OldUseList.Clear;
          end;

          AFunction := Value;
          if AFunction <> '' then
          begin
            Compiler.Compile(AFunction);
            NewUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
          end;

          Item.Values.MixtureFormula := Value;
          for UseIndex := OldUseList.Count - 1 downto 0 do
          begin
            OtherIndex := NewUseList.IndexOf(OldUseList[UseIndex]);
            if OtherIndex >= 0 then
            begin
              OldUseList.Delete(UseIndex);
              NewUseList.Delete(OtherIndex);
            end;
          end;
          for UseIndex := 0 to OldUseList.Count - 1 do
          begin
            DS := Model.GetObserverByName(OldUseList[UseIndex]);
            Assert(DS <> nil);
            DS.StopsTalkingTo(Observer);
          end;
          for UseIndex := 0 to NewUseList.Count - 1 do
          begin
            DS := Model.GetObserverByName(NewUseList[UseIndex]);
            Assert(DS <> nil);
            DS.TalksTo(Observer);
          end;
          Invalidate;
        finally
          OldUseList.Free;
          NewUseList.Free;
        end;

        if not (csDestroying in Model.ComponentState) then
        begin
          Observer.UpToDate := True;
          Observer.UpToDate := False;
          DataSets[Index].Invalidate;
          Observer.UpToDate := True;
        end;
      finally
        if not (csDestroying in Model.ComponentState) then
        begin
          if Observer.IsRecursive then
          begin
            Item.Values.MixtureFormula := OldFunction;
          end;
        end;
      end;
    end
    else
    begin
      Item.Values.MixtureFormula := Value;
    end;
  end;
end;

procedure TScreenObject.ResetMixtureSubscriptions;
var
  Index: integer;
  Subscription: TObserver;
begin
  for Index := 0 to FDataSetMixtureSubscriptions.Count - 1 do
  begin
    Subscription := FDataSetMixtureSubscriptions[Index] as TObserver;
    Subscription.UpToDate := True;
  end;
end;

procedure TScreenObject.UpdateMixtureExpression;
begin
  if FFluxBoundary <> nil then
  begin
    FluxBoundary.UpdateMixtureExpression;
  end;
  if FLeakyBoundary <> nil then
  begin
    LeakyBoundary.UpdateMixtureExpression;
  end;
  if FRiverBoundary <> nil then
  begin
    RiverBoundary.UpdateMixtureExpression;
  end;
  if FSpecifiedHeadBoundary <> nil then
  begin
    SpecifiedHeadBoundary.UpdateMixtureExpression;
  end;
  if FWellBoundary <> nil then
  begin
    WellBoundary.UpdateMixtureExpression;
  end;
end;

procedure TScreenObject.UpdateModel(Model: TComponent);
begin
  Assert((Model = nil) or (Model is TPhastModel));
  FModel := Model;
end;

procedure TScreenObject.UpdateModflowTimes(ModflowTimes: TRealList);
var
  PhastModel: TPhastModel;
begin
  PhastModel := Model as TPhastModel;
  if PhastModel.ModflowPackages.ChdBoundary.IsSelected
    and (ModflowChdBoundary <> nil)
    and ModflowChdBoundary.Used then
  begin
    ModflowChdBoundary.UpdateTimes(ModflowTimes);
  end;
  if PhastModel.ModflowPackages.GhbBoundary.IsSelected
    and (ModflowGhbBoundary <> nil)
    and ModflowGhbBoundary.Used then
  begin
    ModflowGhbBoundary.UpdateTimes(ModflowTimes);
  end;
  if PhastModel.ModflowPackages.WelPackage.IsSelected
    and (ModflowWellBoundary <> nil)
    and ModflowWellBoundary.Used then
  begin
    ModflowWellBoundary.UpdateTimes(ModflowTimes);
  end;
  if PhastModel.ModflowPackages.RivPackage.IsSelected
    and (ModflowRivBoundary <> nil)
    and ModflowRivBoundary.Used then
  begin
    ModflowRivBoundary.UpdateTimes(ModflowTimes);
  end;
  if PhastModel.ModflowPackages.DrnPackage.IsSelected
    and (ModflowDrnBoundary <> nil)
    and ModflowDrnBoundary.Used then
  begin
    ModflowDrnBoundary.UpdateTimes(ModflowTimes);
  end;
  if PhastModel.ModflowPackages.DrtPackage.IsSelected
    and (ModflowDrtBoundary <> nil)
    and ModflowDrtBoundary.Used then
  begin
    ModflowDrtBoundary.UpdateTimes(ModflowTimes);
  end;
  if PhastModel.ModflowPackages.RchPackage.IsSelected
    and (ModflowRchBoundary <> nil)
    and ModflowRchBoundary.Used then
  begin
    ModflowRchBoundary.UpdateTimes(ModflowTimes);
  end;
  if PhastModel.ModflowPackages.EvtPackage.IsSelected
    and (ModflowEvtBoundary <> nil)
    and ModflowEvtBoundary.Used then
  begin
    ModflowEvtBoundary.UpdateTimes(ModflowTimes);
  end;
  if PhastModel.ModflowPackages.EtsPackage.IsSelected
    and (ModflowEtsBoundary <> nil)
    and ModflowEtsBoundary.Used then
  begin
    ModflowEtsBoundary.UpdateTimes(ModflowTimes);
  end;
  if PhastModel.ModflowPackages.ResPackage.IsSelected
    and (ModflowResBoundary <> nil)
    and ModflowResBoundary.Used then
  begin
    ModflowResBoundary.UpdateTimes(ModflowTimes);
  end;
  if PhastModel.ModflowPackages.LakPackage.IsSelected
    and (ModflowLakBoundary <> nil)
    and ModflowLakBoundary.Used then
  begin
    ModflowLakBoundary.UpdateTimes(ModflowTimes);
  end;
  if PhastModel.ModflowPackages.SfrPackage.IsSelected
    and (ModflowSfrBoundary <> nil)
    and ModflowSfrBoundary.Used then
  begin
    ModflowSfrBoundary.UpdateTimes(ModflowTimes);
  end;
  if PhastModel.ModflowPackages.UzfPackage.IsSelected
    and (ModflowUzfBoundary <> nil)
    and ModflowUzfBoundary.Used then
  begin
    ModflowUzfBoundary.UpdateTimes(ModflowTimes);
  end;
  if PhastModel.ModflowPackages.Mnw2Package.IsSelected
    and (ModflowMnw2Boundary <> nil)
    and ModflowMnw2Boundary.Used then
  begin
    ModflowMnw2Boundary.UpdateTimes(ModflowTimes);
  end;
end;

procedure TScreenObject.CreateValueArrayStorage(
  var StoredValues: TValueArrayStorage);
begin
  if StoredValues = nil then
  begin
    StoredValues := TValueArrayStorage.Create;
  end;
end;

procedure TScreenObject.SetValueStorageField(const Value: TValueArrayStorage;
  var StoredValues: TValueArrayStorage);
begin
  if Value = nil then
  begin
    FreeAndNil(StoredValues);
  end
  else
  begin
    CreateValueArrayStorage(StoredValues);
    StoredValues.Assign(Value);
  end;
end;

procedure TScreenObject.GetCellsToAssign(const Grid: TCustomGrid;
  const DataSetFunction: string; OtherData: TObject;
  const DataSet: TDataArray; CellList: TCellAssignmentList;
  AssignmentLocation: TAssignmentLocation);
begin
  Assert(CellList.Count = 0);
  case ViewDirection of
    vdTop: Delegate.GetTopCellsToAssign(Grid, DataSetFunction, OtherData,
      DataSet, CellList, AssignmentLocation);
    vdFront: Delegate.GetFrontCellsToAssign(Grid, DataSetFunction, OtherData,
      DataSet, CellList, AssignmentLocation);
    vdSide: Delegate.GetSideCellsToAssign(Grid, DataSetFunction, OtherData,
      DataSet, CellList, AssignmentLocation);
    else Assert(False);
  end;
end;  

procedure TScreenObject.Draw1ElevModflow(const Direction: TViewDirection;
  const Bitmap32: TBitmap32; const DrawAsSelected: Boolean);
var
  LayerIndex: Integer;
  Segment: TLineSegment;
  SegmentFound: Boolean;
  AnotherSegment: TLineSegment;
  LocalDelegate: TModflowDelegate;
  ModflowGrid: TModflowGrid;
  PointCapacity: Integer;
  SegmentList, NewSegmentList: TList;
  ShouldDraw: Boolean;
  PointCount: Integer;
  RealPoint: TPoint2D;
  ColIndex: Integer;
  RowIndex: Integer;
  SegmentIndex: Integer;
  ObjectIndex: Integer;
  ObjectIndex1: Integer;
  ThreeDPoints: TPointArray;
  LineColor32: TColor32;
  LineDrawn: boolean;
  FrontPoints: T2DRealPointArray;
  Point1, Point2: TPoint2D;
  SidePoints: T2DRealPointArray;
  LineWidth: double;
  function ConvertPoint: TPoint;
  begin
    Assert(FModel <> nil);
    result := (FModel as TPhastModel).ConvertPoint(Direction, RealPoint);
  end;
  function FrontPoint(ThreeDPoint: T3DRealPoint): TPoint2D;
  begin
    result.X := ThreeDPoint.X;
    result.Y := ThreeDPoint.Z;
  end;
  function SidePoint(ThreeDPoint: T3DRealPoint): TPoint2D;
  begin
    result.X := ThreeDPoint.Y;
    result.Y := ThreeDPoint.Z;
  end;
begin
  if DrawAsSelected then
  begin
    LineWidth := 3;
  end
  else
  begin
    LineWidth := 2;
  end;
  LocalDelegate := Delegate as TModflowDelegate;
  ModflowGrid := (Model as TPhastModel).ModflowGrid;
  try
    LocalDelegate.AssignSelectedCells(ModflowGrid);
    if LocalDelegate.SelectedCells.HasCells then
    begin
      PointCapacity := 5;
      SetLength(ThreeDPoints, PointCapacity);
      case Direction of
        vdTop:
          begin
            LayerIndex := ModflowGrid.SelectedLayer;
            if LayerIndex >= ModflowGrid.LayerCount then
            begin
              LayerIndex := ModflowGrid.LayerCount - 1;
            end;
            SegmentList := TList.Create;
            NewSegmentList := TObjectList.Create;
            try
              for ColIndex := 0 to ModflowGrid.ColumnCount - 1 do
              begin
                for RowIndex := 0 to ModflowGrid.RowCount - 1 do
                begin
                  if LocalDelegate.
                    SelectedCells[LayerIndex, RowIndex, ColIndex] then
                  begin
                    LineDrawn := False;

                    ShouldDraw := (ColIndex > 0) and LocalDelegate.
                      SelectedCells[LayerIndex, RowIndex, ColIndex - 1];
                    if ShouldDraw then
                    begin
                      // -----------------
                      // |       |       |
                      // |       |       |
                      // |       |       |
                      // |   --------+   |
                      // |       |       |
                      // |       |       |
                      // |       |       |
                      // -----------------
                      LineDrawn := True;
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := ModflowGrid.
                        TwoDElementCenter(ColIndex - 1, RowIndex);
                      Segment.Point2 := ModflowGrid.
                        TwoDElementCenter(ColIndex, RowIndex);
                    end;

                    ShouldDraw := (RowIndex > 0) and LocalDelegate.
                      SelectedCells[LayerIndex, RowIndex-1, ColIndex];
                    if ShouldDraw then
                    begin
                      //  ---------
                      //  |       |
                      //  |       |
                      //  |       |
                      //  |   |   |
                      //  |   |   |
                      //  |   |   |
                      //  |   |   |
                      //  ----|----
                      //  |   |   |
                      //  |   |   |
                      //  |   |   |
                      //  |   +   |
                      //  |       |
                      //  |       |
                      //  |       |
                      //  ---------
                      LineDrawn := True;
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := ModflowGrid.
                        TwoDElementCenter(ColIndex, RowIndex);
                      Segment.Point2 := ModflowGrid.
                        TwoDElementCenter(ColIndex, RowIndex - 1);
                    end;

                    ShouldDraw := (ColIndex > 0) and (RowIndex > 0)
                      and LocalDelegate.
                      SelectedCells[LayerIndex, RowIndex - 1, ColIndex - 1]
                      and not LocalDelegate.
                      SelectedCells[LayerIndex, RowIndex, ColIndex - 1]
                      and not LocalDelegate.
                      SelectedCells[LayerIndex, RowIndex - 1, ColIndex];
                    if ShouldDraw then
                    begin
                      // ---------
                      // |       |
                      // |       |
                      // |       |
                      // |   \   |
                      // |    \  |
                      // |     \ |
                      // |      \|
                      // -----------------
                      //         |\      |
                      //         | \     |
                      //         |  \    |
                      //         |   +   |
                      //         |       |
                      //         |       |
                      //         |       |
                      //         ---------
                      LineDrawn := True;
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := ModflowGrid.
                        TwoDElementCenter(ColIndex, RowIndex);
                      Segment.Point2 := ModflowGrid.
                        TwoDElementCenter(ColIndex - 1, RowIndex-1);
                    end;

                    ShouldDraw := (ColIndex < ModflowGrid.ColumnCount-1)
                      and (RowIndex > 0)
                      and LocalDelegate.
                      SelectedCells[LayerIndex, RowIndex - 1, ColIndex + 1]
                      and not LocalDelegate.
                      SelectedCells[LayerIndex, RowIndex, ColIndex + 1]
                      and not LocalDelegate.
                      SelectedCells[LayerIndex, RowIndex - 1, ColIndex];
                    if ShouldDraw then
                    begin
                      //          ---------
                      //          |       |
                      //          |       |
                      //          |       |
                      //          |   /   |
                      //          |  /    |
                      //          | /     |
                      //          |/      |
                      //  -----------------
                      //  |      /|
                      //  |     / |
                      //  |    /  |
                      //  |   +   |
                      //  |       |
                      //  |       |
                      //  |       |
                      //  ---------
                      LineDrawn := True;
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := ModflowGrid.
                        TwoDElementCenter(ColIndex, RowIndex);
                      Segment.Point2 := ModflowGrid.
                        TwoDElementCenter(ColIndex + 1, RowIndex - 1);
                    end;

                    if not LineDrawn then
                    begin
                      LineDrawn := (ColIndex < ModflowGrid.ColumnCount -1)
                        and LocalDelegate.
                        SelectedCells[LayerIndex, RowIndex, ColIndex + 1];
                    end;
                    if not LineDrawn then
                    begin
                      LineDrawn := (RowIndex <  ModflowGrid.RowCount -1)
                        and LocalDelegate.
                        SelectedCells[LayerIndex, RowIndex + 1, ColIndex];
                    end;
                    if not LineDrawn then
                    begin
                      LineDrawn := (ColIndex < ModflowGrid.ColumnCount -1)
                        and (RowIndex < ModflowGrid.RowCount -1)
                        and LocalDelegate.
                        SelectedCells[LayerIndex, RowIndex + 1, ColIndex + 1];
                    end;
                    if not LineDrawn then
                    begin
                      LineDrawn := (ColIndex > 0)
                        and (RowIndex < ModflowGrid.RowCount -1)
                        and LocalDelegate.
                        SelectedCells[LayerIndex, RowIndex + 1, ColIndex - 1];
                    end;
                    if not LineDrawn then
                    begin
                      //   --------
                      //   |  /   |
                      //   | /    |
                      //   |/     |
                      //   |      |
                      //   |      |
                      //   |      |
                      //   --------
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := ModflowGrid.
                        TwoDColumnEdgeCenter(ColIndex, RowIndex);
                      Segment.Point2 := ModflowGrid.
                        TwoDRowEdgeCenter(ColIndex, RowIndex);

                      //  --------
                      //  |      |
                      //  |      |
                      //  |      |
                      //  |\     |
                      //  | \    |
                      //  |  \   |
                      //  --------
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := ModflowGrid.
                        TwoDRowEdgeCenter(ColIndex, RowIndex + 1);
                      Segment.Point2 := ModflowGrid.
                        TwoDColumnEdgeCenter(ColIndex, RowIndex);

                      // --------
                      // |      |
                      // |      |
                      // |      |
                      // |     /|
                      // |    / |
                      // |   /  |
                      // --------
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := ModflowGrid.
                        TwoDColumnEdgeCenter(ColIndex + 1, RowIndex);
                      Segment.Point2 := ModflowGrid.
                        TwoDRowEdgeCenter(ColIndex, RowIndex + 1);

                      // --------
                      // |   \  |
                      // |    \ |
                      // |     \|
                      // |      |
                      // |      |
                      // |      |
                      // --------
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := ModflowGrid.
                        TwoDColumnEdgeCenter(ColIndex + 1, RowIndex);
                      Segment.Point2 := ModflowGrid.
                        TwoDRowEdgeCenter(ColIndex, RowIndex);
                    end;
                  end;
                end;
              end;
              while SegmentList.Count > 0 do
              begin
                NewSegmentList.Clear;
                Segment := SegmentList[SegmentList.Count - 1];
                NewSegmentList.Add(Segment);
                SegmentList.Delete(SegmentList.Count - 1);
                SegmentFound := True;
                while SegmentFound do
                begin
                  SegmentFound := False;
                  for SegmentIndex := SegmentList.Count - 1 downto 0 do
                  begin
                    AnotherSegment := SegmentList[SegmentIndex];
                    if Segment.IsNextSegment(AnotherSegment) then
                    begin
                      Segment := AnotherSegment;
                      NewSegmentList.Add(Segment);
                      SegmentList.Delete(SegmentIndex);
                      SegmentFound := True;
                    end;
                  end;
                end;
                PointCount := NewSegmentList.Count + 1;
                SetLength(ThreeDPoints, PointCount);
                if NewSegmentList.Count > 0 then
                begin
                  Segment := NewSegmentList[0];
                  RealPoint := Segment.Point1;
                  ThreeDPoints[0] := ConvertPoint;
                  for ObjectIndex := 0 to NewSegmentList.Count - 1 do
                  begin
                    Segment := NewSegmentList[ObjectIndex];
                    RealPoint := Segment.Point2;
                    ThreeDPoints[ObjectIndex + 1] := ConvertPoint;
                  end;
                  if ColorLine then
                  begin
                    LineColor32 := Color32(LineColor);
                  end
                  else
                  begin
                    LineColor32 := clBlack32;
                  end;
                  DrawBigPolyline32(BitMap32, LineColor32, LineWidth,
                    ThreeDPoints, True, True, 0, PointCount);
                end;
              end;
            finally
              for ObjectIndex1 := SegmentList.Count - 1 downto 0 do
              begin
                TObject(SegmentList[ObjectIndex1]).Free;
              end;
              SegmentList.Free;
              NewSegmentList.Free;
            end;
          end;
        vdFront:
          begin
            RowIndex := ModflowGrid.SelectedRow;
            if RowIndex >= ModflowGrid.RowCount then
            begin
              RowIndex := ModflowGrid.RowCount - 1;
            end;
            FrontPoints := nil;
            SegmentList := TList.Create;
            NewSegmentList := TObjectList.Create;
            try
              for ColIndex := 0 to ModflowGrid.ColumnCount - 1 do
              begin
                for LayerIndex := 0 to ModflowGrid.LayerCount - 1 do
                begin
                  if LocalDelegate.
                    SelectedCells[LayerIndex, RowIndex, ColIndex] then
                  begin
                    LineDrawn := False;

                    ShouldDraw := (ColIndex > 0) and LocalDelegate.
                      SelectedCells[LayerIndex, RowIndex, ColIndex - 1];
                    if ShouldDraw then
                    begin
                      // -----------------
                      // |       |       |
                      // |       |       |
                      // |       |       |
                      // |   --------+   |
                      // |       |       |
                      // |       |       |
                      // |       |       |
                      // -----------------
                      LineDrawn := True;
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := FrontPoint(ModflowGrid.
                        ThreeDElementCenter(ColIndex - 1, RowIndex, LayerIndex));
                      Segment.Point2 := FrontPoint(ModflowGrid.
                        ThreeDElementCenter(ColIndex, RowIndex, LayerIndex));
                    end;

                    ShouldDraw := (LayerIndex > 0) and LocalDelegate.
                      SelectedCells[LayerIndex-1, RowIndex, ColIndex];
                    if ShouldDraw then
                    begin
                      //  ---------
                      //  |       |
                      //  |       |
                      //  |       |
                      //  |   |   |
                      //  |   |   |
                      //  |   |   |
                      //  |   |   |
                      //  ----|----
                      //  |   |   |
                      //  |   |   |
                      //  |   |   |
                      //  |   +   |
                      //  |       |
                      //  |       |
                      //  |       |
                      //  ---------
                      LineDrawn := True;
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := FrontPoint(ModflowGrid.
                        ThreeDElementCenter(ColIndex, RowIndex, LayerIndex));
                      Segment.Point2 := FrontPoint(ModflowGrid.
                        ThreeDElementCenter(ColIndex, RowIndex, LayerIndex - 1));
                    end;

                    ShouldDraw := not LineDrawn and (ColIndex > 0)
                      and (LayerIndex > 0) and LocalDelegate.
                      SelectedCells[LayerIndex - 1, RowIndex, ColIndex - 1]
                      and not LocalDelegate.
                      SelectedCells[LayerIndex - 1, RowIndex, ColIndex]
                      and not LocalDelegate.
                      SelectedCells[LayerIndex, RowIndex, ColIndex - 1];
                    if ShouldDraw then
                    begin
                      // ---------
                      // |       |
                      // |       |
                      // |       |
                      // |   \   |
                      // |    \  |
                      // |     \ |
                      // |      \|
                      // -----------------
                      //         |\      |
                      //         | \     |
                      //         |  \    |
                      //         |   +   |
                      //         |       |
                      //         |       |
                      //         |       |
                      //         ---------
                      LineDrawn := True;
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := FrontPoint(ModflowGrid.
                        ThreeDElementCenter(ColIndex, RowIndex, LayerIndex));
                      Segment.Point2 := FrontPoint(ModflowGrid.
                        ThreeDElementCenter(ColIndex - 1, RowIndex,
                        LayerIndex-1));
                    end;

                    ShouldDraw := not LineDrawn
                      and (ColIndex < ModflowGrid.ColumnCount-1)
                      and (LayerIndex > 0) and LocalDelegate.
                      SelectedCells[LayerIndex - 1, RowIndex, ColIndex + 1]
                      and not LocalDelegate.
                      SelectedCells[LayerIndex - 1, RowIndex, ColIndex]
                      and not LocalDelegate.
                      SelectedCells[LayerIndex, RowIndex, ColIndex + 1];
                    if ShouldDraw then
                    begin
                      //          ---------
                      //          |       |
                      //          |       |
                      //          |       |
                      //          |   /   |
                      //          |  /    |
                      //          | /     |
                      //          |/      |
                      //  -----------------
                      //  |      /|
                      //  |     / |
                      //  |    /  |
                      //  |   +   |
                      //  |       |
                      //  |       |
                      //  |       |
                      //  ---------
                      LineDrawn := True;
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := FrontPoint(ModflowGrid.
                        ThreeDElementCenter(ColIndex, RowIndex, LayerIndex));
                      Segment.Point2 := FrontPoint(ModflowGrid.
                        ThreeDElementCenter(ColIndex + 1, RowIndex,
                        LayerIndex - 1));
                    end;

                    if not LineDrawn then
                    begin
                      LineDrawn := (ColIndex < ModflowGrid.ColumnCount -1)
                        and LocalDelegate.
                        SelectedCells[LayerIndex, RowIndex, ColIndex + 1];
                    end;
                    if not LineDrawn then
                    begin
                      LineDrawn := (LayerIndex <  ModflowGrid.LayerCount -1)
                        and LocalDelegate.
                        SelectedCells[LayerIndex + 1, RowIndex, ColIndex];
                    end;
                    if not LineDrawn then
                    begin
                      LineDrawn := (ColIndex < ModflowGrid.ColumnCount -1)
                        and (LayerIndex < ModflowGrid.LayerCount -1)
                        and LocalDelegate.
                        SelectedCells[LayerIndex + 1, RowIndex, ColIndex + 1];
                    end;
                    if not LineDrawn then
                    begin
                      LineDrawn := (ColIndex > 0)
                        and (LayerIndex < ModflowGrid.LayerCount -1)
                        and LocalDelegate.
                        SelectedCells[LayerIndex + 1, RowIndex, ColIndex - 1];
                    end;
                    if not LineDrawn then
                    begin
                      if FrontPoints = nil then
                      begin
                        FrontPoints := ModflowGrid.FrontCellPoints(RowIndex);
                      end;
                      Point1 := FrontPoints[ColIndex * 2, LayerIndex];
                      Point2 := FrontPoints[ColIndex * 2, LayerIndex+1];

                      //   --------
                      //   |  /   |
                      //   | /    |
                      //   |/     |
                      //   |      |
                      //   |      |
                      //   |      |
                      //   --------
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1.x := Point1.x;
                      Segment.Point1.y := (Point1.y + Point2.y)/2;
                      Segment.Point2 := FrontPoints[ColIndex * 2 +1, LayerIndex];

                      //  --------
                      //  |      |
                      //  |      |
                      //  |      |
                      //  |\     |
                      //  | \    |
                      //  |  \   |
                      //  --------
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := FrontPoints[ColIndex * 2 + 1,
                        LayerIndex+1];
                      Segment.Point2.x := Point1.x;
                      Segment.Point2.y := (Point1.y + Point2.y)/2;

                      Point1 := FrontPoints[ColIndex * 2 + 2, LayerIndex];
                      Point2 := FrontPoints[ColIndex * 2 + 2, LayerIndex+1];
                      // --------
                      // |      |
                      // |      |
                      // |      |
                      // |     /|
                      // |    / |
                      // |   /  |
                      // --------
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1.x := Point1.x;
                      Segment.Point1.y := (Point1.y + Point2.y)/2;
                      Segment.Point2 := FrontPoints[ColIndex * 2 +1,
                        LayerIndex+1];

                      // --------
                      // |   \  |
                      // |    \ |
                      // |     \|
                      // |      |
                      // |      |
                      // |      |
                      // --------
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := FrontPoints[ColIndex * 2 + 1, LayerIndex];
                      Segment.Point2.x := Point1.x;
                      Segment.Point2.y := (Point1.y + Point2.y)/2;
                    end;
                  end;
                end;
              end;
              while SegmentList.Count > 0 do
              begin
                NewSegmentList.Clear;
                Segment := SegmentList[SegmentList.Count - 1];
                NewSegmentList.Add(Segment);
                SegmentList.Delete(SegmentList.Count - 1);
                SegmentFound := True;
                while SegmentFound do
                begin
                  SegmentFound := False;
                  for SegmentIndex := SegmentList.Count - 1 downto 0 do
                  begin
                    AnotherSegment := SegmentList[SegmentIndex];
                    if Segment.IsNextSegment(AnotherSegment) then
                    begin
                      Segment := AnotherSegment;
                      NewSegmentList.Add(Segment);
                      SegmentList.Delete(SegmentIndex);
                      SegmentFound := True;
                    end;
                  end;
                end;
                PointCount := NewSegmentList.Count + 1;
                SetLength(ThreeDPoints, PointCount);
                if NewSegmentList.Count > 0 then
                begin
                  Segment := NewSegmentList[0];
                  RealPoint := Segment.Point1;
                  ThreeDPoints[0] := ConvertPoint;
                  for ObjectIndex := 0 to NewSegmentList.Count - 1 do
                  begin
                    Segment := NewSegmentList[ObjectIndex];
                    RealPoint := Segment.Point2;
                    ThreeDPoints[ObjectIndex + 1] := ConvertPoint;
                  end;
                  if ColorLine then
                  begin
                    LineColor32 := Color32(LineColor);
                  end
                  else
                  begin
                    LineColor32 := clBlack32;
                  end;
                  DrawBigPolyline32(BitMap32, LineColor32, LineWidth,
                    ThreeDPoints, True, True, 0, PointCount);
                end;
              end;
            finally
              for ObjectIndex1 := SegmentList.Count - 1 downto 0 do
              begin
                TObject(SegmentList[ObjectIndex1]).Free;
              end;
              SegmentList.Free;
              NewSegmentList.Free;
            end;
          end;
        vdSide:
          begin
            ColIndex := ModflowGrid.SelectedColumn;
            if ColIndex >= ModflowGrid.ColumnCount then
            begin
              ColIndex := ModflowGrid.ColumnCount - 1;
            end;
            SidePoints := nil;
            SegmentList := TList.Create;
            NewSegmentList := TObjectList.Create;
            try
              for RowIndex := 0 to ModflowGrid.RowCount - 1 do
              begin
                for LayerIndex := 0 to ModflowGrid.LayerCount - 1 do
                begin
                  if LocalDelegate.
                    SelectedCells[LayerIndex, RowIndex, ColIndex] then
                  begin
                    LineDrawn := False;

                    ShouldDraw := (RowIndex > 0) and LocalDelegate.
                      SelectedCells[LayerIndex, RowIndex - 1, ColIndex];
                    if ShouldDraw then
                    begin
                      // -----------------
                      // |       |       |
                      // |       |       |
                      // |       |       |
                      // |   --------+   |
                      // |       |       |
                      // |       |       |
                      // |       |       |
                      // -----------------
                      LineDrawn := True;
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := SidePoint(ModflowGrid.
                        ThreeDElementCenter(ColIndex, RowIndex - 1, LayerIndex));
                      Segment.Point2 := SidePoint(ModflowGrid.
                        ThreeDElementCenter(ColIndex, RowIndex, LayerIndex));
                    end;

                    ShouldDraw := (LayerIndex > 0) and LocalDelegate.
                      SelectedCells[LayerIndex-1, RowIndex, ColIndex];
                    if ShouldDraw then
                    begin
                      //  ---------
                      //  |       |
                      //  |       |
                      //  |       |
                      //  |   |   |
                      //  |   |   |
                      //  |   |   |
                      //  |   |   |
                      //  ----|----
                      //  |   |   |
                      //  |   |   |
                      //  |   |   |
                      //  |   +   |
                      //  |       |
                      //  |       |
                      //  |       |
                      //  ---------
                      LineDrawn := True;
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := SidePoint(ModflowGrid.
                        ThreeDElementCenter(ColIndex, RowIndex, LayerIndex));
                      Segment.Point2 := SidePoint(ModflowGrid.
                        ThreeDElementCenter(ColIndex, RowIndex, LayerIndex - 1));
                    end;

                    ShouldDraw := not LineDrawn and (RowIndex > 0)
                      and (LayerIndex > 0) and LocalDelegate.
                      SelectedCells[LayerIndex - 1, RowIndex - 1, ColIndex]
                      and not LocalDelegate.
                      SelectedCells[LayerIndex - 1, RowIndex, ColIndex]
                      and not LocalDelegate.
                      SelectedCells[LayerIndex, RowIndex - 1, ColIndex];
                    if ShouldDraw then
                    begin
                      // ---------
                      // |       |
                      // |       |
                      // |       |
                      // |   \   |
                      // |    \  |
                      // |     \ |
                      // |      \|
                      // -----------------
                      //         |\      |
                      //         | \     |
                      //         |  \    |
                      //         |   +   |
                      //         |       |
                      //         |       |
                      //         |       |
                      //         ---------
                      LineDrawn := True;
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := SidePoint(ModflowGrid.
                        ThreeDElementCenter(ColIndex, RowIndex, LayerIndex));
                      Segment.Point2 := SidePoint(ModflowGrid.
                        ThreeDElementCenter(ColIndex, RowIndex - 1,
                          LayerIndex-1));
                    end;

                    ShouldDraw := not LineDrawn
                      and (RowIndex < ModflowGrid.RowCount-1)
                      and (LayerIndex > 0) and LocalDelegate.
                      SelectedCells[LayerIndex - 1, RowIndex + 1, ColIndex]
                      and not LocalDelegate.
                      SelectedCells[LayerIndex - 1, RowIndex, ColIndex]
                      and not LocalDelegate.
                      SelectedCells[LayerIndex, RowIndex + 1, ColIndex];
                    if ShouldDraw then
                    begin
                      //          ---------
                      //          |       |
                      //          |       |
                      //          |       |
                      //          |   /   |
                      //          |  /    |
                      //          | /     |
                      //          |/      |
                      //  -----------------
                      //  |      /|
                      //  |     / |
                      //  |    /  |
                      //  |   +   |
                      //  |       |
                      //  |       |
                      //  |       |
                      //  ---------
                      LineDrawn := True;
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := SidePoint(ModflowGrid.
                        ThreeDElementCenter(ColIndex, RowIndex, LayerIndex));
                      Segment.Point2 := SidePoint(ModflowGrid.
                        ThreeDElementCenter(ColIndex, RowIndex + 1,
                        LayerIndex - 1));
                    end;

                    if not LineDrawn then
                    begin
                      LineDrawn := (RowIndex < ModflowGrid.RowCount -1)
                        and LocalDelegate.
                        SelectedCells[LayerIndex, RowIndex + 1, ColIndex];
                    end;
                    if not LineDrawn then
                    begin
                      LineDrawn := (LayerIndex <  ModflowGrid.LayerCount -1)
                        and LocalDelegate.
                        SelectedCells[LayerIndex + 1, RowIndex, ColIndex];
                    end;
                    if not LineDrawn then
                    begin
                      LineDrawn := (RowIndex < ModflowGrid.RowCount -1)
                        and (LayerIndex < ModflowGrid.LayerCount -1)
                        and LocalDelegate.
                        SelectedCells[LayerIndex + 1, RowIndex + 1, ColIndex];
                    end;
                    if not LineDrawn then
                    begin
                      LineDrawn := (RowIndex > 0)
                        and (LayerIndex < ModflowGrid.LayerCount -1)
                        and LocalDelegate.
                        SelectedCells[LayerIndex + 1, RowIndex - 1, ColIndex];
                    end;
                    if not LineDrawn then
                    begin
                      if SidePoints = nil then
                      begin
                        SidePoints := ModflowGrid.SideCellPoints(ColIndex);
                      end;
                      Point1 := SidePoints[RowIndex * 2, LayerIndex];
                      Point2 := SidePoints[RowIndex * 2, LayerIndex+1];

                      //   --------
                      //   |  /   |
                      //   | /    |
                      //   |/     |
                      //   |      |
                      //   |      |
                      //   |      |
                      //   --------
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1.x := Point1.x;
                      Segment.Point1.y := (Point1.y + Point2.y)/2;
                      Segment.Point2 := SidePoints[RowIndex * 2 +1, LayerIndex];

                      //  --------
                      //  |      |
                      //  |      |
                      //  |      |
                      //  |\     |
                      //  | \    |
                      //  |  \   |
                      //  --------
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := SidePoints[RowIndex * 2 + 1,
                        LayerIndex+1];
                      Segment.Point2.x := Point1.x;
                      Segment.Point2.y := (Point1.y + Point2.y)/2;

                      Point1 := SidePoints[RowIndex * 2 + 2, LayerIndex];
                      Point2 := SidePoints[RowIndex * 2 + 2, LayerIndex+1];
                      // --------
                      // |      |
                      // |      |
                      // |      |
                      // |     /|
                      // |    / |
                      // |   /  |
                      // --------
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1.x := Point1.x;
                      Segment.Point1.y := (Point1.y + Point2.y)/2;
                      Segment.Point2 := SidePoints[RowIndex * 2 +1, LayerIndex+1];

                      // --------
                      // |   \  |
                      // |    \ |
                      // |     \|
                      // |      |
                      // |      |
                      // |      |
                      // --------
                      Segment := TLineSegment.Create;
                      SegmentList.Add(Segment);
                      Segment.Point1 := SidePoints[RowIndex * 2 + 1, LayerIndex];
                      Segment.Point2.x := Point1.x;
                      Segment.Point2.y := (Point1.y + Point2.y)/2;
                    end;
                  end;
                end;
              end;
              while SegmentList.Count > 0 do
              begin
                NewSegmentList.Clear;
                Segment := SegmentList[SegmentList.Count - 1];
                NewSegmentList.Add(Segment);
                SegmentList.Delete(SegmentList.Count - 1);
                SegmentFound := True;
                while SegmentFound do
                begin
                  SegmentFound := False;
                  for SegmentIndex := SegmentList.Count - 1 downto 0 do
                  begin
                    AnotherSegment := SegmentList[SegmentIndex];
                    if Segment.IsNextSegment(AnotherSegment) then
                    begin
                      Segment := AnotherSegment;
                      NewSegmentList.Add(Segment);
                      SegmentList.Delete(SegmentIndex);
                      SegmentFound := True;
                    end;
                  end;
                end;
                PointCount := NewSegmentList.Count + 1;
                SetLength(ThreeDPoints, PointCount);
                if NewSegmentList.Count > 0 then
                begin
                  Segment := NewSegmentList[0];
                  RealPoint := Segment.Point1;
                  ThreeDPoints[0] := ConvertPoint;
                  for ObjectIndex := 0 to NewSegmentList.Count - 1 do
                  begin
                    Segment := NewSegmentList[ObjectIndex];
                    RealPoint := Segment.Point2;
                    ThreeDPoints[ObjectIndex + 1] := ConvertPoint;
                  end;
                  if ColorLine then
                  begin
                    LineColor32 := Color32(LineColor);
                  end
                  else
                  begin
                    LineColor32 := clBlack32;
                  end;
                  DrawBigPolyline32(BitMap32, LineColor32, LineWidth,
                    ThreeDPoints, True, True, 0, PointCount);
                end;
              end;
            finally
              for ObjectIndex1 := SegmentList.Count - 1 downto 0 do
              begin
                TObject(SegmentList[ObjectIndex1]).Free;
              end;
              SegmentList.Free;
              NewSegmentList.Free;
            end;
          end;
      else
        Assert(False);
      end;
    end;
  finally
    LocalDelegate.SelectedCells.Clear;
  end;
end;

procedure TScreenObject.Draw1ElevPhast(const Direction: TViewDirection;
  const Bitmap32: TBitmap32; const DrawAsSelected: Boolean);
var
  LayerIndex: Integer;
  Segment: TLineSegment;
  SegmentFound: Boolean;
  AnotherSegment: TLineSegment;
  LocalDelegate: TPhastDelegate;
  PointCapacity: Integer;
  SegmentList, NewSegmentList: TList;
  ShouldDraw: Boolean;
  PointCount: Integer;
  RealPoint: TPoint2D;
  ColIndex: Integer;
  RowIndex: Integer;
  SegmentIndex: Integer;
  ObjectIndex: Integer;
  ObjectIndex1: Integer;
  ThreeDPoints: TPointArray;
  LineColor32: TColor32;
  LineDrawn: boolean;
  FrontPoints: T2DRealPointArray;
  Point1, Point2: TPoint2D;
  SidePoints: T2DRealPointArray;
  PhastGrid: TPhastGrid;
  ParallelDirection1: TViewDirection;
  ParallelDirection2: TViewDirection;
  PerpendicularIndex: Integer;
  ParallelLimit1: Integer;
  ParallelLimit2: Integer;
  ParallelIndex1: Integer;
  LowerIndex1Bound: Double;
  UpperIndex1Bound: Double;
  CellBelowIndex1Bound: Double;
  Dummy: Double;
  CellAboveIndex1Bound: Double;
  ParallelIndex2: Integer;
  LowerIndex2Bound: Double;
  UpperIndex2Bound: Double;
  CellBelowIndex2Bound: Double;
  CellAboveIndex2Bound: Double;
  LineWidth: double;
  function ConvertPoint: TPoint;
  begin
    Assert(FModel <> nil);
    result := (FModel as TPhastModel).ConvertPoint(Direction, RealPoint);
  end;
  function FrontPoint(ThreeDPoint: T3DRealPoint): TPoint2D;
  begin
    result.X := ThreeDPoint.X;
    result.Y := ThreeDPoint.Z;
  end;
  function SidePoint(ThreeDPoint: T3DRealPoint): TPoint2D;
  begin
    result.X := ThreeDPoint.Y;
    result.Y := ThreeDPoint.Z;
  end;
  function IsSelectedCell(PerpendicularIndex, ParallelIndex2,
    ParallelIndex1: integer): boolean;
  var
    ColIndex, RowIndex, LayerIndex: integer;
  begin
    ColIndex := -1;
    RowIndex := -1;
    LayerIndex := -1;
    case Direction of
      vdTop:
        begin
          ColIndex := ParallelIndex1;
          RowIndex := ParallelIndex2;
          LayerIndex := PerpendicularIndex;
        end;
      vdFront:
        begin
          ColIndex := ParallelIndex1;
          RowIndex := PerpendicularIndex;
          LayerIndex := ParallelIndex2;
        end;
      vdSide:
        begin
          ColIndex := PerpendicularIndex;
          RowIndex := ParallelIndex1;
          LayerIndex := ParallelIndex2;
        end;
      else Assert(False);
    end;
    result := LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex];
  end;
begin
  LocalDelegate := Delegate as TPhastDelegate;
  PhastGrid := (Model as TPhastModel).PhastGrid;
  try
    LocalDelegate.AssignSelectedCells(PhastGrid);
    if DrawAsSelected then
    begin
      LineWidth := 2;
    end
    else
    begin
      LineWidth := 3;
    end;
    if LocalDelegate.SelectedCells.HasCells then
    begin
      PointCapacity := 5;
      SetLength(ThreeDPoints, PointCapacity);
      PerpendicularIndex := -1;
      ParallelDirection1 := vdSide;
      ParallelDirection2 := vdFront;
      ParallelLimit1 := -1;
      ParallelLimit2 := -1;
      case Direction of
        vdTop:
          begin
            ParallelDirection1 := vdSide;
            ParallelDirection2 := vdFront;
            PerpendicularIndex := PhastGrid.DisplayLayer;

            case EvaluatedAt of
              eaBlocks:
                begin
                  if PerpendicularIndex >= PhastGrid.LayerCount then
                  begin
                    PerpendicularIndex := PhastGrid.LayerCount - 1;
                  end;
                  ParallelLimit1 := PhastGrid.ColumnCount - 1;
                  ParallelLimit2 := PhastGrid.RowCount - 1;
                end;
              eaNodes:
                begin
                  if PerpendicularIndex > PhastGrid.LayerCount then
                  begin
                    PerpendicularIndex := PhastGrid.LayerCount;
                  end;
                  ParallelLimit1 := PhastGrid.ColumnCount;
                  ParallelLimit2 := PhastGrid.RowCount;
                end;
              else Assert(False);
            end;
          end;
        vdFront:
          begin
            ParallelDirection1 := vdSide;
            ParallelDirection2 := vdTop;
            PerpendicularIndex := PhastGrid.DisplayRow;
            case EvaluatedAt of
              eaBlocks:
                begin
                  if PerpendicularIndex >= PhastGrid.RowCount then
                  begin
                    PerpendicularIndex := PhastGrid.RowCount - 1;
                  end;
                  ParallelLimit1 := PhastGrid.ColumnCount - 1;
                  ParallelLimit2 := PhastGrid.LayerCount - 1;
                end;
              eaNodes:
                begin
                  if PerpendicularIndex > PhastGrid.RowCount then
                  begin
                    PerpendicularIndex := PhastGrid.RowCount;
                  end;
                  ParallelLimit1 := PhastGrid.ColumnCount;
                  ParallelLimit2 := PhastGrid.LayerCount;
                end;
              else Assert(False);
            end;
          end;
        vdSide:
          begin
            ParallelDirection1 := vdFront;
            ParallelDirection2 := vdTop;
            PerpendicularIndex := PhastGrid.DisplayColumn;
            case EvaluatedAt of
              eaBlocks:
                begin
                  if PerpendicularIndex >= PhastGrid.ColumnCount then
                  begin
                    PerpendicularIndex := PhastGrid.ColumnCount - 1;
                  end;
                  ParallelLimit1 := PhastGrid.RowCount - 1;
                  ParallelLimit2 := PhastGrid.LayerCount - 1;
                end;
              eaNodes:
                begin
                  if PerpendicularIndex > PhastGrid.ColumnCount then
                  begin
                    PerpendicularIndex := PhastGrid.ColumnCount;
                  end;
                  ParallelLimit1 := PhastGrid.RowCount;
                  ParallelLimit2 := PhastGrid.LayerCount;
                end;
              else Assert(False);
            end;
          end;
        Else Assert(False);
      end;

      SegmentList := TList.Create;
      NewSegmentList := TObjectList.Create;
      try
        for ParallelIndex1 := 0 to ParallelLimit1 do
        begin

          PhastGrid.DrawBlockExtents(ParallelDirection1, EvaluatedAt,
            ParallelIndex1, LowerIndex1Bound, UpperIndex1Bound);
          if ParallelIndex1 > 0 then
          begin
            PhastGrid.DrawBlockExtents(ParallelDirection1, EvaluatedAt,
              ParallelIndex1-1, CellBelowIndex1Bound, Dummy);
          end;
          if ParallelIndex1 < ParallelLimit1 then
          begin
            PhastGrid.DrawBlockExtents(ParallelDirection1, EvaluatedAt,
              ParallelIndex1+1, Dummy, CellAboveIndex1Bound);
          end;
          for ParallelIndex2 := 0 to ParallelLimit2 do
          begin
            PhastGrid.DrawBlockExtents(ParallelDirection2, EvaluatedAt,
              ParallelIndex2, LowerIndex2Bound, UpperIndex2Bound);
            if ParallelIndex2 > 0 then
            begin
              PhastGrid.DrawBlockExtents(ParallelDirection2, EvaluatedAt,
                ParallelIndex2-1, CellBelowIndex2Bound, Dummy);
            end;
            if ParallelIndex2 < ParallelLimit2 then
            begin
              PhastGrid.DrawBlockExtents(ParallelDirection2, EvaluatedAt,
                ParallelIndex2+1, Dummy, CellAboveIndex2Bound);
            end;
            if IsSelectedCell(PerpendicularIndex, ParallelIndex2,
              ParallelIndex1) then
            begin
              LineDrawn := False;

              ShouldDraw := (ParallelIndex1 > 0) and
                IsSelectedCell(PerpendicularIndex, ParallelIndex2,
                ParallelIndex1 - 1);
              if ShouldDraw then
              begin
                // -----------------
                // |       |       |
                // |       |       |
                // |       |       |
                // |   --------+   |
                // |       |       |
                // |       |       |
                // |       |       |
                // -----------------
                LineDrawn := True;
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := (LowerIndex1Bound + UpperIndex1Bound)/2;
                Segment.Point1.y := (LowerIndex2Bound + UpperIndex2Bound)/2;
                Segment.Point2.x := (CellBelowIndex1Bound + LowerIndex1Bound)/2;
                Segment.Point2.y := (LowerIndex2Bound + UpperIndex2Bound)/2;
              end;

              ShouldDraw := (ParallelIndex2 > 0) and
                IsSelectedCell(PerpendicularIndex, ParallelIndex2-1,
                ParallelIndex1);
              if ShouldDraw then
              begin
                //  ---------
                //  |       |
                //  |       |
                //  |       |
                //  |   |   |
                //  |   |   |
                //  |   |   |
                //  |   |   |
                //  ----|----
                //  |   |   |
                //  |   |   |
                //  |   |   |
                //  |   +   |
                //  |       |
                //  |       |
                //  |       |
                //  ---------
                LineDrawn := True;
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := (LowerIndex1Bound + UpperIndex1Bound)/2;
                Segment.Point1.y := (LowerIndex2Bound + UpperIndex2Bound)/2;
                Segment.Point2.x := (LowerIndex1Bound + UpperIndex1Bound)/2;
                Segment.Point2.y := (CellBelowIndex2Bound + LowerIndex2Bound)/2;
              end;

              ShouldDraw := (ParallelIndex1 > 0) and (ParallelIndex2 > 0)
                and IsSelectedCell(PerpendicularIndex, ParallelIndex2 - 1,
                  ParallelIndex1 - 1)
                and not IsSelectedCell(PerpendicularIndex, ParallelIndex2,
                  ParallelIndex1 - 1)
                and not IsSelectedCell(PerpendicularIndex, ParallelIndex2 - 1,
                  ParallelIndex1);
              if ShouldDraw then
              begin
                // ---------
                // |       |
                // |       |
                // |       |
                // |   \   |
                // |    \  |
                // |     \ |
                // |      \|
                // -----------------
                //         |\      |
                //         | \     |
                //         |  \    |
                //         |   +   |
                //         |       |
                //         |       |
                //         |       |
                //         ---------
                LineDrawn := True;
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := (LowerIndex1Bound + UpperIndex1Bound)/2;
                Segment.Point1.y := (LowerIndex2Bound + UpperIndex2Bound)/2;
                Segment.Point2.x := (CellBelowIndex1Bound + LowerIndex1Bound)/2;
                Segment.Point2.y := (CellBelowIndex2Bound + LowerIndex2Bound)/2;
              end;

              ShouldDraw := (ParallelIndex1 < ParallelLimit1)
                and (ParallelIndex2 > 0)
                and IsSelectedCell(PerpendicularIndex, ParallelIndex2 - 1,
                  ParallelIndex1 + 1)
                and not IsSelectedCell(PerpendicularIndex, ParallelIndex2,
                  ParallelIndex1 + 1)
                and not IsSelectedCell(PerpendicularIndex, ParallelIndex2 - 1,
                  ParallelIndex1);
              if ShouldDraw then
              begin
                //          ---------
                //          |       |
                //          |       |
                //          |       |
                //          |   /   |
                //          |  /    |
                //          | /     |
                //          |/      |
                //  -----------------
                //  |      /|
                //  |     / |
                //  |    /  |
                //  |   +   |
                //  |       |
                //  |       |
                //  |       |
                //  ---------
                LineDrawn := True;
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := (LowerIndex1Bound + UpperIndex1Bound)/2;
                Segment.Point1.y := (LowerIndex2Bound + UpperIndex2Bound)/2;
                Segment.Point2.x := (UpperIndex1Bound + CellAboveIndex1Bound)/2;
                Segment.Point2.y := (CellBelowIndex2Bound + LowerIndex2Bound)/2;
              end;

              if not LineDrawn then
              begin
                LineDrawn := (ParallelIndex1 < ParallelLimit1)
                  and IsSelectedCell(PerpendicularIndex, ParallelIndex2,
                  ParallelIndex1 + 1);
              end;
              if not LineDrawn then
              begin
                LineDrawn := (ParallelIndex2 <  ParallelLimit2)
                  and IsSelectedCell(PerpendicularIndex, ParallelIndex2 + 1,
                  ParallelIndex1);
              end;
              if not LineDrawn then
              begin
                LineDrawn := (ParallelIndex1 < ParallelLimit1)
                  and (ParallelIndex2 < ParallelLimit2)
                  and IsSelectedCell(PerpendicularIndex, ParallelIndex2 + 1,
                  ParallelIndex1 + 1);
              end;
              if not LineDrawn then
              begin
                LineDrawn := (ParallelIndex1 > 0)
                  and (ParallelIndex2 < ParallelLimit2)
                  and IsSelectedCell(PerpendicularIndex, ParallelIndex2 + 1,
                  ParallelIndex1 - 1);
              end;
              if not LineDrawn then
              begin
                //   --------
                //   |  /   |
                //   | /    |
                //   |/     |
                //   |      |
                //   |      |
                //   |      |
                //   --------
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := LowerIndex1Bound;
                Segment.Point1.y := (LowerIndex2Bound + UpperIndex2Bound)/2;
                Segment.Point2.x := (LowerIndex1Bound + UpperIndex1Bound)/2;
                Segment.Point2.y := LowerIndex2Bound;

                //  --------
                //  |      |
                //  |      |
                //  |      |
                //  |\     |
                //  | \    |
                //  |  \   |
                //  --------
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := (LowerIndex1Bound + UpperIndex1Bound)/2;
                Segment.Point1.y := UpperIndex2Bound;
                Segment.Point2.x := LowerIndex1Bound;
                Segment.Point2.y := (LowerIndex2Bound + UpperIndex2Bound)/2;

                // --------
                // |      |
                // |      |
                // |      |
                // |     /|
                // |    / |
                // |   /  |
                // --------
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := UpperIndex1Bound;
                Segment.Point1.y := (LowerIndex2Bound + UpperIndex2Bound)/2;
                Segment.Point2.x := (LowerIndex1Bound + UpperIndex1Bound)/2;
                Segment.Point2.y := UpperIndex2Bound;

                // --------
                // |   \  |
                // |    \ |
                // |     \|
                // |      |
                // |      |
                // |      |
                // --------
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := (LowerIndex1Bound + UpperIndex1Bound)/2;
                Segment.Point1.y := LowerIndex2Bound;
                Segment.Point2.x := UpperIndex1Bound;
                Segment.Point2.y := (LowerIndex2Bound + UpperIndex2Bound)/2;
              end;
            end;
          end;
        end;
        while SegmentList.Count > 0 do
        begin
          NewSegmentList.Clear;
          Segment := SegmentList[SegmentList.Count - 1];
          NewSegmentList.Add(Segment);
          SegmentList.Delete(SegmentList.Count - 1);
          SegmentFound := True;
          while SegmentFound do
          begin
            SegmentFound := False;
            for SegmentIndex := SegmentList.Count - 1 downto 0 do
            begin
              AnotherSegment := SegmentList[SegmentIndex];
              if Segment.IsNextSegment(AnotherSegment) then
              begin
                Segment := AnotherSegment;
                NewSegmentList.Add(Segment);
                SegmentList.Delete(SegmentIndex);
                SegmentFound := True;
              end;
            end;
          end;
          PointCount := NewSegmentList.Count + 1;
          SetLength(ThreeDPoints, PointCount);
          if NewSegmentList.Count > 0 then
          begin
            Segment := NewSegmentList[0];
            RealPoint := Segment.Point1;
            if Direction = vdTop then
            begin
              RealPoint := PhastGrid.
                RotateFromGridCoordinatesToRealWorldCoordinates(RealPoint)
            end;
            ThreeDPoints[0] := ConvertPoint;
            for ObjectIndex := 0 to NewSegmentList.Count - 1 do
            begin
              Segment := NewSegmentList[ObjectIndex];
              RealPoint := Segment.Point2;
              if Direction = vdTop then
              begin
                RealPoint := PhastGrid.
                  RotateFromGridCoordinatesToRealWorldCoordinates(RealPoint)
              end;
              ThreeDPoints[ObjectIndex + 1] := ConvertPoint;
            end;
            if ColorLine then
            begin
              LineColor32 := Color32(LineColor);
            end
            else
            begin
              LineColor32 := clBlack32;
            end;
            DrawBigPolyline32(BitMap32, LineColor32, LineWidth, ThreeDPoints,
              True, True, 0, PointCount);
          end;
        end;

      finally
        for ObjectIndex1 := SegmentList.Count - 1 downto 0 do
        begin
          TObject(SegmentList[ObjectIndex1]).Free;
        end;
        SegmentList.Free;
        NewSegmentList.Free;
      end;
    end;
  finally
    LocalDelegate.SelectedCells.Clear;
  end;
end;

procedure TScreenObject.Draw2ElevPhast(const Direction: TViewDirection;
  const Bitmap32: TBitmap32);
var
  Segment: TLineSegment;
  SegmentFound: Boolean;
  AnotherSegment: TLineSegment;
  LocalDelegate: TPhastDelegate;
  PhastGrid: TPhastGrid;
  PointCapacity: Integer;
  SegmentList, NewSegmentList: TList;
  ShouldDraw: Boolean;
  PointCount: Integer;
  RealPoint: TPoint2D;
  SegmentIndex: Integer;
  ObjectIndex: Integer;
  ThreeDPoints: TPointArray;
  FillColor32: TColor32;
  LineColor32: TColor32;
  P: TPolygon32;
  MultiplePolygons: boolean;
  Dummy: double;
  PerpendicularIndex, ParallelIndex1, ParallelIndex2: integer;
  CellBelowIndex1Bound, LowerIndex1Bound: double;
  UpperIndex1Bound, CellAboveIndex1Bound: double;
  CellBelowIndex2Bound, LowerIndex2Bound: double;
  UpperIndex2Bound, CellAboveIndex2Bound: double;
  ParallelLimit1: integer;
  ParallelLimit2: integer;
  ParallelDirection1: TViewDirection;
  ParallelDirection2: TViewDirection;
  function ConvertPoint: TPoint;
  begin
    Assert(FModel <> nil);
    result := (FModel as TPhastModel).ConvertPoint(Direction, RealPoint);
  end;
  function IsSelectedCell(PerpendicularIndex, ParallelIndex2,
    ParallelIndex1: integer): boolean;
  var
    ColIndex, RowIndex, LayerIndex: integer;
  begin
    ColIndex := -1;
    RowIndex := -1;
    LayerIndex := -1;
    case Direction of
      vdTop:
        begin
          ColIndex := ParallelIndex1;
          RowIndex := ParallelIndex2;
          LayerIndex := PerpendicularIndex;
        end;
      vdFront:
        begin
          ColIndex := ParallelIndex1;
          RowIndex := PerpendicularIndex;
          LayerIndex := ParallelIndex2;
        end;
      vdSide:
        begin
          ColIndex := PerpendicularIndex;
          RowIndex := ParallelIndex1;
          LayerIndex := ParallelIndex2;
        end;
      else Assert(False);
    end;
    result := LocalDelegate.SelectedCells[LayerIndex, RowIndex, ColIndex];
  end;
begin
  LocalDelegate := Delegate as TPhastDelegate;
  PhastGrid := (Model as TPhastModel).PhastGrid;
  try
    LocalDelegate.AssignSelectedCells(PhastGrid);
    if LocalDelegate.SelectedCells.HasCells then
    begin
      PointCapacity := 5;
      PerpendicularIndex := -1;
      ParallelDirection1 := vdSide;
      ParallelDirection2 := vdFront;
      ParallelLimit1 := -1;
      ParallelLimit2 := -1;
      SetLength(ThreeDPoints, PointCapacity);
      case Direction of
        vdTop:
          begin
            ParallelDirection1 := vdSide;
            ParallelDirection2 := vdFront;
            PerpendicularIndex := PhastGrid.DisplayLayer;
            case EvaluatedAt of
              eaBlocks:
                begin
                  if PerpendicularIndex >= PhastGrid.LayerCount then
                  begin
                    PerpendicularIndex := PhastGrid.LayerCount - 1;
                  end;
                  ParallelLimit1 := PhastGrid.ColumnCount - 1;
                  ParallelLimit2 := PhastGrid.RowCount - 1;
                end;
              eaNodes:
                begin
                  ParallelLimit1 := PhastGrid.ColumnCount;
                  ParallelLimit2 := PhastGrid.RowCount;
                end;
              else Assert(False);
            end;
          end;
        vdFront:
          begin
            ParallelDirection1 := vdSide;
            ParallelDirection2 := vdTop;
            PerpendicularIndex := PhastGrid.DisplayRow;
            case EvaluatedAt of
              eaBlocks:
                begin
                  if PerpendicularIndex >= PhastGrid.RowCount then
                  begin
                    PerpendicularIndex := PhastGrid.RowCount - 1;
                  end;
                  ParallelLimit1 := PhastGrid.ColumnCount - 1;
                  ParallelLimit2 := PhastGrid.LayerCount - 1;
                end;
              eaNodes:
                begin
                  ParallelLimit1 := PhastGrid.ColumnCount;
                  ParallelLimit2 := PhastGrid.LayerCount;
                end;
              else Assert(False);
            end;
          end;
        vdSide:
          begin
            ParallelDirection1 := vdFront;
            ParallelDirection2 := vdTop;
            PerpendicularIndex := PhastGrid.DisplayColumn;
            case EvaluatedAt of
              eaBlocks:
                begin
                  if PerpendicularIndex >= PhastGrid.ColumnCount then
                  begin
                    PerpendicularIndex := PhastGrid.ColumnCount - 1;
                  end;
                  ParallelLimit1 := PhastGrid.RowCount - 1;
                  ParallelLimit2 := PhastGrid.LayerCount - 1;
                end;
              eaNodes:
                begin
                  ParallelLimit1 := PhastGrid.RowCount;
                  ParallelLimit2 := PhastGrid.LayerCount;
                end;
              else Assert(False);
            end;
          end;
        Else Assert(False);
      end;
      P := nil;
      MultiplePolygons := false;
      SegmentList := TList.Create;
      NewSegmentList := TObjectList.Create;
      try
        for ParallelIndex1 := 0 to ParallelLimit1 do
        begin
          PhastGrid.DrawBlockExtents(ParallelDirection1, EvaluatedAt,
            ParallelIndex1, LowerIndex1Bound, UpperIndex1Bound);
          if ParallelIndex1 > 0 then
          begin
            PhastGrid.DrawBlockExtents(ParallelDirection1, EvaluatedAt,
              ParallelIndex1-1, CellBelowIndex1Bound, Dummy);
          end;
          if ParallelIndex1 < ParallelLimit1 then
          begin
            PhastGrid.DrawBlockExtents(ParallelDirection1, EvaluatedAt,
              ParallelIndex1+1, Dummy, CellAboveIndex1Bound);
          end;
          for ParallelIndex2 := 0 to ParallelLimit2 do
          begin
            PhastGrid.DrawBlockExtents(ParallelDirection2, EvaluatedAt,
              ParallelIndex2, LowerIndex2Bound, UpperIndex2Bound);
            if ParallelIndex2 > 0 then
            begin
              PhastGrid.DrawBlockExtents(ParallelDirection2, EvaluatedAt,
                ParallelIndex2-1, CellBelowIndex2Bound, Dummy);
            end;
            if ParallelIndex2 < ParallelLimit2 then
            begin
              PhastGrid.DrawBlockExtents(ParallelDirection2, EvaluatedAt,
                ParallelIndex2+1, Dummy, CellAboveIndex2Bound);
            end;
            if IsSelectedCell(PerpendicularIndex,
              ParallelIndex2, ParallelIndex1) then
            begin
              ShouldDraw := True;
              if (ParallelIndex1 > 0) and IsSelectedCell(
                PerpendicularIndex, ParallelIndex2, ParallelIndex1-1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex2 > 0) and IsSelectedCell(
                PerpendicularIndex, ParallelIndex2 - 1, ParallelIndex1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex1 > 0) and (ParallelIndex2 > 0) and
                IsSelectedCell(PerpendicularIndex, ParallelIndex2 - 1,
                ParallelIndex1 - 1) then
              begin
                ShouldDraw := False;
              end;
              if ShouldDraw then
              begin
                //   --------
                //   |  /   |
                //   | /    |
                //   |/     |
                //   |      |
                //   |      |
                //   |      |
                //   --------
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := LowerIndex1Bound;
                Segment.Point1.y := (LowerIndex2Bound+UpperIndex2Bound)/2;
                Segment.Point2.x := (LowerIndex1Bound+UpperIndex1Bound)/2;
                Segment.Point2.y := LowerIndex2Bound;
              end;

              ShouldDraw := True;
              if (ParallelIndex1 > 0) and IsSelectedCell(
                PerpendicularIndex, ParallelIndex2, ParallelIndex1 - 1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex2 < ParallelLimit2) and IsSelectedCell(
                PerpendicularIndex, ParallelIndex2 + 1, ParallelIndex1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex1 > 0) and (ParallelIndex2 < ParallelLimit2)
                and IsSelectedCell(PerpendicularIndex, ParallelIndex2 + 1,
                ParallelIndex1 - 1) then
              begin
                ShouldDraw := False;
              end;
              if ShouldDraw then
              begin
                //  --------
                //  |      |
                //  |      |
                //  |      |
                //  |\     |
                //  | \    |
                //  |  \   |
                //  --------
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := (LowerIndex1Bound+UpperIndex1Bound)/2;
                Segment.Point1.y :=  UpperIndex2Bound;
                Segment.Point2.x := LowerIndex1Bound;
                Segment.Point2.y := (LowerIndex2Bound+UpperIndex2Bound)/2;
              end;

              ShouldDraw := True;
              if (ParallelIndex1 < ParallelLimit1) and IsSelectedCell(
                PerpendicularIndex, ParallelIndex2, ParallelIndex1 + 1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex2 < ParallelLimit2) and IsSelectedCell(
                PerpendicularIndex, ParallelIndex2 + 1, ParallelIndex1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex1 < ParallelLimit1)
                and (ParallelIndex2 < ParallelLimit2)
                and IsSelectedCell(PerpendicularIndex, ParallelIndex2 + 1,
                ParallelIndex1 + 1) then
              begin
                ShouldDraw := False;
              end;
              if ShouldDraw then
              begin
                // --------
                // |      |
                // |      |
                // |      |
                // |     /|
                // |    / |
                // |   /  |
                // --------
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := UpperIndex1Bound;
                Segment.Point1.y := (LowerIndex2Bound+UpperIndex2Bound)/2;
                Segment.Point2.x := (LowerIndex1Bound+UpperIndex1Bound)/2;
                Segment.Point2.y := UpperIndex2Bound;
              end;

              ShouldDraw := True;
              if (ParallelIndex1 < ParallelLimit1) and IsSelectedCell(
                PerpendicularIndex, ParallelIndex2, ParallelIndex1 + 1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex2 > 0) and IsSelectedCell(
                PerpendicularIndex, ParallelIndex2 - 1, ParallelIndex1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex1 < ParallelLimit1) and (ParallelIndex2 > 0)
                and IsSelectedCell(PerpendicularIndex, ParallelIndex2 - 1,
                ParallelIndex1 + 1) then
              begin
                ShouldDraw := False;
              end;
              if ShouldDraw then
              begin
                // ---------
                // |    \  |
                // |     \ |
                // |      \|
                // |   +   |
                // |       |
                // |       |
                // |       |
                // ---------
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := (LowerIndex1Bound+UpperIndex1Bound)/2;
                Segment.Point1.y := LowerIndex2Bound;
                Segment.Point2.x := UpperIndex1Bound;
                Segment.Point2.y := (LowerIndex2Bound+UpperIndex2Bound)/2;
              end;

              ShouldDraw := ParallelIndex1 > 0;
              if (ParallelIndex1 > 0) and not IsSelectedCell(
                PerpendicularIndex, ParallelIndex2, ParallelIndex1 - 1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex2 > 0) and IsSelectedCell(
                PerpendicularIndex, ParallelIndex2 - 1, ParallelIndex1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex1 > 0) and (ParallelIndex2 > 0) and
                IsSelectedCell(PerpendicularIndex, ParallelIndex2 - 1,
                ParallelIndex1 - 1) then
              begin
                ShouldDraw := False;
              end;
              if ShouldDraw then
              begin
                // -----=======-----
                // |       |       |
                // |       |       |
                // |       |       |
                // |       |   +   |
                // |       |       |
                // |       |       |
                // |       |       |
                // -----------------
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := (CellBelowIndex1Bound+LowerIndex1Bound)/2;
                Segment.Point1.y := LowerIndex2Bound;
                Segment.Point2.x := (LowerIndex1Bound+UpperIndex1Bound)/2;
                Segment.Point2.y := LowerIndex2Bound;
              end;

              ShouldDraw := (ParallelIndex1 > 0);
              if (ParallelIndex1 > 0) and not IsSelectedCell(
                PerpendicularIndex, ParallelIndex2, ParallelIndex1 - 1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex2 < ParallelLimit2) and IsSelectedCell(
                PerpendicularIndex, ParallelIndex2 + 1, ParallelIndex1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex1 > 0) and (ParallelIndex2 < ParallelLimit2)
                and IsSelectedCell(PerpendicularIndex, ParallelIndex2 + 1,
                ParallelIndex1 - 1) then
              begin
                ShouldDraw := False;
              end;
              if ShouldDraw then
              begin
                // -----------------
                // |       |       |
                // |       |       |
                // |       |       |
                // |       |   +   |
                // |       |       |
                // |       |       |
                // |       |       |
                // -----=======-----
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := (LowerIndex1Bound+UpperIndex1Bound)/2;
                Segment.Point1.y := UpperIndex2Bound;
                Segment.Point2.x := (CellBelowIndex1Bound+LowerIndex1Bound)/2;
                Segment.Point2.y := UpperIndex2Bound;
              end;

              ShouldDraw := ParallelIndex2 > 0;
              if (ParallelIndex1 > 0) and IsSelectedCell(
                PerpendicularIndex, ParallelIndex2, ParallelIndex1 - 1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex2 > 0) and not IsSelectedCell(
                PerpendicularIndex, ParallelIndex2 - 1, ParallelIndex1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex1 > 0) and (ParallelIndex2 > 0) and
                IsSelectedCell(PerpendicularIndex, ParallelIndex2 - 1,
                ParallelIndex1 - 1) then
              begin
                ShouldDraw := False;
              end;
              if ShouldDraw then
              begin
                //  ---------
                //  |       |
                //  |       |
                //  |       |
                //  |       |
                //  +       |
                //  +       |
                //  +       |
                //  +--------
                //  +       |
                //  +       |
                //  +       |
                //  |   +   |
                //  |       |
                //  |       |
                //  |       |
                //  ---------
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := LowerIndex1Bound;
                Segment.Point1.y := (LowerIndex2Bound + UpperIndex2Bound)/2;
                Segment.Point2.x := LowerIndex1Bound;
                Segment.Point2.y := (CellBelowIndex2Bound + LowerIndex2Bound)/2;
              end;

              ShouldDraw := (ParallelIndex2 > 0);
              if (ParallelIndex1 < ParallelLimit1) and
                IsSelectedCell(PerpendicularIndex, ParallelIndex2,
                ParallelIndex1 + 1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex2 > 0) and not IsSelectedCell(
                PerpendicularIndex, ParallelIndex2 - 1, ParallelIndex1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex1 < ParallelLimit1) and (ParallelIndex2 > 0)
                and IsSelectedCell(PerpendicularIndex, ParallelIndex2 - 1,
                ParallelIndex1 + 1) then
              begin
                ShouldDraw := False;
              end;
              if ShouldDraw then
              begin
                //  ---------
                //  |       |
                //  |       |
                //  |       |
                //  |       |
                //  |       +
                //  |       +
                //  |       +
                //  --------+
                //  |       +
                //  |       +
                //  |       +
                //  |   +   |
                //  |       |
                //  |       |
                //  |       |
                //  ---------
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := UpperIndex1Bound;
                Segment.Point1.y := (CellBelowIndex2Bound + LowerIndex2Bound)/2;
                Segment.Point2.x := UpperIndex1Bound;
                Segment.Point2.y := (LowerIndex2Bound + UpperIndex2Bound)/2;
              end;

              ShouldDraw := (ParallelIndex1 > 0) and (ParallelIndex2 > 0);
              if (ParallelIndex1 > 0) and IsSelectedCell(
                PerpendicularIndex, ParallelIndex2, ParallelIndex1 - 1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex1 > 0) and (ParallelIndex2 > 0) and
                not IsSelectedCell(PerpendicularIndex, ParallelIndex2 - 1,
                ParallelIndex1 - 1) then
              begin
                ShouldDraw := False;
              end;
              if ShouldDraw then
              begin
                // ---------
                // |       |
                // |       |
                // |       |
                // |       |
                // |       |
                // |       |
                // |       |
                // -----------------
                //      \  |       |
                //       \ |       |
                //        \|       |
                //         |   +   |
                //         |       |
                //         |       |
                //         |       |
                //         ---------
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := LowerIndex1Bound;
                Segment.Point1.y := (LowerIndex2Bound + UpperIndex2Bound)/2;
                Segment.Point2.x := (CellBelowIndex1Bound + LowerIndex1Bound)/2;
                Segment.Point2.y := LowerIndex2Bound;
              end;

              ShouldDraw := (ParallelIndex1 > 0) and (ParallelIndex2 > 0);
              if (ParallelIndex2 > 0) and IsSelectedCell(
                PerpendicularIndex, ParallelIndex2 - 1, ParallelIndex1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex1 > 0) and (ParallelIndex2 > 0) and
                not IsSelectedCell(PerpendicularIndex, ParallelIndex2 - 1,
                ParallelIndex1 - 1) then
              begin
                ShouldDraw := False;
              end;
              if ShouldDraw then
              begin
                //  ---------
                //  |       |
                //  |       |
                //  |       |
                //  |       |
                //  |       |\
                //  |       | \
                //  |       |  \
                //  -----------------
                //          |       |
                //          |       |
                //          |       |
                //          |   +   |
                //          |       |
                //          |       |
                //          |       |
                //          ---------
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := LowerIndex1Bound;
                Segment.Point1.y := (CellBelowIndex2Bound + LowerIndex2Bound)/2;
                Segment.Point2.x := (LowerIndex1Bound + UpperIndex1Bound)/2;
                Segment.Point2.y := LowerIndex2Bound;
              end;

              ShouldDraw := (ParallelIndex1 < ParallelLimit1)
                and (ParallelIndex2 > 0);
              if (ParallelIndex2 > 0) and IsSelectedCell(
                PerpendicularIndex, ParallelIndex2 - 1, ParallelIndex1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex1 < ParallelLimit1)
                and (ParallelIndex2 > 0)
                and not IsSelectedCell(
                PerpendicularIndex, ParallelIndex2 - 1, ParallelIndex1 + 1) then
              begin
                ShouldDraw := False;
              end;
              if ShouldDraw then
              begin
                //          ---------
                //          |       |
                //          |       |
                //          |       |
                //          |       |
                //         /|       |
                //        / |       |
                //       /  |       |
                //  -----------------
                //  |       |
                //  |       |
                //  |       |
                //  |   +   |
                //  |       |
                //  |       |
                //  |       |
                //  ---------
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := (LowerIndex1Bound + UpperIndex1Bound)/2;
                Segment.Point1.y := LowerIndex2Bound;
                Segment.Point2.x := UpperIndex1Bound;
                Segment.Point2.y := (CellBelowIndex2Bound + LowerIndex2Bound)/2;
              end;

              ShouldDraw := (ParallelIndex2 > 0)
                and (ParallelIndex1 < ParallelLimit1);
              if (ParallelIndex1 < ParallelLimit1) and
                IsSelectedCell(PerpendicularIndex, ParallelIndex2,
                ParallelIndex1 + 1) then
              begin
                ShouldDraw := False;
              end;
              if (ParallelIndex1 < ParallelLimit1)
                and (ParallelIndex2 > 0)
                and not IsSelectedCell(PerpendicularIndex, ParallelIndex2 - 1,
                ParallelIndex1 + 1) then
              begin
                ShouldDraw := False;
              end;
              if ShouldDraw then
              begin
                //          ---------
                //          |       |
                //          |       |
                //          |       |
                //          |       |
                //          |       |
                //          |       |
                //          |       |
                //  -----------------
                //  |       |  /
                //  |       | /
                //  |       |/
                //  |   +   |
                //  |       |
                //  |       |
                //  |       |
                //  ---------
                Segment := TLineSegment.Create;
                SegmentList.Add(Segment);
                Segment.Point1.x := (UpperIndex1Bound + CellAboveIndex1Bound)/2;
                Segment.Point1.y :=  LowerIndex2Bound;
                Segment.Point2.x := UpperIndex1Bound;
                Segment.Point2.y := (LowerIndex2Bound + UpperIndex2Bound)/2;
              end;
            end;
          end;
        end;
        while SegmentList.Count > 0 do
        begin
          NewSegmentList.Clear;
          Segment := SegmentList[SegmentList.Count - 1];
          NewSegmentList.Add(Segment);
          SegmentList.Delete(SegmentList.Count - 1);
          SegmentFound := True;
          while SegmentFound do
          begin
            SegmentFound := False;
            for SegmentIndex := SegmentList.Count - 1 downto 0 do
            begin
              AnotherSegment := SegmentList[SegmentIndex];
              if Segment.IsNextSegment(AnotherSegment) then
              begin
                Segment := AnotherSegment;
                NewSegmentList.Add(Segment);
                SegmentList.Delete(SegmentIndex);
                SegmentFound := True;
              end;
            end;
          end;
          PointCount := NewSegmentList.Count + 1;
          SetLength(ThreeDPoints, PointCount);
          if NewSegmentList.Count > 0 then
          begin
            Segment := NewSegmentList[0];
            RealPoint := Segment.Point1;
            if Direction = vdTop then
            begin
              RealPoint := PhastGrid.
                RotateFromGridCoordinatesToRealWorldCoordinates(RealPoint);
            end;
            ThreeDPoints[0] := ConvertPoint;
            for ObjectIndex := 0 to NewSegmentList.Count - 1 do
            begin
              Segment := NewSegmentList[ObjectIndex];
              RealPoint := Segment.Point2;
              if Direction = vdTop then
              begin
                RealPoint := PhastGrid.
                  RotateFromGridCoordinatesToRealWorldCoordinates(RealPoint);
              end;
              ThreeDPoints[ObjectIndex + 1] := ConvertPoint;
            end;
            if ColorLine then
            begin
              LineColor32 := Color32(LineColor);
            end
            else
            begin
              LineColor32 := clBlack32;
            end;
            if FillScreenObject then
            begin
              FillColor32 := Color32(FillColor);
              FillColor32 := SetAlpha(FillColor32, 128);
            end
            else
            begin
              FillColor32 := clTransparent32;
            end;
            DrawBigPolygon32(Bitmap32, LineColor32, FillColor32, 1,
              ThreeDPoints, P, MultiplePolygons, True, True);
          end;
        end;
      finally
        SegmentList.Free;
        NewSegmentList.Free;
      end;
    end;
  finally
    LocalDelegate.SelectedCells.Clear;
  end;
end;

procedure TScreenObject.Draw2ElevModflow(const Direction: TViewDirection;
  const Bitmap32: TBitmap32);
var
  LayerIndex: Integer;
  LocalDelegate: TModflowDelegate;
  ModflowGrid: TModflowGrid;
  SegmentList: TList;
  ColIndex: Integer;
  RowIndex: Integer;
  ObjectIndex1: Integer;
  FrontPoints: T2DRealPointArray;
  SidePoints: T2DRealPointArray;
begin
  LocalDelegate := Delegate as TModflowDelegate;
  ModflowGrid := (Model as TPhastModel).ModflowGrid;
  try
    LocalDelegate.AssignSelectedCells(ModflowGrid);
    if LocalDelegate.SelectedCells.HasCells then
    begin
      SegmentList := TList.Create;
      try
        case Direction of
          vdTop:
            begin
              LayerIndex := ModflowGrid.SelectedLayer;
              if LayerIndex >= ModflowGrid.LayerCount then
              begin
                LayerIndex := ModflowGrid.LayerCount - 1;
              end;
                for ColIndex := 0 to ModflowGrid.ColumnCount - 1 do
                begin
                  for RowIndex := 0 to ModflowGrid.RowCount - 1 do
                  begin
                    if LocalDelegate.
                      SelectedCells[LayerIndex, RowIndex, ColIndex] then
                    begin
                      //   --------
                      //   |  /   |
                      //   | /    |
                      //   |/     |
                      //   |      |
                      //   |      |
                      //   |      |
                      //   --------
                      DrawModflowTopSegmentInsideTopLeft(RowIndex, ColIndex,
                        SegmentList, LayerIndex, ModflowGrid, LocalDelegate);
                  
                      //  --------
                      //  |      |
                      //  |      |
                      //  |      |
                      //  |\     |
                      //  | \    |
                      //  |  \   |
                      //  --------
                      DrawModflowTopSegmentInsideBottomLeft(SegmentList, LayerIndex,
                        ModflowGrid, LocalDelegate, RowIndex, ColIndex);

                      // --------
                      // |      |
                      // |      |
                      // |      |
                      // |     /|
                      // |    / |
                      // |   /  |
                      // --------
                      DrawModflowTopSegmentInsideBottomRight(RowIndex, ColIndex,
                        SegmentList, LayerIndex, ModflowGrid, LocalDelegate);

                      // ---------
                      // |    \  |
                      // |     \ |
                      // |      \|
                      // |   +   |
                      // |       |
                      // |       |
                      // |       |
                      // ---------
                      DrawModflowTopSegmentInsideTopRight(ModflowGrid,
                        LocalDelegate, RowIndex, ColIndex, SegmentList, LayerIndex);

                      // -----=======-----
                      // |       |       |
                      // |       |       |
                      // |       |       |
                      // |       |   +   |
                      // |       |       |
                      // |       |       |
                      // |       |       |
                      // -----------------
                      DrawModflowTopSegmentTopLeft(
                        RowIndex, ColIndex, SegmentList, LayerIndex, ModflowGrid,
                        LocalDelegate);

                      // -----------------
                      // |       |       |
                      // |       |       |
                      // |       |       |
                      // |       |   +   |
                      // |       |       |
                      // |       |       |
                      // |       |       |
                      // -----=======-----
                      DrawModflowTopSegmentBottomLeft(
                        RowIndex, ColIndex, SegmentList, LayerIndex, ModflowGrid,
                        LocalDelegate);

                      //  ---------
                      //  |       |
                      //  |       |
                      //  |       |
                      //  |       |
                      //  +       |
                      //  +       |
                      //  +       |
                      //  +--------
                      //  +       |
                      //  +       |
                      //  +       |
                      //  |   +   |
                      //  |       |
                      //  |       |
                      //  |       |
                      //  ---------
                      DrawModflowTopSegmentAboveLeft(
                        RowIndex, ColIndex, SegmentList, LayerIndex, ModflowGrid,
                        LocalDelegate);

                      //  ---------
                      //  |       |
                      //  |       |
                      //  |       |
                      //  |       |
                      //  |       +
                      //  |       +
                      //  |       +
                      //  --------+
                      //  |       +
                      //  |       +
                      //  |       +
                      //  |   +   |
                      //  |       |
                      //  |       |
                      //  |       |
                      //  ---------
                      DrawModflowTopSegmentAboveRight(
                        RowIndex, ColIndex, SegmentList, LayerIndex, ModflowGrid,
                        LocalDelegate);

                      // ---------
                      // |       |
                      // |       |
                      // |       |
                      // |       |
                      // |       |
                      // |       |
                      // |       |
                      // -----------------
                      //      \  |       |
                      //       \ |       |
                      //        \|       |
                      //         |   +   |
                      //         |       |
                      //         |       |
                      //         |       |
                      //         ---------
                      DrawModflowTopSegmentLeftOutsideAbove(
                        RowIndex, ColIndex, SegmentList, LayerIndex, ModflowGrid,
                        LocalDelegate);

                      //  ---------
                      //  |       |
                      //  |       |
                      //  |       |
                      //  |       |
                      //  |       |\
                      //  |       | \
                      //  |       |  \
                      //  -----------------
                      //          |       |
                      //          |       |
                      //          |       |
                      //          |   +   |
                      //          |       |
                      //          |       |
                      //          |       |
                      //          ---------
                      DrawModflowTopSegmentAboveOutsideLeft(
                        RowIndex, ColIndex, SegmentList, LayerIndex, ModflowGrid,
                        LocalDelegate);

                      //          ---------
                      //          |       |
                      //          |       |
                      //          |       |
                      //          |       |
                      //         /|       |
                      //        / |       |
                      //       /  |       |
                      //  -----------------
                      //  |       |
                      //  |       |
                      //  |       |
                      //  |   +   |
                      //  |       |
                      //  |       |
                      //  |       |
                      //  ---------
                      DrawModflowTopSegmentAboveOutsideRight(
                        RowIndex, ColIndex, SegmentList, LayerIndex, ModflowGrid,
                        LocalDelegate);

                      //          ---------
                      //          |       |
                      //          |       |
                      //          |       |
                      //          |       |
                      //          |       |
                      //          |       |
                      //          |       |
                      //  -----------------
                      //  |       |  /
                      //  |       | /
                      //  |       |/
                      //  |   +   |
                      //  |       |
                      //  |       |
                      //  |       |
                      //  ---------
                      DrawModflowTopSegmentRightOutsideAbove(
                        RowIndex, ColIndex, SegmentList, LayerIndex,
                        ModflowGrid, LocalDelegate);
                    end;
                  end;
                end;
                DrawSegmentList(Direction, Bitmap32, SegmentList);
            end;
          vdFront:
            begin
              FrontPoints := nil;
              RowIndex := ModflowGrid.SelectedRow;
              if RowIndex >= ModflowGrid.RowCount then
              begin
                RowIndex := ModflowGrid.RowCount - 1;
              end;
              for ColIndex := 0 to ModflowGrid.ColumnCount - 1 do
              begin
                for LayerIndex := 0 to ModflowGrid.LayerCount - 1 do
                begin
                  if LocalDelegate.
                    SelectedCells[LayerIndex, RowIndex, ColIndex] then
                  begin
                    //   --------
                    //   |  /   |
                    //   | /    |
                    //   |/     |
                    //   |      |
                    //   |      |
                    //   |      |
                    //   --------
                    DrawModflowFrontSegmentInsideTopLeft(FrontPoints,
                      RowIndex, ColIndex, LayerIndex, ModflowGrid,
                      LocalDelegate, SegmentList);
                  
                    //  --------
                    //  |      |
                    //  |      |
                    //  |      |
                    //  |\     |
                    //  | \    |
                    //  |  \   |
                    //  --------
                    DrawModflowFrontSegmentInsideBottomLeft(FrontPoints,
                      RowIndex, ColIndex, LayerIndex, ModflowGrid, LocalDelegate,
                      SegmentList);

                    // --------
                    // |      |
                    // |      |
                    // |      |
                    // |     /|
                    // |    / |
                    // |   /  |
                    // --------
                    DrawModflowFrontSegmentInsideBottomRight(
                      RowIndex, ColIndex, LayerIndex, ModflowGrid,
                      LocalDelegate, FrontPoints, SegmentList);

                    // ---------
                    // |    \  |
                    // |     \ |
                    // |      \|
                    // |   +   |
                    // |       |
                    // |       |
                    // |       |
                    // ---------
                    DrawModflowFrontSegmentInsideTopRight(FrontPoints,
                      RowIndex, ColIndex, LayerIndex, ModflowGrid,
                      LocalDelegate, SegmentList);

                    // -----=======-----
                    // |       |       |
                    // |       |       |
                    // |       |       |
                    // |       |   +   |
                    // |       |       |
                    // |       |       |
                    // |       |       |
                    // -----------------
                    DrawModflowFrontSegmentTopLeft(
                      RowIndex, ColIndex, LayerIndex, LocalDelegate,
                      FrontPoints, SegmentList);

                    // -----------------
                    // |       |       |
                    // |       |       |
                    // |       |       |
                    // |       |   +   |
                    // |       |       |
                    // |       |       |
                    // |       |       |
                    // -----=======-----
                    DrawModflowFrontSegmentBottomLeft(ModflowGrid, LocalDelegate,
                      FrontPoints, RowIndex, ColIndex, LayerIndex, SegmentList);

                    //  ---------
                    //  |       |
                    //  |       |
                    //  |       |
                    //  |       |
                    //  +       |
                    //  +       |
                    //  +       |
                    //  +--------
                    //  +       |
                    //  +       |
                    //  +       |
                    //  |   +   |
                    //  |       |
                    //  |       |
                    //  |       |
                    //  ---------
                    DrawModflowFrontSegmentAboveLeft(FrontPoints,
                      RowIndex, ColIndex, LayerIndex, LocalDelegate, SegmentList);

                    //  ---------
                    //  |       |
                    //  |       |
                    //  |       |
                    //  |       |
                    //  |       +
                    //  |       +
                    //  |       +
                    //  --------+
                    //  |       +
                    //  |       +
                    //  |       +
                    //  |   +   |
                    //  |       |
                    //  |       |
                    //  |       |
                    //  ---------
                    DrawModflowFrontSegmentAboveRight(
                      RowIndex, ColIndex, LayerIndex, ModflowGrid, LocalDelegate,
                      FrontPoints, SegmentList);

                    // ---------
                    // |       |
                    // |       |
                    // |       |
                    // |       |
                    // |       |
                    // |       |
                    // |       |
                    // -----------------
                    //      \  |       |
                    //       \ |       |
                    //        \|       |
                    //         |   +   |
                    //         |       |
                    //         |       |
                    //         |       |
                    //         ---------
                    DrawModflowFrontSegmentLeftOutsideAbove(FrontPoints,
                      RowIndex, ColIndex, LayerIndex, LocalDelegate, SegmentList);

                    //  ---------
                    //  |       |
                    //  |       |
                    //  |       |
                    //  |       |
                    //  |       |\
                    //  |       | \
                    //  |       |  \
                    //  -----------------
                    //          |       |
                    //          |       |
                    //          |       |
                    //          |   +   |
                    //          |       |
                    //          |       |
                    //          |       |
                    //          ---------
                    DrawModflowFrontSegmentAboveOutsideLeft(FrontPoints,
                      RowIndex, ColIndex, LayerIndex, LocalDelegate, SegmentList);

                    //          ---------
                    //          |       |
                    //          |       |
                    //          |       |
                    //          |       |
                    //         /|       |
                    //        / |       |
                    //       /  |       |
                    //  -----------------
                    //  |       |
                    //  |       |
                    //  |       |
                    //  |   +   |
                    //  |       |
                    //  |       |
                    //  |       |
                    //  ---------
                    DrawModflowFrontSegmentAboveOutsideRight(LocalDelegate,
                      FrontPoints, RowIndex, ColIndex, LayerIndex,
                      ModflowGrid, SegmentList);

                  //          ---------
                  //          |       |
                  //          |       |
                  //          |       |
                  //          |       |
                  //          |       |
                  //          |       |
                  //          |       |
                  //  -----------------
                  //  |       |  /
                  //  |       | /
                  //  |       |/
                  //  |   +   |
                  //  |       |
                  //  |       |
                  //  |       |
                  //  ---------
                    DrawModflowFrontSegmentRightOutsideAbove(FrontPoints,
                      RowIndex, ColIndex, LayerIndex, ModflowGrid,
                      LocalDelegate, SegmentList);
                  end;
                end;
              end;
              DrawSegmentList(Direction, Bitmap32, SegmentList);
            end;
          vdSide:
            begin
              SidePoints := nil;
              ColIndex := ModflowGrid.SelectedColumn;
              if ColIndex >= ModflowGrid.ColumnCount then
              begin
                ColIndex := ModflowGrid.ColumnCount - 1;
              end;
              for RowIndex := 0 to ModflowGrid.RowCount - 1 do
              begin
                for LayerIndex := 0 to ModflowGrid.LayerCount - 1 do
                begin
                  if LocalDelegate.
                    SelectedCells[LayerIndex, RowIndex, ColIndex] then
                  begin
                    //   --------
                    //   |  /   |
                    //   | /    |
                    //   |/     |
                    //   |      |
                    //   |      |
                    //   |      |
                    //   --------
                    DrawModflowSideSegmentInsideTopLeft(ModflowGrid,
                      LocalDelegate, SidePoints,
                      RowIndex, ColIndex, LayerIndex, SegmentList);

                    //  --------
                    //  |      |
                    //  |      |
                    //  |      |
                    //  |\     |
                    //  | \    |
                    //  |  \   |
                    //  --------
                    DrawModflowSideSegmentInsideBottomLeft(SidePoints,
                      RowIndex, ColIndex, LayerIndex, ModflowGrid,
                      LocalDelegate, SegmentList);

                    // --------
                    // |      |
                    // |      |
                    // |      |
                    // |     /|
                    // |    / |
                    // |   /  |
                    // --------
                    DrawModflowSideSegmentInsideBottomRight(SidePoints,
                      RowIndex, ColIndex, LayerIndex, ModflowGrid,
                      LocalDelegate, SegmentList);
                  
                    // ---------
                    // |    \  |
                    // |     \ |
                    // |      \|
                    // |   +   |
                    // |       |
                    // |       |
                    // |       |
                    // ---------
                    DrawModflowSideSegmentInsideTopRight(LayerIndex, ModflowGrid,
                      LocalDelegate, SidePoints, RowIndex, ColIndex, SegmentList);

                    // -----=======-----
                    // |       |       |
                    // |       |       |
                    // |       |       |
                    // |       |   +   |
                    // |       |       |
                    // |       |       |
                    // |       |       |
                    // -----------------
                    DrawModflowSideSegmentTopLeft(RowIndex, ColIndex, LayerIndex,
                      LocalDelegate, SidePoints, SegmentList);

                    // -----------------
                    // |       |       |
                    // |       |       |
                    // |       |       |
                    // |       |   +   |
                    // |       |       |
                    // |       |       |
                    // |       |       |
                    // -----=======-----
                    DrawModflowSideSegmentBottomLeft(SidePoints,
                      RowIndex, ColIndex, LayerIndex, ModflowGrid,
                      LocalDelegate, SegmentList);

                    //  ---------
                    //  |       |
                    //  |       |
                    //  |       |
                    //  |       |
                    //  +       |
                    //  +       |
                    //  +       |
                    //  +--------
                    //  +       |
                    //  +       |
                    //  +       |
                    //  |   +   |
                    //  |       |
                    //  |       |
                    //  |       |
                    //  ---------
                    DrawModflowSideSegmentAboveLeft(
                      RowIndex, ColIndex, LayerIndex, LocalDelegate,
                      SidePoints, SegmentList);

                    //  ---------
                    //  |       |
                    //  |       |
                    //  |       |
                    //  |       |
                    //  |       +
                    //  |       +
                    //  |       +
                    //  --------+
                    //  |       +
                    //  |       +
                    //  |       +
                    //  |   +   |
                    //  |       |
                    //  |       |
                    //  |       |
                    //  ---------
                    DrawModflowSideSegmentAboveRight(SidePoints,
                      RowIndex, ColIndex, LayerIndex, ModflowGrid, LocalDelegate,
                      SegmentList);

                    // ---------
                    // |       |
                    // |       |
                    // |       |
                    // |       |
                    // |       |
                    // |       |
                    // |       |
                    // -----------------
                    //      \  |       |
                    //       \ |       |
                    //        \|       |
                    //         |   +   |
                    //         |       |
                    //         |       |
                    //         |       |
                    //         ---------
                    DrawModflowSideSegmentLeftOutsideAbove(
                      RowIndex, ColIndex, LayerIndex, LocalDelegate, SidePoints,
                      SegmentList);

                    //  ---------
                    //  |       |
                    //  |       |
                    //  |       |
                    //  |       |
                    //  |       |\
                    //  |       | \
                    //  |       |  \
                    //  -----------------
                    //          |       |
                    //          |       |
                    //          |       |
                    //          |   +   |
                    //          |       |
                    //          |       |
                    //          |       |
                    //          ---------
                    DrawModflowSideSegmentAboveOutsideLeft(SidePoints,
                      RowIndex, ColIndex, LayerIndex, LocalDelegate, SegmentList);

                    //          ---------
                    //          |       |
                    //          |       |
                    //          |       |
                    //          |       |
                    //         /|       |
                    //        / |       |
                    //       /  |       |
                    //  -----------------
                    //  |       |
                    //  |       |
                    //  |       |
                    //  |   +   |
                    //  |       |
                    //  |       |
                    //  |       |
                    //  ---------
                    DrawModflowSideSegmentAboveOutsideRight(
                      RowIndex, ColIndex, LayerIndex, ModflowGrid, LocalDelegate,
                      SidePoints, SegmentList);

                    //          ---------
                    //          |       |
                    //          |       |
                    //          |       |
                    //          |       |
                    //          |       |
                    //          |       |
                    //          |       |
                    //  -----------------
                    //  |       |  /
                    //  |       | /
                    //  |       |/
                    //  |   +   |
                    //  |       |
                    //  |       |
                    //  |       |
                    //  ---------
                    DrawModflowSideSegmentRightOutsideAbove(SidePoints,
                      LocalDelegate, ModflowGrid,
                      LayerIndex, ColIndex, RowIndex, SegmentList);
                  end;
                end;
              end;
              DrawSegmentList(Direction, Bitmap32, SegmentList);
            end;
          else
            Assert(False);
        end;
      finally
        for ObjectIndex1 := SegmentList.Count - 1 downto 0 do
        begin
          TObject(SegmentList[ObjectIndex1]).Free;
        end;
        SegmentList.Free;
      end;
    end;
  finally
    LocalDelegate.SelectedCells.Clear;
  end;
end;

procedure TScreenObject.CreateBoundaryDataSets;
begin
  if FBoundaryDataSets = nil then
  begin
    FBoundaryDataSets := TList.Create;
  end;
end;

procedure TScreenObject.CreateBoundaryDataSetSubscriptions;
begin
  if FBoundaryDataSetSubscriptions = nil then
  begin
    FBoundaryDataSetSubscriptions := TObjectList.Create;
  end;
end;

procedure TScreenObject.CreateBoundaryDataSetFormulas;
begin
  if FBoundaryDataSetFormulas = nil then
  begin
    FBoundaryDataSetFormulas := TList.Create;
  end;
end;

procedure TScreenObject.CreateSectionStarts;
begin
  if FSectionStarts = nil then
  begin
    FSectionStarts := TValueArrayStorage.Create;
    FSectionStarts.DataType := rdtInteger;
  end;
end;

procedure TScreenObject.CreateBottomElevationSubscription;
begin
  if FBottomElevSubscription = nil then
  begin
    FBottomElevSubscription := TObserver.Create(nil);
  end;
  FBottomElevSubscription.UpdateWithName(Name + '_Bottom_Elevation_Formula');
  FBottomElevSubscription.OnUpToDateSet := Changed;
  FBottomElevSubscription.OnNotify := Changed;
end;

procedure TScreenObject.CreateTopElevationSubscription;
begin
  if FTopElevSubscription = nil then
  begin
    FTopElevSubscription := TObserver.Create(nil);
  end;
  FTopElevSubscription.UpdateWithName(Name + '_Top_Elevation_Formula');
  FTopElevSubscription.OnUpToDateSet := Changed;
  FTopElevSubscription.OnNotify := Changed;
end;

procedure TScreenObject.CreateUzfBoundary;
begin
  if (ModflowBoundaries.FModflowUzfBoundary = nil) then
  begin
    ModflowBoundaries.FModflowUzfBoundary := TUzfBoundary.Create(FModel, self);
  end;
end;

procedure TScreenObject.CreateElevationSubscription;
begin
  if FElevSubscription = nil then
  begin
    FElevSubscription := TObserver.Create(nil);
  end;
  FElevSubscription.UpdateWithName(Name + '_Elevation_Formula');
  FElevSubscription.OnUpToDateSet := Changed;
  FElevSubscription.OnNotify := Changed;
end;

procedure TScreenObject.CreatePhastWellBoundary;
begin
  if FWellBoundary = nil then
  begin
    FWellBoundary := TWellBoundary.Create(self, FModel);
    FWellBoundary.ScreenObject := self;
  end;
end;

procedure TScreenObject.CreatePhastSpecifiedSolutionBoundary;
begin
  if FSpecifiedSolutionBoundary = nil then
  begin
    FSpecifiedSolutionBoundary :=
      TSpecifiedSolutionBoundary.Create(self, FModel);
    FSpecifiedSolutionBoundary.ScreenObject := self;
  end;
end;

procedure TScreenObject.CreatePhastSpecifiedHeadBoundary;
begin
  if FSpecifiedHeadBoundary = nil then
  begin
    FSpecifiedHeadBoundary := TSpecifiedHeadBoundary.Create(self, FModel);
    FSpecifiedHeadBoundary.ScreenObject := self;
  end;
end;

procedure TScreenObject.CreatePhastRiverBoundary;
begin
  if FRiverBoundary = nil then
  begin
    FRiverBoundary := TRiverBoundary.Create(self, FModel);
    FRiverBoundary.ScreenObject := self;
  end;
end;

procedure TScreenObject.CreatePhastLeakyBoundary;
begin
  if FLeakyBoundary = nil then
  begin
    FLeakyBoundary := TLeakyBoundary.Create(self, FModel);
    FLeakyBoundary.ScreenObject := self;
    FLeakyBoundary.Orientation := FViewDirection;
  end;
end;

procedure TScreenObject.CreatePhastFluxBoundary;
begin
  if FFluxBoundary = nil then
  begin
    FFluxBoundary := TFluxBoundary.Create(self, FModel);
    FFluxBoundary.ScreenObject := self;
    FFluxBoundary.Orientation := FViewDirection;
  end;
end;

procedure TScreenObject.CreateChdBoundary;
begin
  if (ModflowBoundaries.FModflowChdBoundary = nil) then
  begin
    ModflowBoundaries.FModflowChdBoundary := TChdBoundary.Create(FModel, self);
  end;
end;

procedure TScreenObject.CreateGhbBoundary;
begin
  if (ModflowBoundaries.FModflowGhbBoundary = nil) then
  begin
    ModflowBoundaries.FModflowGhbBoundary := TGhbBoundary.Create(FModel, self);
  end;
end;

procedure TScreenObject.CreateWelBoundary;
begin
  if (ModflowBoundaries.FModflowWellBoundary = nil) then
  begin
    ModflowBoundaries.FModflowWellBoundary :=
      TMfWellBoundary.Create(FModel, self);
  end;
end;

procedure TScreenObject.CreateRivBoundary;
begin
  if (ModflowBoundaries.FModflowRivBoundary = nil) then
  begin
    ModflowBoundaries.FModflowRivBoundary := TRivBoundary.Create(FModel, self);
  end;
end;

procedure TScreenObject.CreateDrnBoundary;
begin
  if (ModflowBoundaries.FModflowDrnBoundary = nil) then
  begin
    ModflowBoundaries.FModflowDrnBoundary := TDrnBoundary.Create(FModel, self);
  end;
end;

procedure TScreenObject.CreateDrtBoundary;
begin
  if (ModflowBoundaries.FModflowDrtBoundary = nil) then
  begin
    ModflowBoundaries.FModflowDrtBoundary := TDrtBoundary.Create(FModel, self);
  end;
end;

procedure TScreenObject.CreateMnw2Boundary;
begin
  if (ModflowBoundaries.FModflowMnw2Boundary = nil) then
  begin
    ModflowBoundaries.FModflowMnw2Boundary := TMnw2Boundary.Create(FModel, self);
  end;
end;

procedure TScreenObject.CreateRchBoundary;
begin
  if (ModflowBoundaries.FModflowRchBoundary = nil) then
  begin
    ModflowBoundaries.FModflowRchBoundary := TRchBoundary.Create(FModel, self);
  end;
end;

procedure TScreenObject.CreateEvtBoundary;
begin
  if (ModflowBoundaries.FModflowEvtBoundary = nil) then
  begin
    ModflowBoundaries.FModflowEvtBoundary := TEvtBoundary.Create(FModel, self);
  end;
end;

procedure TScreenObject.CreateEtsBoundary;
begin
  if (ModflowBoundaries.FModflowEtsBoundary = nil) then
  begin
    ModflowBoundaries.FModflowEtsBoundary := TEtsBoundary.Create(FModel, self);
  end;
end;

procedure TScreenObject.CreateResBoundary;
begin
  if (ModflowBoundaries.FModflowResBoundary = nil) then
  begin
    ModflowBoundaries.FModflowResBoundary := TResBoundary.Create(FModel, self);
  end;
end;

procedure TScreenObject.CreateLakBoundary;
begin
  if (ModflowBoundaries.FModflowLakBoundary = nil) then
  begin
    ModflowBoundaries.FModflowLakBoundary := TLakBoundary.Create(FModel, self);
  end;
end;

procedure TScreenObject.CreateHfbBoundary;
begin
  if (ModflowBoundaries.FModflowHfbBoundary = nil) then
  begin
    ModflowBoundaries.FModflowHfbBoundary := THfbBoundary.Create(FModel, self);
  end;
end;

procedure TScreenObject.CreateSfrBoundary;
begin
  if (ModflowBoundaries.FModflowSfrBoundary = nil) then
  begin
    ModflowBoundaries.FModflowSfrBoundary := TSfrBoundary.Create(FModel, self);
  end;
end;

procedure TScreenObject.CreateGagBoundary;
begin
  if (ModflowBoundaries.FModflowGage = nil) then
  begin
    ModflowBoundaries.FModflowGage := TStreamGage.Create(FModel, self);
  end;
end;


procedure TScreenObject.DeleteExtraSections;
var
  SectionIndex: Integer;
  Value1: integer;
  Value2: integer;
begin
  for SectionIndex := SectionStarts.Count - 2 downto 0 do
  begin
    Value1 := SectionStarts.IntValues[SectionIndex];
    Value2 := SectionStarts.IntValues[SectionIndex + 1];
    if (Value2 >= Count) or (Value1 = Value2) then
    begin
      SectionStarts.Delete(SectionIndex + 1);
    end;
    if Value1 = 0 then
    begin
      SectionStarts.Delete(SectionIndex);
    end;
  end;
end;

procedure TScreenObject.AssignTopDataSetValues(const Grid: TCustomGrid;
  Expression: TExpression; const DataSetFunction: string; Compiler: TRbwParser;
  UsedVariables: TStringList; OtherData: TObject; const DataSet: TDataArray;
  AssignmentLocation: TAssignmentLocation = alAll);
var
  CellList: TCellAssignmentList;
  CellAssignment: TCellAssignment;
  AssignmentIndex: Integer;
begin
  CellList := TCellAssignmentList.Create;
  try
    InitializeVariables(UsedVariables, DataSet, Expression, Compiler);
    Delegate.GetTopCellsToAssign(Grid, DataSetFunction, OtherData, DataSet,
      CellList, AssignmentLocation);
    for AssignmentIndex := 0 to CellList.Count - 1 do
    begin
      CellAssignment := CellList[AssignmentIndex];
      UpdateCurrentSegment(CellAssignment.Segment);
      UpdateCurrentSection(CellAssignment.Section);
      UpdateImportedValues(DataSet);
      AssignCellValue(UsedVariables, DataSet, CellAssignment.Layer,
        CellAssignment.Row, CellAssignment.Column, Compiler,
        CellAssignment.Annotation, Expression, OtherData);
    end;
  finally
    CellList.Free;
  end;
end;

function TScreenObject.GetImportedHigherSectionElevations: TValueArrayStorage;
begin
  CreateValueArrayStorage(FImportedHigherSectionElevations);
  result := FImportedHigherSectionElevations;
end;

function TScreenObject.GetImportedLowerSectionElevations: TValueArrayStorage;
begin
  CreateValueArrayStorage(FImportedLowerSectionElevations);
  result := FImportedLowerSectionElevations;
end;

function TScreenObject.GetImportedSectionElevations: TValueArrayStorage;
begin
  CreateValueArrayStorage(FImportedSectionElevations);
  result := FImportedSectionElevations;
end;

procedure TScreenObject.GetInterpDistance(const InterpValue: TInterpValuesItem;
  var Distance: Double; const DataSet: TDataArray;
  const LayerIndex, RowIndex, ColIndex: Integer);
var
  DI: Integer;
  Compiler: TRbwParser;
  TempFormula: string;
begin
  Distance := 0;
  case InterpValue.Values.InterpolationDirection of
    pidX:
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              Distance := ((FModel as TPhastModel).PhastGrid.
                ColumnPosition[ColIndex]
                + (FModel as TPhastModel).PhastGrid.
                ColumnPosition[ColIndex + 1]) / 2;
            end;
          eaNodes:
            begin
              Distance := (FModel as TPhastModel).PhastGrid.
                ColumnPosition[ColIndex];
            end;
        else
          Assert(False);
        end;
      end;
    pidY:
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              Distance := ((FModel as TPhastModel).PhastGrid.
                RowPosition[RowIndex]
                + (FModel as TPhastModel).PhastGrid.
                RowPosition[RowIndex + 1]) / 2;
            end;
          eaNodes:
            begin
              Distance := (FModel as TPhastModel).PhastGrid.
                RowPosition[RowIndex];
            end;
        else
          Assert(False);
        end;
      end;
    pidZ:
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              Distance := ((FModel as TPhastModel).PhastGrid.
                LayerElevation[LayerIndex]
                + (FModel as TPhastModel).PhastGrid.
                LayerElevation[LayerIndex + 1]) / 2;
            end;
          eaNodes:
            begin
              Distance := (FModel as TPhastModel).PhastGrid.
                LayerElevation[LayerIndex];
            end;
        else
          Assert(False);
        end;
      end;
    pidMix:
      begin
        UpdateVariables(FMixtureVariables, DataSet, LayerIndex, RowIndex,
          ColIndex, FMixtureCompiler);
        try
          FMixtureExpression.Evaluate;
        except on E: ERbwParserError do
          begin
            DI := IndexOfDataSet(DataSet);

            frmFormulaErrors.AddError(Name, 'Mixture formula for: '
              + DataSet.Name, MixtureDataSetFormula[DI], E.Message);
            MixtureDataSetFormula[DI] := '0.5';

            Compiler := GetCompiler(DataSet.Orientation);
            TempFormula := MixtureDataSetFormula[DI];
            Compiler.Compile(TempFormula);
            FMixtureExpression := Compiler.CurrentExpression;
            FMixtureExpression.Evaluate;
          end;
        end;
        Distance := 1 - FMixtureExpression.DoubleResult;
      end;
  else
    Assert(False);
  end;
end;

function TScreenObject.PhastBoundaryType: TBoundaryTypes;
begin
  result := btNone;
  if (SpecifiedHeadBoundary.BoundaryValue.Count > 0)
    or (SpecifiedHeadBoundary.Solution.Count > 0) then
  begin
    result := btSpecifiedHead;
  end;

  if (FluxBoundary.BoundaryValue.Count > 0)
    or (FluxBoundary.Solution.Count > 0) then
  begin
    Assert(result = btNone);
    result := btFlux;
  end;

  if (LeakyBoundary.BoundaryValue.Count > 0)
    or (LeakyBoundary.Solution.Count > 0) then
  begin
    Assert(result = btNone);
    result := btLeaky;
  end;

  if (RiverBoundary.BoundaryValue.Count > 0)
    or (RiverBoundary.Solution.Count > 0) then
  begin
    Assert(result = btNone);
    result := btRiver;
  end;

  if (WellBoundary.BoundaryValue.Count > 0)
    or (WellBoundary.Solution.Count > 0) then
  begin
    Assert(result = btNone);
    result := btWell;
  end;
end;

function TScreenObject.BoundaryType: integer;
begin
  result := Ord(PhastBoundaryType);
end;

function TScreenObject.BoundaryTypeUsed: TBoundaryTypes;
begin
  result := btNone;
  if (FFluxBoundary <> nil)
    and ((FluxBoundary.BoundaryValue.Count > 0)
    or (FluxBoundary.Solution.Count > 0)) then
  begin
    Assert(result = btNone);
    result := btFlux;
  end;

  if (FLeakyBoundary <> nil)
    and ((LeakyBoundary.BoundaryValue.Count > 0)
    or (LeakyBoundary.Solution.Count > 0)) then
  begin
    Assert(result = btNone);
    result := btLeaky;
  end;

  if (FRiverBoundary <> nil)
    and ((RiverBoundary.BoundaryValue.Count > 0)
    or (RiverBoundary.Solution.Count > 0)) then
  begin
    Assert(result = btNone);
    result := btRiver;
  end;

  if (FSpecifiedHeadBoundary <> nil)
    and ((SpecifiedHeadBoundary.BoundaryValue.Count > 0)
    or (SpecifiedHeadBoundary.Solution.Count > 0)) then
  begin
    Assert(result = btNone);
    result := btSpecifiedHead;
  end;

  if (FWellBoundary <> nil)
    and ((WellBoundary.BoundaryValue.Count > 0)
    or (WellBoundary.Solution.Count > 0)) then
  begin
    Assert(result = btNone);
    result := btWell;
  end;
end;

procedure TScreenObject.OtherIndex(const LayerOrRow, RowOrColumn: integer;
  out First, Last: integer; const DataSet: TDataArray);
var
  IsRiverDataSet: boolean;
  Model: TPhastModel;
begin
  Model := FModel as TPhastModel;
  IsRiverDataSet := (RiverBoundary.BoundaryValue.Count > 0)
    or (RiverBoundary.Solution.Count > 0);
  if IsRiverDataSet then
  begin
    IsRiverDataSet := (DataSet = Model.Top2DBoundaryType);

    if not IsRiverDataSet then
    begin
      if (DataSet is TSparseArrayPhastInterpolationDataSet) then
      begin
        IsRiverDataSet :=
          (Model.RiverHead.IndexOfDataSet
          (TSparseArrayPhastInterpolationDataSet(DataSet)) >= 0)
          or
          (Model.RiverAssociatedSolution.IndexOfDataSet
          (TSparseArrayPhastInterpolationDataSet(DataSet)) >= 0);
      end;
      if not IsRiverDataSet then
      begin
        IsRiverDataSet := (Model.RiverDataSets.IndexOf(DataSet) >= 0);
      end;
      if not IsRiverDataSet then
      begin
        if DataSet is TIntegerSparseDataSet then
        begin
          IsRiverDataSet :=
            TIntegerSparseDataSet(DataSet).IsBoundaryTypeDataSet;
        end;
      end;
    end;
  end;
  { TODO :
Changing the layer as woiuld be done here prevents the river data from
being displayed on the status bar.  Find a way around this problem. }

  {if IsRiverDataSet then
  begin
    First := frmGoPhast.PhastGrid.LayerCount;
    Last := First;
  end
  else
  begin  }
    Delegate.OtherIndex(LayerOrRow, RowOrColumn, First, Last, DataSet)
//  end;
end;

{ TCustomPhastBoundaryCondition }

procedure TCustomPhastBoundaryCondition.AddMixtureSubscriptions;
begin
  AddDataSetSubscriptions(FMixtureDataSetList, FMixtureObserver,
    ResetMixtureFormula, Values.MixtureFormula);
end;

procedure TCustomPhastBoundaryCondition.AddFormulaSubscriptions;
begin
  AddDataSetSubscriptions(FFormulaDataSetList, FFormulaObserver,
    ResetFormulaExpression, FFormulaObject.Formula);
end;

procedure TCustomPhastBoundaryCondition.Assign(Source: TPersistent);
var
  OtherBoundary: TCustomPhastBoundaryCondition;
  Item: TInterpValuesItem;
begin
  if Source is TCustomPhastBoundaryCondition then
  begin
    OtherBoundary := TCustomPhastBoundaryCondition(Source);
    MixtureExpression := OtherBoundary.MixtureExpression;
    InterpolationDirection := OtherBoundary.InterpolationDirection;
    FormulaExpression := OtherBoundary.FormulaExpression;
    Time := OtherBoundary.Time;
    // Fix source in case the MixtureExpression was invalid.
    OtherBoundary.MixtureExpression := MixtureExpression;
  end
  else if Source is TInterpValuesItem then
  begin
    Item := TInterpValuesItem(Source);
    MixtureExpression := Item.Values.MixtureFormula;
    InterpolationDirection := Item.Values.InterpolationDirection;
    // Fix source in case the MixtureExpression was invalid.
    Item.Values.MixtureFormula := MixtureExpression;
  end;
  inherited;
end;

procedure GlobalRemovePhastBoundarySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TCustomPhastBoundaryCondition).RemoveSubscription(Sender, AName);
end;

procedure GlobalRestorePhastBoundarySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TCustomPhastBoundaryCondition).RestoreSubscription(Sender, AName);
end;

constructor TCustomPhastBoundaryCondition.Create(Collection: TCollection);
begin
  inherited;
  FFormulaObserver := TObserver.Create(nil);
  FMixtureDataSetList := TList.Create;
  FMixtureObserver := TObserver.Create(nil);
  FFormulaDataSetList := TList.Create;
  FFormulaObject := frmGoPhast.PhastModel.FormulaManager.Add;
  FFormulaObject.Parser := frmGoPhast.PhastModel.rpThreeDFormulaCompilerNodes;
  FFormulaObject.AddSubscriptionEvents(
    GlobalRemovePhastBoundarySubscription, GlobalRestorePhastBoundarySubscription, self);
end;

destructor TCustomPhastBoundaryCondition.Destroy;
begin
  RemoveMixtureSubscriptions;
  FMixtureObserver.Free;
  FMixtureDataSetList.Free;
  FFormulaObserver.Free;
  FFormulaDataSetList.Free;
  frmGoPhast.PhastModel.FormulaManager.Remove(FFormulaObject,
    GlobalRemovePhastBoundarySubscription, GlobalRestorePhastBoundarySubscription, self);
  inherited;
end;

function TCustomPhastBoundaryCondition.GetScreenObject: TScreenObject;
begin
  result := (Collection as TCustomPhastBoundaryCollection).FScreenObject;
end;

function TCustomPhastBoundaryCondition.GetDataSet:
  TSparseArrayPhastInterpolationDataSet;
begin
  result := (Collection as TCustomPhastBoundaryCollection).GetDataSet(Time);
end;

procedure TCustomPhastBoundaryCondition.AddDataSetSubscriptions(
  DataSetList: TList; Observer: TObserver; Reset: TResetProcedure;
  NewFormula: string);
var
  Expression: TExpression;
  TempFormula: string;
  Compiler: TRbwParser;
  DS: TDataArray;
  AScreenObject: TScreenObject;
  Model: TPhastModel;
  Obs: TObserver;
  UseIndex: Integer;
  UsedVariables: TStringList;
begin
  Model := (Collection as TPhastCollection).Model as TPhastModel;
  if Model = nil then
  begin
    Exit;
  end;
  if ScreenObject.Model = nil then
  begin
    Exit;
  end;
  AScreenObject := ScreenObject;
  DS := GetDataSet;
  if (AScreenObject <> nil) and (DS <> nil) then
  begin
    Observer.OnUpToDateSet := (DS as TSparseArrayPhastInterpolationDataSet).ATimeList.Changed;
    AScreenObject.TalksTo(Observer);
    Compiler := AScreenObject.GetCompiler(DS.Orientation, AScreenObject.EvaluatedAt);
    TempFormula := NewFormula;
    try
      Compiler.Compile(TempFormula);
    except
      on ERbwParserError do
      begin
        Reset(Compiler);
//        TempFormula := '0.5';
//        Values.MixtureFormula := TempFormula;
//        Compiler.Compile(TempFormula);
      end;
    end;
    Expression := Compiler.CurrentExpression;
    UsedVariables := TStringList.Create;
    try
      UsedVariables.Assign(Expression.VariablesUsed);
      for UseIndex := 0 to UsedVariables.Count - 1 do
      begin
        Obs := Model.GetObserverByName(UsedVariables[UseIndex]);
        Assert(Obs <> nil);
        Obs.TalksTo(Observer);
        DataSetList.Add(Obs);
      end;
    finally
      UsedVariables.Free;
    end;
    Observer.UpToDate := True;
    Observer.UpToDate := False;
  end;
end;

procedure TCustomPhastBoundaryCondition.RemoveDataSetSubscriptions(DataSetList: TList; Observer: TObserver);
var
  AScreenObject: TScreenObject;
  DS: TObserver;
  Index: Integer;
begin
  Observer.OnUpToDateSet := nil;
  AScreenObject := ScreenObject;
  if AScreenObject <> nil then
  begin
    AScreenObject.StopsTalkingTo(Observer);
  end;
  Observer.StopTalkingToAnyone;
  for Index := 0 to DataSetList.Count - 1 do
  begin
    DS := DataSetList[Index];
    DS.StopsTalkingTo(Observer);
  end;
  DataSetList.Clear;
end;

function TCustomPhastBoundaryCondition.GetDistance1: double;
begin
  result := Values.Distance1;
end;

function TCustomPhastBoundaryCondition.GetDistance2: double;
begin
  result := Values.Distance2;
end;

function TCustomPhastBoundaryCondition.GetFormulaExpression: string;
begin
  result := FFormulaObject.Formula
end;

function TCustomPhastBoundaryCondition.GetInterpolationDirection:
  TInterpolationDirection;
begin
  result := Values.InterpolationDirection;
end;

function TCustomPhastBoundaryCondition.GetMixtureFormula: string;
begin
  result := Values.MixtureFormula;
end;

function TCustomPhastBoundaryCondition.GetUsePHAST_Interpolation: boolean;
begin
  result := Values.UsePHAST_Interpolation;
end;

procedure TCustomPhastBoundaryCondition.InvalidateModel;
begin
  if ScreenObject <> nil then
  begin
    ScreenObject.InvalidateModel;
  end;
end;

procedure TCustomPhastBoundaryCondition.RemoveMixtureSubscriptions;
begin
  RemoveDataSetSubscriptions(FMixtureDataSetList, FMixtureObserver);
end;

procedure TCustomPhastBoundaryCondition.RemoveSubscription(Sender: TObject;
  const AName: string);
var
  Model: TPhastModel;
  DS: TObserver;
begin
  Model := (Collection as TCustomPhastBoundaryCollection).Model as TPhastModel;
  if Model <> nil then
  begin
    DS := Model.GetObserverByName(AName);
    Assert(DS <> nil);
    DS.StopsTalkingTo(FFormulaObserver);
  end;
end;

procedure TCustomPhastBoundaryCondition.RemoveFormulaSubscriptions;
begin
  RemoveDataSetSubscriptions(FFormulaDataSetList, FFormulaObserver);
end;

procedure TCustomPhastBoundaryCondition.ResetFormulaExpression(
  Compiler: TRbwParser);
var
  TempFormula: string;
begin
  TempFormula := '0.';
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FFormulaObject, TempFormula, Compiler,
    GlobalRemovePhastBoundarySubscription, GlobalRestorePhastBoundarySubscription, self);
  Compiler.Compile(TempFormula);
end;

procedure TCustomPhastBoundaryCondition.ResetMixtureFormula(
  Compiler: TRbwParser);
var
  TempFormula: string;
begin
  TempFormula := '0.5';
  Values.MixtureFormula := TempFormula;
  Compiler.Compile(TempFormula);
end;

procedure TCustomPhastBoundaryCondition.ResetMixtureSubscription;
begin
  FMixtureObserver.UpToDate := True;
end;

procedure TCustomPhastBoundaryCondition.RestoreSubscription(Sender: TObject;
  const AName: string);
var
  Model: TPhastModel;
  Observer: TObserver;
  DataArray: TSparseArrayPhastInterpolationDataSet;
begin
  Model := (Collection as TCustomPhastBoundaryCollection).Model as TPhastModel;
  if Model <> nil then
  begin
    Observer := Model.GetObserverByName(AName);
    Assert(Observer <> nil);
    Observer.TalksTo(FFormulaObserver);

    DataArray := GetDataSet;
    if  (DataArray <> nil) then
    begin
      FFormulaObserver.UpToDate := False;
      DataArray.ATimeList.Changed(FFormulaObserver);
    end;
  end;
end;

procedure TCustomPhastBoundaryCondition.SetDistance1(const Value: double);
begin
  if Values.Distance1 <> Value then
  begin
    Values.Distance1 := Value;
    InvalidateModel;
  end;
end;

procedure TCustomPhastBoundaryCondition.SetDistance2(const Value: double);
begin
  if Values.Distance2 <> Value then
  begin
    Values.Distance2 := Value;
    InvalidateModel;
  end;
end;

procedure TCustomPhastBoundaryCondition.SetFormula(const Value: string);
begin
    FFormula := Value;
//  if FFormula <> Value then
//  begin
//    InvalidateModel;
//  end;
end;

procedure TCustomPhastBoundaryCondition.SetFormulaExpression(
  const Value: string);
begin
  if FormulaExpression <> Value then
  begin
    RemoveFormulaSubscriptions;
    frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
      FFormulaObject, Value, frmGoPhast.PhastModel.rpThreeDFormulaCompilerNodes,
      GlobalRemovePhastBoundarySubscription, GlobalRestorePhastBoundarySubscription, self);
    AddFormulaSubscriptions;
    InvalidateModel;
  end;
end;

procedure TCustomPhastBoundaryCondition.SetInterpolationDirection(
  const Value: TInterpolationDirection);
begin
  if Values.InterpolationDirection <> Value then
  begin
    Values.InterpolationDirection := Value;
    if Value = pidMix then
    begin
      AddMixtureSubscriptions;
    end
    else
    begin
      RemoveMixtureSubscriptions;
    end;
    InvalidateModel;
  end;
end;

procedure TCustomPhastBoundaryCondition.SetMixtureExpression(
  const Value: string);
begin
  if Values.MixtureFormula <> Value then
  begin
    RemoveMixtureSubscriptions;
    Values.MixtureFormula := Value;
    if Values.InterpolationDirection = pidMix then
    begin
      AddMixtureSubscriptions;
    end;
    InvalidateModel;
  end;
end;

procedure TCustomPhastBoundaryCondition.SetMixtureFormula(const Value: string);
begin
  FMixtureFormula := Value;
end;

procedure TCustomPhastBoundaryCondition.SetTime(const Value: double);
begin
  if FTime <> Value then
  begin
    FTime := Value;
    InvalidateModel;
  end;
end;

procedure TCustomPhastBoundaryCondition.SetUsePHAST_Interpolation(
  const Value: boolean);
begin
  if Values.UsePHAST_Interpolation <> Value then
  begin
    Values.UsePHAST_Interpolation := Value;
    InvalidateModel;
  end;
end;

procedure TCustomPhastBoundaryCondition.UpdateFormulaExpression;
begin
  FormulaExpression := FFormula;
end;

procedure TCustomPhastBoundaryCondition.UpdateMixtureExpression;
begin
  MixtureExpression := FMixtureFormula;
end;

{ TRealPhastBoundaryCondition }

function TRealPhastBoundaryCondition.GetDatatype: TRbwDataType;
begin
  result := rdtDouble;
end;

function TRealPhastBoundaryCondition.GetValue1: double;
begin
  result := Values.RealValue1;
end;

function TRealPhastBoundaryCondition.GetValue2: double;
begin
  result := Values.RealValue2;
end;

procedure TRealPhastBoundaryCondition.SetValue1(const Value: double);
begin
  if Values.RealValue1 <> Value then
  begin
    Values.RealValue1 := Value;
    InvalidateModel;
  end;
end;

procedure TRealPhastBoundaryCondition.SetValue2(const Value: double);
begin
  if Values.RealValue2 <> Value then
  begin
    Values.RealValue2 := Value;
    InvalidateModel;
  end;
end;

{ TIntegerPhastBoundaryCondition }

function TIntegerPhastBoundaryCondition.GetDatatype: TRbwDataType;
begin
  result := rdtInteger;
end;

function TIntegerPhastBoundaryCondition.GetValue1: integer;
begin
  result := Values.IntValue1;
end;

function TIntegerPhastBoundaryCondition.GetValue2: integer;
begin
  result := Values.IntValue2;
end;

procedure TIntegerPhastBoundaryCondition.SetValue1(const Value: integer);
begin
  if Values.IntValue1 <> Value then
  begin
    Values.IntValue1 := Value;
    InvalidateModel;
  end;
end;

procedure TIntegerPhastBoundaryCondition.SetValue2(const Value: integer);
begin
  if Values.IntValue2 <> Value then
  begin
    Values.IntValue2 := Value;
    InvalidateModel;
  end;
end;

{ TRealPhastBoundaries }

constructor TRealPhastBoundaries.Create(Model: TComponent);
begin
  inherited Create(TRealPhastBoundaryCondition, Model);
end;

function TRealPhastBoundaries.GetDatatype: TRbwDataType;
begin
  result := rdtDouble;
end;

{ TIntegerPhastBoundaries }

constructor TIntegerPhastBoundaries.Create(Model: TComponent);
begin
  inherited Create(TIntegerPhastBoundaryCondition, Model);
end;

function TIntegerPhastBoundaries.GetDatatype: TRbwDataType;
begin
  result := rdtInteger;
end;

{ TFluxBoundary }

constructor TFluxBoundary.Create(ScreenObject: TScreenObject;
  Model: TComponent);
begin
  inherited Create(ScreenObject, Model);
  FBoundaryValue.PropName := 'Flux';
  FSolution.PropName := 'Flux_Associated_Solution';
end;

procedure TFluxBoundary.SetScreenObject(const Value: TScreenObject);
begin
  inherited;
  if (FSolution <> nil) and (FBoundaryValue <> nil) then
  begin
    FSolution.ScreenObject := Value;
    FBoundaryValue.ScreenObject := Value;
  end;
end;

procedure TFluxBoundary.SetOrientation(const Value: TViewDirection);
var
  Model: TPhastModel;
begin
  inherited;
  if (FModel <> nil) and (FBoundaryValue <> nil) and (FSolution <> nil) then
  begin
    Model := FModel as TPhastModel;
    case Value of
      vdTop:
        begin
          FBoundaryValue.TimeList := Model.TopFluxBoundaryFlux;
          FSolution.TimeList :=
            Model.TopFluxBoundaryChemistry;
        end;
      vdFront:
        begin
          FBoundaryValue.TimeList := Model.FrontFluxBoundaryFlux;
          FSolution.TimeList :=
            Model.FrontFluxBoundaryChemistry;
        end;
      vdSide:
        begin
          FBoundaryValue.TimeList := Model.SideFluxBoundaryFlux;
          FSolution.TimeList :=
            Model.SideFluxBoundaryChemistry;
        end;
    else
      Assert(False);
    end;
  end;
end;

{ TLeakyBoundary }

procedure TLeakyBoundary.Reset;
begin
  if (BoundaryValue.Count = 0) and (Solution.Count = 0) then
  begin
    Thickness := '';
    HydraulicConductivity := '';
    InvalidateModel;
  end;
end;

procedure TLeakyBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TLeakyBoundary;
begin
  InvalidateModel;
  if Source is TLeakyBoundary then
  begin
    SourceBoundary := TLeakyBoundary(Source);
    Thickness := SourceBoundary.Thickness;
    HydraulicConductivity := SourceBoundary.HydraulicConductivity;
  end;
  inherited;
end;

constructor TLeakyBoundary.Create(ScreenObject: TScreenObject;
  Model: TComponent);
begin
  inherited Create(ScreenObject, Model);
  FBoundaryValue.PropName := 'Leaky_Head';
  FSolution.PropName := 'Leaky_Associated_Solution';
end;

function TLeakyBoundary.GetHydraulicConductivity: string;
var
  DataSetName: string;
begin
  if (BoundaryValue.Count > 0) or (Solution.Count > 0) then
  begin
    if (ScreenObject <> nil) and (FModel <> nil) then
    begin
      case Orientation of
        vdTop:
          begin
            DataSetName := rsTopLeakyHydraulicConductivity;
          end;
        vdFront:
          begin
            DataSetName := rsFrontLeakyHydraulicConductivity;
          end;
        vdSide:
          begin
            DataSetName := rsSideLeakyHydraulicConductivity;
          end;
      end;
      Result := BoundaryDataSetFormula(DataSetName);
    end
    else
    begin
      result := FHydraulicConductivity;
    end;
  end
  else
  begin
    result := '';
  end;
end;

function TLeakyBoundary.GetThickness: string;
var
  DataSetName: string;
begin
  if (BoundaryValue.Count > 0) or (Solution.Count > 0) then
  begin
    if (ScreenObject <> nil) and (FModel <> nil) then
    begin
      case Orientation of
        vdTop:
          begin
            DataSetName := rsTopLeakyThickness;
          end;
        vdFront:
          begin
            DataSetName := rsFrontLeakyThickness;
          end;
        vdSide:
          begin
            DataSetName := rsSideLeakyThickness;
          end;
      end;
      Result := BoundaryDataSetFormula(DataSetName);
    end
    else
    begin
      result := FThickness;
    end;
  end
  else
  begin
    result := '';
  end;
end;

procedure TLeakyBoundary.Loaded;
begin
  if (BoundaryValue.Count > 0) or (Solution.Count > 0) then
  begin
    SetHydraulicConductivity(FHydraulicConductivity);
    SetThickness(FThickness)
  end;
end;

procedure TLeakyBoundary.PartialAssign(Source: TPersistent);
var
  SourceBoundary: TLeakyBoundary;
begin
  if Source is TLeakyBoundary then
  begin
    SourceBoundary := TLeakyBoundary(Source);
    BoundaryValue := SourceBoundary.BoundaryValue;
    Solution := SourceBoundary.Solution;
    InvalidateModel;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TLeakyBoundary.SetHydraulicConductivity(const Value: string);
begin
  if FHydraulicConductivity <> Trim(Value) then
  begin
    FHydraulicConductivity := Trim(Value);
    InvalidateModel;
  end;
  case Orientation of
    vdTop:
      begin
        UpdateBoundaryDataSet(rsTopLeakyHydraulicConductivity,
          FHydraulicConductivity);
      end;
    vdFront:
      begin
        UpdateBoundaryDataSet(rsFrontLeakyHydraulicConductivity,
          FHydraulicConductivity);
      end;
    vdSide:
      begin
        UpdateBoundaryDataSet(rsSideLeakyHydraulicConductivity,
          FHydraulicConductivity);
      end;
  else
    Assert(False);
  end;
end;

procedure TLeakyBoundary.SetOrientation(const Value: TViewDirection);
var
  Model: TPhastModel;
begin
  inherited;
  if FModel <> nil then
  begin
    Model := FModel as TPhastModel;
    case Value of
      vdTop:
        begin
          FSolution.TimeList :=
            Model.TopLeakyAssociatedSolution;
          FBoundaryValue.TimeList := Model.TopLeakyHead;
        end;
      vdFront:
        begin
          FSolution.TimeList :=
            Model.FrontLeakyAssociatedSolution;
          FBoundaryValue.TimeList := Model.FrontLeakyHead;
        end;
      vdSide:
        begin
          FSolution.TimeList :=
            Model.SideLeakyAssociatedSolution;
          FBoundaryValue.TimeList := Model.SideLeakyHead;
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TLeakyBoundary.SetThickness(const Value: string);
begin
  if FThickness <> trim(Value) then
  begin
    FThickness := trim(Value);
    InvalidateModel;
  end;
  case Orientation of
    vdTop:
      begin
        UpdateBoundaryDataSet(rsTopLeakyThickness, FThickness);
      end;
    vdFront:
      begin
        UpdateBoundaryDataSet(rsFrontLeakyThickness, FThickness);
      end;
    vdSide:
      begin
        UpdateBoundaryDataSet(rsSideLeakyThickness, FThickness);
      end;
  else
    Assert(False);
  end;
end;

{ TRiverBoundary }

procedure TRiverBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TRiverBoundary;
begin
  if Source is TRiverBoundary then
  begin
    SourceBoundary := TRiverBoundary(Source);
    Description := SourceBoundary.Description;
    Width := SourceBoundary.Width;
    Depth := SourceBoundary.Depth;
    BedThickness := SourceBoundary.BedThickness;
    BedHydraulicConductivity := SourceBoundary.BedHydraulicConductivity;
    InvalidateModel;
  end;
  inherited;
end;

constructor TRiverBoundary.Create(ScreenObject: TScreenObject;
  Model: TComponent);
begin
  inherited Create(ScreenObject, Model);

  if Model = nil then
  begin
    FSolution.TimeList := nil;
    FBoundaryValue.TimeList := nil;
  end
  else
  begin
    FSolution.TimeList := (Model as TPhastModel).RiverAssociatedSolution;
    FBoundaryValue.TimeList := (Model as TPhastModel).RiverHead;
  end;
  FBoundaryValue.PropName := 'River_Head';
  FSolution.PropName := 'River_Associated_Solution';
end;

function TRiverBoundary.GetBedHydraulicConductivity: string;
begin
  if (BoundaryValue.Count > 0) or (Solution.Count > 0) then
  begin
    if (ScreenObject <> nil) and (FModel <> nil) then
    begin
      Result := BoundaryDataSetFormula(rsRiverHydraulicConductivity);
    end
    else
    begin
      result := FBedHydraulicConductivity;
    end;
  end
  else
  begin
    result := '';
  end;
end;

function TRiverBoundary.GetBedThickness: string;
begin
  if (BoundaryValue.Count > 0) or (Solution.Count > 0) then
  begin
    if (ScreenObject <> nil) and (FModel <> nil) then
    begin
      Result := BoundaryDataSetFormula(rsRiverBedThickness);
    end
    else
    begin
      result := FBedThickness;
    end;
  end
  else
  begin
    result := '';
  end;
end;

function TRiverBoundary.GetDepth: string;
begin
  if (BoundaryValue.Count > 0) or (Solution.Count > 0) then
  begin
    if (ScreenObject <> nil) and (FModel <> nil) then
    begin
      Result := BoundaryDataSetFormula(rsRiverDepth);
    end
    else
    begin
      result := FDepth;
    end;
  end
  else
  begin
    result := '';
  end;
end;

function TRiverBoundary.GetWidth: string;
begin
  if (BoundaryValue.Count > 0) or (Solution.Count > 0) then
  begin
    if (ScreenObject <> nil) and (FModel <> nil) then
    begin
      Result := BoundaryDataSetFormula(rsRiverWidth);
    end
    else
    begin
      result := FWidth;
    end;
  end
  else
  begin
    result := '';
  end;
end;

function TRiverBoundary.IsBoundary: boolean;
begin
  result := (BoundaryValue.Count > 0) and (Width <> '') and (Depth <> '')
    and (BedThickness <> '') and (BedHydraulicConductivity <> '');
  if result and (FModel as TPhastModel).SoluteTransport then
  begin
    result := (Solution.Count > 0)
  end;
end;

procedure TRiverBoundary.Loaded;
begin
  if (BoundaryValue.Count > 0) or (Solution.Count > 0) then
  begin
    SetBedHydraulicConductivity(FBedHydraulicConductivity);
    SetBedThickness(FBedThickness);
    SetDepth(FDepth);
    SetWidth(FWidth);
  end;
end;

procedure TRiverBoundary.PartialAssign(Source: TPersistent);
var
  SourceBoundary: TRiverBoundary;
begin
  if Source is TRiverBoundary then
  begin
    SourceBoundary := TRiverBoundary(Source);
    BoundaryValue := SourceBoundary.BoundaryValue;
    Solution := SourceBoundary.Solution;
    InvalidateModel;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TRiverBoundary.SetBedHydraulicConductivity(const Value: string);
begin
  if FBedHydraulicConductivity <> Trim(Value) then
  begin
    FBedHydraulicConductivity := Trim(Value);
    InvalidateModel;
  end;
  UpdateBoundaryDataSet(rsRiverHydraulicConductivity,
    FBedHydraulicConductivity);
end;

procedure TRiverBoundary.SetBedThickness(const Value: string);
begin
  if FBedThickness <> Trim(Value) then
  begin
    FBedThickness := Trim(Value);
    InvalidateModel;
  end;
  UpdateBoundaryDataSet(rsRiverBedThickness, FBedThickness);
end;

procedure TRiverBoundary.SetDepth(const Value: string);
begin
  if FDepth <> Trim(Value) then
  begin
    FDepth := Trim(Value);
    InvalidateModel;
  end;
  UpdateBoundaryDataSet(rsRiverDepth, FDepth);
end;

procedure TRiverBoundary.SetDescription(const Value: string);
begin
  if FDescription <> Trim(Value) then
  begin
    FDescription := Trim(Value);
    InvalidateModel;
  end;
end;

procedure TRiverBoundary.SetWidth(const Value: string);
begin
  if FWidth <> Trim(Value) then
  begin
    FWidth := Trim(Value);
    InvalidateModel;
  end;
  UpdateBoundaryDataSet(rsRiverWidth, FWidth);
end;

{ TSpecifiedHeadBoundary }

procedure TSpecifiedHeadBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TSpecifiedHeadBoundary;
begin
  if Source is TSpecifiedHeadBoundary then
  begin
    SourceBoundary := TSpecifiedHeadBoundary(Source);
    SolutionType := SourceBoundary.SolutionType;
    InvalidateModel;
  end;
  inherited;
end;

constructor TSpecifiedHeadBoundary.Create(ScreenObject: TScreenObject;
  Model: TComponent);
begin
  inherited Create(ScreenObject, Model);
  if Model = nil then
  begin
    FSolution.TimeList := nil;
    FBoundaryValue.TimeList := nil;
  end
  else
  begin
    FSolution.TimeList :=
      (Model as TPhastModel).SpecifiedHeadAssociatedSolution;
    FBoundaryValue.TimeList := (Model as TPhastModel).SpecifiedHeadHead;
  end;
  FBoundaryValue.PropName := 'Specified_Head';
  FSolution.PropName := 'Specified_Head_Solution';
end;

procedure TSpecifiedHeadBoundary.Loaded;
begin
  if (AssociatedSolution.Count  > 0) or (Head.Count > 0)then
  begin
    SetSolutionType(FSolutionType);
  end;
end;

procedure TSpecifiedHeadBoundary.SetSolutionType(
  const Value: TSolutionType);
var
  TempFormula: string;
begin
  if (FSolutionType <> Value) then
  begin
    FSolutionType := Value;
    TempFormula := IntToStr(Ord(FSolutionType));
    UpdateBoundaryDataSet(rsSolutionType, TempFormula);
    InvalidateModel;
  end
  else
  begin
    TempFormula := IntToStr(Ord(FSolutionType));
    UpdateBoundaryDataSet(rsSolutionType, TempFormula);
  end;
end;

{ TSpecifiedSolutionBoundary }

procedure TSpecifiedSolutionBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TSpecifiedSolutionBoundary;
begin
  if Source is TSpecifiedSolutionBoundary then
  begin
    SourceBoundary := TSpecifiedSolutionBoundary(Source);
    Solution := SourceBoundary.Solution;
    InvalidateModel;
  end
  else
  begin
    inherited;
  end;
end;

procedure TSpecifiedSolutionBoundary.Clear;
begin
  Solution.Clear;
end;

constructor TSpecifiedSolutionBoundary.Create(ScreenObject: TScreenObject;
  Model: TComponent);
begin
  FSolution := TIntegerPhastBoundaries.Create(Model);
  if Model = nil then
  begin
    FSolution.TimeList := nil;
  end
  else
  begin
    FSolution.TimeList := (Model as TPhastModel).SpecifiedSolution;
  end;
  FSolution.PropName := 'Specified_Solution';
  inherited Create(ScreenObject, Model);
end;

destructor TSpecifiedSolutionBoundary.Destroy;
begin
  FSolution.Free;
  inherited;
end;

procedure TSpecifiedSolutionBoundary.SetScreenObject(
  const Value: TScreenObject);
begin
  inherited;
  FSolution.ScreenObject := Value;
end;

procedure TSpecifiedSolutionBoundary.SetSolution(
  const Value: TIntegerPhastBoundaries);
begin
  FSolution.Assign(Value);
  InvalidateModel;
end;

{ TCustomPhastBoundaryCollection }

procedure TCustomPhastBoundaryCollection.Clear;
begin
  inherited;
  if TimeList <> nil then
  begin
    TimeList.Invalidate;
  end;
end;


function TCustomPhastBoundaryCollection.GetDataSet(
  const ATime: double): TSparseArrayPhastInterpolationDataSet;
var
  TimeIndex: integer;
  AnArray: TSparseArrayPhastInterpolationDataSet;
  Model: TPhastModel;
begin
  Model := FModel as TPhastModel;
  TimeIndex := TimeList.IndexOf(ATime);
  if TimeIndex < 0 then
  begin
    AnArray := nil;
    case GetDatatype of
      rdtDouble:
        begin
          AnArray := TSparseRealPhastDataSet.Create(Model);
        end;
      rdtInteger:
        begin
          AnArray := TSparseIntegerPhastDataSet.Create(Model);
        end;
    else
      Assert(False);
    end;
    Assert(ScreenObject <> nil);
    case ScreenObject.ViewDirection of
      vdTop:
        begin
          case FTimeList.BoundaryType of
            btRiver, btWell:
              begin
                AnArray.BoundaryDataType := Model.Top2DBoundaryType;
              end;
          else
            begin
              AnArray.BoundaryDataType := Model.TopBoundaryType;
            end;
          end;

        end;
      vdFront:
        begin
          AnArray.BoundaryDataType := Model.FrontBoundaryType;
        end;
      vdSide:
        begin
          AnArray.BoundaryDataType := Model.SideBoundaryType;
        end;
    else
      Assert(False);
    end;

    AnArray.Orientation := TimeList.Orientation;
    AnArray.ATimeList := TimeList;
    TimeIndex := TimeList.Add(ATime, AnArray);
  end;
  result := TimeList.Items[TimeIndex] ;
end;

procedure TSpecifiedSolutionBoundary.UpdateFormulaExpression;
begin
  FSolution.UpdateFormulaExpression;
end;

procedure TSpecifiedSolutionBoundary.UpdateMixtureExpression;
begin
  FSolution.UpdateMixtureExpression;
end;

{ TWellBoundary }

procedure TWellBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TWellBoundary;
begin
  if Source is TWellBoundary then
  begin
    SourceBoundary := TWellBoundary(Source);
    Description := SourceBoundary.Description;
    LandSurfaceDatum := SourceBoundary.LandSurfaceDatum;
    Diameter := SourceBoundary.Diameter;
    AllocateByPressureAndMobility :=
      SourceBoundary.AllocateByPressureAndMobility;
    WellElevationFormat := SourceBoundary.WellElevationFormat;
    Intervals := SourceBoundary.Intervals;
    InvalidateModel;
  end;
  inherited;
end;

procedure TWellBoundary.Clear;
begin
  inherited;
  Intervals.Clear;
  InvalidateModel;
end;

constructor TWellBoundary.Create(ScreenObject: TScreenObject;
  Model: TComponent);
begin
  inherited Create(ScreenObject, Model);
  FAllocateByPressureAndMobility := true;
  FIntervals := TWellIntervals.Create(ScreenObject, Model);

  if Model = nil then
  begin
    FSolution.TimeList := nil;
    FBoundaryValue.TimeList := nil;
  end
  else
  begin
    FSolution.TimeList := (Model as TPhastModel).WellSolution;
    FBoundaryValue.TimeList :=
      (Model as TPhastModel).WellInjectionOrPumpingRate;
  end;
end;

destructor TWellBoundary.Destroy;
begin
  FIntervals.Free;
  inherited;
end;

function TWellBoundary.IsBoundary: boolean;
begin
  result := (BoundaryValue.Count > 0) and (Intervals.Count > 0);
  if result and (FModel as TPhastModel).SoluteTransport then
  begin
    result := (Solution.Count > 0)
  end;
end;

procedure TWellBoundary.PartialAssign(Source: TPersistent);
var
  SourceBoundary: TWellBoundary;
begin
  if Source is TWellBoundary then
  begin
    SourceBoundary := TWellBoundary(Source);
    Solution := SourceBoundary.Solution;
    BoundaryValue := SourceBoundary.BoundaryValue;
    InvalidateModel;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TWellBoundary.SetAllocateByPressureAndMobility(
  const Value: boolean);
begin
  FAllocateByPressureAndMobility := Value;
end;

procedure TWellBoundary.SetDescription(const Value: string);
begin
  if FDescription <> Value then
  begin
    FDescription := Value;
    InvalidateModel;
  end;
end;

procedure TWellBoundary.SetDiameter(const Value: double);
begin
  if FDiameter <> Value then
  begin
    FDiameter := Value;
    InvalidateModel;
  end;
end;

procedure TWellBoundary.SetIntervals(const Value: TWellIntervals);
begin
  FIntervals.Assign(Value);
  InvalidateModel;
end;

procedure TWellBoundary.SetLandSurfaceDatum(const Value: double);
begin
  if FLandSurfaceDatum <> Value then
  begin
    FLandSurfaceDatum := Value;
    InvalidateModel;
  end;
end;

procedure TWellBoundary.SetWellElevationFormat(
  const Value: TWellElevationFormat);
begin
  if FWellElevationFormat <> Value then
  begin
    FWellElevationFormat := Value;
    InvalidateModel;
  end;
end;

{ TWellInterval }

procedure TWellInterval.Assign(Source: TPersistent);
var
  SourceInterval: TWellInterval;
begin
  if Source is TWellInterval then
  begin
    SourceInterval := TWellInterval(Source);
    FirstElevation := SourceInterval.FirstElevation;
    SecondElevation := SourceInterval.SecondElevation;
    InvalidateModel;
  end
  else
  begin
    inherited;
  end;
end;

procedure TWellInterval.InvalidateModel;
var
  ScreenObject: TScreenObject;
begin
  ScreenObject:= (Collection as TWellIntervals).ScreenObject;
  if ScreenObject <> nil then
  begin
    ScreenObject.InvalidateModel;
  end;
end;

procedure TWellInterval.SetFirstElevation(const Value: double);
begin
  if FFirstElevation <> Value then
  begin
    FFirstElevation := Value;
    InvalidateModel;
  end;
end;

procedure TWellInterval.SetSecondElevation(const Value: double);
begin
  if FSecondElevation <> Value then
  begin
    FSecondElevation := Value;
    InvalidateModel;
  end;
end;

{ TWellIntervals }

constructor TWellIntervals.Create(ScreenObject: TScreenObject;
  Model: TComponent);
begin
  inherited Create(TWellInterval, Model);
  FScreenObject := ScreenObject;
end;

{ TCustomPhastBoundary }

constructor TCustomPhastBoundary.Create(ScreenObject: TScreenObject;
  Model: TComponent);
begin
  FModel := Model;
  inherited Create;
  SetScreenObject(ScreenObject);
end;

procedure TCustomPhastBoundary.InvalidateModel;
begin
  if ScreenObject <> nil then
  begin
    ScreenObject.InvalidateModel;
  end;
end;

procedure TCustomPhastBoundary.SetModel(const Value: TComponent);
begin
  assert ((Value = nil) or (Value is TPhastModel));
  FModel := Value;
end;

procedure TCustomPhastBoundary.SetScreenObject(const Value: TScreenObject);
begin
  FScreenObject := Value;
end;

procedure TCustomPhastBoundary.UpdateBoundaryDataSet(
  const DataArray: TDataArray; var Formula: string);
var
  NewPosition: integer;
  OldFormula: string;
  OldUses: TStringList;
  NewUses: TStringList;
  Compiler: TRbwParser;
  Index: Integer;
  Position: Integer;
  Observer: TObserver;
  ObserverIndex: Integer;
  ObserverName: string;
  DS: TObserver;
begin
  if ScreenObject <> nil then
  begin
    if ScreenObject.FModel <> nil then
    begin
      Compiler := ScreenObject.GetCompiler(DataArray.Orientation, DataArray.EvaluatedAt);
    end
    else
    begin
      Compiler := nil;
    end;
    if Formula = '' then
    begin
      NewPosition := ScreenObject.IndexOfBoundaryDataSet(DataArray);
      if NewPosition >= 0 then
      begin
        if ScreenObject.FCanInvalidateModel then
        begin
          OldFormula := ScreenObject.BoundaryDataSetFormulas[NewPosition];
          Observer :=ScreenObject.FBoundaryDataSetSubscriptions[NewPosition] as TObserver;
          if (OldFormula <> '') and (Model <> nil) and (Compiler <> nil) then
          begin
            Compiler.Compile(OldFormula);
            OldUses := Compiler.CurrentExpression.VariablesUsed;
            for ObserverIndex := 0 to OldUses.Count - 1 do
            begin
              ObserverName := OldUses[ObserverIndex];
              DS := (Model as TPhastModel).GetObserverByName(ObserverName);
              DS.StopsTalkingTo(Observer);
            end;
          end;
        end;
        ScreenObject.DeleteBoundaryDataSet(NewPosition);
      end;
    end
    else
    begin
      NewPosition :=
        ScreenObject.AddBoundaryDataSet(DataArray);
      OldFormula := ScreenObject.BoundaryDataSetFormulas[NewPosition];
      if ScreenObject.FCanInvalidateModel then
      begin
        Observer := ScreenObject.FBoundaryDataSetSubscriptions[NewPosition] as TObserver;
      end
      else
      begin
        Observer := nil;
      end;
      ScreenObject.BoundaryDataSetFormulas[NewPosition] := Formula;
      Formula := ScreenObject.BoundaryDataSetFormulas[NewPosition];
      if ScreenObject.FCanInvalidateModel then
      begin
        OldUses := TStringList.Create;
        NewUses := TStringList.Create;
        try

          if (OldFormula <> '') and (Model <> nil) and (Compiler <> nil) then
          begin
            Compiler.Compile(OldFormula);
            OldUses.Assign(Compiler.CurrentExpression.VariablesUsed);
          end
          else
          begin
            FreeAndNil(OldUses);
          end;
          if (Formula <> '')  and (Model <> nil) and (Compiler <> nil) then
          begin
            Compiler.Compile(Formula);
            NewUses.Assign(Compiler.CurrentExpression.VariablesUsed);
          end
          else
          begin
            FreeAndNil(NewUses);
          end;
          if (OldUses <> nil) and (NewUses <> nil) then
          begin
            for Index := OldUses.Count - 1 downto 0 do
            begin
              Position := NewUses.IndexOf(OldUses[Index]);
              if Position >= 0 then
              begin
                OldUses.Delete(Index);
                NewUses.Delete(Position);
              end;
            end;
          end;
          if OldUses <> nil then
          begin
            for ObserverIndex := 0 to OldUses.Count - 1 do
            begin
              ObserverName := OldUses[ObserverIndex];
              DS := (Model as TPhastModel).GetObserverByName(ObserverName);
              DS.StopsTalkingTo(Observer);
            end;
          end;
          if NewUses <> nil then
          begin
            for ObserverIndex := 0 to NewUses.Count - 1 do
            begin
              ObserverName := NewUses[ObserverIndex];
              DS := (Model as TPhastModel).GetObserverByName(ObserverName);
              DS.TalksTo(Observer);
            end;
          end;
        finally
          NewUses.Free;
          OldUses.Free;
        end;
      end;
    end;
  end;
end;

procedure TCustomPhastBoundary.UpdateBoundaryDataSet(const DataSetName: string;
  var Formula: string);
var
  BoundaryPosition: integer;
  Model: TPhastModel;
  DataArray : TDataArray;
begin
  if ScreenObject <> nil then
  begin
    if (FModel = nil) or (csLoading in FModel.ComponentState) then
    begin
      Exit;
    end;
    
    Model := (FModel as TPhastModel);
    BoundaryPosition :=
      Model.IndexOfBoundaryDataSet(DataSetName);
    Assert(BoundaryPosition >= 0);
    DataArray := Model.BoundaryDataSets[BoundaryPosition];
    UpdateBoundaryDataSet(DataArray, Formula);
  end;
end;

{ TCustomOrientedPhastBoundary }

procedure TCustomOrientedPhastBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TCustomOrientedPhastBoundary;
begin
  if Source is TCustomOrientedPhastBoundary then
  begin
    SourceBoundary := TCustomOrientedPhastBoundary(Source);
    Orientation := SourceBoundary.Orientation;
  end;
  inherited;
end;

procedure TCustomOrientedPhastBoundary.SetScreenObject(
  const Value: TScreenObject);
begin
  inherited;
  if Value <> nil then
  begin
    Orientation := Value.ViewDirection;
  end
end;

procedure TCustomOrientedPhastBoundary.SetOrientation(
  const Value: TViewDirection);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    InvalidateModel;
  end;
end;

{ TMultiValueScreenObject }

procedure TMultiValueScreenObject.Assign(Source: TPersistent);
var
  SourceObject: TMultiValueScreenObject;
begin
  if Source is TMultiValueScreenObject then
  begin
    SourceObject := TMultiValueScreenObject(Source);
    IntegerValues := SourceObject.IntegerValues;
    RealValues := SourceObject.RealValues;
  end;
  inherited;
end;

constructor TMultiValueScreenObject.Create(AnOwner: TComponent);
begin
  inherited;
  FRealValues := TRealDataListCollection.Create(self, AnOwner);
  FIntegerValues := TIntegerDataListCollection.Create(self, AnOwner);
end;

destructor TMultiValueScreenObject.Destroy;
begin
  FRealValues.Free;
  FIntegerValues.Free;
  inherited;
end;

procedure TMultiValueScreenObject.SetIntegerValues(
  const Value: TIntegerDataListCollection);
begin
  FIntegerValues.Assign(Value);
  InvalidateModel;
end;

procedure TMultiValueScreenObject.SetRealValues(
  const Value: TRealDataListCollection);
begin
  FRealValues.Assign(Value);
  InvalidateModel;
end;

{ TScreenObjectItem }

function TScreenObjectItem.GetMixtureFormulas: TStrings;
var
  Index: integer;
begin
  FMixtureFormulas.Clear;
  FMixtureFormulas.Capacity := ScreenObject.DataSetCount;
  for Index := 0 to ScreenObject.DataSetCount - 1 do
  begin
    FMixtureFormulas.Add(ScreenObject.MixtureDataSetFormula[Index]);
  end;
  Result := FMixtureFormulas;
end;

{ TScreenObjectCollection }

constructor TScreenObjectCollection.Create(Model: TComponent);
begin
  FModel := Model;
  Assert((FModel = nil) or (FModel is TPhastModel));
  inherited Create(TScreenObjectItem);
end;

procedure TCustomPhastBoundaryCollection.SetTimeList(
  const Value: TPhastTimeList);
begin
  FTimeList := Value;
end;

procedure TCustomPhastBoundaryCollection.UpdateFormulaExpression;
var
  Index: integer;
  Item: TCustomPhastBoundaryCondition;
begin
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TCustomPhastBoundaryCondition;
    Item.UpdateFormulaExpression;
  end;
end;

procedure TCustomPhastBoundaryCollection.UpdateMixtureExpression;
var
  Index: integer;
  Item: TCustomPhastBoundaryCondition;
begin
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TCustomPhastBoundaryCondition;
    Item.UpdateMixtureExpression;
  end;
end;

procedure TCustomInterpolatedBoundary.SetScreenObject(
  const Value: TScreenObject);
begin
  inherited;
  FSolution.ScreenObject := Value;
  FBoundaryValue.ScreenObject := Value;
end;

procedure TCustomInterpolatedBoundary.SetSolution(
  const Value: TIntegerPhastBoundaries);
begin
  FSolution.Assign(Value);
  InvalidateModel;
end;

procedure TCustomInterpolatedBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TCustomInterpolatedBoundary;
begin
  if Source is TCustomInterpolatedBoundary then
  begin
    SourceBoundary := TCustomInterpolatedBoundary(Source);
    BoundaryValue := SourceBoundary.BoundaryValue;
    Solution := SourceBoundary.Solution;
    InvalidateModel;
  end
  else
  begin
    inherited;
  end;
end;

procedure TCustomInterpolatedBoundary.Clear;
begin
  if (BoundaryValue.Count > 0) or (Solution.Count > 0) then
  begin
    InvalidateModel;
  end;
  BoundaryValue.Clear;
  Solution.Clear;
end;

constructor TCustomInterpolatedBoundary.Create(ScreenObject: TScreenObject;
  Model: TComponent);
begin
  FSolution := TIntegerPhastBoundaries.Create(Model);
  FBoundaryValue := TRealPhastBoundaries.Create(Model);

  inherited Create(ScreenObject, Model);
end;

destructor TCustomInterpolatedBoundary.Destroy;
begin
  FSolution.Free;
  FBoundaryValue.Free;
  inherited;
end;

procedure TCustomInterpolatedBoundary.SetBoundaryValue(
  const Value: TRealPhastBoundaries);
begin
  FBoundaryValue.Assign(Value);
  InvalidateModel;
end;

procedure TCustomInterpolatedBoundary.UpdateFormulaExpression;
begin
  FBoundaryValue.UpdateFormulaExpression;
  FSolution.UpdateFormulaExpression;
end;

procedure TCustomInterpolatedBoundary.UpdateMixtureExpression;
begin
  FBoundaryValue.UpdateMixtureExpression;
  FSolution.UpdateMixtureExpression;
end;

{ TIntersectEdge }

procedure TIntersectEdge.Assign(Segment: TCellElementSegment);
begin
  StartPoint.x := Segment.X1;
  StartPoint.y := Segment.Y1;
  EndPoint.x := Segment.X2;
  EndPoint.y := Segment.Y2;
end;

procedure TIntersectEdge.Assign(P1, P2: TPoint2D);
begin
  StartPoint := P1;
  EndPoint := P2;
end;

procedure TIntersectEdge.Reverse;
var
  Temp : TPoint2D;
begin
  Temp := StartPoint;
  StartPoint := EndPoint;
  EndPoint := Temp;
end;

{ TScreenObjectList }

function TScreenObjectList.Add(ScreenObject: TScreenObject): integer;
begin
  result := FList.Add(ScreenObject);
end;

constructor TScreenObjectList.Create;
begin
  FList := TList.Create;
end;

destructor TScreenObjectList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TScreenObjectList.GetCapacity: integer;
begin
  result := FList.Capacity;
end;

function TScreenObjectList.GetCount: integer;
begin
  result := FList.Count;
end;

function TScreenObjectList.GetItems(Index: integer): TScreenObject;
begin
  result := FList[Index];
end;

procedure TScreenObjectList.SetCapacity(const Value: integer);
begin
  FList.Capacity := Value;
end;

{ TIntegerCollection }

constructor TIntegerCollection.Create(Model: TComponent);
begin
  inherited Create(TIntegerItem, Model);
end;

function TIntegerCollection.GetValue(Index: integer): integer;
begin
  result := (Items[Index] as TIntegerItem).Value;
end;

procedure TIntegerCollection.SetValue(Index: integer; const Value: integer);
begin
  (Items[Index] as TIntegerItem).Value := Value;
end;

{ TIntegerItem }

procedure TIntegerItem.Assign(Source: TPersistent);
begin
  if Source is TIntegerItem then
  begin
    Value := TIntegerItem(Source).Value;
  end
  else
  begin
    inherited;
  end;
end;

{ TModflowBoundaries }

destructor TModflowBoundaries.Destroy;
begin
  FModflowMnw2Boundary.Free;
  FModflowGage.Free;
  FModflowHfbBoundary.Free;
  FModflowHeadObservations.Free;
  FModflowUzfBoundary.Free;
  FModflowSfrBoundary.Free;
  FModflowLakBoundary.Free;
  FModflowResBoundary.Free;
  FModflowEtsBoundary.Free;
  FModflowEvtBoundary.Free;
  FModflowRchBoundary.Free;
  FModflowDrtBoundary.Free;
  FModflowDrnBoundary.Free;
  FModflowRivBoundary.Free;
  FModflowWellBoundary.Free;
  FModflowGhbBoundary.Free;
  FModflowChdBoundary.Free;
  inherited;
end;

{ TSelectedCells }

procedure TSelectedCells.Clear;
begin
  SetExtents(0,0,0);
end;

function TSelectedCells.GetHasCells: boolean;
begin
  result := (FLayerCount > 0) and (FRowCount > 0) and (FColCount > 0);
end;

function TSelectedCells.GetSelected(Layer, Row, Col: integer): boolean;
begin
  ValidateIndicies(Col, Row, Layer);
  result := FSelected[Layer, Row, Col];
end;

procedure TSelectedCells.SetExtents(LayerCount, RowCount, ColCount: integer);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  if (LayerCount<0) or (RowCount < 0) or (ColCount < 0) then
  begin
    SetLength(FSelected, 0, 0, 0);
    FLayerCount := LayerCount;
    FRowCount := RowCount;
    FColCount := ColCount;
  end
  else
  begin
    if (LayerCount <> FLayerCount)
      or (RowCount <> FRowCount)
      or (ColCount <> FColCount) then
    begin
      SetLength(FSelected, LayerCount, RowCount, ColCount);
      FLayerCount := LayerCount;
      FRowCount := RowCount;
      FColCount := ColCount;
    end;
    for LayerIndex := 0 to FLayerCount - 1 do
    begin
      for RowIndex := 0 to FRowCount - 1 do
      begin
        for ColIndex := 0 to FColCount - 1 do
        begin
          FSelected[LayerIndex, RowIndex, ColIndex] := false;
        end;
      end;
    end;
  end;
end;

procedure TSelectedCells.ValidateIndicies(Col: Integer; Row: Integer; Layer: Integer);
begin
  Assert(Layer >= 0);
  Assert(Row >= 0);
  Assert(Col >= 0);
  Assert(Layer < FLayerCount);
  Assert(Row < FRowCount);
  Assert(Col < FColCount);
end;

procedure TSelectedCells.SetSelected(Layer, Row, Col: integer;
  const Value: boolean);
begin
  ValidateIndicies(Col, Row, Layer);
  FSelected[Layer, Row, Col] := Value;
end;

{ TLineSegment }

function TLineSegment.IsNextSegment(Seg: TLineSegment): boolean;
begin
  result := (Point2.x = Seg.Point1.x) and (Point2.y = Seg.Point1.y)
end;

{ TCellAssignment }

procedure TCellAssignment.Assign(Cell: TCellAssignment);
begin
  FLayer := Cell.FLayer;
  FRow := Cell.FRow;
  FColumn := Cell.FColumn;
  FSegment := Cell.FSegment;
  FSection := Cell.FSection;
  FAnnotation := Cell.FAnnotation;
  FAssignmentMethod := Cell.FAssignmentMethod;
end;

constructor TCellAssignment.Create(ALayer, ARow, ACol: integer;
      ASegment: TCellElementSegment; ASection: integer;
      const AnAnnotation: string; AnAssignmentMethod: TAssignmentMethod);
begin
  FLayer := ALayer;
  FRow := ARow;
  FColumn := ACol;
  FSegment := ASegment;
  FSection := ASection;
  FAnnotation := AnAnnotation;
  FAssignmentMethod := AnAssignmentMethod;
end;

constructor TCellAssignment.CreateFromCell(Cell: TCellAssignment);
begin
  Assign(Cell);
end;

constructor TCellAssignment.CreateFromStream(Stream: TDecompressionStream;
  const EncloseAnnotation, IntersectAnnotation: string);
begin
  Restore(Stream, EncloseAnnotation, IntersectAnnotation);
end;

destructor TCellAssignment.Destroy;
begin
  FOwnedSegment.Free;
  inherited;
end;

function TCellAssignment.GetSection: integer;
begin
  if Segment = nil then
  begin
    result := FSection;
  end
  else
  begin
    result := Segment.SectionIndex;
  end;
end;

procedure TCellAssignment.Restore(Stream: TDecompressionStream;
  const EncloseAnnotation, IntersectAnnotation: string);
var
  ReadSegment: boolean;
begin
  Stream.Read(FAssignmentMethod, SizeOf(FAssignmentMethod));
  case FAssignmentMethod of
    amEnclose: FAnnotation := EncloseAnnotation;
    amIntersect: FAnnotation := IntersectAnnotation;
    else Assert(False);
  end;
  Stream.Read(ReadSegment, SizeOf(ReadSegment));
  if ReadSegment then
  begin
    FOwnedSegment.Free;
    FOwnedSegment := TCellElementSegment.Create;
    FOwnedSegment.Restore(Stream);
    FSegment := FOwnedSegment;
  end
  else
  begin
    FSegment := nil;
  end;
  FLayer := ReadCompInt(Stream);
  FSection := ReadCompInt(Stream);
  FRow := ReadCompInt(Stream);
  FColumn := ReadCompInt(Stream);
end;

procedure TCellAssignment.Store(Stream: TCompressionStream);
var
  StoreSegment: boolean;
begin
  Stream.Write(FAssignmentMethod, SizeOf(FAssignmentMethod));
  StoreSegment := FSegment <> nil;
  Stream.Write(StoreSegment, SizeOf(StoreSegment));
  if StoreSegment then
  begin
    FSegment.Store(Stream);
  end;
  WriteCompInt(Stream, FLayer);
  WriteCompInt(Stream, FSection);
  WriteCompInt(Stream, FRow);
  WriteCompInt(Stream, FColumn);
end;

{ TCellList }

procedure TCellAssignmentList.Add(Item: TCellAssignment);
var
  PriorItem: TCellAssignment;
begin
  if FList.Count > 0 then
  begin
    PriorItem := FList[Count-1];
    if (PriorItem.Layer = Item.Layer)
      and (PriorItem.Row = Item.Row)
      and (PriorItem.Column = Item.Column)
      and (PriorItem.Section = Item.Section)
      and (PriorItem.AssignmentMethod = Item.AssignmentMethod)
      and (PriorItem.Annotation = Item.Annotation)
      then
    begin
      FList.Delete(Count-1);
    end;
  end;
  FList.Add(Item);
end;

procedure TCellAssignmentList.Assign(CachedList: TCellAssignmentList;
  const EncloseAnnotation, IntersectAnnotation: string);
begin
  Assert(Count = 0);
  FCached := CachedList.FCached;
  FCleared := CachedList.FCleared;
  FMemoryStream := CachedList.FMemoryStream;
  try
    Restore(EncloseAnnotation, IntersectAnnotation);
  finally
    FCached := False;
    FCleared := False;
    FMemoryStream := nil;
  end;
end;

procedure TCellAssignmentList.Cache;
var
  Compressor: TCompressionStream;
  Index: Integer;
begin
  if not FCached then
  begin
    if FMemoryStream = nil then
    begin
      FMemoryStream := TMemoryStream.Create;
    end;
    Compressor := TCompressionStream.Create(ZLib.clDefault, FMemoryStream);
    try
      FMemoryStream.Position := 0;
      WriteCompInt(Compressor, Count);
      for Index := 0 to Count - 1 do
      begin
        Items[Index].Store(Compressor);
      end;
    finally
      Compressor.Free;
    end;
    FCached := True;
  end;
  FList.Clear;
  FCleared := True;
end;

procedure TCellAssignmentList.Clear;
begin
  FList.Clear;
  FCached := False;
  FreeAndNil(FMemoryStream);
  FCleared := False;
end;

constructor TCellAssignmentList.Create;
begin
  FList := TObjectList.Create;
end;

procedure TCellAssignmentList.Delete(Index: integer);
begin
  FList.Delete(Index);
end;

destructor TCellAssignmentList.Destroy;
begin
  FMemoryStream.Free;
//  if FileExists(FTempFileName) then
//  begin
//    DeleteFile(FTempFileName);
//  end;
  FList.Free;
  inherited;
end;

function TCellAssignmentList.GetCount: integer;
begin
  result := FList.Count;
end;

function TCellAssignmentList.GetItem(Index: integer): TCellAssignment;
begin
  result := FList[Index];
end;

procedure TCellAssignmentList.Restore(const EncloseAnnotation, IntersectAnnotation: string);
var
  DecompressionStream: TDecompressionStream;
  NewCount: integer;
  Index: Integer;
  NewItem: TCellAssignment;
begin
  Assert(FMemoryStream <> nil);
  Assert(FCached);
  Assert(FCleared);
  FMemoryStream.Position := 0;
  DecompressionStream := TDecompressionStream.Create(FMemoryStream);
  try
    NewCount := ReadCompInt(DecompressionStream);
    FList.Capacity := NewCount;
    for Index := 0 to NewCount - 1 do
    begin
      NewItem := TCellAssignment.CreateFromStream(DecompressionStream,
        EncloseAnnotation, IntersectAnnotation);
      Add(NewItem);
    end;
  finally
    DecompressionStream.Free;
    FCleared := False;
  end;
end;

procedure SelectAScreenObject(ScreenObject: TScreenObject);
var
  AScreenObject: TScreenObject;
  Index: Integer;
  UndoChangeSelection: TUndoChangeSelection;
begin
  UndoChangeSelection := TUndoChangeSelection.Create;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    AScreenObject.Selected := AScreenObject = ScreenObject;
  end;
  UndoChangeSelection.SetPostSelection;
  if UndoChangeSelection.SelectionChanged then
  begin
    frmGoPhast.UndoStack.Submit(UndoChangeSelection);
  end
  else
  begin
    UndoChangeSelection.Free;
  end;
end;

{ TScreenObjectClipboard }

constructor TScreenObjectClipboard.Create(AOwner: TComponent);
begin
  inherited;
  FScreenObjects := TScreenObjectCollection.Create(AOwner);
end;

destructor TScreenObjectClipboard.Destroy;
begin
  FScreenObjects.Free;
  inherited;
end;

procedure TScreenObjectClipboard.SetScreenObjects(
  const Value: TScreenObjectCollection);
begin
  FScreenObjects.Assign(Value);
end;

procedure TScreenObjectClipboard.UpdateModel(Model: TComponent);
begin
  Assert((Model = nil) or (Model is TPhastModel));
  FScreenObjects.FModel := Model;
end;

{ TCachedCells }

destructor TCachedCells.Destroy;
var
  Index: TAssignmentLocation;
begin
  for Index := Low(TAssignmentLocation) to High(TAssignmentLocation) do
  begin
    FCachedLists[Index].Free;
  end;
  inherited;
end;

procedure TCachedCells.Invalidate;
begin
  FreeAndNil(FCachedLists);
end;

function TCachedCells.RestoreFromCache(CellList: TCellAssignmentList;
  EvalAt: TEvaluatedAt; Orientation: TDataSetOrientation;
  AssignmentLocation: TAssignmentLocation; const EncloseAnnotation,
  IntersectAnnotation: string): boolean;
begin
  result := (FCachedLists[AssignmentLocation] <> nil) and (FEvalAt = EvalAt)
    and (FOrientation = Orientation);
//    and (FAssignmentLocation = AssignmentLocation);
  if result then
  begin
    CellList.Assign(FCachedLists[AssignmentLocation],
      EncloseAnnotation, IntersectAnnotation);
  end;
end;

procedure TCachedCells.UpdateCache(CellList: TCellAssignmentList;
  EvalAt: TEvaluatedAt; Orientation: TDataSetOrientation;
  AssignmentLocation: TAssignmentLocation);
var
  Index: Integer;
  Cell: TCellAssignment;
begin
//  FMinLayer := -1;
//  FMaxLayer := -1;
//  FMinRow := -1;
//  FMaxRow := -1;
//  FMinCol := -1;
//  FMaxCol := -1;
  FCachedLists[AssignmentLocation].Free;
  FCachedLists[AssignmentLocation] := TCellAssignmentList.Create;
  FEvalAt := EvalAt;
  FOrientation := Orientation;
  FAssignmentLocation := AssignmentLocation;
  FCachedLists[AssignmentLocation].FList.Capacity := CellList.Count;
  for Index := 0 to CellList.Count - 1 do
  begin
    Cell := CellList[Index];
    FCachedLists[AssignmentLocation].Add(TCellAssignment.CreateFromCell(Cell));
  end;
  FCachedLists[AssignmentLocation].Cache;
end;

function FindIntersectionPoints(Poly1, Poly2: TSubPolygon;
  var Intersections: TIntersectionArray;
  var Count: integer): boolean;
  function SimpleIntersection: boolean;
  var
    Index1: Integer;
    Index2: Integer;
  begin
    result := False;
    for Index1 := Poly1.FStart to Poly1.FCount - 1 do
    begin
      for Index2 := Poly2.FStart to Poly2.FCount - 1 do
      begin
        if Intersect(Poly1.FPoints[Index1],
          Poly1.FPoints[Index1+1],
          Poly2.FPoints[Index2],
          Poly2.FPoints[Index2+1]) then
        begin
          result := True;

          if Length(Intersections) = Count then
          begin
            if Length(Intersections) = 0 then
            begin
              SetLength(Intersections, 4);
            end
            else
            begin
              SetLength(Intersections, Count*2);
            end;
          end;

          Intersections[Count].Point :=
            IntersectionPoint(Poly1.FPoints[Index1],Poly1.FPoints[Index1+1],
            Poly2.FPoints[Index2],Poly2.FPoints[Index2+1]);

          Intersections[Count].Location1.Position := Index1+1;
          Intersections[Count].Location1.New := False;
          if IsEqual(Poly1.FPoints[Index1+1],Intersections[Count].Point) then
          begin
            Poly1.FPoints[Index1+1] := Intersections[Count].Point;
            Intersections[Count].Location1.New := True;
          end
          else if IsEqual(Poly1.FPoints[Index1],Intersections[Count].Point) then
          begin
            Poly1.FPoints[Index1] := Intersections[Count].Point;
            Intersections[Count].Location1.New := True;
            Intersections[Count].Location1.Position := Index1;
          end;

          Intersections[Count].Location2.Position := Index2+1;
          Intersections[Count].Location2.New := False;
          if IsEqual(Poly2.FPoints[Index2+1],Intersections[Count].Point) then
          begin
            Poly2.FPoints[Index2+1] := Intersections[Count].Point;
            Intersections[Count].Location2.New := True;
          end
          else if IsEqual(Poly2.FPoints[Index2],Intersections[Count].Point) then
          begin
            Poly2.FPoints[Index2] := Intersections[Count].Point;
            Intersections[Count].Location2.New := True;
            Intersections[Count].Location2.Position := Index2;
          end;
          
          Inc(Count);
        end;
      end;
    end;
  end;
begin
  result := False;
  if ((Poly1.FMinX <= Poly2.FMaxX) and (Poly1.FMaxX >= Poly2.FMinX)) or
    ((Poly2.FMinX <= Poly1.FMaxX) and (Poly2.FMaxX >= Poly1.FMinX)) then
  begin
    if ((Poly1.FMinY <= Poly2.FMaxY) and (Poly1.FMaxY >= Poly2.FMinY)) or
      ((Poly2.FMinY <= Poly1.FMaxY) and (Poly2.FMaxY >= Poly1.FMinY)) then
    begin
      if Poly1.FCount > Poly2.FCount then
      begin
        if Poly1.FSubPolygon1 <> nil then
        begin
          result := FindIntersectionPoints(Poly1.FSubPolygon1, Poly2,
            Intersections, Count)
            or FindIntersectionPoints(Poly1.FSubPolygon2, Poly2,
            Intersections, Count)
        end
        else
        begin
          Assert(Poly2.FSubPolygon1 = nil);
          result := SimpleIntersection;
        end;
      end
      else
      begin
        if Poly2.FSubPolygon1 <> nil then
        begin
          result := FindIntersectionPoints(Poly1, Poly2.FSubPolygon1,
            Intersections, Count)
            or FindIntersectionPoints(Poly1, Poly2.FSubPolygon2,
            Intersections, Count)
        end
        else
        begin
          Assert(Poly1.FSubPolygon1 = nil);
          result := SimpleIntersection;
        end;
      end;
    end;
  end;
end;

function TCustomInterpolatedBoundary.BoundaryDataSetFormula(DataSetName: string): string;
var
  BoundaryPosition: Integer;
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  Model := (FModel as TPhastModel);
  BoundaryPosition := Model.IndexOfBoundaryDataSet(DataSetName);
  DataArray := Model.BoundaryDataSets[BoundaryPosition];
  BoundaryPosition := ScreenObject.IndexOfBoundaryDataSet(DataArray);
  result := ScreenObject.BoundaryDataSetFormulas[BoundaryPosition];
end;

{ TCellElementLeaf }

constructor TCellElementLeaf.Create(Segment: TCellElementSegment;
  ViewDirection: TViewDirection);
var
  FirstPoint: TPoint2D;
  SecondPoint: TPoint2D;
begin
  FSegment := Segment;
  FirstPoint := Segment.FirstPointRealCoord(ViewDirection);
  SecondPoint := Segment.SecondPointRealCoord(ViewDirection);
  FX1 := Min(FirstPoint.x, SecondPoint.x);
  FX2 := Max(FirstPoint.x, SecondPoint.x);
  FY1 := Min(FirstPoint.y, SecondPoint.y);
  FY2 := Max(FirstPoint.y, SecondPoint.y);
end;

function TCellElementLeaf.GetCoordinate(Depth: integer): double;
begin
  case Depth of
    0:  result := FX1;
    1:  result := FX2;
    2:  result := FY1;
    3:  result := FY2;
    else
    begin
      result := 0;
      Assert(False);
    end;
  end;
end;

{ TCellElementLeafList }

function TCellElementLeafList.CoordinateCount: integer;
begin
  result := 4;
end;

constructor TCellElementLeafList.Create;
begin
  inherited;
  OwnsObjects := True;
end;

initialization
  RegisterClass(TScreenObject);
  RegisterClass(TPhastDelegate);
  RegisterClass(TModflowDelegate);
  RegisterClass(TMultiValueScreenObject);
  RegisterClass(TScreenObjectClipboard);

end.

