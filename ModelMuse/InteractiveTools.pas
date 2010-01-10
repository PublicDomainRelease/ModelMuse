{
May 23, 2006: Added code to prevent frmScreenObjects from being displayed
  when it shouldn't be.
  Removed code for creating frmScreenObjectProperties in FinishScreenObjects
  and replaced with an assertion that it isn't nil because
  frmScreenObjectProperties is created in frmGoPhast.FormCreate.
  Fixed destruction of CurrentScreenObject and also destroyed
  CurrentScreenObject if the number of nodes it has is zero.
May 24, 2006: implemented setting hint of the top front and side view
  @link(TQRbwZoomBox2 zoomboxes) with the current tool.
}

{@abstract(@name defines descendants of @link(TCustomInteractiveTool) that
  manage the interaction between the user and the model.)}

unit InteractiveTools;

interface

uses
  GR32_Layers, // TPositionedLayer is declared in GR32_Layers.
  GR32, // TBitmap32, and TFloatRect are declared in GR32.
  Types, SysUtils, Classes, Controls, Forms, Graphics, FastGEO, GoPhastTypes,
  AbstractGridUnit, frameViewUnit, SelectUnit, ScreenObjectUnit, UndoItems,
  UndoItemsScreenObjects, QuadTreeClass;

const
  // @name is the color (silver) used to draw selected cells or elements.
  SelectedCellsColor = clSilver;

var
  SelectingObjectsWithLine: boolean = False;  

type
  {@abstract(@name defines the behavior when the user wants to zoom in
    on a particular area that has been outlined.)}
  TZoomTool = class(TCustomInteractiveTool)
  protected
    // @name changes the cursor to the appropriate value when
    // the user wants to zoom in on an area.
    function GetCursor: TCursor; override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
  public
    // @name starts the zoom process.
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name completes the zooming in process.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to zoom in by a factor of 2 at the location
    where the user clicks the mouse.)}
  TZoomInTool = class(TCustomInteractiveTool)
  protected
    // @name returns the cursor to the class.
    function GetCursor: TCursor; override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
  public
    // @name causes the magnification to increase by a factor of two.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to zoom out by a factor of 2 at the location
    where the user clicks the mouse.)}
  TZoomOutTool = class(TCustomInteractiveTool)
  protected
    // @name returns the cursor to the class.
    function GetCursor: TCursor; override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
  public
    // @name causes the magnification to decrease by a factor of two.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to move the view of the model.)}
  TPanTool = class(TCustomInteractiveTool)
  protected
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
  public
    // @name starts to move the view of the model.
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name finishes moving the view of the model.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name cancels the panning operation.
    procedure RightClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is an abstract base class for
    @link(TCustomInteractiveTool)s that interact with the grid.)}
  TCustomGridTool = class(TCustomInteractiveTool)
  private
    procedure DrawColumnRowOrLayer(const Direction: TViewDirection;
      const BitMap: TBitmap32);
    function IsInsideSelectionWidth(RealPoint: TPoint2D;
      const X, Y: Integer): boolean;
  protected
    // @name returns @true if a column boundary on the front view of the model
    // is within @link(SelectionWidth) pixels of (X,Y).
    function IsOnFrontColumn(const X, Y: integer): boolean;
    // @name returns @true if a layer boundary on the front view of the model
    // is within @link(SelectionWidth) pixels of (X,Y).
    function IsOnFrontLayer(const X, Y: integer): boolean;
    // @name returns @true if a layer boundary on the side view of the model
    // is within @link(SelectionWidth) pixels of (X,Y).
    function IsOnSideLayer(const X, Y: integer): boolean;
    // @name returns @true if a row boundary on the side view of the model
    // is within @link(SelectionWidth) pixels of (X,Y).
    function IsOnSideRow(const X, Y: integer): boolean;
    // @name returns @true if a column boundary on the top view of the model
    // is within @link(SelectionWidth) pixels of (X,Y).
    function IsOnTopColumn(const X, Y: integer): boolean;
    // @name returns @true if a row boundary on the top view of the model
    // is within @link(SelectionWidth) pixels of (X,Y).
    function IsOnTopRow(const X, Y: integer): boolean;
    // @name shows the position of a column or layer boundary in the front
    // view of the model when a column or
    // layer boundary is being added or moved.
    procedure ShowNewColumnOrLayer(const BitMap: TBitmap32);
    // @name shows the position of a column or row boundary in the top
    // view of the model when a column or
    // row boundary is being added or moved.
    procedure ShowNewColumnOrRow(const BitMap: TBitmap32);
    // @name shows the position of a row or layer boundary in the side
    // view of the model when a row or
    // layer boundary is being added or moved.
    procedure ShowNewRowOrLayer(const BitMap: TBitmap32);
  end;

  {@abstract(@name is an abstract base class for @link(TCustomGridTool)s
    that have a different cursor depending on whether the mouse is or is not
    over a column, row, or layer boundary.)}
  TCustomGridCursorTool = class(TCustomGridTool)
  private
    // See @link(UseSelectedCursor).
    FUseSelectedCursor: boolean;
  protected
    // @name returns the cursor to use when the mouse is
    // not over a column, row, or layer boundary.
    function GetNonSelectedCursor: TCursor; virtual;
    // @name returns the cursor to use when the mouse is
    // over a column, row, or layer boundary.
    function GetSelectedCursor: TCursor; virtual; abstract;
    // @name determines whether (X,Y) (in screen coordinates) is over a column
    // or layer boundary on the front view of the model.
    function IsOverFrontColumnOrLayer(X, Y: integer): boolean;
    // @name determines whether the point Y) (in screen coordinates) is over a
    // row or layer boundary on the side view of the model.
    function IsOverSideRowOrLayer(X, Y: integer): boolean;
    // @name determines whether the point X,Y (in screen coordinates) is over
    // a column or row on the top view of the model.
    function IsOverTopColumnOrRow(X, Y: integer): boolean;
    // @name sets the cursor to @link(GetNonSelectedCursor) or
    // @link(GetSelectedCursor) depending on whether X,Y is
    // over a grid boundary on the front view of the model.
    procedure SetFrontCursor(X, Y: integer);
    // @name sets the cursor to @link(GetNonSelectedCursor) or
    // @link(GetSelectedCursor) depending on whether X,Y is
    // over a grid boundary on the side view of the model.
    procedure SetSideCursor(X, Y: integer);
    // @name sets the cursor to @link(GetNonSelectedCursor) or
    // @link(GetSelectedCursor) depending on whether X,Y is
    // over a grid boundary on the top view of the model.
    procedure SetTopCursor(X, Y: integer);
    // If @name is true then @link(SetTopCursor), @link(SetFrontCursor)
    // and @link(SetSideCursor) will always set the cursor to
    // the value returned by @link(GetSelectedCursor).
    property UseSelectedCursor: boolean read FUseSelectedCursor
      write FUseSelectedCursor;
  public
    // @name sets the cursor depending on whether X,Y is
    // over a grid boundary.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
  end;

{ TODO :
Consider creating descendants that each only handle one view of the model. }

  {@abstract(@name is used to add column, row, or layer boundaries to the grid
    at a position where the user clicks the mouse.)}
  TAddGridBoundaryTool = class(TCustomGridTool)
  protected
    // @name adds a column or layer boundary on the front view of the model.
    procedure AddColumnOrLayer(X, Y: Integer);
    // @name adds a column or row boundary on the top view of the model.
    procedure AddColumnOrRow(X, Y: Integer);
    // @name adds a row or layer boundary on the side view of the model.
    procedure AddRowOrLayer(X, Y: Integer);
    // @name defines the cursor to use with this @classname.
    function GetCursor: TCursor; override;
    procedure DrawOnBitMap32(Sender: TObject;
      Buffer: TBitmap32); override;
  public
    // @name sets the Hint of TfrmGoPhast.@link(TfrmGoPhast.frameTopView),
    // TfrmGoPhast.@link(TfrmGoPhast.frameFrontView), and
    // TfrmGoPhast.@link(TfrmGoPhast.frameSideView). Each is set to a
    // different value.
    procedure Activate; override;
    // @name causes @link(TCustomInteractiveTool.ZoomBox).Image32
    // to be redrawn.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name adds a column, row, or layer boundary.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to move a column, row, or layer boundary.)}
  TMovingGridBoundaryTool = class(TCustomGridCursorTool)
  private
    // See @link(MovingColumn).
    FMovingColumn: boolean;
    // See @link(MovingLayer).
    FMovingLayer: boolean;
    // See @link(MovingRow).
    FMovingRow: boolean;
    // See @link(MovingColumn).
    procedure SetMovingColumn(const Value: boolean);
    // See @link(MovingLayer).
    procedure SetMovingLayer(const Value: boolean);
    // See @link(MovingRow).
    procedure SetMovingRow(const Value: boolean);
  protected
    // @name: integer;
    // @name is the index of the column boundary being moved.
    FColumnBeingMoved: integer;
    // Name is the X-coordinate of the cursor.  It is set in
    // @link(MouseMove) and used in @link(GetSelectedCursor).
    FCurrentX: integer;
    // Name is the Y-coordinate of the cursor.  It is set in
    // @link(MouseMove) and used in @link(GetSelectedCursor).
    FCurrentY: integer;
    // @name: integer;
    // @name is the index of the layer boundary being moved.
    FLayerBeingMoved: integer;
    // @name: integer;
    // @name is the index of the row boundary being moved.
    FRowBeingMoved: integer;
    // @name starts to move a column or layer on the front view of the model.
    procedure BeginFrontMove(X, Y: Integer);
    // @name starts to move a row or layer on the side view of the model.
    procedure BeginSideMove(X, Y: Integer);
    // @name starts to move a column or row on the top view of the model.
    procedure BeginTopMove(X, Y: Integer);
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    // @name returns the correct cursor for the top, front, or side
    // view of the model when the cursor is over a column, row, or layer
    // boundary is is moving a column, row, or layer boundary.
    function GetSelectedCursor: TCursor; override;
    // @name moves a column or layer on the front view of the model.
    procedure MoveColumnOrLayer(X, Y: Integer);
    // @name moves a column or row on the top view of the model.
    procedure MoveColumnOrRow(X, Y: Integer);
    // @name moves a row or layer on the side view of the model.
    procedure MoveRowOrLayer(X, Y: Integer);
    // @name indicates that a column boundary is being moved.
    property MovingColumn: boolean read FMovingColumn write SetMovingColumn;
    // @name indicates that a layer boundary is being moved.
    property MovingLayer: boolean read FMovingLayer write SetMovingLayer;
    // @name indicates that a row boundary is being moved.
    property MovingRow: boolean read FMovingRow write SetMovingRow;
    procedure DrawOnBitMap32(Sender: TObject;
      Buffer: TBitmap32); override;
  public
    procedure Activate; override;
    // @name starts to move a column, row, or layer boundary.
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name sets the cursor.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name moves the column, row, or layer boundary.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name aborts movement.
    procedure RightClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name deletes a column, row, or layer boundary.)}
  TDeleteGridBoundaryTool = class(TCustomGridCursorTool)
  protected
    // @name deletes the column or layer (or both) boundary at X,Y
    // on the front view of the model.
    procedure DeleteColumnOrLayer(X, Y: Integer);
    // @name deletes the column or row (or both) boundary at X,Y
    // on the top view of the model.
    procedure DeleteColumnOrRow(X, Y: Integer);
    // @name deletes the row or layer (or both) boundary at X,Y
    // on the side view of the model.
    procedure DeleteRowOrLayer(X, Y: Integer);
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    // @name specifies the cursor to use when deleting a column, row, or layer
    // boundary.
    function GetSelectedCursor: TCursor; override;
  public
    // @name deletes a column, row, or layer boundary at X,Y.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is an abstract base class. Its descendants are
    used for cases where the user selects a range of cells on which
    to perform some action.)
    @SeeAlso(frmSetSpacingUnit.TSpacingGridTool)
    @SeeAlso(frmSubdivideUnit.TSubdivideGridTool)
    @SeeAlso(TColRowLayerSelectorTool)}
  TCustomCellSelectionTool = class(TCustomInteractiveTool)
  private
    function GetEvalAt: TEvaluatedAt;
  protected
    // @name draws the selected cells on the front view of the model.
    procedure DrawSelectedFrontCells(FirstCol, LastCol, FirstLayer,
      LastLayer: integer; const BitMap: TBitmap32; Direction: TViewDirection;
      Color: TColor = SelectedCellsColor);
    // @name draws the selected cells on the side view of the model.
    procedure DrawSelectedSideCells(FirstRow, LastRow, FirstLayer,
      LastLayer: integer; const BitMap: TBitmap32; Direction: TViewDirection;
      Color: TColor = SelectedCellsColor);
    // @name draws the selected cells on the top view of the model.
    procedure DrawSelectedTopCells(FirstCol, LastCol, FirstRow,
      LastRow: integer; const BitMap: TBitmap32; Direction: TViewDirection;
      Color: TColor = SelectedCellsColor);
  public
    procedure Activate;override;
  end;

  TColRowLayerSelectorTool = class (TCustomCellSelectionTool)
  private
    FNewRow: integer;
    FNewColumn: integer;
    FNewLayer: integer;
    FShouldDraw: boolean;
    procedure DrawNewColRowLayerSelection(const Direction: TViewDirection;
      const BitMap: TBitmap32);
    procedure DrawExistingColRowLayerSelection(const Direction: TViewDirection;
      const BitMap: TBitmap32);
    procedure GetCellUnderMouse(X, Y: integer);
    procedure SetNewSelection;
    procedure DrawASelection(Col, Row, Lay: Integer; Color1, Color2: TColor;
      const BitMap: TBitmap32; const Direction: TViewDirection);
  protected
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
    function GetHint: string; override;
  public
    property NewColumn: integer read FNewColumn;
    property NewRow: integer read FNewRow;
    property NewLayer: integer read FNewLayer;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure Activate; override;
  end;

  {@abstract(@name is used to rotate the grid on the top view of the model.)}
  TRotateGridTool = class(TCustomInteractiveTool)
  private
    // @name: boolean;
    // See @link(Rotating).
    FRotating: boolean;
    // @name: double;
    // See @link(StartAngle).
    FStartAngle: double;
    // @name draws the rotated grid.
    procedure DrawRotatedGrid(Bitmap: TBitmap32);
    // @name returns the point at the center of the grid
    // on the top view of the model.
    function GridCenter: TPoint2D;
  protected
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
  public
    // @name sets the Hint of TfrmGoPhast.@link(TfrmGoPhast.frameTopView),
    // TfrmGoPhast.@link(TfrmGoPhast.frameFrontView), and
    // TfrmGoPhast.@link(TfrmGoPhast.frameSideView). Each is set to a
    // different value.
    procedure Activate; override;
    // @name starts rotating the grid.
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name causes ZoomBox.Image32 to be redrawn.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
  // @name rotates the grid.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // Abort grid rotation
    procedure RightClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name indicates whether the grid is being rotated.
    property Rotating: boolean read FRotating;
    // @name indicates the grid angle when rotating was started.
    property StartAngle: double read FStartAngle;
  end;

  TCustomStoreVerticesTool = class(TCustomInteractiveTool)
  private
    function FindPointInNearbyScreenObject(const APoint: TPoint;
      out NearbyPoint: TPoint2D): boolean;
    procedure ClearPoints;
  protected
    // @name: @link(TScreenObject);
    // See @link(CurrentScreenObject).
    FCurrentScreenObject: TScreenObject;
    FShift: TShiftState;
    FViewDirection: TViewDirection;
    FVisibleVertices: TRbwQuadTree;
    FStoredPoints: TList;
    // Store the screen coordinates of visible objects.
    procedure StorePointsOfOtherObjects(ScreenObject: TScreenObject);
    function GetSnapPoint: TPoint;
  public
    // @name is the @link(TScreenObject) that is being created.
    property CurrentScreenObject: TScreenObject read FCurrentScreenObject
      write FCurrentScreenObject;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Activate; override;
  end;

  {@abstract(@name is an abstract base class
    for @link(TCustomInteractiveTool)s for creating @link(TScreenObject)s.)}
  TCustomCreateScreenObjectTool = class(TCustomStoreVerticesTool)
  private
    function CanAddPoint32: boolean;
  protected
    // @name: @link(TCustomUndo);
    // See @link(CurrentUndo).
    FCurrentUndo: TCustomUndo;
    // @name: boolean;
    // @name is set to true in @link(DoubleClick) and is used in @link(MouseUp)
    // to indicate when the @link(TScreenObject) has been completed.
    FDoubleClicked: boolean;
    // @name: integer;
    // @name is set to the X-coordinate of the cursor in @link(MouseUp).
    // In some descendants it is tested for identity with the current
    // X-coordinate of the cursor before being set and action is taken
    // if the location is different.
    FPriorCursorX: integer;
    // @name: integer;
    // @name is set to the Y-coordinate of the cursor in @link(MouseUp).
    // In some descendants it is tested for identity with the current
    // X-coordinate of the cursor before being set and action is taken
    // if the location is different.
    FPriorCursorY: integer;
    // @name returns true if the mouse is over the correct view to add a
    // point to @link(CurrentScreenObject).
    function CanAddPoint: boolean;
    // @name is used to undo or redo the creation of the @link(TScreenObject).
    property CurrentUndo: TCustomUndo read FCurrentUndo;
    // @name is used to set the default values for the elevation formulas
    // when creating a @link(TScreenObject).
    procedure SetDefaultElevationFormulas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // @name responds to Double-Click events to set @link(FDoubleClicked)
    // to @true.  @link(FDoubleClicked) is used in @link(MouseUp)
    // to indicate when the @link(TScreenObject) has been completed.
    procedure DoubleClick(Sender: TObject); override;
    // @name shows the form that allows the user to specify
    // the properties of the new @link(TScreenObject).
    procedure FinishScreenObjects;
    // @name checks if @link(FDoubleClicked) is @true.  if so, it calls
    // @link(FinishScreenObjects).  It also sets @link(FPriorCursorX)
    // and @link(FPriorCursorY).
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
  end;

  TCustomCreateScreenObjectTool32 = class(TCustomCreateScreenObjectTool)
  public
    procedure Activate; override;
  end;

  TCustomEditScreenObjectTool = class(TCustomStoreVerticesTool)
  private
    // @name returns @true if any @link(TScreenObject)s are selected
    // in the view to which this @classname applies.
    function AreScreenObjectsSelected: boolean;
    // @name is used to select (or deselect) the @link(TScreenObject) at X,Y.
    // @param(X is the X mouse coordinate.)
    // @param(Y is the Y mouse coordinate.)
    // @param(SelectLowerScreenObjects If SelectLowerScreenObjects is true,
    //   @name will look for a @link(TScreenObject) that is below the
    //   selected one and try to select it. )
    // @param(CanChangeSelection If CanChangeSelection is true, the
    //   @link(TScreenObject) at X,Y will be selected. otherwise, there will
    //   be no change in what is selected. )
    // @param(ReturnScreenObjectPresent ReturnScreenObjectPresent is only
    //   used if CanChangeSelection is false.  If ReturnScreenObjectPresent
    //   is true, @name returns true if there is a @link(TScreenObject) at
    //   X,Y that can be selected.  If ReturnScreenObjectPresent, @name
    //   returns true if there already is a selected @link(TScreenObject) at
    //   X,Y.)
    // @returns(@name returns @true if a @link(TScreenObject)
    // can be selected at X,Y. Otherwise it returns @false.)
    function SelectScreenObjects(const X, Y: integer;
      const SelectLowerScreenObjects, ToggleSelectedItem: boolean;
      const CanChangeSelection: boolean = True;
      const ReturnScreenObjectPresent: boolean = False): boolean;
  end;

  TScreenObjectTool = class (TCustomEditScreenObjectTool)
  private
    // @name is used to select @link(TScreenObject)s by enclosing them
    // in a polygon.
    // @name is the @link(TLine) used to select the @link(TScreenObject)s
    //  by enclosing them in a polygon.
    FSelectLine: TLine;
    function SelectScreenObjectsInGui(const ToggleSelection: boolean): boolean;
  protected
    function ScreenObjectInside(AScreenObject: TScreenObject): boolean; virtual;
  public
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
  end;

  {@abstract(@name is used to select @link(TScreenObject)s by enclosing them
    with a polygon.)}
  TLassoTool = class(TScreenObjectTool)
  protected
    function ScreenObjectInside(AScreenObject: TScreenObject): boolean;override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    // @name returns @true if an entire object is selected by
    // being inside @link(FSelectLine). If ToggleSelection is true,
    // @link(TScreenObject)s inside
    // @link(FSelectLine) are toggled from Selected
    // to not Selected and vice-versa.
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
  public
    procedure Activate; override;
    // @name creates a new @link(FSelectLine) (and frees the old one)
    // and adds a point to it.
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name adds points to @link(FSelectLine)
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name selects @link(TScreenObject)s with @link(FSelectLine).
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to create a point @link(TScreenObject).)}
  TCreatePointScreenObjectTool = class(TCustomCreateScreenObjectTool)
  private
    // @name creates the point @link(TScreenObject).
    procedure CreatePointScreenObject(X, Y: Integer; Shift: TShiftState);
  protected
    function GetCursor: TCursor; override;
  public
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name responds to Mouse-Up events by creating a @link(TScreenObject).
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to create a line or polygon @link(TScreenObject).)}
  TCreateLineScreenObjectTool = class(TCustomCreateScreenObjectTool32)
  private
    // @name creates TCustomCreateScreenObjectTool.@link(
    // TCustomStoreVerticesTool.CurrentScreenObject) if it does not
    // exist.  It adds a point at X,Y to @link(
    // TCustomStoreVerticesTool.CurrentScreenObject).
    procedure ContinueLineScreenObject(X, Y: Integer; Shift: TShiftState);
  protected
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    function GetCursor: TCursor; override;
  public
    // @name sets the cursor and shows what the @link(TScreenObject)
    // would look like if the mouse button was clicked.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name checks that the cursor has moved from its previous position.
    // If it has, it calls @link(ContinueLineScreenObject).
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to create a line @link(TScreenObject)
    in which the segments are aligned with the grid.)}
  TCreateStraightLineScreenObjectTool = class(TCustomCreateScreenObjectTool32)
  private
    // @name creates TCustomCreateScreenObjectTool.@link(
    // TCustomStoreVerticesTool.CurrentScreenObject) if it does not
    // exist.  It adds a point based on X,Y to @link(
    // TCustomStoreVerticesTool.CurrentScreenObject).  The point is added
    // so that the last segment is aligned with the grid.
    procedure ContinueStraightLineScreenObject(X, Y: Integer; Shift: TShiftState);
    procedure GetPointFromCursorPosition(var APoint: TPoint2D;
      X, Y: Integer; PreviousPoint: TPoint2D);
  protected
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    function GetCursor: TCursor; override;
  public
    // @name sets the cursor and shows what the @link(TScreenObject)
    // would look like if the mouse button was clicked.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name checks that the cursor has moved from its previous position.
    // If it has, it calls @link(ContinueStraightLineScreenObject).
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name creates a @link(TScreenObject) that is shaped like
    a rectangle aligned with the grid.)}
  TCreateRectangleScreenObjectTool = class(TCustomCreateScreenObjectTool32)
  private
    // If TCustomCreateScreenObjectTool.@link(
    // TCustomStoreVerticesTool.CurrentScreenObject) does not exist,
    // @name creates it and adds a point at X,Y.
    // If it does already exist, @name adds additional points to create
    // a @link(TScreenObject) shaped like a rectangle with the
    // point at X,Y at the opposite corner from the first point and
    // with the sides aligned with the grid.
    procedure ContinueRectangle(X, Y: Integer; Shift: TShiftState);
    procedure GetRemaingPointsOnRectangleCorners
      (FirstCorner, ThirdCorner: TPoint2D;
         var SecondCorner, FourthCorner: TPoint2D);
  protected
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
    function GetCursor: TCursor; override;
  public
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    // @name sets the cursor and shows what the rectangle would look
    // if the mouse button were clicked.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name calls @link(ContinueRectangle).
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is meant to be the abstract ancestor of tools
    that need to edit @link(TScreenObject)s .)}
  TCustomModifyGeometryTool = class(TCustomEditScreenObjectTool)
  private
    // @name returns an index to the edge of the selected
    // @link(TScreenObject) that is at X,Y.  If there isn't
    // an edge at X,Y, @name returns -1.
    function GetEdge(const X, Y: integer): integer;
  protected
    // In descendants, @name is used to edit a @link(TScreenObject).
    procedure DoEdit(const X,Y: integer); virtual; abstract;
    // In descendants, @name is used to set a different cursor at
    // different locations.
    procedure SetCursorAtLocation(const X, Y: Integer); virtual; abstract;
  public
    // @name responds to OnMouseDown events by selecting the
    // @link(TScreenObject) under the cursor.
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name responds to OnMouseMove events by changing the
    // cursor depending on whether or not the mouse
    //  is over a @link(TScreenObject) or not.
    // See @link(SetCursorAtLocation).
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name responds to OnMouseUp events editing
    // the @link(TScreenObject) under the cursor.
    // See @link(DoEdit).
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to delete a segment in a @link(TScreenObject).)}
  TDeleteSegmentTool = class(TCustomModifyGeometryTool)
  protected
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    {@name changes the cursor depending on whether or not the mouse
      is over a @link(TScreenObject) or not.)}
    procedure SetCursorAtLocation(const X, Y: Integer); override;
    // @name deletes a segment in a @link(TScreenObject).
    procedure DoEdit(const X, Y: integer); override;
  end;

  {@abstract(@name is used to insert a vertex into a @link(TScreenObject).)}
  TInsertPointTool = class(TCustomModifyGeometryTool)
  protected
    // @name insert a vertex into a @link(TScreenObject) at the position
    // closest to X,Y.
    procedure DoEdit(const X, Y: integer); override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    // @name change the cursor depending on whether or not the cursor is
    // over a @link(TScreenObject).
    procedure SetCursorAtLocation(const X, Y: Integer); override;
  end;

  TCustomAddPartTool = class(TCustomEditScreenObjectTool)
  protected
//    FScreenObject: TScreenObject;
  protected
    FUndoAddPart: TUndoAddPart;
    procedure SubmitUndo;
    procedure EnsureUndo;
  public
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Activate; override;
    procedure Deactivate; override;
    Destructor Destroy; override;
  end;

  TAddLinePartTool = class(TCustomAddPartTool)
  protected
    FNewPart: boolean;
    FDoubleClicked: boolean;
    function GetCursor: TCursor; override;
  public
    procedure FinishSection; virtual;
    procedure Activate; override;
    procedure DoubleClick(Sender: TObject); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  TAddPolygonPartTool = class(TAddLinePartTool)
  protected
    function GetCursor: TCursor; override;
  public
    procedure FinishSection; override;
  end;

  TAddPointPartTool = class(TCustomAddPartTool)
  protected
    function GetCursor: TCursor; override;
  public
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  { @abstract(@name is the abstract ancestor of tools used
    to select and move @link(TScreenObject)s mainly by clicking on them.)
    @SeeAlso(TLassoTool)}
  TCustomSelectScreenObjectTool = class abstract (TScreenObjectTool)
  private
    // @name is set to true while the mouse button is down.
    FMouseButtonIsDown: boolean;
    // @name is set to true when moving @link(TScreenObject)s.
    FMovingScreenObjects: boolean;
    // @name is the starting X-coordinate of the mouse.  It is used
    // when moving the selected @link(TScreenObject)s.
    FStartX: integer;
    // @name is the starting Y-coordinate of the mouse.  It is used
    // when moving the selected @link(TScreenObject)s.
    FStartY: integer;
    // @name moves a @link(TScreenObject) based on X and Y.
    procedure MoveScreenObjects(const X, Y: integer; Shift: TShiftState);
    // @name draws a rectangle outline the area that will be used to
    // select @link(TScreenObject)s when draging with the mouse.
    procedure DrawSelectionRectangle32(BitMap: TBitmap32);
  protected
    procedure GetOffset(const APoint: TPoint2D; out XOffset, YOffset: real); virtual;
  public
    // @name responds to OnMouseDown events by initializing @link(FStartX),
    // @link(FStartY), and @link(FMouseButtonIsDown).
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name responds to OnMouseMove events. by updating
    // @link(FMovingScreenObjects).
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer); override;
    // @name responds to OnMouseUp events. by setting @link(FMouseButtonIsDown)
    // to @false.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to select and move one or more individual vertices
    in a @link(TScreenObject).)
    Points can be selected in either of two ways.
    1. The user clicks on a node of the selected @link(TScreenObject).
       If no @link(TScreenObject) is selected, the one under the curson will
       be selected.
    2. The user drags with the mouse button down to select a group
       of nodes inside the selection rectangle.}
  TSelectPointTool = class abstract (TCustomSelectScreenObjectTool)
  private
    // @name: boolean;
    // @name is set to true if one or more vertices on the selected
    // @link(TScreenObject) is selected.
    FPointIsSelected: boolean;
    // @name shows how the selected @link(TScreenObject) would look if the
    // mouse was released at its current position.
    procedure ShowMovedPoints(const BitMap: TBitmap32);
    // @name returns the @link(TScreenObject) that is already selected
    // and which has a node at or near X,Y.
    function FindSelectedScreenObject(const X, Y: integer): TScreenObject;
    // This procedure selects the node of a selected screen object
    // that is at or near (X,Y).
    function SelectPointsOfASelectedScreenObject(const X, Y: integer;
      const AddToSelection: boolean): boolean;
    // @name selects the nodes in
    // @name uses a "lasso" to select nodes in one selected object.
    // All other selected objects are de-selected.
    function SelectPointsOfAllSelectedScreenObjectsWithLine(
      const AddToSelection: boolean): boolean;
    // @name selects nodes that are inside TScreenObjectTool.@link(
    // TScreenObjectTool.FSelectLine).
    // if AddToSelection is not True, it deselects nodes that are not
    // inside  @link(TScreenObjectTool.FSelectLine).
    // Changed is set to True if any nodes have been
    // changed from unselected to selected or vice versa.
    function SelectPointsWithLine(const AScreenObject: TScreenObject;
      const AddToSelection: boolean; out Changed: boolean): boolean;
    // @name returns the index of the first vertex in AScreenObject
    // that is within @link(SelectionWidth) of X,Y.
    function FindNodeInSelectedScreenObjects(const X, Y: integer;
      const AScreenObject: TScreenObject): integer;
  protected
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
    procedure GetOffset(const APoint: TPoint2D; out XOffset, YOffset: real); override;
    function GetCursor: TCursor; override;
  public
    procedure Activate; override;
    // @name responds to OnMouseDown events by seeing if a node
    // in a @link(TScreenObject) could be
    // selected at this position.  If so, it is selected.
    // It also creates an outline that is used in dragging to select points.
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name responds to OnMouseMove events by showing how the
    // @link(TScreenObject) would be changed if the mouse were released
    // at X,Y. See @link(ShowMovedPoints).
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name responds to OnMouseUp events by either moving the selected
    // points or selecting the points inside a rectangle created by dragging
    // with the mouse button down.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
  end;

  {@abstract(@name is used to select and move one
    or more  @link(TScreenObject)s.)
    Points can be selected in either of two ways.
    1. The user clicks on a @link(TScreenObject).
    2. The user drags with the mouse button down to select a group
       of @link(TScreenObject) that are completely inside the selection
       rectangle.}
  TSelectScreenObjectTool = class(TCustomSelectScreenObjectTool)
  private
    // @name is set to @true when the user double-clicks.
    FDoubleClicked: boolean;
    // @name indicates that the selection rectangle should be drawn.
    FShouldDrawSelectionRectangle: boolean;
    FMaxX: double;
    FMinX: double;
    FMaxY: double;
    FMinY: double;
  protected
    function ScreenObjectInside(AScreenObject: TScreenObject): boolean;
      override;
    procedure DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32); override;
    // Used to define @link(TCustomInteractiveTool.Hint)
    function GetHint: string; override;
  public
    // @name sets @link(FShouldDrawSelectionRectangle) to @True.
    procedure Activate; override;
    // @name responds to OnDoubleClick events by setting @link(FDoubleClicked)
    // to true.
    procedure DoubleClick(Sender: TObject); override;
    // @name responds to OnMouseDown events by initializing
    // @link(TCustomSelectScreenObjectTool.FMovingScreenObjects) and
    // @link(TCustomSelectScreenObjectTool).
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name responds to OnMouseMove events by
    // redrawing the selection rectangle.
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer); override;
    // @name responds to OnMouseUp events by either selecting or moving
    // @link(TScreenObject)s.
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    // @name indicates that the selection rectangle should be drawn.
    property ShouldDrawSelectionRectangle: boolean
      read FShouldDrawSelectionRectangle write FShouldDrawSelectionRectangle;
  end;

{$REGION 'Global_Variables'}
  var
    // @name is the instance of @link(TZoomTool) used in GoPhast.
    ZoomTool: TZoomTool;
    // @name is the instance of @link(TZoomInTool) used in GoPhast.
    ZoomInTool: TZoomInTool;
    // @name is the instance of @link(TZoomOutTool) used in GoPhast.
    ZoomOutTool: TZoomOutTool;
    // @name is the instance of @link(TPanTool) used in GoPhast.
    PanTool: TPanTool;
    // @name is the instance of @link(TAddGridBoundaryTool) used in GoPhast.
    AddGridBoundaryTool: TAddGridBoundaryTool;
    // @name is the instance of @link(TMovingGridBoundaryTool) used in GoPhast.
    MovingGridBoundaryTool: TMovingGridBoundaryTool;
    // @name is the instance of @link(TDeleteGridBoundaryTool) used in GoPhast.
    DeleteGridBoundaryTool: TDeleteGridBoundaryTool;
    // @name is the instance of @link(TRotateGridTool) used in GoPhast.
    RotateGridTool: TRotateGridTool;
    // @name is the instance of @link(TLassoTool) used in GoPhast.
    LassoTool: TLassoTool;
    // @name is the instance of @link(TCreatePointScreenObjectTool)
    // used in GoPhast.
    CreatePointScreenObjectTool: TCreatePointScreenObjectTool;
    // @name is the instance of @link(TCreateLineScreenObjectTool)
    // used in GoPhast.
    CreateLineScreenObjectTool: TCreateLineScreenObjectTool;
    // @name is the instance of @link(TCreateStraightLineScreenObjectTool)
    // used in GoPhast.
    CreateStraightLineScreenObjectTool: TCreateStraightLineScreenObjectTool;
    // @name is the instance of @link(TCreateRectangleScreenObjectTool)
    // used in GoPhast.
    CreateRectangleScreenObjectTool: TCreateRectangleScreenObjectTool;
    // @name is the instance of @link(TDeleteSegmentTool) used in GoPhast.
    DeleteSegmentTool: TDeleteSegmentTool;
    // @name is the instance of @link(TInsertPointTool) used in GoPhast.
    InsertPointTool: TInsertPointTool;
    // @name is the instance of @link(TSelectPointTool) used in GoPhast.
    SelectPointTool: TSelectPointTool;
    // @name is the instance of @link(TSelectScreenObjectTool) used in GoPhast.
    SelectScreenObjectTool: TSelectScreenObjectTool;
    ColRowLayerSelectorTool: TColRowLayerSelectorTool;
    AddLinePartTool: TAddLinePartTool;
    AddPolygonPartTool: TAddPolygonPartTool;
    AddPointPartTool: TAddPointPartTool;

{$ENDREGION}

const
  // When selecting objects or nodes, the cursor may not be exactly over the
  // object to be selected.  @name defines how far off the cursor
  // can be and still select the object.
  // @name also controls how far you have to drag an object before
  // it will actually move.
  SelectionWidth = 5;

implementation

uses Math, CursorsFoiledAgain, GR32_Polygons, frmGoPhastUnit, frmSubdivideUnit,
  frmSetSpacingUnit, frmScreenObjectPropertiesUnit, BigCanvasMethods,
  LayerStructureUnit, DataSetUnit, ZoomBox2, Contnrs;

function ConvertSidePoint(APoint: TPoint2D): TPoint2D;
begin
  result.x := APoint.y;
  result.y := APoint.x;
end;

type
  TScreenPointStorage = class(TObject)
    FScreenObject: TScreenObject;
    FVertexIndex: integer;
  end;

{$REGION 'TZoomTool'}
{ TZoomTool }

function TZoomTool.GetCursor: TCursor;
begin
  if ZoomBox.CanZoomIn then
  begin
    result := crZoom;
  end
  else
  begin
    result := crArrow;
  end;
end;
  
function TZoomTool.GetHint: string;
begin
  result := 'Click and drag to zoom in';
end;
  
procedure TZoomTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  //  if ZoomBox.CanZoomIn then
  begin
    ZoomBox.BeginZoom(X, Y);
  end;
end;
  
procedure TZoomTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateCursors;
  with View do
  begin
    AdjustScales;
    ShowMagnification;
    MagnificationChanged := True;
    frmGoPhast.SynchronizeViews(ViewDirection);
  end;
  UpdateCursors;
  inherited;
end;
{ TZoomInTool }
  
function TZoomInTool.GetCursor: TCursor;
begin
  if ZoomBox.CanZoomIn then
  begin
    result := crZoomIn;
  end
  else
  begin
    result := crArrow;
  end;
end;
  
function TZoomInTool.GetHint: string;
begin
  result := 'Click to zoom in';
end;
  
procedure TZoomInTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbRight then
  begin
    ZoomBox.ZoomByAt(2, X, Y);
    // If further zooming in is impossible, change the cursor.
    If not ZoomBox.CanZoomIn then
    begin
      UpdateCursors;
    end;
    with View do
    begin
      AdjustScales;
      ShowMagnification;
      MagnificationChanged := True;
      frmGoPhast.SynchronizeViews(ViewDirection);
    end;
    UpdateCursors;
  end;
  inherited;
end;
{$ENDREGION}

{$REGION 'TZoomOutTool'}
{ TZoomOutTool }

function TZoomOutTool.GetCursor: TCursor;
begin
  if ZoomBox.CanZoomOut then
  begin
    result := crZoomOut;
  end
  else
  begin
    result := crArrow;
  end;
end;
  
function TZoomOutTool.GetHint: string;
begin
  result := 'Click to zoom out';
end;
  
procedure TZoomOutTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbRight then
  begin
    ZoomBox.ZoomByAt(0.5, X, Y);
    with View do
    begin
      AdjustScales;
      ShowMagnification;
      MagnificationChanged := True;
      frmGoPhast.SynchronizeViews(ViewDirection);
    end;
    UpdateCursors;
  end;
  inherited;
end;
{$ENDREGION}

{$REGION 'TPanTool'}
{ TPanTool }

function TPanTool.GetHint: string;
begin
  result := 'Click and drag to move view';
end;
  
procedure TPanTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  // The user wants to pan to a different position.
  // Start panning and change the cursor to indicate that panning has
  // begun.
  if Button <> mbLeft then Exit;
  ZoomBox.Panning := True;
  Cursor := crHandGrab;
  Screen.Cursor := crHandGrab;
end;
  
procedure TPanTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // The user has finished panning to a different position.
  // Stop panning and change the cursor to indicate that panning has
  // stopped.
  ZoomBox.Panning := False;
  Cursor := crHandFlat;
  Screen.Cursor := crDefault;
  with View do
  begin
    MagnificationChanged := True;
    AdjustScales;
    frmGoPhast.SynchronizeViews(ViewDirection);
  end;
  inherited;
end;
  
procedure TPanTool.RightClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  MouseUp(Sender, Button, Shift, X, Y);
end;
{$ENDREGION}

{$REGION 'TCustomGridTool'}
{ TCustomGridTool }
  
function TCustomGridTool.IsOnTopColumn(const X, Y: integer): boolean;
var
  RealPoint: TPoint2D;
  GridPoint: TPoint2D;
  Column: integer;
  Temp1, Temp2: real;
  MaxRow, MinRow: real;
begin
  // This function returns true if X,Y is over a column boundary
  // as seen from above.
  
  // Find the real world coordinates of the current location
  result := false;
  RealPoint.X := ZoomBox.X(X);
  RealPoint.Y := ZoomBox.Y(Y);
  // Put that location in the coordintate system of the grid
  GridPoint := frmGoPhast.
    Grid.RotateFromRealWorldCoordinatesToGridCoordinates(RealPoint);
  // Find the nearest column to that position..
  Column := frmGoPhast.Grid.NearestColumnPosition(GridPoint.X);
  if (Column >= 0) and (Column <= frmGoPhast.Grid.ColumnCount) then
  begin
    // offset the point to the position of the nearest column
    GridPoint.X := frmGoPhast.Grid.ColumnPosition[Column];
    // rotate back to screen coordinates
    RealPoint := frmGoPhast.Grid.
      RotateFromGridCoordinatesToRealWorldCoordinates(GridPoint);
    if IsInsideSelectionWidth(RealPoint, X, Y) then
    begin
      if (frmGoPhast.Grid.ColumnCount <= 0)
        or (frmGoPhast.Grid.RowCount <= 0)
        or (frmGoPhast.Grid.LayerCount <= 0) then
      begin
        result := true;
      end
      else
      begin
        Temp1 := frmGoPhast.Grid.RowPosition[0];
        Temp2 := frmGoPhast.Grid.RowPosition[frmGoPhast.Grid.RowCount];
        MaxRow := Max(Temp1, Temp2);
        MinRow := Min(Temp1, Temp2);

        result := (GridPoint.Y >= MinRow) and (GridPoint.Y <= MaxRow);
      end;
    end;
  end;
end;
  
function TCustomGridTool.IsOnTopRow(const X, Y: integer): boolean;
var
  RealPoint: TPoint2D;
  GridPoint: TPoint2D;
  Row: integer;
begin
  // This function returns true if X,Y is over a row boundary
  // as seen from above.
  
  // Find the real world coordinates of the current location
  result := false;
  RealPoint.X := ZoomBox.X(X);
  RealPoint.Y := ZoomBox.Y(Y);
  // Put that location in the coordintate system of the grid
  GridPoint := frmGoPhast.Grid.
    RotateFromRealWorldCoordinatesToGridCoordinates(RealPoint);
  // find the nearest row to that position.
  Row := frmGoPhast.Grid.NearestRowPosition(GridPoint.Y);
  if (Row >= 0) and (Row <= frmGoPhast.Grid.RowCount) then
  begin
    // offset the point to the position of the nearest row
    GridPoint.Y := frmGoPhast.Grid.RowPosition[Row];
    // rotate back to screen coordinates
    RealPoint := frmGoPhast.Grid.
      RotateFromGridCoordinatesToRealWorldCoordinates(GridPoint);
    if IsInsideSelectionWidth(RealPoint, X, Y) then
    begin
      if (frmGoPhast.Grid.ColumnCount <= 0)
        or (frmGoPhast.Grid.RowCount <= 0)
        or (frmGoPhast.Grid.LayerCount <= 0) then
      begin
        result := true;
      end
      else
      begin
        result := (GridPoint.X >= frmGoPhast.Grid.ColumnPosition[0])
          and (GridPoint.X <= frmGoPhast.Grid.ColumnPosition[
          frmGoPhast.Grid.ColumnCount]);
      end;
    end;
  end;
end;
  
function TCustomGridTool.IsOnFrontColumn(const X, Y: integer): boolean;
var
  APoint: TPoint2D;
  Column: integer;
  Top, Bottom: real;
begin
  result := false;
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Column := frmGoPhast.Grid.NearestColumnPosition(APoint.X);
  if (Column >= 0) and (Column <= frmGoPhast.Grid.ColumnCount) then
  begin
    APoint.X := frmGoPhast.Grid.ColumnPosition[Column];
    if IsInsideSelectionWidth(APoint, X, Y) then
    begin
      if (frmGoPhast.Grid.ColumnCount <= 0)
        or (frmGoPhast.Grid.RowCount <= 0)
        or (frmGoPhast.Grid.LayerCount <= 0) then
      begin
        result := true;
      end
      else
      begin
        if Column = frmGoPhast.Grid.ColumnCount then
        begin
          Column := frmGoPhast.Grid.ColumnCount-1
        end;
        Top := frmGoPhast.Grid.CellElevation[
          Column,frmGoPhast.Grid.SelectedRow ,0];
        Bottom := frmGoPhast.Grid.CellElevation[
          Column,frmGoPhast.Grid.SelectedRow,frmGoPhast.Grid.LayerCount];
        if Column < frmGoPhast.Grid.ColumnCount-1 then
        begin
          Top :=
            Max(Top,frmGoPhast.Grid.CellElevation[
            Column+1,frmGoPhast.Grid.SelectedRow,0]);
          Bottom :=
            Min(Bottom,frmGoPhast.Grid.CellElevation[Column+1,
            frmGoPhast.Grid.SelectedRow,frmGoPhast.Grid.LayerCount]);
        end;

        result := (APoint.Y >= Bottom) and (APoint.Y <= Top);
      end;
    end;
  end;
end;
  
function TCustomGridTool.IsOnFrontLayer(const X, Y: integer): boolean;
var
  APoint: TPoint2D;
  Layer: integer;
begin
  result := false;
  if frmGoPhast.PhastModel.ModelSelection <> msPhast then
  begin
    Exit;
  end;

  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.Y);
  if (Layer >= 0) and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
  begin
    APoint.Y := frmGoPhast.PhastGrid.LayerElevation[Layer];
    if IsInsideSelectionWidth(APoint, X, Y) then
    begin
      if (frmGoPhast.PhastGrid.ColumnCount <= 0)
        or (frmGoPhast.PhastGrid.RowCount <= 0)
        or (frmGoPhast.PhastGrid.LayerCount <= 0) then
      begin
        result := true;
      end
      else
      begin
        result := (APoint.X >= frmGoPhast.PhastGrid.ColumnPosition[0])
          and (APoint.X <= frmGoPhast.PhastGrid.ColumnPosition[
          frmGoPhast.PhastGrid.ColumnCount]);
      end;
    end;
  end;
end;
  
function TCustomGridTool.IsOnSideLayer(const X, Y: integer): boolean;
var
  APoint: TPoint2D;
  Layer: integer;
begin
  result := false;
  if frmGoPhast.PhastModel.ModelSelection <> msPhast then
  begin
    Exit;
  end;
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.X);
  if (Layer >= 0) and (Layer <= frmGoPhast.PhastGrid.LayerCount) then
  begin
    APoint.X := frmGoPhast.PhastGrid.LayerElevation[Layer];
    if IsInsideSelectionWidth(APoint, X, Y) then
    begin
      if (frmGoPhast.PhastGrid.ColumnCount <= 0)
        or (frmGoPhast.PhastGrid.RowCount <= 0)
        or (frmGoPhast.PhastGrid.LayerCount <= 0) then
      begin
        result := true;
      end
      else
      begin
        result := (APoint.Y >= frmGoPhast.PhastGrid.RowPosition[0])
          and (APoint.Y <= frmGoPhast.PhastGrid.RowPosition[
          frmGoPhast.PhastGrid.RowCount]);
      end;
    end;
  end;
end;
  
function TCustomGridTool.IsOnSideRow(const X, Y: integer): boolean;
var
  APoint: TPoint2D;
  Row: integer;
  Top, Bottom: real;
begin
  result := false;
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Row := frmGoPhast.Grid.NearestRowPosition(APoint.Y);
  if (Row >= 0) and (Row <= frmGoPhast.Grid.RowCount) then
  begin
    APoint.Y := frmGoPhast.Grid.RowPosition[Row];
    if IsInsideSelectionWidth(APoint, X, Y) then
    begin
      if (frmGoPhast.Grid.ColumnCount <= 0)
        or (frmGoPhast.Grid.RowCount <= 0)
        or (frmGoPhast.Grid.LayerCount <= 0) then
      begin
        result := true;
      end
      else
      begin
        if Row = frmGoPhast.Grid.RowCount then
        begin
          Row := frmGoPhast.Grid.RowCount-1;
        end;
        Top := frmGoPhast.Grid.CellElevation[frmGoPhast.Grid.SelectedColumn,
          Row, 0];
        Bottom := frmGoPhast.Grid.CellElevation[frmGoPhast.Grid.SelectedColumn,
          Row,frmGoPhast.Grid.LayerCount];
        if Row < frmGoPhast.Grid.RowCount-1 then
        begin
          Top :=
            Max(Top,frmGoPhast.Grid.CellElevation[frmGoPhast.Grid.SelectedColumn,
          Row+1, 0]);
          Bottom :=
            Min(Bottom,frmGoPhast.Grid.CellElevation[frmGoPhast.Grid.SelectedColumn,
            Row+1,frmGoPhast.Grid.LayerCount]);
        end;

        result := (APoint.X >= Bottom) and (APoint.X <= Top);
      end;
    end;
  end;
end;

procedure TCustomGridTool.ShowNewColumnOrLayer(const BitMap: TBitmap32);
var
  P1, P2: T3DRealPoint;
  CursorPoint: TPoint2D;
  Column: integer;
begin
  CursorPoint.X := ZoomBox.X(frmGoPhast.CursorX);
  CursorPoint.Y := ZoomBox.Y(frmGoPhast.CursorY);

  if (frmGoPhast.Grid.ColumnCount > 0)
    and (frmGoPhast.Grid.RowCount > 0)
    and (frmGoPhast.Grid.LayerCount > 0) then
  begin
    if (frmGoPhast.tbMove.Down and MovingGridBoundaryTool.MovingColumn) or
      frmGoPhast.tbAddVerticalBoundary.Down then
    begin
      if frmGoPhast.Grid.SelectedRow >= 0 then
      begin
        P1.X := CursorPoint.X;
        Column := frmGoPhast.Grid.NearestColumnCenter(P1.X);
        P1.Y := frmGoPhast.Grid.CellElevation[Column,frmGoPhast.Grid.SelectedRow,0];

        P2.X := CursorPoint.X;
        P2.Y := frmGoPhast.Grid.CellElevation[Column,frmGoPhast.
          Grid.SelectedRow,frmGoPhast.Grid.LayerCount];

        DrawBigPolyline32(BitMap, clBlack32, 1,
          [Point(ZoomBox.XCoord(P1.X),
            ZoomBox.YCoord(P1.Y)),
          Point(ZoomBox.XCoord(P2.X),
            ZoomBox.YCoord(P2.Y))], True);
      end;
    end
    else if (frmGoPhast.tbMove.Down and MovingGridBoundaryTool.MovingLayer) or
      frmGoPhast.tbAddHorizontalBoundary.Down then
    begin
      if frmGoPhast.PhastModel.ModelSelection <> msPhast then
      begin
        Exit;
      end;
      P1.X := frmGoPhast.Grid.ColumnPosition[0];
      P1.Y := CursorPoint.Y;
  
      P2.X :=
        frmGoPhast.Grid.ColumnPosition[frmGoPhast.Grid.ColumnCount];
      P2.Y := CursorPoint.Y;
  
      DrawBigPolyline32(BitMap, clBlack32, 1,
        [Point(ZoomBox.XCoord(P1.X),
          ZoomBox.YCoord(P1.Y)),
        Point(ZoomBox.XCoord(P2.X),
          ZoomBox.YCoord(P2.Y))], True);
    end;
  end;
end;
  
procedure TCustomGridTool.ShowNewColumnOrRow(const BitMap: TBitmap32);
var
  P1, P2: TPoint2D;
  CursorPoint: TPoint2D;
begin
  // If you are editing the grid, show that.
  CursorPoint.X := ZoomBox.X(frmGoPhast.CursorX);
  CursorPoint.Y := ZoomBox.Y(frmGoPhast.CursorY);
  CursorPoint := frmGoPhast.Grid.
    RotateFromRealWorldCoordinatesToGridCoordinates(CursorPoint);
  
  if (frmGoPhast.Grid.ColumnCount > 0)
    and (frmGoPhast.Grid.RowCount > 0)
    and (frmGoPhast.Grid.LayerCount > 0) then
  begin
    if (frmGoPhast.tbMove.Down and MovingGridBoundaryTool.MovingColumn)
      or frmGoPhast.tbAddVerticalBoundary.Down then
    begin
      // moving or adding a column
      P1.X := CursorPoint.X;
      P1.Y := frmGoPhast.Grid.RowPosition[0];
      P1 := frmGoPhast. Grid.
        RotateFromGridCoordinatesToRealWorldCoordinates(P1);
  
      P2.X := CursorPoint.X;
      P2.Y := frmGoPhast.Grid.RowPosition[
        frmGoPhast.Grid.RowCount];
      P2 := frmGoPhast.Grid.
        RotateFromGridCoordinatesToRealWorldCoordinates(P2);
  
      DrawBigPolyline32(BitMap, clBlack32, 1,
        [View.ConvertPoint(P1),View.ConvertPoint(P2)], True);
    end
    else if (frmGoPhast.tbMove.Down and MovingGridBoundaryTool.MovingRow)
      or frmGoPhast.tbAddHorizontalBoundary.Down then
    begin
      // moving or adding a row
      P1.X := frmGoPhast.Grid.ColumnPosition[0];
      P1.Y := CursorPoint.Y;
      P1 := frmGoPhast.Grid.
        RotateFromGridCoordinatesToRealWorldCoordinates(P1);
  
      P2.X := frmGoPhast.Grid.ColumnPosition[
        frmGoPhast.Grid.ColumnCount];
      P2.Y := CursorPoint.Y;
      P2 := frmGoPhast.Grid.
        RotateFromGridCoordinatesToRealWorldCoordinates(P2);

      DrawBigPolyline32(BitMap, clBlack32, 1,
        [View.ConvertPoint(P1),View.ConvertPoint(P2)], True);
    end;
  end;
end;
  
procedure TCustomGridTool.ShowNewRowOrLayer(const BitMap: TBitmap32);
var
  P1, P2: T3DRealPoint;
  CursorPoint: TPoint2D;
  Row: integer;
begin
  CursorPoint.X := ZoomBox.X(frmGoPhast.CursorX);
  CursorPoint.Y := ZoomBox.Y(frmGoPhast.CursorY);

  if (frmGoPhast.Grid.ColumnCount > 0)
    and (frmGoPhast.Grid.RowCount > 0)
    and (frmGoPhast.Grid.LayerCount > 0) then
  begin
    if (frmGoPhast.tbMove.Down and MovingGridBoundaryTool.MovingLayer)
      or frmGoPhast.tbAddVerticalBoundary.Down then
    begin
      if frmGoPhast.PhastModel.ModelSelection <> msPhast then
      begin
        Exit;
      end;
      P1.Y := frmGoPhast.PhastGrid.RowPosition[0];
      P1.X := CursorPoint.X;

      P2.Y := frmGoPhast.PhastGrid.RowPosition
        [frmGoPhast.PhastGrid.RowCount];
      P2.X := CursorPoint.X;

      DrawBigPolyline32(BitMap, clBlack32, 1,
        [Point(ZoomBox.XCoord(P1.X),
          ZoomBox.YCoord(P1.Y)),
        Point(ZoomBox.XCoord(P2.X),
          ZoomBox.YCoord(P2.Y))], True);
    end
    else if (frmGoPhast.tbMove.Down and MovingGridBoundaryTool.MovingRow)
      or frmGoPhast.tbAddHorizontalBoundary.Down then
    begin
      if frmGoPhast.Grid.SelectedColumn >= 0 then
      begin
        P1.Y := CursorPoint.Y;
        Row := frmGoPhast.Grid.NearestRowCenter(P1.X);
        P1.X := frmGoPhast.Grid.CellElevation[frmGoPhast.Grid.SelectedColumn,
          Row,0];

        P2.Y := CursorPoint.Y;
        P2.X := frmGoPhast.Grid.CellElevation[frmGoPhast.Grid.SelectedColumn,
          Row,frmGoPhast.Grid.LayerCount];

        DrawBigPolyline32(BitMap, clBlack32, 1,
          [Point(ZoomBox.XCoord(P1.X),
            ZoomBox.YCoord(P1.Y)),
          Point(ZoomBox.XCoord(P2.X),
            ZoomBox.YCoord(P2.Y))], True);

      end;
    end;
  end;
end;

function TCustomGridTool.IsInsideSelectionWidth(RealPoint: TPoint2D;
  const X, Y: Integer): boolean;
begin
  result := (Abs(ZoomBox.XCoord(RealPoint.X) - X) <= SelectionWidth)
    and (Abs(ZoomBox.YCoord(RealPoint.Y) - Y) <= SelectionWidth);
end;
  
procedure TCustomGridTool.DrawColumnRowOrLayer(const Direction: TViewDirection;
  const BitMap: TBitmap32);
begin
  case Direction of
    vdTop:
      ShowNewColumnOrRow(BitMap);
    vdFront:
      ShowNewColumnOrLayer(BitMap);
    vdSide:
      ShowNewRowOrLayer(BitMap);
  else
    Assert(False);
  end;
end;
{$ENDREGION}

{$REGION 'TAddGridBoundaryTool'}
{ TAddGridBoundaryTool }

procedure TAddGridBoundaryTool.Activate;
begin
  inherited;
  if frmGoPhast.tbAddVerticalBoundary.Down then
  begin
    frmGoPhast.frameTopView.ZoomBox.Hint := 'Click to add a column.'
  end
  else if frmGoPhast.tbAddHorizontalBoundary.Down then
  begin
    frmGoPhast.frameTopView.ZoomBox.Hint := 'Click to add a row.'
  end
  else
  begin
    Assert(False);
  end;
  
  if frmGoPhast.tbAddVerticalBoundary.Down then
  begin
    frmGoPhast.frameFrontView.ZoomBox.Hint := 'Click to add a column.'
  end
  else if frmGoPhast.tbAddHorizontalBoundary.Down then
  begin
    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      frmGoPhast.frameFrontView.ZoomBox.Hint := 'Click to add a layer.'
    end
    else
    begin
      frmGoPhast.frameFrontView.ZoomBox.Hint := 'Layers can not be added this way.';
    end;
  end
  else
  begin
    Assert(False);
  end;

  if frmGoPhast.tbAddVerticalBoundary.Down then
  begin
    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      frmGoPhast.frameSideView.ZoomBox.Hint := 'Click to add a layer.'
    end
    else
    begin
      frmGoPhast.frameSideView.ZoomBox.Hint := 'Layers can not be added this way.';
    end;
  end
  else if frmGoPhast.tbAddHorizontalBoundary.Down then
  begin
    frmGoPhast.frameSideView.ZoomBox.Hint := 'Click to add a row.'
  end
  else
  begin
    Assert(False);
  end;
  CreateLayers;
end;

procedure TAddGridBoundaryTool.AddColumnOrLayer(X, Y: Integer);
begin
  if frmGoPhast.tbAddVerticalBoundary.Down then
  begin
    frmGoPhast.UndoStack.Submit(TUndoAddColumn.Create(ZoomBox.X(X)));
  end
  else if frmGoPhast.tbAddHorizontalBoundary.Down then
  begin
    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      frmGoPhast.UndoStack.Submit(TUndoAddLayer.Create(ZoomBox.Y(Y)));
    end;
  end;
end;
  
procedure TAddGridBoundaryTool.AddColumnOrRow(X, Y: Integer);
var
  APoint: TPoint2D;
begin
  // transform the mouse coordinates to grid coordinates.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  APoint := frmGoPhast.Grid.
    RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
  if frmGoPhast.tbAddVerticalBoundary.Down then
  begin
    // Add a column.
    frmGoPhast.UndoStack.Submit(TUndoAddColumn.Create(APoint.X));
  end
  else if frmGoPhast.tbAddHorizontalBoundary.Down then
  begin
    // Add a row.
    frmGoPhast.UndoStack.Submit(TUndoAddRow.Create(APoint.Y));
  end;
end;
  
procedure TAddGridBoundaryTool.AddRowOrLayer(X, Y: Integer);
begin
  if frmGoPhast.tbAddVerticalBoundary.Down then
  begin
    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      frmGoPhast.UndoStack.Submit(TUndoAddLayer.Create(ZoomBox.X(X)));
    end;
  end
  else if frmGoPhast.tbAddHorizontalBoundary.Down then
  begin
    frmGoPhast.UndoStack.Submit(TUndoAddRow.Create(ZoomBox.Y(Y)));
  end;
end;
  
procedure TAddGridBoundaryTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
begin
  inherited;
  if frmGophast.CurrentTool <> self then Exit;
  if Sender <> Layer32 then Exit;
  Layer32.BringToFront;
  Buffer.BeginUpdate;
  try
    DrawColumnRowOrLayer(View.ViewDirection, Buffer);
  finally
    Buffer.EndUpdate;
  end;
end;
  
function TAddGridBoundaryTool.GetCursor: TCursor;
begin
  result := crArrow;
  if frmGoPhast.tbAddVerticalBoundary.Down then
  begin
    case ViewDirection of
      vdTop:
        begin
          result := frmGoPhast.dcAddColCursor.Cursor;
        end;
      vdFront, vdSide:
        begin
          result := crVertical;
        end;
    else
      Assert(False);
    end;
  end
  else if frmGoPhast.tbAddHorizontalBoundary.Down then
  begin
    case ViewDirection of
      vdTop:
        begin
          result := frmGoPhast.dcAddRowCursor.Cursor;
        end;
      vdFront, vdSide:
        begin
          result := crHorizontal;
        end;
    else
      Assert(False);
    end;
  end;
end;
  
procedure TAddGridBoundaryTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  ZoomBox.Image32.Invalidate;
end;
  
procedure TAddGridBoundaryTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Add a column or row at the mouse position.
  case ViewDirection of
    vdTop: AddColumnOrRow(X, Y);
    vdFront: AddColumnOrLayer(X, Y);
    vdSide: AddRowOrLayer(X, Y);
  else
    Assert(False);
  end;
  inherited;
end;
  
{$ENDREGION}

{$REGION 'TMovingGridBoundaryTool'}
{ TMovingGridBoundaryTool }

procedure TMovingGridBoundaryTool.Activate;
begin
  inherited;
  CreateLayers;
end;
  
procedure TMovingGridBoundaryTool.BeginFrontMove(X, Y: Integer);
var
  APoint: TPoint2D;
  Column, Layer: integer;
begin
  // start to move a column or layer.

  if IsOverFrontColumnOrLayer(X,Y) then
  begin
    // The point is over the grid.

    // First find the nearest column or layer boundaries.
    APoint.X := ZoomBox.X(X);
    APoint.Y := ZoomBox.Y(Y);

    if IsOnFrontColumn(X, Y) then
    begin
      Column := frmGoPhast.Grid.NearestColumnPosition(APoint.X);
      // If the point is over a column, set the cursor.
      // and set MovingColumn to true.
      // Store the column that is being moved.
      Cursor := crMoveColumn;
      MovingLayer := False;
      MovingColumn := True;
      FColumnBeingMoved := Column;
    end
    else if IsOnFrontLayer(X, Y) then
    begin
      Assert(frmGoPhast.PhastModel.ModelSelection = msPhast);
      Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.Y);
      // If the point is over a layer, set the cursor.
      // and set MovingLayer to true.
      // Store the row that is being moved.
      Cursor := crMoveRow;
      MovingColumn := False;
      MovingLayer := True;
      FLayerBeingMoved := Layer;
    end;
  end;
end;
  
procedure TMovingGridBoundaryTool.BeginSideMove(X, Y: Integer);
var
  APoint: TPoint2D;
  Row, Layer: integer;
begin
  if IsOverSideRowOrLayer(X,Y) then
  begin
    APoint.X := ZoomBox.X(X);
    APoint.Y := ZoomBox.Y(Y);

    if IsOnSideLayer(X, Y) then
    begin
      Assert(frmGoPhast.PhastModel.ModelSelection = msPhast);
      Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.X);
      Cursor := crMoveColumn;
      MovingRow := False;
      MovingLayer := True;
      FLayerBeingMoved := Layer;
    end
    else if IsOnSideRow(X, Y) then
    begin
      Row := frmGoPhast.Grid.NearestRowPosition(APoint.Y);
      Cursor := crMoveRow;
      MovingLayer := False;
      MovingRow := True;
      FRowBeingMoved := Row;
    end;
  end;
end;
  
procedure TMovingGridBoundaryTool.BeginTopMove(X, Y: Integer);
var
  APoint: TPoint2D;
  Column, Row: integer;
begin
  // This procedure is used to start moving a column or row as seen from
  // the top.
  
  if IsOverTopColumnOrRow(X,Y) then
  begin
    // First find the nearest column or row boundaries.
    APoint.X := ZoomBox.X(X);
    APoint.Y := ZoomBox.Y(Y);
    APoint := frmGoPhast.Grid.
      RotateFromRealWorldCoordinatesToGridCoordinates(APoint);

    // The point is over the grid.
    if IsOnTopColumn(X, Y) then
    begin
      Column := frmGoPhast.Grid.NearestColumnPosition(APoint.X);
      // If the point is over a column, set the cursor.
      // and set MovingColumn to true.
      // Store the column that is being moved.
      Cursor := frmGoPhast.dcMoveColCursor.Cursor;

      MovingRow := False;
      MovingColumn := True;
      FColumnBeingMoved := Column;
    end
    else if IsOnTopRow(X, Y) then
    begin
      Row := frmGoPhast.Grid.NearestRowPosition(APoint.Y);
      // If the point is over a row, set the cursor.
      // and set MovingRow to true.
      // Store the row that is being moved.
      Cursor := frmGoPhast.dcMoveRowCursor.Cursor;
      MovingColumn := False;
      MovingRow := True;
      FRowBeingMoved := Row;
    end;
  end;
end;
  
procedure TMovingGridBoundaryTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
begin
  inherited;
  if frmGophast.CurrentTool <> self then Exit;
  if MouseIsDown then
  begin
    Layer32.BringToFront;
    Buffer.BeginUpdate;
    try
      DrawColumnRowOrLayer(View.ViewDirection, Buffer);
    finally
      Buffer.EndUpdate;
    end;
  end;
end;
  
function TMovingGridBoundaryTool.GetHint: string;
begin
  case frmGoPhast.PhastModel.ModelSelection of
    msUndefined: Assert(False);
    msPhast:
      begin
        result := 'Click on grid line and drag to move it.';
      end;
    msModflow:
      begin
        case ViewDirection of
          vdTop: result := 'Click on grid line and drag to move it.';
          vdFront: result := 'Click on column line and drag to move it.';
          vdSide: result := 'Click on row line and drag to move it.';
          else Assert(False);
        end;
      end
    else Assert(False);
  end;

end;
  
function TMovingGridBoundaryTool.GetSelectedCursor: TCursor;
begin
  result := crArrow;
  case ViewDirection of
    vdTop:
      begin
        if MovingColumn then
        begin
          result := frmGoPhast.dcMoveColCursor.Cursor;
        end
        else if MovingRow then
        begin
          result := frmGoPhast.dcMoveRowCursor.Cursor;
        end
        else if IsOnTopColumn(FCurrentX, FCurrentY) then
        begin
          // Set the cursor for moving columns
          result := frmGoPhast.dcMoveColCursor.Cursor;
        end
        else if IsOnTopRow(FCurrentX, FCurrentY) then
        begin
          // Set the cursor for moving rows
          result := frmGoPhast.dcMoveRowCursor.Cursor;
        end
        else
        begin
          result := crArrow;
        end;
      end;
    vdFront:
      begin
        if MovingColumn then
        begin
          result := crMoveColumn;
        end
        else if MovingLayer then
        begin
          result := crMoveRow;
        end
        else if IsOnFrontColumn(FCurrentX, FCurrentY) then
        begin
          // Set the cursor for moving columns
          result := crMoveColumn;
        end
        else if IsOnFrontLayer(FCurrentX, FCurrentY) then
        begin
          // Set the cursor for moving layers
          result := crMoveRow;
        end
        else
        begin
          result := crArrow;
        end;
      end;
    vdSide:
      begin
        if MovingLayer then
        begin
          result := crMoveColumn;
        end
        else if MovingRow then
        begin
          result := crMoveRow;
        end
        else if IsOnSideLayer(FCurrentX, FCurrentY) then
        begin
          result := crMoveColumn;
        end
        else if IsOnSideRow(FCurrentX, FCurrentY) then
        begin
          result := crMoveRow;
        end
        else
        begin
          result := crArrow;
        end;
      end;
  else
    Assert(False);
  end;
end;
  
procedure TMovingGridBoundaryTool.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;
  case ViewDirection of
    vdTop: BeginTopMove(X, Y);
    vdFront: BeginFrontMove(X, Y);
    vdSide: BeginSideMove(X, Y);
  else
    Assert(False);
  end;
end;
  
procedure TMovingGridBoundaryTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  FCurrentX := X;
  FCurrentY := Y;
  inherited;
  if UseSelectedCursor then
  begin
    ZoomBox.Image32.Invalidate;
  end;
  
end;
  
procedure TMovingGridBoundaryTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case ViewDirection of
    vdTop: MoveColumnOrRow(X, Y);
    vdFront: MoveColumnOrLayer(X, Y);
    vdSide: MoveRowOrLayer(X, Y);
  else
    Assert(False);
  end;
  inherited;
end;
  
procedure TMovingGridBoundaryTool.MoveColumnOrLayer(X, Y: Integer);
begin
  if not (MovingColumn or MovingLayer) then
  begin
    Exit;
  end;
  if MovingColumn then
  begin
    frmGoPhast.UndoStack.Submit(TUndoMoveColumn.Create
      (FColumnBeingMoved, ZoomBox.X(X)));
  end
  else if MovingLayer then
  begin
    frmGoPhast.UndoStack.Submit(TUndoMoveLayer.Create
      (FLayerBeingMoved, ZoomBox.Y(Y)));
  end;
  MovingColumn := False;
  MovingLayer := False;
end;
  
procedure TMovingGridBoundaryTool.MoveColumnOrRow(X, Y: Integer);
var
  APoint: TPoint2D;
begin
  // If the user isn't moving a column or row, quit.
  if not (MovingColumn or MovingRow) then
  begin
    Exit;
  end;
  // transform the mouse coordinates to grid coordinates.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  APoint := frmGoPhast.Grid.
    RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
  
  if MovingColumn then
  begin
    // move a column
    frmGoPhast.UndoStack.Submit(TUndoMoveColumn.Create(
      FColumnBeingMoved, APoint.X));
  end
  else if MovingRow then
  begin
    // move a row.
    frmGoPhast.UndoStack.Submit(TUndoMoveRow.Create(
      FRowBeingMoved, APoint.Y));
  end;
  // Stop moving columns and rows.
  MovingColumn := False;
  MovingRow := False;
end;
  
procedure TMovingGridBoundaryTool.MoveRowOrLayer(X, Y: Integer);
var
  APoint: TPoint2D;
begin
  if not (MovingRow or MovingLayer) then
  begin
    Exit;
  end;
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  if MovingRow then
  begin
    frmGoPhast.UndoStack.Submit(TUndoMoveRow.Create(FRowBeingMoved,
      APoint.Y));
  end
  else if MovingLayer then
  begin
    frmGoPhast.UndoStack.Submit(TUndoMoveLayer.Create(FLayerBeingMoved,
      APoint.X));
  end;
  MovingRow := False;
  MovingLayer := False;
end;
  
procedure TMovingGridBoundaryTool.RightClick(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  MovingColumn := False;
  MovingRow := False;
  MovingLayer := False;
end;
  
procedure TMovingGridBoundaryTool.SetMovingColumn(const Value: boolean);
begin
  FMovingColumn := Value;
  if Value then
  begin
    UseSelectedCursor := True;
  end
  else
  begin
    UseSelectedCursor := False;
  end;
end;
  
procedure TMovingGridBoundaryTool.SetMovingLayer(const Value: boolean);
begin
  FMovingLayer := Value;
  if Value then
  begin
    UseSelectedCursor := True;
  end
  else
  begin
    UseSelectedCursor := False;
  end;
end;
  
procedure TMovingGridBoundaryTool.SetMovingRow(const Value: boolean);
begin
  FMovingRow := Value;
  if Value then
  begin
    UseSelectedCursor := True;
  end
  else
  begin
    UseSelectedCursor := False;
  end;
end;
{$ENDREGION}

{$REGION 'TDeleteGridBoundaryTool'}
{ TDeleteGridBoundaryTool }

procedure TDeleteGridBoundaryTool.DeleteColumnOrLayer(X, Y: Integer);
var
  Column, Layer: integer;
  UndoDeleteColumn: TUndoDeleteColumn;
  UndoDeleteLayer: TUndoDeleteLayer;
begin
  if IsOverFrontColumnOrLayer(X,Y) then
  begin
    if IsOnFrontColumn(X, Y) then
    begin
      Column := frmGoPhast.Grid.NearestColumnPosition(ZoomBox.X(X));
      UndoDeleteColumn := TUndoDeleteColumn.Create(Column);
      frmGoPhast.UndoStack.Submit(UndoDeleteColumn);
    end;
    if IsOnFrontLayer(X, Y) then
    begin
      Assert(frmGoPhast.PhastModel.ModelSelection = msPhast);
      Layer := frmGoPhast.PhastGrid.NearestLayerPosition(ZoomBox.Y(Y));
      UndoDeleteLayer := TUndoDeleteLayer.Create(Layer);
      frmGoPhast.UndoStack.Submit(UndoDeleteLayer);
    end;
  end;
end;
  
procedure TDeleteGridBoundaryTool.DeleteColumnOrRow(X, Y: Integer);
var
  APoint: TPoint2D;
  Column, Row: integer;
begin
  if IsOverTopColumnOrRow(X,Y) then
  begin
    // The cursor is over the grid.
  
    // Convert to grid coordinates.
    APoint.X := ZoomBox.X(X);
    APoint.Y := ZoomBox.Y(Y);
    APoint := frmGoPhast.Grid.
      RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
    if IsOnTopColumn(X, Y) then
    begin
      // the cursor is over a column so delete it.
      Column := frmGoPhast.Grid.NearestColumnPosition(APoint.X);
      frmGoPhast.UndoStack.Submit(TUndoDeleteColumn.Create(Column));
    end;
    if IsOnTopRow(X, Y) then
    begin
      // the cursor is over a row so delete it.
      Row := frmGoPhast.Grid.NearestRowPosition(APoint.Y);
      frmGoPhast.UndoStack.Submit(TUndoDeleteRow.Create(Row));
    end;
  end;
end;
  
procedure TDeleteGridBoundaryTool.DeleteRowOrLayer(X, Y: Integer);
var
  Row, Layer: integer;
  UndoDeleteRow: TUndoDeleteRow;
  UndoDeleteLayer: TUndoDeleteLayer;
begin
  if IsOverSideRowOrLayer(X,Y) then
  begin
    if IsOnSideLayer(X, Y) then
    begin
      Assert(frmGoPhast.PhastModel.ModelSelection = msPhast);
      Layer := frmGoPhast.PhastGrid.NearestLayerPosition(ZoomBox.X(X));
      UndoDeleteLayer := TUndoDeleteLayer.Create(Layer);
      frmGoPhast.UndoStack.Submit(UndoDeleteLayer);
    end;
    if IsOnSideRow(X, Y) then
    begin
      Row := frmGoPhast.Grid.NearestRowPosition(ZoomBox.Y(Y));
      UndoDeleteRow := TUndoDeleteRow.Create(Row);
      frmGoPhast.UndoStack.Submit(UndoDeleteRow);
    end;
  end;
end;
  
function TDeleteGridBoundaryTool.GetHint: string;
begin
  if frmGoPhast.PhastModel.ModelSelection = msPhast then
  begin
    result := 'Click on grid boundary to delete it';
  end
  else if frmGoPhast.PhastModel.ModelSelection = msModflow then
  begin
    case ViewDirection of
      vdTop: result := 'Click on grid boundary to delete it';
      vdFront: result := 'Click on column boundary to delete it';
      vdSide: result := 'Click on row boundary to delete it';
    else
      Assert(False);
    end;
  end
  else
  begin
    Assert(False);
  end;

end;
  
function TDeleteGridBoundaryTool.GetSelectedCursor: TCursor;
begin
  result := crDelete;
end;
  
procedure TDeleteGridBoundaryTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case ViewDirection of
    vdTop: DeleteColumnOrRow(X, Y);
    vdFront: DeleteColumnOrLayer(X, Y);
    vdSide: DeleteRowOrLayer(X, Y);
  else
    Assert(False);
  end;
  inherited;
end;
{$ENDREGION}

{$REGION 'TCustomGridCursorTool'}
{TCustomGridCursorTool}

function TCustomGridCursorTool.GetNonSelectedCursor: TCursor;
begin
  result := crArrow;
end;
  
procedure TCustomGridCursorTool.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  case ViewDirection of
    vdTop: SetTopCursor(X, Y);
    vdFront: SetFrontCursor(X, Y);
    vdSide: SetSideCursor(X, Y);
  else
    Assert(False);
  end;
end;
  
function TCustomGridCursorTool.IsOverFrontColumnOrLayer(X, Y: integer): boolean;
var
  APoint: TPoint2D;
  Column, Layer: integer;
begin
  // Determine whether (X,Y) (in screen coordinates) is over a column
  // or layer boundary on the front view of the model.

  // get the location in real-world coordinates of the current cursor location
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);

  // Get the column and layer boundaries closest to the current cursor.
  Column := frmGoPhast.Grid.NearestColumnPosition(APoint.X);
  if ((Column >= 0) and (Column <= frmGoPhast.Grid.ColumnCount)) then
  begin
    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.Y);
      if (Layer <= 0) and (Layer > frmGoPhast.PhastGrid.LayerCount) then
      begin
        Result := False;
        Exit;
      end;
    end;


    // The location is over the grid
    if IsOnFrontColumn(X, Y) or IsOnFrontLayer(X, Y) then
    begin
      // The location is over a column or layer boundary
      result := True;
    end
    else
    begin
      result := False;
    end;
  end
  else
  begin
    // The location is not over the grid
    result := False;
  end;
end;
  
procedure TCustomGridCursorTool.SetFrontCursor(X, Y: integer);
begin
  if UseSelectedCursor then
  begin
    Cursor := GetSelectedCursor;
    Exit;
  end;
  
  if IsOverFrontColumnOrLayer(X, Y) then
  begin
    Cursor := GetSelectedCursor;
  end
  else
  begin
    Cursor := GetNonSelectedCursor;
  end;
end;
  
function TCustomGridCursorTool.IsOverSideRowOrLayer(X, Y: integer): boolean;
var
  APoint: TPoint2D;
  Row, Layer: integer;
begin
  // Determine whether the point Y) (in screen coordinates) is over a
  // row or layer boundary on the side view of the model.

  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  Row := frmGoPhast.Grid.NearestRowPosition(APoint.Y);
  if ((Row >= 0) and (Row <= frmGoPhast.Grid.RowCount)) then
  begin
    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      Layer := frmGoPhast.PhastGrid.NearestLayerPosition(APoint.Y);
      if (Layer <= 0) and (Layer > frmGoPhast.PhastGrid.LayerCount) then
      begin
        Result := False;
        Exit;
      end;
    end;
    // The location is over the grid.
    if IsOnSideRow(X, Y) or IsOnSideLayer(X, Y) then
    begin
      // The location is over a row or layer boundary
      result := True;
    end
    else
    begin
      result := False;
    end;
  end
  else
  begin
    // The location is not over the grid.
    result := False;
  end;
end;
  
procedure TCustomGridCursorTool.SetSideCursor(X, Y: integer);
begin
  if UseSelectedCursor then
  begin
    Cursor := GetSelectedCursor;
    Exit;
  end;
  
  if IsOverSideRowOrLayer(X, Y) then
  begin
    Cursor := GetSelectedCursor;
  end
  else
  begin
    Cursor := GetNonSelectedCursor;
  end;
end;
  
function TCustomGridCursorTool.IsOverTopColumnOrRow(X, Y: integer): boolean;
var
  APoint: TPoint2D;
  Column, Row: integer;
begin
  // Determine whether the point X,Y (in screen coordinates) is over
  // a column or row on the top view of the model.
  
  // Find the real world coordinates of the point
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  // Put that location in the coordintate system of the grid
  APoint :=
    frmGoPhast.Grid.RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
  // find the nearest column and row to that position.
  Column := frmGoPhast.Grid.NearestColumnPosition(APoint.X);
  Row := frmGoPhast.Grid.NearestRowPosition(APoint.Y);
  if ((Column >= 0) and (Column <= frmGoPhast.Grid.ColumnCount))
    or ((Row >= 0) and (Row <= frmGoPhast.Grid.RowCount)) then
  begin
    // The point is over the grid
    if IsOnTopColumn(X, Y) or IsOnTopRow(X, Y) then
    begin
      // The point is over a column or row boundary
      result := True;
    end
    else
    begin
      result := False;
    end;
  end
  else
  begin
    // The point is not over the grid
    result := False;
  end;
end;
  
procedure TCustomGridCursorTool.SetTopCursor(X, Y: integer);
begin
  if UseSelectedCursor then
  begin
    Cursor := GetSelectedCursor;
    Exit;
  end;
  
  if IsOverTopColumnOrRow(X, Y) then
  begin
    Cursor := GetSelectedCursor;
  end
  else
  begin
    Cursor := GetNonSelectedCursor;
  end;
end;
{$ENDREGION}

{$REGION 'TRotateGridTool'}
{ TRotateGridTool }
  
procedure TRotateGridTool.Activate;
begin
  frmGoPhast.frameTopView.ZoomBox.Hint := frmGoPhast.tbGridAngle.Hint;
  frmGoPhast.frameFrontView.ZoomBox.Hint := 'Only the top view can be rotated.';
  frmGoPhast.frameSideView.ZoomBox.Hint := 'Only the top view can be rotated.';
  Layer32;
end;
  
procedure TRotateGridTool.DrawRotatedGrid(Bitmap: TBitmap32);
var
  ARealPoint: TPoint2D;
  Center: TPoint2D;
  NewAngle: double;
begin
  ARealPoint.X := ZoomBox.X(frmGoPhast.CursorX);
  ARealPoint.Y := ZoomBox.Y(frmGoPhast.CursorY);
  Center := GridCenter;
  NewAngle := ArcTan2(ARealPoint.Y - Center.Y, ARealPoint.X - Center.X);
  Layer32.BringToFront;
  View.DrawRotatedGrid(NewAngle - RotateGridTool.StartAngle, Bitmap);
end;

function TRotateGridTool.GridCenter: TPoint2D;
begin
  result := View.GridCenter;
end;

procedure TRotateGridTool.DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32);
begin
  if frmGophast.CurrentTool <> self then Exit;
  if Rotating then
  begin
    DrawRotatedGrid(Buffer);
  end;
end;

procedure TRotateGridTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ARealPoint: TPoint2D;
  Center: TPoint2D;
begin
  inherited;
  if Button <> mbLeft then Exit;
  if ViewDirection = vdTop then
  begin
    ARealPoint.X := ZoomBox.X(X);
    ARealPoint.Y := ZoomBox.Y(Y);
    Center := GridCenter;
    FStartAngle := ArcTan2(ARealPoint.Y - Center.Y, ARealPoint.X - Center.X);
    FRotating := True;
  end;
end;
  
procedure TRotateGridTool.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (ViewDirection = vdTop) and (ssLeft in Shift) then
  begin
    ZoomBox.Image32.Invalidate;
  end;
end;
  
procedure TRotateGridTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ARealPoint: TPoint2D;
  Center: TPoint2D;
  NewAngle: double;
begin
  if ViewDirection = vdTop then
  begin
    ARealPoint.X := ZoomBox.X(X);
    ARealPoint.Y := ZoomBox.Y(Y);
    Center := GridCenter;
    NewAngle := ArcTan2(ARealPoint.Y - Center.Y, ARealPoint.X - Center.X);
    FRotating := False;
    if NewAngle <> StartAngle then
    begin
      frmGoPhast.UndoStack.Submit(
        TUndoSetAngle.Create(frmGoPhast.Grid.GridAngle
        + NewAngle - StartAngle));
      frmGoPhast.SynchronizeViews(vdTop);
    end;
  end;
  inherited;
end;
  
procedure TRotateGridTool.RightClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FRotating := false;
end;
{$ENDREGION}

{$REGION 'TLassoTool'}
{ TLassoTool }
  
procedure TLassoTool.Activate;
begin
  inherited;
  CreateLayers;
end;
  
procedure TLassoTool.DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32);
begin
  inherited;
  if frmGoPhast.CurrentTool <> self then Exit;
  if FSelectLine <> nil then
  begin
    Buffer.BeginUpdate;
    try
      FSelectLine.Draw(Buffer);
    finally
      Buffer.EndUpdate
    end;
  end;
end;
  
function TLassoTool.GetHint: string;
begin
  result := 'Click and drag to select objects with lasso';
end;
  
procedure TLassoTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FSelectLine.Free;
  FSelectLine := TLine.Create(1000);
  FSelectLine.AddPoint(Point(X, Y));
  ZoomBox.Image32.Invalidate;
end;
  
procedure TLassoTool.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (FSelectLine <> nil) then
  begin
    // set the correct cursor
    Cursor := crArrow;
    // If the cursor has moved far enough, add another point to the
    // lasso.
    if (FSelectLine.Count = 0)
      or (Abs(FSelectLine.Points[FSelectLine.Count - 1].X - X) > 10)
      or (Abs(FSelectLine.Points[FSelectLine.Count - 1].Y - Y) > 10) then
    begin
      FSelectLine.AddPoint(Point(X, Y));
      ZoomBox.Image32.Invalidate;
    end;
  end;
end;
  
procedure TLassoTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (FSelectLine <> nil) then
  begin
    // finish FSelectLine
    FSelectLine.AddPoint(Point(X, Y));
    FSelectLine.AddPoint(FSelectLine.Points[0]);
    // Select screen objects with SelectLine.
    SelectScreenObjectsInGui(ssShift in Shift);
    // Get rid of SelectLine
    FreeAndNil(FSelectLine);
    // redraw
    ZoomBox.Image32.Invalidate;
    frmGoPhast.tbSelect.Down := true;
    frmGoPhast.tbSelect.OnMouseDown(frmGoPhast.tbSelect, mbLeft, [], 0, 0);
    frmGoPhast.tbSelectClick(frmGoPhast.tbSelect);
  end;
  inherited;
end;

function TLassoTool.ScreenObjectInside(AScreenObject: TScreenObject): boolean;
var
  APoint: TPoint;
  PointIsInside: Boolean;
  SectionIndex: Integer;
begin
  // If at least one point of the screen object is inside SelectLine
  // and SelectLine does not intersect the screen object then
  // the entire object is inside SelectLine.

  // PointIsInside  will be set to true if the first point in
  // AScreenObject is inside SelectLine.

  Assert(FSelectLine <> nil);
  PointIsInside := false;
  if AScreenObject.Count > 0 then
  begin
    for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
    begin
      APoint := AScreenObject.CanvasCoordinates[
        AScreenObject.SectionStart[SectionIndex]];
      PointIsInside := FSelectLine.Inside(APoint);
      if not PointIsInside then
      begin
        break;
      end;
    end;
  end;
  result := PointIsInside and not AScreenObject.SelectLines.Intersect(FSelectLine);
end;
  
function TScreenObjectTool.SelectScreenObjectsInGui(
  const ToggleSelection: boolean): boolean;
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
  Update: boolean;
  UndoChangeSelection: TUndoChangeSelection;
begin
  result := false;
  SelectingObjectsWithLine := True;
  try
    // SelectScreenObjectsWithLine returns true if an entire object is selected by
    // being inside SelectLine. If ToggleSelection is true, screen objects inside
    // SelectLine are toggled from Selected to not Selected and vice-versa.
    UndoChangeSelection := TUndoChangeSelection.Create;
    try
      Update := False;
      try
        for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
        begin
          AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
          // Do some simple checks to make sure the screen object can be selected.
          if (AScreenObject.ViewDirection = ViewDirection)
            and not AScreenObject.Deleted and AScreenObject.Visible then
          begin
            if ScreenObjectInside(AScreenObject) then
            begin
              if ToggleSelection then
              begin
                AScreenObject.Selected := not AScreenObject.Selected;
                if AScreenObject.Selected then
                begin
                  result := True;
                end;
                Update := True;
              end
              else
              begin
                if not AScreenObject.Selected then
                begin
                  AScreenObject.Selected := True;
                  Update := True;
                end;
                result := True;
              end;
            end
            else if not ToggleSelection then
            begin
              if AScreenObject.Selected then
              begin
                AScreenObject.Selected := False;
                Update := True;
              end;
            end;
          end
          else
          begin
            if AScreenObject.Selected then
            begin
              AScreenObject.Selected := False;
              Update := True;
            end;
          end;
        end;
      finally
        View.UpdateSelectRectangle;
        if Update then
        begin
          View.ScreenObjectsHaveChanged := True;
        end;
      end;
      UndoChangeSelection.SetPostSelection;
      // If the selection needs to be changed, add UndoChangeSelection
      // to the UndoStack. This will cause the selection to be changed.
      // The UndoStack will take over ownership of UndoChangeSelection.
      // If there is no change in what's selected.
      // get rid of UndoChangeSelection.
      if UndoChangeSelection.SelectionChanged then
      begin
        frmGoPhast.UndoStack.Submit(UndoChangeSelection);
      end
      else
      begin
        UndoChangeSelection.Free;
      end;
    except
      UndoChangeSelection.Free;
      raise;
    end;
  finally
    SelectingObjectsWithLine := False;
  end;
end;
  
{$ENDREGION}

{$REGION 'TCustomCellSelectionTool'}
{ TCustomCellSelectionTool }

procedure TCustomCellSelectionTool.Activate;
begin
  inherited;
  CreateLayers;
end;

function TCustomCellSelectionTool.GetEvalAt: TEvaluatedAt;
begin
  result := eaBlocks;
  if frmGoPhast.PhastGrid.TopDataSet <> nil then
  begin
    result := frmGoPhast.PhastGrid.TopDataSet.EvaluatedAt;
  end;
  if frmGoPhast.PhastGrid.FrontDataSet <> nil then
  begin
    result := frmGoPhast.PhastGrid.FrontDataSet.EvaluatedAt;
  end;
  if frmGoPhast.PhastGrid.SideDataSet <> nil then
  begin
    result := frmGoPhast.PhastGrid.SideDataSet.EvaluatedAt;
  end;
  if frmGoPhast.PhastGrid.ThreeDDataSet <> nil then
  begin
    result := frmGoPhast.PhastGrid.ThreeDDataSet.EvaluatedAt;
  end;
end;

procedure TCustomCellSelectionTool.DrawSelectedFrontCells(FirstCol,
  LastCol, FirstLayer, LastLayer: integer; const BitMap: TBitmap32;
  Direction: TViewDirection; Color: TColor = SelectedCellsColor);
var
  APoint: T3DRealPoint;
  L1, L2, C1, C2: integer;
  Polygon: TPointArray;
  SelectColor32: TColor32;
  CIndex, LIndex: integer;
  FrontPoints: T2DRealPointArray;
  APoint2D: TPoint2D;
  P: TPolygon32;
  MultiplePolygons: boolean;
begin
  P := nil;
  MultiplePolygons := False;
  Assert((FirstCol >= 0) and (LastCol >= 0)
    and (FirstLayer >= 0) and (LastLayer >= 0));
  if FirstCol < LastCol then
  begin
    C1 := FirstCol;
    C2 := LastCol + 1;
  end
  else
  begin
    C1 := LastCol;
    C2 := FirstCol + 1;
  end;
  if FirstLayer < LastLayer then
  begin
    L1 := FirstLayer;
    L2 := LastLayer + 1;
  end
  else
  begin
    L1 := LastLayer;
    L2 := FirstLayer + 1;
  end;

  if frmGoPhast.PhastModel.ModelSelection = msPhast then
  begin
    SetLength(Polygon, 4);
    if GetEvalAt = eaBlocks then
    begin
      APoint := frmGoPhast.PhastGrid.ThreeDElementCorner(C1, 0, L1);
      Polygon[0].X := View(Direction).ZoomBox.XCoord(APoint.X);
      Polygon[0].Y := View(Direction).ZoomBox.YCoord(APoint.Z);

      APoint := frmGoPhast.PhastGrid.ThreeDElementCorner(C2, 0, L1);
      Polygon[1].X := View(Direction).ZoomBox.XCoord(APoint.X);
      Polygon[1].Y := View(Direction).ZoomBox.YCoord(APoint.Z);

      APoint := frmGoPhast.PhastGrid.ThreeDElementCorner(C2, 0, L2);
      Polygon[2].X := View(Direction).ZoomBox.XCoord(APoint.X);
      Polygon[2].Y := View(Direction).ZoomBox.YCoord(APoint.Z);

      APoint := frmGoPhast.PhastGrid.ThreeDElementCorner(C1, 0, L2);
      Polygon[3].X := View(Direction).ZoomBox.XCoord(APoint.X);
      Polygon[3].Y := View(Direction).ZoomBox.YCoord(APoint.Z);
    end
    else
    begin
      APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(C1, 0, L1);
      Polygon[0].X := View(Direction).ZoomBox.XCoord(APoint.X);
      Polygon[0].Y := View(Direction).ZoomBox.YCoord(APoint.Z);

      APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(C2, 0, L1);
      Polygon[1].X := View(Direction).ZoomBox.XCoord(APoint.X);
      Polygon[1].Y := View(Direction).ZoomBox.YCoord(APoint.Z);

      APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(C2, 0, L2);
      Polygon[2].X := View(Direction).ZoomBox.XCoord(APoint.X);
      Polygon[2].Y := View(Direction).ZoomBox.YCoord(APoint.Z);

      APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(C1, 0, L2);
      Polygon[3].X := View(Direction).ZoomBox.XCoord(APoint.X);
      Polygon[3].Y := View(Direction).ZoomBox.YCoord(APoint.Z);
    end;
    SelectColor32 := Color32(Color);
    SelectColor32 := SetAlpha(SelectColor32, 128);
    DrawBigPolygon32(BitMap, SelectColor32,
      SelectColor32, 0, Polygon, P, MultiplePolygons, True);
  end
  else
  begin
    Assert(frmGoPhast.PhastModel.ModelSelection = msModflow);

    FrontPoints := frmGoPhast.PhastModel.ModflowGrid.FrontCellPoints(
      frmGoPhast.PhastModel.ModflowGrid.SelectedRow);
    if FrontPoints = nil then
    begin
      Exit;
    end;
    SetLength(Polygon, 6);

    for CIndex := C1 to C2 - 1 do
    begin
      for LIndex := L1 to L2 - 1 do
      begin
        APoint2D := FrontPoints[CIndex*2, LIndex];
        Polygon[0] := View(Direction).ConvertPoint(APoint2D);

        APoint2D := FrontPoints[CIndex*2+1, LIndex];
        Polygon[1] := View(Direction).ConvertPoint(APoint2D);

        APoint2D := FrontPoints[CIndex*2+2, LIndex];
        Polygon[2] := View(Direction).ConvertPoint(APoint2D);

        APoint2D := FrontPoints[CIndex*2+2, LIndex+1];
        Polygon[3] := View(Direction).ConvertPoint(APoint2D);

        APoint2D := FrontPoints[CIndex*2+1, LIndex+1];
        Polygon[4] := View(Direction).ConvertPoint(APoint2D);

        APoint2D := FrontPoints[CIndex*2, LIndex+1];
        Polygon[5] := View(Direction).ConvertPoint(APoint2D);

        SelectColor32 := Color32(Color);
        SelectColor32 := SetAlpha(SelectColor32, 128);
        DrawBigPolygon32(BitMap, SelectColor32,
          SelectColor32, 0, Polygon, P, MultiplePolygons, True);
      end;
    end;
  end;
end;

procedure TCustomCellSelectionTool.DrawSelectedSideCells(FirstRow, LastRow,
  FirstLayer, LastLayer: integer; const BitMap: TBitmap32;
  Direction: TViewDirection; Color: TColor = SelectedCellsColor);
var
  APoint: T3DRealPoint;
  L1, L2, R1, R2: integer;
  Polygon: TPointArray;
  SelectColor32: TColor32;
  RIndex, LIndex: integer;
  SidePoints: T2DRealPointArray;
  APoint2D: TPoint2D;
  P: TPolygon32;
  MultiplePolygons: boolean;
begin
  P := nil;
  MultiplePolygons := False;
  Assert((FirstRow >= 0) and (LastRow >= 0)
    and (FirstLayer >= 0) and (LastLayer >= 0));
  if FirstRow < LastRow then
  begin
    R1 := FirstRow;
    R2 := LastRow + 1;
  end
  else
  begin
    R1 := LastRow;
    R2 := FirstRow + 1;
  end;

  if FirstLayer < LastLayer then
  begin
    L1 := FirstLayer;
    L2 := LastLayer + 1;
  end
  else
  begin
    L1 := LastLayer;
    L2 := FirstLayer + 1;
  end;

  if frmGoPhast.PhastModel.ModelSelection = msPhast then
  begin
    SetLength(Polygon, 4);
    if GetEvalAt = eaBlocks then
    begin
      APoint := frmGoPhast.PhastGrid.ThreeDElementCorner(0, R1, L1);
      Polygon[0].X := View(Direction).ZoomBox.XCoord(APoint.Z);
      Polygon[0].Y := View(Direction).ZoomBox.YCoord(APoint.Y);

      APoint := frmGoPhast.PhastGrid.ThreeDElementCorner(0, R2, L1);
      Polygon[1].X := View(Direction).ZoomBox.XCoord(APoint.Z);
      Polygon[1].Y := View(Direction).ZoomBox.YCoord(APoint.Y);

      APoint := frmGoPhast.PhastGrid.ThreeDElementCorner(0, R2, L2);
      Polygon[2].X := View(Direction).ZoomBox.XCoord(APoint.Z);
      Polygon[2].Y := View(Direction).ZoomBox.YCoord(APoint.Y);

      APoint := frmGoPhast.PhastGrid.ThreeDElementCorner(0, R1, L2);
      Polygon[3].X := View(Direction).ZoomBox.XCoord(APoint.Z);
      Polygon[3].Y := View(Direction).ZoomBox.YCoord(APoint.Y);
    end
    else
    begin
      APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(0, R1, L1);
      Polygon[0].X := View(Direction).ZoomBox.XCoord(APoint.Z);
      Polygon[0].Y := View(Direction).ZoomBox.YCoord(APoint.Y);

      APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(0, R2, L1);
      Polygon[1].X := View(Direction).ZoomBox.XCoord(APoint.Z);
      Polygon[1].Y := View(Direction).ZoomBox.YCoord(APoint.Y);

      APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(0, R2, L2);
      Polygon[2].X := View(Direction).ZoomBox.XCoord(APoint.Z);
      Polygon[2].Y := View(Direction).ZoomBox.YCoord(APoint.Y);

      APoint := frmGoPhast.PhastGrid.ThreeDCellCorner(0, R1, L2);
      Polygon[3].X := View(Direction).ZoomBox.XCoord(APoint.Z);
      Polygon[3].Y := View(Direction).ZoomBox.YCoord(APoint.Y);
    end;

    SelectColor32 := Color32(Color);
    SelectColor32 := SetAlpha(SelectColor32, 128);

    DrawBigPolygon32(BitMap, SelectColor32,
      SelectColor32, 0, Polygon, P, MultiplePolygons, True);
  end
  else
  begin
    Assert(frmGoPhast.PhastModel.ModelSelection = msModflow);

    SidePoints := frmGoPhast.PhastModel.ModflowGrid.SideCellPoints(
      frmGoPhast.PhastModel.ModflowGrid.SelectedColumn);
    if SidePoints = nil then
    begin
      Exit;
    end;
    SetLength(Polygon, 6);

    for RIndex := R1 to R2 - 1 do
    begin
      for LIndex := L1 to L2 - 1 do
      begin
        { TODO : Use conversion routines like ConvertPoint wherever possible. }
        APoint2D := SidePoints[RIndex*2, LIndex];
        Polygon[0] := View(Direction).ConvertPoint(APoint2D);

        APoint2D := SidePoints[RIndex*2+1, LIndex];
        Polygon[1] := View(Direction).ConvertPoint(APoint2D);

        APoint2D := SidePoints[RIndex*2+2, LIndex];
        Polygon[2] := View(Direction).ConvertPoint(APoint2D);

        APoint2D := SidePoints[RIndex*2+2, LIndex+1];
        Polygon[3] := View(Direction).ConvertPoint(APoint2D);

        APoint2D := SidePoints[RIndex*2+1, LIndex+1];
        Polygon[4] := View(Direction).ConvertPoint(APoint2D);

        APoint2D := SidePoints[RIndex*2, LIndex+1];
        Polygon[5] := View(Direction).ConvertPoint(APoint2D);

        SelectColor32 := Color32(Color);
        SelectColor32 := SetAlpha(SelectColor32, 128);
        DrawBigPolygon32(BitMap, SelectColor32,
          SelectColor32, 0, Polygon, P, MultiplePolygons, True);
      end;
    end;
  end;
end;

procedure TCustomCellSelectionTool.DrawSelectedTopCells(FirstCol, LastCol,
  FirstRow, LastRow: integer; const BitMap: TBitmap32;
  Direction: TViewDirection; Color: TColor = SelectedCellsColor);
var
  APoint: TPoint2D;
  R1, R2, C1, C2: integer;
  Polygon: TPointArray;
  SelectColor32: TColor32;
  P: TPolygon32;
  MultiplePolygons: boolean;
begin
  P := nil;
  MultiplePolygons := False;
  Assert((FirstRow >= 0) and (LastRow >= 0)
    and (FirstRow >= 0) and (LastRow >= 0));
  SetLength(Polygon, 4);
  if FirstCol < LastCol then
  begin
    C1 := FirstCol;
    C2 := LastCol + 1;
  end
  else
  begin
    C1 := LastCol;
    C2 := FirstCol + 1;
  end;
  if FirstRow < LastRow then
  begin
    R1 := FirstRow;
    R2 := LastRow + 1;
  end
  else
  begin
    R1 := LastRow;
    R2 := FirstRow + 1;
  end;

  if GetEvalAt = eaBlocks then
  begin
    APoint := frmGoPhast.Grid.TwoDElementCorner(C1, R1);
    Polygon[0] := View(Direction).ConvertPoint(APoint);

    APoint := frmGoPhast.Grid.TwoDElementCorner(C1, R2);
    Polygon[1] := View(Direction).ConvertPoint(APoint);

    APoint := frmGoPhast.Grid.TwoDElementCorner(C2, R2);
    Polygon[2] := View(Direction).ConvertPoint(APoint);

    APoint := frmGoPhast.Grid.TwoDElementCorner(C2, R1);
    Polygon[3] := View(Direction).ConvertPoint(APoint);
  end
  else
  begin
    APoint := frmGoPhast.Grid.TwoDCellCorner(C1, R1);
    Polygon[0] := View(Direction).ConvertPoint(APoint);

    APoint := frmGoPhast.Grid.TwoDCellCorner(C1, R2);
    Polygon[1] := View(Direction).ConvertPoint(APoint);

    APoint := frmGoPhast.Grid.TwoDCellCorner(C2, R2);
    Polygon[2] := View(Direction).ConvertPoint(APoint);

    APoint := frmGoPhast.Grid.TwoDCellCorner(C2, R1);
    Polygon[3] := View(Direction).ConvertPoint(APoint);
  end;

  SelectColor32 := Color32(Color);
  SelectColor32 := SetAlpha(SelectColor32, 128);
  DrawBigPolygon32(BitMap, SelectColor32,
    SelectColor32, 0, Polygon, P, MultiplePolygons, True);
end;
{$ENDREGION}

{$REGION 'TCustomCreateScreenObjectTool'}
{ TCustomCreateScreenObjectTool }
procedure TCustomCreateScreenObjectTool32.Activate;
begin
  inherited;
  CreateLayers;
end;

function TCustomCreateScreenObjectTool.CanAddPoint: boolean;
  function CompatibleCursorGrid: boolean;
  begin
    result := false;
    case CurrentScreenObject.ViewDirection of
      vdTop:
        begin
          result := frmGoPhast.CursorGrid = cgTop;
        end;
      vdFront:
        begin
          result := frmGoPhast.CursorGrid = cgFront;
        end;
      vdSide:
        begin
          result := frmGoPhast.CursorGrid = cgSide;
        end;
    else Assert(False);
    end;
  end;
begin
  result := (CurrentScreenObject = nil) or CompatibleCursorGrid;
end;
  
function TCustomCreateScreenObjectTool.CanAddPoint32: boolean;
  function CompatibleCursorGrid: boolean;
  begin
    result := false;
    case CurrentScreenObject.ViewDirection of
      vdTop:
        begin
          result := (Layer32 = FTopLayer);
        end;
      vdFront:
        begin
          result := (Layer32 = FFrontLayer);
        end;
      vdSide:
        begin
          result := (Layer32 = FSideLayer);
        end;
    else Assert(False);
    end;
  end;
begin
  result := (CurrentScreenObject = nil) or CompatibleCursorGrid;
end;
  
constructor TCustomCreateScreenObjectTool.Create(AOwner: TComponent);
begin
  inherited;
  FCurrentUndo := nil;
end;

destructor TCustomCreateScreenObjectTool.Destroy;
begin
  FCurrentUndo.Free;
//  if (FCurrentUndo <> nil) and (FCurrentUndo is TUndoCreateScreenObject)
//    and not (TUndoCreateScreenObject(FCurrentUndo).HasBeenUsed) then
//  begin
//    FreeAndNil(FCurrentUndo)
//  end;
  inherited;
end;
  
procedure TCustomCreateScreenObjectTool.DoubleClick(Sender: TObject);
begin
  inherited;
  FDoubleClicked := True;
end;
  
procedure TCustomCreateScreenObjectTool.FinishScreenObjects;
var
  UndoCreateScreenObject: TUndoCreateScreenObject;
begin
  if not frmGoPhast.CanEdit then
  begin
    //CurrentScreenObject.Free;
    frmGoPhast.PhastModel.RemoveScreenObject(CurrentScreenObject);
    CurrentScreenObject := nil;
    ClearPoints;
    FreeAndNil(FCurrentUndo);
    Exit;
  end;
  frmGoPhast.CanEdit := False;
  try
    if CurrentScreenObject <> nil then
    begin
      if CurrentScreenObject.Count <= 0 then
      begin
        frmGoPhast.PhastModel.RemoveScreenObject(CurrentScreenObject);
        CurrentScreenObject := nil;
        ClearPoints;
        FreeAndNil(FCurrentUndo);
        Exit;
      end;
  
      { TODO : This step should be replaced by a call to a virtual function. }
      if frmGoPhast.tbPolygon.Down then
      begin
        try
          CurrentScreenObject.AddPoint(CurrentScreenObject.Points[0], False);
        except on E: EScreenObjectError do
          begin
            Beep;
          end;
        end;
      end;

      if CurrentScreenObject.Closed then
      begin
        CurrentScreenObject.SetValuesOfEnclosedCells := True;
      end
      else
      begin
        CurrentScreenObject.SetValuesOfIntersectedCells := True;
      end;
  
      SetDefaultElevationFormulas;
  
      UndoCreateScreenObject := CurrentUndo as TUndoCreateScreenObject;
      try
        try
          Assert(frmScreenObjectProperties <> nil);
  
          frmScreenObjectProperties.GetData(CurrentScreenObject);
          if frmScreenObjectProperties.ShowModal <> mrOK then
          begin
            frmGoPhast.PhastModel.RemoveScreenObject(CurrentScreenObject);
            FreeAndNil(FCurrentUndo);
          end
          else
          begin
            UndoCreateScreenObject.SetPostSelection;
            UndoCreateScreenObject.UpdateObservations;
            frmGoPhast.UndoStack.Submit(UndoCreateScreenObject);
            FCurrentUndo := nil;
            // FPreviousScreenObjects allows the screen object
            // to still be visible
            // in ZoomBox.Image32 while the grid is being
            // redrawn.
            View.PreviousScreenObjects.Add(CurrentScreenObject);
          end;
          CurrentScreenObject := nil;
          ClearPoints;
        finally
          View.SetFocus;
        end;
      except
        frmGoPhast.PhastModel.RemoveScreenObject(CurrentScreenObject);
        CurrentScreenObject := nil;
        ClearPoints;
        FreeAndNil(FCurrentUndo);
        raise;
      end;
      View.ScreenObjectsHaveChanged := True;
    end;
  finally
    frmGoPhast.CanEdit := True;
  end;
end;
  
procedure TCustomCreateScreenObjectTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (FCurrentScreenObject = nil)
    and (FViewDirection <> ViewDirection)
    and (FStoredPoints <> nil) then
  begin
    ClearPoints
  end;
  FViewDirection := ViewDirection;
end;

procedure TCustomCreateScreenObjectTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FDoubleClicked then
  begin
    FinishScreenObjects;
    FDoubleClicked := False;
  end;
  FPriorCursorX := X;
  FPriorCursorY := Y;
  ZoomBox.Image32.Invalidate;
end;
  
procedure TCustomCreateScreenObjectTool.SetDefaultElevationFormulas;
begin
  Assert(CurrentScreenObject <> nil);
  CurrentScreenObject.HigherElevationFormula :=
    frmGoPhast.PhastModel.DefaultHigherElevationFormula(
    CurrentScreenObject.ViewDirection);
  CurrentScreenObject.LowerElevationFormula :=
    frmGoPhast.PhastModel.DefaultLowerElevationFormula(
    CurrentScreenObject.ViewDirection);
  CurrentScreenObject.ElevationFormula :=
    frmGoPhast.PhastModel.DefaultElevationFormula(
    CurrentScreenObject.ViewDirection);
end;

procedure TCustomStoreVerticesTool.StorePointsOfOtherObjects(
  ScreenObject: TScreenObject);
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  PointIndex: Integer;
  APoint: TPoint2D;
  AZoomBox: TQRbwZoomBox2;
  IntPoint: TPoint;
  PointStorage: TScreenPointStorage;
  LocalViewDirection: TViewDirection;
begin
  if FStoredPoints = nil then
  begin
    if ScreenObject <> nil then
    begin
      LocalViewDirection := ScreenObject.ViewDirection;
    end
    else
    begin
      LocalViewDirection := ViewDirection;
    end;
    AZoomBox := ZoomBox;
    FStoredPoints := TObjectList.Create;
    for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
      if AScreenObject.Visible and not AScreenObject.Deleted
        and (AScreenObject.ViewDirection = LocalViewDirection)
        and (AScreenObject <> ScreenObject) then
      begin
        for PointIndex := 0 to AScreenObject.Count - 1 do
        begin
          APoint := AScreenObject.Points[PointIndex];
          IntPoint.X := AZoomBox.XCoord(APoint.X);
          IntPoint.Y := AZoomBox.YCoord(APoint.Y);
          PointStorage := TScreenPointStorage.Create;
          FStoredPoints.Add(PointStorage);
          PointStorage.FScreenObject := AScreenObject;
          PointStorage.FVertexIndex := PointIndex;
          FVisibleVertices.AddPoint(IntPoint.X, IntPoint.Y, PointStorage);
        end;
      end;
    end;
  end;
end;

{$ENDREGION}

{$REGION 'TCreatePointScreenObjectTool'}
{ TCreatePointScreenObjectTool }
  
procedure TCreatePointScreenObjectTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FShift := Shift;
  UpdateCursors;
end;

procedure TCreatePointScreenObjectTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then Exit;
  CreatePointScreenObject(X, Y, Shift);
  inherited;
end;
  
procedure TCreatePointScreenObjectTool.CreatePointScreenObject(X, Y: Integer;
  Shift: TShiftState);
var
  APoint: TPoint2D;
begin
  // create the screen object.
  if frmGoPhast.tbPoint.Down then
  begin
    CurrentScreenObject :=
      frmGoPhast.PhastModel.ScreenObjectClass.CreateWithViewDirection(
      frmGoPhast.PhastModel, ViewDirection, FCurrentUndo);
    CurrentScreenObject.ElevationCount :=
      TElevationCount(frmGoPhast.comboZCount.ItemIndex);
    ClearPoints;
  end
  else
  begin
    Assert(False);
  end;

  frmGoPhast.PhastModel.AddScreenObject(CurrentScreenObject);
  StorePointsOfOtherObjects(CurrentScreenObject);

  if not (ssShift in Shift)
    or not FindPointInNearbyScreenObject(Point(X, Y), APoint) then
  begin
    // Get the real-world coordinates of the mouse.
    APoint.X := ZoomBox.X(X);
    APoint.Y := ZoomBox.Y(Y);
  end;

  // Add a point at the current mouse position.
  try
    CurrentScreenObject.AddPoint(APoint, False);
  except on E: EScreenObjectError do
    begin
      Beep;
    end
  end;
  // Get the object properties from the user.
  FinishScreenObjects;
end;
  
function TCreatePointScreenObjectTool.GetCursor: TCursor;
var
  APoint: TPoint2D;
begin
  StorePointsOfOtherObjects(CurrentScreenObject);

  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(
      Point(frmGoPhast.CursorX, frmGoPhast.CursorY), APoint) then
  begin
    result := crSnapPointArrow;
  end
  else
  begin
    result := crPointArrow;
  end;
end;

function TCreatePointScreenObjectTool.GetHint: string;
begin
  result := 'Click to create point object';
end;
{$ENDREGION}

{$REGION 'TCreateLineScreenObjectTool'}
{ TCreateLineScreenObjectTool }
  
procedure TCreateLineScreenObjectTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then Exit;
  if ((FPriorCursorX <> X) or (FPriorCursorY <> Y)) and CanAddPoint then
  begin
    ContinueLineScreenObject(X, Y, Shift);
  end;
  inherited;
end;

procedure TCreateLineScreenObjectTool.ContinueLineScreenObject(X, Y: Integer;
  Shift: TShiftState);
var
  APoint: TPoint2D;
begin
  // If the screen object hasn't been started yet, start it.
  if CurrentScreenObject = nil then
  begin
    if frmGoPhast.tbLine.Down then
    begin
      CurrentScreenObject :=
        frmGoPhast.PhastModel.ScreenObjectClass.CreateWithViewDirection(
        frmGoPhast.PhastModel, ViewDirection, FCurrentUndo);
      ClearPoints;
    end
    else if frmGoPhast.tbPolygon.Down then
    begin
      CurrentScreenObject :=
        frmGoPhast.PhastModel.ScreenObjectClass.CreateWithViewDirection(
        frmGoPhast.PhastModel, ViewDirection, FCurrentUndo);
      ClearPoints;
    end
    else
    begin
      Assert(False);
    end;
    CurrentScreenObject.ElevationCount := TElevationCount(frmGoPhast.comboZCount.ItemIndex);
    frmGoPhast.PhastModel.AddScreenObject(CurrentScreenObject);
    ClearPoints;
  end;
  StorePointsOfOtherObjects(CurrentScreenObject);
  if not (ssShift in Shift)
    or not FindPointInNearbyScreenObject(Point(X, Y), APoint) then
  begin
    // Get the real-world coordinates of the mouse.
    APoint.X := ZoomBox.X(X);
    APoint.Y := ZoomBox.Y(Y);
  end;
  // Add a point at the current mouse position.
  try
    CurrentScreenObject.AddPoint(APoint, False);
  except on EScreenObjectError do
    begin
      Beep;
    end;
  end;
end;
  
procedure TCreateLineScreenObjectTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
var
  Points: array of TPoint;
begin
  if (frmGoPhast.CurrentTool <> self) then Exit;
  if (CurrentScreenObject <> nil) and CanAddPoint32
    and (CurrentScreenObject.Count > 0)
    and (Sender = Layer32) then
  begin
    Buffer.BeginUpdate;
    try
      if frmGoPhast.tbLine.Down then
      begin
        SetLength(Points, 2);
        Points[0] := FCurrentScreenObject.
          CanvasCoordinates[FCurrentScreenObject.Count-1];
        Points[1] := GetSnapPoint;
      end
      else if frmGoPhast.tbPolygon.Down then
      begin
        if (ssShift in FShift) then
        begin
          StorePointsOfOtherObjects(FCurrentScreenObject);
        end;
        SetLength(Points, 3);
        Points[0] := FCurrentScreenObject.
          CanvasCoordinates[FCurrentScreenObject.Count-1];
        Points[1] := GetSnapPoint;
        Points[2] := FCurrentScreenObject.
          CanvasCoordinates[0];
      end
      else
      begin
        Assert(False);
      end;
  
      Layer32.BringToFront;
      DrawBigPolyline32(Buffer, clBlack32, 1, Points, True);
    finally
      Buffer.EndUpdate;
    end;
  end;
end;
  
procedure TCreateLineScreenObjectTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FShift := Shift;
  UpdateCursors;
  // redraw to indicate where the screen object would be if you clicked now.
  if (CurrentScreenObject <> nil) and CanAddPoint then
  begin
    if ssShift in Shift then
    begin
      StorePointsOfOtherObjects(CurrentScreenObject);
    end;
    Layer32.Changed;
    View.ZoomBox.Image32.Invalidate;
  end;
  
end;
  
function TCreateLineScreenObjectTool.GetCursor: TCursor;
var
  APoint: TPoint2D;
begin
  StorePointsOfOtherObjects(CurrentScreenObject);
  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(
      Point(frmGoPhast.CursorX, frmGoPhast.CursorY), APoint) then
  begin
    if frmGoPhast.tbLine.Down then
    begin
      result := crSnapLineArrow;
    end
    else
    begin
      Assert(frmGoPhast.tbPolygon.Down);
      result := crSnapPolygonArrow;
    end;
  end
  else
  begin
    if frmGoPhast.tbLine.Down then
    begin
      result := crLineArrow;
    end
    else
    begin
      Assert(frmGoPhast.tbPolygon.Down);
      result := crPolygonArrow;
    end;
  end;
end;

function TCreateLineScreenObjectTool.GetHint: string;
begin
  if frmGoPhast.tbLine.Down then
  begin
    result := 'Click to create polyline object';
  end
  else if frmGoPhast.tbPolygon.Down then
  begin
    result := 'Click to create polygon object';
  end
  else
  begin
    Assert(False);
  end;
end;
{$ENDREGION}

{$REGION 'TCreateStraightLineScreenObjectTool'}
{ TCreateStraightLineScreenObjectTool }
  
procedure TCreateStraightLineScreenObjectTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  FShift := Shift;
  UpdateCursors;
  
  // redraw to indicate where the screen object would be if you clicked now.
  if (CurrentScreenObject <> nil) and CanAddPoint then
  begin
    View.ZoomBox.Image32.Invalidate;
  end;
  
  inherited;
end;
  
procedure TCreateStraightLineScreenObjectTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then Exit;
  if ((FPriorCursorX <> X) or (FPriorCursorY <> Y)) and not FDoubleClicked
    and CanAddPoint then
  begin
    ContinueStraightLineScreenObject(X, Y, Shift);
  end;
  inherited;
end;
  
procedure TCreateStraightLineScreenObjectTool.GetPointFromCursorPosition
  (var APoint: TPoint2D; X, Y: Integer; PreviousPoint: TPoint2D);
var
  YR: Real;
  XR: Real;
  Angle: Real;
  RotatedAngle: Real;
  R: Real;
  XPrimeD: Real;
  YPrimeD: Real;
  XPrimeI: Integer;
  YPrimeI: Integer;
begin
  case View.ViewDirection of
    vdTop:
      begin
        YR := APoint.Y - PreviousPoint.Y;
        XR := APoint.X - PreviousPoint.X;
        Angle := ArcTan2(YR, XR);
        RotatedAngle := Angle - frmGoPhast.Grid.GridAngle;
        R := Sqrt(Sqr(XR) + Sqr(YR));
        XPrimeD := Cos(RotatedAngle) * R;
        YPrimeD := Sin(RotatedAngle) * R;
        if Abs(XPrimeD) > Abs(YPrimeD) then
        begin
          APoint.X := PreviousPoint.X +
            Cos(frmGoPhast.Grid.GridAngle) * XPrimeD;
          APoint.Y := PreviousPoint.Y +
            Sin(frmGoPhast.Grid.GridAngle) * XPrimeD;
        end
        else
        begin
          APoint.X := PreviousPoint.X -
            Sin(frmGoPhast.Grid.GridAngle) * YPrimeD;
          APoint.Y := PreviousPoint.Y +
            Cos(frmGoPhast.Grid.GridAngle) * YPrimeD;
        end;
      end;
    vdFront, vdSide:
      begin
        XPrimeI := X - ZoomBox.XCoord(PreviousPoint.X);
        YPrimeI := Y - ZoomBox.YCoord(PreviousPoint.Y);
        if Abs(XPrimeI) > Abs(YPrimeI) then
        begin
          APoint.X := ZoomBox.X(X);
          APoint.Y := PreviousPoint.Y;
        end
        else
        begin
          APoint.X := PreviousPoint.X;
          APoint.Y := ZoomBox.Y(Y);
        end;
      end;
  else
    Assert(False);
  end;
end;
  
procedure TCreateStraightLineScreenObjectTool.
  ContinueStraightLineScreenObject(X, Y: Integer; Shift: TShiftState);
var
  APoint, PreviousPoint: TPoint2D;
begin
  // if the straight line object hasn't been started yet, begin it.
  if CurrentScreenObject = nil then
  begin
    if frmGoPhast.tbStraightLine.Down then
    begin
      CurrentScreenObject :=
        frmGoPhast.PhastModel.ScreenObjectClass.CreateWithViewDirection(
        frmGoPhast.PhastModel, ViewDirection, FCurrentUndo);
      ClearPoints;
    end
    else
    begin
      Assert(False);
    end;
    CurrentScreenObject.ElevationCount := TElevationCount(frmGoPhast.comboZCount.ItemIndex);

    frmGoPhast.PhastModel.AddScreenObject(CurrentScreenObject);
    ClearPoints;
  end;
  StorePointsOfOtherObjects(CurrentScreenObject);
  if not (ssShift in Shift)
    or not FindPointInNearbyScreenObject(Point(X, Y), APoint) then
  begin
    // Get the real-world coordinates of the mouse.
    APoint.X := ZoomBox.X(X);
    APoint.Y := ZoomBox.Y(Y);
  end;
  // Check and see whether this is a new box or not.
  if CurrentScreenObject.count = 0 then
  begin
    // If it is a new box, add one point at the mouse position.
    try
      CurrentScreenObject.AddPoint(APoint, False);
    except on E: EScreenObjectError do
      begin
        Beep;
      end
    end;
  end
  else
  begin
    // If it is not a new box, add another point in such a way that the
    // new edge is parallel to one of the edges of the grid.
    PreviousPoint := CurrentScreenObject.Points[CurrentScreenObject.Count -
      1];
    GetPointFromCursorPosition(APoint, X, Y, PreviousPoint);
  
    try
      CurrentScreenObject.AddPoint(APoint, False);
    except on E: EScreenObjectError do
      begin
        Beep;
      end
    end;
  end;
end;
  
procedure TCreateStraightLineScreenObjectTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
var
  Points: array of TPoint;
  APoint, PreviousPoint: TPoint2D;
  SnapPoint: TPoint;
begin
  if (frmGoPhast.CurrentTool <> self) then Exit;
  if (CurrentScreenObject <> nil) and CanAddPoint32
    and (CurrentScreenObject.Count > 0)
    and (Sender = Layer32) then
  begin
    Buffer.BeginUpdate;
    try
      SnapPoint := GetSnapPoint;
      APoint.X := ZoomBox.X(SnapPoint.X);
      APoint.Y := ZoomBox.Y(SnapPoint.Y);
      SetLength(Points, 2);
      PreviousPoint := CurrentScreenObject.Points[CurrentScreenObject.Count -
        1];

      GetPointFromCursorPosition(APoint, SnapPoint.X,
        SnapPoint.Y, PreviousPoint);

      if View.ViewDirection = vdSide then
      begin
        PreviousPoint := ConvertSidePoint(PreviousPoint);
        APoint := ConvertSidePoint(APoint);
      end;


      Points[0] := View.ConvertPoint(PreviousPoint);
      Points[1] := View.ConvertPoint(APoint);

      Layer32.BringToFront;
      DrawBigPolyline32(Buffer, clBlack32, 1, Points, True);
    finally
      Buffer.EndUpdate;
    end;
  end;
end;
  
function TCreateStraightLineScreenObjectTool.GetCursor: TCursor;
var
  APoint: TPoint2D;
begin
  StorePointsOfOtherObjects(CurrentScreenObject);

  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(
      Point(frmGoPhast.CursorX, frmGoPhast.CursorY), APoint) then
  begin
    result := crSnapStraightLineArrow;
  end
  else
  begin
    result := crStraightLineArrow;
  end;
end;

function TCreateStraightLineScreenObjectTool.GetHint: string;
begin
  result := 'Click to create straight line object';
end;
{$ENDREGION}

{$REGION 'TCreateRectangleScreenObjectTool'}
{ TCreateRectangleScreenObjectTool }
  
procedure TCreateRectangleScreenObjectTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  FShift := Shift;
  UpdateCursors;
  
  // redraw to indicate where the screen object would be if you clicked now.
  if (CurrentScreenObject <> nil) and CanAddPoint then
  begin
    View.ZoomBox.Image32.Invalidate;
  end;
  
  inherited;
end;
  
procedure TCreateRectangleScreenObjectTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then Exit;
  if CanAddPoint then
  begin
    ContinueRectangle(X, Y, Shift);
  end;
  inherited;
end;
  
procedure TCreateRectangleScreenObjectTool.GetRemaingPointsOnRectangleCorners
  (FirstCorner, ThirdCorner: TPoint2D;
  var SecondCorner, FourthCorner: TPoint2D);
var
  YPrime: Real;
  XPrime: Real;
  R: Real;
  RotatedAngle: Real;
  Angle: Real;
  XR: Real;
  YR: Real;
begin
  case ViewDirection of
    vdTop:
      begin
        YR := ThirdCorner.Y - FirstCorner.Y;
        XR := ThirdCorner.X - FirstCorner.X;
        Angle := ArcTan2(YR, XR);
        RotatedAngle := Angle - frmGoPhast.Grid.GridAngle;
        R := SqrT(Sqr(XR) + Sqr(YR));
        XPrime := Cos(RotatedAngle) * R;
        YPrime := Sin(RotatedAngle) * R;
        SecondCorner.X := FirstCorner.X +
          Cos(frmGoPhast.Grid.GridAngle) * XPrime;
        SecondCorner.Y := FirstCorner.Y +
          Sin(frmGoPhast.Grid.GridAngle) * XPrime;
        FourthCorner.X := FirstCorner.X -
          Sin(frmGoPhast.Grid.GridAngle) * YPrime;
        FourthCorner.Y := FirstCorner.Y +
          Cos(frmGoPhast.Grid.GridAngle) * YPrime;
      end;
    vdFront, vdSide:
      begin
        SecondCorner.X := FirstCorner.X;
        SecondCorner.Y := ThirdCorner.Y;
        FourthCorner.X := ThirdCorner.X;
        FourthCorner.Y := FirstCorner.Y;
      end;
  end;
end;
  
procedure TCreateRectangleScreenObjectTool.ContinueRectangle(X, Y: Integer;
  Shift: TShiftState);
var
  ThirdCorner, SecondCorner, FourthCorner, FirstCorner: TPoint2D;
begin
  // if the box hasn't been started yet, begin it.
  if CurrentScreenObject = nil then
  begin
    if frmGoPhast.tbRectangle.Down then
    begin
      CurrentScreenObject :=
        frmGoPhast.PhastModel.ScreenObjectClass.CreateWithViewDirection(
        frmGoPhast.PhastModel, ViewDirection, FCurrentUndo);
      ClearPoints;
    end
    else
    begin
      Assert(False);
    end;
    CurrentScreenObject.ElevationCount := TElevationCount(
      frmGoPhast.comboZCount.ItemIndex);

    frmGoPhast.PhastModel.AddScreenObject(CurrentScreenObject);
    ClearPoints;
  end;
  StorePointsOfOtherObjects(CurrentScreenObject);
  if not (ssShift in Shift)
    or not FindPointInNearbyScreenObject(Point(X, Y), ThirdCorner) then
  begin
    // Get the real-world coordinates of the mouse.
    ThirdCorner.X := ZoomBox.X(X);
    ThirdCorner.Y := ZoomBox.Y(Y);
  end;
  // Check and see whether this is a new box or not.
  if CurrentScreenObject.count = 0 then
  begin
    // If it is a new box, add one point at the mouse position.
    try
      CurrentScreenObject.AddPoint(ThirdCorner, False);
    except on EScreenObjectError do
      begin
        Beep;
      end;
    end;
  end
  else
  begin
    // If it is not a new box, add 4 points to finish the box.
  
    // The edges of box should be parallel to the grid.
    FirstCorner := CurrentScreenObject.Points[CurrentScreenObject.Count - 1];
  
    GetRemaingPointsOnRectangleCorners (FirstCorner, ThirdCorner,
      SecondCorner, FourthCorner);
  
    try
      CurrentScreenObject.AddPoint(SecondCorner, False);
    except on EScreenObjectError do
      begin
        Beep;
      end;
    end;
  
    try
      CurrentScreenObject.AddPoint(ThirdCorner, False);
    except on EScreenObjectError do
      begin
        Beep;
      end;
    end;
  
    try
      CurrentScreenObject.AddPoint(FourthCorner, False);
    except on EScreenObjectError do
      begin
        Beep;
      end;
    end;
  
    try
      CurrentScreenObject.AddPoint(CurrentScreenObject.Points[0], False);
    except on E: EScreenObjectError do
      begin
        Beep;
      end
    end;
  
    // Get the object properties from the user.
    FinishScreenObjects;
  end;
end;

procedure TCreateRectangleScreenObjectTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
var
  Points: array of TPoint;
  ThirdCorner, SecondCorner, FourthCorner, FirstCorner: TPoint2D;
  SnapPoint: TPoint;
begin
  if (frmGoPhast.CurrentTool <> self) then Exit;
  if (CurrentScreenObject <> nil) and CanAddPoint32
    and (CurrentScreenObject.Count > 0)
    and (Sender = Layer32) then
  begin
    Buffer.BeginUpdate;
    try
      SetLength(Points, 5);
      FirstCorner := CurrentScreenObject.Points[CurrentScreenObject.Count - 1];

      SnapPoint := GetSnapPoint;
      ThirdCorner.X := ZoomBox.X(SnapPoint.X);
      ThirdCorner.Y := ZoomBox.Y(SnapPoint.Y);

      GetRemaingPointsOnRectangleCorners (FirstCorner, ThirdCorner,
        SecondCorner, FourthCorner);

      if View.ViewDirection = vdSide then
      begin
        FirstCorner := ConvertSidePoint(FirstCorner);
        SecondCorner := ConvertSidePoint(SecondCorner);
        FourthCorner := ConvertSidePoint(FourthCorner);
      end;

      Points[0] := View.ConvertPoint(FirstCorner);
      Points[1] := View.ConvertPoint(SecondCorner);
      Points[2] := SnapPoint;
      Points[3] := View.ConvertPoint(FourthCorner);
      Points[4] := Points[0];
  
      Layer32.BringToFront;
      DrawBigPolyline32(Buffer, clBlack32, 1, Points, True);
    finally
      Buffer.EndUpdate;
    end;
  end;
end;
  
function TCreateRectangleScreenObjectTool.GetCursor: TCursor;
var
  APoint: TPoint2D;
begin
  StorePointsOfOtherObjects(CurrentScreenObject);

  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(
      Point(frmGoPhast.CursorX, frmGoPhast.CursorY), APoint) then
  begin
    result := crSnapRectangleArrow;
  end
  else
  begin
    result := crRectangleArrow;
  end;
end;

function TCreateRectangleScreenObjectTool.GetHint: string;
begin
  result := 'Click to create rectangle object';
end;
{$ENDREGION}

{$REGION 'TCustomEditScreenObjectTool'}
{ TCustomEditScreenObjectTool }
  
function TCustomEditScreenObjectTool.AreScreenObjectsSelected: boolean;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  // This procedure returns True if any screen object is selected.
  result := False;
  for Index := frmGoPhast.PhastModel.ScreenObjectCount - 1 downto 0 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Selected and (AScreenObject.ViewDirection = ViewDirection)
      then
    begin
      result := True;
      Exit;
    end;
  end;
end;
  
function TCustomEditScreenObjectTool.SelectScreenObjects(const X, Y: integer;
  const SelectLowerScreenObjects, ToggleSelectedItem: boolean;
  const CanChangeSelection: boolean = True;
  const ReturnScreenObjectPresent: boolean = False): boolean;
var
  Index: integer;
  AScreenObject: TScreenObject;
  Update: boolean;
  Start: integer;
  UndoChangeSelection: TUndoChangeSelection;
begin
  if not SelectLowerScreenObjects and not ToggleSelectedItem and
    CanChangeSelection and not ReturnScreenObjectPresent then
  begin
    // This may be the first click of a double-click.
    // If so, exit without doing anything.
    with frmGoPhast.PhastModel do
    begin
      for Index := 0 to ScreenObjectCount - 1 do
      begin
        AScreenObject := ScreenObjects[Index];
        if AScreenObject.Selected and (AScreenObject.ViewDirection =
          ViewDirection) then
        begin
          if AScreenObject.Select(X, Y) then
          begin
            result := True;
            Exit;
          end;
        end;
      end;
    end;
  end;
  
  // This procedure selects screen objects that are at or near (X,Y).
  UndoChangeSelection := TUndoChangeSelection.Create;
  try
    result := False;
    Update := False;
    if SelectLowerScreenObjects then
    begin
      // Start before the first selected TScreenObject at X,Y.
      Start := frmGoPhast.PhastModel.ScreenObjectCount - 1;
      for Index := frmGoPhast.PhastModel.ScreenObjectCount - 1 downto 0 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
        if (AScreenObject.ViewDirection = ViewDirection) then
        begin
          if AScreenObject.Select(X, Y) then
          begin
            if AScreenObject.Selected then
            begin
              Start := Index - 1;
              if CanChangeSelection and not ToggleSelectedItem then
              begin
                AScreenObject.Selected := False;
              end;
              if Start = -1 then
              begin
                Start := frmGoPhast.PhastModel.ScreenObjectCount - 1
              end;
            end
            else
            begin
              // break;
            end;
          end;
        end;
      end;
    end
    else
    begin
      // Start at the last TScreenObject.
      Start := frmGoPhast.PhastModel.ScreenObjectCount - 1;
    end;
  
    try
      for Index := Start downto 0 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
        if result then
        begin
          // generally speaking, the TScreenObject that
          // is already selected should be deselected.
          // if another TScreenObject is being selected.
          if CanChangeSelection and AScreenObject.Selected
            and not ToggleSelectedItem then
          begin
            AScreenObject.Selected := False;
            Update := True;
          end;
          if (AScreenObject.ViewDirection <> ViewDirection) then
          begin
            // Deselect TScreenObjects that are on a diffent view of the model.
            if CanChangeSelection and AScreenObject.Selected then
            begin
              AScreenObject.Selected := False;
              Update := True;
            end;
          end;
          continue;
        end;
        if (AScreenObject.ViewDirection <> ViewDirection) then
        begin
          // Deselect TScreenObjects that are on a diffent view of the model.
          if CanChangeSelection and AScreenObject.Selected then
          begin
            AScreenObject.Selected := False;
            Update := True;
          end;
        end
        else
        begin
          if AScreenObject.Select(X, Y) then
          begin
            if not CanChangeSelection then
            begin
              if ReturnScreenObjectPresent then
              begin
                result := True;
              end
              else
              begin
                result := AScreenObject.Selected;
              end;
              if result then
              begin
                UndoChangeSelection.Free;
                Exit;
              end
              else
              begin
                Continue;
              end;
            end;
            if ToggleSelectedItem then
            begin
              AScreenObject.Selected := not AScreenObject.Selected;
              Update := True;
              if AScreenObject.Selected then
              begin
                result := True;
              end;
            end
            else
            begin
              if not AScreenObject.Selected then
              begin
                AScreenObject.Selected := True;
                Update := True;
              end;
              result := True;
            end;
          end
          else
          begin
            if not CanChangeSelection then
            begin
              Continue;
            end;
            if not ToggleSelectedItem then
            begin
              if AScreenObject.Selected then
              begin
                AScreenObject.Selected := False;
                Update := True;
              end;
            end;
          end;
        end;
      end;
    finally
      View.UpdateSelectRectangle;
      if Update then
      begin
        //        ContoursChanged := True;
      end;
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
  except
    UndoChangeSelection.Free;
    raise;
  end;
end;
{$ENDREGION}

{$REGION 'TDeleteSegmentTool'}
{ TDeleteSegmentTool }
  
procedure TDeleteSegmentTool.SetCursorAtLocation(const X, Y: Integer);
var
  Edge: integer;
begin
  // The user wants to delete on segment of an object.  Check if the
  // mouse is over the edge of an object.
  Edge := GetEdge(X,Y);
  
  // set the Cursor based on whether or not the mouse is over an
  // edge of an object.
  if Edge >= 0 then
  begin
    Cursor := crDeleteSegment;
  end
  else
  begin
    Cursor := crDisabledDeleteSegment;
  end;
end;
  
procedure TDeleteSegmentTool.DoEdit(const X, Y: integer);
var
  Index: integer;
  AScreenObject: TScreenObject;
  Edge: integer;
  UndoDeleteSegment: TUndoDeleteSegment;
begin
  // This procedure allows the user to delete one edge of a selected object.
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Selected and (AScreenObject.ViewDirection = ViewDirection)
      then
    begin
      Edge := AScreenObject.SelectEdge(X, Y);
      if Edge >= 0 then
      begin
        UndoDeleteSegment := TUndoDeleteSegment.Create(AScreenObject, Edge);
        frmGoPhast.UndoStack.Submit(UndoDeleteSegment);
        UndoDeleteSegment.SetPostSelection;
        Exit;
      end;
    end;
  end;
end;
  
function TDeleteSegmentTool.GetHint: string;
begin
  result := 'Click to delete segment';
end;
{$ENDREGION}

{$REGION 'TInsertPointTool'}
{ TInsertPointTool }
  
procedure TInsertPointTool.DoEdit(const X, Y: integer);
var
  Index: integer;
  AScreenObject: TScreenObject;
  Edge: integer;
  APoint: TPoint2D;
  UndoInsertPoint: TUndoInsertPoint;
begin
  // This procedure allows the user to insert a node into a selected object.
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Selected and (AScreenObject.ViewDirection = ViewDirection)
      then
    begin
      Edge := AScreenObject.SelectEdge(X, Y);
      if Edge >= 0 then
      begin
        APoint.X := ZoomBox.X(X);
        APoint.Y := ZoomBox.Y(Y);
        UndoInsertPoint := TUndoInsertPoint.Create(AScreenObject, Edge + 1,
          APoint);
        frmGoPhast.UndoStack.Submit(UndoInsertPoint);
        UndoInsertPoint.SetPostSelection;
        Exit;
      end;
    end;
  end;
end;
  
function TInsertPointTool.GetHint: string;
begin
  result := 'Click on a segment to create a new node there.';
end;
  
procedure TInsertPointTool.SetCursorAtLocation(const X, Y: Integer);
var
  Edge: integer;
begin
  // The user wants to insert a point.  Check if the mouse is over the
  // edge of a selected object.
  Edge := GetEdge(X,Y);
  // set the Cursor based on whether or not the mouse is over an
  // edge of a selected object.
  if Edge >= 0 then
  begin
    Cursor := crInsertPoint;
  end
  else
  begin
    Cursor := crDisabledInsertPoint;
  end;
end;
{$ENDREGION}

{$REGION 'TSelectPointTool'}
{ TSelectPointTool }
  
procedure TSelectPointTool.Activate;
begin
  inherited;
  CreateLayers;
end;
  
procedure TSelectPointTool.DrawOnBitMap32(Sender: TObject; Buffer: TBitmap32);
begin
  inherited;
  if frmGoPhast.CurrentTool <> self then Exit;
  Layer32.BringToFront;
  Buffer.BeginUpdate;
  try
    ShowMovedPoints(Buffer);
  finally
    Buffer.EndUpdate;
  end;
end;
  
procedure TSelectPointTool.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ScreenObjectAlreadySelected: boolean;
  NodeToSelect: integer;
  SelectedScreenObject: TScreenObject;
begin
  inherited;
  // The user wants to select a node on a screen object.
  // See if the user clicked on a node.
  ScreenObjectAlreadySelected := AreScreenObjectsSelected;
  FMovingScreenObjects := ScreenObjectAlreadySelected or SelectScreenObjects
    (X, Y, (ssCtrl in Shift), (ssShift in Shift), False, True);
  if FMovingScreenObjects then
  begin
    if not ScreenObjectAlreadySelected then
    begin
      SelectScreenObjects(X, Y, (ssCtrl in Shift), (ssShift in Shift), True,
        True);
    end;
  
    SelectedScreenObject := FindSelectedScreenObject(X, Y);
    CurrentScreenObject := SelectedScreenObject;
    ClearPoints;

    if SelectedScreenObject = nil then
    begin
      FMovingScreenObjects := False;
      NodeToSelect := -1;
    end
    else
    begin
      NodeToSelect := FindNodeInSelectedScreenObjects(X, Y,
        SelectedScreenObject);
  
      FMovingScreenObjects := (NodeToSelect >= 0) and
        (SelectedScreenObject.SelectedVertices[NodeToSelect] or (ssShift in
          Shift));
    end;
  
    if FMovingScreenObjects then
    begin
      FPointIsSelected := True;
    end
    else
    begin
      FPointIsSelected := (NodeToSelect >= 0); //False;
    end;
  end;
  // define a box that will outline the nodes to be selected.
  FSelectLine.Free;
  FSelectLine := TLine.Create(5);
  FSelectLine.AddPoint(Point(X, Y));
end;
  
procedure TSelectPointTool.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if (FCurrentScreenObject = nil)
    and (FViewDirection <> ViewDirection)
    and (FStoredPoints <> nil) then
  begin
    ClearPoints
  end;
  FViewDirection := ViewDirection;
  FShift := Shift;
  // If something is happening, redraw.
  if (FSelectLine <> nil) and not FMovingScreenObjects then
  begin
    ZoomBox.Image32.Invalidate;
  end
  else if (FCurrentScreenObject <> nil)
    and FCurrentScreenObject.Selected and FMovingScreenObjects
    and (ssLeft in Shift) then
  begin
    ZoomBox.Image32.Invalidate;
  end;
  UpdateCursors;
end;
  
procedure TSelectPointTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  APoint: TPoint;
begin
  inherited;
  if (FSelectLine <> nil) then
  begin
    // Don't do anything if the mouse hasn't moved much.
    if ((Abs(X - FStartX) > SelectionWidth)
      or (Abs(Y - FStartY) > SelectionWidth)) then
    begin
      // if you are moving nodes, move them
      if FMovingScreenObjects then
      begin
        Assert(FCurrentScreenObject <> nil);
        StorePointsOfOtherObjects(FCurrentScreenObject);
        MoveScreenObjects(X, Y, Shift);
      end
      else
      begin
        // otherwise select screen objects with a rectangle.
        // finish SelectLine
        APoint := FSelectLine.Points[0];
        FSelectLine.AddPoint(Point(APoint.X, Y));
        FSelectLine.AddPoint(Point(X, Y));
        FSelectLine.AddPoint(Point(X, APoint.Y));
        FSelectLine.AddPoint(APoint);
        // Select screen objects with SelectLine.
        SelectPointsOfAllSelectedScreenObjectsWithLine(ssShift in Shift);
      end;
    end
    else
    begin
      if FPointIsSelected then
      begin
        SelectPointsOfASelectedScreenObject(X, Y,
          (ssShift in Shift));
      end;
    end;
    // Get rid of SelectLine
    FreeAndNil(FSelectLine);
    // redraw
    ZoomBox.Image32.Invalidate;
  end;
end;
  
procedure TSelectPointTool.ShowMovedPoints(const BitMap: TBitmap32);
var
  PointIndex: integer;
  PointArray: array of TPoint;
  IsSegment: boolean;
  Function IsStartPoint(Out IsSegment: boolean): boolean;
  var
    SectionIndex: integer;
  begin
    result := False;
    IsSegment := False;
    for SectionIndex := 0 to FCurrentScreenObject.SectionCount - 1 do
    begin
      if PointIndex = FCurrentScreenObject.SectionStart[SectionIndex] then
      begin
        result := true;
        IsSegment := FCurrentScreenObject.SectionLength[SectionIndex] > 1;
        break;
      end
    end;
  end;
  Function IsEndPoint(Out IsSegment: boolean): boolean;
  var
    SectionIndex: integer;
  begin
    result := False;
    IsSegment := False;
    for SectionIndex := 0 to FCurrentScreenObject.SectionCount - 1 do
    begin
      if PointIndex = FCurrentScreenObject.SectionEnd[SectionIndex] then
      begin
        result := true;
        IsSegment := FCurrentScreenObject.SectionLength[SectionIndex] > 1;
        break;
      end;
    end;
  end;
begin
  if FMouseButtonIsDown and FMovingScreenObjects
    and frmGoPhast.tbSelectPoint.Down
    and (FCurrentScreenObject <> nil)
    and FCurrentScreenObject.Selected then
  begin
    // show the new locations of the selected points.
    for PointIndex := 0 to FCurrentScreenObject.Count - 1 do
    begin
      if FCurrentScreenObject.SelectedVertices[PointIndex] then
      begin
        if FCurrentScreenObject.Count = 1 then
        begin
          DrawBigRectangle32(BitMap, clBlack32, clTransparent32, 1,
            frmGoPhast.CursorX - 3,
            frmGoPhast.CursorY - 3,
            frmGoPhast.CursorX + 4,
            frmGoPhast.CursorY + 4);
        end
        else
        begin
//          if PointIndex = 0 then
          if IsStartPoint(IsSegment) then
          begin
            if IsSegment then
            begin
              SetLength(PointArray, 2);
            end
            else
            begin
              SetLength(PointArray, 1);
            end;
//            SetLength(PointArray, 2);
            PointArray[0] := FCurrentScreenObject.CanvasCoordinates[PointIndex];
            PointArray[0].X := PointArray[0].X
              + frmGoPhast.CursorX - FStartX;
            PointArray[0].Y := PointArray[0].Y
              + frmGoPhast.CursorY - FStartY;

            if IsSegment then
            begin

              PointArray[1] := FCurrentScreenObject.CanvasCoordinates[PointIndex+1];
              if FCurrentScreenObject.SelectedVertices[PointIndex+1] then
              begin
                PointArray[1].X := PointArray[1].X
                  + frmGoPhast.CursorX - FStartX;
                PointArray[1].Y := PointArray[1].Y
                  + frmGoPhast.CursorY - FStartY;
              end;
            end;
          end
//          else if PointIndex = FSelectedPointScreenObject.Count - 1 then
          else if IsEndPoint(IsSegment) then
          begin
            if IsSegment then
            begin
              SetLength(PointArray, 2);
            end
            else
            begin
              SetLength(PointArray, 1);
            end;
//            SetLength(PointArray, 2);
              PointArray[0] := FCurrentScreenObject.
                CanvasCoordinates[PointIndex];
              PointArray[0].X := PointArray[0].X
                + frmGoPhast.CursorX - FStartX;
              PointArray[0].Y := PointArray[0].Y
                + frmGoPhast.CursorY - FStartY;

            if IsSegment then
            begin
              PointArray[1] := FCurrentScreenObject.
                CanvasCoordinates[PointIndex - 1];
              if FCurrentScreenObject.SelectedVertices[PointIndex - 1] then
              begin
                PointArray[1].X := PointArray[1].X
                  + frmGoPhast.CursorX - FStartX;
                PointArray[1].Y := PointArray[1].Y
                  + frmGoPhast.CursorY - FStartY;
              end;
            end;
          end
          else
          begin
            SetLength(PointArray, 3);
            PointArray[0] := FCurrentScreenObject.
              CanvasCoordinates[PointIndex - 1];
            if FCurrentScreenObject.SelectedVertices[PointIndex - 1] then
            begin
              PointArray[0].X := PointArray[0].X
                + frmGoPhast.CursorX - FStartX;
              PointArray[0].Y := PointArray[0].Y
                + frmGoPhast.CursorY - FStartY;
            end;

            PointArray[1] := FCurrentScreenObject.
              CanvasCoordinates[PointIndex];
            PointArray[1].X := PointArray[1].X
              + frmGoPhast.CursorX - FStartX;
            PointArray[1].Y := PointArray[1].Y
              + frmGoPhast.CursorY - FStartY;

            PointArray[2] := FCurrentScreenObject.
              CanvasCoordinates[PointIndex + 1];
            if FCurrentScreenObject.SelectedVertices[PointIndex + 1] then
            begin
              PointArray[2].X := PointArray[2].X
                + frmGoPhast.CursorX - FStartX;
              PointArray[2].Y := PointArray[2].Y
                + frmGoPhast.CursorY - FStartY;
            end;
          end;
  
          DrawBigPolyline32(BitMap, clBlack32, 1, PointArray, True);
        end;
      end;
    end;
  end
  else
  begin
    DrawSelectionRectangle32(BitMap);
  end;
end;
  
function TSelectPointTool.FindSelectedScreenObject(const X, Y: integer):
  TScreenObject;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  result := nil;
  for Index := frmGoPhast.PhastModel.ScreenObjectCount - 1 downto 0 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Selected and (AScreenObject.ViewDirection = ViewDirection)
      then
    begin
      if FindNodeInSelectedScreenObjects(X, Y, AScreenObject) >= 0 then
      begin
        result := AScreenObject;
        Exit;
      end;
    end;
  end;
end;
  
function TSelectPointTool.SelectPointsOfASelectedScreenObject(
  const X, Y: integer; const AddToSelection: boolean): boolean;
var
  Index: integer;
  AScreenObject, SelectedScreenObject: TScreenObject;
  UndoChangeSelection: TUndoChangeSelection;
  SelectedPoint: integer;
  PointIndex: integer;
  SectionIndex: Integer;
begin
  // This procedure selects the node of a selected screen object
  // that is at or near (X,Y).
  UndoChangeSelection := TUndoChangeSelection.Create;
  try
    result := False;
    SelectedScreenObject := nil;
    try
      for Index := frmGoPhast.PhastModel.ScreenObjectCount - 1 downto 0 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
  
        if (AScreenObject.ViewDirection = ViewDirection) and
          AScreenObject.Selected then
        begin
          SelectedPoint := FindNodeInSelectedScreenObjects(X, Y, AScreenObject);
          if SelectedPoint >= 0 then
          begin
            result := True;
            SelectedScreenObject := AScreenObject;
            FCurrentScreenObject := AScreenObject;
            ClearPoints;
            if AddToSelection then
            begin
              SelectedScreenObject.SelectedVertices[SelectedPoint] :=
                not SelectedScreenObject.SelectedVertices[SelectedPoint];
            end
            else
            begin
              for PointIndex := 0 to SelectedScreenObject.Count - 1 do
              begin
                SelectedScreenObject.SelectedVertices[PointIndex] :=
                  PointIndex = SelectedPoint;
              end;
            end;
            if SelectedScreenObject.Closed then
            begin
              for SectionIndex := 0 to SelectedScreenObject.SectionCount - 1 do
              begin
                if SelectedScreenObject.SectionClosed[SectionIndex] and
                  (SelectedPoint =
                  SelectedScreenObject.SectionStart[SectionIndex]) then
                begin
                  SelectedScreenObject.SelectedVertices[
                    SelectedScreenObject.SectionEnd[SectionIndex]] :=
                    SelectedScreenObject.SelectedVertices[
                    SelectedScreenObject.SectionStart[SectionIndex]];
                end;
              end;
            end;
            break;
          end;
        end;
      end;
    finally
      if result then
      begin
        for Index := frmGoPhast.PhastModel.ScreenObjectCount - 1 downto 0 do
        begin
          AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
          AScreenObject.Selected := AScreenObject = SelectedScreenObject;
        end;
      end;
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
  except
    UndoChangeSelection.Free;
    raise;
  end;
end;
  
function TSelectPointTool.SelectPointsOfAllSelectedScreenObjectsWithLine
  (const AddToSelection: boolean): boolean;
var
  Index: integer;
  AScreenObject: TScreenObject;
  Update: boolean;
  Changed: boolean;
  UndoChangeSelection: TUndoChangeSelection;
begin
  // This procedure uses a "lasso" to select nodes in one selected object.
  // All other selected objects are deselected.
  UndoChangeSelection := TUndoChangeSelection.Create;
  try
    result := False;
    Update := False;
    try
      for Index := frmGoPhast.PhastModel.ScreenObjectCount - 1 downto 0 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
        if result then
        begin
          if AScreenObject.Selected then
          begin
            AScreenObject.Selected := False;
            Update := True;
          end;
          Continue;
        end;
        if (AScreenObject.ViewDirection = ViewDirection) and
          AScreenObject.Selected then
        begin
          Changed := False;
          if SelectPointsWithLine(AScreenObject, AddToSelection, Changed) then
          begin
            result := True;
            FCurrentScreenObject := AScreenObject;
            ClearPoints;
          end
          else
          begin
            AScreenObject.Selected := false;
            Update := True;
          end;
          if Changed then
          begin
            Update := True;
          end;
        end
        else if AScreenObject.Selected then
        begin
          AScreenObject.Selected := False;
          Update := True;
        end;
      end;
    finally
      if Update then
      begin
        View.ScreenObjectsHaveChanged := True;
      end;
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
  except
    UndoChangeSelection.Free;
    raise;
  end;
end;
  
function TSelectPointTool.SelectPointsWithLine(
  const AScreenObject: TScreenObject;
  const AddToSelection: boolean; out Changed: boolean): boolean;
var
  PointIndex: integer;
  APoint: TPoint;
begin
  // This procedure selects nodes that are inside SelectLine.
  // if AddToSelection is not True, it deselects nodes that are not
  // inside SelectLine.  Changed is set to True if any nodes have been
  // changed from unselected to selected or vice versa.
  Changed := False;
  result := False;
  for PointIndex := 0 to AScreenObject.Count - 1 do
  begin
    APoint := AScreenObject.CanvasCoordinates[PointIndex];
    if FSelectLine.Inside(APoint) then
    begin
      result := True;
      if AddToSelection then
      begin
        AScreenObject.SelectedVertices[PointIndex] :=
          not AScreenObject.SelectedVertices[PointIndex];
        Changed := True;
      end
      else
      begin
        if not AScreenObject.SelectedVertices[PointIndex] then
        begin
          AScreenObject.SelectedVertices[PointIndex] := True;
          Changed := True;
        end;
      end;
    end
    else
    begin
      if not AddToSelection then
      begin
        if AScreenObject.SelectedVertices[PointIndex] then
        begin
          Changed := True;
          AScreenObject.SelectedVertices[PointIndex] := False;
        end;
      end;
    end;
  end;
end;
  
function TSelectPointTool.FindNodeInSelectedScreenObjects(const X, Y: integer;
  const AScreenObject: TScreenObject): integer;
var
  PointIndex: integer;
  APoint: TPoint;
begin
  result := -1;
  Assert(AScreenObject.Selected);
  for PointIndex := 0 to AScreenObject.Count - 1 do
  begin
    APoint := AScreenObject.CanvasCoordinates[PointIndex];
    if (Abs(X - APoint.X) < SelectionWidth)
      and (Abs(Y - APoint.Y) < SelectionWidth) then
    begin
      result := PointIndex;
      Exit;
    end;
  end;
end;
  
function TSelectPointTool.GetCursor: TCursor;
var
  APoint: TPoint2D;
begin
  StorePointsOfOtherObjects(FCurrentScreenObject);


  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(
    Point(frmGoPhast.CursorX, frmGoPhast.CursorY), APoint) then
  begin
    result := crSnapSelectPoint;
  end
  else
  begin
    result := crSelectPoint;
  end;
end;

function TSelectPointTool.GetHint: string;
begin
  result := 'Click on a vertex to select it';
end;

procedure TSelectPointTool.GetOffset(const APoint: TPoint2D; out XOffset,
  YOffset: real);
var
  StartPoint: TPoint2D;
  ClosestLocation: TPoint2D;
  PointDistance: real;
begin
  StartPoint.x := ZoomBox.X(FStartX);
  StartPoint.Y := ZoomBox.Y(FStartY);

  ClosestLocation := FCurrentScreenObject.Points[0];
  PointDistance := Distance(ClosestLocation, StartPoint);
  FCurrentScreenObject.IsAnyPointCloser(StartPoint, PointDistance, ClosestLocation, 1);

  XOffset := APoint.X - ClosestLocation.X;
  YOffset := APoint.Y - ClosestLocation.Y;
end;

{$ENDREGION}

{$REGION 'TSelectScreenObjectTool'}
{ TSelectScreenObjectTool }
  
procedure TSelectScreenObjectTool.DoubleClick(Sender: TObject);
begin
  inherited;
  FDoubleClicked := True;
end;
  
procedure TSelectScreenObjectTool.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FShouldDrawSelectionRectangle := True;
  // The user wants to select screen objects.
  if ssDouble in Shift then
  begin
    FMovingScreenObjects := False;
    // Don't change the selected screen objects when double-clicking.
    Exit;
  end;
  // The user wants to select screen objects and may want to move them too.
  FMovingScreenObjects := SelectScreenObjects
    (X, Y, (ssCtrl in Shift), (ssShift in Shift),
    not (ssShift in Shift) and ((ssCtrl in Shift) or not
    AreScreenObjectsSelected));
  if not FMovingScreenObjects then
  begin
    // if the user didn't click on anything, the user must want to
    // draq a box around the objects to be selected.
  
    // SelectLine is used to define the box.  It will have 5 nodes because
    // it is rectangular and the first node will be at the same position as
    // the last one.
    FSelectLine.Free;
    FSelectLine := TLine.Create(5);
    FSelectLine.AddPoint(Point(X, Y));
    ZoomBox.Image32.Invalidate;
  end;
end;
  
procedure TSelectScreenObjectTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  // Set the correct cursor.
  Cursor := crArrow;
  // If something is happening, redraw.
  if (ssLeft in Shift) or FMovingScreenObjects
    or (FSelectLine <> nil) then
  begin
    ZoomBox.Image32.Invalidate;
  end;
end;
  
procedure TSelectScreenObjectTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Val1: double;
  Val2: double;
begin
  inherited;
  if not FDoubleClicked then
  begin
    if ((Abs(X - FStartX) > SelectionWidth)
      or (Abs(Y - FStartY) > SelectionWidth)) then
    begin
      // Don't do anything if the mouse hasn't moved much.
      if FMovingScreenObjects then
      begin
        // If you are moving screen objects, move them.
        MoveScreenObjects(X, Y, Shift);
      end
      else
      begin
        // Otherwise select screen objects with a rectangle.
        // finish SelectLine
        if FSelectLine <> nil then
        begin
          Val1 := ZoomBox.X(FStartX);
          Val2 := ZoomBox.X(X);
          FMaxX := Max(Val1, Val2);
          FMinX := Min(Val1, Val2);

          Val1 := ZoomBox.Y(FStartY);
          Val2 := ZoomBox.Y(Y);
          FMaxY := Max(Val1, Val2);
          FMinY := Min(Val1, Val2);


          // Select screen objects with a rectangle.
          SelectScreenObjectsInGui(ssShift in Shift);
          // redraw
          ZoomBox.Image32.Invalidate;
        end;
  
      end;
    end
    else if not FMovingScreenObjects or (ssShift in Shift) then
    begin
      SelectScreenObjects(X, Y, (ssCtrl in Shift), (ssShift in Shift),
        True);
    end;
  end;
  // Get rid of SelectLine
  FreeAndNil(FSelectLine);
  
  if FDoubleClicked then
  begin
    // Edit the object properties.
    try
      frmGoPhast.EditScreenObjects;
    finally
      FDoubleClicked := False;
    end;

  end;
end;

function TSelectScreenObjectTool.ScreenObjectInside(AScreenObject: TScreenObject): boolean;
begin
  // If at least one point of the screen object is inside SelectLine
  // and SelectLine does not intersect SelectLine then
  // the entire object is inside SelectLine.
  result := false;
  if AScreenObject.Count > 0 then
  begin
    result := (AScreenObject.MaxX <= FMaxX)
      and (AScreenObject.MinX >= FMinX)
      and (AScreenObject.MaxY <= FMaxY)
      and (AScreenObject.MinY >= FMinY);
  end;
end;
  
procedure TSelectScreenObjectTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
var
  TL, BR: TPoint;
  BitMap: TBitmap32;
begin
  if (FSelectLine = nil) and FShouldDrawSelectionRectangle
    and (frmGoPhast.CurrentTool = self)
    and (Sender = Layer32)
    and (frmGoPhast.CursorGrid = View.CursorGrid) then
  begin
    Layer32.BringToFront;
    Buffer.BeginUpdate;
    try
      try
        // draw a rectangle around the selected screen objects.
        TL := View.FSelectTopLeft;
        BR := View.FSelectBottomRight;
        if FMovingScreenObjects then
        begin
          TL.X := TL.X + frmGoPhast.CursorX - FStartX;
          BR.X := BR.X + frmGoPhast.CursorX - FStartX;
          TL.Y := TL.Y + frmGoPhast.CursorY - FStartY;
          BR.Y := BR.Y + frmGoPhast.CursorY - FStartY;
        end;
  
        BitMap := TBitmap32.Create;
        try
          BitMap.Width := BR.X - TL.X + 1;
          BitMap.Height := BR.Y - TL.Y + 1;
          BitMap.DrawMode := dmBlend;
          BitMap.SetStipple([0, 0, 0, 0, 0,
            clBlack32, clBlack32, clBlack32, clBlack32, clBlack32]);
          BitMap.MoveTo(0,0);
          BitMap.LineToFSP(0,BitMap.Height-1);
          BitMap.LineToFSP(BitMap.Width-1,BitMap.Height-1);
          BitMap.LineToFSP(BitMap.Width-1,0);
          BitMap.LineToFSP(0,0);
          Buffer.Draw(TL.X, TL.Y, BitMap);
        finally
          BitMap.Free;
        end;
      except
        ZoomBox.ZoomBy(0.5);
        with View do
        begin
          AdjustScales;
          ShowMagnification;
          MagnificationChanged := True;
          UpdateSelectRectangle;
        end;
      end;
    finally
      Buffer.EndUpdate;
    end;
  end;
  DrawSelectionRectangle32(Buffer);
end;
  
procedure TSelectScreenObjectTool.Activate;
begin
  inherited;
  CreateLayers;
  FShouldDrawSelectionRectangle := True;
end;
  
function TSelectScreenObjectTool.GetHint: string;
begin
  result := 'Click on object to select it or click and drag';
end;
{$ENDREGION}

{$REGION 'TCustomSelectScreenObjectTool'}
{ TCustomSelectScreenObjectTool }
procedure TCustomSelectScreenObjectTool.DrawSelectionRectangle32(
  BitMap: TBitmap32);
var
  TopLeft: TPoint;
begin
  if (FSelectLine <> nil) and (frmGoPhast.CurrentTool = self) and
    (frmGoPhast.CursorGrid = View.CursorGrid) then
  begin
    if FSelectLine.Count = 1 then
    begin
      Layer32.BringToFront;
      BitMap.BeginUpdate;
      try
        TopLeft := FSelectLine.Points[0];
        DrawBigRectangle32(BitMap, clBlack32, clTransparent32, 1,
          TopLeft.X, TopLeft.Y, frmGoPhast.CursorX, frmGoPhast.CursorY, True);
      finally
        BitMap.EndUpdate;
      end;
    end;
  end;
end;

//destructor TCustomSelectScreenObjectTool.Destroy;
//begin
//  FSelectLine.Free;
//  inherited;
//end;

procedure TCustomSelectScreenObjectTool.GetOffset(const APoint: TPoint2D;
  out XOffset, YOffset: real);
begin
  Assert(False);
end;

procedure TCustomSelectScreenObjectTool.MoveScreenObjects(const X, Y: integer;
  Shift: TShiftState);
var
  XOffset, YOffset: real;
  UndoMoveScreenObject: TUndoMoveScreenObject;
  APoint: TPoint2D;
begin
  // Move the screen objects a distance specified by where the mouse was clicked
  // down and where it was clicked up.
  if  (ssShift in Shift)
    and FindPointInNearbyScreenObject(Point(X, Y), APoint) then
  begin
    GetOffset(APoint, XOffset, YOffset);
  end
  else
  begin
    // Get the real-world coordinates of the mouse.
    APoint.X := ZoomBox.X(X);
    APoint.Y := ZoomBox.Y(Y);
    XOffset := APoint.X - ZoomBox.X(FStartX);
    YOffset := APoint.Y - ZoomBox.Y(FStartY);
  end;

  if (XOffset <> 0) or (YOffset <> 0) then
  begin
    // if the cursor has moved, move the selected screen objects.
    UndoMoveScreenObject := TUndoMoveScreenObject.Create(XOffset, YOffset,
      ViewDirection);
    frmGoPhast.UndoStack.Submit(UndoMoveScreenObject);
    UndoMoveScreenObject.SetPostSelection;
    FStartX := X;
    FStartY := Y;
    FMovingScreenObjects := False;
  end;
end;
  
procedure TCustomSelectScreenObjectTool.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FStartX := X;
  FStartY := Y;
  FMouseButtonIsDown := True;
end;
  
procedure TCustomSelectScreenObjectTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseButtonIsDown := False;
end;
  
procedure TCustomSelectScreenObjectTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMovingScreenObjects := FMovingScreenObjects and (ssLeft in Shift);
end;
  
{$ENDREGION}

{$REGION 'TCustomModifyGeometryTool'}
{ TCustomModifyGeometryTool }
  
procedure TCustomModifyGeometryTool.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  // The user wants to insert a node in an existing object.
  // You need to select an object.
  if not AreScreenObjectsSelected then
  begin
    SelectScreenObjects(X, Y, (ssCtrl in Shift), (ssShift in Shift));
  end;
end;
  
procedure TCustomModifyGeometryTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  // The user wants to insert a point.
  // Check if the mouse is over the
  // edge of a selected object and change the cursor appropriately.
  SetCursorAtLocation(X, Y);
end;
  
procedure TCustomModifyGeometryTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  // The user wants to insert a point
  if not AreScreenObjectsSelected then
  begin
    SelectScreenObjects(X, Y, (ssCtrl in Shift), (ssShift in Shift));
  end;
  DoEdit(X, Y);
end;
  
function TCustomModifyGeometryTool.GetEdge(const X, Y: integer): integer;
var
  Index: integer;
  AScreenObject: TScreenObject;
  SelectedObjects: boolean;
begin
  // The user wants to insert a point.  Check if the mouse is over the
  // edge of a selected object.
  result := -1;
  SelectedObjects := AreScreenObjectsSelected;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if (AScreenObject.Selected or not SelectedObjects)
      and (AScreenObject.ViewDirection = ViewDirection) then
    begin
      result := AScreenObject.SelectEdge(X, Y);
      if result >= 0 then
      begin
        Exit;
      end;
    end
  end;
end;
  
{$ENDREGION}

{$REGION 'TColRowLayerSelectorTool'}
{ TColRowLayerSelectorTool }

procedure TColRowLayerSelectorTool.DrawExistingColRowLayerSelection(
  const Direction: TViewDirection; const BitMap: TBitmap32);
var
  Color1, Color2: TColor;
begin
  Color1 := clBlack;
  Color2 := clBlack;
  case Direction of
    vdTop:
      begin
        Color1 := ExistingColumnSelectionCellColor;
        Color2 := ExistingRowSelectionCellColor;
      end;
    vdFront:
      begin
        Color1 := ExistingColumnSelectionCellColor;
        Color2 := ExistingLayerSelectionCellColor;
      end;
    vdSide: 
      begin
        Color1 := ExistingRowSelectionCellColor;
        Color2 := ExistingLayerSelectionCellColor;
      end;
    else Assert(False);
  end;
  DrawASelection(frmGoPhast.Grid.SelectedColumn, frmGoPhast.Grid.SelectedRow,
    frmGoPhast.Grid.SelectedLayer,
    Color1, Color2, BitMap, Direction);
end;

procedure TColRowLayerSelectorTool.DrawNewColRowLayerSelection(
  const Direction: TViewDirection; const BitMap: TBitmap32);
begin
  DrawASelection(NewColumn, NewRow, NewLayer,
    SelectedCellsColor, SelectedCellsColor, BitMap, Direction);
end;

procedure TColRowLayerSelectorTool.DrawOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
begin
  if frmGoPhast.Grid = nil then
  begin
    Exit;
  end;
  
  if (frmGoPhast.Grid.ColumnCount <= 0)
    or (frmGoPhast.Grid.RowCount <= 0)
    or (frmGoPhast.Grid.LayerCount <= 0) then
  begin
    Exit
  end;

  if (frmGoPhast.CurrentTool = self)
    and ((Sender = FTopLayer) or (Sender = FFrontLayer)
    or (Sender = FSideLayer))  then
  begin
    if Sender = FTopLayer then
    begin
      DrawExistingColRowLayerSelection(vdTop, Buffer);
    end
    else if Sender = FFrontLayer then
    begin
      DrawExistingColRowLayerSelection(vdFront, Buffer);
    end
    else if Sender = FSideLayer then
    begin
      DrawExistingColRowLayerSelection(vdSide, Buffer);
    end;
  end;
  if FShouldDraw and (frmGoPhast.CurrentTool = self)
    and (Sender = Layer32) 
    and (frmGoPhast.CursorGrid = View.CursorGrid) then
  begin
    DrawNewColRowLayerSelection(View.ViewDirection, Buffer);
  end;
end;

procedure TColRowLayerSelectorTool.GetCellUnderMouse(X, Y: integer);
var
  APoint: TPoint2D;
  Column, Row, Layer: integer;
begin
  // This procedure is used to update the view on the screen when
  // setting the spacing of rows, columns or layers.

  // Get the row and column number of the cell underneath
  // the cursor.
  APoint.X := ZoomBox.X(X);
  APoint.Y := ZoomBox.Y(Y);
  case ViewDirection of
    vdTop:
      begin
        GetRowCol(APoint, Row, Column);
        if (Row >= 0) and (Column >= 0) then
        begin
          FNewColumn := Column;
          FNewRow := Row;
          FShouldDraw := True;
          ZoomBox.Image32.Invalidate;
        end
        else
        begin
          FShouldDraw := False;
        end;
      end;
    vdFront:
      begin
        GetColLayer(APoint, Column, Layer);
        if (Column >= 0) and (Layer >= 0) then
        begin
          FNewLayer := Layer;
          FNewColumn := Column;
          FShouldDraw := True;
          ZoomBox.Image32.Invalidate;
        end
        else
        begin
          FShouldDraw := False;
        end;
      end;
    vdSide:
      begin
        GetRowLayer(APoint, Row, Layer);
        if (Row >= 0) and (Layer >= 0) then
        begin
          FNewLayer := Layer;
          FNewRow := Row;
          FShouldDraw := True;
          ZoomBox.Image32.Invalidate;
        end
        else
        begin
          FShouldDraw := False;
        end;
      end;
  else
    Assert(False);
  end;
end;

function TColRowLayerSelectorTool.GetHint: string;
begin
  result := 'Click to change selected column, row, and layer.';
end;

procedure TColRowLayerSelectorTool.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  GetCellUnderMouse(X,Y)
end;

procedure TColRowLayerSelectorTool.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GetCellUnderMouse(X,Y);
  SetNewSelection;
  inherited;
end;

procedure TColRowLayerSelectorTool.Activate;
begin
  inherited;
  FTopLayer.Changed;
  FSideLayer.Changed;
  FFrontLayer.Changed;
  frmGoPhast.frameTopView.ZoomBox.Image32.Invalidate;
  frmGoPhast.frameFrontView.ZoomBox.Image32.Invalidate;
  frmGoPhast.frameSideView.ZoomBox.Image32.Invalidate;
end;

procedure TColRowLayerSelectorTool.DrawASelection(Col, Row, Lay: Integer;
  Color1, Color2: TColor; const BitMap: TBitmap32;
  const Direction: TViewDirection);
var
  EvaluatedAt: TEvaluatedAt;
  Limit2: Integer;
  Limit1: Integer;
begin
  EvaluatedAt := GetEvaluatedAt(Direction);
  frmGoPhast.Grid.GetLimits(EvaluatedAt, Direction, Limit1, Limit2);
  case Direction of
    vdTop:
      begin
        DrawSelectedTopCells(Col, Col, 0, Limit2, BitMap, Direction, Color1);
        DrawSelectedTopCells(0, Limit1, Row, Row, BitMap, Direction, Color2);
      end;
    vdFront:
      begin
        DrawSelectedFrontCells(Col, Col, 0, Limit2, BitMap, Direction, Color1);
        DrawSelectedFrontCells(0, Limit1, Lay, Lay, BitMap, Direction, Color2);
      end;
    vdSide:
      begin
        DrawSelectedSideCells(Row, Row, 0, Limit2, BitMap, Direction, Color1);
        DrawSelectedSideCells(0, Limit1, Lay, Lay, BitMap, Direction, Color2);
      end;
  else
    Assert(False);
  end;
end;

procedure TColRowLayerSelectorTool.SetNewSelection;
begin
  if not FShouldDraw then Exit;
  case ViewDirection of
    vdTop:
      begin
          frmGoPhast.Grid.SelectedColumn := NewColumn;
          frmGoPhast.Grid.SelectedRow := NewRow;
      end;
    vdFront:
      begin
          frmGoPhast.Grid.SelectedLayer := NewLayer;
          frmGoPhast.Grid.SelectedColumn := NewColumn;
      end;
    vdSide:
      begin
          frmGoPhast.Grid.SelectedLayer := NewLayer;
          frmGoPhast.Grid.SelectedRow := NewRow;
      end;
  else
    Assert(False);
  end;
end;

{$ENDREGION}

{ TAddPartTool }

procedure TAddLinePartTool.Activate;
begin
  inherited;
  FNewPart := True;
  FDoubleClicked := False;
end;

procedure TAddLinePartTool.DoubleClick(Sender: TObject);
begin
  inherited;
  FinishSection;
  SubmitUndo;
end;

procedure TAddLinePartTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  APoint: TPoint2D;
begin
  inherited;
  if FDoubleClicked then
  begin
    FDoubleClicked := False;
  end
  else
  begin
    if (FCurrentScreenObject <> nil)
      and (ViewDirection = FCurrentScreenObject.ViewDirection) then
    begin
      if ssShift in Shift then
      begin
        StorePointsOfOtherObjects(FCurrentScreenObject);
      end;
      if not (ssShift in Shift)
        or not FindPointInNearbyScreenObject(Point(X, Y), APoint) then
      begin
        // Get the real-world coordinates of the mouse.
        APoint.X := ZoomBox.X(X);
        APoint.Y := ZoomBox.Y(Y);
      end;
      FCurrentScreenObject.AddPoint(APoint, FNewPart);
      EnsureUndo;
      FUndoAddPart.AddPoint(APoint);
      FNewPart := False;
    end;
  end;
end;

procedure TAddLinePartTool.FinishSection;
begin
  FDoubleClicked := True;
  FNewPart := True;
end;

function TAddLinePartTool.GetCursor: TCursor;
var
  APoint: TPoint2D;
begin
  StorePointsOfOtherObjects(FCurrentScreenObject);

  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(
      Point(frmGoPhast.CursorX, frmGoPhast.CursorY), APoint) then
  begin
    result := crSnapMultiPartLine;
  end
  else
  begin
    result := crMultiPartLine;
  end;
end;

{ TAddClosedPartTool }

procedure TAddPolygonPartTool.FinishSection;
var
  APoint: TPoint2D;
begin
  inherited;
  if FDoubleClicked then
  begin
    if (FCurrentScreenObject <> nil)
      and (ViewDirection = FCurrentScreenObject.ViewDirection) then
    begin
      APoint := FCurrentScreenObject.Points[FCurrentScreenObject.
        SectionStart[FCurrentScreenObject.SectionCount -1]];
      FCurrentScreenObject.AddPoint(APoint, False);
      EnsureUndo;
      FUndoAddPart.AddPoint(APoint);
    end;
  end;
end;

{ TCustomAddPartTool }

procedure TCustomAddPartTool.Activate;
var
  AScreenObject: TScreenObject;
  Index: Integer;
begin
  inherited;
  FCurrentScreenObject := nil;
  ClearPoints;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Selected then
    begin
      FCurrentScreenObject := AScreenObject;
      break;
    end;
  end;
  Assert(FCurrentScreenObject <> nil);
  StorePointsOfOtherObjects(FCurrentScreenObject);
  FreeAndNil(FUndoAddPart);
end;

procedure TCustomAddPartTool.DeActivate;
begin
  inherited;
  SubmitUndo;
end;

destructor TCustomAddPartTool.Destroy;
begin
  FUndoAddPart.Free;
  inherited;
end;

procedure TCustomAddPartTool.EnsureUndo;
begin
  if FUndoAddPart = nil then
  begin
    FUndoAddPart := TUndoAddPart.Create(FCurrentScreenObject);
  end;
end;

procedure TCustomAddPartTool.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if (FCurrentScreenObject = nil)
    and (FViewDirection <> ViewDirection)
    and (FStoredPoints <> nil) then
  begin
    ClearPoints
  end;
  FViewDirection := ViewDirection;
  FShift := Shift;
  UpdateCursors;
end;

procedure TCustomAddPartTool.SubmitUndo;
begin
  try
    if FUndoAddPart <> nil then
    begin
      FUndoAddPart.SetPostSelection;
      frmGoPhast.UndoStack.Submit(FUndoAddPart);
      FUndoAddPart := nil;
    end;
  except
    FreeAndNil(FUndoAddPart);
    raise;
  end;
end;

{ TAddPointPartTool }

function TAddPointPartTool.GetCursor: TCursor;
var
  APoint: TPoint2D;
begin
  StorePointsOfOtherObjects(FCurrentScreenObject);

  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(
      Point(frmGoPhast.CursorX, frmGoPhast.CursorY), APoint) then
  begin
    result := crSnapMultiPartPoint;
  end
  else
  begin
    result := crMultiPartPoint;
  end;
end;

procedure TAddPointPartTool.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  APoint: TPoint2D;
begin
  inherited;
  if (FCurrentScreenObject <> nil)
    and (ViewDirection = FCurrentScreenObject.ViewDirection) then
  begin
    if ssShift in Shift then
    begin
      StorePointsOfOtherObjects(FCurrentScreenObject);
    end;
    if not (ssShift in Shift)
      or not FindPointInNearbyScreenObject(Point(X, Y), APoint) then
    begin
      // Get the real-world coordinates of the mouse.
      APoint.X := ZoomBox.X(X);
      APoint.Y := ZoomBox.Y(Y);
    end;
    FCurrentScreenObject.AddPoint(APoint, True);
    EnsureUndo;
    FUndoAddPart.AddPoint(APoint);
    SubmitUndo;
  end;
end;

{ TScreenObjectTool }

destructor TScreenObjectTool.Destroy;
begin
  FSelectLine.Free;
  inherited;
end;

function TScreenObjectTool.ScreenObjectInside(
  AScreenObject: TScreenObject): boolean;
begin
  result := False;
end;

{ TCustomStoreVerticesTool }

procedure TCustomStoreVerticesTool.Activate;
begin
  inherited;
  ClearPoints;
end;

procedure TCustomStoreVerticesTool.ClearPoints;
begin
  FVisibleVertices.Clear;
  FreeAndNil(FStoredPoints);
end;

constructor TCustomStoreVerticesTool.Create(AOwner: TComponent);
begin
  inherited;
  FVisibleVertices := TRbwQuadTree.Create(self);
  FStoredPoints := nil;
end;

destructor TCustomStoreVerticesTool.Destroy;
begin
  FStoredPoints.Free;
  inherited;
end;

function TCustomStoreVerticesTool.FindPointInNearbyScreenObject(const APoint: TPoint;
  out NearbyPoint: TPoint2D): boolean;
const
  SelectRange = 3;
var
  Block: T2DBlock;
  Points: TQuadPointInRegionArray;
  PointStorage: TScreenPointStorage;
  DeltaX: double;
  DeltaY: double;
  DistSqr: Extended;
  SelectIndex: Integer;
  Index: Integer;
  TestDistSqr: double;
begin
  result := False;
  if (FStoredPoints <> nil) and (FStoredPoints.Count > 0) then
  begin
    Block.XMin := APoint.X - SelectRange;
    Block.XMax := APoint.X + SelectRange;
    Block.YMin := APoint.Y - SelectRange;
    Block.YMax := APoint.Y + SelectRange;
    FVisibleVertices.FindPointsInBlock(Block, Points);
    if Length(Points) > 0 then
    begin
      if Length(Points) = 1 then
      begin
        SelectIndex := 0;
      end
      else
      begin
        DeltaX := Points[0].X - APoint.X;
        DeltaY := Points[0].Y - APoint.Y;
        DistSqr := Sqr(DeltaX) + Sqr(DeltaY);
        SelectIndex := 0;
        for Index := 1 to Length(Points) - 1 do
        begin
          DeltaX := Points[Index].X - APoint.X;
          DeltaY := Points[Index].Y - APoint.Y;
          TestDistSqr := Sqr(DeltaX) + Sqr(DeltaY);
          if TestDistSqr < DistSqr then
          begin
            DistSqr := TestDistSqr;
            SelectIndex:= Index;
          end;
        end;
      end;
      PointStorage := Points[SelectIndex].Data[0];
      NearbyPoint := PointStorage.FScreenObject.Points[
        PointStorage.FVertexIndex];
      result := True;
    end;
  end;
end;

function TCustomStoreVerticesTool.GetSnapPoint: TPoint;
var
  APoint: TPoint2D;
begin
  if (ssShift in FShift) then
  begin
    StorePointsOfOtherObjects(FCurrentScreenObject);
  end;
  result := Point(frmGoPhast.CursorX, frmGoPhast.CursorY);
  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(Point(result.X, result.Y), APoint) then
  begin
    // Get the real-world coordinates of the mouse.
    result.X := ZoomBox.XCoord(APoint.X);
    result.Y := ZoomBox.YCoord(APoint.Y);
  end;
end;

function TAddPolygonPartTool.GetCursor: TCursor;
var
  APoint: TPoint2D;
begin
  StorePointsOfOtherObjects(FCurrentScreenObject);

  if (ssShift in FShift)
    and FindPointInNearbyScreenObject(
      Point(frmGoPhast.CursorX, frmGoPhast.CursorY), APoint) then
  begin
    result := crSnapMultiPartPolygon;
  end
  else
  begin
    result := crMultiPartPolygon;
  end;
end;

initialization
  ZoomTool := TZoomTool.Create(nil);
  ZoomInTool := TZoomInTool.Create(nil);
  ZoomOutTool := TZoomOutTool.Create(nil);
  PanTool := TPanTool.Create(nil);
  AddGridBoundaryTool := TAddGridBoundaryTool.Create(nil);
  MovingGridBoundaryTool := TMovingGridBoundaryTool.Create(nil);
  DeleteGridBoundaryTool := TDeleteGridBoundaryTool.Create(nil);
  RotateGridTool := TRotateGridTool.Create(nil);
  LassoTool := TLassoTool.Create(nil);
  CreatePointScreenObjectTool := TCreatePointScreenObjectTool.Create(nil);
  CreateLineScreenObjectTool := TCreateLineScreenObjectTool.Create(nil);
  CreateStraightLineScreenObjectTool :=
    TCreateStraightLineScreenObjectTool.Create(nil);
  CreateRectangleScreenObjectTool :=
    TCreateRectangleScreenObjectTool.Create(nil);
  DeleteSegmentTool := TDeleteSegmentTool.Create(nil);
  InsertPointTool := TInsertPointTool.Create(nil);
  SelectPointTool := TSelectPointTool.Create(nil);
  SelectScreenObjectTool := TSelectScreenObjectTool.Create(nil);
  ColRowLayerSelectorTool:= TColRowLayerSelectorTool.Create(nil);
  AddLinePartTool:= TAddLinePartTool.Create(nil);
  AddPolygonPartTool:= TAddPolygonPartTool.Create(nil);
  AddPointPartTool := TAddPointPartTool.Create(nil);

finalization
  ZoomTool.Free;
  ZoomInTool.Free;
  ZoomOutTool.Free;
  PanTool.Free;
  AddGridBoundaryTool.Free;
  MovingGridBoundaryTool.Free;
  DeleteGridBoundaryTool.Free;
  RotateGridTool.Free;
  LassoTool.Free;

  CreatePointScreenObjectTool.Free;
  CreateLineScreenObjectTool.Free;
  CreateStraightLineScreenObjectTool.Free;
  CreateRectangleScreenObjectTool.Free;
  DeleteSegmentTool.Free;
  InsertPointTool.Free;
  SelectPointTool.Free;
  SelectScreenObjectTool.Free;
  ColRowLayerSelectorTool.Free;
  AddLinePartTool.Free;
  AddPolygonPartTool.Free;
  AddPointPartTool.Free;

end.