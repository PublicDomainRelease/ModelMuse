{
  @abstract(The main item in @name is @link(TCustomGrid) which is an abstract
  class that defines the interface for a grid.
  It implements storage for columns and rows but not for layers.)


@author(Richard B. Winston <rbwinst@usgs.gov>)
  }
unit AbstractGridUnit;

interface

uses Types, SysUtils, Classes, Graphics, Controls, Forms, OpenGL12x,
  GR32, // TBitmap32 and TFloatRect are declared in GR32.
  FastGEO, GoPhastTypes, DataSetUnit, RbwParser, ZoomBox2, SubscriptionUnit,
  IsosurfaceUnit, ContourUnit;

type
  // @abstract(@name is used for grid related errors.)
  EInvalidGrid = class(Exception);

  {@abstract(The row and column of a cell)
  @longcode(#
  T2DTopCell = record
    Col: integer;
    Row: integer;
  end;
  #)
  @member(Col @name is the column number.)
  @member(Row @name is the row number.)
  }
  T2DTopCell = record
    Col: integer;
    Row: integer;
  end;

  {@abstract(The column and layer of a cell)
  @longcode(#
  T2DFrontCell = record
    Col: integer;
    Lay: integer;
  end;
  #)
  @member(Col @name is the column number.)
  @member(Lay @name is the layer number.)
  }
  T2DFrontCell = record
    Col: integer;
    Lay: integer;
  end;

  {@abstract(The row and layer of a cell)
  @longcode(#
  T2DSideCell = record
    Row: integer;
    Lay: integer;
  end;
  #)
  @member(Row @name is the row number.)
  @member(Lay @name is the layer number.)
  }
  T2DSideCell = record
    Row: integer;
    Lay: integer;
  end;

  {@abstract(The columns, row, and layer of a cell)
  @longcode(#
  T3DCell = record
    Col: integer;
    Row: integer;
    Lay: integer;
  end;
  #)
  @member(Col @name is the column number.)
  @member(Row @name is the row number.)
  @member(Lay @name is the layer number.)
  }
  T3DCell = record
    Col: integer;
    Row: integer;
    Lay: integer;
  end;

  // @name is used to indicate the direction in which columns are numbered.
  TColumnDirection = (cdWestToEast, cdEastToWest);
  // @name is used to indicate the direction in which rows are numbered.
  TRowDirection = (rdSouthToNorth, rdNorthToSouth);
  // @name is used to indicate the direction in which layers are numbered.
  TLayerDirection = (ldBottomToTop, ldTopToBottom);

  { @name is used to store a color for each cell in @link(TCustomGrid).
  @longcode(#TCellColors = array of array of array of TColor;#)
  }
  TCellColors = array of array of array of TColor;

  TLimit = record
    MinX: double;
    MaxX: double;
    MinY: double;
    MaxY: double;
    MinZ: double;
    MaxZ: double;
  end;

  { TODO : ThreeDDataSet, TPhastModel.ThreeDTimeList, and
TPhastModel.ThreeDDisplayTime are all related.  Maybe they should be
put together somehow.  There are similar groupings for the top, front, and 
side views of the model.}

  {@abstract(@name is an abstract class that defines the interface for a grid.
   It implements storage for columns and rows but not for layers.)  It also
   implements the display of the top view of the grid but not the front or side
   views.  The columns and rows boundaries are stored in one-dimensional
   arrays.  For some finite-difference models such as PHAST, the layers can
   also be stored in a one-dimensional arrays.  Others, such as MODFLOW,
   require a more complex access method.  The interface defined
   in @name defines the more complex access method required by a
   MODFLOW-type grid. 
   )
   @seealso(TPhastGrid)
   @seealso(TModflowGrid)
   }
  TCustomGrid = class(TPersistent)
  private
    // See @link(EdgesGLIndex).
    FEdgesGLIndex: GLuint;
    // See @link(CellsGLIndex).
    FCellsGLIndex: GLuint;
    // See @link(ColumnCount).
    FColumnCount: integer;
    // See @link(ColumnDirection).
    FColumnDirection: TColumnDirection;
    // See @link(ColumnPositions).
    FColumnPositions: TOneDRealArray;
    // See @link(FrontCellColors).
    FFrontCellColors: array of array of TColor;
    // See @link(FrontDataSet).
    FFrontDataSet: TDataArray;
    // See @link(FrontGridGLIndex).
    FFrontGridGLIndex: GLuint;
    // See @link(GridAngle).
    FGridAngle: real;
    // See @link(GridShellGLIndex).
    FGridShellGLIndex: GLuint;
    // See @link(LayerCount).
    FLayerCount: integer;
    // See @link(LayerDirection).
    FLayerDirection: TLayerDirection;
    // See @link(NeedToRecalculate3DCellColors).
    FNeedToRecalculate3DCellColors: boolean;
    // See @link(OnSelectedColumnChange).
    FOnSelectedColumnChange: TNotifyEvent;
    // @name: TNotifyEvent;
    // See @link(OnSelectedLayerChange).
    FOnSelectedLayerChange: TNotifyEvent;
    // See @link(OnSelectedRowChange).
    FOnSelectedRowChange: TNotifyEvent;
    // See @link(RowCount).
    FRowCount: integer;
    // See @link(RowDirection).
    FRowDirection: TRowDirection;
    // See @link(RowPositions).
    FRowPositions: TOneDRealArray;
    // See @link(SelectedColumn).
    FSelectedColumn: integer;
    // See @link(SelectedLayer).
    FSelectedLayer: integer;
    // See @link(SelectedRow).
    FSelectedRow: integer;
    // See @link(SideCellColors).
    FSideCellColors: array of array of TColor;
    // See @link(SideDataSet).
    FSideDataSet: TDataArray;
    // See @link(SideGridGLIndex)
    FSideGridGLIndex: GLuint;
    // See @link(ThreeDDataSet).
    FThreeDDataSet: TDataArray;
    // See @link(TopCellColors).
    FTopCellColors: array of array of TColor;
    // See @link(TopDataSet).
    FTopDataSet: TDataArray;
    // See @link(TopGridGLIndex).
    FTopGridGLIndex: GLuint;
    // See @link(TopGridObserver).
    FTopGridObserver: TObserver;
    // See @link(ThreeDGridObserver).
    FThreeDGridObserver: TObserver;
    // See @link(DisplayColumn).
    FDisplayColumn: integer;
    // See @link(DisplayLayer).
    FDisplayLayer: integer;
    // See @link(DisplayRow).
    FDisplayRow: integer;
    // @name is used in @link(LayersChanged) to delay redrawing the
    // grid while there are still pending changes.
    // @seealso(BeginLayerChange)
    // @seealso(EndLayerChange)
    FLayerUpdate: Integer;
    // @name is used in @link(ColumnsChanged) to delay redrawing the
    // grid while there are still pending changes.
    // @SeeAlso(BeginColumnChange)
    // @SeeAlso(EndColumnChange)
    FColumnUpdate: Integer;
    // @name is used in @link(GridChanged) to delay redrawing the
    // grid while there are still pending changes.
    // @SeeAlso(BeginGridChange)
    // @SeeAlso(EndGridChange)
    FGridUpdate: Integer;
    // @name is used in @link(RowsChanged) to delay redrawing the
    // grid while there are still pending changes.
    // @SeeAlso(BeginRowChange)
    // @SeeAlso(EndRowChange)
    FRowUpdate: Integer;
    // See @link(CreateNodeGlGrid).
    FNodeGlGrid: TGrid;
    // See @link(CreateBlockGlGrid).
    FBlockGlGrid: TGrid;
    // @name is used for drawing contours on the top view of the
    // model for data evaluated at nodes.
    // See @link(ContourGrid).
    FTopNodeContourGrid: T2DGrid;
    // @name is used for drawing contours on the front view of the
    // model for data evaluated at nodes.
    // See @link(ContourGrid).
    FFrontNodeContourGrid: T2DGrid;
    // @name is used for drawing contours on the side view of the
    // model for data evaluated at nodes.
    // See @link(ContourGrid).
    FSideNodeContourGrid: T2DGrid;
    // @name is used for drawing contours on the top view of the
    // model for data evaluated at elements.
    // See @link(ContourGrid).
    FTopElementContourGrid: T2DGrid;
    // @name is used for drawing contours on the front view of the
    // model for data evaluated at elements.
    // See @link(ContourGrid).
    FFrontElementContourGrid: T2DGrid;
    // @name is used for drawing contours on the side view of the
    // model for data evaluated at elements.
    // See @link(ContourGrid).
    FSideElementContourGrid: T2DGrid;
    // @name is used to store whether or not the
    // grid shell display
    // in 3D has been recorded or not.
    FRecordedShell: Boolean;
    // @name is used to store whether or not the
    // grid in side view
    // in 3D has been recorded or not.
    FRecordedSideGrid: Boolean;
    // @name is used to store whether or not the
    // grid in front view
    // in 3D has been recorded or not.
    FRecordedFrontGrid: Boolean;
    // @name is used to store whether or not the
    // grid in top view
    // in 3D has been recorded or not.
    FRecordedTopGrid: Boolean;
    // @name is the number of elements in a grid.
    // (In MODFLOW, @name is the number of cells in the grid.)
    FElementCount: integer;
    FDrawInteriorGridLines2D: boolean;
    FTopContourDataSet: TDataArray;
    FThreeDContourDataSet: TDataArray;
    FFrontContourDataSet: TDataArray;
    FSideContourDataSet: TDataArray;
    FDrawing3DGrid: Boolean;
    FCanDraw: boolean;
    FBlockGridCache: TMemoryStream;
    FNodeGridCache: TMemoryStream;
    // See @link(ColWidths).
    function GetColWidths: TOneDRealArray;
    // See @link(RowWidths).
    function GetRowWidths: TOneDRealArray;
    // @name returns an array that has a length equal to
    // Length(PositionArray)-1.  The result has the difference between
    // each adjacent member of PositionArray.  @name is used in
    // @link(GetColWidths) and @link(GetRowWidths).
    function GetWidths(const PositionArray: TOneDRealArray): TOneDRealArray;
    // @name is used to read @link(ColumnPositions) from a stream.
    // It calls @link(ReadRealArray) to do this.
    procedure ReadColumnPositions(Reader: TReader);
    // @name is used to read @link(RowPositions) from a stream.
    // It calls @link(ReadRealArray) to do this.
    procedure ReadRowPositions(Reader: TReader);
    // See @link(FrontDataSet).
    procedure SetFrontDataSet(const Value: TDataArray);
    // See @link(NeedToRecalculate3DCellColors).
    procedure SetNeedToRecalculate3DCellColors(const Value: boolean);
    // See @link(NeedToRecalculateFrontCellColors).
    procedure SetNeedToRecalculateFrontCellColors(const Value: boolean);
    // See @link(NeedToRecalculateSideCellColors).
    procedure SetNeedToRecalculateSideCellColors(const Value: boolean);
    // See @link(NeedToRecalculateTopCellColors).
    procedure SetNeedToRecalculateTopCellColors(const Value: boolean);
    // See @link(SelectedColumn).
    procedure SetSelectedColumn(Value: integer);
    // See @link(SelectedLayer).
    procedure SetSelectedLayer(Value: integer);
    // See @link(SelectedRow).
    procedure SetSelectedRow(Value: integer);
    // See @link(SideDataSet).
    procedure SetSideDataSet(const Value: TDataArray);
    // See @link(ThreeDDataSet).
    procedure SetThreeDDataSet(const Value: TDataArray);
    // See @1ink(ThreeDGridObserver).
    procedure SetThreeDGridObserver(const Value: TObserver);
    // See @link(TopDataSet).
    procedure SetTopDataSet(const Value: TDataArray);
    // See @1ink(TopGridObserver).
    procedure SetTopGridObserver(const Value: TObserver);
    // @name writes @link(ColumnPositions) to
    // a stream using @link(WriteRealArray).
    procedure WriteColumnPositions(Writer: TWriter);
    // @name writes @link(RowPositions) to
    // a stream using @link(WriteRealArray).
    procedure WriteRowPositions(Writer: TWriter);
    // See @link(DisplayColumn).
    procedure SetDisplayColumn(Value: integer);
    // See @link(DisplayLayer).
    procedure SetDisplayLayer(Value: integer);
    // See @link(DisplayRow).
    procedure SetDisplayRow(Value: integer);
    procedure RecordColoredGridEdges;
    procedure DrawLeftCellSide3D(ColIndex, RowIndex, LayerIndex: integer;
      const XPositions, YPositions: TOneDRealArray;
      const ZPositions: TThreeDRealArray);
    procedure DrawRightCellSide3D(ColIndex, RowIndex, LayerIndex: integer;
      const XPositions, YPositions: TOneDRealArray;
      const ZPositions: TThreeDRealArray);
    procedure DrawBackCellSide3D(ColIndex, RowIndex, LayerIndex: integer;
      const XPositions, YPositions: TOneDRealArray;
      const ZPositions: TThreeDRealArray);
    procedure DrawFrontCellSide3D(ColIndex, RowIndex, LayerIndex: integer;
      const XPositions, YPositions: TOneDRealArray;
      const ZPositions: TThreeDRealArray);
    procedure DrawBottomCellSide3D(ColIndex, RowIndex, LayerIndex: integer;
      const XPositions, YPositions: TOneDRealArray;
      const ZPositions: TThreeDRealArray);
    procedure DrawTopCellSide3D(ColIndex, RowIndex, LayerIndex: integer;
      const XPositions, YPositions: TOneDRealArray;
      const ZPositions: TThreeDRealArray);
    // @name creates @link(FBlockGlGrid) which can be used for
    // drawing isosurfaces of data at nodes. @link(FBlockGlGrid)
    // has dimensions (@link(ColumnCount) + 2, @link(RowCount) + 2,
    // @link(LayerCount) + 2.  Thus there is an extra point in each
    // direction outside the element centers.  These points are used in drawing
    // @link(TScreenObject)s in 3D.
    //
    // @name is called by @link(PrivateGlGrid).
    procedure CreateBlockGlGrid;
    // @name creates @link(FNodeGlGrid) which can be used for
    // drawing isosurfaces of data at nodes. @link(FNodeGlGrid)
    // has dimensions (@link(ColumnCount) + 3, @link(RowCount) + 3,
    // @link(LayerCount) + 3.  Thus there is an extra point in each
    // direction outside the grid.  These points are used in drawing
    // @link(TScreenObject)s in 3D.
    //
    // @name is called by @link(PrivateGlGrid).
    procedure CreateNodeGlGrid;
    // @name creates and caches a @link(TGrid) which can then
    // be used for drawing isosurfaces or contour lines.
    // @name is called from @link(ContourGrid) and @link(GlGrid).
    function PrivateGlGrid(EvaluatedAt: TEvaluatedAt;
      ModelSelection: TModelSelection):TGrid;
    procedure SetDrawInteriorGridLines2D(const Value: boolean);
    procedure SetFrontContourDataSet(const Value: TDataArray);
    procedure SetSideContourDataSet(const Value: TDataArray);
    procedure SetThreeDContourDataSet(const Value: TDataArray);
    procedure SetTopContourDataSet(const Value: TDataArray);
    procedure DrawTopContours(const ZoomBox: TQRbwZoomBox2;
      const BitMap: TBitmap32);
    procedure SetCanDraw(const Value: boolean);
    procedure SetOnSelectedLayerChange(const Value: TNotifyEvent);
    procedure CacheBlockGlGrid;
    procedure CacheGlGrid(GridCacheStream: TMemoryStream; var AGlGrid: TGrid);
    procedure RestoreBlockGlGrid;
    procedure RestoreGlGrid(var AGlGrid: TGrid; GridCacheStream: TMemoryStream);
    procedure CacheNodeGlGrid;
    procedure RestoreNodeGlGrid;
    function IsActiveOk(const DataSet: TDataArray; const Layer, Row,
      Col: integer): boolean;
    function ValueOK(DataSet: TDataArray; const Layer, Row,
      Col: integer): boolean;
  protected
    // @name is used to indicate whether or not the display lists
    // @link(FGridShellGLIndex), @link(FTopGridGLIndex),
    // @link(FFrontGridGLIndex), @link(FSideGridGLIndex), and
    // @link(FCellsGLIndex) have been created.
    FListsCreated: boolean;
    { @name indicates that the colors of the cells
      in the front view of the grid
      need to be recalculated.}
    FNeedToRecalculateFrontCellColors: boolean;
    { @name indicates that the colors of the cells
      in the side view of the grid
      need to be recalculated.}
    FNeedToRecalculateSideCellColors: boolean;
    { @name indicates that the colors of the cells
      in the top view of the grid
      need to be recalculated.}
    FNeedToRecalculateTopCellColors: boolean;
    { @name indicates that the 3D colored cells in the grid
      or the grid shell need to be redrawn.}
    FNeedToRedraw3d: boolean;
    // @name is used to store an OpenGL display list for display
    // of properties of the edges of cells. @name is used to display
    // Horizontal Flow Barriers in MODFLOW.
    property EdgesGLIndex: GLuint read FEdgesGLIndex;
    // @name is used to store an OpenGL display list for the colored
    // cells or elements of the grid.
    property CellsGLIndex: GLuint read FCellsGLIndex;
    // @name causes @link(TopGridObserver) to have @link(TObserver.UpToDate)
    // set to false and then to true. That will invalidate everything that
    // depends on @link(TopGridObserver).
    procedure ColumnsChanged;
    { @name is used to define how @link(ColumnPositions) and
      @link(RowPositions) are written to and read from a stream.  This
      allows those properties to be saved to a file and read from a file.
      @SeeAlso(ReadRealArray) @SeeAlso(ReadColumnPositions)
      @SeeAlso(ReadRowPositions) @SeeAlso(WriteRealArray)
      @SeeAlso(WriteColumnPositions) @SeeAlso(WriteRowPositions)}
    procedure DefineProperties(Filer: TFiler); override;
    {@name draws a front view of the grid on BitMap.}
    procedure DrawFront(const BitMap: TBitmap32;
      const ZoomBox: TQRbwZoomBox2); virtual; abstract;
    {@name draws a side view of the grid on BitMap.}
    procedure DrawSide(const BitMap: TBitmap32;
      const ZoomBox: TQRbwZoomBox2); virtual; abstract;
    {@name draws a top view of the grid on BitMap.}
    procedure DrawTop(const BitMap: TBitmap32;
      const ZoomBox: TQRbwZoomBox2); virtual;
    // @name is used to store a OpenGL display list for the front
    // view of the grid.
    property FrontGridGLIndex: GLuint read FFrontGridGLIndex;
    // @name sets Elevations to a 3D array of real numbers.  Each
    // number in the array is the elevation of one corner of a cell or
    // element.
    procedure GetCellCornerElevations(const EvalAt: TEvaluatedAt;
      out Elevations: TThreeDRealArray); virtual; abstract;
    // See @link(CellElevation).
    function GetCellElevation(const Column, Row, Layer: integer): real;
      virtual; abstract;
    // See @link(CellThickness).
    function GetCellThickness(const Column, Row, Layer: integer): real;
      virtual; abstract;
    // See @link(ColumnPosition).
    function GetColumnPosition(const Column: integer): real; virtual;
    // See @link(ColumnWidth).
    function GetColumnWidth(const Column: integer): real; virtual;
    { Positions is an array of column or row boundaries.
     @name gets the column or row (not column boundary
     or row boundary) that contains APosition.
     If APosition < Positions[0] then result := -1.
     If APosition > Positions[Length(Positions)-1] then
       result := Length(Positions)-1. }
    function GetContainingColumnOrRow(const Positions: TOneDRealArray;
      const APosition: real): integer;
    {See @link(FrontCellColors).}
    function GetFrontCellColors(const Column, Layer: integer): TColor; virtual;
    // See @link(RowPosition).
    function GetRowPosition(const Row: integer): real; virtual;
    // See @link(RowWidth).
    function GetRowWidth(const Row: integer): real; virtual;
    {See @link(SideCellColors).}
    function GetSideCellColors(const Row, Layer: integer): TColor; virtual;
    {See @link(TopCellColors).}
    function GetTopCellColors(const Column, Row: integer): TColor; virtual;
    // See @link(TwoDCellElevations)
    function GetTwoDCellElevations(const Col, Row: integer): TOneDRealArray;
      virtual; abstract;
    // @name is used to store a OpenGL display list for the grid shell.
    property GridShellGLIndex: GLuint read FGridShellGLIndex;
    // @name causes @link(ThreeDGridObserver) to have @link(TObserver.UpToDate)
    // set to false and then to true. That will invalidate everything that
    // depends on @link(ThreeDGridObserver).
    procedure LayersChanged; 
    {@name finds the index of the element in Positions that is
     closest to APosition.  If the index is known to be within a certain range,
     NearestColumnOrRow can be made faster by specifying First and Last.
     If APosition is less than the first member of Positions, @name returns -1.
     If APosition is greater than the last member of Positions, @name returns
     Length(Positions).
     @param(Positions is an array of real numbers sorted in ascending order.)
     @param(APosition, is the value that is being searched for.)
     @param(First is the first member of Positions to be included in the
     search. If First = -1, use the first member of Positions as the
     beginning of the search.)
     @param(Last is the last member of Positions to be included in the
     search. If Last = -1, use the last member of Positions as the
     beginning of the search.)
     }
    function NearestColumnOrRow(const Positions: TOneDRealArray;
      const APosition: real; const First: integer = -1;
      const Last: integer = -1): integer;
    // @name creates an OpenGL display list using @link(FCellsGLIndex)
    // to show colored grid cells or elements.
    procedure RecordColoredGrid; virtual;
    // @name creates an OpenGL display list using @link(FFrontGridGLIndex)
    // to show the grid on the front view of the model.
    procedure RecordFront; virtual;
    // @name creates an OpenGL display list using @link(GridShellGLIndex)
    // to show the grid shell.
    procedure RecordShell; virtual;
    // @name creates an OpenGL display list using @link(FSideGridGLIndex)
    // to show the grid on the side view of the model.
    procedure RecordSide; virtual;
    // @name creates an OpenGL display list using @link(FTopGridGLIndex)
    // to show the grid on the top view of the model.
    procedure RecordTop; virtual;
    // @name causes @link(TopGridObserver) to have @link(TObserver.UpToDate)
    // set to false and then to true. That will invalidate everything that
    // depends on @link(TopGridObserver).
    procedure RowsChanged;
    // See @link(CellElevation).
    procedure SetCellElevation(const Column, Row, Layer: integer;
      const Value: real); virtual; abstract;
    // See @link(CellThickness).
    procedure SetCellThickness(const Column, Row, Layer: integer;
      const Value: real); virtual; abstract;
    // See @link(ColumnCount).
    procedure SetColumnCount(const Value: integer); virtual;
    // See @link(ColumnDirection).
    procedure SetColumnDirection(const Value: TColumnDirection); virtual;
    // @name sets LocalLineColor to an appropriate value
    // for the column indicated by ColIndex.
    procedure SetColumnLineColor(ColIndex: Integer; LocalEvalAt: TEvaluatedAt;
      var LocalLineColor: TColor32; var LineWidth: single);
    // See @link(ColumnPosition).
    procedure SetColumnPosition(const Column: integer; const Value: real);
      virtual;
    // See @link(ColumnPositions).
    procedure SetColumnPositions(const Value: TOneDRealArray);
    // See @link(ColumnWidth).
    procedure SetColumnWidth(const Column: integer; const Value: real);
    {See @link(FrontCellColors).}
    procedure SetFrontCellColors(const Column, Layer: integer;
      const Value: TColor); virtual;
    // See @link(GridAngle).
    procedure SetGridAngle(Value: real); virtual;
    // see @link(LayerCount).
    procedure SetLayerCount(const Value: integer); virtual;
    // See @link(LayerDirection).
    procedure SetLayerDirection(const Value: TLayerDirection); virtual;
    // @name sets LocalLineColor to an appropriate value
    // for the layer indicated by LayerIndex.
    procedure SetLayerLineColor(LayerIndex: Integer; LocalEvalAt: TEvaluatedAt;
      var LocalLineColor: TColor32; var LineWidth: single);
    // @name sets LocalEvalAt to an appropriate value based on ViewDirection
    // and the data set used to color the grid.
    procedure SetLocalEvalAt(ViewDirection: TViewDirection;
      var LocalEvalAt: TEvaluatedAt);
    // See @link(RowCount).
    procedure SetRowCount(const Value: integer); virtual;
    // See @link(RowDirection).
    procedure SetRowDirection(const Value: TRowDirection); virtual;
    // @name sets LocalLineColor to an appropriate value for the row indicated
    // by ColIndex.
    procedure SetRowLineColor(RowIndex: Integer; LocalEvalAt: TEvaluatedAt;
      var LocalLineColor: TColor32; var LineWidth: single);
    // See @link(RowPosition).
    procedure SetRowPosition(const Row: integer; const Value: real); virtual;
    // See @link(RowPositions).
    procedure SetRowPositions(const Value: TOneDRealArray);
    // See @link(RowWidth).
    procedure SetRowWidth(const Row: integer; const Value: real); virtual;
    {See @link(SideCellColors).}
    procedure SetSideCellColors(const Row, Layer: integer;
      const Value: TColor); virtual;
    {See @link(TopCellColors).}
    procedure SetTopCellColors(const Column, Row: integer;
      const Value: TColor); virtual;
    // @name is used to store a OpenGL display list for the side
    // view of the grid.
    property SideGridGLIndex: GLuint read FSideGridGLIndex;
    // @name is used to store a OpenGL display list for the horizontal
    // view of the grid.
    property TopGridGLIndex: GLuint read FTopGridGLIndex;
    // @name modifies CellColors to reflect the values of @link(ThreeDDataSet).
    procedure Update3DCellColors(var CellColors: TCellColors);
    procedure ElementCountChanged;
    procedure DrawFrontContours(const ZoomBox: TQRbwZoomBox2;
      const BitMap: TBitmap32);
    procedure DrawSideContours(const ZoomBox: TQRbwZoomBox2;
      const BitMap: TBitmap32);
  public
    property Drawing3DGrid: boolean read FDrawing3DGrid;
    // @name creates and caches a @link(T2DGrid) which can then be used
    // in drawing contour lines in @link(TContourCreator.DrawContour).
    // The dimensions are based on the dimensions of @link(GlGrid).
    function ContourGrid(EvaluatedAt: TEvaluatedAt;
      ModelSelection: TModelSelection; ViewDirection: TViewDirection;
      ColRowOrLayer: integer): T2DGrid;
    procedure BeginLayerChange;
    procedure EndLayerChange;
    procedure BeginGridChange;
    procedure EndGridChange;
    procedure BeginColumnChange;
    procedure EndColumnChange;
    procedure BeginRowChange;
    procedure EndRowChange;

    { Add a new column boundary at position "Value".}
    procedure AddColumn(const Value: real);
    { Add a new row boundary at position "Value".}
    procedure AddRow(const Value: real);
    {Copies the properties of Source into self.  Only those properties that
     normally would be saved to file are copied.}
    procedure Assign(Source: TPersistent); override;
    property CanDraw: boolean read FCanDraw write SetCanDraw;
    // @name is used to determine whether or not a grid can be drawn in 3D.
    function CanDraw3D: boolean;
    { @name is an abstract property that represents the elevation of
      the boundary between the cell at Row, Column between Layer and Layer-1.
      There are LayerCount + 1 layers of elevations in the grid.  }
    property CellElevation[const Column, Row, Layer: integer]: real
      read GetCellElevation write SetCellElevation;
    { @name is an abstract property that represents the thickness of
      a cell.  Thicknesses must be greater than or equal to 0.}
    property CellThickness[const Column, Row, Layer: integer]: real
      read GetCellThickness write SetCellThickness;
    {@name returns the position of the center of a column}
    function ColumnCenter(const Column: integer): real;
    { @name defines either the boundary between one column and its
      neighbor or the position of one edge of the grid.  There are
      @link(ColumnCount)+1 members in ColumnPosition.
      For direct access to the array
      use @link(ColumnPositions) instead. The origin of the grid is at
     (ColumnPosition[0], RowPosition[0]).  }
    property ColumnPosition[const Column: integer]: real
      read GetColumnPosition write SetColumnPosition;
    { @name is the array of boundaries between adjacent columns.
      After editing ColumnPositions, call UpdateColumnPositions to update the
      number of columns and to make sure the columns are sorted in ascending
      order.}
    property ColumnPositions: TOneDRealArray read FColumnPositions
      write SetColumnPositions;
    { @name is the width of a column. ColumnWidth must be greater than
      or equal to 0. }
    property ColumnWidth[const Column: integer]: real read GetColumnWidth
      write SetColumnWidth;
    // @name represents an array of column widths.
    property ColWidths: TOneDRealArray read GetColWidths;
    // @name creates a grid with 0 rows, columns and layers.
    constructor Create;
    // @name deletes the column boundary at the position AColumn.
    procedure DeleteColumn(const AColumn: integer);
    // @name deletes the row boundary at the position ARow.
    procedure DeleteRow(const ARow: integer);
    // @name gets rid of OpenGL display lists.
    destructor Destroy; override;
    {@name draws a top, front or side view of the grid on BitMap.
    ViewDirection determines which one is drawn.  @SeeAlso(DrawTop)
    @SeeAlso(DrawFront)  @SeeAlso(DrawSide)}
    procedure Draw(const BitMap: TBitmap32;
      const ViewDirection: TViewDirection);
    { @name draws the grid in the current OpenGL rendering context.}
    procedure Draw3D; virtual;
    // If @name is @true, the grid will be displayed with all it's grid
    // lines in 2D views of the model. Otherwise, it will only include
    // the first, last selected grid lines.
    property DrawInteriorGridLines2D: boolean read FDrawInteriorGridLines2D
      write SetDrawInteriorGridLines2D;
    {@name is used to retrieve or set the color of a element or
     node in a front view of the grid.  Whether the color is the
     color of a cell
     or a node depends on whether the @link(FrontDataSet) is evaluated at
     elements or nodes.}
    property FrontCellColors[const Column, Layer: integer]: TColor
      read GetFrontCellColors write SetFrontCellColors;
    {@name is the  @link(TDataArray) whose values are used
     to determine the colors
     of the cells in a front view of the grid.  See: @link(FrontCellColors),
     TPhastModel.@link(TPhastModel.FrontDisplayTime), and
     TPhastModel.@link(TPhastModel.FrontTimeList).}
    property FrontDataSet: TDataArray read FFrontDataSet write SetFrontDataSet;
    property FrontContourDataSet: TDataArray read FFrontContourDataSet
      write SetFrontContourDataSet;
    // @name returns the column that contains AnXPosition.
    // See @link(GetContainingColumnOrRow).
    function GetContainingColumn(const AnXPosition: real): integer;
    // @name returns the layer at (ACol, ARow) that contains AZPosition.
    function GetContainingLayer(ACol, ARow: integer;
      const AZPosition: real): integer; virtual; abstract;
    // @name returns the row that contains AYPosition.
    // See @link(GetContainingColumnOrRow).
    function GetContainingRow(const AYPosition: real): integer;
    // @name returns the limits of one of the sparse 2D elevation
    // arrays in a @link(TScreenObject) in Limit1 and Limit2.
    // Depending on ViewDirection. Limit1 and Limit2 will be
    // @unorderedList(
    //   @item(Column and Row limits for top view)
    //   @item(Column and Layer limits for front view)
    //   @item(Row and Layer limits for side view)
    // )
    procedure GetLimits(const EvaluatedAt: TEvaluatedAt;
      const ViewDirection: TViewDirection; out Limit1, Limit2: integer);
    // @name is called when the grid has changed in a way that means it needs
    // to be redrawn.  It notifies the
    // appropriate controls to redraw themselves.
    procedure GridChanged;
    // @name returns the highest elevation in the grid.
    // @seealso(LowestElevation)
    function HighestElevation: real; virtual; abstract;
    // @name sets display @link(TDataArray)s
    // such as @link(TopDataSet), @link(FrontDataSet) to nil.
    // Resets @link(DisplayLayer) @link(DisplayRow) and @link(DisplayColumn)
    // to 0 if they are less than zero.
    procedure Initialize;
    // @name returns the lowest elevation in the grid.
    // @seealso(HighestElevation)
    function LowestElevation: real; virtual; abstract;
    // @name returns the index of the layer boundary in (Col, Row)
    // that is closest to APosition.
    function Nearest2DCellElevation(const Col, Row: integer;
      const APosition: real; const First: integer = -1;
      const Last: integer = -1): integer;
    { @name returns the index of the column center closest to APosition.
      If the index is known to be within a certain range, the function
      can be speeded up by specifying First and Last.}
    function NearestColumnCenter(const APosition: real;
      First: integer = -1; Last: integer = -1): integer;
    { @name returns the index of the column boundary closest to APosition.
      If the index is known to be within a certain range, the function
      can be speeded up by specifying First and Last.}
    function NearestColumnPosition(const APosition: real;
      const First: integer = -1; const Last: integer = -1): integer;
    { @name returns the index of the row center closest to APosition.
      If the index is known to be within a certain range, the function
      can be speeded up by specifying First and Last.}
    function NearestRowCenter(const APosition: real;
      First: integer = -1; Last: integer = -1): integer;
    { @name returns the index of the row boundary closest to APosition.
      If the index is known to be within a certain range, the function
      can be speeded up by specifying First and Last.}
    function NearestRowPosition(const APosition: real;
      const First: integer = -1; const Last: integer = -1): integer;
    { @name indicates that the colors of the cells
      in the 3D view of the grid need to be recalculated.
      @link(FNeedToRecalculate3DCellColors) is a protected field.}
    property NeedToRecalculate3DCellColors: boolean read
      FNeedToRecalculate3DCellColors write SetNeedToRecalculate3DCellColors;
    { @name sets @link(NeedToRecalculateFrontCellColors),
      @link(NeedToRecalculateSideCellColors),
      @link(NeedToRecalculateTopCellColors), and
      @link(NeedToRecalculate3DCellColors) to false.}
    procedure NeedToRecalculateCellColors;
    { @name indicates that the colors of the cells
      in the front view of the grid need to be recalculated.
      NeedToRecalculateFrontCellColors can be used to set
      @link(FNeedToRecalculateFrontCellColors) to false but not to true.
      @link(FNeedToRecalculateFrontCellColors) is a protected field.}
    property NeedToRecalculateFrontCellColors: boolean
      read FNeedToRecalculateFrontCellColors write
      SetNeedToRecalculateFrontCellColors;
    { @name indicates that the colors of the cells
      in the side view of the grid need to be recalculated.
      NeedToRecalculateSideCellColors can be used to set
      @link(FNeedToRecalculateSideCellColors) to false but not to true.
      @link(FNeedToRecalculateSideCellColors) is a protected field.}
    property NeedToRecalculateSideCellColors: boolean
      read FNeedToRecalculateSideCellColors write
      SetNeedToRecalculateSideCellColors;
    { @name indicates that the colors of the cells
      in the top view of the grid need to be recalculated.
      NeedToRecalculateTopCellColors can be used to set
      @link(FNeedToRecalculateTopCellColors) to false but not to true.
      @link(FNeedToRecalculateTopCellColors) is a protected field.}
    property NeedToRecalculateTopCellColors: boolean
      read FNeedToRecalculateTopCellColors write
      SetNeedToRecalculateTopCellColors;
    { @name is an event that can be used to respond to changes
      in which column is selected.}
    property OnSelectedColumnChange: TNotifyEvent read FOnSelectedColumnChange
      write FOnSelectedColumnChange;
    { @name is an event that can be used to respond to changes
      in which layer is selected.}
    property OnSelectedLayerChange: TNotifyEvent read FOnSelectedLayerChange
      write SetOnSelectedLayerChange;
    { @name is an event that can be used to respond to changes
      in which row is selected.}
    property OnSelectedRowChange: TNotifyEvent read FOnSelectedRowChange write
      FOnSelectedRowChange;
    {@name sets the colors of the cells in a front view of the grid to white.
     See @link(UpdateCellColors).}
    procedure ResetFrontCellColors;
    {Sets the colors of the cells in a side view of the grid to white.
     See @link(UpdateCellColors).}
    procedure ResetSideCellColors;
    {Sets the colors of the cells in a top view of the grid to white.
     See @link(UpdateCellColors).}
    procedure ResetTopCellColors;
     { TODO : Change "RealWorld" to "Global" to match the user documentation. }
    { @name converts coordinates
      expressed in terms of Column and Row coordinates to ones based on real
      world coordinates.  }
    function RotateFromGridCoordinatesToRealWorldCoordinates
      (const APoint: TPoint2D): TPoint2D;
    { @name is the inverse of
      @link(RotateFromGridCoordinatesToRealWorldCoordinates).}
    function RotateFromRealWorldCoordinatesToGridCoordinates
      (const APoint: TPoint2D): TPoint2D;
    {@name returns the position of the center of a row}
    function RowCenter(const Row: integer): real;
    { @name defines either the boundary between one row and its
      neighbor or the position of one edge of the grid.  There are
      RowCount+1 members in @name.  For direct access to the array
      use @link(RowPositions) instead. The origin of the grid is at
      (ColumnPosition[0], RowPosition[0]).}
    property RowPosition[const Row: integer]: real read GetRowPosition
      write SetRowPosition;
    { @name is the array of boundaries between adjacent rows.
      After editing @name, call @link(UpdateRowPositions) to update the
      number of rows and to make sure the rows are sorted in ascending
      order.}
    property RowPositions: TOneDRealArray read FRowPositions
      write SetRowPositions;
    { @name is the width of a row. RowWidth must be greater than
      or equal to 0. }
    property RowWidth[const Row: integer]: real read GetRowWidth
      write SetRowWidth;
    // @name represents an array of rows widths.
    property RowWidths: TOneDRealArray read GetRowWidths;
    {@name is used to retrieve or set the color of a element or
     node in a side view of the grid.  Whether the color is the color of a cell
     or a node depends on whether the @link(SideDataSet) is evaluated at
     elements or nodes.}
    property SideCellColors[const Row, Layer: integer]: TColor
      read GetSideCellColors write SetSideCellColors;
    {@name is the  @link(TDataArray) whose values are used
     to determine the colors
     of the cells in a side view of the grid.  See: @link(SideCellColors),
     TPhastModel.@link(TPhastModel.SideDisplayTime), and
     TPhastModel.@link(TPhastModel.SideTimeList).}
    property SideDataSet: TDataArray read FSideDataSet write SetSideDataSet;
    property SideContourDataSet: TDataArray read FSideContourDataSet
      write SetSideContourDataSet;
    { @name returns the X, Y, and Z coordinates of the
      center of a column boundary in the coordinate system of the grid.
      @SeeAlso(ThreeDRowEdgeCenter) @SeeAlso(ThreeDLayerEdgeCenter)}
    function ThreeDColumnEdgeCenter(const Column, Row, Layer: integer):
      T3DRealPoint;
    {@name is the @link(TDataArray) whose values are used
     to determine the colors
     of the cells in a 3D view of the grid.  See: @link(TCellColors),
     TPhastModel.@link(TPhastModel.ThreeDDisplayTime), and
     TPhastModel.@link(TPhastModel.ThreeDTimeList).}
    property ThreeDDataSet: TDataArray read FThreeDDataSet
      write SetThreeDDataSet;
    property ThreeDContourDataSet: TDataArray read FThreeDContourDataSet
      write SetThreeDContourDataSet;
    { @name returns the X, Y, and Z coordinates of the center of
      a grid element in the coordinate system of the grid.}
    function ThreeDElementCenter(const Column, Row, Layer: integer):
      T3DRealPoint; virtual;
    { @name returns the X, Y, and Z coordinates of a corner of
      a grid element in the coordinate system of the grid.}
    function ThreeDElementCorner(const Column, Row, Layer: integer):
      T3DRealPoint; virtual;
    function ThreeDCellCorner(Column, Row, Layer: integer):
      T3DRealPoint; virtual;
    // @name is used to notify @link(TScreenObject)s and @link(TDataArray)s
    // That they need to redraw themselves due to a change in the columns,
    //  rows. or layers.
    property ThreeDGridObserver: TObserver read FThreeDGridObserver
      write SetThreeDGridObserver;
    { @name returns the X, Y, and Z coordinates of the
      center of a layer boundary in the coordinate system of the grid.
      @SeeAlso(ThreeDColumnEdgeCenter) @SeeAlso(ThreeDRowEdgeCenter)}
    function ThreeDLayerEdgeCenter(const Column, Row, Layer: integer):
      T3DRealPoint;
    { @name returns the X, Y, and Z coordinates of the
      center of a row boundary in the coordinate system of the grid.
      @SeeAlso(ThreeDColumnEdgeCenter) @SeeAlso(ThreeDLayerEdgeCenter)}
    function ThreeDRowEdgeCenter(const Column, Row, Layer: integer):
      T3DRealPoint;
    {@name is used to retrieve or set the color of a element or
     node in a top view of the grid.  Whether the color is the color of a cell
     or a node depends on whether the @link(TopDataSet) is evaluated at
     elements or nodes.}
    property TopCellColors[const Column, Row: integer]: TColor
      read GetTopCellColors write SetTopCellColors;
    // @name represents a one-dimensional array of values representing the
    // top and bottom elevations of the elements at (Col,Row).
    property  TwoDCellElevations[const Col, Row: integer]: TOneDRealArray
      read GetTwoDCellElevations;
    { @name retrieves the indices of the element or node that
      contains APoint.  Whether the indices of an element or node are
      retrieved depends on EvaluatedAt.  If APoint is in grid coordinates,
      NeedToRotatePointToGridCoordinates should be set to true.  If the
      result is known to be within a certain range, the function can be
      speeded up by specifying BelowCol, AboveCol, BelowRow, and AboveRow.
      The T2DTopCell that is returned should never have values outside the
      valid range as long as there are at least one column and row in the
      grid.}
    function TopContainingCell(APoint: TPoint2D;
      const EvaluatedAt: TEvaluatedAt;
      const NeedToRotatePointToGridCoordinates: boolean = True;
      const BelowCol: integer = -1; const AboveCol: integer = -1;
      const BelowRow: integer = -1; const AboveRow: integer = -1): T2DTopCell;
    {@name is the  @link(TDataArray) whose values are used
     to determine the colors
     of the cells in a top view of the grid.  See: @link(TopCellColors),
     TPhastModel.@link(TPhastModel.TopDisplayTime), and
     TPhastModel.@link(TPhastModel.TopTimeList).}
    property TopDataSet: TDataArray read FTopDataSet write SetTopDataSet;
    property TopContourDataSet: TDataArray read FTopContourDataSet
      write SetTopContourDataSet;
    // @name is used to notify @link(TScreenObject)s and @link(TDataArray)s
    // That they need to redraw themselves due to a change in the columns
    // or rows.
    property TopGridObserver: TObserver read FTopGridObserver
      write SetTopGridObserver;
    { TwoDCellCorner returns the X ,and Y coordinates of a corner of
      a grid cell in the real-world coordinates.}
    function TwoDCellCorner(const Column, Row: integer): TPoint2D;
    { @name returns the X ,and Y coordinates of the center
      of the edge of a column in the real-world coordinates.}
    function TwoDColumnEdgeCenter(const Column, Row: integer): TPoint2D;
    { @name returns the X, and Y coordinates of the center of
      a grid element in the real-world coordinates.}
    function TwoDElementCenter(const Column, Row: integer): TPoint2D;
    function UnrotatedTwoDElementCenter(const Column, Row: integer): TPoint2D;
    { TwoDCellCorner returns the X ,and Y coordinates of a corner of
      a grid element in the real-world coordinates.}
    function TwoDElementCorner(const Column, Row: integer): TPoint2D;
    function UnrotatedTwoDElementCorner(const Column, Row: integer): TPoint2D;
    { @name returns the X ,and Y coordinates of the center
      of the edge of a row in the real-world coordinates.}
    function TwoDRowEdgeCenter(const Column, Row: integer): TPoint2D;
    {When the values in the displayed @link(TopDataSet), @link(FrontDataSet),
     or @link(SideDataSet) are up-to-date, call @name to update
     colors of the grid cells.}
    procedure UpdateCellColors(const ViewDirection: TViewDirection);
    { After editing @link(ColumnPositions), call @name to update the
      number of columns and to make sure the columns are sorted in ascending
      order.}
    procedure UpdateColumnPositions; virtual;
    { After editing @link(RowPositions), call @name to update the
      number of rows and to make sure the rows are sorted in ascending
      order.}
    procedure UpdateRowPositions; virtual; // update this.
    // notify the views that the need to redraw;
    procedure ViewsChanged;
    // The result of @name is used in drawing @link(TScreenObject)s in
    // the 3D view. The dimensions are
    // ColumnCount + 2, RowCount + 2, LayerCount + 2
    // if EvaluatedAt = eaBlocks and
    // ColumnCount + 3, RowCount + 3, LayerCount + 3
    // if EvaluatedAt = eaNodes.
    function GlGrid(EvaluatedAt: TEvaluatedAt;
      ModelSelection: TModelSelection): TGrid;
    {@name returns the elevation of the center of an element.}
    function LayerCenter(Column, Row, Layer: integer): real;
    // @name returns the extent of the grid in grid coordinates;
    function GridLimits(ViewDirection: TViewDirection): TLimit;
    function GridOutline(ViewDirection: TViewDirection): TPolygon2D;
  published
    { @name is the number of columns in the grid.
      Each column has a width that is greater than or equal to 0.}
    property ColumnCount: integer read FColumnCount write SetColumnCount;
    { @name determines whether columns are numbered
      from west to east or east to west.}
    property ColumnDirection: TColumnDirection read FColumnDirection
      write SetColumnDirection;
    { @name is the angle made by the grid relative to the East direction.
      it is measured counterclockwise.  If @name is 0, columns run
      north/south and rows run east/west. @name is measured in radians.}
    property GridAngle: real read FGridAngle write SetGridAngle;
    { @name is the number of layers in the grid.  Each element in the grid
      has a thickness that is greater than or equal to 0.}
    property LayerCount: integer read FLayerCount write SetLayerCount;
    { @name determines whether layers are numbered from top to bottom
      or bottom to top.}
    property LayerDirection: TLayerDirection read FLayerDirection
      write SetLayerDirection;
    { @name is the number of rows in the grid.  Each row has a
      width that is greater than or equal to 0.}
    property RowCount: integer read FRowCount write SetRowCount;
    { @name determines whether rows are numbered from north to south
      or south to north.}
    property RowDirection: TRowDirection read FRowDirection
      write SetRowDirection;
    {@name is the column that is displayed in the side view
     of the grid.  @SeeAlso(SelectedRow) @SeeAlso(SelectedLayer)}
    property SelectedColumn: integer read FSelectedColumn write
      SetSelectedColumn stored True;
    {@name is the layer that is displayed in the top view
     of the grid.  @SeeAlso(SelectedColumn) @SeeAlso(SelectedRow)}
    property SelectedLayer: integer read FSelectedLayer write SetSelectedLayer stored True;
    {@name is the row that is displayed in the front view
     of the grid.  @SeeAlso(SelectedColumn) @SeeAlso(SelectedLayer)}
    property SelectedRow: integer read FSelectedRow write SetSelectedRow stored True;
    // @name is the grid column that should be displayed on the 3D view.
    property DisplayColumn: integer read FDisplayColumn write SetDisplayColumn stored True;
    // @name is the grid row that should be displayed on the 3D view.
    property DisplayRow: integer read FDisplayRow write SetDisplayRow stored True;
    // @name is the grid layer that should be displayed on the 3D view.
    property DisplayLayer: integer read FDisplayLayer write SetDisplayLayer stored True;
  end;

{
   @name calls @link(TColorParameters.FracToColor
   frmGoPhast.PhastModel.GridColors.FracToColor(Fraction);)
}
function GridFracToRainbow(const Fraction: real): TColor;

{@name is used to read a one-dimensional array of real numbers
 from a stream. See @link(TCustomGrid.DefineProperties)}
procedure ReadRealArray(const Reader: TReader;
  var Positions: TOneDRealArray; const Count: integer);

{@name is used to write a one-dimensional array of real numbers
 to a stream. See @link(TCustomGrid.DefineProperties)}
procedure WriteRealArray(const Writer: TWriter;
  const Positions: TOneDRealArray);

{@name is used to read a one-dimensional array of integers
 from a stream. See @link(TCustomGrid.DefineProperties)}
procedure ReadIntegerArray(const Reader: TReader;
  var Positions: TOneDIntegerArray; const Count: integer);

{@name is used to write a one-dimensional array of integers
 to a stream. See @link(TCustomGrid.DefineProperties)}
procedure WriteIntegerArray(const Writer: TWriter;
  const Positions: TOneDIntegerArray);

// @name returns @True if all members of AnArray are the same.
function IsUniform(const AnArray: TOneDRealArray): boolean;
  
const
  // @name is the thickness of thin grid lines when drawn in the top, front,
  // or side views.
  OrdinaryGridLineThickness = 1.0;
  // @name is the thickness of thick grid lines when drawn in the top, front,
  // or side views.
  ThickGridLineThickness = 2.85;

const
  // @name is the thickness of thick grid lines when drawn in the 3D view.
  ThickLine = 2.0;
  // @name is the thickness of thin grid lines when drawn in the 3D view.
  ThinLine = 1.0;

var
  // @name is the color used for the selected column
  // (@link(TCustomGrid.SelectedColumn)).
  ExistingColumnSelectionCellColor: TColor;
  // @name is the color used for the selected row
  // (@link(TCustomGrid.SelectedRow)).
  ExistingRowSelectionCellColor: TColor;
  // @name is the color used for the selected layer
  // (@link(TCustomGrid.SelectedLayer)).
  ExistingLayerSelectionCellColor: TColor;

  
implementation

uses GR32_Polygons, Math, RealListUnit, frmGoPhastUnit, BigCanvasMethods,
  ModelMuseUtilities, PhastDataSets, ScreenObjectUnit, PhastModelUnit, InteractiveTools,
  EdgeDisplayUnit, ZLib, IntListUnit;

const  
  // @name is the number of OpenGL display lists used to display the grid.
  NumberOfLists = 6;

function IsUniform(const AnArray: TOneDRealArray): boolean;
var
  Size: double;
  Index: integer;
begin
  Assert(Length(AnArray) >= 1);
  Size := AnArray[0];
  result := True;
  for Index := 1 to Length(AnArray) - 1 do
  begin
    result := AnArray[Index] = Size;
    if not result then
    begin
      Exit;
    end;
  end;
end;


procedure ReadRealArray(const Reader: TReader;
  var Positions: TOneDRealArray; const Count: integer);
var
  Index: integer;
  CurrentCount: integer;
begin
  with Reader do
  begin
    if Count <= 1 then
    begin
      CurrentCount := 4;
      SetLength(Positions, CurrentCount);
      ReadListBegin;
      Index := 0;
      while not EndOfList do
      begin
        Positions[Index] := ReadFloat;
        Inc(Index);
        if Index = CurrentCount then
        begin
          CurrentCount := CurrentCount * 2;
          SetLength(Positions, CurrentCount);
        end;
      end;
      ReadListEnd;
      SetLength(Positions, Index);
    end
    else
    begin
      SetLength(Positions, Count);
      ReadListBegin;
      for Index := 0 to Count - 1 do
      begin
        Positions[Index] := ReadFloat;
      end;
      ReadListEnd;
    end;
  end;
end;

procedure WriteRealArray(const Writer: TWriter;
  const Positions: TOneDRealArray);
var
  Count: integer;
  Index: integer;
begin
  with Writer do
  begin
    Count := Length(Positions);
    WriteListBegin;
    for Index := 0 to Count - 1 do
    begin
      WriteFloat(Positions[Index]);
    end;
    WriteListEnd;
  end;
end;

procedure ReadIntegerArray(const Reader: TReader;
  var Positions: TOneDIntegerArray; const Count: integer);
var
  Index: integer;
  CurrentCount: integer;
begin
  with Reader do
  begin
    if Count = 0 then
    begin
      CurrentCount := 4;
      SetLength(Positions, CurrentCount);
      ReadListBegin;
      Index := 0;
      while not EndOfList do
      begin
        Positions[Index] := ReadInteger;
        Inc(Index);
        if Index = CurrentCount then
        begin
          CurrentCount := CurrentCount * 2;
          SetLength(Positions, CurrentCount);
        end;
      end;
      ReadListEnd;
      SetLength(Positions, Index);
    end
    else
    begin
      SetLength(Positions, Count);
      ReadListBegin;
      for Index := 0 to Count - 1 do
      begin
        if EndOfList then
        begin
          SetLength(Positions, Index);
          break;
        end;
        Positions[Index] := ReadInteger;
      end;
      ReadListEnd;
    end;
  end;
end;

procedure WriteIntegerArray(const Writer: TWriter;
  const Positions: TOneDIntegerArray);
var
  Count: integer;
  Index: integer;
begin
  with Writer do
  begin
    Count := Length(Positions);
    WriteListBegin;
    for Index := 0 to Count - 1 do
    begin
      WriteInteger(Positions[Index]);
    end;
    WriteListEnd;
  end;
end;

function GridFracToRainbow(const Fraction: real): TColor;
begin
  result := frmGoPhast.PhastModel.GridColors.FracToColor(Fraction);
end;

{ TCustomGrid }

procedure TCustomGrid.AddColumn(const Value: real);
begin
  SetLength(FColumnPositions, Length(FColumnPositions) + 1);
  FColumnPositions[Length(FColumnPositions) - 1] := Value;
  UpdateColumnPositions;
  frmGoPhast.PhastModel.InvalidateScreenObjects;
  GridChanged;
  ColumnsChanged;
end;

procedure TCustomGrid.AddRow(const Value: real);
begin
  SetLength(FRowPositions, Length(FRowPositions) + 1);
  FRowPositions[Length(FRowPositions) - 1] := Value;
  UpdateRowPositions;
  frmGoPhast.PhastModel.InvalidateScreenObjects;
  GridChanged;
  RowsChanged;
end;

constructor TCustomGrid.Create;
begin
  inherited;
  FDrawInteriorGridLines2D := True;
  FElementCount := 0;
  FNeedToRedraw3d := True;
  FRecordedSideGrid := False;
  FRecordedFrontGrid := False;
  FRecordedTopGrid := False;
  FRecordedShell := False;
  FRecordedSideGrid := False;
  FRecordedFrontGrid := False;
  FRecordedTopGrid := False;
  FGridAngle := 0;
  FColumnCount := -1;
  FLayerCount := -1;
  FRowCount := -1;
  SetLength(FColumnPositions, 0);
  SetLength(FRowPositions, 0);
  FCanDraw := True;
end;

procedure TCustomGrid.DeleteColumn(const AColumn: integer);
var
  Index: integer;
begin
  if (AColumn < 0) or (AColumn >= Length(FColumnPositions)) then
  begin
    Exit;
  end;
  for Index := AColumn + 1 to Length(FColumnPositions) - 1 do
  begin
    FColumnPositions[Index - 1] := FColumnPositions[Index];
  end;
  ColumnCount := ColumnCount - 1;
  frmGoPhast.PhastModel.InvalidateSegments;
  frmGoPhast.PhastModel.InvalidateScreenObjects;
  frmGoPhast.InvalidateModel;
  GridChanged;
  ColumnsChanged;
end;

procedure TCustomGrid.DeleteRow(const ARow: integer);
var
  Index: integer;
begin
  if (ARow < 0) or (ARow >= Length(FRowPositions)) then
  begin
    Exit;
  end;
  for Index := ARow + 1 to Length(FRowPositions) - 1 do
  begin
    FRowPositions[Index - 1] := FRowPositions[Index];
  end;
  RowCount := RowCount - 1;
  frmGoPhast.PhastModel.InvalidateSegments;
  frmGoPhast.PhastModel.InvalidateScreenObjects;
  frmGoPhast.InvalidateModel;
  GridChanged;
  RowsChanged;
end;

procedure TCustomGrid.Draw(const BitMap: TBitmap32;
  const ViewDirection: TViewDirection);
begin
  if not CanDraw then
  begin
    Exit;
  end;
  case ViewDirection of
    vdTop:
      begin
        DrawTop(BitMap, frmGoPhast.frameTopView.ZoomBox);
      end;
    vdFront:
      begin
        DrawFront(BitMap, frmGoPhast.frameFrontView.ZoomBox);
      end;
    vdSide:
      begin
        DrawSide(BitMap, frmGoPhast.frameSideView.ZoomBox);
      end;
  else
    Assert(False);
  end;

end;

procedure TCustomGrid.DrawTop(const BitMap: TBitmap32;
  const ZoomBox: TQRbwZoomBox2);
var
  RowIndex, ColumnIndex: integer;
  Point1, Point2, Point3, Point4: TPoint2D;
  AColor, NewColor: TColor;
  PriorRow: integer;
  BoxCoordinates: TPointArray;
  TempValue: double;
  PointCount: integer;
  ColumnLimit, RowLimit: integer;
  LineWidth: single;
  LocalEvalAt: TEvaluatedAt;
  LocalLineColor: TColor32;
  Polygon: TPolygon32;
  MultiplePolygons: boolean;
  function ConvertTop2D_Point(const APoint: TPoint2D): TPoint;
  begin
    result.X := ZoomBox.XCoord(APoint.X);
    result.Y := ZoomBox.YCoord(APoint.Y);
  end;
  procedure CalculatePoint1;
  var
    X1: double;
    Y1: double;
    X2: double;
    Y2: double;
    Angle: double;
  begin
    Angle := GridAngle;
    X1 := Cos(Angle) * ColumnPosition[ColumnIndex];
    Y1 := Sin(Angle) * ColumnPosition[ColumnIndex];
    Y2 := TempValue - Y1;
    try
      if (Angle = Pi / 2) or (Angle = -Pi / 2) then
      begin
        X2 := 0;
      end
      else
      begin
        X2 := Tan(Angle) * Y2;
      end;
    except
      X2 := 0;
    end;
    Point1.X := X1 - X2;
    Point1.Y := TempValue;
  end;
  procedure CalculatePoint2;
  var
    X1: double;
    Y1: double;
    X2: double;
    Y2: double;
    Angle: double;
  begin
    Angle := GridAngle;
    X1 := Cos(Angle) * ColumnPosition[ColumnIndex];
    Y1 := Sin(Angle) * ColumnPosition[ColumnIndex];
    X2 := X1 - TempValue;
    try
      if (Angle = 0) or (Angle = Pi) or (Angle = -Pi) then
      begin
        Y2 := 0;
      end
      else
      begin
        Y2 := Cot(Angle) * X2;
      end;
    except
      Y2 := 0;
    end;
    Point1.X := TempValue;
    Point1.Y := Y1 + Y2;
  end;
  procedure CalculatePoint3;
  var
    X1: double;
    Y1: double;
    X2: double;
    Y2: double;
    Angle: double;
  begin
    Angle := GridAngle;
    X1 := Sin(Angle) * RowPosition[RowIndex];
    Y1 := Cos(Angle) * RowPosition[RowIndex];
    X2 := X1 + TempValue;
    try
      if (Angle = Pi / 2) or (Angle = -Pi / 2) then
      begin
        Y2 := 0;
      end
      else
      begin
        Y2 := Tan(Angle) * X2;
      end;
    except
      Y2 := 0;
    end;
    Point1.X := TempValue;
    Point1.Y := Y1 + Y2;
  end;
  procedure CalculatePoint4;
  var
    X1: double;
    Y1: double;
    X2: double;
    Y2: double;
    Angle: double;
  begin
    Angle := GridAngle;
    X1 := Sin(Angle) * RowPosition[RowIndex];
    Y1 := Cos(Angle) * RowPosition[RowIndex];
    Y2 := TempValue - Y1;
    try
      if (Angle = 0) or (Angle = Pi) or (Angle = -Pi) then
      begin
        X2 := 0;
      end
      else
      begin
        X2 := Cot(Angle) * Y2;
      end;
    except
      X2 := 0;
    end;
    Point1.X := X2 - X1;
    Point1.Y := TempValue;
  end;
  function GetNodeBasedCorner(Col, Row: integer): TPoint2D;
  begin
    if (Col = 0) or (Col > ColumnLimit) then
    begin
      if Col > ColumnLimit then
      begin
        Dec(Col);
      end;
      // column center or cell center
      if (Row = 0) or (Row > RowLimit) then
      begin
        if (Row > RowLimit) then
        begin
          Dec(Row);
        end;
        // cell center
        result := TwoDElementCorner(Col, Row);
      end
      else
      begin
        // column center
        result := TwoDColumnEdgeCenter(Col, Row - 1);
      end;
    end
    else
    begin
      // row center or cell center
      if (Row = 0) or (Row > RowLimit) then
      begin
        // row center
        if (Row > RowLimit) then
        begin
          Dec(Row);
        end;
        result := TwoDRowEdgeCenter(Col - 1, Row);
      end
      else
      begin
        // cell center
        result := TwoDElementCenter(Col - 1, Row - 1);
      end;
    end;
  end;
begin
  Polygon := nil;
  MultiplePolygons := False;
  SetLength(BoxCoordinates, 4);
  with ZoomBox do
  begin
    if (ColumnCount <= 0)
      or (RowCount <= 0)
      or (LayerCount <= 0) then
    begin
      LineWidth := OrdinaryGridLineThickness;
      for ColumnIndex := 0 to ColumnCount do
      begin
        if Not DrawInteriorGridLines2D and (ColumnIndex <> 0)
          and (ColumnIndex <> ColumnCount) then
        begin
          Continue;
        end;

        PointCount := 0;

        TempValue := Y(Image32.Height);
        CalculatePoint1;
        BoxCoordinates[PointCount] := ConvertTop2D_Point(Point1);
        Inc(PointCount);

        TempValue := Y(0);
        CalculatePoint1;
        BoxCoordinates[PointCount] := ConvertTop2D_Point(Point1);
        //Inc(PointCount);

        DrawBigPolyline32(BitMap, clBlack32, LineWidth, BoxCoordinates,
          True, False, 0, 2);
      end;

      for RowIndex := 0 to RowCount do
      begin
        if Not DrawInteriorGridLines2D and (RowIndex <> 0)
          and (RowIndex <> RowCount) then
        begin
          Continue;
        end;
        PointCount := 0;

        TempValue := X(Image32.Width);
        CalculatePoint3;
        BoxCoordinates[PointCount] := ConvertTop2D_Point(Point1);
        Inc(PointCount);

        TempValue := X(0);
        CalculatePoint3;
        BoxCoordinates[PointCount] := ConvertTop2D_Point(Point1);
        //Inc(PointCount);

        DrawBigPolyline32(BitMap, clBlack32, LineWidth, BoxCoordinates,
          True, False, 0, 2);
      end;
    end
    else
    begin
      if TopDataSet <> nil then
      begin
        case TopDataSet.EvaluatedAt of
          eaBlocks:
            begin
              ColumnLimit := ColumnCount - 1;
              RowLimit := RowCount - 1;
            end;
          eaNodes:
            begin
              ColumnLimit := ColumnCount;
              RowLimit := RowCount;
            end;
        else
          begin
            Assert(False);
            ColumnLimit := -1;
            RowLimit := -1;
          end;
        end;

        for ColumnIndex := 0 to ColumnLimit do
        begin
          if RowCount > 0 then
          begin
            PriorRow := 0;
            AColor := TopCellColors[ColumnIndex, 0];
            NewColor := AColor;
            for RowIndex := 1 to RowLimit do
            begin
              NewColor := TopCellColors[ColumnIndex, RowIndex];
              if (NewColor <> AColor) then
              begin
                if AColor <> clWhite then
                begin
                  case TopDataSet.EvaluatedAt of
                    eaBlocks:
                      begin
                        Point1 := TwoDElementCorner(ColumnIndex, PriorRow);
                        Point2 := TwoDElementCorner(ColumnIndex + 1, PriorRow);
                        Point3 := TwoDElementCorner(ColumnIndex + 1, RowIndex);
                        Point4 := TwoDElementCorner(ColumnIndex, RowIndex);
                      end;
                    eaNodes:
                      begin
                        Point1 := GetNodeBasedCorner(ColumnIndex, PriorRow);
                        Point2 := GetNodeBasedCorner(ColumnIndex + 1, PriorRow);
                        Point3 := GetNodeBasedCorner(ColumnIndex + 1, RowIndex);
                        Point4 := GetNodeBasedCorner(ColumnIndex, RowIndex);
                      end;
                  else
                    Assert(False);
                  end;
                  BoxCoordinates[0] := ConvertTop2D_Point(Point1);
                  BoxCoordinates[1] := ConvertTop2D_Point(Point2);
                  BoxCoordinates[2] := ConvertTop2D_Point(Point3);
                  BoxCoordinates[3] := ConvertTop2D_Point(Point4);
                  DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
                    0, BoxCoordinates, Polygon, MultiplePolygons, True);
                end;
                AColor := NewColor;
                PriorRow := RowIndex;

              end;
            end;

            if NewColor <> clWhite then
            begin
              case TopDataSet.EvaluatedAt of
                eaBlocks:
                  begin
                    Point1 := TwoDElementCorner(ColumnIndex, PriorRow);
                    Point2 := TwoDElementCorner(ColumnIndex + 1, PriorRow);
                    Point3 := TwoDElementCorner(ColumnIndex + 1, RowCount);
                    Point4 := TwoDElementCorner(ColumnIndex, RowCount);
                  end;
                eaNodes:
                  begin
                    Point1 := GetNodeBasedCorner(ColumnIndex, PriorRow);
                    Point2 := GetNodeBasedCorner(ColumnIndex + 1, PriorRow);
                    Point3 := GetNodeBasedCorner(ColumnIndex + 1, RowCount + 1);
                    Point4 := GetNodeBasedCorner(ColumnIndex, RowCount + 1);
                  end;
              else
                Assert(False);
              end;
              BoxCoordinates[0] := ConvertTop2D_Point(Point1);
              BoxCoordinates[1] := ConvertTop2D_Point(Point2);
              BoxCoordinates[2] := ConvertTop2D_Point(Point3);
              BoxCoordinates[3] := ConvertTop2D_Point(Point4);
              DrawBigPolygon32(BitMap, Color32(NewColor), Color32(NewColor),
                0, BoxCoordinates, Polygon, MultiplePolygons, True);
            end;
          end;
        end;
      end;

      SetLocalEvalAt(vdTop, LocalEvalAt);
      for ColumnIndex := 0 to ColumnCount do
      begin
        if (ColumnIndex mod 10 = 0) or (ColumnIndex = ColumnCount) then
        begin
          LineWidth := ThickGridLineThickness;
        end
        else
        begin
          LineWidth := OrdinaryGridLineThickness;
        end;
        SetColumnLineColor(ColumnIndex, LocalEvalAt, LocalLineColor, LineWidth);
        if Not DrawInteriorGridLines2D and (ColumnIndex <> 0)
          and (ColumnIndex <> ColumnCount) and (LocalLineColor = clBlack32) then
        begin
          Continue;
        end;
        Point1 := TwoDElementCorner(ColumnIndex, 0);
        Point2 := TwoDElementCorner(ColumnIndex, RowCount);

        DrawBigPolyline32(BitMap, LocalLineColor, LineWidth,
          [ConvertTop2D_Point(Point1), ConvertTop2D_Point(Point2)], True);
      end;

      for RowIndex := 0 to RowCount do
      begin
        if (RowIndex mod 10 = 0) or (RowIndex = RowCount) then
        begin
          LineWidth := ThickGridLineThickness;
        end
        else
        begin
          LineWidth := OrdinaryGridLineThickness;
        end;
        SetRowLineColor(RowIndex, LocalEvalAt, LocalLineColor, LineWidth);
        if Not DrawInteriorGridLines2D and (RowIndex <> 0)
          and (RowIndex <> RowCount) and (LocalLineColor = clBlack32) then
        begin
          Continue;
        end;
        Point1 := TwoDElementCorner(0, RowIndex);
        Point2 := TwoDElementCorner(ColumnCount, RowIndex);

        DrawBigPolyline32(BitMap, LocalLineColor, LineWidth,
          [ConvertTop2D_Point(Point1), ConvertTop2D_Point(Point2)], True);
      end;

    end;
  end;
  if frmGoPhast.PhastModel.EdgeDisplay <> nil then
  begin
    frmGoPhast.PhastModel.EdgeDisplay.Draw(SelectedLayer, BitMap);
  end;
  DrawTopContours(ZoomBox, BitMap);

end;

procedure TCustomGrid.ElementCountChanged;
const
  BigGrid = 100000;
var
  NewElementCount: integer;
begin
  NewElementCount := Max(FColumnCount,0)*Max(FRowCount,0)*Max(FLayerCount,0);
  if (NewElementCount >= BigGrid) and (FElementCount < BigGrid) then
  begin
    if frmGoPhast.acShow3DObjects.Checked then
    begin
      frmGoPhast.acShow3DObjects.OnExecute(frmGoPhast.acShow3DObjects);
    end;
  end;
  FElementCount := NewElementCount;
end;

procedure TCustomGrid.EndColumnChange;
begin
  Dec(FColumnUpdate);
  ColumnsChanged;
end;

procedure TCustomGrid.EndGridChange;
begin
  Dec(FGridUpdate);
  GridChanged;
end;

procedure TCustomGrid.EndLayerChange;
begin
  Dec(FLayerUpdate);
  LayersChanged;
end;

procedure TCustomGrid.EndRowChange;
begin
  Dec(FRowUpdate);
  RowsChanged;
end;

function TCustomGrid.GetColumnPosition(const Column: integer): real;
begin
  if (Column < 0) or (Column > ColumnCount) then
  begin
    raise EInvalidGrid.Create('Invalid column number');
  end;
  result := FColumnPositions[Column];
end;

function TCustomGrid.GetColumnWidth(const Column: integer): real;
begin
  result := ColumnPosition[Column + 1] - ColumnPosition[Column];
  if ColumnDirection = cdEastToWest then
  begin
    result := -result;
  end;
end;

function TCustomGrid.GetWidths(
  const PositionArray: TOneDRealArray): TOneDRealArray;
var
  Index: Integer;
begin
  if Length(PositionArray) >= 2 then
  begin
    SetLength(result, Length(PositionArray)-1);
    for Index := 1 to Length(PositionArray) - 1 do
    begin
      result[Index-1] := PositionArray[Index] - PositionArray[Index-1]
    end;
  end
  else
  begin
    result := nil;
  end;
end;

procedure TCustomGrid.CacheBlockGlGrid;
begin
  if FBlockGridCache = nil then
  begin
    FBlockGridCache := TMemoryStream.Create;
    FBlockGridCache.Position := 0;
    CacheGlGrid(FBlockGridCache, FBlockGlGrid);
  end;
end;

procedure TCustomGrid.RestoreBlockGlGrid;
begin
  RestoreGlGrid(FBlockGlGrid, FBlockGridCache);
end;

procedure TCustomGrid.CacheNodeGlGrid;
begin
  if FNodeGridCache = nil then
  begin
    FNodeGridCache := TMemoryStream.Create;
    FNodeGridCache.Position := 0;
    CacheGlGrid(FNodeGridCache, FNodeGlGrid);
  end;
end;

procedure TCustomGrid.RestoreNodeGlGrid;
begin
  RestoreGlGrid(FNodeGlGrid, FNodeGridCache);
end;


procedure TCustomGrid.CreateBlockGlGrid;
var
  ColumnIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  APoint3D: T3DRealPoint;
  APoint2D: TPoint2D;
begin
  if FBlockGridCache <> nil then
  begin
    RestoreBlockGlGrid;
    Exit;
  end;
  SetLength(FBlockGlGrid, ColumnCount + 2, RowCount + 2, LayerCount + 2);

  APoint2D := TwoDElementCorner (0, 0);
  FBlockGlGrid[0,0,0].P.X := APoint2D.X;
  FBlockGlGrid[0,0,0].P.Y := APoint2D.Y;
  FBlockGlGrid[0,0,0].P.Z := CellElevation[0,0,0];

  FBlockGlGrid[0,0,LayerCount+1].P.X := APoint2D.X;
  FBlockGlGrid[0,0,LayerCount+1].P.Y := APoint2D.Y;
  FBlockGlGrid[0,0,LayerCount+1].P.Z := CellElevation[0,0,LayerCount];


  APoint2D := TwoDElementCorner (ColumnCount, 0);
  FBlockGlGrid[ColumnCount + 1,0,0].P.X := APoint2D.X;
  FBlockGlGrid[ColumnCount + 1,0,0].P.Y := APoint2D.Y;
  FBlockGlGrid[ColumnCount + 1,0,0].P.Z := CellElevation[ColumnCount-1,0,0];

  FBlockGlGrid[ColumnCount + 1,0,LayerCount+1].P.X := APoint2D.X;
  FBlockGlGrid[ColumnCount + 1,0,LayerCount+1].P.Y := APoint2D.Y;
  FBlockGlGrid[ColumnCount + 1,0,LayerCount+1].P.Z :=
    CellElevation[ColumnCount-1,0,LayerCount];
    

  APoint2D := TwoDElementCorner (0, RowCount);
  FBlockGlGrid[0,RowCount + 1,0].P.X := APoint2D.X;
  FBlockGlGrid[0,RowCount + 1,0].P.Y := APoint2D.Y;
  FBlockGlGrid[0,RowCount + 1,0].P.Z := CellElevation[0,RowCount-1,0];

  APoint2D := TwoDElementCorner (0, RowCount);
  FBlockGlGrid[0,RowCount + 1,LayerCount+1].P.X := APoint2D.X;
  FBlockGlGrid[0,RowCount + 1,LayerCount+1].P.Y := APoint2D.Y;
  FBlockGlGrid[0,RowCount + 1,LayerCount+1].P.Z :=
    CellElevation[0,RowCount-1,LayerCount];


  APoint2D := TwoDElementCorner (ColumnCount, RowCount);
  FBlockGlGrid[ColumnCount + 1,RowCount + 1,0].P.X := APoint2D.X;
  FBlockGlGrid[ColumnCount + 1,RowCount + 1,0].P.Y := APoint2D.Y;
  FBlockGlGrid[ColumnCount + 1,RowCount + 1,0].P.Z :=
    CellElevation[ColumnCount-1,RowCount-1,0];

  FBlockGlGrid[ColumnCount + 1,RowCount + 1,LayerCount+1].P.X := APoint2D.X;
  FBlockGlGrid[ColumnCount + 1,RowCount + 1,LayerCount+1].P.Y := APoint2D.Y;
  FBlockGlGrid[ColumnCount + 1,RowCount + 1,LayerCount+1].P.Z :=
    CellElevation[ColumnCount-1,RowCount-1,LayerCount];


  for ColumnIndex := 0 to ColumnCount - 1 do
  begin
    for RowIndex := 0 to RowCount - 1 do
    begin
      for LayerIndex := 0 to LayerCount - 1 do
      begin
        APoint2D := TwoDElementCenter(ColumnIndex, RowIndex);
        APoint3D.X := APoint2D.x;
        APoint3D.Y := APoint2D.y;
        APoint3D.Z := (CellElevation[ColumnIndex,RowIndex,LayerIndex]
          + CellElevation[ColumnIndex,RowIndex,LayerIndex+1])/2;

        FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+1].P.X := APoint3D.X;
        FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+1].P.Y := APoint3D.Y;
        FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+1].P.Z := APoint3D.Z;

        if LayerIndex = 0 then
        begin
          FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex].P.X := APoint3D.X;
          FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex].P.Y := APoint3D.Y;
          FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex].P.Z :=
            CellElevation[ColumnIndex,RowIndex,LayerIndex];
        end;
        if LayerIndex = LayerCount - 1 then
        begin
          FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+2].P.X := APoint3D.X;
          FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+2].P.Y := APoint3D.Y;
          FBlockGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+2].P.Z :=
            CellElevation[ColumnIndex,RowIndex,LayerIndex+1];
        end;

        if RowIndex = 0 then
        begin
          APoint2D :=  TwoDRowEdgeCenter(ColumnIndex, RowIndex);
          FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex+1].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex+1].P.Y := APoint2D.Y;
          FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex+1].P.Z := APoint3D.Z;
        end;
        if RowIndex = RowCount - 1 then
        begin
          APoint2D :=  TwoDRowEdgeCenter(ColumnIndex, RowIndex+1);
          FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+1].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+1].P.Y := APoint2D.Y;
          FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+1].P.Z := APoint3D.Z;
        end;

        if ColumnIndex = 0 then
        begin
          APoint2D :=  TwoDColumnEdgeCenter(ColumnIndex, RowIndex);
          FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex+1].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex+1].P.Y := APoint2D.Y;
          FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex+1].P.Z := APoint3D.Z;
        end;
        if ColumnIndex = ColumnCount - 1 then
        begin
          APoint2D :=  TwoDColumnEdgeCenter(ColumnIndex+1, RowIndex);
          FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+1].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+1].P.Y := APoint2D.Y;
          FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+1].P.Z := APoint3D.Z;
        end;

        if (ColumnIndex = 0) and (RowIndex = 0) then
        begin
          APoint2D :=  TwoDElementCorner(ColumnIndex, RowIndex);
          FBlockGlGrid[ColumnIndex,RowIndex,LayerIndex+1].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex,RowIndex,LayerIndex+1].P.Y := APoint2D.Y;
          FBlockGlGrid[ColumnIndex,RowIndex,LayerIndex+1].P.Z := APoint3D.Z;
        end;
        if (ColumnIndex = ColumnCount - 1) and (RowIndex = 0) then
        begin
          APoint2D :=  TwoDElementCorner(ColumnIndex+1, RowIndex);
          FBlockGlGrid[ColumnIndex+2,RowIndex,LayerIndex+1].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+2,RowIndex,LayerIndex+1].P.Y := APoint2D.Y;
          FBlockGlGrid[ColumnIndex+2,RowIndex,LayerIndex+1].P.Z := APoint3D.Z;
        end;
        if (ColumnIndex = 0) and (RowIndex = RowCount - 1) then
        begin
          APoint2D :=  TwoDElementCorner(ColumnIndex, RowIndex+1);
          FBlockGlGrid[ColumnIndex,RowIndex+2,LayerIndex+1].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex,RowIndex+2,LayerIndex+1].P.Y := APoint2D.Y;
          FBlockGlGrid[ColumnIndex,RowIndex+2,LayerIndex+1].P.Z := APoint3D.Z;
        end;
        if (ColumnIndex = ColumnCount - 1) and (RowIndex = RowCount - 1) then
        begin
          APoint2D :=  TwoDElementCorner(ColumnIndex+1, RowIndex+1);
          FBlockGlGrid[ColumnIndex+2,RowIndex+2,LayerIndex+1].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+2,RowIndex+2,LayerIndex+1].P.Y := APoint2D.Y;
          FBlockGlGrid[ColumnIndex+2,RowIndex+2,LayerIndex+1].P.Z := APoint3D.Z;
        end;

        if (ColumnIndex = 0) and (LayerIndex = 0) then
        begin
          APoint2D :=  TwoDColumnEdgeCenter(ColumnIndex, RowIndex);
          FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex].P.Y := APoint2D.Y;
          FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex].P.Z :=
            CellElevation[ColumnIndex,RowIndex,LayerIndex];
        end;
        if (ColumnIndex = ColumnCount - 1) and (LayerIndex = 0) then
        begin
          APoint2D :=  TwoDColumnEdgeCenter(ColumnIndex+1, RowIndex);
          FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex].P.Y := APoint2D.Y;
          FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex].P.Z :=
            CellElevation[ColumnIndex,RowIndex,LayerIndex];
        end;
        if (ColumnIndex = 0) and (LayerIndex = LayerCount - 1) then
        begin
          APoint2D :=  TwoDColumnEdgeCenter(ColumnIndex, RowIndex);
          FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex+2].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex+2].P.Y := APoint2D.Y;
          FBlockGlGrid[ColumnIndex,RowIndex+1,LayerIndex+2].P.Z :=
            CellElevation[ColumnIndex,RowIndex,LayerIndex+1];
        end;
        if (ColumnIndex = ColumnCount - 1)
          and (LayerIndex = LayerCount - 1) then
        begin
          APoint2D :=  TwoDColumnEdgeCenter(ColumnIndex+1, RowIndex);
          FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+2].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+2].P.Y := APoint2D.Y;
          FBlockGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+2].P.Z :=
            CellElevation[ColumnIndex,RowIndex,LayerIndex+1];
        end;

        if (RowIndex = 0) and (LayerIndex = 0) then
        begin
          APoint2D :=  TwoDRowEdgeCenter(ColumnIndex, RowIndex);
          FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex].P.Y := APoint2D.Y;
          FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex].P.Z :=
            CellElevation[ColumnIndex,RowIndex,LayerIndex];
        end;
        if (RowIndex = RowCount - 1) and (LayerIndex = 0) then
        begin
          APoint2D :=  TwoDRowEdgeCenter(ColumnIndex, RowIndex+1);
          FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex].P.Y := APoint2D.Y;
          FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex].P.Z :=
            CellElevation[ColumnIndex,RowIndex,LayerIndex];
        end;
        if (RowIndex = 0) and (LayerIndex = LayerCount - 1) then
        begin
          APoint2D :=  TwoDRowEdgeCenter(ColumnIndex, RowIndex);
          FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex+2].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex+2].P.Y := APoint2D.Y;
          FBlockGlGrid[ColumnIndex+1,RowIndex,LayerIndex+2].P.Z :=
            CellElevation[ColumnIndex,RowIndex,LayerIndex+1];
        end;
        if (RowIndex = RowCount - 1) and (LayerIndex = LayerCount - 1) then
        begin
          APoint2D :=  TwoDRowEdgeCenter(ColumnIndex, RowIndex+1);
          FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+2].P.X := APoint2D.X;
          FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+2].P.Y := APoint2D.Y;
          FBlockGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+2].P.Z :=
            CellElevation[ColumnIndex,RowIndex,LayerIndex+1];
        end;
      end;
    end;
  end;
  for ColumnIndex := 0 to ColumnCount + 1 do
  begin
    for RowIndex := 0 to RowCount + 1 do
    begin
      for LayerIndex := 0 to LayerCount + 1 do
      begin
        FBlockGlGrid[ColumnIndex,RowIndex,LayerIndex].Value := 0;
      end;
    end;
  end;
end;

procedure TCustomGrid.CreateNodeGlGrid;
var
  ColumnIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  APoint2D: TPoint2D;
  APoint3D: T3DRealPoint;
  ElevAbove, ElevBelow: double;
begin
  if FNodeGridCache <> nil then
  begin
    RestoreNodeGlGrid;
    Exit;
  end;
  SetLength(FNodeGlGrid, ColumnCount + 3, RowCount + 3, LayerCount + 3);

  ElevAbove := CellElevation[0,0,0]
    + (CellElevation[0,0,0] - CellElevation[0,0,1])/2;
  ElevBelow := CellElevation[0,0,LayerCount]
    + (CellElevation[0,0,LayerCount] - CellElevation[0,0,LayerCount-1])/2;

  APoint2D.x := ColumnPosition[0] + (ColumnPosition[0] - ColumnPosition[1])/2;
  APoint2D.y := RowPosition[0] + (RowPosition[0] - RowPosition[1])/2;
  APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

  FNodeGlGrid[0,0,0].P.X := APoint2D.X;
  FNodeGlGrid[0,0,0].P.Y := APoint2D.Y;
  FNodeGlGrid[0,0,0].P.Z := ElevAbove;

  FNodeGlGrid[0,0,LayerCount + 2].P.X := APoint2D.X;
  FNodeGlGrid[0,0,LayerCount + 2].P.Y := APoint2D.Y;
  FNodeGlGrid[0,0,LayerCount + 2].P.Z := ElevBelow;

  APoint2D.x := ColumnPosition[ColumnCount]
    + (ColumnPosition[ColumnCount] - ColumnPosition[ColumnCount-1]);
  APoint2D.y := RowPosition[0] + (RowPosition[0] - RowPosition[1])/2;
  APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

  FNodeGlGrid[ColumnCount + 2,0,0].P.X := APoint2D.X;
  FNodeGlGrid[ColumnCount + 2,0,0].P.Y := APoint2D.Y;
  FNodeGlGrid[ColumnCount + 2,0,0].P.Z := ElevAbove;

  FNodeGlGrid[ColumnCount + 2,0,LayerCount + 2].P.X := APoint2D.X;
  FNodeGlGrid[ColumnCount + 2,0,LayerCount + 2].P.Y := APoint2D.Y;
  FNodeGlGrid[ColumnCount + 2,0,LayerCount + 2].P.Z := ElevBelow;

  APoint2D.x := ColumnPosition[0] + (ColumnPosition[0] - ColumnPosition[1])/2;
  APoint2D.y := RowPosition[RowCount]
    + (RowPosition[RowCount] - RowPosition[RowCount-1])/2;
  APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

  FNodeGlGrid[0,RowCount + 2,0].P.X := APoint2D.X;
  FNodeGlGrid[0,RowCount + 2,0].P.Y := APoint2D.Y;
  FNodeGlGrid[0,RowCount + 2,0].P.Z := ElevAbove;

  FNodeGlGrid[0,RowCount + 2,LayerCount + 2].P.X := APoint2D.X;
  FNodeGlGrid[0,RowCount + 2,LayerCount + 2].P.Y := APoint2D.Y;
  FNodeGlGrid[0,RowCount + 2,LayerCount + 2].P.Z := ElevBelow;

  APoint2D.x := ColumnPosition[ColumnCount]
    + (ColumnPosition[ColumnCount] - ColumnPosition[ColumnCount-1])/2;
  APoint2D.y := RowPosition[RowCount]
    + (RowPosition[RowCount] - RowPosition[RowCount-1])/2;
  APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

  FNodeGlGrid[ColumnCount + 2,RowCount + 2,0].P.X := APoint2D.X;
  FNodeGlGrid[ColumnCount + 2,RowCount + 2,0].P.Y := APoint2D.Y;
  FNodeGlGrid[ColumnCount + 2,RowCount + 2,0].P.Z := ElevAbove;

  FNodeGlGrid[ColumnCount + 2,RowCount + 2,LayerCount + 2].P.X := APoint2D.X;
  FNodeGlGrid[ColumnCount + 2,RowCount + 2,LayerCount + 2].P.Y := APoint2D.Y;
  FNodeGlGrid[ColumnCount + 2,RowCount + 2,LayerCount + 2].P.Z := ElevBelow;

  for ColumnIndex := 0 to ColumnCount do
  begin
    for RowIndex := 0 to RowCount do
    begin
      APoint2D := TwoDElementCorner(ColumnIndex, RowIndex);
      APoint3D.X := APoint2D.x;
      APoint3D.Y := APoint2D.y;
      APoint3D.Z := ElevAbove;

      FNodeGlGrid[ColumnIndex+1,RowIndex+1,0].P.X := APoint3D.X;
      FNodeGlGrid[ColumnIndex+1,RowIndex+1,0].P.Y := APoint3D.Y;
      FNodeGlGrid[ColumnIndex+1,RowIndex+1,0].P.Z := APoint3D.Z;

      APoint3D.Z := ElevBelow;

      FNodeGlGrid[ColumnIndex+1,RowIndex+1,LayerCount + 2].P.X := APoint3D.X;
      FNodeGlGrid[ColumnIndex+1,RowIndex+1,LayerCount + 2].P.Y := APoint3D.Y;
      FNodeGlGrid[ColumnIndex+1,RowIndex+1,LayerCount + 2].P.Z := APoint3D.Z;

      for LayerIndex := 0 to LayerCount do
      begin
        APoint2D := TwoDElementCorner(ColumnIndex, RowIndex);
        APoint3D.X := APoint2D.x;
        APoint3D.Y := APoint2D.y;
        APoint3D.Z := CellElevation[ColumnIndex,RowIndex,LayerIndex];

        FNodeGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+1].P.X := APoint3D.X;
        FNodeGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+1].P.Y := APoint3D.Y;
        FNodeGlGrid[ColumnIndex+1,RowIndex+1,LayerIndex+1].P.Z := APoint3D.Z;

        if ColumnIndex = 0 then
        begin
          APoint2D.x := ColumnPosition[0]
            + (ColumnPosition[0] - ColumnPosition[1])/2;
          APoint2D.y := RowPosition[RowIndex];
          APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

          FNodeGlGrid[ColumnIndex,RowIndex+1,LayerIndex+1].P.X := APoint2D.X;
          FNodeGlGrid[ColumnIndex,RowIndex+1,LayerIndex+1].P.Y := APoint2D.Y;
          FNodeGlGrid[ColumnIndex,RowIndex+1,LayerIndex+1].P.Z := APoint3D.Z;
        end;
        if ColumnIndex = ColumnCount then
        begin
          APoint2D.x := ColumnPosition[ColumnCount]
            + (ColumnPosition[ColumnCount] - ColumnPosition[ColumnCount-1])/2;
          APoint2D.y := RowPosition[RowIndex];
          APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

          FNodeGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+1].P.X := APoint2D.X;
          FNodeGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+1].P.Y := APoint2D.Y;
          FNodeGlGrid[ColumnIndex+2,RowIndex+1,LayerIndex+1].P.Z := APoint3D.Z;
        end;

        if RowIndex = 0 then
        begin
          APoint2D.x := ColumnPosition[ColumnIndex];
          APoint2D.y := RowPosition[0] + (RowPosition[0] - RowPosition[1])/2;
          APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

          FNodeGlGrid[ColumnIndex+1,RowIndex,LayerIndex+1].P.X := APoint2D.X;
          FNodeGlGrid[ColumnIndex+1,RowIndex,LayerIndex+1].P.Y := APoint2D.Y;
          FNodeGlGrid[ColumnIndex+1,RowIndex,LayerIndex+1].P.Z := APoint3D.Z;
        end;
        if RowIndex = RowCount then
        begin
          APoint2D.x := ColumnPosition[ColumnIndex];
          APoint2D.y := RowPosition[RowCount]
            + (RowPosition[RowCount] - RowPosition[RowCount-1])/2;
          APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

          FNodeGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+1].P.X := APoint2D.X;
          FNodeGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+1].P.Y := APoint2D.Y;
          FNodeGlGrid[ColumnIndex+1,RowIndex+2,LayerIndex+1].P.Z := APoint3D.Z;
        end;

        if (ColumnIndex = 0) and (RowIndex = 0) then
        begin
          APoint2D.x := ColumnPosition[0]
            + (ColumnPosition[0] - ColumnPosition[1])/2;
          APoint2D.y := RowPosition[0] + (RowPosition[0] - RowPosition[1])/2;
          APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

          FNodeGlGrid[ColumnIndex,RowIndex,LayerIndex+1].P.X := APoint2D.X;
          FNodeGlGrid[ColumnIndex,RowIndex,LayerIndex+1].P.Y := APoint2D.Y;
          FNodeGlGrid[ColumnIndex,RowIndex,LayerIndex+1].P.Z := APoint3D.Z;
        end;
        if (ColumnIndex = ColumnCount) and (RowIndex = 0) then
        begin
          APoint2D.x := ColumnPosition[ColumnCount]
            + (ColumnPosition[ColumnCount] - ColumnPosition[ColumnCount-1])/2;
          APoint2D.y := RowPosition[0] + (RowPosition[0] - RowPosition[1])/2;
          APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

          FNodeGlGrid[ColumnIndex+2,RowIndex,LayerIndex+1].P.X := APoint2D.X;
          FNodeGlGrid[ColumnIndex+2,RowIndex,LayerIndex+1].P.Y := APoint2D.Y;
          FNodeGlGrid[ColumnIndex+2,RowIndex,LayerIndex+1].P.Z := APoint3D.Z;
        end;
        if (ColumnIndex = 0) and (RowIndex = RowCount) then
        begin
          APoint2D.x := ColumnPosition[0]
            + (ColumnPosition[0] - ColumnPosition[1])/2;
          APoint2D.y := RowPosition[RowCount]
            + (RowPosition[RowCount] - RowPosition[RowCount-1])/2;
          APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

          FNodeGlGrid[ColumnIndex,RowIndex+2,LayerIndex+1].P.X := APoint2D.X;
          FNodeGlGrid[ColumnIndex,RowIndex+2,LayerIndex+1].P.Y := APoint2D.Y;
          FNodeGlGrid[ColumnIndex,RowIndex+2,LayerIndex+1].P.Z := APoint3D.Z;
        end;
        if (ColumnIndex = ColumnCount) and (RowIndex = RowCount) then
        begin
          APoint2D.x := ColumnPosition[ColumnCount]
            + (ColumnPosition[ColumnCount] - ColumnPosition[ColumnCount-1])/2;
          APoint2D.y := RowPosition[RowCount]
            + (RowPosition[RowCount] - RowPosition[RowCount-1])/2;
          APoint2D := RotateFromGridCoordinatesToRealWorldCoordinates(APoint2D);

          FNodeGlGrid[ColumnIndex+2,RowIndex+2,LayerIndex+1].P.X := APoint2D.X;
          FNodeGlGrid[ColumnIndex+2,RowIndex+2,LayerIndex+1].P.Y := APoint2D.Y;
          FNodeGlGrid[ColumnIndex+2,RowIndex+2,LayerIndex+1].P.Z := APoint3D.Z;
        end;
      end;
    end;
  end;
  for ColumnIndex := 0 to ColumnCount do
  begin
    for RowIndex := 0 to RowCount do
    begin
      for LayerIndex := 0 to LayerCount do
      begin
        FNodeGlGrid[ColumnIndex,RowIndex,LayerIndex].Value := 0;
      end;
    end;
  end;
end;

function TCustomGrid.ContourGrid(EvaluatedAt: TEvaluatedAt;
  ModelSelection: TModelSelection; ViewDirection: TViewDirection;
  ColRowOrLayer: integer): T2DGrid;
var
  Local3DGridPoints: TGrid;
  ColIndex, RowIndex, LayIndex: integer;
begin
  result := nil;
  if (ColumnCount <= 0) or (RowCount <= 0) or (LayerCount <= 0) then
  begin
    Exit;
  end;
  case ViewDirection of
    vdTop:
      begin
        case EvaluatedAt of
          eaBlocks:
            result := FTopElementContourGrid;
          eaNodes:
            result := FTopNodeContourGrid;
          else Assert(False);
        end;
      end;
    vdFront:
      begin
        case EvaluatedAt of
          eaBlocks:
            result := FFrontElementContourGrid;
          eaNodes:
            result := FFrontNodeContourGrid;
          else Assert(False);
        end;
      end;
    vdSide:
      begin
        case EvaluatedAt of
          eaBlocks:
            result := FSideElementContourGrid;
          eaNodes:
            result := FSideNodeContourGrid;
          else Assert(False);
        end;
      end;
    else Assert(False);
  end;
  if result = nil then
  begin
    Local3DGridPoints := PrivateGlGrid(EvaluatedAt, ModelSelection);
    case ViewDirection of
      vdTop:
        begin
          LayIndex := ColRowOrLayer+1;
          SetLength(result,Length(Local3DGridPoints),
            Length(Local3DGridPoints[0]));
          for ColIndex := 0 to Length(Local3DGridPoints)-1 do
          begin
            for RowIndex := 0 to Length(Local3DGridPoints[0])-1 do
            begin
              result[ColIndex,RowIndex].P.x :=
                Local3DGridPoints[ColIndex,RowIndex,LayIndex].P.X;
              result[ColIndex,RowIndex].P.y :=
                Local3DGridPoints[ColIndex,RowIndex,LayIndex].P.Y;
            end;
          end;
        case EvaluatedAt of
          eaBlocks:
            FTopElementContourGrid := result;
          eaNodes:
            FTopNodeContourGrid := result;
          else Assert(False);
        end;
        end;
      vdFront:
        begin
          RowIndex := ColRowOrLayer+1;
          SetLength(result,Length(Local3DGridPoints),
            Length(Local3DGridPoints[0,0]));
          for ColIndex := 0 to Length(Local3DGridPoints)-1 do
          begin
            for LayIndex := 0 to Length(Local3DGridPoints[0,0])-1 do
            begin
              result[ColIndex,LayIndex].P.x :=
                Local3DGridPoints[ColIndex,RowIndex,LayIndex].P.X;
              result[ColIndex,LayIndex].P.y :=
                Local3DGridPoints[ColIndex,RowIndex,LayIndex].P.Z;
            end;
          end;
          case EvaluatedAt of
            eaBlocks:
              FFrontElementContourGrid := result;
            eaNodes:
              FFrontNodeContourGrid := result;
            else Assert(False);
          end;
        end;
      vdSide:
        begin
          ColIndex := ColRowOrLayer+1;
          SetLength(result,Length(Local3DGridPoints[0]),
            Length(Local3DGridPoints[0,0]));
          for RowIndex := 0 to Length(Local3DGridPoints[0])-1 do
          begin
            for LayIndex := 0 to Length(Local3DGridPoints[0,0])-1 do
            begin
              result[RowIndex,LayIndex].P.x :=
                Local3DGridPoints[ColIndex,RowIndex,LayIndex].P.Z;
              result[RowIndex,LayIndex].P.y :=
                Local3DGridPoints[ColIndex,RowIndex,LayIndex].P.Y;
            end;
          end;
          case EvaluatedAt of
            eaBlocks:
              FSideElementContourGrid := result;
            eaNodes:
              FSideNodeContourGrid := result;
            else Assert(False);
          end;
        end;
      else Assert(False);
    end;
    case EvaluatedAt of
      eaBlocks: CacheBlockGlGrid;
      eaNodes: CacheNodeGlGrid;
      else Assert(False);
    end;
  end;
end;

function TCustomGrid.GlGrid(EvaluatedAt: TEvaluatedAt;
  ModelSelection: TModelSelection): TGrid;
begin
  result := nil;
  if (ColumnCount <= 0) or (RowCount <= 0) or (LayerCount <= 0) then
  begin
    Exit;
  end;
  Result := PrivateGlGrid(EvaluatedAt, ModelSelection);
  if Result <> nil then
  begin
    // make a copy of the cached result.
    SetLength(result, Length(result), Length(result[0]), Length(result[0,0]));
    case EvaluatedAt of
      eaBlocks: CacheBlockGlGrid;
      eaNodes: CacheNodeGlGrid;
      else Assert(False);
    end;
  end;
end;

function TCustomGrid.PrivateGlGrid(EvaluatedAt: TEvaluatedAt;
  ModelSelection: TModelSelection): TGrid;
begin
  result := nil;
  case EvaluatedAt of
    eaBlocks:
      begin
        if FBlockGlGrid = nil then
        begin
          CreateBlockGlGrid;
        end;
        result := FBlockGlGrid;
      end;
    eaNodes:
      begin
        if ModelSelection = msPhast then
        begin
          if FNodeGlGrid = nil then
          begin
            CreateNodeGlGrid;
          end;
          result := FNodeGlGrid;
        end;
      end;
    else
      Assert(False);
  end;
end;

function TCustomGrid.GetColWidths: TOneDRealArray;
begin
  result := GetWidths(FColumnPositions);
end;

function TCustomGrid.GetRowPosition(const Row: integer): real;
begin
  if (Row < 0) or (Row > RowCount) then
  begin
    raise EInvalidGrid.Create('Invalid row number');
  end;
  result := FRowPositions[Row];
end;

function TCustomGrid.GetRowWidth(const Row: integer): real;
begin
  result := RowPosition[Row + 1] - RowPosition[Row];
  if RowDirection = rdNorthToSouth then
  begin
    result := -result;
  end;
end;

function TCustomGrid.GetRowWidths: TOneDRealArray;
begin
  result := GetWidths(FRowPositions);
end;

function TCustomGrid.GetTopCellColors(const Column, Row: integer): TColor;
var
  ColumnLimit, RowLimit: integer;
begin
  if TopDataSet = nil then
  begin
    ColumnLimit := ColumnCount;
    RowLimit := RowCount;
  end
  else
  begin
    case TopDataSet.EvaluatedAt of
      eaBlocks:
        begin
          ColumnLimit := ColumnCount;
          RowLimit := RowCount;
        end;
      eaNodes:
        begin
          ColumnLimit := ColumnCount + 1;
          RowLimit := RowCount + 1;
        end;
    else
      begin
        ColumnLimit := -1;
        RowLimit := -1;
        Assert(False);
      end;
    end;
  end;
  if (Column < 0) or (Column >= ColumnLimit) then
  begin
    raise EInvalidGrid.Create('Invalid column number');
  end;
  if (Row < 0) or (Row >= RowLimit) then
  begin
    raise EInvalidGrid.Create('Invalid row number');
  end;
  Result := FTopCellColors[Column, Row];

end;

function TCustomGrid.NearestColumnPosition(const APosition: real;
  const First: integer = -1; const Last: integer = -1): integer;
begin
  result := NearestColumnOrRow(FColumnPositions, APosition, First, Last);
end;

function TCustomGrid.NearestColumnOrRow(const Positions: TOneDRealArray;
  const APosition: real; const First: integer = -1;
  const Last: integer = -1): integer;
var
  Below, Above, Middle: integer;
  Reversed: boolean;
begin
  if Length(Positions) <= 0 then
  begin
    result := -1;
  end
  else if Length(Positions) = 1 then
  begin
    result := 0;
  end
  else
  begin
    Above := Length(Positions) - 1;
    Reversed := Positions[0] > Positions[Above];
    if Reversed then
    begin
      if APosition < Positions[Above] then
      begin
        result := Above + 1;
        Exit;
      end
      else if APosition > Positions[0] then
      begin
        result := -1;
        Exit;
      end;
      if (Last >= 0) and (Above > Last) then
      begin
        Above := Last;
      end;

      Below := 0;
      if (First >= 0) and (Below < First) then
      begin
        Below := First;
      end;
      if Above < Below then
      begin
        result := -1;
        Exit;
      end;
      while Above - Below > 1 do
      begin
        Middle := (Above + Below) div 2;
        if Positions[Middle] < APosition then
        begin
          Above := Middle;
        end
        else
        begin
          Below := Middle;
        end;
      end;
      if Abs(Positions[Below] - APosition)
        < Abs(Positions[Above] - APosition) then
      begin
        result := Below;
      end
      else
      begin
        result := Above;
      end;
    end
    else
    begin
      if APosition < Positions[0] then
      begin
        result := -1;
        Exit;
      end
      else if APosition > Positions[Above] then
      begin
        result := Above + 1;
        Exit;
      end;
      if (Last >= 0) and (Above > Last) then
      begin
        Above := Last;
      end;

      Below := 0;
      if (First >= 0) and (Below < First) then
      begin
        Below := First;
      end;
      if Above < Below then
      begin
        result := -1;
        Exit;
      end;
      while Above - Below > 1 do
      begin
        Middle := (Above + Below) div 2;
        if Positions[Middle] > APosition then
        begin
          Above := Middle;
        end
        else
        begin
          Below := Middle;
        end;
      end;
      if Abs(Positions[Below] - APosition)
        < Abs(Positions[Above] - APosition) then
      begin
        result := Below;
      end
      else
      begin
        result := Above;
      end;
    end;

  end;
end;

function TCustomGrid.NearestRowPosition(const APosition: real;
  const First: integer = -1; const Last: integer = -1): integer;
begin
  result := NearestColumnOrRow(FRowPositions, APosition, First, Last);
end;

function TCustomGrid.RotateFromGridCoordinatesToRealWorldCoordinates(
  const APoint: TPoint2D): TPoint2D;
var
  temp: TPoint2D;
begin
  result := APoint;
  if GridAngle <> 0 then
  begin
    temp.X := Cos(GridAngle) * result.X - Sin(GridAngle) * result.Y;
    temp.Y := Sin(GridAngle) * result.X + Cos(GridAngle) * result.Y;
    result := temp;
  end;
end;

function TCustomGrid.RotateFromRealWorldCoordinatesToGridCoordinates(
  const APoint: TPoint2D): TPoint2D;
var
  temp: TPoint2D;
begin
  result := APoint;
  if GridAngle <> 0 then
  begin
    temp.X := Cos(-GridAngle) * result.X - Sin(-GridAngle) * result.Y;
    temp.Y := Sin(-GridAngle) * result.X + Cos(-GridAngle) * result.Y;
    result := temp;
  end;
end;

procedure TCustomGrid.SetCanDraw(const Value: boolean);
begin
  FCanDraw := Value;
end;

procedure TCustomGrid.SetColumnCount(const Value: integer);
var
  Index: integer;
  LastValue: real;
begin
  if FColumnCount <> Value then
  begin
    FNeedToRecalculateTopCellColors := True;
    FNeedToRecalculateFrontCellColors := True;
    NeedToRecalculate3DCellColors := True;

    SetLength(FColumnPositions, Value + 1);
    if Value > FColumnCount then
    begin
      if FColumnCount >= 0 then
      begin
        LastValue := FColumnPositions[FColumnCount];
      end
      else
      begin
        LastValue := 0;
      end;

      for Index := FColumnCount + 1 to Value do
      begin
        FColumnPositions[Index] := LastValue;
      end;
    end;
    FColumnCount := Value;
    if SelectedColumn >= FColumnCount then
    begin
      SelectedColumn := FColumnCount - 1;
    end;
    frmGoPhast.PhastModel.InvalidateSegments;
    frmGoPhast.UpdateDataSetDimensions;
    frmGoPhast.PhastModel.InvalidateScreenObjects;
    frmGoPhast.InvalidateModel;
    GridChanged;
    ColumnsChanged;
    ElementCountChanged;
  end;
end;

procedure TCustomGrid.SetColumnDirection(const Value: TColumnDirection);
var
  Index: Integer;
  Temp: real;
begin
  if FColumnDirection <> Value then
  begin
    FNeedToRecalculateTopCellColors := True;
    FNeedToRecalculateFrontCellColors := True;
    NeedToRecalculate3DCellColors := True;
    FColumnDirection := Value;
    if FColumnPositions <> nil then
    begin
      for Index := 0 to ColumnCount div 2 do
      begin
        Temp := FColumnPositions[Index];
        FColumnPositions[Index] := FColumnPositions[ColumnCount - Index];
        FColumnPositions[ColumnCount - Index] := Temp;
      end;
    end;
    if frmGoPhast.PhastModel <> nil then
    begin
      frmGoPhast.PhastModel.InvalidateSegments;
      frmGoPhast.PhastModel.InvalidateScreenObjects;
    end;
    frmGoPhast.InvalidateModel;
    GridChanged;
    ColumnsChanged;
  end;
end;

procedure TCustomGrid.SetColumnPosition(const Column: integer;
  const Value: real);
begin
  if (Column < 0) or (Column > ColumnCount) then
  begin
    raise EInvalidGrid.Create('Invalid column number');
  end;
  if FColumnPositions[Column] <> Value then
  begin
    FNeedToRecalculateTopCellColors := True;
    FNeedToRecalculateFrontCellColors := True;
    NeedToRecalculate3DCellColors := True;
    FColumnPositions[Column] := Value;
    frmGoPhast.PhastModel.InvalidateSegments;
    frmGoPhast.PhastModel.InvalidateScreenObjects;
    frmGoPhast.InvalidateModel;
    GridChanged;
    ColumnsChanged;
  end;
  if ColumnDirection = cdWestToEast then
  begin
    if ((Column > 0) and (Value < FColumnPositions[Column - 1]))
      or ((Column < ColumnCount) and (Value > FColumnPositions[Column + 1]))
        then
    begin
      UpdateColumnPositions;
    end;
  end
  else
  begin
    if ((Column > 0) and (Value > FColumnPositions[Column - 1]))
      or ((Column < ColumnCount) and (Value < FColumnPositions[Column + 1]))
        then
    begin
      UpdateColumnPositions;
    end;
  end;
end;

procedure TCustomGrid.SetColumnPositions(const Value: TOneDRealArray);
var
  Index: integer;
begin
  if SelectedColumn >  Length(Value) - 2 then
  begin
    SelectedColumn := Length(Value) - 2;
  end;

  SetLength(FColumnPositions, Length(Value));
  for Index := 0 to Length(Value) - 1 do
  begin
    FColumnPositions[Index] := Value[Index];
  end;

  UpdateColumnPositions;
  frmGoPhast.PhastModel.InvalidateScreenObjects;
  GridChanged;
  ColumnsChanged;
end;

procedure TCustomGrid.SetColumnWidth(const Column: integer;
  const Value: real);
var
  Delta: real;
  Index: integer;
begin
  if Value < 0 then
  begin
    raise
      EInvalidGrid.Create('Column widths must be greater than or equal to 0.');
  end;
  Delta := ColumnWidth[Column] - Value;
  for Index := Column + 1 to ColumnCount do
  begin
    if ColumnDirection = cdWestToEast then
    begin
      FColumnPositions[Index] := FColumnPositions[Index] - Delta;
    end
    else
    begin
      FColumnPositions[Index] := FColumnPositions[Index] + Delta;
    end;
  end;
  frmGoPhast.PhastModel.InvalidateSegments;
  frmGoPhast.PhastModel.InvalidateScreenObjects;
  frmGoPhast.InvalidateModel;
  FNeedToRecalculateTopCellColors := True;
  FNeedToRecalculateFrontCellColors := True;
  NeedToRecalculate3DCellColors := True;
  GridChanged;
  ColumnsChanged;
end;

procedure TCustomGrid.SetDisplayColumn(Value: integer);
begin
  if Value < 0 then
  begin
    Value := 0;
  end
  else if Value > ColumnCount then
  begin
    Value := ColumnCount;
  end;
  if FDisplayColumn <> Value then
  begin
    SelectedColumn := Value;
    FDisplayColumn := Value;
    FRecordedSideGrid := False;
    if Assigned(FOnSelectedColumnChange) then
    begin
      FOnSelectedColumnChange(self);
    end;
  end;
end;

procedure TCustomGrid.SetDisplayLayer(Value: integer);
begin
  if Value < 0 then
  begin
    Value := 0;
  end
  else if Value > LayerCount then
  begin
    Value := LayerCount;
  end;
  if FDisplayLayer <> Value then
  begin
    SelectedLayer := Value;
    FDisplayLayer := Value;
    FRecordedTopGrid := False;
    if Assigned(FOnSelectedLayerChange) then
    begin
      FOnSelectedLayerChange(self);
    end;
  end;
end;

procedure TCustomGrid.SetDisplayRow(Value: integer);
begin
  if Value < 0 then
  begin
    Value := 0;
  end
  else if Value > RowCount then
  begin
    Value := RowCount;
  end;
  if FDisplayRow <> Value then
  begin
    SelectedRow := Value;
    FDisplayRow := Value;
    FRecordedFrontGrid := False;
    if Assigned(FOnSelectedRowChange) then
    begin
      FOnSelectedRowChange(self);
    end;
  end;
end;

procedure TCustomGrid.SetDrawInteriorGridLines2D(const Value: boolean);
begin
  FDrawInteriorGridLines2D := Value;
end;

procedure TCustomGrid.SetGridAngle(Value: real);
var
  Index: integer;
  GridCenter: TPoint2D;
  TrueCenter: TPoint2D;
begin
  while Value > Pi do
  begin
    Value := Value - 2 * Pi;
  end;
  while Value < -Pi do
  begin
    Value := Value + 2 * Pi;
  end;
  if FGridAngle <> Value then
  begin
    FNeedToRecalculateTopCellColors := True;
    FNeedToRecalculateFrontCellColors := True;
    FNeedToRecalculatesideCellColors := True;
    NeedToRecalculate3DCellColors := True;
    frmGoPhast.PhastModel.InvalidateSegments;
    frmGoPhast.InvalidateModel;
    //  Find coordinates of center in grid coordinates.
    if ColumnCount >= 0 then
    begin
      GridCenter.X := (FColumnPositions[0] +
        FColumnPositions[Length(FColumnPositions) - 1]) / 2;
    end
    else
    begin
      GridCenter.X := 0;
    end;

    if RowCount >= 0 then
    begin
      GridCenter.Y := (FRowPositions[0] +
        FRowPositions[Length(FRowPositions) - 1]) / 2;
    end
    else
    begin
      GridCenter.Y := 0;
    end;

    // convert to true coordinates.
    TrueCenter := RotateFromGridCoordinatesToRealWorldCoordinates(GridCenter);

    // Off set all positions so that center is at (0,0).
    for Index := 0 to Length(FColumnPositions) - 1 do
    begin
      FColumnPositions[Index] := FColumnPositions[Index] - GridCenter.X;
    end;

    for Index := 0 to Length(FRowPositions) - 1 do
    begin
      FRowPositions[Index] := FRowPositions[Index] - GridCenter.Y;
    end;

    // change grid angle
    FGridAngle := Value;

    // find coordinates of (old) center in new grid coordinates
    GridCenter := RotateFromRealWorldCoordinatesToGridCoordinates(TrueCenter);

    // Offset all positions to grid center;
    for Index := 0 to Length(FColumnPositions) - 1 do
    begin
      FColumnPositions[Index] := FColumnPositions[Index] + GridCenter.X;
    end;

    for Index := 0 to Length(FRowPositions) - 1 do
    begin
      FRowPositions[Index] := FRowPositions[Index] + GridCenter.Y;
    end;

    frmGoPhast.dcMoveColCursor.RedrawCursor;
    frmGoPhast.dcMoveRowCursor.RedrawCursor;
    frmGoPhast.dcAddColCursor.RedrawCursor;
    frmGoPhast.dcAddRowCursor.RedrawCursor;
    frmGoPhast.dcSubdivide.RedrawCursor;
    frmGoPhast.dcSetSpacing.RedrawCursor;
    frmGoPhast.PhastModel.InvalidateScreenObjects;
    GridChanged;
    ColumnsChanged;
    RowsChanged;
  end;
end;

procedure TCustomGrid.SetLayerCount(const Value: integer);
begin
  if FLayerCount <> Value then
  begin
    FLayerCount := Value;
    FNeedToRecalculateFrontCellColors := True;
    FNeedToRecalculateSideCellColors := True;
    NeedToRecalculate3DCellColors := True;
    frmGoPhast.PhastModel.InvalidateSegments;
    frmGoPhast.PhastModel.InvalidateScreenObjects;
    frmGoPhast.InvalidateModel;
    if SelectedLayer >= FLayerCount then
    begin
      SelectedLayer := FLayerCount - 1;
    end;
    frmGoPhast.UpdateDataSetDimensions;
    GridChanged;
    LayersChanged;
    ElementCountChanged;
  end;
end;

procedure TCustomGrid.SetLayerDirection(const Value: TLayerDirection);
begin
  if FLayerDirection <> Value then
  begin
    FLayerDirection := Value;
    FNeedToRecalculateFrontCellColors := True;
    FNeedToRecalculateSideCellColors := True;
    NeedToRecalculate3DCellColors := True;
    if frmGoPhast.PhastModel <> nil then
    begin
      frmGoPhast.PhastModel.InvalidateSegments;
      frmGoPhast.PhastModel.InvalidateScreenObjects;
    end;
    frmGoPhast.InvalidateModel;
    GridChanged;
    LayersChanged;
  end;
end;

procedure TCustomGrid.ResetTopCellColors;
var
  ColIndex, RowIndex: integer;
begin
  if (FColumnCount <= 0) or (FRowCount <= 0) then
  begin
    SetLength(FTopCellColors, 0, 0);
    FNeedToRecalculateTopCellColors := False;
    Exit;
  end;
  if TopDataSet <> nil then
  begin
    case TopDataSet.EvaluatedAt of
      eaBlocks:
        begin
          if (Length(FTopCellColors) <> FColumnCount)
            or (Length(FTopCellColors[0]) <> FRowCount) then
          begin
            SetLength(FTopCellColors, FColumnCount, FRowCount);
          end;
          for ColIndex := 0 to FColumnCount - 1 do
          begin
            for RowIndex := 0 to FRowCount - 1 do
            begin
              TopCellColors[ColIndex, RowIndex] := clWhite;
            end;
          end;
        end;
      eaNodes:
        begin
          if (Length(FTopCellColors) <> FColumnCount + 1)
            or (Length(FTopCellColors[0]) <> FRowCount + 1) then
          begin
            SetLength(FTopCellColors, FColumnCount + 1, FRowCount + 1);
          end;
          for ColIndex := 0 to FColumnCount do
          begin
            for RowIndex := 0 to FRowCount do
            begin
              TopCellColors[ColIndex, RowIndex] := clWhite;
            end;
          end;
        end;
    else
      Assert(False);
    end;
  end;

  FNeedToRecalculateTopCellColors := False;
end;

procedure TCustomGrid.SetRowCount(const Value: integer);
var
  Index: integer;
  LastValue: real;
begin
  if FRowCount <> Value then
  begin
    FNeedToRecalculateTopCellColors := True;
    FNeedToRecalculateSideCellColors := True;
    NeedToRecalculate3DCellColors := True;
    frmGoPhast.PhastModel.InvalidateScreenObjects;
    frmGoPhast.PhastModel.InvalidateSegments;
    frmGoPhast.InvalidateModel;
    SetLength(FRowPositions, Value + 1);
    if Value > FRowCount then
    begin
      if FRowCount >= 0 then
      begin
        LastValue := FRowPositions[FRowCount];
      end
      else
      begin
        LastValue := 0;
      end;

      for Index := FRowCount + 1 to Value do
      begin
        FRowPositions[Index] := LastValue;
      end;
    end;
    FRowCount := Value;
    if SelectedRow >= FRowCount then
    begin
      SelectedRow := FRowCount - 1;
    end;
    frmGoPhast.UpdateDataSetDimensions;
    GridChanged;
    RowsChanged;
    ElementCountChanged;
  end;
end;

procedure TCustomGrid.SetRowDirection(const Value: TRowDirection);
var
  Index: integer;
  Temp: real;
begin
  if FRowDirection <> Value then
  begin
    FNeedToRecalculateTopCellColors := True;
    FNeedToRecalculateSideCellColors := True;
    NeedToRecalculate3DCellColors := True;
    FRowDirection := Value;
    if frmGoPhast.PhastModel <> nil then
    begin
      frmGoPhast.PhastModel.InvalidateSegments;
      frmGoPhast.PhastModel.InvalidateScreenObjects;
    end;
    frmGoPhast.InvalidateModel;
    if FRowPositions <> nil then
    begin
      for Index := 0 to RowCount div 2 do
      begin
        Temp := FRowPositions[Index];
        FRowPositions[Index] := FRowPositions[RowCount - Index];
        FRowPositions[RowCount - Index] := Temp;
      end;
    end;
    GridChanged;
    RowsChanged;
  end;
end;

procedure TCustomGrid.SetRowPosition(const Row: integer;
  const Value: real);
begin
  if (Row < 0) or (Row > RowCount) then
  begin
    raise EInvalidGrid.Create('Invalid row number');
  end;
  if FRowPositions[Row] <> Value then
  begin
    FNeedToRecalculateTopCellColors := True;
    FNeedToRecalculateSideCellColors := True;
    NeedToRecalculate3DCellColors := True;
    FRowPositions[Row] := Value;
    frmGoPhast.PhastModel.InvalidateSegments;
    frmGoPhast.PhastModel.InvalidateScreenObjects;
    frmGoPhast.InvalidateModel;
    GridChanged;
    RowsChanged;
  end;
  if (RowDirection = rdSouthToNorth) then
  begin
    if ((Row > 0) and (Value < FRowPositions[Row - 1]))
      or ((Row < RowCount) and (Value > FRowPositions[Row + 1])) then
    begin
      UpdateRowPositions;
    end;
  end
  else
  begin
    if ((Row > 0) and (Value > FRowPositions[Row - 1]))
      or ((Row < RowCount) and (Value < FRowPositions[Row + 1])) then
    begin
      UpdateRowPositions;
    end;
  end;
end;

procedure TCustomGrid.SetRowPositions(const Value: TOneDRealArray);
var
  Index: integer;
begin
  if SelectedRow > Length(Value) - 2 then
  begin
    SelectedRow := Length(Value) - 2;
  end;

  SetLength(FRowPositions, Length(Value));
  for Index := 0 to Length(Value) - 1 do
  begin
    FRowPositions[Index] := Value[Index];
  end;
  UpdateRowPositions;
  frmGoPhast.PhastModel.InvalidateScreenObjects;
  GridChanged;
  RowsChanged;
end;

procedure TCustomGrid.SetRowWidth(const Row: integer; const Value: real);
var
  Delta: real;
  Index: integer;
begin
  if Value < 0 then
  begin
    raise EInvalidGrid.Create('Row widths must be greater than or equal to 0.');
  end;
  FNeedToRecalculateTopCellColors := True;
  FNeedToRecalculateSideCellColors := True;
  NeedToRecalculate3DCellColors := True;
  frmGoPhast.PhastModel.InvalidateScreenObjects;
  frmGoPhast.PhastModel.InvalidateSegments;
  frmGoPhast.InvalidateModel;
  Delta := RowWidth[Row] - Value;
  for Index := Row + 1 to RowCount do
  begin
    if RowDirection = rdSouthToNorth then
    begin
      FRowPositions[Index] := FRowPositions[Index] - Delta;
    end
    else
    begin
      FRowPositions[Index] := FRowPositions[Index] + Delta;
    end;
  end;
  GridChanged;
  RowsChanged;
end;

procedure TCustomGrid.SetTopCellColors(const Column, Row: integer;
  const Value: TColor);
var
  ColumnLimit, RowLimit: integer;
begin
  if TopDataSet = nil then
  begin
    ColumnLimit := ColumnCount;
    RowLimit := RowCount;
  end
  else
  begin
    case TopDataSet.EvaluatedAt of
      eaBlocks:
        begin
          ColumnLimit := ColumnCount;
          RowLimit := RowCount;
        end;
      eaNodes:
        begin
          ColumnLimit := ColumnCount + 1;
          RowLimit := RowCount + 1;
        end;
    else
      begin
        ColumnLimit := -1;
        RowLimit := -1;
        Assert(False);
      end;
    end;
  end;
  if (Column < 0) or (Column >= ColumnLimit) then
  begin
    raise EInvalidGrid.Create('Invalid column number');
  end;
  if (Row < 0) or (Row >= RowLimit) then
  begin
    raise EInvalidGrid.Create('Invalid row number');
  end;
  FTopCellColors[Column, Row] := Value;
end;

procedure TCustomGrid.SetTopContourDataSet(const Value: TDataArray);
begin
  Assert((Value = nil) or (Value.Orientation in [dsoTop, dso3D]));
  if FTopContourDataSet <> Value then
  begin
    FTopContourDataSet := Value;
  end;
end;

function TCustomGrid.ThreeDElementCenter(const Column, Row,
  Layer: integer): T3DRealPoint;
begin
  result.X := (ColumnPosition[Column] + ColumnPosition[Column + 1]) / 2;
  result.Y := (RowPosition[Row] + RowPosition[Row + 1]) / 2;
  result.Z := (CellElevation[Column, Row, Layer]
    + CellElevation[Column, Row, Layer + 1]) / 2;
end;

function TCustomGrid.ThreeDElementCorner(const Column, Row,
  Layer: integer): T3DRealPoint;
begin
  result.X := ColumnPosition[Column];
  result.Y := RowPosition[Row];
  result.Z := CellElevation[Column, Row, Layer];
end;

function TCustomGrid.UnrotatedTwoDElementCenter(const Column,
  Row: integer): TPoint2D;
begin
  result.X := (ColumnPosition[Column] + ColumnPosition[Column + 1]) / 2;
  result.Y := (RowPosition[Row] + RowPosition[Row + 1]) / 2;
end;

function TCustomGrid.TwoDElementCenter(const Column,
  Row: integer): TPoint2D;
begin
  result.X := (ColumnPosition[Column] + ColumnPosition[Column + 1]) / 2;
  result.Y := (RowPosition[Row] + RowPosition[Row + 1]) / 2;
  result := RotateFromGridCoordinatesToRealWorldCoordinates(result);
end;

function TCustomGrid.UnrotatedTwoDElementCorner(const Column,
  Row: integer): TPoint2D;
begin
  result.X := ColumnPosition[Column];
  result.Y := RowPosition[Row];
end;

function TCustomGrid.TwoDElementCorner(const Column,
  Row: integer): TPoint2D;
begin
  result.X := ColumnPosition[Column];
  result.Y := RowPosition[Row];
  result := RotateFromGridCoordinatesToRealWorldCoordinates(result);
end;

procedure TCustomGrid.UpdateColumnPositions;
var
  ARealList: TRealList;
  Index: integer;
begin
  FColumnCount := Length(FColumnPositions) - 1;
  FNeedToRecalculateTopCellColors := True;
  FNeedToRecalculateFrontCellColors := True;
  NeedToRecalculate3DCellColors := True;
  frmGoPhast.PhastModel.InvalidateSegments;
  frmGoPhast.InvalidateModel;
  ARealList := TRealList.Create;
  try
    ARealList.Capacity := ColumnCount + 1;
    if ColumnDirection = cdWestToEast then
    begin
      for Index := 0 to ColumnCount do
      begin
        ARealList.Add(FColumnPositions[Index]);
      end;
    end
    else
    begin
      for Index := ColumnCount downto 0 do
      begin
        ARealList.Add(FColumnPositions[Index]);
      end;
    end;

    ARealList.Sort;

    for Index := ARealList.Count - 1 downto 1 do
    begin
      if ARealList[Index] = ARealList[Index - 1] then
      begin
        ARealList.Delete(Index);
      end;
    end;

    ColumnCount := ARealList.Count - 1;

    if ColumnDirection = cdWestToEast then
    begin
      for Index := 0 to ColumnCount do
      begin
        FColumnPositions[Index] := ARealList[Index];
      end;
    end
    else
    begin
      for Index := ColumnCount downto 0 do
      begin
        FColumnPositions[ColumnCount-Index] := ARealList[Index];
      end;
    end;
    if SelectedColumn < 0 then
    begin
      SelectedColumn := ColumnCount - 1;
    end;
  finally
    ARealList.Free;
  end;
  ElementCountChanged;
end;

procedure TCustomGrid.UpdateRowPositions;
var
  ARealList: TRealList;
  Index: integer;
begin
  FRowCount := Length(FRowPositions) - 1;
  FNeedToRecalculateTopCellColors := True;
  FNeedToRecalculateSideCellColors := True;
  NeedToRecalculate3DCellColors := True;
  frmGoPhast.PhastModel.InvalidateSegments;
  frmGoPhast.InvalidateModel;
  ARealList := TRealList.Create;
  try
    ARealList.Capacity := RowCount + 1;
    if RowDirection = rdSouthToNorth then
    begin
      for Index := 0 to RowCount do
      begin
        ARealList.Add(FRowPositions[Index]);
      end;
    end
    else
    begin
      for Index := RowCount downto 0 do
      begin
        ARealList.Add(FRowPositions[Index]);
      end;
    end;

    ARealList.Sort;

    for Index := ARealList.Count - 1 downto 1 do
    begin
      if ARealList[Index] = ARealList[Index - 1] then
      begin
        ARealList.Delete(Index);
      end;
    end;

    RowCount := ARealList.Count - 1;

    if RowDirection = rdSouthToNorth then
    begin
      for Index := 0 to RowCount do
      begin
        FRowPositions[Index] := ARealList[Index];
      end;
    end
    else
    begin
      for Index := RowCount downto 0 do
      begin
        FRowPositions[RowCount-Index] := ARealList[Index];
      end;
    end;
    if SelectedRow < 0 then
    begin
      SelectedRow := RowCount - 1;
    end;

  finally
    ARealList.Free;
  end;
  ElementCountChanged;
end;

function TCustomGrid.TopContainingCell(APoint: TPoint2D;
  const EvaluatedAt: TEvaluatedAt;
  const NeedToRotatePointToGridCoordinates: boolean = True;
  const BelowCol: integer = -1; const AboveCol: integer = -1;
  const BelowRow: integer = -1; const AboveRow: integer = -1): T2DTopCell;
begin
  if NeedToRotatePointToGridCoordinates then
  begin
    APoint := RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
  end;
  result.Col := NearestColumnPosition(APoint.X);
  result.Row := NearestRowPosition(APoint.Y);
  case EvaluatedAt of
    eaBlocks:
      begin
        // do nothing
      end;
    eaNodes:
      begin
        Exit;
      end;
  else
    Assert(False);
  end;

  if result.Col < 0 then
  begin
    result.Col := 0;
  end;
  if result.Row < 0 then
  begin
    result.Row := 0;
  end;

//  if (result.Col > -1) and (result.Row > -1) then
//  begin
    if result.Col >= ColumnCount then
    begin
      Dec(result.Col);
    end;
    if result.Row >= RowCount then
    begin
      Dec(result.Row);
    end;
    if ColumnDirection = cdWestToEast then
    begin
      if (result.Col > 0) and (ColumnPosition[result.Col] > APoint.X) then
      begin
        Dec(result.Col);
      end;
    end
    else
    begin
      if (result.Col > 0) and (ColumnPosition[result.Col] < APoint.X) then
      begin
        Dec(result.Col);
      end;
    end;
    if RowDirection = rdSouthToNorth then
    begin
      if (result.Row > 0) and (RowPosition[result.Row] > APoint.Y) then
      begin
        Dec(result.Row);
      end;
    end
    else
    begin
      if (result.Row > 0) and (RowPosition[result.Row] < APoint.Y) then
      begin
        Dec(result.Row);
      end;
    end;
//  end;
end;

procedure TCustomGrid.SetNeedToRecalculateTopCellColors(
  const Value: boolean);
begin
  if Value then
  begin
    FNeedToRecalculateTopCellColors := True;
  end;
end;

procedure TCustomGrid.SetOnSelectedLayerChange(const Value: TNotifyEvent);
begin
  FOnSelectedLayerChange := Value;
end;

procedure TCustomGrid.SetSelectedColumn(Value: integer);
begin
  if Value < 0 then
  begin
    Value := 0;
  end
  else
  begin
    case frmGoPhast.frameSideView.EvaluatedAt of
      eaBlocks:
        begin
          if Value > ColumnCount - 1 then
          begin
            Value := ColumnCount - 1;
          end;
        end;
      eaNodes:
        begin
          if Value > ColumnCount then
          begin
            Value := ColumnCount;
          end;
        end;
    else
      Assert(False);
    end;
  end;
  if FSelectedColumn <> Value then
  begin
    FSideElementContourGrid := nil;
    FSideNodeContourGrid := nil;
    FSelectedColumn := Value;
    if not (csLoading in frmGoPhast.PhastModel.ComponentState) then
    begin
      FDisplayColumn := Value;
    end;
    FRecordedSideGrid := False;
    if Assigned(FOnSelectedColumnChange) then
    begin
      FOnSelectedColumnChange(self);
    end;
  end;
end;

procedure TCustomGrid.SetSelectedLayer(Value: integer);
begin
  if Value < 0 then
  begin
    Value := 0;
  end
  else
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
  if FSelectedLayer <> Value then
  begin
    FSelectedLayer := Value;
    if not (csLoading in frmGoPhast.PhastModel.ComponentState) then
    begin
      FDisplayLayer := Value;
    end;
    FRecordedTopGrid := False;
    if Assigned(FOnSelectedLayerChange) then
    begin
      FOnSelectedLayerChange(self);
    end;
  end;
end;

procedure TCustomGrid.SetSelectedRow(Value: integer);
begin
  if Value < 0 then
  begin
    Value := 0;
  end
  else
  begin
    case frmGoPhast.frameFrontView.EvaluatedAt of
      eaBlocks:
        begin
          if Value > RowCount - 1 then
          begin
            Value := RowCount - 1;
          end;
        end;
      eaNodes:
        begin
          if Value > RowCount then
          begin
            Value := RowCount;
          end;
        end;
    else
      Assert(False);
    end;
  end;
  if FSelectedRow <> Value then
  begin
    FFrontElementContourGrid := nil;
    FFrontNodeContourGrid := nil;
    FSelectedRow := Value;
    if not (csLoading in frmGoPhast.PhastModel.ComponentState) then
    begin
      FDisplayRow := Value;
    end;
    FRecordedFrontGrid := False;
    if Assigned(FOnSelectedRowChange) then
    begin
      FOnSelectedRowChange(self);
    end;
  end;
end;

procedure TCustomGrid.SetNeedToRecalculateFrontCellColors(
  const Value: boolean);
begin
  if Value then
  begin
    FNeedToRecalculateFrontCellColors := True;
  end;
end;

function TCustomGrid.GetFrontCellColors(const Column,
  Layer: integer): TColor;
var
  ColumnLimit, LayerLimit: integer;
begin
  if FrontDataSet = nil then
  begin
    ColumnLimit := ColumnCount;
    LayerLimit := LayerCount;
  end
  else
  begin
    case FrontDataSet.EvaluatedAt of
      eaBlocks:
        begin
          ColumnLimit := ColumnCount;
          LayerLimit := LayerCount;
        end;
      eaNodes:
        begin
          ColumnLimit := ColumnCount + 1;
          LayerLimit := LayerCount + 1;
        end;
    else
      begin
        ColumnLimit := -1;
        LayerLimit := -1;
        Assert(False);
      end;
    end;
  end;
  if (Column < 0) or (Column >= ColumnLimit) then
  begin
    raise EInvalidGrid.Create('Invalid column number');
  end;
  if (Layer < 0) or (Layer >= LayerLimit) then
  begin
    raise EInvalidGrid.Create('Invalid layer number');
  end;
  Result := FFrontCellColors[Column, Layer];
end;

function TCustomGrid.GridLimits(ViewDirection: TViewDirection): TLimit;
var
  ACorner: TPoint2D;
begin
  if ViewDirection = vdTop then
  begin
    ACorner := TwoDElementCorner(0,0);
    result.MinX := ACorner.x;
    result.MaxX := ACorner.x;
    result.MinY := ACorner.y;
    result.MaxY := ACorner.y;
    ACorner := TwoDElementCorner(ColumnCount,0);
    result.MinX := Min(result.MinX, ACorner.x);
    result.MaxX := Max(result.MaxX, ACorner.x);
    result.MinY := Min(result.MinY, ACorner.y);
    result.MaxY := Max(result.MaxY, ACorner.y);
    ACorner := TwoDElementCorner(ColumnCount,RowCount);
    result.MinX := Min(result.MinX, ACorner.x);
    result.MaxX := Max(result.MaxX, ACorner.x);
    result.MinY := Min(result.MinY, ACorner.y);
    result.MaxY := Max(result.MaxY, ACorner.y);
    ACorner := TwoDElementCorner(0,RowCount);
    result.MinX := Min(result.MinX, ACorner.x);
    result.MaxX := Max(result.MaxX, ACorner.x);
    result.MinY := Min(result.MinY, ACorner.y);
    result.MaxY := Max(result.MaxY, ACorner.y);
  end
  else
  begin
    if ViewDirection = vdFront then
    begin
      case ColumnDirection of
        cdWestToEast:
          begin
            result.MinX := ColumnPosition[0];
            result.MaxX := ColumnPosition[ColumnCount];
          end;
        cdEastToWest:
          begin
            result.MinX := ColumnPosition[ColumnCount];
            result.MaxX := ColumnPosition[0];
          end;
      end;
    end
    else
    begin
      result.MinX := 0;
      result.MaxX := 0;
    end;
    if ViewDirection = vdSide then
    begin
      case RowDirection of
        rdSouthToNorth:
          begin
            result.MinY := RowPosition[0];
            result.MaxY := RowPosition[RowCount];
          end;
        rdNorthToSouth:
          begin
            result.MinY := RowPosition[RowCount];
            result.MaxY := RowPosition[0];
          end;
      end;
    end
    else
    begin
      result.MinY := 0;
      result.MaxY := 0;
    end;
    result.MinZ := LowestElevation;
    result.MaxZ := HighestElevation;
  end;
end;

function TCustomGrid.GridOutline(ViewDirection: TViewDirection): TPolygon2D;
begin
  SetLength(result, 4);
  case ViewDirection of
    vdTop:
      begin
        result[0] := TwoDElementCorner(0,0);
        result[1] := TwoDElementCorner(ColumnCount,0);
        result[2] := TwoDElementCorner(ColumnCount,RowCount);
        result[3] := TwoDElementCorner(0,RowCount);
      end;
    vdFront:
      begin
        result[0].x := ColumnPosition[0];
        result[0].y := HighestElevation;
        result[1].x := ColumnPosition[ColumnCount];
        result[1].y := HighestElevation;
        result[2].x := ColumnPosition[ColumnCount];
        result[2].y := LowestElevation;
        result[3].x := ColumnPosition[0];
        result[3].y := LowestElevation;
      end;
    vdSide:
      begin
        result[0].x := HighestElevation;
        result[0].y := RowPosition[0];
        result[1].x := HighestElevation;
        result[1].y := RowPosition[RowCount];
        result[2].x := LowestElevation;
        result[2].y := RowPosition[RowCount];
        result[3].x := LowestElevation;
        result[3].y := RowPosition[0];
      end;
  end;
end;

procedure TCustomGrid.RestoreGlGrid(var AGlGrid: TGrid; GridCacheStream: TMemoryStream);
var
  RowIndex: Integer;
  ColIndex: Integer;
  LayerCount: Integer;
  RowCount: Integer;
  ColCount: Integer;
  DecompressionStream: TDecompressionStream;
begin
  Assert(GridCacheStream <> nil);
  GridCacheStream.Position := 0;
  DecompressionStream := TDecompressionStream.Create(GridCacheStream);
  try
    DecompressionStream.Read(ColCount, SizeOf(ColCount));
    DecompressionStream.Read(RowCount, SizeOf(RowCount));
    DecompressionStream.Read(LayerCount, SizeOf(LayerCount));
    SetLength(AGlGrid, ColCount, RowCount, LayerCount);
    for ColIndex := 0 to ColCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        DecompressionStream.Read(AGlGrid[ColIndex, RowIndex, 0], LayerCount * SizeOf(TGridPoint));
      end;
    end;
  finally
    DecompressionStream.Free;
  end;
end;

procedure TCustomGrid.CacheGlGrid(GridCacheStream: TMemoryStream;
  var AGlGrid: TGrid);
var
  RowIndex: Integer;
  ColIndex: Integer;
  Compressor: TCompressionStream;
  LayerCount: Integer;
  RowCount: Integer;
  ColCount: Integer;
begin
  Assert(AGlGrid <> nil);
  ColCount := Length(AGlGrid);
  RowCount := Length(AGlGrid[0]);
  LayerCount := Length(AGlGrid[0, 0]);
  Compressor := TCompressionStream.Create(clDefault, GridCacheStream);
  try
    Compressor.Write(ColCount, SizeOf(ColCount));
    Compressor.Write(RowCount, SizeOf(RowCount));
    Compressor.Write(LayerCount, SizeOf(LayerCount));
    for ColIndex := 0 to ColCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        Compressor.Write(AGlGrid[ColIndex, RowIndex, 0], LayerCount * SizeOf(TGridPoint));
      end;
    end;
  finally
    Compressor.Free;
  end;
  AGlGrid := nil;
end;

procedure TCustomGrid.GetLimits(const EvaluatedAt: TEvaluatedAt;
  const ViewDirection: TViewDirection; out Limit1, Limit2: integer);
begin
  // Get the limits of one of the sparse 2D
  // elevation arrays in a TScreenObject.
  // Col, Row limits for top view
  // Col, Lay limits for front view
  // Row, Lay limits for side view
  case ViewDirection of
    vdTop:
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              Limit1 := ColumnCount-1;
              Limit2 := RowCount-1;
            end;
          eaNodes:
            begin
              Limit1 := ColumnCount;
              Limit2 := RowCount;
            end;
        else
          Assert(False);
        end;
      end;
    vdFront:
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              Limit1 := ColumnCount-1;
              Limit2 := LayerCount-1;
            end;
          eaNodes:
            begin
              Limit1 := ColumnCount;
              Limit2 := LayerCount;
            end;
        else
          Assert(False);
        end;
      end;
    vdSide:
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              Limit1 := RowCount-1;
              Limit2 := LayerCount-1;
            end;
          eaNodes:
            begin
              Limit1 := RowCount;
              Limit2 := LayerCount;
            end;
        else
          Assert(False);
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TCustomGrid.SetFrontCellColors(const Column, Layer: integer;
  const Value: TColor);
var
  ColumnLimit, LayerLimit: integer;
begin
  if FrontDataSet = nil then
  begin
    ColumnLimit := ColumnCount;
    LayerLimit := LayerCount;
  end
  else
  begin
    case FrontDataSet.EvaluatedAt of
      eaBlocks:
        begin
          ColumnLimit := ColumnCount;
          LayerLimit := LayerCount;
        end;
      eaNodes:
        begin
          ColumnLimit := ColumnCount + 1;
          LayerLimit := LayerCount + 1;
        end;
    else
      begin
        ColumnLimit := -1;
        LayerLimit := -1;
        Assert(False);
      end;
    end;
  end;
  if (Column < 0) or (Column >= ColumnLimit) then
  begin
    raise EInvalidGrid.Create('Invalid column number');
  end;
  if (Layer < 0) or (Layer >= LayerLimit) then
  begin
    raise EInvalidGrid.Create('Invalid layer number');
  end;
  FFrontCellColors[Column, Layer] := Value;
end;

procedure TCustomGrid.SetFrontContourDataSet(const Value: TDataArray);
begin
  Assert((Value = nil) or (Value.Orientation in [dsoFront, dso3D]));
  if FFrontContourDataSet <> Value then
  begin
    FFrontContourDataSet := Value;
  end;
end;

procedure TCustomGrid.ResetFrontCellColors;
var
  ColIndex, LayerIndex: integer;
begin
  if (FColumnCount <= 0) or (FLayerCount <= 0) then
  begin
    SetLength(FFrontCellColors, 0, 0);
    FNeedToRecalculateFrontCellColors := False;
    Exit;
  end;

  if FrontDataSet <> nil then
  begin
    case FrontDataSet.EvaluatedAt of
      eaBlocks:
        begin
          if (Length(FFrontCellColors) <> FColumnCount)
            or (Length(FFrontCellColors[0]) <> FLayerCount) then
          begin
            SetLength(FFrontCellColors, FColumnCount, FLayerCount);
          end;
          for ColIndex := 0 to FColumnCount - 1 do
          begin
            for LayerIndex := 0 to FLayerCount - 1 do
            begin
              FrontCellColors[ColIndex, LayerIndex] := clWhite;
            end;
          end;
        end;
      eaNodes:
        begin
          if (Length(FFrontCellColors) <> FColumnCount + 1)
            or (Length(FFrontCellColors[0]) <> FLayerCount + 1) then
          begin
            SetLength(FFrontCellColors, FColumnCount + 1, FLayerCount + 1);
          end;
          for ColIndex := 0 to FColumnCount do
          begin
            for LayerIndex := 0 to FLayerCount do
            begin
              FrontCellColors[ColIndex, LayerIndex] := clWhite;
            end;
          end;
        end;
    else
      Assert(False);
    end;
  end;

  FNeedToRecalculateFrontCellColors := False;
end;

procedure TCustomGrid.SetNeedToRecalculateSideCellColors(
  const Value: boolean);
begin
  if Value then
  begin
    FNeedToRecalculateSideCellColors := True;
  end;
end;

procedure TCustomGrid.ResetSideCellColors;
var
  RowIndex, LayerIndex: integer;
begin
  if (FRowCount <= 0) or (FLayerCount <= 0) then
  begin
    SetLength(FSideCellColors, 0, 0);
    FNeedToRecalculateSideCellColors := False;
    Exit;
  end;
  if SideDataSet <> nil then
  begin
    case SideDataSet.EvaluatedAt of
      eaBlocks:
        begin
          if (Length(FSideCellColors) <> FRowCount)
            or (Length(FSideCellColors[0]) <> FLayerCount) then
          begin
            SetLength(FSideCellColors, FRowCount, FLayerCount);
          end;
          for RowIndex := 0 to FRowCount - 1 do
          begin
            for LayerIndex := 0 to FLayerCount - 1 do
            begin
              FSideCellColors[RowIndex, LayerIndex] := clWhite;
            end;
          end;
        end;
      eaNodes:
        begin
          if (Length(FSideCellColors) <> FRowCount + 1)
            or (Length(FSideCellColors[0]) <> FLayerCount + 1) then
          begin
            SetLength(FSideCellColors, FRowCount + 1, FLayerCount + 1);
          end;
          for RowIndex := 0 to FRowCount do
          begin
            for LayerIndex := 0 to FLayerCount do
            begin
              FSideCellColors[RowIndex, LayerIndex] := clWhite;
            end;
          end;
        end;
    else
      Assert(False);
    end;
  end;

  FNeedToRecalculateSideCellColors := False;
end;

function TCustomGrid.GetSideCellColors(const Row, Layer: integer): TColor;
var
  RowLimit, LayerLimit: integer;
begin
  if SideDataSet = nil then
  begin
    RowLimit := RowCount;
    LayerLimit := LayerCount;
  end
  else
  begin
    case SideDataSet.EvaluatedAt of
      eaBlocks:
        begin
          RowLimit := RowCount;
          LayerLimit := LayerCount;
        end;
      eaNodes:
        begin
          RowLimit := RowCount + 1;
          LayerLimit := LayerCount + 1;
        end;
    else
      begin
        RowLimit := -1;
        LayerLimit := -1;
        Assert(False);
      end;
    end;
  end;
  if (Row < 0) or (Row >= RowLimit) then
  begin
    raise EInvalidGrid.Create('Invalid row number');
  end;
  if (Layer < 0) or (Layer >= LayerLimit) then
  begin
    raise EInvalidGrid.Create('Invalid layer number');
  end;
  Result := FSideCellColors[Row, Layer];
end;

procedure TCustomGrid.SetSideCellColors(const Row, Layer: integer;
  const Value: TColor);
var
  RowLimit, LayerLimit: integer;
begin
  if SideDataSet = nil then
  begin
    RowLimit := RowCount;
    LayerLimit := LayerCount;
  end
  else
  begin
    case SideDataSet.EvaluatedAt of
      eaBlocks:
        begin
          RowLimit := RowCount;
          LayerLimit := LayerCount;
        end;
      eaNodes:
        begin
          RowLimit := RowCount + 1;
          LayerLimit := LayerCount + 1;
        end;
    else
      begin
        RowLimit := -1;
        LayerLimit := -1;
        Assert(False);
      end;
    end;
  end;
  if (Row < 0) or (Row >= RowLimit) then
  begin
    raise EInvalidGrid.Create('Invalid row number');
  end;
  if (Layer < 0) or (Layer >= LayerLimit) then
  begin
    raise EInvalidGrid.Create('Invalid layer number');
  end;
  FSideCellColors[Row, Layer] := Value;
end;

procedure TCustomGrid.SetSideContourDataSet(const Value: TDataArray);
begin
  Assert((Value = nil) or (Value.Orientation in [dsoSide, dso3D]));
  if FSideContourDataSet <> Value then
  begin
    FSideContourDataSet := Value;
  end;
end;

procedure TCustomGrid.SetFrontDataSet(const Value: TDataArray);
begin
  Assert((Value = nil) or (Value.Orientation in [dsoFront, dso3D]));
  if FFrontDataSet <> Value then
  begin
    FFrontDataSet := Value;
    NeedToRecalculateFrontCellColors := True;

    // This ensures that the selected row is still valid for the new data set.
    frmGoPhast.Grid.SelectedRow := frmGoPhast.Grid.SelectedRow;

    frmGoPhast.frameFrontView.ItemChange(nil);
    frmGoPhast.frameFrontView.ZoomBox.Image32.Invalidate;
  end;
end;

procedure TCustomGrid.SetSideDataSet(const Value: TDataArray);
begin
  Assert((Value = nil) or (Value.Orientation in [dsoSide, dso3D]));
  if FSideDataSet <> Value then
  begin
    FSideDataSet := Value;
    NeedToRecalculateSideCellColors := True;

    // This ensures that the selected column
    // is still valid for the new data set.
    frmGoPhast.Grid.SelectedColumn := frmGoPhast.Grid.SelectedColumn;

    frmGoPhast.frameSideView.ItemChange(nil);
    frmGoPhast.frameSideView.ZoomBox.Image32.Invalidate;
  end;
end;

procedure TCustomGrid.SetTopDataSet(const Value: TDataArray);
begin
  Assert((Value = nil) or (Value.Orientation in [dsoTop, dso3D]));
  if FTopDataSet <> Value then
  begin
    FTopDataSet := Value;
    NeedToRecalculateTopCellColors := True;

    // This ensures that the selected layer
    // is still valid for the new data set.
    frmGoPhast.Grid.SelectedLayer := frmGoPhast.Grid.SelectedLayer;

    frmGoPhast.frameTopView.ItemChange(nil);
    frmGoPhast.frameTopView.ZoomBox.Image32.Invalidate;
  end;
end;

procedure TCustomGrid.SetTopGridObserver(const Value: TObserver);
begin
  FTopGridObserver := Value;
  if Assigned(FTopGridObserver) then
  begin
    FTopGridObserver.UpToDate := False;
    FTopGridObserver.UpToDate := True;
  end;
end;

procedure TCustomGrid.UpdateCellColors(
  const ViewDirection: TViewDirection);
var
  DataSet: TDataArray;
  LayerCount, RowCount, ColCount: integer;
  LayerIndex, RowIndex, ColIndex: integer;
  RMin, RMax, RTemp, RMinPositive: real;
  IMin, IMax, ITemp: integer;
  BMin, BMax, BTemp: boolean;
  SMin, SMax, STemp: string;
  SIndex: integer;
  UseString: boolean;
  StringValues: TStringList;
  TempString: string;
  MinMaxInitialized: boolean;
  LogRMin: double;
  LogRMax: double;
  LogRTemp: double;
  procedure GetCounts;
  begin
    case DataSet.EvaluatedAt of
      eaBlocks:
        begin
          LayerCount := DataSet.LayerCount;
          RowCount := DataSet.RowCount;
          ColCount := DataSet.ColumnCount;
        end;
      eaNodes:
        begin
          case DataSet.Orientation of
            dsoTop:
              begin
                { TODO : This is a clumsy hack. It should be updated. }
                if (DataSet = frmGoPhast.PhastModel.
                  GetDataSetByName(rsInitial_Water_Table)) then
                begin
                  LayerCount := ThreeDDataSet.LayerCount;
                end
                else if
                  ((DataSet is TSparseArrayPhastInterpolationDataSet) and
                  ((frmGoPhast.PhastModel.RiverHead.
                  IndexOfDataSet(TSparseArrayPhastInterpolationDataSet(
                  DataSet)) >= 0)
                  or (frmGoPhast.PhastModel.RiverAssociatedSolution.
                  IndexOfDataSet(TSparseArrayPhastInterpolationDataSet(
                  DataSet)) >= 0)))
                  or (frmGoPhast.PhastModel.RiverDataSets.IndexOf(
                  DataSet) >= 0) then
                begin
                  LayerCount := frmGoPhast.PhastModel.PhastGrid.LayerCount + 1;
                end
                else
                begin
                  LayerCount := DataSet.LayerCount;
                end;
                RowCount := DataSet.RowCount + 1;
                ColCount := DataSet.ColumnCount + 1;
              end;
            dsoFront:
              begin
                LayerCount := DataSet.LayerCount + 1;
                RowCount := DataSet.RowCount;
                ColCount := DataSet.ColumnCount + 1;
              end;
            dsoSide:
              begin
                LayerCount := DataSet.LayerCount + 1;
                RowCount := DataSet.RowCount + 1;
                ColCount := DataSet.ColumnCount;
              end;
            dso3D:
              begin
                LayerCount := DataSet.LayerCount + 1;
                RowCount := DataSet.RowCount + 1;
                ColCount := DataSet.ColumnCount + 1;
              end;
          else
            Assert(False);
          end;
        end;
    else
      Assert(False);
    end;

  end;
  procedure InitializeMinMax(const Layer, Row, Col: integer);
  begin
    if not DataSet.IsValue[Layer, Row, Col] then
      Exit;
    if not IsActiveOK(DataSet, Layer, Row, Col) then
      Exit;
    if not ValueOK(DataSet, Layer, Row, Col) then
      Exit;
    MinMaxInitialized := True;
    case DataSet.Datatype of
      rdtDouble:
        begin
          RMin := DataSet.RealData[Layer, Row, Col];
          RMax := RMin;
          if DataSet.Limits.LowerLimit.UseLimit then
          begin
            RMin := DataSet.Limits.LowerLimit.RealLimitValue;
          end;
          if DataSet.Limits.UpperLimit.UseLimit then
          begin
            RMax := DataSet.Limits.UpperLimit.RealLimitValue;
          end;
          if RMin > 0 then
          begin
            RMinPositive := RMin;
          end;
        end;
      rdtInteger:
        begin
          if DataSet.DisplayRealValue then
          begin
            RMin := DataSet.RealData[Layer, Row, Col];
            RMax := Rmin;
            if DataSet.Limits.LowerLimit.UseLimit then
            begin
              RMin := DataSet.Limits.LowerLimit.IntegerLimitValue;
            end;
            if DataSet.Limits.UpperLimit.UseLimit then
            begin
              RMax := DataSet.Limits.UpperLimit.IntegerLimitValue;
            end;
          end
          else
          begin
            IMin := DataSet.IntegerData[Layer, Row, Col];
            IMax := IMin;
            if DataSet.Limits.LowerLimit.UseLimit then
            begin
              IMin := DataSet.Limits.LowerLimit.IntegerLimitValue;
            end;
            if DataSet.Limits.UpperLimit.UseLimit then
            begin
              IMax := DataSet.Limits.UpperLimit.IntegerLimitValue;
            end;
          end;
        end;
      rdtBoolean:
        begin
          BMin := False;
          BMax := True;
          if DataSet.Limits.LowerLimit.UseLimit then
          begin
            BMin := DataSet.Limits.LowerLimit.BooleanLimitValue;
          end;
          if DataSet.Limits.UpperLimit.UseLimit then
          begin
            BMax := DataSet.Limits.UpperLimit.BooleanLimitValue;
          end;
        end;
      rdtString:
        begin
          if DataSet.Limits.LowerLimit.UseLimit then
          begin
            SMin := DataSet.Limits.LowerLimit.StringLimitValue;
          end;
          if DataSet.Limits.UpperLimit.UseLimit then
          begin
            SMax := DataSet.Limits.UpperLimit.StringLimitValue;
          end;
        end;
    else
      Assert(False);
    end;
  end;
  procedure UpdateMinMax(const Layer, Row, Col: integer);
  begin
    if not DataSet.IsValue[Layer, Row, Col] then
      Exit;
    if not IsActiveOK(DataSet, Layer, Row, Col) then
      Exit;
    if not ValueOK(DataSet, Layer, Row, Col) then
      Exit;
    if not MinMaxInitialized then
    begin
      InitializeMinMax(Layer, Row, Col);
    end
    else
    begin
      case DataSet.Datatype of
        rdtDouble:
          begin
            RTemp := DataSet.RealData[Layer, Row, Col];
            if (RTemp > RMax) and not DataSet.Limits.UpperLimit.UseLimit then
            begin
              RMax := RTemp;
            end
            else if (RTemp < RMin) and not DataSet.Limits.LowerLimit.UseLimit
              then
            begin
              RMin := RTemp;
            end;
            if (RTemp > 0) then
            begin
              if (RMinPositive = 0) or (RTemp < RMinPositive) then
              begin
                RMinPositive := RTemp;
              end;
            end;
          end;
        rdtInteger:
          begin
            if DataSet.DisplayRealValue then
            begin
              RTemp := DataSet.RealData[Layer, Row, Col];
              if (RTemp > RMax) and not DataSet.Limits.UpperLimit.UseLimit then
              begin
                RMax := RTemp;
              end
              else if (RTemp < RMin) and not DataSet.Limits.LowerLimit.UseLimit
                then
              begin
                RMin := RTemp;
              end;
            end
            else
            begin
              ITemp := DataSet.IntegerData[Layer, Row, Col];
              if (ITemp > IMax) and not DataSet.Limits.UpperLimit.UseLimit then
              begin
                IMax := ITemp;
              end
              else if (ITemp < IMin) and not DataSet.Limits.LowerLimit.UseLimit
                then
              begin
                IMin := ITemp;
              end;
            end;
          end;
        rdtBoolean:
          begin
            Exit;
          end;
        rdtString:
          begin
            UseString := True;
            TempString := DataSet.StringData[Layer, Row, Col];
            if DataSet.Limits.UpperLimit.UseLimit then
            begin
              if TempString > SMax then
              begin
                UseString := False;
              end;
            end;
            if DataSet.Limits.LowerLimit.UseLimit then
            begin
              if TempString < SMin then
              begin
                UseString := False;
              end;
            end;
            if UseString then
            begin
              StringValues.Add(TempString);
            end;
          end;
      else
        Assert(False);
      end;
    end;
  end;
  procedure SetMinMax;
  var
    LayerIndex: integer;
    RowIndex: integer;
    ColIndex: integer;
  begin
    for LayerIndex := 0 to LayerCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        for ColIndex := 0 to ColCount - 1 do
        begin
          UpdateMinMax(LayerIndex, RowIndex, ColIndex);
        end;
      end;
    end;
  end;
begin
  RMinPositive := 0;
  MinMaxInitialized := False;
  StringValues := TStringList.Create;
  try
    StringValues.Sorted := True;
    StringValues.Duplicates := dupIgnore;
    StringValues.CaseSensitive := True;
    case ViewDirection of
      vdTop:
        begin
          DataSet := TopDataSet;
          if DataSet = nil then
            Exit;
          GetCounts;
          if (LayerCount = 0) or (RowCount = 0) or (ColCount = 0) then
          begin
            Exit;
          end;
          SetMinMax;
          if DataSet.Limits.LogTransform and (RMinPositive > 0) then
          begin
            LogRMin := Log10(RMinPositive);
            LogRMax := Log10(RMax);
          end
          else
          begin
            LogRMin := 0.;
            LogRMax := 0.;
          end;
          if LayerCount > 1 then
          begin
            LayerIndex := SelectedLayer;
          end
          else
          begin
            LayerIndex := 0;
          end;
          for RowIndex := 0 to RowCount - 1 do
          begin
            for ColIndex := 0 to ColCount - 1 do
            begin
              case DataSet.Datatype of
                rdtDouble:
                  begin
                    if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                      or not IsActiveOK(DataSet, LayerIndex, RowIndex, ColIndex)
                      or not ValueOK(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      TopCellColors[ColIndex, RowIndex] := clWhite;
                    end
                    else if (RMax = RMin) then
                    begin
                      RTemp := DataSet.RealData
                        [LayerIndex, RowIndex, ColIndex];
                      if RTemp = RMin then
                      begin
                        TopCellColors[ColIndex, RowIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      RTemp := DataSet.RealData
                        [LayerIndex, RowIndex, ColIndex];
                      if (RTemp >= RMin) and (RTemp <= RMax) then
                      begin
                        if DataSet.Limits.LogTransform  then
                        begin
                          Assert(RMinPositive > 0);
                          Assert(RTemp > 0);
                          LogRTemp := Log10(RTemp);
                          TopCellColors[ColIndex, RowIndex] :=
                            GridFracToRainbow((LogRMax - LogRTemp) / (LogRMax - LogRMin));
                        end
                        else
                        begin
                          TopCellColors[ColIndex, RowIndex] :=
                            GridFracToRainbow((RMax - RTemp) / (RMax - RMin));
                        end;
                      end;
                    end;
                  end;
                rdtInteger:
                  begin
                    if DataSet.DisplayRealValue then
                    begin
                      if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                        or not IsActiveOK(DataSet, LayerIndex, RowIndex, ColIndex)
                        or not ValueOK(DataSet, LayerIndex, RowIndex, ColIndex) then
                      begin
                        TopCellColors[ColIndex, RowIndex] := clWhite;
                      end
                      else if (RMax = RMin) then
                      begin
                        RTemp := DataSet.RealData
                          [LayerIndex, RowIndex, ColIndex];
                        if RTemp = RMin then
                        begin
                          TopCellColors[ColIndex, RowIndex] :=
                            GridFracToRainbow(0.5);
                        end;
                      end
                      else
                      begin
                        RTemp := DataSet.RealData
                          [LayerIndex, RowIndex, ColIndex];
                        if (RTemp >= RMin) and (RTemp <= RMax) then
                        begin
                          TopCellColors[ColIndex, RowIndex] :=
                            GridFracToRainbow((RMax - RTemp) / (RMax - RMin));
                        end;
                      end;
                    end
                    else
                    begin
                      if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                        or not IsActiveOK(DataSet, LayerIndex, RowIndex, ColIndex)
                        or not ValueOK(DataSet, LayerIndex, RowIndex, ColIndex) then
                      begin
                        TopCellColors[ColIndex, RowIndex] := clWhite;
                      end
                      else if (IMax = IMin) then
                      begin
                        ITemp := DataSet.IntegerData
                          [LayerIndex, RowIndex, ColIndex];
                        if ITemp = IMin then
                        begin
                          TopCellColors[ColIndex, RowIndex] :=
                            GridFracToRainbow(0.5);
                        end;
                      end
                      else
                      begin
                        ITemp := DataSet.IntegerData
                          [LayerIndex, RowIndex, ColIndex];
                        if (ITemp >= IMin) and (ITemp <= IMax) then
                        begin
                          TopCellColors[ColIndex, RowIndex] :=
                            GridFracToRainbow((IMax - ITemp) / (IMax - IMin));
                        end;
                      end;
                    end;
                  end;
                rdtBoolean:
                  begin
                    if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                      or not IsActiveOK(DataSet, LayerIndex, RowIndex, ColIndex)
                      or not ValueOK(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      TopCellColors[ColIndex, RowIndex] := clWhite;
                    end
                    else if BMax = Bmin then
                    begin
                      BTemp := DataSet.BooleanData
                        [LayerIndex, RowIndex, ColIndex];
                      if BTemp = Bmin then
                      begin
                        TopCellColors[ColIndex, RowIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      if DataSet.BooleanData[LayerIndex, RowIndex, ColIndex]
                        then
                      begin
                        TopCellColors[ColIndex, RowIndex] :=
                          GridFracToRainbow(1);
                      end
                      else
                      begin
                        TopCellColors[ColIndex, RowIndex] :=
                          GridFracToRainbow(0);
                      end;
                    end;
                  end;
                rdtString:
                  begin
                    if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                      or (StringValues.Count = 0)
                      or not IsActiveOK(DataSet, LayerIndex, RowIndex, ColIndex)
                      or not ValueOK(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      TopCellColors[ColIndex, RowIndex] := clWhite;
                    end
                    else if StringValues.Count = 1 then
                    begin
                      STemp := DataSet.StringData[
                        LayerIndex, RowIndex, ColIndex];
                      if StringValues.IndexOf(STemp) >= 0 then
                      begin
                        TopCellColors[ColIndex, RowIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      STemp := DataSet.StringData[LayerIndex, RowIndex,
                        ColIndex];
                      SIndex := StringValues.IndexOf(STemp);
                      if SIndex >= 0 then
                      begin
                        TopCellColors[ColIndex, RowIndex] :=
                          GridFracToRainbow(SIndex / (StringValues.Count - 1));
                      end;
                    end;
                  end;
              else
                Assert(False);
              end;
            end;
          end;
          FNeedToRecalculateTopCellColors := False;
        end;
      vdFront:
        begin
          DataSet := FrontDataSet;
          if DataSet = nil then
            Exit;
          GetCounts;
          if (LayerCount = 0) or (RowCount = 0) or (ColCount = 0) then
          begin
            Exit;
          end;
          SetMinMax;
          if DataSet.Limits.LogTransform and (RMinPositive > 0) then
          begin
            LogRMin := Log10(RMinPositive);
            LogRMax := Log10(RMax);
          end
          else
          begin
            LogRMin := 0.;
            LogRMax := 0.;
          end;
          if RowCount > 1 then
          begin
            RowIndex := SelectedRow;
          end
          else
          begin
            RowIndex := 0;
          end;
          for LayerIndex := 0 to LayerCount - 1 do
          begin
            for ColIndex := 0 to ColCount - 1 do
            begin
              case DataSet.Datatype of
                rdtDouble:
                  begin
                    if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                      or not IsActiveOK(DataSet, LayerIndex, RowIndex, ColIndex)
                      or not ValueOK(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      FrontCellColors[ColIndex, LayerIndex] := clWhite;
                    end
                    else if (RMax = RMin) then
                    begin
                      RTemp := DataSet.RealData
                        [LayerIndex, RowIndex, ColIndex];
                      if RTemp = RMin then
                      begin
                        FrontCellColors[ColIndex, LayerIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      RTemp := DataSet.RealData
                        [LayerIndex, RowIndex, ColIndex];
                      if (RTemp >= RMin) and (RTemp <= RMax) then
                      begin
                        if DataSet.Limits.LogTransform  then
                        begin
                          Assert(RMinPositive > 0);
                          Assert(RTemp > 0);
                          LogRTemp := Log10(RTemp);
                          FrontCellColors[ColIndex, LayerIndex] :=
                            GridFracToRainbow((LogRMax - LogRTemp) / (LogRMax - LogRMin));
                        end
                        else
                        begin
                          FrontCellColors[ColIndex, LayerIndex] :=
                            GridFracToRainbow((RMax - RTemp) / (RMax - RMin));
                        end;
                      end;
                    end;
                  end;
                rdtInteger:
                  begin
                    if DataSet.DisplayRealValue then
                    begin
                      if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                        or not IsActiveOK(DataSet, LayerIndex, RowIndex, ColIndex)
                        or not ValueOK(DataSet, LayerIndex, RowIndex, ColIndex) then
                      begin
                        FrontCellColors[ColIndex, LayerIndex] := clWhite;
                      end
                      else if (RMax = RMin) then
                      begin
                        RTemp := DataSet.RealData
                          [LayerIndex, RowIndex, ColIndex];
                        if RTemp = RMin then
                        begin
                          FrontCellColors[ColIndex, LayerIndex] :=
                            GridFracToRainbow(0.5);
                        end;
                      end
                      else
                      begin
                        RTemp := DataSet.RealData
                          [LayerIndex, RowIndex, ColIndex];
                        if (RTemp >= RMin) and (RTemp <= RMax) then
                        begin
                          FrontCellColors[ColIndex, LayerIndex] :=
                            GridFracToRainbow((RMax - RTemp) / (RMax - RMin));
                        end;
                      end;
                    end
                    else
                    begin
                      if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                        or not IsActiveOK(DataSet, LayerIndex, RowIndex, ColIndex)
                        or not ValueOK(DataSet, LayerIndex, RowIndex, ColIndex) then
                      begin
                        FrontCellColors[ColIndex, LayerIndex] := clWhite;
                      end
                      else if (IMax = IMin) then
                      begin
                        ITemp := DataSet.IntegerData
                          [LayerIndex, RowIndex, ColIndex];
                        if ITemp = IMin then
                        begin
                          FrontCellColors[ColIndex, LayerIndex] :=
                            GridFracToRainbow(0.5);
                        end;
                      end
                      else
                      begin
                        ITemp := DataSet.IntegerData
                          [LayerIndex, RowIndex, ColIndex];
                        if (ITemp >= IMin) and (ITemp <= IMax) then
                        begin
                          FrontCellColors[ColIndex, LayerIndex] :=
                            GridFracToRainbow((IMax - ITemp) / (IMax - IMin));
                        end;
                      end;
                    end;
                  end;
                rdtBoolean:
                  begin

                    if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                      or not IsActiveOK(DataSet, LayerIndex, RowIndex, ColIndex)
                      or not ValueOK(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      FrontCellColors[ColIndex, LayerIndex] := clWhite;
                    end
                    else if BMax = Bmin then
                    begin
                      BTemp := DataSet.BooleanData
                        [LayerIndex, RowIndex, ColIndex];
                      if BTemp = Bmin then
                      begin
                        FrontCellColors[ColIndex, LayerIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      if DataSet.BooleanData[LayerIndex, RowIndex, ColIndex]
                        then
                      begin
                        FrontCellColors[ColIndex, LayerIndex] :=
                          GridFracToRainbow(1);
                      end
                      else
                      begin
                        FrontCellColors[ColIndex, LayerIndex] :=
                          GridFracToRainbow(0);
                      end;
                    end;
                  end;
                rdtString:
                  begin
                    if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                      or (StringValues.Count = 0)
                      or not IsActiveOK(DataSet, LayerIndex, RowIndex, ColIndex)
                      or not ValueOK(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      FrontCellColors[ColIndex, LayerIndex] := clWhite;
                    end
                    else if StringValues.Count = 1 then
                    begin
                      STemp := DataSet.StringData[
                        LayerIndex, RowIndex, ColIndex];
                      if StringValues.IndexOf(STemp) >= 0 then
                      begin
                        FrontCellColors[ColIndex, LayerIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      STemp := DataSet.StringData[LayerIndex, RowIndex,
                        ColIndex];
                      SIndex := StringValues.IndexOf(STemp);
                      if SIndex >= 0 then
                      begin
                        FrontCellColors[ColIndex, LayerIndex] :=
                          GridFracToRainbow(SIndex / (StringValues.Count - 1));
                      end;
                    end;
                  end;
              else
                Assert(False);
              end;
            end;
          end;
          FNeedToRecalculateFrontCellColors := False;
        end;
      vdSide:
        begin
          DataSet := SideDataSet;
          if DataSet = nil then
            Exit;
          GetCounts;
          if (LayerCount = 0) or (RowCount = 0) or (ColCount = 0) then
          begin
            Exit;
          end;
          SetMinMax;
          if DataSet.Limits.LogTransform and (RMinPositive > 0) then
          begin
            LogRMin := Log10(RMinPositive);
            LogRMax := Log10(RMax);
          end
          else
          begin
            LogRMin := 0.;
            LogRMax := 0.;
          end;
          if ColCount > 1 then
          begin
            ColIndex := SelectedColumn;
          end
          else
          begin
            ColIndex := 0;
          end;
          for LayerIndex := 0 to LayerCount - 1 do
          begin
            for RowIndex := 0 to RowCount - 1 do
            begin
              case DataSet.Datatype of
                rdtDouble:
                  begin
                    if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                      or not IsActiveOK(DataSet, LayerIndex, RowIndex, ColIndex)
                      or not ValueOK(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      SideCellColors[RowIndex, LayerIndex] := clWhite;
                    end
                    else if (RMax = RMin) then
                    begin
                      RTemp := DataSet.RealData
                        [LayerIndex, RowIndex, ColIndex];
                      if RTemp = RMin then
                      begin
                        SideCellColors[RowIndex, LayerIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      RTemp := DataSet.RealData
                        [LayerIndex, RowIndex, ColIndex];
                      if (RTemp >= RMin) and (RTemp <= RMax) then
                      begin
                       if DataSet.Limits.LogTransform  then
                        begin
                          Assert(RMinPositive > 0);
                          Assert(RTemp > 0);
                          LogRTemp := Log10(RTemp);
                          SideCellColors[RowIndex, LayerIndex]  :=
                            GridFracToRainbow((LogRMax - LogRTemp) / (LogRMax - LogRMin));
                        end
                        else
                        begin
                          SideCellColors[RowIndex, LayerIndex] :=
                            GridFracToRainbow((RMax - RTemp) / (RMax - RMin));
                        end;
                      end;
                    end;
                  end;
                rdtInteger:
                  begin
                    if DataSet.DisplayRealValue then
                    begin
                      if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                        or not IsActiveOK(DataSet, LayerIndex, RowIndex, ColIndex)
                        or not ValueOK(DataSet, LayerIndex, RowIndex, ColIndex) then
                      begin
                        SideCellColors[RowIndex, LayerIndex] := clWhite;
                      end
                      else if (RMax = RMin) then
                      begin
                        RTemp := DataSet.RealData
                          [LayerIndex, RowIndex, ColIndex];
                        if RTemp = RMin then
                        begin
                          SideCellColors[RowIndex, LayerIndex] :=
                            GridFracToRainbow(0.5);
                        end;
                      end
                      else
                      begin
                        RTemp := DataSet.RealData
                          [LayerIndex, RowIndex, ColIndex];
                        if (RTemp >= RMin) and (RTemp <= RMax) then
                        begin
                          SideCellColors[RowIndex, LayerIndex] :=
                            GridFracToRainbow((RMax - RTemp) / (RMax - RMin));
                        end;
                      end;
                    end
                    else
                    begin
                      if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                        or not IsActiveOK(DataSet, LayerIndex, RowIndex, ColIndex)
                        or not ValueOK(DataSet, LayerIndex, RowIndex, ColIndex) then
                      begin
                        SideCellColors[RowIndex, LayerIndex] := clWhite;
                      end
                      else if (IMax = IMin) then
                      begin
                        ITemp := DataSet.IntegerData
                          [LayerIndex, RowIndex, ColIndex];
                        if ITemp = IMin then
                        begin
                          SideCellColors[RowIndex, LayerIndex] :=
                            GridFracToRainbow(0.5);
                        end;
                      end
                      else
                      begin
                        ITemp := DataSet.IntegerData
                          [LayerIndex, RowIndex, ColIndex];
                        if (ITemp >= IMin) and (ITemp <= IMax) then
                        begin
                          SideCellColors[RowIndex, LayerIndex] :=
                            GridFracToRainbow((IMax - ITemp) / (IMax - IMin));
                        end;
                      end;
                    end;
                  end;
                rdtBoolean:
                  begin
                    if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                      or not IsActiveOK(DataSet, LayerIndex, RowIndex, ColIndex)
                      or not ValueOK(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      SideCellColors[RowIndex, LayerIndex] := clWhite;
                    end
                    else if BMax = Bmin then
                    begin
                      BTemp := DataSet.BooleanData
                        [LayerIndex, RowIndex, ColIndex];
                      if BTemp = Bmin then
                      begin
                        SideCellColors[RowIndex, LayerIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      if DataSet.BooleanData[LayerIndex, RowIndex, ColIndex]
                        then
                      begin
                        SideCellColors[RowIndex, LayerIndex] :=
                          GridFracToRainbow(1);
                      end
                      else
                      begin
                        SideCellColors[RowIndex, LayerIndex] :=
                          GridFracToRainbow(0);
                      end;
                    end;
                  end;
                rdtString:
                  begin
                    if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                      or (StringValues.Count = 0)
                      or not IsActiveOK(DataSet, LayerIndex, RowIndex, ColIndex)
                      or not ValueOK(DataSet, LayerIndex, RowIndex, ColIndex) then
                    begin
                      SideCellColors[RowIndex, LayerIndex] := clWhite;
                    end
                    else if StringValues.Count = 1 then
                    begin
                      STemp := DataSet.StringData[
                        LayerIndex, RowIndex, ColIndex];
                      if StringValues.IndexOf(STemp) >= 0 then
                      begin
                        SideCellColors[RowIndex, LayerIndex] :=
                          GridFracToRainbow(0.5);
                      end;
                    end
                    else
                    begin
                      STemp := DataSet.StringData[LayerIndex, RowIndex,
                        ColIndex];
                      SIndex := StringValues.IndexOf(STemp);
                      if SIndex >= 0 then
                      begin
                        SideCellColors[RowIndex, LayerIndex] :=
                          GridFracToRainbow(SIndex / (StringValues.Count - 1));
                      end;
                    end;
                  end;
              else
                Assert(False);
              end;
            end;
          end;
          FNeedToRecalculateSideCellColors := False;
        end;
    else
      Assert(False);
    end;
  finally
    StringValues.Free;
  end;
end;

function TCustomGrid.ValueOK(DataSet: TDataArray; const Layer, Row, Col: integer): boolean;
begin
  result := DataSet.ColorGridValueOK(Layer, Row, Col);
end;

function TCustomGrid.IsActiveOk(const DataSet: TDataArray;
  const Layer, Row, Col: integer): boolean;
var
  ActiveDataSet: TDataArray;
  LayerIndicies: TIntegerList;
  RowIndicies: TIntegerList;
  ColumnIndicies: TIntegerList;
  LayToCheck: Integer;
  RowToCheck: Integer;
  ColToCheck: Integer;
  LayerIndex: integer;
  RowIndex: integer;
  ColIndex: integer;
//  Index: Integer;
begin
  result := True;
  if DataSet.Limits.ActiveOnly then
  begin
    ActiveDataSet := frmGoPhast.PhastModel.GetDataSetByName(rsActive);
    if ActiveDataSet <> nil then
    begin
      result := False;
      LayerIndicies := TIntegerList.Create;
      RowIndicies := TIntegerList.Create;
      ColumnIndicies := TIntegerList.Create;
      try
        ActiveDataSet.Initialize;
        case DataSet.EvaluatedAt of
          eaBlocks:
            begin
              LayerIndicies.Add(Layer);
              RowIndicies.Add(Row);
              ColumnIndicies.Add(Col);
              case DataSet.Orientation of
                dsoTop:
                  begin
                    LayerIndicies.Clear;
                    LayerIndicies.Add(SelectedLayer);
//                    for Index := 0 to LayerCount - 1 do
//                    begin
//                      LayerIndicies.Add(Index)
//                    end;
                  end;
                dsoFront:
                  begin
                    RowIndicies.Clear;
                    RowIndicies.Add(SelectedRow)
//                    for Index := 0 to RowCount - 1 do
//                    begin
//                      RowIndicies.Add(Index)
//                    end;
                  end;
                dsoSide:
                  begin
                    ColumnIndicies.Clear;
                    ColumnIndicies.Add(SelectedColumn);
//                    for Index := 0 to ColumnCount - 1 do
//                    begin
//                      ColumnIndicies.Add(Index)
//                    end;
                  end;
                dso3D:
                  begin
                    // do nothing
                  end
                else Assert(False);
              end;
//              result := ActiveDataSet.BooleanData[Layer, Row, Col];
            end;
          eaNodes:
            begin
              if Layer < LayerCount then
              begin
                LayerIndicies.Add(Layer);
              end;
              if Layer > 0 then
              begin
                LayerIndicies.Add(Layer-1);
              end;
              if Row < RowCount then
              begin
                RowIndicies.Add(Row);
              end;
              if Row > 0 then
              begin
                RowIndicies.Add(Row-1);
              end;
              if Col < ColumnCount then
              begin
                ColumnIndicies.Add(Col);
              end;
              if Col > 0 then
              begin
                ColumnIndicies.Add(Col-1);
              end;
              case DataSet.Orientation of
                dsoTop:
                  begin
                    LayerIndicies.Clear;
                    LayerIndicies.Add(SelectedLayer);
//                    for Index := 0 to LayerCount do
//                    begin
//                      LayerIndicies.Add(Index)
//                    end;
                  end;
                dsoFront:
                  begin
                    RowIndicies.Clear;
                    RowIndicies.Add(SelectedRow)
//                    for Index := 0 to RowCount do
//                    begin
//                      RowIndicies.Add(Index)
//                    end;
                  end;
                dsoSide:
                  begin
                    ColumnIndicies.Clear;
                    ColumnIndicies.Add(SelectedColumn);
//                    for Index := 0 to ColumnCount do
//                    begin
//                      ColumnIndicies.Add(Index)
//                    end;
                  end;
                dso3D:
                  begin
                    // do nothing
                  end
                else Assert(False);
              end;
            end;
          else Assert(False);
        end;
        for LayerIndex := 0 to LayerIndicies.Count - 1 do
        begin
          LayToCheck := LayerIndicies[LayerIndex];
          for RowIndex := 0 to RowIndicies.Count - 1 do
          begin
            RowToCheck := RowIndicies[RowIndex];
            for ColIndex := 0 to ColumnIndicies.Count - 1 do
            begin
              ColToCheck := ColumnIndicies[ColIndex];
              result := ActiveDataSet.BooleanData[
                LayToCheck, RowToCheck, ColToCheck];
              if result then Exit;
            end;
          end;
        end;
      finally
        LayerIndicies.Free;
        ColumnIndicies.Free;
        RowIndicies.Free;
      end;
    end;
  end;
end;


procedure TCustomGrid.Update3DCellColors(var CellColors: TCellColors);
var
  DataSet: TDataArray;
  LayerCount, RowCount, ColCount: integer;
  LayerIndex, RowIndex, ColIndex: integer;
  RMin, RMax, RTemp, RMinPositive: real;
  IMin, IMax, ITemp: integer;
  BMin, BMax, BTemp: boolean;
  SMin, SMax, STemp: string;
  SIndex: integer;
  UseString: boolean;
  TempString: string;
  StringValues: TStringList;
  MinMaxInitialized: boolean;
  LogRMin: double;
  LogRMax: double;
  LogRTemp: double;
  procedure GetCounts;
  begin
    case DataSet.EvaluatedAt of
      eaBlocks:
        begin
          LayerCount := DataSet.LayerCount;
          RowCount := DataSet.RowCount;
          ColCount := DataSet.ColumnCount;
        end;
      eaNodes:
        begin
          case DataSet.Orientation of
            dsoTop:
              begin
                { TODO : This is a clumsy hack. It should be updated. }
                if (DataSet = frmGoPhast.PhastModel.
                  GetDataSetByName(rsInitial_Water_Table)) then
                begin
                  LayerCount := ThreeDDataSet.LayerCount;
                end
                else if
                  ((DataSet is TSparseArrayPhastInterpolationDataSet) and
                  ((frmGoPhast.PhastModel.RiverHead.
                  IndexOfDataSet(TSparseArrayPhastInterpolationDataSet(
                  DataSet)) >= 0)
                  or (frmGoPhast.PhastModel.RiverAssociatedSolution.
                  IndexOfDataSet(TSparseArrayPhastInterpolationDataSet(
                  DataSet)) >= 0)))
                  or (frmGoPhast.PhastModel.RiverDataSets.IndexOf(
                  DataSet) >= 0) then
                begin
                  LayerCount := frmGoPhast.PhastModel.PhastGrid.LayerCount + 1;
                end
                else
                begin
                  LayerCount := ThreeDDataSet.LayerCount;
                end;
                //LayerCount := DataSet.LayerCount;
                RowCount := DataSet.RowCount + 1;
                ColCount := DataSet.ColumnCount + 1;
              end;
            dsoFront:
              begin
                LayerCount := DataSet.LayerCount + 1;
                RowCount := DataSet.RowCount;
                ColCount := DataSet.ColumnCount + 1;
              end;
            dsoSide:
              begin
                LayerCount := DataSet.LayerCount + 1;
                RowCount := DataSet.RowCount + 1;
                ColCount := DataSet.ColumnCount;
              end;
            dso3D:
              begin
                LayerCount := DataSet.LayerCount + 1;
                RowCount := DataSet.RowCount + 1;
                ColCount := DataSet.ColumnCount + 1;
              end;
          else
            Assert(False);
          end;
        end;
    else
      Assert(False);
    end;
  end;
  procedure InitializeMinMax(const Layer, Row, Col: integer);
  begin
    if not DataSet.IsValue[Layer, Row, Col] then
      Exit;
    if not IsActiveOk(DataSet, Layer, Row, Col) then
      Exit;
    if not ValueOk(DataSet, Layer, Row, Col) then
      Exit;
    MinMaxInitialized := True;
    case DataSet.Datatype of
      rdtDouble:
        begin
          RMin := DataSet.RealData[Layer, Row, Col];
          RMax := RMin;
          if DataSet.Limits.LowerLimit.UseLimit then
          begin
            RMin := DataSet.Limits.LowerLimit.RealLimitValue;
          end;
          if DataSet.Limits.UpperLimit.UseLimit then
          begin
            RMax := DataSet.Limits.UpperLimit.RealLimitValue;
          end;
          if RMin > 0 then
          begin
            RMinPositive := RMin;
          end;
        end;
      rdtInteger:
        begin
          if DataSet.DisplayRealValue then
          begin
            RMin := DataSet.RealData[Layer, Row, Col];
            RMax := Rmin;
            if DataSet.Limits.LowerLimit.UseLimit then
            begin
              RMin := DataSet.Limits.LowerLimit.IntegerLimitValue;
            end;
            if DataSet.Limits.UpperLimit.UseLimit then
            begin
              RMax := DataSet.Limits.UpperLimit.IntegerLimitValue;
            end;
          end
          else
          begin
            IMin := DataSet.IntegerData[Layer, Row, Col];
            IMax := IMin;
            if DataSet.Limits.LowerLimit.UseLimit then
            begin
              IMin := DataSet.Limits.LowerLimit.IntegerLimitValue;
            end;
            if DataSet.Limits.UpperLimit.UseLimit then
            begin
              IMax := DataSet.Limits.UpperLimit.IntegerLimitValue;
            end;
          end;
        end;
      rdtBoolean:
        begin
          BMin := False;
          BMax := True;
          if DataSet.Limits.LowerLimit.UseLimit then
          begin
            BMin := DataSet.Limits.LowerLimit.BooleanLimitValue;
          end;
          if DataSet.Limits.UpperLimit.UseLimit then
          begin
            BMax := DataSet.Limits.UpperLimit.BooleanLimitValue;
          end;
        end;
      rdtString:
        begin
          if DataSet.Limits.LowerLimit.UseLimit then
          begin
            SMin := DataSet.Limits.LowerLimit.StringLimitValue;
          end;
          if DataSet.Limits.UpperLimit.UseLimit then
          begin
            SMax := DataSet.Limits.UpperLimit.StringLimitValue;
          end;
        end;
    else
      Assert(False);
    end;
  end;
  procedure UpdateMinMax(const Layer, Row, Col: integer);
  begin
    if not DataSet.IsValue[Layer, Row, Col] then
      Exit;
    if not IsActiveOk(DataSet, Layer, Row, Col) then
      Exit;
    if not ValueOk(DataSet, Layer, Row, Col) then
      Exit;
    if not MinMaxInitialized then
    begin
      InitializeMinMax(Layer, Row, Col);
    end
    else
    begin
      case DataSet.Datatype of
        rdtDouble:
          begin
            RTemp := DataSet.RealData[Layer, Row, Col];
            if (RTemp > RMax) and not DataSet.Limits.UpperLimit.UseLimit then
            begin
              RMax := RTemp;
            end
            else if (RTemp < RMin) and not DataSet.Limits.LowerLimit.UseLimit
              then
            begin
              RMin := RTemp;
            end;
            if RTemp > 0 then
            begin
               if (RMinPositive = 0) or (RTemp < RMinPositive) then
               begin
                 RMinPositive := RTemp;
               end;
            end;
          end;
        rdtInteger:
          begin
            if DataSet.DisplayRealValue then
            begin
              RTemp := DataSet.RealData[Layer, Row, Col];
              if (RTemp > RMax) and not DataSet.Limits.UpperLimit.UseLimit then
              begin
                RMax := RTemp;
              end
              else if (RTemp < RMin) and not DataSet.Limits.LowerLimit.UseLimit
                then
              begin
                RMin := RTemp;
              end;
            end
            else
            begin
              ITemp := DataSet.IntegerData[Layer, Row, Col];
              if (ITemp > IMax) and not DataSet.Limits.UpperLimit.UseLimit then
              begin
                IMax := ITemp;
              end
              else if (ITemp < IMin) and not DataSet.Limits.LowerLimit.UseLimit
                then
              begin
                IMin := ITemp;
              end;
            end;
          end;
        rdtBoolean:
          begin
            Exit;
          end;
        rdtString:
          begin
            UseString := True;
            TempString := DataSet.StringData[Layer, Row, Col];
            if DataSet.Limits.UpperLimit.UseLimit then
            begin
              if TempString > SMax then
              begin
                UseString := False;
              end;
            end;
            if DataSet.Limits.LowerLimit.UseLimit then
            begin
              if TempString < SMin then
              begin
                UseString := False;
              end;
            end;
            if UseString then
            begin
              StringValues.Add(TempString);
            end;
          end;
      else
        Assert(False);
      end;
    end;
  end;
begin
  RMinPositive := 0;
  MinMaxInitialized := False;
  StringValues := TStringList.Create;
  try
    StringValues.Sorted := True;
    StringValues.Duplicates := dupIgnore;
    StringValues.CaseSensitive := True;

    DataSet := ThreeDDataSet;
    if DataSet = nil then
      Exit;

    GetCounts;
    if (LayerCount = 0) or (RowCount = 0) or (ColCount = 0) then
    begin
      Exit;
    end;
    for LayerIndex := 0 to LayerCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        for ColIndex := 0 to ColCount - 1 do
        begin
          UpdateMinMax(LayerIndex, RowIndex, ColIndex);
        end;
      end;
    end;
    if DataSet.Limits.LogTransform and (RMinPositive > 0) then
    begin
      LogRMin := Log10(RMinPositive);
      LogRMax := Log10(RMax);
    end
    else
    begin
      LogRMin := 0;
      LogRMax := 0;
    end;
    for LayerIndex := 0 to LayerCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        for ColIndex := 0 to ColCount - 1 do
        begin
          case DataSet.Datatype of
            rdtDouble:
              begin
                if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                  or not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex)
                  or not ValueOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                begin
                  CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                end
                else if (RMax = RMin) then
                begin
                  RTemp := DataSet.RealData
                    [LayerIndex, RowIndex, ColIndex];
                  if RTemp = RMin then
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] :=
                      GridFracToRainbow(0.5);
                  end
                  else
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                  end;
                end
                else
                begin
                  RTemp := DataSet.RealData
                    [LayerIndex, RowIndex, ColIndex];
                  if (RTemp >= RMin) and (RTemp <= RMax) then
                  begin
                    if DataSet.Limits.LogTransform then
                    begin
                      Assert(RMinPositive > 0);
                      Assert(RTemp > 0);
                      LogRTemp := Log10(RTemp);
                      CellColors[LayerIndex, RowIndex, ColIndex] :=
                        GridFracToRainbow((LogRMax - LogRTemp) / (LogRMax - LogRMin));
                    end
                    else
                    begin
                      CellColors[LayerIndex, RowIndex, ColIndex] :=
                        GridFracToRainbow((RMax - RTemp) / (RMax - RMin));
                    end;
                  end
                  else
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                  end;
                end;
              end;
            rdtInteger:
              begin
                if DataSet.DisplayRealValue then
                begin
                  if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                    or not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) 
                    or not ValueOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                  end
                  else if (RMax = RMin) then
                  begin
                    RTemp := DataSet.RealData
                      [LayerIndex, RowIndex, ColIndex];
                    if RTemp = RMin then
                    begin
                      CellColors[LayerIndex, RowIndex, ColIndex] :=
                        GridFracToRainbow(0.5);
                    end
                    else
                    begin
                      CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                    end;
                  end
                  else
                  begin
                    RTemp := DataSet.RealData
                      [LayerIndex, RowIndex, ColIndex];
                    if (RTemp >= RMin) and (RTemp <= RMax) then
                    begin
                      CellColors[LayerIndex, RowIndex, ColIndex] :=
                        GridFracToRainbow((RMax - RTemp) / (RMax - RMin));
                    end
                    else
                    begin
                      CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                    end;
                  end;
                end
                else
                begin
                  if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                    or not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) 
                    or not ValueOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                  end
                  else if (IMax = IMin) then
                  begin
                    ITemp := DataSet.IntegerData
                      [LayerIndex, RowIndex, ColIndex];
                    if ITemp = IMin then
                    begin
                      CellColors[LayerIndex, RowIndex, ColIndex] :=
                        GridFracToRainbow(0.5);
                    end;
                  end
                  else
                  begin
                    ITemp := DataSet.IntegerData
                      [LayerIndex, RowIndex, ColIndex];
                    if (ITemp >= IMin) and (ITemp <= IMax) then
                    begin
                      CellColors[LayerIndex, RowIndex, ColIndex] :=
                        GridFracToRainbow((IMax - ITemp) / (IMax - IMin));
                    end;
                  end;
                end;
              end;
            rdtBoolean:
              begin
                if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                  or not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) 
                  or not ValueOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                begin
                  CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                end
                else if BMax = Bmin then
                begin
                  BTemp := DataSet.BooleanData
                    [LayerIndex, RowIndex, ColIndex];
                  if BTemp = Bmin then
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] :=
                      GridFracToRainbow(0.5);
                  end
                  else
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                  end;
                end
                else
                begin
                  if DataSet.BooleanData[LayerIndex, RowIndex, ColIndex] then
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] :=
                      GridFracToRainbow(1);
                  end
                  else
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] :=
                      GridFracToRainbow(0);
                  end;
                end;
              end;
            rdtString:
              begin
                if not DataSet.IsValue[LayerIndex, RowIndex, ColIndex]
                  or (StringValues.Count = 0)
                  or not IsActiveOk(DataSet, LayerIndex, RowIndex, ColIndex) 
                  or not ValueOk(DataSet, LayerIndex, RowIndex, ColIndex) then
                begin
                  CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                end
                else if StringValues.Count = 1 then
                begin
                  STemp := DataSet.StringData[
                    LayerIndex, RowIndex, ColIndex];
                  if StringValues.IndexOf(STemp) >= 0 then
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] :=
                      GridFracToRainbow(0.5);
                  end
                  else
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                  end;
                end
                else
                begin
                  STemp := DataSet.StringData[LayerIndex, RowIndex, ColIndex];
                  SIndex := StringValues.IndexOf(STemp);
                  if SIndex >= 0 then
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] :=
                      GridFracToRainbow(SIndex / (StringValues.Count - 1));
                  end
                  else
                  begin
                    CellColors[LayerIndex, RowIndex, ColIndex] := clWhite;
                  end;
                end;
              end;
          else
            Assert(False);
          end;
        end;
      end;
    end;
    FNeedToRecalculate3DCellColors := False;
  finally
    StringValues.Free;
  end;
end;

function TCustomGrid.GetContainingColumnOrRow(
  const Positions: TOneDRealArray; const APosition: real): integer;
var
  NearestPositionIndex: integer;
  PositionLength: integer;
  Reversed: boolean;
begin
  PositionLength := Length(Positions);
  if PositionLength <= 1 then
  begin
    result := -1;
    Exit;
  end;
  NearestPositionIndex := NearestColumnOrRow(Positions, APosition);
  if (NearestPositionIndex < 0) then
  begin
    result := -1;
  end
  else if (NearestPositionIndex >= PositionLength) then
  begin
    result := PositionLength - 1;
  end
  else
  begin
    Reversed := Positions[PositionLength-1] < Positions[0];
    if Reversed then
    begin
      if APosition < Positions[NearestPositionIndex] then
      begin
        result := NearestPositionIndex;
      end
      else
      begin
        if (NearestPositionIndex = 0)
          and (APosition = Positions[0]) then
        begin
          result := NearestPositionIndex;
        end
        else
        begin
          result := NearestPositionIndex - 1;
        end;
      end;
    end
    else
    begin
      if APosition > Positions[NearestPositionIndex] then
      begin
        result := NearestPositionIndex;
      end
      else
      begin
        if (NearestPositionIndex = 0)
          and (APosition = Positions[0]) then
        begin
          result := NearestPositionIndex;
        end
        else
        begin
          result := NearestPositionIndex - 1;
        end;
      end;
    end;
  end;
end;

function TCustomGrid.GetContainingColumn(
  const AnXPosition: real): integer;
begin
  result := GetContainingColumnOrRow(FColumnPositions, AnXPosition);
end;

function TCustomGrid.GetContainingRow(const AYPosition: real): integer;
begin
  result := GetContainingColumnOrRow(FRowPositions, AYPosition);
end;

procedure TCustomGrid.NeedToRecalculateCellColors;
begin
  NeedToRecalculateFrontCellColors := True;
  NeedToRecalculateSideCellColors := True;
  NeedToRecalculateTopCellColors := True;
  NeedToRecalculate3DCellColors := True;
end;

procedure TCustomGrid.Assign(Source: TPersistent);
var
  SourceGrid: TCustomGrid;
begin
  if Source is TCustomGrid then
  begin
    SourceGrid := TCustomGrid(Source);
    ColumnPositions := SourceGrid.ColumnPositions;
    RowPositions := SourceGrid.RowPositions;
    ColumnDirection := SourceGrid.ColumnDirection;
    GridAngle := SourceGrid.GridAngle;
    LayerCount := SourceGrid.LayerCount;
    SelectedLayer := SourceGrid.SelectedLayer;
    SelectedColumn := SourceGrid.SelectedColumn;
    SelectedRow := SourceGrid.SelectedRow;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

procedure TCustomGrid.BeginColumnChange;
begin
  Inc(FColumnUpdate);
end;

procedure TCustomGrid.BeginGridChange;
begin
  Inc(FGridUpdate);
end;

procedure TCustomGrid.BeginLayerChange;
begin
  Inc(FLayerUpdate);
  CanDraw := False;
end;

procedure TCustomGrid.BeginRowChange;
begin
  Inc(FRowUpdate);
end;

procedure TCustomGrid.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('ColumnPositions', ReadColumnPositions,
    WriteColumnPositions, True);
  Filer.DefineProperty('RowPositions', ReadRowPositions,
    WriteRowPositions, True);
end;

procedure TCustomGrid.ReadColumnPositions(Reader: TReader);
var
  Positions: TOneDRealArray;
begin
  ReadRealArray(Reader, Positions, ColumnCount + 1);
  ColumnPositions := Positions;
end;

procedure TCustomGrid.WriteColumnPositions(Writer: TWriter);
begin
  WriteRealArray(Writer, ColumnPositions);
end;

procedure TCustomGrid.ReadRowPositions(Reader: TReader);
var
  Positions: TOneDRealArray;
begin
  ReadRealArray(Reader, Positions, RowCount + 1);
  RowPositions := Positions;
end;

procedure TCustomGrid.WriteRowPositions(Writer: TWriter);
begin
  WriteRealArray(Writer, RowPositions);
end;

function TCustomGrid.TwoDCellCorner(const Column, Row: integer): TPoint2D;
begin
  if (Column = 0) and (Row = 0) then
  begin
    result := TwoDElementCorner(Column, Row);
  end
  else if (Column = 0) then
  begin
    if Row = RowCount+1 then
    begin
      result := TwoDElementCorner(Column, Row-1);
    end
    else
    begin
      result := TwoDColumnEdgeCenter(Column, Row-1);
    end;
  end
  else if (Row = 0) then
  begin
    if Column = ColumnCount+1 then
    begin
      result := TwoDElementCorner(Column-1, Row);
    end
    else
    begin
      result := TwoDRowEdgeCenter(Column-1, Row);
    end;
  end
  else if Column = ColumnCount+1 then
  begin
    if Row = RowCount+1 then
    begin
      result := TwoDElementCorner(Column-1, Row-1);
    end
    else
    begin
      result := TwoDColumnEdgeCenter(Column-1, Row-1);
    end;
  end
  else if Row = RowCount+1 then
  begin
    result := TwoDRowEdgeCenter(Column-1, Row-1);
  end
  else
  begin
    result := TwoDElementCenter(Column-1, Row-1);
  end;
end;

function TCustomGrid.TwoDColumnEdgeCenter(const Column,
  Row: integer): TPoint2D;
var
  Point1, Point2: TPoint2D;
begin
  Point1 := TwoDElementCorner(Column, Row + 1);
  Point2 := TwoDElementCorner(Column, Row);
  result.X := (Point1.X + Point2.X) / 2;
  result.Y := (Point1.Y + Point2.Y) / 2;
end;

function TCustomGrid.TwoDRowEdgeCenter(const Column,
  Row: integer): TPoint2D;
var
  Point1, Point2: TPoint2D;
begin
  Point1 := TwoDElementCorner(Column + 1, Row);
  Point2 := TwoDElementCorner(Column, Row);
  result.X := (Point1.X + Point2.X) / 2;
  result.Y := (Point1.Y + Point2.Y) / 2;
end;

function TCustomGrid.ThreeDCellCorner(Column, Row,
  Layer: integer): T3DRealPoint;
begin
//  if Column > ColumnCount then
//  begin
//    Column := ColumnCount
//  end;
//  if Row > RowCount then
//  begin
//    Row := RowCount
//  end;
//  if Layer > LayerCount then
//  begin
//    Layer := LayerCount
//  end;
  if (Column = 0) then
  begin
    result.X := ColumnPosition[Column];
  end
  else if (Column = ColumnCount+1) then
  begin
    result.X := ColumnPosition[Column-1];
  end
  else
  begin
    result.X := (ColumnPosition[Column] + ColumnPosition[Column-1])/2;
  end;
  if (Row = 0)  then
  begin
    result.Y := RowPosition[Row];
  end
  else if (Row = RowCount+1) then
  begin
    result.Y := RowPosition[Row-1];
  end
  else
  begin
    result.Y := (RowPosition[Row] + RowPosition[Row-1])/2;
  end;
  if (Layer = 0) then
  begin
    result.Z := CellElevation[Column, Row, Layer];
  end
  else if (Layer = LayerCount+1) then
  begin
    result.Z := CellElevation[Column, Row, Layer-1];
  end
  else
  begin
    result.Z := (CellElevation[Column, Row, Layer]
      + CellElevation[Column, Row, Layer-1])/ 2;
  end;
end;

function TCustomGrid.ThreeDColumnEdgeCenter(const Column, Row,
  Layer: integer): T3DRealPoint;
var
  Point1, Point2: T3DRealPoint;
begin
  Point1 := ThreeDElementCorner(Column, Row, Layer);
  Point2 := ThreeDElementCorner(Column, Row + 1, Layer);
  result.X := (Point1.X + Point2.X) / 2;
  result.Y := (Point1.Y + Point2.Y) / 2;
  result.Z := (Point1.Z + Point2.Z) / 2;
end;

function TCustomGrid.ThreeDLayerEdgeCenter(const Column, Row,
  Layer: integer): T3DRealPoint;
var
  Point1, Point2: T3DRealPoint;
begin
  Point1 := ThreeDElementCorner(Column, Row, Layer);
  Point2 := ThreeDElementCorner(Column, Row, Layer + 1);
  result.X := (Point1.X + Point2.X) / 2;
  result.Y := (Point1.Y + Point2.Y) / 2;
  result.Z := (Point1.Z + Point2.Z) / 2;
end;

function TCustomGrid.ThreeDRowEdgeCenter(const Column, Row,
  Layer: integer): T3DRealPoint;
var
  Point1, Point2: T3DRealPoint;
begin
  Point1 := ThreeDElementCorner(Column, Row, Layer);
  Point2 := ThreeDElementCorner(Column + 1, Row, Layer);
  result.X := (Point1.X + Point2.X) / 2;
  result.Y := (Point1.Y + Point2.Y) / 2;
  result.Z := (Point1.Z + Point2.Z) / 2;
end;

procedure TCustomGrid.RecordShell;
var
  X, Y, Z: single;
  ColumnIndex, RowIndex, LayerIndex: integer;
begin
  try
    glNewList(FGridShellGLIndex, GL_COMPILE);
    glLineWidth(ThinLine);
    // Draw left side;
    glColor3f(0.0, 0.0, 0.0);
    glBegin(GL_LINE_LOOP);
    LayerIndex := 0;
    ColumnIndex := 0;
    X := ColumnPosition[ColumnIndex];
    for RowIndex := 0 to RowCount do
    begin
      Y := RowPosition[RowIndex];
      Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
      glVertex3f(X, Y, Z);
    end;
    LayerIndex := LayerCount;
    for RowIndex := RowCount downto 0 do
    begin
      Y := RowPosition[RowIndex];
      Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
      glVertex3f(X, Y, Z);
    end;
    glEnd;

    // Draw right side;
    glBegin(GL_LINE_LOOP);
    LayerIndex := 0;
    ColumnIndex := ColumnCount;
    X := ColumnPosition[ColumnIndex];
    for RowIndex := 0 to RowCount do
    begin
      Y := RowPosition[RowIndex];
      Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
      glVertex3f(X, Y, Z);
    end;
    LayerIndex := LayerCount;
    for RowIndex := RowCount downto 0 do
    begin
      Y := RowPosition[RowIndex];
      Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
      glVertex3f(X, Y, Z);
    end;
    glEnd;

    // Draw front side;
    glBegin(GL_LINE_LOOP);
    LayerIndex := 0;
    RowIndex := 0;
    Y := RowPosition[RowIndex];
    for ColumnIndex := 0 to ColumnCount do
    begin
      X := ColumnPosition[ColumnIndex];
      Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
      glVertex3f(X, Y, Z);
    end;
    LayerIndex := LayerCount;
    for ColumnIndex := ColumnCount downto 0 do
    begin
      X := ColumnPosition[ColumnIndex];
      Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
      glVertex3f(X, Y, Z);
    end;
    glEnd;

    // Draw back side;
    glBegin(GL_LINE_LOOP);
    LayerIndex := 0;
    RowIndex := RowCount;
    Y := RowPosition[RowIndex];
    for ColumnIndex := 0 to ColumnCount do
    begin
      X := ColumnPosition[ColumnIndex];
      Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
      glVertex3f(X, Y, Z);
    end;
    LayerIndex := LayerCount;
    for ColumnIndex := ColumnCount downto 0 do
    begin
      X := ColumnPosition[ColumnIndex];
      Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
      glVertex3f(X, Y, Z);
    end;
    glEnd;
    glEndList;
  finally
    FRecordedShell := True;
  end;
end;

procedure TCustomGrid.RecordSide;
var
  X, Y, Z: single;
  ColumnIndex, RowIndex, LayerIndex: integer;
begin
  try
    glNewList(FSideGridGLIndex, GL_COMPILE);
    // Draw grid lines on selected column.

    ColumnIndex := DisplayColumn;
    if (ColumnIndex >= 0) and (ColumnIndex <= ColumnCount) then
    begin
      X := ColumnPosition[ColumnIndex];
      for RowIndex := 0 to RowCount do
      begin
        if (RowIndex = RowCount) or (RowIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        glBegin(GL_LINES);

        LayerIndex := 0;
        Y := RowPosition[RowIndex];
        Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
        glVertex3f(X, Y, Z);
        LayerIndex := LayerCount;
        Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
        glVertex3f(X, Y, Z);
        glEnd;
      end;
      for LayerIndex := 0 to LayerCount do
      begin
        if (LayerIndex = LayerCount) or (LayerIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        glBegin(GL_LINE_STRIP);
        for RowIndex := 0 to RowCount do
        begin
          Y := RowPosition[RowIndex];
          Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
          glVertex3f(X, Y, Z);
        end;
        glEnd;
      end;
    end;
    glEndList;
  finally
    FRecordedSideGrid := True;
  end;
end;

procedure TCustomGrid.RecordFront;
var
  X, Y, Z: single;
  ColumnIndex, RowIndex, LayerIndex: integer;
begin
  try
    glNewList(FFrontGridGLIndex, GL_COMPILE);
    // Draw grid lines on selected Row.

    RowIndex := DisplayRow;
    if (RowIndex >= 0) and (RowIndex <= RowCount) then
    begin
      Y := RowPosition[RowIndex];
      for ColumnIndex := 0 to ColumnCount do
      begin
        if (ColumnIndex = ColumnCount) or (ColumnIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        glBegin(GL_LINES);
        LayerIndex := 0;
        X := ColumnPosition[ColumnIndex];
        Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
        glVertex3f(X, Y, Z);
        LayerIndex := LayerCount;
        Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
        glVertex3f(X, Y, Z);
        glEnd;
      end;

      for LayerIndex := 0 to LayerCount do
      begin
        if (LayerIndex = LayerCount) or (LayerIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        glBegin(GL_LINE_STRIP);
        for ColumnIndex := 0 to ColumnCount do
        begin
          X := ColumnPosition[ColumnIndex];
          Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
          glVertex3f(X, Y, Z);
        end;
        glEnd;
      end;
    end;
    glEndList;
  finally
    FRecordedFrontGrid := True;
  end;
end;

procedure TCustomGrid.RecordTop;
var
  X, Y, Z: single;
  ColumnIndex, RowIndex, LayerIndex: integer;
begin
  try
    glNewList(FTopGridGLIndex, GL_COMPILE);
    // Draw grid lines on selected layer.

    LayerIndex := DisplayLayer;
    if (LayerIndex >= 0) and (LayerIndex <= LayerCount) then
    begin
      for ColumnIndex := 0 to ColumnCount do
      begin
        if (ColumnIndex = ColumnCount) or (ColumnIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        glBegin(GL_LINE_STRIP);
        for RowIndex := 0 to RowCount do
        begin
          X := ColumnPosition[ColumnIndex];
          Y := RowPosition[RowIndex];
          Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
          glVertex3f(X, Y, Z);
        end;
        glEnd;
      end;

      for RowIndex := 0 to RowCount do
      begin
        if (RowIndex = RowCount) or (RowIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        glBegin(GL_LINE_STRIP);
        for ColumnIndex := 0 to ColumnCount do
        begin
          X := ColumnPosition[ColumnIndex];
          Y := RowPosition[RowIndex];
          Z := CellElevation[ColumnIndex, RowIndex, LayerIndex];
          glVertex3f(X, Y, Z);
        end;
        glEnd;
      end;
    end;
    glEndList;
  finally
    FRecordedTopGrid := True;
  end;
end;

procedure TCustomGrid.Draw3D;
var
  Colors: array[0..2] of TGLint;
begin
  if not CanDraw3D then
    Exit;

  if FDrawing3DGrid then
  begin
    Exit;
  end;
  // Draw outer box

  glDisable(GL_LIGHTING);
  glDisable(GL_LIGHT0);
  glMatrixMode(GL_MODELVIEW);

  glPushMatrix;

  glEnable(GL_LINE_SMOOTH);
  Colors[0] := 0;
  Colors[1] := 0;
  Colors[2] := 0;
  glMaterialiv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, @Colors);

  // If the grid has changed, record display lists of the grid.
  if FNeedToRedraw3d then
  begin
    // If the display lists have not been created, create them.
    if not FListsCreated then
    begin
      FGridShellGLIndex := glGenLists(NumberOfLists);
      FTopGridGLIndex := Succ(FGridShellGLIndex);
      FFrontGridGLIndex := Succ(FTopGridGLIndex);
      FSideGridGLIndex := Succ(FFrontGridGLIndex);
      FCellsGLIndex := Succ(FSideGridGLIndex);
      FEdgesGLIndex := Succ(FCellsGLIndex);
      FListsCreated := True;
    end;
  end;

  if frmGoPhast.tbShell.Down and
    (FNeedToRedraw3d or not FRecordedShell) then
  begin
    // Record display lists of the
    // grid shell, Side, Front, and Top.
    RecordShell;
    FRecordedShell := True;
  end;

  if frmGoPhast.tbSideGrid.Down and
    (not FRecordedSideGrid) then
  begin
    RecordSide;
    // FRecordedSideGrid is private and won't be set
    // by overridden versions of RecordSide.
    FRecordedSideGrid := True;
  end;

  if frmGoPhast.tbFrontGrid.Down and
    (not FRecordedFrontGrid) then
  begin
    RecordFront;
    // FRecordedFrontGrid is private and won't be set
    // by overridden versions of RecordFront.
    FRecordedFrontGrid := True;
  end;

  if frmGoPhast.tbTopGrid.Down and
    (not FRecordedTopGrid) then
  begin
    RecordTop;
    // FRecordedTopGrid is private and won't be set
    // by overridden versions of RecordTop.
    FRecordedTopGrid := True;
  end;

  if FNeedToRedraw3d then
  begin
    if frmGoPhast.tb3DColors.Down and (ThreeDDataSet <> nil) then
    begin
      FDrawing3DGrid := True;
      try
        RecordColoredGrid;
      finally
        FDrawing3DGrid := False;
      end;
    end;
    if frmGoPhast.tb3DColors.Down
      and (frmGoPhast.PhastModel.EdgeDisplay <> nil) then
    begin
      RecordColoredGridEdges;
    end;

    // indicate that the display lists are up to date.
    FNeedToRedraw3d := False;
  end;

  // Show the display lists.
  if frmGoPhast.tbShell.Down then
  begin
    glCallList(FGridShellGLIndex);
  end;
  if frmGoPhast.tbTopGrid.Down then
  begin
    glCallList(FTopGridGLIndex);
  end;
  if frmGoPhast.tbFrontGrid.Down then
  begin
    glCallList(FFrontGridGLIndex);
  end;
  if frmGoPhast.tbSideGrid.Down then
  begin
    glCallList(FSideGridGLIndex);
  end;
  if frmGoPhast.tb3DColors.Down and (ThreeDDataSet <> nil) then
  begin
    glCallList(FCellsGLIndex);
  end;
  if frmGoPhast.tb3DColors.Down
    and (frmGoPhast.PhastModel.EdgeDisplay <> nil) then
  begin
    glCallList(FEdgesGLIndex);
  end;

  glPopMatrix;
end;

function TCustomGrid.CanDraw3D: boolean;
begin
  result := (ColumnCount >= 1) and (RowCount >= 1) and (LayerCount >= 1)
    and (FLayerUpdate = 0) and (FColumnUpdate = 0) and (FGridUpdate = 0)
    and (FRowUpdate = 0) and frmGoPhast.CanDraw;
end;

procedure TCustomGrid.ViewsChanged;
begin
  frmGoPhast.frameTopView.ItemChange(nil);
  frmGoPhast.frameFrontView.ItemChange(nil);
  frmGoPhast.frameSideView.ItemChange(nil);
  if frmGoPhast.frame3DView.glWidModelView <> nil then
  begin
    frmGoPhast.frame3DView.glWidModelView.Invalidate;
  end;
end;

procedure TCustomGrid.GridChanged;
begin
  if FGridUpdate > 0 then Exit;

  CanDraw := True;
  if (LayerCount <= 0) or (RowCount <= 0) or (ColumnCount <= 0) then
  begin
    TopDataSet := nil;
    FrontDataSet := nil;
    SideDataSet := nil;
    ThreeDDataSet := nil;

    TopContourDataSet := nil;
    FrontContourDataSet := nil;
    SideContourDataSet := nil;
    ThreeDContourDataSet := nil;
  end;

  FBlockGlGrid := nil;
  FreeAndNil(FBlockGridCache);
  FNodeGlGrid := nil;
  FreeAndNil(FNodeGridCache);

  FTopElementContourGrid := nil;
  FTopNodeContourGrid := nil;
  FFrontElementContourGrid := nil;
  FFrontNodeContourGrid := nil;
  FSideElementContourGrid := nil;
  FSideNodeContourGrid := nil;
  FRecordedShell := False;
  FRecordedSideGrid := False;
  FRecordedFrontGrid := False;
  FRecordedTopGrid := False;

  if frmGoPhast.frame3DView.glWidModelView <> nil then
  begin
    FNeedToRedraw3d := True;
  end;
  ViewsChanged;
end;

destructor TCustomGrid.Destroy;
begin
  if FListsCreated then
  begin
    glDeleteLists(FGridShellGLIndex, NumberOfLists);
  end;
  FreeAndNil(FBlockGridCache);
  FreeAndNil(FNodeGridCache);
  inherited;
end;

procedure TCustomGrid.SetThreeDContourDataSet(const Value: TDataArray);
begin
  if FThreeDContourDataSet <> Value then
  begin
    FThreeDContourDataSet := Value;
  end;
end;

procedure TCustomGrid.SetThreeDDataSet(const Value: TDataArray);
begin
//  FThreeDDataSet := Value;
  if FThreeDDataSet <> Value then
  begin
    FThreeDDataSet := Value;
    NeedToRecalculate3DCellColors := True;
    GridChanged;
  end;
end;

procedure TCustomGrid.SetThreeDGridObserver(const Value: TObserver);
begin
  FThreeDGridObserver := Value;
  if Assigned(FThreeDGridObserver) then
  begin
    FThreeDGridObserver.UpToDate := False;
    FThreeDGridObserver.UpToDate := True;
  end;
end;

procedure TCustomGrid.SetNeedToRecalculate3DCellColors(
  const Value: boolean);
begin
  if FNeedToRecalculate3DCellColors <> Value then
  begin
    FNeedToRecalculate3DCellColors := Value;
    if not FNeedToRecalculate3DCellColors then
    begin
      GridChanged;
    end;
  end;
end;

procedure TCustomGrid.RecordColoredGridEdges;
var
  Index: Integer;
  Edge: TCustomModflowGridEdgeFeature;
  ZPositions: TThreeDRealArray;
  XPositions: TOneDRealArray;
  YPositions: TOneDRealArray;
  MinValue, MaxValue, MinPositive: double;
  Value: double;
  Color: TColor;
  Red: TGLubyte;
  Green: TGLubyte;
  Blue: TGLubyte;
  LocalEdgeDisplay: TCustomModflowGridEdgeDisplay;
  ColoringLimits: TColoringLimits;
  ActiveDataArray: TDataArray;
  LogMax, LogMin: double;
begin
  Screen.Cursor := crHourGlass;
  try
    LocalEdgeDisplay := frmGoPhast.PhastModel.EdgeDisplay;
    Assert(LocalEdgeDisplay <> nil);
    LocalEdgeDisplay.UpdateData;

    XPositions := FColumnPositions;
    YPositions := FRowPositions;
    GetCellCornerElevations(eaBlocks, ZPositions);

    glNewList(FEdgesGLIndex, GL_COMPILE);

    LocalEdgeDisplay.GetValueRangeToDisplay(MinValue, MaxValue, MinPositive);

    ColoringLimits := LocalEdgeDisplay.Limits[LocalEdgeDisplay.DataToPlot];

    LogMax := 0;
    LogMin := 0;
    if ColoringLimits.LogTransform then
    begin
      if MinValue <= 0 then
      begin
        MinValue := MinPositive;
      end;
      if MinValue > 0 then
      begin
        LogMin := Log10(MinValue);
        LogMax := Log10(MaxValue);
      end;
    end;
    
    ActiveDataArray := nil;
    if ColoringLimits.ActiveOnly then
    begin
      ActiveDataArray := frmGoPhast.PhastModel.GetDataSetByName(rsActive);
      ActiveDataArray.Initialize;
    end;

    for Index := 0 to LocalEdgeDisplay.Count - 1 do
    begin
      Edge := LocalEdgeDisplay[Index];
      Value := Edge.RealValue[LocalEdgeDisplay.DataToPlot];
      if (Value < MinValue) or (Value > MaxValue) then
      begin
        Continue;
      end;

      if not LocalEdgeDisplay.UseEdge(ActiveDataArray, Edge) then
      begin
        Continue; 
      end;

      if not ColoringLimits.ValueOk(Value) then
      begin
        Continue;
      end;

      if MinValue = MaxValue then
      begin
        Value := 0.5;
      end
      else
      begin
        if ColoringLimits.LogTransform then
        begin
          Assert(Value > 0);
          Value := Log10(Value);
          Value := (LogMax - Value)/(LogMax - LogMin);
        end
        else
        begin
          Value := (MaxValue - Value)/(MaxValue - MinValue);
        end;
      end;
      Color := GridFracToRainbow(Value);
      ExtractColorComponents(Color, Red, Green, Blue);
      glColor3ub(Red, Green, Blue);

      if Edge.Col1 <> Edge.Col2 then
      begin
        if Edge.Col2 > Edge.Col1 then
        begin
          DrawRightCellSide3D(Edge.Col1, Edge.Row1, Edge.Layer, XPositions,
            YPositions, ZPositions);
        end
        else
        begin
          DrawLeftCellSide3D(Edge.Col1, Edge.Row1, Edge.Layer, XPositions,
            YPositions, ZPositions);
        end;
      end
      else
      begin
        Assert(Edge.Row1 <> Edge.Row2);
        if Edge.Row2 > Edge.Row1 then
        begin
          DrawBackCellSide3D(Edge.Col1, Edge.Row1, Edge.Layer, XPositions,
            YPositions, ZPositions);
        end
        else
        begin
          DrawFrontCellSide3D(Edge.Col1, Edge.Row1, Edge.Layer, XPositions,
            YPositions, ZPositions);
        end;
      end;
    end;

    glEndList;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TCustomGrid.DrawLeftCellSide3D(ColIndex, RowIndex,
  LayerIndex: integer; const XPositions,
  YPositions: TOneDRealArray; const ZPositions: TThreeDRealArray);
var
  X, Y, Z: single;
begin
  X := XPositions[ColIndex];
  glBegin(GL_POLYGON);
  // back bottom point
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex, RowIndex + 1, LayerIndex];
  glVertex3f(X, Y, Z);

  // front bottom point
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex, RowIndex, LayerIndex];
  glVertex3f(X, Y, Z);

  // front top point
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex, RowIndex, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // back top point
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex, RowIndex + 1, LayerIndex + 1];
  glVertex3f(X, Y, Z);
  glEnd;
end;

procedure TCustomGrid.DrawRightCellSide3D(ColIndex, RowIndex,
  LayerIndex: integer; const XPositions,
  YPositions: TOneDRealArray; const ZPositions: TThreeDRealArray);
var
  X, Y, Z: single;
begin
  X := XPositions[ColIndex + 1];
  glBegin(GL_POLYGON);
  // back top point
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex + 1, RowIndex + 1, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // front top point
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex + 1, RowIndex, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // front bottom point
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex + 1, RowIndex, LayerIndex];
  glVertex3f(X, Y, Z);

  // back bottom point
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex + 1, RowIndex + 1, LayerIndex];
  glVertex3f(X, Y, Z);

  glEnd;
end;

procedure TCustomGrid.DrawSideContours(const ZoomBox: TQRbwZoomBox2;
  const BitMap: TBitmap32);
var
  Contourer: TMultipleContourCreator;
begin
  if (ColumnCount >= 0) and (RowCount > 0) and (LayerCount > 0) then
  begin
    if SideContourDataSet <> nil then
    begin
      Contourer := TMultipleContourCreator.Create;
      try
        Contourer.DataSet := SideContourDataSet;
        Contourer.ActiveDataSet := frmGoPhast.PhastModel.GetDataSetByName(rsActive);
        Contourer.BitMap := BitMap;
        Contourer.ViewDirection := vdSide;
        Contourer.Grid := ContourGrid(SideContourDataSet.EvaluatedAt,
          frmGoPhast.PhastModel.ModelSelection, vdSide, SelectedColumn);
        Contourer.ZoomBox := ZoomBox;
        Contourer.DrawContours(SelectedColumn,
          frmGoPhast.PhastModel.ContourColors);
      finally
        Contourer.Free;
      end;
    end;
  end;
end;

procedure TCustomGrid.DrawBackCellSide3D(ColIndex, RowIndex,
  LayerIndex: integer; const XPositions,
  YPositions: TOneDRealArray; const ZPositions: TThreeDRealArray);
var
  X, Y, Z: single;
begin
  glBegin(GL_POLYGON);
  // right bottom point
  X := XPositions[ColIndex + 1];
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex + 1, RowIndex + 1, LayerIndex];
  glVertex3f(X, Y, Z);

  // left bottom point
  X := XPositions[ColIndex];
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex, RowIndex + 1, LayerIndex];
  glVertex3f(X, Y, Z);

  // left top point
  X := XPositions[ColIndex];
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex, RowIndex + 1, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // right top point
  X := XPositions[ColIndex + 1];
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex + 1, RowIndex + 1, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  glEnd;
end;

procedure TCustomGrid.DrawFrontCellSide3D(ColIndex, RowIndex,
  LayerIndex: integer; const XPositions,
  YPositions: TOneDRealArray; const ZPositions: TThreeDRealArray);
var
  X, Y, Z: single;
begin
  glBegin(GL_POLYGON);
  // right top point
  X := XPositions[ColIndex + 1];
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex + 1, RowIndex, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // left top point
  X := XPositions[ColIndex];
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex, RowIndex, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // left bottom point
  X := XPositions[ColIndex];
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex, RowIndex, LayerIndex];
  glVertex3f(X, Y, Z);

  // right bottom point
  X := XPositions[ColIndex + 1];
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex + 1, RowIndex, LayerIndex];
  glVertex3f(X, Y, Z);

  glEnd;
end;

procedure TCustomGrid.DrawTopCellSide3D(ColIndex, RowIndex,
  LayerIndex: integer; const XPositions,
  YPositions: TOneDRealArray; const ZPositions: TThreeDRealArray);
var
  X, Y, Z: single;
begin
  glBegin(GL_POLYGON);
  
  // north east point
  X := XPositions[ColIndex];
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex, RowIndex + 1, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // south east point
  X := XPositions[ColIndex];
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex, RowIndex, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // south west point
  X := XPositions[ColIndex + 1];
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex + 1, RowIndex, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  // north west point
  X := XPositions[ColIndex + 1];
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex + 1, RowIndex + 1, LayerIndex + 1];
  glVertex3f(X, Y, Z);

  glEnd;
end;

procedure TCustomGrid.DrawBottomCellSide3D(ColIndex, RowIndex,
  LayerIndex: integer; const XPositions,
  YPositions: TOneDRealArray; const ZPositions: TThreeDRealArray);
var
  X, Y, Z: single;
begin
  glBegin(GL_POLYGON);
  // north west point
  X := XPositions[ColIndex + 1];
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex + 1, RowIndex + 1, LayerIndex];
  glVertex3f(X, Y, Z);

  // south west point
  X := XPositions[ColIndex + 1];
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex + 1, RowIndex, LayerIndex];
  glVertex3f(X, Y, Z);

  // south east point
  X := XPositions[ColIndex];
  Y := YPositions[RowIndex];
  Z := ZPositions[ColIndex, RowIndex, LayerIndex];
  glVertex3f(X, Y, Z);

  // north east point
  X := XPositions[ColIndex];
  Y := YPositions[RowIndex + 1];
  Z := ZPositions[ColIndex, RowIndex + 1, LayerIndex];
  glVertex3f(X, Y, Z);

  glEnd;
end;

procedure TCustomGrid.RecordColoredGrid;
var
  CellColors: TCellColors;
  LayerLength, RowLength, ColLength: integer;
  LayerIndex, RowIndex, ColIndex: integer;
  Red, Green, Blue: TGLubyte;
  XPositions, YPositions: TOneDRealArray;
  ZPositions: TThreeDRealArray;
  Index: integer;
var
  SelectedLayer, SelectedRow, SelectedColumn: integer;
begin
  Screen.Cursor := crHourGlass;
  try
    Assert(ThreeDDataSet <> nil);
    if frmGoPhast.PhastModel.ThreeDTimeList <> nil then
    begin
      SelectedLayer := frmGoPhast.PhastGrid.SelectedLayer;
      SelectedRow := frmGoPhast.PhastGrid.SelectedRow;
      SelectedColumn := frmGoPhast.PhastGrid.SelectedColumn;
      try
        frmGoPhast.PhastModel.UpdateThreeDTimeDataSet(
          frmGoPhast.PhastModel.ThreeDTimeList,
          frmGoPhast.PhastModel.ThreeDDisplayTime);
        if frmGoPhast.PhastModel.TopTimeList <> nil then
        begin
          frmGoPhast.PhastModel.UpdateTopTimeDataSet(
            frmGoPhast.PhastModel.TopTimeList,
            frmGoPhast.PhastModel.TopDisplayTime);
        end;
        if frmGoPhast.PhastModel.FrontTimeList <> nil then
        begin
          frmGoPhast.PhastModel.UpdateFrontTimeDataSet(
            frmGoPhast.PhastModel.FrontTimeList,
            frmGoPhast.PhastModel.FrontDisplayTime);
        end;
        if frmGoPhast.PhastModel.SideTimeList <> nil then
        begin
          frmGoPhast.PhastModel.UpdateSideTimeDataSet(
            frmGoPhast.PhastModel.SideTimeList,
            frmGoPhast.PhastModel.SideDisplayTime);
        end;
      finally
        frmGoPhast.PhastGrid.SelectedLayer := SelectedLayer;
        frmGoPhast.PhastGrid.SelectedRow := SelectedRow;
        frmGoPhast.PhastGrid.SelectedColumn := SelectedColumn;
      end;
    end
    else
    begin
      ThreeDDataSet.Initialize;
    end;

    if ThreeDDataSet = nil then
    begin
      Exit;
    end;

    glNewList(FCellsGLIndex, GL_COMPILE);
    LayerLength := -1;
    RowLength := -1;
    ColLength := -1;
    case ThreeDDataSet.EvaluatedAt of
      eaBlocks:
        begin
          LayerLength := ThreeDDataSet.LayerCount;
          RowLength := ThreeDDataSet.RowCount;
          ColLength := ThreeDDataSet.ColumnCount;
        end;
      eaNodes:
        begin
          { TODO : This is a clumsy hack. It should be updated. }
          if (ThreeDDataSet = frmGoPhast.PhastModel.
            GetDataSetByName(rsInitial_Water_Table)) then
          begin
            LayerLength := ThreeDDataSet.LayerCount;
          end
          else if
            ((ThreeDDataSet is TSparseArrayPhastInterpolationDataSet) and
            ((frmGoPhast.PhastModel.RiverHead.
            IndexOfDataSet(TSparseArrayPhastInterpolationDataSet(
            ThreeDDataSet)) >= 0)
            or (frmGoPhast.PhastModel.RiverAssociatedSolution.
            IndexOfDataSet(TSparseArrayPhastInterpolationDataSet(
            ThreeDDataSet)) >= 0)))
            or (frmGoPhast.PhastModel.RiverDataSets.IndexOf(
            ThreeDDataSet) >= 0) then
          begin
            LayerLength := frmGoPhast.PhastModel.PhastGrid.LayerCount + 1;
          end
          else
          begin
            LayerLength := ThreeDDataSet.LayerCount + 1;
          end;
          //LayerLength := ThreeDDataSet.LayerCount+1;
          RowLength := ThreeDDataSet.RowCount + 1;
          ColLength := ThreeDDataSet.ColumnCount + 1;
          case ThreeDDataSet.Orientation of
            dsoTop:
              begin
                { TODO : This is a clumsy hack. It should be updated. }
                if (ThreeDDataSet = frmGoPhast.PhastModel.
                  GetDataSetByName(rsInitial_Water_Table)) then
                begin
                  LayerLength := ThreeDDataSet.LayerCount;
                end
                else if
                  ((ThreeDDataSet is TSparseArrayPhastInterpolationDataSet) and
                  ((frmGoPhast.PhastModel.RiverHead.
                  IndexOfDataSet(TSparseArrayPhastInterpolationDataSet(
                  ThreeDDataSet)) >= 0)
                  or (frmGoPhast.PhastModel.RiverAssociatedSolution.
                  IndexOfDataSet(TSparseArrayPhastInterpolationDataSet(
                  ThreeDDataSet)) >= 0)))
                  or (frmGoPhast.PhastModel.RiverDataSets.IndexOf(
                  ThreeDDataSet) >= 0)
                    then
                begin
                  LayerLength := frmGoPhast.PhastModel.PhastGrid.LayerCount + 1;
                end
                else
                begin
                  LayerLength := ThreeDDataSet.LayerCount;
                end;
                //LayerLength := 1;
              end;
            dsoFront:
              begin
                RowLength := 1;
              end;
            dsoSide:
              begin
                ColLength := 1;
              end;
            dso3D:
              begin
                // do nothing
              end;
          else
            Assert(False);
          end;
        end;
    else
      Assert(False);
    end;

    SetLength(CellColors, LayerLength, RowLength, ColLength);

    Update3DCellColors(CellColors);

    case ThreeDDataSet.EvaluatedAt of
      eaBlocks:
        begin
          XPositions := FColumnPositions;
          YPositions := FRowPositions;
        end;
      eaNodes:
        begin
          SetLength(XPositions, Length(FColumnPositions) + 1);
          XPositions[0] := FColumnPositions[0];
          for Index := 1 to Length(FColumnPositions) - 1 do
          begin
            XPositions[Index] := (FColumnPositions[Index]
              + FColumnPositions[Index - 1]) / 2;
          end;
          XPositions[Length(FColumnPositions)]
            := FColumnPositions[Length(FColumnPositions) - 1];

          SetLength(YPositions, Length(FRowPositions) + 1);
          YPositions[0] := FRowPositions[0];
          for Index := 1 to Length(FRowPositions) - 1 do
          begin
            YPositions[Index] := (FRowPositions[Index]
              + FRowPositions[Index - 1]) / 2;
          end;
          YPositions[Length(FRowPositions)]
            := FRowPositions[Length(FRowPositions) - 1];
        end;
    else
      Assert(False);
    end;

    GetCellCornerElevations(ThreeDDataSet.EvaluatedAt, ZPositions);

    for LayerIndex := 0 to LayerLength - 1 do
    begin
      for RowIndex := 0 to RowLength - 1 do
      begin
        for ColIndex := 0 to ColLength - 1 do
        begin
          if CellColors[LayerIndex, RowIndex, ColIndex] = clWhite then
            Continue;

          ExtractColorComponents(CellColors[LayerIndex, RowIndex, ColIndex],
            Red, Green, Blue);

          glColor3ub(Red, Green, Blue);

          if (ColIndex = ColLength - 1) or
            (CellColors[LayerIndex, RowIndex, ColIndex + 1] = clWhite) then
          begin
            DrawRightCellSide3D(ColIndex, RowIndex, LayerIndex, XPositions,
              YPositions, ZPositions);
          end;
          if (ColIndex = 0) or
            (CellColors[LayerIndex, RowIndex, ColIndex - 1] = clWhite) then
          begin
            DrawLeftCellSide3D(ColIndex, RowIndex, LayerIndex, XPositions,
              YPositions, ZPositions);
          end;
          if (RowIndex = RowLength - 1) or
            (CellColors[LayerIndex, RowIndex + 1, ColIndex] = clWhite) then
          begin
            DrawBackCellSide3D(ColIndex, RowIndex, LayerIndex, XPositions,
              YPositions, ZPositions);
          end;
          if (RowIndex = 0) or
            (CellColors[LayerIndex, RowIndex - 1, ColIndex] = clWhite) then
          begin
            DrawFrontCellSide3D(ColIndex, RowIndex, LayerIndex, XPositions,
              YPositions, ZPositions);
          end;
          if (LayerIndex = LayerLength - 1) or
            (CellColors[LayerIndex + 1, RowIndex, ColIndex] = clWhite) then
          begin
            DrawTopCellSide3D(ColIndex, RowIndex, LayerIndex, XPositions,
              YPositions, ZPositions);
          end;
          if (LayerIndex = 0) or
            (CellColors[LayerIndex - 1, RowIndex, ColIndex] = clWhite) then
          begin
            DrawBottomCellSide3D(ColIndex, RowIndex, LayerIndex, XPositions,
              YPositions, ZPositions);
          end;
        end;
      end;
    end;
    glEndList;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TCustomGrid.Initialize;
begin
  TopDataSet := nil;
  FrontDataSet := nil;
  SideDataSet := nil;
  ThreeDDataSet := nil;
  TopContourDataSet := nil;
  FrontContourDataSet := nil;
  SideContourDataSet := nil;
  ThreeDContourDataSet := nil;
  if (FDisplayColumn < 0) and (ColumnCount > 0) then
  begin
    FDisplayColumn := 0;
  end;
  if (FDisplayRow < 0) and (RowCount > 0) then
  begin
    FDisplayRow := 0;
  end;
  if (FDisplayLayer < 0) and (LayerCount > 0) then
  begin
    FDisplayLayer := 0;
  end;
end;

function TCustomGrid.LayerCenter(Column, Row, Layer: integer): real;
begin
  result := (CellElevation[Column, Row, Layer]
    + CellElevation[Column, Row, Layer+1])/2;
end;

procedure TCustomGrid.DrawTopContours(const ZoomBox: TQRbwZoomBox2;
  const BitMap: TBitmap32);
var
  Contourer: TMultipleContourCreator;
begin
  if (ColumnCount >= 0) and (RowCount > 0) and (LayerCount > 0) then
  begin
    if TopContourDataSet <> nil then
    begin
      Contourer := TMultipleContourCreator.Create;
      try
        Contourer.DataSet := TopContourDataSet;
        Contourer.ActiveDataSet :=
          frmGoPhast.PhastModel.GetDataSetByName(rsActive);
        Contourer.BitMap := BitMap;
        Contourer.ViewDirection := vdTop;
        Contourer.Grid := ContourGrid(TopContourDataSet.EvaluatedAt,
          frmGoPhast.PhastModel.ModelSelection, vdTop, SelectedLayer);
        Contourer.ZoomBox := ZoomBox;
        Contourer.DrawContours(SelectedLayer,
          frmGoPhast.PhastModel.ContourColors);
      finally
        Contourer.Free;
      end;
    end;
  end;
end;

procedure TCustomGrid.LayersChanged;
begin
  if FLayerUpdate > 0 then Exit;
  
  GridChanged;
  if Assigned(ThreeDGridObserver) then
  begin
    ThreeDGridObserver.UpToDate := False;
    ThreeDGridObserver.UpToDate := True;
  end;
end;

function TCustomGrid.Nearest2DCellElevation(const Col, Row: integer;
  const APosition: real; const First, Last: integer): integer;
begin
  result := NearestColumnOrRow(TwoDCellElevations[Col, Row],
    APosition, First, Last);
end;

function TCustomGrid.NearestColumnCenter(const APosition: real;
  First, Last: integer): integer;
begin
  if First <> -1 then
  begin
    Dec(First);
  end;
  if Last <> -1 then
  begin
    Inc(Last);
  end;
  result := NearestColumnPosition(APosition, First, Last);
  if result > 0 then
  begin
    if (result > ColumnCount) or (ColumnPosition[result] > APosition) then
    begin
      Dec(result);
    end;
    if (result >= ColumnCount)  then
    begin
      result := ColumnCount -1
    end;
  end
  else
  begin
    result := 0;
  end;
end;

function TCustomGrid.NearestRowCenter(const APosition: real; First,
  Last: integer): integer;
begin
  if First <> -1 then
  begin
    Dec(First);
  end;
  if Last <> -1 then
  begin
    Inc(Last);
  end;
  result := NearestRowPosition(APosition, First, Last);
  if result > 0 then
  begin
    if (result > RowCount) or(RowPosition[result] > APosition) then
    begin
      Dec(result);
    end;
    if (result >= RowCount) then
    begin
      result := RowCount-1;
    end;
  end
  else
  begin
    result := 0;
  end;
end;

function TCustomGrid.ColumnCenter(const Column: integer): real;
begin
  result := (ColumnPosition[Column] + ColumnPosition[Column + 1])/2
end;

procedure TCustomGrid.ColumnsChanged;
begin
  if FColumnUpdate > 0 then Exit;
  
  GridChanged;
  if Assigned(TopGridObserver) then
  begin
    TopGridObserver.UpToDate := False;
    TopGridObserver.UpToDate := True;
  end;
end;

function TCustomGrid.RowCenter(const Row: integer): real;
begin
  result := (RowPosition[Row] + RowPosition[Row + 1])/2
end;

procedure TCustomGrid.RowsChanged;
begin
  if FRowUpdate > 0 then Exit;
  
  GridChanged;
  if Assigned(TopGridObserver) then
  begin
    TopGridObserver.UpToDate := False;
    TopGridObserver.UpToDate := True;
  end;
end;

procedure TCustomGrid.SetLocalEvalAt(ViewDirection: TViewDirection;
  var LocalEvalAt: TEvaluatedAt);
begin
  case ViewDirection of
    vdTop:
      begin
        if SideDataSet = nil then
        begin
          LocalEvalAt := eaBlocks;
        end
        else
        begin
          LocalEvalAt := SideDataSet.EvaluatedAt;
        end;
      end;
    vdFront:
      begin
        if FrontDataSet = nil then
        begin
          LocalEvalAt := eaBlocks;
        end
        else
        begin
          LocalEvalAt := FrontDataSet.EvaluatedAt;
        end;
      end;
    vdSide:
      begin
        if TopDataSet = nil then
        begin
          LocalEvalAt := eaBlocks;
        end
        else
        begin
          LocalEvalAt := TopDataSet.EvaluatedAt;
        end;
      end;
  end;
end;

procedure TCustomGrid.SetLayerLineColor(LayerIndex: Integer;
  LocalEvalAt: TEvaluatedAt; var LocalLineColor: TColor32;
  var LineWidth: single);
begin
  case LocalEvalAt of
    eaBlocks:
      begin
        if (LayerIndex = SelectedLayer)
          or (LayerIndex = SelectedLayer + 1) then
        begin
          LocalLineColor := Color32(ExistingLayerSelectionCellColor);
          LineWidth := ThickGridLineThickness;
        end
        else
        begin
          LocalLineColor := clBlack32;
        end;
      end;
    eaNodes:
      begin
        if (LayerIndex = SelectedLayer) then
        begin
          LocalLineColor := Color32(ExistingLayerSelectionCellColor);
          LineWidth := ThickGridLineThickness;
        end
        else
        begin
          LocalLineColor := clBlack32;
        end;
      end;
  else
    begin
      Assert(False);
    end;
  end;
end;

procedure TCustomGrid.SetColumnLineColor(ColIndex: Integer;
  LocalEvalAt: TEvaluatedAt; var LocalLineColor: TColor32;
  var LineWidth: single);
begin
  case LocalEvalAt of
    eaBlocks:
      begin
        if (ColIndex = SelectedColumn) or (ColIndex = SelectedColumn + 1) then
        begin
          LocalLineColor := Color32(ExistingColumnSelectionCellColor);
          LineWidth := ThickGridLineThickness;
        end
        else
        begin
          LocalLineColor := clBlack32;
        end;
      end;
    eaNodes:
      begin
        if (ColIndex = SelectedColumn) then
        begin
          LocalLineColor := Color32(ExistingColumnSelectionCellColor);
          LineWidth := ThickGridLineThickness;
        end
        else
        begin
          LocalLineColor := clBlack32;
        end;
      end;
  else
    begin
      Assert(False);
    end;
  end;
end;

procedure TCustomGrid.SetRowLineColor(RowIndex: Integer;
  LocalEvalAt: TEvaluatedAt; var LocalLineColor: TColor32;
  var LineWidth: single);
begin
  case LocalEvalAt of
    eaBlocks:
      begin
        if (RowIndex = SelectedRow) or (RowIndex = SelectedRow + 1) then
        begin
          LocalLineColor := Color32(ExistingRowSelectionCellColor);
          LineWidth := ThickGridLineThickness;
        end
        else
        begin
          LocalLineColor := clBlack32;
        end;
      end;
    eaNodes:
      begin
        if (RowIndex = SelectedRow) then
        begin
          LocalLineColor := Color32(ExistingRowSelectionCellColor);
          LineWidth := ThickGridLineThickness;
        end
        else
        begin
          LocalLineColor := clBlack32;
        end;
      end;
  else
    begin
      Assert(False);
    end;
  end;
end;

procedure TCustomGrid.DrawFrontContours(const ZoomBox: TQRbwZoomBox2;
      const BitMap: TBitmap32);
var
  Contourer: TMultipleContourCreator;
begin
  if (ColumnCount >= 0) and (RowCount > 0) and (LayerCount > 0) then
  begin
    if FrontContourDataSet <> nil then
    begin
      Contourer := TMultipleContourCreator.Create;
      try
        Contourer.DataSet := FrontContourDataSet;
        Contourer.ActiveDataSet := frmGoPhast.PhastModel.GetDataSetByName(rsActive);
        Contourer.BitMap := BitMap;
        Contourer.ViewDirection := vdFront;
        Contourer.Grid := ContourGrid(FrontContourDataSet.EvaluatedAt,
          frmGoPhast.PhastModel.ModelSelection, vdFront, SelectedRow);
        Contourer.ZoomBox := ZoomBox;
        Contourer.DrawContours(SelectedRow,
          frmGoPhast.PhastModel.ContourColors);
      finally
        Contourer.Free;
      end;
    end;
  end;
end;

end.
