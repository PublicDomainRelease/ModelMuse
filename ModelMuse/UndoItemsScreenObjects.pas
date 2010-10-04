{@abstract(@name is used to define a series of @link(TCustomUndo)s that
  relate to @link(TScreenObject)s.)  
  @author(Richard B. Winston <rbwinst@usgs.gov>)}
unit UndoItemsScreenObjects;

interface

uses SubscriptionUnit, PhastModelUnit, Types, SysUtils, Classes, Graphics,
  Contnrs, QuadTreeClass, UndoItems, AbstractGridUnit, ScreenObjectUnit,
  FastGEO, GoPhastTypes, ValueArrayStorageUnit;

type
  TScreenObjectEditItem = class(TCollectionItem)
  private
    FScreenObject: TScreenObject;
    FOwnScreenObject: boolean;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    property OwnScreenObject: boolean read FOwnScreenObject
      write FOwnScreenObject;
    property ScreenObject: TScreenObject read FScreenObject
      write FScreenObject;
  end;

  TScreenObjectEditCollection = class(TCollection)
  private
    FOwnScreenObject: boolean;
    procedure SetOwnScreenObject(const Value: boolean);
    function GetItems(Index: integer): TScreenObjectEditItem;
  public
    function Add: TScreenObjectEditItem;
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    property OwnScreenObject: boolean read FOwnScreenObject
      write SetOwnScreenObject;
    property Items[Index: integer]: TScreenObjectEditItem read GetItems; default;
  end;

  TCustomUpdateScreenObjectDisplayUndo =  class(TCustomUndo)
  protected
    FShouldUpdateShowHideObjectsDisplay: Boolean;
    FShouldUpdateShowHideObjects: boolean;
    // @name updates @link(frmShowHideObjects) if @link(frmShowHideObjects)
    // is not nil and  @link(FShouldUpdateShowHideObjects) is @true.
    // It also enables the "Invert Selection"
    procedure UpdateDisplay;
    procedure UpdateShowHideObjects;
    procedure EnableInvertSelection;
    procedure WarnSfrLengthProblems(List: TList);
  end;

  { @abstract(@name is used to change or restore which
    @link(TScreenObject)s or
    vertices in an individual @link(TScreenObject) are selected.)
    To use, Create a
    @name, change the @link(TScreenObject)s
    that are selected, and call
    @link(TUndoChangeSelection.SetPostSelection).
    Then Undo and Redo can be used to restore or redo
    the changes in what is selected.

    @longcode(#
      UndoChangeSelection := TUndoChangeSelection.Create;
      try
        // Change the selected TScreenObjects here.
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
    #)
  }
  TUndoChangeSelection = class(TCustomUpdateScreenObjectDisplayUndo)
  private
    procedure ResetVisible(const ScreenObjects: TList);
  protected
    {
     @name records the selected vertices of a single @link(TScreenObject) when
     @link(SetPostSelection) was called.}
    FNewSelectedVertices: T2DBoolArray;
    {
    @name records the selected @link(TScreenObject)s when
     @link(SetPostSelection) was called.}
    FNewSelectedScreenObjects: TList;
    {
     @name records the selected vertices of a single @link(TScreenObject) when
     the @classname was created.}
    FOldSelectedVertices: T2DBoolArray;
    {
     @name records the selected @link(TScreenObject)s when
     the @classname was created.}
    FOldSelectedScreenObjects: TList;
    FOldVisibleScreenObjects: TList;
    FNewVisibleScreenObjects: TList;
    FUpdateVisible: boolean;
    {@name sets the selected @link(TScreenObject)s and the selected vertices
     of a single @link(TScreenObject) to the values specified by
     ScreenObjects and SelectedVertices.}
    procedure ResetSelection(const ScreenObjects: TList;
      const SelectedVertices: T2DBoolArray);
    // @name stores the selected @link(TScreenObject)s and the selected
    // vertices if any) of a @link(TScreenObject) in
    // @link(FOldSelectedScreenObjects) and @link(FOldSelectedVertices).
    procedure SetPriorSelection;
    // @name stores the selected @link(TScreenObject)s in ScreenObjects
    // and the selected vertices of a single @link(TScreenObject) in
    // SelectedVertices.
    procedure SetSelection(const ScreenObjects: TList;
      var SelectedVertices: T2DBoolArray);
    procedure StoreVisible(const ScreenObjects: TList);
  protected
    // Description tells what @classname does.
    function Description: string; override;
    // @name causes all the @link(TQRbwZoomBox2 zoomboxes) to redraw.
    procedure InvalidateImages;
  public
    // @name creates an instance of @classname and stores the
    // selected @link(TScreenObject)s.
    constructor Create;
    // Destroy gets rid of the instance of TUndoChangeSelection.
    destructor Destroy; override;
    // @name causes the paint boxes showing the selected
    // @link(TScreenObject)s to
    // be invalidated.
    procedure DoCommand; override;
    // @name sets the selection to what it was when @Link(SetPostSelection)
    // was called.
    procedure Redo; override;
    // @name indicates whether or not the selection changed.
    function SelectionChanged: boolean;
    // @name stores the selected @link(TScreenObject)s and the selected
    // vertices if any) of a @link(TScreenObject) in
    // @link(FNewSelectedScreenObjects) and @link(FNewSelectedVertices).
    procedure SetPostSelection;
    // @name sets the selection to what it was when the TUndoChangeSelection
    // was created.
    procedure Undo; override;
  end;

  TCustomImportMultipleScreenObjects = class(TUndoChangeSelection)
  protected
    // @name holds the newly imported @link(TScreenObject)s.
    FNewScreenObjects: TList;
    procedure UnDeleteNewScreenObjects;
    procedure DeleteNewScreenObjects;
  public
    Destructor Destroy; override;
    // @name deselects all the existing @link(TScreenObject)s,
    // creates @link(FNewScreenObjects) and puts the contents of
    // ListOfScreenObjects in @link(FNewScreenObjects).
    procedure StoreNewScreenObjects(const ListOfScreenObjects: TList); virtual;
    procedure UpdateScreenObject(const AScreenObject: TScreenObject);
  end;

  TUndoPasteScreenObjects = class(TCustomImportMultipleScreenObjects)
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    procedure StoreNewScreenObjects(const ListOfScreenObjects: TList); override;
    procedure DoCommand; override;
    procedure Undo; override;
    procedure Redo; override;
  end;

  // @name provides methods that descendants can use to notify the
  // GUI of changes.
  TCustomUpdateScreenObjectUndo = class(TUndoChangeSelection)
  protected
    // @name tells @link(frmGoPhastUnit.frmGoPhast) which views of the model
    // need updating and then sets ScreenObject.@link(TObserver.UpToDate)
    // to @true.
    procedure UpdateScreenObject(const ScreenObject: TScreenObject);
    // @name tells @link(frmGoPhastUnit.frmGoPhast) to update the
    // selection rectangle.
    procedure UpdateSelectionRectangle;
  end;

  {@abstract(@name is used to undo or redo the creation of
    a @link(TScreenObject).)  The @link(TScreenObject) is not actually
    created or destroyed by @name.  Instead @name just sets
    TScreenObject.@link(TScreenObject.Deleted).}
  TUndoCreateScreenObject = class(TCustomUpdateScreenObjectUndo)
  private
    // See @link(HasBeenUsed).
    FHasBeenUsed: boolean;
    // @name: @link(TScreenObject);
    // @name is the @link(TScreenObject) that has been created.
    FScreenObject: TScreenObject;
    FUndoEditFluxObservations: TUndoEditFluxObservations;
    FSectionStarts: TValueArrayStorage;
    procedure WarnSfrLengthProblem;
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    // @name creates and instance of @classname.  AScreenObject is
    // the @link(TScreenObject) that has been created.
    constructor Create(const AScreenObject: TScreenObject);
    destructor Destroy; override;
    // @name sets Deleted to @False and makes other required changes.
    procedure DoCommand; override;
    // @name is set to @true in @link(DoCommand), @link(Redo) and @link(Undo);
    // It is used to indicate that the @classname needs to be freed when @name
    // is false/
    property HasBeenUsed: boolean read FHasBeenUsed write FHasBeenUsed;
    // @name calls @link(DoCommand).
    procedure Redo; override;
    // @name sets Deleted to @true and makes other required changes.
    procedure Undo; override;
    procedure UpdateObservations;
  end;

  {@abstract(@name is used to undo or redo the deletion of
    a one or more @link(TScreenObject)s.)
    The @link(TScreenObject)s are not actually
    created or destroyed by @name.  Instead @name just sets
    TScreenObject.@link(TScreenObject.Deleted).}
  TUndoDeleteScreenObjects = class(TCustomUpdateScreenObjectUndo)
  private
    // @name: TList;
    // @name is a list of @link(TScreenObject)s that are being deleted.
    FScreenObjects: TList;
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    // @name creates an instance of @classname.
    // @param(ListOfScreenObjects is a TList containing @link(TScreenObject)s
    // that are to be deleted.)
    constructor Create(const ListOfScreenObjects: TScreenObjectList);
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name sets Deleted to @True and makes other required changes.
    procedure DoCommand; override;
    // @name calls @link(DoCommand).
    procedure Redo; override;
    // @name sets Deleted to @False and makes other required changes.
    procedure Undo; override;
  end;

  TUndoCutScreenObjects = class(TUndoDeleteScreenObjects)
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    procedure DoCommand; override;
    procedure Redo; override;
  end;

  {@abstract(@name moves one or more @link(TScreenObject)s
    or points within them.)}
  TUndoMoveScreenObject = class(TCustomUpdateScreenObjectUndo)
  private
    // @name: array of Boolean;
    // each element of @name indicates whether the corresponding
    // @link(TScreenObject) can be moved.
    FCanMoveScreenObject: array of Boolean;
    // @name: @link(TViewDirection);
    // @name indicates the @link(TViewDirection) of the @link(TScreenObject)s
    // that will be moved.
    FDirection: TViewDirection;
    // @name: double;
    // @name is the distance in the X direction that the
    // @link(TScreenObject)s will be moved.
    FX: double;
    // @name: double;
    // @name is the distance in the Y direction that the
    // @link(TScreenObject)s will be moved.
    FY: double;
    // @name moves @link(TScreenObject)s by the amount XOffset, YOffset.
    procedure Move(const XOffset, YOffset: double;
      const Undoing: boolean);
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    // @name creates an instance of @classname.
    // @param(X is the distance in the X direction that the selected
    // @link(TScreenObject)s or the selected points in the
    // selected @link(TScreenObject) should be moved.)
    // @param(Y is the distance in the Y direction that the selected
    // @link(TScreenObject)s or the selected points in the
    // selected @link(TScreenObject) should be moved.)
    // @param(ADirection is the @link(TViewDirection) of the
    // @link(TScreenObject)s that should be moved.)
    constructor Create(const X, Y: double; const ADirection: TViewDirection);
    // @name moves the selected
    // @link(TScreenObject)s or the selected points in the
    // selected @link(TScreenObject).
    procedure DoCommand; override;
    // @name calls @link(DoCommand).
    procedure Redo; override;
    // @name reverses the movement of the selected
    // @link(TScreenObject)s or the selected points in the
    // selected @link(TScreenObject).
    procedure Undo; override;
  end;

  {@abstract(@name inserts a point into a @link(TScreenObject).)}
  TUndoInsertPoint = class(TCustomUpdateScreenObjectUndo)
  private
    // @name indicates whether or not the point can be inserted.
    FCanInsert: boolean;
    // @name is the point to be inserted.
    FPoint: TPoint2D;
    // @name is the position in TScreenObject.@link(TScreenObject.Points)
    // where @link(FPoint) should be inserted.
    FPosition: integer;
    // @name is the @link(TScreenObject) into
    // which @link(FPoint) is being inserted.
    FScreenObject: TScreenObject;
    FSectionStarts: TValueArrayStorage;
    FPoints: TRealPointArray;
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    // @name creates an instance of @classname.
    // @param(AScreenObject is is the @link(TScreenObject) into
    // which APoint is being inserted.)
    // @param(APosition is the position in
    // TScreenObject.@link(TScreenObject.Points)
    // where APoint should be inserted.)
    // @param(APoint is the point to be inserted.)
    constructor Create(const AScreenObject: TScreenObject;
      const APosition: integer; const APoint: TPoint2D);
    destructor Destroy; override;
    // @name inserts the point.
    procedure DoCommand; override;
    // @name calls @link(DoCommand).
    procedure Redo; override;
    // @name deletes the point.
    procedure Undo; override;
  end;

  // @abstract(@name is an abstract base class for classes that change the
  // order of @link(TScreenObject)s.)
  TCustomUndoChangeOrder = class(TCustomUpdateScreenObjectUndo)
  private
    // @name: TList;
    // @name is a list of the @link(TScreenObject)s whose positions will
    // be changed.
    FScreenObjectList: TScreenObjectList;
    // @name: array of integer;
    // @name is the original positions of the @link(TScreenObject)s in
    // @link(FScreenObjectList).
    FPositions: array of integer;
  public
    // @name creates an instance of @classname and initializes the private
    // fields.
    constructor Create(const ViewDirection: TViewDirection);
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call free instead.
    destructor Destroy; override;
    // @name calls @link(TUndoChangeSelection.DoCommand).
    procedure Redo; override;
  end;

  {@abstract(@name moves the selected @link(TScreenObject)s to the end of
    the list of @link(TScreenObject)s.)}
  TUndoToFront = class(TCustomUndoChangeOrder)
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    // @name moves the selected @link(TScreenObject)s to the end of
    // the list of @link(TScreenObject)s.
    procedure DoCommand; override;
    // @name restore the original order of the @link(TScreenObject)s.
    procedure Undo; override;
  end;

  {@abstract(@name moves the selected @link(TScreenObject)s to the beginning of
    the list of @link(TScreenObject)s.)}
  TUndoToBack = class(TCustomUndoChangeOrder)
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    // @name moves the selected @link(TScreenObject)s to the beginning of
    // the list of @link(TScreenObject)s.
    procedure DoCommand; override;
    // @name restore the original order of the @link(TScreenObject)s.
    procedure Undo; override;
  end;

  {@abstract(@name moves the selected @link(TScreenObject)s toward the end of
    the list of @link(TScreenObject)s by one position.)}
  TUndoMoveUp = class(TUndoToFront)
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    // @name moves the selected @link(TScreenObject)s toward the end of
    // the list of @link(TScreenObject)s by one position.
    procedure DoCommand; override;
  end;

  {@abstract(@name moves the selected @link(TScreenObject)s toward the
    beginning of the list of @link(TScreenObject)s by one position.)}
  TUndoMoveDown = class(TUndoToBack)
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
  public
    // @name moves the selected @link(TScreenObject)s toward the beginning of
    // the list of @link(TScreenObject)s by one position.
    procedure DoCommand; override;
  end;

  {@abstract(@name is used to change the order of @link(TScreenObject)s and
    to rename them.)}
  TUndoRearrangeScreenObjects = class(TCustomUpdateScreenObjectUndo)
  private
    // @name is a list of the @link(TScreenObject)s in their original order.
    // All the @link(TScreenObject)s must be included in @name.
    FList: TScreenObjectList;
    // @name is a list of the names of the @link(TScreenObject)s in
    // their original order.
    // All the names must be included in @name.
    FNames: TStringList;
  protected
    // @name describes what this @classname does.  It is used in menu captions
    // and hints.
    function Description: string; override;
    // @name Changes the order of the @link(TScreenObject)s to
    procedure SetOrder(const ScreenObjectList: TScreenObjectList;
      const NameList: TStringList);
  public
    // @name is a list of the @link(TScreenObject)s in their new order.
    // All the @link(TScreenObject)s must be included in @name.
    FNewList: TScreenObjectList;
    // @name is a list of the names of the @link(TScreenObject)s in
    // their new order.
    // All the names must be included in @name.
    FNewNames: TStringList;
    // @name sets the order of the @link(TScreenObject)s to be the order
    // specified in @link(FNewList) and the names to be the names specified
    // in @link(FNewNames).
    procedure DoCommand; override;
    // @name calls @link(DoCommand).
    procedure Redo; override;
    // @name restores the order and names of the @link(TScreenObject)s to be
    // the order order and names.
    procedure Undo; override;
    // @name creates an instance of @classname and initializes the private
    // fields.
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call free instead.
    destructor Destroy; override;
  end;

  {@abstract(@name is used to show or hide @link(TScreenObject)s.)}
  TUndoShowHideScreenObject = class(TUndoChangeSelection)
  protected
    // @name is a list of the @link(TScreenObject)s whose
    // @link(TScreenObject.Visible) property will be changed.
    FScreenObjectsToChange: TList;

    // @name tells what @classname does.
    function Description: string; override;
    // @name changes the @link(TScreenObject.Visible) property
    // of each @link(TScreenObject) in @link(FScreenObjectsToChange).
    procedure ToggleVisibility;

  public
    // @name stores ScreenObject and will toggle its
    // @link(TScreenObject.Visible) property when required.
    function AddScreenObjectToChange(const ScreenObject: TScreenObject):
      integer;
    // @name creates and instance of @classname.
    constructor Create;
    // @name destroys the current @classname.  Do not call @name directly.
    // Call Free instead.
    destructor Destroy; override;
    // @name changes the visibility of all the @link(TScreenObject)s
    // in @link(FScreenObjectsToChange).
    procedure DoCommand; override;
    // @name redoes the changes in visibility of all the @link(TScreenObject)s
    // in @link(FScreenObjectsToChange).
    procedure Redo; override;
    // @name undoes the changes in visibility of all the @link(TScreenObject)s
    // in @link(FScreenObjectsToChange).
    procedure Undo; override;
  end;

  // @abstract(@name is used to set or undo the setting of a
  // @link(TScreenObject).)
  TUndoSetScreenObjectProperties = class(TCustomUpdateScreenObjectDisplayUndo)
  private
    FNewScreenObjects: TScreenObjectEditCollection;
    FOldScreenObjects: TScreenObjectEditCollection;
    FExistingScreenObjects: TScreenObjectEditCollection;
    // @name: TList;
    // @name contains all the @link(TScreenObject)'s whose properties
    // are being changed.
    FListOfScreenObjects: TList;
    FUndoEditFluxObservations: TUndoEditFluxObservations;
  protected
    // If @name is true then @link(frmShowHideObjects) can be updated.
    // Descendants should set @name to @false before calling inherited
    // @link(DoCommand) or @link(Undo) if they will update
    // @link(frmShowHideObjects) with @link(UpdateShowHideObjects).
    // Description describes what this TCustomUndo does for display in
    // a menu or tool tip.
    function Description: string; override;
  private
    // @name is called to make sure each
    // @link(TScreenObject)'s subscriptions are set appropriately.
    // @name calls TScreenObject.@link(
    // TScreenObject.ResetMixtureSubscriptions)
    // and TScreenObject.@link(
    // TScreenObject.ResetBoundaryMixtureSubscriptions) for each
    // @link(TScreenObject).
    procedure ResetScreenObjectDataSetSubscriptions;
  public
    // @name determines whether or not the
    // @link(TUndoSetScreenObjectProperties)
    // will need to notify the grid that the
    // colors of the cells needs to be updated.
    FSetCellsColor: boolean;
    // @name assigns the new values to the @link(TScreenObject)s.
    procedure DoCommand; override;
    // @name restores the old values to the @link(TScreenObject)s.
    procedure Undo; override;
    // @name stores the @link(TScreenObject)s in AListOfScreenObjects and reads
    // and stores their properties.
    // @param(AListOfScreenObjects contains the @link(TScreenObject)s
    // that will be affected by this @classname.)
    constructor Create(const AListOfScreenObjects: TList; var NewScreenObjects,
      OldScreenObjects: TScreenObjectEditCollection);
    // @name destroys the @link(TUndoSetScreenObjectProperties)
    // and releases its memory.
    destructor Destroy; override;
    procedure UpdateObservations;
  end;

  {@abstract(@name is used to update the view of a @link(TScreenObject).)}
  {@abstract(@name is used to delete a segment of a @link(TScreenObject).)}
  TUndoDeleteSegment = class(TCustomUpdateScreenObjectUndo)
  private
    FSections: TValueArrayStorage;
    // @name indicates which segment is being deleted.
    FEdge: integer;
    // If deleting a segment divides the @link(TScreenObject) into two
    // @link(TScreenObject)s, @name will be one of @link(TScreenObject).
    FNewScreenObject: TScreenObject;
    // @name stores the @link(TScreenObject.Points) of the @link(TScreenObject)
    // which is having a segment deleted.
    FPoints: TRealPointArray;
    // @name stores the @link(TScreenObject.SelectedVertices)
    // of the @link(TScreenObject)
    // which is having a segment deleted.
    FSelectedVertices: TBooleanDynArray;
    // @name is the @link(TScreenObject) from which a segment is being deleted.
    FScreenObject: TScreenObject;
    FSetPropertiesOfEnclosed: boolean;
  protected
    // @name tells what @classname does.
    function Description: string; override;
  public
    // @name creates an instance of @classname and store the information
    // needed to delete or restore a segment.
    constructor Create(const AScreenObject: TScreenObject;
      const AnEdge: integer);
    destructor Destroy; override;
    // @name deletes a segment in a @link(TScreenObject).
    procedure DoCommand; override;
    // @name re-deletes a segment in a @link(TScreenObject).
    procedure Redo; override;
    // @name restores a segment in a @link(TScreenObject).
    procedure Undo; override;
  end;

  TUndoMergeObjects = class(TCustomUpdateScreenObjectUndo)
  private
    FScreenObjects: TList;
    FOldPoints: T2DRealPointArray;
    FQuadTree: TRbwQuadTree;
    FDeleted: array of boolean;
    FNewPoints: T2DRealPointArray;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Undo; override;
    procedure DoCommand; override;
    procedure Redo; override;
    function ShouldUse: boolean;
    Function Description: string; override;
  end;

  TUndoReverseVerticies = class(TCustomUpdateScreenObjectUndo)
  private
    FScreenObjects: TList;
    procedure ReverseObjects;
  protected
    // @name tells what @classname does.
    function Description: string; override;
  public
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  {@abstract(@name is used to delete a vertex of a @link(TScreenObject).)}
  TUndoDeleteVertices = class(TCustomUpdateScreenObjectUndo)
  private
    // @name: boolean;
    // @name indicates whether a the vertices can be deleted from the selected
    // @link(TScreenObject).  If deleting a vertex would cause the
    // @link(TScreenObject) to cross itself, the vertices can not be deleted.
    FCanDeleteVertices: boolean;
    // @name: array of array of TPoint2D;
    // @name holds the TPoint2Ds
    // of the @link(TScreenObject)s.
    FPoints: array of TRealPointArray;
    FSectionStarts: array of TValueArrayStorage;
    // @name: TList;
    // @name is a list of the @link(TScreenObject)s from which vertices will
    // be deleted.
    FScreenObjects: TList;
    FVertexValues: TList;
  protected
    // @name tells what @classname does.
    function Description: string; override;
  public
    // @name creates an instance of @classname and stores a list of the
    // @link(TScreenObject)s from which vertices will be deleted.
    constructor Create(const ListOfScreenObjects: TScreenObjectList);
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    // @name deletes the selected vertices
    // from the selected @link(TScreenObject)s
    procedure DoCommand; override;
    // @name re-deletes the selected vertices from the selected
    // @link(TScreenObject)s
    procedure Redo; override;
    // @name restores the deleted vertices
    // from the selected @link(TScreenObject)s
    procedure Undo; override;
  end;

  TUndoAddPart = class(TCustomUpdateScreenObjectUndo)
  private
    FScreenObject: TScreenObject;
    FNewPoints: TRealPointArray;
    FCount: integer;
    FCapacity: integer;
    procedure Grow;
    procedure SetCapacity(const Value: integer);
  protected
    // @name tells what @classname does.
    property Capacity: integer read FCapacity write SetCapacity;
    property Count: integer read FCount;
    function Description: string; override;
  public
    procedure AddPoint(Point: TPoint2D);
    Constructor Create(ScreenObject: TScreenObject);
    procedure DoCommand; override;
    procedure Redo; override;
    procedure Undo; override;
  end;

implementation

uses Math, frmGoPhastUnit, frmSelectedObjectsUnit, frmShowHideObjectsUnit,
  InteractiveTools, PhastDataSets, DataSetUnit, CountObjectsUnit, 
  ModflowSfrReachUnit, frmErrorsAndWarningsUnit, IntListUnit;

{ TCustomUpdateScreenObjectDisplayUndo }

procedure TCustomUpdateScreenObjectDisplayUndo.EnableInvertSelection;
begin
  frmGoPhast.EnableInvertSelection;
end;

procedure TCustomUpdateScreenObjectDisplayUndo.UpdateDisplay;
begin
  frmSelectedObjects.UpdateDisplay;
  UpdateShowHideObjects;
  EnableInvertSelection;
end;

procedure TCustomUpdateScreenObjectDisplayUndo.UpdateShowHideObjects;
begin
  if frmShowHideObjects <> nil then
  begin
    if FShouldUpdateShowHideObjects then
    begin
      frmShowHideObjects.UpdateScreenObjects;
    end
    else if FShouldUpdateShowHideObjectsDisplay then
    begin
      frmShowHideObjects.SupressUndo := True;
      try
        frmShowHideObjects.UpdateDisplay;
      finally
        frmShowHideObjects.SupressUndo := False;
      end;
    end;
  end;
end;

procedure TCustomUpdateScreenObjectDisplayUndo.WarnSfrLengthProblems(
  List: TList);
const
  WarningRoot = 'SFR Reach Length is zero.';
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  Item: TSfrItem;
  ErrorFound: boolean;
begin
  if not frmGoPhast.PhastModel.ModflowPackages.SfrPackage.IsSelected then
  begin
    Exit;
  end;

  frmErrorsAndWarnings.RemoveWarningGroup(WarningRoot);
  ErrorFound := False;
  for ScreenObjectIndex := 0 to List.Count - 1 do
  begin
    AScreenObject := List[ScreenObjectIndex];
    if AScreenObject.Deleted then
    begin
      Continue;
    end;
    if AScreenObject.ModflowSfrBoundary = nil then
    begin
      Continue;
    end;
    if not AScreenObject.ModflowSfrBoundary.Used then
    begin
      Continue;
    end;

    Assert(AScreenObject.ModflowSfrBoundary.Values.Count> 0);
    Item := AScreenObject.ModflowSfrBoundary.Values[0] as TSfrItem;
    if (Trim(Item.ReachLength) = '0') then
    begin
      ErrorFound := True;
      frmErrorsAndWarnings.AddWarning(WarningRoot, AScreenObject.Name);
      Continue;
    end;

    if AScreenObject.ScreenObjectLength > 0 then
    begin
      Continue;
    end;

    if (UpperCase(Item.ReachLength) = UpperCase(StrObjectIntersectLength)) then
    begin
      ErrorFound := True;
      frmErrorsAndWarnings.AddWarning(WarningRoot, AScreenObject.Name);
    end;
  end;
  if ErrorFound then
  begin
    frmErrorsAndWarnings.ShowAfterDelay;
  end;
end;

{ TUndoChangeSelection }

constructor TUndoChangeSelection.Create;
begin
  inherited;
  FOldSelectedScreenObjects := TList.Create;
  FNewSelectedScreenObjects := TList.Create;
  FOldVisibleScreenObjects := TList.Create;
  FNewVisibleScreenObjects := TList.Create;
  SetPriorSelection;
  FShouldUpdateShowHideObjectsDisplay := True;
end;

function TUndoChangeSelection.Description: string;
begin
  result := 'change selection';
end;

destructor TUndoChangeSelection.Destroy;
begin
  FOldSelectedScreenObjects.Free;
  FNewSelectedScreenObjects.Free;
  FOldVisibleScreenObjects.Free;
  FNewVisibleScreenObjects.Free;
  inherited;
end;

procedure TUndoChangeSelection.DoCommand;
begin
  InvalidateImages;
//  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
end;

procedure TUndoChangeSelection.InvalidateImages;
begin
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;

  frmGoPhast.frameTopView.ZoomBox.Image32.Invalidate;
  frmGoPhast.frameFrontView.ZoomBox.Image32.Invalidate;
  frmGoPhast.frameSideView.ZoomBox.Image32.Invalidate;
end;

procedure TUndoChangeSelection.Redo;
begin
  ResetSelection(FNewSelectedScreenObjects, FNewSelectedVertices);
  if FUpdateVisible then
  begin
    ResetVisible(FNewVisibleScreenObjects);
  end;
  SelectScreenObjectTool.ShouldDrawSelectionRectangle :=
    FNewSelectedScreenObjects.Count > 0;
  InvalidateImages;
  UpdateDisplay;
end;

procedure TUndoChangeSelection.ResetVisible(const ScreenObjects: TList);
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
begin
  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObjects.IndexOf(AScreenObject) >= 0 then
    begin
      AScreenObject.Visible := True;
    end
    else
    begin
      AScreenObject.Visible := False;
    end;
  end;
end;

procedure TUndoChangeSelection.ResetSelection(const ScreenObjects: TList;
  const SelectedVertices: T2DBoolArray);
var
  ScreenObjectIndex, VertexIndex: integer;
  AScreenObject: TScreenObject;
begin
  frmGoPhast.ChangingSelection := True;
  try
    for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
      if ScreenObjects.IndexOf(AScreenObject) >= 0 then
      begin
        AScreenObject.Selected := True;
      end
      else
      begin
        AScreenObject.Selected := False;
      end;
    end;
    for ScreenObjectIndex := 0 to ScreenObjects.Count - 1 do
    begin
      AScreenObject := ScreenObjects[ScreenObjectIndex];
      if Length(SelectedVertices[ScreenObjectIndex]) > 0 then
      begin
        for VertexIndex := 0 to AScreenObject.Count - 1 do
        begin
          AScreenObject.SelectedVertices[VertexIndex] :=
            SelectedVertices[ScreenObjectIndex, VertexIndex];
        end;
      end
      else
      begin
        if AScreenObject.SelectedVertexCount > 0 then
        begin
          AScreenObject.ClearSelectedVertices;
        end;
      end;
    end;
    FShouldUpdateShowHideObjects := True;
    UpdateDisplay;
  finally
    frmGoPhast.ChangingSelection := False;
  end;
end;

function TUndoChangeSelection.SelectionChanged: boolean;
var
  Index, VertexIndex: integer;
begin
  result := (FOldSelectedScreenObjects.Count <> FNewSelectedScreenObjects.Count);
  if result then
    Exit;
  for Index := 0 to FOldSelectedScreenObjects.Count - 1 do
  begin
    result := (FOldSelectedScreenObjects[Index] <>
      FNewSelectedScreenObjects[Index]);
    if result then
      Exit;
  end;
  for Index := 0 to Length(FOldSelectedVertices) - 1 do
  begin
    result := Length(FOldSelectedVertices[Index]) <>
      Length(FNewSelectedVertices[Index]);
    if result then
      Exit;
    for VertexIndex := 0 to Length(FOldSelectedVertices[Index]) - 1 do
    begin
      result := FOldSelectedVertices[Index, VertexIndex] <>
        FNewSelectedVertices[Index, VertexIndex];
      if result then
        Exit;
    end;
  end;
end;

procedure TUndoChangeSelection.SetPostSelection;
var
  Index: Integer;
begin
  SetSelection(FNewSelectedScreenObjects, FNewSelectedVertices);
  SelectScreenObjectTool.ShouldDrawSelectionRectangle :=
    FNewSelectedScreenObjects.Count > 0;
  StoreVisible(FNewVisibleScreenObjects);
  FUpdateVisible :=
    FNewVisibleScreenObjects.Count <> FOldVisibleScreenObjects.Count;
  if not FUpdateVisible  then
  begin
    for Index := 0 to FNewVisibleScreenObjects.Count - 1 do
    begin
      if FNewVisibleScreenObjects[Index] <> FOldVisibleScreenObjects[Index] then
      begin
        FUpdateVisible := True;
        break;
      end;
    end;
  end;
end;

procedure TUndoChangeSelection.SetPriorSelection;
begin
  SetSelection(FOldSelectedScreenObjects, FOldSelectedVertices);
  StoreVisible(FOldVisibleScreenObjects);
end;

procedure TUndoChangeSelection.StoreVisible(const ScreenObjects: TList);
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
begin
  ScreenObjects.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    if AScreenObject.Visible then
    begin
      ScreenObjects.Add(AScreenObject)
    end;
  end;
  ScreenObjects.Capacity := ScreenObjects.Count;
end;

procedure TUndoChangeSelection.SetSelection(const ScreenObjects: TList;
  var SelectedVertices: T2DBoolArray);
var
  ScreenObjectIndex, VertexIndex: integer;
  AScreenObject: TScreenObject;
begin
  ScreenObjects.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    if AScreenObject.Selected then
    begin
      ScreenObjects.Add(AScreenObject)
    end;
  end;
  SetLength(SelectedVertices, ScreenObjects.Count);
  for ScreenObjectIndex := 0 to ScreenObjects.Count - 1 do
  begin
    AScreenObject := ScreenObjects[ScreenObjectIndex];
    if AScreenObject.SelectedVertexCount > 0 then
    begin
      SetLength(SelectedVertices[ScreenObjectIndex], AScreenObject.Count);
      for VertexIndex := 0 to AScreenObject.Count - 1 do
      begin
        SelectedVertices[ScreenObjectIndex, VertexIndex] :=
          AScreenObject.SelectedVertices[VertexIndex];
      end;
    end
    else
    begin
      SetLength(SelectedVertices[ScreenObjectIndex], 0);
    end;
  end;
end;

procedure TUndoChangeSelection.Undo;
begin
  ResetSelection(FOldSelectedScreenObjects, FOldSelectedVertices);
  if FUpdateVisible then
  begin
    ResetVisible(FOldVisibleScreenObjects);
  end;
  SelectScreenObjectTool.ShouldDrawSelectionRectangle :=
    FOldSelectedScreenObjects.Count > 0;
  InvalidateImages;
  UpdateDisplay;
end;

{ TUndoCreateScreenObject }

constructor TUndoCreateScreenObject.Create(const AScreenObject: TScreenObject);
begin
  inherited Create;
  HasBeenUsed := False;
  Assert(AScreenObject <> nil);
  FScreenObject := AScreenObject;
  FSectionStarts:= TValueArrayStorage.Create;
  FSectionStarts.Assign(FScreenObject.SectionStarts);
  FUndoEditFluxObservations:= TUndoEditFluxObservations.Create;
  UpdateObservations;
end;

function TUndoCreateScreenObject.Description: string;
begin
  result := 'create object';
end;

destructor TUndoCreateScreenObject.Destroy;
begin
  FUndoEditFluxObservations.Free;
  FSectionStarts.Free;
  inherited;
end;

procedure TUndoCreateScreenObject.DoCommand;
begin
  HasBeenUsed := True;
  if FScreenObject.Count = 0 then
  begin
    FScreenObject.Deleted := true;
  end;
  if FScreenObject.Deleted and (FScreenObject.Count > 0) then
  begin
    FScreenObject.Deleted := False;
    FScreenObject.Invalidate;
    UpdateScreenObject(FScreenObject);
    FScreenObject.SectionStarts.Assign(FSectionStarts);
  end;
  FUndoEditFluxObservations.DoCommand;
  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
  WarnSfrLengthProblem;

end;

procedure TUndoCreateScreenObject.Redo;
begin
  DoCommand;
  inherited;
  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
  WarnSfrLengthProblem;
end;

procedure TUndoCreateScreenObject.Undo;
begin
  inherited;
  HasBeenUsed := True;
  if not FScreenObject.Deleted or (FScreenObject.Count = 0) then
  begin
    FScreenObject.Deleted := True;
    FScreenObject.Invalidate;
    UpdateScreenObject(FScreenObject);
  end;
  FUndoEditFluxObservations.Undo;
  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
  WarnSfrLengthProblem;
end;

procedure TUndoCreateScreenObject.UpdateObservations;
var
  Model: TPhastModel;
begin
  Model := frmGoPhast.PhastModel;
  FUndoEditFluxObservations.AssignNewObservations(Model.HeadFluxObservations,
    Model.DrainObservations, Model.GhbObservations, Model.RiverObservations);
end;

procedure TUndoCreateScreenObject.WarnSfrLengthProblem;
var
  List: TList;
begin
  List := TList.Create;
  try
    List.Add(FScreenObject);
    WarnSfrLengthProblems(List);
  finally
    List.Free;
  end;
end;

{ TUndoDeleteScreenObjects }

constructor TUndoDeleteScreenObjects.Create(
  const ListOfScreenObjects: TScreenObjectList);
var
  Index: Integer;
begin
  inherited Create;
  FScreenObjects := TList.Create;
  FScreenObjects.Capacity := ListOfScreenObjects.Count;
  for Index := 0 to ListOfScreenObjects.Count - 1 do
  begin
    FScreenObjects.Add(ListOfScreenObjects[Index]);
  end;
end;

function TUndoDeleteScreenObjects.Description: string;
begin
  if FScreenObjects.Count > 1 then
  begin
    result := 'delete objects';
  end
  else
  begin
    result := 'delete object';
  end;
end;

destructor TUndoDeleteScreenObjects.Destroy;
begin
  FScreenObjects.Free;
  inherited;
end;

procedure TUndoDeleteScreenObjects.DoCommand;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  Assert(FScreenObjects.Count > 0);
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    AScreenObject := FScreenObjects[Index];
    if not AScreenObject.Deleted then
    begin
      if not AScreenObject.UpToDate then
      begin
        AScreenObject.UpToDate := True;
      end;
      AScreenObject.Deleted := True;
      AScreenObject.Invalidate;
      UpdateScreenObject(AScreenObject);
    end;
  end;
  SelectScreenObjectTool.ShouldDrawSelectionRectangle := False;
  UpdateSelectionRectangle;
  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
end;

procedure TUndoDeleteScreenObjects.Redo;
begin
  inherited;
  DoCommand;
end;

procedure TUndoDeleteScreenObjects.Undo;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    AScreenObject := FScreenObjects[Index];
    if AScreenObject.Deleted then
    begin
      if not AScreenObject.UpToDate then
      begin
        AScreenObject.UpToDate := True;
      end;
      AScreenObject.Deleted := False;
      AScreenObject.Invalidate;
      UpdateScreenObject(AScreenObject);
    end;
  end;
  inherited;
  SelectScreenObjectTool.ShouldDrawSelectionRectangle := True;
  UpdateSelectionRectangle;
  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
end;

{ TUndoMoveScreenObject }

constructor TUndoMoveScreenObject.Create(const X, Y: double;
  const ADirection: TViewDirection);
begin
  inherited Create;
  FDirection := ADirection;
  FX := X;
  FY := Y;
end;

function TUndoMoveScreenObject.Description: string;
begin
  result := 'move objects'
end;

procedure TUndoMoveScreenObject.DoCommand;
begin
  Move(FX, FY, False);
end;

procedure TUndoMoveScreenObject.Move(const XOffset, YOffset: double;
  const Undoing: boolean);
var
  ScreenObjectIndex, PointIndex: integer;
  AScreenObject: TScreenObject;
  APoint: TPoint2D;
  MoveAll: boolean;
  Points: array of TPoint2D;
  TempScreenObject: TScreenObject;
  SectionIndex: integer;
  NextStart: Integer;
  NewSection: Boolean;
begin
  if not Undoing then
  begin
    SetLength(FCanMoveScreenObject, frmGoPhast.PhastModel.ScreenObjectCount);
  end;
  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    AScreenObject.ResetSubscriptions;
    if not Undoing then
    begin
      FCanMoveScreenObject[ScreenObjectIndex] := False;
    end;
    if (AScreenObject.ViewDirection = FDirection) and AScreenObject.Selected then
    begin
      if not Undoing then
      begin
        FCanMoveScreenObject[ScreenObjectIndex] := True;
      end;
      MoveAll := not ((AScreenObject.SelectedVertexCount > 0)
        and (AScreenObject.SelectedVertexCount < AScreenObject.Count));
      if Length(Points) < AScreenObject.Count then
      begin
        SetLength(Points, AScreenObject.Count);
      end;
      for PointIndex := 0 to AScreenObject.Count - 1 do
      begin
        if MoveAll or AScreenObject.SelectedVertices[PointIndex] then
        begin
          APoint := AScreenObject.Points[PointIndex];
          APoint.X := APoint.X + XOffset;
          APoint.Y := APoint.Y + YOffset;
          Points[PointIndex] := APoint;
        end
        else
        begin
          Points[PointIndex] := AScreenObject.Points[PointIndex];
        end;
      end;
      if not Undoing and not MoveAll then
      begin
        TempScreenObject := frmGoPhast.PhastModel.ScreenObjectClass.Create(nil);
        try
          TempScreenObject.Capacity := AScreenObject.Count;
          SectionIndex := 0;
          NextStart := AScreenObject.SectionStart[SectionIndex];

          for PointIndex := 0 to AScreenObject.Count - 1 do
          begin
            NewSection := PointIndex = NextStart;
            if NewSection then
            begin
              Inc(SectionIndex);
              if (SectionIndex < AScreenObject.SectionCount) then
              begin
                NextStart := AScreenObject.SectionStart[SectionIndex];
              end;
            end;
            try
              if (PointIndex > 0) and (SectionIndex < AScreenObject.SectionCount)
                and (PointIndex = AScreenObject.SectionStart[SectionIndex]) then
              begin
                TempScreenObject.SectionStarts.Add;
                TempScreenObject.SectionStarts.IntValues[TempScreenObject.SectionStarts.Count-1] := PointIndex;
//                (TempScreenObject.Sections.Add as TIntegerItem).Value := PointIndex;
                Inc(SectionIndex);
              end;
              TempScreenObject.AddPoint(Points[PointIndex], NewSection);
            except on E: EScreenObjectError do
              begin
                //ignore
              end;
            end;
            if TempScreenObject.Count <> PointIndex + 1 then
            begin
              FCanMoveScreenObject[ScreenObjectIndex] := False;
              Break;
            end;
          end;
        finally
          TempScreenObject.Free;
        end;
      end;

      if FCanMoveScreenObject[ScreenObjectIndex] then
      begin
        AScreenObject.BeginUpdate;
        try
          for PointIndex := 0 to AScreenObject.Count - 1 do
          begin
            if MoveAll or AScreenObject.SelectedVertices[PointIndex] then
            begin
              AScreenObject.Points[PointIndex] := Points[PointIndex];
            end;
          end;
        finally
          AScreenObject.EndUpdate;
        end;
        UpdateScreenObject(AScreenObject);
      end;
      AScreenObject.Invalidate;
      AScreenObject.UpToDate := True;
    end;
  end;
  UpdateSelectionRectangle;
  FShouldUpdateShowHideObjects := False;
  UpdateDisplay;
end;

procedure TUndoMoveScreenObject.Redo;
begin
  inherited;
  DoCommand;
end;

procedure TUndoMoveScreenObject.Undo;
begin
  Move(-FX, -FY, True);
end;

{ TUndoInsertPoint }

constructor TUndoInsertPoint.Create(const AScreenObject: TScreenObject;
  const APosition: integer; const APoint: TPoint2D);
begin
  inherited Create;
  FScreenObject := AScreenObject;
  FSectionStarts:= TValueArrayStorage.Create;
  FSectionStarts.Assign(AScreenObject.SectionStarts);
  AScreenObject.MovePoints(FPoints);
  FPosition := APosition;
  FPoint := APoint;
end;

function TUndoInsertPoint.Description: string;
begin
  result := 'insert node';
end;

destructor TUndoInsertPoint.Destroy;
begin
  FSectionStarts.Free;
  inherited;
end;

procedure TUndoInsertPoint.DoCommand;
var
  TempScreenObject: TScreenObject;
  Index: integer;
  SectionIndex: Integer;
  NewPart: Boolean;
  NextStart: Integer;
begin
  inherited;
  FCanInsert := True;
  TempScreenObject := frmGoPhast.PhastModel.ScreenObjectClass.Create(nil);
  try
    TempScreenObject.Capacity := FScreenObject.Count + 1;
    SectionIndex := 0;
    NextStart := 0;
    for Index := 0 to FPosition - 1 do
    begin
      NewPart := Index = NextStart;
      if NewPart then
      begin
        Inc(SectionIndex);
        if SectionIndex < FScreenObject.SectionCount then
        begin
          NextStart := FScreenObject.SectionStart[SectionIndex];
        end;
      end;
      try
        TempScreenObject.AddPoint(FScreenObject.Points[Index], NewPart);
      except on E: EScreenObjectError do
        begin
          // ignore
        end;
      end;
      if TempScreenObject.Count <> Index + 1 then
      begin
        FCanInsert := False;
        break;
      end;
    end;
    if FCanInsert then
    begin
      try
        TempScreenObject.AddPoint(FPoint, False);
      except on E: EScreenObjectError do
        begin
          // ignore
        end;
      end;
      if TempScreenObject.Count <> FPosition + 1 then
      begin
        FCanInsert := False;
      end;
    end;
    if FCanInsert then
    begin
      for Index := FPosition to FScreenObject.Count - 1 do
      begin
        NewPart := Index = NextStart;
        if NewPart then
        begin
          Inc(SectionIndex);
          if SectionIndex < FScreenObject.SectionCount then
          begin
            NextStart := FScreenObject.SectionStart[SectionIndex];
          end;
        end;
        try
          TempScreenObject.AddPoint(FScreenObject.Points[Index], NewPart);
        except on E: EScreenObjectError do
          begin
            // ignore
          end;
        end;
        if TempScreenObject.Count <> Index + 2 then
        begin
          FCanInsert := False;
          break;
        end;
      end;
    end;
  finally
    TempScreenObject.Free;
  end;

  if FCanInsert then
  begin
    FScreenObject.InsertPoint(FPosition, FPoint);
    FScreenObject.SelectedVertices[FPosition] := True;
    FScreenObject.Invalidate;
    UpdateScreenObject(FScreenObject);
    UpdateSelectionRectangle;
    FShouldUpdateShowHideObjects := False;
    UpdateDisplay;
  end;
end;

procedure TUndoInsertPoint.Redo;
begin
  if FCanInsert then
  begin
    DoCommand;
    inherited;
  end;
end;

procedure TUndoInsertPoint.Undo;
begin
  if FCanInsert then
  begin
    FScreenObject.MoveToPoints(FPoints);
    FScreenObject.Count := Length(FPoints);
    FScreenObject.SectionStarts := FSectionStarts;
    FScreenObject.Invalidate;
    UpdateScreenObject(FScreenObject);
    UpdateSelectionRectangle;
    FShouldUpdateShowHideObjects := False;
    UpdateDisplay;
    inherited;
  end;
end;

{ TUndoToFront }

function TUndoToFront.Description: string;
begin
  result := 'top-view objects to front';
end;

procedure TUndoToFront.DoCommand;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  //  inherited;
  for Index := 0 to FScreenObjectList.Count - 1 do
  begin
    AScreenObject := FScreenObjectList[Index];
    frmGoPhast.PhastModel.ExtractScreenObject(AScreenObject);
    frmGoPhast.PhastModel.AddScreenObject(AScreenObject);
    AScreenObject.Invalidate;
    AScreenObject.UpToDate := True;
  end;
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;
  FShouldUpdateShowHideObjects := False;
  UpdateDisplay;
end;

procedure TUndoToFront.Undo;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  inherited;
  for Index := 0 to FScreenObjectList.Count - 1 do
  begin
    AScreenObject := FScreenObjectList[Index];
    frmGoPhast.PhastModel.ExtractScreenObject(AScreenObject);
    frmGoPhast.PhastModel.InsertScreenObject(FPositions[Index], AScreenObject);
    AScreenObject.Invalidate;
    AScreenObject.UpToDate := True;
  end;
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;
end;

{ TCustomUndoChangeOrder }

constructor TCustomUndoChangeOrder.Create(const ViewDirection: TViewDirection);
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  inherited Create;
  FScreenObjectList := TScreenObjectList.Create;
  FScreenObjectList.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  SetLength(FPositions, FScreenObjectList.Capacity);
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Selected and (AScreenObject.ViewDirection = ViewDirection)
      then
    begin
      FPositions[FScreenObjectList.Add(AScreenObject)] := Index;
    end;
  end;
  SetLength(FPositions, FScreenObjectList.Count);
end;

destructor TCustomUndoChangeOrder.Destroy;
begin
  FScreenObjectList.Free;
  inherited;
end;

procedure TCustomUndoChangeOrder.Redo;
begin
  inherited;
  DoCommand;
end;

{ TUndoToBack }

function TUndoToBack.Description: string;
begin
  result := 'top-view objects to back';
end;

procedure TUndoToBack.DoCommand;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  //  inherited;
  for Index := FScreenObjectList.Count - 1 downto 0 do
  begin
    AScreenObject := FScreenObjectList[Index];
    frmGoPhast.PhastModel.ExtractScreenObject(AScreenObject);
    frmGoPhast.PhastModel.InsertScreenObject(0, AScreenObject);
    AScreenObject.Invalidate;
    AScreenObject.UpToDate := True;
  end;
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;
  FShouldUpdateShowHideObjects := False;
  UpdateDisplay;
end;

procedure TUndoToBack.Undo;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  inherited;
  for Index := FScreenObjectList.Count - 1 downto 0 do
  begin
    AScreenObject := FScreenObjectList[Index];
    frmGoPhast.PhastModel.ExtractScreenObject(AScreenObject);
    frmGoPhast.PhastModel.InsertScreenObject(FPositions[Index], AScreenObject);
    AScreenObject.Invalidate;
    AScreenObject.UpToDate := True;
  end;
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;
end;

{ TUndoMoveUp }

function TUndoMoveUp.Description: string;
begin
  result := 'top-view objects forward one';
end;

procedure TUndoMoveUp.DoCommand;
var
  Index: integer;
  AScreenObject: TScreenObject;
  NewPosition: integer;
begin
  //  inherited;
  for Index := 0 to FScreenObjectList.Count - 1 do
  begin
    AScreenObject := FScreenObjectList[Index];
    NewPosition := FPositions[Index] + 1;
    if NewPosition = frmGoPhast.PhastModel.ScreenObjectCount then
      break;
    frmGoPhast.PhastModel.ExtractScreenObject(AScreenObject);
    frmGoPhast.PhastModel.InsertScreenObject(NewPosition, AScreenObject);
    AScreenObject.Invalidate;
    AScreenObject.UpToDate := True;
  end;
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;
  FShouldUpdateShowHideObjects := False;
  UpdateDisplay;
end;

{ TUndoMoveDown }

function TUndoMoveDown.Description: string;
begin
  result := 'top-view objects back one';
end;

procedure TUndoMoveDown.DoCommand;
var
  Index: integer;
  AScreenObject: TScreenObject;
  NewPosition: integer;
begin
  //  inherited;
  for Index := FScreenObjectList.Count - 1 downto 0 do
  begin
    AScreenObject := FScreenObjectList[Index];
    NewPosition := FPositions[Index] - 1;
    if NewPosition = -1 then
      break;
    frmGoPhast.PhastModel.ExtractScreenObject(AScreenObject);
    frmGoPhast.PhastModel.InsertScreenObject(NewPosition, AScreenObject);
    AScreenObject.Invalidate;
    AScreenObject.UpToDate := True;
  end;
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;
  FShouldUpdateShowHideObjects := False;
  UpdateDisplay;
end;

{ TUndoRearrangeScreenObjects }

constructor TUndoRearrangeScreenObjects.Create;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  inherited;
  FList := TScreenObjectList.Create;
  FNames := TStringList.Create;
  FNewList := TScreenObjectList.Create;
  FNewNames := TStringList.Create;
  FList.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  FNames.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  FNewList.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  FNewNames.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    FList.Add(AScreenObject);
    FNames.Add(AScreenObject.Name)
  end;
end;

function TUndoRearrangeScreenObjects.Description: string;
begin
  result := 'rearrange objects';
end;

destructor TUndoRearrangeScreenObjects.Destroy;
begin
  FList.Free;
  FNames.Free;
  FNewList.Free;
  FNewNames.Free;
  inherited;
end;

procedure TUndoRearrangeScreenObjects.DoCommand;
begin
  inherited;
  SetOrder(FNewList, FNewNames);
end;

procedure TUndoRearrangeScreenObjects.Redo;
begin
  DoCommand;
  inherited;
end;

procedure TUndoRearrangeScreenObjects.SetOrder(const ScreenObjectList: TScreenObjectList;
  const NameList: TStringList);
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  frmGoPhast.PhastModel.OwnsScreenObjects := False;
  frmGoPhast.PhastModel.ClearScreenObjects;
  frmGoPhast.PhastModel.OwnsScreenObjects := True;
  for Index := 0 to ScreenObjectList.Count - 1 do
  begin
    AScreenObject := ScreenObjectList[Index];
    AScreenObject.Name := NameList[Index];
    frmGoPhast.PhastModel.AddScreenObject(AScreenObject);
    AScreenObject.Invalidate;
    AScreenObject.UpToDate := True;
  end;
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;
end;

procedure TUndoRearrangeScreenObjects.Undo;
begin
  inherited;
  SetOrder(FList, FNames);
end;

function TUndoShowHideScreenObject.AddScreenObjectToChange(const ScreenObject: TScreenObject): integer;
begin
  result := FScreenObjectsToChange.IndexOf(ScreenObject);
  if result < 0 then
  begin
    result := FScreenObjectsToChange.Add(ScreenObject);
  end;
end;

constructor TUndoShowHideScreenObject.Create;
begin
  inherited;
  FScreenObjectsToChange := TList.Create;
end;

function TUndoShowHideScreenObject.Description: string;
begin
  result := 'show or hide objects';
end;

destructor TUndoShowHideScreenObject.Destroy;
begin
  FScreenObjectsToChange.Free;
  inherited;
end;

procedure TUndoShowHideScreenObject.DoCommand;
begin
  ToggleVisibility;
  inherited;
end;

procedure TUndoShowHideScreenObject.Redo;
begin
  inherited;
  ToggleVisibility;
  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
end;

procedure TUndoShowHideScreenObject.ToggleVisibility;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  for Index := 0 to FScreenObjectsToChange.Count - 1 do
  begin
    AScreenObject := FScreenObjectsToChange[Index];
    AScreenObject.Visible := not AScreenObject.Visible;
  end;
  FShouldUpdateShowHideObjectsDisplay := True;
//  UpdateDisplay;
end;

procedure TUndoShowHideScreenObject.Undo;
begin
  ToggleVisibility;
  inherited;
end;


{ TUndoSetScreenObjectProperties }

constructor TUndoSetScreenObjectProperties.Create(
  const AListOfScreenObjects: TList; var NewScreenObjects,
  OldScreenObjects: TScreenObjectEditCollection);
var
  AScreenObject: TScreenObject;
  ScreenObjectIndex: integer;
  Item: TScreenObjectEditItem;
begin
  inherited Create;
  FExistingScreenObjects := TScreenObjectEditCollection.Create;
  FExistingScreenObjects.OwnScreenObject := False;
  FNewScreenObjects := NewScreenObjects;
  NewScreenObjects := nil;
  FOldScreenObjects := OldScreenObjects;
  OldScreenObjects := nil;

  FShouldUpdateShowHideObjects := True;
  FListOfScreenObjects := TList.Create;
  FListOfScreenObjects.Assign(AListOfScreenObjects);
  for ScreenObjectIndex := 0 to FListOfScreenObjects.Count - 1 do
  begin
    Item := FExistingScreenObjects.Add;
    AScreenObject := FListOfScreenObjects[ScreenObjectIndex];
    Item.ScreenObject := AScreenObject;
  end;

  FUndoEditFluxObservations:= TUndoEditFluxObservations.Create;
end;

function TUndoSetScreenObjectProperties.Description: string;
begin
  result := 'edit object properties';
end;

destructor TUndoSetScreenObjectProperties.Destroy;
begin
  FUndoEditFluxObservations.Free;
  FListOfScreenObjects.Free;
  FExistingScreenObjects.Free;
  FNewScreenObjects.Free;
  FOldScreenObjects.Free;
  inherited;
end;

procedure TUndoSetScreenObjectProperties.DoCommand;
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
begin
  FShouldUpdateShowHideObjects := False;
  ResetScreenObjectDataSetSubscriptions;
  for ScreenObjectIndex := 0 to FListOfScreenObjects.Count - 1 do
  begin
    AScreenObject := FListOfScreenObjects[ScreenObjectIndex];
    AScreenObject.Invalidate;


    if FSetCellsColor then
    begin
      case AScreenObject.ViewDirection of
        vdTop:
          begin
            frmGoPhast.TopScreenObjectsChanged := True;
          end;
        vdFront:
          begin
            frmGoPhast.FrontScreenObjectsChanged := True;
          end;
        vdSide:
          begin
            frmGoPhast.SideScreenObjectsChanged := True;
          end;
      else
        Assert(False);
      end;
    end;
    if AScreenObject.Count = 0 then
    begin
      AScreenObject.Deleted := True;
    end;
    AScreenObject.UpToDate := True;
  end;
  UpdateDisplay;
  FShouldUpdateShowHideObjects := True;

  FExistingScreenObjects.Assign(FNewScreenObjects);
  FUndoEditFluxObservations.DoCommand;

  UpdateShowHideObjects;
  WarnSfrLengthProblems(FListOfScreenObjects);
end;

procedure TUndoSetScreenObjectProperties.ResetScreenObjectDataSetSubscriptions;
var
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    AScreenObject.ResetSubscriptions;
  end;

  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    AScreenObject.ResetMixtureSubscriptions;
    AScreenObject.ResetBoundaryMixtureSubscriptions;
  end;
end;

procedure TUndoSetScreenObjectProperties.Undo;
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
begin
  FShouldUpdateShowHideObjects := False;
  ResetScreenObjectDataSetSubscriptions;
  for ScreenObjectIndex := 0 to FListOfScreenObjects.Count - 1 do
  begin
    AScreenObject := FListOfScreenObjects[ScreenObjectIndex];
    AScreenObject.ClearDataSets;
    AScreenObject.Invalidate;

    AScreenObject.Deleted := False;
    AScreenObject.UpToDate := True;
  end;
  UpdateDisplay;
  FShouldUpdateShowHideObjects := True;
  FExistingScreenObjects.Assign(FOldScreenObjects);
  FUndoEditFluxObservations.Undo;
  UpdateShowHideObjects;
  WarnSfrLengthProblems(FListOfScreenObjects);
end;

procedure TUndoSetScreenObjectProperties.UpdateObservations;
var
  Model: TPhastModel;
begin
  Model := frmGoPhast.PhastModel;
  FUndoEditFluxObservations.AssignNewObservations(Model.HeadFluxObservations,
    Model.DrainObservations, Model.GhbObservations, Model.RiverObservations);
end;

{ TCustomUpdateScreenObjectUndo }

procedure TCustomUpdateScreenObjectUndo.UpdateScreenObject(const ScreenObject:
  TScreenObject);
begin
  if (ScreenObject.ElevationCount <> ecZero) then
  begin
    frmGoPhast.TopScreenObjectsChanged := True;
    frmGoPhast.FrontScreenObjectsChanged := True;
    frmGoPhast.SideScreenObjectsChanged := True;
  end
  else
  begin
    case ScreenObject.ViewDirection of
      vdTop:
        begin
          frmGoPhast.TopScreenObjectsChanged := True;
        end;
      vdFront:
        begin
          frmGoPhast.FrontScreenObjectsChanged := True;
        end;
      vdSide:
        begin
          frmGoPhast.SideScreenObjectsChanged := True;
        end;
    else
      Assert(False);
    end;
  end;
  if ScreenObject.ModflowHfbBoundary <> nil then
  begin
    ScreenObject.ModflowHfbBoundary.InvalidateDisplay
  end;
  ScreenObject.UpToDate := True;
end;

procedure TCustomUpdateScreenObjectUndo.UpdateSelectionRectangle;
begin
  if frmGoPhast.TopScreenObjectsChanged then
  begin
    frmGoPhast.frameTopView.UpdateSelectRectangle;
  end;
  if frmGoPhast.FrontScreenObjectsChanged then
  begin
    frmGoPhast.frameFrontView.UpdateSelectRectangle;
  end;
  if frmGoPhast.SideScreenObjectsChanged then
  begin
    frmGoPhast.frameSideView.UpdateSelectRectangle;
  end;
end;

{ TUndoDeleteSegment }

constructor TUndoDeleteSegment.Create(const AScreenObject: TScreenObject;
  const AnEdge: integer);
begin
  inherited Create;
  FSetPropertiesOfEnclosed := AScreenObject.SetValuesOfEnclosedCells;
  FSections:= TValueArrayStorage.Create;
  FSections.Assign(AScreenObject.SectionStarts);
  FScreenObject := AScreenObject;
  FNewScreenObject := nil;
  FEdge := AnEdge;
  FScreenObject.MovePoints(FPoints);
//  SetLength(FPoints, FScreenObject.Count);
  FScreenObject.MoveSelectedPoints(FSelectedVertices);
end;

function TUndoDeleteSegment.Description: string;
begin
  result := 'delete segment';
end;

destructor TUndoDeleteSegment.Destroy;
begin
  FSections.Free;
  inherited;
end;

procedure TUndoDeleteSegment.DoCommand;
var
  DataSetIndex: integer;
begin
  inherited;
  FScreenObject.DeleteEdge(FEdge);

  for DataSetIndex := 0 to FScreenObject.DataSetCount - 1 do
  begin
    FScreenObject.DataSets[DataSetIndex].Invalidate;
    FScreenObject.UpToDate := True;
  end;

  frmGoPhast.InvalidateModel;

  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;

  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
end;

procedure TUndoDeleteSegment.Redo;
begin
  DoCommand;
  inherited;
  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
end;

procedure TUndoDeleteSegment.Undo;
var
  DataSetIndex: integer;
begin
  FScreenObject.Count := Length(FPoints);
  FScreenObject.MoveToPoints(FPoints);
  FScreenObject.MoveToSelectedPoints(FSelectedVertices);
  FScreenObject.NeedToResetSelectedVertexCount := True;
  FScreenObject.SectionStarts.Assign(FSections);
  FScreenObject.SetValuesOfEnclosedCells := FSetPropertiesOfEnclosed;

  for DataSetIndex := 0 to FScreenObject.DataSetCount - 1 do
  begin
    FScreenObject.DataSets[DataSetIndex].Invalidate;
    FScreenObject.UpToDate := True;
  end;
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;
  inherited;
  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
end;

{ TUndoDeleteVertices }

constructor TUndoDeleteVertices.Create(const ListOfScreenObjects: TScreenObjectList);
var
  Index: integer;
  AScreenObject: TScreenObject;
  PointPositionValues: TPointPositionValues;
  OldPointPositionValues: TPointPositionValues;
begin
  inherited Create;
  FScreenObjects := TList.Create;
  FVertexValues := TObjectList.Create;
  FScreenObjects.Capacity := ListOfScreenObjects.Count;
  for Index := 0 to ListOfScreenObjects.Count - 1 do
  begin
    AScreenObject := ListOfScreenObjects[Index];
    if AScreenObject.SelectedVertexCount > 0 then
    begin
      FScreenObjects.Add(AScreenObject);
      PointPositionValues := AScreenObject.PointPositionValues;
      if PointPositionValues = nil then
      begin
        FVertexValues.Add(nil)
      end
      else
      begin
        OldPointPositionValues:= TPointPositionValues.Create(nil);
        OldPointPositionValues.Assign(PointPositionValues);
        FVertexValues.Add(OldPointPositionValues);
      end;
    end;
  end;
  SetLength(FPoints, FScreenObjects.Count);
  SetLength(FSectionStarts, FScreenObjects.Count);
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    AScreenObject := FScreenObjects[Index];
    AScreenObject.MovePoints(FPoints[Index]);
    FSectionStarts[Index] := TValueArrayStorage.Create;
    FSectionStarts[Index].Assign(AScreenObject.SectionStarts);
  end;
end;

function TUndoDeleteVertices.Description: string;
begin
  result := 'delete vertices';
end;

destructor TUndoDeleteVertices.Destroy;
var
  Index: Integer;
begin
  FVertexValues.Free;
  FScreenObjects.Free;
  for Index := 0 to Length(FSectionStarts) - 1 do
  begin
    FSectionStarts[Index].Free;
  end;
  inherited;
end;

procedure TUndoDeleteVertices.DoCommand;
var
  Index, VertexIndex: integer;
  AScreenObject: TScreenObject;
  DataSetIndex: integer;
  TempScreenObject: TScreenObject;
  PointCount: integer;
  CloseScreenObject: boolean;
  SectionIndex: Integer;
  NextPart: Boolean;
  NextEnd: Integer;
  ClosedSection: boolean;
  LastPoint: TPoint2D;
  TempSectionIndex: integer;
  TempVertextIndex: integer;
  NewSection: boolean;
  NeedToCloseSection: boolean;
  CurrentStart: integer;
  CurrentEnd: integer;
  TempIndex: integer;
begin
  FCanDeleteVertices := True;
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    AScreenObject := FScreenObjects[Index];
    if AScreenObject.SelectedVertexCount = AScreenObject.Count then
    begin
      AScreenObject.Deleted := True;
    end
    else
    begin
      if AScreenObject.SelectedVertexCount > 0 then
      begin
        CloseScreenObject := AScreenObject.Closed
          and AScreenObject.SelectedVertices[0];

        TempScreenObject := frmGoPhast.PhastModel.ScreenObjectClass.Create(nil);
        try
          PointCount := 0;
          SectionIndex := AScreenObject.SectionCount -1;
          NextEnd := AScreenObject.SectionEnd[SectionIndex];
          NewSection := False;
          ClosedSection := AScreenObject.SectionClosed[SectionIndex];
          for VertexIndex := AScreenObject.Count - 1 downto 0 do
          begin
            NextPart := VertexIndex = NextEnd;
            if NextPart then
            begin
              ClosedSection := AScreenObject.SectionClosed[SectionIndex];
              NewSection := True;
              Dec(SectionIndex);
              if SectionIndex >= 0  then
              begin
                NextEnd := AScreenObject.SectionEnd[SectionIndex];
              end;
            end;
            if AScreenObject.SelectedVertices[VertexIndex] then
            begin
              TempIndex := TempScreenObject.SectionCount-1;
              if ClosedSection and not
                TempScreenObject.SectionClosed[TempIndex]
                and (TempScreenObject.SectionLength[TempIndex] > 2) then
              begin
                TempIndex := TempScreenObject.SectionStart[TempIndex];
                LastPoint := TempScreenObject.Points[TempIndex];
                TempScreenObject.AddPoint(LastPoint, False);
                Inc(PointCount);
                if PointCount <> TempScreenObject.Count then
                begin
                  FCanDeleteVertices := false;
                end;
              end;
            end
            else
            begin
              TempScreenObject.AddPoint(AScreenObject.Points[VertexIndex],
                NewSection);
              NewSection := False;
              Inc(PointCount);
              if PointCount <> TempScreenObject.Count then
              begin
                FCanDeleteVertices := false;
                break;
              end;
            end;
          end;

          if FCanDeleteVertices and CloseScreenObject then
          begin
            TempSectionIndex := TempScreenObject.SectionCount -1;
            TempVertextIndex := TempScreenObject.SectionStart[TempSectionIndex];
            TempScreenObject.AddPoint(TempScreenObject.Points[TempVertextIndex], False);
            Inc(PointCount);
            if PointCount <> TempScreenObject.Count then
            begin
              FCanDeleteVertices := false;
            end;
          end;
        finally
          TempScreenObject.Free;
        end;

        if FCanDeleteVertices then
        begin
//          PointPositionValues := AScreenObject.PointPositionValues;
//          if PointPositionValues <> nil then
//          begin
//            PointPositionValues.Sort;
//            Assert(PointPositionValues.Count > 0);
//            PointPositionIndex := 0;
//            Item := PointPositionValues.Items[PointPositionIndex]
//              as TPointValuesItem;
//            AmountToReduce := 0;
//            for VertexIndex := 0 to AScreenObject.Count - 1 do
//            begin
//              if AScreenObject.SelectedVertices[VertexIndex] then
//              begin
//                Inc(AmountToReduce);
//              end;
//              if Item.Position = VertexIndex then
//              begin
//                Item.Position := Item.Position - AmountToReduce;
//                Inc(PointPositionIndex);
//                if PointPositionIndex < PointPositionValues.Count then
//                begin
//                  Item := PointPositionValues.Items[PointPositionIndex]
//                    as TPointValuesItem;
//                end
//                else
//                begin
//                  break;
//                end;
//              end;
//            end;
//          end;

          SectionIndex := AScreenObject.SectionCount -1;
          NextEnd := AScreenObject.SectionEnd[SectionIndex];
          CurrentStart := AScreenObject.SectionStart[SectionIndex];
          CurrentEnd := NextEnd;
          NeedToCloseSection := False;
          ClosedSection := AScreenObject.SectionClosed[SectionIndex];
          for VertexIndex := AScreenObject.Count - 1 downto 0 do
          begin
            NextPart := VertexIndex = NextEnd;
            if NextPart then
            begin
              CurrentStart := AScreenObject.SectionStart[SectionIndex];
              CurrentEnd := AScreenObject.SectionEnd[SectionIndex];
              ClosedSection := AScreenObject.SectionClosed[SectionIndex];
              Dec(SectionIndex);
              if SectionIndex >= 0  then
              begin
                NextEnd := AScreenObject.SectionEnd[SectionIndex];
              end;
            end;
            if AScreenObject.SelectedVertices[VertexIndex] then
            begin
              if ClosedSection and
                ((VertexIndex = CurrentStart) or (VertexIndex = CurrentEnd)) then
              begin
                NeedToCloseSection := true;
              end;
              AScreenObject.DeletePoint(VertexIndex);
            end;
            if NeedToCloseSection and (VertexIndex = CurrentStart)
              and not AScreenObject.SectionClosed[SectionIndex+1]
              and (AScreenObject.SectionLength[SectionIndex+1] > 2) then
            begin
              LastPoint := AScreenObject.Points[VertexIndex];
              CurrentEnd := AScreenObject.SectionEnd[SectionIndex+1];
              AScreenObject.InsertPoint(CurrentEnd+1, LastPoint);
            end;
          end;
          if not AScreenObject.UpToDate then
          begin
            AScreenObject.UpToDate := True;
          end;
          AScreenObject.Invalidate;
          AScreenObject.UpToDate := True;
        end;
      end;
    end;
    for DataSetIndex := 0 to AScreenObject.DataSetCount - 1 do
    begin
      AScreenObject.DataSets[DataSetIndex].Invalidate;
      AScreenObject.UpToDate := True;
    end;
  end;
  frmGoPhast.TopScreenObjectsChanged := True;
  frmGoPhast.FrontScreenObjectsChanged := True;
  frmGoPhast.SideScreenObjectsChanged := True;
  FShouldUpdateShowHideObjects := True;
  UpdateDisplay;
end;

procedure TUndoDeleteVertices.Redo;
begin
  if FCanDeleteVertices then
  begin
    DoCommand;
    inherited;
  end;
end;

procedure TUndoDeleteVertices.Undo;
var
  Index: integer;
  AScreenObject: TScreenObject;
  DataSetIndex: integer;
  OldPointPositionValues: TPointPositionValues;
begin
  if FCanDeleteVertices then
  begin
    for Index := 0 to FScreenObjects.Count - 1 do
    begin
      AScreenObject := FScreenObjects[Index];
      if AScreenObject.Deleted then
      begin
        AScreenObject.Deleted := False;
      end
      else
      begin
        AScreenObject.Count := Length(FPoints[Index]);
        AScreenObject.MoveToPoints(FPoints[Index]);
        AScreenObject.SectionStarts := FSectionStarts[Index];
        OldPointPositionValues := FVertexValues[Index];
        AScreenObject.PointPositionValues := OldPointPositionValues;
        AScreenObject.Invalidate;
        AScreenObject.UpToDate := True;
      end;
      for DataSetIndex := 0 to AScreenObject.DataSetCount - 1 do
      begin
        AScreenObject.DataSets[DataSetIndex].Invalidate;
        AScreenObject.UpToDate := True;
      end;
    end;
    frmGoPhast.TopScreenObjectsChanged := True;
    frmGoPhast.FrontScreenObjectsChanged := True;
    frmGoPhast.SideScreenObjectsChanged := True;
    inherited;
  end;
end;

{ TScreenObjectEditItem }

procedure TScreenObjectEditItem.Assign(Source: TPersistent);
var
  SourceItem: TScreenObjectEditItem;
begin
  if Source is TScreenObjectEditItem then
  begin
    SourceItem := TScreenObjectEditItem(Source);
    Assert(FScreenObject <> nil);
    Assert(SourceItem.FScreenObject <> nil);
    ScreenObject.Assign(SourceItem.ScreenObject);
  end
  else
  begin
    inherited;
  end;
end;

constructor TScreenObjectEditItem.Create(Collection: TCollection);
begin
  inherited;
  OwnScreenObject :=
    (Collection as TScreenObjectEditCollection).OwnScreenObject;
end;

destructor TScreenObjectEditItem.Destroy;
begin
  if OwnScreenObject then
  begin
    FreeAndNil(FScreenObject);
  end;
  inherited;
end;

{ TScreenObjectEditCollection }

function TScreenObjectEditCollection.Add: TScreenObjectEditItem;
begin
  result := inherited Add as TScreenObjectEditItem;
end;

procedure TScreenObjectEditCollection.Assign(Source: TPersistent);
var
  SourceCollection: TScreenObjectEditCollection;
  ItemIndex: Integer;
  Item, SourceItem: TScreenObjectEditItem;
begin
  SourceCollection := Source as TScreenObjectEditCollection;
  Assert(Count = SourceCollection.Count);
  for ItemIndex := 0 to Count - 1 do
  begin
    Item := Items[ItemIndex] as TScreenObjectEditItem;
    SourceItem := SourceCollection.Items[ItemIndex] as TScreenObjectEditItem;
    Item.Assign(SourceItem);
  end;
end;

constructor TScreenObjectEditCollection.Create;
begin
  inherited Create(TScreenObjectEditItem);
end;

function TScreenObjectEditCollection.GetItems(
  Index: integer): TScreenObjectEditItem;
begin
  result := inherited Items[Index] as TScreenObjectEditItem;
end;

procedure TScreenObjectEditCollection.SetOwnScreenObject(const Value: boolean);
var
  Item: TScreenObjectEditItem;
  ItemIndex: Integer;
begin
  FOwnScreenObject := Value;
  for ItemIndex := 0 to Count - 1 do
  begin
    Item := Items[ItemIndex] as TScreenObjectEditItem;
    Item.OwnScreenObject := Value;
  end;
end;

{ TUndoAddPart }

procedure TUndoAddPart.AddPoint(Point: TPoint2D);
begin
  if Count = Capacity then
  begin
    Grow;
  end;
  FNewPoints[Count] := Point;
  Inc(FCount);
end;

constructor TUndoAddPart.Create(ScreenObject: TScreenObject);
begin
  inherited Create;
  FScreenObject := ScreenObject;
end;

function TUndoAddPart.Description: string;
begin
  result := 'add part to object';
end;

procedure TUndoAddPart.DoCommand;
begin
  inherited;
  UpdateScreenObject(FScreenObject);
  UpdateSelectionRectangle;
end;

procedure TUndoAddPart.Grow;
var
  Delta: integer;
begin
  if FCapacity < 16 then
  begin
    Delta := 4;
  end
  else
  begin
    Delta := FCapacity div 4;
  end;
  Capacity := FCapacity + Delta;
end;

procedure TUndoAddPart.Redo;
var
  Index: integer;
begin
  for Index := 0 to Count - 1 do
  begin
    FScreenObject.AddPoint(FNewPoints[Index], (Index = 0));
  end;
  inherited;
  DoCommand;
end;

procedure TUndoAddPart.SetCapacity(const Value: integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;
    SetLength(FNewPoints, FCapacity);
  end;
end;

procedure TUndoAddPart.Undo;
begin
  FScreenObject.Count := FScreenObject.Count - Count;
  UpdateScreenObject(FScreenObject);
  UpdateSelectionRectangle;
  inherited;
end;

{ TUndoMergeObjects }

constructor TUndoMergeObjects.Create;
var
  Index: Integer;
  ScreenObject: TScreenObject;
  MaxX: double;
  MinX: Real;
  MaxY: Real;
  MinY: Real;
  X, Y: double;
  StoredObjects: TPointerArray;
  APoint: TPoint2D;
  AnotherPoint: TPoint2D;
  OtherScreenObject: TScreenObject;
  OriginalCount: Integer;
  TempScreenObject: TScreenObject;
  PointIndex: Integer;
  TempPoints: TRealPointArray;
  LocalEpsilon: double;
  function NearlyTheSame(const A, B: real): boolean;
  begin
    result := A = B;
    if not result then
    begin
      result := Abs(A - B) / (Abs(A) + Abs(B)) < LocalEpsilon;
    end;
  end;
begin
  inherited;
  FScreenObjects:= TList.Create;
  FQuadTree := TRbwQuadTree.Create(nil);
  FScreenObjects.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if ScreenObject.Selected and (ScreenObject.SectionCount = 1) then
    begin
      if not ScreenObject.Closed then
      begin
        FScreenObjects.Add(ScreenObject);
      end;
    end;
  end;
  FScreenObjects.Capacity := FScreenObjects.Count;
  SetLength(FOldPoints, FScreenObjects.Count);
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    ScreenObject := FScreenObjects[Index];
    SetLength(FOldPoints[Index], ScreenObject.Count);
    ScreenObject.MovePoints(FOldPoints[Index]);
  end;
  LocalEpsilon := 0;
  if FScreenObjects.Count > 0 then
  begin
    ScreenObject := FScreenObjects[0];
    MaxX := ScreenObject.MaxX;
    MinX := ScreenObject.MinX;
    MaxY := ScreenObject.MaxY;
    MinY := ScreenObject.MinY;
    for Index := 1 to FScreenObjects.Count - 1 do
    begin
      ScreenObject := FScreenObjects[Index];
      MaxX := Max(MaxX, ScreenObject.MaxX);
      MinX := Min(MinX, ScreenObject.MinX);
      MaxY := Max(MaxY, ScreenObject.MaxY);
      MinY := Min(MinY, ScreenObject.MinY);
    end;
    FQuadTree.XMax := MaxX;
    FQuadTree.XMin := MinX;
    FQuadTree.YMax := MaxY;
    FQuadTree.YMin := MinY;
    LocalEpsilon := Max(Abs(MaxX), Abs(MinX));
    LocalEpsilon := Max(LocalEpsilon, Abs(MaxY));
    LocalEpsilon := Max(LocalEpsilon, Abs(MinY));
    LocalEpsilon := LocalEpsilon/1E14;
  end;
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    ScreenObject := FScreenObjects[Index];
    APoint := ScreenObject.Points[0];
    X := APoint.x;
    Y := APoint.y;
    if FQuadTree.Count > 0 then
    begin
      FQuadTree.FindClosestPointsData(X, Y, StoredObjects);
      if NearlyTheSame(X, APoint.x) and NearlyTheSame(Y, APoint.y)
        and (Length(StoredObjects) > 0) then
      begin
        OriginalCount := ScreenObject.Count;
        try
          OtherScreenObject := StoredObjects[0];
          AnotherPoint := OtherScreenObject.Points[0];
          if (APoint.x = AnotherPoint.x) and (APoint.y = AnotherPoint.y) then
          begin
            // The first point of ScreenObject is at the same location as
            // the first point of OtherScreenObject
            TempScreenObject := TScreenObject.Create(nil);
            try
              TempScreenObject.Capacity :=
                OtherScreenObject.Count + ScreenObject.Count -1;
              for PointIndex := OtherScreenObject.Count -1 downto 0 do
              begin
                TempScreenObject.AddPoint(OtherScreenObject.Points[PointIndex], False);
              end;
              for PointIndex := 1 to ScreenObject.Count - 1 do
              begin
                TempScreenObject.AddPoint(ScreenObject.Points[PointIndex], False);
              end;
              SetLength(TempPoints, TempScreenObject.Count);
              TempScreenObject.MovePoints(TempPoints);
              ScreenObject.Count := TempScreenObject.Count;
              ScreenObject.MoveToPoints(TempPoints);
              Assert(FQuadTree.RemovePoint(X, Y, OtherScreenObject));
              AnotherPoint := OtherScreenObject.Points[OtherScreenObject.Count-1];
              Assert(FQuadTree.RemovePoint(AnotherPoint.X, AnotherPoint.Y, OtherScreenObject));
              if not ScreenObject.Closed then
              begin
                FQuadTree.AddPoint(AnotherPoint.X, AnotherPoint.Y, ScreenObject);
              end;
              OtherScreenObject.Deleted := True;
            finally
              TempScreenObject.Free;
            end;
          end
          else
          begin
            AnotherPoint := OtherScreenObject.Points[OtherScreenObject.Count-1];
            Assert((APoint.x = AnotherPoint.x) and (APoint.y = AnotherPoint.y));
            TempScreenObject := TScreenObject.Create(nil);
            try
              // The first point of ScreenObject is at the same location as
              // the last point of OtherScreenObject
              TempScreenObject.Capacity :=
                OtherScreenObject.Count + ScreenObject.Count -1;
              for PointIndex := 0 to OtherScreenObject.Count -1 do
              begin
                TempScreenObject.AddPoint(OtherScreenObject.Points[PointIndex], False);
              end;
              for PointIndex := 1 to ScreenObject.Count - 1 do
              begin
                TempScreenObject.AddPoint(ScreenObject.Points[PointIndex], False);
              end;
              SetLength(TempPoints, TempScreenObject.Count);
              TempScreenObject.MovePoints(TempPoints);
              ScreenObject.Count := TempScreenObject.Count;
              ScreenObject.MoveToPoints(TempPoints);
              Assert(FQuadTree.RemovePoint(X, Y, OtherScreenObject));
              AnotherPoint := OtherScreenObject.Points[0];
              Assert(FQuadTree.RemovePoint(AnotherPoint.X, AnotherPoint.Y, OtherScreenObject));
              if not ScreenObject.Closed then
              begin
                FQuadTree.AddPoint(AnotherPoint.X, AnotherPoint.Y, ScreenObject);
              end;
              OtherScreenObject.Deleted := True;
            finally
              TempScreenObject.Free;
            end;
          end;
        except on E: EScreenObjectError do
          ScreenObject.Count := OriginalCount;
        end;
      end
      else
      begin
        FQuadTree.AddPoint(APoint.x, APoint.y, ScreenObject);
      end;
    end
    else
    begin
      FQuadTree.AddPoint(APoint.x, APoint.y, ScreenObject);
    end;
    if not ScreenObject.Closed then
    begin
      APoint := ScreenObject.Points[ScreenObject.Count-1];
      X := APoint.x;
      Y := APoint.y;
      if FQuadTree.Count > 0 then
      begin
        FQuadTree.FindClosestPointsData(X, Y, StoredObjects);
        if NearlyTheSame(X, APoint.x) and NearlyTheSame(Y, APoint.y)
          and (Length(StoredObjects) > 0) then
        begin
          OriginalCount := ScreenObject.Count;
          try
            OtherScreenObject := StoredObjects[0];
            ScreenObject.Capacity := ScreenObject.Count + OtherScreenObject.Count -1;
            AnotherPoint := OtherScreenObject.Points[0];
            if (APoint.x = AnotherPoint.x) and (APoint.y = AnotherPoint.y) then
            begin
              // The last point of ScreenObject is at the same location as
              // the first point of OtherScreenObject
              for PointIndex := 1 to OtherScreenObject.Count - 1 do
              begin
                ScreenObject.AddPoint(OtherScreenObject.Points[PointIndex], False);
              end;
              Assert(FQuadTree.RemovePoint(X, Y, OtherScreenObject));
              AnotherPoint := OtherScreenObject.Points[OtherScreenObject.Count-1];
              Assert(FQuadTree.RemovePoint(AnotherPoint.X, AnotherPoint.Y, OtherScreenObject));
              if not ScreenObject.Closed then
              begin
                FQuadTree.AddPoint(AnotherPoint.X, AnotherPoint.Y, ScreenObject);
              end;
            end
            else
            begin
              // The last point of ScreenObject is at the same location as
              // the last point of OtherScreenObject
              AnotherPoint := OtherScreenObject.Points[OtherScreenObject.Count-1];
              Assert((APoint.x = AnotherPoint.x) and (APoint.y = AnotherPoint.y));
              for PointIndex := OtherScreenObject.Count - 2 downto 0 do
              begin
                ScreenObject.AddPoint(OtherScreenObject.Points[PointIndex], False);
              end;
              Assert(FQuadTree.RemovePoint(X, Y, OtherScreenObject));
              AnotherPoint := OtherScreenObject.Points[0];
              Assert(FQuadTree.RemovePoint(AnotherPoint.X, AnotherPoint.Y, OtherScreenObject));
              if not ScreenObject.Closed then
              begin
                FQuadTree.AddPoint(AnotherPoint.X, AnotherPoint.Y, ScreenObject);
              end;
            end;
            OtherScreenObject.Deleted := True;
          except on E: EScreenObjectError do
            ScreenObject.Count := OriginalCount;
          end;
        end
        else
        begin
          FQuadTree.AddPoint(APoint.x, APoint.y, ScreenObject);
        end;
      end
      else
      begin
        FQuadTree.AddPoint(APoint.x, APoint.y, ScreenObject);
      end;
    end;
  end;
  SetLength(FDeleted, FScreenObjects.Count);
  SetLength(FNewPoints, FScreenObjects.Count);
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    ScreenObject := FScreenObjects[Index];
    FDeleted[Index] := ScreenObject.Deleted;
    if ScreenObject.Deleted then
    begin
      SetLength(FNewPoints[Index], 0);
    end
    else
    begin
      SetLength(FNewPoints[Index], ScreenObject.Count);
      ScreenObject.MovePoints(FNewPoints[Index]);
    end;
  end;
end;

function TUndoMergeObjects.Description: string;
begin
  result := 'merge objects';
end;

destructor TUndoMergeObjects.Destroy;
begin
  FQuadTree.Free;
  FScreenObjects.Free;
  inherited;
end;

procedure TUndoMergeObjects.DoCommand;
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  inherited;
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    ScreenObject := FScreenObjects[Index];
    ScreenObject.Deleted := FDeleted[Index];
    if not ScreenObject.Deleted then
    begin
      ScreenObject.Count := Length(FNewPoints[Index]);
      ScreenObject.MoveToPoints(FNewPoints[Index]);
      ScreenObject.Invalidate;
    end;
  end;
end;

procedure TUndoMergeObjects.Redo;
begin
  DoCommand;

end;

function TUndoMergeObjects.ShouldUse: boolean;
begin
  result := FScreenObjects.Count > 1;
end;

procedure TUndoMergeObjects.Undo;
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    ScreenObject := FScreenObjects[Index];
    ScreenObject.Count := Length(FOldPoints[Index]);
    ScreenObject.MoveToPoints(FOldPoints[Index]);
    ScreenObject.Deleted := False;
    ScreenObject.Invalidate;
  end;
  inherited;
end;

{ TUndoReverseVerticies }

constructor TUndoReverseVerticies.Create;
var
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  inherited;
  FScreenObjects := TList.Create;
  FScreenObjects.Capacity := frmGoPhast.PhastModel.ScreenObjectCount;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index] as TScreenObject;
    if AScreenObject.Selected and not AScreenObject.Deleted then
    begin
      FScreenObjects.Add(AScreenObject)
    end;
  end;
  FScreenObjects.Capacity := FScreenObjects.Count;
  SetPostSelection;
end;

function TUndoReverseVerticies.Description: string;
begin
  result := 'reverse object order';
end;

destructor TUndoReverseVerticies.Destroy;
begin
  FScreenObjects.Free;
  inherited;
end;

procedure TUndoReverseVerticies.DoCommand;
begin
  inherited;
  ReverseObjects;
end;

procedure TUndoReverseVerticies.ReverseObjects;
var
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  for Index := 0 to FScreenObjects.Count - 1 do
  begin
    AScreenObject := FScreenObjects[Index];
    AScreenObject.ReverseDirection;
  end;
end;

procedure TUndoReverseVerticies.Undo;
begin
  inherited;
  ReverseObjects;
end;

destructor TCustomImportMultipleScreenObjects.Destroy;
begin
  FNewScreenObjects.Free;
  inherited;
end;

procedure TCustomImportMultipleScreenObjects.StoreNewScreenObjects(const ListOfScreenObjects: TList);
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  FNewScreenObjects := TList.Create;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    AScreenObject.Selected := false;
  end;
  FNewScreenObjects.Assign(ListOfScreenObjects);
  frmGoPhast.PhastModel.IncreaseScreenObjectCapacity(ListOfScreenObjects.Count);
  for Index := 0 to FNewScreenObjects.Count - 1 do
  begin
    AScreenObject := FNewScreenObjects[Index];
    frmGoPhast.PhastModel.AddScreenObject(AScreenObject);
    AScreenObject.ImportedValues.CacheData;
  end;
  SetPostSelection;
end;

procedure TCustomImportMultipleScreenObjects.UpdateScreenObject(const AScreenObject: TScreenObject);
begin
  if (AScreenObject.ElevationCount <> ecZero) then
  begin
    frmGoPhast.TopScreenObjectsChanged := True;
    frmGoPhast.FrontScreenObjectsChanged := True;
    frmGoPhast.SideScreenObjectsChanged := True;
  end
  else
  begin
    case AScreenObject.ViewDirection of
      vdTop:
        begin
          frmGoPhast.TopScreenObjectsChanged := True;
        end;
      vdFront:
        begin
          frmGoPhast.FrontScreenObjectsChanged := True;
        end;
      vdSide:
        begin
          frmGoPhast.SideScreenObjectsChanged := True;
        end;
    else
      Assert(False);
    end;
  end;
  AScreenObject.UpToDate := True;
end;

procedure TCustomImportMultipleScreenObjects.UnDeleteNewScreenObjects;
var
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  for Index := 0 to FNewScreenObjects.Count - 1 do
  begin
    AScreenObject := FNewScreenObjects[Index];
    // Always invalidate AScreenObject so that any data sets
    // that are set by new objects get invalidated.
    AScreenObject.Deleted := False;
    AScreenObject.Invalidate;
    UpdateScreenObject(AScreenObject);
  end;
end;

procedure TCustomImportMultipleScreenObjects.DeleteNewScreenObjects;
var
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  for Index := 0 to FNewScreenObjects.Count - 1 do
  begin
    AScreenObject := FNewScreenObjects[Index];
    if not AScreenObject.Deleted then
    begin
      AScreenObject.Deleted := True;
      AScreenObject.Invalidate;
      UpdateScreenObject(AScreenObject);
    end;
  end;
end;

{ TUndoCutScreenObjects }

function TUndoCutScreenObjects.Description: string;
begin
  if FScreenObjects.Count > 1 then
  begin
    result := 'cut objects';
  end
  else
  begin
    result := 'cut object';
  end;
end;

procedure TUndoCutScreenObjects.DoCommand;
begin
  frmGoPhast.PhastModel.CopyScreenObjectsToClipboard;
  inherited;
end;

procedure TUndoCutScreenObjects.Redo;
begin
  frmGoPhast.PhastModel.CopyScreenObjectsToClipboard;
  inherited;
end;

{ TUndoPasteScreenObjects }


function TUndoPasteScreenObjects.Description: string;
begin
  result := 'paste object(s)';
end;

procedure TUndoPasteScreenObjects.DoCommand;
begin
  UnDeleteNewScreenObjects;
  FShouldUpdateShowHideObjects := True;
  inherited;
end;

procedure TUndoPasteScreenObjects.Redo;
begin
  UnDeleteNewScreenObjects;
  FShouldUpdateShowHideObjects := True;
  inherited;
end;

procedure TUndoPasteScreenObjects.StoreNewScreenObjects(
  const ListOfScreenObjects: TList);
begin
  FNewScreenObjects := TList.Create;
  FNewScreenObjects.Assign(ListOfScreenObjects);
  frmGoPhast.PhastModel.IncreaseScreenObjectCapacity(ListOfScreenObjects.Count);
  SetPostSelection;
end;

procedure TUndoPasteScreenObjects.Undo;
begin
  DeleteNewScreenObjects;
  FShouldUpdateShowHideObjects := True;
  inherited;
end;

end.
