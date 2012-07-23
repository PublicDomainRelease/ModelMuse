{@abstract(@name defines a series of @link(TUndoItem)s used with GoPhast.
  Others are defined in @link(ScreenObjectUnit), 
  @link(frmChemistryOptionsUnit), @link(frmImportShapefileUnit),
  @link(frmPhastGridOptionsUnit), @link(frmPrintFrequencyUnit),
  @link(frmPrintInitialUnit), @link(frmImportDXFUnit),
  @link(frmSolutionMethodUnit), @link(frmSteadyFlowUnit),
  @link(frmTimeControlUnit), @link(frmUnitsUnit), and
  @link(UndoItemsScreenObjects).)

  @author(Richard B. Winston <rbwinst@usgs.gov>)}
unit UndoItems;

interface

uses Classes, Contnrs, Controls, Forms, RbwParser, Undo, GoPhastTypes, AbstractGridUnit,
  DataSetUnit, PhastDataSets, FluxObservationUnit, FormulaManagerUnit,
  DisplaySettingsUnit, Mt3dmsFluxObservationsUnit, FastGEO;

type
  {@abstract(@name is an abstract base class used as an ancestor
   of all @link(TUndoItem)s in ModelMuse.)}
  TCustomUndo = class(TUndoItem)
  protected
    // @name provides a single description of the item which is then
    // used in other @link(GetUndoDescription), @link(GetUndoDescription),
    // @link(GetShortUndoDescription), @link(GetRedoDescription),
    // @link(GetShortRedoDescription), @link(GetUndoMenuText), and
    // @link(GetRedoMenuText).
    function Description: string; virtual; abstract;
    // @name returns 'Undo ' + @link(Description).
    function GetUndoDescription: string; override;
    // @name returns 'Undo ' + @link(Description).
    function GetShortUndoDescription: string; override;
    // @name returns 'Redo ' + @link(Description).
    function GetRedoDescription: string; override;
    // @name returns 'Redo ' + @link(Description).
    function GetShortRedoDescription: string; override;
    // @name returns @link(TUndoItem.UndoDescription)
    function GetUndoMenuText: string; override;
    // @name returns @link(TUndoItem.RedoDescription)
    function GetRedoMenuText: string; override;
    // @name tells the model that the grid has changed and that related
    // things need to be updated.
    procedure InvalidateGrid;
    procedure DisallowChildGridUpdates;
    procedure AllowChildGridUpdates;
  public
    // @name calls @link(TUndoItem.DoCommand).
    procedure Redo; override;
  end;

  {@abstract(@name is the abstract ancestor of @link(TUndoItem)s
    that make changes to the grid.)}
  TCustomUndoChangeGridDimensions = class(TCustomUndo)
  private
    FLayerCount: integer;
  protected
    // @name tells the @link(TDataArray)s what the new grid dimensions are.
    procedure UpdateDataSets;
  public
    Constructor Create;
    // @name calls @link(UpdateDataSets).
    procedure DoCommand; override;
    // @name calls @link(UpdateDataSets).
    procedure Undo; override;
  end;

  {@abstract(@name is used for deleting a row boundary from the grid.)}
  TUndoDeleteRow = class(TCustomUndoChangeGridDimensions)
  private
    // @name: integer;
    // @name is the index of the row boundary.
    FRow: integer;
    // @name: real;
    // @name is the position of the row in grid coordinates.
    FRowPosition: real;
    // @name: integer;
    // @name stores the row in the grid that was selected.
    FSelectedRow: Integer;
  protected
    // See TCustomUndo.@link(TCustomUndo.Description).
    function Description: string; override;
  public
    // @name creates an instance of @classname. ARow is the index of the
    // row boundary to be deleted.
    constructor Create(const ARow: integer);
    // @name deletes the row.
    procedure DoCommand; override;
    // @name restores the row.
    procedure Undo; override;
  end;

  {@abstract(@name is used for deleting a column boundary from the grid.)}
  TUndoDeleteColumn = class(TCustomUndoChangeGridDimensions)
  private
    // @name: integer;
    // @name is the index of the column boundary.
    FColumn: integer;
    // @name: real;
    // @name is the position of the column in grid coordinates.
    FColumnPosition: real;
    // @name: integer;
    // @name stores the column in the grid that was selected.
    FSelectedColumn: integer;
  protected
    // See TCustomUndo.@link(TCustomUndo.Description).
    function Description: string; override;
  public
    // @name creates an instance of @classname. AColumn is the index of the
    // column boundary to be deleted.
    constructor Create(const AColumn: integer);
    // @name deletes the column.
    procedure DoCommand; override;
    // @name restores the column.
    procedure Undo; override;
  end;

  {@abstract(@name is used for deleting a layer boundary from the grid.)}
  TUndoDeleteLayer = class(TCustomUndoChangeGridDimensions)
  protected
    // @name: integer;
    // @name is the index of the layer boundary.
    FLayer: integer;
    // @name: real;
    // @name is the position of the layer.
    FLayerElevation: real;
    // @name: integer;
    // @name stores the layer in the grid that was selected.
    FSelectedLayer: integer;
    // See TCustomUndo.@link(TCustomUndo.Description).
    function Description: string; override;
  public
    // @name creates an instance of @classname. ALayer is the index of the
    // layer boundary to be deleted.
    constructor Create(const ALayer: integer);
    // @name deletes the layer.
    procedure DoCommand; override;
    // @name restores the layer.
    procedure Undo; override;
  end;

  {@abstract(@name is used for moving a row boundary.)}
  TUndoMoveRow = class(TUndoDeleteRow)
  private
    // @name: integer;
    // @name is the index of the row after being moved.
    FNewRow: integer;
    // @name: real;
    // @name is the position of the row after being moved.
    FNewRowPosition: real;
  protected
    // See TCustomUndo.@link(TCustomUndo.Description).
    function Description: string; override;
  public
    // @name creates an instance of @classname.
    // @param(ARow is the index of the row boundary to be moved.)
    // @param(NewPosition is the position of that row after being moved.)
    constructor Create(const ARow: integer; const NewPosition: real);
    // @name moves the row.
    procedure DoCommand; override;
    // @name moves the row back where it was.
    procedure Undo; override;
  end;

  {@abstract(@name is used for moving a column boundary.)}
  TUndoMoveColumn = class(TUndoDeleteColumn)
  private
    // @name: integer;
    // @name is the index of the column after being moved.
    FNewColumn: integer;
    // @name: real;
    // @name is the position of the column after being moved.
    FNewColumnPosition: real;
  protected
    // See TCustomUndo.@link(TCustomUndo.Description).
    function Description: string; override;
  public
    // @name creates an instance of @classname.
    // @param(AColumn is the index of the column boundary to be moved.)
    // @param(NewPosition is the position of that column after being moved.)
    constructor Create(const AColumn: integer; const NewPosition: real);
    // @name moves the column.
    procedure DoCommand; override;
    // @name moves the column back where it was.
    procedure Undo; override;
  end;

  {@abstract(@name is used for moving a layer boundary.)}
  TUndoMoveLayer = class(TUndoDeleteLayer)
  private
    // @name: integer;
    // @name is the index of the layer after being moved.
    FNewLayer: integer;
    // @name: real;
    // @name is the position of the layer after being moved.
    FNewLayerElevation: real;
  protected
    // See TCustomUndo.@link(TCustomUndo.Description).
    function Description: string; override;
  public
    // @name creates an instance of @classname.
    // @param(ALayer is the index of the layer boundary to be moved.)
    // @param(NewPosition is the position of that layer after being moved.)
    constructor Create(const ALayer: integer; const NewPosition: real);
    // @name moves the layer.
    procedure DoCommand; override;
    // @name moves the layer back where it was.
    procedure Undo; override;
  end;

  {@abstract(@name is used to add a column boundary.)}
  TUndoAddColumn = class(TCustomUndoChangeGridDimensions)
  private
    // @name: integer;
    // @name is the index of the column being added.
    FColumn: integer;
    // @name: real;
    // @name is the position of the column being added.
    FColumnPosition: real;
    // @name: integer;
    // @name is the selected column.
    FSelectedColumn: integer;
  protected
    // See TCustomUndo.@link(TCustomUndo.Description).
    function Description: string; override;
  public
    // @name creates an instance of @classname.
    // @param(NewPosition is the position where column is being added.)
    constructor Create(const NewPosition: real);
    // @name adds the column
    procedure DoCommand; override;
    // @name removes the column
    procedure Undo; override;
  end;

  {@abstract(@name is used to add a row boundary.)}
  TUndoAddRow = class(TCustomUndoChangeGridDimensions)
  private
    // @name: integer;
    // @name is the index of the row being added.
    Row: integer;
    // @name: real;
    // @name is the position of the row being added.
    RowPosition: real;
    // @name: integer;
    // @name is the selected row.
    SelectedRow: integer;
  protected
    // See TCustomUndo.@link(TCustomUndo.Description).
    function Description: string; override;
  public
    // @name creates an instance of @classname.
    // @param(NewPosition is the position where row is being added.)
    constructor Create(const NewPosition: real);
    // @name adds the row.
    procedure DoCommand; override;
    // @name removes the row.
    procedure Undo; override;
  end;

  {@abstract(@name is used to add a layer boundary.)}
  TUndoAddLayer = class(TCustomUndoChangeGridDimensions)
  private
    // @name: integer;
    // @name is the index of the layer being added.
    FLayer: integer;
    // @name: real;
    // @name is the position of the layer being added.
    FLayerElevation: real;
    // @name: integer;
    // @name is the selected layer.
    FSelectedLayer: integer;
  protected
    // See TCustomUndo.@link(TCustomUndo.Description).
    function Description: string; override;
  public
    // @name creates an instance of @classname.
    // @param(NewPosition is the position where layer is being added.)
    constructor Create(const NewPosition: real);
    // @name adds the layer.
    procedure DoCommand; override;
    // @name removes the layer.
    procedure Undo; override;
  end;

  {@abstract(@name is used to subdivide columns, rows, or layers.)}
  TUndoSubdivide = class(TCustomUndoChangeGridDimensions)
  private
    // @name: integer;
    // See @link(ColumnCount).
    FColumnCount: integer;
    // @name: integer;
    // See @link(FirstColumn).
    FFirstColumn: integer;
    // @name: integer;
    // See @link(FirstLayer).
    FFirstLayer: integer;
    // @name: integer;
    // See @link(FirstRow).
    FFirstRow: integer;
    // @name: integer;
    // See @link(LastColumn).
    FLastColumn: integer;
    // @name: integer;
    // See @link(LastLayer).
    FLastLayer: integer;
    // @name: integer;
    // See @link(LastRow).
    FLastRow: integer;
    // @name: integer;
    // See @link(LayerCount).
    FLayerCount: integer;
    // @name: integer;
    // See @link(RowCount).
    FRowCount: integer;
  protected
    // See TCustomUndo.@link(TCustomUndo.Description).
    function Description: string; override;
    // @name rejoins several columns, rows, or layers that
    // have been subdivided into a single column, row, or layer.
    // @param(Count is the number of columns, rows, or layers to combine)
    // @param(OldPosition is the index of the
    // first of the columns, rows, or layers to be recombined.)
    procedure Recombine(const Count, OldPosition: integer;
      var AnArray: TOneDRealArray);
    // @name subdivides a column, row, or layer.
    // @param(Count is the number of columns, rows, or layers in which to
    // subdivide the selected column, row, or layer.)
    // @param(OldPosition is the index of the
    // first of the column, row, or layer to be recombined.)
    procedure Subdivide(const Count, OldPosition: integer;
      var AnArray: TOneDRealArray);
  public
    // @name is the number of columns each
    // selected column should be divided into.
    // If columns will not be subdivided, @name is zero.
    property ColumnCount: integer read FColumnCount write FColumnCount;
    // @name subdivides the selected columns, rows, and layers.
    procedure DoCommand; override;
    // @name is the first column to subdivide.
    property FirstColumn: integer read FFirstColumn write FFirstColumn;
    // @name is the first layer to subdivide.
    property FirstLayer: integer read FFirstLayer write FFirstLayer;
    // @name is the first row to subdivide.
    property FirstRow: integer read FFirstRow write FFirstRow;
    // @name is the last column to subdivide.
    property LastColumn: integer read FLastColumn write FLastColumn;
    // @name is the last layer to subdivide.
    property LastLayer: integer read FLastLayer write FLastLayer;
    // @name is the last row to subdivide.
    property LastRow: integer read FLastRow write FLastRow;
    // @name is the number of layers each
    // selected layer should be divided into.
    // If layers will not be subdivided, @name is zero.
    property LayerCount: integer read FLayerCount write FLayerCount;
    // @name is the number of rows each
    // selected row should be divided into.
    // If rows will not be subdivided, @name is zero.
    property RowCount: integer read FRowCount write FRowCount;
    // @name recombines the selected columns, rows, and layers.
    procedure Undo; override;
  end;

  // @abstract(@name is used to set the grid angle.)
  TUndoSetAngle = class(TCustomUndo)
  private
    // @name: real;
    // @name is the new grid angle in radians.
    FNewAngle: real;
    // @name: real;
    // @name is the old grid angle in radians.
    FOldAngle: real;
    // @name: @link(TOneDRealArray);
    // @name is the column positions before changing the grid angle.
    FOldColumns: TOneDRealArray;
    // @name: @link(TOneDRealArray);
    // @name is the row positions before changing the grid angle.
    FOldRows: TOneDRealArray;
  protected
    // See TCustomUndo.@link(TCustomUndo.Description).
    function Description: string; override;
  public
    // @name creates an instance of @classname.
    // @param(Angle is the new grid angle.)
    constructor Create(const Angle: real);
    // @name rotates the grid.
    procedure DoCommand; override;
    // @name restores the grid to its previous angle.
    procedure Undo; override;
  end;

  // @abstract(@name is used to change multiple column, row, and layer
  // positions at one time.)
  TUndoEditGridLines = class(TCustomUndoChangeGridDimensions)
  private
    // @name: @link(TOneDRealArray);
    // @name is the positions of the column boundaries when the @classname
    // was created.
    FOldColumns: TOneDRealArray;
    // @name: @link(TOneDRealArray);
    // @name is the positions of the layer boundaries when the @classname
    // was created.
    FOldLayerElevations: TOneDRealArray;
    // @name: @link(TOneDRealArray);
    // @name is the positions of the row boundaries when the @classname
    // was created.
    FOldRows: TOneDRealArray;
    // Set the magnifications so that the grid will fill most of the screen.
    procedure ChangeView;
  protected
    // See TCustomUndo.@link(TCustomUndo.Description).
    function Description: string; override;
  public
    // @name: @link(TOneDRealArray);
    // @name is the new positions of the column boundaries
    // that should be set in @link(DoCommand).
    FNewColumns: TOneDRealArray;
    // @name: @link(TOneDRealArray);
    // @name is the new positions of the layer boundaries
    // that should be set in @link(DoCommand).
    FNewLayerElevations: TOneDRealArray;
    // @name: @link(TOneDRealArray);
    // @name is the new positions of the row boundaries
    // that should be set in @link(DoCommand).
    FNewRows: TOneDRealArray ;
    // @name creates an instance of @classname and initializes the private
    // fields.
    constructor Create;
    // @name sets the column, row, and layer boundaries to
    // @link(FNewColumns), @link(FNewRows), and @link(FNewLayerElevations).
    // Those must all be set before @link(DoCommand) is called.
    procedure DoCommand; override;
    // @name restores the column, row, and layer boundaries to
    // @link(FOldColumns), @link(FOldRows), and @link(FOldLayerElevations),
    procedure Undo; override;
  end;

  // @abstract(@name is used to generate a new grid.)
  TUndoCreateGrid = class(TUndoEditGridLines)
  private
    // @name: double;
    // @name is the angle of the new grid.
    // See @link(NewAngle).
    FNewAngle: double;
    // @name: double;
    // @name is the angle of the old grid.
    FOldAngle: double;
  protected
    // See TCustomUndo.@link(TCustomUndo.Description).
    function Description: string; override;
  public
    // @name is the angle of the new grid.
    property NewAngle: double read FNewAngle write FNewAngle;
    // @name creates an instance of @classname and
    // initializes the old grid angle.
    constructor Create;
    // @name creates the new grid.
    // @link(TUndoEditGridLines.FNewColumns),
    // @link(TUndoEditGridLines.FNewRows), and
    // @link(TUndoEditGridLines.FNewLayerElevations).
    // and @link(NewAngle) must all be set before @link(DoCommand) is called.
    procedure DoCommand; override;
    // @name restores the old grid.
    procedure Undo; override;
  end;

  // @abstract(@name is used when adjusting grid lines so they
  // meet a criterion regarding the maximum ratio of adjacent
  // column, row, or layer widths.)
  TUndoSmoothGrid = class(TUndoEditGridLines)
  protected
    // See TCustomUndo.@link(TCustomUndo.Description).
    function Description: string; override;
  end;

  // @abstract(@name is used to change the vertical exaggeration)
  TUndoVerticalExaggeration = class(TCustomUndo)
  private
    // @name: real;
    // @name is the old vertical exaggeration.
    FOldVerticalExaggeration: real;
    // @name: real;
    // @name is the new vertical exaggeration.
    FNewVerticalExaggeration: real;
    // @name sets the VerticalExaggeration.
    procedure SetVE(VerticalExaggeration: real);
  protected
    // See TCustomUndo.@link(TCustomUndo.Description).
    function Description: string; override;
  public
    // @name creates an instance of @classname.
    constructor Create(const ANewVerticalExaggeration: real);
    // @name sets the new vertical exaggeration.
    procedure DoCommand; override;
    // @name gets the existing vertical exaggeration.
    class function GetOldVE: real;
    // @name restores the old vertical exaggeration.
    procedure Undo; override;
  end;

  // @abstract(@name is used to set TPhastModel.@link(TPhastModel.FreeSurface)
  // and TPhastModel.@link(TPhastModel.UseWaterTable).)
  TUndoFreeSurface = class(TCustomUndo)
  private
    // @name: boolean;
    // @name is the TPhastModel.@link(TPhastModel.FreeSurface) that
    // will be set.
    FNewFreeSurface: boolean;
    // @name: boolean;
    // @name is the TPhastModel.@link(TPhastModel.UseWaterTable) that
    // will be set.
    FNewInitialWaterTable: boolean;
    // @name: boolean;
    // @name is the existing TPhastModel.@link(TPhastModel.FreeSurface)
    FOldFreeSurface: boolean;
    // @name: boolean;
    // @name is the existing TPhastModel.@link(TPhastModel.UseWaterTable)
    FOldInitialWaterTable: boolean;
  protected
    // See TCustomUndo.@link(TCustomUndo.Description).
    function Description: string; override;
  public
    // @name is used to determine if this @classname will change anything.
    // if not, there is no reason to use it.
    function Changed: boolean;
    // @name creates an instance of @classname.
    constructor Create;
    // @name sets TPhastModel.@link(TPhastModel.FreeSurface)
    // and TPhastModel.@link(TPhastModel.UseWaterTable).
    // Set @link(NewFreeSurface) and @link(NewInitialWaterTable)
    // before @name is called.
    procedure DoCommand; override;
    // @name is the TPhastModel.@link(TPhastModel.FreeSurface)
    // to be set in @link(DoCommand).
    property NewFreeSurface: boolean read FNewFreeSurface
      write FNewFreeSurface;
    // @name is the TPhastModel.@link(TPhastModel.UseWaterTable)
    // to be set in @link(DoCommand).
    property NewInitialWaterTable: boolean read FNewInitialWaterTable
      write FNewInitialWaterTable;
    // @name restores TPhastModel.@link(TPhastModel.FreeSurface)
    // and TPhastModel.@link(TPhastModel.UseWaterTable).
    procedure Undo; override;
  end;

  // @name stores the properties of a @link(TDataArray) or
  // @link(TCustomPhastDataSet)
  // for use in @link(TUndoChangeDataSets).
  TPhastDataSetStorage = class(TObject)
  private
    FNeedToInvalidate: boolean;
    // @name is the @link(TDataArray) whose values are being stored.
    FDataSet: TDataArray;
    // See @link(Name).
    FName: TComponentName;
    // See @link(Visible).
    {FVisible: boolean;}
    // See @link(Formula).
    FFormula: TFormulaObject;
    // See @link(Units).
    FUnits: string;
    // See @link(TwoDInterpolator).
    FTwoDInterpolator: TCustom2DInterpolater;
    // See @link(Orientation).
    FOrientation: TDataSetOrientation;
    // See @link(EvaluatedAt).
    FEvaluatedAt: TEvaluatedAt;
    // See @link(PhastInterpolationValues).
    FPhastInterpolationValues: TPhastInterpolationValues;
    // See @link(DataType).
    FDataType: TRbwDataType;
    FComment: string;
    // See @link(PhastInterpolationValues).
    procedure SetPhastInterpolationValues(
      const Value: TPhastInterpolationValues);
    // See @link(TwoDInterpolator).
    procedure SetTwoDInterpolator(const Value: TCustom2DInterpolater);
    // @name returns true if @link(FDataSet) differs from the
    // data in @classname.
    function DataSetChanged: boolean;
    function GetFormula: string;
    procedure SetFormula(const Value: string);
    procedure SetDataSet(const Value: TDataArray);
  public
    procedure AssignBasicProperties;
    // @name is the @link(TDataArray) whose values are being stored.
    property DataSet: TDataArray read FDataSet write SetDataSet;
    // @name is the name to assign to the @link(TDataArray).
    property Name: TComponentName read FName write FName;
    // @name copies the properties of DataSet to @classname and stores
    // DataSet in FDataSet;
    procedure Assign(const DataSet: TDataArray);
    // @name copies the Formula
    // and PhastInterpolationValues properties of @classname to DataSet.
    procedure AssignFormulasToDataSet;
    // @name copies the properties of @classname (except Formula
    // and PhastInterpolationValues) to DataSet.
    procedure AssignToDataSet(out ShouldInvalidate: boolean);
    // @name creates and instance of @classname
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    { @name is the Orientation property to assign to the @link(TDataArray).}
    property Orientation: TDataSetOrientation read FOrientation
      write FOrientation;
    // @name is the EvaluatedAt property to assign to the @link(TDataArray).
    property EvaluatedAt: TEvaluatedAt read FEvaluatedAt write FEvaluatedAt;
    // @name is the DataType property to assign to the @link(TDataArray).
    property DataType: TRbwDataType read FDataType write FDataType;
    // @name is the Units property to assign to the @link(TDataArray).
    property Units: string read FUnits write FUnits;
    // @name is the TwoDInterpolator property to assign to the @link(TDataArray).
    property TwoDInterpolator: TCustom2DInterpolater read FTwoDInterpolator
      write SetTwoDInterpolator;
    // @name is the Formula property to assign to the @link(TDataArray).
    property Formula: string read GetFormula write SetFormula;
    // @name is the PhastInterpolationValues property to assign to the @link(TDataArray).
    property PhastInterpolationValues: TPhastInterpolationValues
      read FPhastInterpolationValues write SetPhastInterpolationValues;
    property Comment: string read FComment write FComment;
  end;

  // @name is used to add, delete, and change the properties of
  // @link(TDataArray TDataArrays).
  TUndoChangeDataSets = class(TCustomUndo)
  private
    FTopDataSet: TDataArray;
    FFrontDataSet: TDataArray;
    FSideDataSet: TDataArray;
    F3DDataSet: TDataArray;
    // @name contains the @link(TDataArray TDataArrays) to be deleted.
    FDeletedDataSets: TList;
    // @name contains the @link(TDataArray TDataArrays) to be addead.
    FNewDataSets: TList;
    // @name is implemented as a TObjectList.
    // It is used to store a series of instances of @link(TPhastDataSetStorage)
    // for old properties of @link(TDataArray TDataArrays)
    // or @link(TCustomPhastDataSet TCustomPhastDataSets).
    FOldDataSetProperties: TList;
    // @name is implemented as a TObjectList.
    // It is used to store a series of instances of @link(TPhastDataSetStorage)
    // for new properties of @link(TDataArray TDataArrays)
    // or @link(TCustomPhastDataSet TCustomPhastDataSets).
    FNewDataSetProperties: TList;
    FOldNames: TStringList;
    FNewNames: TStringList;
    // @name stores the current state of @link(TDataArrayManager.DataSets
    // frmGoPhast.Model.FDataArrayManager.DataSets)
    // and the @link(TDataArray)s that are being
    // used to color the grid.
    procedure StoreData;
    // @name clears expressions and variables in @link(frmGoPhast).
    procedure ClearExpressionsAndVariables;
    // @name sets the properties of the @link(TDataArray TDataArrays).
    // @param(AddedDataSets the new data sets that have been created.)
    // @param(DeletedDataSets the existing data sets that have been deleted.)
    // @param(DataSetProperties stores a series of @link(TPhastDataSetStorage)
    // that store the properties of the @link(TDataArray TDataArrays).)
    procedure SetProperties(AddedDataSets, DeletedDataSets,
      DataSetProperties: TList);
    procedure UpdateFormulas(DataSetProperties: TList);
  protected
    // @name describes what @classname does.
    function Description: string; override;
  public
    // @name creates and instance of @classname
    // @param(DeletedDataSets DeletedDataSets contains a series of
    // @link(TDataArray)s that have been deleted.)
    // @param(NewDataSets NewDataSets contains a series of
    // @link(TDataArray)s that have been added.)
    // @param(NewDataSetProperties NewDataSetProperties contains a series of
    // @link(TPhastDataSetStorage)s that define the changed properties of
    // existing @link(TDataArray)s.)
    Constructor Create(var DeletedDataSets, NewDataSets: TList;
      var NewDataSetProperties: TObjectList);
    // @name returns @true if any of the @link(TDataArray TDataArrays) has
    // changed;
    function DataSetsChanged: boolean;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    Destructor Destroy; override;
    // @name does the command for the first time.
    procedure DoCommand; override;
    // @name undoes the command.
    procedure Undo; override;
  end;

  TMassFluxObs = record
    Mt3dmsHeadMassFluxObservations: TMt3dmsFluxObservationGroups;
    Mt3dmsWellMassFluxObservations: TMt3dmsFluxObservationGroups;
    Mt3dmsDrnMassFluxObservations: TMt3dmsFluxObservationGroups;
    Mt3dmsRivMassFluxObservations: TMt3dmsFluxObservationGroups;
    Mt3dmsGhbMassFluxObservations: TMt3dmsFluxObservationGroups;
    Mt3dmsRchMassFluxObservations: TMt3dmsFluxObservationGroups;
    Mt3dmsEvtMassFluxObservations: TMt3dmsFluxObservationGroups;
    Mt3dmsMassLoadingMassFluxObservations: TMt3dmsFluxObservationGroups;
    Mt3dmsResMassFluxObservations: TMt3dmsFluxObservationGroups;
    Mt3dmsLakMassFluxObservations: TMt3dmsFluxObservationGroups;
    Mt3dmsDrtMassFluxObservations: TMt3dmsFluxObservationGroups;
    Mt3dmsEtsMassFluxObservations: TMt3dmsFluxObservationGroups;
    procedure NilAll;
    procedure CreateAll;
    procedure FreeAll;
  end;

  TUndoEditFluxObservations = class(TCustomUndo)
  private
    FOldChobObservations: TFluxObservationGroups;
    FNewChobObservations: TFluxObservationGroups;
    FOldDrobObservations: TFluxObservationGroups;
    FNewDrobObservations: TFluxObservationGroups;
    FOldGbobObservations: TFluxObservationGroups;
    FNewGbobObservations: TFluxObservationGroups;
    FOldRvobObservations: TFluxObservationGroups;
    FNewRvobObservations: TFluxObservationGroups;
    FOldMt3dObs: TMassFluxObs;
    FNewMt3dObs: TMassFluxObs;
    procedure AssignMt3dObsToModel(MtsdObs: TMassFluxObs);
    procedure FillMt3dLists(var Mt3dObs: TMassFluxObs);
  protected
    function Description: string; override;
  public
    Constructor Create;
    procedure AssignNewObservations(NewChobObservations, NewDrobObservations,
      NewGbobObservations, NewRvobObservations: TFluxObservationGroups;
      var NewMtsdObs: TMassFluxObs);
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TUndoEditDisplaySettings = class(TCustomUndo)
  private
    FOldSettings: TDisplaySettingsCollection;
    FNewSettings: TDisplaySettingsCollection;
  protected
    function Description: string; override;
  public
    Constructor Create(var NewSettings: TDisplaySettingsCollection);
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TUndoMoveCrossSection = class(TCustomUndo)
  private
    FNewLocation: TSegment2D;
    FOldLocation: TSegment2D;
  protected
    function Description: string; override;
  public
    Constructor Create(Location: TSegment2D);
    procedure DoCommand; override;
    procedure Undo; override;
  end;

implementation

uses SysUtils, Math, frmGoPhastUnit, InteractiveTools, frmSubdivideUnit,
  frmGoToUnit, frmShowHideObjectsUnit,
  frmGridValueUnit, PhastModelUnit, frmDisplayDataUnit;

resourcestring
  StrDeleteRow0dAt = 'delete row %0:d at %1:g.';
  StrDeleteLayer0dAt = 'delete layer %0:d at %1:g.';
  StrDeleteColumn0dA = 'delete column %0:d at %1:g.';
  StrMoveRow0dFrom = 'move row %0:d from %1:g to %2:g.';
  StrMoveColumn0dFro = 'move column %0:d from %1:g to %2:g.';
  StrMoveLayer0dFrom = 'move layer %0:d from %1:g to %2:g.';
  StrAddColumnAtG = 'add column at %g.';
  StrAddRowAtG = 'add row at %g.';
  StrAddLayerAtG = 'add layer at %g.';
  StrChangeAngleFrom0 = 'change angle from %0:g to %1:g.';
  StrEditGridLines = 'edit grid lines';
  StrCreateGrid = 'create grid';
  StrSmoothGrid = 'smooth grid';
  StrChangeVerticalExag = 'change vertical exaggeration';
  StrFreeSurface = 'free surface';
  StrChangeDataSets = 'change data sets';
  StrEditFluxObservatio = 'edit flux observations';
  StrChangeImageSetting = 'change image settings';
  StrRedoS = 'Redo %s';
  StrUndoS = 'Undo %s';
  StrSubdivide = ' subdivide ';
  StrLayerD = '%0:s layer %1:d, ';
  Str0sLayers1d2 = '%0:s layers %1:d-%2:d, ';
  Str0sRow1d = '%0:s row %1:d, ';
  Str0sRows1d2d = '%0:s rows %1:d-%2:d, ';
  Str0sColumn1d = '%0:s column %1:d, ';
  Str0sColumns1d2 = '%0:s columns %1:d-%2:d, ';
  StrMoveCrossSection = 'move cross section';

{ TUndoDeleteRow }

constructor TUndoDeleteRow.Create(const ARow: integer);
begin
  inherited Create;
  FRow := ARow;
  FSelectedRow := frmGoPhast.PhastModel.SelectedRow;
  FRowPosition := frmGoPhast.Grid.RowPosition[FRow];
end;

function TUndoDeleteRow.Description: string;
begin
  result := Format(StrDeleteRow0dAt, [FRow, FRowPosition]);
end;

procedure TUndoDeleteRow.DoCommand;
begin
  frmGoPhast.CanDraw := False;
  try
    if FRow <= frmGoPhast.PhastModel.SelectedRow then
    begin
      frmGoPhast.PhastModel.SelectedRow :=
        frmGoPhast.PhastModel.SelectedRow - 1;
    end;
    frmGoPhast.Grid.DeleteRow(FRow);
    frmGoPhast.frameFrontView.ItemChange(nil);
    InvalidateGrid;
    inherited;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

procedure TUndoDeleteRow.Undo;
begin
  frmGoPhast.CanDraw := False;
  try
    frmGoPhast.Grid.AddRow(FRowPosition);
    frmGoPhast.PhastModel.SelectedRow := FSelectedRow;
    frmGoPhast.frameFrontView.ItemChange(nil);
    InvalidateGrid;
    inherited;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

{ TUndoDeleteColumn }

constructor TUndoDeleteColumn.Create(const AColumn: integer);
begin
  inherited Create;
  FSelectedColumn := frmGoPhast.PhastModel.SelectedColumn;
  FColumn := AColumn;
  FColumnPosition := frmGoPhast.Grid.ColumnPosition[FColumn];
end;

function TUndoDeleteColumn.Description: string;
begin
  result := Format(StrDeleteColumn0dA, [FColumn, FColumnPosition]);
end;

procedure TUndoDeleteColumn.DoCommand;
begin
  frmGoPhast.CanDraw := False;
  try
    if FColumn <= frmGoPhast.PhastModel.SelectedColumn then
    begin
      frmGoPhast.PhastModel.SelectedColumn :=
        frmGoPhast.PhastModel.SelectedColumn - 1;
    end;
    frmGoPhast.Grid.DeleteColumn(FColumn);
    frmGoPhast.frameSideView.ItemChange(nil);
    InvalidateGrid;
    inherited;
  finally
  frmGoPhast.CanDraw := True;
  end;
end;

procedure TUndoDeleteColumn.Undo;
begin
  frmGoPhast.CanDraw := False;
  try
    frmGoPhast.Grid.AddColumn(FColumnPosition);
    frmGoPhast.PhastModel.SelectedColumn := FSelectedColumn;
    frmGoPhast.frameSideView.ItemChange(nil);
    InvalidateGrid;
    inherited;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

{ TCustomUndo }

function TCustomUndo.GetRedoDescription: string;
begin
  result := Format(StrRedoS, [Description]);
end;

function TCustomUndo.GetRedoMenuText: string;
begin
  result := '&' + RedoDescription;
end;

function TCustomUndo.GetShortRedoDescription: string;
begin
  result := Format(StrRedoS, [Description]);
end;

function TCustomUndo.GetShortUndoDescription: string;
begin
  result := Format(StrUndoS, [Description]);
end;

function TCustomUndo.GetUndoDescription: string;
begin
  result := Format(StrUndoS, [Description]);
end;

function TCustomUndo.GetUndoMenuText: string;
begin
  result := '&' + UndoDescription;
end;

procedure TCustomUndo.InvalidateGrid;
begin
  frmGoPhast.InvalidateGrid;
end;

procedure TCustomUndo.Redo;
begin
  DoCommand;
end;

{ TUndoDeleteLayer }

constructor TUndoDeleteLayer.Create(const ALayer: integer);
begin
  inherited Create;
  FLayer := ALayer;
  FSelectedLayer := frmGoPhast.PhastGrid.SelectedLayer;
  FLayerElevation := frmGoPhast.PhastGrid.LayerElevation[FLayer];
end;

function TUndoDeleteLayer.Description: string;
begin
  result := Format(StrDeleteLayer0dAt, [FLayer, FLayerElevation]);
end;

procedure TUndoDeleteLayer.DoCommand;
begin
  frmGoPhast.CanDraw := False;
  try
    if FLayer <= frmGoPhast.PhastGrid.SelectedLayer then
    begin
      frmGoPhast.PhastGrid.SelectedLayer :=
        frmGoPhast.PhastGrid.SelectedLayer - 1;
    end;
    frmGoPhast.PhastGrid.DeleteLayer(FLayer);
    frmGoPhast.frameTopView.ItemChange(nil);
    InvalidateGrid;
    inherited;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

procedure TUndoDeleteLayer.Undo;
begin
  frmGoPhast.CanDraw := False;
  try
    frmGoPhast.PhastGrid.AddLayer(FLayerElevation);
    frmGoPhast.PhastGrid.SelectedLayer := FSelectedLayer;
    frmGoPhast.frameTopView.ItemChange(nil);
    InvalidateGrid;
    inherited;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

{ TUndoMoveRow }

constructor TUndoMoveRow.Create(const ARow: integer;
  const NewPosition: real);
begin
  inherited Create(ARow);
  FNewRowPosition := NewPosition;
end;

function TUndoMoveRow.Description: string;
begin
  result := Format(StrMoveRow0dFrom, [FRow, FRowPosition, FNewRowPosition]);
end;

procedure TUndoMoveRow.DoCommand;
begin
  frmGoPhast.CanDraw := False;
  try
    frmGoPhast.Grid.RowPosition[FRow] := FNewRowPosition;
    FNewRow := frmGoPhast.Grid.NearestRowPosition(FNewRowPosition);
    InvalidateGrid;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

procedure TUndoMoveRow.Undo;
begin
  frmGoPhast.CanDraw := False;
  try
    frmGoPhast.Grid.RowPosition[FNewRow] := FRowPosition;
    InvalidateGrid;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

{ TUndoMoveColumn }

constructor TUndoMoveColumn.Create(const AColumn: integer;
  const NewPosition: real);
begin
  inherited Create(AColumn);
  FNewColumnPosition := NewPosition;
end;

function TUndoMoveColumn.Description: string;
begin
  result :=  Format(StrMoveColumn0dFro,
    [FColumn, FColumnPosition, FNewColumnPosition]);
end;

procedure TUndoMoveColumn.DoCommand;
begin
  frmGoPhast.CanDraw := False;
  try
    frmGoPhast.Grid.ColumnPosition[FColumn] := FNewColumnPosition;
    FNewColumn := frmGoPhast.Grid.NearestColumnPosition(FNewColumnPosition);
    InvalidateGrid;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

procedure TUndoMoveColumn.Undo;
begin
  frmGoPhast.CanDraw := False;
  try
    frmGoPhast.Grid.ColumnPosition[FNewColumn] := FColumnPosition;
    InvalidateGrid;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

{ TUndoMoveLayer }

constructor TUndoMoveLayer.Create(const ALayer: integer;
  const NewPosition: real);
begin
  inherited Create(ALayer);
  FNewLayerElevation := NewPosition;
end;

function TUndoMoveLayer.Description: string;
begin
  result := Format(StrMoveLayer0dFrom,
    [FLayer, FLayerElevation, FNewLayerElevation]);
end;

procedure TUndoMoveLayer.DoCommand;
begin
  frmGoPhast.CanDraw := False;
  try
    Assert(frmGoPhast.PhastModel.ModelSelection = msPhast);
    frmGoPhast.PhastGrid.LayerElevation[FLayer] := FNewLayerElevation;
    FNewLayer := frmGoPhast.PhastGrid.NearestLayerPosition(FNewLayerElevation);
    InvalidateGrid;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

procedure TUndoMoveLayer.Undo;
begin
  frmGoPhast.CanDraw := False;
  try
    Assert(frmGoPhast.PhastModel.ModelSelection = msPhast);
    frmGoPhast.PhastGrid.LayerElevation[FNewLayer] := FLayerElevation;
    InvalidateGrid;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

{ TUndoAddColumn }

constructor TUndoAddColumn.Create(const NewPosition: real);
begin
  inherited Create;
  FSelectedColumn := frmGoPhast.PhastModel.SelectedColumn;
  FColumnPosition := NewPosition;
end;

function TUndoAddColumn.Description: string;
begin
  result := Format(StrAddColumnAtG, [FColumnPosition]);
end;

procedure TUndoAddColumn.DoCommand;
begin
  frmGoPhast.CanDraw := False;
  try
    frmGoPhast.Grid.AddColumn(FColumnPosition);
    FColumn := frmGoPhast.Grid.NearestColumnPosition(FColumnPosition);
    if FColumn <= frmGoPhast.PhastModel.SelectedColumn then
    begin
      frmGoPhast.PhastModel.SelectedColumn :=
        frmGoPhast.PhastModel.SelectedColumn + 1;
    end;
    frmGoPhast.frameSideView.ItemChange(nil);
    InvalidateGrid;
    inherited;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

procedure TUndoAddColumn.Undo;
begin
  frmGoPhast.CanDraw := False;
  try
    frmGoPhast.Grid.DeleteColumn(FColumn);
    frmGoPhast.PhastModel.SelectedColumn := FSelectedColumn;
    frmGoPhast.frameSideView.ItemChange(nil);
    InvalidateGrid;
    inherited;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

{ TUndoAddRow }

constructor TUndoAddRow.Create(const NewPosition: real);
begin
  inherited Create;
  SelectedRow := frmGoPhast.PhastModel.SelectedRow;
  RowPosition := NewPosition;
end;

function TUndoAddRow.Description: string;
begin
  result := Format(StrAddRowAtG, [RowPosition]);
end;

procedure TUndoAddRow.DoCommand;
begin
  frmGoPhast.CanDraw := False;
  try
    frmGoPhast.Grid.AddRow(RowPosition);
    Row := frmGoPhast.Grid.NearestRowPosition(RowPosition);
    if Row <= frmGoPhast.PhastModel.SelectedRow then
    begin
      frmGoPhast.PhastModel.SelectedRow := frmGoPhast.PhastModel.SelectedRow + 1;
    end;
    frmGoPhast.frameFrontView.ItemChange(nil);
    InvalidateGrid;
    inherited;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

procedure TUndoAddRow.Undo;
begin
  frmGoPhast.CanDraw := False;
  try
    frmGoPhast.Grid.DeleteRow(Row);
    frmGoPhast.PhastModel.SelectedRow := SelectedRow;
    frmGoPhast.frameFrontView.ItemChange(nil);
    InvalidateGrid;
    inherited;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

{ TUndoAddLayer }

constructor TUndoAddLayer.Create(const NewPosition: real);
begin
  inherited Create;
  Assert(frmGoPhast.PhastModel.ModelSelection = msPhast);
  FSelectedLayer := frmGoPhast.PhastGrid.SelectedLayer;
  FLayerElevation := NewPosition;
end;

function TUndoAddLayer.Description: string;
begin
  result := Format(StrAddLayerAtG, [FLayerElevation]);
end;

procedure TUndoAddLayer.DoCommand;
begin
  frmGoPhast.CanDraw := False;
  try
    Assert(frmGoPhast.PhastModel.ModelSelection = msPhast);
    frmGoPhast.PhastGrid.AddLayer(FLayerElevation);
    FLayer := frmGoPhast.PhastGrid.NearestLayerPosition(FLayerElevation);
    if FLayer <= frmGoPhast.PhastGrid.SelectedLayer then
    begin
      frmGoPhast.PhastGrid.SelectedLayer := frmGoPhast.PhastGrid.SelectedLayer +
        1;
    end;
    InvalidateGrid;
    frmGoPhast.frameTopView.ItemChange(nil);
    inherited;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

procedure TUndoAddLayer.Undo;
begin
  frmGoPhast.CanDraw := False;
  try
    Assert(frmGoPhast.PhastModel.ModelSelection = msPhast);
    frmGoPhast.PhastGrid.DeleteLayer(FLayer);
    frmGoPhast.PhastGrid.SelectedLayer := FSelectedLayer;
    InvalidateGrid;
    frmGoPhast.frameTopView.ItemChange(nil);
    inherited;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

{ TUndoSubdivide }

function TUndoSubdivide.Description: string;
begin
  result := StrSubdivide;
  if LayerCount > 1 then
  begin
    if FirstLayer = LastLayer then
    begin
      result := Format(StrLayerD, [result, FirstLayer+1]);
    end
    else
    begin
      result := Format(Str0sLayers1d2, [result, FirstLayer+1, LastLayer+1]);
    end;
  end;
  if RowCount > 1 then
  begin
    if FirstRow = LastRow then
    begin
      result := Format(Str0sRow1d, [result, FirstRow+1]);
    end
    else
    begin
      result := Format(Str0sRows1d2d, [result, FirstRow+1, LastRow+1]);
    end;
  end;
  if ColumnCount > 1 then
  begin
    if FirstColumn = LastColumn then
    begin
      result := Format(Str0sColumn1d, [result, FirstColumn+1]);
    end
    else
    begin
      result := Format(Str0sColumns1d2, [result, FirstColumn+1, LastColumn+1]);
    end;
  end;
  SetLength(result, Length(result) - 2);
end;

procedure TUndoSubdivide.DoCommand;
var
  Count: integer;
  Positions: TOneDRealArray;
  Index: integer;
begin
  frmGoPhast.CanDraw := False;
  try
    SubdivideGridTool.Subdividing := False;
    if (FirstColumn >= 0) and (LastColumn >= 0) then

    begin
      Count := ColumnCount;
      if Count > 1 then
      begin
        Positions := frmGoPhast.Grid.ColumnPositions;
        for Index := LastColumn downto FirstColumn do
        begin
          Subdivide(Count, Index, Positions);
        end;
        frmGoPhast.Grid.ColumnPositions := Positions;
      end;
    end;
    if (FirstRow >= 0) and (LastRow >= 0) then
    begin
      Count := RowCount;
      if Count > 1 then
      begin
        Positions := frmGoPhast.Grid.RowPositions;
        for Index := LastRow downto FirstRow do
        begin
          Subdivide(Count, Index, Positions);
        end;
        frmGoPhast.Grid.RowPositions := Positions;
      end;
    end;
    if (FirstLayer >= 0) and (LastLayer >= 0)
      and (frmGoPhast.PhastModel.ModelSelection = msPhast) then
    begin
      Count := LayerCount;
      if Count > 1 then
      begin
        Positions := frmGoPhast.PhastGrid.LayerElevations;
        for Index := LastLayer downto FirstLayer do
        begin
          Subdivide(Count, Index, Positions);
        end;
        frmGoPhast.PhastGrid.LayerElevations := Positions;
      end;
    end;
    InvalidateGrid;
    inherited;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

procedure TUndoSubdivide.Recombine(const Count, OldPosition: integer;
  var AnArray: TOneDRealArray);
var
  OldLength: integer;
  Index: integer;
  NewPosition: real;
begin
  OldLength := Length(AnArray);
  for Index := OldPosition to OldLength - Count - 1 do
  begin
    NewPosition := AnArray[Index + Count];
    AnArray[Index + 1] := NewPosition;
  end;
  SetLength(AnArray, OldLength - Count + 1);
end;

procedure TUndoSubdivide.Subdivide(const Count, OldPosition: integer;
  var AnArray: TOneDRealArray);
var
  OldLength: integer;
  Index: integer;
  NewPosition: real;
begin
  OldLength := Length(AnArray);
  SetLength(AnArray, OldLength + Count - 1);
  for Index := 1 to Count - 1 do
  begin
    NewPosition := (Index / Count) * (AnArray[OldPosition + 1] -
      AnArray[OldPosition]) + AnArray[OldPosition];
    AnArray[OldLength + Index - 1] := NewPosition;
  end;
end;

procedure TUndoSubdivide.Undo;
var
  Count: integer;
  Positions: TOneDRealArray;
  Index: integer;
begin
  frmGoPhast.CanDraw := False;
  try
    SubdivideGridTool.Subdividing := False;
    if (FirstColumn >= 0) and (LastColumn >= 0) then

    begin
      Count := ColumnCount;
      if Count > 1 then
      begin
        Positions := frmGoPhast.Grid.ColumnPositions;
        for Index := FirstColumn to LastColumn do
        begin
          Recombine(Count, Index, Positions);
        end;
        frmGoPhast.Grid.ColumnPositions := Positions;
      end;
    end;
    if (FirstRow >= 0) and (LastRow >= 0) then
    begin
      Count := RowCount;
      if Count > 1 then
      begin
        Positions := frmGoPhast.Grid.RowPositions;
        for Index := FirstRow to LastRow do
        begin
          Recombine(Count, Index, Positions);
        end;
        frmGoPhast.Grid.RowPositions := Positions;
      end;
    end;
    if (FirstLayer >= 0) and (LastLayer >= 0)
      and (frmGoPhast.PhastModel.ModelSelection = msPhast) then
    begin
      Count := LayerCount;
      if Count > 1 then
      begin
        Positions := frmGoPhast.PhastGrid.LayerElevations;
        for Index := FirstLayer to LastLayer do
        begin
          Recombine(Count, Index, Positions);
        end;
        frmGoPhast.PhastGrid.LayerElevations := Positions;
      end;
    end;
    InvalidateGrid;
    inherited;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

{ TUndoSetAngle }

constructor TUndoSetAngle.Create(const Angle: real);
begin
  inherited Create;
  FNewAngle := Angle;
  FOldAngle := frmGoPhast.Grid.GridAngle;
  FOldRows := frmGoPhast.Grid.RowPositions;
  SetLength(FOldRows, Length(FOldRows));
  FOldColumns := frmGoPhast.Grid.ColumnPositions;
  SetLength(FOldColumns, Length(FOldColumns));

end;

function TUndoSetAngle.Description: string;
begin
  result := Format(StrChangeAngleFrom0,
    [(FOldAngle / Pi * 180), (FNewAngle / Pi * 180)]);
end;

procedure TUndoSetAngle.DoCommand;
begin
  Screen.Cursor := crHourGlass;
  frmGoPhast.CanDraw := False;
  try
    frmGoPhast.Grid.GridAngle := FNewAngle;
    frmGoPhast.dcMoveColCursor.RedrawCursor;
    frmGoPhast.dcMoveRowCursor.RedrawCursor;
    frmGoPhast.dcAddColCursor.RedrawCursor;
    frmGoPhast.dcAddRowCursor.RedrawCursor;
    frmGoPhast.dcSubdivide.RedrawCursor;
    frmGoPhast.dcSetSpacing.RedrawCursor;
    InvalidateGrid;
    frmGoPhast.SynchronizeViews(vdTop);
  finally
    frmGoPhast.CanDraw := True;
    Screen.Cursor := crDefault;
  end;
end;

procedure TUndoSetAngle.Undo;
begin
  Screen.Cursor := crHourGlass;
  frmGoPhast.CanDraw := False;
  try
    frmGoPhast.Grid.GridAngle := FOldAngle;
    frmGoPhast.Grid.RowPositions := FOldRows;
    frmGoPhast.Grid.ColumnPositions := FOldColumns;
    frmGoPhast.dcMoveColCursor.RedrawCursor;
    frmGoPhast.dcMoveRowCursor.RedrawCursor;
    frmGoPhast.dcAddColCursor.RedrawCursor;
    frmGoPhast.dcAddRowCursor.RedrawCursor;
    frmGoPhast.dcSubdivide.RedrawCursor;
    frmGoPhast.dcSetSpacing.RedrawCursor;
    InvalidateGrid;
    frmGoPhast.SynchronizeViews(vdTop);
  finally
    frmGoPhast.CanDraw := True;
    Screen.Cursor := crDefault;
  end;
end;

{ TUndoEditGridLines }

procedure TUndoEditGridLines.ChangeView;
var
  Temp1, Temp2: double;
begin
  // Set the magnifications so that the grid will fill most of the screen.
  with frmGoPhast.Grid do
  begin
    with frmGoPhast.frameTopView.ZoomBox do
    begin
      if ColumnCount >= 0 then
      begin
        Temp1 := Width / Abs(ColumnPosition[ColumnCount] - ColumnPosition[0])
      end
      else
      begin
        Temp1 := 0
      end;
      if RowCount >= 0 then
      begin
        Temp2 := Height / Abs(RowPosition[RowCount] - RowPosition[0])
      end
      else
      begin
        Temp2 := 0
      end;
      Temp1 := Min(Temp1, Temp2);
      if Temp1 > 0 then
      begin
        Magnification := 0.90 * Temp1;
      end;

    end;
    with frmGoPhast.frameFrontView.ZoomBox do
    begin
      if ColumnCount >= 0 then
      begin
        Temp1 := Width / Abs(ColumnPosition[ColumnCount] - ColumnPosition[0])
      end
      else
      begin
        Temp1 := 0
      end;
      if LayerCount >= 0 then
      begin
        Temp2 := Height / (Abs(HighestElevation - LowestElevation) *
          Exaggeration)
      end
      else
      begin
        Temp2 := 0
      end;
      Temp1 := Min(Temp1, Temp2);
      if Temp1 > 0 then
      begin
        Magnification := 0.90 * Temp1;
      end;
    end;

    with frmGoPhast.frameSideView.ZoomBox do
    begin
      if RowCount >= 0 then
      begin
        Temp1 := Height / Abs(RowPosition[RowCount] - RowPosition[0])
      end
      else
      begin
        Temp1 := 0
      end;
      if LayerCount >= 0 then
      begin
        Temp2 := Width / (Abs(HighestElevation - LowestElevation) *
          Exaggeration)
      end
      else
      begin
        Temp2 := 0
      end;
      Temp1 := Min(Temp1, Temp2);
      if Temp1 > 0 then
      begin
        Magnification := 0.90 * Temp1;
      end;
    end;

    // Make sure the grid is visible on the screen.
    if (ColumnCount > 0) and (RowCount > 0) then
    begin
      MoveToTopCell(frmGoPhast.Grid, ColumnCount div 2, RowCount div 2);
    end;
    if (ColumnCount > 0) and (RowCount > 0) and (LayerCount > 0) then
    begin
      MoveToFrontCell(frmGoPhast.Grid, ColumnCount div 2, LayerCount div 2);
    end;
    if (ColumnCount > 0) and (RowCount > 0) and (LayerCount > 0) then
    begin
      MoveToSideCell(frmGoPhast.Grid, RowCount div 2, LayerCount div 2);
    end;
  end;

end;

constructor TUndoEditGridLines.Create;
begin
  inherited Create;
  FOldRows := frmGoPhast.Grid.RowPositions;
  SetLength(FOldRows, Length(FOldRows));
  FOldColumns := frmGoPhast.Grid.ColumnPositions;
  SetLength(FOldColumns, Length(FOldColumns));
  if frmGoPhast.PhastModel.ModelSelection = msPhast then
  begin
    FOldLayerElevations := frmGoPhast.PhastGrid.LayerElevations;
    SetLength(FOldLayerElevations, Length(FOldLayerElevations));
  end;
end;

function TUndoEditGridLines.Description: string;
begin
  result := StrEditGridLines;
end;

procedure TUndoEditGridLines.DoCommand;
begin
  frmGoPhast.CanDraw := False;
  frmGoPhast.PhastModel.BeginGridChange;
  try
    frmGoPhast.Grid.BeginRowChange;
    try
      frmGoPhast.Grid.RowPositions := FNewRows;
    finally
      frmGoPhast.Grid.EndRowChange;
    end;
    frmGoPhast.Grid.BeginColumnChange;
    try
      frmGoPhast.Grid.ColumnPositions := FNewColumns;
    finally
      frmGoPhast.Grid.EndColumnChange;
    end;
    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      frmGoPhast.Grid.BeginLayerChange;
      try
        frmGoPhast.PhastGrid.LayerElevations := FNewLayerElevations;
      finally
        frmGoPhast.Grid.EndLayerChange;
      end;
    end
    else
    begin
      frmGoPhast.ModflowGrid.LayerCount :=
        frmGoPhast.PhastModel.LayerStructure.LayerCount;
      frmGoPhast.UpdateDataSetDimensions;
      frmGoPhast.ModflowGrid.UpdateCellElevations;
    end;
  finally
    frmGoPhast.PhastModel.EndGridChange;
    frmGoPhast.CanDraw := True;
  end;
  inherited;
  ChangeView;
  InvalidateGrid;
  frmGoPhast.InvalidateImage32AllViews;
end;

procedure TUndoEditGridLines.Undo;
begin
  frmGoPhast.CanDraw := False;
  try
    frmGoPhast.Grid.RowPositions := FOldRows;
    frmGoPhast.Grid.ColumnPositions := FOldColumns;
    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      frmGoPhast.PhastGrid.LayerElevations := FOldLayerElevations;
    end;
    inherited;
  finally
    frmGoPhast.CanDraw := True;
  end;
  ChangeView;
  InvalidateGrid;
end;

{ TUndoSmoothGrid }

function TUndoSmoothGrid.Description: string;
begin
  result := StrSmoothGrid;
end;

{ TUndoVerticalExaggeration }

constructor TUndoVerticalExaggeration.Create(
  const ANewVerticalExaggeration: real);
begin
  inherited Create;
  FOldVerticalExaggeration := GetOldVE;
  FNewVerticalExaggeration := ANewVerticalExaggeration;
end;

function TUndoVerticalExaggeration.Description: string;
begin
  result := StrChangeVerticalExag;
end;

procedure TUndoVerticalExaggeration.DoCommand;
begin
  SetVE(FNewVerticalExaggeration);
end;

class function TUndoVerticalExaggeration.GetOldVE: real;
begin
  result := 1;
  if frmGoPhast.frameFrontView <> nil then
  begin
    result := frmGoPhast.frameFrontView.ZoomBox.Exaggeration;
  end
  else if frmGoPhast.frameSideView <> nil then
  begin
    result := frmGoPhast.frameSideView.ZoomBox.Exaggeration;
  end;
end;


procedure TUndoVerticalExaggeration.SetVE(
  VerticalExaggeration: real);
var
  FrontCenter: TPoint2D;
  SideCenter: TPoint2D;
  Temp: TPoint2D;
begin
  if VerticalExaggeration = 0 then
  begin
    VerticalExaggeration := 1;
  end;
  with frmGoPhast do
  begin
    if (frameFrontView <> nil) and (frameFrontView.ZoomBox.Exaggeration <>
      VerticalExaggeration)
      or (frameSideView <> nil) and (frameSideView.ZoomBox.Exaggeration <>
      VerticalExaggeration) then
    begin
      FrontCenter.X := frameFrontView.ZoomBox.X(
        frameFrontView.ZoomBox.Image32.Width div 2);
      FrontCenter.Y := frameFrontView.ZoomBox.Y(
        frameFrontView.ZoomBox.Image32.Height div 2);
      SideCenter.X := frameSideView.ZoomBox.X(
        frameSideView.ZoomBox.Image32.Width div 2);
      SideCenter.Y := frameSideView.ZoomBox.Y(
        frameSideView.ZoomBox.Image32.Height div 2);

      PhastModel.Exaggeration := VerticalExaggeration;

      Temp.X := frameFrontView.ZoomBox.X(
        frameFrontView.ZoomBox.Image32.Width div 2);
      Temp.Y := frameFrontView.ZoomBox.Y(
        frameFrontView.ZoomBox.Image32.Height div 2);
      frameFrontView.ZoomBox.OriginX := frameFrontView.ZoomBox.OriginX
        - Temp.X + FrontCenter.X;
      frameFrontView.ZoomBox.OriginY := frameFrontView.ZoomBox.OriginY
        - Temp.Y + FrontCenter.Y;

      Temp.X := frameSideView.ZoomBox.X(
        frameSideView.ZoomBox.Image32.Width div 2);
      Temp.Y := frameSideView.ZoomBox.Y(
        frameSideView.ZoomBox.Image32.Height div 2);
      frameSideView.ZoomBox.OriginX := frameSideView.ZoomBox.OriginX
        - Temp.X + SideCenter.X;
      frameSideView.ZoomBox.OriginY := frameSideView.ZoomBox.OriginY
        - Temp.Y + SideCenter.Y;

      frmGoPhast.FrontDiscretizationChanged := True;
      frmGoPhast.SideDiscretizationChanged := True;
      frameSideView.ZoomBox.InvalidateImage32;
      frameFrontView.ZoomBox.InvalidateImage32;
      AdjustScales;
    end;
  end;
end;

procedure TUndoVerticalExaggeration.Undo;
begin
  SetVE(FOldVerticalExaggeration);
end;

{ TCustomUndoChangeGridDimensions }

constructor TCustomUndoChangeGridDimensions.Create;
begin
  inherited;
  if frmGoPhast.PhastModel.ModelSelection in [msModflow, msModflowLGR, msModflowNWT] then
  begin
    FLayerCount := frmGoPhast.PhastModel.ModflowGrid.LayerCount;
  end;
end;

procedure TCustomUndoChangeGridDimensions.DoCommand;
begin
  UpdateDataSets;
  if frmGoPhast.PhastModel.ModelSelection in [msModflow, msModflowLGR, msModflowNWT] then
  begin
    if frmGoPhast.PhastModel.ModflowGrid.LayerCount < 0 then
    begin
      frmGoPhast.PhastModel.ModflowGrid.LayerCount := frmGoPhast.PhastModel.LayerStructure.LayerCount;
    end;
  end;
end;

procedure TCustomUndoChangeGridDimensions.Undo;
begin
  UpdateDataSets;
  if frmGoPhast.PhastModel.ModelSelection in [msModflow, msModflowLGR, msModflowNWT] then
  begin
      frmGoPhast.PhastModel.ModflowGrid.LayerCount := FLayerCount;
  end;
end;

procedure TCustomUndoChangeGridDimensions.UpdateDataSets;
begin
  frmGoPhast.PhastModel.UpdateDataSetDimensions;
end;
{ TUndoCreateGrid }

constructor TUndoCreateGrid.Create;
begin
  inherited;
  FOldAngle := frmGoPhast.PhastModel.Grid.GridAngle;
end;

function TUndoCreateGrid.Description: string;
begin
  result := StrCreateGrid;
end;

procedure TUndoCreateGrid.DoCommand;
begin
  frmGoPhast.CanDraw := False;
  try
    // Set the grid angle first because setting the grid angle
    // changes the row and column positions that are set in
    // inherited.
    frmGoPhast.PhastModel.Grid.GridAngle := NewAngle;
    inherited;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

procedure TUndoCreateGrid.Undo;
begin
  frmGoPhast.CanDraw := False;
  try
    // Set the grid angle first because setting the grid angle
    // changes the row and column positions that are set in
    // inherited.
    frmGoPhast.PhastModel.Grid.GridAngle := FOldAngle;
    inherited;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

{ TUndoFreeSurface }

function TUndoFreeSurface.Changed: boolean;
begin
  result := (NewFreeSurface <> FOldFreeSurface)
    or (NewInitialWaterTable <> FOldInitialWaterTable);
end;

constructor TUndoFreeSurface.Create;
begin
  FOldFreeSurface := frmGoPhast.PhastModel.FreeSurface;
  FOldInitialWaterTable := frmGoPhast.PhastModel.UseWaterTable;
end;

function TUndoFreeSurface.Description: string;
begin
  result := StrFreeSurface;
end;

procedure TUndoFreeSurface.DoCommand;
var
  PhastModel: TPhastModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  PhastModel.FreeSurface := NewFreeSurface;
  PhastModel.UseWaterTable := NewFreeSurface and NewInitialWaterTable;
  PhastModel.DataArrayManager.CreateInitialDataSets;
end;

procedure TUndoFreeSurface.Undo;
var
  PhastModel: TPhastModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  PhastModel.FreeSurface := FOldFreeSurface;
  PhastModel.UseWaterTable := FOldFreeSurface and FOldInitialWaterTable;
  PhastModel.DataArrayManager.CreateInitialDataSets;
end;

{ TUndoChangeDataSets }

procedure TUndoChangeDataSets.ClearExpressionsAndVariables;
begin
  with frmGoPhast.PhastModel do
  begin
    rpTopFormulaCompiler.ClearExpressions;
    rpTopFormulaCompiler.ClearVariables;
    rpFrontFormulaCompiler.ClearExpressions;
    rpFrontFormulaCompiler.ClearVariables;
    rpSideFormulaCompiler.ClearExpressions;
    rpSideFormulaCompiler.ClearVariables;
    rpThreeDFormulaCompiler.ClearExpressions;
    rpThreeDFormulaCompiler.ClearVariables;

    rpTopFormulaCompilerNodes.ClearExpressions;
    rpTopFormulaCompilerNodes.ClearVariables;
    rpFrontFormulaCompilerNodes.ClearExpressions;
    rpFrontFormulaCompilerNodes.ClearVariables;
    rpSideFormulaCompilerNodes.ClearExpressions;
    rpSideFormulaCompilerNodes.ClearVariables;
    rpThreeDFormulaCompilerNodes.ClearExpressions;
    rpThreeDFormulaCompilerNodes.ClearVariables;
  end;
end;

constructor TUndoChangeDataSets.Create(var DeletedDataSets, NewDataSets: TList;
  var NewDataSetProperties: TObjectList);
begin
  inherited Create;
  FOldNames := TStringList.Create;
  FNewNames := TStringList.Create;

  // take over ownership of DeletedDataSets;
  FDeletedDataSets := DeletedDataSets;
  DeletedDataSets := nil;

  // take over ownership of NewDataSets;
  FNewDataSets := NewDataSets;
  NewDataSets := nil;

  FOldDataSetProperties:= TObjectList.Create;

//  FNewDataSetProperties:= TObjectList.Create;
  // take over ownership of NewDataSetProperties;
  FNewDataSetProperties := NewDataSetProperties;
  NewDataSetProperties := nil;

  StoreData;
end;

function TUndoChangeDataSets.Description: string;
begin
  result := StrChangeDataSets;
end;

destructor TUndoChangeDataSets.Destroy;
begin
  FDeletedDataSets.Free;
  FNewDataSets.Free;
  FOldDataSetProperties.Free;
  FNewDataSetProperties.Free;
  FNewNames.Free;
  FOldNames.Free;
  inherited;
end;

procedure TUndoChangeDataSets.SetProperties(AddedDataSets, DeletedDataSets,
  DataSetProperties: TList);
var
  Index: integer;
//  DataSet : TDataArray;
  DataStorage: TPhastDataSetStorage;
  UpdateObjectDisplay: boolean;
  ShouldInvalidateDataArray: array of boolean;
  DataArrayManager: TDataArrayManager;
begin
  DisallowChildGridUpdates;
  try
    UpdateFormulas(DataSetProperties);

    UpdateObjectDisplay := (DeletedDataSets.Count > 0)
      or (AddedDataSets.Count > 0);
    ClearExpressionsAndVariables;
    frmGoPhast.PhastModel.CreateGlobalVariables;
    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    DataArrayManager.HandleDeletedDataArrays(DeletedDataSets);
    // extract any data sets that have been deleted and store them.
    DataArrayManager.HandleAddedDataArrays(AddedDataSets);

    for Index := 0 to DataSetProperties.Count - 1 do
    begin
      DataStorage := DataSetProperties[Index];
      if (DataStorage.Name <> DataStorage.FDataSet.Name) then
      begin
        UpdateObjectDisplay := True;
      end;
      DataStorage.AssignBasicProperties;
      frmGoPhast.PhastModel.CreateVariables(DataStorage.FDataSet);
    end;

    // update the properties of the data sets.
    SetLength(ShouldInvalidateDataArray, DataSetProperties.Count);
    for Index := 0 to DataSetProperties.Count - 1 do
    begin
      // set the data set properties except for the formula.
      DataStorage := DataSetProperties[Index];
      if (DataStorage.Name <> DataStorage.FDataSet.Name) then
      begin
        UpdateObjectDisplay := True;
      end;

      DataStorage.AssignToDataSet(ShouldInvalidateDataArray[Index]);

      frmGoPhast.PhastModel.UpdateDataArrayDimensions(DataStorage.FDataSet);
//      DataStorage.FDataSet.UpdateDimensions(frmGoPhast.Grid.LayerCount,
//        frmGoPhast.Grid.RowCount, frmGoPhast.Grid.ColumnCount);
  //    frmGoPhast.PhastModel.CreateVariables(DataStorage.FDataSet);
    end;

    // After all the data set names have been updated,
    // update the data set formulas.
    for Index := 0 to DataSetProperties.Count - 1 do
    begin
      DataStorage := DataSetProperties[Index];
      DataStorage.AssignFormulasToDataSet;
      if ShouldInvalidateDataArray[Index] then
      begin
        DataStorage.DataSet.Invalidate;
      end;
    end;

    if FOldNames.Count > 0 then
    begin
      frmGoPhast.PhastModel.FormulaManager.RestoreSubscriptions;
    end;

    // make sure that if the orientation of the data set has
    // changed that the data sets that are used to color the
    // grid are still valid.
    if (frmGoPhast.PhastModel <> nil) then
    begin
      if (frmGoPhast.PhastModel.TopDataSet <> nil)
        and not (frmGoPhast.PhastModel.TopDataSet.Orientation
        in [dsoTop, dso3D]) then
      begin
        frmGoPhast.PhastModel.TopDataSet := nil;
      end;
      if (frmGoPhast.PhastModel.FrontDataSet <> nil)
        and not (frmGoPhast.PhastModel.FrontDataSet.Orientation
        in [dsoFront, dso3D]) then
      begin
        frmGoPhast.PhastModel.FrontDataSet := nil;
      end;
      if (frmGoPhast.PhastModel.SideDataSet <> nil)
        and not (frmGoPhast.PhastModel.SideDataSet.Orientation
        in [dsoSide, dso3D]) then
      begin
        frmGoPhast.PhastModel.SideDataSet := nil;
      end;
    end;

    if UpdateObjectDisplay and (frmShowHideObjects <> nil) then
    begin
      frmShowHideObjects.UpdateScreenObjects;
    end;
  finally
    AllowChildGridUpdates;
  end;

end;

procedure TUndoChangeDataSets.DoCommand;
begin
  inherited;
  SetProperties(FNewDataSets, FDeletedDataSets, FNewDataSetProperties);
  if FDeletedDataSets.IndexOf(FTopDataSet) >= 0 then
  begin
    frmGoPhast.PhastModel.TopDataSet := nil;
  end;
  if FDeletedDataSets.IndexOf(FFrontDataSet) >= 0 then
  begin
    frmGoPhast.PhastModel.FrontDataSet := nil;
  end;
  if FDeletedDataSets.IndexOf(FSideDataSet) >= 0 then
  begin
    frmGoPhast.PhastModel.SideDataSet := nil;
  end;
  if FDeletedDataSets.IndexOf(F3DDataSet) >= 0 then
  begin
    frmGoPhast.PhastModel.ThreeDDataSet := nil;
  end;
  UpdateFrmDisplayData;
//  UpdateFrmContourData;
  UpdateFrmGridValue;
//  frmGoPhast.PhastModel.DataArrayNameChangeWarning;
end;

procedure TUndoChangeDataSets.StoreData;
var
  Index: integer;
  DataSet: TDataArray;
  DataStorage: TPhastDataSetStorage;
  DataArrayManager: TDataArrayManager;
begin
  if frmGoPhast.PhastModel <> nil then
  begin
    FTopDataSet := frmGoPhast.PhastModel.TopDataSet;
    FFrontDataSet := frmGoPhast.PhastModel.FrontDataSet;
    FSideDataSet := frmGoPhast.PhastModel.SideDataSet;
    F3DDataSet := frmGoPhast.PhastModel.ThreeDDataSet;
  end
  else
  begin
    FTopDataSet := nil;
    FFrontDataSet := nil;
    FSideDataSet := nil;
    F3DDataSet := nil;
  end;

  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  for Index := 0 to DataArrayManager.DataSetCount -1 do
  begin
    DataSet := DataArrayManager.DataSets[Index];
    DataStorage := TPhastDataSetStorage.Create;
    FOldDataSetProperties.Add(DataStorage);
    DataStorage.Assign(DataSet);
  end;
end;

procedure TUndoChangeDataSets.Undo;
begin
  inherited;
  SetProperties(FDeletedDataSets, FNewDataSets, FOldDataSetProperties);
  if (FTopDataSet = nil) or (FDeletedDataSets.IndexOf(FTopDataSet) >= 0) then
  begin
    frmGoPhast.PhastModel.TopDataSet := FTopDataSet;
  end;
  if (FFrontDataSet = nil) or (FDeletedDataSets.IndexOf(FFrontDataSet) >= 0) then
  begin
    frmGoPhast.PhastModel.FrontDataSet := FFrontDataSet;
  end;
  if (FSideDataSet = nil) or (FDeletedDataSets.IndexOf(FSideDataSet) >= 0) then
  begin
    frmGoPhast.PhastModel.SideDataSet := FSideDataSet;
  end;
  if (F3DDataSet = nil) or (FDeletedDataSets.IndexOf(F3DDataSet) >= 0) then
  begin
    frmGoPhast.PhastModel.ThreeDDataSet := F3DDataSet;
  end;
  UpdateFrmDisplayData;
//  UpdateFrmContourData;
  UpdateFrmGridValue;
//  frmGoPhast.PhastModel.DataArrayNameChangeWarning;
end;

procedure TCustomUndo.AllowChildGridUpdates;
begin
  frmGoPhast.PhastModel.AllowChildGridUpdates;
end;

procedure TCustomUndo.DisallowChildGridUpdates;
begin
  frmGoPhast.PhastModel.DisallowChildGridUpdates;
end;

procedure TUndoChangeDataSets.UpdateFormulas(DataSetProperties: TList);
var
  VarIndex: Integer;
  Compiler: TRbwParser;
  CompilerIndex: Integer;
  DataStorage: TPhastDataSetStorage;
  Index: Integer;
  CompilerList: TList;
  VariableIndex: Integer;
  OldVariableName: string;
  NewVariableName: string;
begin
  FOldNames.Clear;
  FNewNames.Clear;
  for Index := 0 to DataSetProperties.Count - 1 do
  begin
    // set the data set properties except for the formula.
    DataStorage := DataSetProperties[Index];
    if (DataStorage.Name <> DataStorage.FDataSet.Name) then
    begin
      FOldNames.Add(DataStorage.FDataSet.Name);
      FNewNames.Add(DataStorage.Name);
    end;
  end;
  if FOldNames.Count > 0 then
  begin
    frmGoPhast.PhastModel.FormulaManager.RemoveSubscriptions(
      FOldNames, FNewNames);
    CompilerList := TList.Create;
    try
      frmGoPhast.PhastModel.FillCompilerList(CompilerList);

      for VariableIndex := 0 to FOldNames.Count - 1 do
      begin
        OldVariableName := FOldNames[VariableIndex];
        NewVariableName := FNewNames[VariableIndex];
        for CompilerIndex := 0 to CompilerList.Count - 1 do
        begin
          Compiler := CompilerList[CompilerIndex];
          VarIndex := Compiler.IndexOfVariable(OldVariableName);
          if VarIndex >= 0 then
          begin
            Compiler.RenameVariable(VarIndex, NewVariableName);
          end;
        end;
      end;
    finally
      CompilerList.Free;
    end;

    frmGoPhast.PhastModel.FormulaManager.ResetFormulas;
  end;
end;

function TUndoChangeDataSets.DataSetsChanged: boolean;
var
  Index: integer;
  DataStorage: TPhastDataSetStorage;
begin
  result := (FDeletedDataSets.Count > 0) or (FNewDataSets.Count > 0);
  if not result then
  begin
    for Index := 0 to FNewDataSetProperties.Count -1 do
    begin
      DataStorage := FNewDataSetProperties[Index];
      result := DataStorage.DataSetChanged;
      if result then Exit;
    end;
  end;
end;

{ TPhastDataSetStorage }

procedure TPhastDataSetStorage.Assign(const DataSet: TDataArray);
begin
  FDataSet := DataSet;
  Name := DataSet.Name;
//  Visible := DataSet.Visible;
  Orientation := DataSet.Orientation;
  EvaluatedAt := DataSet.EvaluatedAt;
  DataType := DataSet.DataType;
  Units := DataSet.Units;
  TwoDInterpolator := DataSet.TwoDInterpolator;
  Formula := DataSet.Formula;
  Comment := DataSet.Comment;
  if DataSet is TCustomPhastDataSet then
  begin
    PhastInterpolationValues.Assign(DataSet);
  end;
end;

procedure TPhastDataSetStorage.AssignFormulasToDataSet;
var
  LocalModel: TPhastModel;
  Index: Integer;
  ChildModel: TChildModel;
  ChildDataArray: TDataArray;
begin
  if FDataSet.Model is TPhastModel then
  begin
    LocalModel := TPhastModel(FDataSet.Model);
//    if LocalModel.LgrUsed then
    begin
      for Index := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalModel.ChildModels[Index].ChildModel;
        ChildDataArray := ChildModel.DataArrayManager.
          GetDataSetByName(FDataSet.Name);
        if ChildDataArray <> nil then
        begin
          ChildDataArray.Formula := Formula;
          if ChildDataArray is TCustomPhastDataSet then
          begin
            ChildDataArray.Assign(PhastInterpolationValues);
          end;
        end;
      end;
    end;
  end;
  FDataSet.Formula := Formula;
  if FDataSet is TCustomPhastDataSet then
  begin
    FDataSet.Assign(PhastInterpolationValues);
  end;
//  if FDataSet.Model is TPhastModel then
//  begin
//    LocalModel := TPhastModel(FDataSet.Model);
//    for Index := 0 to LocalModel.ChildModels.Count - 1 do
//    begin
//      ChildModel := LocalModel.ChildModels[Index].ChildModel;
//      ChildModel.CanUpdateGrid := True;
//    end;
//  end;
end;

procedure TPhastDataSetStorage.AssignBasicProperties;
var
  LocalModel: TPhastModel;
  Index: Integer;
  ChildModel: TChildModel;
  ChildDataArray: TDataArray;
begin
  if FDataSet.Model is TPhastModel then
  begin
    LocalModel := TPhastModel(FDataSet.Model);
//    if LocalModel.LgrUsed then
    begin
      for Index := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalModel.ChildModels[Index].ChildModel;
        ChildDataArray := ChildModel.DataArrayManager.
          GetDataSetByName(FDataSet.Name);
//        Assert(ChildDataArray <> nil);
        if ChildDataArray <> nil then
        begin
          ChildDataArray.Name := Name;
          ChildDataArray.UpdateWithoutNotification(Orientation, EvaluatedAt,
            DataType, FNeedToInvalidate);
        end;
  //      ChildDataArray.Orientation := Orientation;
  //      ChildDataArray.EvaluatedAt := EvaluatedAt;
  //      ChildDataArray.DataType := DataType;
      end;
    end;
  end;
  FDataSet.Name := Name;
  FDataSet.UpdateWithoutNotification(Orientation, EvaluatedAt,
    DataType, FNeedToInvalidate);
//  FDataSet.Orientation := Orientation;
//  FDataSet.EvaluatedAt := EvaluatedAt;
//  FDataSet.DataType := DataType;
end;

procedure TPhastDataSetStorage.AssignToDataSet(out ShouldInvalidate: boolean);
var
  LocalModel: TPhastModel;
  Index: Integer;
  ChildModel: TChildModel;
  ChildDataArray: TDataArray;
begin
  if FDataSet.Model is TPhastModel then
  begin
    LocalModel := TPhastModel(FDataSet.Model);
//    if LocalModel.LgrUsed then
    begin
      for Index := 0 to LocalModel.ChildModels.Count - 1 do
      begin
        ChildModel := LocalModel.ChildModels[Index].ChildModel;
        ChildDataArray := ChildModel.DataArrayManager.
          GetDataSetByName(FDataSet.Name);
        if ChildDataArray <> nil then
        begin
          ChildDataArray.Name := Name;
          ChildDataArray.Orientation := Orientation;
          ChildDataArray.EvaluatedAt := EvaluatedAt;
          ChildDataArray.DataType := DataType;
          ChildDataArray.Units := Units;
          ChildDataArray.TwoDInterpolator := TwoDInterpolator;
          ChildDataArray.Comment := Comment;
        end;
      end;
    end;
  end;
  ShouldInvalidate := FNeedToInvalidate;
  if (FDataSet.Name <> Name)
    or (FDataSet.Orientation <> Orientation)
    or (FDataSet.EvaluatedAt <> EvaluatedAt)
    or (FDataSet.DataType <> DataType)
    or (FDataSet.Units <> Units) then
  begin
    ShouldInvalidate := True;
  end
  else if (FDataSet.TwoDInterpolator = nil) <> (TwoDInterpolator = nil) then
  begin
    ShouldInvalidate := True;
  end
  else if (FDataSet.TwoDInterpolator <> nil)
    and (TwoDInterpolator <> nil)
    and not FDataSet.TwoDInterpolator.SameAs(TwoDInterpolator) then
  begin
    ShouldInvalidate := True;
  end;

  FDataSet.Name := Name;
  FDataSet.Orientation := Orientation;
  FDataSet.EvaluatedAt := EvaluatedAt;
  FDataSet.DataType := DataType;
  FDataSet.Units := Units;
  FDataSet.TwoDInterpolator := TwoDInterpolator;
  FDataSet.Comment := Comment;
  FNeedToInvalidate := False;
end;

constructor TPhastDataSetStorage.Create;
begin
  inherited;
  FNeedToInvalidate := False;
  FFormula := frmGoPhast.PhastModel.FormulaManager.Add;
  FTwoDInterpolator := nil;
  FPhastInterpolationValues := TPhastInterpolationValues.Create;
end;

function TPhastDataSetStorage.DataSetChanged: boolean;
var
  TempValues: TPhastInterpolationValues;
begin
  result := (Name <> FDataSet.Name)
    or (DataType <> FDataSet.DataType)
    or (EvaluatedAt <> FDataSet.EvaluatedAt)
    or (Formula <> FDataSet.Formula)
    or (Orientation <> FDataSet.Orientation)
    or (Units <> FDataSet.Units)
    or (Comment <> FDataSet.Comment);
//    or (Visible <> FDataSet.Visible);
  if not result then
  begin
    if TwoDInterpolator = nil then
    begin
      result := FDataSet.TwoDInterpolator <> nil;
    end
    else
    begin
      result := not TwoDInterpolator.SameAs(FDataSet.TwoDInterpolator);
    end;
  end;
  if not result then
  begin
    if FDataSet is TCustomPhastDataSet  then
    begin
      TempValues := TPhastInterpolationValues.Create;
      try
        TempValues.Assign(FDataSet);
        result := not PhastInterpolationValues.SameAs(TempValues);
      finally
        TempValues.Free;
      end;
    end;
  end;
end;

destructor TPhastDataSetStorage.Destroy;
begin
  FPhastInterpolationValues.Free;
  FTwoDInterpolator.Free;
  frmGoPhast.PhastModel.FormulaManager.Remove(FFormula, nil, nil, self);
  inherited;
end;

function TPhastDataSetStorage.GetFormula: string;
begin
  result := FFormula.Formula;
end;

procedure TPhastDataSetStorage.SetDataSet(const Value: TDataArray);
begin
  FDataSet := Value;
  FFormula.Parser := frmGoPhast.PhastModel.GetCompiler(FDataSet.Orientation,
    FDataSet.EvaluatedAt)
end;

procedure TPhastDataSetStorage.SetFormula(const Value: string);
begin
  frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
    FFormula, Value,
    frmGoPhast.PhastModel.GetCompiler(FDataSet.Orientation,
    FDataSet.EvaluatedAt),
    nil, nil, self);
end;

procedure TPhastDataSetStorage.SetPhastInterpolationValues(
  const Value: TPhastInterpolationValues);
begin
  FPhastInterpolationValues.Assign(Value);
end;

procedure TPhastDataSetStorage.SetTwoDInterpolator(
  const Value: TCustom2DInterpolater);
begin
  FTwoDInterpolator.Free;
  if Value = nil then
  begin
    FTwoDInterpolator := nil;
  end
  else
  begin
    FTwoDInterpolator := TInterpolatorType(Value.ClassType).Create(nil);
    FTwoDInterpolator.Assign(Value);
  end;
end;

{ TUndoEditFluxObservations }

constructor TUndoEditFluxObservations.Create;
begin
  FOldChobObservations := TFluxObservationGroups.Create(nil);
  FOldChobObservations.Assign(frmGoPhast.PhastModel.HeadFluxObservations);

  FOldDrobObservations := TFluxObservationGroups.Create(nil);
  FOldDrobObservations.Assign(frmGoPhast.PhastModel.DrainObservations);

  FOldGbobObservations := TFluxObservationGroups.Create(nil);
  FOldGbobObservations.Assign(frmGoPhast.PhastModel.GhbObservations);

  FOldRvobObservations := TFluxObservationGroups.Create(nil);
  FOldRvobObservations.Assign(frmGoPhast.PhastModel.RiverObservations);

  FillMt3dLists(FOldMt3dObs);
end;

function TUndoEditFluxObservations.Description: string;
begin
  result := StrEditFluxObservatio;
end;

destructor TUndoEditFluxObservations.Destroy;
begin
  FOldChobObservations.Free;
  FNewChobObservations.Free;
  FOldDrobObservations.Free;
  FNewDrobObservations.Free;
  FOldGbobObservations.Free;
  FNewGbobObservations.Free;
  FOldRvobObservations.Free;
  FNewRvobObservations.Free;
  FOldMt3dObs.FreeAll;
  FNewMt3dObs.FreeAll;
  inherited;
end;

procedure TUndoEditFluxObservations.DoCommand;
begin
  frmGoPhast.PhastModel.HeadFluxObservations := FNewChobObservations;
  frmGoPhast.PhastModel.DrainObservations := FNewDrobObservations;
  frmGoPhast.PhastModel.GhbObservations := FNewGbobObservations;
  frmGoPhast.PhastModel.RiverObservations := FNewRvobObservations;
  AssignMt3dObsToModel(FNewMt3dObs)
end;

procedure TUndoEditFluxObservations.Undo;
begin
  frmGoPhast.PhastModel.HeadFluxObservations := FOldChobObservations;
  frmGoPhast.PhastModel.DrainObservations := FOldDrobObservations;
  frmGoPhast.PhastModel.GhbObservations := FOldGbobObservations;
  frmGoPhast.PhastModel.RiverObservations := FOldRvobObservations;
  AssignMt3dObsToModel(FOldMt3dObs)
end;

procedure TUndoEditFluxObservations.FillMt3dLists(var Mt3dObs: TMassFluxObs);
begin
  Mt3dObs.CreateAll;
  Mt3dObs.Mt3dmsHeadMassFluxObservations.Assign(frmGoPhast.PhastModel.Mt3dmsHeadMassFluxObservations);
  Mt3dObs.Mt3dmsWellMassFluxObservations.Assign(frmGoPhast.PhastModel.Mt3dmsWellMassFluxObservations);
  Mt3dObs.Mt3dmsDrnMassFluxObservations.Assign(frmGoPhast.PhastModel.Mt3dmsDrnMassFluxObservations);
  Mt3dObs.Mt3dmsRivMassFluxObservations.Assign(frmGoPhast.PhastModel.Mt3dmsRivMassFluxObservations);
  Mt3dObs.Mt3dmsGhbMassFluxObservations.Assign(frmGoPhast.PhastModel.Mt3dmsGhbMassFluxObservations);
  Mt3dObs.Mt3dmsRchMassFluxObservations.Assign(frmGoPhast.PhastModel.Mt3dmsRchMassFluxObservations);
  Mt3dObs.Mt3dmsEvtMassFluxObservations.Assign(frmGoPhast.PhastModel.Mt3dmsEvtMassFluxObservations);
  Mt3dObs.Mt3dmsMassLoadingMassFluxObservations.Assign(frmGoPhast.PhastModel.Mt3dmsMassLoadingMassFluxObservations);
  Mt3dObs.Mt3dmsResMassFluxObservations.Assign(frmGoPhast.PhastModel.Mt3dmsResMassFluxObservations);
  Mt3dObs.Mt3dmsLakMassFluxObservations.Assign(frmGoPhast.PhastModel.Mt3dmsLakMassFluxObservations);
  Mt3dObs.Mt3dmsDrtMassFluxObservations.Assign(frmGoPhast.PhastModel.Mt3dmsDrtMassFluxObservations);
  Mt3dObs.Mt3dmsEtsMassFluxObservations.Assign(frmGoPhast.PhastModel.Mt3dmsEtsMassFluxObservations);
end;

procedure TUndoEditFluxObservations.AssignMt3dObsToModel(MtsdObs: TMassFluxObs);
begin
  frmGoPhast.PhastModel.Mt3dmsHeadMassFluxObservations :=
    MtsdObs.Mt3dmsHeadMassFluxObservations;
  frmGoPhast.PhastModel.Mt3dmsWellMassFluxObservations :=
    MtsdObs.Mt3dmsWellMassFluxObservations;
  frmGoPhast.PhastModel.Mt3dmsDrnMassFluxObservations :=
    MtsdObs.Mt3dmsDrnMassFluxObservations;
  frmGoPhast.PhastModel.Mt3dmsRivMassFluxObservations :=
    MtsdObs.Mt3dmsRivMassFluxObservations;
  frmGoPhast.PhastModel.Mt3dmsGhbMassFluxObservations :=
    MtsdObs.Mt3dmsGhbMassFluxObservations;
  frmGoPhast.PhastModel.Mt3dmsRchMassFluxObservations :=
    MtsdObs.Mt3dmsRchMassFluxObservations;
  frmGoPhast.PhastModel.Mt3dmsEvtMassFluxObservations :=
    MtsdObs.Mt3dmsEvtMassFluxObservations;
  frmGoPhast.PhastModel.Mt3dmsMassLoadingMassFluxObservations :=
    MtsdObs.Mt3dmsMassLoadingMassFluxObservations;
  frmGoPhast.PhastModel.Mt3dmsResMassFluxObservations :=
    MtsdObs.Mt3dmsResMassFluxObservations;
  frmGoPhast.PhastModel.Mt3dmsLakMassFluxObservations :=
    MtsdObs.Mt3dmsLakMassFluxObservations;
  frmGoPhast.PhastModel.Mt3dmsDrtMassFluxObservations :=
    MtsdObs.Mt3dmsDrtMassFluxObservations;
  frmGoPhast.PhastModel.Mt3dmsEtsMassFluxObservations :=
    MtsdObs.Mt3dmsEtsMassFluxObservations;
end;

procedure TUndoEditFluxObservations.AssignNewObservations(NewChobObservations, NewDrobObservations,
  NewGbobObservations, NewRvobObservations: TFluxObservationGroups;
  var NewMtsdObs: TMassFluxObs);
begin
  FNewChobObservations.Free;
  FNewChobObservations := TFluxObservationGroups.Create(nil);
  FNewChobObservations.Assign(NewChobObservations);

  FNewDrobObservations.Free;
  FNewDrobObservations := TFluxObservationGroups.Create(nil);
  FNewDrobObservations.Assign(NewDrobObservations);

  FNewGbobObservations.Free;
  FNewGbobObservations := TFluxObservationGroups.Create(nil);
  FNewGbobObservations.Assign(NewGbobObservations);

  FNewRvobObservations.Free;
  FNewRvobObservations := TFluxObservationGroups.Create(nil);
  FNewRvobObservations.Assign(NewRvobObservations);

  FNewMt3dObs := NewMtsdObs;
  if FNewMt3dObs.Mt3dmsHeadMassFluxObservations = nil then
  begin
    FillMt3dLists(FNewMt3dObs);
  end;
  NewMtsdObs.NilAll;
end;

{ TUndoEditDisplaySettings }

constructor TUndoEditDisplaySettings.Create(
  var NewSettings: TDisplaySettingsCollection);
begin
  FNewSettings := NewSettings;
  NewSettings := nil;
  FOldSettings := TDisplaySettingsCollection.Create(nil);
  FOldSettings.Assign(frmGoPhast.PhastModel.DisplaySettings);
end;

function TUndoEditDisplaySettings.Description: string;
begin
  result := StrChangeImageSetting;
end;

destructor TUndoEditDisplaySettings.Destroy;
begin
  FNewSettings.Free;
  FOldSettings.Free;
  inherited;
end;

procedure TUndoEditDisplaySettings.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.DisplaySettings.Assign(FNewSettings);
end;

procedure TUndoEditDisplaySettings.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.DisplaySettings.Assign(FOldSettings);
end;

{ TMassFluxObs }

procedure TMassFluxObs.CreateAll;
begin
  Mt3dmsHeadMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  Mt3dmsWellMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  Mt3dmsDrnMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  Mt3dmsRivMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  Mt3dmsGhbMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  Mt3dmsRchMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  Mt3dmsEvtMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  Mt3dmsMassLoadingMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  Mt3dmsResMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  Mt3dmsLakMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  Mt3dmsDrtMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);
  Mt3dmsEtsMassFluxObservations := TMt3dmsFluxObservationGroups.Create(nil);

  Mt3dmsHeadMassFluxObservations.FluxObservationType := mfotHead;
  Mt3dmsWellMassFluxObservations.FluxObservationType := mfotWell;
  Mt3dmsDrnMassFluxObservations.FluxObservationType := mfotDrain;
  Mt3dmsRivMassFluxObservations.FluxObservationType := mfotRiver;
  Mt3dmsGhbMassFluxObservations.FluxObservationType := mfotGHB;
  Mt3dmsRchMassFluxObservations.FluxObservationType := mfotRecharge;
  Mt3dmsEvtMassFluxObservations.FluxObservationType := mfotEVT;
  Mt3dmsMassLoadingMassFluxObservations.FluxObservationType := mfotMassLoading;
  Mt3dmsResMassFluxObservations.FluxObservationType := mfotReservoir;
  Mt3dmsLakMassFluxObservations.FluxObservationType := mfotLake;
  Mt3dmsDrtMassFluxObservations.FluxObservationType := mfotDRT;
  Mt3dmsEtsMassFluxObservations.FluxObservationType := mfotETS;
end;

procedure TMassFluxObs.FreeAll;
begin
  Mt3dmsHeadMassFluxObservations.Free;
  Mt3dmsWellMassFluxObservations.Free;
  Mt3dmsDrnMassFluxObservations.Free;
  Mt3dmsRivMassFluxObservations.Free;
  Mt3dmsGhbMassFluxObservations.Free;
  Mt3dmsRchMassFluxObservations.Free;
  Mt3dmsEvtMassFluxObservations.Free;
  Mt3dmsMassLoadingMassFluxObservations.Free;
  Mt3dmsResMassFluxObservations.Free;
  Mt3dmsLakMassFluxObservations.Free;
  Mt3dmsDrtMassFluxObservations.Free;
  Mt3dmsEtsMassFluxObservations.Free;
end;

procedure TMassFluxObs.NilAll;
begin
  Mt3dmsHeadMassFluxObservations := nil;
  Mt3dmsWellMassFluxObservations := nil;
  Mt3dmsDrnMassFluxObservations := nil;
  Mt3dmsRivMassFluxObservations := nil;
  Mt3dmsGhbMassFluxObservations := nil;
  Mt3dmsRchMassFluxObservations := nil;
  Mt3dmsEvtMassFluxObservations := nil;
  Mt3dmsMassLoadingMassFluxObservations := nil;
  Mt3dmsResMassFluxObservations := nil;
  Mt3dmsLakMassFluxObservations := nil;
  Mt3dmsDrtMassFluxObservations := nil;
  Mt3dmsEtsMassFluxObservations := nil;
end;

{ TUndoMoveCrossSection }

constructor TUndoMoveCrossSection.Create(Location: TSegment2D);
begin
  FNewLocation := Location;
  FOldLocation := frmGoPhast.PhastModel.SutraMesh.CrossSection.Segment;
end;

function TUndoMoveCrossSection.Description: string;
begin
  result := StrMoveCrossSection;
end;

procedure TUndoMoveCrossSection.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.SutraMesh.CrossSection.Segment := FNewLocation;
end;

procedure TUndoMoveCrossSection.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.SutraMesh.CrossSection.Segment := FOldLocation;
end;

end.

