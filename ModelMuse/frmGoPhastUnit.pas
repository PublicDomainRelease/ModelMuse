{@abstract(The main purpose of @name is to define @link(TfrmGoPhast)
  and declare @link(frmGoPhast).  The latter is the main form of GoPhast.
  The former is the type of @link(frmGoPhast).)
  @author(Richard B. Winston <rbwinst@usgs.gov>)}
unit frmGoPhastUnit;

interface

uses
  Windows, Messages, SysUtils, Types, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus, ActnList, StdActns, FastGEO,
  ImgList, RbwRuler, ScreenObjectUnit, PhastGridUnit, frmCustomGoPhastUnit,
  RbwDynamicCursor, ZoomBox2, frameViewUnit, Undo, Contnrs, frmSaveArchiveUnit,
  RbwParser, DataSetUnit, GoPhastTypes, SubscriptionUnit, PhastModelUnit,
  arcball, GLWidget, frame3DViewUnit, IniFiles, MostRecentlyUsedFiles,
  ToolWin, frmUndoUnit, ImageDLLLoader, FileUtils,ModflowGridUnit,
  AbstractGridUnit, JvExExtCtrls, JvNetscapeSplitter, frmRunModflowUnit,
  frmRunPhastUnit, ModelMateClassesUnit, SyncObjs, frmRunModpathUnit,
  frmRunZoneBudgetUnit, RbwModelCube, frmRunModelMateUnit;

  { TODO : 
Consider making CurrentTool a property of TframeView instead of 
TfrmGoPhast.  This might allow for simpler tools. }

{ TODO : Allow the user to zoom to show the full grid extents in one step. }

{ TODO : When creating a new object, it should automatically set values
of enclosed cells if it is a polygon and set values of intersected cells
otherwise. }

{ TODO : Add hints to all menu items; they show up on the status bar. }

type
  // @name is used to specify the format of the files that
  // can be opened or saved by GoPhast.
  //
  // ffAscii = an ASCII text file.
  //
  // ffBinary = a binary file.
  //
  // ffXML = an XML file.
  TFileFormat = (ffAscii, ffBinary, ffXML, ffZLib);

  TVersionCompare = (vcUnknown, vcSame, vcExternalOlder, vcExternalNewer);

  // Modified from http://delphi.about.com/od/vclusing/a/menuitemhints.htm
  TMenuItemHint = class(THintWindow)
  private
    activeMenuItem : TMenuItem;
    showTimer : TTimer;
    hideTimer : TTimer;
    procedure HideTime(Sender : TObject);
    procedure ShowTime(Sender : TObject);
  public
    constructor Create(AOwner : TComponent); override;
    procedure DoActivateHint(menuItem : TMenuItem);
    destructor Destroy; override;
  end;

  TFramePosition = record
    XCenter: double;
    YCenter: double;
    Magnification: double;
  end;

  TPositionStorage = class(TObject)
    Top: TFramePosition;
    Front: TFramePosition;
    Side: TFramePosition;
  end;

  TOnNewPositionEvent = procedure (Sender: TObject;
    NewPosition: TPositionStorage) of object;

  TPositionList = class(TObject)
  private
    FList: TList;
    FCurrentPosition: integer;
    FMaxPositions: integer;
    FOnNewPosition: TOnNewPositionEvent;
    function GetCanRedo: boolean;
    function GetCanUndo: boolean;
  public
    Constructor Create(MaxPositions: integer);
    Destructor Destroy; override;
    procedure Submit(NewPosition: TPositionStorage);
    procedure Undo;
    procedure Redo;
    property OnNewPosition: TOnNewPositionEvent read FOnNewPosition
      write FOnNewPosition;
    property CanUndo: boolean read GetCanUndo;
    property CanRedo: boolean read GetCanRedo;
    procedure Clear;
  end;

  {
    @abstract(@name is the type of @link(frmGoPhast) which is
     the main form of GoPhast.)
    @name has tool bars and a menu
    to control the operation of GoPhast.  The main part of the form is
    occupied by three @link(TframeView)s which display top, front, and side
    views of the model.  Most of the user interaction with the spatial data
    in the model is through the three @link(TframeView)s.  A status bar
    @link(sbMain) on @name displays messages to the user.

    See @link(frmGoPhast).
  }
  TfrmGoPhast = class(TUndoForm)
    cbControlBar: TControlBar;
    tbarEdit: TToolBar;
    tbCut: TToolButton;
    tbCopy: TToolButton;
    tbPaste: TToolButton;
    tbUndo: TToolButton;
    tbRedo: TToolButton;
    tbarFile: TToolBar;
    tbSave: TToolButton;
    tbOpen: TToolButton;
    tbPrint: TToolButton;
    tbNew: TToolButton;
    tbarEditScreenObjects: TToolBar;
    tbSeparator1: TToolButton;
    tbInsertPoint: TToolButton;
    tbDeleteSegment: TToolButton;
    tbLasso: TToolButton;
    tbSelectPoint: TToolButton;
    tbSelect: TToolButton;
    tbSeparator2: TToolButton;
    tbShowHideObjects: TToolButton;
    tbarView: TToolBar;
    tbZoom: TToolButton;
    tbZoomOut: TToolButton;
    tbZoomIn: TToolButton;
    tbPan: TToolButton;
    tbarEditGrid: TToolBar;
    tbDeleteColumnRow: TToolButton;
    tbMove: TToolButton;
    tbAddVerticalBoundary: TToolButton;
    tbAddHorizontalBoundary: TToolButton;
    tbSubdivide: TToolButton;
    tbGridAngle: TToolButton;
    tbSpacing: TToolButton;
    tbGenerateGrid: TToolButton;
    tbarCreateScreenObject: TToolBar;
    tbStraightLine: TToolButton;
    tbLine: TToolButton;
    tbPolygon: TToolButton;
    tbRectangle: TToolButton;
    tbPoint: TToolButton;
    tbarView3D: TToolBar;
    tbShell: TToolButton;
    tbTopGrid: TToolButton;
    tbFrontGrid: TToolButton;
    tbSideGrid: TToolButton;
    tb3DColors: TToolButton;
    tb3DObjects: TToolButton;
    MostRecentlyUsed: TvRbwMostRecentlyUsed;
    Model1: TMenuItem;
    miModflow: TMenuItem;
    miPhast: TMenuItem;
    acModflowActive: TAction;
    acPhastActive: TAction;
    miNewModflowModel: TMenuItem;
    miNewPHASTModel: TMenuItem;
    acFileNewPhastModel: TAction;
    miLayers: TMenuItem;
    acLayers: TAction;
    miGeneral: TMenuItem;
    tbSelectColRowLayer: TToolButton;
    acSelectColRowLay: TAction;
    miSelectColRowLayer: TMenuItem;
    miTime: TMenuItem;
    miOutputControl: TMenuItem;
    sdModflowInput: TSaveDialog;
    miExport: TMenuItem;
    miExportModflow: TMenuItem;
    WarningsandErrors1: TMenuItem;
    miInvertSelection: TMenuItem;
    miSelectAllTop: TMenuItem;
    acSelectAllFront: TAction;
    acSelectAllSide: TAction;
    miSelectAllFront: TMenuItem;
    miSelectAllSide: TMenuItem;
    ShowGridValues1: TMenuItem;
    miPackages: TMenuItem;
    miProgramLocations: TMenuItem;
    N7: TMenuItem;
    EditGlobalVariables1: TMenuItem;
    AddPartstoObject1: TMenuItem;
    tbAddPartsToObject: TToolButton;
    acAddPolygonsToObject: TAction;
    tbSeparator3: TToolButton;
    tbAddLinesToObjects: TToolButton;
    acAddLinesToObject: TAction;
    AddLinesToObject1: TMenuItem;
    tbAddPointsToObject: TToolButton;
    acAddPointsToObject: TAction;
    AddPointstoObject1: TMenuItem;
    ShallAllObjects1: TMenuItem;
    HideAllObjects1: TMenuItem;
    tbColorGrid: TToolButton;
    acColorGrid: TAction;
    N8: TMenuItem;
    FilesToArchive1: TMenuItem;
    ModflowReference1: TMenuItem;
    Index1: TMenuItem;
    miLinkSFRStreams: TMenuItem;
    IntroductoryVideo1: TMenuItem;
    comboZCount: TComboBox;
    ExportShapefile1: TMenuItem;
    miModflow2005Model: TMenuItem;
    ilImageList: TImageList;
    miModelResults: TMenuItem;
    miScaleRotateorMoveObjects: TMenuItem;
    miMergeObjects: TMenuItem;
    tbShow2DGrid: TToolButton;
    miShow2DGridlines: TMenuItem;
    // @name separates @link(frameSideView) from @link(frameTopView)
    // and can be used to resize them.  See @link(splitVertTopMoved).
    splitVertTop: TJvNetscapeSplitter;
    // @name separates @link(frame3DView) from @link(frameFrontView)
    // and can be used to resize them.  See @link(splitVertBottomMoved).
    splitVertBottom: TJvNetscapeSplitter;
    // @name separates @link(pnlBottom) from @link(pnlTop) and can be used
    // to resize them.  See @link(splitHorizMoved).
    splitHoriz: TJvNetscapeSplitter;
    miModflowNameFile: TMenuItem;
    miReverseSelectedObjects: TMenuItem;
    tbRun: TToolButton;
    acRunModflow: TAction;
    RestoreDefault2DView1: TMenuItem;
    tbRestoreDefault2DView: TToolButton;
    tbPositionUndo: TToolButton;
    tbPositionRedo: TToolButton;
    acPositionForward: TAction;
    acPositionBackward: TAction;
    miUndoPosition: TMenuItem;
    RedoPosition: TMenuItem;
    GriddedData1: TMenuItem;
    miExportModpath: TMenuItem;
    acExportModpath: TAction;
    sdModpathInput: TSaveDialog;
    miManageFluxObservations: TMenuItem;
    Create1: TMenuItem;
    Edit1: TMenuItem;
    Navigation1: TMenuItem;
    miAllVideos: TMenuItem;
    miShowVideoTips: TMenuItem;
    N2: TMenuItem;
    N9: TMenuItem;
    acContourData: TAction;
    ContourData1: TMenuItem;
    tbContourData: TToolButton;
    acExportModelMate: TAction;
    ModelMateFile1: TMenuItem;
    acImportModelMate: TAction;
    odModelMate: TOpenDialog;
    miImportModelMate: TMenuItem;
    miObservationType: TMenuItem;
    miObservations: TMenuItem;
    miPredictions: TMenuItem;
    acEditSelecteObjects: TAction;
    miMF_HydrogeologicUnits: TMenuItem;
    miBatchFileAdditions: TMenuItem;
    miSelectObjectsforEditing: TMenuItem;
    miDisplayDataSetValues: TMenuItem;
    miObjectstoShapefile: TMenuItem;
    miDeleteImage: TMenuItem;
    miModpathPathline: TMenuItem;
    miPHASTProgramLocation: TMenuItem;
    miConfigurePathlines: TMenuItem;
    miModpathEndpoints: TMenuItem;
    miConfigureEndpoints: TMenuItem;
    miModpathTimeSeries: TMenuItem;
    miConfigureTimeSeries: TMenuItem;
    SurferGridFile1: TMenuItem;
    SampleDEMData1: TMenuItem;
    ZONEBUDGETInputFiles1: TMenuItem;
    sdZonebudgetInput: TSaveDialog;
    tbVertexValue: TToolButton;
    acVertexValue: TAction;
    EditVertexValues1: TMenuItem;
    sdModelMate: TSaveDialog;
    menuGridLineChoice: TPopupMenu;
    Showall1: TMenuItem;
    Showexterior1: TMenuItem;
    Showactive1: TMenuItem;
    ShowAll2: TMenuItem;
    Showexterior2: TMenuItem;
    Showactive2: TMenuItem;
    acShowAllGridLines: TAction;
    acShowExteriorGridLines: TAction;
    acShowActiveGridLines: TAction;
    Showactiveedge1: TMenuItem;
    acShowActiveEdge: TAction;
    Showactiveedge2: TMenuItem;
    acRestoreDefault2DView: TAction;
    acExportImage: TAction;
    ExportImage1: TMenuItem;
    miManageParameters: TMenuItem;
    miManageHeadObservations: TMenuItem;
    miContourstoShapefile: TMenuItem;
    sdShapefile: TSaveDialog;
    miShapefile: TMenuItem;
    miPathlinestoShapefile: TMenuItem;
    miEndpointsatStartingLocationstoShapefile: TMenuItem;
    miEndpointsatEndingLocationstoShapefile: TMenuItem;
    miTimeSeriestoShapefile: TMenuItem;
    miDataSetstoCSV: TMenuItem;
    tbShowGridValues: TToolButton;
    acShowGridValues: TAction;
    acModflowLgrActive: TAction;
    miModflowLgr: TMenuItem;
    miChildModels: TMenuItem;
    miASCII_RasterFile: TMenuItem;
    tbImportModelResults: TToolButton;
    acImportModelResults: TAction;
    acRunModflowLgr: TAction;
    sdModflowLgr: TSaveDialog;
    miExportModflowLgr: TMenuItem;
    miInvertSelectedVertices: TMenuItem;
    miSplitSelectedObjects: TMenuItem;
    miMakeSelectedVerticesASeparateObject: TMenuItem;
    miSplitObjectAtSelectedVertices: TMenuItem;
    procedure tbUndoClick(Sender: TObject);
    procedure acUndoExecute(Sender: TObject);
    procedure tbRedoClick(Sender: TObject);
    procedure acPhastActiveExecute(Sender: TObject);
    procedure acModflowActiveExecute(Sender: TObject);
    procedure acLayersExecute(Sender: TObject);
    procedure miGeneralClick(Sender: TObject);
    procedure acSelectColRowLayExecute(Sender: TObject);
    procedure miTimeClick(Sender: TObject);
    procedure miOutputControlClick(Sender: TObject);
    procedure miExportModflowClick(Sender: TObject);
    procedure WarningsandErrors1Click(Sender: TObject);
    procedure miInvertSelectionClick(Sender: TObject);
    procedure acSelectAllTopExecute(Sender: TObject);
    procedure acSelectAllFrontExecute(Sender: TObject);
    procedure acSelectAllSideExecute(Sender: TObject);
    procedure acShowGridValuesClick(Sender: TObject);
    procedure miPackagesClick(Sender: TObject);
    procedure miProgramLocationsClick(Sender: TObject);
    procedure EditGlobalVariables1Click(Sender: TObject);
    procedure acAddPolygonsToObjectExecute(Sender: TObject);
    procedure acAddLinesToObjectExecute(Sender: TObject);
    procedure acAddPointsToObjectExecute(Sender: TObject);
    procedure ShallAllObjects1Click(Sender: TObject);
    procedure HideAllObjects1Click(Sender: TObject);
    // @name allows the user to color the grid
    // with the values of a @link(TDataArray)
    // in GoPhast via @link(TfrmGridColor).
    procedure acColorGridExecute(Sender: TObject);
    procedure FilesToArchive1Click(Sender: TObject);
    procedure sdSaveDialogClose(Sender: TObject);
    procedure sdSaveDialogShow(Sender: TObject);
    procedure ModflowReference1Click(Sender: TObject);
    procedure Index1Click(Sender: TObject);
    procedure miLinkSFRStreamsClick(Sender: TObject);
    procedure IntroductoryVideo1Click(Sender: TObject);
    procedure ExportShapefile1Click(Sender: TObject);
    procedure sdSaveDialogTypeChange(Sender: TObject);
    procedure miModelResultsClick(Sender: TObject);
    procedure miScaleRotateorMoveObjectsClick(Sender: TObject);
    procedure miMergeObjectsClick(Sender: TObject);
    procedure miModflowNameFileClick(Sender: TObject);
    procedure miReverseSelectedObjectsClick(Sender: TObject);
    procedure RestoreDefault2DView1Click(Sender: TObject);
    procedure sdModflowInputShow(Sender: TObject);
    procedure sdModflowInputClose(Sender: TObject);
    procedure sdPhastInputShow(Sender: TObject);
    procedure sdPhastInputClose(Sender: TObject);
    procedure acPositionForwardExecute(Sender: TObject);
    procedure acPositionBackwardExecute(Sender: TObject);
    procedure GriddedData1Click(Sender: TObject);
    procedure sdModpathInputShow(Sender: TObject);
    procedure sdModpathInputClose(Sender: TObject);
    procedure acExportModpathExecute(Sender: TObject);
    procedure miFileClick(Sender: TObject);
    procedure miManageFluxObservationsClick(Sender: TObject);
    procedure miAllVideosClick(Sender: TObject);
    procedure miShowVideoTipsClick(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acCutExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure acContourDataExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure acExportModelMateExecute(Sender: TObject);
    procedure acImportModelMateExecute(Sender: TObject);
    procedure miObservationsClick(Sender: TObject);
    procedure miMF_HydrogeologicUnitsClick(Sender: TObject);
    procedure miBatchFileAdditionsClick(Sender: TObject);
    procedure miSelectObjectsforEditingClick(Sender: TObject);
    procedure miDisplayDataSetValuesClick(Sender: TObject);
    procedure miObjectstoShapefileClick(Sender: TObject);
    procedure miDeleteImageClick(Sender: TObject);
    procedure miModpathPathlineClick(Sender: TObject);
    procedure miPHASTProgramLocationClick(Sender: TObject);
    procedure miModpathEndpointsClick(Sender: TObject);
    procedure miModpathTimeSeriesClick(Sender: TObject);
    procedure SurferGridFile1Click(Sender: TObject);
    procedure sbMainDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure SampleDEMData1Click(Sender: TObject);
    procedure ZONEBUDGETInputFiles1Click(Sender: TObject);
    procedure sdZonebudgetInputShow(Sender: TObject);
    procedure sdZonebudgetInputClose(Sender: TObject);
    procedure acVertexValueExecute(Sender: TObject);
    procedure acExportImageExecute(Sender: TObject);
    procedure miManageParametersClick(Sender: TObject);
    procedure miManageHeadObservationsClick(Sender: TObject);
    procedure sdModelMateShow(Sender: TObject);
    procedure sdModelMateClose(Sender: TObject);
    procedure miContourstoShapefileClick(Sender: TObject);
    procedure miPathlinestoShapefileClick(Sender: TObject);
    procedure miEndpointsatStartingLocationstoShapefileClick(Sender: TObject);
    procedure miEndpointsatEndingLocationstoShapefileClick(Sender: TObject);
    procedure miTimeSeriestoShapefileClick(Sender: TObject);
    procedure miDataSetstoCSVClick(Sender: TObject);
    procedure acModflowLgrActiveExecute(Sender: TObject);
    procedure miChildModelsClick(Sender: TObject);
    procedure miASCII_RasterFileClick(Sender: TObject);
    procedure acRunModflowLgrExecute(Sender: TObject);
    procedure miInvertSelectedVerticesClick(Sender: TObject);
    procedure miSplitSelectedObjectsClick(Sender: TObject);
    procedure miMakeSelectedVerticesASeparateObjectClick(Sender: TObject);
    procedure miSplitObjectAtSelectedVerticesClick(Sender: TObject);
  private
    FCreateArchive: Boolean;
    CreateArchiveSet: boolean;
    FCreatingMainForm: Boolean;
    miHint : TMenuItemHint;
    FSaveModelForm: TfrmSaveArchive;
    FOtherSplitterMoving: Boolean;
    FSynchronizeCount: integer;
    FRunModflowForm: TfrmRunModflow;
    FRunModpathForm: TfrmRunModpath;
    FRunZoneBudgetForm: TfrmRunZoneBudget;
    FRunModflow: Boolean;
    FRunModpath: Boolean;
    FCreateNewCompositeBudgetFile: boolean;
    FRunZoneBudget: Boolean;
    FRunPhastForm: TfrmRunPhast;
    FRunPhast: boolean;
    FPositionList: TPositionList;
    FBrowser: string;
//    FShowUcodeInterface: Boolean;
    FStartTime: TDateTime;
    FFileStream: TFileStream;
    FFileSize: Int64;
    FRunModelMateForm: TfrmRunModelMate;
    FRunModelMate: boolean;
    FObservationFileName: string;
    FPredictionFileName: string;
    FReadingFile: Boolean;
    FSizeWarningDisplayed: Boolean;
    FRunModelSelection: Integer;
    procedure SetCreateArchive(const Value: Boolean);
    property CreateArchive: Boolean read FCreateArchive write SetCreateArchive;
    procedure WMMenuSelect(var Msg: TWMMenuSelect); message WM_MENUSELECT;
    // @name deletes the last point the the current @link(TScreenObject).
    // If there is no current @link(TScreenObject), it deletes the selected
    // @link(TScreenObject)(s).
    procedure DeleteLastPointInScreenObject;
    // @name deletes the selected @link(TScreenObject)(s).
    procedure DeleteSelectedNodesOrSelectedScreenObjects;
    procedure CreatePhastModel;
    procedure UpdateDisplay(Sender: TObject);
    procedure UpdatePermanantDialogBoxAppearances;
    procedure ShowOrHideAllScreenObjects(ShowAll: Boolean);
    procedure PlayIntroductoryVideo;
    procedure StoreInitalPosition;
    procedure ReadModelMateProject(FileName: string;
      ModelMateProject: TProject);
    procedure InitializeModflowInputDialog;
//    procedure SetVisibilityOfModelMateActions;
    procedure EnableDeleteImage;
    procedure CancelCurrentScreenObject;
    procedure InternalSaveFile(FileName: string);
  published
    // @name is the TAction for @link(miAddVerticalGridLine)
    // and @link(tbAddVerticalBoundary).
    // See @link(tbAddVerticalBoundaryClick).
    acAddColumn: TAction;
    // @name is the TAction for @link(miAddHorizontalGridLine)
    // and @link(tbAddHorizontalBoundary).
    // See @link(tbAddHorizontalBoundaryClick).
    acAddRow: TAction;
    // @name is the TAction for @link(miColor) and @link(tb3DColors).
    // See @link(acColorExecute).
    acColor: TAction;
    // @name is the TAction for @link(miShowColoredGrid) and @link(tb3DColors).
    // See @link(tb3DColorsClick).
    acColoredGrid: TAction;
    // @name is the TAction for @link(miCopy) and @link(tbCopy).
    // @name is not yet used.
    acCopy: TAction;
    // @name is the TAction for @link(miCreateLine) and @link(tbLine).
    // See @link(tbLineClick).
    acCreateLine: TAction;
    // @name is the TAction for @link(miCreatePoint) and @link(tbPoint).
    // See @link(tbPointClick).
    acCreatePoint: TAction;
    // @name is the TAction for @link(miCreatePolygon) and @link(tbPolygon).
    // See @link(tbPolygonClick).
    acCreatePolygon: TAction;
    // @name is the TAction for @link(miCreateRectangle)
    // and @link(tbRectangle).
    // See @link(tbRectangleClick).
    acCreateRectangle: TAction;
    // @name is the TAction for @link(miCreateStraightLine)
    // and @link(tbStraightLine).
    // See @link(tbStraightLineClick).
    acCreateStraightLine: TAction;
    // @name is the TAction for @link(miCut) and @link(tbCut).
    // @name is not yet used.
    acCut: TAction;
    // @name is the TAction for @link(miDeleteGridLine)
    // and @link(tbDeleteColumnRow).
    // See @link(acDeleteColumnRowExecute).
    acDeleteColumnRow: TAction;
    // @name is the TAction for @link(miDeleteSegment)
    // and @link(tbDeleteSegment).
    // See @link(tbDeleteSegmentClick).
    acDeleteSegment: TAction;
    // @name is the TAction for @link(miEditDataSet).
    // See @link(acEditDataSetsExecute).
    acEditDataSets: TAction;
    // @name is the TAction for @link(miEditGridLines).
    // See @link(acEditGridLinesExecute).
    acEditGridLines: TAction;
    // @name is the TAction for @link(miVerticalExaggeration).
    // See @link(miVerticalExaggerationClick).
    acEditVerticalExaggeration: TAction;
    // @name is the TAction for @link(miExit).
    // See @link(acExitExecute).
    acExit: TAction;
    // @name is the TAction for @link(miExportPhast).
    // See @link(acExportPhastInputFileExecute).
    acExportPhastInputFile: TAction;
    acFileNewModflowModel: TAction;
    // @name is the TAction for @link(miOpen) and @link(tbOpen).
    // See @link(acFileOpenExecute).
    acFileOpen: TAction;
    // @name is the TAction for @link(miPrint) and @link(tbPrint).
    // @name is not yet used.
    acFilePrint: TAction;
    // @name is the TAction for @link(miSave) and @link(tbSave).
    // See @link(acFileSaveExecute).
    acFileSave: TAction;
    // @name is the TAction for @link(miSaveAs).
    // See @link(acFileSaveAsExecute).
    acFileSaveAs: TAction;
    // @name is the TAction for @link(miFont).
    // See @link(acFontExecute).
    acFont: TAction;
    // @name is the TAction for @link(miGenerateGrid) and @link(tbGenerateGrid).
    // See @link(acGenerateGridExecute).
    acGenerateGrid: TAction;
    // @name is the TAction for @link(miGridAngle).
    // See @link(acGridAngleExecute).
    acGridAngle: TAction;
    // @name is the TAction for @link(miDragtoRotate) and @link(tbGridAngle).
    // See @link(tbGridAngleClick).
    acGridDragRotate: TAction;
    // @name is the TAction for @link(miContents).
    // See @link(acHelpContentsExecute).
    acHelpContents: THelpContents;
    // @name is the TAction for @link(miInsertNode) and @link(tbInsertPoint).
    // See @link(tbInsertPointClick).
    acInsertNode: TAction;
    // @name is the TAction for @link(miMoveColumnOrRow) and @link(tbMove).
    // See @link(acMoveColumnOrRowExecute).
    acMoveColumnOrRow: TAction;
    // @name is the TAction for @link(miGoTo).
    // See @link(acMoveToExecute).
    acMoveTo: TAction;
    // @name is the TAction for @link(miPan) and @link(tbPan).
    // See @link(tbPanClick).
    acPan: TAction;
    // @name is the TAction for @link(miPaste) and @link(tbPaste).
    // @name is not used yet.
    acPaste: TAction;
    // @name is the TAction for @link(miRestoreDefaultView).
    // See @link(acRestoreDefaultViewExecute).
    acRestoreDefaultView: TAction;
    acSelectAllTop: TAction;
    // @name is the TAction for @link(miSelectNodes) and @link(tbSelectPoint).
    // See @link(tbSelectPointClick).
    acSelectNode: TAction;
    // @name is the TAction for @link(miSelectObjects) and @link(tbSelect).
    // See @link(tbSelectClick).
    acSelectObjects: TAction;
    // @name is the TAction for @link(miSelectWithLasso) and @link(tbLasso).
    // See @link(tbLassoClick).
    acSelectWithLasso: TAction;
    // @name is the TAction for @link(miSetSpacing) and @link(tbSpacing).
    // See @link(acSetSpacingExecute).
    acSetSpacing: TAction;
    // @name is the TAction for @link(miShow3DObjects) and @link(tb3DObjects).
    // See @link(tb3DObjectsClick).
    acShow3DObjects: TAction;
    // @name is the TAction for @link(miShowFrontGrid) and @link(tbFrontGrid).
    // See @link(acShowFrontGridExecute).
    acShowFrontGrid: TAction;
    // @name is the TAction for @link(miShowGridShell) and @link(tbShell).
    // See @link(acShowGridShellExecute).
    acShowGridShell: TAction;
    // @name is the action for showing or hiding @link(frmShowHideObjects).
    // See @link(miShowHideObjectsClick).
    acShowHideObjects: TAction;
    // @name is the TAction for @link(miShowSideGrid) and @link(tbSideGrid).
    // See @link(acShowSideGridExecute).
    acShowSideGrid: TAction;
    // @name is the TAction for @link(miShowTopGrid) and @link(tbTopGrid).
    // See @link(acShowTopGridExecute).
    acShowTopGrid: TAction;
    // @name is the TAction for @link(miSmoothGrid).
    // See @link(acSmoothGridExecute).
    acSmoothGrid: TAction;
    // @name is the TAction for @link(miSubdivide) and @link(tbSubdivide).
    // See @link(acSubdivideExecute).
    acSubdivide: TAction;
    // @name is the TAction for @link(miZoom) and @link(tbZoom).
    // See @link(tbZoomClick).
    acZoom: TAction;
    // @name is the TAction for @link(miZoomIn) and @link(tbZoomIn).
    // See @link(miZoomInClick).
    acZoomIn: TAction;
    // @name is the TAction for @link(miZoomOut) and @link(tbZoomOut).
    // See @link(tbZoomOutClick).
    acZoomOut: TAction;
    // @name holds the instances of TAction in GoPhast.
    alActionList: TActionList;
    // @name  is used to pick the background color for GoPhast
    // in @link(acColorExecute).
    cdColorDialog: TColorDialog;
    // @name is used to draw a rotated version of the AddColumn cursor
    // in the top view of the model.
    dcAddColCursor: TRbwDynamicCursor;
    // @name is used to draw a rotated version of the AddRow cursor
    // in the top view of the model.
    dcAddRowCursor: TRbwDynamicCursor;
    // @name is used to draw a rotated version of the MoveColumn cursor
    // in the top view of the model.
    dcMoveColCursor: TRbwDynamicCursor;
    // @name is used to draw a rotated version of the MoveRow cursor
    // in the top view of the model.
    dcMoveRowCursor: TRbwDynamicCursor;
    // @name is used to draw a rotated version of the SetCellSpacing cursor
    // in the top view of the model.
    dcSetSpacing: TRbwDynamicCursor;
    // @name is used to draw a rotated version of the Subdivide cursor
    // in the top view of the model.
    dcSubdivide: TRbwDynamicCursor;
    // @name  is used to pick the font for GoPhast
    // in @link(acFontExecute).
    fdFontDialog: TFontDialog;
    // @name displays a 3D view of the model and handles interaction
    // with the 3D view.
    frame3DView: Tframe3DView;
    // @name displays a front view of the model and handles interaction
    // with the front view.
    frameFrontView: TframeView;
    // @name displays a side view of the model and handles interaction
    // with the side view.
    frameSideView: TframeView;
    // @name displays a top view of the model and handles interaction
    // with the top view.
    frameTopView: TframeView;
    ilDisabledImageList: TImageList;
    // See @link(mi3D_ColorsClick).
    mi3D_Colors: TMenuItem;
    // See @link(miAboutClick).
    miAbout: TMenuItem;
    // See @link(tbAddHorizontalBoundaryClick) and @link(acAddRow).
    miAddHorizontalGridLine: TMenuItem;
    // See @link(tbAddVerticalBoundaryClick) and @link(acAddColumn).
    miAddVerticalGridLine: TMenuItem;
    // See @link(miChemistryOptionsClick).
    miChemistryOptions: TMenuItem;
    // See @link(acColorExecute) and @link(acColor).
    miColor: TMenuItem;
    // See @link(acColorGridExecute).
    miColorGrid: TMenuItem;
    // See @link(acHelpContentsExecute) and @link(acHelpContents).
    miContents: TMenuItem;
    // See @link(acCopy).
    miCopy: TMenuItem;
    // See @link(tbLineClick) and @link(acCreateLine).
    miCreateLine: TMenuItem;
    // See @link(tbPointClick) and @link(acCreatePoint).
    miCreatePoint: TMenuItem;
    // See @link(tbPolygonClick) and @link(acCreatePolygon).
    miCreatePolygon: TMenuItem;
    // See @link(tbRectangleClick) and @link(acCreateRectangle).
    miCreateRectangle: TMenuItem;
    // See @link(tbStraightLineClick) and @link(acCreateStraightLine).
    miCreateStraightLine: TMenuItem;
    // @name holds the @link(miFont), @link(miColor), @link(miHintDelay),
    // and @link(miRulerFormat) menu items.
    miCustomize: TMenuItem;
    // See @link(acCut).
    miCut: TMenuItem;
    // @name holds the @link(miEditDataSet), @link(miColorGrid),
    // and @link(miShowFormulaErrors) menu items.
    miData: TMenuItem;
    // See @link(acDeleteColumnRowExecute) and @link(acDeleteColumnRow).
    miDeleteGridLine: TMenuItem;
    // See @link(tbDeleteSegmentClick) and @link(acDeleteSegment).
    miDeleteSegment: TMenuItem;
    // See @link(tbGridAngleClick) and @link(acGridDragRotate).
    miDragtoRotate: TMenuItem;
    // @name holds the @link(miUndo), @link(miRedo), @link(miCut),
    // @link(miCopy), @link(miPaste), @link(miSelectAll), @link(miEditBitmaps),
    // and @link(miShowHideBitmaps) menu items.
    miEdit: TMenuItem;
    // See @link(miEditBitmapsClick).
    miEditBitmaps: TMenuItem;
    // See @link(acEditDataSetsExecute) and @link(acEditDataSets).
    miEditDataSet: TMenuItem;
    // See @link(acEditGridLinesExecute) and @link(acEditGridLines).
    miEditGridLines: TMenuItem;
    // This TMenuItem [Object|Edit Selected Object(s)], allows the user to
    // edit the properties of the selected objects by displaying the
    // @link(TfrmScreenObjectProperties) dialog box.
    // @seealso(miEditSelectedObjectsClick)
    miEditSelectedObjects: TMenuItem;
    // The OnClick event for @name, @link(miExamplesClick), opens the default
    // web browser with the web page for the instructions for
    // recreating the examples.
    miExamples: TMenuItem;
    // See @link(acExitExecute) and @link(acExit).
    miExit: TMenuItem;
    // See @link(acExportPhastInputFileExecute)
    // and @link(acExportPhastInputFile).
    miExportPhast: TMenuItem;
    // @name holds the @link(miNew), @link(miOpen), @link(miSave),
    // @link(miSaveAs), @link(miImport), @link(miExport), @link(miPrint),
    // and @link(miExit) menu items.
    miFile: TMenuItem;
    // See @link(acFontExecute) and @link(acFont).
    miFont: TMenuItem;
    // See @link(miFreeSurfaceClick).
    miFreeSurface: TMenuItem;
    // See @link(acGenerateGridExecute) and @link(acGenerateGrid).
    miGenerateGrid: TMenuItem;
    // See @link(acMoveToExecute) and @link(acMoveTo).
    miGoTo: TMenuItem;
    // @name holds the
    // @link(miDeleteGridLine),
    // @link(miMoveColumnOrRow),
    // @link(miAddVerticalGridLine),
    // @link(miAddHorizontalGridLine),
    // @link(miSubdivide),
    // @link(miSetSpacing),
    // @link(miDragtoRotate),
    // @link(miGridAngle),
    // @link(miGenerateGrid),
    // @link(miEditGridLines),
    // @link(miSmoothGrid), and
    // @link(miSetSelectedColRowLayer) menu items.
    miGrid: TMenuItem;
    // See @link(acGridAngleExecute) and @link(acGridAngle).
    miGridAngle: TMenuItem;
    // See @link(miGridOptionsClick).
    miGridOptions: TMenuItem;
    // @name holds the
    // @link(miContents),
    // @link(miHelpOnMainWindow),
    // @link(miExamples), and
    // @link(miAbout) menu items.
    miHelp: TMenuItem;
    // @name provides help on the main window of GoPhast.
    miHelpOnMainWindow: TMenuItem;
    // See @link(miHintDelayClick).
    miHintDelay: TMenuItem;
    // @name holds the
    // @link(miImportDistributedDatabyZone),
    // @link(miImportShapefile),
    // @link(miImportDXFFile), and
    // @link(miImportBitmap) menu items.
    // @name is under the @link(miFile) menu item.
    miImport: TMenuItem;
    // See @link(miImportBitmapClick).
    miImportBitmap: TMenuItem;
    // See @link(miImportDistributedDatabyZoneClick).
    miImportDistributedDatabyZone: TMenuItem;
    // See @link(miImportDXFFileClick).
    miImportDXFFile: TMenuItem;
    // See @link(miImportPointsClick).
    miImportPoints: TMenuItem;
    // See @link(miImportShapefileClick).
    miImportShapefile: TMenuItem;
    // See @link(tbInsertPointClick) and @link(acInsertNode).
    miInsertNode: TMenuItem;
    // See @link(acMoveColumnOrRowExecute) and @link(acMoveColumnOrRow).
    miMoveColumnOrRow: TMenuItem;
    // @name holds @link(miNewModflowModel) and @link(miNewPHASTModel).
    miNew: TMenuItem;
    // @name holds the
    // @link(miSelectObjects),
    // @link(miSelectNodes),
    // @link(miSelectWithLasso),
    // @link(N4),
    // @link(miCreatePoint),
    // @link(miCreateLine),
    // @link(miCreatePolygon),
    // @link(miCreateStraightLine),
    // @link(miCreateRectangle),
    // @link(miInsertNode),
    // @link(miDeleteSegment),
    // @link(miRearrangeObjects),
    // @link(N1),
    // @link(miSearchForObject),
    // @link(miShowSelectedObjects),
    // @link(miShowHideObjects),
    // @link(miSelectObjectsByName), and
    // @link(mi3D_Colors) menu items.
    miObject: TMenuItem;
    // See @link(acFileOpenExecute) and @link(acFileOpen).
    miOpen: TMenuItem;
    // See @link(tbPanClick) and @link(acPan).
    miPan: TMenuItem;
    // See and @link(acPaste).
    miPaste: TMenuItem;
    // See @link(acFilePrint).
    miPrint: TMenuItem;
    // See @link(miPrintFrequencyClick).
    miPrintFrequency: TMenuItem;
    // See @link(miPrintInitialClick).
    miPrintInitial: TMenuItem;
    // See @link(miRearrangeObjectsClick).
    miRearrangeObjects: TMenuItem;
    // @name is used to redo the users actions.
    // Its OnClick event handler is assigned
    // at runtime in @link(TUndoStack.SetUndoMenuItems).
    // @SeeAlso(tbRedo).
    miRedo: TMenuItem;
    // See @link(acRestoreDefaultViewExecute) and @link(acRestoreDefaultView).
    miRestoreDefaultView: TMenuItem;
    // See @link(miRulerFormatClick).
    miRulerFormat: TMenuItem;
    // See @link(acFileSaveExecute) and @link(acFileSave).
    miSave: TMenuItem;
    // See @link(acFileSaveAsExecute) and @link(acFileSaveAs).
    miSaveAs: TMenuItem;
    // See @link(miSearchForObjectClick).
    miSearchForObject: TMenuItem;
    // @name is the parent menu item for  @link(miSelectAllTop),
    // @link(miSelectAllFront), and @link(miSelectAllSide).
    miSelectAll: TMenuItem;
    // See @link(tbSelectPointClick) and @link(acSelectNode).
    miSelectNodes: TMenuItem;
    // See @link(tbSelectClick) and @link(acSelectObjects).
    miSelectObjects: TMenuItem;
    // See @link(miSelectObjectsByNameClick).
    miSelectObjectsByName: TMenuItem;
    // See @link(tbLassoClick) and @link(acSelectWithLasso).
    miSelectWithLasso: TMenuItem;
    // See @link(miSetSelectedColRowLayerClick).
    miSetSelectedColRowLayer: TMenuItem;
    // See @link(acSetSpacingExecute) and @link(acSetSpacing).
    miSetSpacing: TMenuItem;
    // See @link(tb3DObjectsClick) and @link(acShow3DObjects).
    miShow3DObjects: TMenuItem;
    // See @link(tb3DColorsClick) and @link(acColoredGrid).
    miShowColoredGrid: TMenuItem;
    // See @link(miShowFormulaErrorsClick).
    miShowFormulaErrors: TMenuItem;
    // See @link(acShowFrontGridExecute) and @link(acShowFrontGrid).
    miShowFrontGrid: TMenuItem;
    // See @link(acShowGridShellExecute) and @link(acShowGridShell).
    miShowGridShell: TMenuItem;
    // See @link(miShowHideBitmapsClick).
    miShowHideBitmaps: TMenuItem;
    // See @link(miShowHideObjectsClick).
    miShowHideObjects: TMenuItem;
    // See @link(miShowSelectedObjectsClick).
    miShowSelectedObjects: TMenuItem;
    // See @link(acShowSideGridExecute) and @link(acShowSideGrid).
    miShowSideGrid: TMenuItem;
    // See @link(acShowTopGridExecute) and @link(acShowTopGrid).
    miShowTopGrid: TMenuItem;
    // See @link(acSmoothGridExecute) and @link(acSmoothGrid).
    miSmoothGrid: TMenuItem;
    // See @link(miSolutionMethodClick).
    miSolutionMethod: TMenuItem;
    // See @link(miSteadyFlowClick).
    miSteadyFlow: TMenuItem;
    // See @link(acSubdivideExecute) and @link(acSubdivide).
    miSubdivide: TMenuItem;
    // See @link(miTimeControlClick).
    miTimeControl: TMenuItem;
    // See @link(miTitleAndUnitsClick).
    miTitleAndUnits: TMenuItem;
    // @name is used to undo the users actions.
    // Its OnClick event handler is assigned
    // at runtime in @link(TUndoStack.SetUndoMenuItems).
    // @SeeAlso(tbUndo)
    miUndo: TMenuItem;
    // See @link(miVerticalExaggerationClick)
    // and @link(acEditVerticalExaggeration).
    miVerticalExaggeration: TMenuItem;
    // @name holds the
    // @link(miZoom),
    // @link(miZoomIn),
    // @link(miZoomOut),
    // @link(miPan),
    // @link(miGoTo),
    // @link(miVerticalExaggeration),
    // @link(N3),
    // @link(miShowGridShell),
    // @link(miShowTopGrid),
    // @link(miShowFrontGrid),
    // @link(miShowSideGrid),
    // @link(miShowColoredGrid),
    // @link(miShow3DObjects), and
    // @link(miRestoreDefaultView) menu items.
    miView: TMenuItem;
    // See @link(tbZoomClick) and @link(acZoom).
    miZoom: TMenuItem;
    // See @link(miZoomInClick) and @link(acZoomIn).
    miZoomIn: TMenuItem;
    // See @link(tbZoomOutClick) and @link(acZoomOut).
    miZoomOut: TMenuItem;
    // @name is the main menu of GoPhast.
    mmMainMenu: TMainMenu;
    // @name is a divider between menu items.
    N1: TMenuItem;
    // @name is a divider between menu items.
    N3: TMenuItem;
    // @name is a divider between menu items.
    N4: TMenuItem;
    // @name represents a line above the mostly opened files in the File menu.
    N5: TMenuItem;
    // @name represents a line below the mostly opened files in the File menu.
    N6: TMenuItem;
    // @name is used to open GoPhast files in @link(acFileOpenExecute).
    odOpenDialog: TOpenDialog;
    // @name contains the views of the model in the bottom half of the
    // main form.
    pnlBottom: TPanel;
    // @name contains the views of the model in the top half of the
    // main form.
    pnlTop: TPanel;
    // @name is the status bar at the bottom of the @classname.
    sbMain: TStatusBar;
    // @name is used when saving the PHAST input file to the disk in
    // @link(acExportPhastInputFileExecute).
    sdPhastInput: TSaveDialog;
    // @name is used to save GoPhast files to disk in
    // @link(acFileSaveAsExecute).
    sdSaveDialog: TSaveDialog;
    // The OnTimer event handler for @name is set to @link(ResizeZoomBoxes)
    // in TframeView.@link(TframeView.ZoomBoxResize).
    timTimer: TTimer;
    // @name changes the background color of GoPhast.
    procedure acColorExecute(Sender: TObject);
    // @name sets @link(CurrentTool) to @link(DeleteGridBoundaryTool).
    procedure acDeleteColumnRowExecute(Sender: TObject);
    // @name allows the user to edit data sets by showing @link(TfrmDataSets).
    procedure acEditDataSetsExecute(Sender: TObject);
    // @name allows the user to edit grid lines by showing
    // @link(TfrmGridSpacing).
    procedure acEditGridLinesExecute(Sender: TObject);
    // @name closes GoPhast.
    procedure acExitExecute(Sender: TObject);
    // @name exports the transport input file for PHAST.
    procedure acExportPhastInputFileExecute(Sender: TObject);
    {@name creates a new model.}
    procedure acFileNewModflowModelExecute(Sender: TObject);
    // @name reads an existing file from the disk.
    procedure acFileOpenExecute(Sender: TObject);
    // @name saves a file to the disk with a user-specified name.
    procedure acFileSaveAsExecute(Sender: TObject);
    // @name saves a file to the disk with the existing file name.
    procedure acFileSaveExecute(Sender: TObject);
    // @name sets the font of GoPhast.
    procedure acFontExecute(Sender: TObject);
    // @name generates a grid by showing @link(TfrmGenerateGrid).
    procedure acGenerateGridExecute(Sender: TObject);
    // @name allows the user to set the grid angle using @link(TfrmGridAngle).
    procedure acGridAngleExecute(Sender: TObject);
    // @name looks for help for HelpKeyword.
    // Displays the Help contents for GoPhast.
    procedure acHelpContentsExecute(Sender: TObject);
    // @name sets @link(CurrentTool) to @link(MovingGridBoundaryTool).
    procedure acMoveColumnOrRowExecute(Sender: TObject);
    // @name allows the user to move to a location, object, cell or element
    // by showing @link(TfrmGoTo).
    procedure acMoveToExecute(Sender: TObject);
    // @name restores the 3D view to its default orientation and magnification.
    procedure acRestoreDefaultViewExecute(Sender: TObject);
    // @name sets @link(CurrentTool) to @link(SpacingGridTool).
    procedure acSetSpacingExecute(Sender: TObject);
    // @name allows the user to show or hide the front grid in the 3D view.
    procedure acShowFrontGridExecute(Sender: TObject);
    // @name allows the user to show or hide the grid shell in the 3D view.
    procedure acShowGridShellExecute(Sender: TObject);
    // @name allows the user to show or hide the side grid in the 3D view.
    procedure acShowSideGridExecute(Sender: TObject);
    // @name allows the user to show or hide the top grid in the 3D view.
    procedure acShowTopGridExecute(Sender: TObject);
    // @name allows the user to adjust row, column, or layer widths
    //  by showing @link(TfrmSmoothGrid).
    procedure acSmoothGridExecute(Sender: TObject);
    // @name sets @link(CurrentTool) to @link(SubdivideGridTool).
    procedure acSubdivideExecute(Sender: TObject);
    // @name is used to draw a rotated version of the AddColumn cursor
    // in the top view of the model.
    procedure dcAddColCursorDrawCursor(Sender: TObject; const ABitMap,
      AMask: TBitmap);
    // @name is used to draw a rotated version of the AddRow cursor
    // in the top view of the model.
    procedure dcAddRowCursorDrawCursor(Sender: TObject; const ABitMap,
      AMask: TBitmap);
    // @name is used to draw a rotated version of the MoveColumn cursor
    // in the top view of the model.
    procedure dcMoveColCursorDrawCursor(Sender: TObject; const ABitMap,
      AMask: TBitmap);
    // @name is used to draw a rotated version of the MoveRow cursor
    // in the top view of the model.
    procedure dcMoveRowCursorDrawCursor(Sender: TObject; const ABitMap,
      AMask: TBitmap);
    // @name is used to draw a rotated version of the SetCellSpacing cursor
    // in the top view of the model.
    procedure dcSetSpacingDrawCursor(Sender: TObject; const ABitMap,
      AMask: TBitmap);
    // @name is used to draw a rotated version of the Subdivide cursor
    // in the top view of the model.
    procedure dcSubdivideDrawCursor(Sender: TObject; const ABitMap,
      AMask: TBitmap);
    // @name checks that the model has closed and allows the user an chance to
    // save it if it hasn't been saved.
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    { @name does the following
      1. It creates the observer list which is used to notify objects when they
         are out of date.
      2. It creates an initial model.
      3. It adds PHAST specific functions to each TRbwParser.
      4. It sets up the initial view of the model.
      5. It update the cursors.
      6. It sets some properties of the Application object.
    }
    procedure FormCreate(Sender: TObject); override;
    // @name cleans up by destroying the model and other objects.
    procedure FormDestroy(Sender: TObject); override;
    // When the form is resized, synchronize the rulers with the views
    // of the model.
    procedure FormResize(Sender: TObject);
    {@name responds to the Escape, Return, Delete, PageUp, PageDown
      and arrow keys.

     @unorderedList(
     @item(Escape: delete the last point in a @link(TScreenObject) that is being
       created.)

     @item(Return: finish any @link(TScreenObject)s.)

     @item(Delete: delete the selected @link(TScreenObject) or the selected
       vertices in the selected @link(TScreenObject).)

     @item(Left arrow: increase the selected column.)

     @item(Right arrow: decrease the selected column.)

     @item(Up arrow: increase the selected row.)

     @item(Down arrow: decrease the selected row.)

     @item(Page Up: increase the selected layer.)

     @item(Page Down: decrease the selected layer.))

     @name also implements the shortcuts of the TMenuItems.  It should not
     need to do that; CLX should do it but does not.
    }
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    // @name shows @link(frmColors) to allow the user to specify the
    // color of the light in the 3D view.
    procedure mi3D_ColorsClick(Sender: TObject);
    // @name shows the About form for GoPhast.
    // See link(frmAbout) and @link(TfrmAbout).
    procedure miAboutClick(Sender: TObject);
    // @name allows the user to set the options related to chemistry
    // in PHAST via @link(TfrmChemistryOptions).
    procedure miChemistryOptionsClick(Sender: TObject);
    // @name allows the user to edit the position of bitmaps
    // in GoPhast via @link(TfrmImportBitmap).
    procedure miEditBitmapsClick(Sender: TObject);
    // @name edits @link(TScreenObject TScreenObjects) by calling @link(EditScreenObjects).
    // @seealso(miEditSelectedObjects)
    procedure miEditSelectedObjectsClick(Sender: TObject);
    // See @link(miExamples)
    procedure miExamplesClick(Sender: TObject);
    // @name allows the user to set the options related to a free surface
    // in PHAST via @link(TfrmFreeSurface).
    procedure miFreeSurfaceClick(Sender: TObject);
    // @name allows the user to set the options related to the grid
    // in PHAST via @link(TfrmPhastGridOptions).
    procedure miGridOptionsClick(Sender: TObject);
    // See @link(miHelpOnMainWindow).
    procedure miHelpOnMainWindowClick(Sender: TObject);
    // @name allows the user to adjust how long hints should be visible
    // in GoPhast via @link(TfrmHintDelay).
    procedure miHintDelayClick(Sender: TObject);
    // @name allows the user to import bitmaps
    // into GoPhast via @link(TfrmImportBitmap).
    procedure miImportBitmapClick(Sender: TObject);
    // @name allows the user to import zones from an existing PHAST model
    // into GoPhast via @link(TfrmImportDistributedData).
    procedure miImportDistributedDatabyZoneClick(Sender: TObject);
    // @name allows the user to import DXF files
    // into GoPhast via @link(TfrmImportDXF).
    procedure miImportDXFFileClick(Sender: TObject);
    // @name is used to import scattered point data.
    procedure miImportPointsClick(Sender: TObject);
    // @name allows the user to import Shapefiles
    // into GoPhast via @link(TfrmImportShapefile).
    procedure miImportShapefileClick(Sender: TObject);
    // @name allows the user to set the options related to the print frequency
    // in PHAST via @link(TfrmPrintFrequency).
    procedure miPrintFrequencyClick(Sender: TObject);
    // @name allows the user to set the options related to the initial printing
    // in PHAST via @link(TfrmPrintInitial).
    procedure miPrintInitialClick(Sender: TObject);
    // @name allows the user to change the order of objects
    // in GoPhast via @link(TfrmRearrangeObjects).
    procedure miRearrangeObjectsClick(Sender: TObject);
    // @name allows the user to change the format of the rulers
    // in GoPhast via @link(TfrmRulerOptions).
    procedure miRulerFormatClick(Sender: TObject);
    // @name allows the user to select an object based on what it does
    // in GoPhast via @link(TfrmSearch).
    procedure miSearchForObjectClick(Sender: TObject);
    // @name allows the user to select an object based on its name
    // in GoPhast via @link(TfrmSelectObjects).
    procedure miSelectObjectsByNameClick(Sender: TObject);
    // @name allows the user to change the selected column, row, or layer
    // in GoPhast via @link(TfrmSelectColRowLayer).
    procedure miSetSelectedColRowLayerClick(Sender: TObject);
    // @name allows the user to see errors in formulas
    // in GoPhast via @link(frmFormulaErrors) and @link(TfrmFormulaErrors).
    procedure miShowFormulaErrorsClick(Sender: TObject);
    // @name allows the user to show or hide bitmaps
    // in GoPhast via @link(TfrmShowHideBitmaps).
    procedure miShowHideBitmapsClick(Sender: TObject);
    // @name allows the user to show or hide @link(TScreenObject)s
    // in GoPhast via @link(TfrmShowHideObjects).
    procedure miShowHideObjectsClick(Sender: TObject);
    // @name allows the user to see the names of the selected @link(TScreenObject)s
    // in GoPhast via @link(frmSelectedObjects) and @link(TfrmSelectedObjects).
    procedure miShowSelectedObjectsClick(Sender: TObject);
    // @name allows the user to set the options related to the solution method
    // in PHAST via @link(TfrmSolutionMethod).
    procedure miSolutionMethodClick(Sender: TObject);
    // @name allows the user to set the options related to steady flow
    // in PHAST via @link(TfrmSteadyFlow).
    procedure miSteadyFlowClick(Sender: TObject);
    // @name allows the user to set the options related to time control
    // in PHAST via @link(TfrmTimeControl).
    procedure miTimeControlClick(Sender: TObject);
    // @name allows the user to set the title and units
    // in PHAST via @link(TfrmUnits).
    procedure miTitleAndUnitsClick(Sender: TObject);
    // @name allows the user to change the vertical exaggeration of the model.
    // in GoPhast via @link(TfrmVerticalExaggeration).
    procedure miVerticalExaggerationClick(Sender: TObject);
    // @name allows the user to zoom in to a particular region.
    // @name sets @link(CurrentTool) to @link(ZoomInTool).
    procedure miZoomInClick(Sender: TObject);
    // @name is the event handler for the menu items created by
    // @link(MostRecentlyUsed).
    procedure OpenMostRecentlyUsed(Sender: TObject);
    // @name is used as the OnMouseMove event handler for most components.
    // It sets @link(CursorGrid) to cgNone;
    procedure pnlLowerRightMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    // If the cursor is over one of the dividers between panels on
    // sbMain when the MouseDown event occurs,
    // @name starts moving the divider.
    procedure sbMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    // When the cursor is over one of the dividers between panels in
    // sbMain or when the divider is being moved,
    // @name uses @link(crMoveColumn)
    // as the cursor.  Otherwise it, uses crDefault.
    procedure sbMainMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    // If the user is moving the dividers between two panels on sbMain,
    // @name moves the divider in the MouseUp event handler.
    procedure sbMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    // @name sets @link(FileFormat) based on the filter that the user
    // has selected in @link(sdSaveDialog).
    procedure sdSaveDialogFilterChange(Sender: TObject; NewIndex: Integer);
    // This procedure keeps the rulers updated when the horizontal splitter
    // separating the upper and lower halves of the main window is moved.
    procedure splitHorizMoved(Sender: TObject);
    // One vertical splitter separates the top and side views.
    // Another separates the bottom and blank panel.  This
    // procedure updates the upper one when the lower one is moved.
    procedure splitVertBottomMoved(Sender: TObject);
    // One vertical splitter separates the top and side views.
    // Another separates the bottom and blank panel.  This
    // procedure updates the lower one when the upper one is moved.
    procedure splitVertTopMoved(Sender: TObject);
    // @name shows or hides the colors of the selected @link(TDataArray) in the
    // 3D view of the model.
    procedure tb3DColorsClick(Sender: TObject);
    // @name shows or hides the colors of the @link(TScreenObject)s in the
    // 3D view of the model.
    procedure tb3DObjectsClick(Sender: TObject);
    // @name allows the user to add a horizontal boundary to the grid.
    // @name sets @link(CurrentTool) to @link(AddGridBoundaryTool).
    procedure tbAddHorizontalBoundaryClick(Sender: TObject);
    // @name allows the user to add a vertical boundary to the grid.
    // @name sets @link(CurrentTool) to @link(AddGridBoundaryTool).
    procedure tbAddVerticalBoundaryClick(Sender: TObject);
    // @name allows the user to delete a segment of a @link(TScreenObject)
    // @name sets @link(CurrentTool) to @link(DeleteSegmentTool).
    procedure tbDeleteSegmentClick(Sender: TObject);
    // @name allows the user to rotate the grid
    // @name sets @link(CurrentTool) to @link(RotateGridTool).
    procedure tbGridAngleClick(Sender: TObject);
    // @name allows the user to insert a point in a @link(TScreenObject)
    // @name sets @link(CurrentTool) to @link(InsertPointTool).
    procedure tbInsertPointClick(Sender: TObject);
    // @name allows the user to @link(TScreenObject)s
    // by dragging a line around them.
    // @name sets @link(CurrentTool) to @link(LassoTool).
    procedure tbLassoClick(Sender: TObject);
    // @name allows the user to create a line @link(TScreenObject).
    // @name sets @link(CurrentTool) to @link(CreateLineScreenObjectTool).
    procedure tbLineClick(Sender: TObject);
    // @name allows the user to start panning.
    // @name sets @link(CurrentTool) to @link(PanTool).
    procedure tbPanClick(Sender: TObject);
    // @name allows the user to create a point @link(TScreenObject).
    // @name sets @link(CurrentTool) to @link(CreatePointScreenObjectTool).
    procedure tbPointClick(Sender: TObject);
    // @name makes sure all buttons except the current one are up.
    // @param(Sender is the TToolButton that has been depressed.)
    procedure tbPointMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    // @name allows the user to create a polygon @link(TScreenObject).
    // @name sets @link(CurrentTool) to @link(CreateLineScreenObjectTool).
    procedure tbPolygonClick(Sender: TObject);
    // @name allows the user to create a rectangle @link(TScreenObject).
    // @name sets @link(CurrentTool) to @link(CreateRectangleScreenObjectTool).
    procedure tbRectangleClick(Sender: TObject);
    // @name allows the user to select a @link(TScreenObject) by clicking on it.
    // @name sets @link(CurrentTool) to @link(SelectScreenObjectTool).
    procedure tbSelectClick(Sender: TObject);
    // @name allows the user to select a node in a @link(TScreenObject)
    // by clicking on it.
    // @name sets @link(CurrentTool) to @link(SelectPointTool).
    procedure tbSelectPointClick(Sender: TObject);
    // @name shows or hides the grid shell in the
    // 3D view of the model.
    procedure tbShellClick(Sender: TObject);
    // @name allows the user to create a straight-line @link(TScreenObject).
    // @name sets @link(CurrentTool) to @link(CreateStraightLineScreenObjectTool).
    procedure tbStraightLineClick(Sender: TObject);
    // @name allows the user to zoom in on a particular region
    // by selecting it with the mouse.
    // @name sets @link(CurrentTool) to @link(ZoomTool).
    procedure tbZoomClick(Sender: TObject);
    // @name allows the user to zoom out from a particular region.
    // @name sets @link(CurrentTool) to @link(ZoomOutTool).
    procedure tbZoomOutClick(Sender: TObject);
    // When handling the OnMouseUp event, @name checks that the
    // user released the mouse while it was over the TToolButton.
    // If so, ToolButton.Down is set to false and @link(CurrentTool) is
    // set to @nil.
    procedure ToolButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetGridLineDrawingChoice(Sender: TObject);
  private
    // See @link(CanDraw).
    FCanDraw: boolean;
    // See @link(CanEdit).
    FCanEdit: boolean;
    // See @link(ClickedRuler).
    FClickedRuler: TObject;
    // See @link(CurrentTool).
    FCurrentTool: TCustomInteractiveTool;
    // See @link(CursorGrid).
    FCursorGrid: TCursorGrid;
    // See @link(CursorX).
    FCursorX: integer;
    // See @link(CursorY).
    FCursorY: integer;
    // See @link(FileFormat).
    FFileFormat: TFileFormat;
    // See @link(FrontGridChanged).
    FFrontGridChanged: boolean;
    // See @link(FrontScreenObjectsChanged).
    FFrontScreenObjectsChanged: boolean;
    // See @link(PhastModel).
    FPhastModel: TPhastModel;
    // @name indicates which boundary between panels on @link(sbMain) the
    // mouse was over when it started to drag the panel.
    FMovingPanelIndex: integer;
    // @name indicates whether or not the user is dragging the boundary
    // between two panels on @link(sbMain).
    FMovingStatusBarPanel: boolean;
    // @name is set to Height in @link(FormCreate) and @link(FormResize).
    // See @link(OldHeight).
    FOldHeight: integer;
    // @name is set to Width in @link(FormCreate) and @link(FormResize).
    // See @link(OldWidth).
    FOldWidth: integer;
    // See @link(SideGridChanged).
    FSideGridChanged: boolean;
    // See @link(SideScreenObjectsChanged).
    FSideScreenObjectsChanged: boolean;
    // See @link(TopGridChanged).
    FTopGridChanged: boolean;
    // See @link(TopScreenObjectsChanged).
    FTopScreenObjectsChanged: boolean;
    // See @link(IniFile).
    FIniFile: TMemInifile;
    FChangingSelection: boolean;
    // @name is the event handler for @link(TPhastModel.On3DViewChanged).
    // It Invalidates @link(Tframe3DView.glWidModelView).
    procedure Invalidate3DView(Sender: TObject);
    // @name is the event handler for @link(TPhastModel.OnGetZoomBox).
    // @param(VD VD indicates which @link(TQrbwZoomBox2) is desired.)
    // @param(ZoomBox ZoomBox is the @link(TQrbwZoomBox2) in the
    //   @link(TframeView) indicated by VD.)
    procedure GetZoomBox(Sender: TObject; VD: TViewDirection;
      var ZoomBox: TQrbwZoomBox2);
    // @name is the event handler for
    // @link(TPhastModel.OnGetCurrentScreenObject).
    // @param(VD VD indicates which @link(TframeView) to check for the
    //  @link(TframeView.CurrentScreenObject).)
    // @param(ScreenObject ScreenObject is the
    //   @link(TframeView.CurrentScreenObject) in the
    //   @link(TframeView) indicated by VD.)
    procedure GetCurrentScreenObject (Sender: TObject; VD: TViewDirection;
      var ScreenObject: TScreenObject);
    // @name is the event handler for @link(TPhastModel.OnConvertPoint).
    // @param(VD VD indicates which @link(TframeView) that will be used for the
    //   conversion.)
    // @param(RealPoint RealPoint is the TPoint2D to be converted.)
    // @param(ScreenCoordinate ScreenCoordinate is the TPoint corresponding to
    //   RealPoint.)
    procedure ConvertPoint(Sender: TObject; VD: TViewDirection;
      const RealPoint: TPoint2D; var ScreenCoordinate: TPoint);
    // @name is the event handler for
    // @link(TPhastModel.OnScreenObjectSelected) and
    // @link(TPhastModel.OnScreenObjectUnSelected). It enables or disables
    // @link(miEditSelectedObjects),  @link(acAddPolygonsToObject),
    // @link(acAddLinesToObject), and @link(acAddPointsToObject).
    procedure ScreenObjectSelectionChange(Sender: TObject);
    // @name is the event handler for @link(TPhastModel.OnCheckScreenObject).
    // @name sets IsACurrentScreenObject to true if ScreenObject is the
    // @link(TframeView.CurrentScreenObject) on any @link(TframeView).
    procedure CheckScreenObject(Sender: TObject; ScreenObject: TScreenObject;
      var IsACurrentScreenObject: boolean);
    // @name adds FileName to @link(MostRecentlyUsed).
    procedure AddMostRecentlyUsedFile(const FileName: string);
    // @name adjusts SecondToolBar.Left so that is just to the right of
    // FirstToolBar.  It is assumed that FirstToolBar and SecondToolBar
    // are on the same row.
    procedure AdjustToolbarPositions(FirstToolBar,
      SecondToolBar: TToolBar);
    // If @link(frmProgressMM), @link(frmSelectedObjects), or @link(frmColors)
    // are visible, @name brings them to the front.
    // @name is the Application.OnActivate event handler.
    procedure BringFormsToFront(Sender: TObject);
    // @name checks that the model has closed and allows the user an chance to
    // save it if it hasn't been saved.
    function CheckModel: boolean;
    // If any individual vertices in a @link(TScreenObject) are selected
    // @name deselects them. @name is used in @link(tbSelectClick).
    procedure ClearSelectedNodes;
    // @name is used to draw the bitmaps for the
    // add-column or add-row cursor for the top view.
    // It is also used to help draw the subdivide cursor.
    // The cursor is drawn with lines
    // that are parallel to the grid.
    procedure DrawAddColRowCursor(const AndImage: TBitmap; Angle: real;
      const CursorComponent: TRbwDynamicCursor);
    // @name is used to draw the bitmaps for the
    // move-column or move-row cursor for the top view.
    // The cursor is drawn with lines
    // that are parallel to the grid.
    procedure DrawMoveColRowCursor(const AndImage: TBitmap; Angle: real;
      const CursorComponent: TRbwDynamicCursor);
    // @name is used to help draw the bitmaps for the
    // subdivide cursor for the top view.  The cursor is drawn with lines
    // that are parallel to the grid.
    procedure DrawSubdivideCursor(const AndImage: TBitmap; Angle: real;
      const CursorComponent: TRbwDynamicCursor);
    // @name writes the transport input file for PHAST.
    // The work is delegated to @link(WritePhastInput).
    procedure ExportFile(const FileName: string; RunModel: boolean);
    // @name fills AList with the buttons that can't all be down at the same
    // time.
    procedure FillButtonList(AList: TList);
    // See @link(PhastGrid).
    function GetPhastGrid: TPhastGrid;
    // @name returns true if X indicates that the mouse is over
    // one of the dividers between panels on @link(sbMain).
    // @name is called from @link(sbMainMouseMove).
    function IsOverStatusPanelDivider(const X: integer): boolean;
    // @name saves the file named FileName.
    procedure SaveAFile(FileName: string);
    // If none of the buttons for creating @link(TScreenObject)s, editing
    // the grid or other similar activities is Down. @name sets @link(tbSelect)
    // down.
    procedure SelectDefaultButton;
    // @name toggles the checked state of an action or the action
    // associated with a control.
    // @param(Sender must be the action or control whose state is to be toggled
    // for @name to do anything.  If it isn't, @name does nothing.)
    procedure SetActionChecked(Sender: TObject);
    {@name sets the Down property of all buttons related to CurrentButton
     to false.
     @param(CurrentButton is the button that should remain down.)}
    procedure SetButtonsUp(const CurrentButton: TObject);
    // See @link(CurrentTool).
    procedure SetCurrentTool(const Value: TCustomInteractiveTool);
    // See @link(CursorGrid).
    procedure SetCursorGrid(const Value: TCursorGrid);
    // See @link(FileFormat).
    procedure SetFileFormat(const Value: TFileFormat);
    // See @link(FrontScreenObjectsChanged).
    procedure SetFrontScreenObjectsChanged(const Value: boolean);
    // See @link(SideScreenObjectsChanged).
    procedure SetSideScreenObjectsChanged(const Value: boolean);
    // See @link(TopScreenObjectsChanged).
    procedure SetTopScreenObjectsChanged(const Value: boolean);
    // @name sets TframeView..@link(TframeView.ZoomBox).Cursor and
    // TframeView.@link(TframeView.ZoomBox).ImageBox32.Cursor
    // to ACursor in @link(frameTopView), @link(frameFrontView),
    // and @link(frameSideView).
    procedure SetZB_Cursors(const ACursor: TCursor);
    // @name is the event-handler for Application.OnHint.
    // @name shows a long version of the hint on the status bar (@link(sbMain)).
    procedure ShowHint(Sender: TObject);
    function GetModflowGrid: TModflowGrid;
    function GetGrid: TCustomGrid;
    procedure InvalidateViewOfModel;
    function GetModelSelection: TModelSelection;
    procedure SetModelSelection(const Value: TModelSelection);
    procedure NewPosition(Sender: TObject; NewPosition: TPositionStorage);
    procedure CheckInternet;
    procedure SaveModelMateProject;
    procedure SetChangingSelection(const Value: boolean);
//    procedure SetShowUcodeInterface(const Value: boolean);
    procedure OnOpenFile(Sender: TObject);
    procedure SetCanDraw(const Value: boolean);
    function GetObservationFileName(SD: TSaveDialog): string;
    function GetPredictionFileName(SD: TSaveDialog): string;
    function GetCanDraw: boolean;
    procedure InitializeModflowLgrInputDialog;
    { Private declarations }
  protected
    // @name is used to specify the format of the files that
    // can be opened or saved by GoPhast.
    property FileFormat: TFileFormat read FFileFormat write SetFileFormat;
    // @name assigns the event handlers to the undo/redo buttons and
    // undo/redo menu items.
    procedure OnAppIdle(Sender: TObject; var Done: Boolean);
  public
    FCubeControl: TRbwModelCube;
    procedure EnableHufMenuItems;
//    property ShowUcodeInterface: boolean read FShowUcodeInterface
//      write SetShowUcodeInterface;
    property ChangingSelection: boolean read FChangingSelection
      write SetChangingSelection;
    procedure EnableManageFlowObservations;
    procedure EnableManageHeadObservations;
    // @name reads an ini file containing the most recently used files.
    // @seealso(WriteIniFile)
    procedure ReadIniFile;
    // @name writes an ini file containing the most recently used files.
    // @seealso(ReadIniFile)
    procedure WriteIniFile;
    procedure SynchronizeViews(SourceView: TViewDirection);
    procedure ReDrawAllViews(Sender: TObject);
    procedure EnableLinkStreams;
    // @name calls TframeView.@link(TframeView.AdjustScales) for
    // @link(frameTopView), @link(frameFrontView),
    // and @link(frameSideView).
    procedure AdjustScales;
    // Setting @name to False causes TframeView.@link(
    // TframeView.Paint) to
    // exit immediately without doing anything.
    property CanDraw: boolean read GetCanDraw write SetCanDraw;
    // @name is used to prevent editing of two or more sets of
    // @link(TScreenObject TScreenObjects) at one time.
    property CanEdit: boolean read FCanEdit write FCanEdit;
    // @name is set to the TRbwRuler in
    // TframeView.@link(TframeView.rulerDblClick)
    // It determines which tab of @link(TfrmRulerOptions) will be visible
    // when it is first displayed.
    property ClickedRuler: TObject read FClickedRuler write FClickedRuler;
    // @name is the @link(TCustomInteractiveTool) that is currently
    // being used to handle the user interaction with
    // TframeView.@link(TframeView.ZoomBox).
    property CurrentTool: TCustomInteractiveTool read FCurrentTool
      write SetCurrentTool;
    // @name is used to indicate which view of the model, if any, the mouse
    // is over.
    property CursorGrid: TCursorGrid read FCursorGrid write SetCursorGrid;
    // @name is used to store the X-coordinate of the current mouse position
    // in the current TframeView.@link(TframeView.ZoomBox).
    property CursorX: integer read FCursorX write FCursorX;
    // @name is used to store the Y-coordinate of the current mouse position
    // in the current TframeView.@link(TframeView.ZoomBox).
    property CursorY: integer read FCursorY write FCursorY;
    // @name allows the user to edit the properties of the selected
    // screen objects.
    function DefaultVE: Real;
    procedure EditScreenObjects;
    // @name is used to indicate that a change has been made to the grid
    // in the front view of the model
    // so that the view of the model needs to be redrawn.
    property FrontGridChanged: boolean read FFrontGridChanged
      write FFrontGridChanged;
    // Setting @name to true causes the front view of the model to be redrawn.
    property FrontScreenObjectsChanged: boolean read FFrontScreenObjectsChanged
      write SetFrontScreenObjectsChanged;
    // @name is the ini file for GoPhast.  It stores a list of the most
    // recently opened files.
    property Grid: TCustomGrid read GetGrid;
    // @name is the name of a file containing the initialization data
    // for the program.  It contains the names of the most recently opened
    // files.
    property IniFile: TMemInifile read FIniFile;
    // @name invalidates all the @link(TDataArray)s in the model.
    procedure InvalidateDataSets;
    // @name calls  @link(InvalidateTop), @link(InvalidateFront), and
    // @link(InvalidateSide).
    procedure InvalidateAllViews;
   // @name causes the the front view of the model to be redrawn.
    procedure InvalidateFront;
    // @name invalidates @link(PhastModel).
    procedure InvalidateModel;
   // @name causes the the side view of the model to be redrawn.
    procedure InvalidateSide;
   // @name causes the the top view of the model to be redrawn.
    procedure InvalidateTop;
    procedure InitializeView(ModelXWidth, ModelYWidth, ModelHeight: Real);
    property ModflowGrid: TModflowGrid read GetModflowGrid;
    // @name is the @link(TPhastModel) that is being edited in GoPhast.
    property PhastModel: TPhastModel read FPhastModel write FPhastModel;
    // @name represents the height of the main form in GoPhast when it is not
    // minimized.  It is used in TPhastModel.@link(TPhastModel.Height).
    property OldHeight: integer read FOldHeight;
    // @name represents the width of the main form in GoPhast when it is not
    // minimized.  It is used in TPhastModel.@link(TPhastModel.Width).
    property OldWidth: integer read FOldWidth;
    // @name is used to read a GoPhast file from the disk.
    // @param(FileName is the name of the file that is to be read.)
    procedure OpenAFile(const FileName: string);
    // @name is the @link(TCustomModel.PhastGrid) of @link(PhastModel).
    property PhastGrid: TPhastGrid read GetPhastGrid;
    // @name sets the @link(TScreenObject.Selected) property of
    // all @link(TScreenObject)s to false.  @name returns true if any
    // of them were selected.
    function ResetSelectedScreenObjects: boolean;
    // @name causes the @link(TFrameView.ZoomBox)es to be resized.
    // @name is set to be the OnTimer event handler of @link(timTimer)
    // in TframeView.@link(TframeView.ZoomBoxResize).
    procedure ResizeZoomBoxes(Sender: TObject);
    // @name is the event handler of @link(TPhastModel.OnScreenObjectsChanged).
    // @name is used to update all views of the model.
    procedure ScreenObjectsChanged(Sender: TObject);
    // @name is used to indicate that a change has been made to the grid
    // in the side view of the model
    // so that the view of the model needs to be redrawn.
    property SideGridChanged: boolean read FSideGridChanged
      write FSideGridChanged;
    // Setting @name to true causes the side view of the model to be redrawn.
    property SideScreenObjectsChanged: boolean read FSideScreenObjectsChanged
      write SetSideScreenObjectsChanged;
    // @name is used to indicate that a change has been made to the grid
    // in the top view of the model
    // so that the view of the model needs to be redrawn.
    property TopGridChanged: boolean read FTopGridChanged
      write FTopGridChanged;
    // Setting @name to true causes the top view of the model to be redrawn.
    property TopScreenObjectsChanged: boolean read FTopScreenObjectsChanged
      write SetTopScreenObjectsChanged;
    // @name tells all the @link(TDataArray)s what the new grid dimensions are.
    procedure UpdateDataSetDimensions;
    procedure UpdateModelSelection;
    // @seealso(ModelSelectionChange)
    property ModelSelection: TModelSelection read GetModelSelection
      write SetModelSelection;
    procedure ModelSelectionChange(Sender: TObject);
    procedure EnableInvertSelection;
    procedure InvalidateGrid;
    property CreateNewCompositeBudgetFile: boolean
      read FCreateNewCompositeBudgetFile write FCreateNewCompositeBudgetFile;
    { Public declarations }
    property ObservationFileName[SD: TSaveDialog]: string read GetObservationFileName;
    property PredictionFileName[SD: TSaveDialog]: string read GetPredictionFileName;
    procedure EnableVisualization;
  end;


var
  // @name is the main form of ModelMuse.
  frmGoPhast: TfrmGoPhast;

const
  VideoUrl = 'http://water.usgs.gov/nrp/gwsoftware/ModelMuse/ModelMuseVideos.html';
  StrCustomization = 'Customization';
  StrShowTips = 'ShowTips';

implementation

uses
  Math, frmVerticalExaggerationUnit, CursorsFoiledAgain, frmSubdivideUnit,
  frmGridAngleUnit, frmGridSpacingUnit, frmSmoothGridUnit,
  frmAboutUnit, frmHintDelayUnit, UndoItemsScreenObjects,
  frmRearrangeObjectsUnit, frmSelectColRowLayerUnit, frmSetSpacingUnit,
  frmScreenObjectPropertiesUnit, frmDataSetsUnits, frmGridColorUnit,
  InteractiveTools, GIS_Functions, frmRulerOptionsUnit, frmGoToUnit,
  frmFormulaErrorsUnit, GridGeneration, frmGenerateGridUnit,
  frmPhastGridOptionsUnit, frmPrintFrequencyUnit, frmPrintInitialUnit,
  frmSolutionMethodUnit, frmUnitsUnit, frmSteadyFlowUnit, frmTimeControlUnit,
  rwXMLConv, WritePhastUnit, frmChemistryOptionsUnit,
  frmImportDistributedDataUnit, UndoItems, frmFreeSurfaceUnit,
  frmImportShapefileUnit, frmImportDXFUnit, CompressedImageUnit,
  frmImportBitmapUnit, frmSelectImageUnit, frmStartUpUnit, OpenGL12x,
  frmSearchUnit, frmSelectedObjectsUnit, PhastDataSets, frmShowHideObjectsUnit,
  frmColorsUnit, ModelMuseUtilities, frmProgressUnit, frmShowHideBitmapsUnit,
  frmSelectObjectsUnit, frmImportPointsUnits, GuiSettingsUnit, frmLayersUnit,
  frmModflowOptionsUnit, frmModflowTimeUnit, frmModflowOutputControlUnit,
  frmErrorsAndWarningsUnit, CountObjectsUnit,frmGridValueUnit,
  frmModflowPackagesUnit, frmProgramLocationsUnit, frmGlobalVariablesUnit,
  IniFileUtilities, frmFilesToArchiveUnit, RequiredDataSetsUndoUnit,
  frmLinkStreamsUnit, frmExportShapefileUnit, frmImportModflowUnit,
  frmSelectResultToImportUnit, frmScaleRotateMoveUnit, RbwInternetUtilities,
  frmModflowNameFileUnit, frmImportGriddedDataUnit, FluxObservationUnit,
  frmManageFluxObservationsUnit, TempFiles, frmContourDataUnit, ZLib,
  GlobalTypesUnit, JupiterUnit, CheckInternetUnit, 
  frmHUF_LayersUnit, frmBatchFileAdditionsUnit, frmSelectObjectsForEditingUnit, 
  frmDataSetValuesUnit, frmExportShapefileObjectsUnit, frmDeleteImageUnit,
  frmModpathDisplayUnit, frmPhastLocationUnit, frmEndPointDisplayUnit,
  frmTimeSeriesDisplayUnit, frmImportSurferGrdFileUnitUnit, frmImportDEMUnit,
  frmExportImageUnit, frmManageParametersUnit, frmManageHeadObservationsUnit,
  RealListUnit, ContourExport, frmExportCSVUnit, frmChildModelsUnit,
  frmImportAsciiRasterUnit, CustomModflowWriterUnit, ModflowUnitNumbers;

resourcestring
  StrModelMate = 'ModelMate';
  StrEnableModelMate = 'EnableModelMate';
  StrPathlineshp = 'Pathline.shp';
  StrEndpointsAtStartshp = 'EndpointsAtStart.shp';
  StrEndpointsAtEndshp = 'EndpointsAtEnd.shp';
  StrTimeSeriesshp = 'TimeSeries.shp';
  StrDisplayOption = 'DisplayOption';
  StrColor = 'Color';
  StrContour = 'Contour';
  StrDisplayNone = 'None';


{$R *.dfm}

const
  DividerWidth = 2;
var
  SbMainHeight: integer;

const
  HELP_TAB = 15;
  TAB_CONTENTS = 0;
  TAB_INDEX = -2;
  TAB_FIND = 1;


procedure TfrmGoPhast.splitVertTopMoved(Sender: TObject);
begin
  if FOtherSplitterMoving then
  begin
    Exit;
  end;

  FOtherSplitterMoving := True;
  try
    // One vertical splitter separates the top and side views.
    // Another separates the bottom and blank panel.  This
    // procedure updates the lower one when the upper one is moved.
    if (frameSideView <> nil) and (frameSideView.Width <= 0) then
    begin
      frameSideView.Width := 1;
    end;
    if frameSideView <> nil then
    begin
      if frameSideView.Width = splitVertTop.MinSize then
      begin
        splitVertBottom.Maximized := True;
      end
      else
      begin
        splitVertBottom.Maximized := False;
        frame3DView.Width := frameSideView.Width;
      end;
      splitVertBottom.Invalidate;
    end;
    AdjustScales;
  finally
    FOtherSplitterMoving := False;
  end;
end;

procedure TfrmGoPhast.SynchronizeViews(SourceView: TViewDirection);
var
  CenterPoint: TPoint;
  RealCenterPoint: TPoint2D;
  RotatedCenterPoint: TPoint2D;
  ZInt: integer;
  Z: double;
  XInt: integer;
  XPrime: double;
  YInt: Integer;
  YPrime: double;
  NewPosition: TPositionStorage;
begin
  if FSynchronizeCount > 0 then
  begin
    Exit;
  end;
  Inc(FSynchronizeCount);
  try
    NewPosition := TPositionStorage.Create;
    case SourceView of
      vdTop:
        begin
          CenterPoint.x := frameTopView.ZoomBox.Width div 2;
          CenterPoint.Y := frameTopView.ZoomBox.Height div 2;
          RealCenterPoint.x := frameTopView.ZoomBox.X(CenterPoint.x);
          RealCenterPoint.Y := frameTopView.ZoomBox.Y(CenterPoint.y);
          RotatedCenterPoint :=
            Grid.RotateFromRealWorldCoordinatesToGridCoordinates(RealCenterPoint);
          ZInt := frameFrontView.ZoomBox.Height div 2;
          Z := frameFrontView.ZoomBox.Y(ZInt);

          frameFrontView.ZoomBox.Magnification := frameTopView.ZoomBox.Magnification;
          frameSideView.ZoomBox.Magnification := frameTopView.ZoomBox.Magnification;

          SetFrontPosition(RotatedCenterPoint.x, Z);
          SetSidePosition(RotatedCenterPoint.Y, Z);

          NewPosition.Top.XCenter := RealCenterPoint.x;
          NewPosition.Top.YCenter := RealCenterPoint.y;
          NewPosition.Top.Magnification := frameTopView.ZoomBox.Magnification;

          NewPosition.Front.XCenter := RotatedCenterPoint.x;
          NewPosition.Front.YCenter := Z;
          NewPosition.Front.Magnification := frameFrontView.ZoomBox.Magnification;

          NewPosition.Side.XCenter := RotatedCenterPoint.Y;
          NewPosition.Side.YCenter := Z;
          NewPosition.Side.Magnification := frameSideView.ZoomBox.Magnification;
        end;
      vdFront:
        begin
          CenterPoint.x := frameTopView.ZoomBox.Width div 2;
          CenterPoint.Y := frameTopView.ZoomBox.Height div 2;
          RealCenterPoint.x := frameTopView.ZoomBox.X(CenterPoint.x);
          RealCenterPoint.Y := frameTopView.ZoomBox.Y(CenterPoint.y);
          RotatedCenterPoint :=
            Grid.RotateFromRealWorldCoordinatesToGridCoordinates(RealCenterPoint);

          XInt := frameFrontView.ZoomBox.Width div 2;
          XPrime := frameFrontView.ZoomBox.X(XInt);
          ZInt := frameFrontView.ZoomBox.Height div 2;
          Z := frameFrontView.ZoomBox.Y(ZInt);
          RotatedCenterPoint.X := XPrime;
          RealCenterPoint :=
            Grid.RotateFromGridCoordinatesToRealWorldCoordinates(RotatedCenterPoint);

          frameTopView.ZoomBox.Magnification := frameFrontView.ZoomBox.Magnification;
          frameSideView.ZoomBox.Magnification := frameFrontView.ZoomBox.Magnification;

          SetTopPosition(RealCenterPoint.x, RealCenterPoint.y);
          SetSidePosition(RotatedCenterPoint.Y, Z);

          NewPosition.Top.XCenter := RealCenterPoint.x;
          NewPosition.Top.YCenter := RealCenterPoint.y;
          NewPosition.Top.Magnification := frameTopView.ZoomBox.Magnification;

          NewPosition.Front.XCenter := XPrime;
          NewPosition.Front.YCenter := Z;
          NewPosition.Front.Magnification := frameFrontView.ZoomBox.Magnification;

          NewPosition.Side.XCenter := RotatedCenterPoint.Y;
          NewPosition.Side.YCenter := Z;
          NewPosition.Side.Magnification := frameSideView.ZoomBox.Magnification;
        end;
      vdSide:
        begin
          CenterPoint.x := frameTopView.ZoomBox.Width div 2;
          CenterPoint.Y := frameTopView.ZoomBox.Height div 2;
          RealCenterPoint.x := frameTopView.ZoomBox.X(CenterPoint.x);
          RealCenterPoint.Y := frameTopView.ZoomBox.Y(CenterPoint.y);
          RotatedCenterPoint :=
            Grid.RotateFromRealWorldCoordinatesToGridCoordinates(RealCenterPoint);

          YInt := frameSideView.ZoomBox.Height div 2;
          YPrime := frameSideView.ZoomBox.Y(YInt);
          ZInt := frameSideView.ZoomBox.Width div 2;
          Z := frameSideView.ZoomBox.X(ZInt);
          RotatedCenterPoint.Y := YPrime;
          RealCenterPoint :=
            Grid.RotateFromGridCoordinatesToRealWorldCoordinates(RotatedCenterPoint);

          frameTopView.ZoomBox.Magnification := frameSideView.ZoomBox.Magnification;
          frameFrontView.ZoomBox.Magnification := frameSideView.ZoomBox.Magnification;

          SetTopPosition(RealCenterPoint.x, RealCenterPoint.y);
          SetFrontPosition(RotatedCenterPoint.X, Z);

          NewPosition.Top.XCenter := RealCenterPoint.x;
          NewPosition.Top.YCenter := RealCenterPoint.y;
          NewPosition.Top.Magnification := frameTopView.ZoomBox.Magnification;

          NewPosition.Front.XCenter := RotatedCenterPoint.X;
          NewPosition.Front.YCenter := Z;
          NewPosition.Front.Magnification := frameFrontView.ZoomBox.Magnification;

          NewPosition.Side.XCenter := YPrime;
          NewPosition.Side.YCenter := Z;
          NewPosition.Side.Magnification := frameSideView.ZoomBox.Magnification;
        end;
      else Assert(False);
    end;
    FPositionList.Submit(NewPosition);
  finally
    Dec(FSynchronizeCount);
  end;
end;

procedure TfrmGoPhast.splitVertBottomMoved(Sender: TObject);
begin
  if FOtherSplitterMoving then
  begin
    Exit;
  end;

  FOtherSplitterMoving := True;
  try
    // One vertical splitter separates the top and side views.
    // Another separates the bottom and blank panel.  This
    // procedure updates the upper one when the lower one is moved.
    if (frame3DView <> nil) and (frame3DView.Width <= 0) then
    begin
      frame3DView.Width := 1;
    end;
    if frameSideView <> nil then
    begin
      if frame3DView.Width = splitVertBottom.MinSize then
      begin
        splitVertTop.Maximized := True;
      end
      else
      begin
        splitVertTop.Maximized := False;
        frameSideView.Width := frame3DView.Width;
      end;
      splitVertTop.Invalidate;
    end;
    AdjustScales;
  finally
    FOtherSplitterMoving := False;
  end;
end;

procedure TfrmGoPhast.acFileNewModflowModelExecute(Sender: TObject);
begin
  if CheckModel then
  begin
    if frmScreenObjectProperties <> nil then
    begin
      frmScreenObjectProperties.ClearExpressionsAndVariables;
    end;
    CancelCurrentScreenObject;
    if frmShowHideObjects <> nil then
    begin
      frmShowHideObjects.Close;
    end;
    FreeAndNil(frmGridColor);
    if frmGridValue <> nil then
    begin
      frmGridValue.Close;
    end;
    if frmSelectedObjects <> nil then
    begin
      frmSelectedObjects.Close;
    end;
    MostRecentlyUsed.FileToIgnore := '';
    MostRecentlyUsed.Capacity := 4;
    {FileNewExecute creates a new model.}
    sdSaveDialog.FileName := '';
    Caption := StrModelName;

    PhastModel.DataArrayManager.ClearDeletedDataSets;
    //  ObserverList.Clear;
    UndoStack.Clear;
    PhastModel.Clear;
    FPositionList.Clear;
    UndoStack.SetUndoMenuItems(miUndo, miRedo);
    tbUndo.Enabled := False;
    tbRedo.Enabled := False;
//    UndoStack.SetUndoToolButtons(tbUndo, tbRedo);

    PhastModel.ClearExpressionsAndVariables; 

    PhastGrid.ColumnCount := -1;
    PhastGrid.RowCount := -1;
    PhastGrid.LayerCount := -1;
    // Formula manager needs FPhastModel to be defined during FPhastModel.Free;
    FPhastModel.Free;
    FPhastModel := nil;
//    FreeAndNil(FPhastModel);

    if frmColors <> nil then
    begin
      frmColors.HideMe;
      frmColorsUnit.SetDefaults;
    end;
    CreatePhastModel;
    PhastModel.Name := 'Model';
    ResetScreenObjectCount;
    PhastGrid.OnSelectedLayerChange := frameTopView.ItemChange;
    PhastGrid.OnSelectedColumnChange := frameSideView.ItemChange;
    PhastGrid.OnSelectedRowChange := frameFrontView.ItemChange;
    ModflowGrid.OnSelectedLayerChange := frameTopView.ItemChange;
    ModflowGrid.OnSelectedColumnChange := frameSideView.ItemChange;
    ModflowGrid.OnSelectedRowChange := frameFrontView.ItemChange;
    TopGridChanged := True;
    FrontGridChanged := True;
    SideGridChanged := True;
    InvalidateAllViews;
//    SetGridLineDrawingChoice;

    if Sender = miModflow2005Model then
    begin
      ShowAForm(TfrmImportModflow);
      if frmGoPhast.Grid = nil then
      begin
        with TfrmStartUp.Create(nil) do
        try
        begin
          rgChoice.ItemIndex := 0;
          btnNextClick(nil);
          btnDontCreateGridClick(nil);
        end;
        finally
          Free;
        end;
      end;
    end
    else
    begin
      with TfrmStartUp.Create(nil) do
      begin
        try
          if Sender = acFileNewModflowModel then
          begin
            rgChoice.ItemIndex := 0;
          end
          else if Sender = acFileNewPhastModel then
          begin
            rgChoice.ItemIndex := 1;
          end
          else
          begin
            Assert(False);
          end;

          btnNextClick(nil);
          self.Hide;
          try
            ShowModal;
          finally
            self.Show;
          end;
        finally
          Free;
        end;
      end;
    end;
    FPositionList.Clear;
    SynchronizeViews(vdTop);
    frame3DView.SetDefaultOrientation;
    EnableInvertSelection;
    Application.Title := StrModelName;
  end;
  EnableDeleteImage;
end;

procedure TfrmGoPhast.splitHorizMoved(Sender: TObject);
begin
  // This procedure keeps the rulers updated when the horizontal splitter
  // separating the upper and lower halves of the main window is moved.
  AdjustScales;

  if pnlBottom.Height <= 0 then
  begin
    pnlBottom.Height := 1;
  end;
  // Make sure that the wrong thing doesn't have it's height changed.

  if SbMainHeight < sbMain.Height then
  begin
    pnlBottom.Height := sbMain.Height - SbMainHeight;
    sbMain.Height := SbMainHeight;
  end;
  // Make sure splitHoriz is above the right thing.
  if splitHoriz.Top > pnlBottom.Top then
  begin
    splitHoriz.Top :=
      pnlBottom.Top - splitHoriz.Height;
  end;
end;

procedure TfrmGoPhast.miSplitObjectAtSelectedVerticesClick(Sender: TObject);
begin
  inherited;
  UndoStack.Submit(TUndoSplitScreenObject.Create);
end;

procedure TfrmGoPhast.ReadIniFile;
var
  Keys: TStringList;
  Index: integer;
  FileName: string;
  IniFName: string;
  WebIniFileName: string;
  WebIniFile: TMemIniFile;
  ShownURL: Boolean;
  LastTipDate: TDateTime;
  LastCheckInternetDate: TDateTime;
begin
  FIniFile.Free;
  IniFName := IniFileName(Handle, Application.ExeName);
  FIniFile:= TMemInifile.Create(IniFName);
  miShowVideoTips.Checked := FIniFile.ReadBool(StrCustomization, StrShowTips, True);
//  FShowUcodeInterface := FIniFile.ReadBool(StrModelMate, StrEnableModelMate, False);
//  SetVisibilityOfModelMateActions;

  DisplayChoices[dcColor] := FIniFile.ReadInteger(StrDisplayOption, StrColor, 0);
  DisplayChoices[dcContour] := FIniFile.ReadInteger(StrDisplayOption, StrContour, 0);
  DisplayChoices[dcNone] := FIniFile.ReadInteger(StrDisplayOption, StrDisplayNone, 0);

  Keys := TStringList.Create;
  try
    IniFile.ReadSection(MRU_Section, Keys);
    for Index := Keys.Count -1 downto 0 do
    begin
      FileName := IniFile.ReadString(MRU_Section, Keys[Index], '');
      if FileExists(FileName) then
      begin
        MostRecentlyUsed.AddFileName(FileName);
      end;
    end;
    PhastModel.ProgramLocations.ReadFromIniFile(IniFile);
    N5.Visible :=  MostRecentlyUsed.MenuItemCount > 0;
    N6.Visible := N5.Visible;

    WebIniFileName := InternetIniFileName(Handle, Application.ExeName);
    if not FileExists(WebIniFileName) then
    begin
      WebIniFile:= TMemInifile.Create(WebIniFileName);
      try
        FIniFile.ReadSection(StrVideoDisplayed, Keys);
        for Index := 0 to Keys.Count - 1 do
        begin
          ShownURL := FIniFile.ReadBool(StrVideoDisplayed, Keys[Index], False);
          WebIniFile.WriteBool(StrVideoDisplayed, Keys[Index], ShownURL);
        end;
        LastTipDate := FIniFile.ReadDateTime(StrCustomization, StrTipDate, 0);
        WebIniFile.WriteDateTime(StrCustomization, StrTipDate, LastTipDate);
        LastCheckInternetDate := FIniFile.ReadDateTime(StrCustomization, StrInternetCheckDate, LastTipDate);
        WebIniFile.WriteDateTime(StrCustomization, StrInternetCheckDate, LastCheckInternetDate);


        WebIniFile.UpdateFile;
      finally
        WebIniFile.Free;
      end;
    end;

  finally
    Keys.Free;
  end;
end;

procedure TfrmGoPhast.WarningsandErrors1Click(Sender: TObject);
begin
  inherited;
  frmErrorsAndWarnings.Show;
end;

procedure TfrmGoPhast.WMMenuSelect(var Msg: TWMMenuSelect);
var
  menuItem : TMenuItem;
  hSubMenu : HMENU;
begin
  inherited; // from TCustomForm

  menuItem := nil;
  if (Msg.MenuFlag <> $FFFF) or (Msg.IDItem <> 0) then
  begin
    if Msg.MenuFlag and MF_POPUP = MF_POPUP then
    begin
      hSubMenu := GetSubMenu(Msg.Menu, Msg.IDItem);
      try
        menuItem := Self.Menu.FindItem(hSubMenu, fkHandle);
      except on ERangeError do
        menuItem := nil;
      end;
    end
    else
    begin
      try
        menuItem := Self.Menu.FindItem(Msg.IDItem, fkCommand);
      except on ERangeError do
        menuItem := nil;
      end;
    end;
    // Modified from http://delphi.about.com/od/vclusing/a/menuitemhints.htm
    // Only display hint windows for menu items for
    // the most recently opened files.
    if (menuItem <> nil) and not (menuItem is TRecentFileMenuItem) then
    begin
      menuItem := nil;
    end;
  end;

  miHint.DoActivateHint(menuItem);
end;

procedure TfrmGoPhast.WriteIniFile;
var
  FileName: string;
  Index: integer;
begin
  for Index := 0 to MostRecentlyUsed.FileNames.Count -1 do
  begin
    FileName := MostRecentlyUsed.FileNames[Index];
    IniFile.WriteString(MRU_Section,
      'FileName' + IntToStr(Index), FileName);
  end;
//  FIniFile.WriteBool(StrModelMate, StrEnableModelMate, ShowUcodeInterface);

  While (DisplayChoices[dcColor] > MaxDisplayChoiceCount)
    or (DisplayChoices[dcContour] > MaxDisplayChoiceCount)
    or (DisplayChoices[dcNone] > MaxDisplayChoiceCount)
    do
  begin
    DisplayChoices[dcColor] := DisplayChoices[dcColor] div 2;
    DisplayChoices[dcContour] := DisplayChoices[dcContour] div 2;
    DisplayChoices[dcNone] := DisplayChoices[dcNone] div 2;
  end;

  FIniFile.WriteInteger(StrDisplayOption, StrColor, DisplayChoices[dcColor]);
  FIniFile.WriteInteger(StrDisplayOption, StrContour, DisplayChoices[dcContour]);
  FIniFile.WriteInteger(StrDisplayOption, StrDisplayNone, DisplayChoices[dcNone]);

  PhastModel.ProgramLocations.WriteToIniFile(IniFile);
  try
    IniFile.UpdateFile;
  except on EFileStreamError do
    begin
      // Ignore errors saving ini files.
    end;
  end;
end;

procedure TfrmGoPhast.ZONEBUDGETInputFiles1Click(Sender: TObject);
var
  FileName: string;
begin
  inherited;
  if not PhastModel.ModflowPackages.ZoneBudget.IsSelected then
  begin
    Beep;
    MessageDlg('You must activate ZONEBUDGET in the MODFLOW Packages and '
      + 'Programs dialog box before running ZONEBUDGET.',
      mtWarning, [mbOK], 0);
    Exit;
  end;

  if (sdZonebudgetInput.FileName = '') and (sdModflowInput.FileName <> '') then
  begin
    sdZonebudgetInput.FileName := ChangeFileExt(sdModflowInput.FileName,
      sdZonebudgetInput.DefaultExt);
  end;
  if (sdZonebudgetInput.FileName = '') and (sdSaveDialog.FileName <> '') then
  begin
    sdZonebudgetInput.FileName := ChangeFileExt(sdSaveDialog.FileName,
      sdZonebudgetInput.DefaultExt);
  end;
  if sdZonebudgetInput.Execute then
  begin
    if not FileExists(PhastModel.ProgramLocations.ZoneBudgetLocation) then
    begin
      ShowAForm(TfrmProgramLocations);
      if not FileExists(PhastModel.ProgramLocations.ZoneBudgetLocation) then
      begin
        Beep;
        if MessageDlg('ZONEBUDGET does not exist at the location you specified.  '
          + 'Do you still want to export the ZONEBUDGET input files?', mtWarning,
          [mbYes, mbNo], 0) <> mrYes then
        begin
          Exit;
        end;
      end;
    end;

    FileName := sdZonebudgetInput.FileName;
    frmFormulaErrors.sgErrors.BeginUpdate;
    try
      PhastModel.ExportZoneBudgetModel(FileName, FRunZoneBudget);
    finally
      frmFormulaErrors.sgErrors.EndUpdate;
    end;

    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  end;

end;

procedure TfrmGoPhast.AdjustToolbarPositions(FirstToolBar,
  SecondToolBar: TToolBar);
begin
  SecondToolBar.Left := FirstToolBar.Left + FirstToolBar.Width + 13;
end;

procedure TfrmGoPhast.NewPosition(Sender: TObject; NewPosition: TPositionStorage);
begin
  Inc(FSynchronizeCount);
  try
    frameTopView.ZoomBox.Magnification := NewPosition.Top.Magnification;
    frameFrontView.ZoomBox.Magnification := NewPosition.Front.Magnification;
    frameSideView.ZoomBox.Magnification := NewPosition.Side.Magnification;
    SetTopPosition(NewPosition.Top.XCenter, NewPosition.Top.YCenter);
    SetFrontPosition(NewPosition.Front.XCenter, NewPosition.Front.YCenter);
    SetSidePosition(NewPosition.Side.XCenter, NewPosition.Side.YCenter);

    acPositionForward.Enabled := FPositionList.CanRedo;
    acPositionBackward.Enabled := FPositionList.CanUndo;
  finally
    Dec(FSynchronizeCount);
  end;
end;

procedure TfrmGoPhast.FormCreate(Sender: TObject);
var
  AFont: TFont;
  OpenedFile: boolean;
  FileName: string;
  Option: string;
  ExportAFile: boolean;
  CloseFile: boolean;
  AComponent: TComponent;
  ComponentIndex: integer;
  WorkAreaRect: TRect;
  WorkAreaWidth: Integer;
  WorkAreaHeight: Integer;
  HelpFileName: string;
  NewFileName: string;
  NameWriter: TNameFileWriter;
  Index: Integer;
  ChildModel: TChildModel;
  ChildModelNameFile: string;
begin
  inherited;
  // Some laptops of the Dept. of the Interior contract have a
  // screen height of 600 pixels so ensure that the height of the main
  // form is never more than that.
  Assert(Height <= 600);

  frameTopView.miEditSelectedObjects.Action := acEditSelecteObjects;
  frameFrontView.miEditSelectedObjects.Action := acEditSelecteObjects;
  frameSideView.miEditSelectedObjects.Action := acEditSelecteObjects;

  FPositionList := TPositionList.Create(100);
  FPositionList.OnNewPosition := NewPosition;
  FRunModflow := True;
  FRunModpath := True;
  FRunZoneBudget := True;
  FRunPhast := True;
  FRunModelMate := True;
  FSynchronizeCount := 0;
  FCreatingMainForm := True;
  try
    {$IFDEF MSWINDOWS}
      // Because the CLX Screen.Height doesn't take the taskbar into account,
      // use the VCL Screen object instead under windows to determine the available
      // space on the screen.
      WorkAreaRect := Monitor.WorkareaRect;
    {$ELSE}
    {$IFDEF LINUX}
      // With Linux, the screen area and the available work area are the same.
      // Use the CLX screen object to determine them.
      WorkAreaRect.Top := 0;
      WorkAreaRect.Left := 0;
      WorkAreaRect.Right := Screen.Width;
      WorkAreaRect.Bottom := Screen.Height;
    {$ELSE}
      Assert(False);
    {$ENDIF}
    {$ENDIF}
    WorkAreaWidth := WorkAreaRect.Right - WorkAreaRect.Left;
    WorkAreaHeight := WorkAreaRect.Bottom - WorkAreaRect.Top;
    if Width > WorkAreaWidth then
    begin
      Width := WorkAreaWidth;
    end;
    if Height > WorkAreaHeight then
    begin
      Height := WorkAreaHeight;
    end;

    FCreateArchive := True;
    CreateArchiveSet := False;
//    Application.HelpFile := ChangeFileExt(Application.ExeName, '.chm');
    Caption := StrModelName;
    miHint := TMenuItemHint.Create(self);

    ExistingColumnSelectionCellColor := frameSideView.ModelCube.SelectionColor;
    ExistingRowSelectionCellColor := frameFrontView.ModelCube.SelectionColor;
    ExistingLayerSelectionCellColor := frameTopView.ModelCube.SelectionColor;

    SbMainHeight := sbMain.Height;
    ImageDLLLoader.Default.FindDLLs(ProgramPath);
    UndoStack.UndoToolButton := tbUndo;
    UndoStack.RedoToolButton := tbRedo;
    PopUpMode := pmNone;
    Popupparent := nil;
    HelpFileName := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName));
    HelpFileName := IncludeTrailingPathDelimiter(HelpFileName + 'Help');
    HelpFileName := HelpFileName + ExtractFileName(Application.ExeName);
    Application.HelpFile := ChangeFileExt(HelpFileName, '.chm');
    CanEdit := True;
    // Visual CLX patch version 3.10 doesn't work.
    // Shortcuts are broken and something is wrong with redrawing after
    // minimizing and then maximizing.
  //  Assert(PatchedVCLX = 3.9);

    // Adjust the toolbar postions so their isn't blank space between them.
    AdjustToolbarPositions(tbarFile, tbarEdit);
    AdjustToolbarPositions(tbarEdit, tbarEditScreenObjects);
    AdjustToolbarPositions(tbarEditScreenObjects, tbarView);

    Application.OnActivate := BringFormsToFront;

    CanDraw := True;
    FileFormat := ffAscii;
    CreatePhastModel;
    ReadIniFile;

    // make sure the Panels aren't a weird size.

    frameFrontView.ZoomBox.Exaggeration := 20;
    frameSideView.ZoomBox.Exaggeration := 20;

    frameTopView.ZoomBox.Magnification := 0.014;
    frameTopView.ZoomBox.OriginX := -1000.0;
    frameTopView.ZoomBox.OriginY := -1000.0;

    frameFrontView.ZoomBox.Magnification := 0.04;
    frameFrontView.ZoomBox.OriginX := -1000.0;
    frameFrontView.ZoomBox.OriginY := -20;

    frameSideView.ZoomBox.Magnification := 0.015;
    frameSideView.ZoomBox.OriginY := -1000.0;
    frameSideView.ZoomBox.OriginX := -80;

    // Make sure that everything is the right color.
    pnlBottom.ParentColor := True;
    frameFrontView.ZoomBox.ParentColor := True;
    //  pnlLowerRight.ParentColor := True;
    pnlTop.ParentColor := True;
    frameTopView.ZoomBox.ParentColor := True;
    frameSideView.ZoomBox.ParentColor := True;

    frameTopView.ViewDirection := vdTop;
    frameFrontView.ViewDirection := vdFront;
    framesideView.ViewDirection := vdSide;

    // Make sure the rulers are synchronized with the views of the model.
    AdjustScales;

    // create the grid.
    PhastGrid.OnSelectedLayerChange := frameTopView.ItemChange;
    PhastGrid.OnSelectedColumnChange := frameSideView.ItemChange;
    PhastGrid.OnSelectedRowChange := frameFrontView.ItemChange;
    ModflowGrid.OnSelectedLayerChange := frameTopView.ItemChange;
    ModflowGrid.OnSelectedColumnChange := frameSideView.ItemChange;
    ModflowGrid.OnSelectedRowChange := frameFrontView.ItemChange;

    // Adjust the paint box sizes.
    if frameTopView <> nil then
    begin
      frameTopView.ZoomBox.ZoomBy(1);
    end;
    if frameFrontView <> nil then
    begin
      frameFrontView.ZoomBox.ZoomBy(1);
    end;
    if frameSideView <> nil then
    begin
      frameSideView.ZoomBox.ZoomBy(1);
    end;
    Cursor := crArrow;

    // update the cursors.
    dcMoveColCursor.RedrawCursor;
    dcMoveRowCursor.RedrawCursor;
    dcAddColCursor.RedrawCursor;
    dcAddRowCursor.RedrawCursor;
    dcSubdivide.RedrawCursor;
    dcSetSpacing.RedrawCursor;

  {$IFDEF LINUX}
    // The Arial font on Linux doesn't look good.
    // Use Helvetica instead.
    Font.Name := 'Helvetica';
  {$ENDIF}

    AFont := TFont.Create;
    try
      AFont.Assign(Font);
      AFont.Size := AFont.Size - 2;
      Font := AFont;
      GlobalFont := Font;
  //    Application.Font := Font;
      AFont.Size := AFont.Size + 2;
      Font := AFont;
    finally
      AFont.Free;
    end;
    GlobalColor := Color;

  //  Application.Font := Font;
  //  for Index := 0 to ComponentCount - 1 do
  //  begin
  //    AComponent := Components[Index];
  //    if AComponent is TMenuItem then
  //    begin
  //      AMenuItem := TMenuItem(AComponent);
  //      QPopupMenu_setFont(AMenuItem.Handle, Font.Handle);
  //    end;
  //  end;

    // Set the icon for the application.
//    Application.Icon := Icon;

    // Make sure the undo stack is updated.
    Application.OnIdle := OnAppIdle;

    // Make sure the background color of the hints is white.
    // (On some systems, it is black.)
    Application.HintColor := clWhite;

    // make Sure the color of the font used to draw hints is
    // black instead of white.
  //  Application.Style.DrawHint := DrawHint;

    // Show the hint in the status bar.
    Application.OnHint := ShowHint;

    PhastModel.UpToDate := True;

//    frmScreenObjectProperties := TfrmScreenObjectProperties.Create(Application);

    OpenedFile := False;
    if ParamCount > 0 then
    begin
      try
      FileName := ParamStr(1);
      if FileExists(FileName) then
      begin
        PhastModel.UpToDate := True;
        try
          OpenAFile(FileName);
          OpenedFile := True;
        except on E: Exception do
          begin
            Beep;
            MessageDlg(E.Message, mtError, [mbOK], 0);
            raise;
          end;
        end;
        if ParamCount > 1 then
        begin
          ExportAFile := False;
          CloseFile := False;
          Option := ParamStr(2);
          if (Length(Option) > 0) and (Option[1] = '-') then
          begin
            Option := Copy(Option, 2, MAXINT);
            if LowerCase(Option) = 'e' then
            begin
              ExportAFile := True;
            end;
          end;
          if ParamCount > 2 then
          begin
            Option := ParamStr(3);
            if (Length(Option) > 0) and (Option[1] = '-') then
            begin
              Option := Copy(Option, 2, MAXINT);
              if LowerCase(Option) = 'c' then
              begin
                CloseFile := True;
              end;
            end;
          end;
          if ExportAFile then
          begin
            case PhastModel.ModelSelection of
              msUndefined: Assert(False);
              msPhast:
                begin
                  ExportFile(ChangeFileExt(FileName, sdPhastInput.DefaultExt),
                    False);
                end;
              msModflow:
                begin
                  NewFileName := ChangeFileExt(FileName, '.nam');
                  NewFileName := PhastModel.FixFileName(NewFileName);
                  NameWriter := TNameFileWriter.Create(PhastModel, NewFileName);
                  try
                    PhastModel.NameFileWriter := NameWriter;
                    PhastModel.ExportModflowModel(
                      NewFileName, False);
                  finally
                    NameWriter.Free;
                    PhastModel.NameFileWriter := nil;
                  end;
                  if PhastModel.ModflowPackages.ModPath.IsSelected then
                  begin
                    PhastModel.ExportModpathModel(
                      ChangeFileExt(FileName, '.mpn'), False, True);
                  end;
                  if PhastModel.ModflowPackages.ZoneBudget.IsSelected then
                  begin
                    PhastModel.ExportZoneBudgetModel(
                      ChangeFileExt(FileName, '.zon'), False);
                  end;
                end;
              msModflowLGR:
                begin
                  NewFileName := PhastModel.FixFileName(ChangeFileExt(FileName, '.lgr'));

                  NameWriter := TNameFileWriter.Create(PhastModel, FileName);
                  try
                    PhastModel.NameFileWriter := NameWriter;
                    for Index := 0 to PhastModel.ChildModels.Count - 1 do
                    begin
                      ChildModel := PhastModel.ChildModels[Index].ChildModel;
                      ChildModelNameFile := ChildModel.Child_NameFile_Name(FileName);
                      NameWriter := TNameFileWriter.Create(ChildModel, ChildModelNameFile);
                      ChildModel.NameFileWriter := NameWriter;
                    end;
                    PhastModel.ExportModflowLgrModel(NewFileName, False);
                  finally
                    PhastModel.NameFileWriter.Free;
                    PhastModel.NameFileWriter := nil;
                    for Index := 0 to PhastModel.ChildModels.Count - 1 do
                    begin
                      ChildModel := PhastModel.ChildModels[Index].ChildModel;
                      ChildModel.NameFileWriter.Free;
                      ChildModel.NameFileWriter := nil;
                    end;
                  end;


                  if PhastModel.ModflowPackages.ModPath.IsSelected then
                  begin
                    PhastModel.ExportModpathModel(
                      ChangeFileExt(FileName, '.mpn'), False, True);
                  end;
                  if PhastModel.ModflowPackages.ZoneBudget.IsSelected then
                  begin
                    PhastModel.ExportZoneBudgetModel(
                      ChangeFileExt(FileName, '.zon'), False);
                  end;
                end;
              else Assert(False);
            end;
          end;
          if CloseFile then
          begin
            Application.Terminate;
          end;
        end;
      end
      else
      begin
        MessageDlg('"' + FileName + '" does not exist.',
          mtError, [mbOK], 0);
      end;
      except On E: Exception do
        begin
          Application.Terminate;
        end;

      end;
    end;

    if not OpenedFile then
    begin
      if ShowAForm(TfrmStartUp) <> mrOK then
      begin
        Exit
      end;
    end;
    FPositionList.Clear;
    StoreInitalPosition;

    FOldWidth := Width;
    FOldHeight := Height;

    tbSelectClick(nil);
    frame3DView.SetDefaultOrientation;

    // Save the widths of the TToolBar components.
    for ComponentIndex := 0 to ComponentCount - 1 do
    begin
      AComponent := Components[ComponentIndex];
      if AComponent is TToolBar then
      begin
        AComponent.Tag := TToolBar(AComponent).Width;
      end;
    end;
    CheckInternet;
  finally
    FCreatingMainForm := false;
  end;
end;


procedure TfrmGoPhast.FormResize(Sender: TObject);
begin
  // When the form is resized, synchronize the rulers with the views
  // of the model.

  if frameSideView.Width > ClientWidth - splitVertTop.Width then
  begin
    frameSideView.Width := ClientWidth - splitVertTop.Width;
    frame3DView.Width := frameSideView.Width;
  end;

  if pnlBottom.Height > ClientHeight - cbControlBar.Height
    - splitHoriz.Height then
  begin
    pnlBottom.Height := ClientHeight - cbControlBar.Height - splitHoriz.Height;
  end;

  AdjustScales;

  if WindowState = wsNormal then
  begin
    FOldWidth := Width;
    FOldHeight := Height;
  end;

  { TODO :
When maximizing, make non modal forms (TfrmSelectedObjects,
TfrmShowHideObjects) stay on top (or else give the user the ability to 
make them stay on top). See TForm.FormStyle}
  inherited;
end;

procedure TfrmGoPhast.tbPanClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbPan.OnMouseDown(tbPan, mbLeft, [ssLeft], 0, 0);
  end;

  if tbPan.Down then
  begin
    // Try to start panning.
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbPan);

    SetZB_Cursors(crHandFlat);
  end
  else
  begin
    SetZB_Cursors(crArrow);
  end;
  CurrentTool := PanTool;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbZoomClick(Sender: TObject);
var
  CanZoomIn: boolean;
  Procedure SetCursor(Frame: TframeView);
  begin
    if (Frame <> nil)
      and (Frame.ZoomBox.CanZoomOut) then
    begin
      Frame.ZoomBox.Cursor := crZoom;
      Frame.ZoomBox.Image32.Cursor := crZoom;
    end;
  end;
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbZoom.OnMouseDown(tbZoom, mbLeft, [ssLeft], 0, 0);
  end;

  if tbZoom.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbZoom);

    CanZoomIn := (frameTopView.ZoomBox.CanZoomIn)
      or (frameFrontView.ZoomBox.CanZoomIn)
      or (frameSideView.ZoomBox.CanZoomIn);
    SetZB_Cursors(crArrow);
    // Set the cursor for the top view.
    SetCursor(frameTopView);

    // Set the cursor for the front view.
    SetCursor(frameFrontView);

    // Set the cursor for the side view.
    SetCursor(frameSideView);

    if not CanZoomIn then
    begin
      tbZoom.Down := False;
      Beep;
      MessageDlg('All of the views are already zoomed in as far as they '
        + 'can go.',
        mtInformation, [mbOK], 0, mbOK);
      Exit;
    end;
    CurrentTool := ZoomTool;
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbZoomOutClick(Sender: TObject);
var
  CanZoomOut: boolean;
  Procedure SetCursor(Frame: TframeView);
  begin
    if (Frame <> nil)
      and (Frame.ZoomBox.CanZoomOut) then
    begin
      Frame.ZoomBox.Cursor := crZoomOut;
      Frame.ZoomBox.Image32.Cursor := crZoomOut;
    end;
  end;
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbZoomOut.OnMouseDown(tbZoomOut, mbLeft, [ssLeft], 0, 0);
  end;

  if tbZoomOut.Down then
  begin
    CanZoomOut := (frameTopView.ZoomBox.CanZoomOut)
      or (frameFrontView.ZoomBox.CanZoomOut)
      or (frameSideView.ZoomBox.CanZoomOut);
    // Make sure all buttons except the current one are up.
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbZoomOut);

    // Set the cursors.
    SetZB_Cursors(crArrow);

    // Set the cursor for the top view.
    SetCursor(frameTopView);

    // Set the cursor for the front view.
    SetCursor(frameFrontView);

    // Set the cursor for the side view.
    SetCursor(frameSideView);

    if not CanZoomOut then
    begin
      tbZoomOut.Down := False;
      Beep;
      MessageDlg('All of the views are already zoomed out as far as they '
        + 'can go.',
        mtInformation, [mbOK], 0, mbOK);
      Exit;
    end;

    CurrentTool := ZoomOutTool;

  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.FormDeactivate(Sender: TObject);
begin
  inherited;
  // allow cut, copy, and paste on other non-modal forms to work properly
  // by deactivating shortcuts when the main form becomes inactive.
  // Re-enable the Shortcuts in FormActivate.
  acCut.ShortCut := 0;
  acCopy.ShortCut := 0;
  acPaste.ShortCut := 0;
end;

procedure TfrmGoPhast.FormDestroy(Sender: TObject);
begin
//  OutputDebugString('SAMPLING ON');
  WriteIniFile;
  IniFile.Free;
  FPositionList.Free;

  // Hide doesn't work when the application is shutting down.

  {
  // It might take a while to completely get rid of the grid and objects
  // so hide the forms first.
  if frmShowHideObjects <> nil then
  begin
    frmShowHideObjects.Hide;
  end;
  Hide;
  }

  UndoStack.Clear;
  // Get rid of the model.
//  FreeAndNil(FPhastModel);
  // Formula manager needs FPhastModel to be defined during FPhastModel.Free;
  FPhastModel.Free;
  FPhastModel := nil;
  FPhastModel := nil;
  inherited;
//  OutputDebugString('SAMPLING OFF');
end;

procedure TfrmGoPhast.miVerticalExaggerationClick(Sender: TObject);
begin
  // change the vertical exaggeration of the model.
  ShowAForm(TfrmVerticalExaggeration);
end;

procedure TfrmGoPhast.AdjustScales;
begin
  // Synchronize the horizontal and vertical scales with the views of the grid.

  if frameTopView <> nil then
  begin
    frameTopView.AdjustScales;
  end;

  if frameFrontView <> nil then
  begin
    frameFrontView.AdjustScales;
  end;

  if frameSideView <> nil then
  begin
    frameSideView.AdjustScales;
  end;
end;

procedure TfrmGoPhast.tbPointClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbPoint.OnMouseDown(Sender, mbLeft, [ssLeft], 0, 0);
  end;

  if tbPoint.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbPoint);
    // Set the cursor.
    SetZB_Cursors(crPointArrow);
    CurrentTool := CreatePointScreenObjectTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.miZoomInClick(Sender: TObject);
var
  CanZoomIn: boolean;
  Procedure SetCursor(Frame: TframeView);
  begin
    if (Frame <> nil)
      and (Frame.ZoomBox.CanZoomIn) then
    begin
      Frame.ZoomBox.Cursor := crZoomIn;
      Frame.ZoomBox.Image32.Cursor := crZoomIn;
    end;
  end;
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbZoomIn.OnMouseDown(tbZoomIn, mbLeft, [ssLeft], 0, 0);
  end;

  if tbZoomIn.Down then
  begin
    CanZoomIn := (frameTopView.ZoomBox.CanZoomIn)
      or (frameFrontView.ZoomBox.CanZoomIn)
      or (frameSideView.ZoomBox.CanZoomIn);
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbZoomIn);
    SetZB_Cursors(crArrow);
    // Set the cursor for the top view.
    SetCursor(frameTopView);

    // Set the cursor for the front view.
    SetCursor(frameFrontView);

    // Set the cursor for the side view.
    SetCursor(frameSideView);
    // If you can't zoom in, warn the user.
    if not CanZoomIn then
    begin
      tbZoomIn.Down := False;
      Beep;
      MessageDlg('All of the views are already zoomed in as far as they '
        + 'can go.',
        mtInformation, [mbOK], 0, mbOK);
      Exit;
    end;
    CurrentTool := ZoomInTool;
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.miModelResultsClick(Sender: TObject);
begin
  inherited;
  with TfrmSelectResultToImport.Create(nil) do
  begin
    try
      if SelectFiles then
      begin
        ShowModal;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmGoPhast.ModelSelectionChange(Sender: TObject);
  procedure UpdateRunShortCut(Action: TAction);
  begin
    if Action.Enabled then
    begin
      Action.ShortCut := ShortCut(Word('E'), [ssCtrl]);
      tbRun.Action := Action;
    end
    else
    begin
      Action.ShortCut := 0;
    end;
  end;
begin
  case PhastModel.ModelSelection of
    msUndefined: ; // ignore
    msPhast:
      begin
        frameTopView.ModelCube.ZOrigin := zoBottom;
        frameFrontView.ModelCube.YOrigin := yoSouth;
        acPhastActive.Checked := True;
        acSubdivide.Caption := 'Subdivide Grid &Elements...';
        acSubdivide.Hint := 'Subdivide grid elements|'
          + 'Click down and drag to select elements to be subdivided.';
      end;
    msModflow:
      begin
        frameTopView.ModelCube.ZOrigin := zoTop;
        frameFrontView.ModelCube.YOrigin := yoNorth;
        acModflowActive.Checked := True;
        acSubdivide.Caption := 'Subdivide Grid &Cells...';
        acSubdivide.Hint := 'Subdivide grid cells|'
          + 'Click down and drag to select cells to be subdivided.';
      end;
    msModflowLGR:
      begin
        frameTopView.ModelCube.ZOrigin := zoTop;
        frameFrontView.ModelCube.YOrigin := yoNorth;
        acModflowLgrActive.Checked := True;
        acSubdivide.Caption := 'Subdivide Grid &Cells...';
        acSubdivide.Hint := 'Subdivide grid cells|'
          + 'Click down and drag to select cells to be subdivided.';
      end;
    else Assert(False);
  end;

  // update the cursors.
  dcMoveColCursor.RedrawCursor;
  dcMoveRowCursor.RedrawCursor;
  dcAddColCursor.RedrawCursor;
  dcAddRowCursor.RedrawCursor;
  dcSubdivide.RedrawCursor;
  dcSetSpacing.RedrawCursor;

  acImportModelResults.Enabled := PhastModel.ModelSelection in [msModflow, msModflowLGR];
  acExportModpath.Enabled := PhastModel.ModelSelection in [msModflow, msModflowLGR];
//  miExportPhast.Enabled := PhastModel.ModelSelection = msPhast;
  miImportDistributedDatabyZone.Enabled := PhastModel.ModelSelection = msPhast;
  miChildModels.Enabled := PhastModel.ModelSelection = msModflowLGR;

  acExportPhastInputFile.Enabled := PhastModel.ModelSelection = msPhast;
  acRunModflow.Enabled := PhastModel.ModelSelection = msModflow;
  acRunModflowLgr.Enabled := PhastModel.ModelSelection = msModflowLGR;

  UpdateRunShortCut(acExportPhastInputFile);
  UpdateRunShortCut(acRunModflow);
  UpdateRunShortCut(acRunModflowLgr);

  miLayers.Enabled := PhastModel.ModelSelection in [msModflow, msModflowLGR];
  EnableHufMenuItems;
  miGeneral.Enabled := PhastModel.ModelSelection in [msModflow, msModflowLGR];
  miTime.Enabled := PhastModel.ModelSelection in [msModflow, msModflowLGR];
  miOutputControl.Enabled := PhastModel.ModelSelection in [msModflow, msModflowLGR];
  miPackages.Enabled := PhastModel.ModelSelection in [msModflow, msModflowLGR];
  miProgramLocations.Enabled := PhastModel.ModelSelection in [msModflow, msModflowLGR];
  miManageParameters.Enabled := PhastModel.ModelSelection in [msModflow, msModflowLGR];
  miModflowNameFile.Enabled := PhastModel.ModelSelection in [msModflow, msModflowLGR];
  EnableLinkStreams;
  EnableManageFlowObservations;
  EnableManageHeadObservations;
  miObservationType.Enabled := PhastModel.ModelSelection in [msModflow, msModflowLGR];

  miTitleAndUnits.Enabled := PhastModel.ModelSelection = msPhast;
  miGridOptions.Enabled := PhastModel.ModelSelection = msPhast;
  miChemistryOptions.Enabled := PhastModel.ModelSelection = msPhast;
  miSolutionMethod.Enabled := PhastModel.ModelSelection = msPhast;
  miSteadyFlow.Enabled := PhastModel.ModelSelection = msPhast;
  miTimeControl.Enabled := PhastModel.ModelSelection = msPhast;
  miFreeSurface.Enabled := PhastModel.ModelSelection = msPhast;
  miPrintInitial.Enabled := PhastModel.ModelSelection = msPhast;
  miPrintFrequency.Enabled := PhastModel.ModelSelection = msPhast;
  miPHASTProgramLocation.Enabled := PhastModel.ModelSelection = msPhast;
end;

procedure TfrmGoPhast.miMF_HydrogeologicUnitsClick(Sender: TObject);
begin
  inherited;
  if PhastModel.HufParameters.Count > 0 then
  begin
    ShowAForm(TfrmHUF_Layers);
  end
  else
  begin
    Beep;
    MessageDlg('You must define some parameters for the HUF package in the '
      + 'Model|MODFLOW Packages and programs dialog box before you can '
      + 'display the MODFLOW Hydrogeologic Units dialog box.',
      mtWarning, [mbOK], 0);
  end;
end;

procedure TfrmGoPhast.miModflowNameFileClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmModflowNameFile);
end;

procedure TfrmGoPhast.miModpathEndpointsClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmEndPointDisplay);
end;

procedure TfrmGoPhast.miModpathPathlineClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmModpathDisplay);
end;

procedure TfrmGoPhast.ModflowReference1Click(Sender: TObject);
begin
  inherited;
  HelpRouter.HelpJump('', 'Introduction');
end;

procedure TfrmGoPhast.miModpathTimeSeriesClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmTimeSeriesDisplay);
end;

procedure TfrmGoPhast.CreatePhastModel;
begin
  PhastModel := TPhastModel.Create(self);
  PhastModel.OnGetZoomBox := GetZoomBox;
  PhastModel.OnGetCurrentScreenObject := GetCurrentScreenObject;
  PhastModel.OnConvertPoint := ConvertPoint;
  PhastModel.OnScreenObjectSelected := ScreenObjectSelectionChange;
  PhastModel.OnScreenObjectUnSelected := ScreenObjectSelectionChange;
  PhastModel.OnCheckScreenObject := CheckScreenObject;
  PhastModel.On3DViewChanged := Invalidate3DView;

  PhastModel.OnModelSelectionChange := ModelSelectionChange;
  PhastModel.OnScreenObjectsChanged := ScreenObjectsChanged;
  PhastModel.OnRefreshScreenObjects := UpdateDisplay;
  PhastModel.GuiSettings := TGuiSettings.Create;
  ModelSelectionChange(PhastModel);

  PhastModel.UpdateTimeLists;
  PhastModel.Name := 'PhastModel';
  PhastModel.UpToDate := True;

end;

procedure TfrmGoPhast.miDataSetstoCSVClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmExportCSV);
end;

procedure TfrmGoPhast.acDeleteColumnRowExecute(Sender: TObject);
begin
  inherited;
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbDeleteColumnRow.OnMouseDown(tbDeleteColumnRow, mbLeft, [ssLeft], 0, 0);
  end;

  if tbDeleteColumnRow.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbDeleteColumnRow);

    // Set the cursors.
    SetZB_Cursors(crArrow);
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
  end;
  CurrentTool := DeleteGridBoundaryTool;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.DrawSubdivideCursor(const AndImage: TBitmap;
  Angle: real; const CursorComponent: TRbwDynamicCursor);
const
  LineSeparation = 5.1;
var
  LineLength: real;
  X1, Y1, X2, Y2: integer;
  lsCa, llSa, lsSa, llCa: real;
  procedure DrawLine;
  begin
    with AndImage.Canvas do
    begin
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
    end
  end;
begin
  // This procedure is used to help draw the bitmaps for the
  // subdivide cursor for the top view.  The cursor is drawn with lines
  // that are parallel to the grid.
  while Angle > Pi do
  begin
    Angle := Angle - Pi;
  end;

  while Angle < -Pi / 4 do
  begin
    Angle := Angle + Pi;
  end;

  if Angle > Pi * 3 / 4 then
  begin
    Angle := Angle - Pi;
  end;

  LineLength := (CursorComponent.CursorHeight - 7) / 2 + 0.1;
  CursorComponent.HotPointX := CursorComponent.CursorWidth div 2;
  CursorComponent.HotPointY := CursorComponent.CursorHeight div 2;

  lsCa := LineSeparation * Cos(Angle);
  llSa := LineLength * Sin(Angle);
  lsSa := LineSeparation * Sin(Angle);
  llCa := LineLength * Cos(Angle);

  X1 := Round(CursorComponent.HotPointX + lsCa - llSa);
  Y1 := Round(CursorComponent.HotPointY - (lsSa + llCa));
  X2 := Round(CursorComponent.HotPointX + lsCa + llSa);
  Y2 := Round(CursorComponent.HotPointY - (lsSa - llCa));
  DrawLine;

  X1 := Round(CursorComponent.HotPointX - lsCa - llSa);
  Y1 := Round(CursorComponent.HotPointY - (-lsSa + llCa));
  X2 := Round(CursorComponent.HotPointX - lsCa + llSa);
  Y2 := Round(CursorComponent.HotPointY - (-lsSa - llCa));
  DrawLine;
end;

procedure TfrmGoPhast.DrawAddColRowCursor(const AndImage: TBitmap;
  Angle: real; const CursorComponent: TRbwDynamicCursor);
var
  LineLength: real;
  X1, Y1, X2, Y2: integer;
  llSa, llCa: real;
  procedure DrawLine;
  begin
    with AndImage.Canvas do
    begin
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
    end
  end;
begin
  // This procedure is used to draw the bitmaps for the
  // add-column or add-row cursor for the top view.
  // It is also used to help draw the subdivide cursor.
  // The cursor is drawn with lines
  // that are parallel to the grid.
  while Angle > Pi do
  begin
    Angle := Angle - Pi;
  end;

  while Angle < -Pi / 4 do
  begin
    Angle := Angle + Pi;
  end;

  if Angle > Pi * 3 / 4 then
  begin
    Angle := Angle - Pi;
  end;

  LineLength := (CursorComponent.CursorHeight - 5) / 2 + 0.1;
  CursorComponent.HotPointX := CursorComponent.CursorWidth div 2;
  CursorComponent.HotPointY := CursorComponent.CursorHeight div 2;

  llSa := LineLength * Sin(Angle);
  llCa := LineLength * Cos(Angle);

  X1 := Round(CursorComponent.HotPointX - llSa);
  Y1 := Round(CursorComponent.HotPointY - llCa);
  X2 := Round(CursorComponent.HotPointX + llSa);
  Y2 := Round(CursorComponent.HotPointY + llCa);
  DrawLine;
end;

procedure TfrmGoPhast.DrawMoveColRowCursor(const AndImage: TBitmap;
  Angle: real; const CursorComponent: TRbwDynamicCursor);
const
  LineSeparation = 3.1;
  ArrowLength = 7.1;
  ArrowHeadLength = 4.1;
var
  LineLength: real;
  X1, Y1, X2, Y2: integer;
  lsCa, llSa, lsSa, llCa: real;
  ArrowAngle: real;
  procedure DrawLine;
  begin
    with AndImage.Canvas do
    begin
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
    end
  end;
begin
  // This procedure is used to draw the bitmaps for the
  // move-column or move-row cursor for the top view.
  // The cursor is drawn with lines
  // that are parallel to the grid.
  while Angle > Pi do
  begin
    Angle := Angle - Pi;
  end;

  while Angle < -Pi / 4 do
  begin
    Angle := Angle + Pi;
  end;

  if Angle > Pi * 3 / 4 then
  begin
    Angle := Angle - Pi;
  end;

  LineLength := (CursorComponent.CursorHeight - 5) / 2 + 0.1;
  CursorComponent.HotPointX := CursorComponent.CursorWidth div 2;
  CursorComponent.HotPointY := CursorComponent.CursorHeight div 2;

  lsCa := LineSeparation * Cos(Angle);
  llSa := LineLength * Sin(Angle);
  lsSa := LineSeparation * Sin(Angle);
  llCa := LineLength * Cos(Angle);

  X1 := Round(CursorComponent.HotPointX + lsCa - llSa);
  Y1 := Round(CursorComponent.HotPointY - (lsSa + llCa));
  X2 := Round(CursorComponent.HotPointX + lsCa + llSa);
  Y2 := Round(CursorComponent.HotPointY - (lsSa - llCa));
  DrawLine;

  X1 := Round(CursorComponent.HotPointX - lsCa - llSa);
  Y1 := Round(CursorComponent.HotPointY - (-lsSa + llCa));
  X2 := Round(CursorComponent.HotPointX - lsCa + llSa);
  Y2 := Round(CursorComponent.HotPointY - (-lsSa - llCa));
  DrawLine;

  // draw arrows;
  X2 := Round(CursorComponent.HotPointX + 2 * lsCa);
  Y2 := Round(CursorComponent.HotPointY - 2 * lsSa);

  X1 := X2 + Round(ArrowLength * Cos(Angle));
  Y1 := Y2 - Round(ArrowLength * Sin(Angle));
  DrawLine;

  ArrowAngle := Angle + Pi / 4;
  X2 := X1 - Round(ArrowHeadLength * Cos(ArrowAngle));
  Y2 := Y1 + Round(ArrowHeadLength * Sin(ArrowAngle));
  DrawLine;

  ArrowAngle := Angle - Pi / 4;
  X2 := X1 - Round(ArrowHeadLength * Cos(ArrowAngle));
  Y2 := Y1 + Round(ArrowHeadLength * Sin(ArrowAngle));
  DrawLine;

  X2 := Round(CursorComponent.HotPointX - 2 * lsCa);
  Y2 := Round(CursorComponent.HotPointY + 2 * lsSa);
  X1 := X2 - Round(ArrowLength * Cos(Angle));
  Y1 := Y2 + Round(ArrowLength * Sin(Angle));
  DrawLine;

  ArrowAngle := Angle + Pi / 4;
  X2 := X1 + Round(ArrowHeadLength * Cos(ArrowAngle));
  Y2 := Y1 - Round(ArrowHeadLength * Sin(ArrowAngle));
  DrawLine;

  ArrowAngle := Angle - Pi / 4;
  X2 := X1 + Round(ArrowHeadLength * Cos(ArrowAngle));
  Y2 := Y1 - Round(ArrowHeadLength * Sin(ArrowAngle));
  DrawLine;

end;

procedure TfrmGoPhast.dcMoveColCursorDrawCursor(Sender: TObject; const ABitMap,
  AMask: TBitmap);
begin
  inherited;
  // Draw the bitmaps for the move-column cursor.
  if Grid <> nil then
  begin
    DrawMoveColRowCursor(ABitMap, Grid.GridAngle, dcMoveColCursor);
    AMask.Canvas.Pen.Width := 3;
    DrawMoveColRowCursor(AMask, Grid.GridAngle, dcMoveColCursor);
    //    DrawMask(ABitMap, AMask);
  end;
end;

procedure TfrmGoPhast.dcMoveRowCursorDrawCursor(Sender: TObject; const ABitMap,
  AMask: TBitmap);
begin
  inherited;
  // Draw the bitmaps for the move-row cursor.
  // Use the same code as for the move-column cursor except change the angle
  // by 90 degrees.
  if Grid <> nil then
  begin
    DrawMoveColRowCursor(ABitMap, Grid.GridAngle + Pi / 2,
      dcMoveRowCursor);
    AMask.Canvas.Pen.Width := 3;
    DrawMoveColRowCursor(AMask, Grid.GridAngle + Pi / 2, dcMoveRowCursor);
    //    DrawMask(ABitMap, AMask);
  end;
end;

procedure TfrmGoPhast.acModflowActiveExecute(Sender: TObject);
begin
  if frmGoPhast.ModelSelection <> msModflow then
  begin
    UndoStack.Submit(TUndoModelSelectionChange.Create(msModflow));
  end;
end;

procedure TfrmGoPhast.acModflowLgrActiveExecute(Sender: TObject);
begin
  inherited;
  if ModelSelection <> msModflowLGR then
  begin
    UndoStack.Submit(TUndoModelSelectionChange.Create(msModflowLGR));
    if PhastModel.ChildModels.Count = 0 then
    begin
      miChildModelsClick(nil);
    end;

  end;
end;

procedure TfrmGoPhast.acMoveColumnOrRowExecute(Sender: TObject);
begin
  inherited;
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbMove.OnMouseDown(tbMove, mbLeft, [ssLeft], 0, 0);
  end;

  if tbMove.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbMove);

    // Set the cursors
    SetZB_Cursors(crArrow);
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
  end;
  CurrentTool := MovingGridBoundaryTool;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.FilesToArchive1Click(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmFilesToArchive);
end;

procedure TfrmGoPhast.FillButtonList(AList: TList);
begin
  Assert(AList.Count = 0);
  AList.Add(tbZoomIn);
  AList.Add(tbZoomOut);
  AList.Add(tbZoom);
  AList.Add(tbPan);
  AList.Add(tbDeleteColumnRow);
  AList.Add(tbMove);
  AList.Add(tbAddVerticalBoundary);
  AList.Add(tbAddHorizontalBoundary);
  AList.Add(tbSubdivide);
  AList.Add(tbPoint);
  AList.Add(tbLine);
  AList.Add(tbPolygon);
  AList.Add(tbStraightLine);
  AList.Add(tbRectangle);
  AList.Add(tbSelect);
  AList.Add(tbLasso);
  AList.Add(tbSelectPoint);
  AList.Add(tbInsertPoint);
  AList.Add(tbDeleteSegment);
  AList.Add(tbGridAngle);
  AList.Add(tbSpacing);
  AList.Add(tbSelectColRowLayer);
  AList.Add(tbAddPartsToObject);
  AList.Add(tbAddLinesToObjects);
  AList.Add(tbAddPointsToObject);
  AList.Add(tbVertexValue);
end;

procedure TfrmGoPhast.SetButtonsUp(const CurrentButton: TObject);
var
  AList: TList;
  index: integer;
  AButton: TToolButton;
begin
  // terminated any screen objects that haven't been ended yet.
  frameTopView.FinishScreenObjects;
  frameFrontView.FinishScreenObjects;
  frameSideView.FinishScreenObjects;

  // Make a list of all the buttons that might need to be set up.
  AList := TList.Create;
  try
    FillButtonList(AList);

    // Go through the list and if a button
    // is not the current button then it
    // need to have it's Down property set to
    // false.  If the button has an associated
    // TAction, set the Checked state
    // of the TAction to false.  This will not only change
    // the Down state of the button but also the Checked
    // state of the associated menu item.
    for index := 0 to AList.Count - 1 do
    begin
      AButton := AList[Index];
      if AButton <> CurrentButton then
      begin
        if AButton.Action = nil then
        begin
          AButton.Down := False;
        end
        else
        begin
          AButton.Down := False;
          // Setting the Action.Checked to false is not
          // enough to get the button to redraw if OnClick
          // event does not also occur.  Setting AButton.Down
          // forces it to redraw for reasons unknown.
          (AButton.Action as TAction).Checked := false;
        end;
      end;
    end;
  finally
    AList.Free;
  end;
  CurrentTool := nil;
end;

procedure TfrmGoPhast.tbAddVerticalBoundaryClick(Sender: TObject);
begin
  inherited;
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbAddVerticalBoundary.OnMouseDown(tbAddVerticalBoundary, mbLeft, [ssLeft],
      0, 0);
  end;

  if tbAddVerticalBoundary.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbAddVerticalBoundary);

    // Set the cursors.
    SetZB_Cursors(crVertical);
    if frameTopView <> nil then
    begin
      frameTopView.ZoomBox.Cursor := dcAddColCursor.Cursor;
      frameTopView.ZoomBox.Image32.Cursor := dcAddColCursor.Cursor;
    end;
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
  end;
  if tbAddVerticalBoundary.Down or tbAddHorizontalBoundary.Down then
  begin
    CurrentTool := AddGridBoundaryTool;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbAddHorizontalBoundaryClick(Sender: TObject);
begin
  inherited;
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbAddHorizontalBoundary.OnMouseDown(tbAddHorizontalBoundary, mbLeft,
      [ssLeft], 0, 0);
  end;

  if tbAddHorizontalBoundary.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbAddHorizontalBoundary);

    // Set the cursors.
    SetZB_Cursors(crHorizontal);
    if frameTopView <> nil then
    begin
      frameTopView.ZoomBox.Cursor := dcAddRowCursor.Cursor;
      frameTopView.ZoomBox.Image32.Cursor := dcAddRowCursor.Cursor;
    end;
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
  end;
  if tbAddVerticalBoundary.Down or tbAddHorizontalBoundary.Down then
  begin
    CurrentTool := AddGridBoundaryTool;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.dcAddColCursorDrawCursor(Sender: TObject;
  const ABitMap, AMask: TBitmap);
begin
  inherited;
  // Draw the bitmaps for the add-column cursor.
  if Grid <> nil then
  begin
    DrawAddColRowCursor(ABitMap, Grid.GridAngle, dcAddColCursor);
    AMask.Canvas.Pen.Width := 3;
    DrawAddColRowCursor(AMask, Grid.GridAngle, dcAddColCursor);
  end;
end;

procedure TfrmGoPhast.dcAddRowCursorDrawCursor(Sender: TObject;
  const ABitMap, AMask: TBitmap);
begin
  inherited;
  // Draw the bitmaps for the add-row cursor.
  // Use the same code as for the add-column cursor except change the angle
  // by 90 degrees.
  if Grid <> nil then
  begin
    DrawAddColRowCursor(ABitMap, Grid.GridAngle + Pi / 2, dcAddRowCursor);
    AMask.Canvas.Pen.Width := 3;
    DrawAddColRowCursor(AMask, Grid.GridAngle + Pi / 2, dcAddRowCursor);
  end;

end;

procedure TfrmGoPhast.acSubdivideExecute(Sender: TObject);
begin
  inherited;
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbSubdivide.OnMouseDown(tbSubdivide, mbLeft, [ssLeft], 0, 0);
  end;

  if tbSubdivide.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbSubdivide);

    // Set the cursors.
    SetZB_Cursors(crSubdivide);
    if frameTopView <> nil then
    begin
      frameTopView.ZoomBox.Cursor := dcSubdivide.Cursor;
      frameTopView.ZoomBox.Image32.Cursor := dcSubdivide.Cursor;
    end;
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
  end;
  CurrentTool := SubdivideGridTool;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.acUndoExecute(Sender: TObject);
begin
  inherited;
  UndoStack.UndoEvent(Sender);
end;

procedure TfrmGoPhast.acVertexValueExecute(Sender: TObject);
begin
  inherited;
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbVertexValue.OnMouseDown(tbVertexValue, mbLeft, [ssLeft], 0, 0);
  end;

  if tbVertexValue.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbVertexValue);
    // Set the cursors.
    SetZB_Cursors(crVertexValue);
    // don't draw a rectangle around the selected screen objects.
    CurrentTool := EditVertexValueTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.dcSubdivideDrawCursor(Sender: TObject; const ABitMap,
  AMask: TBitmap);
begin
  inherited;
  // Draw the bitmaps for the Subdivide cursor.
  if Grid <> nil then
  begin
    DrawSubdivideCursor(ABitMap, Grid.GridAngle, dcSubdivide);
    DrawSubdivideCursor(ABitMap, Grid.GridAngle + Pi / 2, dcSubdivide);
    DrawAddColRowCursor(ABitMap, Grid.GridAngle, dcSubdivide);
    DrawAddColRowCursor(ABitMap, Grid.GridAngle + Pi / 2, dcSubdivide);
    AMask.Canvas.Pen.Width := 3;
    DrawSubdivideCursor(AMask, Grid.GridAngle, dcSubdivide);
    DrawSubdivideCursor(AMask, Grid.GridAngle + Pi / 2, dcSubdivide);
    DrawAddColRowCursor(AMask, Grid.GridAngle, dcSubdivide);
    DrawAddColRowCursor(AMask, Grid.GridAngle + Pi / 2, dcSubdivide);
  end;
end;

procedure TfrmGoPhast.SetZB_Cursors(const ACursor: TCursor);
begin
  // This sets the cursors for several controls at once.
  if frameTopView <> nil then
  begin
    frameTopView.ZoomBox.Cursor := ACursor;
    frameTopView.ZoomBox.Image32.Cursor := ACursor;
  end;
  if frameFrontView <> nil then
  begin
    frameFrontView.ZoomBox.Cursor := ACursor;
    frameFrontView.ZoomBox.Image32.Cursor := ACursor;
  end;
  if frameSideView <> nil then
  begin
    frameSideView.ZoomBox.Cursor := ACursor;
    frameSideView.ZoomBox.Image32.Cursor := ACursor;
  end;
end;

procedure TfrmGoPhast.InvalidateTop;
begin
  // redraw the top view.
  if frameTopView <> nil then
  begin
    frameTopView.ZoomBox.Image32.Invalidate;
  end;
end;

procedure TfrmGoPhast.InvalidateFront;
begin
  // redraw the front view.
  if frameFrontView <> nil then
  begin
    frameFrontView.ZoomBox.Image32.Invalidate;
  end;
end;

procedure TfrmGoPhast.InvalidateGrid;
begin
  InvalidateDataSets;
  TopGridChanged := True;
  FrontGridChanged := True;
  SideGridChanged := True;
  Grid.NeedToRecalculateCellColors;
  InvalidateAllViews;
  Grid.GridChanged;
end;

procedure TfrmGoPhast.InternalSaveFile(FileName: string);
const
  FiveMB = 5*1024*1024;
  OneSecond = 1/24/3600;
var
  CompressionStream: TCompressionStream;
  FileStream: TFileStream;
  MemStream: TMemoryStream;
  StartTime: Extended;
begin
  MemStream := TMemoryStream.Create;
  try
    try
      FileStream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite,
        ReadWritePermissions);
    except on EFCreateError do
      begin
        // try again after 1 second.
        StartTime := Now;
        repeat
        until (Now - StartTime > OneSecond);
        FileStream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite,
          ReadWritePermissions);
      end;
    end;
    try
      case FileFormat of
        ffAscii:
          begin
            MemStream.WriteComponent(PhastModel);
            PhastModel.ClearScreenObjectCollection;
            MemStream.Position := 0;
            ObjectBinaryToText(MemStream, FileStream);
            if not FSizeWarningDisplayed and (FileStream.Size > FiveMB) then
            begin
              FSizeWarningDisplayed := True;
              Beep;
              MessageDlg('If you want to save disk space, next time save '
                +'this file as a .mmZLib file instead of a .gpt file.',
                mtInformation, [mbOK], 0);
            end;
          end;
        ffBinary:
          begin
            FileStream.WriteComponent(PhastModel);
            PhastModel.ClearScreenObjectCollection;
          end;
        ffXML:
          begin
            MemStream.WriteComponent(PhastModel);
            PhastModel.ClearScreenObjectCollection;
            MemStream.Position := 0;
            rwObjectBinaryToXML(MemStream, FileStream);
          end;
        ffZLib:
          begin
            CompressionStream := TCompressionStream.Create(clMax, FileStream);
            try
              CompressionStream.WriteComponent(PhastModel);
            finally
              CompressionStream.Free;
            end;
            PhastModel.ClearScreenObjectCollection;
          end;
      else
        Assert(False);
      end;
    finally
      FileStream.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TfrmGoPhast.SetGridLineDrawingChoice(Sender: TObject);
begin
  if acShowAllGridLines.Checked then
  begin
    PhastGrid.GridLineDrawingChoice := gldcAll;
    ModflowGrid.GridLineDrawingChoice := gldcAll;
    tbShow2DGrid.ImageIndex := acShowAllGridLines.ImageIndex;
    miShow2DGridlines.ImageIndex := acShowAllGridLines.ImageIndex;
  end
  else if acShowExteriorGridLines.Checked then
  begin
    PhastGrid.GridLineDrawingChoice := gldcExterior;
    ModflowGrid.GridLineDrawingChoice := gldcExterior;
    tbShow2DGrid.ImageIndex := acShowExteriorGridLines.ImageIndex;
    miShow2DGridlines.ImageIndex := acShowExteriorGridLines.ImageIndex;
  end
  else if acShowActiveGridLines.Checked then
  begin
    PhastGrid.GridLineDrawingChoice := gldcActive;
    ModflowGrid.GridLineDrawingChoice := gldcActive;
    tbShow2DGrid.ImageIndex := acShowActiveGridLines.ImageIndex;
    miShow2DGridlines.ImageIndex := acShowActiveGridLines.ImageIndex;
  end
  else if acShowActiveEdge.Checked then
  begin
    PhastGrid.GridLineDrawingChoice := gldcActiveEdge;
    ModflowGrid.GridLineDrawingChoice := gldcActiveEdge;
    tbShow2DGrid.ImageIndex := acShowActiveEdge.ImageIndex;
    miShow2DGridlines.ImageIndex := acShowActiveEdge.ImageIndex;
  end
  else
  begin
    Assert(False);
  end;
  UpdateDisplay(nil);
end;

procedure TfrmGoPhast.CancelCurrentScreenObject;
begin
  if CurrentTool is TCustomCreateScreenObjectTool then
  begin
    CanEdit := False;
    try
      TCustomCreateScreenObjectTool(CurrentTool).FinishScreenObjects;
    finally
      CanEdit := True;
    end;
  end;
end;

procedure TfrmGoPhast.EnableDeleteImage;
begin
  miDeleteImage.Enabled := frmGoPhast.PhastModel.Bitmaps.Count > 0;
end;

procedure TfrmGoPhast.EnableHufMenuItems;
begin
  miMF_HydrogeologicUnits.Enabled := (PhastModel.ModelSelection in [msModflow, msModflowLGR])
    and PhastModel.ModflowPackages.HufPackage.IsSelected;
end;

//procedure TfrmGoPhast.SetVisibilityOfModelMateActions;
//begin
//  acImportModelMate.Visible := FShowUcodeInterface;
//  acExportModelMate.Visible := FShowUcodeInterface;
//end;

procedure TfrmGoPhast.InitializeModflowInputDialog;
begin
  case PhastModel.ObservationPurpose of
    ofObserved: sdModflowInput.FileName := ObservationFileName[sdModflowInput];
    ofPredicted: sdModflowInput.FileName := PredictionFileName[sdModflowInput];
    else Assert(False);
  end;
end;

procedure TfrmGoPhast.InitializeModflowLgrInputDialog;
begin
  case PhastModel.ObservationPurpose of
    ofObserved: sdModflowLgr.FileName := ObservationFileName[sdModflowLgr];
    ofPredicted: sdModflowLgr.FileName := PredictionFileName[sdModflowLgr];
    else Assert(False);
  end;
end;

procedure TfrmGoPhast.ReadModelMateProject(FileName: string; ModelMateProject: TProject);
var
  MemStream: TMemoryStream;
  FileStream: TFileStream;
  TextStream: TMemoryStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  MemStream := TMemoryStream.Create;
  TextStream := TMemoryStream.Create;
  try
    FileStream.Position := 0;
    TextStream.CopyFrom(FileStream, FileStream.Size);
    TextStream.Position := 0;
    ObjectTextToBinary(TextStream, MemStream);
    MemStream.Position := 0;
    MemStream.ReadComponent(ModelMateProject);
    ModelMateProject.FileName := FileName;
  finally
    TextStream.Free;
    MemStream.Free;
    FileStream.Free;
  end;
end;

procedure TfrmGoPhast.StoreInitalPosition;
var
  NewPosition: TPositionStorage;
  Z: Double;
  ZInt: Integer;
  RotatedCenterPoint: TPoint2D;
  RealCenterPoint: TPoint2D;
  CenterPoint: TPoint;
begin
  CenterPoint.x := frameTopView.ZoomBox.Width div 2;
  CenterPoint.Y := frameTopView.ZoomBox.Height div 2;
  RealCenterPoint.x := frameTopView.ZoomBox.X(CenterPoint.x);
  RealCenterPoint.Y := frameTopView.ZoomBox.Y(CenterPoint.y);
  RotatedCenterPoint := Grid.RotateFromRealWorldCoordinatesToGridCoordinates(RealCenterPoint);
  ZInt := frameFrontView.ZoomBox.Height div 2;
  Z := frameFrontView.ZoomBox.Y(ZInt);

  NewPosition := TPositionStorage.Create;

  NewPosition.Top.XCenter := RealCenterPoint.x;
  NewPosition.Top.YCenter := RealCenterPoint.y;
  NewPosition.Top.Magnification := frameTopView.ZoomBox.Magnification;

  NewPosition.Front.XCenter := RotatedCenterPoint.x;
  NewPosition.Front.YCenter := Z;
  NewPosition.Front.Magnification := frameFrontView.ZoomBox.Magnification;

  NewPosition.Side.XCenter := RotatedCenterPoint.Y;
  NewPosition.Side.YCenter := Z;
  NewPosition.Side.Magnification := frameSideView.ZoomBox.Magnification;

  FPositionList.FList.Add(NewPosition);
  Inc(FPositionList.FCurrentPosition);

  acPositionBackward.Enabled := False;
  tbPositionRedo.Enabled := False;
end;

procedure TfrmGoPhast.SurferGridFile1Click(Sender: TObject);
begin
  inherited;
  with TfrmImportSurferGrdFile.Create(nil) do
  begin
    try
      if GetData then
      begin
        ShowModal;
      end;
    finally
      Free;
    end;
  end;

end;

procedure TfrmGoPhast.miPHASTProgramLocationClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmPhastLocation);

end;

procedure TfrmGoPhast.PlayIntroductoryVideo;
var
  URL: string;
begin
  URL := ExtractFileDir(ParamStr(0));
  if URL[Length(URL)] <> PathDelim then
  begin
    URL := URL + PathDelim;
  end;
  URL := URL + 'Videos\IntroductoryVideo\IntroductoryVideo.html';
  if FileExists(URL) then
  begin
    URL := FileNameToURL(URL);
  end
  else
  begin
    URL := 'http://water.usgs.gov/nrp/gwsoftware/ModelMuse/IntroductoryVideo/IntroductoryVideo.html';
  end;
  LaunchURL(FBrowser, URL);
end;

procedure TfrmGoPhast.EnableManageFlowObservations;
begin
  miManageFluxObservations.Enabled :=
    (PhastModel.ModelSelection in [msModflow, msModflowLGR])
    and (PhastModel.ChobIsSelected
    or PhastModel.DrobIsSelected
    or PhastModel.GbobIsSelected
    or PhastModel.RvobIsSelected);
end;

procedure TfrmGoPhast.EnableManageHeadObservations;
begin
  miManageHeadObservations.Enabled :=
    (PhastModel.ModelSelection in [msModflow, msModflowLGR])
    and PhastModel.HobIsSelected
end;

procedure TfrmGoPhast.EnableVisualization;
var
  LocalGrid: TCustomGrid;
begin
  LocalGrid := Grid;
  acColorGrid.Enabled := (LocalGrid <> nil)
    and (LocalGrid.ColumnCount > 0)
    and (LocalGrid.RowCount > 0)
    and (LocalGrid.LayerCount > 0);
  acContourData.Enabled := acColorGrid.Enabled;
end;

procedure TfrmGoPhast.miEndpointsatEndingLocationstoShapefileClick(
  Sender: TObject);
var
  FileName: string;
begin
  inherited;
  FileName := ChangeFileExt(PhastModel.ModelFileName, '');
  if FileName = '' then
  begin
    FileName := IncludeTrailingPathDelimiter(GetCurrentDir)
      + StrEndpointsAtEndshp;
  end
  else
  begin
    FileName := FileName + '_' + StrEndpointsAtEndshp;
  end;
  sdShapefile.FileName := FileName;
  if sdShapefile.Execute then
  begin
    PhastModel.EndPoints.Points.
      ExportShapefileAtEndingLocations(sdShapefile.FileName);
  end;
end;

procedure TfrmGoPhast.miEndpointsatStartingLocationstoShapefileClick(
  Sender: TObject);
var
  FileName: string;
begin
  inherited;
  FileName := ChangeFileExt(PhastModel.ModelFileName, '');
  if FileName = '' then
  begin
    FileName := IncludeTrailingPathDelimiter(GetCurrentDir)
      + StrEndpointsAtStartshp;
  end
  else
  begin
    FileName := FileName + '_' + StrEndpointsAtStartshp;
  end;
  sdShapefile.FileName := FileName;
  if sdShapefile.Execute then
  begin
    PhastModel.EndPoints.Points.
      ExportShapefileAtStartingLocations(sdShapefile.FileName);
  end;
end;

procedure TfrmGoPhast.EnableLinkStreams;
begin
  miLinkSFRStreams.Enabled := (PhastModel.ModelSelection in [msModflow, msModflowLGR])
    and PhastModel.ModflowPackages.SfrPackage.IsSelected;
end;

procedure TfrmGoPhast.ShowOrHideAllScreenObjects(ShowAll: Boolean);
var
  ScreenObject: TScreenObject;
  Count: Integer;
  Undo: TUndoShowHideScreenObject;
  Index: Integer;
begin
  Count := 0;
  Undo := TUndoShowHideScreenObject.Create;
  try
    for Index := 0 to PhastModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := PhastModel.ScreenObjects[Index];
      if ScreenObject.Visible <> ShowAll then
      begin
        Count := Undo.AddScreenObjectToChange(ScreenObject);
      end;
    end;
    Undo.SetPostSelection;
  finally
    if Count = 0 then
    begin
      Undo.Free;
    end
    else
    begin
      UndoStack.Submit(Undo);
    end;
  end;
end;

procedure TfrmGoPhast.UpdatePermanantDialogBoxAppearances;
begin
  if frmGridColor <> nil then
  begin
    frmGridColor.CustomizeControls;
  end;
  frmProgressMM.CustomizeControls;
  frmSelectedObjects.CustomizeControls;
  frmColors.CustomizeControls;
  frmErrorsAndWarnings.CustomizeControls;
  if frmGridValue <> nil then
  begin
    frmGridValue.CustomizeControls;
  end;
end;

procedure TfrmGoPhast.InvalidateSide;
begin
  // redraw the side view.
  if frameSideView <> nil then
  begin
    frameSideView.ZoomBox.Image32.Invalidate;
  end;
end;

procedure TfrmGoPhast.acGridAngleExecute(Sender: TObject);
begin
  inherited;
  // Allow the user to change the grid angle.
  Application.CreateForm(TfrmGridAngle, frmGridAngle);
  try
    frameTopView.DeltaGridAngle := 0;
    frmGridAngle.ShowModal;
  finally
    // There is a test to see if frmGridAngle is equal to nil
    // when drawing the rotated grid.
    FreeAndNil(frmGridAngle);
  end;
end;

procedure TfrmGoPhast.acEditGridLinesExecute(Sender: TObject);
begin
  inherited;
  // Allow the user the edit the positions of the grid lines.
  ShowAForm(TfrmGridSpacing);
end;

procedure TfrmGoPhast.acSmoothGridExecute(Sender: TObject);
begin
  inherited;
  // Allow the user the smooth the grid.
  ShowAForm(TfrmSmoothGrid);
end;

procedure TfrmGoPhast.miObjectstoShapefileClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmExportShapefileObjects);
end;

procedure TfrmGoPhast.OnAppIdle(Sender: TObject; var Done: Boolean);
begin
  // This assigns the event handlers to the undo/redo buttons and
  // undo/redo menu items.
  if UndoStack <> nil then
  begin
    UndoStack.SetUndoMenuItems(miUndo, miRedo);
  end;
end;

procedure TfrmGoPhast.SaveModelMateProject;
var
  FileStream: TFileStream;     // Text file defining one or more objects.
  MemStream: TMemoryStream;    // Temporarily hold an object.
  TextStream: TMemoryStream;   // Text form of a stream.
begin
  MemStream := TMemoryStream.Create;
  TextStream := TMemoryStream.Create;
  
  // Open a text file
  FileStream := TFileStream.Create(PhastModel.ModelMateProjectFileName, fmCreate);
  try
    //
    // Write the TProject data to the memory stream.
    MemStream.WriteComponent(PhastModel.ModelMateProject);
    //MemStream.WriteComponent(PCurrent.UcProject);
    MemStream.Position := 0;
    // Convert the memory stream to a text stream.
    ObjectBinaryToText(MemStream, TextStream);
    // write the TProject text stream to the text file.
    TextStream.Position := 0;
    FileStream.CopyFrom(TextStream, TextStream.Size);
    //
  finally
    // Free all streams.
    FileStream.Free;
    MemStream.Free;
    TextStream.Free;
  end;
end;

procedure TfrmGoPhast.acFontExecute(Sender: TObject);
begin
{ TODO : Make sure the status bar is resized to be appropriate for the
selected font. }  
  // Change the font for the application.
  // Other forms will copy the
  // font from frmGoPhast when they are created.
  fdFontDialog.Font := Font;
  if fdFontDialog.Execute then
  begin
    Font := fdFontDialog.Font;
    GlobalFont := Font;
    UpdatePermanantDialogBoxAppearances;
  end;
end;

procedure TfrmGoPhast.acAddLinesToObjectExecute(Sender: TObject);
begin
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbAddLinesToObjects.OnMouseDown(tbAddLinesToObjects, mbLeft, [ssLeft], 0, 0);
  end;

  if tbAddLinesToObjects.Down and tbAddLinesToObjects.Enabled then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbAddLinesToObjects);
    // Set the cursors.
    SetZB_Cursors(crMultiPartLine);
    // don't draw a rectangle around the selected screen objects.
    CurrentTool := AddLinePartTool;
  end
  else
  begin
    CurrentTool := nil;
    tbAddLinesToObjects.Down := False;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.acAddPointsToObjectExecute(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbAddPointsToObject.OnMouseDown(tbAddPointsToObject, mbLeft, [ssLeft], 0, 0);
  end;

  if tbAddPointsToObject.Down and tbAddPointsToObject.Enabled then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbAddPointsToObject);
    // Set the cursors.
    SetZB_Cursors(crMultiPartPoint);
    // don't draw a rectangle around the selected screen objects.
    CurrentTool := AddPointPartTool;
  end
  else
  begin
    CurrentTool := nil;
    tbAddPointsToObject.Down := False;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.acAddPolygonsToObjectExecute(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbAddPartsToObject.OnMouseDown(tbAddPartsToObject, mbLeft, [ssLeft], 0, 0);
  end;

  if tbAddPartsToObject.Down and tbAddPartsToObject.Enabled then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbAddPartsToObject);
    // Set the cursors.
    SetZB_Cursors(crMultiPartPolygon);
    // don't draw a rectangle around the selected screen objects.
    CurrentTool := AddPolygonPartTool;
  end
  else
  begin
    CurrentTool := nil;
    tbAddPartsToObject.Down := False;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.acColorExecute(Sender: TObject);
begin
  // Change the color for the application.
  // Other forms will copy the
  // color from frmGoPhast when they are created.
  cdColorDialog.Color := Color;
  if cdColorDialog.Execute then
  begin
    Color := cdColorDialog.Color;
    GlobalColor := Color;
    UpdatePermanantDialogBoxAppearances;
//    mmMainMenu.Color := Color;
  end;

end;

procedure TfrmGoPhast.acColorGridExecute(Sender: TObject);
begin
  inherited;
  if frmGridColor = nil then
  begin
    frmGridColor := TfrmGridColor.Create(nil);
  end;
  frmGridColor.Show;
  if frmGridColor.WindowState = wsMinimized then
  begin
    frmGridColor.WindowState := wsNormal;
  end;
  UpdateFrmGridColor;
  UpdateFrmContourData;
end;

procedure TfrmGoPhast.acContourDataExecute(Sender: TObject);
begin
  inherited;
  if frmContourData = nil then
  begin
    frmContourData := TfrmContourData.Create(nil);
  end;
  frmContourData.Show;
  if frmContourData.WindowState = wsMinimized then
  begin
    frmContourData.WindowState := wsNormal;
  end;
  UpdateFrmGridColor;
  UpdateFrmContourData;
end;

procedure TfrmGoPhast.acCopyExecute(Sender: TObject);
begin
  inherited;
  PhastModel.CopyScreenObjectsToClipboard;
end;

procedure TfrmGoPhast.acCutExecute(Sender: TObject);
var
  SelectedScreenObjects: TScreenObjectList;
  Index: Integer;
  AScreenObject: TScreenObject;
  UndoCutScreenObjects: TUndoCutScreenObjects;
begin
  inherited;
  SelectedScreenObjects := TScreenObjectList.Create;
  try
    SelectedScreenObjects.Capacity := PhastModel.ScreenObjectCount;
    for Index := PhastModel.ScreenObjectCount - 1 downto 0 do
    begin
      AScreenObject := PhastModel.ScreenObjects[Index] as TScreenObject;
      if AScreenObject.Selected then
      begin
        SelectedScreenObjects.Add(AScreenObject);
      end;
    end;
    if SelectedScreenObjects.Count > 0 then
    begin
      UndoCutScreenObjects := TUndoCutScreenObjects.Create(SelectedScreenObjects);
      UndoStack.Submit(UndoCutScreenObjects);
      UndoCutScreenObjects.SetPostSelection;
    end;
  finally
    SelectedScreenObjects.Free;
  end;
end;

procedure TfrmGoPhast.miAboutClick(Sender: TObject);
begin
  // Show the about box.
  if frmAbout = nil then
  begin
    Application.CreateForm(TfrmAbout, frmAbout);
    frmAbout.Show;
  end
  else
  begin
    frmAbout.SetFocus;
  end;
end;

procedure TfrmGoPhast.miAllVideosClick(Sender: TObject);
begin
  inherited;

  LaunchURL(FBrowser, VideoUrl);
end;

procedure TfrmGoPhast.miASCII_RasterFileClick(Sender: TObject);
begin
  inherited;
  with TfrmImportAsciiRaster.Create(nil) do
  begin
    try
      if GetData then
      begin
        ShowModal;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmGoPhast.miBatchFileAdditionsClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmBatchFileAdditions);
end;

procedure TfrmGoPhast.SetCursorGrid(const Value: TCursorGrid);
begin
  if FCursorGrid <> Value then
  begin
    // record the view of the grid over which the mouse was moved.
    FCursorGrid := Value;
  end;
end;

procedure TfrmGoPhast.pnlLowerRightMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  CursorGrid := cgNone;
end;

procedure TfrmGoPhast.miProgramLocationsClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmProgramLocations);
end;

procedure TfrmGoPhast.ShallAllObjects1Click(Sender: TObject);
begin
  inherited;
  ShowOrHideAllScreenObjects(True);
end;

procedure TfrmGoPhast.acShowGridValuesClick(Sender: TObject);
begin
  inherited;
  if frmGridValue = nil then
  begin
    frmGridValue := TfrmGridValue.Create(self);
  end;
  frmGridValue.Show
end;

procedure TfrmGoPhast.ShowHint(Sender: TObject);
begin
  // Show the hint on the status bar too.
  sbMain.Panels[0].Text := GetLongHint(Application.Hint);
end;

procedure TfrmGoPhast.tbLineClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbLine.OnMouseDown(tbLine, mbLeft, [ssLeft], 0, 0);
  end;

  if tbLine.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbLine);
    // Set the cursors.
    SetZB_Cursors(crLineArrow);
    CurrentTool := CreateLineScreenObjectTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbPolygonClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbPolygon.OnMouseDown(tbPolygon, mbLeft, [ssLeft], 0, 0);
  end;

  if tbPolygon.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbPolygon);
    // Set the cursors.
    SetZB_Cursors(crPolygonArrow);
    CurrentTool := CreateLineScreenObjectTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbStraightLineClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbStraightLine.OnMouseDown(tbStraightLine, mbLeft, [ssLeft], 0, 0);
  end;

  if tbStraightLine.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbStraightLine);
    // Set the cursors.
    SetZB_Cursors(crStraightLineArrow);
    CurrentTool := CreateStraightLineScreenObjectTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbUndoClick(Sender: TObject);
begin
  inherited;
  UndoStack.UndoEvent(Sender);
end;

procedure TfrmGoPhast.tbRectangleClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbRectangle.OnMouseDown(tbRectangle, mbLeft, [ssLeft], 0, 0);
  end;

  if tbRectangle.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbRectangle);
    // Set the cursors.
    SetZB_Cursors(crRectangleArrow);
    CurrentTool := CreateRectangleScreenObjectTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbRedoClick(Sender: TObject);
begin
  inherited;
  UndoStack.RedoEvent(Sender);
end;

procedure TfrmGoPhast.ClearSelectedNodes;
var
  Index: integer;
  AScreenObject: TScreenObject;
  //  Update: boolean;
  UndoChangeSelection: TUndoChangeSelection;
begin
  // Make sure no screen objects have selected nodes.
  UndoChangeSelection := TUndoChangeSelection.Create;
  //  Update := False;
  for Index := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := PhastModel.ScreenObjects[Index];
    if AScreenObject.SelectedVertexCount > 0 then
    begin
      AScreenObject.ClearSelectedVertices;
      //      Update := True;
    end;
  end;

  UndoChangeSelection.SetPostSelection;
  if UndoChangeSelection.SelectionChanged then
  begin
    UndoStack.Submit(UndoChangeSelection);
  end
  else
  begin
    UndoChangeSelection.Free;
  end;
end;

procedure TfrmGoPhast.miContourstoShapefileClick(Sender: TObject);
var
  ContourExporter: TContourExtractor;
begin
  inherited;
  if Grid.TopContourDataSet = nil then
  begin
    Beep;
    MessageDlg('You must contour data on the grid to export '
      + 'contours to a Shapefile.', mtWarning, [mbOK], 0);
  end
  else
  begin
    sdShapefile.FileName := IncludeTrailingPathDelimiter(GetCurrentDir)
      + Grid.TopContourDataSet.Name + '.shp';
    if sdShapefile.Execute then
    begin
      ContourExporter := TContourExtractor.Create(PhastModel);
      try
        ContourExporter.CreateShapes(PhastModel.ContourLegend.Values, Grid.TopContourDataSet,
          sdShapefile.FileName);
      finally
        ContourExporter.Free;
      end;
    end;
  end;
end;

procedure TfrmGoPhast.ConvertPoint(Sender: TObject; VD: TViewDirection;
  const RealPoint: TPoint2D; var ScreenCoordinate: TPoint);
begin
  case VD of
    vdTop:
      begin
        ScreenCoordinate := frameTopView.ConvertPoint(RealPoint);
      end;
    vdFront:
      begin
        ScreenCoordinate := frameFrontView.ConvertPoint(RealPoint);
      end;
    vdSide:
      begin
        ScreenCoordinate := frameSideView.ConvertPoint(RealPoint);
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmGoPhast.tbSelectClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbSelect.OnMouseDown(tbSelect, mbLeft, [ssLeft], 0, 0);
  end;

  if tbSelect.Down then
  begin
    if not FCreatingMainForm then
    begin
      // When you want to select screen objects, don't let any nodes be selected.
      ClearSelectedNodes;
    end;
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbSelect);
    // Set the cursors.
    SetZB_Cursors(crArrow);
    // Show a rectangle around the selected nodes.
    frameTopView.UpdateSelectRectangle;
    frameFrontView.UpdateSelectRectangle;
    frameSideView.UpdateSelectRectangle;
    CurrentTool := SelectScreenObjectTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.miHintDelayClick(Sender: TObject);
begin
  // Allow the user to adjust how long hints should be visible.
  ShowAForm(TfrmHintDelay);
end;

procedure TfrmGoPhast.miTimeSeriestoShapefileClick(Sender: TObject);
var
  FileName: string;
begin
  inherited;
  FileName := ChangeFileExt(PhastModel.ModelFileName, '');
  if FileName = '' then
  begin
    FileName := IncludeTrailingPathDelimiter(GetCurrentDir)
      + StrTimeSeriesshp;
  end
  else
  begin
    FileName := FileName + '_' + StrTimeSeriesshp;
  end;
  sdShapefile.FileName := FileName;
  if sdShapefile.Execute then
  begin
    PhastModel.TimeSeries.ExportShapefile(sdShapefile.FileName);
  end;
end;

procedure TfrmGoPhast.SetTopScreenObjectsChanged(const Value: boolean);
begin
  // When the top screen objects have changed redraw.
  FTopScreenObjectsChanged := Value;
  if FTopScreenObjectsChanged and not (csDestroying in ComponentState) then
  begin
    frameTopView.ZoomBox.Image32.Invalidate;
  end;
end;

function TfrmGoPhast.ResetSelectedScreenObjects: boolean;
begin
  // Deselect all objects.
  result := PhastModel.ResetSelectedScreenObjects;
end;

procedure TfrmGoPhast.acExitExecute(Sender: TObject);
begin
  // Closing the main form (this one) closes the application.
  Close;
end;

procedure TfrmGoPhast.tbLassoClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbLasso.OnMouseDown(tbLasso, mbLeft, [ssLeft], 0, 0);
  end;

  if tbLasso.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbLasso);
    // Set the cursors.
    SetZB_Cursors(crArrow);
  end;
  CurrentTool := LassoTool;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbSelectPointClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbSelectPoint.OnMouseDown(tbSelectPoint, mbLeft, [ssLeft], 0, 0);
  end;

  if tbSelectPoint.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbSelectPoint);
    // Set the cursors.
    SetZB_Cursors(crSelectPoint);
    // don't draw a rectangle around the selected screen objects.
    CurrentTool := SelectPointTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.SetActionChecked(Sender: TObject);
var
  AnAction: Taction;
begin
  // toggle the checked state of an action or the action
  // associated with a control.
  AnAction := nil;
  if Sender is TAction then
  begin
    AnAction := TAction(Sender);
  end
  else if Sender is TMenuItem then
  begin
    AnAction := TMenuItem(Sender).Action as TAction;
  end
  else if Sender is TToolButton then
  begin
    AnAction := TToolButton(Sender).Action as TAction;
  end;
  if AnAction <> nil then
  begin
    AnAction.Checked := not AnAction.Checked;
  end;
end;

procedure TfrmGoPhast.tbInsertPointClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbInsertPoint.OnMouseDown(tbInsertPoint, mbLeft, [ssLeft], 0, 0);
  end;

  if tbInsertPoint.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbInsertPoint);
    // Set the cursors.
    SetZB_Cursors(crDisabledInsertPoint);
    CurrentTool := InsertPointTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.tbDeleteSegmentClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbDeleteSegment.OnMouseDown(tbDeleteSegment, mbLeft, [ssLeft], 0, 0);
  end;

  if tbDeleteSegment.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbDeleteSegment);
    // Set the cursors.
    SetZB_Cursors(crDisabledDeleteSegment);
    CurrentTool := DeleteSegmentTool;
  end
  else
  begin
    CurrentTool := nil;
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.miRearrangeObjectsClick(Sender: TObject);
begin
  // Allow the user to change the order of objects.
  ShowAForm(TfrmRearrangeObjects);
end;

procedure TfrmGoPhast.miReverseSelectedObjectsClick(Sender: TObject);
begin
  inherited;
  UndoStack.Submit(TUndoReverseVerticies.Create);
end;

procedure TfrmGoPhast.tbPointMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ToolButton: TToolButton;
begin
  if Sender is TToolButton then
  begin
    ToolButton := TToolButton(Sender);
    if ToolButton.Down then
    begin
      // Make sure all buttons except the current one are up.
      SetButtonsUp(ToolButton);
    end;
  end;
end;

procedure TfrmGoPhast.tbGridAngleClick(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbGridAngle.OnMouseDown(tbGridAngle, mbLeft, [ssLeft], 0, 0);
  end;

  // Make sure all buttons except the current one are up.
  SetButtonsUp(tbGridAngle);

  // Set the cursors.
  SetZB_Cursors(crArrow);
  if frameTopView <> nil then
  begin
    frameTopView.ZoomBox.Cursor := crRotate;
    frameTopView.ZoomBox.Image32.Cursor := crRotate;
  end;

  CurrentTool := RotateGridTool;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.miSetSelectedColRowLayerClick(Sender: TObject);
begin
  ShowAForm(TfrmSelectColRowLayer);
end;

function TfrmGoPhast.DefaultVE: Real;
var
  LocalGrid: TCustomGrid;
  ZHeight: Real;
  XWidth: Real;
  FrameRatio: Real;
  E: Integer;
  D: Integer;
begin
  result := 20;
  LocalGrid := frmGoPhast.Grid;
  if (LocalGrid <> nil) and (LocalGrid.LayerCount >= 1)
    and (LocalGrid.RowCount >= 1) and (LocalGrid.ColumnCount >= 1) then
  begin
    ZHeight := LocalGrid.HighestElevation - LocalGrid.LowestElevation;
    if ZHeight > 0 then
    begin
      XWidth := LocalGrid.ColumnPosition[LocalGrid.ColumnCount] - LocalGrid.ColumnPosition[0];
      XWidth := Abs(XWidth);
      if XWidth > 0 then
      begin
        result := XWidth / ZHeight;
        if ((frameFrontView.ZoomBox.Height > 10)
          and (frameFrontView.ZoomBox.Width > 10)) then
        begin
          FrameRatio := frameFrontView.ZoomBox.Width
            / frameFrontView.ZoomBox.Height;
          result := result/FrameRatio;
        end
        else
        begin
          result := result/3;
        end;
        E := Floor(Log10(result));
        D := ceil(result*Power(10, -E));
        result := D*Power(10, E);
      end;
    end;
  end;
end;

procedure TfrmGoPhast.miDeleteImageClick(Sender: TObject);
begin
  inherited;
  if frmGoPhast.PhastModel.Bitmaps.Count  > 0 then
  begin
    if frmGoPhast.PhastModel.Bitmaps.Count = 1 then
    begin
      frmGoPhast.PhastModel.Bitmaps.Delete(0);
    end
    else
    begin
       ShowAForm(TfrmDeleteImage);
    end;
    InvalidateViewOfModel;
    ReDrawAllViews(nil);
    frmGoPhast.PhastModel.Invalidate;
  end;
  EnableDeleteImage;
end;

procedure TfrmGoPhast.DeleteLastPointInScreenObject;
var
  AScreenObject: TScreenObject;
begin
  AScreenObject := frameTopView.CurrentScreenObject;
  if AScreenObject = nil then
  begin
    AScreenObject := frameFrontView.CurrentScreenObject;
  end;
  if AScreenObject = nil then
  begin
    AScreenObject := frameSideView.CurrentScreenObject;
  end;
  if (AScreenObject <> nil) and (AScreenObject.Count > 0) then
  begin
    AScreenObject.Count := AScreenObject.Count -1;
  end
  else if AScreenObject = nil then
  begin
    DeleteSelectedNodesOrSelectedScreenObjects;
  end;
end;

procedure TfrmGoPhast.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
const
  Key_Escape = VK_ESCAPE; // = 27
  Key_Return = VK_RETURN; // = 13
  Key_Delete = VK_DELETE; // = 46
  Key_Left = VK_LEFT;     // = 37
  Key_Up = VK_UP;         //= 38
  Key_Right = VK_RIGHT;   // = 39
  Key_Down = VK_DOWN;     // = 40
  Key_PageUp = VK_PRIOR;  // = 33
  Key_PageDown = VK_NEXT; // = 34
var
  BigJump: boolean;
  ReverseFactor: integer;
begin
  // This only works because frmGoPhast.KeyPreview is true.

  BigJump := ssShift in Shift;
  ReverseFactor := 1;
  case Key of
    Key_Left, Key_Right:
      begin
        case Grid.ColumnDirection of
          cdWestToEast: ReverseFactor := 1;
          cdEastToWest: ReverseFactor := -1;
          else Assert(False);
        end;
      end;
    Key_Up, Key_Down:
      begin
        case Grid.RowDirection of
          rdSouthToNorth: ReverseFactor := 1;
          rdNorthToSouth: ReverseFactor := -1;
          else Assert(False);
        end;
      end;
    Key_PageUp, Key_PageDown:
      begin
        case Grid.LayerDirection of
          ldBottomToTop: ReverseFactor := 1;
          ldTopToBottom: ReverseFactor := -1;
          else Assert(False);
        end;
      end;
    // else ignore it.
  end;
  case Key of
    Key_Escape:
      begin
        DeleteLastPointInScreenObject;
      end;
    Key_Return:
      begin
        if CurrentTool is TAddLinePartTool then
        begin
          TAddLinePartTool(CurrentTool).FinishSection;
        end
        else
        begin
          frameTopView.FinishScreenObjects;
          frameFrontView.FinishScreenObjects;
          frameSideView.FinishScreenObjects;
        end;
      end;
    Key_Delete:
      begin
        if CurrentTool is TCustomCreateScreenObjectTool then
        begin
          DeleteLastPointInScreenObject;
        end
        else
        begin
          DeleteSelectedNodesOrSelectedScreenObjects;
        end;
      end;
    Key_Left:
      begin
        if BigJump then
        begin
          PhastModel.SelectedColumn := PhastModel.SelectedColumn - ReverseFactor*10;
        end
        else
        begin
          PhastModel.SelectedColumn := PhastModel.SelectedColumn - ReverseFactor*1;
        end;
        frameSideView.DisplayItem;
      end;
    Key_Up:
      begin
        if BigJump then
        begin
          PhastModel.SelectedRow := PhastModel.SelectedRow + ReverseFactor*10;
        end
        else
        begin
          PhastModel.SelectedRow := PhastModel.SelectedRow + ReverseFactor*1;
        end;
        frameFrontView.DisplayItem;
      end;
    Key_Right:
      begin
        if BigJump then
        begin
          Grid.SelectedColumn := Grid.SelectedColumn + ReverseFactor*10;
        end
        else
        begin
          Grid.SelectedColumn := Grid.SelectedColumn + ReverseFactor*1;
        end;
        frameSideView.DisplayItem;
      end;
    Key_Down:
      begin
        if BigJump then
        begin
          PhastModel.SelectedRow := PhastModel.SelectedRow - ReverseFactor*10;
        end
        else
        begin
          PhastModel.SelectedRow := PhastModel.SelectedRow - ReverseFactor*1;
        end;
        frameFrontView.DisplayItem;
      end;
    Key_PageUp:
      begin
        if BigJump then
        begin
          PhastModel.SelectedLayer := PhastModel.SelectedLayer + ReverseFactor*10;
        end
        else
        begin
          PhastModel.SelectedLayer := PhastModel.SelectedLayer + ReverseFactor*1;
        end;
        frameTopView.DisplayItem;
      end;
    Key_PageDown:
      begin
        if BigJump then
        begin
          PhastModel.SelectedLayer := PhastModel.SelectedLayer - ReverseFactor*10;
        end
        else
        begin
          PhastModel.SelectedLayer := PhastModel.SelectedLayer - ReverseFactor*1;
        end;
        frameTopView.DisplayItem;
      end;
    else
    begin
//      Assert(PatchedVCLX = 3.9);
      // This shouldn't be required - CLX should take care of this.
      // This was broken with PatchedVCLX = 3.91.
      // If it is fixed, the following code and the
      // above assertion should be removed.

      // Respond to keyboard shortcuts.
      {for ControlIndex := 0 to ComponentCount -1 do
      begin
        if Components[ControlIndex] is TMenuItem then
        begin
          Item := TMenuItem(Components[ControlIndex]);
          ShortcutToKey(Item.ShortCut, MenuShortCutKey, MenuShortCutShift);
          if (MenuShortCutKey = Key)
            and (MenuShortCutShift = Shift)
            and Item.Enabled then
          begin
            Item.OnClick(Item);
            break;
          end;
        end;
      end;}
    end
  end;
end;

procedure TfrmGoPhast.acSelectAllFrontExecute(Sender: TObject);
begin
  inherited;
  frameFrontView.SelectAll;
end;

procedure TfrmGoPhast.acSelectAllSideExecute(Sender: TObject);
begin
  inherited;
  frameSideView.SelectAll;
end;

procedure TfrmGoPhast.acSelectAllTopExecute(Sender: TObject);
begin
  inherited;
  frameTopView.SelectAll;
end;

procedure TfrmGoPhast.acSelectColRowLayExecute(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbSelectColRowLayer.OnMouseDown(tbSelectColRowLayer, mbLeft, [ssLeft], 0, 0);
  end;

  if tbSelectColRowLayer.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbSelectColRowLayer);

    // Set the cursors.
    SetZB_Cursors(crArrow);
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
  end;
  CurrentTool := ColRowLayerSelectorTool;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.acSetSpacingExecute(Sender: TObject);
begin
  // Toggle the Checked state of the Sender or it's associated action.
  SetActionChecked(Sender);

  if not (Sender is TToolButton) then
  begin
    tbSpacing.OnMouseDown(tbSpacing, mbLeft, [ssLeft], 0, 0);
  end;

  if tbSpacing.Down then
  begin
    // Make sure all buttons except the current one are up.
    SetButtonsUp(tbSpacing);

    // Set the cursors.
    SetZB_Cursors(crSetWidth);
    if frameTopView <> nil then
    begin
      frameTopView.ZoomBox.Cursor := dcSetSpacing.Cursor;
      frameTopView.ZoomBox.Image32.Cursor := dcSetSpacing.Cursor;
    end;
  end
  else
  begin
    // Reset the cursors.
    SetZB_Cursors(crArrow);
  end;
  CurrentTool := SpacingGridTool;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.dcSetSpacingDrawCursor(Sender: TObject;
  const ABitMap, AMask: TBitmap);
begin
  // Draw the bitmaps for the SetSpacing cursor.
  if Grid <> nil then
  begin
    DrawSubdivideCursor(ABitMap, Grid.GridAngle, dcSetSpacing);
    DrawSubdivideCursor(ABitMap, Grid.GridAngle + Pi / 2, dcSetSpacing);
    AMask.Canvas.Pen.Width := 3;
    DrawSubdivideCursor(AMask, Grid.GridAngle, dcSetSpacing);
    DrawSubdivideCursor(AMask, Grid.GridAngle + Pi / 2, dcSetSpacing);
  end;
end;

procedure TfrmGoPhast.SetFrontScreenObjectsChanged(const Value: boolean);
begin
  FFrontScreenObjectsChanged := Value;
  if FFrontScreenObjectsChanged  and not (csDestroying in ComponentState) then
  begin
    frameFrontView.ZoomBox.Image32.Invalidate;
  end;
end;

procedure TfrmGoPhast.SetModelSelection(const Value: TModelSelection);
begin
  if PhastModel.ModelSelection <> Value then
  begin
    PhastModel.ModelSelection := Value;
    case Value of
      msUndefined: Assert(False);
      msPhast, msModflow, msModflowLGR:
        begin
          InvalidateViewOfModel;
          InvalidateAllViews;
        end;
      else Assert(False);
    end;
  end;
  case Value of
    msUndefined: Assert(False);
    msPhast:
      begin
        acPhastActive.Checked := True;
      end;
    msModflow:
      begin
        acModflowActive.Checked := True;
      end;
    msModflowLGR:
      begin
        acModflowLgrActive.Checked := True;
      end;
    else Assert(False);
  end;
end;

procedure TfrmGoPhast.EditGlobalVariables1Click(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmGlobalVariables);
end;

procedure TfrmGoPhast.EditScreenObjects;
var
  AList: TList;
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  // This procedure allows the user to edit the properties of the selected
  // screen objects.
  if not CanEdit then Exit;
  CanEdit := False;
  try
    AList := TList.Create;
    try
      for Index := 0 to PhastModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := PhastModel.ScreenObjects[Index];
        if AScreenObject.Selected then
        begin
          AList.Add(AScreenObject);
        end;
      end;
      if AList.Count > 0 then
      begin
        Assert(frmScreenObjectProperties <> nil);

        frmScreenObjectProperties.GetDataForMultipleScreenObjects(AList);
        frmScreenObjectProperties.ShowModal;
        BringToFront;
        BringFormsToFront(nil);
      end;
    finally
      AList.Free;
    end;
  finally
    CanEdit := True;
  end;
end;

procedure TfrmGoPhast.EnableInvertSelection;
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  miInvertSelection.Enabled := False;
  for Index := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := PhastModel.ScreenObjects[Index];
    if ScreenObject.Selected then
    begin
      miInvertSelection.Enabled := True;
      break;
    end;
  end;
  ScreenObjectSelectionChange(nil);
end;

procedure TfrmGoPhast.UpdateDisplay(Sender: TObject);
begin
  TopScreenObjectsChanged := True;
  FrontScreenObjectsChanged := True;
  SideScreenObjectsChanged := True;
  frame3DView.glWidModelView.Invalidate;
end;

//procedure TfrmGoPhast.SetShowUcodeInterface(const Value: boolean);
//begin
//  if FShowUcodeInterface <> Value then
//  begin
//    FShowUcodeInterface := Value;
//    SetVisibilityOfModelMateActions;
//  end;
//end;

procedure TfrmGoPhast.SetSideScreenObjectsChanged(const Value: boolean);
begin
  FSideScreenObjectsChanged := Value;
  if FSideScreenObjectsChanged  and not (csDestroying in ComponentState) then
  begin
    frameSideView.ZoomBox.Image32.Invalidate;
  end;
end;

procedure TfrmGoPhast.acEditDataSetsExecute(Sender: TObject);
begin
  ShowAForm(TfrmDataSets);
end;

procedure TfrmGoPhast.UpdateDataSetDimensions;
begin
  PhastModel.UpdateDataSetDimensions;

  frameTopView.ZoomBox.Image32.Invalidate;
  frameFrontView.ZoomBox.Image32.Invalidate;
  frameSideView.ZoomBox.Image32.Invalidate;
end;

procedure TfrmGoPhast.UpdateModelSelection;
begin
  acModflowActive.Checked := (PhastModel.Grid <> nil)
    and (PhastModel.Grid = PhastModel.ModflowGrid);
  acPhastActive.Checked := (PhastModel.Grid <> nil)
    and (PhastModel.Grid = PhastModel.PhastGrid);
end;

procedure TfrmGoPhast.DeleteSelectedNodesOrSelectedScreenObjects;
var
  SelectedScreenObjects: TScreenObjectList;
  DeleteNodes: Boolean;
  Index: Integer;
  AScreenObject: TScreenObject;
  UndoDeleteNodes: TUndoDeleteVertices;
  UndoDeleteScreenObjects: TUndoDeleteScreenObjects;
begin
  SelectedScreenObjects := TScreenObjectList.Create;
  try
    SelectedScreenObjects.Capacity := PhastModel.ScreenObjectCount;
    // If any screen objects have selected nodes, delete the
    // selected nodes instead of the entire screen object.
    // (However, if all the nodes are selected, deleting
    // the selected nodes deletes the screen object instead.)
    DeleteNodes := False;
    for Index := PhastModel.ScreenObjectCount - 1 downto 0 do
    begin
      AScreenObject := PhastModel.ScreenObjects[Index] as TScreenObject;
      if AScreenObject.Selected then
      begin
        if AScreenObject.SelectedVertexCount > 0 then
        begin
          DeleteNodes := True;
        end;
        SelectedScreenObjects.Add(AScreenObject);
      end;
    end;
    if SelectedScreenObjects.Count > 0 then
    begin
      if DeleteNodes then
      begin
        UndoDeleteNodes := TUndoDeleteVertices.Create(SelectedScreenObjects);
        try
          UndoStack.Submit(UndoDeleteNodes);
          UndoDeleteNodes.SetPostSelection;
        except
          on EScreenObjectError do
          begin
            UndoDeleteNodes.Free;
            Beep;
            MessageDlg('Sorry; You can''t delete that node.', mtError, [mbOK], 0);
          end;
        end;
      end
      else
      begin
        UndoDeleteScreenObjects := TUndoDeleteScreenObjects.Create(SelectedScreenObjects);
        UndoStack.Submit(UndoDeleteScreenObjects);
        UndoDeleteScreenObjects.SetPostSelection;
      end;
    end;
  finally
    SelectedScreenObjects.Free;
  end;
end;

procedure TfrmGoPhast.miGeneralClick(Sender: TObject);
begin
  ShowAForm(TfrmModflowOptions);
  if PhastModel.CheckWetting then
  begin
    frmErrorsAndWarnings.Show;
  end;
end;

function TfrmGoPhast.GetCanDraw: boolean;
begin
  result := FCanDraw and not FReadingFile and (PhastModel.DataSetUpdateCount = 0);
end;

procedure TfrmGoPhast.GetCurrentScreenObject(Sender: TObject;
  VD: TViewDirection; var ScreenObject: TScreenObject);
begin
  case VD of
    vdTop:
      begin
        ScreenObject := frameTopView.CurrentScreenObject;
      end;
    vdFront:
      begin
        ScreenObject := frameFrontView.CurrentScreenObject;
      end;
    vdSide:
      begin
        ScreenObject := frameSideView.CurrentScreenObject;
      end;
  else
    Assert(False);
    ScreenObject := nil;
  end;
end;

function TfrmGoPhast.GetGrid: TCustomGrid;
begin
  if PhastModel = nil then
  begin
    result := nil;
  end
  else
  begin
    result := PhastModel.Grid;
  end;
end;

function TfrmGoPhast.GetModelSelection: TModelSelection;
begin
  result := PhastModel.ModelSelection;
end;

function TfrmGoPhast.GetModflowGrid: TModflowGrid;
begin
  if PhastModel = nil then
  begin
    result := nil;
  end
  else
  begin
    result := PhastModel.ModflowGrid;
  end
end;

function TfrmGoPhast.GetObservationFileName(SD: TSaveDialog): string;
begin
  result := FObservationFileName;
  if (result = '') and (PhastModel.ModelFileName <> '') then
  begin
    result := ChangeFileExt(PhastModel.ModelFileName,
      SD.DefaultExt);
  end;
  result := PhastModel.FixFileName(result);
end;

function TfrmGoPhast.IsOverStatusPanelDivider(const X: integer): boolean;
var
  Index: integer;
  Position: integer;
begin
  // This function returns True if X indicates that the cursor is over
  // one of the bars between the panels in sbMain.
  result := false;
  Position := 0;
  FMovingPanelIndex := -1;
  for Index := 0 to sbMain.Panels.Count - 2 do
  begin
    Position := Position + sbMain.Panels[Index].Width;
    result := (X >= Position) and (X <= Position + DividerWidth);
    if result then
    begin
      FMovingPanelIndex := Index;
      Exit;
    end;
    Position := Position + DividerWidth;
  end;
end;

procedure TfrmGoPhast.miMakeSelectedVerticesASeparateObjectClick(
  Sender: TObject);
begin
  inherited;
  UndoStack.Submit(TUndoMakeSelectedVerticesNewScreenObject.Create);
end;

procedure TfrmGoPhast.miManageFluxObservationsClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmManageFluxObservations);
end;

procedure TfrmGoPhast.miManageHeadObservationsClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmManageHeadObservations);
end;

procedure TfrmGoPhast.miManageParametersClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmManageParameters);
end;

procedure TfrmGoPhast.miMergeObjectsClick(Sender: TObject);
var
  Undo: TUndoMergeObjects;
begin
  inherited;
  Undo := TUndoMergeObjects.Create;
  if Undo.ShouldUse then
  begin
    UndoStack.Submit(Undo);
  end
  else
  begin
    Undo.Free;
  end;
end;

procedure TfrmGoPhast.miLinkSFRStreamsClick(Sender: TObject);
begin
  inherited;
  if frmLinkStreams = nil then
  begin
    Application.CreateForm(TfrmLinkStreams, frmLinkStreams);
  end;
  frmLinkStreams.Show;
end;

procedure TfrmGoPhast.sbMainMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  { TODO : Put this functionality in a component. }
  // When the cursor is over one of the dividers between panels in
  // sbMain or when the divider is being moved, use crMoveColumn
  // as the cursor.  Otherwise, use crDefault.
  if FMovingStatusBarPanel or IsOverStatusPanelDivider(X) then
  begin
    sbMain.Cursor := crHSplit;
  end
  else
  begin
    sbMain.Cursor := crDefault;
  end;
end;

procedure TfrmGoPhast.sbMainDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  inherited;
  // The usual drawing of the text will truncate
  // the text after about 110 characters.
  StatusBar.Canvas.TextRect(Rect,Rect.Left+1,3,Panel.Text);
end;

procedure TfrmGoPhast.sbMainMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  // If the cursor is over one of the dividers between panels on
  // sbMain when the MouseDown event occurs, start moving the divider.
  FMovingStatusBarPanel := IsOverStatusPanelDivider(X);
end;

procedure TfrmGoPhast.sbMainMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PriorWidths: integer;
  OldPosition, NewPosition: integer;
  Index: integer;
  OldWidth, NewWidth: integer;
  MaxPosition: integer;
begin
  { TODO : Put this functionality in a component. }
  // If the user is moving the dividers between two panels on sbMain,
  // move it on the MouseUp event.
  if FMovingStatusBarPanel then
  begin
    // Determine the widths of all the panels before the current one.
    // (All panels have a panel position of ppLeft and are visible.
    PriorWidths := 0;
    for Index := 0 to FMovingPanelIndex - 1 do
    begin
      PriorWidths := PriorWidths + sbMain.Panels[Index].Width + DividerWidth;
    end;
    // Find the middle of the divider between panels that the user
    // wants to move.
    OldPosition := PriorWidths + sbMain.Panels[FMovingPanelIndex].Width +
      DividerWidth div 2;
    // Store the width of the panel before the divider that the user
    // wants to move.
    OldWidth := sbMain.Panels[FMovingPanelIndex].Width;
    // Store the new width for that panel.
    NewWidth := sbMain.Panels[FMovingPanelIndex].Width + X - OldPosition;
    // Make sure the new width isn't too small.
    if NewWidth < 5 then
    begin
      NewWidth := 5;
    end;
    // Make sure the new width isn't too big.
    NewPosition := PriorWidths + NewWidth + DividerWidth div 2;
    if FMovingPanelIndex = sbMain.Panels.Count - 2 then
    begin
      MaxPosition := sbMain.Width - 20;
    end
    else
    begin
      MaxPosition := PriorWidths + OldWidth + sbMain.Panels[
        FMovingPanelIndex + 1].Width
    end;

    if NewPosition >= MaxPosition then
    begin
      NewPosition := MaxPosition;
      NewWidth := NewPosition - PriorWidths - DividerWidth div 2;
    end;

    // Set the new width of the panel.
    sbMain.Panels[FMovingPanelIndex].Width := NewWidth;

    // Set the width of the panel above the divider so that
    // its right edge doesn't move.
    NewWidth := sbMain.Panels[FMovingPanelIndex + 1].Width
      + OldWidth - NewWidth;
    if NewWidth < 5 then
    begin
      NewWidth := 5;
    end;
    sbMain.Panels[FMovingPanelIndex + 1].Width := NewWidth;
    // Stop moving the divider.
    FMovingStatusBarPanel := False;
  end;
end;

procedure TfrmGoPhast.Index1Click(Sender: TObject);
begin
  inherited;
  Application.helpcommand(HELP_TAB, TAB_INDEX);
end;

procedure TfrmGoPhast.InitializeView(ModelXWidth, ModelYWidth,
  ModelHeight: Real);
begin
  // Set the magnification so that the grid will fill most of the screen.
  frameTopView.ZoomBox.Magnification := 0.9 *
    Min(frameTopView.ZoomBox.Width / ModelXWidth,
    frameTopView.ZoomBox.Height / ModelYWidth);
  // the following statement may not be required because the
  // magnification is set in SynchronizeViews.
  frameFrontView.ZoomBox.Magnification := 0.9 *
    Min(frameFrontView.ZoomBox.Width / ModelXWidth,
    frameFrontView.ZoomBox.Height /
    (ModelHeight * frameFrontView.ZoomBox.Exaggeration));

  // Make sure the grid is visible on the screen.
  MoveToTopCell(Grid, (Grid.ColumnCount - 1) div 2,
    (Grid.RowCount - 1) div 2);
  MoveToFrontCell(Grid, (Grid.ColumnCount - 1) div 2,
    (Grid.LayerCount - 1) div 2);

  SynchronizeViews(vdTop);
end;

procedure TfrmGoPhast.IntroductoryVideo1Click(Sender: TObject);
begin
  inherited;
  PlayIntroductoryVideo;
end;

procedure TfrmGoPhast.Invalidate3DView(Sender: TObject);
begin
  frame3DView.glWidModelView.Invalidate;
end;

procedure TfrmGoPhast.InvalidateAllViews;
begin
  InvalidateTop;
  InvalidateFront;
  InvalidateSide;
end;

procedure TfrmGoPhast.InvalidateDataSets;
var
  Index: integer;
  TimeList: TCustomTimeList;
begin
  PhastModel.DataArrayManager.InvalidateAllDataSets;
  for Index := 0 to PhastModel.TimeListCount - 1 do
  begin
    TimeList := PhastModel.TimeLists[Index];
    TimeList.Invalidate;
  end;
end;

procedure TfrmGoPhast.ScreenObjectsChanged(Sender: TObject);
begin
  if (Sender = nil) or not (Sender as TObserver).UpToDate then
  begin
    PhastGrid.NeedToRecalculateCellColors;
    ModflowGrid.NeedToRecalculateCellColors;
    UpdateDisplay(Sender);
    Invalidate;
  end;
end;

procedure TfrmGoPhast.ScreenObjectSelectionChange(Sender: TObject);
var
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  if ChangingSelection then
  begin
    Exit;
  end;
  acEditSelecteObjects.Enabled := (PhastModel <> nil)
    and (PhastModel.SelectedScreenObjectCount >= 1);
  acAddPolygonsToObject.Enabled := (PhastModel <> nil)
    and (PhastModel.SelectedScreenObjectCount = 1);
  acAddLinesToObject.Enabled := (PhastModel <> nil)
    and (PhastModel.SelectedScreenObjectCount = 1);
  acAddPointsToObject.Enabled := (PhastModel <> nil)
    and (PhastModel.SelectedScreenObjectCount = 1);
  miScaleRotateorMoveObjects.Enabled := (PhastModel <> nil)
    and (PhastModel.SelectedScreenObjectCount >= 1);
  miMergeObjects.Enabled := (PhastModel <> nil)
    and (PhastModel.SelectedScreenObjectCount > 1);
  miReverseSelectedObjects.Enabled := (PhastModel <> nil)
    and (PhastModel.SelectedScreenObjectCount >= 1);
  frameFrontView.miMergeObjects.Enabled := miMergeObjects.Enabled;
  frameTopView.miMergeObjects.Enabled := miMergeObjects.Enabled;
  frameSideView.miMergeObjects.Enabled := miMergeObjects.Enabled;

  miInvertSelectedVertices.Enabled := False;
  miMakeSelectedVerticesASeparateObject.Enabled := False;
  miSplitObjectAtSelectedVertices.Enabled := False;
  miSplitSelectedObjects.Enabled := False;
  if (PhastModel <> nil)
    and (PhastModel.SelectedScreenObjectCount >= 1) then
  begin
    for Index := 0 to PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := PhastModel.ScreenObjects[Index];
      if AScreenObject.Selected then
      begin
        if (AScreenObject.SelectedVertexCount > 0)
          and (AScreenObject.SelectedVertexCount < AScreenObject.Count) then
        begin
          miInvertSelectedVertices.Enabled := True;
          miMakeSelectedVerticesASeparateObject.Enabled := True;
          miSplitObjectAtSelectedVertices.Enabled := True;
          break;
        end;
      end;
    end;
    for Index := 0 to PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := PhastModel.ScreenObjects[Index];
      if AScreenObject.Selected and (AScreenObject.SectionCount > 1) then
      begin
        miSplitSelectedObjects.Enabled := True;
        break;
      end;
    end;
  end;
end;

procedure TfrmGoPhast.ReDrawAllViews(Sender: TObject);
begin
  if not frameTopView.Drawing
    and not frameFrontView.Drawing
    and not framesideView.Drawing then
  begin
    timTimer.Enabled := False;
    frmGoPhast.frameTopView.ZoomBox.Image32.Invalidate;
    frmGoPhast.frameFrontView.ZoomBox.Image32.Invalidate;
    frmGoPhast.frameSideView.ZoomBox.Image32.Invalidate;
  end;
end;

procedure TfrmGoPhast.ResizeZoomBoxes(Sender: TObject);
begin
  if not frameTopView.Drawing
    and not frameFrontView.Drawing
    and not framesideView.Drawing then
  begin
    timTimer.Enabled := False;
    if not frameTopView.ZoomBox.ImmediateResize then
    begin
      frameTopView.ZoomBox.ImmediateResize := True;
      frameTopView.IsResizing := False;
    end;
    if not frameFrontView.ZoomBox.ImmediateResize then
    begin
      frameFrontView.ZoomBox.ImmediateResize := True;
      frameFrontView.IsResizing := False;
    end;
    if not frameSideView.ZoomBox.ImmediateResize then
    begin
      frameSideView.ZoomBox.ImmediateResize := True;
      frameSideView.IsResizing := False;
    end;
  end;
end;

procedure TfrmGoPhast.RestoreDefault2DView1Click(Sender: TObject);
var
  ModelXWidth, ModelYWidth, ModelHeight: double;
begin
  inherited;
  if (Grid.ColumnCount >= 1) and (Grid.RowCount >= 1)
    and (Grid.LayerCount >= 1) then
  begin
    ModelXWidth := Abs(Grid.ColumnPosition[0]
      - Grid.ColumnPosition[Grid.ColumnCount]);
    ModelYWidth := Abs(Grid.RowPosition[0]
      - Grid.RowPosition[Grid.RowCount]);
    ModelHeight := Abs(Grid.HighestElevation - Grid.LowestElevation);
    InitializeView(ModelXWidth, ModelYWidth, ModelHeight);
  end;
end;

procedure TfrmGoPhast.miRulerFormatClick(Sender: TObject);
begin
  FClickedRuler := nil;
  ShowAForm(TfrmRulerOptions);
end;

procedure TfrmGoPhast.ToolButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ToolButton: TToolButton;
begin
  ToolButton := Sender as TToolButton;
  if (X < 0) or (Y < 0) or (X >= ToolButton.Width)
    or (Y >= ToolButton.Height) then
  begin
    // If the user clicked someplace that is not on the button,
    // act as if they didn't click the button.
    ToolButton.Down := False;
    SetZB_Cursors(crArrow);
    CurrentTool := nil;
  end;
end;

procedure TfrmGoPhast.SampleDEMData1Click(Sender: TObject);
begin
  inherited;
  with TfrmImportDEM.Create(nil) do
  begin
    try
      if GetData then
      begin
        ShowModal;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmGoPhast.SaveAFile(FileName: string);
var
  TempHeadFlux: TFluxObservationGroups;
  TempDrainFlux: TFluxObservationGroups;
  TempGhbFlux: TFluxObservationGroups;
  TempRivFlux: TFluxObservationGroups;
  BackUpName: string;
begin
  Screen.Cursor := crHourGlass;

  if FileExists(FileName) then
  begin
    BackUpName := ChangeFileExt(FileName, '.bak');
    if FileExists(BackUpName) then
    begin
      DeleteFile(BackUpName);
    end;
    RenameFile(FileName, BackUpName);
  end;

//  ReclaimMemory;
  try
    AddMostRecentlyUsedFile(FileName);

    PhastModel.ModelFileName := FileName;

    TempHeadFlux := TFluxObservationGroups.Create(nil);
    TempDrainFlux := TFluxObservationGroups.Create(nil);
    TempGhbFlux := TFluxObservationGroups.Create(nil);
    TempRivFlux := TFluxObservationGroups.Create(nil);
    try
      TempHeadFlux.Assign(PhastModel.HeadFluxObservations);
      TempDrainFlux.Assign(PhastModel.DrainObservations);
      TempGhbFlux.Assign(PhastModel.GhbObservations);
      TempRivFlux.Assign(PhastModel.RiverObservations);

      PhastModel.HeadFluxObservations.EliminatedDeletedScreenObjects;
      PhastModel.DrainObservations.EliminatedDeletedScreenObjects;
      PhastModel.GhbObservations.EliminatedDeletedScreenObjects;
      PhastModel.RiverObservations.EliminatedDeletedScreenObjects;

      PhastModel.DataArrayManager.StoreCachedData := true;
      try
        InternalSaveFile(FileName);
      except on EOutOfMemory do
        begin
          PhastModel.DataArrayManager.StoreCachedData := False;
          InternalSaveFile(FileName);
        end;
      end;
    finally
      PhastModel.HeadFluxObservations.Assign(TempHeadFlux);
      PhastModel.DrainObservations.Assign(TempDrainFlux);
      PhastModel.GhbObservations.Assign(TempGhbFlux);
      PhastModel.RiverObservations.Assign(TempRivFlux);
      TempHeadFlux.Free;
      TempDrainFlux.Free;
      TempGhbFlux.Free;
      TempRivFlux.Free;
    end;
    PhastModel.UpToDate := True;
  finally
    WriteIniFile;
    Screen.Cursor := crDefault;
  end;
  Application.Title := ExtractFileName(FileName) + ' ' + StrModelName;
end;

procedure TfrmGoPhast.acFileSaveExecute(Sender: TObject);
begin
  if sdSaveDialog.FileName = '' then
  begin
    acFileSaveAsExecute(Sender);
  end
  else
  begin
    SaveAFile(sdSaveDialog.FileName);
    if not CreateArchiveSet then
    begin
      CreateArchive := MessageDlg('Do you want to create a model archive too?',
        mtInformation, [mbYes, mbNo], 0) = mrYes;
    end;
    if CreateArchive then
    begin
      PhastModel.CreateArchive('');
    end;
    ReadIniFile;
  end;
end;

procedure TfrmGoPhast.AddMostRecentlyUsedFile(const FileName: string);
begin
  N5.Visible := True;
  N6.Visible := True;
  MostRecentlyUsed.Capacity := 5;
  MostRecentlyUsed.AddFileName(FileName);
  MostRecentlyUsed.FileToIgnore := FileName;
  N5.Visible := MostRecentlyUsed.MenuItemCount > 0;
  N6.Visible := N5.Visible;
end;

Procedure TfrmGoPhast.OnOpenFile(Sender: TObject);
const
  OneSecond = 1/24/3600;
begin
  if (Now - FStartTime) > OneSecond then
  begin
    FStartTime := Now;
    if not frmFileProgress.Visible then
    begin
      frmFileProgress.pbProgress.Max := 1000;
      frmFileProgress.Show;
    end;
    frmFileProgress.pbProgress.Position :=
      Round((1- (FFileSize - FFileStream.Position)/FFileSize)*1000);
    Application.ProcessMessages;
  end;
end;

procedure TfrmGoPhast.OpenAFile(const FileName: string);
var
  TempStream: TMemoryStream;
  NewTop, NewLeft: integer;
  Extension: string;
  DecompressionStream: TDecompressionStream;
  DataArray: TDataArray;
  Index: Integer;
  DataArrayManager: TDataArrayManager;
begin
  if not FileExists(FileName) then
  begin
    Beep;
    MessageDlg(FileName + ' does not exist.', mtError, [mbOK], 0);
    Exit;
  end;
  FreeAndNil(frmModflowPackages);
  frmErrorsAndWarnings.Clear;
  Extension := LowerCase(ExtractFileExt(FileName));
  if Extension = '.gpt' then
  begin
    FileFormat := ffAscii;
  end
  else if Extension = '.gpb' then
  begin
    FileFormat := ffBinary;
  end
  else if Extension = '.xml' then
  begin
    FileFormat := ffXML;
  end
  else if Extension = '.mmzlib' then
  begin
    FileFormat := ffZLib;
  end
  else
  begin
    Beep;
    MessageDlg('ModelMuse files must have one of the following extensions: '
      + '".gpt", ".gpb", ".xml", or ".mmZLib".  The file you tried to open, "'
      + FileName + ' does not have one of those extensions.',
      mtWarning, [mbOK], 0);
    Exit;
  end;

  FReadingFile := True;
  try
    sdModflowInput.FileName := '';
    FObservationFileName := '';
    FPredictionFileName := '';
    AddMostRecentlyUsedFile(FileName);

    Screen.Cursor := crHourGlass;

    // Free forms that aren't destroyed automatically after
    // being shown.
    FreeAndNil(frmShowHideObjects);
    FreeAndNil(frmGridColor);
    FreeAndNil(frmContourData);
    FreeAndNil(frmGridValue);
    FreeAndNil(frmLinkStreams);
    FreeAndNil(frmExportImage);

    sdPhastInput.FileName := '';
    frmFileProgress := TfrmProgressMM.Create(nil);
    FFileStream := TFileStream.Create(FileName,
      fmOpenRead or fmShareDenyWrite, ReadWritePermissions);
    try
      FFileStream.Position := 0;
      FFileSize := FFileStream.Size;
      if FFileSize = 0 then
      begin
        Beep;
        MessageDlg('The file can not be opened because it is empty.  You may '
          + 'wish to see if you can open the backup file (with the extension '
          + '".bak" or there may be an archived file in the directory '
          + 'containing the file.', mtError, [mbOK], 0);
        Exit;
      end;
      if frmScreenObjectProperties <> nil then
      begin
        frmScreenObjectProperties.Initialize;
      end;

      UndoStack.Clear;
      PhastModel.Clear;
      PhastModel.DataArrayManager.CreateInitialBoundaryDataSets;
      PhastModel.DataArrayManager.ClearDeletedDataSets;
      FPositionList.Clear;
      PhastModel.ClearExpressionsAndVariables;
      PhastModel.BeginScreenObjectUpdate;
      ClearFormulaErrors;
      try
        DecompressionStream := nil;
        TempStream := TMemoryStream.Create;
        try
          case FileFormat of
            ffAscii:
              begin
                ObjectTextToBinary(FFileStream, TempStream);
              end;
            ffBinary:
              begin
                // do nothing
              end;
            ffXML:
              begin
                rwObjectXMLToBinary(FFileStream, TempStream);
              end;
            ffZLib:
              begin
                DecompressionStream := TDecompressionStream.Create(FFileStream);
                DecompressionStream.OnProgress := OnOpenFile;
              end;
          else
            Assert(False);
          end;

          PhastModel.PhastGrid.Initialize;
          PhastModel.ModflowGrid.Initialize;
          if FileFormat = ffBinary then
          begin
            FFileStream.ReadComponent(PhastModel);
          end
          else if FileFormat = ffZLib then
          begin
            FStartTime := Now;
            DecompressionStream.ReadComponent(PhastModel);
          end
          else
          begin
            TempStream.Position := 0;
            TempStream.ReadComponent(PhastModel);
          end;
        finally
          TempStream.Free;
          DecompressionStream.Free;
        end;
        if PhastModel.ModelSelection = msUndefined then
        begin
          PhastModel.ModelSelection := msPhast
        end;
        PhastModel.CreateGlobalVariables;
        PhastModel.UpdateDataSets;
        PhastModel.ChildModels.Loaded;
        PhastModel.UpdateScreenObjects;
        PhastModel.LayerStructure.Loaded;
        PhastModel.HeadFluxObservations.Loaded;
        PhastModel.DrainObservations.Loaded;
        PhastModel.GhbObservations.Loaded;
        PhastModel.RiverObservations.Loaded;
        PhastModel.UpdateOnPostInitialize;
        PhastModel.UpdateDataArrayParameterUsed;
        PhastModel.ModelFileName := FileName;
        PhastModel.FormulaManager.Pack;
        PhastModel.FormulaManager.FixSubscriptions;
        PhastModel.UpdateChildGrids;
        PhastModel.UpdateDataSetConnections;
        miObservations.Checked := PhastModel.ObservationPurpose = ofObserved;
        miPredictions.Checked := PhastModel.ObservationPurpose = ofPredicted;

        DataArrayManager := PhastModel.DataArrayManager;
        DataArray := DataArrayManager.GetDataSetByName(rsActive);
        if not Assigned(DataArray.OnUpToDateSet) then
        begin
          DataArray.OnUpToDateSet := PhastModel.OnActiveDataSetChanged;
        end;



        for Index := 0 to DataArrayManager.DataSetCount - 1 do
        begin
          DataArrayManager.DataSets[Index].RestoreUpToDataStatus;
        end;

        PhastModel.UpToDate := True;
      finally
        PhastModel.EndScreenObjectUpdate;
      end;

      UpdateDataSetDimensions;
      PhastModel.FixOldModel;


      sdSaveDialog.FileName := FileName;
      Caption := StrModelName + ': ' + FileName;
    finally
      FreeAndNil(FFileStream);
      FreeAndNil(frmFileProgress);
      Screen.Cursor := crDefault;
      CreateArchiveSet := False;
      FCreateArchive := True;
    end;
    frameTopView.AdjustScales;
    frameFrontView.AdjustScales;
    frameSideView.AdjustScales;
    ResetScreenObjectCount;
    frameTopView.ItemChange(nil);
    frameFrontView.ItemChange(nil);
    frameSideView.ItemChange(nil);
    frame3DView.SetDefaultOrientation;
    EnableInvertSelection;
    case PhastModel.ModelSelection of
      msUndefined: Assert(False);
      msPhast: acPhastActive.Checked := True;
      msModflow: acModflowActive.Checked := True;
      msModflowLGR: acModflowLgrActive.Checked := True;
      else Assert(False);
    end;
    miConfigurePathlines.Enabled := PhastModel.PathLines.Lines.Count > 0;
    miPathlinestoShapefile.Enabled := miConfigurePathlines.Enabled;

    miConfigureEndpoints.Enabled := PhastModel.EndPoints.Points.Count > 0;
    miEndpointsatStartingLocationstoShapefile.Enabled := miConfigureEndpoints.Enabled;
    miEndpointsatEndingLocationstoShapefile.Enabled := miConfigureEndpoints.Enabled;

    miConfigureTimeSeries.Enabled := PhastModel.TimeSeries.Series.Count > 0;
    miTimeSeriestoShapefile.Enabled := miConfigureTimeSeries.Enabled;
    Application.Title := ExtractFileName(FileName) + ' ' + StrModelName;
    frameTopView.ZoomBox.Image32.Invalidate;
    frameFrontView.ZoomBox.Image32.Invalidate;
    frameSideView.ZoomBox.Image32.Invalidate;

    TopGridChanged := True;
    FrontGridChanged := True;
    SideGridChanged := True;
    frameTopView.ZoomBoxResize(nil);
    frameFrontView.ZoomBoxResize(nil);
    frameSideView.ZoomBoxResize(nil);
    EnableLinkStreams;
    EnableManageFlowObservations;
    EnableManageHeadObservations;
    EnableHufMenuItems;

    if WindowState <> wsMaximized then
    begin

      NewTop := Top;
      if Top + Height > Screen.Height then
      begin
        NewTop := Screen.Height - Height;
      end;
      if NewTop < 0 then
      begin
        NewTop := 0;
      end;
      if NewTop <> Top then
      begin
        Top := NewTop;
        if Top + Height > Screen.Height then
        begin
          Height := Screen.Height - Top;
        end;
      end;

      NewLeft := Left;
      if Left + Width > Screen.Width then
      begin
        NewLeft := Screen.Width - Width;
      end;
      if NewLeft < 0 then
      begin
        NewLeft := 0;
      end;
      if NewLeft <> Left then
      begin
        Left := NewLeft;
        if Left + Width > Screen.Width then
        begin
          Width := Screen.Width - Left;
        end;
      end;
    end;
  finally
    FReadingFile := False;
  end;
  // If Application.ProcessMessages is called here
  // and there is an error in a formula in the file,
  // frmFormulaErrors will not be displayed.
//  Application.ProcessMessages;
  FPositionList.Clear;
  StoreInitalPosition;

  PhastModel.ProgramLocations.ReadFromIniFile(IniFile);

//  if PhastModel.ModelMateProjectFileName <> '' then
//  begin
//    ShowUcodeInterface := True;
//  end;
  EnableDeleteImage;

  WriteIniFile;
  acRestoreDefaultViewExecute(nil);
end;

procedure TfrmGoPhast.acFileOpenExecute(Sender: TObject);
begin
  if CheckModel then
  begin
    if odOpenDialog.Execute then
    begin
      OpenAFile(odOpenDialog.FileName);
    end;
  end;
end;

procedure TfrmGoPhast.acFileSaveAsExecute(Sender: TObject);
begin
  if sdSaveDialog.Execute then
  begin
    acFileSaveExecute(Sender);
    Caption := StrModelName + ': ' + sdSaveDialog.FileName;
    sdPhastInput.FileName := ChangeFileExt(sdSaveDialog.FileName,
      sdPhastInput.DefaultExt);
    sdModflowInput.FileName :=
      ChangeFileExt(sdSaveDialog.FileName, sdModflowInput.DefaultExt);
    FObservationFileName := sdModflowInput.FileName;
    FPredictionFileName := sdModflowInput.FileName;
    PhastModel.ModelFileName := sdSaveDialog.FileName;
  end;
end;

function TfrmGoPhast.GetPhastGrid: TPhastGrid;
begin
  if PhastModel = nil then
  begin
    result := nil;
  end
  else
  begin
    result := PhastModel.PhastGrid;
  end
end;

function TfrmGoPhast.GetPredictionFileName(SD: TSaveDialog): string;
begin
  result := FPredictionFileName;
  if (result = '') and (PhastModel.ModelFileName <> '') then
  begin
    result := ChangeFileExt(PhastModel.ModelFileName,
      '_pred' + SD.DefaultExt);
  end;
  result := PhastModel.FixFileName(result);
end;

procedure TfrmGoPhast.GetZoomBox(Sender: TObject; VD: TViewDirection;
  var ZoomBox: TQrbwZoomBox2);
begin
  // use the correct zoombox depending on which way you are viewing the
  // screen object from.
  case VD of
    vdTop:
      begin
        ZoomBox := frameTopView.ZoomBox;
      end;
    vdFront:
      begin
        ZoomBox := frameFrontView.ZoomBox;
      end;
    vdSide:
      begin
        ZoomBox := frameSideView.ZoomBox;
      end;
  else
    Assert(False);
    ZoomBox := nil;
  end;
end;

procedure TfrmGoPhast.GriddedData1Click(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmImportGriddedData);
end;

procedure TfrmGoPhast.HideAllObjects1Click(Sender: TObject);
begin
  inherited;
  ShowOrHideAllScreenObjects(False);
end;

procedure TfrmGoPhast.acMoveToExecute(Sender: TObject);
begin
  ShowAForm(TfrmGoTo);
end;

procedure TfrmGoPhast.InvalidateViewOfModel;
begin
  frameTopView.ModelChanged := True;
  frameFrontView.ModelChanged := True;
  frameSideView.ModelChanged := True;
end;

procedure TfrmGoPhast.miInvertSelectedVerticesClick(Sender: TObject);
var
  Undo: TUndoChangeSelection;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  PointIndex: Integer;
begin
  Undo := TUndoChangeSelection.Create;
  for ScreenObjectIndex := 0 to
    frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    if not AScreenObject.Deleted and AScreenObject.Visible and
      AScreenObject.Selected then
    begin
      for PointIndex := 0 to AScreenObject.Count - 1 do
      begin
        AScreenObject.SelectedVertices[PointIndex]
          := not AScreenObject.SelectedVertices[PointIndex];
      end;
    end;
  end;
  Undo.SetPostSelection;
  if Undo.SelectionChanged then
  begin
    frmGoPhast.UndoStack.Submit(Undo)
  end
  else
  begin
    Undo.Free;
  end;
end;

procedure TfrmGoPhast.miInvertSelectionClick(Sender: TObject);
var
  Index: integer;
  AScreenObject: TScreenObject;
  //  Update: boolean;
  UndoChangeSelection: TUndoChangeSelection;
  ViewDirection: TViewDirection;
  FoundSelected: boolean;
begin
  inherited;

  ViewDirection := vdTop;
  FoundSelected := false;
  for Index := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := PhastModel.ScreenObjects[Index];
    if AScreenObject.Selected then
    begin
      ViewDirection := AScreenObject.ViewDirection;
      FoundSelected := True;
    end;
  end;
  if not FoundSelected then
  begin
    Exit;
  end;

  // Make sure no screen objects have selected nodes.
  UndoChangeSelection := TUndoChangeSelection.Create;

  for Index := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := PhastModel.ScreenObjects[Index];
    if AScreenObject.Visible and
      (ViewDirection = AScreenObject.ViewDirection) then
    begin
      AScreenObject.Selected := not AScreenObject.Selected;
    end;
  end;

  UndoChangeSelection.SetPostSelection;
  if UndoChangeSelection.SelectionChanged then
  begin
    UndoStack.Submit(UndoChangeSelection);
  end
  else
  begin
    UndoChangeSelection.Free;
  end;
end;

procedure TfrmGoPhast.acPasteExecute(Sender: TObject);
var
  Undo: TUndoPasteScreenObjects;
  Index: Integer;
  List: TList;
begin
  inherited;
  Undo := TUndoPasteScreenObjects.Create;
  try
    for Index := 0 to PhastModel.ScreenObjectCount - 1 do
    begin
      PhastModel.ScreenObjects[Index].Selected := False;
    end;
    List := TList.Create;
    try
      PhastModel.PasteObjectsFromClipboard(List);
      if List.Count > 0 then
      begin
        Undo.StoreNewScreenObjects(List);
        UndoStack.Submit(Undo);
        Undo := nil;
      end;
    finally
      List.Free;
    end;
  finally
    begin
      Undo.Free;
    end;
  end;
end;

procedure TfrmGoPhast.acPhastActiveExecute(Sender: TObject);
begin
  if frmGoPhast.ModelSelection <> msPhast then
  begin
    UndoStack.Submit(TUndoModelSelectionChange.Create(msPhast));
  end;
end;

procedure TfrmGoPhast.acPositionBackwardExecute(Sender: TObject);
begin
  inherited;
  FPositionList.Undo;
end;

procedure TfrmGoPhast.acPositionForwardExecute(Sender: TObject);
begin
  inherited;
  FPositionList.Redo;
end;

procedure TfrmGoPhast.miShowFormulaErrorsClick(Sender: TObject);
begin
  frmFormulaErrors.Show;
end;

procedure TfrmGoPhast.acGenerateGridExecute(Sender: TObject);
begin
  SetButtonsUp(nil);
  SetZB_Cursors(crArrow);
  if (ModelSelection in [msModflow, msModflowLGR]) and
    (PhastModel.LayerStructure.Count <= 1) then
  begin
    Beep;
    MessageDlg('You must define the layer groups in your MODFLOW model before '
      + 'generating the grid.', mtError, [mbOK], 0);
  end
  else
  begin
    ShowAForm(TfrmGenerateGrid);
  end;
  SelectDefaultButton;
end;

procedure TfrmGoPhast.acLayersExecute(Sender: TObject);
begin
  ShowAForm(TfrmLayers);
  if PhastModel.CheckWetting then
  begin
    frmErrorsAndWarnings.Show;
  end;
end;

procedure TfrmGoPhast.miGridOptionsClick(Sender: TObject);
begin
  ShowAForm(TfrmPhastGridOptions);
end;

procedure TfrmGoPhast.miFileClick(Sender: TObject);
begin
  inherited;
//  ReadIniFile;
end;

procedure TfrmGoPhast.miFreeSurfaceClick(Sender: TObject);
begin
  ShowAForm(TfrmFreeSurface);
end;

procedure TfrmGoPhast.miPrintFrequencyClick(Sender: TObject);
begin
  ShowAForm(TfrmPrintFrequency);
end;

procedure TfrmGoPhast.miPrintInitialClick(Sender: TObject);
begin
  ShowAForm(TfrmPrintInitial);
end;

procedure TfrmGoPhast.miSolutionMethodClick(Sender: TObject);
begin
  ShowAForm(TfrmSolutionMethod);
end;

procedure TfrmGoPhast.miSplitSelectedObjectsClick(Sender: TObject);
begin
  inherited;
  UndoStack.Submit(TUndoExplodeScreenObject.Create);
end;

procedure TfrmGoPhast.miTitleAndUnitsClick(Sender: TObject);
begin
  ShowAForm(TfrmUnits);
end;

procedure TfrmGoPhast.miSteadyFlowClick(Sender: TObject);
begin
  ShowAForm(TfrmSteadyFlow);
end;

procedure TfrmGoPhast.miTimeClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmModflowTime);
end;

procedure TfrmGoPhast.miTimeControlClick(Sender: TObject);
begin
  ShowAForm(TfrmTimeControl);
end;

procedure TfrmGoPhast.SetFileFormat(const Value: TFileFormat);
var
  DefaultExtension: string;
begin
  Assert(Value in [Low(TFileFormat)..High(TFileFormat)]);
  if FFileFormat <> Value then
  begin
    FFileFormat := Value;
    case FileFormat of
      ffAscii:
        begin
          DefaultExtension := 'gpt'
        end;
      ffBinary:
        begin
          DefaultExtension := 'gpb'
        end;
      ffXML:
        begin
          DefaultExtension := 'xml'
        end;
      ffZLib:
        begin
          DefaultExtension := 'mmZLib'
        end;
    else
      Assert(False);
    end;
    if sdSaveDialog.FilterIndex <> Ord(FileFormat)+1 then
    begin
      sdSaveDialog.FilterIndex := Ord(FileFormat)+1;
    end;
    sdSaveDialog.DefaultExt := DefaultExtension;
  end;
end;

procedure TfrmGoPhast.sdModelMateClose(Sender: TObject);
begin
  inherited;
  FRunModelMate := FRunModelMateForm.cbOpen.Checked;
  FRunModelMateForm.free;
end;

procedure TfrmGoPhast.sdModelMateShow(Sender: TObject);
begin
  inherited;
  FRunModelMateForm := TfrmRunModelMate.createfordialog(sdModelMate);
  FRunModelMateForm.cbOpen.Checked := FRunModelMate;
end;

procedure TfrmGoPhast.sdModflowInputClose(Sender: TObject);
begin
  inherited;
  FRunModflow := FRunModflowForm.cbRun.Checked;
  if FRunModflowForm.comboModelSelection.ItemIndex >= 0 then
  begin
    FRunModelSelection := FRunModflowForm.comboModelSelection.ItemIndex;
  end
  else
  begin
    FRunModelSelection := 0;
  end;
  FRunModflowForm.free;
end;

procedure TfrmGoPhast.sdModflowInputShow(Sender: TObject);
var
  ADialog: TSaveDialog;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  inherited;
  ADialog := Sender as TSaveDialog;
  FRunModflowForm := TfrmRunModflow.createfordialog(ADialog);
  FRunModflowForm.cbRun.Checked := FRunModflow;
  if PhastModel.LgrUsed then
  begin
    FRunModflowForm.comboModelSelection.Items.Add('Combined model');
    FRunModflowForm.comboModelSelection.Items.AddObject(PhastModel.DisplayName, PhastModel);
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      FRunModflowForm.comboModelSelection.Items.AddObject(ChildModel.DisplayName, ChildModel);
    end;
    FRunModflowForm.comboModelSelection.ItemIndex := FRunModelSelection;
  end
  else
  begin
    FRunModflowForm.comboModelSelection.Visible := False;
  end;
end;

procedure TfrmGoPhast.sdModpathInputClose(Sender: TObject);
begin
  inherited;
  FCreateNewCompositeBudgetFile := FRunModpathForm.cbForceCBF.Checked;
  FRunModpath := FRunModpathForm.cbRun.Checked;
  FRunModpathForm.free;
end;

procedure TfrmGoPhast.sdModpathInputShow(Sender: TObject);
begin
  inherited;
  FRunModpathForm := TfrmRunModpath.createfordialog(sdModpathInput);
  FRunModpathForm.cbRun.Checked := FRunModpath;
  FRunModpathForm.cbForceCBF.Checked := FCreateNewCompositeBudgetFile;
end;

procedure TfrmGoPhast.sdPhastInputClose(Sender: TObject);
begin
  inherited;
  FRunPhast := FRunPhastForm.cbRun.Checked;
  FRunPhastForm.Free;
end;

procedure TfrmGoPhast.sdPhastInputShow(Sender: TObject);
begin
  inherited;
  FRunPhastForm := TfrmRunPhast.createfordialog(sdPhastInput);
  FRunPhastForm.cbRun.Checked := FRunPhast;
end;

procedure TfrmGoPhast.sdSaveDialogClose(Sender: TObject);
begin
  inherited;
  CreateArchive := FSaveModelForm.cbSaveArchive.Checked;
  if FSaveModelForm.cbSaveDataSetValues.Checked then
  begin
    PhastModel.SaveDataSetValues := sdsvAlways;
  end
  else
  begin
    PhastModel.SaveDataSetValues := sdsvNever;
  end;
  FSaveModelForm.free;
end;

procedure TfrmGoPhast.sdSaveDialogFilterChange(Sender: TObject;
  NewIndex: Integer);
begin
  // Argh! FilterIndex is different on Linux and Windows;
  // The version in Windows is the one that is documented.  The other isn't.
  //
  // This is fixed with the Unofficial Visual CLX patch version 3.6

//{$IFDEF LINUX}
//  FileFormat := TFileFormat(NewIndex);
//{$ELSE}
//{$IFDEF MSWINDOWS}

  FileFormat := TFileFormat(NewIndex - 1);

//{$ELSE}
//  Assert(False);
//{$ENDIF}
//{$ENDIF}
end;

procedure TfrmGoPhast.sdSaveDialogShow(Sender: TObject);
begin
  inherited;
  FSaveModelForm := TfrmSaveArchive.createfordialog(sdSaveDialog);
  FSaveModelForm.cbSaveArchive.Checked := CreateArchive;
  FSaveModelForm.cbSaveDataSetValues.Checked :=
    PhastModel.SaveDataSetValues = sdsvAlways;
end;

procedure TfrmGoPhast.sdSaveDialogTypeChange(Sender: TObject);
var
  Dialog: TOpenDialog;
  NewFileName: string;
  Extension: string;
begin
  inherited;
  Dialog := Sender as TOpenDialog;
  case Dialog.FilterIndex of
    1:
      begin
        FileFormat := ffAscii;
        if sdSaveDialog.FileName <> '' then
        begin
          NewFileName :=
            ChangeFileExt(sdSaveDialog.FileName, '.gpt');
        end;
      end;
    2:
      begin
        FileFormat := ffBinary;
        if sdSaveDialog.FileName <> '' then
        begin
          NewFileName :=
            ChangeFileExt(sdSaveDialog.FileName, '.gpb');
        end;
      end;
    3:
      begin
        FileFormat := ffXML;
        if sdSaveDialog.FileName <> '' then
        begin
          NewFileName :=
            ChangeFileExt(sdSaveDialog.FileName, '.xml');
        end;
      end;
    4:
      begin
        FileFormat := ffZLib;
        if sdSaveDialog.FileName <> '' then
        begin
          NewFileName :=
            ChangeFileExt(sdSaveDialog.FileName, '.mmZLib');
        end;
      end;
    5:
      begin
        Assert(Dialog = odOpenDialog);
        Extension := ExtractFileExt(Dialog.FileName);
        if Extension <> '' then
        begin
          Extension := LowerCase(Extension);
          if Extension = '.gpt' then
          begin
            FileFormat := ffAscii;
            if sdSaveDialog.FileName <> '' then
            begin
              NewFileName :=
                ChangeFileExt(sdSaveDialog.FileName, '.gpt');
            end;
          end
          else if Extension = '.gpb' then
          begin
            FileFormat := ffBinary;
            if sdSaveDialog.FileName <> '' then
            begin
              NewFileName :=
                ChangeFileExt(sdSaveDialog.FileName, '.gpb');
            end;
          end
          else if Extension = '.xml' then
          begin
            FileFormat := ffXML;
            if sdSaveDialog.FileName <> '' then
            begin
              NewFileName :=
                ChangeFileExt(sdSaveDialog.FileName, '.xml');
            end;
          end
          else if Extension = '.mmzlib' then
          begin
            FileFormat := ffZLib;
            if sdSaveDialog.FileName <> '' then
            begin
              NewFileName :=
                ChangeFileExt(sdSaveDialog.FileName, '.mmZLib');
            end;
          end
          else
          begin
            NewFileName := sdSaveDialog.FileName;
          end;
        end;
      end
    else
      begin
        FileFormat := ffAscii;
        if sdSaveDialog.FileName <> '' then
        begin
          NewFileName :=
            ChangeFileExt(sdSaveDialog.FileName, '.gpt');
        end;
      end;
  end;
  sdSaveDialog.FileName := NewFileName;
  if Sender = sdSaveDialog then
  begin
    UpdateDialogBoxFileName(sdSaveDialog, NewFileName);
//    SendMessage( GetParent(sdSaveDialog.Handle), CDM_SETCONTROLTEXT,
//      CB_FILENAME_ID, LongInt(Pchar(ExtractFileName(NewFileName))));
  end;
end;

procedure TfrmGoPhast.sdZonebudgetInputClose(Sender: TObject);
begin
  inherited;
  FRunZoneBudget := FRunZoneBudgetForm.cbRun.Checked;
  FRunZoneBudgetForm.free;
end;

procedure TfrmGoPhast.sdZonebudgetInputShow(Sender: TObject);
begin
  inherited;
  FRunZoneBudgetForm := TfrmRunZoneBudget.createfordialog(sdZonebudgetInput);
  FRunZoneBudgetForm.cbRun.Checked := FRunZoneBudget;
end;

procedure TfrmGoPhast.ExportFile(const FileName: string; RunModel: boolean);
begin
  Screen.Cursor := crHourGlass;
  CanDraw := False;
  try
    WritePhastInput(FileName, RunModel);
  finally
    Screen.Cursor := crDefault;
    CanDraw := True;
  end;
end;

procedure TfrmGoPhast.ExportShapefile1Click(Sender: TObject);
begin
  inherited;
  if (Grid.ColumnCount > 0) and (Grid.RowCount > 0) and (Grid.LayerCount > 0) then
  begin
    ShowAForm(TfrmExportShapefile)
  end
  else
  begin
    Beep;
    MessageDlg('You must create a grid in order to export Shapefiles',
      mtError, [mbOK], 0);
  end;
end;

procedure TfrmGoPhast.miExportModflowClick(Sender: TObject);
var
  FileName: string;
  AGrid: TCustomGrid;
  NameWriter: TNameFileWriter;
begin
  inherited;
  AGrid := Grid;
  if (AGrid = nil) or (AGrid.ColumnCount <= 0)
    or (AGrid.RowCount <= 0) or (AGrid.LayerCount <= 0) then
  begin
    Beep;
    MessageDlg('You must define the grid before you can export the MODFLOW '
      + 'input files.', mtError, [mbOK], 0);
    Exit;
  end;
  InitializeModflowInputDialog;
  if sdModflowInput.Execute then
  begin
    if sdModflowInput.FileName <>
      PhastModel.FixFileName(sdModflowInput.FileName) then
    begin
      Beep;
      MessageDlg('Space characters are not allowed in the names of '
        + 'MODFLOW input files.', mtError, [mbOK], 0);
      Exit;
    end;
    case PhastModel.ObservationPurpose of
      ofObserved: FObservationFileName := sdModflowInput.FileName;
      ofPredicted: FPredictionFileName := sdModflowInput.FileName;
      else Assert(False);
    end;
    // erase the list of model input files to be stored in the archive.
    frmGoPhast.PhastModel.ModelInputFiles.Clear;

    if not FileExists(PhastModel.ProgramLocations.ModflowLocation) then
    begin
      ShowAForm(TfrmProgramLocations);
      if not FileExists(PhastModel.ProgramLocations.ModflowLocation) then
      begin
        Beep;
        if MessageDlg('MODFLOW does not exist at the location you specified.  '
          + 'Do you still want to export the MODFLOW input files?', mtWarning,
          [mbYes, mbNo], 0) <> mrYes then
        begin
          Exit;
        end;
      end;
    end;
    if FileExists(PhastModel.ProgramLocations.ModflowLocation) then
    begin
      PhastModel.AddModelInputFile(PhastModel.ProgramLocations.ModflowLocation);
    end;
    frmErrorsAndWarnings.Clear;

    FileName := sdModflowInput.FileName;
    frmFormulaErrors.sgErrors.BeginUpdate;
    CanDraw := False;
    try
      PhastModel.Grid.ThreeDDataSet := nil;
      PhastModel.Grid.TopDataSet := nil;
      PhastModel.Grid.FrontDataSet := nil;
      PhastModel.Grid.SideDataSet := nil;
      PhastModel.Grid.ThreeDContourDataSet := nil;
      PhastModel.Grid.TopContourDataSet := nil;
      PhastModel.Grid.FrontContourDataSet := nil;
      PhastModel.Grid.SideContourDataSet := nil;
      PhastModel.ThreeDTimeList := nil;
      PhastModel.TopTimeList := nil;
      PhastModel.FrontTimeList := nil;
      PhastModel.SideTimeList := nil;

      FileName := PhastModel.FixFileName(FileName);

      NameWriter := TNameFileWriter.Create(PhastModel, FileName);
      try
        PhastModel.NameFileWriter := NameWriter;
        PhastModel.ExportModflowModel(FileName, FRunModflow);
      finally
        NameWriter.Free;
        PhastModel.NameFileWriter := nil;
      end;

    finally
      CanDraw := True;
      frmFormulaErrors.sgErrors.EndUpdate;
    end;

    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  end;
end;

procedure TfrmGoPhast.acExportImageExecute(Sender: TObject);
begin
  inherited;
  if frmGridColor = nil then
  begin
    frmGridColor := TfrmGridColor.Create(nil);
    UpdateFrmGridColor(True);
  end;
  if frmContourData = nil then
  begin
    frmContourData := TfrmContourData.Create(nil);
    UpdateFrmContourData(True);
  end;
  if frmExportImage = nil then
  begin
    Application.CreateForm(TfrmExportImage, frmExportImage);
  end;
  frmExportImage.Show;
end;

procedure TfrmGoPhast.acExportModelMateExecute(Sender: TObject);
var
  TempProject: TProject;
  ModelFile: string;
  AppFile: string;
  Index: Integer;
  ModelPair: TModelIOPair;
  FoundPair: TModelIOPair;
  CurrentDir: string;
  InputFiles: TModelIOPairs;
  NameFile: string;
  CommandLine: string;
begin
  inherited;

  if sdSaveDialog.FileName = '' then
  begin
    Beep;
    MessageDlg('You must save your ModelMuse file '
      + 'before creating or updating the ModelMate file.', mtError, [mbOK], 0);
    Exit;
  end;

  sdModelMate.FileName := PhastModel.ModelMateProjectFileName;
  if sdModelMate.Execute then
  begin
    if not DirectoryExists(ExtractFileDir(sdModelMate.FileName)) then
    begin
      Beep;
      MessageDlg('The directory for the ModelMate file does not exist', mtError, [mbOK], 0);
      Exit;
    end;
    PhastModel.ModelMateProjectFileName :=
      ExtractRelativePath(sdSaveDialog.FileName, sdModelMate.FileName);

    PhastModel.ModelMateProject := nil;
    TempProject := TProject.Create(nil);
    try
      TempProject.ModelID := midModflow2005;
      PhastModel.ModelMateProject := TempProject;
    finally
      TempProject.Free;
    end;
    InitializeModflowInputDialog;

    if FileExists(PhastModel.ModelMateProjectFileName) then
    begin
      ReadModelMateProject(PhastModel.ModelMateProjectFileName,
        PhastModel.ModelMateProject);
    end;

    CurrentDir := GetCurrentDir;
    try
      SetCurrentDir(ExtractFileDir(sdSaveDialog.FileName));
      if PhastModel.ModelMateProject.ProjName = '' then
      begin
        PhastModel.ModelMateProject.ProjName := ChangeFileExt(
          ExtractFileName(sdSaveDialog.FileName), '');
      end;

//      NameFile := ExtractRelativePath(sdSaveDialog.FileName,
//        ChangeFileExt(sdSaveDialog.FileName, '.nam'));
      case PhastModel.ObservationPurpose of
        ofObserved:
          begin
            NameFile := ExtractRelativePath(PhastModel.ModelFileName,
              ObservationFileName[sdModflowInput]);
            PhastModel.ModelMateProject.ModflowNameFile := NameFile
          end;
        ofPredicted:
          begin
            NameFile := ExtractRelativePath(PhastModel.ModelFileName,
              PredictionFileName[sdModflowInput]);
            PhastModel.ModelMateProject.ModflowNameFilePred := NameFile
          end;
        ofInacative:
          begin
            NameFile := '';
          end;
        else Assert(False);
      end;

      NameFile := ExpandFileName(NameFile);

      ModelFile := ExtractRelativePath(PhastModel.ModelFileName,
        ChangeFileExt(NameFile, StrPvalExt));
      AppFile := ExtractRelativePath(PhastModel.ModelFileName,
        ChangeFileExt(NameFile, StrJtf));
      FoundPair := nil;

      InputFiles := nil;
      case PhastModel.ObservationPurpose of
        ofObserved: InputFiles := PhastModel.ModelMateProject.MIFiles;
        ofPredicted: InputFiles := PhastModel.ModelMateProject.MIFilesPred;
        ofInacative: ;
        else Assert(False);
      end;

      if InputFiles <> nil then
      begin
        for Index := 0 to InputFiles.Count - 1 do
        begin
          ModelPair := InputFiles.Items[Index];
          if SameText(ExtractFileExt(ModelPair.ModelFile), StrPvalExt) then
          begin
            FoundPair := ModelPair;
            break;
          end;
        end;
        if FoundPair = nil then
        begin
          FoundPair := InputFiles.Add as TModelIOPair;
        end;
        FoundPair.ModelFile := ModelFile;
        FoundPair.AppFile := AppFile;
      end;

      CommandLine := ExtractRelativePath(NameFile,
        PhastModel.ProgramLocations.ModflowLocation)
        + ' ' + ExtractFileName(NameFile);
      case PhastModel.ObservationPurpose of
        ofObserved:
          begin
            PhastModel.ModelMateProject.MCLForward := CommandLine;

          end;
        ofPredicted:
          begin
            PhastModel.ModelMateProject.MCLPred := CommandLine;
          end;
        else Assert(False);
      end;

      PhastModel.UpdateModelMateProject;

      SaveModelMateProject;
      if FRunModelMate then
      begin
        if not FileExists(PhastModel.ProgramLocations.ModelMateLocation) then
        begin
          miProgramLocationsClick(nil);
        end;

        if FileExists(PhastModel.ProgramLocations.ModelMateLocation) then
        begin
          WinExec(PChar('"' + PhastModel.ProgramLocations.ModelMateLocation
            + '" "' + ExpandFileName(PhastModel.ModelMateProjectFileName)
            + '"'), SW_SHOW);
        end;
      end;
    finally
      SetCurrentDir(CurrentDir);
    end;
  end;
end;

procedure TfrmGoPhast.acExportModpathExecute(Sender: TObject);
var
  FileName: string;
begin
  inherited;
  if not PhastModel.ModflowPackages.ModPath.IsSelected then
  begin
    Beep;
    MessageDlg('You must activate MODPATH in the MODFLOW Packages and '
      + 'Programs dialog box before running MODPATH.',
      mtWarning, [mbOK], 0);
    Exit;
  end;


  if (sdModpathInput.FileName = '') and (sdModflowInput.FileName <> '') then
  begin
    sdModpathInput.FileName := ChangeFileExt(sdModflowInput.FileName,
      sdModpathInput.DefaultExt);
  end;
  if (sdModpathInput.FileName = '') and (sdSaveDialog.FileName <> '') then
  begin
    sdModpathInput.FileName := ChangeFileExt(sdSaveDialog.FileName,
      sdModpathInput.DefaultExt);
  end;
  if sdModpathInput.Execute then
  begin
    if not FileExists(PhastModel.ProgramLocations.ModPathLocation) then
    begin
      ShowAForm(TfrmProgramLocations);
      if not FileExists(PhastModel.ProgramLocations.ModPathLocation) then
      begin
        Beep;
        if MessageDlg('MODPATH does not exist at the location you specified.  '
          + 'Do you still want to export the MODPATH input files?', mtWarning,
          [mbYes, mbNo], 0) <> mrYes then
        begin
          Exit;
        end;
      end;
    end;


    FileName := sdModpathInput.FileName;
    frmFormulaErrors.sgErrors.BeginUpdate;
    try
      PhastModel.ExportModpathModel(FileName, FRunModpath,
        FCreateNewCompositeBudgetFile);
    finally
      frmFormulaErrors.sgErrors.EndUpdate;
    end;
    FCreateNewCompositeBudgetFile := False;

    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  end;
end;

procedure TfrmGoPhast.acExportPhastInputFileExecute(Sender: TObject);
var
  FileName: string;
  AGrid: TCustomGrid;
begin
  AGrid := Grid;
  if (AGrid = nil) or (AGrid.ColumnCount <= 0)
    or (AGrid.RowCount <= 0) or (AGrid.LayerCount <= 0) then
  begin
    Beep;
    MessageDlg('You must define the grid before you can export the PHAST '
      + 'input files.', mtError, [mbOK], 0);
  end;
  if (sdPhastInput.FileName = '') and (sdSaveDialog.FileName <> '') then
  begin
    sdPhastInput.FileName := ChangeFileExt(sdSaveDialog.FileName,
      sdPhastInput.DefaultExt);
  end;
  if sdPhastInput.Execute then
  begin
    FileName := ChangeFileExt(sdPhastInput.FileName, '');
    FileName := ChangeFileExt(FileName, '');
    FileName := ChangeFileExt(FileName, '');
    FileName := ChangeFileExt(FileName, '');
    FileName := ChangeFileExt(FileName, sdPhastInput.DefaultExt);
    sdPhastInput.FileName := FileName;
    frmErrorsAndWarnings.Clear;
    frmFormulaErrors.sgErrors.BeginUpdate;
    try
      ExportFile(FileName, FRunPhast);
    finally
      frmFormulaErrors.sgErrors.EndUpdate;
    end;
  end;
end;

procedure TfrmGoPhast.miChemistryOptionsClick(Sender: TObject);
begin
  ShowAForm(TfrmChemistryOptions);
end;

procedure TfrmGoPhast.miDisplayDataSetValuesClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmDataSetValues)
end;

procedure TfrmGoPhast.acHelpContentsExecute(Sender: TObject);
begin
  HelpRouter.HelpContent;
end;

procedure TfrmGoPhast.acImportModelMateExecute(Sender: TObject);
var
  Project: TProject;
begin
  inherited;
  if sdSaveDialog.FileName = '' then
  begin
    Beep;
    MessageDlg('You must save your ModelMuse file '
      + 'before importing a ModelMate file.', mtError, [mbOK], 0);
    Exit;
  end;

  odModelMate.FileName := PhastModel.ModelMateProjectFileName;
  if odModelMate.Execute then
  begin
    Project := TProject.Create(nil);
    try
      ReadModelMateProject(odModelMate.FileName, Project);
      PhastModel.ImportFromModelMateProject(Project);
    finally
      Project.Free;
    end;
//    PhastModel.ModelMateProjectFileName := odModelMate.FileName;
    PhastModel.ModelMateProjectFileName :=
      ExtractRelativePath(sdSaveDialog.FileName, odModelMate.FileName);
  end;
end;

procedure TfrmGoPhast.miImportDistributedDatabyZoneClick(Sender: TObject);
begin
  ShowAForm(TfrmImportDistributedData);
end;

procedure TfrmGoPhast.CheckInternet;
var
  WebIniFileName: string;
  WebIniFile: TMemIniFile;
begin
  if ParamCount > 1 then
  begin
    // don't display video's if running from the command line.
    Exit;
  end;

  ReadIniFile;

  WebIniFileName := InternetIniFileName(Handle, Application.ExeName);
  Assert(FileExists(WebIniFileName));
  WebIniFile:= TMemInifile.Create(WebIniFileName);
  TCheckInternetThread.Create(PhastModel.Version, WebIniFile,
    miShowVideoTips.Checked);



end;

function TfrmGoPhast.CheckModel: boolean;
var
  ModalResult: integer;
begin
  if PhastModel.UpToDate then
  begin
    result := True;
  end
  else
  begin
    ModalResult :=
      MessageDlg('Do you want to save the changes you made to your model?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if ModalResult = mrYes then
    begin
      acFileSaveExecute(nil);
//      acFileSave.Execute
    end;
    result := ModalResult in [mrYes, mrNo];
  end;
end;

procedure TfrmGoPhast.CheckScreenObject(Sender: TObject;
  ScreenObject: TScreenObject; var IsACurrentScreenObject: boolean);
begin
  IsACurrentScreenObject :=
    (frameTopView.CurrentScreenObject = ScreenObject)
    or (frameFrontView.CurrentScreenObject = ScreenObject)
    or (frameSideView.CurrentScreenObject = ScreenObject);
end;

procedure TfrmGoPhast.miChildModelsClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmChildModels);
end;

procedure TfrmGoPhast.FormActivate(Sender: TObject);
begin
  inherited;
  acCut.ShortCut := ShortCut(Word('X'), [ssCtrl]);
  acCopy.ShortCut := ShortCut(Word('C'), [ssCtrl]);
  acPaste.ShortCut := ShortCut(Word('V'), [ssCtrl]);
end;

procedure TfrmGoPhast.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not CheckModel then
    Action := caNone;
end;

procedure TfrmGoPhast.InvalidateModel;
begin
  if PhastModel <> nil then
  begin
    PhastModel.Invalidate;
  end;
end;

procedure TfrmGoPhast.miImportShapefileClick(Sender: TObject);
begin
  with TfrmImportShapefile.Create(nil) do
  begin
    try
      if GetData then
      begin
        ShowModal;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmGoPhast.miImportDXFFileClick(Sender: TObject);
begin
  with TfrmImportDXF.Create(nil) do
  begin
    try
      if GetData then
      begin
        ShowModal;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmGoPhast.miImportBitmapClick(Sender: TObject);
begin
  ShowAForm(TfrmImportBitmap);
  EnableDeleteImage;
end;

procedure TfrmGoPhast.miEditBitmapsClick(Sender: TObject);
begin
  // Check that there is an image to edit.
  if frmGoPhast.PhastModel.Bitmaps.Count > 0 then
  begin
    // Check if there are more than one image that could be edited.
    if frmGoPhast.PhastModel.Bitmaps.Count = 1 then
    begin
      // There is only one image that can be edited.
      // Allow the user to edit it.
      with TfrmImportBitmap.Create(nil) do
      begin
        try
          GetData(frmGoPhast.PhastModel.Bitmaps.Items[0] as TCompressedBitmapItem);
          ShowModal;
        finally
          Free;
        end;
      end;
    end
    else
    begin
      // Let the user pick which image to edit.
      ShowAForm(TfrmSelectImage);
    end;
  end
  else
  begin
    // There aren't any images to edit.  Allow the user to import one instead.
    miImportBitmapClick(nil);
  end;
  EnableDeleteImage;
end;

procedure TfrmGoPhast.SelectDefaultButton;
var
  AList: TList;
  AllUp: boolean;
  Index: integer;
  Button: TToolButton;
begin
  AList := TList.Create;
  try
    FillButtonList(AList);
    AllUp := True;
    for Index := 0 to AList.Count - 1 do
    begin
      Button := AList[Index];
      if Button.Down then
      begin
        AllUp := False;
        Break;
      end;
    end;
    if AllUp then
    begin
      tbSelect.Down := True;
      tbSelectClick(tbSelect);
    end;
  finally
    AList.Free
  end;
end;

procedure TfrmGoPhast.miScaleRotateorMoveObjectsClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmScaleRotateMove);
end;

procedure TfrmGoPhast.miSearchForObjectClick(Sender: TObject);
begin
  // Allow the user to select an object based on what it does.
  ShowAForm(TfrmSearch);
end;

procedure TfrmGoPhast.miShowSelectedObjectsClick(Sender: TObject);
begin
  frmSelectedObjects.Show;
end;

procedure TfrmGoPhast.miShowVideoTipsClick(Sender: TObject);
begin
  inherited;
  miShowVideoTips.Checked := not miShowVideoTips.Checked;
  FIniFile.WriteBool(StrCustomization, StrShowTips, miShowVideoTips.Checked);
  WriteIniFile;
end;

procedure TfrmGoPhast.tbShellClick(Sender: TObject);
begin
  frame3DView.glWidModelView.Invalidate;
end;

procedure TfrmGoPhast.acShowGridShellExecute(Sender: TObject);
begin
  acShowGridShell.Checked := not acShowGridShell.Checked;
  tbShell.Down := acShowGridShell.Checked;
  frame3DView.glWidModelView.Invalidate;
end;

procedure TfrmGoPhast.acShowTopGridExecute(Sender: TObject);
begin
  acShowTopGrid.Checked := not acShowTopGrid.Checked;
  tbTopGrid.Down := acShowTopGrid.Checked;
  frame3DView.glWidModelView.Invalidate;
end;

procedure TfrmGoPhast.acShowFrontGridExecute(Sender: TObject);
begin
  acShowFrontGrid.Checked := not acShowFrontGrid.Checked;
  tbFrontGrid.Down := acShowFrontGrid.Checked;
  frame3DView.glWidModelView.Invalidate;
end;

procedure TfrmGoPhast.acShowSideGridExecute(Sender: TObject);
begin
  acShowSideGrid.Checked := not acShowSideGrid.Checked;
  tbSideGrid.Down := acShowSideGrid.Checked;
  frame3DView.glWidModelView.Invalidate;
end;

procedure TfrmGoPhast.acRestoreDefaultViewExecute(Sender: TObject);
begin
  frame3DView.SetDefaultOrientation;
  frame3DView.glWidModelView.Invalidate;
end;

procedure TfrmGoPhast.acRunModflowLgrExecute(Sender: TObject);
var
  FileName: string;
  AGrid: TCustomGrid;
  NameWriter: TNameFileWriter;
  Index: Integer;
  ChildModel: TChildModel;
  ChildModelNameFile: string;
begin
  inherited;
  AGrid := Grid;
  if (AGrid = nil) or (AGrid.ColumnCount <= 0)
    or (AGrid.RowCount <= 0) or (AGrid.LayerCount <= 0) then
  begin
    Beep;
    MessageDlg('You must define the grid before you can export the MODFLOW '
      + 'input files.', mtError, [mbOK], 0);
    Exit;
  end;
  for Index := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    AGrid := PhastModel.ChildModels[Index].ChildModel.Grid;
    if (AGrid = nil) or (AGrid.ColumnCount <= 0)
      or (AGrid.RowCount <= 0) or (AGrid.LayerCount <= 0) then
    begin
      Beep;
      MessageDlg('You must define the grid before you can export the MODFLOW '
        + 'input files.', mtError, [mbOK], 0);
      Exit;
    end;
  end;
  InitializeModflowLgrInputDialog;
  if sdModflowLgr.Execute then
  begin
    if sdModflowLgr.FileName <>
      PhastModel.FixFileName(sdModflowLgr.FileName) then
    begin
      Beep;
      MessageDlg('Space characters are not allowed in the names of '
        + 'MODFLOW input files.', mtError, [mbOK], 0);
      Exit;
    end;
    case PhastModel.ObservationPurpose of
      ofObserved: FObservationFileName := sdModflowLgr.FileName;
      ofPredicted: FPredictionFileName := sdModflowLgr.FileName;
      else Assert(False);
    end;
    // erase the list of model input files to be stored in the archive.
    frmGoPhast.PhastModel.ModelInputFiles.Clear;

    if not FileExists(PhastModel.ProgramLocations.ModflowLgrLocation) then
    begin
      ShowAForm(TfrmProgramLocations);
      if not FileExists(PhastModel.ProgramLocations.ModflowLgrLocation) then
      begin
        Beep;
        if MessageDlg('MODFLOW-LGR does not exist at the location you specified.  '
          + 'Do you still want to export the MODFLOW input files?', mtWarning,
          [mbYes, mbNo], 0) <> mrYes then
        begin
          Exit;
        end;
      end;
    end;
    if FileExists(PhastModel.ProgramLocations.ModflowLgrLocation) then
    begin
      PhastModel.AddModelInputFile(PhastModel.ProgramLocations.ModflowLgrLocation);
    end;
    frmErrorsAndWarnings.Clear;

    FileName := sdModflowLgr.FileName;
    frmFormulaErrors.sgErrors.BeginUpdate;
    CanDraw := False;
    try
      PhastModel.Grid.ThreeDDataSet := nil;
      PhastModel.Grid.TopDataSet := nil;
      PhastModel.Grid.FrontDataSet := nil;
      PhastModel.Grid.SideDataSet := nil;
      PhastModel.Grid.ThreeDContourDataSet := nil;
      PhastModel.Grid.TopContourDataSet := nil;
      PhastModel.Grid.FrontContourDataSet := nil;
      PhastModel.Grid.SideContourDataSet := nil;
      PhastModel.ThreeDTimeList := nil;
      PhastModel.TopTimeList := nil;
      PhastModel.FrontTimeList := nil;
      PhastModel.SideTimeList := nil;

      FileName := PhastModel.FixFileName(FileName);

      Assert(FRunModelSelection >= 0);
      case FRunModelSelection of
        0:
          begin
            NameWriter := TNameFileWriter.Create(PhastModel, FileName);
            try
              PhastModel.NameFileWriter := NameWriter;
              for Index := 0 to PhastModel.ChildModels.Count - 1 do
              begin
                ChildModel := PhastModel.ChildModels[Index].ChildModel;
                ChildModelNameFile := ChildModel.Child_NameFile_Name(FileName);
                NameWriter := TNameFileWriter.Create(ChildModel,
                  ChildModelNameFile);
                ChildModel.NameFileWriter := NameWriter;
              end;
              PhastModel.ExportModflowLgrModel(FileName, FRunModflow);
            finally
              PhastModel.NameFileWriter.Free;
              PhastModel.NameFileWriter := nil;
              for Index := 0 to PhastModel.ChildModels.Count - 1 do
              begin
                ChildModel := PhastModel.ChildModels[Index].ChildModel;
                ChildModel.NameFileWriter.Free;
                ChildModel.NameFileWriter := nil;
              end;
            end;
          end;
        1:
          begin
            NameWriter := TNameFileWriter.Create(PhastModel, FileName);
            try
              PhastModel.NameFileWriter := NameWriter;
              PhastModel.ExportSeparateLgrModel(FileName, FRunModflow);
            finally
              PhastModel.NameFileWriter.Free;
              PhastModel.NameFileWriter := nil;
            end;
          end;
        else
          begin
            ChildModel := PhastModel.ChildModels[FRunModelSelection-2].ChildModel;
            ChildModelNameFile := ChildModel.Child_NameFile_Name(FileName);
            NameWriter := TNameFileWriter.Create(ChildModel,
              ChildModelNameFile);
            try
              ChildModel.NameFileWriter := NameWriter;
              ChildModel.ExportSeparateLgrModel(FileName, FRunModflow);
            finally
              ChildModel.NameFileWriter.Free;
              ChildModel.NameFileWriter := nil;
            end;
          end;
      end;


    finally
      CanDraw := True;
      frmFormulaErrors.sgErrors.EndUpdate;
    end;

    if frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;
  end;
end;

procedure TfrmGoPhast.tb3DColorsClick(Sender: TObject);
begin
  acColoredGrid.Checked := not acColoredGrid.Checked;
  tb3DColors.Down := acColoredGrid.Checked;
  PhastModel.Grid.GridChanged;
end;

procedure TfrmGoPhast.miShowHideObjectsClick(Sender: TObject);
begin
  if frmShowHideObjects = nil then
  begin
    frmShowHideObjects := TfrmShowHideObjects.Create(nil);
  end;
  frmShowHideObjects.Show;
  if frmShowHideObjects.WindowState = wsMinimized then
  begin
    frmShowHideObjects.WindowState := wsNormal;
  end;
end;

procedure TfrmGoPhast.mi3D_ColorsClick(Sender: TObject);
begin
  frmColors.Show;
end;

procedure TfrmGoPhast.tb3DObjectsClick(Sender: TObject);
begin
  acShow3DObjects.Checked := not acShow3DObjects.Checked;
  tb3DObjects.Down := acShow3DObjects.Checked;
  frame3DView.glWidModelView.Invalidate;
  UpdateDisplay(nil);
end;

procedure TfrmGoPhast.BringFormsToFront(Sender: TObject);
begin
  if frmProgressMM <> nil then
  begin
    if frmProgressMM.Visible then
    begin
      frmProgressMM.BringToFront;
    end;
  end;
  if frmSelectedObjects <> nil then
  begin
    if frmSelectedObjects.Visible then
    begin
      frmSelectedObjects.BringToFront;
    end;
  end;
  if frmColors <> nil then
  begin
    if frmColors.Visible then
    begin
      frmColors.BringToFront;
    end;
  end;

end;

procedure TfrmGoPhast.miShowHideBitmapsClick(Sender: TObject);
var
  Item: TCompressedBitmapItem;
begin
  if PhastModel.Bitmaps.Count > 0 then
  begin
    if PhastModel.Bitmaps.Count = 1 then
    begin
      Item := PhastModel.Bitmaps.Items[0] as
        TCompressedBitmapItem;
      Item.Visible := not Item.Visible;
    end
    else
    begin
      ShowAForm(TfrmShowHideBitmaps);
    end;
  end
  else
  begin
    ShowMessage('This model doesn''t have any images.');
  end;
end;

procedure TfrmGoPhast.miSelectObjectsByNameClick(Sender: TObject);
begin
  with TfrmSelectObjects.Create(nil) do
  begin
    try
      if tabTop.TabVisible or tabFront.TabVisible or tabSide.TabVisible then
      begin
        ShowModal;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmGoPhast.miSelectObjectsforEditingClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmSelectObjectsForEditing)
end;

procedure TfrmGoPhast.SetCanDraw(const Value: boolean);
begin
  FCanDraw := Value;
  if frame3DView <> nil then
  begin
    frame3DView.glWidModelView.Visible := FCanDraw;
  end;
end;

procedure TfrmGoPhast.SetChangingSelection(const Value: boolean);
begin
  if FChangingSelection <> Value then
  begin
    FChangingSelection := Value;
    if not FChangingSelection then
    begin
      self.ScreenObjectSelectionChange(nil);
    end;
  end;
end;

procedure TfrmGoPhast.SetCreateArchive(const Value: Boolean);
begin
  FCreateArchive := Value;
  CreateArchiveSet := True;
end;

procedure TfrmGoPhast.SetCurrentTool(const Value: TCustomInteractiveTool);
begin
  if CurrentTool <> Value then
  begin
    if CurrentTool <> nil then
    begin
      CurrentTool.Deactivate;
    end;
    FCurrentTool := Value;
    if CurrentTool <> nil then
    begin
      CurrentTool.Activate;
    end;
  end;
end;

procedure TfrmGoPhast.miImportPointsClick(Sender: TObject);
begin
  ShowAForm(TfrmImportPoints);
end;

procedure TfrmGoPhast.miEditSelectedObjectsClick(Sender: TObject);
begin
  EditScreenObjects;
end;

procedure TfrmGoPhast.OpenMostRecentlyUsed(Sender: TObject);
var
  Item: TRecentFileMenuItem;
begin
  if CheckModel then
  begin
    Item := Sender as TRecentFileMenuItem;
    OpenAFile(Item.FileName);
  end;
end;

procedure TfrmGoPhast.miObservationsClick(Sender: TObject);
begin
  inherited;
  if miObservations.Checked then
  begin
    PhastModel.ObservationPurpose := ofObserved;
  end
  else
  begin
    PhastModel.ObservationPurpose := ofPredicted;
  end;
end;

procedure TfrmGoPhast.miOutputControlClick(Sender: TObject);
begin
  inherited;
  ShowAForm(TfrmModflowOutputControl);
end;

procedure TfrmGoPhast.miPackagesClick(Sender: TObject);
begin
  inherited;
  if frmModflowPackages = nil then
  begin
    Application.CreateForm(TfrmModflowPackages, frmModflowPackages);
  end;
  frmModflowPackages.GetData;
  frmModflowPackages.ShowModal;
end;

procedure TfrmGoPhast.miPathlinestoShapefileClick(Sender: TObject);
var
  FileName: string;
begin
  inherited;
  FileName := ChangeFileExt(PhastModel.ModelFileName, '');
  if FileName = '' then
  begin
    FileName := IncludeTrailingPathDelimiter(GetCurrentDir) + StrPathlineshp;
  end
  else
  begin
    FileName := FileName + '_' + StrPathlineshp;
  end;
  sdShapefile.FileName := FileName;
  if sdShapefile.Execute then
  begin
    PhastModel.PathLines.Lines.ExportShapefile(sdShapefile.FileName);
  end;
end;

procedure TfrmGoPhast.miExamplesClick(Sender: TObject);
begin
  HelpRouter.HelpJump('', 'Examples');
end;

procedure TfrmGoPhast.miHelpOnMainWindowClick(Sender: TObject);
begin
  HelpRouter.HelpJump('', HelpKeyword);
end;

{ TMenuItemHint }

constructor TMenuItemHint.Create(AOwner: TComponent);
begin
  inherited;

  showTimer := TTimer.Create(self);
  showTimer.Interval := Application.HintPause;

  hideTimer := TTimer.Create(self);
  hideTimer.Interval := Application.HintHidePause;
end; (*Create*)

destructor TMenuItemHint.Destroy;
begin
  hideTimer.OnTimer := nil;
  showTimer.OnTimer := nil;
  self.ReleaseHandle;
  inherited;
end; (*Destroy*)

procedure TMenuItemHint.DoActivateHint(menuItem: TMenuItem);
begin
  //force remove of the "old" hint window
  hideTime(self);

  if (menuItem = nil) or (menuItem.Hint = '') then
  begin
    activeMenuItem := nil;
    Exit;
  end;

  activeMenuItem := menuItem;

  showTimer.OnTimer := ShowTime;
  hideTimer.OnTimer := HideTime;
end; (*DoActivateHint*)

procedure TMenuItemHint.ShowTime(Sender: TObject);
var
  r : TRect;
  wdth : integer;
  hght : integer;
begin
  if activeMenuItem <> nil then
  begin
    // RBW begin modification.
    Color := Application.HintColor;
    // RBW end modification

    //position and resize
    wdth := Canvas.TextWidth(activeMenuItem.Hint);
    hght := Canvas.TextHeight(activeMenuItem.Hint);

    r.Left := Mouse.CursorPos.X + 16;
    r.Top := Mouse.CursorPos.Y + 16;
    r.Right := r.Left + wdth + 6;
    r.Bottom := r.Top + hght + 4;

    ActivateHint(r,activeMenuItem.Hint);
  end;

  showTimer.OnTimer := nil;
end; (*ShowTime*)

procedure TMenuItemHint.HideTime(Sender: TObject);
begin
  //hide (destroy) hint window
  self.ReleaseHandle;
  hideTimer.OnTimer := nil;
end; (*HideTime*)

{ TPositionList }

procedure TPositionList.Clear;
begin
  FList.Clear;
  FCurrentPosition := 0;
end;

constructor TPositionList.Create(MaxPositions: integer);
begin
  Assert(MaxPositions > 0);
  FMaxPositions := MaxPositions;
  FCurrentPosition := 0;
  FList:= TObjectList.Create;
end;

destructor TPositionList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TPositionList.GetCanRedo: boolean;
begin
  result := FCurrentPosition < FList.Count;
end;

function TPositionList.GetCanUndo: boolean;
begin
  result := (FCurrentPosition > 1) and (FList.Count > 0);
end;

procedure TPositionList.Redo;
var
  APosition: TPositionStorage;
begin
  if CanRedo then
  begin
    APosition := FList[FCurrentPosition];
    Inc(FCurrentPosition);
    if Assigned(FOnNewPosition) then
    begin
      FOnNewPosition(self, APosition);
    end;
  end;
end;

procedure TPositionList.Submit(NewPosition: TPositionStorage);
var
  Index: Integer;
begin
  for Index := FList.Count - 1 downto FCurrentPosition do
  begin
    FList.Delete(Index);
  end;

  FList.Add(NewPosition);
  if FCurrentPosition = FMaxPositions then
  begin
    FList.Delete(0);
  end
  else
  begin
    Inc(FCurrentPosition);
  end;
  if Assigned(FOnNewPosition) then
  begin
    FOnNewPosition(self, NewPosition);
  end;
end;

procedure TPositionList.Undo;
var
  APosition: TPositionStorage;
begin
  if CanUndo then
  begin
    Dec(FCurrentPosition);
    APosition := FList[FCurrentPosition-1];
    if Assigned(FOnNewPosition) then
    begin
      FOnNewPosition(self, APosition);
    end;
  end;
end;

end.

