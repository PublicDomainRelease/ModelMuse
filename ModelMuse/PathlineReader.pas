unit PathlineReader;

interface

uses Windows, Classes, SysUtils, GoPhastTypes, ColorSchemes, Graphics, GR32,
  OpenGL12x, RealListUnit, QuadtreeClass;

type
  TShowChoice = (scAll, scSpecified, scStart, scEnd);

  TShowIntegerLimit = class(TPersistent)
  private
    FEndLimit: integer;
    FUseLimit: boolean;
    FStartLimit: integer;
    procedure SetEndLimit(const Value: integer);
    procedure SetStartLimit(const Value: integer);
    procedure SetUseLimit(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property UseLimit: boolean read FUseLimit write SetUseLimit;
    property StartLimit: integer read FStartLimit write SetStartLimit;
    property EndLimit: integer read FEndLimit write SetEndLimit;
  end;

  TShowFloatLimit = class(TPersistent)
  private
    FEndLimit: double;
    FUseLimit: boolean;
    FStartLimit: double;
    procedure SetEndLimit(const Value: double);
    procedure SetStartLimit(const Value: double);
    procedure SetUseLimit(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property UseLimit: boolean read FUseLimit write SetUseLimit;
    property StartLimit: double read FStartLimit write SetStartLimit;
    property EndLimit: double read FEndLimit write SetEndLimit;
  end;

  TPathLineDisplayLimits = class(TPersistent)
  private
    FLimitToCurrentIn2D: boolean;
    FLayerLimits: TShowIntegerLimit;
    FRowLimits: TShowIntegerLimit;
    FColumnLimits: TShowIntegerLimit;
    FShowChoice: TShowChoice;
    FTimeLimits: TShowFloatLimit;
    procedure SetLimitToCurrentIn2D(const Value: boolean);
    procedure SetColumnLimits(const Value: TShowIntegerLimit);
    procedure SetLayerLimits(const Value: TShowIntegerLimit);
    procedure SetRowLimits(const Value: TShowIntegerLimit);
    procedure SetShowChoice(const Value: TShowChoice);
    procedure SetTimeLimits(const Value: TShowFloatLimit);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
    Destructor Destroy; override;
  published
    property ShowChoice: TShowChoice read FShowChoice write SetShowChoice;
    property LimitToCurrentIn2D: boolean read FLimitToCurrentIn2D
      write SetLimitToCurrentIn2D default True;
    property ColumnLimits: TShowIntegerLimit read FColumnLimits write SetColumnLimits;
    property RowLimits: TShowIntegerLimit read FRowLimits write SetRowLimits;
    property LayerLimits: TShowIntegerLimit read FLayerLimits write SetLayerLimits;
    property TimeLimits: TShowFloatLimit read FTimeLimits write SetTimeLimits;
  end;

  TColorLimitChoice = (clcNone, clcTime, clcXPrime, clcYPrime, clcZ);

  TPathlineColorLimits = class(TPersistent)
  private
    FColoringChoice: TColorLimitChoice;
    FMaxColorLimit: double;
    FMinColorLimit: double;
    FUseLimit: boolean;
    procedure SetColoringChoice(const Value: TColorLimitChoice);
    procedure SetMinColorLimit(const Value: double);
    procedure SetMaxColorLimit(const Value: double);
    procedure SetUseLimit(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
  published
    property ColoringChoice: TColorLimitChoice read FColoringChoice
      write SetColoringChoice default clcTime;
    property MinColorLimit: double read FMinColorLimit write SetMinColorLimit;
    property MaxColorLimit: double read FMaxColorLimit write SetMaxColorLimit;
    property UseLimit: boolean read FUseLimit write SetUseLimit;
  end;

  TPathLine = class;

  TPathLinePoint = class(TCollectionItem)
  private
    FLayer: integer;
    FTimeStep: integer;
    FLocalZ: double;
    FZ: double;
    FX: double;
    FY: double;
    FTime: double;
    FRow: integer;
    FColumn: integer;
    FXPrime: double;
    FYPrime: double;
    function CheckLimits(Limits: TPathLineDisplayLimits): boolean;
  public
    procedure Assign(Source: TPersistent); override;
    function ShouldShow(Limits: TPathLineDisplayLimits;
      Orientation: TDataSetOrientation; CurrentColRowOrLayer: integer): boolean;
    function ShouldShowLine(Limits: TPathLineDisplayLimits): boolean;
    function ParentLine: TPathLine;
  published
    // Real world X coordinate
    property X: double read FX write FX;
    // Real world Y coordinate
    property Y: double read FY write FY;
    property Z: double read FZ write FZ;
    // X position in grid coordinates;
    property XPrime: double read FXPrime write FXPrime;
    // Y position in grid coordinates;
    property YPrime: double read FYPrime write FYPrime;
    property LocalZ: double read FLocalZ write FLocalZ;
    property Time: double read FTime write FTime;
    property Layer: integer read FLayer write FLayer;
    property Row: integer read FRow write FRow;
    property Column: integer read FColumn write FColumn;
    property TimeStep: integer read FTimeStep write FTimeStep;
  end;

  TPathLinePoints = class(TCollection)
  private
    FPathLine: TPathLine;
    function GetPoint(Index: integer): TPathLinePoint;
  public
    Constructor Create(PathLine: TPathLine);
    property Points[Index: integer]: TPathLinePoint read GetPoint; default;
    function TestGetMaxTime(var Maxtime: double): boolean;
  end;

  TPathLine = class(TCollectionItem)
  private
    FPoints: TPathLinePoints;
    procedure SetPoints(const Value: TPathLinePoints);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  published
    property Points: TPathLinePoints read FPoints write SetPoints;
  end;

  TPathLines = class(TCollection)
  private
    function GetLine(Index: integer): TPathLine;
  public
    Constructor Create;
    property Lines[Index: integer]: TPathLine read GetLine; default;
    procedure ExportShapefile(FileName: string);
    function TestGetMaxTime(var Maxtime: double): boolean;
  end;

  TCustomModpathSettings = class(TPersistent)
  private
    FColorParameters: TColorParameters;
    procedure SetColorParameters(const Value: TColorParameters);
  protected
    FVisible: boolean;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
    Destructor Destroy; override;
  published
    property ColorParameters: TColorParameters read FColorParameters
      write SetColorParameters;
    property Visible: boolean read FVisible
      write FVisible default True;
  end;

  TPathLineSettings = class(TCustomModpathSettings)
  private
    FDisplayLimits: TPathLineDisplayLimits;
    FColorLimits: TPathlineColorLimits;
    procedure SetDisplayLimits(const Value: TPathLineDisplayLimits);
    procedure SetColorLimits(const Value: TPathlineColorLimits);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
    Destructor Destroy; override;
  published
    property DisplayLimits: TPathLineDisplayLimits read FDisplayLimits
      write SetDisplayLimits;
    property ColorLimits: TPathlineColorLimits read FColorLimits
      write SetColorLimits;
    property DisplayPathLines: boolean read FVisible
      write FVisible Stored False;
   end;

  TPathLineReader = class(TPathLineSettings)
  private
    FLines: TPathLines;
    FFileName: string;
    FFile: TFileStream;
    FFileDate: TDateTime;
    FMaxTime: double;
    FMinTime: double;
    FRecordedPathLines: Boolean;
    FDrawingPathLines: Boolean;
    FTopQuadTree: TRbwQuadTree;
    FFrontQuadTree: TRbwQuadTree;
    FSideQuadTree: TRbwQuadTree;
    class var
      FPathlineGLIndex: TGLuint;
      FListInitialized: boolean;
    procedure SetLines(const Value: TPathLines);
    procedure SetFileDate(const Value: TDateTime);
    procedure SetMaxTime(const Value: double);
    procedure SetMinTime(const Value: double);
    function GetPointColor(MaxValue, MinValue: double;
      Point: TPathLinePoint): TColor;
    procedure GetMinMaxValues(var MaxValue: Double; var MinValue: Double);
    function CheckShowLine(Line: TPathLine): Boolean;
    class function GetPathlineGLIndex: TGLuint; static;
    procedure Record3DPathLines;
  protected
    class property PathlineGLIndex: TGLuint read GetPathlineGLIndex;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
    Destructor Destroy; override;
    procedure ReadFile;
    procedure Draw(Orientation: TDataSetOrientation; const BitMap: TBitmap32);
    procedure Draw3D;
    procedure Invalidate;
    property TopQuadTree: TRbwQuadTree read FTopQuadTree;
    property FrontQuadTree: TRbwQuadTree read FFrontQuadTree;
    property SideQuadTree: TRbwQuadTree read FSideQuadTree;
  published
    property Lines: TPathLines read FLines write SetLines;
    property FileName: string read FFileName write FFileName;
    property FileDate: TDateTime read FFileDate write SetFileDate;
    property MaxTime: double read FMaxTime write SetMaxTime;
    property MinTime: double read FMinTime write SetMinTime;
  end;

  TEndpointShowChoice = (escAll, escSpecified);
  TWhereToPlot = (wtpStart, wtpEnd);

  TEndPointDisplayLimits = class(TPersistent)
  private
    FLimitToCurrentIn2D: boolean;
    FStartLayerLimits: TShowIntegerLimit;
    FStartRowLimits: TShowIntegerLimit;
    FStartColumnLimits: TShowIntegerLimit;
    FShowChoice: TEndpointShowChoice;
    FReleaseTimeLimits: TShowFloatLimit;
    FEndZoneLimits: TShowIntegerLimit;
    FEndLayerLimits: TShowIntegerLimit;
    FTrackingTimeLimits: TShowFloatLimit;
    FEndRowLimits: TShowIntegerLimit;
    FEndColumnLimits: TShowIntegerLimit;
    FStartZoneLimits: TShowIntegerLimit;
    FWhereToPlot: TWhereToPlot;
    procedure SetLimitToCurrentIn2D(const Value: boolean);
    procedure SetStartColumnLimits(const Value: TShowIntegerLimit);
    procedure SetStartLayerLimits(const Value: TShowIntegerLimit);
    procedure SetStartRowLimits(const Value: TShowIntegerLimit);
    procedure SetShowChoice(const Value: TEndpointShowChoice);
    procedure SetReleaseTimeLimits(const Value: TShowFloatLimit);
    procedure SetEndColumnLimits(const Value: TShowIntegerLimit);
    procedure SetEndLayerLimits(const Value: TShowIntegerLimit);
    procedure SetEndRowLimits(const Value: TShowIntegerLimit);
    procedure SetEndZoneLimits(const Value: TShowIntegerLimit);
    procedure SetStartZoneLimits(const Value: TShowIntegerLimit);
    procedure SetTrackingTimeLimits(const Value: TShowFloatLimit);
    procedure SetWhereToPlot(const Value: TWhereToPlot);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
    Destructor Destroy; override;
  published
    property ShowChoice: TEndpointShowChoice read FShowChoice 
      write SetShowChoice;
    property LimitToCurrentIn2D: boolean read FLimitToCurrentIn2D
      write SetLimitToCurrentIn2D default True;
    property StartColumnLimits: TShowIntegerLimit read FStartColumnLimits 
      write SetStartColumnLimits;
    property StartRowLimits: TShowIntegerLimit read FStartRowLimits 
      write SetStartRowLimits;
    property StartLayerLimits: TShowIntegerLimit read FStartLayerLimits
      write SetStartLayerLimits;
    property StartZoneLimits: TShowIntegerLimit read FStartZoneLimits 
      write SetStartZoneLimits;
    property ReleaseTimeLimits: TShowFloatLimit read FReleaseTimeLimits 
      write SetReleaseTimeLimits;
    property EndColumnLimits: TShowIntegerLimit read FEndColumnLimits 
      write SetEndColumnLimits;
    property EndRowLimits: TShowIntegerLimit read FEndRowLimits 
      write SetEndRowLimits;
    property EndLayerLimits: TShowIntegerLimit read FEndLayerLimits 
      write SetEndLayerLimits;
    property EndZoneLimits: TShowIntegerLimit read FEndZoneLimits
      write SetEndZoneLimits;
    property TrackingTimeLimits: TShowFloatLimit read FTrackingTimeLimits
      write SetTrackingTimeLimits;
    property WhereToPlot: TWhereToPlot read FWhereToPlot write SetWhereToPlot 
      default wtpEnd;
  end;

  TEndPoint = class(TCollectionItem)
  private
    FStartTimeStep: integer;
    FStartLocalZ: double;
    FStartZ: double;
    FTerminationCode: integer;
    FStartX: double;
    FEndLayer: integer;
    FStartY: double;
    FEndZoneCode: integer;
    FStartXPrime: double;
    FStartRow: integer;
    FTrackingTime: double;
    FStartYPrime: double;
    FStartColumn: integer;
    FEndTimeStep: integer;
    FEndLocalZ: double;
    FEndZ: double;
    FEndX: double;
    FEndY: double;
    FEndXPrime: double;
    FEndRow: integer;
    FEndYPrime: double;
    FEndColumn: integer;
    FStartLayer: integer;
    FReleaseTime: double;
    FStartZoneCode: integer;
    function CheckLimits(Limits: TEndPointDisplayLimits): boolean;
  public
    procedure Assign(Source: TPersistent); override;
    function ShouldShow(Limits: TEndPointDisplayLimits;
      Orientation: TDataSetOrientation; CurrentColRowOrLayer: integer): boolean;
  published
    property EndZoneCode: integer read FEndZoneCode write FEndZoneCode;
    property EndColumn: integer read FEndColumn write FEndColumn;
    property EndRow: integer read FEndRow write FEndRow;
    property EndLayer: integer read FEndLayer write FEndLayer;
    // Real world X coordinate of end point
    property EndX: double read FEndX write FEndX;
    // Real world Y coordinate of end point
    property EndY: double read FEndY write FEndY;
    property EndZ: double read FEndZ write FEndZ;
    // X position in grid coordinates of end point;
    property EndXPrime: double read FEndXPrime write FEndXPrime;
    // Y position in grid coordinates of end point;
    property EndYPrime: double read FEndYPrime write FEndYPrime;
    property EndLocalZ: double read FEndLocalZ write FEndLocalZ;
    property TrackingTime: double read FTrackingTime write FTrackingTime;
    property StartZoneCode: integer read FStartZoneCode write FStartZoneCode;
    property StartColumn: integer read FStartColumn write FStartColumn;
    property StartRow: integer read FStartRow write FStartRow;
    property StartLayer: integer read FStartLayer write FStartLayer;
    // Real world X coordinate of starting point
    property StartX: double read FStartX write FStartX;
    // Real world Y coordinate of starting point
    property StartY: double read FStartY write FStartY;
    property StartZ: double read FStartZ write FStartZ;
    // X position in grid coordinates of starting point;
    property StartXPrime: double read FStartXPrime write FStartXPrime;
    // Y position in grid coordinates of starting point;
    property StartYPrime: double read FStartYPrime write FStartYPrime;
    property StartLocalZ: double read FStartLocalZ write FStartLocalZ;
    property StartTimeStep: integer read FStartTimeStep write FStartTimeStep;
    property TerminationCode: integer read FTerminationCode write FTerminationCode;
    property EndTimeStep: integer read FEndTimeStep write FEndTimeStep;
    property ReleaseTime: double read FReleaseTime write FReleaseTime;
  end;

  TEndPoints = class(TCollection)
  private
    function GetPoint(Index: integer): TEndPoint;
  public
    Constructor Create;
    property Points[Index: integer]: TEndPoint read GetPoint; default;
    procedure ExportShapefileAtStartingLocations(FileName: string);
    procedure ExportShapefileAtEndingLocations(FileName: string);
  end;

  TEndpointColorLimitChoice = (elcNone, elcReleaseTime, elcTrackingTime,
    elcStartXPrime, elcStartYPrime, elcStartZ, elcStartZone,
    elcEndXPrime, elcEndYPrime, elcEndZ, elcEndZone);

  TEndPointColorLimits = class(TPersistent)
  private
    FColoringChoice: TEndpointColorLimitChoice;
    FMinColorLimit: double;
    FUseLimit: boolean;
    FMaxColorLimit: double;
    procedure SetColoringChoice(const Value: TEndpointColorLimitChoice);
    procedure SetMaxColorLimit(const Value: double);
    procedure SetMinColorLimit(const Value: double);
    procedure SetUseLimit(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
  published
    property ColoringChoice: TEndpointColorLimitChoice read FColoringChoice
      write SetColoringChoice default elcTrackingTime;
    property MinColorLimit: double read FMinColorLimit write SetMinColorLimit;
    property MaxColorLimit: double read FMaxColorLimit write SetMaxColorLimit;
    property UseLimit: boolean read FUseLimit write SetUseLimit;
  end;

  TEndPointSettings = class(TCustomModpathSettings)
  private
    FColorLimits: TEndPointColorLimits;
    FDisplayLimits: TEndPointDisplayLimits;
    procedure SetColorLimits(const Value: TEndPointColorLimits);
    procedure SetDisplayLimits(const Value: TEndPointDisplayLimits);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
    Destructor Destroy; override;
  published
    property ColorLimits: TEndPointColorLimits read FColorLimits write
      SetColorLimits;
    property DisplayLimits: TEndPointDisplayLimits read FDisplayLimits
      write SetDisplayLimits;
    property DisplayEndPoints: boolean read FVisible
      write FVisible Stored False;
  end;

  TEndPointReader = class(TEndPointSettings)
  private
    FPoints: TEndPoints;
    FFileName: string;
    FFileDate: TDateTime;
    StartZ: Single;
    FMaxTrackingTime: double;
    FMinTrackingTime: double;
    FMinReleaseTime: double;
    FMaxReleaseTime: double;
    FMinStartZone: integer;
    FMaxEndZone: integer;
    FMinEndZone: integer;
    FMaxStartZone: integer;
    FDrawingEndPoints: Boolean;
    FRecordedEndPoints: Boolean;
    class var
      FListInitialized: Boolean;
      FPathlineGLIndex: Cardinal;
    procedure SetPoints(const Value: TEndPoints);
    procedure SetFileDate(const Value: TDateTime);
    class function GetEndPointGLIndex: TGLuint; static;
    procedure GetMinMaxValues(var MaxValue: Double; var MinValue: Double);
    function GetPointColor(MaxValue, MinValue: double;
      Point: TEndPoint): TColor;
    procedure SetMaxTrackingTime(const Value: double);
    procedure SetTrackingMinTime(const Value: double);
    procedure SetMaxReleaseTime(const Value: double);
    procedure SetMinReleaseTime(const Value: double);
    procedure SetMaxEndZone(const Value: integer);
    procedure SetMaxStartZone(const Value: integer);
    procedure SetMinEndZone(const Value: integer);
    procedure SetMinStartZone(const Value: integer);
    procedure Record3DEndPoints;
  protected
    class property EndPointGLIndex: TGLuint read GetEndPointGLIndex;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
    Destructor Destroy; override;
    procedure ReadFile;
    procedure Draw(Orientation: TDataSetOrientation; const BitMap: TBitmap32);
    procedure Draw3D;
    procedure Invalidate;
  published
    property FileDate: TDateTime read FFileDate write SetFileDate;
    property FileName: string read FFileName write FFileName;
    property MinReleaseTime: double read FMinReleaseTime
      write SetMinReleaseTime;
    property MaxReleaseTime: double read FMaxReleaseTime
      write SetMaxReleaseTime;
    property MinTrackingTime: double read FMinTrackingTime
      write SetTrackingMinTime;
    property MaxTrackingTime: double read FMaxTrackingTime
      write SetMaxTrackingTime;
    property MinStartZone: integer read FMinStartZone write SetMinStartZone;
    property MaxStartZone: integer read FMaxStartZone write SetMaxStartZone;
    property MinEndZone: integer read FMinEndZone write SetMinEndZone;
    property MaxEndZone: integer read FMaxEndZone write SetMaxEndZone;
    property Points: TEndPoints read FPoints write SetPoints;
  end;

  TTimeSeriesDisplayLimits = class(TPersistent)
  private
    FShowChoice: TShowChoice;
    FLayerLimits: TShowIntegerLimit;
    FLimitToCurrentIn2D: boolean;
    FRowLimits: TShowIntegerLimit;
    FColumnLimits: TShowIntegerLimit;
    procedure SetShowChoice(const Value: TShowChoice);
    procedure SetColumnLimits(const Value: TShowIntegerLimit);
    procedure SetLayerLimits(const Value: TShowIntegerLimit);
    procedure SetLimitToCurrentIn2D(const Value: boolean);
    procedure SetRowLimits(const Value: TShowIntegerLimit);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
    Destructor Destroy; override;
  published
    property ShowChoice: TShowChoice read FShowChoice write SetShowChoice;
    property LimitToCurrentIn2D: boolean read FLimitToCurrentIn2D
      write SetLimitToCurrentIn2D default True;
    property ColumnLimits: TShowIntegerLimit read FColumnLimits
      write SetColumnLimits;
    property RowLimits: TShowIntegerLimit read FRowLimits write SetRowLimits;
    property LayerLimits: TShowIntegerLimit read FLayerLimits
      write SetLayerLimits;
  end;

  TTimeSeriesPoint = class(TCollectionItem)
  private
    FTimeStepIndex: integer;
    FParticleIndex: integer;
    FLayer: integer;
    FTrackingTime: double;
    FTimeStep: integer;
    FLocalZ: double;
    FZ: double;
    FX: double;
    FY: double;
    FXPrime: double;
    FRow: integer;
    FYPrime: double;
    FColumn: integer;
    function CheckLimits(Limits: TTimeSeriesDisplayLimits): boolean;
  public
    procedure Assign(Source: TPersistent); override;
    function ShouldShow(Limits: TTimeSeriesDisplayLimits;
      Orientation: TDataSetOrientation; CurrentColRowOrLayer: integer): boolean;
    function ShouldShowSeries(Limits: TTimeSeriesDisplayLimits): boolean;
  published
    property TimeStepIndex: integer read FTimeStepIndex write FTimeStepIndex;
    property ParticleIndex: integer read FParticleIndex write FParticleIndex;
    property Layer: integer read FLayer write FLayer;
    property Row: integer read FRow write FRow;
    property Column: integer read FColumn write FColumn;
    property XPrime: double read FXPrime write FXPrime;
    property YPrime: double read FYPrime write FYPrime;
    property X: double read FX write FX;
    property Y: double read FY write FY;
    property Z: double read FZ write FZ;
    property LocalZ: double read FLocalZ write FLocalZ;
    property TrackingTime: double read FTrackingTime write FTrackingTime;
    property TimeStep: integer read FTimeStep write FTimeStep;
  end;

  TTimeSeriesPoints = class(TCollection)
  private
    function GetPoint(Index: integer): TTimeSeriesPoint;
  public
    Constructor Create;
    property Points[Index: integer]: TTimeSeriesPoint read GetPoint; default;
  end;

  // @name represents the position of a particle at different times.
  TTimeSeries = class(TCollectionItem)
  private
    FPoints: TTimeSeriesPoints;
    FTimes: TRealList;
    procedure SetPoints(const Value: TTimeSeriesPoints);
    function GetTimes: TRealList;
    procedure SetTimes(const Value: TRealList);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Times: TRealList read GetTimes write SetTimes;
  published
    property Points: TTimeSeriesPoints read FPoints write SetPoints;
  end;

  TTimeSeriesCollection = class(TCollection)
  private
    function GetSeries(Index: integer): TTimeSeries;
  public
    Constructor Create;
    property Series[Index: integer]: TTimeSeries read GetSeries; default;
  end;

  TTimeSeriesColorLimitChoice = (tscNone, tscParticleNumber,
    tscXPrime, tscYPrime, tscZ,
    tscStartXPrime, tscStartYPrime, tscStartZ,
    tscEndXPrime, tscEndYPrime, tscEndZ);

  TTimeSeriesColorLimits = class(TPersistent)
  private
    FColoringChoice: TTimeSeriesColorLimitChoice;
    FMinColorLimit: double;
    FUseLimit: boolean;
    FMaxColorLimit: double;
    procedure SetColoringChoice(const Value: TTimeSeriesColorLimitChoice);
    procedure SetMaxColorLimit(const Value: double);
    procedure SetMinColorLimit(const Value: double);
    procedure SetUseLimit(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
  published
    property ColoringChoice: TTimeSeriesColorLimitChoice read FColoringChoice
      write SetColoringChoice default tscParticleNumber;
    property MinColorLimit: double read FMinColorLimit write SetMinColorLimit;
    property MaxColorLimit: double read FMaxColorLimit write SetMaxColorLimit;
    property UseLimit: boolean read FUseLimit write SetUseLimit;
  end;

  TTimeSeriesSettings = class(TCustomModpathSettings)
  private
    FColorLimits: TTimeSeriesColorLimits;
    FDisplayLimits: TTimeSeriesDisplayLimits;
    procedure SetColorLimits(const Value: TTimeSeriesColorLimits);
    procedure SetDisplayLimits(const Value: TTimeSeriesDisplayLimits);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    Destructor Destroy; override;
  published
    property ColorLimits: TTimeSeriesColorLimits read FColorLimits
      write SetColorLimits;
    property DisplayLimits: TTimeSeriesDisplayLimits read FDisplayLimits
      write SetDisplayLimits;
    property DisplayTimeSeries: boolean read FVisible
      write FVisible Stored False;
  end;

  TTimeSeriesReader = class(TTimeSeriesSettings)
  private
    FFileName: string;
    FFileDate: TDateTime;
    FSeries: TTimeSeriesCollection;
    FMaxTime: double;
    FMinTime: double;
    FTimeIndex: integer;
    FDrawingTimeSeries: Boolean;
    FTimeSeriesGLIndex: array of TGLuint;
    FRecordedTimeSeries: array of Boolean;
    FRealList: TRealList;
    procedure SetFileDate(const Value: TDateTime);
    procedure SetLines(const Value: TTimeSeriesCollection);
    procedure SetMaxTime(const Value: double);
    procedure SetMinTime(const Value: double);
    procedure SetTimeIndex(const Value: integer);
    procedure GetMinMaxValues(var MaxValue: Double; var MinValue: Double);
    function CheckShowSeries(Series: TTimeSeries): Boolean;
    function GetPointColor(MaxValue, MinValue: double;
      Point: TTimeSeriesPoint): TColor;
    function GetRecordedTimeSeries(ATimeIndex: integer): boolean;
    procedure SetRecordedTimeSeries(ATimeIndex: integer; const Value: boolean);
    function GetTimeSeriesGLIndex(ATimeIndex: integer): TGLuint;
    procedure Record3DTimeSeries(TimeIndex: integer);
    procedure EnsureGLArrays(ATimeIndex: Integer);
    function GetTimes: TRealList;
    procedure SetTimes(const Value: TRealList);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    Destructor Destroy; override;
    procedure ReadFile;
    procedure Draw(Orientation: TDataSetOrientation; const BitMap: TBitmap32);
    procedure Draw3D;
    property RecordedTimeSeries[ATimeIndex: integer]: boolean read
      GetRecordedTimeSeries write SetRecordedTimeSeries;
    property TimeSeriesGLIndex[ATimeIndex: integer]: TGLuint
      read GetTimeSeriesGLIndex;
    property Times: TRealList read GetTimes write SetTimes;
    procedure Invalidate;
    procedure ExportShapefile(FileName: string);
  published
    property FileName: string read FFileName write FFileName;
    property FileDate: TDateTime read FFileDate write SetFileDate;
    property Series: TTimeSeriesCollection read FSeries write SetLines;
    property MaxTime: double read FMaxTime write SetMaxTime;
    property MinTime: double read FMinTime write SetMinTime;
    property TimeIndex: integer read FTimeIndex write SetTimeIndex;
  end;


implementation

uses
  Contnrs, frmGoPhastUnit, FastGEO, ZoomBox2, ModflowGridUnit, BigCanvasMethods,
  ModelMuseUtilities, PhastModelUnit, XBase1, frmExportShapefileUnit, 
  ShapefileUnit, AbstractGridUnit;

const
  StrSTARTLAY: AnsiString = 'START_LAY';
  StrSTARTROW: AnsiString = 'START_ROW';
  StrSTARTCOL: AnsiString = 'START_COL';
  StrSTARTTIME: AnsiString = 'START_TIME';
  StrENDLAY: AnsiString = 'END_LAY';
  StrENDROW: AnsiString = 'END_ROW';
  StrENDCOL: AnsiString = 'END_COL';
  StrENDTIME: AnsiString = 'END_TIME';
  StrSTARTTS: AnsiString = 'START_TS';
  StrSTARTZONE: AnsiString = 'START_ZONE';
  StrENDTS: AnsiString = 'END_TS';
  StrENDZONE: AnsiString = 'END_ZONE';
  StrENDX: AnsiString = 'END_X';
  StrENDY: AnsiString = 'END_Y';
  StrENDZ: AnsiString = 'END_Z';
  StrTRACKTIME: AnsiString = 'TRACK_TIME';
  StrTERMCODE: AnsiString = 'TERM_CODE';
  StrRELEASET: AnsiString = 'RELEASE_T';
  StrSTARTX: AnsiString = 'START_X';
  StrSTARTY: AnsiString = 'START_Y';
  StrSTARTZ: AnsiString = 'START_Z';
  StrLAYER: AnsiString = 'LAYER';
  StrROW: AnsiString = 'ROW';
  StrCOLUMN: AnsiString = 'COLUMN';
  StrTIMESTEP: AnsiString = 'TIME_STEP';
  StrPARTICLE: AnsiString = 'PARTICLE';

procedure ConvertIndicies(NCol, NRow: Integer;
  var I, K, J: Integer);
begin
  K := (J div (NRow * NCol)) + 1;
  J := J - (K - 1) * (NRow * NCol);
  I := (J div NCol) + 1;
  J := J - (I - 1) * NCol;
  if J = 0 then
  begin
    J := NCol;
    Dec(I);
    if I = 0 then
    begin
      I := NRow;
      Dec(K);
    end;
  end;
end;

procedure ConvertCoordinates(Grid: TModflowGrid; var XPrime, YPrime: single;
  var Point2D: TPoint2D);
begin
  // need to convert X and Y to real world coordinates.
  XPrime := XPrime + Grid.ColumnPosition[0];
  YPrime := YPrime + Grid.RowPosition[Grid.RowCount];
  Point2D.X := XPrime;
  Point2D.Y := YPrime;
  Point2D := Grid.RotateFromGridCoordinatesToRealWorldCoordinates(Point2D);
end;

procedure AssignColor(AColor: TColor);
var
  Red: TGLubyte;
  Green: TGLubyte;
  Blue: TGLubyte;
  Colors: array[0..3] of TGLfloat;
begin
  ExtractColorComponents(AColor, Red, Green, Blue);

  Colors[0] := Red / 255;
  Colors[1] := Green / 255;
  Colors[2] := Blue / 255;
  Colors[3] := 1;

  glColor3ub(Red, Green, Blue);

//    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, @Colors);
//    glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 0.7);
//    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @Colors);
end;

{ TPathLine }

procedure TPathLine.Assign(Source: TPersistent);
begin
  if Source is TPathLine then
  begin
    FPoints.Assign(TPathLine(Source).FPoints);
  end
  else
  begin
    inherited;
  end;
end;

constructor TPathLine.Create(Collection: TCollection);
begin
  inherited;
  FPoints := TPathLinePoints.Create(self);
end;

destructor TPathLine.Destroy;
begin
  FPoints.Free;
  inherited;
end;

procedure TPathLine.SetPoints(const Value: TPathLinePoints);
begin
  FPoints.Assign(Value);
end;

{ TPathLineReader }

procedure TPathLineReader.Assign(Source: TPersistent);
var
  PathLineReader: TPathLineReader;
begin
  if Source is TPathLineReader then
  begin
    PathLineReader := TPathLineReader(Source);
    Lines := PathLineReader.Lines;
    FileName := PathLineReader.FileName;
    MinTime := PathLineReader.MinTime;
    MaxTime := PathLineReader.MaxTime;
    FileDate := PathLineReader.FileDate;
    TopQuadTree.Clear;
    FrontQuadTree.Clear;
    SideQuadTree.Clear;
  end;
  inherited;
end;

constructor TPathLineReader.Create;
begin
  inherited;
  FLines := TPathLines.Create;
  FTopQuadTree := TRbwQuadTree.Create(nil);
  FFrontQuadTree := TRbwQuadTree.Create(nil);
  FSideQuadTree := TRbwQuadTree.Create(nil);
end;

destructor TPathLineReader.Destroy;
begin
  FSideQuadTree.Free;
  FFrontQuadTree.Free;
  FTopQuadTree.Free;
  FLines.Free;
  inherited;
end;


procedure TPathLineReader.Draw(Orientation: TDataSetOrientation;
  const BitMap: TBitmap32);
var
  LineIndex: Integer;
  Line: TPathLine;
  APoint: TPathLinePoint;
  ColRowOrLayer: integer;
  PointIndex: Integer;
  ShowPriorPoint: Boolean;
  ZoomBox: TQRbwZoomBox2;
  Points: array [0..1] of TPoint;
  ADisplayPoint: TPoint;
  MaxValue, MinValue: double;
  Grid: TModflowGrid;
  AColor: TColor;
  AColor32: TColor32;
  QuadTree: TRbwQuadTree;
  ShouldInitializeTree: Boolean;
  Limits: TGridLimit;
begin
  if not Visible then
  begin
    Exit;
  end;
  Grid := frmGoPhast.ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  if (Grid.LayerCount <= 0) or (Grid.RowCount <= 0)
    or (Grid.ColumnCount <= 0) then
  begin
    Exit;
  end;
  ColRowOrLayer := -1;
  ZoomBox := nil;
  QuadTree := nil;
  case Orientation of
    dsoTop:
      begin
        QuadTree := TopQuadTree;
        ZoomBox := frmGoPhast.frameTopView.ZoomBox;
        ColRowOrLayer := Grid.SelectedLayer+1;
      end;
    dsoFront:
      begin
        QuadTree := FrontQuadTree;
        ZoomBox := frmGoPhast.frameFrontView.ZoomBox;
        ColRowOrLayer := Grid.SelectedRow+1;
      end;
    dsoSide:
      begin
        QuadTree := SideQuadTree;
        ZoomBox := frmGoPhast.frameSideView.ZoomBox;
        ColRowOrLayer := Grid.SelectedColumn+1;
      end;
    dso3D: Assert(False);
    else Assert(False);
  end;
  GetMinMaxValues(MaxValue, MinValue);

  ShouldInitializeTree := QuadTree.Count = 0;
  if ShouldInitializeTree then
  begin
    Limits := Grid.GridLimits(OrientationToViewDirection(Orientation));
    case Orientation of
      dsoTop:
        begin
          QuadTree.XMax := Limits.MaxX;
          QuadTree.XMin := Limits.MinX;
          QuadTree.YMax := Limits.MaxY;
          QuadTree.YMin := Limits.MinY;
        end;
      dsoFront:
        begin
          QuadTree.XMax := Limits.MaxX;
          QuadTree.XMin := Limits.MinX;
          QuadTree.YMax := Limits.MaxZ;
          QuadTree.YMin := Limits.MinZ;
        end;
      dsoSide:
        begin
          QuadTree.XMax := Limits.MaxY;
          QuadTree.XMin := Limits.MinY;
          QuadTree.YMax := Limits.MaxZ;
          QuadTree.YMin := Limits.MinZ;
        end
      else Assert(False);
    end;
  end;
  for LineIndex := 0 to Lines.Count - 1 do
  begin
    Line := Lines[LineIndex];
    if Line.Points.Count > 0 then
    begin
      if CheckShowLine(Line) then
      begin
        ShowPriorPoint := False;
        Points[0].X := 0;
        Points[0].Y := 0;
        for PointIndex := 0 to Line.Points.Count - 1 do
        begin
          APoint := Line.Points[PointIndex];
          if APoint.ShouldShow(DisplayLimits, Orientation, ColRowOrLayer) then
          begin
            case Orientation of
              dsoTop:
                begin
                  ADisplayPoint.X := ZoomBox.XCoord(APoint.X);
                  ADisplayPoint.Y := ZoomBox.YCoord(APoint.Y);
                  if ShouldInitializeTree then
                  begin
                    QuadTree.AddPoint(APoint.X, APoint.Y, APoint);
                  end;
                end;
              dsoFront:
                begin
                  ADisplayPoint.X := ZoomBox.XCoord(APoint.X);
                  ADisplayPoint.Y := ZoomBox.YCoord(APoint.Z);
                  if ShouldInitializeTree then
                  begin
                    QuadTree.AddPoint(APoint.X, APoint.Z, APoint);
                  end;
                end;
              dsoSide:
                begin
                  ADisplayPoint.X := ZoomBox.XCoord(APoint.Z);
                  ADisplayPoint.Y := ZoomBox.YCoord(APoint.Y);
                  if ShouldInitializeTree then
                  begin
                    QuadTree.AddPoint(APoint.Z, APoint.X, APoint);
                  end;
                end;
              else Assert(False);
            end;
            Points[1] := ADisplayPoint;
            if ShowPriorPoint then
            begin
              AColor := GetPointColor(MaxValue, MinValue, APoint);
              AColor32 := Color32(AColor);
              DrawBigPolyline32(BitMap, AColor32, 1, Points, True);
            end;
            Points[0] := ADisplayPoint;
            ShowPriorPoint := True;
          end
          else
          begin
            ShowPriorPoint := False;
          end;
        end;
      end;
    end;
  end;
end;

procedure TPathLineReader.Draw3D;
var
  Grid: TModflowGrid;
begin
  if FDrawingPathLines then
  begin
    Exit;
  end;
  try
    FDrawingPathLines := True;


    if (not FRecordedPathLines) then
    begin
      Record3DPathLines;
      // FRecordedPathLines is private and won't be set
      // by overridden versions of RecordFront.
      FRecordedPathLines := True;
    end;

    if not Visible then
    begin
      Exit;
    end;
    Grid := frmGoPhast.ModflowGrid;
    if Grid = nil then
    begin
      Exit;
    end;
    if (Grid.LayerCount <= 0) or (Grid.RowCount <= 0)
      or (Grid.ColumnCount <= 0) then
    begin
      Exit;
    end;
//    EnableLighting;
    glCallList(PathlineGLIndex);
  finally
    FDrawingPathLines := False;
  end;

end;

procedure TPathLineReader.Record3DPathLines;
var
  LineIndex: Integer;
  Line: TPathLine;
  APoint: TPathLinePoint;
  ColRowOrLayer: integer;
  PointIndex: Integer;
  ShowPriorPoint: Boolean;
  MaxValue, MinValue: double;
  Grid: TModflowGrid;
  AColor: TColor;
  LineVisible: boolean;
  PriorPoint: TPathLinePoint;
  procedure StartLine;
  begin
    if not LineVisible then
    begin
      glBegin(GL_LINE_STRIP);
      LineVisible := True;
      AColor := GetPointColor(MaxValue, MinValue, PriorPoint);
      AssignColor(AColor);

      glVertex3f(PriorPoint.X, PriorPoint.Y, PriorPoint.Z);
    end;
  end;
  procedure EndLine;
  begin
    if LineVisible then
    begin
      glEnd;
      LineVisible := False;
    end;
  end;
begin
  if not Visible then
  begin
    Exit;
  end;
  Grid := frmGoPhast.ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  if (Grid.LayerCount <= 0) or (Grid.RowCount <= 0)
    or (Grid.ColumnCount <= 0) then
  begin
    Exit;
  end;
  ColRowOrLayer := -1;

//    EnableLighting;
  glMatrixMode(GL_MODELVIEW);

  glNewList(PathlineGLIndex, GL_COMPILE);
  try
    glPushMatrix;
    try
      glEnable(GL_LINE_SMOOTH);
      glShadeModel(GL_SMOOTH);

      GetMinMaxValues(MaxValue, MinValue);
      glLineWidth(1);

      LineVisible := False;
      for LineIndex := 0 to Lines.Count - 1 do
      begin
        Line := Lines[LineIndex];
        if Line.Points.Count > 0 then
        begin
          if CheckShowLine(Line) then
          begin
            PriorPoint := nil;
            ShowPriorPoint := False;
            for PointIndex := 0 to Line.Points.Count - 1 do
            begin
              APoint := Line.Points[PointIndex];
              if APoint.ShouldShow(DisplayLimits, dso3D, ColRowOrLayer) then
              begin

                if ShowPriorPoint then
                begin
                  StartLine;
                  AColor := GetPointColor(MaxValue, MinValue, APoint);
                  AssignColor(AColor);
                  glVertex3f(APoint.X, APoint.Y, APoint.Z);
                end;
                ShowPriorPoint := True;
                PriorPoint := APoint;
              end
              else
              begin
                ShowPriorPoint := False;
                EndLine;
              end;
            end;
          end;
          EndLine;
        end;
      end;
    finally
      glPopMatrix;
    end;
  finally
    glEndList;
  end;
end;

function TPathLineReader.CheckShowLine(Line: TPathLine): Boolean;
var
  APoint: TPathLinePoint;
begin
  result := True;
  case DisplayLimits.ShowChoice of
    scAll, scSpecified:
      begin
      end;
    scStart:
      begin
        APoint := Line.Points[0];
        result := APoint.ShouldShowLine(DisplayLimits);
      end;
    scEnd:
      begin
        APoint := Line.Points[Line.Points.Count - 1];
        result := APoint.ShouldShowLine(DisplayLimits);
      end;
  else
    Assert(False);
  end;
end;

procedure TPathLineReader.GetMinMaxValues(var MaxValue: Double; var MinValue: Double);
var
  Grid: TModflowGrid;
begin
  Grid := frmGoPhast.ModflowGrid;
  if ColorLimits.UseLimit then
  begin
    MinValue := ColorLimits.MinColorLimit;
    MaxValue := ColorLimits.MaxColorLimit;
  end
  else
  begin
    MinValue := 0;
    MaxValue := 1;
    case ColorLimits.ColoringChoice of
      clcNone:
        begin
          MinValue := 0;
          MaxValue := 1;
        end;
      clcTime:
        begin
          MinValue := MinTime;
          MaxValue := MaxTime;
        end;
      clcXPrime:
        begin
          MinValue := Grid.ColumnPosition[0];
          MaxValue := Grid.ColumnPosition[Grid.ColumnCount];
        end;
      clcYPrime:
        begin
          MaxValue := Grid.RowPosition[0];
          MinValue := Grid.RowPosition[Grid.RowCount];
        end;
      clcZ:
        begin
          MinValue := Grid.LowestElevation;
          MaxValue := Grid.HighestElevation;
        end;
    else
      Assert(False);
    end;
  end;
end;

class function TPathLineReader.GetPathlineGLIndex: TGLuint;
begin
  if not FListInitialized and frmGoPhast.frame3DView.glWidModelView.Started then
  begin
    FListInitialized := True;
    FPathlineGLIndex := glGenLists(1);
  end;
  result := FPathlineGLIndex;
end;

function TPathLineReader.GetPointColor(MaxValue, MinValue: double;
  Point: TPathLinePoint): TColor;
var
  AValue: Double;
begin
  AValue := 0;
  case ColorLimits.ColoringChoice of
    clcNone:
      begin
        result := clBlack;
        Exit;
      end;
    clcTime:
      begin
        AValue := Abs(Point.Time);
      end;
    clcXPrime:
      begin
        AValue := Point.XPrime;
      end;
    clcYPrime:
      begin
        AValue := Point.YPrime;
      end;
    clcZ:
      begin
        AValue := Point.Z;
      end;
    else Assert(False);
  end;
  if AValue > MaxValue then
  begin
    result := clBlack;
  end
  else if AValue < MinValue then
  begin
    result := clBlack;
  end
  else
  begin
    if MaxValue = MinValue then
    begin
      result := ColorParameters.FracToColor(0.5)
    end
    else
    begin
      result := ColorParameters.FracToColor(1-((AValue-MinValue)/(MaxValue-MinValue)))
    end;
  end;
end;

procedure TPathLineReader.Invalidate;
begin
  FRecordedPathLines := False;
end;

procedure TPathLineReader.ReadFile;
var
  AFile: TFileStream;
  AChar: AnsiChar;
  IsTextFile: Boolean;
  ALine: string;
  CompactFormat: Boolean;
  ParticleIndex: integer;
  XPrime: single;
  YPrime: single;
  LocalZ: single;
  Z: single;
  Time: single;
  J: integer;
  TS: integer;
  NRow: integer;
  NCol: integer;
  K: integer;
  I: integer;
  PathLine: TPathLine;
  APoint: TPathLinePoint;
  Description: array[0..79] of AnsiChar;
  // 4 null bytes separate Description from the following data.
  // Use Terminator to read and ignore those 4 null bytes.
  Terminator: array[0..3] of AnsiChar;
  Grid: TModflowGrid;
  ADate: TDateTime;
  LineIndex: Integer;
  Line: TPathLine;
  FirstPoint: TPathLinePoint;
  LastPoint: TPathLinePoint;
  FirstTimeFound: Boolean;
  FTextFile: TextFile;
  procedure CreateParticle;
  var
    Point2D: TPoint2D;
  begin
    While FLines.Count < ParticleIndex do
    begin
      FLines.Add;
    end;

    PathLine := FLines[ParticleIndex-1];

    APoint := PathLine.FPoints.Add as TPathLinePoint;
    ConvertCoordinates(Grid, XPrime, YPrime, Point2D);

    APoint.FXPrime := XPrime;
    APoint.FYPrime := YPrime;
    APoint.FX := Point2D.X;
    APoint.FY := Point2D.Y;
    APoint.FLocalZ := LocalZ;
    APoint.FZ := Z;
    APoint.FTime := Time;
    APoint.FLayer := K;
    APoint.FRow := I;
    APoint.FColumn := J;
    APoint.FTimeStep := TS;
    Assert(APoint.FLayer >= 1);
    Assert(APoint.FRow >= 1);
    Assert(APoint.FColumn >= 1);
  end;
begin
  Grid := frmGoPhast.ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  if FileAge(FileName, ADate) then
  begin
    FileDate := ADate;
  end;
  FLines.Clear;
  AFile := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
  try
    AFile.Read(AChar, SizeOf(AChar));
  finally
    AFile.Free;
  end;
  IsTextFile := AChar = '@';
  NRow := Grid.RowCount;
  NCol := Grid.ColumnCount;
  if IsTextFile then
  begin
    AssignFile(FTextFile, FFileName);
    try
      Reset(FTextFile);
      Readln(FTextFile, ALine);
      CompactFormat := Pos('COMPACT',ALine) >= 1;
      While Not Eof(FTextFile) do
      begin
        if CompactFormat then
        begin
          Readln(FTextFile, ParticleIndex, XPrime, YPrime, LocalZ, Z, Time, J, TS);
          ConvertIndicies(NCol, NRow, I, K, J);
        end
        else
        begin
          Readln(FTextFile, ParticleIndex, XPrime, YPrime, LocalZ, Z, Time, J, I, K, TS);
        end;
//        Time := Abs(Time);

        CreateParticle;
      end;
    finally
      CloseFile(FTextFile);
    end;
  end
  else
  begin
    FFile := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
    try
      AFile.Read(Description, SizeOf(Description));
      AFile.Read(Terminator, SizeOf(Terminator));
      while FFile.Position < FFile.Size do
      begin
        AFile.Read(ParticleIndex, SizeOf(ParticleIndex));
        AFile.Read(XPrime, SizeOf(XPrime));
        AFile.Read(YPrime, SizeOf(YPrime));
        AFile.Read(LocalZ, SizeOf(LocalZ));
        AFile.Read(Z, SizeOf(Z));
        AFile.Read(Time, SizeOf(Time));
        AFile.Read(J, SizeOf(J));
        AFile.Read(TS, SizeOf(TS));

        ConvertIndicies(NCol, NRow, I, K, J);

        CreateParticle;
      end;

    finally
      FFile.Free;
    end;
  end;

  FirstTimeFound := False;
  if Lines.Count > 0 then
  begin
    for LineIndex := 0 to Lines.Count - 1 do
    begin
      Line := Lines[LineIndex];
      if Line.Points.Count > 0 then
      begin
        FirstPoint := Line.Points[0];
        LastPoint := Line.Points[Line.Points.Count-1];
        if not FirstTimeFound then
        begin
          if FirstPoint.Time < LastPoint.Time then
          begin
            MinTime := FirstPoint.Time;
            MaxTime := LastPoint.Time;
          end
          else
          begin
            MinTime := LastPoint.Time;
            MaxTime := FirstPoint.Time;
          end;
          FirstTimeFound := True;
        end
        else
        begin
          if FirstPoint.Time < LastPoint.Time then
          begin
            if FirstPoint.Time < MinTime then
            begin
              MinTime := FirstPoint.Time
            end;
            if LastPoint.Time > MaxTime then
            begin
              MaxTime := LastPoint.Time;
            end;
          end
          else
          begin
            if LastPoint.Time < MinTime then
            begin
              MinTime := LastPoint.Time
            end;
            if FirstPoint.Time > MaxTime then
            begin
              MaxTime := FirstPoint.Time;
            end;
          end;
        end;
      end;
    end;
  end;

end;

procedure TPathLineReader.SetFileDate(const Value: TDateTime);
begin
  FFileDate := Value;
end;

procedure TPathLineReader.SetLines(const Value: TPathLines);
begin
  FLines.Assign(Value);
end;

procedure TPathLineReader.SetMaxTime(const Value: double);
begin
  FMaxTime := Value;
end;

procedure TPathLineReader.SetMinTime(const Value: double);
begin
  FMinTime := Value;
end;

{ TPathLinePoint }

procedure TPathLinePoint.Assign(Source: TPersistent);
var
  SourcePoint: TPathLinePoint;
begin
  if Source is TPathLinePoint then
  begin
    SourcePoint := TPathLinePoint(Source);
    X := SourcePoint.X;
    Y := SourcePoint.Y;
    Z := SourcePoint.Z;
    LocalZ := SourcePoint.LocalZ;
    Time := SourcePoint.Time;
    Layer := SourcePoint.Layer;
    Row := SourcePoint.Row;
    Column := SourcePoint.Column;
    TimeStep := SourcePoint.TimeStep;
    XPrime := SourcePoint.XPrime;
    YPrime := SourcePoint.YPrime;
  end
  else
  begin
    inherited;
  end;
end;

function TPathLinePoint.CheckLimits(Limits: TPathLineDisplayLimits): boolean;
begin
  result := True;
  if Limits.ColumnLimits.UseLimit then
  begin
    result := (Limits.ColumnLimits.StartLimit <= Column)
      and (Column <= Limits.ColumnLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.RowLimits.UseLimit then
  begin
    result := (Limits.RowLimits.StartLimit <= Row)
      and (Row <= Limits.RowLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.LayerLimits.UseLimit then
  begin
    result := (Limits.LayerLimits.StartLimit <= Layer)
      and (Layer <= Limits.LayerLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.TimeLimits.UseLimit then
  begin
    result := (Limits.TimeLimits.StartLimit <= Time)
      and (Time <= Limits.TimeLimits.EndLimit);
    if not result then Exit;
  end;
end;


function TPathLinePoint.ParentLine: TPathLine;
begin
  result := (Collection as TPathLinePoints).FPathLine;
end;

function TPathLinePoint.ShouldShow(Limits: TPathLineDisplayLimits;
  Orientation: TDataSetOrientation; CurrentColRowOrLayer: integer): boolean;
var
  ColRowOrLayerToCheck: integer;
begin
  result := True;
  if Limits.LimitToCurrentIn2D and (Orientation <> dso3D) then
  begin
    ColRowOrLayerToCheck := -1;
    case Orientation of
      dsoTop:
        begin
          ColRowOrLayerToCheck := Layer;
        end;
      dsoFront:
        begin
          ColRowOrLayerToCheck := Row;
        end;
      dsoSide:
        begin
          ColRowOrLayerToCheck := Column;
        end;
      else Assert(False);
    end;
    Assert(ColRowOrLayerToCheck >= 1);
    result := ColRowOrLayerToCheck = CurrentColRowOrLayer;
    if not result then
    begin
      Exit;
    end;
  end;
  case Limits.ShowChoice of
    scAll, scStart, scEnd:
      begin
        // do nothing
      end;
    scSpecified:
      begin
        result := CheckLimits(Limits);
        if not result then Exit;
      end;
    else Assert(False);
  end;
end;

function TPathLinePoint.ShouldShowLine(Limits: TPathLineDisplayLimits): boolean;
begin
  result := True;
  case Limits.ShowChoice of
    scAll:
      begin
        // do nothing
      end;
    scSpecified:
      begin
        // do nothing
      end;
    scStart:
      begin
        Assert(Index = 0);
        result := CheckLimits(Limits);
      end;
    scEnd:
      begin
        Assert(Index = Collection.Count-1);
        result := CheckLimits(Limits);
      end;
    else Assert(False);
  end;

end;


{ TPathLinePoints }

constructor TPathLinePoints.Create(PathLine: TPathLine);
begin
  inherited Create(TPathLinePoint);
  FPathLine := PathLine;
end;

function TPathLinePoints.GetPoint(Index: integer): TPathLinePoint;
begin
  result := Items[Index] as TPathLinePoint;
end;

function TPathLinePoints.TestGetMaxTime(var Maxtime: double): boolean;
begin
  result := Count > 0;
  if result then
  begin
    Maxtime := Points[Count -1].Time;
  end;
end;

{ TPathLines }

constructor TPathLines.Create;
begin
  inherited Create(TPathLine);
end;

procedure TPathLines.ExportShapefile(FileName: string);
var
  ShapeDataBase: TXBase;
  Fields: TStringList;
  ShapeFileWriter: TShapefileGeometryWriter;
  LineIndex: Integer;
  ALine: TPathLine;
  FirstPoint: TPathLinePoint;
  LastPoint: TPathLinePoint;
  PointIndex: Integer;
  APoint: TPathLinePoint;
  Shape: TShapeObject;
begin
  ShapeDataBase := TXBase.Create(nil);
  try
    Fields := TStringList.Create;
    try
      Fields.Add(string(StrSTARTLAY) + '=N');
      Fields.Add(string(StrSTARTROW) + '=N');
      Fields.Add(string(StrSTARTCOL) + '=N');
      Fields.Add(string(StrSTARTTIME) + '=N18,10');
      Fields.Add(string(StrENDLAY) + '=N');
      Fields.Add(string(StrENDROW) + '=N');
      Fields.Add(string(StrENDCOL) + '=N');
      Fields.Add(string(StrENDTIME) + '=N18,10');
      InitializeDataBase(FileName, ShapeDataBase, Fields);
    finally
      Fields.Free;
    end;

    ShapeFileWriter := TShapefileGeometryWriter.Create(stPolyLineZ, True);
    try
      for LineIndex := 0 to Count - 1 do
      begin
        ALine := Lines[LineIndex];
        if ALine.Points.Count > 0 then
        begin
          ShapeDataBase.AppendBlank;

          FirstPoint := ALine.Points[0];
          ShapeDataBase.UpdFieldInt(StrSTARTLAY, FirstPoint.FLayer);
          ShapeDataBase.UpdFieldInt(StrSTARTROW, FirstPoint.FRow);
          ShapeDataBase.UpdFieldInt(StrSTARTCOL, FirstPoint.FColumn);
          ShapeDataBase.UpdFieldNum(StrSTARTTIME, FirstPoint.FTime);

          LastPoint := ALine.Points[ALine.Points.Count-1];
          ShapeDataBase.UpdFieldInt(StrENDLAY, LastPoint.FLayer);
          ShapeDataBase.UpdFieldInt(StrENDROW, LastPoint.FRow);
          ShapeDataBase.UpdFieldInt(StrENDCOL, LastPoint.FColumn);
          ShapeDataBase.UpdFieldNum(StrENDTIME, LastPoint.FTime);

          ShapeDataBase.PostChanges;

          Shape := TShapeObject.Create;
          try
            Shape.FShapeType := stPolyLineZ;
            Shape.FNumPoints := ALine.Points.Count;
            SetLength(Shape.FPoints, ALine.Points.Count);
            SetLength(Shape.FZArray, ALine.Points.Count);
            SetLength(Shape.FMArray, ALine.Points.Count);
            Shape.FNumParts := 1;
            SetLength(Shape.FParts, 1);
            Shape.FParts[0] := 0;

            for PointIndex := 0 to ALine.Points.Count - 1 do
            begin
              APoint := ALine.Points[PointIndex];
              Shape.FMArray[PointIndex] := APoint.FTime;
              Shape.FPoints[PointIndex].X := APoint.FX;
              Shape.FPoints[PointIndex].Y := APoint.FY;
              Shape.FZArray[PointIndex] := APoint.FZ;
            end;
          except
            Shape.Free;
            raise;
          end;
          ShapeFileWriter.AddShape(Shape);
        end;
      end;
      ShapeFileWriter.WriteToFile(FileName, ChangeFileExt(FileName, '.shx'));
    finally
      ShapeFileWriter.Free;
    end;
  finally
    ShapeDataBase.Active := False;
    ShapeDataBase.Free;
  end;
end;

function TPathLines.GetLine(Index: integer): TPathLine;
begin
  result := Items[Index] as TPathLine;
end;

function TPathLines.TestGetMaxTime(var Maxtime: double): boolean;
var
  LineIndex: Integer;
  AValue: double;
begin
  result := False;
  for LineIndex := 0 to Count - 1 do
  begin
    if Lines[LineIndex].Points.TestGetMaxTime(AValue) then
    begin
      if result then
      begin
        if AValue > Maxtime then
        begin
          Maxtime := AValue;
        end
      end
      else
      begin
        Maxtime := AValue;
        result := True
      end;
    end;
  end;
end;

{ TPathLineDisplayLimits }

procedure TPathLineDisplayLimits.Assign(Source: TPersistent);
var
  SourceLimits: TPathLineDisplayLimits;
begin
  if Source is TPathLineDisplayLimits then
  begin
    SourceLimits:= TPathLineDisplayLimits(Source);
    ShowChoice := SourceLimits.ShowChoice;
    LimitToCurrentIn2D := SourceLimits.LimitToCurrentIn2D;
    ColumnLimits := SourceLimits.ColumnLimits;
    RowLimits := SourceLimits.RowLimits;
    LayerLimits := SourceLimits.LayerLimits;
    TimeLimits := SourceLimits.TimeLimits;
  end
  else
  begin
    inherited;
  end;
end;

constructor TPathLineDisplayLimits.Create;
begin
  inherited;
  FLayerLimits := TShowIntegerLimit.Create;
  FRowLimits := TShowIntegerLimit.Create;
  FColumnLimits := TShowIntegerLimit.Create;
  FTimeLimits := TShowFloatLimit.Create;
  FLimitToCurrentIn2D := True;
end;

destructor TPathLineDisplayLimits.Destroy;
begin
  FTimeLimits.Free;
  FColumnLimits.Free;
  FRowLimits.Free;
  FLayerLimits.Free;
  inherited;
end;

procedure TPathLineDisplayLimits.SetColumnLimits(const Value: TShowIntegerLimit);
begin
  FColumnLimits.Assign(Value);
end;

procedure TPathLineDisplayLimits.SetLayerLimits(const Value: TShowIntegerLimit);
begin
  FLayerLimits.Assign(Value);
end;

procedure TPathLineDisplayLimits.SetLimitToCurrentIn2D(const Value: boolean);
begin
  FLimitToCurrentIn2D := Value;
end;

procedure TPathLineDisplayLimits.SetRowLimits(const Value: TShowIntegerLimit);
begin
  FRowLimits.Assign(Value);
end;

procedure TPathLineDisplayLimits.SetShowChoice(const Value: TShowChoice);
begin
  FShowChoice := Value;
end;

procedure TPathLineDisplayLimits.SetTimeLimits(const Value: TShowFloatLimit);
begin
  FTimeLimits.Assign(Value);
end;

{ TShowLimit }

procedure TShowIntegerLimit.Assign(Source: TPersistent);
var
  SourceLimit: TShowIntegerLimit;
begin
  if Source is TShowIntegerLimit then
  begin
    SourceLimit:= TShowIntegerLimit(Source);
    UseLimit := SourceLimit.UseLimit;
    StartLimit := SourceLimit.StartLimit;
    EndLimit := SourceLimit.EndLimit;
  end
  else
  begin
    inherited;
  end;
end;

procedure TShowIntegerLimit.SetEndLimit(const Value: integer);
begin
  FEndLimit := Value;
end;

procedure TShowIntegerLimit.SetStartLimit(const Value: integer);
begin
  FStartLimit := Value;
end;

procedure TShowIntegerLimit.SetUseLimit(const Value: boolean);
begin
  FUseLimit := Value;
end;

{ TShowFloatLimit }

procedure TShowFloatLimit.Assign(Source: TPersistent);
var
  SourceLimit: TShowFloatLimit;
begin
  if Source is TShowFloatLimit then
  begin
    SourceLimit:= TShowFloatLimit(Source);
    UseLimit := SourceLimit.UseLimit;
    StartLimit := SourceLimit.StartLimit;
    EndLimit := SourceLimit.EndLimit;
  end
  else
  begin
    inherited;
  end;
end;

procedure TShowFloatLimit.SetEndLimit(const Value: double);
begin
  FEndLimit := Value;
end;

procedure TShowFloatLimit.SetStartLimit(const Value: double);
begin
  FStartLimit := Value;
end;

procedure TShowFloatLimit.SetUseLimit(const Value: boolean);
begin
  FUseLimit := Value;
end;

{ TPathlineColorLimits }

procedure TPathlineColorLimits.Assign(Source: TPersistent);
var
  SourceLimits: TPathlineColorLimits;
begin
  if Source is TPathlineColorLimits then
  begin
    SourceLimits := TPathlineColorLimits(Source);
    ColoringChoice := SourceLimits.ColoringChoice;
    MinColorLimit := SourceLimits.MinColorLimit;
    MaxColorLimit := SourceLimits.MaxColorLimit;
    UseLimit := SourceLimits.UseLimit;
  end
  else
  begin
    inherited;
  end;
end;

constructor TPathlineColorLimits.Create;
begin
  inherited;
  FColoringChoice := clcTime;
end;

procedure TPathlineColorLimits.SetColoringChoice(
  const Value: TColorLimitChoice);
begin
  FColoringChoice := Value;
end;

procedure TPathlineColorLimits.SetMinColorLimit(const Value: double);
begin
  FMinColorLimit := Value;
end;

procedure TPathlineColorLimits.SetMaxColorLimit(const Value: double);
begin
  FMaxColorLimit := Value;
end;

procedure TPathlineColorLimits.SetUseLimit(const Value: boolean);
begin
  FUseLimit := Value;
end;

{ TEndPoints }

constructor TEndPoints.Create;
begin
  inherited Create(TEndPoint)
end;

procedure TEndPoints.ExportShapefileAtEndingLocations(FileName: string);
var
  ShapeDataBase: TXBase;
  Fields: TStringList;
  ShapeFileWriter: TShapefileGeometryWriter;
  PointIndex: Integer;
  APoint: TEndPoint;
  Shape: TShapeObject;
begin
  ShapeDataBase := TXBase.Create(nil);
  try
    Fields := TStringList.Create;
    try
      Fields.Add(string(StrSTARTLAY) + '=N');
      Fields.Add(string(StrSTARTROW) + '=N');
      Fields.Add(string(StrSTARTCOL) + '=N');

      Fields.Add(string(StrENDLAY) + '=N');
      Fields.Add(string(StrENDROW) + '=N');
      Fields.Add(string(StrENDCOL) + '=N');

      Fields.Add(string(StrSTARTTS) + '=N');
      Fields.Add(string(StrSTARTZONE) + '=N');
      Fields.Add(string(StrENDTS) + '=N');
      Fields.Add(string(StrENDZONE) + '=N');

      Fields.Add(string(StrSTARTX) + '=N18,10');
      Fields.Add(string(StrSTARTY) + '=N18,10');
      Fields.Add(string(StrSTARTZ) + '=N18,10');

      Fields.Add(string(StrTRACKTIME) + '=N18,10');
      Fields.Add(string(StrTERMCODE) + '=N');
      Fields.Add(string(StrRELEASET) + '=N18,10');
      InitializeDataBase(FileName, ShapeDataBase, Fields);
    finally
      Fields.Free;
    end;

    ShapeFileWriter := TShapefileGeometryWriter.Create(stPointZ, True);
    try
      for PointIndex := 0 to Count - 1 do
      begin
        APoint := Points[PointIndex];

        ShapeDataBase.AppendBlank;

        ShapeDataBase.UpdFieldInt(StrSTARTLAY, APoint.FStartLayer);
        ShapeDataBase.UpdFieldInt(StrSTARTROW, APoint.FStartRow);
        ShapeDataBase.UpdFieldInt(StrSTARTCOL, APoint.FStartColumn);

        ShapeDataBase.UpdFieldInt(StrENDLAY, APoint.FEndLayer);
        ShapeDataBase.UpdFieldInt(StrENDROW, APoint.FEndRow);
        ShapeDataBase.UpdFieldInt(StrENDCOL, APoint.FEndColumn);

        ShapeDataBase.UpdFieldInt(StrSTARTTS, APoint.FStartTimeStep);
        ShapeDataBase.UpdFieldInt(StrSTARTZONE, APoint.FStartZoneCode);
        ShapeDataBase.UpdFieldInt(StrENDTS, APoint.FEndTimeStep);
        ShapeDataBase.UpdFieldInt(StrENDZONE, APoint.FEndZoneCode);

        ShapeDataBase.UpdFieldNum(StrSTARTX, APoint.FStartX);
        ShapeDataBase.UpdFieldNum(StrSTARTY, APoint.FStartY);
        ShapeDataBase.UpdFieldNum(StrSTARTZ, APoint.FStartZ);

        ShapeDataBase.UpdFieldNum(StrTRACKTIME, APoint.FTrackingTime);
        ShapeDataBase.UpdFieldInt(StrTERMCODE, APoint.FTerminationCode);
        ShapeDataBase.UpdFieldNum(StrRELEASET, APoint.FReleaseTime);

        ShapeDataBase.PostChanges;

        Shape := TShapeObject.Create;
        try
          Shape.FShapeType := stPointZ;

          Shape.FNumPoints := 1;
          SetLength(Shape.FPoints, 1);
          SetLength(Shape.FZArray, 1);
          SetLength(Shape.FMArray, 1);
          Shape.FNumParts := 1;
          SetLength(Shape.FParts, 0);
          Shape.FMArray[0] := -1e40;
          Shape.FPoints[0].X := APoint.FEndX;
          Shape.FPoints[0].Y := APoint.FEndY;
          Shape.FZArray[0] := APoint.FEndZ;
        except
          Shape.Free;
          raise;
        end;
        ShapeFileWriter.AddShape(Shape);
      end;
      ShapeFileWriter.WriteToFile(FileName, ChangeFileExt(FileName, '.shx'));
    finally
      ShapeFileWriter.Free;
    end;

  finally
    ShapeDataBase.Active := False;
    ShapeDataBase.Free;
  end;
end;

procedure TEndPoints.ExportShapefileAtStartingLocations(FileName: string);
var
  ShapeDataBase: TXBase;
  Fields: TStringList;
  ShapeFileWriter: TShapefileGeometryWriter;
  PointIndex: Integer;
  APoint: TEndPoint;
  Shape: TShapeObject;
begin
  ShapeDataBase := TXBase.Create(nil);
  try
    Fields := TStringList.Create;
    try
      Fields.Add(string(StrSTARTLAY) + '=N');
      Fields.Add(string(StrSTARTROW) + '=N');
      Fields.Add(string(StrSTARTCOL) + '=N');

      Fields.Add(string(StrENDLAY) + '=N');
      Fields.Add(string(StrENDROW) + '=N');
      Fields.Add(string(StrENDCOL) + '=N');

      Fields.Add(string(StrSTARTTS) + '=N');
      Fields.Add(string(StrSTARTZONE) + '=N');
      Fields.Add(string(StrENDTS) + '=N');
      Fields.Add(string(StrENDZONE) + '=N');

      Fields.Add(string(StrENDX) + '=N18,10');
      Fields.Add(string(StrENDY) + '=N18,10');
      Fields.Add(string(StrENDZ) + '=N18,10');

      Fields.Add(string(StrTRACKTIME) + '=N18,10');
      Fields.Add(string(StrTERMCODE) + '=N');
      Fields.Add(string(StrRELEASET) + '=N18,10');

      InitializeDataBase(FileName, ShapeDataBase, Fields);
    finally
      Fields.Free;
    end;

    ShapeFileWriter := TShapefileGeometryWriter.Create(stPointZ, True);
    try
      for PointIndex := 0 to Count - 1 do
      begin
        APoint := Points[PointIndex];

        ShapeDataBase.AppendBlank;

        ShapeDataBase.UpdFieldInt(StrSTARTLAY, APoint.FStartLayer);
        ShapeDataBase.UpdFieldInt(StrSTARTROW, APoint.FStartRow);
        ShapeDataBase.UpdFieldInt(StrSTARTCOL, APoint.FStartColumn);

        ShapeDataBase.UpdFieldInt(StrENDLAY, APoint.FEndLayer);
        ShapeDataBase.UpdFieldInt(StrENDROW, APoint.FEndRow);
        ShapeDataBase.UpdFieldInt(StrENDCOL, APoint.FEndColumn);

        ShapeDataBase.UpdFieldInt(StrSTARTTS, APoint.FStartTimeStep);
        ShapeDataBase.UpdFieldInt(StrSTARTZONE, APoint.FStartZoneCode);
        ShapeDataBase.UpdFieldInt(StrENDTS, APoint.FEndTimeStep);
        ShapeDataBase.UpdFieldInt(StrENDZONE, APoint.FEndZoneCode);

        ShapeDataBase.UpdFieldNum(StrENDX, APoint.FEndX);
        ShapeDataBase.UpdFieldNum(StrENDY, APoint.FEndY);
        ShapeDataBase.UpdFieldNum(StrENDZ, APoint.FEndZ);

        ShapeDataBase.UpdFieldNum(StrTRACKTIME, APoint.FTrackingTime);
        ShapeDataBase.UpdFieldInt(StrTERMCODE, APoint.FTerminationCode);
        ShapeDataBase.UpdFieldNum(StrRELEASET, APoint.FReleaseTime);

        ShapeDataBase.PostChanges;

        Shape := TShapeObject.Create;
        try
          Shape.FShapeType := stPointZ;

          Shape.FNumPoints := 1;
          SetLength(Shape.FPoints, 1);
          SetLength(Shape.FZArray, 1);
          SetLength(Shape.FMArray, 1);
          Shape.FNumParts := 1;
          SetLength(Shape.FParts, 0);
          Shape.FMArray[0] := -1e40;
          Shape.FPoints[0].X := APoint.FStartX;
          Shape.FPoints[0].Y := APoint.FStartY;
          Shape.FZArray[0] := APoint.FStartZ;
        except
          Shape.Free;
          raise;
        end;
        ShapeFileWriter.AddShape(Shape);
      end;
      ShapeFileWriter.WriteToFile(FileName, ChangeFileExt(FileName, '.shx'));
    finally
      ShapeFileWriter.Free;
    end
  finally
    ShapeDataBase.Active := False;
    ShapeDataBase.Free;
  end;

end;

function TEndPoints.GetPoint(Index: integer): TEndPoint;
begin
  result := inherited Items[Index] as TEndPoint;
end;

{ TEndPoint }

procedure TEndPoint.Assign(Source: TPersistent);
var
  SourcePoint: TEndPoint;
begin
  if Source is TEndPoint then
  begin
    SourcePoint := TEndPoint(Source);
    EndZoneCode := SourcePoint.EndZoneCode;
    EndColumn := SourcePoint.EndColumn;
    EndRow := SourcePoint.EndRow;
    EndLayer := SourcePoint.EndLayer;
    EndX := SourcePoint.EndX;
    EndY := SourcePoint.EndY;
    EndZ := SourcePoint.EndZ;
    EndXPrime := SourcePoint.EndXPrime;
    EndYPrime := SourcePoint.EndYPrime;
    EndLocalZ := SourcePoint.EndZoneCode;
    TrackingTime := SourcePoint.TrackingTime;
    StartZoneCode := SourcePoint.StartZoneCode;
    StartColumn := SourcePoint.StartColumn;
    StartRow := SourcePoint.StartRow;
    StartLayer := SourcePoint.StartLayer;
    StartX := SourcePoint.StartX;
    StartY := SourcePoint.StartY;
    StartZ := SourcePoint.StartZ;
    StartXPrime := SourcePoint.StartXPrime;
    StartYPrime := SourcePoint.StartYPrime;
    StartLocalZ := SourcePoint.StartLocalZ;
    StartTimeStep := SourcePoint.StartTimeStep;
    TerminationCode := SourcePoint.TerminationCode;
    EndTimeStep := SourcePoint.EndTimeStep;
    ReleaseTime := SourcePoint.ReleaseTime;
  end
  else
  begin
    inherited;
  end;
end;

function TEndPoint.CheckLimits(Limits: TEndPointDisplayLimits): boolean;
begin
  result := True;
  if Limits.EndColumnLimits.UseLimit then
  begin
    result := (Limits.EndColumnLimits.StartLimit <= EndColumn)
      and (EndColumn <= Limits.EndColumnLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.EndRowLimits.UseLimit then
  begin
    result := (Limits.EndRowLimits.StartLimit <= EndRow)
      and (EndRow <= Limits.EndRowLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.EndLayerLimits.UseLimit then
  begin
    result := (Limits.EndLayerLimits.StartLimit <= EndLayer)
      and (EndLayer <= Limits.EndLayerLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.EndZoneLimits.UseLimit then
  begin
    result := (Limits.EndZoneLimits.StartLimit <= EndZoneCode)
      and (EndZoneCode <= Limits.EndZoneLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.TrackingTimeLimits.UseLimit then
  begin
    result := (Limits.TrackingTimeLimits.StartLimit <= Time)
      and (Time <= Limits.TrackingTimeLimits.EndLimit);
    if not result then Exit;
  end;

  if Limits.StartColumnLimits.UseLimit then
  begin
    result := (Limits.StartColumnLimits.StartLimit <= StartColumn)
      and (StartColumn <= Limits.StartColumnLimits.StartLimit);
    if not result then Exit;
  end;
  if Limits.StartRowLimits.UseLimit then
  begin
    result := (Limits.StartRowLimits.StartLimit <= StartRow)
      and (StartRow <= Limits.StartRowLimits.StartLimit);
    if not result then Exit;
  end;
  if Limits.StartLayerLimits.UseLimit then
  begin
    result := (Limits.StartLayerLimits.StartLimit <= StartLayer)
      and (StartLayer <= Limits.StartLayerLimits.StartLimit);
    if not result then Exit;
  end;
  if Limits.StartZoneLimits.UseLimit then
  begin
    result := (Limits.StartZoneLimits.StartLimit <= StartZoneCode)
      and (StartZoneCode <= Limits.StartZoneLimits.StartLimit);
    if not result then Exit;
  end;
  if Limits.ReleaseTimeLimits.UseLimit then
  begin
    result := (Limits.ReleaseTimeLimits.StartLimit <= ReleaseTime)
      and (ReleaseTime <= Limits.ReleaseTimeLimits.StartLimit);
    if not result then Exit;
  end;

end;

function TEndPoint.ShouldShow(Limits: TEndPointDisplayLimits;
  Orientation: TDataSetOrientation; CurrentColRowOrLayer: integer): boolean;
var
  ColRowOrLayerToCheck: integer;
begin
  result := True;
  if Limits.LimitToCurrentIn2D and (Orientation <> dso3D) then
  begin
    ColRowOrLayerToCheck := -1;
    case Limits.WhereToPlot of
      wtpStart:
        begin
          case Orientation of
            dsoTop:
              begin
                ColRowOrLayerToCheck := StartLayer;
              end;
            dsoFront:
              begin
                ColRowOrLayerToCheck := StartRow;
              end;
            dsoSide:
              begin
                ColRowOrLayerToCheck := StartColumn;
              end;
            else Assert(False);
          end;
        end;
      wtpEnd:
        begin
          case Orientation of
            dsoTop:
              begin
                ColRowOrLayerToCheck := EndLayer;
              end;
            dsoFront:
              begin
                ColRowOrLayerToCheck := EndRow;
              end;
            dsoSide:
              begin
                ColRowOrLayerToCheck := EndColumn;
              end;
            else Assert(False);
          end;
        end;
    end;
    Assert(ColRowOrLayerToCheck >= 1);
    result := ColRowOrLayerToCheck = CurrentColRowOrLayer;
    if not result then
    begin
      Exit;
    end;
  end;
  case Limits.ShowChoice of
    escAll:
      begin
        // do nothing
      end;
    escSpecified:
      begin
        result := CheckLimits(Limits);
        if not result then Exit;
      end;
    else Assert(False);
  end;
end;

{ TEndPointReader }

procedure TEndPointReader.Assign(Source: TPersistent);
var
  EndPointSource: TEndPointReader;
begin
  if Source is TEndPointReader then
  begin
    EndPointSource := TEndPointReader(Source);
    FileDate := EndPointSource.FileDate;
    FileName := EndPointSource.FileName;
    MinReleaseTime := EndPointSource.MinReleaseTime;
    MaxReleaseTime := EndPointSource.MaxReleaseTime;
    MinTrackingTime := EndPointSource.MinTrackingTime;
    MaxTrackingTime := EndPointSource.MaxTrackingTime;
    MinStartZone := EndPointSource.MinStartZone;
    MaxStartZone := EndPointSource.MaxStartZone;
    MinEndZone := EndPointSource.MinEndZone;
    MaxEndZone := EndPointSource.MaxEndZone;
    Points := EndPointSource.Points;
  end;
  inherited;
end;

constructor TEndPointReader.Create;
begin
  inherited;
  FPoints:= TEndPoints.Create;
end;

destructor TEndPointReader.Destroy;
begin
  FPoints.Free;
  inherited;
end;

procedure TEndPointReader.Draw(Orientation: TDataSetOrientation;
  const BitMap: TBitmap32);
var
  EndPointIndex: Integer;
  EndPoint: TEndPoint;
  ColRowOrLayer: integer;
  ZoomBox: TQRbwZoomBox2;
  ADisplayPoint: TPoint;
  MaxValue, MinValue: double;
  Grid: TModflowGrid;
  AColor: TColor;
  AColor32: TColor32;
  ARect: TRect;
begin
  if not Visible then
  begin
    Exit;
  end;
  Grid := frmGoPhast.ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  if (Grid.LayerCount <= 0) or (Grid.RowCount <= 0)
    or (Grid.ColumnCount <= 0) then
  begin
    Exit;
  end;
  ColRowOrLayer := -1;
  ZoomBox := nil;
  case Orientation of
    dsoTop:
      begin
        ZoomBox := frmGoPhast.frameTopView.ZoomBox;
        ColRowOrLayer := Grid.SelectedLayer+1;
      end;
    dsoFront:
      begin
        ZoomBox := frmGoPhast.frameFrontView.ZoomBox;
        ColRowOrLayer := Grid.SelectedRow+1;
      end;
    dsoSide:
      begin
        ZoomBox := frmGoPhast.frameSideView.ZoomBox;
        ColRowOrLayer := Grid.SelectedColumn+1;
      end;
    dso3D: Assert(False);
    else Assert(False);
  end;
  GetMinMaxValues(MaxValue, MinValue);

  for EndPointIndex := 0 to Points.Count - 1 do
  begin
    EndPoint := Points[EndPointIndex];
    if EndPoint.ShouldShow(DisplayLimits, Orientation, ColRowOrLayer) then
    begin
      case DisplayLimits.WhereToPlot of
        wtpStart: 
          begin
            case Orientation of
              dsoTop:
                begin
                  ADisplayPoint.X := ZoomBox.XCoord(EndPoint.StartX);
                  ADisplayPoint.Y := ZoomBox.YCoord(EndPoint.StartY);
                end;
              dsoFront:
                begin
                  ADisplayPoint.X := ZoomBox.XCoord(EndPoint.StartX);
                  ADisplayPoint.Y := ZoomBox.YCoord(EndPoint.StartZ);
                end;
              dsoSide:
                begin
                  ADisplayPoint.X := ZoomBox.XCoord(EndPoint.StartZ);
                  ADisplayPoint.Y := ZoomBox.YCoord(EndPoint.StartY);
                end;
              else Assert(False);
            end;
          end;
        wtpEnd: 
          begin
            case Orientation of
              dsoTop:
                begin
                  ADisplayPoint.X := ZoomBox.XCoord(EndPoint.EndX);
                  ADisplayPoint.Y := ZoomBox.YCoord(EndPoint.EndY);
                end;
              dsoFront:
                begin
                  ADisplayPoint.X := ZoomBox.XCoord(EndPoint.EndX);
                  ADisplayPoint.Y := ZoomBox.YCoord(EndPoint.EndZ);
                end;
              dsoSide:
                begin
                  ADisplayPoint.X := ZoomBox.XCoord(EndPoint.EndZ);
                  ADisplayPoint.Y := ZoomBox.YCoord(EndPoint.EndY);
                end;
              else Assert(False);
            end;
          end;
      end;
      AColor := GetPointColor(MaxValue, MinValue, EndPoint);
      AColor32 := Color32(AColor);
      ARect.Top := ADisplayPoint.Y -1;
      ARect.Bottom := ADisplayPoint.Y +1;
      ARect.Left := ADisplayPoint.X -1;
      ARect.Right := ADisplayPoint.X +1;
      DrawBigRectangle32(BitMap, AColor32, AColor32, 1, ARect);
    end;
  end;
end;

procedure TEndPointReader.Draw3D;
var
  Grid: TModflowGrid;
begin
  if FDrawingEndPoints then
  begin
    Exit;
  end;
  try
    FDrawingEndPoints := True;


    if (not FRecordedEndPoints) then
    begin
      Record3DEndPoints;
      // FRecordedEndPoints is private and won't be set
      // by overridden versions of RecordFront.
      FRecordedEndPoints := True;
    end;

    if not Visible then
    begin
      Exit;
    end;
    Grid := frmGoPhast.ModflowGrid;
    if Grid = nil then
    begin
      Exit;
    end;
    if (Grid.LayerCount <= 0) or (Grid.RowCount <= 0)
      or (Grid.ColumnCount <= 0) then
    begin
      Exit;
    end;
//    EnableLighting;
    glCallList(EndPointGLIndex);
  finally
    FDrawingEndPoints := False;
  end;

end;

class function TEndPointReader.GetEndPointGLIndex: TGLuint;
begin
  if not FListInitialized and frmGoPhast.frame3DView.glWidModelView.Started then
  begin
    FListInitialized := True;
    FPathlineGLIndex := glGenLists(1);
  end;
  result := FPathlineGLIndex;
end;

procedure TEndPointReader.GetMinMaxValues(var MaxValue, MinValue: Double);
var
  Grid: TModflowGrid;
begin
  Grid := frmGoPhast.ModflowGrid;
  if ColorLimits.UseLimit then
  begin
    MinValue := ColorLimits.MinColorLimit;
    MaxValue := ColorLimits.MaxColorLimit;
  end
  else
  begin
    MinValue := 0;
    MaxValue := 1;
    case ColorLimits.ColoringChoice of
      elcNone: 
        begin
          MinValue := 0;
          MaxValue := 1;
        end;
      elcReleaseTime:  
        begin
          MinValue := MinReleaseTime;
          MaxValue := MaxReleaseTime;
        end;
      elcTrackingTime: 
        begin
          MinValue := MinTrackingTime;
          MaxValue := MaxTrackingTime;
        end;
      elcStartXPrime, elcEndXPrime: 
        begin
          MinValue := Grid.ColumnPosition[0];
          MaxValue := Grid.ColumnPosition[Grid.ColumnCount];
        end;
      elcStartYPrime, elcEndYPrime: 
        begin
          MaxValue := Grid.RowPosition[0];
          MinValue := Grid.RowPosition[Grid.RowCount];
        end;
      elcStartZ, elcEndZ:
        begin
          MinValue := Grid.LowestElevation;
          MaxValue := Grid.HighestElevation;
        end;
      elcStartZone:
        begin
          MinValue := MinStartZone;
          MaxValue := MaxStartZone;
        end; 
      elcEndZone:
        begin
          MinValue := MinEndZone;
          MaxValue := MaxEndZone;
        end;
      else Assert(False);
    end;
  end;
end;

function TEndPointReader.GetPointColor(MaxValue, MinValue: double;
  Point: TEndPoint): TColor;
var
  AValue: Double;
begin
  AValue := 0;
  case ColorLimits.ColoringChoice of
    elcNone: 
      begin
        result := clBlack;
        Exit;
      end;
    elcReleaseTime: 
      begin
        AValue := Point.ReleaseTime;      
      end;
    elcTrackingTime:
      begin
        AValue := Point.TrackingTime;
      end;
    elcStartXPrime:
      begin
        AValue := Point.StartXPrime;
      end;
    elcStartYPrime:
      begin
        AValue := Point.StartYPrime;
      end;
    elcStartZ:
      begin
        AValue := Point.StartZ;
      end;
    elcStartZone:
      begin
        AValue := Point.StartZoneCode
      end;
    elcEndXPrime: 
      begin
        AValue := Point.EndXPrime;
      end;
    elcEndYPrime: 
      begin
        AValue := Point.EndYPrime;
      end;
    elcEndZ: 
      begin
        AValue := Point.EndZ;
      end;
    elcEndZone: 
      begin
        AValue := Point.EndZoneCode
      end;
  end;
  if AValue > MaxValue then
  begin
    result := clBlack;
  end
  else if AValue < MinValue then
  begin
    result := clBlack;
  end
  else
  begin
    if MaxValue = MinValue then
    begin
      result := ColorParameters.FracToColor(0.5)
    end
    else
    begin
      result := ColorParameters.FracToColor(1-((AValue-MinValue)/(MaxValue-MinValue)))
    end;
  end;
end;

procedure TEndPointReader.Invalidate;
begin
  FRecordedEndPoints := False;
end;

procedure GetZ(Grid: TModflowGrid; J, I, K: integer; LocalZ: single; var Z: single);
var
  Column: Integer;
  Row: Integer;
  Layer: Integer;
  Z1: Real;
  Z2: Real;
  Thickness: Real;
begin
  Column := J -1;
  Row := I -1;
  Layer := frmGoPhast.PhastModel.ModflowLayerToDataSetLayer(K);
  if (Column < Grid.ColumnCount) and (Row < Grid.RowCount)
    and (Layer < Grid.LayerCount) then
  begin
    Z1 := Grid.CellElevation[Column, Row, Layer+1];
  end
  else
  begin
    Z := 0;
    Exit;
  end;
  if LocalZ < 0 then
  begin
    Z2 := Grid.CellElevation[Column, Row, Layer+2];
    Thickness := Z1-Z2;
  end
  else
  begin
    Z2 := Grid.CellElevation[Column, Row, Layer];
    Thickness := Z2-Z1;
  end;
  Z := Z1 + Thickness*LocalZ;
end;

procedure TEndPointReader.ReadFile;
var
  Grid: TModflowGrid;
  ADate: TDateTime;
  AFile: TFileStream;
  AChar: AnsiChar;
  IsTextFile: Boolean;
  NRow: Integer;
  NCol: Integer;
  FTextFile: TextFile;
  ALine: string;
  CompactFormat: boolean;
  FFile: TFileStream;
  Description: array[0..79] of AnsiChar;
  Terminator: array[0..3] of AnsiChar;
  EndZoneCode: integer;
  EndJ: integer;
  EndI: integer;
  EndK: integer;
  StartJ: integer;
  StartI: integer;
  StartK: integer;
  StartZoneCode: integer;
  IPCODE: integer;
  EndXPrime: single;
  EndYPrime: single;
  EndZ: single;
  EndLocalZ: single;
  TrackingTime: single;
  StartXPrime: single;
  StartYPrime: single;
  StartLocalZ: single;
  ReleaseTime: single;
  StartTimeStep: integer;
  PointIndex: Integer;
  EndPoint: TEndPoint;
  procedure CreateEndPoint;
  var
    APoint: TEndPoint;
    Point2D: TPoint2D;
  begin
    APoint := TEndPoint.Create(FPoints);
    APoint.EndZoneCode := EndZoneCode;
    APoint.EndColumn := EndJ;
    APoint.EndRow := EndI;
    APoint.EndLayer := EndK;

    ConvertCoordinates(Grid, EndXPrime, EndYPrime, Point2D);

    APoint.EndX := Point2D.x;
    APoint.EndY := Point2D.Y;
    APoint.EndZ := EndZ;
    APoint.EndXPrime := EndXPrime;
    APoint.EndYPrime := EndYPrime;
    APoint.EndLocalZ := EndLocalZ;
    APoint.TrackingTime := TrackingTime;
    APoint.StartZoneCode := StartZoneCode;
    APoint.StartColumn := StartJ;
    APoint.StartRow := StartI;
    APoint.StartLayer := StartK;

    ConvertCoordinates(Grid, StartXPrime, StartYPrime, Point2D);

    APoint.StartX := Point2D.x;
    APoint.StartY := Point2D.y;
    APoint.StartZ := StartZ;
    APoint.StartXPrime := StartXPrime;
    APoint.StartYPrime := StartYPrime;
    APoint.StartTimeStep := StartTimeStep;
    if IPCODE < 0 then
    begin
      APoint.TerminationCode := IPCODE;
      APoint.EndTimeStep := 0;
    end
    else
    begin
      APoint.TerminationCode := IPCODE mod 10;
      APoint.EndTimeStep := IPCODE div 10;
    end;
    APoint.ReleaseTime := ReleaseTime;
  end;
begin
  Grid := frmGoPhast.ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  if FileAge(FileName, ADate) then
  begin
    FileDate := ADate;
  end;
  Points.Clear;
  AFile := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
  try
    AFile.Read(AChar, SizeOf(AChar));
  finally
    AFile.Free;
  end;
  IsTextFile := AChar = '@';
  NRow := Grid.RowCount;
  NCol := Grid.ColumnCount;
  if IsTextFile then
  begin
    AssignFile(FTextFile, FFileName);
    try
      Reset(FTextFile);
      Readln(FTextFile, ALine);
      CompactFormat := Pos('COMPACT',ALine) >= 1;
      While Not Eof(FTextFile) do
      begin
        if CompactFormat then
        begin
          Readln(FTextFile, EndZoneCode, EndJ, EndXPrime, EndYPrime,
            EndLocalZ, TrackingTime, StartXPrime, StartYPrime, StartLocalZ,
            StartJ, StartZoneCode, StartTimeStep, IPCODE, ReleaseTime);
          ConvertIndicies(NCol, NRow, EndI, EndK, EndJ);
          ConvertIndicies(NCol, NRow, StartI, StartK, StartJ);
          GetZ(Grid, EndJ, EndI, EndK, EndLocalZ, EndZ);
          GetZ(Grid, StartJ, StartI, StartK, StartLocalZ, StartZ);
        end
        else
        begin
          Readln(FTextFile, EndZoneCode, EndJ, EndI, EndK, EndXPrime, EndYPrime,
            EndZ, EndLocalZ, TrackingTime, StartXPrime, StartYPrime, StartLocalZ,
            StartJ, StartI, StartK, StartZoneCode, StartTimeStep, IPCODE,
            ReleaseTime);
          GetZ(Grid, StartJ, StartI, StartK, StartLocalZ, StartZ);
        end;

        CreateEndPoint;
      end;
    finally
      CloseFile(FTextFile);
    end;
  end
  else
  begin
    FFile := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
    try
      AFile.Read(Description, SizeOf(Description));
      AFile.Read(Terminator, SizeOf(Terminator));
      while FFile.Position < FFile.Size do
      begin
        AFile.Read(EndZoneCode, SizeOf(EndZoneCode));
        AFile.Read(EndJ, SizeOf(EndJ));
        AFile.Read(EndXPrime, SizeOf(EndXPrime));
        AFile.Read(EndYPrime, SizeOf(EndYPrime));
        AFile.Read(EndLocalZ, SizeOf(EndLocalZ));
        AFile.Read(TrackingTime, SizeOf(TrackingTime));
        AFile.Read(StartXPrime, SizeOf(StartXPrime));
        AFile.Read(StartYPrime, SizeOf(StartYPrime));
        AFile.Read(StartLocalZ, SizeOf(StartLocalZ));
        AFile.Read(StartJ, SizeOf(StartJ));
        AFile.Read(StartZoneCode, SizeOf(StartZoneCode));
        AFile.Read(StartTimeStep, SizeOf(StartTimeStep));
        AFile.Read(IPCODE, SizeOf(IPCODE));
        AFile.Read(ReleaseTime, SizeOf(ReleaseTime));

        ConvertIndicies(NCol, NRow, EndI, EndK, EndJ);
        ConvertIndicies(NCol, NRow, StartI, StartK, StartJ);
        GetZ(Grid, EndJ, EndI, EndK, EndLocalZ, EndZ);
        GetZ(Grid, StartJ, StartI, StartK, StartLocalZ, StartZ);

        CreateEndPoint;
      end;
    finally
      FFile.Free;
    end;
  end;

  for PointIndex := 0 to Points.Count - 1 do
  begin
    EndPoint := Points[PointIndex];
    if PointIndex = 0 then
    begin
      MinTrackingTime := EndPoint.TrackingTime;
      MaxTrackingTime := EndPoint.TrackingTime;
      MinReleaseTime := EndPoint.ReleaseTime;
      MaxReleaseTime := EndPoint.ReleaseTime;
      MinStartZone := EndPoint.StartZoneCode;
      MaxStartZone := EndPoint.StartZoneCode;
      MinEndZone := EndPoint.EndZoneCode;
      MaxEndZone := EndPoint.EndZoneCode;
    end
    else
    begin
      if EndPoint.TrackingTime < MinTrackingTime then
      begin
        MinTrackingTime := EndPoint.TrackingTime
      end;
      if EndPoint.TrackingTime > MaxTrackingTime then
      begin
        MaxTrackingTime := EndPoint.TrackingTime;
      end;
      if EndPoint.ReleaseTime < MinReleaseTime then
      begin
        MinReleaseTime := EndPoint.ReleaseTime
      end;
      if EndPoint.ReleaseTime > MaxReleaseTime then
      begin
        MaxReleaseTime := EndPoint.ReleaseTime;
      end;
      if EndPoint.StartZoneCode < MinStartZone then
      begin
        MinStartZone := EndPoint.StartZoneCode
      end;
      if EndPoint.StartZoneCode > MaxStartZone then
      begin
        MaxStartZone := EndPoint.StartZoneCode;
      end;
      if EndPoint.EndZoneCode < MinEndZone then
      begin
        MinEndZone := EndPoint.EndZoneCode
      end;
      if EndPoint.EndZoneCode > MaxEndZone then
      begin
        MaxEndZone := EndPoint.EndZoneCode;
      end;
    end;
  end
end;

procedure TEndPointReader.Record3DEndPoints;
var
  Grid: TModflowGrid;
  ColRowOrLayer: Integer;
  MaxValue: Double;
  MinValue: Double;
  PointIndex: Integer;
  EndPoint: TEndPoint;
  AColor: TColor;
begin
  if not Visible then
  begin
    Exit;
  end;
  Grid := frmGoPhast.ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  if (Grid.LayerCount <= 0) or (Grid.RowCount <= 0)
    or (Grid.ColumnCount <= 0) then
  begin
    Exit;
  end;
  ColRowOrLayer := -1;

//    EnableLighting;
  glMatrixMode(GL_MODELVIEW);

  glNewList(EndPointGLIndex, GL_COMPILE);
  try
    glPushMatrix;
    try
      glEnable(GL_LINE_SMOOTH);
      glShadeModel(GL_SMOOTH);

      GetMinMaxValues(MaxValue, MinValue);
      glLineWidth(1);

      glBegin(GL_POINTS);
      for PointIndex := 0 to Points.Count - 1 do
      begin
        EndPoint := Points[PointIndex];
        if EndPoint.ShouldShow(DisplayLimits, dso3D, ColRowOrLayer) then
        begin
          AColor := GetPointColor(MaxValue, MinValue, EndPoint);
          AssignColor(AColor);
          case DisplayLimits.WhereToPlot of
            wtpStart:
              begin
                glVertex3f(EndPoint.StartX, EndPoint.StartY, EndPoint.StartZ);
              end;
            wtpEnd:
              begin
                glVertex3f(EndPoint.EndX, EndPoint.EndY, EndPoint.EndZ);
              end;
            else Assert(False);
          end;
        end;
      end;
      glEnd;
    finally
      glPopMatrix;
    end;
  finally
    glEndList;
  end;
end;

procedure TEndPointReader.SetFileDate(const Value: TDateTime);
begin
  FFileDate := Value;
end;

procedure TEndPointReader.SetMaxEndZone(const Value: integer);
begin
  FMaxEndZone := Value;
end;

procedure TEndPointReader.SetMaxReleaseTime(const Value: double);
begin
  FMaxReleaseTime := Value;
end;

procedure TEndPointReader.SetMaxStartZone(const Value: integer);
begin
  FMaxStartZone := Value;
end;

procedure TEndPointReader.SetMaxTrackingTime(const Value: double);
begin
  FMaxTrackingTime := Value;
end;

procedure TEndPointReader.SetMinEndZone(const Value: integer);
begin
  FMinEndZone := Value;
end;

procedure TEndPointReader.SetMinReleaseTime(const Value: double);
begin
  FMinReleaseTime := Value;
end;

procedure TEndPointReader.SetMinStartZone(const Value: integer);
begin
  FMinStartZone := Value;
end;

procedure TEndPointReader.SetTrackingMinTime(const Value: double);
begin
  FMinTrackingTime := Value;
end;

procedure TEndPointReader.SetPoints(const Value: TEndPoints);
begin
  FPoints.Assign(Value);
end;

{ TEndPointDisplayLimits }

procedure TEndPointDisplayLimits.Assign(Source: TPersistent);
var
  SourceLimits: TEndPointDisplayLimits;
begin
  if Source is TEndPointDisplayLimits then
  begin
    SourceLimits := TEndPointDisplayLimits(Source);
    ShowChoice := SourceLimits.ShowChoice;
    LimitToCurrentIn2D := SourceLimits.LimitToCurrentIn2D;
    StartColumnLimits := SourceLimits.StartColumnLimits;
    StartRowLimits := SourceLimits.StartRowLimits;
    StartLayerLimits := SourceLimits.StartLayerLimits;
    StartZoneLimits := SourceLimits.StartZoneLimits;
    ReleaseTimeLimits := SourceLimits.ReleaseTimeLimits;
    EndColumnLimits := SourceLimits.EndColumnLimits;
    EndRowLimits := SourceLimits.EndRowLimits;
    EndLayerLimits := SourceLimits.EndLayerLimits;
    EndZoneLimits := SourceLimits.EndZoneLimits;
    TrackingTimeLimits := SourceLimits.TrackingTimeLimits;
    WhereToPlot := SourceLimits.WhereToPlot;
  end
  else
  begin
    inherited;
  end;
end;

constructor TEndPointDisplayLimits.Create;
begin
  inherited;
  FStartLayerLimits := TShowIntegerLimit.Create;
  FStartRowLimits := TShowIntegerLimit.Create;
  FStartColumnLimits := TShowIntegerLimit.Create;
  FStartZoneLimits := TShowIntegerLimit.Create;
  FEndLayerLimits := TShowIntegerLimit.Create;
  FEndRowLimits := TShowIntegerLimit.Create;
  FEndColumnLimits := TShowIntegerLimit.Create;
  FEndZoneLimits := TShowIntegerLimit.Create;
  FReleaseTimeLimits := TShowFloatLimit.Create;
  FTrackingTimeLimits := TShowFloatLimit.Create;
  FWhereToPlot := wtpEnd;
end;

destructor TEndPointDisplayLimits.Destroy;
begin
  FTrackingTimeLimits.Free;
  FReleaseTimeLimits.Free;
  FEndZoneLimits.Free;
  FEndColumnLimits.Free;
  FEndRowLimits.Free;
  FEndLayerLimits.Free;
  FStartZoneLimits.Free;
  FStartColumnLimits.Free;
  FStartRowLimits.Free;
  FStartLayerLimits.Free;
  inherited;
end;

procedure TEndPointDisplayLimits.SetEndColumnLimits(
  const Value: TShowIntegerLimit);
begin
  FEndColumnLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetEndLayerLimits(
  const Value: TShowIntegerLimit);
begin
  FEndLayerLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetEndRowLimits(
  const Value: TShowIntegerLimit);
begin
  FEndRowLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetEndZoneLimits(
  const Value: TShowIntegerLimit);
begin
  FEndZoneLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetLimitToCurrentIn2D(const Value: boolean);
begin
  FLimitToCurrentIn2D := Value;
end;

procedure TEndPointDisplayLimits.SetReleaseTimeLimits(
  const Value: TShowFloatLimit);
begin
  FReleaseTimeLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetShowChoice(
  const Value: TEndpointShowChoice);
begin
  FShowChoice := Value;
end;

procedure TEndPointDisplayLimits.SetStartColumnLimits(
  const Value: TShowIntegerLimit);
begin
  FStartColumnLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetStartLayerLimits(
  const Value: TShowIntegerLimit);
begin
  FStartLayerLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetStartRowLimits(
  const Value: TShowIntegerLimit);
begin
  FStartRowLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetStartZoneLimits(
  const Value: TShowIntegerLimit);
begin
  FStartZoneLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetTrackingTimeLimits(
  const Value: TShowFloatLimit);
begin
  FTrackingTimeLimits.Assign(Value);
end;

procedure TEndPointDisplayLimits.SetWhereToPlot(const Value: TWhereToPlot);
begin
  FWhereToPlot := Value;
end;

{ TEndPointColorLimits }

procedure TEndPointColorLimits.Assign(Source: TPersistent);
var
  SourceLimits: TEndPointColorLimits;
begin
  if Source is TEndPointColorLimits then
  begin
    SourceLimits := TEndPointColorLimits(Source);
    ColoringChoice := SourceLimits.ColoringChoice;
    MinColorLimit := SourceLimits.MinColorLimit;
    MaxColorLimit := SourceLimits.MaxColorLimit;
    UseLimit := SourceLimits.UseLimit;
  end
  else
  begin
    inherited;
  end;
end;

constructor TEndPointColorLimits.Create;
begin
  inherited;
  FColoringChoice := elcTrackingTime;
end;

procedure TEndPointColorLimits.SetColoringChoice(
  const Value: TEndpointColorLimitChoice);
begin
  FColoringChoice := Value;
end;

procedure TEndPointColorLimits.SetMaxColorLimit(const Value: double);
begin
  FMaxColorLimit := Value;
end;

procedure TEndPointColorLimits.SetMinColorLimit(const Value: double);
begin
  FMinColorLimit := Value;
end;

procedure TEndPointColorLimits.SetUseLimit(const Value: boolean);
begin
  FUseLimit := Value;
end;

{ TTimeSeriesReader }

procedure TTimeSeriesReader.Assign(Source: TPersistent);
var
  SourceSeries: TTimeSeriesReader;
begin
  if Source is TTimeSeriesReader then
  begin
    SourceSeries := TTimeSeriesReader(Source);
    FileName := SourceSeries.FileName;
    FileDate := SourceSeries.FileDate;
    Series := SourceSeries.Series;
    MaxTime := SourceSeries.MaxTime;
    MinTime := SourceSeries.MinTime;
    TimeIndex := SourceSeries.TimeIndex;
    Times := SourceSeries.Times;
  end;
  inherited;
end;

function TTimeSeriesReader.CheckShowSeries(Series: TTimeSeries): Boolean;
var
  APoint: TTimeSeriesPoint;
begin
  result := True;
  case DisplayLimits.ShowChoice of
    scAll, scSpecified:
      begin
      end;
    scStart:
      begin
        APoint := Series.Points[0];
        result := APoint.ShouldShowSeries(DisplayLimits);
      end;
    scEnd:
      begin
        APoint := Series.Points[Series.Points.Count - 1];
        result := APoint.ShouldShowSeries(DisplayLimits);
      end;
  else
    Assert(False);
  end;
end;

constructor TTimeSeriesReader.Create;
begin
  inherited;
  FRealList := nil;
  FSeries:= TTimeSeriesCollection.Create;
end;

destructor TTimeSeriesReader.Destroy;
begin
  FSeries.Free;
  FRealList.Free;
  inherited;
end;

procedure TTimeSeriesReader.Draw(Orientation: TDataSetOrientation;
  const BitMap: TBitmap32);
var
  TimeSeriesIndex: Integer;
  TimeSeries: TTimeSeries;
  APoint: TTimeSeriesPoint;
  ColRowOrLayer: integer;
  ZoomBox: TQRbwZoomBox2;
  ADisplayPoint: TPoint;
  MaxValue, MinValue: double;
  Grid: TModflowGrid;
  AColor: TColor;
  AColor32: TColor32;
  ARect: TRect;
  TimeToPlot: Double;
  PlotIndex: Integer;
begin
  if not Visible then
  begin
    Exit;
  end;
  if TimeIndex < 0 then
  begin
    Exit;
  end;
  if Series.Count = 0 then
  begin
    Exit;
  end;
  Grid := frmGoPhast.ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  if (Grid.LayerCount <= 0) or (Grid.RowCount <= 0)
    or (Grid.ColumnCount <= 0) then
  begin
    Exit;
  end;
  ColRowOrLayer := -1;
  ZoomBox := nil;
  case Orientation of
    dsoTop:
      begin
        ZoomBox := frmGoPhast.frameTopView.ZoomBox;
        ColRowOrLayer := Grid.SelectedLayer+1;
      end;
    dsoFront:
      begin
        ZoomBox := frmGoPhast.frameFrontView.ZoomBox;
        ColRowOrLayer := Grid.SelectedRow+1;
      end;
    dsoSide:
      begin
        ZoomBox := frmGoPhast.frameSideView.ZoomBox;
        ColRowOrLayer := Grid.SelectedColumn+1;
      end;
    dso3D: Assert(False);
    else Assert(False);
  end;
  GetMinMaxValues(MaxValue, MinValue);

  TimeToPlot := Times[TimeIndex];
  for TimeSeriesIndex := 0 to Series.Count - 1 do
  begin
    TimeSeries := Series[TimeSeriesIndex];
    PlotIndex := TimeSeries.Times.IndexOf(TimeToPlot);
    if PlotIndex >= 0 then
    begin
      if CheckShowSeries(TimeSeries) then
      begin
        APoint := TimeSeries.Points[PlotIndex];
        if APoint.ShouldShow(DisplayLimits, Orientation, ColRowOrLayer) then
        begin
          case Orientation of
            dsoTop:
              begin
                ADisplayPoint.X := ZoomBox.XCoord(APoint.X);
                ADisplayPoint.Y := ZoomBox.YCoord(APoint.Y);
              end;
            dsoFront:
              begin
                ADisplayPoint.X := ZoomBox.XCoord(APoint.X);
                ADisplayPoint.Y := ZoomBox.YCoord(APoint.Z);
              end;
            dsoSide:
              begin
                ADisplayPoint.X := ZoomBox.XCoord(APoint.Z);
                ADisplayPoint.Y := ZoomBox.YCoord(APoint.Y);
              end;
            else Assert(False);
          end;
          AColor := GetPointColor(MaxValue, MinValue, APoint);
          AColor32 := Color32(AColor);
          ARect.Top := ADisplayPoint.Y -1;
          ARect.Bottom := ADisplayPoint.Y +1;
          ARect.Left := ADisplayPoint.X -1;
          ARect.Right := ADisplayPoint.X +1;
          DrawBigRectangle32(BitMap, AColor32, AColor32, 1, ARect);
        end;
      end;
    end;
  end;
end;

procedure TTimeSeriesReader.Draw3D;
var
  Grid: TModflowGrid;
begin
  if TimeIndex < 0 then
  begin
    Exit;
  end;
  if FDrawingTimeSeries then
  begin
    Exit;
  end;
  if Series.Count = 0 then
  begin
    Exit;
  end;
  try
    FDrawingTimeSeries := True;


    if (not RecordedTimeSeries[TimeIndex]) then
    begin
      Record3DTimeSeries(TimeIndex);
      // FRecordedTimeSeries is private and won't be set
      // by overridden versions of RecordFront.
      RecordedTimeSeries[TimeIndex] := True;
    end;

    if not Visible then
    begin
      Exit;
    end;
    Grid := frmGoPhast.ModflowGrid;
    if Grid = nil then
    begin
      Exit;
    end;
    if (Grid.LayerCount <= 0) or (Grid.RowCount <= 0)
      or (Grid.ColumnCount <= 0) then
    begin
      Exit;
    end;
//    EnableLighting;
    glCallList(TimeSeriesGLIndex[TimeIndex]);
  finally
    FDrawingTimeSeries := False;
  end;

end;

procedure TTimeSeriesReader.EnsureGLArrays(ATimeIndex: Integer);
var
  Index: Integer;
  OldLength: Integer;
  GLIndex: TGLuint;
  MaxPoints: Integer;
begin
  Assert(Length(FRecordedTimeSeries) = Length(FTimeSeriesGLIndex));
  if ATimeIndex >= Length(FRecordedTimeSeries) then
  begin
    MaxPoints := Times.Count;
    Assert((MaxPoints > ATimeIndex) or (MaxPoints = 0));
    OldLength := Length(FRecordedTimeSeries);
    SetLength(FRecordedTimeSeries, MaxPoints);
    SetLength(FTimeSeriesGLIndex, MaxPoints);
    GLIndex := glGenLists(MaxPoints - OldLength);
    for Index := OldLength to MaxPoints-1 do
    begin
      FRecordedTimeSeries[Index] := False;
      FTimeSeriesGLIndex[Index] := GLIndex;
      Inc(GLIndex);
    end;
  end;
end;

procedure TTimeSeriesReader.ExportShapefile(FileName: string);
var
  ShapeDataBase: TXBase;
  Fields: TStringList;
  ShapeFileWriter: TShapefileGeometryWriter;
  ATime: Double;
  SeriesIndex: Integer;
  ASeries: TTimeSeries;
  PlotIndex: Integer;
  APoint: TTimeSeriesPoint;
  Shape: TShapeObject;
  PointCount: Integer;
  PlotTimeIndex: Integer;
begin
  ShapeDataBase := TXBase.Create(nil);
  try
    Fields := TStringList.Create;
    try
      Fields.Add(string(StrTRACKTIME) + '=N18,10');
      InitializeDataBase(FileName, ShapeDataBase, Fields);
    finally
      Fields.Free;
    end;

    ShapeFileWriter := TShapefileGeometryWriter.Create(stMultiPointZ, True);
    try
      for PlotTimeIndex := 0 to Times.Count - 1 do
      begin
        ATime := Times[PlotTimeIndex];
        Shape := TShapeObject.Create;
        try
          Shape.FShapeType := stMultiPointZ;
          SetLength(Shape.FMArray, Series.Count);
          SetLength(Shape.FParts, Series.Count);
          SetLength(Shape.FPoints, Series.Count);
          SetLength(Shape.FZArray, Series.Count);

          PointCount := 0;
          APoint := nil;
          for SeriesIndex := 0 to Series.Count - 1 do
          begin
            ASeries := Series[SeriesIndex];
            PlotIndex := ASeries.Times.IndexOf(ATime);
            if PlotIndex >= 0 then
            begin
              APoint := ASeries.Points[PlotIndex];
              Shape.FMArray[PointCount] := APoint.FParticleIndex;
              Shape.FZArray[PointCount] := APoint.FZ;
              Shape.FParts[PointCount] := PointCount;
              Shape.FPoints[PointCount].x := APoint.FX;
              Shape.FPoints[PointCount].y := APoint.FY;
              Inc(PointCount);
            end;
          end;
          SetLength(Shape.FMArray, PointCount);
          SetLength(Shape.FParts, PointCount);
          SetLength(Shape.FPoints, PointCount);
          SetLength(Shape.FZArray, PointCount);
          if PointCount > 0 then
          begin
            Shape.FNumParts := PointCount;
            Shape.FNumPoints := PointCount;
            Shape.FNumPoints := PointCount;

            Assert(APoint <> nil);
            ShapeFileWriter.AddShape(Shape);

            ShapeDataBase.AppendBlank;
            ShapeDataBase.UpdFieldNum(StrTRACKTIME, APoint.FTrackingTime);
            ShapeDataBase.PostChanges;
          end
          else
          begin
            Shape.Free;
          end;
        except
          Shape.Free;
          raise;
        end;
      end;
      ShapeFileWriter.WriteToFile(FileName, ChangeFileExt(FileName, '.shx'));
    finally
      ShapeFileWriter.Free;
    end;
  finally
    ShapeDataBase.Active := False;
    ShapeDataBase.Free;
  end;
end;

procedure TTimeSeriesReader.GetMinMaxValues(var MaxValue, MinValue: Double);
var
  Grid: TModflowGrid;
begin
  Grid := frmGoPhast.ModflowGrid;
  if ColorLimits.UseLimit then
  begin
    MinValue := ColorLimits.MinColorLimit;
    MaxValue := ColorLimits.MaxColorLimit;
  end
  else
  begin
    MinValue := 0;
    MaxValue := 1;
    case ColorLimits.ColoringChoice of
      tscNone:
        begin
          MinValue := 0;
          MaxValue := 1;
        end;
      tscParticleNumber:
        begin
          MinValue := 1;
          MaxValue := Series.Count;
        end;
      tscXPrime, tscStartXPrime, tscEndXPrime:
        begin
          MinValue := Grid.ColumnPosition[0];
          MaxValue := Grid.ColumnPosition[Grid.ColumnCount];
        end;
      tscYPrime, tscStartYPrime, tscEndYPrime:
        begin
          MaxValue := Grid.RowPosition[0];
          MinValue := Grid.RowPosition[Grid.RowCount];
        end;
      tscZ, tscStartZ, tscEndZ:
        begin
          MinValue := Grid.LowestElevation;
          MaxValue := Grid.HighestElevation;
        end;
      else Assert(False);
    end;
  end;
end;

function TTimeSeriesReader.GetPointColor(MaxValue, MinValue: double;
  Point: TTimeSeriesPoint): TColor;
var
  AValue: Double;
  StartPoint: TTimeSeriesPoint;
  EndPoint: TTimeSeriesPoint;
begin
  AValue := 0;
  case ColorLimits.ColoringChoice of
    tscNone:
      begin
        result := clBlack;
        Exit;
      end;
    tscParticleNumber:
      begin
        AValue := Point.ParticleIndex;
      end;
    tscXPrime:
      begin
        AValue := Point.XPrime;
      end;
    tscStartXPrime:
      begin
        StartPoint := Point.Collection.Items[0] as TTimeSeriesPoint;
        AValue := StartPoint.XPrime;
      end;
    tscEndXPrime:
      begin
        EndPoint := Point.Collection.Items[Point.Collection.Count-1]
          as TTimeSeriesPoint;
        AValue := EndPoint.XPrime;
      end;
    tscYPrime:
      begin
        AValue := Point.YPrime;
      end;
    tscStartYPrime:
      begin
        StartPoint := Point.Collection.Items[0] as TTimeSeriesPoint;
        AValue := StartPoint.YPrime;
      end;
    tscEndYPrime:
      begin
        EndPoint := Point.Collection.Items[Point.Collection.Count-1]
          as TTimeSeriesPoint;
        AValue := EndPoint.YPrime;
      end;
    tscZ:
      begin
        AValue := Point.Z;
      end;
    tscStartZ:
      begin
        StartPoint := Point.Collection.Items[0] as TTimeSeriesPoint;
        AValue := StartPoint.Z;
      end;
    tscEndZ:
      begin
        EndPoint := Point.Collection.Items[Point.Collection.Count-1]
          as TTimeSeriesPoint;
        AValue := EndPoint.Z;
      end;
    else Assert(False);
  end;
  if AValue > MaxValue then
  begin
    result := clBlack;
  end
  else if AValue < MinValue then
  begin
    result := clBlack;
  end
  else
  begin
    if MaxValue = MinValue then
    begin
      result := ColorParameters.FracToColor(0.5)
    end
    else
    begin
      result := ColorParameters.FracToColor
        (1-((AValue-MinValue)/(MaxValue-MinValue)))
    end;
  end;
end;

function TTimeSeriesReader.GetRecordedTimeSeries(ATimeIndex: integer): boolean;
begin
  EnsureGLArrays(ATimeIndex);
  result := FRecordedTimeSeries[ATimeIndex];
end;

function TTimeSeriesReader.GetTimes: TRealList;
var
  MaxCount: integer;
  MaxIndex: integer;
  Index: Integer;
  ASeries: TTimeSeries;
  APoint: TTimeSeriesPoint;
  SeriesIndex: Integer;
begin
  if FRealList = nil then
  begin
    FRealList := TRealList.Create;
    if Series.Count > 0 then
    begin
      MaxCount := 0;
      MaxIndex := 0;
      for Index := 0 to Series.Count - 1 do
      begin
        ASeries := Series[Index];
        if ASeries.Points.Count > MaxCount then
        begin
          MaxCount := ASeries.Points.Count;
          MaxIndex := Index;
        end;
      end;
      ASeries := Series[MaxIndex];
      if ASeries.Points.Count > 0 then
      begin
        FRealList.Capacity := ASeries.Points.Count;
        FRealList.Sorted := True;
        for SeriesIndex := 0 to Series.Count - 1 do
        begin
          ASeries := Series[SeriesIndex];
          for Index := 0 to ASeries.Points.Count - 1 do
          begin
            APoint := ASeries.Points[Index];
            FRealList.AddUnique(APoint.TrackingTime);
          end;
        end;
      end;
    end;
  end;
  result := FRealList;
end;

function TTimeSeriesReader.GetTimeSeriesGLIndex(ATimeIndex: integer): TGLuint;
begin
  EnsureGLArrays(ATimeIndex);
  result := FTimeSeriesGLIndex[ATimeIndex];
end;

procedure TTimeSeriesReader.Invalidate;
var
  Index: Integer;
begin
  for Index := 0 to Length(FRecordedTimeSeries) - 1 do
  begin
    FRecordedTimeSeries[Index] := False;
  end;
end;

procedure TTimeSeriesReader.ReadFile;
var
  AFile: TFileStream;
  AChar: AnsiChar;
  IsTextFile: Boolean;
  ALine: string;
  CompactFormat: Boolean;
  ParticleIndex: integer;
  XPrime: single;
  YPrime: single;
  LocalZ: single;
  Z: single;
  TrackingTime: single;
  J: integer;
  TS: integer;
  NRow: integer;
  NCol: integer;
  K: integer;
  I: integer;
  TimeSeries: TTimeSeries;
  APoint: TTimeSeriesPoint;
  Description: array[0..79] of AnsiChar;
  // 4 null bytes separate Description from the following data.
  // Use Terminator to read and ignore those 4 null bytes.
  Terminator: array[0..3] of AnsiChar;
  Grid: TModflowGrid;
  ADate: TDateTime;
  LineIndex: Integer;
  Line: TTimeSeries;
  FirstPoint: TTimeSeriesPoint;
  LastPoint: TTimeSeriesPoint;
  FirstTimeFound: Boolean;
  FTextFile: TextFile;
  FFile: TFileStream;
  TimeStepIndex: integer;
  MaxPoints: Integer;
  procedure CreateParticle;
  var
    Point2D: TPoint2D;
  begin
    While FSeries.Count < ParticleIndex do
    begin
      FSeries.Add;
    end;

    TimeSeries := FSeries[ParticleIndex-1];

    APoint := TimeSeries.FPoints.Add as TTimeSeriesPoint;
    ConvertCoordinates(Grid, XPrime, YPrime, Point2D);

    APoint.ParticleIndex := ParticleIndex;
    APoint.FTimeStepIndex := TimeStepIndex;
    APoint.FXPrime := XPrime;
    APoint.FYPrime := YPrime;
    APoint.FX := Point2D.X;
    APoint.FY := Point2D.Y;
    APoint.FLocalZ := LocalZ;
    APoint.FZ := Z;
    APoint.FTrackingTime := TrackingTime;
    APoint.FLayer := K;
    APoint.FRow := I;
    APoint.FColumn := J;
    APoint.FTimeStep := TS;
    Assert(APoint.FLayer >= 1);
    Assert(APoint.FRow >= 1);
    Assert(APoint.FColumn >= 1);
  end;
begin
  Grid := frmGoPhast.ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  FreeAndNil(FRealList);
  if FileAge(FileName, ADate) then
  begin
    FileDate := ADate;
  end;
  FSeries.Clear;
  AFile := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
  try
    AFile.Read(AChar, SizeOf(AChar));
  finally
    AFile.Free;
  end;
  IsTextFile := AChar = '@';
  NRow := Grid.RowCount;
  NCol := Grid.ColumnCount;
  if IsTextFile then
  begin
    AssignFile(FTextFile, FFileName);
    try
      Reset(FTextFile);
      Readln(FTextFile, ALine);
      CompactFormat := Pos('COMPACT',ALine) >= 1;
      While Not Eof(FTextFile) do
      begin
        if CompactFormat then
        begin
          Readln(FTextFile, TimeStepIndex, ParticleIndex, J, XPrime, YPrime,
            LocalZ, TrackingTime, TS);
          ConvertIndicies(NCol, NRow, I, K, J);
          GetZ(Grid, J, I, K, LocalZ, Z);
        end
        else
        begin
          Readln(FTextFile, TimeStepIndex, ParticleIndex, J, I, K, XPrime,
            YPrime, Z, LocalZ, TrackingTime, TS);
        end;

        CreateParticle;
      end;
    finally
      CloseFile(FTextFile);
    end;
  end
  else
  begin
    FFile := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
    try
      AFile.Read(Description, SizeOf(Description));
      AFile.Read(Terminator, SizeOf(Terminator));
      while FFile.Position < FFile.Size do
      begin
        AFile.Read(TimeStepIndex, SizeOf(TimeStepIndex));
        AFile.Read(ParticleIndex, SizeOf(ParticleIndex));
        AFile.Read(J, SizeOf(J));
        AFile.Read(XPrime, SizeOf(XPrime));
        AFile.Read(YPrime, SizeOf(YPrime));
        AFile.Read(LocalZ, SizeOf(LocalZ));
        AFile.Read(Z, SizeOf(Z));
        AFile.Read(TrackingTime, SizeOf(TrackingTime));
        AFile.Read(TS, SizeOf(TS));

        ConvertIndicies(NCol, NRow, I, K, J);
        GetZ(Grid, J, I, K, LocalZ, Z);

        CreateParticle;
      end;

    finally
      FFile.Free;
    end;
  end;

  MaxPoints := 0;
  FirstTimeFound := False;
  if Series.Count > 0 then
  begin
    for LineIndex := 0 to Series.Count - 1 do
    begin
      Line := Series[LineIndex];
      if Line.Points.Count > MaxPoints then
      begin
        MaxPoints := Line.Points.Count;
      end;
      if Line.Points.Count > 0 then
      begin
        FirstPoint := Line.Points[0];
        LastPoint := Line.Points[Line.Points.Count-1];
        if not FirstTimeFound then
        begin
          if FirstPoint.TrackingTime < LastPoint.TrackingTime then
          begin
            MinTime := FirstPoint.TrackingTime;
            MaxTime := LastPoint.TrackingTime;
          end
          else
          begin
            MinTime := LastPoint.TrackingTime;
            MaxTime := FirstPoint.TrackingTime;
          end;
          FirstTimeFound := True;
        end
        else
        begin
          if FirstPoint.TrackingTime < LastPoint.TrackingTime then
          begin
            if FirstPoint.TrackingTime < MinTime then
            begin
              MinTime := FirstPoint.TrackingTime
            end;
            if LastPoint.TrackingTime > MaxTime then
            begin
              MaxTime := LastPoint.TrackingTime;
            end;
          end
          else
          begin
            if LastPoint.TrackingTime < MinTime then
            begin
              MinTime := LastPoint.TrackingTime
            end;
            if FirstPoint.TrackingTime > MaxTime then
            begin
              MaxTime := FirstPoint.TrackingTime;
            end;
          end;
        end;
      end;
    end;
  end;
  if TimeIndex >= MaxPoints  then
  begin
    TimeIndex := MaxPoints-1;
  end;
end;

procedure TTimeSeriesReader.Record3DTimeSeries(TimeIndex: integer);
var
  Grid: TModflowGrid;
  ColRowOrLayer: Integer;
  MaxValue: Double;
  MinValue: Double;
  PointIndex: Integer;
  TimeSeriesPoint: TTimeSeriesPoint;
  AColor: TColor;
  ASeries: TTimeSeries;
  TimeToPlot: Double;
  PlotIndex: Integer;
begin
  if not Visible then
  begin
    Exit;
  end;
  if TimeIndex < 0 then
  begin
    Exit;
  end;
  Grid := frmGoPhast.ModflowGrid;
  if Grid = nil then
  begin
    Exit;
  end;
  if (Grid.LayerCount <= 0) or (Grid.RowCount <= 0)
    or (Grid.ColumnCount <= 0) then
  begin
    Exit;
  end;
  if Times.Count = 0 then
  begin
    Exit;
  end;
  ColRowOrLayer := -1;

//    EnableLighting;
  glMatrixMode(GL_MODELVIEW);

  glNewList(TimeSeriesGLIndex[TimeIndex], GL_COMPILE);
  try
    glPushMatrix;
    try
      glEnable(GL_LINE_SMOOTH);
      glShadeModel(GL_SMOOTH);

      GetMinMaxValues(MaxValue, MinValue);
      glLineWidth(1);

      TimeToPlot := Times[TimeIndex];

      glBegin(GL_POINTS);
      for PointIndex := 0 to Series.Count - 1 do
      begin
        ASeries := Series[PointIndex];
        PlotIndex := ASeries.Times.IndexOf(TimeToPlot);

        if PlotIndex >= 0 then
        begin
          if CheckShowSeries(ASeries) then
          begin
            TimeSeriesPoint := ASeries.Points[PlotIndex];
            if TimeSeriesPoint.ShouldShow(DisplayLimits, dso3D, ColRowOrLayer) then
            begin
              AColor := GetPointColor(MaxValue, MinValue, TimeSeriesPoint);
              AssignColor(AColor);
              glVertex3f(TimeSeriesPoint.X, TimeSeriesPoint.Y, TimeSeriesPoint.Z);
            end;
          end;
        end;
      end;
      glEnd;
    finally
      glPopMatrix;
    end;
  finally
    glEndList;
  end;
end;

procedure TTimeSeriesReader.SetFileDate(const Value: TDateTime);
begin
  FFileDate := Value;
end;

procedure TTimeSeriesReader.SetLines(const Value: TTimeSeriesCollection);
begin
  FSeries.Assign(Value);
end;

procedure TTimeSeriesReader.SetMaxTime(const Value: double);
begin
  FMaxTime := Value;
end;

procedure TTimeSeriesReader.SetMinTime(const Value: double);
begin
  FMinTime := Value;
end;

procedure TTimeSeriesReader.SetRecordedTimeSeries(ATimeIndex: integer;
  const Value: boolean);
begin
  EnsureGLArrays(ATimeIndex);
  FRecordedTimeSeries[ATimeIndex] := Value;
end;

procedure TTimeSeriesReader.SetTimeIndex(const Value: integer);
begin
  FTimeIndex := Value;
end;

procedure TTimeSeriesReader.SetTimes(const Value: TRealList);
begin
  if (Value = nil) or (Value.Count = 0) then
  begin
    FreeAndNil(FRealList);
  end
  else
  begin
    if FRealList = nil then
    begin
      FRealList := TRealList.Create;
    end;
    FRealList.Assign(Value);
  end;
end;

{ TTimeSeries }

procedure TTimeSeries.Assign(Source: TPersistent);
var
  SourceSeries: TTimeSeries;
begin
  if Source is TTimeSeries then
  begin
    SourceSeries := TTimeSeries(Source);
    Points := SourceSeries.Points;
    Times := SourceSeries.Times;
  end
  else
  begin
    inherited;
  end;
end;

constructor TTimeSeries.Create(Collection: TCollection);
begin
  inherited;
  FTimes := nil;
  FPoints:= TTimeSeriesPoints.Create;
end;

destructor TTimeSeries.Destroy;
begin
  FPoints.Free;
  FTimes.Free;
  inherited;
end;

procedure TTimeSeries.SetPoints(const Value: TTimeSeriesPoints);
begin
  FPoints.Assign(Value);
end;

procedure TTimeSeries.SetTimes(const Value: TRealList);
begin
  if (Value = nil) or (Value.Count = 0) then
  begin
    FreeAndNil(FTimes);
  end
  else
  begin
    if FTimes = nil then
    begin
      FTimes := TRealList.Create;
    end;
    FTimes.Assign(Value);
  end;
end;

function TTimeSeries.GetTimes: TRealList;
var
  APoint: TTimeSeriesPoint;
  PointIndex: Integer;
begin
  if FTimes = nil then
  begin
    FTimes := TRealList.Create;
    for PointIndex := 0 to Points.Count - 1 do
    begin
      APoint := Points[PointIndex];
      FTimes.Add(APoint.TrackingTime);
    end;
    FTimes.Sorted := True;
  end;
  result := FTimes;
end;

{ TTimeSeriesPoint }

procedure TTimeSeriesPoint.Assign(Source: TPersistent);
var
  SourcePoint: TTimeSeriesPoint;
begin
  if Source is TTimeSeriesPoint then
  begin
    SourcePoint:= TTimeSeriesPoint(Source);
    TimeStepIndex := SourcePoint.TimeStepIndex;
    ParticleIndex := SourcePoint.ParticleIndex;
    Layer := SourcePoint.Layer;
    Row := SourcePoint.Row;
    Column := SourcePoint.Column;
    XPrime := SourcePoint.XPrime;
    YPrime := SourcePoint.YPrime;
    X := SourcePoint.X;
    Y := SourcePoint.Y;
    Z := SourcePoint.Z;
    LocalZ := SourcePoint.LocalZ;
    TrackingTime := SourcePoint.TrackingTime;
    TimeStep := SourcePoint.TimeStep;
  end
  else
  begin
    inherited;
  end;
end;

function TTimeSeriesPoint.CheckLimits(
  Limits: TTimeSeriesDisplayLimits): boolean;
begin
  result := True;
  if Limits.ColumnLimits.UseLimit then
  begin
    result := (Limits.ColumnLimits.StartLimit <= Column)
      and (Column <= Limits.ColumnLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.RowLimits.UseLimit then
  begin
    result := (Limits.RowLimits.StartLimit <= Row)
      and (Row <= Limits.RowLimits.EndLimit);
    if not result then Exit;
  end;
  if Limits.LayerLimits.UseLimit then
  begin
    result := (Limits.LayerLimits.StartLimit <= Layer)
      and (Layer <= Limits.LayerLimits.EndLimit);
    if not result then Exit;
  end;

end;

function TTimeSeriesPoint.ShouldShow(Limits: TTimeSeriesDisplayLimits;
  Orientation: TDataSetOrientation; CurrentColRowOrLayer: integer): boolean;
var
  ColRowOrLayerToCheck: Integer;
begin
  result := True;
  if Limits.LimitToCurrentIn2D and (Orientation <> dso3D) then
  begin
    ColRowOrLayerToCheck := -1;
    case Orientation of
      dsoTop:
        begin
          ColRowOrLayerToCheck := Layer;
        end;
      dsoFront:
        begin
          ColRowOrLayerToCheck := Row;
        end;
      dsoSide:
        begin
          ColRowOrLayerToCheck := Column;
        end;
      else Assert(False);
    end;
    result := ColRowOrLayerToCheck = CurrentColRowOrLayer;
    if not result then
    begin
      Exit;
    end;
  end;
  case Limits.ShowChoice of
    scAll:
      begin
        // do nothing
      end;
    scSpecified, scStart, scEnd:
      begin
        result := CheckLimits(Limits);
        if not result then Exit;
      end;
    else Assert(False);
  end;

end;

function TTimeSeriesPoint.ShouldShowSeries(
  Limits: TTimeSeriesDisplayLimits): boolean;
begin
  result := True;
  case Limits.ShowChoice of
    scAll:
      begin
        // do nothing
      end;
    scSpecified:
      begin
        // do nothing
      end;
    scStart:
      begin
        Assert(Index = 0);
        result := CheckLimits(Limits);
      end;
    scEnd:
      begin
        Assert(Index = Collection.Count-1);
        result := CheckLimits(Limits);
      end;
    else Assert(False);
  end;

end;

{ TTimeSeriesPoints }

constructor TTimeSeriesPoints.Create;
begin
  inherited Create(TTimeSeriesPoint);
end;

function TTimeSeriesPoints.GetPoint(Index: integer): TTimeSeriesPoint;
begin
  result := Items[Index] as TTimeSeriesPoint;
end;

{ TTimeSeriesCollection }

constructor TTimeSeriesCollection.Create;
begin
  inherited Create(TTimeSeries);
end;

function TTimeSeriesCollection.GetSeries(Index: integer): TTimeSeries;
begin
  result := Items[Index] as TTimeSeries;
end;

{ TTimeSeriesDisplayLimits }

procedure TTimeSeriesDisplayLimits.Assign(Source: TPersistent);
var
  TimeSeriesSource: TTimeSeriesDisplayLimits;
begin
  if Source is TTimeSeriesDisplayLimits then
  begin
    TimeSeriesSource := TTimeSeriesDisplayLimits(Source);
    ShowChoice := TimeSeriesSource.ShowChoice;
    LimitToCurrentIn2D := TimeSeriesSource.LimitToCurrentIn2D;
    ColumnLimits := TimeSeriesSource.ColumnLimits;
    RowLimits := TimeSeriesSource.RowLimits;
    LayerLimits := TimeSeriesSource.LayerLimits;
  end
  else
  begin
    inherited;
  end;
end;

constructor TTimeSeriesDisplayLimits.Create;
begin
  inherited;
  FLimitToCurrentIn2D := True;
  FLayerLimits:= TShowIntegerLimit.Create;
  FRowLimits:= TShowIntegerLimit.Create;
  FColumnLimits:= TShowIntegerLimit.Create;
end;

destructor TTimeSeriesDisplayLimits.Destroy;
begin
  FColumnLimits.Free;
  FRowLimits.Free;
  FLayerLimits.Free;
  inherited;
end;

procedure TTimeSeriesDisplayLimits.SetColumnLimits(
  const Value: TShowIntegerLimit);
begin
  FColumnLimits.Assign(Value);
end;

procedure TTimeSeriesDisplayLimits.SetLayerLimits(
  const Value: TShowIntegerLimit);
begin
  FLayerLimits.Assign(Value)
end;

procedure TTimeSeriesDisplayLimits.SetLimitToCurrentIn2D(const Value: boolean);
begin
  FLimitToCurrentIn2D := Value;
end;

procedure TTimeSeriesDisplayLimits.SetRowLimits(const Value: TShowIntegerLimit);
begin
  FRowLimits.Assign(Value)
end;

procedure TTimeSeriesDisplayLimits.SetShowChoice(const Value: TShowChoice);
begin
  FShowChoice := Value;
end;

{ TTimeSeriesColorLimits }

procedure TTimeSeriesColorLimits.Assign(Source: TPersistent);
var
  SourceLimits: TTimeSeriesColorLimits;
begin
  if Source is TTimeSeriesColorLimits then
  begin
    SourceLimits := TTimeSeriesColorLimits(Source);
    ColoringChoice := SourceLimits.ColoringChoice;
    MinColorLimit := SourceLimits.MinColorLimit;
    MaxColorLimit := SourceLimits.MaxColorLimit;
    UseLimit := SourceLimits.UseLimit;
  end
  else
  begin
    inherited;
  end;
end;

constructor TTimeSeriesColorLimits.Create;
begin
  inherited;
  FColoringChoice := tscParticleNumber;
end;

procedure TTimeSeriesColorLimits.SetColoringChoice(
  const Value: TTimeSeriesColorLimitChoice);
begin
  FColoringChoice := Value;
end;

procedure TTimeSeriesColorLimits.SetMaxColorLimit(const Value: double);
begin
  FMaxColorLimit := Value;
end;

procedure TTimeSeriesColorLimits.SetMinColorLimit(const Value: double);
begin
  FMinColorLimit := Value;
end;

procedure TTimeSeriesColorLimits.SetUseLimit(const Value: boolean);
begin
  FUseLimit := Value;
end;

procedure TPathLineSettings.SetDisplayLimits(const Value: TPathLineDisplayLimits);
begin
  FDisplayLimits.Assign(Value);
end;

procedure TPathLineSettings.Assign(Source: TPersistent);
var
  PathLineSettings: TPathLineSettings;
begin
  if Source is TPathLineSettings then
  begin
    PathLineSettings := TPathLineSettings(Source);
    DisplayLimits := PathLineSettings.DisplayLimits;
    ColorLimits := PathLineSettings.ColorLimits;
  end;
  inherited;
end;

constructor TPathLineSettings.Create;
begin
  inherited;
  FDisplayLimits:= TPathLineDisplayLimits.Create;
  FColorLimits := TPathlineColorLimits.Create;
end;

destructor TPathLineSettings.Destroy;
begin
  FColorLimits.Free;
  FDisplayLimits.Free;
  inherited;
end;

procedure TPathLineSettings.SetColorLimits(const Value: TPathlineColorLimits);
begin
  FColorLimits.Assign(Value);
end;

procedure TEndPointSettings.Assign(Source: TPersistent);
var
  EndPointSettings: TEndPointSettings;
begin
  if Source is TEndPointSettings then
  begin
    EndPointSettings := TEndPointSettings(Source);
    ColorLimits := EndPointSettings.ColorLimits;
    DisplayLimits := EndPointSettings.DisplayLimits;
  end;
  inherited;
end;

constructor TEndPointSettings.Create;
begin
  inherited;
  FColorLimits := TEndPointColorLimits.Create;
  FDisplayLimits:= TEndPointDisplayLimits.Create;
end;

destructor TEndPointSettings.Destroy;
begin
  FDisplayLimits.Free;
  FColorLimits.Free;
  inherited;
end;

procedure TEndPointSettings.SetColorLimits(const Value: TEndPointColorLimits);
begin
  FColorLimits.Assign(Value);
end;

procedure TEndPointSettings.SetDisplayLimits(const Value: TEndPointDisplayLimits);
begin
  FDisplayLimits.Assign(Value);
end;

procedure TTimeSeriesSettings.Assign(Source: TPersistent);
var
  SourceSettings: TTimeSeriesSettings;
begin
  if Source is TTimeSeriesSettings then
  begin
    SourceSettings := TTimeSeriesSettings(Source);
    ColorLimits := SourceSettings.ColorLimits;
    DisplayLimits := SourceSettings.DisplayLimits;
  end;
  inherited;
end;

constructor TTimeSeriesSettings.Create;
begin
  inherited;
  FDisplayLimits := TTimeSeriesDisplayLimits.Create;
  FColorLimits := TTimeSeriesColorLimits.Create;
end;

destructor TTimeSeriesSettings.Destroy;
begin
  FColorLimits.Free;
  FDisplayLimits.Free;
  inherited;
end;

procedure TTimeSeriesSettings.SetColorLimits(const Value: TTimeSeriesColorLimits);
begin
  FColorLimits.Assign(Value);
end;

procedure TTimeSeriesSettings.SetDisplayLimits(const Value: TTimeSeriesDisplayLimits);
begin
  FDisplayLimits.Assign(Value);
end;

procedure TCustomModpathSettings.Assign(Source: TPersistent);
var
  Settings: TCustomModpathSettings;
begin
  if Source is TCustomModpathSettings then
  begin
    Settings := TCustomModpathSettings(Source);
    ColorParameters := Settings.ColorParameters;
    Visible := Settings.Visible;
  end
  else
  begin
    inherited;
  end;
end;

constructor TCustomModpathSettings.Create;
begin
  inherited;
  FColorParameters:= TColorParameters.Create;
  FVisible := True;
end;

destructor TCustomModpathSettings.Destroy;
begin
  FColorParameters.Free;
  inherited;
end;

procedure TCustomModpathSettings.SetColorParameters(const Value: TColorParameters);
begin
  FColorParameters.Assign(Value);
end;

end.
