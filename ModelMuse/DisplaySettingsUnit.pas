unit DisplaySettingsUnit;

interface

uses
  Types, Classes, Graphics, GoPhastTypes, LegendUnit, AbstractGridUnit,
  PathlineReader, SysUtils, EdgeDisplayUnit, DataSetUnit,
  Generics.Collections, Generics.Defaults;

type
  TTextDisplay = class(TGoPhastPersistent)
  private
    FFont: TFont;
    FText: string;
    FRect: TRect;
    procedure FontChanged(Sender: TObject);
    procedure SetFont(const Value: TFont);
    procedure SetRect(const Value: TRect);
    procedure SetText(const Value: string);
    function GetBottom: integer;
    function GetLeft: integer;
    function GetRight: integer;
    function GetTop: integer;
    procedure SetBottom(const Value: integer);
    procedure SetLeft(const Value: integer);
    procedure SetRight(const Value: integer);
    procedure SetTop(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    property Rect: TRect read FRect write SetRect;
  published
    property Text: string read FText write SetText;
    property Font: TFont read FFont write SetFont;
    property Top: integer read GetTop write SetTop;
    property Bottom: integer read GetBottom write SetBottom;
    property Left: integer read GetLeft write SetLeft;
    property Right: integer read GetRight write SetRight;
  end;

  TTextItem = class(TPhastCollectionItem)
  private
    FTextDisplay: TTextDisplay;
    procedure SetTextDisplay(const Value: TTextDisplay);
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property TextDisplay: TTextDisplay read FTextDisplay write SetTextDisplay;
  end;

  TTextCollection = class(TPhastCollection)
    constructor Create(Model: TBaseModel);
  end;

  TContourDisplaySettings= class(TGoPhastPersistent)
  private
    FDataSetName: string;
    FLegend: TLegend;
    FLegendVisible: boolean;
    FLimits: TColoringLimits;
    procedure SetDataSetName(const Value: string);
    procedure SetLegend(const Value: TLegend);
    procedure SetLegendVisible(const Value: boolean);
    procedure SetLimits(const Value: TColoringLimits);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
    destructor Destroy; override;
  published
    property DataSetName: string read FDataSetName write SetDataSetName;
    property Legend: TLegend read FLegend write SetLegend;
    property LegendVisible: boolean read FLegendVisible write SetLegendVisible;
    property Limits: TColoringLimits read FLimits write SetLimits;
  end;

  TColorDisplaySettings = class(TContourDisplaySettings)
  private
    FTimeListName: string;
    FTime: double;
    FShadeInactiveArea: boolean;
    procedure SetTime(const Value: double);
    procedure SetTimeListName(const Value: string);
    procedure SetShadeInactiveArea(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property TimeListName: string read FTimeListName write SetTimeListName;
    property Time: double read FTime write SetTime;
    property ShadeInactiveArea: boolean read FShadeInactiveArea
      write SetShadeInactiveArea;
  end;

  TRulerSettings = class(TGoPhastPersistent)
  private
    FRulerPrecision: integer;
    FRulerDigits: integer;
    FVisible: boolean;
    procedure SetRulerDigits(const Value: integer);
    procedure SetRulerPrecision(const Value: integer);
    procedure SetVisible(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property RulerPrecision: integer read FRulerPrecision write SetRulerPrecision;
    property RulerDigits: integer read FRulerDigits write SetRulerDigits;
    property Visible: boolean read FVisible write SetVisible;
  end;

  TStreamsToPlot = (stpNone, stpAll, stpVisible, stpSelected);

  TSfrStreamPlot = class(TObject)
  private
    FStreamObject: TObject;
    procedure SetStreamObject(const Value: TObject);
  public
    Segment: Integer;
    OutflowSegment: integer;
    DiversionSegment: integer;
    property StreamObject: TObject read FStreamObject write SetStreamObject;
  end;

  TLakePlot = class(TObject)
  private
    FLakeObject: TObject;
    procedure SetLakeObject(const Value: TObject);
  public
    LakeId: Integer;
    property LakeObject: TObject read FLakeObject write SetLakeObject;
  end;

  TSfrStreamPlotComparer = class(TComparer<TSfrStreamPlot>)
    function Compare(const Left, Right: TSfrStreamPlot): Integer; override;
  end;

  TSfrStreamPlotList = class(TObjectList<TSfrStreamPlot>)
  private
    procedure Sort;
  end;

  TSfrLakePlotComparer = class(TComparer<TLakePlot>)
    function Compare(const Left, Right: TLakePlot): Integer; override;
  end;

  TLakePlotList = class(TObjectList<TLakePlot>)
  private
    procedure Sort;
  end;

  TSfrStreamLinkPlot = class(TGoPhastPersistent)
  private
    FPlotStreamConnections: boolean;
    FStreamsToPlot: TStreamsToPlot;
    FPlotDiversions: boolean;
    FStreamColor: TColor;
    FDiversionColor: TColor;
    FTimeToPlot: TDateTime;
    FPlotUnconnected: Boolean;
    FUnconnectedColor: TColor;
    procedure SetDiversionColor(const Value: TColor);
    procedure SetPlotDiversions(const Value: boolean);
    procedure SetPlotStreamConnections(const Value: boolean);
    procedure SetStreamColor(const Value: TColor);
    procedure SetStreamsToPlot(const Value: TStreamsToPlot);
    procedure SetTimeToPlot(const Value: TDateTime);
    procedure SetPlotUnconnected(const Value: Boolean);
    procedure SetUnconnectedColor(const Value: TColor);
  public
    procedure GetObjectsToPlot(StreamList: TSfrStreamPlotList;
      LakeList: TLakePlotList);
    Constructor Create(Model: TBaseModel);
    procedure Assign(Source: TPersistent); override;
  published
    property PlotStreamConnections: boolean read FPlotStreamConnections
      write SetPlotStreamConnections default True;
    property PlotDiversions: boolean read FPlotDiversions
      write SetPlotDiversions default True;
    property PlotUnconnected: Boolean read FPlotUnconnected
      write SetPlotUnconnected default True;
    property StreamColor: TColor read FStreamColor
      write SetStreamColor default clBlue;
    property DiversionColor: TColor read FDiversionColor
      write SetDiversionColor default clLime;
    property UnconnectedColor: TColor read FUnconnectedColor
      write SetUnconnectedColor default clRed;
    property StreamsToPlot: TStreamsToPlot read FStreamsToPlot
      write SetStreamsToPlot;
    property TimeToPlot: TDateTime read FTimeToPlot write SetTimeToPlot;
  end;



  {A @name stores all the information
   needed to restore the appearance of ModelMuse to a previous
   state including the name of the data set used to color or
   contour the grid and the magnification.
  }
  TDisplaySettingsItem = class(TPhastCollectionItem)
  private
    FShowColoredGridLines: boolean;
    FViewToDisplay: TViewDirection;
    FTitle: TTextDisplay;
    FVerticaRuler: TRulerSettings;
    FAdditionalText: TTextCollection;
    FGridDisplayChoice: TGridLineDrawingChoice;
    FHorizontalRuler: TRulerSettings;
    FModpathPathLineSettings: TPathLineSettings;
    FModpathEndPointSettings: TEndPointSettings;
    FModpathTimeSeriesSettings: TTimeSeriesSettings;
    FColorDisplaySettings: TColorDisplaySettings;
    FMagnification: double;
    FReferencePointX: double;
    FReferencePointY: double;
    FContourDisplaySettings: TContourDisplaySettings;
    FName: string;
    FVerticalExaggeration: double;
    FEdgeDisplaySettings: TEdgeDisplaySettings;
    FImageHeight: integer;
    FImageWidth: integer;
    FVisibleObjects: TStringList;
    FContourFont: TFont;
    FLabelContours: boolean;
    FSfrStreamLinkPlot: TSfrStreamLinkPlot;
    procedure SetAdditionalText(const Value: TTextCollection);
    procedure SetGridDisplayChoice(const Value: TGridLineDrawingChoice);
    procedure SetHorizontalRuler(const Value: TRulerSettings);
    procedure SetShowColoredGridLines(const Value: boolean);
    procedure SetTitle(const Value: TTextDisplay);
    procedure SetVerticalRuler(const Value: TRulerSettings);
    procedure SetViewToDisplay(const Value: TViewDirection);
    procedure SetModpathEndPointSettings(const Value: TEndPointSettings);
    procedure SetShowModpathPathLineSettings(const Value: TPathLineSettings);
    procedure SetModpathTimeSeriesSettings(const Value: TTimeSeriesSettings);
    procedure SetColorDisplaySettings(const Value: TColorDisplaySettings);
    procedure SetMagnification(const Value: double);
    procedure SetReferencePointX(const Value: double);
    procedure SetReferencePointY(const Value: double);
    procedure SetContourDisplaySettings(const Value: TContourDisplaySettings);
    procedure SetVerticalExaggeration(const Value: double);
    procedure SetEdgeDisplaySettings(const Value: TEdgeDisplaySettings);
    procedure SetImageHeight(const Value: integer);
    procedure SetImageWidth(const Value: integer);
    procedure SetVisibleObjects(const Value: TStringList);
    procedure SetContourFont(const Value: TFont);
    procedure SetSfrStreamLinkPlot(const Value: TSfrStreamLinkPlot);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property ViewToDisplay: TViewDirection read FViewToDisplay
      write SetViewToDisplay;
    property Title: TTextDisplay read FTitle write SetTitle;
    property AdditionalText: TTextCollection read FAdditionalText
      write SetAdditionalText;
    property GridDisplayChoice: TGridLineDrawingChoice read FGridDisplayChoice
      write SetGridDisplayChoice;
    property ShowColoredGridLines: boolean read FShowColoredGridLines
      write SetShowColoredGridLines;
    property HorizontalRuler: TRulerSettings read FHorizontalRuler
      write SetHorizontalRuler;
    property VerticalRuler: TRulerSettings read FVerticaRuler
      write SetVerticalRuler;
    property ModpathEndPointSettings: TEndPointSettings
      read FModpathEndPointSettings write SetModpathEndPointSettings;
    property ModpathPathLineSettings: TPathLineSettings
      read FModpathPathLineSettings write SetShowModpathPathLineSettings;
    property ModpathTimeSeriesSettings: TTimeSeriesSettings
      read FModpathTimeSeriesSettings write SetModpathTimeSeriesSettings;
    property ColorDisplaySettings: TColorDisplaySettings
      read FColorDisplaySettings write SetColorDisplaySettings;
    property ContourDisplaySettings: TContourDisplaySettings
      read FContourDisplaySettings write SetContourDisplaySettings;
    property EdgeDisplaySettings: TEdgeDisplaySettings read FEdgeDisplaySettings
      write SetEdgeDisplaySettings;
    property Magnification: double read FMagnification write SetMagnification;
    property ReferencePointX: double read FReferencePointX
      write SetReferencePointX;
    property ReferencePointY: double read FReferencePointY
      write SetReferencePointY;
    property VerticalExaggeration: double read FVerticalExaggeration
      write SetVerticalExaggeration;
    property Name: string read FName write FName;
    property ImageWidth: integer read FImageWidth write SetImageWidth;
    property ImageHeight: integer read FImageHeight write SetImageHeight;
    property VisibleObjects: TStringList read FVisibleObjects
      write SetVisibleObjects;
    property ContourFont: TFont read FContourFont write SetContourFont;
    property LabelContours: boolean read FLabelContours write FLabelContours;
    property SfrStreamLinkPlot: TSfrStreamLinkPlot read FSfrStreamLinkPlot
      write SetSfrStreamLinkPlot;
  end;

  { @name is a collection of @link(TDisplaySettingsItem)s.
    Each @link(TDisplaySettingsItem) stores all the information
    needed to restore the appearance of ModelMuse to a previous
    state including the name of the data set used to color or
    contour the grid and the magnification.
  }
  TDisplaySettingsCollection = class(TPhastCollection)
  public
    constructor Create(Model: TBaseModel);
    function GetItemByName(const AName: string): TDisplaySettingsItem;
  end;


implementation

uses
  DrawTextUnit, RbwRuler, ScreenObjectUnit, PhastModelUnit, ModflowSfrUnit,
  ModflowSfrParamIcalcUnit, ModflowLakUnit;

{ TTextDisplay }

procedure TTextDisplay.Assign(Source: TPersistent);
var
  SourceText: TTextDisplay;
  SourceDrawItem: TDrawItem;
begin
  if Source is TTextDisplay then
  begin
    SourceText :=TTextDisplay(Source);
    Text := SourceText.Text;
    Top := SourceText.Top;
    Left := SourceText.Left;
    Bottom := SourceText.Bottom;
    Right := SourceText.Right;
    Font := SourceText.Font;
  end
  else if Source is TDrawItem then
  begin
    SourceDrawItem :=TDrawItem(Source);
    Text := SourceDrawItem.Text;
    Rect := SourceDrawItem.Rect;
    Font := SourceDrawItem.Font;
  end
  else
  begin
    inherited;
  end;
end;

procedure TTextDisplay.AssignTo(Dest: TPersistent);
var
  DestItem: TDrawItem;
begin
  if Dest is TDrawItem then
  begin
    DestItem := TDrawItem(Dest);
    DestItem.Text := Text;
    DestItem.Rect := Rect;
    DestItem.Font := Font;
  end
  else
  begin
    inherited;
  end;
end;

constructor TTextDisplay.Create(Model: TBaseModel);
begin
  inherited;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
end;

destructor TTextDisplay.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TTextDisplay.FontChanged(Sender: TObject);
begin
  InvalidateModel;
end;


function TTextDisplay.GetBottom: integer;
begin
  result := Rect.Bottom;
end;

function TTextDisplay.GetLeft: integer;
begin
  result := Rect.Left;
end;

function TTextDisplay.GetRight: integer;
begin
  result := Rect.Left;
end;

function TTextDisplay.GetTop: integer;
begin
  result := Rect.Top;
end;

procedure TTextDisplay.SetBottom(const Value: integer);
begin
  SetIntegerProperty(FRect.Bottom, Value);
end;

procedure TTextDisplay.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TTextDisplay.SetLeft(const Value: integer);
begin
  SetIntegerProperty(FRect.Left, Value);
end;

procedure TTextDisplay.SetRect(const Value: TRect);
begin
  SetPointProperty(FRect.TopLeft, Value.TopLeft);
  SetPointProperty(FRect.BottomRight, Value.BottomRight);
end;

procedure TTextDisplay.SetRight(const Value: integer);
begin
  SetIntegerProperty(FRect.Right, Value);
end;

procedure TTextDisplay.SetText(const Value: string);
begin
  SetStringProperty(FText, Value);
end;

procedure TTextDisplay.SetTop(const Value: integer);
begin
  SetIntegerProperty(FRect.Top, Value);
end;

{ TTextItem }

procedure TTextItem.Assign(Source: TPersistent);
var
  SourceText: TTextItem;
begin
  if Source is TTextItem then
  begin
    SourceText := TTextItem(Source);
    TextDisplay := SourceText.TextDisplay;
  end
  else if Source is TDrawItem then
  begin
    TextDisplay.Assign(Source);
  end
  else
  begin
    inherited;
  end;
end;

procedure TTextItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TDrawItem then
  begin
    TextDisplay.AssignTo(Dest);
  end
  else
  begin
    inherited;
  end;
end;

constructor TTextItem.Create(Collection: TCollection);
begin
  inherited;
  FTextDisplay:= TTextDisplay.Create(Model);
end;

destructor TTextItem.Destroy;
begin
  FTextDisplay.Free;
  inherited;
end;

procedure TTextItem.SetTextDisplay(const Value: TTextDisplay);
begin
  FTextDisplay.Assign(Value);
end;

{ TTextCollection }

constructor TTextCollection.Create(Model: TBaseModel);
begin
  inherited Create(TTextItem, Model);
end;

{ TDisplaySettingsItem }

procedure TDisplaySettingsItem.Assign(Source: TPersistent);
var
  SourceDisplay: TDisplaySettingsItem;
begin
  if Source is TDisplaySettingsItem then
  begin
    SourceDisplay := TDisplaySettingsItem(Source);
    ViewToDisplay := SourceDisplay.ViewToDisplay;
    Title := SourceDisplay.Title;
    AdditionalText := SourceDisplay.AdditionalText;
    GridDisplayChoice := SourceDisplay.GridDisplayChoice;
    ShowColoredGridLines := SourceDisplay.ShowColoredGridLines;
    HorizontalRuler := SourceDisplay.HorizontalRuler;
    VerticalRuler := SourceDisplay.VerticalRuler;
    ModpathEndPointSettings := SourceDisplay.ModpathEndPointSettings;
    ModpathPathLineSettings := SourceDisplay.ModpathPathLineSettings;
    ModpathTimeSeriesSettings := SourceDisplay.ModpathTimeSeriesSettings;
    ColorDisplaySettings := SourceDisplay.ColorDisplaySettings;
    ContourDisplaySettings := SourceDisplay.ContourDisplaySettings;
    Magnification := SourceDisplay.Magnification;
    ReferencePointX := SourceDisplay.ReferencePointX;
    ReferencePointY := SourceDisplay.ReferencePointY;
    VerticalExaggeration := SourceDisplay.VerticalExaggeration;
    Name := SourceDisplay.Name;
    ImageWidth := SourceDisplay.ImageWidth;
    ImageHeight := SourceDisplay.ImageHeight;
    VisibleObjects := SourceDisplay.VisibleObjects;
    ContourFont := SourceDisplay.ContourFont;
    LabelContours := SourceDisplay.LabelContours;
    SfrStreamLinkPlot := SourceDisplay.SfrStreamLinkPlot;
  end
  else
  begin
    inherited;
  end;
end;

constructor TDisplaySettingsItem.Create(Collection: TCollection);
begin
  inherited;
  FTitle:= TTextDisplay.Create(Model);
  FAdditionalText := TTextCollection.Create(Model);
  FColorDisplaySettings := TColorDisplaySettings.Create(Model);
  FContourDisplaySettings := TContourDisplaySettings.Create(Model);
  FModpathPathLineSettings := TPathLineSettings.Create;
  FModpathEndPointSettings := TEndPointSettings.Create;
  FModpathTimeSeriesSettings := TTimeSeriesSettings.Create;
  FEdgeDisplaySettings := TEdgeDisplaySettings.Create;
  FHorizontalRuler := TRulerSettings.Create(Model);
  FVerticaRuler := TRulerSettings.Create(Model);
  FVisibleObjects := TStringList.Create;
  FContourFont := TFont.Create;
  FSfrStreamLinkPlot := TSfrStreamLinkPlot.Create(Model);
end;

destructor TDisplaySettingsItem.Destroy;
begin
  FSfrStreamLinkPlot.Free;
  FContourFont.Free;
  FVisibleObjects.Free;
  FVerticaRuler.Free;
  FHorizontalRuler.Free;
  FEdgeDisplaySettings.Free;
  FModpathTimeSeriesSettings.Free;
  FModpathEndPointSettings.Free;
  FModpathPathLineSettings.Free;
  FContourDisplaySettings.Free;
  FColorDisplaySettings.Free;
  FAdditionalText.Free;
  FTitle.Free;
  inherited;
end;

procedure TDisplaySettingsItem.SetAdditionalText(const Value: TTextCollection);
begin
  FAdditionalText.Assign(Value);
end;

procedure TColorDisplaySettings.Assign(Source: TPersistent);
var
  SourceDisplay: TColorDisplaySettings;
begin
  if Source is TColorDisplaySettings then
  begin
    SourceDisplay := TColorDisplaySettings(Source);
    TimeListName := SourceDisplay.TimeListName;
    Time := SourceDisplay.Time;
    ShadeInactiveArea := SourceDisplay.ShadeInactiveArea;
  end;
  inherited;
end;

procedure TColorDisplaySettings.SetTime(const Value: double);
begin
  SetRealProperty(FTime, Value);
end;

procedure TColorDisplaySettings.SetTimeListName(const Value: string);
begin
  SetStringProperty(FTimeListName, Value);
end;

procedure TDisplaySettingsItem.SetColorDisplaySettings(
  const Value: TColorDisplaySettings);
begin
  FColorDisplaySettings.Assign(Value);
end;

procedure TDisplaySettingsItem.SetContourDisplaySettings(
  const Value: TContourDisplaySettings);
begin
  FContourDisplaySettings.Assign(Value);
end;

procedure TDisplaySettingsItem.SetContourFont(const Value: TFont);
begin
  FContourFont.Assign(Value);
end;

procedure TDisplaySettingsItem.SetEdgeDisplaySettings(
  const Value: TEdgeDisplaySettings);
begin
  FEdgeDisplaySettings.Assign(Value);
end;

procedure TDisplaySettingsItem.SetGridDisplayChoice(
  const Value: TGridLineDrawingChoice);
begin
  if (FGridDisplayChoice <> Value) then
  begin
    FGridDisplayChoice := Value;
    InvalidateModel;
  end;
end;

procedure TDisplaySettingsItem.SetHorizontalRuler(const Value: TRulerSettings);
begin
  FHorizontalRuler.Assign(Value);
end;

procedure TDisplaySettingsItem.SetImageHeight(const Value: integer);
begin
  SetIntegerProperty(FImageHeight, Value);
end;

procedure TDisplaySettingsItem.SetImageWidth(const Value: integer);
begin
  SetIntegerProperty(FImageWidth, Value);
end;

procedure TDisplaySettingsItem.SetMagnification(const Value: double);
begin
  SetRealProperty(FMagnification, Value);
end;

procedure TDisplaySettingsItem.SetReferencePointX(const Value: double);
begin
  SetRealProperty(FReferencePointX, Value);
end;

procedure TDisplaySettingsItem.SetReferencePointY(const Value: double);
begin
  SetRealProperty(FReferencePointY, Value);
end;

procedure TColorDisplaySettings.SetShadeInactiveArea(const Value: boolean);
begin
  SetBooleanProperty(FShadeInactiveArea, Value);
end;

procedure TDisplaySettingsItem.SetSfrStreamLinkPlot(
  const Value: TSfrStreamLinkPlot);
begin
  FSfrStreamLinkPlot.Assign(Value);
end;

procedure TDisplaySettingsItem.SetShowColoredGridLines(const Value: boolean);
begin
  SetBooleanProperty(FShowColoredGridLines, Value);
end;

procedure TDisplaySettingsItem.SetModpathEndPointSettings(const Value: TEndPointSettings);
begin
  FModpathEndPointSettings.Assign(Value);
end;

procedure TDisplaySettingsItem.SetShowModpathPathLineSettings(
  const Value: TPathLineSettings);
begin
  FModpathPathLineSettings.Assign(Value);
end;

procedure TDisplaySettingsItem.SetModpathTimeSeriesSettings(
  const Value: TTimeSeriesSettings);
begin
  FModpathTimeSeriesSettings.Assign(Value);
end;

procedure TDisplaySettingsItem.SetTitle(const Value: TTextDisplay);
begin
  FTitle.Assign(Value);
end;

procedure TDisplaySettingsItem.SetVerticalExaggeration(const Value: double);
begin
  SetRealProperty(FVerticalExaggeration, Value);
end;

procedure TDisplaySettingsItem.SetVerticalRuler(const Value: TRulerSettings);
begin
  FVerticaRuler.Assign(Value);
end;

procedure TDisplaySettingsItem.SetViewToDisplay(
  const Value: TViewDirection);
begin
  if (FViewToDisplay <> Value) then
  begin
    FViewToDisplay := Value;
    InvalidateModel;
  end;
end;

procedure TDisplaySettingsItem.SetVisibleObjects(const Value: TStringList);
begin
  FVisibleObjects.Assign(Value);
end;

{ TDisplaySettingsCollection }

constructor TDisplaySettingsCollection.Create(Model: TBaseModel);
begin
  inherited Create(TDisplaySettingsItem, Model);
end;

function TDisplaySettingsCollection.GetItemByName(
  const AName: string): TDisplaySettingsItem;
var
  Index: Integer;
  AnItem: TDisplaySettingsItem;
begin
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    AnItem := Items[Index] as TDisplaySettingsItem;
    if SameText(AName, AnItem.Name) then
    begin
      result := AnItem;
      Exit;
    end;
  end;
end;

{ TContourDisplaySettings }

procedure TContourDisplaySettings.Assign(Source: TPersistent);
var
  SourceDisplay: TContourDisplaySettings;
begin
  if Source is TContourDisplaySettings then
  begin
    SourceDisplay := TContourDisplaySettings(Source);
    DataSetName := SourceDisplay.DataSetName;
    Legend := SourceDisplay.Legend;
    LegendVisible := SourceDisplay.LegendVisible;
    Limits := SourceDisplay.Limits;
  end
  else
  begin
    inherited;
  end;
end;

constructor TContourDisplaySettings.Create(Model: TBaseModel);
begin
  inherited;
  FLegend := TLegend.Create(FModel);
  FLimits:= TColoringLimits.Create;
end;

destructor TContourDisplaySettings.Destroy;
begin
  FLegend.Free;
  FLimits.Free;
  inherited;
end;

procedure TContourDisplaySettings.SetDataSetName(const Value: string);
begin
  SetStringProperty(FDataSetName, Value);
end;

procedure TContourDisplaySettings.SetLegend(const Value: TLegend);
begin
  if Value = nil then
  begin
    FreeAndNil(FLegend);
  end
  else
  begin
    if FLegend = nil then
    begin
      FLegend := TLegend.Create(FModel);
    end;
    FLegend.Assign(Value);
  end;
end;

procedure TContourDisplaySettings.SetLegendVisible(const Value: boolean);
begin
  SetBooleanProperty(FLegendVisible, Value);
end;

procedure TContourDisplaySettings.SetLimits(const Value: TColoringLimits);
begin
  FLimits.Assign(Value);
end;

{ TRulerSettings }

procedure TRulerSettings.Assign(Source: TPersistent);
var
  SourceSettings: TRulerSettings;
  SourceRuler: TRbwRuler;
begin
  if Source is TRulerSettings then
  begin
    SourceSettings := TRulerSettings(Source);
    RulerPrecision := SourceSettings.RulerPrecision;
    RulerDigits := SourceSettings.RulerDigits;
    Visible := SourceSettings.Visible;
  end
  else if Source is TRbwRuler then
  begin
    SourceRuler := TRbwRuler(Source);
    RulerPrecision := SourceRuler.RulerPrecision;
    RulerDigits := SourceRuler.RulerDigits;
  end
  else
  begin
    inherited;
  end;
end;

procedure TRulerSettings.AssignTo(Dest: TPersistent);
var
  DestRuler: TRbwRuler;
begin
  if Dest is TRbwRuler then
  begin
    DestRuler := TRbwRuler(Dest);
    DestRuler.RulerPrecision := RulerPrecision;
    DestRuler.RulerDigits := RulerDigits;
  end
  else
  begin
    inherited;
  end;
end;

procedure TRulerSettings.SetRulerDigits(const Value: integer);
begin
  SetIntegerProperty(FRulerDigits, Value);
end;

procedure TRulerSettings.SetRulerPrecision(const Value: integer);
begin
  SetIntegerProperty(FRulerPrecision, Value);
end;

procedure TRulerSettings.SetVisible(const Value: boolean);
begin
  SetBooleanProperty(FVisible, Value);
end;

{ TSfrStreamPlotComparer }

function TSfrStreamPlotComparer.Compare(const Left,
  Right: TSfrStreamPlot): Integer;
begin
  result := Left.Segment - Right.Segment;
end;

{ TSfrStreamPlotList }

procedure TSfrStreamPlotList.Sort;
var
  Comparer: IComparer<TSfrStreamPlot>;
begin
  Comparer:= TSfrStreamPlotComparer.Create;
  inherited Sort(Comparer);
end;

{ TSfrLakePlotComparer }

function TSfrLakePlotComparer.Compare(const Left, Right: TLakePlot): Integer;
begin
  result := Left.LakeId - Right.LakeId;
end;

{ TLakePlotList }

procedure TLakePlotList.Sort;
var
  Comparer: IComparer<TLakePlot>;
begin
  Comparer:= TSfrLakePlotComparer.Create;
  inherited Sort(Comparer);
end;

{ TStreamLinkPlot }

procedure TSfrStreamLinkPlot.Assign(Source: TPersistent);
var
  SourceStreamLink: TSfrStreamLinkPlot;
begin
  if Source is TSfrStreamLinkPlot then
  begin
    SourceStreamLink := TSfrStreamLinkPlot(Source);
    PlotStreamConnections := SourceStreamLink.PlotStreamConnections;
    PlotDiversions := SourceStreamLink.PlotDiversions;
    StreamColor := SourceStreamLink.StreamColor;
    DiversionColor := SourceStreamLink.DiversionColor;
    StreamsToPlot := SourceStreamLink.StreamsToPlot;
    TimeToPlot := SourceStreamLink.TimeToPlot;
    PlotUnconnected := SourceStreamLink.PlotUnconnected;
    UnconnectedColor := SourceStreamLink.UnconnectedColor;
  end
  else
  begin
    inherited;
  end;
end;

constructor TSfrStreamLinkPlot.Create(Model: TBaseModel);
begin
  inherited;
  FStreamColor := clBlue;
  FDiversionColor := clLime;
  FUnconnectedColor := clRed;
  FPlotStreamConnections := True;
  FPlotDiversions := True;
  FPlotUnconnected := True;
end;

procedure TSfrStreamLinkPlot.GetObjectsToPlot(StreamList: TSfrStreamPlotList;
  LakeList: TLakePlotList);
var
  LocalModel: TPhastModel;
  Index : integer;
  ScreenObject: TScreenObject;
  SfrBoundary: TSfrBoundary;
  Item: TSfrParamIcalcItem;
  StreamPlot: TSfrStreamPlot;
  LakBoundary: TLakBoundary;
  Lake: TLakePlot;
begin
  StreamList.Clear;
  LakeList.Clear;
  if StreamsToPlot <> stpNone then
  begin
    LocalModel := FModel as TPhastModel;
    if not LocalModel.SfrIsSelected then
    begin
      Exit;
    end;
    for Index := 0 to LocalModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := LocalModel.ScreenObjects[Index];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      case StreamsToPlot of
        stpVisible:
          begin
            if not ScreenObject.Visible then
            begin
              Continue;
            end;
          end;
        stpSelected:
          begin
            if not ScreenObject.Selected then
            begin
              Continue;
            end;
          end;
      end;
      SfrBoundary := ScreenObject.ModflowBoundaries.ModflowSfrBoundary;
      if SfrBoundary <> nil then
      begin
        Item := SfrBoundary.ParamIcalc.GetItemByStartTime(TimeToPlot);
        if Item <> nil then
        begin
          StreamPlot := TSfrStreamPlot.Create;
          StreamPlot.StreamObject := ScreenObject;
          StreamPlot.Segment := SfrBoundary.SegementNumber;
          StreamPlot.OutflowSegment := Item.OutflowSegment;
          StreamPlot.DiversionSegment := Item.DiversionSegment;
          StreamList.Add(StreamPlot);
        end;
      end;
      if LocalModel.LakIsSelected then
      begin
        LakBoundary := ScreenObject.ModflowBoundaries.ModflowLakBoundary;
        if LakBoundary <> nil then
        begin
          Lake := TLakePlot.Create;
          Lake.LakeObject := ScreenObject;
          Lake.LakeId := LakBoundary.LakeID;
          LakeList.Add(Lake);
        end;
      end;
    end;
    StreamList.Sort;
    LakeList.Sort;
  end;
end;

procedure TSfrStreamLinkPlot.SetDiversionColor(const Value: TColor);
begin
  SetColorProperty(FDiversionColor, Value);
end;

procedure TSfrStreamLinkPlot.SetPlotDiversions(const Value: boolean);
begin
  SetBooleanProperty(FPlotDiversions, Value);
end;

procedure TSfrStreamLinkPlot.SetPlotStreamConnections(const Value: boolean);
begin
  SetBooleanProperty(FPlotStreamConnections, Value);
end;

procedure TSfrStreamLinkPlot.SetPlotUnconnected(const Value: Boolean);
begin
  SetBooleanProperty(FPlotUnconnected, Value);
end;

procedure TSfrStreamLinkPlot.SetStreamColor(const Value: TColor);
begin
  SetColorProperty(FStreamColor, Value);
end;

procedure TSfrStreamLinkPlot.SetStreamsToPlot(const Value: TStreamsToPlot);
begin
  if FStreamsToPlot <> Value then
  begin
    FStreamsToPlot := Value;
  end;
end;

procedure TSfrStreamLinkPlot.SetTimeToPlot(const Value: TDateTime);
begin
  SetDataTimeProperty(FTimeToPlot, Value);
end;

procedure TSfrStreamLinkPlot.SetUnconnectedColor(const Value: TColor);
begin
  SetColorProperty(FUnconnectedColor, Value);
end;


{ TLakePlot }

procedure TLakePlot.SetLakeObject(const Value: TObject);
begin
  Assert(Value is TScreenObject);
  FLakeObject := Value;
end;

{ TSfrStreamPlot }

procedure TSfrStreamPlot.SetStreamObject(const Value: TObject);
begin
  Assert(Value is TScreenObject);
  FStreamObject := Value;
end;

end.
