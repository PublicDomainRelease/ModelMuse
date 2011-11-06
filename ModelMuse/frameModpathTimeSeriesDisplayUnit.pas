unit frameModpathTimeSeriesDisplayUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, Grids, RbwDataGrid4, StdCtrls, ExtCtrls, JvSpin, JvExControls,
  JvxSlider, ComCtrls, JvExComCtrls, JvUpDown, Mask, JvExMask, JvToolEdit,
  frameModpathDisplayUnit, UndoItems, PathlineReader, RealListUnit;

type
  TTimeSeriesLimits = (tslNone, tslColors, tslLayer, tslRow, tslColumn);

const
  TableCaptions: array[Low(TTimeSeriesLimits)..
    High(TTimeSeriesLimits)] of string =
    ('', Colorlimits, Layer, Row, Column);

type
  TUndoImportTimeSeries = class(TCustomUndo)
  private
    FExistingTimeSeries: TTimeSeriesReader;
    FNewTimeSeries: TTimeSeriesReader;
    FImportedNewFile: Boolean;
    procedure ForceRedraw;
    procedure EnableMenuItems;
  public
    Constructor Create(var NewTimeSeries: TTimeSeriesReader;
      ImportedNewFile: boolean);
    Destructor Destroy; override;
    function Description: string; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TframeModpathTimeSeriesDisplay = class(TFrame)
    pcMain: TPageControl;
    tabBasic: TTabSheet;
    lblModpathFile: TLabel;
    lblTimeToPlot: TLabel;
    lblColorScheme: TLabel;
    pbColorScheme: TPaintBox;
    lblColorAdjustment: TLabel;
    lblCycles: TLabel;
    fedModpathFile: TJvFilenameEdit;
    comboTimeToPlot: TComboBox;
    udTimeToPlot: TJvUpDown;
    cbShowPathlines: TCheckBox;
    cbLimitToCurrentIn2D: TCheckBox;
    comboColorScheme: TComboBox;
    jsColorExponent: TJvxSlider;
    seColorExponent: TJvSpinEdit;
    seCycles: TJvSpinEdit;
    tabOptions: TTabSheet;
    rgShow2D: TRadioGroup;
    rgColorBy: TRadioGroup;
    rdgLimits: TRbwDataGrid4;
    procedure rdgLimitsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgLimitsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgLimitsStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure rgShow2DClick(Sender: TObject);
    procedure rgColorByClick(Sender: TObject);
    procedure fedModpathFileBeforeDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
    procedure fedModpathFileChange(Sender: TObject);
    procedure udTimeToPlotChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
    procedure pbColorSchemePaint(Sender: TObject);
    procedure comboColorSchemeChange(Sender: TObject);
    procedure seColorExponentChange(Sender: TObject);
    procedure seCyclesChange(Sender: TObject);
    procedure jsColorExponentChanged(Sender: TObject);
    procedure comboTimeToPlotChange(Sender: TObject);
  protected
    procedure Loaded; override;
  private
    procedure AssignTimesToComboBox(Times: TRealList);
    procedure ReadIntLimit(IntLimits: TShowIntegerLimit;
      ALimitRow: TTimeSeriesLimits);
    procedure SetIntLimit(LimitRow: TTimeSeriesLimits; DefaultLimit: integer;
      IntLimit: TShowIntegerLimit);
    { Private declarations }
  public
    procedure GetData;
    procedure SetData;
    { Public declarations }
  end;

implementation

uses
  frmGoPhastUnit, ColorSchemes, ModflowGridUnit, ModelMuseUtilities;

resourcestring
  StrTheTimeSeriesFile = 'The time series file on disk has a different date ' +
  'than the file that was imported into ModelMuse.  Do you want to import th' +
  'e new file?';
  StrLimitingFactor = 'Limiting factor';
  StrLowerLimit = 'Lower limit';
  StrUpperLimit = 'Upper limit';

{$R *.dfm}

{ TUndoImportTimeSeries }

constructor TUndoImportTimeSeries.Create(var NewTimeSeries: TTimeSeriesReader;
  ImportedNewFile: boolean);
begin
  FImportedNewFile := ImportedNewFile;
  FExistingTimeSeries:= TTimeSeriesReader.Create;
  FExistingTimeSeries.Assign(frmGoPhast.PhastModel.TimeSeries);
  // Take ownership of NewTimeSeries.
  FNewTimeSeries := NewTimeSeries;
  NewTimeSeries := nil;
end;

function TUndoImportTimeSeries.Description: string;
begin
  result := 'import MODPATH time series'
end;

destructor TUndoImportTimeSeries.Destroy;
begin
  FExistingTimeSeries.Free;
  FNewTimeSeries.Free;
  inherited;
end;

procedure TUndoImportTimeSeries.DoCommand;
begin
  frmGoPhast.PhastModel.TimeSeries := FNewTimeSeries;
  EnableMenuItems;
  ForceRedraw;
end;

procedure TUndoImportTimeSeries.ForceRedraw;
begin
  frmGoPhast.PhastModel.TimeSeries.Invalidate;
  frmGoPhast.frame3DView.glWidModelView.Invalidate;

  frmGoPhast.frameTopView.ModelChanged := True;
  frmGoPhast.frameTopView.ZoomBox.Image32.Invalidate;
  frmGoPhast.frameFrontView.ModelChanged := True;
  frmGoPhast.frameFrontView.ZoomBox.Image32.Invalidate;
  frmGoPhast.frameSideView.ModelChanged := True;
  frmGoPhast.frameSideView.ZoomBox.Image32.Invalidate;
end;

procedure TUndoImportTimeSeries.Undo;
begin
  frmGoPhast.PhastModel.TimeSeries := FExistingTimeSeries;
  EnableMenuItems;
  ForceRedraw;
  inherited;

end;

procedure TUndoImportTimeSeries.EnableMenuItems;
begin
  frmGoPhast.miTimeSeriestoShapefile.Enabled :=
    frmGoPhast.PhastModel.TimeSeries.Series.Count > 0;
end;

{ TframeModpathTimeSeriesDisplay }

procedure TframeModpathTimeSeriesDisplay.AssignTimesToComboBox(
  Times: TRealList);
var
  Index: Integer;
begin
  comboTimeToPlot.Items.Clear;
  comboTimeToPlot.Items.Capacity := Times.Count;
  for Index := 0 to Times.Count - 1 do
  begin
    comboTimeToPlot.Items.Add(FloatToStr(Times[Index]));
  end;
  if Times.Count > 0 then
  begin
    udTimeToPlot.Max := Times.Count - 1;
    udTimeToPlot.Min := -udTimeToPlot.Max;
  end;

  if (comboTimeToPlot.ItemIndex < 0) and (comboTimeToPlot.Items.Count > 0) then
  begin
    comboTimeToPlot.ItemIndex := 0;
  end;
end;

procedure TframeModpathTimeSeriesDisplay.comboColorSchemeChange(
  Sender: TObject);
begin
  pbColorScheme.Invalidate;
end;

procedure TframeModpathTimeSeriesDisplay.comboTimeToPlotChange(Sender: TObject);
begin
  udTimeToPlot.Position := 0;
end;

procedure TframeModpathTimeSeriesDisplay.fedModpathFileBeforeDialog(
  Sender: TObject; var AName: string; var AAction: Boolean);
begin
  if AName = '' then
  begin
    if frmGoPhast.sdModpathInput.FileName <> '' then
    begin
      AName := ChangeFileExt(frmGoPhast.sdModpathInput.FileName,
        fedModpathFile.DefaultExt);
    end
    else if frmGoPhast.sdModflowInput.FileName <> '' then
    begin
      AName := ChangeFileExt(frmGoPhast.sdModflowInput.FileName,
        fedModpathFile.DefaultExt);
    end
    else if frmGoPhast.sdSaveDialog.FileName <> '' then
    begin
      AName := ChangeFileExt(frmGoPhast.sdSaveDialog.FileName,
        fedModpathFile.DefaultExt);
    end;
  end;
end;

procedure TframeModpathTimeSeriesDisplay.fedModpathFileChange(Sender: TObject);
var
  TimeSeries: TTimeSeriesReader;
begin
  if FileExists(fedModpathFile.FileName) then
  begin
    TimeSeries := TTimeSeriesReader.Create;
    try
      TimeSeries.FileName := fedModpathFile.FileName;
      TimeSeries.ReadFile;
      AssignTimesToComboBox(TimeSeries.Times);
    finally
      TimeSeries.Free;
    end;
  end;
end;

procedure TframeModpathTimeSeriesDisplay.GetData;
var
  TimeSeries : TTimeSeriesReader;
  Times: TRealList;
  DisplayLimits: TTimeSeriesDisplayLimits;
  ColorLimits: TTimeSeriesColorLimits;
  ALimitRow: TTimeSeriesLimits;
  ARow: Integer;
  ColorParameters: TColorParameters;
begin
  if frmGoPhast.PhastModel.ModflowPackages.ModPath.Binary then
  begin
    fedModpathFile.DefaultExt := '.ts_bin';
  end
  else
  begin
    fedModpathFile.DefaultExt := '.ts';
  end;
  TimeSeries := frmGoPhast.PhastModel.TimeSeries;
  fedModpathFile.FileName := TimeSeries.FileName;
  Times := TimeSeries.Times;
  AssignTimesToComboBox(Times);
  udTimeToPlot.Position := TimeSeries.TimeIndex;
  comboTimeToPlot.ItemIndex := TimeSeries.TimeIndex;
  cbShowPathlines.Checked := TimeSeries.Visible;

  DisplayLimits := TimeSeries.DisplayLimits;
  cbLimitToCurrentIn2D.Checked := DisplayLimits.LimitToCurrentIn2D;
  rgShow2D.ItemIndex := Ord(DisplayLimits.ShowChoice);

  ColorLimits := TimeSeries.ColorLimits;

  rgColorBy.ItemIndex := Ord(ColorLimits.ColoringChoice);

  ReadIntLimit(DisplayLimits.ColumnLimits, tslColumn);
  ReadIntLimit(DisplayLimits.RowLimits, tslRow);
  ReadIntLimit(DisplayLimits.LayerLimits, tslLayer);

  ALimitRow := tslColors;

  ARow := Ord(ALimitRow);
  rdgLimits.Checked[0, ARow] := ColorLimits.UseLimit;
  if ColorLimits.UseLimit then
  begin
    rdgLimits.Cells[1, ARow] := FloatToStr(ColorLimits.MinColorLimit);
    rdgLimits.Cells[2, ARow] := FloatToStr(ColorLimits.MaxColorLimit);
  end;

  ColorParameters := TimeSeries.ColorParameters;
  comboColorScheme.ItemIndex := ColorParameters.ColorScheme;
  seCycles.AsInteger := ColorParameters.ColorCycles;
  seColorExponent.Value := ColorParameters.ColorExponent;
  jsColorExponent.Value := Round(ColorParameters.ColorExponent*100);

end;

procedure TframeModpathTimeSeriesDisplay.jsColorExponentChanged(
  Sender: TObject);
begin
  if Sender <> seColorExponent then
  begin
    seColorExponent.Value := jsColorExponent.Value / 100
  end;
  pbColorScheme.Invalidate;
end;

procedure TframeModpathTimeSeriesDisplay.Loaded;
var
  Index: TTimeSeriesLimits;
begin
  inherited;
  pcMain.ActivePageIndex := 0;
  rdgLimits.BeginUpdate;
  try
    rdgLimits.RowCount := Succ(Ord(High(TTimeSeriesLimits)));
    for Index := Low(TTimeSeriesLimits) to High(TTimeSeriesLimits) do
    begin
      rdgLimits.Cells[0,Ord(Index)] := TableCaptions[Index];
    end;
    rdgLimits.Cells[0,0] := StrLimitingFactor;
    rdgLimits.Cells[1,0] := StrLowerLimit;
    rdgLimits.Cells[2,0] := StrUpperLimit;
    Index := tslColors;
    rdgLimits.UseSpecialFormat[1,Ord(Index)] := True;
    rdgLimits.UseSpecialFormat[2,Ord(Index)] := True;
    rdgLimits.SpecialFormat[1,Ord(Index)] := rcf4Real;
    rdgLimits.SpecialFormat[2,Ord(Index)] := rcf4Real;
  finally
    rdgLimits.EndUpdate;
  end;
end;

procedure TframeModpathTimeSeriesDisplay.pbColorSchemePaint(Sender: TObject);
var
  X: integer;
  Fraction: Real;
  AColor: TColor;
  ColorAdjustmentFactor: Real;
begin
  for X := 0 to pbColorScheme.Width - 1 do
  begin
    Fraction := 1 - X / pbColorScheme.Width;
    ColorAdjustmentFactor := seColorExponent.Value;

    AColor := FracAndSchemeToColor(comboColorScheme.ItemIndex,
      Fraction, ColorAdjustmentFactor, seCycles.AsInteger);

    with pbColorScheme.Canvas do
    begin
      Pen.Color := AColor;
      MoveTo(X, 0);
      LineTo(X, pbColorScheme.Height - 1);
    end;
  end;
end;

procedure TframeModpathTimeSeriesDisplay.rdgLimitsSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ARow >= rdgLimits.FixedRows) then
  begin
    if ARow = Ord(tslColors) then
    begin
      CanSelect := rgColorBy.ItemIndex <> 0;
    end
    else
    begin
      CanSelect := rgShow2D.ItemIndex <> 0;
    end;
    if CanSelect then
    begin
      case ACol of
        0:
          begin
            // do nothing.
          end;
        1,2:
          begin
            CanSelect := rdgLimits.Checked[0,ARow];
          end;
        else Assert(False);
      end;
    end;
  end;

end;

procedure TframeModpathTimeSeriesDisplay.rdgLimitsSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  if (ARow in [Ord(tslLayer)..Ord(tslColumn)]) and (ACol in [1,2]) then
  begin
    rdgLimits.Columns[ACol].CheckACell(ACol, ARow, False, True, 0, 1);
  end;
end;

procedure TframeModpathTimeSeriesDisplay.rdgLimitsStateChange(Sender: TObject;
  ACol, ARow: Integer; const Value: TCheckBoxState);
begin
  rdgLimits.Invalidate;
end;

procedure TframeModpathTimeSeriesDisplay.ReadIntLimit(
  IntLimits: TShowIntegerLimit; ALimitRow: TTimeSeriesLimits);
var
  ARow: Integer;
begin
  ARow := Ord(ALimitRow);
  rdgLimits.Checked[0, ARow] := IntLimits.UseLimit;
  if IntLimits.UseLimit then
  begin
    rdgLimits.Cells[1, ARow] := IntToStr(IntLimits.StartLimit);
    rdgLimits.Cells[2, ARow] := IntToStr(IntLimits.EndLimit);
  end;
end;

procedure TframeModpathTimeSeriesDisplay.rgColorByClick(Sender: TObject);
begin
  rdgLimits.Invalidate;
end;

procedure TframeModpathTimeSeriesDisplay.rgShow2DClick(Sender: TObject);
begin
  rdgLimits.Invalidate;
end;

procedure TframeModpathTimeSeriesDisplay.seColorExponentChange(Sender: TObject);
begin
  jsColorExponent.Value := Round(seColorExponent.Value * 100);
  pbColorScheme.Invalidate;
end;

procedure TframeModpathTimeSeriesDisplay.seCyclesChange(Sender: TObject);
begin
  pbColorScheme.Invalidate;
end;

procedure TframeModpathTimeSeriesDisplay.SetData;
var
  ImportedNewFile: Boolean;
  Grid: TModflowGrid;
  ExistingTimeSeries: TTimeSeriesReader;
  TimeSeries: TTimeSeriesReader;
  ADate: TDateTime;
  Limits: TTimeSeriesDisplayLimits;
  ColorLimits: TTimeSeriesColorLimits;
  ARow: Integer;
  ColorParameters: TColorParameters;
  Undo: TUndoImportTimeSeries;
begin
  ImportedNewFile := False;
  Grid := frmGoPhast.ModflowGrid;
  ExistingTimeSeries := frmGoPhast.PhastModel.TimeSeries;
  TimeSeries := TTimeSeriesReader.Create;
  try
    TimeSeries.Assign(ExistingTimeSeries);

    TimeSeries.FileName := fedModpathFile.FileName;
    if TimeSeries.FileName = '' then
    begin
      TimeSeries.Series.Clear;
    end
    else
    begin
      if FileExists(TimeSeries.FileName) then
      begin
        if(TimeSeries.FileName <> ExistingTimeSeries.FileName) then
        begin
          TimeSeries.ReadFile;
          ImportedNewFile := True;
        end
        else
        begin
          if FileAge(TimeSeries.FileName, ADate)
            and (TimeSeries.FileDate <> ADate) then
          begin
            if (MessageDlg(StrTheTimeSeriesFile,
              mtInformation, [mbYes, mbNo], 0) = mrYes) then
            begin
              TimeSeries.ReadFile;
              ImportedNewFile := True;
            end;
          end;
        end;
      end;
      TimeSeries.TimeIndex := comboTimeToPlot.ItemIndex;
      TimeSeries.Visible := cbShowPathlines.Checked;

      Limits := TimeSeries.DisplayLimits;
      Limits.LimitToCurrentIn2D := cbLimitToCurrentIn2D.Checked;
      Limits.ShowChoice := TShowChoice(rgShow2D.ItemIndex);

      if Limits.ShowChoice <> scAll then
      begin
        SetIntLimit(tslColumn, Grid.ColumnCount, Limits.ColumnLimits);
        SetIntLimit(tslRow, Grid.RowCount, Limits.RowLimits);
        SetIntLimit(tslLayer, Grid.LayerCount, Limits.LayerLimits);
      end;

      ColorLimits := TimeSeries.ColorLimits;
      ColorLimits.ColoringChoice :=
        TTimeSeriesColorLimitChoice(rgColorBy.ItemIndex);

      if ColorLimits.ColoringChoice <> tscNone then
      begin
        ARow := Ord(tslColors);
        ColorLimits.UseLimit := rdgLimits.Checked[0, ARow];
        if ColorLimits.UseLimit then
        begin
          ColorLimits.MinColorLimit := StrToFloatDef(rdgLimits.Cells[1, ARow], 0);
          ColorLimits.MaxColorLimit := StrToFloatDef(rdgLimits.Cells[2, ARow], 1);
        end;
      end;

      ColorParameters := TimeSeries.ColorParameters;
      ColorParameters.ColorScheme := comboColorScheme.ItemIndex;
      ColorParameters.ColorCycles := seCycles.AsInteger;
      ColorParameters.ColorExponent := seColorExponent.Value;
    end;

    Undo := TUndoImportTimeSeries.Create(TimeSeries, ImportedNewFile);
    frmGoPhast.UndoStack.Submit(Undo);
  finally
    TimeSeries.Free
  end;

end;

procedure TframeModpathTimeSeriesDisplay.SetIntLimit(
  LimitRow: TTimeSeriesLimits; DefaultLimit: integer;
  IntLimit: TShowIntegerLimit);
var
  ARow: Integer;
begin
  ARow := Ord(LimitRow);
  IntLimit.UseLimit := rdgLimits.Checked[0, ARow];
  if IntLimit.UseLimit then
  begin
    IntLimit.StartLimit := StrToIntDef(rdgLimits.Cells[1, ARow], 1);
    IntLimit.EndLimit := StrToIntDef(rdgLimits.Cells[2, ARow], DefaultLimit);
  end;
end;

procedure TframeModpathTimeSeriesDisplay.udTimeToPlotChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  case Direction of
    updNone: ;
    updUp:
      begin
        if comboTimeToPlot.ItemIndex < comboTimeToPlot.Items.Count -1 then
        begin
          comboTimeToPlot.ItemIndex := comboTimeToPlot.ItemIndex+1;
          SetData;
        end;
      end;
    updDown:
      begin
        if comboTimeToPlot.ItemIndex > 0 then
        begin
          comboTimeToPlot.ItemIndex := comboTimeToPlot.ItemIndex-1;
          SetData;
        end;
      end;
  end;
  udTimeToPlot.ControlStyle := udTimeToPlot.ControlStyle - [csCaptureMouse];
end;

end.
