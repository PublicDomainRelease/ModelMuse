unit frmEndPointDisplayUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, Grids, RbwDataGrid4, JvExControls, JvxSlider,
  JvSpin, StdCtrls, Mask, JvExMask, JvToolEdit, ArgusDataEntry, ExtCtrls,
  Buttons, UndoItems, PathlineReader, ComCtrls;

type
  TEndLimits = (elNone, elColors, elStartLayer, elStartRow, elStartColumn, elStartZone,
    elEndLayer, elEndRow, elEndColumn, elEndZone,
    elTrackingTime, elReleaseTime);

const
  TableCaptions: array[Low(TEndLimits)..High(TEndLimits)] of string =
    ('', 'Color limits',
    'Starting layer', 'Starting row', 'Starting column', 'Starting zone',
    'Ending layer', 'Ending row', 'Ending column', 'Ending zone',
    'Tracking time', 'Release time');

type    
  TfrmEndPointDisplay = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pcMain: TPageControl;
    tabBasic: TTabSheet;
    tabOptions: TTabSheet;
    lblModpathFile: TLabel;
    fedModpathFile: TJvFilenameEdit;
    cbShowPathlines: TCheckBox;
    cbLimitToCurrentIn2D: TCheckBox;
    rgShow2D: TRadioGroup;
    rgWhereToPlot: TRadioGroup;
    rgColorBy: TRadioGroup;
    rdgLimits: TRbwDataGrid4;
    lblColorScheme: TLabel;
    comboColorScheme: TComboBox;
    pbColorScheme: TPaintBox;
    lblColorAdjustment: TLabel;
    jsColorExponent: TJvxSlider;
    seColorExponent: TJvSpinEdit;
    lblCycles: TLabel;
    seCycles: TJvSpinEdit;
    procedure FormCreate(Sender: TObject); override;
    procedure rdgLimitsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgLimitsStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure rgShow2DClick(Sender: TObject);
    procedure rgColorByClick(Sender: TObject);
    procedure pbColorSchemePaint(Sender: TObject);
    procedure comboColorSchemeChange(Sender: TObject);
    procedure jsColorExponentChanged(Sender: TObject);
    procedure seColorExponentChange(Sender: TObject);
    procedure seCyclesChange(Sender: TObject);
    procedure rdgLimitsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure btnOKClick(Sender: TObject);
  private
    procedure GetData;
    procedure SetData;
    procedure SetIntLimit(LimitRow: TEndLimits; DefaultLimit: integer;
      IntLimit: TShowIntegerLimit);
    procedure SetFloatLimit(LimitRow: TEndLimits; DefaultLimit: Double;
      FloatLimit: TShowFloatLimit);
    procedure ReadIntLimit(IntLimits: TShowIntegerLimit; ALimitRow: TEndLimits);
    procedure ReadFloatLimits(FloatLimits: TShowFloatLimit; ALimitRow: TEndLimits);
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoImportEndpoints = class(TCustomUndo)
  private
    FExistingEndPoints: TEndPointReader;
    FNewEndPoints: TEndPointReader;
    FImportedNewFile: Boolean;
    procedure ForceRedraw;
  public
    Constructor Create(var NewEndPoints: TEndPointReader; ImportedNewFile: boolean);
    Destructor Destroy; override;
    function Description: string; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

var
  frmEndPointDisplay: TfrmEndPointDisplay;

implementation

uses
  ModelMuseUtilities, ColorSchemes, frmGoPhastUnit, ModflowGridUnit;

{$R *.dfm}

procedure TfrmEndPointDisplay.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmEndPointDisplay.comboColorSchemeChange(Sender: TObject);
begin
  inherited;
  pbColorScheme.Invalidate;
end;

procedure TfrmEndPointDisplay.FormCreate(Sender: TObject);
var
  Index: TEndLimits;
begin
  inherited;
  pcMain.ActivePageIndex := 0;
  rdgLimits.BeginUpdate;
  try
    rdgLimits.RowCount := Succ(Ord(High(TEndLimits)));
    for Index := Low(TEndLimits) to High(TEndLimits) do
    begin
      rdgLimits.Cells[0,Ord(Index)] := TableCaptions[Index];
    end;
    rdgLimits.Cells[0,0] := 'Limiting factor';
    rdgLimits.Cells[1,0] := 'Lower limit';
    rdgLimits.Cells[2,0] := 'Upper limit';
    for Index := elTrackingTime to elReleaseTime do
    begin
      rdgLimits.UseSpecialFormat[1,Ord(Index)] := True;
      rdgLimits.UseSpecialFormat[2,Ord(Index)] := True;
      rdgLimits.SpecialFormat[1,Ord(Index)] := rcf4Real;
      rdgLimits.SpecialFormat[2,Ord(Index)] := rcf4Real;
    end;
    Index := elColors;
    rdgLimits.UseSpecialFormat[1,Ord(Index)] := True;
    rdgLimits.UseSpecialFormat[2,Ord(Index)] := True;
    rdgLimits.SpecialFormat[1,Ord(Index)] := rcf4Real;
    rdgLimits.SpecialFormat[2,Ord(Index)] := rcf4Real;
  finally
    rdgLimits.EndUpdate;
  end;

  GetData;
end;

procedure TfrmEndPointDisplay.GetData;
var
  EndPoints: TEndPointReader;
  Limits: TEndPointDisplayLimits;
  ColorParameters: TColorParameters;
  ALimitRow: TEndLimits;
  ARow: Integer;
begin
  EndPoints := frmGoPhast.PhastModel.EndPoints;
  fedModpathFile.FileName := EndPoints.FileName;

  cbShowPathlines.Checked := EndPoints.DisplayEndPoints;
  Limits := EndPoints.DisplayLimits;

  cbLimitToCurrentIn2D.Checked := Limits.LimitToCurrentIn2D;
  rgShow2D.ItemIndex := Ord(Limits.ShowChoice);
  rgWhereToPlot.ItemIndex := Ord(Limits.WhereToPlot);

  ReadIntLimit(Limits.StartColumnLimits, elStartColumn);
  ReadIntLimit(Limits.StartRowLimits, elStartRow);
  ReadIntLimit(Limits.StartLayerLimits, elStartLayer);
  ReadIntLimit(Limits.StartZoneLimits, elStartZone);

  ReadIntLimit(Limits.EndColumnLimits, elEndColumn);
  ReadIntLimit(Limits.EndRowLimits, elEndRow);
  ReadIntLimit(Limits.EndLayerLimits, elEndLayer);
  ReadIntLimit(Limits.EndZoneLimits, elEndZone);

  ReadFloatLimits(Limits.ReleaseTimeLimits, elReleaseTime);
  ReadFloatLimits(Limits.TrackingTimeLimits, elTrackingTime);

  rgColorBy.ItemIndex := Ord(EndPoints.ColorLimits.ColoringChoice);

  ALimitRow := elColors;
  ARow := Ord(ALimitRow);
  rdgLimits.Checked[0, ARow] := EndPoints.ColorLimits.UseLimit;
  if EndPoints.ColorLimits.UseLimit then
  begin
    rdgLimits.Cells[1, ARow] := FloatToStr(EndPoints.ColorLimits.MinColorLimit);
    rdgLimits.Cells[2, ARow] := FloatToStr(EndPoints.ColorLimits.MaxColorLimit);
  end;

  ColorParameters := EndPoints.ColorParameters;
  comboColorScheme.ItemIndex := ColorParameters.ColorScheme;
  seCycles.AsInteger := ColorParameters.ColorCycles;
  seColorExponent.Value := ColorParameters.ColorExponent;
  jsColorExponent.Value := Round(ColorParameters.ColorExponent*100);
end;

procedure TfrmEndPointDisplay.jsColorExponentChanged(Sender: TObject);
begin
  inherited;
  if Sender <> seColorExponent then
  begin
    seColorExponent.Value := jsColorExponent.Value / 100
  end;
  pbColorScheme.Invalidate;
end;

procedure TfrmEndPointDisplay.pbColorSchemePaint(Sender: TObject);
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

procedure TfrmEndPointDisplay.rdgLimitsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ARow >= rdgLimits.FixedRows) then
  begin
    if ARow = Ord(elColors) then
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

procedure TfrmEndPointDisplay.rdgLimitsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  if (ARow in [Ord(elStartLayer)..Ord(elEndZone)]) and (ACol in [1,2]) then
  begin
    rdgLimits.Columns[ACol].CheckACell(ACol, ARow, False, True, 0, 1);
  end;
end;

procedure TfrmEndPointDisplay.rdgLimitsStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  inherited;
  rdgLimits.Invalidate;
end;

procedure TfrmEndPointDisplay.rgColorByClick(Sender: TObject);
begin
  inherited;
  rdgLimits.Invalidate;
end;

procedure TfrmEndPointDisplay.rgShow2DClick(Sender: TObject);
begin
  inherited;
  rdgLimits.Invalidate;
end;

procedure TfrmEndPointDisplay.seColorExponentChange(Sender: TObject);
begin
  inherited;
  jsColorExponent.Value := Round(seColorExponent.Value * 100);
  pbColorScheme.Invalidate;
end;

procedure TfrmEndPointDisplay.seCyclesChange(Sender: TObject);
begin
  inherited;
  pbColorScheme.Invalidate;
end;

procedure TfrmEndPointDisplay.SetData;
var
  EndPoints: TEndPointReader;
  Limits: TEndPointDisplayLimits;
  ColorParameters: TColorParameters;
  Grid: TModflowGrid;
  ExistingEndPoints: TEndPointReader;
  ADate: TDateTime;
  Undo: TUndoImportEndPoints;
  ColorLimits: TEndPointColorLimits;
  ImportedNewFile: Boolean;
  ARow: Integer;
begin
  inherited;
  ImportedNewFile := False;
  Grid := frmGoPhast.ModflowGrid;
  ExistingEndPoints := frmGoPhast.PhastModel.EndPoints;
  EndPoints := TEndPointReader.Create;
  try
    EndPoints.Assign(ExistingEndPoints);

    EndPoints.FileName := fedModpathFile.FileName;
    if EndPoints.FileName = '' then
    begin
      EndPoints.Points.Clear;
    end
    else
    begin
      if FileExists(EndPoints.FileName) then
      begin
        if(EndPoints.FileName <> ExistingEndPoints.FileName) then
        begin
          EndPoints.ReadFile;
          ImportedNewFile := True;
        end
        else
        begin
          if FileAge(EndPoints.FileName, ADate)
            and (EndPoints.FileDate <> ADate) then
          begin
            if (MessageDlg('The endpoint file on disk has a different date '
              + 'than the file that was imported into ModelMuse.  Do you want '
              + 'to import the new file?',
              mtInformation, [mbYes, mbNo], 0) = mrYes) then
            begin
              EndPoints.ReadFile;
              ImportedNewFile := True;
            end;
          end;
        end;
      end;
      EndPoints.DisplayEndPoints := cbShowPathlines.Checked;

      Limits := EndPoints.DisplayLimits;

      Limits.LimitToCurrentIn2D := cbLimitToCurrentIn2D.Checked;
      Limits.ShowChoice := TEndpointShowChoice(rgShow2D.ItemIndex);
      Limits.WhereToPlot := TWhereToPlot(rgWhereToPlot.ItemIndex);

      if Limits.ShowChoice <> escAll then
      begin
        SetIntLimit(elStartColumn, Grid.ColumnCount, Limits.StartColumnLimits);
        SetIntLimit(elStartRow, Grid.RowCount, Limits.StartRowLimits);
        SetIntLimit(elStartLayer, Grid.LayerCount, Limits.StartLayerLimits);
        SetIntLimit(elStartZone, EndPoints.MaxStartZone, Limits.StartZoneLimits);

        SetIntLimit(elEndColumn, Grid.ColumnCount, Limits.EndColumnLimits);
        SetIntLimit(elEndRow, Grid.RowCount, Limits.EndRowLimits);
        SetIntLimit(elEndLayer, Grid.LayerCount, Limits.EndLayerLimits);
        SetIntLimit(elEndZone, EndPoints.MaxEndZone, Limits.EndZoneLimits);

        SetFloatLimit(elReleaseTime, EndPoints.MaxReleaseTime,
          Limits.ReleaseTimeLimits);
        SetFloatLimit(elTrackingTime, EndPoints.MaxTrackingTime,
          Limits.TrackingTimeLimits);
      end;

      ColorLimits := EndPoints.ColorLimits;
      ColorLimits.ColoringChoice :=
        TEndpointColorLimitChoice(rgColorBy.ItemIndex);

      if ColorLimits.ColoringChoice <> elcNone then
      begin
        ARow := Ord(elColors);
        ColorLimits.UseLimit := rdgLimits.Checked[0, ARow];
        if ColorLimits.UseLimit then
        begin
          ColorLimits.MinColorLimit := StrToFloatDef(rdgLimits.Cells[1, ARow], 0);
          ColorLimits.MaxColorLimit := StrToFloatDef(rdgLimits.Cells[2, ARow], 1);
        end;
      end;

      ColorParameters := EndPoints.ColorParameters;
      ColorParameters.ColorScheme := comboColorScheme.ItemIndex;
      ColorParameters.ColorCycles := seCycles.AsInteger;
      ColorParameters.ColorExponent := seColorExponent.Value;
    end;

    Undo := TUndoImportEndPoints.Create(EndPoints, ImportedNewFile);
    frmGoPhast.UndoStack.Submit(Undo);
  finally
    EndPoints.Free;
  end;
end;

procedure TfrmEndPointDisplay.SetIntLimit(LimitRow: TEndLimits;
  DefaultLimit: integer; IntLimit: TShowIntegerLimit);
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

procedure TfrmEndPointDisplay.SetFloatLimit(LimitRow: TEndLimits;
  DefaultLimit: Double; FloatLimit: TShowFloatLimit);
var
  ARow: Integer;
begin
  ARow := Ord(LimitRow);
  FloatLimit.UseLimit := rdgLimits.Checked[0, ARow];
  if FloatLimit.UseLimit then
  begin
    FloatLimit.StartLimit := StrToFloatDef(rdgLimits.Cells[1, ARow], 0);
    FloatLimit.EndLimit := StrToFloatDef(rdgLimits.Cells[2, ARow], DefaultLimit);
  end;
end;

procedure TfrmEndPointDisplay.ReadIntLimit(IntLimits: TShowIntegerLimit; ALimitRow: TEndLimits);
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

procedure TfrmEndPointDisplay.ReadFloatLimits(FloatLimits: TShowFloatLimit; ALimitRow: TEndLimits);
var
  ARow: Integer;
begin
  ARow := Ord(ALimitRow);
  rdgLimits.Checked[0, ARow] := FloatLimits.UseLimit;
  if FloatLimits.UseLimit then
  begin
    rdgLimits.Cells[1, ARow] := FloatToStr(FloatLimits.StartLimit);
    rdgLimits.Cells[2, ARow] := FloatToStr(FloatLimits.EndLimit);
  end;
end;

{ TUndoImportEndpoints }

constructor TUndoImportEndpoints.Create(var NewEndPoints: TEndPointReader;
  ImportedNewFile: boolean);
begin
  FImportedNewFile := ImportedNewFile;
  FExistingEndPoints:= TEndPointReader.Create;
  FExistingEndPoints.Assign(frmGoPhast.PhastModel.EndPoints);
  // Take ownership of NewEndPoints.
  FNewEndPoints := NewEndPoints;
  NewEndPoints := nil;
end;

function TUndoImportEndpoints.Description: string;
begin
  if FImportedNewFile then
  begin
    result := 'import endpoints';
  end
  else
  begin
    result := 'configure endpoints';
  end;
end;

destructor TUndoImportEndpoints.Destroy;
begin
  FExistingEndPoints.Free;
  FNewEndPoints.Free;
  inherited;
end;

procedure TUndoImportEndpoints.DoCommand;
begin
  frmGoPhast.PhastModel.EndPoints := FNewEndPoints;
  frmGoPhast.miConfigureEndpoints.Enabled :=
    frmGoPhast.PhastModel.EndPoints.Points.Count > 0;
  ForceRedraw;
end;

procedure TUndoImportEndpoints.ForceRedraw;
begin
  frmGoPhast.PhastModel.EndPoints.Invalidate;
  frmGoPhast.frame3DView.glWidModelView.Invalidate;

  frmGoPhast.frameTopView.ModelChanged := True;
  frmGoPhast.frameTopView.ZoomBox.Image32.Invalidate;
  frmGoPhast.frameFrontView.ModelChanged := True;
  frmGoPhast.frameFrontView.ZoomBox.Image32.Invalidate;
  frmGoPhast.frameSideView.ModelChanged := True;
  frmGoPhast.frameSideView.ZoomBox.Image32.Invalidate;
end;

procedure TUndoImportEndpoints.Undo;
begin
  frmGoPhast.PhastModel.EndPoints := FExistingEndPoints;
  frmGoPhast.miConfigureEndpoints.Enabled :=
    frmGoPhast.PhastModel.EndPoints.Points.Count > 0;
  ForceRedraw;
end;

end.
