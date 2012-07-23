unit frameModpathEndpointDisplayUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, Grids, RbwDataGrid4, StdCtrls, ExtCtrls, JvSpin, JvExControls,
  JvxSlider, Mask, JvExMask, JvToolEdit, ComCtrls, frameModpathDisplayUnit,
  PathlineReader, UndoItems, PhastModelUnit;

type
  TEndLimits = (elNone, elColors, elStartLayer, elStartRow, elStartColumn, elStartZone,
    elEndLayer, elEndRow, elEndColumn, elEndZone,
    elTrackingTime, elReleaseTime);

resourcestring
  StartingLayer = 'Starting layer';
  StartingRow = 'Starting row';
  StartingColumn = 'Starting column';
  StartingZone = 'Starting zone';
  EndingLayer = 'Ending layer';
  EndingRow = 'Ending row';
  EndingColumn = 'Ending column';
  EndingZone = 'Ending zone';
  TrackingTime = 'Tracking time';
  ReleaseTime = 'Release time';

const
  TableCaptions: array[Low(TEndLimits)..High(TEndLimits)] of string =
    ('', Colorlimits,
    StartingLayer, StartingRow, StartingColumn, StartingZone,
    EndingLayer, EndingRow, EndingColumn, EndingZone,
    TrackingTime, ReleaseTime);

type
  TUndoImportEndpoints = class(TCustomUndo)
  private
    FExistingEndPoints: TEndPointReader;
    FNewEndPoints: TEndPointReader;
    FImportedNewFile: Boolean;
    FModel: TCustomModel;
    procedure ForceRedraw;
    procedure EnableMenuItems;
  public
    Constructor Create(Model: TCustomModel; var NewEndPoints: TEndPointReader; ImportedNewFile: boolean);
    Destructor Destroy; override;
    function Description: string; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TframeModpathEndpointDisplay = class(TFrame)
    pcMain: TPageControl;
    tabBasic: TTabSheet;
    lblModpathFile: TLabel;
    lblColorScheme: TLabel;
    pbColorScheme: TPaintBox;
    lblColorAdjustment: TLabel;
    lblCycles: TLabel;
    fedModpathFile: TJvFilenameEdit;
    cbShowPathlines: TCheckBox;
    cbLimitToCurrentIn2D: TCheckBox;
    comboColorScheme: TComboBox;
    jsColorExponent: TJvxSlider;
    seColorExponent: TJvSpinEdit;
    seCycles: TJvSpinEdit;
    tabOptions: TTabSheet;
    rgShow2D: TRadioGroup;
    rgWhereToPlot: TRadioGroup;
    rgColorBy: TRadioGroup;
    rdgLimits: TRbwDataGrid4;
    comboModelSelection: TComboBox;
    lblModelSelection: TLabel;
    procedure rdgLimitsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgLimitsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgLimitsStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure rgShow2DClick(Sender: TObject);
    procedure rgColorByClick(Sender: TObject);
    procedure pbColorSchemePaint(Sender: TObject);
    procedure comboColorSchemeChange(Sender: TObject);
    procedure jsColorExponentChanged(Sender: TObject);
    procedure seColorExponentChange(Sender: TObject);
    procedure seCyclesChange(Sender: TObject);
    procedure fedModpathFileBeforeDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
    procedure comboModelSelectionChange(Sender: TObject);
  protected
    procedure Loaded; override;
  private
    FEndPointsList: TEndPointObjectList;
    procedure SetIntLimit(LimitRow: TEndLimits; DefaultLimit: integer;
      IntLimit: TShowIntegerLimit);
    procedure SetFloatLimit(LimitRow: TEndLimits; DefaultLimit: Double;
      FloatLimit: TShowFloatLimit);
    procedure ReadIntLimit(IntLimits: TShowIntegerLimit; ALimitRow: TEndLimits);
    procedure ReadFloatLimits(FloatLimits: TShowFloatLimit; ALimitRow: TEndLimits);
    { Private declarations }
  public
    procedure GetData;
    procedure SetData;
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  end;

implementation

uses
  frmGoPhastUnit, ColorSchemes, ModflowGridUnit, ModelMuseUtilities;

resourcestring
  StrLimitingFactor = 'Limiting factor';
  StrLowerLimit = 'Lower limit';
  StrUpperLimit = 'Upper limit';
  StrImportEndpoints = 'import endpoints';
  StrConfigureEndpoints = 'configure endpoints';
  StrTheEndpointFileOn = 'The endpoint file on disk has a different date tha' +
  'n the file that was imported into ModelMuse.  Do you want to import the n' +
  'ew file?';

{$R *.dfm}

{ TUndoImportEndpoints }

constructor TUndoImportEndpoints.Create(Model: TCustomModel; var NewEndPoints: TEndPointReader;
  ImportedNewFile: boolean);
begin
  FModel := Model;
  FImportedNewFile := ImportedNewFile;
  FExistingEndPoints:= TEndPointReader.Create(Model);
  FExistingEndPoints.Assign(frmGoPhast.PhastModel.EndPoints);
  // Take ownership of NewEndPoints.
  FNewEndPoints := NewEndPoints;
  NewEndPoints := nil;
end;

function TUndoImportEndpoints.Description: string;
begin
  if FImportedNewFile then
  begin
    result := StrImportEndpoints;
  end
  else
  begin
    result := StrConfigureEndpoints;
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
  FModel.EndPoints := FNewEndPoints;
  EnableMenuItems;
  ForceRedraw;
end;

procedure TUndoImportEndpoints.ForceRedraw;
begin
  FModel.EndPoints.Invalidate;
  frmGoPhast.frame3DView.glWidModelView.Invalidate;

  frmGoPhast.frameTopView.ModelChanged := True;
//  frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
  frmGoPhast.frameFrontView.ModelChanged := True;
//  frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
  frmGoPhast.frameSideView.ModelChanged := True;
//  frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
  frmGoPhast.InvalidateImage32AllViews;
end;

procedure TUndoImportEndpoints.Undo;
begin
  FModel.EndPoints := FExistingEndPoints;
  EnableMenuItems;
  ForceRedraw;
end;

procedure TUndoImportEndpoints.EnableMenuItems;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  frmGoPhast.miEndpointsatStartingLocationstoShapefile.Enabled :=
    frmGoPhast.PhastModel.EndPoints.Points.Count > 0;
  if not frmGoPhast.miEndpointsatStartingLocationstoShapefile.Enabled
    and frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      frmGoPhast.miEndpointsatStartingLocationstoShapefile.Enabled :=
        ChildModel.EndPoints.Points.Count > 0;
      if frmGoPhast.miEndpointsatStartingLocationstoShapefile.Enabled then
      begin
        break;
      end;
    end;
  end;
  frmGoPhast.miEndpointsatEndingLocationstoShapefile.Enabled :=
    frmGoPhast.miEndpointsatStartingLocationstoShapefile.Enabled;
end;

{ TframeModpathEndpointDisplay }

procedure TframeModpathEndpointDisplay.comboColorSchemeChange(Sender: TObject);
begin
  pbColorScheme.Invalidate;
end;

procedure TframeModpathEndpointDisplay.comboModelSelectionChange(
  Sender: TObject);
var
  EndPoints: TEndPointReader;
  Limits: TEndPointDisplayLimits;
  ColorParameters: TColorParameters;
  ALimitRow: TEndLimits;
  ARow: Integer;
  LocalModel: TCustomModel;
begin
  LocalModel := comboModelSelection.Items.Objects[
    comboModelSelection.ItemIndex] as TCustomModel;
  if LocalModel.ModflowPackages.ModPath.Binary then
  begin
    fedModpathFile.DefaultExt := '.end_bin';
  end
  else
  begin
    fedModpathFile.DefaultExt := '.end';
  end;
  EndPoints := FEndPointsList[comboModelSelection.ItemIndex];
  fedModpathFile.FileName := EndPoints.FileName;

  cbShowPathlines.Checked := EndPoints.Visible;
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

constructor TframeModpathEndpointDisplay.Create(Owner: TComponent);
begin
  inherited;
  FEndPointsList := TEndPointObjectList.Create;
end;

destructor TframeModpathEndpointDisplay.Destroy;
begin
  FEndPointsList.Free;
  inherited;
end;

procedure TframeModpathEndpointDisplay.fedModpathFileBeforeDialog(
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

procedure TframeModpathEndpointDisplay.GetData;
var
  EndPoints: TEndPointReader;
  LocalEndPoints: TEndPointReader;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  Handle;
  if frmGoPhast.PhastModel.ModflowPackages.ModPath.Binary then
  begin
    fedModpathFile.DefaultExt := '.end_bin';
  end
  else
  begin
    fedModpathFile.DefaultExt := '.end';
  end;
  EndPoints := frmGoPhast.PhastModel.EndPoints;

  FEndPointsList.Clear;
  comboModelSelection.Items.Clear;
  LocalEndPoints := TEndPointReader.Create(frmGoPhast.PhastModel);
  FEndPointsList.Add(LocalEndPoints);
  LocalEndPoints.Assign(EndPoints);
  comboModelSelection.Items.AddObject(frmGoPhast.PhastModel.DisplayName, frmGoPhast.PhastModel);
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    comboModelSelection.Visible := True;
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      comboModelSelection.Items.AddObject(ChildModel.DisplayName, ChildModel);
      LocalEndPoints := TEndPointReader.Create(ChildModel);
      FEndPointsList.Add(LocalEndPoints);
      EndPoints := ChildModel.EndPoints;
      LocalEndPoints.Assign(EndPoints);
    end;
  end
  else
  begin
    comboModelSelection.Visible := False;
  end;
  lblModelSelection.Visible := comboModelSelection.Visible;
  comboModelSelection.ItemIndex := 0;
  comboModelSelectionChange(nil);

end;

procedure TframeModpathEndpointDisplay.jsColorExponentChanged(Sender: TObject);
begin
  if Sender <> seColorExponent then
  begin
    seColorExponent.Value := jsColorExponent.Value / 100
  end;
  pbColorScheme.Invalidate;
end;

procedure TframeModpathEndpointDisplay.Loaded;
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
    rdgLimits.Cells[0,0] := StrLimitingFactor;
    rdgLimits.Cells[1,0] := StrLowerLimit;
    rdgLimits.Cells[2,0] := StrUpperLimit;
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
end;

procedure TframeModpathEndpointDisplay.pbColorSchemePaint(Sender: TObject);
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

procedure TframeModpathEndpointDisplay.rdgLimitsSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
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

procedure TframeModpathEndpointDisplay.rdgLimitsSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  if (ARow in [Ord(elStartLayer)..Ord(elEndZone)]) and (ACol in [1,2]) then
  begin
    rdgLimits.Columns[ACol].CheckACell(ACol, ARow, False, True, 0, 1);
  end;
end;

procedure TframeModpathEndpointDisplay.rdgLimitsStateChange(Sender: TObject;
  ACol, ARow: Integer; const Value: TCheckBoxState);
begin
  rdgLimits.Invalidate;
end;

procedure TframeModpathEndpointDisplay.ReadFloatLimits(
  FloatLimits: TShowFloatLimit; ALimitRow: TEndLimits);
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

procedure TframeModpathEndpointDisplay.ReadIntLimit(
  IntLimits: TShowIntegerLimit; ALimitRow: TEndLimits);
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

procedure TframeModpathEndpointDisplay.rgColorByClick(Sender: TObject);
begin
  rdgLimits.Invalidate;
end;

procedure TframeModpathEndpointDisplay.rgShow2DClick(Sender: TObject);
begin
  rdgLimits.Invalidate;
end;

procedure TframeModpathEndpointDisplay.seColorExponentChange(Sender: TObject);
begin
  jsColorExponent.Value := Round(seColorExponent.Value * 100);
  pbColorScheme.Invalidate;
end;

procedure TframeModpathEndpointDisplay.seCyclesChange(Sender: TObject);
begin
  pbColorScheme.Invalidate;
end;

procedure TframeModpathEndpointDisplay.SetData;
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
  LocalModel: TCustomModel;
begin
  inherited;
  LocalModel := comboModelSelection.Items.Objects[
    comboModelSelection.ItemIndex] as TCustomModel;
  ImportedNewFile := False;
  Grid := LocalModel.ModflowGrid;
  ExistingEndPoints := LocalModel.EndPoints;
  EndPoints := TEndPointReader.Create(LocalModel);
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
            if (MessageDlg(StrTheEndpointFileOn,
              mtInformation, [mbYes, mbNo], 0) = mrYes) then
            begin
              EndPoints.ReadFile;
              ImportedNewFile := True;
            end;
          end;
        end;
      end;
      EndPoints.Visible := cbShowPathlines.Checked;

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

    Undo := TUndoImportEndPoints.Create(LocalModel, EndPoints, ImportedNewFile);
    frmGoPhast.UndoStack.Submit(Undo);
  finally
    EndPoints.Free;
  end;
end;

procedure TframeModpathEndpointDisplay.SetFloatLimit(LimitRow: TEndLimits;
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

procedure TframeModpathEndpointDisplay.SetIntLimit(LimitRow: TEndLimits;
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

end.
