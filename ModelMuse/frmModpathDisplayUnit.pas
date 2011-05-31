unit frmModpathDisplayUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Mask, JvExMask, JvToolEdit,
  ArgusDataEntry, ExtCtrls, Buttons, JvExControls, JvxSlider, JvSpin, UndoItems,
  PathlineReader, ComCtrls, Grids, RbwDataGrid4;

type
  TPathlineLimits = (plNone, plColors, plLayer, plRow, plColumn, plTime);

const
  TableCaptions: array[Low(TPathlineLimits)..High(TPathlineLimits)] of string =
    ('', 'Color limits', 'Layer', 'Row', 'Column', 'Times');

type    
  TfrmModpathDisplay = class(TfrmCustomGoPhast)
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
    rgColorBy: TRadioGroup;
    lblColorScheme: TLabel;
    comboColorScheme: TComboBox;
    pbColorScheme: TPaintBox;
    lblColorAdjustment: TLabel;
    jsColorExponent: TJvxSlider;
    seColorExponent: TJvSpinEdit;
    seCycles: TJvSpinEdit;
    lblCycles: TLabel;
    rdgLimits: TRbwDataGrid4;
    lblMaxTime: TLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure pbColorSchemePaint(Sender: TObject);
    procedure rgColorByClick(Sender: TObject);
    procedure comboColorSchemeChange(Sender: TObject);
    procedure jsColorExponentChange(Sender: TObject);
    procedure seColorExponentChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure rgShow2DClick(Sender: TObject);
    procedure rdgLimitsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgLimitsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgLimitsStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure fedModpathFileBeforeDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
  private
    procedure GetData;
    procedure SetData;
    procedure ReadIntLimit(IntLimits: TShowIntegerLimit;
      ALimitRow: TPathlineLimits);
    procedure ReadFloatLimits(FloatLimits: TShowFloatLimit;
      ALimitRow: TPathlineLimits);
    procedure SetIntLimit(LimitRow: TPathlineLimits; DefaultLimit: integer;
      IntLimit: TShowIntegerLimit);
    procedure SetFloatLimit(LimitRow: TPathlineLimits;
      MinLimit, MaxLimit: Double; FloatLimit: TShowFloatLimit);
  public
    { Public declarations }
  end;

  TUndoImportPathline = class(TCustomUndo)
  private
    FExistingPathLines: TPathLineReader;
    FNewPathLines: TPathLineReader;
    FImportedNewFile: Boolean;
    procedure ForceRedraw;
    procedure EnableMenuItems;
  public
    Constructor Create(var NewPathLine: TPathLineReader;
      ImportedNewFile: boolean);
    Destructor Destroy; override;
    function Description: string; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

var
  frmModpathDisplay: TfrmModpathDisplay;

implementation

uses
  frmGoPhastUnit, ColorSchemes, ModelMuseUtilities, ModflowGridUnit;

{$R *.dfm}

procedure TfrmModpathDisplay.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmModpathDisplay.comboColorSchemeChange(Sender: TObject);
begin
  inherited;
  pbColorScheme.Invalidate;
end;

procedure TfrmModpathDisplay.fedModpathFileBeforeDialog(Sender: TObject;
  var AName: string; var AAction: Boolean);
begin
  inherited;
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

procedure TfrmModpathDisplay.FormCreate(Sender: TObject);
var
  Index: TPathlineLimits;
begin
  inherited;

  rdgLimits.BeginUpdate;
  try
    rdgLimits.RowCount := Succ(Ord(High(TPathlineLimits)));
    for Index := Low(TPathlineLimits) to High(TPathlineLimits) do
    begin
      rdgLimits.Cells[0,Ord(Index)] := TableCaptions[Index];
    end;
    rdgLimits.Cells[0,0] := 'Limiting factor';
    rdgLimits.Cells[1,0] := 'Lower limit';
    rdgLimits.Cells[2,0] := 'Upper limit';

    Index := plColors;
    rdgLimits.UseSpecialFormat[1,Ord(Index)] := True;
    rdgLimits.UseSpecialFormat[2,Ord(Index)] := True;
    rdgLimits.SpecialFormat[1,Ord(Index)] := rcf4Real;
    rdgLimits.SpecialFormat[2,Ord(Index)] := rcf4Real;

    Index := plTime;
    rdgLimits.UseSpecialFormat[1,Ord(Index)] := True;
    rdgLimits.UseSpecialFormat[2,Ord(Index)] := True;
    rdgLimits.SpecialFormat[1,Ord(Index)] := rcf4Real;
    rdgLimits.SpecialFormat[2,Ord(Index)] := rcf4Real;
  finally
    rdgLimits.EndUpdate;
  end;

  pcMain.ActivePageIndex := 0;

  GetData;
end;

procedure TfrmModpathDisplay.GetData;
var
  PathLines: TPathLineReader;
  Limits: TPathLineDisplayLimits;
  ColorParameters: TColorParameters;
  ALimitRow: TPathlineLimits;
  ARow: Integer;
  MaxTime: Double;
begin
  if frmGoPhast.PhastModel.ModflowPackages.ModPath.Binary then
  begin
    fedModpathFile.DefaultExt := '.path_bin';
  end
  else
  begin
    fedModpathFile.DefaultExt := '.path';
  end;
  PathLines := frmGoPhast.PhastModel.PathLines;
  fedModpathFile.FileName := PathLines.FileName;
  if PathLines.Lines.TestGetMaxTime(MaxTime) then
  begin
    lblMaxTime.Caption := 'Maximum time = '
      + FloatToStrF(MaxTime, ffGeneral, 7, 0);
  end
  else
  begin
    lblMaxTime.Caption := 'Maximum time = ?';
  end;

  cbShowPathlines.Checked := PathLines.Visible;
  Limits := PathLines.DisplayLimits;

  cbLimitToCurrentIn2D.Checked := Limits.LimitToCurrentIn2D;
  rgShow2D.ItemIndex := Ord(Limits.ShowChoice);

  ReadIntLimit(Limits.ColumnLimits, plColumn);
  ReadIntLimit(Limits.RowLimits, plRow);
  ReadIntLimit(Limits.LayerLimits, plLayer);

  ReadFloatLimits(Limits.TimeLimits, plTime);

  rgColorBy.ItemIndex := Ord(PathLines.ColorLimits.ColoringChoice);

  ALimitRow := plColors;
  ARow := Ord(ALimitRow);
  rdgLimits.Checked[0, ARow] := PathLines.ColorLimits.UseLimit;
  if PathLines.ColorLimits.UseLimit then
  begin
    rdgLimits.Cells[1, ARow] := FloatToStr(PathLines.ColorLimits.MinColorLimit);
    rdgLimits.Cells[2, ARow] := FloatToStr(PathLines.ColorLimits.MaxColorLimit);
  end;

  ColorParameters := PathLines.ColorParameters;
  comboColorScheme.ItemIndex := ColorParameters.ColorScheme;
  seCycles.AsInteger := ColorParameters.ColorCycles;
  seColorExponent.Value := ColorParameters.ColorExponent;
  jsColorExponent.Value := Round(ColorParameters.ColorExponent*100);
end;

procedure TfrmModpathDisplay.jsColorExponentChange(Sender: TObject);
begin
  inherited;
  if Sender <> seColorExponent then
  begin
    seColorExponent.Value := jsColorExponent.Value / 100
  end;
  pbColorScheme.Invalidate;
end;

procedure TfrmModpathDisplay.pbColorSchemePaint(Sender: TObject);
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

procedure TfrmModpathDisplay.rdgLimitsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ARow >= rdgLimits.FixedRows) then
  begin
    if ARow = Ord(plColors) then
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

procedure TfrmModpathDisplay.rdgLimitsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  if (ARow in [Ord(plLayer)..Ord(plColumn)]) and (ACol in [1,2]) then
  begin
    rdgLimits.Columns[ACol].CheckACell(ACol, ARow, False, True, 0, 1);
  end;
end;

procedure TfrmModpathDisplay.rdgLimitsStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  inherited;
  rdgLimits.Invalidate;
end;

procedure TfrmModpathDisplay.ReadFloatLimits(FloatLimits: TShowFloatLimit;
  ALimitRow: TPathlineLimits);
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

procedure TfrmModpathDisplay.ReadIntLimit(IntLimits: TShowIntegerLimit;
  ALimitRow: TPathlineLimits);
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

procedure TfrmModpathDisplay.rgColorByClick(Sender: TObject);
begin
  inherited;
  rdgLimits.Invalidate;
end;

procedure TfrmModpathDisplay.rgShow2DClick(Sender: TObject);
begin
  inherited;
  rdgLimits.Invalidate;
end;

procedure TfrmModpathDisplay.seColorExponentChange(Sender: TObject);
begin
  inherited;
  jsColorExponent.Value := Round(seColorExponent.Value * 100);
  pbColorScheme.Invalidate
end;

procedure TfrmModpathDisplay.SetData;
var
  PathLine: TPathLineReader;
  Limits: TPathLineDisplayLimits;
  ColorParameters: TColorParameters;
  Grid: TModflowGrid;
  ExistingPathLines: TPathLineReader;
  ADate: TDateTime;
  Undo: TUndoImportPathline;
  ColorLimits: TPathlineColorLimits;
  ImportedNewFile: Boolean;
  ARow: Integer;
begin
  inherited;
  ImportedNewFile := False;
  Grid := frmGoPhast.ModflowGrid;
  ExistingPathLines := frmGoPhast.PhastModel.PathLines;
  PathLine := TPathLineReader.Create;
  try
    PathLine.Assign(ExistingPathLines);

    PathLine.FileName := fedModpathFile.FileName;
    if PathLine.FileName = '' then
    begin
      PathLine.Lines.Clear;
    end
    else
    begin
      if FileExists(PathLine.FileName) then
      begin
        if(PathLine.FileName <> ExistingPathLines.FileName) then
        begin
          PathLine.ReadFile;
          ImportedNewFile := True;
        end
        else
        begin
          if FileAge(PathLine.FileName, ADate)
            and (PathLine.FileDate <> ADate) then
          begin
            if (MessageDlg('The pathline file on disk has a different date '
              + 'than the file that was imported into ModelMuse.  Do you want '
              + 'to import the new file?',
              mtInformation, [mbYes, mbNo], 0) = mrYes) then
            begin
              PathLine.ReadFile;
              ImportedNewFile := True;
            end;
          end;
        end;
      end;
      PathLine.Visible := cbShowPathlines.Checked;

      Limits := PathLine.DisplayLimits;

      Limits.LimitToCurrentIn2D := cbLimitToCurrentIn2D.Checked;
      Limits.ShowChoice := TShowChoice(rgShow2D.ItemIndex);

      if Limits.ShowChoice <> scAll then
      begin
        SetIntLimit(plColumn, Grid.ColumnCount, Limits.ColumnLimits);
        SetIntLimit(plRow, Grid.RowCount, Limits.RowLimits);
        SetIntLimit(plLayer, Grid.LayerCount, Limits.LayerLimits);

        SetFloatLimit(plTime, PathLine.MinTime, PathLine.MaxTime,
          Limits.TimeLimits);
      end;

      ColorLimits := PathLine.ColorLimits;
      ColorLimits.ColoringChoice :=
        TColorLimitChoice(rgColorBy.ItemIndex);

      if ColorLimits.ColoringChoice <> clcNone then
      begin
        ARow := Ord(plColors);
        ColorLimits.UseLimit := rdgLimits.Checked[0, ARow];
        if ColorLimits.UseLimit then
        begin
          ColorLimits.MinColorLimit := StrToFloatDef(rdgLimits.Cells[1, ARow], 0);
          ColorLimits.MaxColorLimit := StrToFloatDef(rdgLimits.Cells[2, ARow], 1);
        end;

      end;

      ColorParameters := PathLine.ColorParameters;
      ColorParameters.ColorScheme := comboColorScheme.ItemIndex;
      ColorParameters.ColorCycles := seCycles.AsInteger;
      ColorParameters.ColorExponent := seColorExponent.Value;
    end;

    Undo := TUndoImportPathline.Create(PathLine, ImportedNewFile);
    frmGoPhast.UndoStack.Submit(Undo);
  finally
    PathLine.Free;
  end;
end;

procedure TfrmModpathDisplay.SetFloatLimit(LimitRow: TPathlineLimits;
  MinLimit, MaxLimit: Double; FloatLimit: TShowFloatLimit);
var
  ARow: Integer;
begin
  ARow := Ord(LimitRow);
  FloatLimit.UseLimit := rdgLimits.Checked[0, ARow];
  if FloatLimit.UseLimit then
  begin
    FloatLimit.StartLimit := StrToFloatDef(rdgLimits.Cells[1, ARow], MinLimit);
    FloatLimit.EndLimit := StrToFloatDef(rdgLimits.Cells[2, ARow], MaxLimit);
  end;
end;

procedure TfrmModpathDisplay.SetIntLimit(LimitRow: TPathlineLimits;
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

{ TUndoImportPathline }

constructor TUndoImportPathline.Create(var NewPathLine: TPathLineReader;
  ImportedNewFile: boolean);
begin
  FImportedNewFile := ImportedNewFile;
  FExistingPathLines:= TPathLineReader.Create;
  FExistingPathLines.Assign(frmGoPhast.PhastModel.PathLines);
  // Take ownership of NewPathLine.
  FNewPathLines := NewPathLine;
  NewPathLine := nil;
end;

function TUndoImportPathline.Description: string;
begin
  if FImportedNewFile then
  begin
    result := 'import pathline';
  end
  else
  begin
    result := 'configure pathline';
  end;
end;

destructor TUndoImportPathline.Destroy;
begin
  FExistingPathLines.Free;
  FNewPathLines.Free;
  inherited;
end;

procedure TUndoImportPathline.DoCommand;
begin
  frmGoPhast.PhastModel.PathLines := FNewPathLines;
  EnableMenuItems;
  ForceRedraw;
end;

procedure TUndoImportPathline.Undo;
begin
  frmGoPhast.PhastModel.PathLines := FExistingPathLines;
  EnableMenuItems;
  ForceRedraw;
end;

procedure TUndoImportPathline.EnableMenuItems;
begin
  frmGoPhast.miConfigurePathlines.Enabled :=
    frmGoPhast.PhastModel.PathLines.Lines.Count > 0;
  frmGoPhast.miPathlinestoShapefile.Enabled :=
    frmGoPhast.miConfigurePathlines.Enabled;
end;

procedure TUndoImportPathline.ForceRedraw;
begin
  frmGoPhast.PhastModel.PathLines.Invalidate;
  frmGoPhast.frame3DView.glWidModelView.Invalidate;

  frmGoPhast.frameTopView.ModelChanged := True;
  frmGoPhast.frameTopView.ZoomBox.Image32.Invalidate;
  frmGoPhast.frameFrontView.ModelChanged := True;
  frmGoPhast.frameFrontView.ZoomBox.Image32.Invalidate;
  frmGoPhast.frameSideView.ModelChanged := True;
  frmGoPhast.frameSideView.ZoomBox.Image32.Invalidate;
end;

end.
