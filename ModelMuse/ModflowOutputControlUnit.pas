unit ModflowOutputControlUnit;

interface

uses SysUtils, Classes, GoPhastTypes;

type
  TCellSaveFormat = (csfNone, csfBinary, csfListing);
  TFrequencyChoice = (fcTimeSteps, fcStressPeriods);
  TOutputFileType = (oftText, oftBinary);
  TWrapping = (wStrip, wWrap);

  TExtFormatPrefix = (efpNone, efp1P);
  TNumberFormat = (nfF, nfD, nfE, nfEN, nfEs, nfG);

  TPrintFormat = (nf11G_10_3, nf9G_13_6, nf15F_7_1, nf15F_7_2, nf15F_7_3,
    nf15F_7_4, nf20F_5_0, nf20F_5_1, nf20F_5_2, nf20F_5_3, nf20F_5_4,
    nf10G_11_4, nf10F_6_0, nf10F_6_1, nf10F_6_2, nf10F_6_3, nf10F_6_4);

  TExternalFormat = class(TGoPhastPersistent)
  private
    FNumberFormat: TNumberFormat;
    FExtFormatPrefix: TExtFormatPrefix;
    FWidth: integer;
    FDecimals: integer;
    procedure SetDecimals(const Value: integer);
    procedure SetExtFormatPrefix(const Value: TExtFormatPrefix);
    procedure SetNumberFormat(const Value: TNumberFormat);
    procedure SetWidth(const Value: integer);
    procedure Initialize;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TObject);
    function FullFormat: string;
  published
    property ExtFormatPrefix: TExtFormatPrefix read FExtFormatPrefix
      write SetExtFormatPrefix;
    property NumberFormat: TNumberFormat read FNumberFormat
      write SetNumberFormat;
    property Width: integer read FWidth write SetWidth;
    property Decimals: integer read FDecimals write SetDecimals;
  end;

  THeadDrawdownOutputControl = class(TGoPhastPersistent)
  private
    FWrapping: TWrapping;
    FSaveInExternalFile: boolean;
    FPrintInListing: boolean;
    FPrintFormat: TPrintFormat;
    FExternalFormat: TExternalFormat;
    FOutputFileType: TOutputFileType;
    FFrequency: integer;
    FFrequencyChoice: TFrequencyChoice;
    procedure SetExternalFormat(const Value: TExternalFormat);
    procedure SetFrequency(const Value: integer);
    procedure SetFrequencyChoice(const Value: TFrequencyChoice);
    procedure SetOutputFileType(const Value: TOutputFileType);
    procedure SetPrintFormat(const Value: TPrintFormat);
    procedure SetPrintInListing(const Value: boolean);
    procedure SetSaveInExternalFile(const Value: boolean);
    procedure SetWrapping(const Value: TWrapping);
    procedure Initialize;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    function PrintCode: integer;
    function FormatDefined: boolean;
  published
    property ExternalFormat: TExternalFormat read FExternalFormat
      write SetExternalFormat;
    property Frequency: integer read FFrequency write SetFrequency;
    property FrequencyChoice: TFrequencyChoice read FFrequencyChoice
      write SetFrequencyChoice;
    property OutputFileType: TOutputFileType read FOutputFileType
      write SetOutputFileType;
    property PrintFormat: TPrintFormat read FPrintFormat write SetPrintFormat;
    property PrintInListing: boolean read FPrintInListing
      write SetPrintInListing;
    property SaveInExternalFile: boolean read FSaveInExternalFile
      write SetSaveInExternalFile;
    property Wrapping: TWrapping read FWrapping write SetWrapping;
  end;

  TModflowOutputControl = class(TGoPhastPersistent)
  private
    FPrintInputArrays: boolean;
    FSaveCellFlows: TCellSaveFormat;
    FPrintInputCellLists: boolean;
    FCompact: boolean;
    FDrawdownOC: THeadDrawdownOutputControl;
    FHeadOC: THeadDrawdownOutputControl;
    FComments: TStrings;
    FBudgetFrequencyChoice: TFrequencyChoice;
    FBudgetFrequency: integer;
    procedure SetPrintInputArrays(const Value: boolean);
    procedure SetSaveCellFlows(const Value: TCellSaveFormat);
    procedure SetPrintInputCellLists(const Value: boolean);
    procedure SetCompact(const Value: boolean);
    procedure SetDrawdownOC(const Value: THeadDrawdownOutputControl);
    procedure SetHeadOC(const Value: THeadDrawdownOutputControl);
    procedure SetComments(const Value: TStrings);
    procedure SetBudgetFrequency(const Value: integer);
    procedure SetBudgetFrequencyChoice(const Value: TFrequencyChoice);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TComponent);
    Destructor Destroy; override;
    procedure Initialize;
  published
    property PrintInputArrays: boolean read FPrintInputArrays
      write SetPrintInputArrays default True;
    property PrintInputCellLists: boolean read FPrintInputCellLists
      write SetPrintInputCellLists default True;
    property SaveCellFlows: TCellSaveFormat read FSaveCellFlows
      write SetSaveCellFlows default csfBinary;
    property Compact: boolean read FCompact write SetCompact;
    property HeadOC: THeadDrawdownOutputControl read FHeadOC write SetHeadOC;
    property DrawdownOC: THeadDrawdownOutputControl read FDrawdownOC
      write SetDrawdownOC;
    property Comments: TStrings read FComments write SetComments;
    property BudgetFrequency: integer read FBudgetFrequency write SetBudgetFrequency;
    property BudgetFrequencyChoice: TFrequencyChoice read FBudgetFrequencyChoice write SetBudgetFrequencyChoice;

  end;

implementation

uses PhastModelUnit;

{ TModflowOutputControl }

procedure TModflowOutputControl.Assign(Source: TPersistent);
var
  SourceOutputControl: TModflowOutputControl;
begin
  if Source is TModflowOutputControl then
  begin
    SourceOutputControl := TModflowOutputControl(Source);
    PrintInputArrays := SourceOutputControl.PrintInputArrays;
    SaveCellFlows := SourceOutputControl.SaveCellFlows;
    Compact := SourceOutputControl.Compact;
    HeadOC := SourceOutputControl.HeadOC;
    DrawdownOC := SourceOutputControl.DrawdownOC;
    Comments := SourceOutputControl.Comments;
    BudgetFrequency := SourceOutputControl.BudgetFrequency;
    BudgetFrequencyChoice := SourceOutputControl.BudgetFrequencyChoice;
  end
  else
  begin
    inherited;
  end;
end;

constructor TModflowOutputControl.Create(Model: TComponent);
begin
  inherited Create(Model);
  FHeadOC := THeadDrawdownOutputControl.Create(Model);
  FDrawdownOC := THeadDrawdownOutputControl.Create(Model);
  FComments := TStringList.Create;
  Initialize;
end;

destructor TModflowOutputControl.Destroy;
begin
  FComments.Free;
  FHeadOC.Free;
  FDrawdownOC.Free;
  inherited;
end;

procedure TModflowOutputControl.Initialize;
begin
  FBudgetFrequency := 1;
  FBudgetFrequencyChoice := fcTimeSteps;
  FPrintInputArrays := True;
  FPrintInputCellLists := True;
  FSaveCellFlows := csfBinary;
  FCompact := True;
  FComments.Clear;
  HeadOc.Initialize;
  DrawdownOC.Initialize;
end;

procedure TModflowOutputControl.SetBudgetFrequency(const Value: integer);
begin
  if FBudgetFrequency <> Value then
  begin
    Assert(Value >= 1);
    FBudgetFrequency := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOutputControl.SetBudgetFrequencyChoice(
  const Value: TFrequencyChoice);
begin
  if FBudgetFrequencyChoice <> Value then
  begin
    FBudgetFrequencyChoice := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOutputControl.SetComments(const Value: TStrings);
var
  Changed: boolean;
  LineIndex: Integer;
begin
  Changed := FComments.Count <> Value.Count;
  if not Changed then
  begin
    for LineIndex := 0 to FComments.Count - 1 do
    begin
      Changed := FComments[LineIndex] <> Value[LineIndex];
      if Changed then
      begin
        break;
      end;
    end;
  end;
  if Changed then
  begin
    InvalidateModel;
    FComments.Assign(Value);
  end;
end;

procedure TModflowOutputControl.SetCompact(const Value: boolean);
begin
  if FCompact <> Value then
  begin
    FCompact := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOutputControl.SetDrawdownOC(
  const Value: THeadDrawdownOutputControl);
begin
  FDrawdownOC.Assign(Value);
end;

procedure TModflowOutputControl.SetHeadOC(
  const Value: THeadDrawdownOutputControl);
begin
  FHeadOC.Assign(Value);
end;

procedure TModflowOutputControl.SetPrintInputArrays(const Value: boolean);
begin
  if FPrintInputArrays <> Value then
  begin
    FPrintInputArrays := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOutputControl.SetPrintInputCellLists(const Value: boolean);
begin
  if FPrintInputCellLists <> Value then
  begin
    FPrintInputCellLists := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOutputControl.SetSaveCellFlows(const Value: TCellSaveFormat);
begin
  if FSaveCellFlows <> Value then
  begin
    FSaveCellFlows := Value;
    InvalidateModel;
  end;
end;

{ TExternalFormat }

procedure TExternalFormat.Assign(Source: TPersistent);
var
  SourceFormat: TExternalFormat;
begin
  if Source is TExternalFormat then
  begin
    SourceFormat := TExternalFormat(Source);
    ExtFormatPrefix := SourceFormat.ExtFormatPrefix;
    NumberFormat := SourceFormat.NumberFormat;
    Width := SourceFormat.Width;
    Decimals := SourceFormat.Decimals;
  end
  else
  begin
    inherited;
  end;
end;

constructor TExternalFormat.Create(Model: TObject);
begin
  inherited Create(Model);
  Initialize;
end;

function TExternalFormat.FullFormat: string;
begin
  result := '(10(1X';
  case ExtFormatPrefix of
    efpNone: ;
    efp1P: result := result + '1P';
    else Assert(False);
  end;
  case NumberFormat of
    nfF: result := result + 'F';
    nfD: result := result + 'D';
    nfE: result := result + 'E';
    nfEN: result := result + 'EN';
    nfEs: result := result + 'Es';
    nfG: result := result + 'G';
    else Assert(False);
  end;
  result := result + IntToStr(Width) + '.' + IntToStr(Decimals) + '))';
end;

procedure TExternalFormat.Initialize;
begin
  FNumberFormat := nfE;
  FExtFormatPrefix := efp1P;
  FWidth := 13;
  FDecimals := 5;
end;

procedure TExternalFormat.SetDecimals(const Value: integer);
begin
  if FDecimals <> Value then
  begin
    Assert(Value >= 1);
    FDecimals := Value;
    InvalidateModel;
  end;
end;

procedure TExternalFormat.SetExtFormatPrefix(const Value: TExtFormatPrefix);
begin
  if FExtFormatPrefix <> Value then
  begin
    FExtFormatPrefix := Value;
    InvalidateModel;
  end;
end;

procedure TExternalFormat.SetNumberFormat(const Value: TNumberFormat);
begin
  if FNumberFormat <> Value then
  begin
    FNumberFormat := Value;
    InvalidateModel;
  end;
end;

procedure TExternalFormat.SetWidth(const Value: integer);
begin
  if FWidth <> Value then
  begin
    Assert(Value >= 1);
    FWidth := Value;
    InvalidateModel;
  end;
end;

{ THeadDrawdownOutputControl }

procedure THeadDrawdownOutputControl.Assign(Source: TPersistent);
var
  SourceHD: THeadDrawdownOutputControl;
begin
  if Source is THeadDrawdownOutputControl then
  begin
    SourceHD := THeadDrawdownOutputControl(Source);
    ExternalFormat := SourceHD.ExternalFormat;
    Frequency := SourceHD.Frequency;
    FrequencyChoice := SourceHD.FrequencyChoice;
    OutputFileType := SourceHD.OutputFileType;
    PrintFormat := SourceHD.PrintFormat;
    PrintInListing := SourceHD.PrintInListing;
    SaveInExternalFile := SourceHD.SaveInExternalFile;
    Wrapping := SourceHD.Wrapping;
  end
  else
  begin
    inherited;
  end;
end;

constructor THeadDrawdownOutputControl.Create(Model: TObject);
begin
  inherited Create(Model);
  FExternalFormat := TExternalFormat.Create(Model);
  Initialize;
end;

destructor THeadDrawdownOutputControl.Destroy;
begin
  FExternalFormat.Free;
  inherited;
end;

function THeadDrawdownOutputControl.FormatDefined: boolean;
begin
  result := SaveInExternalFile and (OutputFileType = oftText);
end;

function THeadDrawdownOutputControl.PrintCode: integer;
begin
  result := Ord(PrintFormat)+1;
  if Wrapping = wStrip then
  begin
    result := -result;
  end;
end;

procedure THeadDrawdownOutputControl.Initialize;
begin
  FWrapping := wStrip;
  FSaveInExternalFile := True;
  FPrintInListing := False;
  FPrintFormat := nf10G_11_4;
  FOutputFileType := oftText;
  FFrequency := 1;
  FFrequencyChoice := fcTimeSteps;
  ExternalFormat.Initialize;
end;

procedure THeadDrawdownOutputControl.SetExternalFormat(
  const Value: TExternalFormat);
begin
  FExternalFormat.Assign(Value);
end;

procedure THeadDrawdownOutputControl.SetFrequency(const Value: integer);
begin
  if FFrequency <> Value then
  begin
    Assert(Value >= 1);
    FFrequency := Value;
    InvalidateModel;
  end;
end;

procedure THeadDrawdownOutputControl.SetFrequencyChoice(
  const Value: TFrequencyChoice);
begin
  if FFrequencyChoice <> Value then
  begin
    FFrequencyChoice := Value;
    InvalidateModel;
  end;
end;

procedure THeadDrawdownOutputControl.SetOutputFileType(
  const Value: TOutputFileType);
begin
  if FOutputFileType <> Value then
  begin
    FOutputFileType := Value;
    InvalidateModel;
  end;
end;

procedure THeadDrawdownOutputControl.SetPrintFormat(const Value: TPrintFormat);
begin
  if FPrintFormat <> Value then
  begin
    FPrintFormat := Value;
    InvalidateModel;
  end;
end;

procedure THeadDrawdownOutputControl.SetPrintInListing(const Value: boolean);
begin
  if FPrintInListing <> Value then
  begin
    FPrintInListing := Value;
    InvalidateModel;
  end;
end;

procedure THeadDrawdownOutputControl.SetSaveInExternalFile(
  const Value: boolean);
begin
  if FSaveInExternalFile <> Value then
  begin
    FSaveInExternalFile := Value;
    InvalidateModel;
  end;
end;

procedure THeadDrawdownOutputControl.SetWrapping(const Value: TWrapping);
begin
  if FWrapping <> Value then
  begin
    FWrapping := Value;
    InvalidateModel;
  end;
end;

end.
