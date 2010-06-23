unit frmModflowOptionsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, JvExStdCtrls, JvCheckBox, JvCombobox,
  ExtCtrls, ArgusDataEntry, ComCtrls, Buttons, ModflowOptionsUnit, UndoItems,
  JvListComb, Mask, JvExMask, JvSpin, JvExControls, JvComponent, JvXPCore,
  JvXPCheckCtrls, RbwController, RequiredDataSetsUndoUnit;

type
  TfrmModflowOptions = class(TfrmCustomGoPhast)
    pcOptions: TPageControl;
    TabSheet1: TTabSheet;
    Label3: TLabel;
    edProjectName: TEdit;
    Label4: TLabel;
    Label2: TLabel;
    edDate: TEdit;
    edModeler: TEdit;
    Label1: TLabel;
    memoComments: TMemo;
    TabSheet2: TTabSheet;
    GroupBox1: TGroupBox;
    cbPRINTTIME: TJvCheckBox;
    cbCHTOCH: TJvCheckBox;
    Label5: TLabel;
    rdeHNOFLO: TRbwDataEntry;
    Panel1: TPanel;
    Label6: TLabel;
    comboTimeUnit: TJvComboBox;
    Label7: TLabel;
    comboLengthUnit: TJvComboBox;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Label8: TLabel;
    rdeHDRY: TRbwDataEntry;
    tabWetting: TTabSheet;
    lblWetFact: TLabel;
    rdeWettingFact: TRbwDataEntry;
    lblCheckDry: TLabel;
    seCheckDry: TJvSpinEdit;
    lblWettingEquation: TLabel;
    comboWettingEquation: TJvImageComboBox;
    rconWet: TRbwController;
    cbWetting: TCheckBox;
    cbOpenInTextEditor: TJvCheckBox;
    lblWettingDataSets: TLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure cbCHTOCHClick(Sender: TObject);
    procedure cbPRINTTIMEClick(Sender: TObject);
    procedure rdeHNOFLOExit(Sender: TObject);
    procedure comboTimeUnitChange(Sender: TObject);
    procedure comboLengthUnitChange(Sender: TObject);
    procedure edProjectNameExit(Sender: TObject);
    procedure edDateExit(Sender: TObject);
    procedure edModelerExit(Sender: TObject);
    procedure memoCommentsExit(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure rdeHDRYExit(Sender: TObject);
    procedure cbWettingClick(Sender: TObject);
    procedure rdeWettingFactChange(Sender: TObject);
    procedure seCheckDryChange(Sender: TObject);
    procedure comboWettingEquationChange(Sender: TObject);
    procedure pcOptionsChange(Sender: TObject);
    procedure cbOpenInTextEditorClick(Sender: TObject);
  private
    FModflowOptions: TModflowOptions;
    FWettingOptions: TWettingOptions;
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoGeneralOptions = class(TCustomCreateRequiredDataSetsUndo)
  private
    FNewModflowOptions: TModflowOptions;
    FOldModflowOptions: TModflowOptions;
    FNewWettingOptions: TWettingOptions;
    FOldWettingOptions: TWettingOptions;
  protected
    function Description: string; override;
  public
    Constructor Create(var NewModflowOptions: TModflowOptions;
      var NewWettinOptions: TWettingOptions);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

var
  frmModflowOptions: TfrmModflowOptions;

implementation

uses frmGoPhastUnit, frmErrorsAndWarningsUnit, 
  LayerStructureUnit, PhastModelUnit;

{$R *.dfm}

procedure TfrmModflowOptions.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmModflowOptions.cbCHTOCHClick(Sender: TObject);
begin
  inherited;
  if FModflowOptions = nil then Exit;
  FModflowOptions.ComputeFluxesBetweenConstantHeadCells := cbCHTOCH.Checked;
end;

procedure TfrmModflowOptions.cbPRINTTIMEClick(Sender: TObject);
begin
  inherited;
  if FModflowOptions = nil then Exit;
  FModflowOptions.PrintTime := cbPRINTTIME.Checked;
end;

procedure TfrmModflowOptions.FormCreate(Sender: TObject);
begin
  inherited;
  pcOptions.ActivePageIndex := 0;
  FModflowOptions:= TModflowOptions.Create(nil);
  FWettingOptions:= TWettingOptions.Create(nil);
  GetData;
end;

procedure TfrmModflowOptions.FormDestroy(Sender: TObject);
begin
  inherited;
  FModflowOptions.Free;
  FWettingOptions.Free;
end;

procedure TfrmModflowOptions.GetData;
begin
  FModflowOptions.Assign(frmGoPhast.PhastModel.ModflowOptions);
  edProjectName.Text := FModflowOptions.ProjectName;
  edDate.Text := FModflowOptions.ProjectDate;
  edModeler.Text := FModflowOptions.Modeler;
  memoComments.Lines.Assign(FModflowOptions.Description);
  cbCHTOCH.Checked := FModflowOptions.ComputeFluxesBetweenConstantHeadCells;
  cbPRINTTIME.Checked := FModflowOptions.PrintTime;
  rdeHDRY.Text := FloatToStr(FModflowOptions.HDry);
  rdeHNOFLO.Text := FloatToStr(FModflowOptions.HNoFlow);
  comboTimeUnit.ItemIndex := FModflowOptions.TimeUnit;
  comboLengthUnit.ItemIndex := FModflowOptions.LengthUnit;
  cbOpenInTextEditor.Checked := FModflowOptions.OpenInTextEditor;

  FWettingOptions.Assign(frmGoPhast.PhastModel.ModflowWettingOptions);
  cbWetting.Checked := FWettingOptions.WettingActive;
  cbWettingClick(nil);
  rdeWettingFact.Text := FloatToStr(FWettingOptions.WettingFactor);
  seCheckDry.AsInteger := FWettingOptions.WettingIterations;
  comboWettingEquation.ItemIndex := FWettingOptions.WettingEquation;
end;

procedure TfrmModflowOptions.cbOpenInTextEditorClick(Sender: TObject);
begin
  inherited;
  if FModflowOptions = nil then Exit;
  FModflowOptions.OpenInTextEditor := cbOpenInTextEditor.Checked;
end;

procedure TfrmModflowOptions.cbWettingClick(Sender: TObject);
begin
  inherited;
  rconWet.Enabled := cbWetting.Checked;
  lblWettingDataSets.Visible := cbWetting.Checked;
  if FWettingOptions = nil then Exit;
  FWettingOptions.WettingActive := cbWetting.Checked;
end;

procedure TfrmModflowOptions.memoCommentsExit(Sender: TObject);
begin
  inherited;
  if FModflowOptions = nil then Exit;
  FModflowOptions.Description := memoComments.Lines;
end;

procedure TfrmModflowOptions.pcOptionsChange(Sender: TObject);
begin
  inherited;
  HelpKeyWord := pcOptions.ActivePage.HelpKeyword;
end;

procedure TfrmModflowOptions.comboLengthUnitChange(Sender: TObject);
begin
  inherited;
  if FModflowOptions = nil then Exit;
  FModflowOptions.LengthUnit := comboLengthUnit.ItemIndex;
end;

procedure TfrmModflowOptions.comboTimeUnitChange(Sender: TObject);
begin
  inherited;
  if FModflowOptions = nil then Exit;
  FModflowOptions.TimeUnit := comboTimeUnit.ItemIndex;
end;

procedure TfrmModflowOptions.comboWettingEquationChange(Sender: TObject);
begin
  inherited;
  if FWettingOptions = nil then Exit;
  FWettingOptions.WettingEquation := comboWettingEquation.ItemIndex;
end;

procedure TfrmModflowOptions.edDateExit(Sender: TObject);
begin
  inherited;
  if FModflowOptions = nil then Exit;
  FModflowOptions.ProjectDate := edDate.Text;
end;

procedure TfrmModflowOptions.edModelerExit(Sender: TObject);
begin
  inherited;
  if FModflowOptions = nil then Exit;
  FModflowOptions.Modeler := edModeler.Text;
end;

procedure TfrmModflowOptions.edProjectNameExit(Sender: TObject);
begin
  inherited;
  if FModflowOptions = nil then Exit;
  FModflowOptions.ProjectName := edProjectName.Text;
end;

procedure TfrmModflowOptions.rdeHDRYExit(Sender: TObject);
var
  Value: Extended;
begin
  inherited;
  if FModflowOptions = nil then Exit;
  if TryStrToFloat(rdeHDry.Text, Value) then
  begin
    FModflowOptions.HDry := Value;
  end;

end;

procedure TfrmModflowOptions.rdeHNOFLOExit(Sender: TObject);
var
  Value: Extended;
begin
  inherited;
  if FModflowOptions = nil then Exit;
  if TryStrToFloat(rdeHNOFLO.Text, Value) then
  begin
    FModflowOptions.HNoFlow := Value;
  end;
end;

procedure TfrmModflowOptions.rdeWettingFactChange(Sender: TObject);
var
  Value: extended;
begin
  inherited;
  if FWettingOptions = nil then Exit;
  if TryStrToFloat(rdeWettingFact.Text, Value) then
  begin
    FWettingOptions.WettingFactor := Value;
  end;

end;

procedure TfrmModflowOptions.seCheckDryChange(Sender: TObject);
begin
  inherited;
  if FWettingOptions = nil then Exit;
  FWettingOptions.WettingIterations := seCheckDry.AsInteger;
end;

procedure TfrmModflowOptions.SetData;
var
  Undo: TUndoGeneralOptions;
begin
  Undo:= TUndoGeneralOptions.Create(FModflowOptions, FWettingOptions);
  frmGoPhast.UndoStack.Submit(Undo);
end;

{ TUndoGeneralOptions }

constructor TUndoGeneralOptions.Create(var NewModflowOptions: TModflowOptions;
  var NewWettinOptions: TWettingOptions);
begin
  inherited Create;
  FNewModflowOptions := NewModflowOptions;
  NewModflowOptions := nil;
  FOldModflowOptions := TModflowOptions.Create(nil);
  FOldModflowOptions.Assign(frmGoPhast.PhastModel.ModflowOptions);

  FNewWettingOptions := NewWettinOptions;
  NewWettinOptions := nil;

  FOldWettingOptions := TWettingOptions.Create(nil);
  FOldWettingOptions.Assign(frmGoPhast.PhastModel.ModflowWettingOptions);
end;

function TUndoGeneralOptions.Description: string;
begin
  result := 'general options'
end;

destructor TUndoGeneralOptions.Destroy;
begin
  FNewModflowOptions.Free;
  FOldModflowOptions.Free;
  FNewWettingOptions.Free;
  FOldWettingOptions.Free;
  inherited;
end;

procedure TUndoGeneralOptions.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.ModflowOptions.Assign(FNewModflowOptions);
  frmGoPhast.PhastModel.ModflowWettingOptions.Assign(FNewWettingOptions);
  UpdatedRequiredDataSets;
end;

procedure TUndoGeneralOptions.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.ModflowOptions.Assign(FOldModflowOptions);
  frmGoPhast.PhastModel.ModflowWettingOptions.Assign(FOldWettingOptions);
  UpdatedRequiredDataSets;
end;

end.
