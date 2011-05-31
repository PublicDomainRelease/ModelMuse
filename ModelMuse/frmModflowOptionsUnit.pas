unit frmModflowOptionsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, JvExStdCtrls, JvCheckBox, JvCombobox,
  ExtCtrls, ArgusDataEntry, ComCtrls, Buttons, ModflowOptionsUnit, UndoItems,
  JvListComb, Mask, JvExMask, JvSpin, JvExControls, JvComponent, JvXPCore,
  JvXPCheckCtrls, RbwController, RequiredDataSetsUndoUnit, JvToolEdit,
  PhastModelUnit;

type
  TModelOptions = class(TCollectionItem)
  private
    FDescription: TStrings;
    procedure SetDescription(const Value: TStrings);
  public
    Model: TCustomModel;
    CalculateFlow: boolean;
    PrintTime: boolean;
    InitialHeadsFile: string;
    WettingActive: boolean;
    WettingFactor: double;
    WettingIterations: integer;
    WettingEquation: integer;
    OpenInTextEditor: boolean;
    LengthUnit: integer;
    TimeUnit: integer;
    ProjectDate: string;
    Modeler: string;
    ProjectName: string;
    HDry: real;
    HNoFlow: real;
    property Description: TStrings read FDescription write SetDescription;
    procedure AssignOptionsToModel;
    procedure AssignModel(AModel: TCustomModel);
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  end;

  TModelOptionsCollection = class(TCollection)
  private
    function GetItems(Index: integer): TModelOptions;
    procedure SetItems(Index: integer; const Value: TModelOptions);
  public
    Constructor Create(Model: TPhastModel);
    function Add: TModelOptions;
    property Items[Index: integer]: TModelOptions read GetItems write SetItems;
    procedure AssignOptionsToModels;
  end;

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
    pnlBottom: TPanel;
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
    feInitialHeads: TJvFilenameEdit;
    lblInitialHeads: TLabel;
    pnlModel: TPanel;
    lblModel: TLabel;
    comboModel: TComboBox;
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
    procedure feInitialHeadsExit(Sender: TObject);
    procedure comboModelChange(Sender: TObject);
  private
    FCurrentOptions: TModelOptions;
//    FModflowOptions: TModflowOptions;
//    FWettingOptions: TWettingOptions;
    FModelOptionsCollection: TModelOptionsCollection;
    procedure SetCurrentOptions(const Value: TModelOptions);
    property CurrentOptions: TModelOptions read FCurrentOptions
      write SetCurrentOptions;
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoGeneralOptions = class(TCustomCreateRequiredDataSetsUndo)
  private
//    FNewModflowOptions: TModflowOptions;
//    FOldModflowOptions: TModflowOptions;
//    FNewWettingOptions: TWettingOptions;
//    FOldWettingOptions: TWettingOptions;
    FNewOptionsCollection: TModelOptionsCollection;
    FOldOptionsCollection: TModelOptionsCollection;
  protected
    function Description: string; override;
  public
    Constructor Create({var NewModflowOptions: TModflowOptions;}
//      var NewWettinOptions: TWettingOptions;
      var NewOptionsCollection: TModelOptionsCollection);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

var
  frmModflowOptions: TfrmModflowOptions;

implementation

uses frmGoPhastUnit, frmErrorsAndWarningsUnit, 
  LayerStructureUnit, TimeUnit;

{$R *.dfm}

procedure TfrmModflowOptions.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmModflowOptions.cbCHTOCHClick(Sender: TObject);
begin
  inherited;
//  if FModflowOptions = nil then Exit;
//  FModflowOptions.ComputeFluxesBetweenConstantHeadCells := cbCHTOCH.Checked;
end;

procedure TfrmModflowOptions.cbPRINTTIMEClick(Sender: TObject);
begin
  inherited;
//  if FModflowOptions = nil then Exit;
//  FModflowOptions.PrintTime := cbPRINTTIME.Checked;
end;

procedure TfrmModflowOptions.FormCreate(Sender: TObject);
//var
//  ItemIndex: Integer;
//  Model: TCustomModel;
//  Item: TModelOptions;
begin
  inherited;
  pcOptions.ActivePageIndex := 0;
//  FModflowOptions:= TModflowOptions.Create(nil);
//  FWettingOptions:= TWettingOptions.Create(nil);
  FModelOptionsCollection := TModelOptionsCollection.Create(frmGoPhast.PhastModel);
  GetData;
end;

procedure TfrmModflowOptions.FormDestroy(Sender: TObject);
begin
  inherited;
  FModelOptionsCollection.Free;
//  FModflowOptions.Free;
//  FWettingOptions.Free;
end;

procedure TfrmModflowOptions.GetData;
var
  ItemIndex: Integer;
  ModflowOptions: TModflowOptions;
  NewHeight: Integer;
begin
  FillComboWithModelNames(comboModel);
  Assert(comboModel.Items.Count = FModelOptionsCollection.Count);
  for ItemIndex := 0 to comboModel.Items.Count - 1 do
  begin
    comboModel.Items.Objects[ItemIndex] :=
      FModelOptionsCollection.Items[ItemIndex];
  end;
  ModflowOptions := frmGoPhast.PhastModel.ModflowOptions;
  edProjectName.Text := ModflowOptions.ProjectName;
  edDate.Text := ModflowOptions.ProjectDate;
  edModeler.Text := ModflowOptions.Modeler;
  memoComments.Lines.Assign(ModflowOptions.Description);
  rdeHDRY.Text := FloatToStr(ModflowOptions.HDry);
  rdeHNOFLO.Text := FloatToStr(ModflowOptions.HNoFlow);
  comboTimeUnit.ItemIndex := ModflowOptions.TimeUnit;
  comboLengthUnit.ItemIndex := ModflowOptions.LengthUnit;
  cbOpenInTextEditor.Checked := ModflowOptions.OpenInTextEditor;
  cbWettingClick(nil);
  comboModel.ItemIndex := 0;
  comboModelChange(nil);

  if not frmGoPhast.PhastModel.LgrUsed then
  begin
    NewHeight := Height - pnlModel.Height;
    pnlModel.Visible := False;
    Height := NewHeight;
  end;
end;

procedure TfrmModflowOptions.cbOpenInTextEditorClick(Sender: TObject);
begin
  inherited;
//  if FModflowOptions = nil then Exit;
//  FModflowOptions.OpenInTextEditor := cbOpenInTextEditor.Checked;
end;

procedure TfrmModflowOptions.cbWettingClick(Sender: TObject);
begin
  inherited;
  rconWet.Enabled := cbWetting.Checked;
  lblWettingDataSets.Visible := cbWetting.Checked;
//  if FWettingOptions = nil then Exit;
//  FWettingOptions.WettingActive := cbWetting.Checked;
end;

procedure TfrmModflowOptions.memoCommentsExit(Sender: TObject);
begin
  inherited;
//  if FModflowOptions = nil then Exit;
//  FModflowOptions.Description := memoComments.Lines;
end;

procedure TfrmModflowOptions.pcOptionsChange(Sender: TObject);
begin
  inherited;
  HelpKeyWord := pcOptions.ActivePage.HelpKeyword;
end;

procedure TfrmModflowOptions.comboLengthUnitChange(Sender: TObject);
var
  ItemIndex: Integer;
  Options: TModelOptions;
begin
  inherited;
  for ItemIndex := 0 to comboModel.Items.Count - 1 do
  begin
    Options := comboModel.Items.Objects[
      comboModel.ItemIndex] as TModelOptions;
    if Options <> nil then
    begin
      Options.LengthUnit := comboLengthUnit.ItemIndex;
    end;
  end;
end;

procedure TfrmModflowOptions.comboModelChange(Sender: TObject);
begin
  inherited;
  CurrentOptions := comboModel.Items.Objects[
    comboModel.ItemIndex] as TModelOptions;
end;

procedure TfrmModflowOptions.comboTimeUnitChange(Sender: TObject);
var
  ItemIndex: Integer;
  Options: TModelOptions;
begin
  inherited;
  for ItemIndex := 0 to comboModel.Items.Count - 1 do
  begin
    Options := comboModel.Items.Objects[
      comboModel.ItemIndex] as TModelOptions;
    if Options <> nil then
    begin
      Options.TimeUnit := comboTimeUnit.ItemIndex;
    end;
  end;
end;

procedure TfrmModflowOptions.comboWettingEquationChange(Sender: TObject);
begin
  inherited;
//  if FWettingOptions = nil then Exit;
//  FWettingOptions.WettingEquation := comboWettingEquation.ItemIndex;
end;

procedure TfrmModflowOptions.edDateExit(Sender: TObject);
var
  ItemIndex: Integer;
  Options: TModelOptions;
begin
  inherited;
  for ItemIndex := 0 to comboModel.Items.Count - 1 do
  begin
    Options := comboModel.Items.Objects[
      comboModel.ItemIndex] as TModelOptions;
    if Options <> nil then
    begin
      Options.ProjectDate := edDate.Text;
    end;
  end;
end;

procedure TfrmModflowOptions.edModelerExit(Sender: TObject);
var
  ItemIndex: Integer;
  Options: TModelOptions;
begin
  inherited;
  for ItemIndex := 0 to comboModel.Items.Count - 1 do
  begin
    Options := comboModel.Items.Objects[
      comboModel.ItemIndex] as TModelOptions;
    if Options <> nil then
    begin
      Options.Modeler := edModeler.Text;
    end;
  end;
end;

procedure TfrmModflowOptions.edProjectNameExit(Sender: TObject);
var
  ItemIndex: Integer;
  Options: TModelOptions;
begin
  inherited;
  for ItemIndex := 0 to comboModel.Items.Count - 1 do
  begin
    Options := comboModel.Items.Objects[
      comboModel.ItemIndex] as TModelOptions;
    if Options <> nil then
    begin
      Options.ProjectName := edProjectName.Text;
    end;
  end;
end;

procedure TfrmModflowOptions.feInitialHeadsExit(Sender: TObject);
begin
  inherited;
//  if FModflowOptions = nil then Exit;
//  FModflowOptions.InitialHeadFileName := feInitialHeads.Text;
end;

procedure TfrmModflowOptions.rdeHDRYExit(Sender: TObject);
var
  Value: Extended;
  ItemIndex: Integer;
  Options: TModelOptions;
begin
  inherited;
  if TryStrToFloat(rdeHDry.Text, Value) then
  begin
    for ItemIndex := 0 to comboModel.Items.Count - 1 do
    begin
      Options := comboModel.Items.Objects[
        comboModel.ItemIndex] as TModelOptions;
      if Options <> nil then
      begin
        Options.HDry := Value;
      end;
    end;
  end;
end;

procedure TfrmModflowOptions.rdeHNOFLOExit(Sender: TObject);
var
  Value: Extended;
  ItemIndex: Integer;
  Options: TModelOptions;
begin
  inherited;
  if TryStrToFloat(rdeHNOFLO.Text, Value) then
  begin
    for ItemIndex := 0 to comboModel.Items.Count - 1 do
    begin
      Options := comboModel.Items.Objects[
        comboModel.ItemIndex] as TModelOptions;
      if Options <> nil then
      begin
        Options.HNoFlow := Value;
      end;
    end;
  end;
end;

procedure TfrmModflowOptions.rdeWettingFactChange(Sender: TObject);
//var
//  Value: extended;
begin
  inherited;
//  if FWettingOptions = nil then Exit;
//  if TryStrToFloat(rdeWettingFact.Text, Value) then
//  begin
//    FWettingOptions.WettingFactor := Value;
//  end;

end;

procedure TfrmModflowOptions.seCheckDryChange(Sender: TObject);
begin
  inherited;
//  if FWettingOptions = nil then Exit;
//  FWettingOptions.WettingIterations := seCheckDry.AsInteger;
end;

procedure TfrmModflowOptions.SetCurrentOptions(const Value: TModelOptions);
var
  AValue: extended;
begin
  if FCurrentOptions <> Value then
  begin
    if FCurrentOptions <> nil then
    begin
      FCurrentOptions.CalculateFlow := cbCHTOCH.Checked;
      FCurrentOptions.PrintTime := cbPRINTTIME.Checked;
      FCurrentOptions.InitialHeadsFile := feInitialHeads.FileName;
      FCurrentOptions.WettingActive := cbWetting.Checked;
      if TryStrToFloat(rdeWettingFact.Text, AValue) then
      begin
        FCurrentOptions.WettingFactor := AValue;
      end;
//      FCurrentOptions.WettingFactor := StrToFloat(rdeWettingFact.Text);
      FCurrentOptions.WettingIterations := seCheckDry.AsInteger;
      FCurrentOptions.WettingEquation := comboWettingEquation.ItemIndex;
      FCurrentOptions.OpenInTextEditor := cbOpenInTextEditor.Checked;
      FCurrentOptions.Description := memoComments.Lines;
//      FCurrentOptions.LengthUnit := comboLengthUnit.ItemIndex;
    end;
    FCurrentOptions := Value;
    if FCurrentOptions <> nil then
    begin
      cbCHTOCH.Checked := FCurrentOptions.CalculateFlow;
      cbPRINTTIME.Checked := FCurrentOptions.PrintTime;
      feInitialHeads.FileName := FCurrentOptions.InitialHeadsFile;
      cbWetting.Checked := FCurrentOptions.WettingActive;
      cbWettingClick(nil);
      rdeWettingFact.Text := FloatToStr(FCurrentOptions.WettingFactor);
      seCheckDry.AsInteger := FCurrentOptions.WettingIterations;
      comboWettingEquation.ItemIndex := FCurrentOptions.WettingEquation;
      cbOpenInTextEditor.Checked := FCurrentOptions.OpenInTextEditor;
      memoComments.Lines := FCurrentOptions.Description;
//      comboLengthUnit.ItemIndex := FCurrentOptions.LengthUnit;
    end;
  end;
end;

procedure TfrmModflowOptions.SetData;
var
  Undo: TUndoGeneralOptions;
begin
  CurrentOptions := nil;
  Undo:= TUndoGeneralOptions.Create({FModflowOptions, FWettingOptions,}
    FModelOptionsCollection);
  frmGoPhast.UndoStack.Submit(Undo);
end;

{ TUndoGeneralOptions }

constructor TUndoGeneralOptions.Create({var NewModflowOptions: TModflowOptions;}
  {var NewWettinOptions: TWettingOptions;}
  var NewOptionsCollection: TModelOptionsCollection);
begin
  inherited Create;
//  FNewModflowOptions := NewModflowOptions;
//  NewModflowOptions := nil;
//  FOldModflowOptions := TModflowOptions.Create(nil);
//  FOldModflowOptions.Assign(frmGoPhast.PhastModel.ModflowOptions);

//  FNewWettingOptions := NewWettinOptions;
//  NewWettinOptions := nil;
//  FOldWettingOptions := TWettingOptions.Create(nil);
//  FOldWettingOptions.Assign(frmGoPhast.PhastModel.ModflowWettingOptions);

  FNewOptionsCollection := NewOptionsCollection;
  NewOptionsCollection := nil;

  FOldOptionsCollection := TModelOptionsCollection.Create(frmGoPhast.PhastModel);
end;

function TUndoGeneralOptions.Description: string;
begin
  result := 'general options'
end;

destructor TUndoGeneralOptions.Destroy;
begin
//  FNewModflowOptions.Free;
//  FOldModflowOptions.Free;
//  FNewWettingOptions.Free;
//  FOldWettingOptions.Free;
  FNewOptionsCollection.Free;
  FOldOptionsCollection.Free;
  inherited;
end;

procedure TUndoGeneralOptions.DoCommand;
begin
  inherited;
//  frmGoPhast.PhastModel.ModflowOptions.Assign(FNewModflowOptions);
//  frmGoPhast.PhastModel.ModflowWettingOptions.Assign(FNewWettingOptions);
  FNewOptionsCollection.AssignOptionsToModels;
  UpdatedRequiredDataSets;
end;

procedure TUndoGeneralOptions.Undo;
begin
  inherited;
//  frmGoPhast.PhastModel.ModflowOptions.Assign(FOldModflowOptions);
//  frmGoPhast.PhastModel.ModflowWettingOptions.Assign(FOldWettingOptions);
  FOldOptionsCollection.AssignOptionsToModels;
  UpdatedRequiredDataSets;
end;

{ TModelOptions }

procedure TModelOptions.AssignModel(AModel: TCustomModel);
begin
  Model := AModel;
  CalculateFlow := Model.ModflowOptions.ComputeFluxesBetweenConstantHeadCells;
  PrintTime := Model.ModflowOptions.PrintTime;
  InitialHeadsFile := Model.ModflowOptions.InitialHeadFileName;
  WettingActive := Model.ModflowWettingOptions.WettingActive;
  WettingFactor := Model.ModflowWettingOptions.WettingFactor;
  WettingIterations := Model.ModflowWettingOptions.WettingIterations;
  WettingEquation := Model.ModflowWettingOptions.WettingEquation;

  OpenInTextEditor := Model.ModflowOptions.OpenInTextEditor;
  Description := Model.ModflowOptions.Description;
  LengthUnit := Model.ModflowOptions.LengthUnit;
  TimeUnit := Model.ModflowOptions.TimeUnit;
  ProjectDate := Model.ModflowOptions.ProjectDate;
  Modeler := Model.ModflowOptions.Modeler;
  ProjectName := Model.ModflowOptions.ProjectName;
  HDry := Model.ModflowOptions.HDry;
  HNoFlow := Model.ModflowOptions.HNoFlow;
end;

procedure TModelOptions.AssignOptionsToModel;
begin
  Model.ModflowOptions.ComputeFluxesBetweenConstantHeadCells := CalculateFlow;
  Model.ModflowOptions.PrintTime := PrintTime;
  Model.ModflowOptions.InitialHeadFileName := InitialHeadsFile;
  Model.ModflowWettingOptions.WettingActive := WettingActive;
  Model.ModflowWettingOptions.WettingFactor := WettingFactor;
  Model.ModflowWettingOptions.WettingIterations := WettingIterations;
  Model.ModflowWettingOptions.WettingEquation := WettingEquation;

  Model.ModflowOptions.OpenInTextEditor := OpenInTextEditor;
  Model.ModflowOptions.Description := Description;
  Model.ModflowOptions.LengthUnit := LengthUnit;
  Model.ModflowOptions.TimeUnit := TimeUnit;
  Model.ModflowOptions.ProjectDate := ProjectDate;
  Model.ModflowOptions.Modeler := Modeler;
  Model.ModflowOptions.ProjectName := ProjectName;
  Model.ModflowOptions.HDry := HDry;
  Model.ModflowOptions.HNoFlow := HNoFlow;
end;

constructor TModelOptions.Create(Collection: TCollection);
begin
  inherited;
  FDescription := TStringList.Create;
end;

destructor TModelOptions.Destroy;
begin
  FDescription.Free;
  inherited;
end;

procedure TModelOptions.SetDescription(const Value: TStrings);
begin
  FDescription.Assign(Value);
end;

{ TModelOptionsCollection }

function TModelOptionsCollection.Add: TModelOptions;
begin
  result := inherited Add as TModelOptions
end;

procedure TModelOptionsCollection.AssignOptionsToModels;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].AssignOptionsToModel;
  end;
end;

constructor TModelOptionsCollection.Create(Model: TPhastModel);
var
  Item: TModelOptions;
  Index: Integer;
  ChildModel: TChildModel;
begin
  inherited Create(TModelOptions);
  Item := Add;
  Item.AssignModel(Model);
  if Model.LgrUsed then
  begin
    for Index := 0 to Model.ChildModels.Count - 1 do
    begin
      ChildModel := Model.ChildModels[Index].ChildModel;
      Item := Add;
      Item.AssignModel(ChildModel);
   end;
  end;
end;

function TModelOptionsCollection.GetItems(Index: integer): TModelOptions;
begin
  result := inherited Items[Index] as TModelOptions
end;

procedure TModelOptionsCollection.SetItems(Index: integer;
  const Value: TModelOptions);
begin
  inherited Items[Index] := Value;
end;

end.
