unit frmHeadObservationResultsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Mask, JvExMask, JvToolEdit,
  frameDisplayLimitUnit, JvSpin, JvExControls, JvColorBox, JvColorButton,
  Buttons, ExtCtrls, ModflowHeadObsResults, UndoItems, ComCtrls, Grids,
  RbwDataGrid4, GoPhastTypes, Generics.Collections;

type
  TUndoType = (utChange, utImport);
  TObsCol = (ocName, ocResidual, ocObserved, ocSimulated,
    ocX, ocY, ocTime, ocObjectName);

  TObsHeadLink = class(TObject)
  private
    FModel: TBaseModel;
    FOldHeadObsCollection: THeadObsCollection;
    FNewHeadObsCollection: THeadObsCollection;
  public
    constructor Create(AModel: TBaseModel);
    destructor Destroy; override;
  end;

  TObsHeadLinkList = class (TObjectList<TObsHeadLink>)
  public
    function IndexOfModel(AModel: TBaseModel): Integer;
  end;

  TCustomUndoChangeHeadObsResults = class(TCustomUndo)
  private
    FObsLinkList: TObsHeadLinkList;
    procedure ApplyChange(Model: TBaseModel;
      HeadObsCollection: THeadObsCollection);
    procedure UpdateGUI;
  public
    procedure DoCommand; override;
    procedure Undo; override;
  public
    constructor Create(var ObsLinkList: TObsHeadLinkList);
    Destructor Destroy; override;
  end;

  TUndoChangeHeadObsResults = class(TCustomUndoChangeHeadObsResults)
  protected
    function Description: string; override;
  end;

  TUndoImportChangeHeadObsResults = class(TCustomUndoChangeHeadObsResults)
  protected
    function Description: string; override;
  end;

  TfrmHeadObservationResults = class(TfrmCustomGoPhast)
    pnl1: TPanel;
    btnClose: TBitBtn;
    btnHelp: TBitBtn;
    btnApply: TBitBtn;
    pcHeadObs: TPageControl;
    tabControls: TTabSheet;
    lblNegativeColor: TLabel;
    lblColorPositive: TLabel;
    flnmedHeadObsResults: TJvFilenameEdit;
    grpbxFilter: TGroupBox;
    lblMaximumTime: TLabel;
    lblMaxResidual: TLabel;
    lblMinimumTime: TLabel;
    lblMinResidual: TLabel;
    framelmtMaximumTime: TframeDisplayLimit;
    framelmtMaxResidual: TframeDisplayLimit;
    framelmtMinimumTime: TframeDisplayLimit;
    framelmtMinResidual: TframeDisplayLimit;
    clrbtnNegative: TJvColorButton;
    clrbtnPositive: TJvColorButton;
    lblMaxSymbolSize: TLabel;
    spinSymbolSize: TJvSpinEdit;
    tabValues: TTabSheet;
    rdgHeadObs: TRbwDataGrid4;
    cbShow: TCheckBox;
    comboModels: TComboBox;
    lblHeadObsResults: TLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure flnmedHeadObsResultsChange(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure comboModelsChange(Sender: TObject);
  private
    FUndoType: TUndoType;
    FCurrentModelLink: TObsHeadLink;
    FGettingDate: Boolean;
    FImportResult: Boolean;
    ObsLinkList : TObsHeadLinkList;
    procedure SetCurrentModelLink(Value: TObsHeadLink);
    procedure SetData;
    procedure Initialize(AHeadObsColl: THeadObsCollection);
    procedure DisplayValues(AHeadObsColl: THeadObsCollection);
    procedure GetDataForAModel(AHeadObsColl: THeadObsCollection);
    function ReadFileForAModel(AHeadObsColl: THeadObsCollection;
      const FileName: string; ShowDialog: boolean): Boolean;
    procedure SetDataForAModel(HeadObs: THeadObsCollection);
    property CurrentModelLink: TObsHeadLink read FCurrentModelLink
      write SetCurrentModelLink;
    procedure TestForNewFiles;
    { Private declarations }
  public
    procedure GetData;
    function ReadFile(const FileName: string): Boolean;
    procedure UpdateChildModels;
    { Public declarations }
  end;

var
  frmHeadObservationResults: TfrmHeadObservationResults;

implementation

uses
  frmGoPhastUnit, PhastModelUnit, ModflowHobUnit;

resourcestring
  StrTheFileSHasADi = 'The file %s has a different date than the imported re' +
  'sults. Do you want to import it?';
  StrTheFileSHasADiMultiple = 'The following files %s have a different date '
  + 'than the imported results. Do you want to import them?';

{$R *.dfm}

procedure TfrmHeadObservationResults.btnCloseClick(Sender: TObject);
begin
  inherited;
  frmHeadObservationResults := nil;
  Release;
  ModalResult := mrCancel;
end;

procedure TfrmHeadObservationResults.comboModelsChange(Sender: TObject);
var
  AModel: TBaseModel;
  ModelIndex: Integer;
begin
  inherited;
  if comboModels.ItemIndex >= 0 then
  begin
    AModel := comboModels.Items.Objects[comboModels.ItemIndex] as TBaseModel;
    ModelIndex := ObsLinkList.IndexOfModel(AModel);
    if ModelIndex < 0 then
    begin
      CurrentModelLink := TObsHeadLink.Create(AModel);
      ObsLinkList.Add(FCurrentModelLink);
    end
    else
    begin
      CurrentModelLink := ObsLinkList[ModelIndex];
    end;
  end
  else
  begin
    CurrentModelLink := nil;
  end;
end;

procedure TfrmHeadObservationResults.btnApplyClick(Sender: TObject);
begin
  inherited;
  CurrentModelLink := nil;
  TestForNewFiles;
  SetData;
end;

procedure TfrmHeadObservationResults.flnmedHeadObsResultsChange(
  Sender: TObject);
begin
  inherited;
  if FGettingDate then
  begin
    Exit;
  end;
  if FileExists(flnmedHeadObsResults.FileName) then
  begin
    CurrentModelLink.FNewHeadObsCollection.FileName := flnmedHeadObsResults.FileName;
    FImportResult := CurrentModelLink.FNewHeadObsCollection.ReadFromFile;
    FUndoType := utImport
  end
  else
  begin
    CurrentModelLink.FNewHeadObsCollection.FileName := '';
    CurrentModelLink.FNewHeadObsCollection.FileDate := 0;
    CurrentModelLink.FNewHeadObsCollection.Clear;
  end;
end;



procedure TfrmHeadObservationResults.FormCreate(Sender: TObject);
begin
  inherited;
  ObsLinkList := TObsHeadLinkList.Create;

  pcHeadObs.ActivePageIndex := 0;
  framelmtMinResidual.Enabled := True;
  framelmtMaxResidual.Enabled := True;
  framelmtMinimumTime.Enabled := True;
  framelmtMaximumTime.Enabled := True;

  rdgHeadObs.BeginUpdate;
  try
    rdgHeadObs.Cells[Ord(ocName),0] := 'Observation Name';
    rdgHeadObs.Cells[Ord(ocX),0] := 'X';
    rdgHeadObs.Cells[Ord(ocY),0] := 'Y';
    rdgHeadObs.Cells[Ord(ocTime),0] := 'Time';
    rdgHeadObs.Cells[Ord(ocObserved),0] := 'Observed Value';
    rdgHeadObs.Cells[Ord(ocSimulated),0] := 'Simulated Value';
    rdgHeadObs.Cells[Ord(ocResidual),0] := 'Residual';
    rdgHeadObs.Cells[Ord(ocObjectName),0] := 'Object Name';
  finally
    rdgHeadObs.EndUpdate;
  end;

  UpdateChildModels;
end;

procedure TfrmHeadObservationResults.FormDestroy(Sender: TObject);
begin
  inherited;
//  FHeadObsCollection.Free;
  ObsLinkList.Free;
end;

procedure TfrmHeadObservationResults.GetData;
begin
  UpdateChildModels;

  CurrentModelLink := ObsLinkList[0];
end;

function TfrmHeadObservationResults.ReadFile(const FileName: string): Boolean;
var
  Index: Integer;
  ObsItem: TObsHeadLink;
  AFileName: string;
begin
  UpdateChildModels;

  result := True;
  for Index := 0 to ObsLinkList.Count - 1 do
  begin
    ObsItem := ObsLinkList[Index];
    if ObsItem.FModel is TChildModel then
    begin
      AFileName := TChildModel(ObsItem.FModel).Child_NameFile_Name(FileName);
      AFileName := ChangeFileExt(AFileName, StrHobout);
    end
    else
    begin
      AFileName := FileName;
    end;
    FCurrentModelLink := ObsItem;
    Result := ReadFileForAModel(ObsItem.FNewHeadObsCollection, AFileName, Index=0);
    if not result then
    begin
      Exit;
    end;
  end;
  FCurrentModelLink := nil;
  CurrentModelLink := ObsLinkList[0];
end;

procedure TfrmHeadObservationResults.UpdateChildModels;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  LocalModel: TPhastModel;
  Index: Integer;
  AModel: TBaseModel;
begin
  if ObsLinkList = nil then
  begin
    ObsLinkList := TObsHeadLinkList.Create;
  end;
  ObsLinkList.Clear;

  LocalModel := frmGoPhast.PhastModel;
  ObsLinkList.Add(TObsHeadLink.Create(LocalModel));
  if LocalModel.LgrUsed then
  begin
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
      ObsLinkList.Add(TObsHeadLink.Create(ChildModel));
    end;
  end;

  for Index := comboModels.Items.Count - 1 downto 0 do
  begin
    AModel := comboModels.Items.Objects[Index] as TBaseModel;
    if ObsLinkList.IndexOfModel(AModel) < 0 then
    begin
      comboModels.Items.Delete(Index);
    end;
  end;

//  comboModels.Clear;
  if comboModels.Items.IndexOfObject(LocalModel) < 0 then
  begin
    comboModels.Items.InsertObject(0, LocalModel.DisplayName, LocalModel);
  end;
  if LocalModel.LgrUsed then
  begin
    comboModels.Visible := True;
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
      if comboModels.Items.IndexOfObject(ChildModel) < 0 then
      begin
        comboModels.Items.InsertObject(ChildIndex+1, ChildModel.DisplayName, ChildModel);
      end;
    end;
  end
  else
  begin
    comboModels.Visible := False;
  end;
  comboModels.ItemIndex := 0;
end;

procedure TfrmHeadObservationResults.TestForNewFiles;
var
  ObsItem: TObsHeadLink;
  ReadFiles: Boolean;
  Index: Integer;
  FileDate: TDateTime;
  NewerFiles: TStringList;
begin
  NewerFiles := TStringList.Create;
  try
    for Index := 0 to ObsLinkList.Count - 1 do
    begin
      ObsItem := ObsLinkList[Index];
      if FileExists(ObsItem.FNewHeadObsCollection.FileName) then
      begin
        FileAge(ObsItem.FNewHeadObsCollection.FileName, FileDate);
        if FileDate <> ObsItem.FNewHeadObsCollection.FileDate then
        begin
          NewerFiles.Add(ObsItem.FNewHeadObsCollection.FileName);
        end;
      end;
    end;
    ReadFiles := False;
    if NewerFiles.Count > 0 then
    begin
      Beep;
      if NewerFiles.Count = 1 then
      begin
        ReadFiles := MessageDlg(Format(StrTheFileSHasADi, [NewerFiles[0]]),
          mtWarning, [mbYes, mbNo], 0) = mrYes;
      end
      else
      begin
        NewerFiles.LineBreak := ', ';
        ReadFiles := MessageDlg(Format(StrTheFileSHasADiMultiple, [NewerFiles.Text]),
          mtWarning, [mbYes, mbNo], 0) = mrYes;
      end;
    end;
    for Index := 0 to ObsLinkList.Count - 1 do
    begin
      ObsItem := ObsLinkList[Index];
      if ReadFiles and FileExists(ObsItem.FNewHeadObsCollection.FileName) then
      begin
        FileAge(ObsItem.FNewHeadObsCollection.FileName, FileDate);
        if FileDate <> ObsItem.FNewHeadObsCollection.FileDate then
        begin
          ObsItem.FNewHeadObsCollection.ReadFromFile;
        end;
      end;
      if ObsItem.FNewHeadObsCollection.FileName = '' then
      begin
        ObsItem.FNewHeadObsCollection.Clear;
      end;
    end;
  finally
    NewerFiles.Free;
  end;
end;

procedure TfrmHeadObservationResults.SetDataForAModel(HeadObs: THeadObsCollection);
begin
  HeadObs.MinResidualLimit := framelmtMinResidual.Limit;
  HeadObs.MaxResidualLimit := framelmtMaxResidual.Limit;
  HeadObs.MinTimeLimit := framelmtMinimumTime.Limit;
  HeadObs.MaxTimeLimit := framelmtMaximumTime.Limit;
  HeadObs.NegativeColor := clrbtnNegative.Color;
  HeadObs.PositiveColor := clrbtnPositive.Color;
  HeadObs.MaxSymbolSize := spinSymbolSize.AsInteger;
  HeadObs.Visible := cbShow.Checked;
end;

function TfrmHeadObservationResults.ReadFileForAModel(
  AHeadObsColl: THeadObsCollection; const FileName: string;
  ShowDialog: boolean): Boolean;
begin
  Initialize(AHeadObsColl);
  flnmedHeadObsResults.Dialog.FileName := FileName;
  if ShowDialog then
  begin
    result := flnmedHeadObsResults.Dialog.Execute;
  end
  else
  begin
    result := True;
  end;
  if result then
  begin
    FImportResult := False;
    flnmedHeadObsResults.FileName := flnmedHeadObsResults.Dialog.FileName;
    result := FImportResult;
  end;
  DisplayValues(AHeadObsColl);
end;

procedure TfrmHeadObservationResults.GetDataForAModel(AHeadObsColl: THeadObsCollection);
begin
  FGettingDate := True;
  try
    Initialize(AHeadObsColl);
    flnmedHeadObsResults.FileName := AHeadObsColl.FileName;
    DisplayValues(AHeadObsColl);
  finally
    FGettingDate := False;
  end;
end;

procedure TfrmHeadObservationResults.DisplayValues(AHeadObsColl: THeadObsCollection);
var
  Item: THeadObsItem;
  Index: Integer;
begin
  rdgHeadObs.BeginUpdate;
  try
    rdgHeadObs.RowCount := AHeadObsColl.Count + 1;
    for Index := 0 to AHeadObsColl.Count - 1 do
    begin
      Item := AHeadObsColl[Index];
      rdgHeadObs.Cells[Ord(ocName), Index + 1] := Item.Name;
      rdgHeadObs.Cells[Ord(ocX), Index + 1] := FloatToStr(Item.X);
      rdgHeadObs.Cells[Ord(ocY), Index + 1] := FloatToStr(Item.Y);
      rdgHeadObs.Cells[Ord(ocTime), Index + 1] := FloatToStr(Item.Time);
      rdgHeadObs.Cells[Ord(ocObserved), Index + 1] := FloatToStr(Item.ObservedValue);
      rdgHeadObs.Cells[Ord(ocSimulated), Index + 1] := FloatToStr(Item.SimulatedValue);
      rdgHeadObs.Cells[Ord(ocResidual), Index + 1] := FloatToStr(Item.Residual);
      rdgHeadObs.Cells[Ord(ocObjectName), Index + 1] := Item.ScreenObjectName;
    end;
  finally
    rdgHeadObs.EndUpdate;
  end;
end;

procedure TfrmHeadObservationResults.Initialize(AHeadObsColl: THeadObsCollection);
begin
//  FHeadObsCollection.Assign(AHeadObsColl);
  framelmtMinResidual.Limit := AHeadObsColl.MinResidualLimit;
  framelmtMaxResidual.Limit := AHeadObsColl.MaxResidualLimit;
  framelmtMinimumTime.Limit := AHeadObsColl.MinTimeLimit;
  framelmtMaximumTime.Limit := AHeadObsColl.MaxTimeLimit;
  clrbtnNegative.Color := AHeadObsColl.NegativeColor;
  clrbtnPositive.Color := AHeadObsColl.PositiveColor;
  spinSymbolSize.AsInteger := AHeadObsColl.MaxSymbolSize;
  cbShow.Checked := AHeadObsColl.Visible;
end;

procedure TfrmHeadObservationResults.SetCurrentModelLink(Value: TObsHeadLink);
var
  HeadObs: THeadObsCollection;
  FOldModelLink: TObsHeadLink;
begin
  if FCurrentModelLink <> nil then
  begin
    HeadObs := FCurrentModelLink.FNewHeadObsCollection;
    SetDataForAModel(HeadObs);
  end;
  FOldModelLink := FCurrentModelLink;
  FCurrentModelLink := Value;
  if FCurrentModelLink <> nil then
  begin
    if (FOldModelLink <> FCurrentModelLink) and (FOldModelLink <> nil) then
    begin
      FCurrentModelLink.FNewHeadObsCollection.MaxSymbolSize :=
        FOldModelLink.FNewHeadObsCollection.MaxSymbolSize
    end;
    GetDataForAModel(FCurrentModelLink.FNewHeadObsCollection)
  end;
end;

procedure TfrmHeadObservationResults.SetData;
var
  Undo: TCustomUndoChangeHeadObsResults;
begin
  Undo := nil;
  case FUndoType of
    utChange:
      begin
        Undo := TUndoChangeHeadObsResults.Create(ObsLinkList);
      end;
    utImport:
      begin
        Undo := TUndoImportChangeHeadObsResults.Create(ObsLinkList);
      end;
    else Assert(False);
  end;
  frmGoPhast.UndoStack.Submit(Undo);
  FUndoType := utChange;
end;

{ TCustomUndoChangeHeadObsResults }

procedure TCustomUndoChangeHeadObsResults.ApplyChange(Model: TBaseModel;
  HeadObsCollection: THeadObsCollection);
begin
  (Model as TCustomModel).HeadObsResults.Assign(HeadObsCollection);
end;

constructor TCustomUndoChangeHeadObsResults.Create(var ObsLinkList: TObsHeadLinkList);
begin
  FObsLinkList := ObsLinkList;
  ObsLinkList := nil;
end;

destructor TCustomUndoChangeHeadObsResults.Destroy;
begin
  FObsLinkList.Free;
  inherited;
end;

procedure TCustomUndoChangeHeadObsResults.UpdateGUI;
var
  ModelIndex: Integer;
begin
  frmGoPhast.TopGridChanged := True;
  frmGoPhast.frameTopView.ZoomBox.Image32.Invalidate;
  if frmHeadObservationResults <> nil then
  begin
    ModelIndex := frmHeadObservationResults.comboModels.ItemIndex;
    frmHeadObservationResults.GetData;
    if ModelIndex < frmHeadObservationResults.comboModels.Items.Count then
    begin
      frmHeadObservationResults.comboModels.ItemIndex := ModelIndex;
      frmHeadObservationResults.comboModelsChange(nil);
    end;
  end;
  frmGoPhast.miConfigureHeadObservationResults.Enabled := frmGoPhast.PhastModel.StoreHeadObsResults;
end;

procedure TCustomUndoChangeHeadObsResults.DoCommand;
var
  Index: Integer;
  ObsItem: TObsHeadLink;
begin
  inherited;
  for Index := 0 to FObsLinkList.Count - 1 do
  begin
    ObsItem := FObsLinkList[Index];
    ApplyChange(ObsItem.FModel, ObsItem.FNewHeadObsCollection);
  end;
  UpdateGUI;
end;

procedure TCustomUndoChangeHeadObsResults.Undo;
var
  Index: Integer;
  ObsItem: TObsHeadLink;
begin
  inherited;
  for Index := 0 to FObsLinkList.Count - 1 do
  begin
    ObsItem := FObsLinkList[Index];
    ApplyChange(ObsItem.FModel, ObsItem.FOldHeadObsCollection);
  end;
  UpdateGUI;
end;

{ TUndoChangeHeadObsResults }

function TUndoChangeHeadObsResults.Description: string;
begin
  result := 'change head result parameters';
end;

{ TUndoImportChangeHeadObsResults }

function TUndoImportChangeHeadObsResults.Description: string;
begin
  result := 'import head results';
end;

{ TObsHeadLinkList }

function TObsHeadLinkList.IndexOfModel(AModel: TBaseModel): Integer;
var
  Index: Integer;
  AnItem: TObsHeadLink;
begin
  for Index := 0 to Count - 1 do
  begin
    AnItem := Items[Index];
    if AnItem.FModel = AModel then
    begin
      Result := Index;
      Exit;
    end;
  end;
  result := -1;
end;

{ TObsHeadLink }

constructor TObsHeadLink.Create(AModel: TBaseModel);
var
  LocalModel: TCustomModel;
begin
  FModel := AModel;
  FOldHeadObsCollection := THeadObsCollection.Create(nil);
  FNewHeadObsCollection := THeadObsCollection.Create(nil);

  LocalModel := FModel as TCustomModel;
  FOldHeadObsCollection.Assign(LocalModel.HeadObsResults);
  FNewHeadObsCollection.Assign(LocalModel.HeadObsResults);
end;

destructor TObsHeadLink.Destroy;
begin
  FOldHeadObsCollection.Free;
  FNewHeadObsCollection.Free;
  inherited;
end;

end.
