unit frameContourDataUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frameCustomColorUnit, ExtCtrls, ArgusDataEntry, Grids, RbwDataGrid4,
  frameDisplayLimitUnit, SsButtonEd, RbwStringTreeCombo, StdCtrls, ComCtrls,
  JvExComCtrls, JvUpDown, JvExControls, JvxSlider, Mask, JvExMask, JvSpin,
  JvExStdCtrls, JvRichEdit, JvCheckBox, DataSetUnit, VirtualTrees;

type
  TframeContourData = class(TframeCustomColor)
    btnEditContours: TButton;
    cbSpecifyContours: TJvCheckBox;
    fdContourFont: TFontDialog;
    cbLabelContours: TCheckBox;
    btnContourFont: TButton;
    procedure cbSpecifyContoursClick(Sender: TObject);
    procedure btnEditContoursClick(Sender: TObject);
    procedure virttreecomboDataSetsChange(Sender: TObject);
    procedure virttreecomboDataSetsTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure virttreecomboDataSetsTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure cbLogTransformClick(Sender: TObject);
    procedure btnContourFontClick(Sender: TObject);
    procedure cbLabelContoursClick(Sender: TObject);
  private
    FContours: TContours;
    FGettingData: Boolean;
    FContourFont: TFont;
    procedure UpdateTopFrontAndSideItems;
    procedure HandleSelectedObject(AnObject: TObject);
    procedure SetMinMaxLabels;
    procedure HandleLimitChoice(DataSet: TDataArray);
    function GetContourDataSet: TDataArray;
    { Private declarations }
  protected
    function GetSelectedArray: TDataArray; override;
    procedure Loaded; override;
  public
    procedure GetData;
    procedure SetData; override;
    procedure ContourData(AnObject: TObject);
    procedure UpdateLabelsAndLegend;
    procedure UpdateContours;
    destructor Destroy; override;
    { Public declarations }
  end;

var
  frameContourData: TframeContourData;

implementation

uses
  RbwParser, GoPhastTypes, frmGoPhastUnit, ColorSchemes, frmFormulaErrorsUnit,
  frmErrorsAndWarningsUnit, frmProgressUnit, PhastModelUnit, LegendUnit,
  frmSpecifyContoursUnit, ClassificationUnit, Math;

{$R *.dfm}

{ TframeContourData }

procedure TframeContourData.btnContourFontClick(Sender: TObject);
begin
  inherited;
  fdContourFont.Font := FContourFont;
  if fdContourFont.Execute then
  begin
    FContourFont.Assign(fdContourFont.Font);
  end;
end;

procedure TframeContourData.btnEditContoursClick(Sender: TObject);
var
  frmSpecifyContours: TfrmSpecifyContours;
begin
  inherited;
  Application.CreateForm(TfrmSpecifyContours, frmSpecifyContours);
  try
    frmSpecifyContours.GetData(FContours, comboColorScheme.ItemIndex,
      seCycles.AsInteger, seColorExponent.Value, cbLogTransform.Checked);
    if frmSpecifyContours.ShowModal = mrOK then
    begin
      frmSpecifyContours.SetData(FContours);
    end;
  finally
    frmSpecifyContours.Free;
  end;
end;

procedure TframeContourData.cbLabelContoursClick(Sender: TObject);
begin
  inherited;
  btnContourFont.Enabled := cbLabelContours.Checked;
end;

procedure TframeContourData.cbLogTransformClick(Sender: TObject);
var
  AnArray: TOneDRealArray;
  Index: Integer;
  Count: Integer;
begin
  inherited;
  if (FContours <> nil)
    and (FContours.LogTransform <> cbLogTransform.Checked) then
  begin
    AnArray := Copy(FContours.ContourValues);
    if cbLogTransform.Checked then
    begin
      Count := 0;
      for Index := 0 to Length(AnArray) - 1 do
      begin
        if AnArray[Index] > 0 then
        begin
          AnArray[Index] := Log10(AnArray[Index]);
          Inc(Count);
        end;
      end;
    end
    else
    begin
      Count := 0;
      for Index := 0 to Length(AnArray) - 1 do
      begin
        AnArray[Index] := Power(10,AnArray[Index]);
        Inc(Count);
      end;
      SetLength(AnArray, Count);
    end;
    SetLength(AnArray, Count);
    FContours.ContourValues := AnArray;
    FContours.LogTransform := cbLogTransform.Checked
  end;
end;

procedure TframeContourData.cbSpecifyContoursClick(Sender: TObject);
begin
  inherited;
  btnEditContours.Enabled := cbSpecifyContours.Checked;
end;

procedure TframeContourData.ContourData(AnObject: TObject);
var
  Index: Integer;
  DataSet: TDataArray;
  ContourColors: TColorParameters;
  DataArrayManager: TDataArrayManager;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  Application.ProcessMessages;
  frmProgressMM.ShouldContinue := True;
  FreeAndNil(FStoredLegend);
  if AnObject <> nil then
  begin
    frmProgressMM.btnAbort.Visible := False;
    frmProgressMM.Caption := 'Progress';
    frmProgressMM.Show;
    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    for Index := 0 to DataArrayManager.DataSetCount - 1 do
    begin
      DataArrayManager.AddDataSetToCache(DataArrayManager.DataSets[Index]);
    end;
    DataArrayManager.CacheDataArrays;
    if (frmGoPhast.Grid.ThreeDContourDataSet <> nil)
      and (rgUpdateLimitChoice.ItemIndex = 1)
      and not cbSpecifyContours.Checked then
    begin
      FStoredLegend := TLegend.Create(nil);
      FStoredLegend.Assign(frmGoPhast.PhastModel.ContourLegend);
    end;
  end;
  if (AnObject = nil) then
  begin
    frmGoPhast.Grid.TopContourDataSet := nil;
    frmGoPhast.Grid.FrontContourDataSet := nil;
    frmGoPhast.Grid.SideContourDataSet := nil;
    frmGoPhast.Grid.ThreeDContourDataSet := nil;
    FLegend.ValueSource := nil;
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.Grid.TopContourDataSet := nil;
      ChildModel.Grid.FrontContourDataSet := nil;
      ChildModel.Grid.SideContourDataSet := nil;
      ChildModel.Grid.ThreeDContourDataSet := nil;
    end;
  end
  else if (AnObject is TDataArray) then
  begin
    DataSet := TDataArray(AnObject);
    AssignLimits(DataSet.DataType, DataSet.ContourLimits);
    if FContours <> nil then
    begin
      FContours.SpecifyContours := cbSpecifyContours.Checked;
    end;
    DataSet.Contours := FContours;
    if frmGoPhast.Grid.ThreeDContourDataSet <> DataSet then
    begin
      comboMethod.ItemIndex := 0;
    end;
    frmGoPhast.Grid.ThreeDContourDataSet := DataSet;
    if FTopItems.IndexOfObject(AnObject) >= 0 then
    begin
      frmGoPhast.Grid.TopContourDataSet := DataSet;
    end
    else
    begin
      frmGoPhast.Grid.TopContourDataSet := nil;
    end;
    if FFrontItems.IndexOfObject(AnObject) >= 0 then
    begin
      frmGoPhast.Grid.FrontContourDataSet := DataSet;
    end
    else
    begin
      frmGoPhast.Grid.FrontContourDataSet := nil;
    end;
    if FSideItems.IndexOfObject(AnObject) >= 0 then
    begin
      frmGoPhast.Grid.SideContourDataSet := DataSet;
    end
    else
    begin
      frmGoPhast.Grid.SideContourDataSet := nil;
    end;
    FLegend.ValueSource := DataSet;
    FLegend.ColoringLimits := DataSet.ContourLimits;
  end;
  frmGoPhast.PhastModel.Grid.GridChanged;
  ContourColors := frmGoPhast.PhastModel.ContourColorParameters;
  ContourColors.ColorScheme := comboColorScheme.ItemIndex;
  ContourColors.ColorCycles := seCycles.AsInteger;
  ContourColors.ColorExponent := seColorExponent.Value;
  tabLegend.TabVisible := FLegend.ValueSource <> nil;
  FLegend.ColorParameters := ContourColors;
end;

destructor TframeContourData.Destroy;
begin
  FContourFont.Free;
  FContours.Free;
  inherited;
end;

function TframeContourData.GetContourDataSet: TDataArray;
begin
  result := frmGoPhast.Grid.TopContourDataSet;
  if result = nil then
  begin
    result := frmGoPhast.Grid.FrontContourDataSet;
  end;
  if result = nil then
  begin
    result := frmGoPhast.Grid.SideContourDataSet;
  end;
  if result = nil then
  begin
    result := frmGoPhast.Grid.ThreeDContourDataSet;
  end;
end;

procedure TframeContourData.GetData;
var
  ContourColors: TColorParameters;
  VirtNoneNode: PVirtualNode;
begin
  FGettingData := True;
  try
    cbLabelContours.Checked := frmGoPhast.PhastModel.ShowContourLabels;
    FContourFont.Assign(frmGoPhast.PhastModel.ContourFont);
    virttreecomboDataSets.Tree.Clear;
    FFrontItems.Clear;
    FSideItems.Clear;
    FTopItems.Clear;

    VirtNoneNode := virttreecomboDataSets.Tree.AddChild(nil);
    virttreecomboDataSets.Tree.Selected[VirtNoneNode] := True;

    if csDestroying in frmGoPhast.PhastModel.ComponentState then
    begin
      Exit;
    end;

    GetDataSets;
    UpdateTopFrontAndSideItems;
  finally
    FGettingData := False;
  end;


  virttreecomboDataSetsChange(nil);

  ContourColors := frmGoPhast.PhastModel.ContourColorParameters;
  comboColorScheme.ItemIndex := ContourColors.ColorScheme;
  seCycles.Value := ContourColors.ColorCycles;
  jsColorExponent.Value := Round(ContourColors.ColorExponent*100);
  seColorExponent.Value := ContourColors.ColorExponent;
end;

function TframeContourData.GetSelectedArray: TDataArray;
begin
  result := frmGoPhast.Grid.ThreeDContourDataSet;
end;

procedure TframeContourData.HandleLimitChoice(DataSet: TDataArray);
var
  ContourDataSet: TDataArray;
begin
  case rgUpdateLimitChoice.ItemIndex of
    0:
      begin
        ReadLimits(DataSet.DataType, DataSet.ContourLimits);
      end;
    1:
      begin
        ContourDataSet := GetContourDataSet;
        if (ContourDataSet <> nil) and (DataSet.DataType = ContourDataSet.DataType) then
        begin
          ReadLimits(ContourDataSet.DataType, ContourDataSet.ContourLimits);
        end
        else
        begin
          ReadLimits(DataSet.DataType, DataSet.ContourLimits);
        end;
      end;
    else
      Assert(False);
  end;
end;

procedure TframeContourData.HandleSelectedObject(AnObject: TObject);
var
  DataSet: TDataArray;
  Contours: TContours;
  ContourDataSet: TDataArray;
begin
  if (AnObject = nil) then
  begin
    frameCheck3DMin.Enabled := False;
    frameCheck3DMax.Enabled := False;
    reComment.Enabled := False;
    cbSpecifyContours.Checked := False;
    cbSpecifyContours.Enabled := False;
    rdgValuesToIgnore.Enabled := False;
    seNumberOfValuesToIgnore.Enabled := False;
    seNumberOfValuesToIgnore.AsInteger := 0;
    seNumberOfValuesToIgnoreChange(nil);
    cbLogTransform.Enabled := False;
    rdgValuesToIgnore.Cells[0,1] := '';
  end
  else if (AnObject is TDataArray) then
  begin
    reComment.Enabled := True;
    frameCheck3DMin.Enabled := true;
    frameCheck3DMax.Enabled := true;
    DataSet := TDataArray(AnObject);
    rdgValuesToIgnore.Enabled := DataSet.DataType <> rdtBoolean;
    seNumberOfValuesToIgnore.Enabled := DataSet.DataType <> rdtBoolean;
    if not seNumberOfValuesToIgnore.Enabled then
    begin
      seNumberOfValuesToIgnore.AsInteger := 0;
      seNumberOfValuesToIgnoreChange(nil);
      rdgValuesToIgnore.Cells[0,1] := '';
    end;

    reComment.Text := DataSet.Comment;
    cbLogTransform.Enabled := DataSet.DataType = rdtDouble;
    HandleLimitChoice(DataSet);
    cbSpecifyContours.Enabled := True;

    case rgUpdateLimitChoice.ItemIndex of
      0:
        begin
          Contours := DataSet.Contours;
          cbSpecifyContours.Checked := (Contours <> nil) and Contours.SpecifyContours;
        end;
      1:
        begin
          ContourDataSet := GetContourDataSet;
          if (ContourDataSet <> nil) and (DataSet.DataType = ContourDataSet.DataType) then
          begin
            Contours := ContourDataSet.Contours;
            DataSet.Contours := Contours;
            cbSpecifyContours.Checked := (Contours <> nil) and Contours.SpecifyContours;
          end
          else
          begin
            Contours := DataSet.Contours;
            cbSpecifyContours.Checked := (Contours <> nil) and Contours.SpecifyContours;
          end;
        end;
      else
        Assert(False);
    end;
  end;
  UpdateLabelsAndLegend;
end;

procedure TframeContourData.Loaded;
begin
  inherited;
  FLegend := frmGoPhast.PhastModel.ContourLegend;
  FLegend.LegendType := ltContour;

  cbSpecifyContours.Parent := tabSelection;
  btnEditContours.Parent := tabSelection;
  FContourFont := TFont.Create;
end;

procedure TframeContourData.SetData;
var
  AnObject: TObject;
begin
  frmFormulaErrors.sgErrors.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try
    frmGoPhast.PhastModel.ContourFont := FContourFont;
    frmGoPhast.PhastModel.ShowContourLabels := cbLabelContours.Checked;

    RetrieveSelectedObject(AnObject);
    ContourData(AnObject);

    if (AnObject <> nil) and frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;

  finally
    Screen.Cursor := crDefault;
    frmFormulaErrors.sgErrors.EndUpdate;
  end;
  UpdateLabelsAndLegend;
  frmProgressMM.Hide;
end;

procedure TframeContourData.SetMinMaxLabels;
var
  AnObject: TObject;
  DataSet: TDataArray;
begin
  RetrieveSelectedObject(AnObject);
  lblLowerLimit.Caption := StrLowerLimit;
  lblUpperLimit.Caption := StrUpperLimit;
  if (AnObject = nil) then
  begin
  end
  else if (AnObject is TDataArray) then
  begin
    DataSet := TDataArray(AnObject);
    lblLowerLimit.Caption := StrLowerLimit
      + ' (Min value = ' + DataSet.MinValue + ')';
    lblUpperLimit.Caption := StrUpperLimit
      + ' (Max value = ' + DataSet.MaxValue + ')';
  end;
end;

procedure TframeContourData.UpdateContours;
var
  DataArray: TDataArray;
  AnObject: TObject;
begin
  RetrieveSelectedObject(AnObject);
  if (AnObject <> nil) then
  begin
    DataArray := AnObject as TDataArray;
    if DataArray.Contours = nil then
    begin
      FreeAndNil(FContours);
    end
    else
    begin
      if FContours = nil then
      begin
        FContours := TContours.Create;
      end;
      FContours.Assign(DataArray.Contours);
    end;
  end;
end;

procedure TframeContourData.UpdateLabelsAndLegend;
begin
  if FGettingData or (frmGoPhast.PhastModel = nil)
    or (csDestroying in frmGoPhast.PhastModel.ComponentState) then
  begin
    Exit;
  end;
  SetMinMaxLabels;
  UpdateLegend;
end;

procedure TframeContourData.UpdateTopFrontAndSideItems;
begin
  StoreDataSetsInLists;

  FinalizeList(FTopItems);
  FinalizeList(FFrontItems);
  FinalizeList(FSideItems);
end;

procedure TframeContourData.virttreecomboDataSetsChange(Sender: TObject);
var
  AnObject: TObject;
begin
  inherited;
  ResetTreeText;
  RetrieveSelectedObject(AnObject);
  HandleSelectedObject(AnObject);
  UpdateContours;
end;

procedure TframeContourData.virttreecomboDataSetsTreeChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  inherited;
  SetSelectedNode(Sender, Node);
end;

procedure TframeContourData.virttreecomboDataSetsTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

end.