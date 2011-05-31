unit frmContourDataUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomColorUnit, StdCtrls, Buttons, JvExControls, JvxSlider, Mask,
  JvExMask, JvSpin, frameDisplayLimitUnit, JvExStdCtrls, JvRichEdit, ExtCtrls,
  ComCtrls, DataSetUnit, JvCheckBox, VirtualTrees, JvExComCtrls, JvUpDown,
  Grids, RbwDataGrid4, RbwParser, ArgusDataEntry, SsButtonEd,
  RbwStringTreeCombo;

type
  TfrmContourData = class(TfrmCustomColor)
    cbSpecifyContours: TJvCheckBox;
    btnEditContours: TButton;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure cbSpecifyContoursClick(Sender: TObject);
    procedure btnEditContoursClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure virttreecomboDataSetsChange(Sender: TObject);
    procedure virttreecomboDataSetsDropDownTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure virttreecomboDataSetsDropDownTreeGetNodeDataSize(
      Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure cbLogTransformClick(Sender: TObject);
  private
    FContours: TContours;
    FGettingData: Boolean;
    procedure GetData;
    procedure SetData;
    procedure UpdateTopFrontAndSideItems;
    procedure HandleSelectedObject(AnObject: TObject);
    procedure SetMinMaxLabels;
    procedure HandleLimitChoice(DataSet: TDataArray);
    function GetContourDataSet: TDataArray;
  protected
    function GetSelectedArray: TDataArray; override;
    { Private declarations }
  public
    procedure ContourData(AnObject: TObject);
    procedure UpdateLabelsAndLegend;
    procedure UpdateContours;
    { Public declarations }
  end;

procedure UpdateFrmContourData(Force: boolean = False);

var
  frmContourData: TfrmContourData;

implementation

uses
  PhastModelUnit, frmGoPhastUnit, GoPhastTypes, frmProgressUnit,
  frmErrorsAndWarningsUnit, ColorSchemes, frmSpecifyContoursUnit, 
  frmCustomGoPhastUnit, ClassificationUnit, Math, LegendUnit,
  frmFormulaErrorsUnit;

{$R *.dfm}

procedure UpdateFrmContourData(Force: boolean = False);
begin
  if (frmContourData <> nil) and (Force or frmContourData.Visible) then
  begin
    frmContourData.GetData;
    frmContourData.UpdateLabelsAndLegend;
  end;
end;

{ TfrmContourData }

procedure TfrmContourData.btnEditContoursClick(Sender: TObject);
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

procedure TfrmContourData.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
  UpdateLabelsAndLegend;
  frmProgressMM.Hide;
end;

procedure TfrmContourData.cbLogTransformClick(Sender: TObject);
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

procedure TfrmContourData.cbSpecifyContoursClick(Sender: TObject);
begin
  inherited;
  btnEditContours.Enabled := cbSpecifyContours.Checked;
end;

procedure TfrmContourData.FormCreate(Sender: TObject);
begin
  inherited;
  FLegend := frmGoPhast.PhastModel.ContourLegend;
  FLegend.LegendType := ltContour;

  AdjustFormPosition(dpBottomLeft);

  cbSpecifyContours.Parent := tabSelection;
  btnEditContours.Parent := tabSelection;

  GetData;
end;

procedure TfrmContourData.FormDestroy(Sender: TObject);
begin
  inherited;
  FContours.Free;
end;

procedure TfrmContourData.GetData;
var
  ContourColors: TColorParameters;
  VirtNoneNode: PVirtualNode;
begin
  FGettingData := True;
  try
    virttreecomboDataSets1.Tree.Clear;
    FFrontItems.Clear;
    FSideItems.Clear;
    FTopItems.Clear;

    VirtNoneNode := virttreecomboDataSets1.Tree.AddChild(nil);
    virttreecomboDataSets1.Tree.Selected[VirtNoneNode] := True;

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

function TfrmContourData.GetSelectedArray: TDataArray;
begin
  result := frmGoPhast.Grid.ThreeDContourDataSet;
end;

procedure TfrmContourData.SetData;
var
  AnObject: TObject;
begin
  frmFormulaErrors.sgErrors.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try

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
end;

procedure TfrmContourData.UpdateLabelsAndLegend;
begin
  if FGettingData or (frmGoPhast.PhastModel = nil)
    or (csDestroying in frmGoPhast.PhastModel.ComponentState) then
  begin
    Exit;
  end;
  SetMinMaxLabels;
  UpdateLegend;
end;

procedure TfrmContourData.SetMinMaxLabels;
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

procedure TfrmContourData.UpdateContours;
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

procedure TfrmContourData.ContourData(AnObject: TObject);
var
  Index: Integer;
  DataSet: TDataArray;
  ContourColors: TColorParameters;
  DataArrayManager: TDataArrayManager;
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

function TfrmContourData.GetContourDataSet: TDataArray;
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

procedure TfrmContourData.HandleSelectedObject(AnObject: TObject);
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

procedure TfrmContourData.UpdateTopFrontAndSideItems;
begin
  StoreDataSetsInLists;

  FinalizeList(FTopItems);
  FinalizeList(FFrontItems);
  FinalizeList(FSideItems);
end;

procedure TfrmContourData.virttreecomboDataSetsChange(Sender: TObject);
var
  AnObject: TObject;
begin
  inherited;
  ResetTreeText;
  RetrieveSelectedObject(AnObject);
  HandleSelectedObject(AnObject);
  UpdateContours;

end;

procedure TfrmContourData.virttreecomboDataSetsDropDownTreeChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  inherited;
  SetSelectedNode(Sender, Node);
end;

procedure TfrmContourData.virttreecomboDataSetsDropDownTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

procedure TfrmContourData.HandleLimitChoice(DataSet: TDataArray);
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



initialization

finalization
  frmContourData.Free;

end.
