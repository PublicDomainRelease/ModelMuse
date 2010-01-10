unit frmContourDataUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomColorUnit, StdCtrls, Buttons, JvExControls, JvxSlider, Mask,
  JvExMask, JvSpin, frameDisplayLimitUnit, JvExStdCtrls, JvRichEdit, ExtCtrls,
  ComCtrls, DataSetUnit, JvCheckBox, TntStdCtrls, TntExDropDownEdit,
  TntExDropDownVirtualStringTree, VirtualTrees, JvExComCtrls, JvUpDown, Grids,
  RbwDataGrid4, RbwParser;

type
  TfrmContourData = class(TfrmCustomColor)
    cbSpecifyContours: TJvCheckBox;
    btnEditContours: TButton;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure cbSpecifyContoursClick(Sender: TObject);
    procedure btnEditContoursClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure virttreecomboDataSetsChange(Sender: TObject);
    procedure virttreecomboDataSetsDropDownTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure virttreecomboDataSetsDropDownTreeGetNodeDataSize(
      Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure cbLogTransformClick(Sender: TObject);
  private
    FContours: TContours;
    procedure GetData;
    procedure SetData;
    procedure UpdateTopFrontAndSideItems;
    procedure HandleSelectedObject(AnObject: TObject);
  protected
    function GetSelectedArray: TDataArray; override;
    { Private declarations }
  public
    procedure SetMinMaxLabels;
    procedure UpdateContours;
    { Public declarations }
  end;

procedure UpdateFrmContourData;

var
  frmContourData: TfrmContourData;

implementation

uses
  PhastModelUnit, frmGoPhastUnit, GoPhastTypes, frmProgressUnit,
  frmErrorsAndWarningsUnit, ColorSchemes, frmSpecifyContoursUnit, 
  frmCustomGoPhastUnit, ClassificationUnit, Math;

{$R *.dfm}

procedure UpdateFrmContourData;
begin
  if (frmContourData <> nil) and frmContourData.Visible then
  begin
    frmContourData.GetData;
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

  SetMinMaxLabels;
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
  virttreecomboDataSets.Tree.Clear;
  FFrontItems.Clear;
  FSideItems.Clear;
  FTopItems.Clear;

  VirtNoneNode := virttreecomboDataSets.Tree.AddChild(nil);
  virttreecomboDataSets.Tree.Selected[VirtNoneNode] := True;

  GetDataSets;
  UpdateTopFrontAndSideItems;

  virttreecomboDataSetsChange(nil);

  ContourColors := frmGoPhast.PhastModel.ContourColors;
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
  DataSet: TDataArray;
  Index: Integer;
  ContourColors: TColorParameters;
begin
  Screen.Cursor := crHourGlass;
  try
    Application.ProcessMessages;
    frmProgress.ShouldContinue := True;

    RetrieveSelectedObject(AnObject);

    if AnObject <> nil then
    begin
      for Index := 0 to frmGoPhast.PhastModel.DataSetCount - 1 do
      begin
        frmGoPhast.PhastModel.AddDataSetToCache(frmGoPhast.PhastModel.DataSets[Index]);
      end;
      frmGoPhast.PhastModel.CacheDataArrays;
    end;

    if (AnObject = nil) then
    begin
      frmGoPhast.Grid.TopContourDataSet := nil;
      frmGoPhast.Grid.FrontContourDataSet := nil;
      frmGoPhast.Grid.SideContourDataSet := nil;
      frmGoPhast.Grid.ThreeDContourDataSet := nil;
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
    end;

    frmGoPhast.PhastModel.Grid.GridChanged;

    ContourColors := frmGoPhast.PhastModel.ContourColors;
    ContourColors.ColorScheme := comboColorScheme.ItemIndex;
    ContourColors.ColorCycles := seCycles.AsInteger;
    ContourColors.ColorExponent := seColorExponent.Value;

    if (AnObject <> nil) and frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;

  finally
    Screen.Cursor := crDefault;
  end;
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

procedure TfrmContourData.HandleSelectedObject(AnObject: TObject);
var
  DataSet: TDataArray;
  Contours: TContours;
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
    ReadLimits(DataSet.DataType, DataSet.ContourLimits);
    cbSpecifyContours.Enabled := True;
    Contours := DataSet.Contours;
    cbSpecifyContours.Checked := (Contours <> nil) and Contours.SpecifyContours;
  end;
  SetMinMaxLabels;
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

initialization

finalization
  frmContourData.Free;

end.