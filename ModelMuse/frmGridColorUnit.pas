{@abstract(The main purpose of @name is to define @link(TfrmGridColor) which
  is used to allow the user to specify what @link(TDataArray)
  should be used to color the @link(TPhastGrid).)

@author(Richard B. Winston <rbwinst@usgs.gov>)
}
unit frmGridColorUnit;

{ TODO : add a legend for the color scale. }

interface

uses
  SysUtils, Types, Classes, Contnrs, frmCustomGoPhastUnit, frmCustomColorUnit,
  Mask, JvExMask, JvSpin, JvExControls, JvxSlider, StdCtrls, Spin, Forms,
  frameDisplayLimitUnit, ArgusDataEntry, Graphics, Controls, Dialogs, ComCtrls,
  Buttons, ExtCtrls,  JvComponent, EdgeDisplayUnit, JvExStdCtrls, JvRichEdit,
  DataSetUnit, JvCombobox, JvExComCtrls, JvUpDown, TntStdCtrls,
  TntExDropDownEdit, TntExDropDownVirtualStringTree, VirtualTrees,
  ClassificationUnit, Grids, RbwDataGrid4, RbwParser;

type
  TEdgeDisplayEdit = class(TObject)
    Edge: TCustomModflowGridEdgeDisplay;
    DataIndex: integer;
  end;
 
  {@abstract(@name is used to allow the user to specify what @link(TDataArray)
    should be used to color the @link(TPhastGrid).)}
  TfrmGridColor = class(TfrmCustomColor)
    // @name displays "Time".
    lblTime: TLabel;
    // For transient @link(TDataArray)s, @name is used to specify the time
    // used for the @link(TDataArray) to be displayed.
    comboTime3D: TJvComboBox;
    udTime: TJvUpDown;
    // @name causes the grid to be colored with the selected
    // @link(TDataArray) by calling @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name initializes some variables and calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
    // @name destroys private variables.
    procedure FormDestroy(Sender: TObject);
    procedure comboTime3DChange(Sender: TObject);
    procedure udTimeChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure virttreecomboDataSetsDropDownTreeGetNodeDataSize(
      Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    // @name enables or disables other controls based on what is selected in
    // @link(virttreecomboDataSets).  It also updates the values used as limits
    // in @link(frameCheck3DMax) and @link(frameCheck3DMin).
    procedure virttreecomboDataSetsChange(Sender: TObject);
    procedure virttreecomboDataSetsDropDownTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure FormShow(Sender: TObject);
  private
    FEdgeEdits: TList;
    FBoundaryClassifications: TList;
    procedure GetData;
    // @name sets the @link(TDataArray) used to color the @link(TPhastGrid).
    procedure SetData;
    procedure GetBoundaryConditions;
    procedure UpdateTopFrontAndSideItems;
    procedure SetTimeComboColor;
    procedure StoreBoundaryDataSetsInLists;
    procedure StoreTimelistsInLists;
    procedure HandleSelectedObject(AnObject: TObject);

  protected
    function GetSelectedArray: TDataArray; override;
    { Private declarations }
  public
    procedure SetMinMaxLabels;
    { Public declarations }
  end;

  TBoundaryClassification = class(TClassificationObject)
  private
    FDataArray: TDataArray;
    FTimeList: TCustomTimeList;
    FEdgeDisplay: TEdgeDisplayEdit;
    FName: string;
    function GetClassifiedObject: TObject;
  public
    function ClassificationName: string; Override;
    function FullClassification: string; Override;
    Constructor Create(AnObject: TDataArray); overload;
    Constructor Create(AnObject: TCustomTimeList); overload;
    Constructor Create(const Name: string; AnObject: TEdgeDisplayEdit); overload;
    Constructor Create(const Name: string; AnObject: TObject); overload;
    property ClassifiedObject: TObject read GetClassifiedObject;
  end;

procedure UpdateFrmGridColor;

var
  frmGridColor: TfrmGridColor = nil;

resourcestring
  StrBoundaryConditions = 'Boundary Conditions, Observations, and Other Features';

implementation

uses frmGoPhastUnit, PhastDataSets, ModelMuseUtilities,
  GoPhastTypes, StrUtils, PhastModelUnit, frmErrorsAndWarningsUnit,
  frmProgressUnit, RealListUnit, ColorSchemes;

{$R *.dfm}

procedure UpdateFrmGridColor;
begin
  if (frmGridColor <> nil) and frmGridColor.Visible then
  begin
    frmGridColor.GetData;
  end;
end;

procedure TfrmGridColor.FormCreate(Sender: TObject);
begin
  inherited;
  FBoundaryClassifications := TObjectList.Create;
  AdjustFormPosition(dpRight);

  udTime.Max := High(SmallInt);
  udTime.Min := Low(SmallInt);
  FEdgeEdits := TObjectList.Create;

  lblTime.Parent := tabSelection;
  comboTime3D.Parent := tabSelection;
  udTime.Parent := tabSelection;
end;

procedure TfrmGridColor.GetData;
var
  GridColors: TColorParameters;
  VirtNoneNode: PVirtualNode;
begin
  virttreecomboDataSets.Tree.Clear;

  FFrontItems.Clear;
  FSideItems.Clear;
  FTopItems.Clear;

  VirtNoneNode := virttreecomboDataSets.Tree.AddChild(nil);
  virttreecomboDataSets.Tree.Selected[VirtNoneNode] := True;

  GetDataSets;
  GetBoundaryConditions;
  UpdateTopFrontAndSideItems;

  virttreecomboDataSetsChange(nil);

  comboTime3D.Text := FloatToStr(frmGoPhast.PhastModel.ThreeDDisplayTime);

  GridColors := frmGoPhast.PhastModel.GridColors;
  comboColorScheme.ItemIndex := GridColors.ColorScheme;
  seCycles.Value := GridColors.ColorCycles;
  jsColorExponent.Value := Round(GridColors.ColorExponent*100);
  seColorExponent.Value := GridColors.ColorExponent;

  frmGoPhast.PhastModel.ModflowStressPeriods.
    FillStringsWithStartTimes(comboTime3D.Items);
end;

procedure TfrmGridColor.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
  SetMinMaxLabels;
end;

procedure TfrmGridColor.SetData;
var
  AnObject: TObject;
  TimeList: TCustomTimeList;
  Time: double;
  Is3DSelected: boolean;
  DataSet: TDataArray;
  EdgeEdit: TEdgeDisplayEdit;
  Index: Integer;
  GridColors: TColorParameters;
  TimeListIndex: Integer;
  ADataArray: TDataArray;
begin
  Screen.Cursor := crHourGlass;
  try
    Application.ProcessMessages;
    frmProgress.ShouldContinue := True;
    Is3DSelected := (frmGoPhast.Grid.ThreeDDataSet <> nil);
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
      frmGoPhast.Grid.TopDataSet := nil;
      frmGoPhast.PhastModel.TopTimeList := nil;
      frmGoPhast.Grid.FrontDataSet := nil;
      frmGoPhast.PhastModel.FrontTimeList := nil;
      frmGoPhast.Grid.SideDataSet := nil;
      frmGoPhast.PhastModel.SideTimeList := nil;
      frmGoPhast.Grid.ThreeDDataSet := nil;
      frmGoPhast.PhastModel.ThreeDTimeList := nil;
      frmGoPhast.PhastModel.EdgeDisplay := nil;
    end
    else if (AnObject is TDataArray) then
    begin
      frmGoPhast.PhastModel.EdgeDisplay := nil;

      DataSet := TDataArray(AnObject);

      AssignLimits(DataSet.DataType, DataSet.Limits);

      frmGoPhast.Grid.ThreeDDataSet := DataSet;
      frmGoPhast.PhastModel.ThreeDTimeList := nil;
      if FTopItems.IndexOfObject(AnObject) >= 0 then
      begin
        frmGoPhast.Grid.TopDataSet := DataSet;
      end
      else
      begin
        frmGoPhast.Grid.TopDataSet := nil;
      end;
      frmGoPhast.PhastModel.TopTimeList := nil;

      if FFrontItems.IndexOfObject(AnObject) >= 0 then
      begin
        frmGoPhast.Grid.FrontDataSet := DataSet;
      end
      else
      begin
        frmGoPhast.Grid.FrontDataSet := nil;
      end;
      frmGoPhast.PhastModel.FrontTimeList := nil;

      if FSideItems.IndexOfObject(AnObject) >= 0 then
      begin
        frmGoPhast.Grid.SideDataSet := DataSet;
      end
      else
      begin
        frmGoPhast.Grid.SideDataSet := nil;
      end;
      frmGoPhast.PhastModel.SideTimeList := nil;
    end
    else if AnObject is TCustomTimeList then
    begin
      frmGoPhast.PhastModel.EdgeDisplay := nil;

      TimeList := TCustomTimeList(AnObject);
      AssignLimits(TimeList.DataType, TimeList.Limits);
      if TimeList.UpToDate then
      begin
        for TimeListIndex := 0 to TimeList.Count - 1 do
        begin
          ADataArray := TimeList[TimeListIndex];
          AssignLimits(ADataArray.DataType, ADataArray.Limits);
        end;
      end;

      Time := StrToFloat(comboTime3D.Text);
      frmGoPhast.PhastModel.UpdateThreeDTimeDataSet(TimeList, Time);
      if FTopItems.IndexOfObject(AnObject) >= 0 then
      begin
        frmGoPhast.PhastModel.UpdateTopTimeDataSet(TimeList, Time);
      end
      else
      begin
        frmGoPhast.Grid.TopDataSet := nil;
        frmGoPhast.PhastModel.TopTimeList := nil;
      end;
      if FFrontItems.IndexOfObject(AnObject) >= 0 then
      begin
        frmGoPhast.PhastModel.UpdateFrontTimeDataSet(TimeList, Time);
      end
      else
      begin
        frmGoPhast.Grid.FrontDataSet := nil;
        frmGoPhast.PhastModel.FrontTimeList := nil;
      end;
      if FSideItems.IndexOfObject(AnObject) >= 0 then
      begin
        frmGoPhast.PhastModel.UpdateSideTimeDataSet(TimeList, Time);
      end
      else
      begin
        frmGoPhast.Grid.SideDataSet := nil;
        frmGoPhast.PhastModel.SideTimeList := nil;
      end;
    end
    else
    begin
      frmGoPhast.Grid.TopDataSet := nil;
      frmGoPhast.PhastModel.TopTimeList := nil;
      frmGoPhast.Grid.FrontDataSet := nil;
      frmGoPhast.PhastModel.FrontTimeList := nil;
      frmGoPhast.Grid.SideDataSet := nil;
      frmGoPhast.PhastModel.SideTimeList := nil;
      frmGoPhast.Grid.ThreeDDataSet := nil;
      frmGoPhast.PhastModel.ThreeDTimeList := nil;

      EdgeEdit := AnObject as TEdgeDisplayEdit;

      AssignLimits(rdtDouble, EdgeEdit.Edge.Limits[EdgeEdit.DataIndex]);

      EdgeEdit.Edge.DataToPlot := EdgeEdit.DataIndex;
      frmGoPhast.PhastModel.EdgeDisplay := EdgeEdit.Edge;
    end;

    frmGoPhast.acColoredGrid.Enabled := (frmGoPhast.Grid.ThreeDDataSet <> nil)
      or (frmGoPhast.PhastModel.EdgeDisplay <> nil);
    if not frmGoPhast.acColoredGrid.Enabled then
    begin
      frmGoPhast.acColoredGrid.Checked := False;
      frmGoPhast.tb3DColors.Down := False;
    end;

    if frmGoPhast.acColoredGrid.Enabled and not Is3DSelected then
    begin
      frmGoPhast.acColoredGrid.Checked := True;
      frmGoPhast.tb3DColors.Down := True;
    end;
    frmGoPhast.PhastModel.Grid.GridChanged;

    GridColors := frmGoPhast.PhastModel.GridColors;
    GridColors.ColorScheme := comboColorScheme.ItemIndex;
    GridColors.ColorCycles := seCycles.AsInteger;
    GridColors.ColorExponent := seColorExponent.Value;

    if (AnObject <> nil) and frmErrorsAndWarnings.HasMessages then
    begin
      frmErrorsAndWarnings.Show;
    end;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmGridColor.GetBoundaryConditions;
var
  BoundaryIndex: Integer;
  TimeList: TCustomTimeList;
  List: TStringList;
  ClassificationPosition: Integer;
  DataSet: TDataArray;
  Index: Integer;
  Classifications: TStringList;
  SelectedTimeList: TCustomTimeList;
  SelectedDataArray: TDataArray;
  EdgeEdit: TEdgeDisplayEdit;
  DummyRootClassification: TDummyClassification;
  RootNode: PVirtualNode;
  NodeData: PClassificationNodeData;
  DummyClassification: TDummyClassification;
  VirtualClassificationNode: PVirtualNode;
  BounddaryClassification: TBoundaryClassification;
  VirtualNode: PVirtualNode;
  ParentNode: PVirtualNode;
  SelectedEdgeDisplay: TCustomModflowGridEdgeDisplay;
  AnObject: TObject;
begin
  FBoundaryClassifications.Clear;

  SelectedDataArray := frmGoPhast.Grid.ThreeDDataSet;
  SelectedTimeList := frmGoPhast.PhastModel.ThreeDTimeList;
  SelectedEdgeDisplay := frmGoPhast.PhastModel.EdgeDisplay;

  DummyRootClassification := TDummyClassification.Create(StrBoundaryConditions);
  FBoundaryClassifications.Add(DummyRootClassification);
  RootNode := virttreecomboDataSets.Tree.AddChild(nil);
  NodeData := virttreecomboDataSets.Tree.GetNodeData(RootNode);
  NodeData.ClassificationObject := DummyRootClassification;

  Classifications := TStringList.Create;
  try
    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      for Index := 0 to frmGoPhast.PhastModel.BoundaryDataSetCount - 1 do
      begin
        DataSet := frmGoPhast.PhastModel.BoundaryDataSets[Index];
        ClassificationPosition := Classifications.IndexOf(DataSet.Classification);
        if ClassificationPosition < 0 then
        begin
          List := TStringList.Create;
          Classifications.AddObject(DataSet.Classification, List);
        end
        else
        begin
          List := Classifications.Objects[ClassificationPosition] as TStringList;
        end;
        List.AddObject(DataSet.Name, DataSet);
      end;
    end;
    FEdgeEdits.Clear;
    if (frmGoPhast.PhastModel.ModelSelection = msModflow)
      and frmGoPhast.PhastModel.ModflowPackages.HfbPackage.IsSelected then
    begin
      List := TStringList.Create;
      Classifications.AddObject('MODFLOW Horizontal Flow Barrier', List);
      for Index := 0 to
        frmGoPhast.PhastModel.HfbDisplayer.RealValueTypeCount - 1 do
      begin
        EdgeEdit := TEdgeDisplayEdit.Create;
        FEdgeEdits.Add(EdgeEdit);
        EdgeEdit.DataIndex := Index;
        EdgeEdit.Edge := frmGoPhast.PhastModel.HfbDisplayer;
        List.AddObject(EdgeEdit.Edge.RealDescription[Index], EdgeEdit);
      end;
    end;
    for Index := 0 to frmGoPhast.PhastModel.TimeListCount - 1 do
    begin
      TimeList := frmGoPhast.PhastModel.TimeLists[Index];
      if TimeList.UsedByModel then
      begin
        ClassificationPosition := Classifications.IndexOf(
          TimeList.Classification);
        if ClassificationPosition < 0 then
        begin
          List := TStringList.Create;
          Classifications.AddObject(TimeList.Classification, List);
        end
        else
        begin
          List := Classifications.Objects[ClassificationPosition] as TStringList;
        end;
        List.AddObject(TimeList.Name, TimeList);
      end;
    end;
    Classifications.Sort;
    for Index := 0 to Classifications.Count - 1 do
    begin
      DummyClassification := TDummyClassification.Create(Classifications[Index]);
      FBoundaryClassifications.Add(DummyClassification);
      VirtualClassificationNode := virttreecomboDataSets.Tree.AddChild(RootNode);
      NodeData := virttreecomboDataSets.Tree.GetNodeData(VirtualClassificationNode);
      NodeData.ClassificationObject := DummyClassification;

      List := Classifications.Objects[Index] as TStringList;
      List.Sort;

      for BoundaryIndex := 0 to List.Count - 1 do
      begin
        BounddaryClassification := TBoundaryClassification.Create(List[BoundaryIndex], List.Objects[BoundaryIndex]);
        FBoundaryClassifications.Add(BounddaryClassification);
        VirtualNode := virttreecomboDataSets.Tree.AddChild(VirtualClassificationNode);
        NodeData := virttreecomboDataSets.Tree.GetNodeData(VirtualNode);
        NodeData.ClassificationObject := BounddaryClassification;
        AnObject := BounddaryClassification.ClassifiedObject;

        if AnObject is TEdgeDisplayEdit then
        begin
          EdgeEdit := TEdgeDisplayEdit(AnObject);
          virttreecomboDataSets.Tree.Selected[VirtualNode] :=
            (EdgeEdit.Edge = SelectedEdgeDisplay)
            and (EdgeEdit.DataIndex = SelectedEdgeDisplay.DataToPlot)
        end
        else
        begin
          virttreecomboDataSets.Tree.Selected[VirtualNode] :=
            (AnObject = SelectedDataArray)
            or (AnObject = SelectedTimeList);
        end;
        if virttreecomboDataSets.Tree.Selected[VirtualNode] then
        begin
          ParentNode := VirtualNode.Parent;
          while ParentNode <> nil do
          begin
            virttreecomboDataSets.Tree.Expanded[ParentNode] := True;
            if ParentNode = RootNode then
            begin
              break;
            end;
            ParentNode := ParentNode.Parent;
          end;
        end;
      end;
    end;
  finally
    for Index := 0 to Classifications.Count - 1 do
    begin
      Classifications.Objects[Index].Free;
    end;
    Classifications.Free;
  end;
end;

procedure TfrmGridColor.UpdateTopFrontAndSideItems;
begin
  StoreDataSetsInLists;
  StoreBoundaryDataSetsInLists;
  StoreTimelistsInLists;

  FinalizeList(FTopItems);
  FinalizeList(FFrontItems);
  FinalizeList(FSideItems);
end;

procedure TfrmGridColor.virttreecomboDataSetsChange(Sender: TObject);
var
  AnObject: TObject;
begin
  inherited;
  ResetTreeText;
  RetrieveSelectedObject(AnObject);
  HandleSelectedObject(AnObject);
end;

procedure TfrmGridColor.virttreecomboDataSetsDropDownTreeChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  inherited;
  SetSelectedNode(Sender, Node);

end;

procedure TfrmGridColor.virttreecomboDataSetsDropDownTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

procedure TfrmGridColor.SetMinMaxLabels;
var
  AnObject: TObject;
  DataSet: TDataArray;
  TimeList: TCustomTimeList;
  ATime: Extended;
  GridDisplay: TEdgeDisplayEdit;
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
  end
  else if AnObject is TCustomTimeList then
  begin
    TimeList := TCustomTimeList(AnObject);
    if TryStrToFloat(comboTime3D.Text, ATime) then
    begin
      lblLowerLimit.Caption := StrLowerLimit
        + ' (Min value = ' + TimeList.MinValue(ATime) + ')';
      lblUpperLimit.Caption := StrUpperLimit
        + ' (Max value = ' + TimeList.MaxValue(ATime) + ')';
    end;
  end
  else
  begin
    GridDisplay := AnObject as TEdgeDisplayEdit;
    lblLowerLimit.Caption := StrLowerLimit
      + ' (Min value = ' + GridDisplay.Edge.MinValue + ')';
    lblUpperLimit.Caption := StrUpperLimit
      + ' (Max value = ' + GridDisplay.Edge.MaxValue + ')';
  end;
end;

procedure TfrmGridColor.HandleSelectedObject(AnObject: TObject);
var
  DataSet: TDataArray;
  GridDisplay: TEdgeDisplayEdit;
  TimeList: TCustomTimeList;
begin
  if (AnObject = nil) then
  begin
    frameCheck3DMin.Enabled := False;
    frameCheck3DMax.Enabled := False;
    reComment.Enabled := False;
    comboTime3D.Enabled := False;
    cbActiveOnly.Enabled := False;
    rdgValuesToIgnore.Enabled := False;
    seNumberOfValuesToIgnore.Enabled := False;
    seNumberOfValuesToIgnore.AsInteger := 0;
    seNumberOfValuesToIgnoreChange(nil);
    rdgValuesToIgnore.Cells[0,1] := '';
    cbLogTransform.Enabled := False;
    SetTimeComboColor;
  end
  else if (AnObject is TDataArray) then
  begin
    reComment.Enabled := True;
    frameCheck3DMin.Enabled := true;
    frameCheck3DMax.Enabled := true;
    cbActiveOnly.Enabled := true;
    DataSet := TDataArray(AnObject);
    reComment.Text := DataSet.Comment;
    rdgValuesToIgnore.Enabled := DataSet.DataType <> rdtBoolean;
    seNumberOfValuesToIgnore.Enabled := DataSet.DataType <> rdtBoolean;
    if not seNumberOfValuesToIgnore.Enabled then
    begin
      seNumberOfValuesToIgnore.AsInteger := 0;
      seNumberOfValuesToIgnoreChange(nil);
      rdgValuesToIgnore.Cells[0,1] := '';
    end;
    comboTime3D.Enabled := False;
    cbLogTransform.Enabled := DataSet.DataType = rdtDouble;
    SetTimeComboColor;

    ReadLimits(DataSet.DataType, DataSet.Limits);
  end
  else if AnObject is TCustomTimeList then
  begin
    reComment.Enabled := False;
    frameCheck3DMin.Enabled := true;
    frameCheck3DMax.Enabled := true;
    cbActiveOnly.Enabled := true;
    comboTime3D.Enabled := true;
    SetTimeComboColor;
    TimeList := TCustomTimeList(AnObject);
    rdgValuesToIgnore.Enabled := TimeList.DataType <> rdtBoolean;
    seNumberOfValuesToIgnore.Enabled := TimeList.DataType <> rdtBoolean;
    if not seNumberOfValuesToIgnore.Enabled then
    begin
      seNumberOfValuesToIgnore.AsInteger := 0;
      seNumberOfValuesToIgnoreChange(nil);
      rdgValuesToIgnore.Cells[0,1] := '';
    end;
    cbLogTransform.Enabled := TimeList.DataType = rdtDouble;
    ReadLimits(TimeList.DataType, TimeList.Limits);
  end
  else
  begin
    reComment.Enabled := False;
    frameCheck3DMin.Enabled := true;
    frameCheck3DMax.Enabled := true;
    cbActiveOnly.Enabled := true;
    comboTime3D.Enabled := False;
    rdgValuesToIgnore.Enabled := True;
    seNumberOfValuesToIgnore.Enabled := True;
    SetTimeComboColor;
    GridDisplay := AnObject as TEdgeDisplayEdit;
    GridDisplay.Edge.DataToPlot := GridDisplay.DataIndex;
    GridDisplay.Edge.UpdateMinMax;
    cbLogTransform.Enabled := True;
    ReadLimits(rdtDouble, GridDisplay.Edge.Limits[GridDisplay.DataIndex]);
  end;
  SetMinMaxLabels;
end;

procedure TfrmGridColor.StoreTimelistsInLists;
var
  TimeList: TCustomTimeList;
  Index: Integer;
begin
  for Index := 0 to frmGoPhast.PhastModel.TimeListCount - 1 do
  begin
    TimeList := frmGoPhast.PhastModel.TimeLists[Index];
    case TimeList.Orientation of
      dsoTop:
        begin
          FTopItems.AddObject(TimeList.Name, TimeList);
        end;
      dsoFront:
        begin
          FFrontItems.AddObject(TimeList.Name, TimeList);
        end;
      dsoSide:
        begin
          FSideItems.AddObject(TimeList.Name, TimeList);
        end;
      dso3D:
        begin
          FTopItems.AddObject(TimeList.Name, TimeList);
          FFrontItems.AddObject(TimeList.Name, TimeList);
          FSideItems.AddObject(TimeList.Name, TimeList);
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TfrmGridColor.StoreBoundaryDataSetsInLists;
var
  Index: Integer;
  DataSet: TDataArray;
begin
  for Index := 0 to frmGoPhast.PhastModel.BoundaryDataSetCount - 1 do
  begin
    DataSet := frmGoPhast.PhastModel.BoundaryDataSets[Index];
    case DataSet.Orientation of
      dsoTop:
        begin
          FTopItems.AddObject(DataSet.Name, DataSet);
        end;
      dsoFront:
        begin
          FFrontItems.AddObject(DataSet.Name, DataSet);
        end;
      dsoSide:
        begin
          FSideItems.AddObject(DataSet.Name, DataSet);
        end;
      dso3D:
        begin
          FTopItems.AddObject(DataSet.Name, DataSet);
          FFrontItems.AddObject(DataSet.Name, DataSet);
          FSideItems.AddObject(DataSet.Name, DataSet);
        end;
    else
      Assert(False);
    end;
  end;
end;

function TfrmGridColor.GetSelectedArray: TDataArray;
begin
  result := frmGoPhast.Grid.ThreeDDataSet;
end;

procedure TfrmGridColor.SetTimeComboColor;
var
  ATime: Double;
begin
  if comboTime3D.Enabled then
  begin
    if (comboTime3D.Text = '') or TryStrToFloat(comboTime3D.Text, ATime) then
    begin
      comboTime3D.Color := clWindow;
    end
    else
    begin
      comboTime3D.Color := clRed;
    end;
  end
  else
  begin
    comboTime3D.Color := clBtnFace;
  end;
end;

procedure TfrmGridColor.udTimeChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint; Direction: TUpDownDirection);
var
  ATime: double;
  NewIndex: Integer;
  RealList: TRealList;
  Index: Integer;
begin
  inherited;
  NewIndex := -1;
  if comboTime3D.ItemIndex >= 0 then
  begin
    NewIndex := comboTime3D.ItemIndex;
  end
  else if TryStrToFloat(comboTime3D.Text, ATime) then
  begin
    RealList := TRealList.Create;
    try
      RealList.Capacity:= comboTime3D.Items.Count;
      for Index := 0 to comboTime3D.Items.Count - 1 do
      begin
        RealList.Add(StrToFloat(comboTime3D.Items[Index]));
      end;
      NewIndex := RealList.IndexOf(ATime);
      if NewIndex < 0 then
      begin
        NewIndex := RealList.IndexOfClosest(ATime);
        if (RealList[NewIndex] > ATime) and (Direction = updUp) then
        begin
          Dec(NewIndex);
        end
        else if (RealList[NewIndex] < ATime) and (Direction = updDown) then
        begin
          Inc(NewIndex);
        end;
      end;
    finally
      RealList.Free;
    end;
  end;
  case Direction of
    updNone: ;
    updUp: Dec(NewIndex);
    updDown: Inc(NewIndex);
  end;
  if NewIndex < 0 then
  begin
    NewIndex := 0;
  end
  else if NewIndex >= comboTime3D.Items.Count then
  begin
    NewIndex := comboTime3D.Items.Count-1;
  end;
  if comboTime3D.ItemIndex <> NewIndex then
  begin
    comboTime3D.ItemIndex := NewIndex;
    btnOKClick(nil);
  end;
end;

procedure TfrmGridColor.comboTime3DChange(Sender: TObject);
begin
  inherited;
  SetTimeComboColor;
end;

procedure TfrmGridColor.FormDestroy(Sender: TObject);
begin
  inherited;
  FEdgeEdits.Free;
  FBoundaryClassifications.Free;
end;

procedure TfrmGridColor.FormShow(Sender: TObject);
begin
  inherited;
  udTime.Left := comboTime3D.Left + comboTime3D.Width;
  udTime.Top := comboTime3D.Top;
  udTime.Height := comboTime3D.Height;
end;

{ TBoundaryClassification }

function TBoundaryClassification.ClassificationName: string;
begin
  if FDataArray <> nil then
  begin
    result := FDataArray.Name;
    Assert(FTimeList = nil);
    Assert(FEdgeDisplay = nil);
  end
  else if FTimeList <> nil then
  begin
    result := FTimeList.Name;
    Assert(FEdgeDisplay = nil);
  end
  else
  begin
    result := FName;
  end;
end;


constructor TBoundaryClassification.Create(AnObject: TDataArray);
begin
  FDataArray := AnObject;
  FTimeList := nil;
  FEdgeDisplay := nil;
end;

constructor TBoundaryClassification.Create(AnObject: TCustomTimeList);
begin
  FTimeList := AnObject;
  FDataArray := nil;
  FEdgeDisplay := nil;
end;

function TBoundaryClassification.FullClassification: string;
begin
  result := ''
end;

function TBoundaryClassification.GetClassifiedObject: TObject;
begin
  if FDataArray <> nil then
  begin
    result := FDataArray;
  end
  else if FTimeList <> nil then
  begin
    result := FTimeList;
  end
  else
  begin
    result := FEdgeDisplay;
    Assert(result <> nil);
  end;
end;


constructor TBoundaryClassification.Create(const Name: string;
  AnObject: TEdgeDisplayEdit);
begin
  FName := Name;
  FEdgeDisplay := AnObject;
  FDataArray := nil;
  FTimeList := nil;
end;

constructor TBoundaryClassification.Create(const Name: string;
  AnObject: TObject);
begin
  FName := Name;
  if AnObject is TDataArray then
  begin
    Create(TDataArray(AnObject));
  end
  else if AnObject is TCustomTimeList then
  begin
    Create(TCustomTimeList(AnObject));
  end
  else if AnObject is TEdgeDisplayEdit then
  begin
    Create(Name, TEdgeDisplayEdit(AnObject));
  end
  else
  begin
    Assert(False);
  end;
end;

initialization

finalization
  frmGridColor.Free;

end.
