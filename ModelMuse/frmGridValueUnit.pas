unit frmGridValueUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ScreenObjectUnit,
  TntStdCtrls, TntExDropDownEdit, TntExDropDownVirtualStringTree, ComCtrls,
  DataSetUnit, VirtualTrees, FastGEO;

type
  TfrmGridValue = class(TfrmCustomGoPhast)
    btnHelp: TBitBtn;
    btnClose: TBitBtn;
    pcDataDisplay: TPageControl;
    tabCurrentData: TTabSheet;
    lblLower3rdDimensionCoordinate: TLabel;
    lblHigher3rdDimensionCoordinate: TLabel;
    cbShowThirdDValues: TCheckBox;
    lblSelectedObject: TLabel;
    memoExplanation: TMemo;
    lblExplanation: TLabel;
    edCellValue: TEdit;
    lblCellValue: TLabel;
    lblDataSet: TLabel;
    lblColumn: TLabel;
    lblRow: TLabel;
    lblLayer: TLabel;
    lblLayerHeight: TLabel;
    lblRowWidth: TLabel;
    lblColumnWidth: TLabel;
    tabAllDataSets: TTabSheet;
    virttreecomboDataSets: TTntExDropDownVirtualStringTree;
    lblSelectValue: TLabel;
    edSelectValue: TEdit;
    lblSelectExplanation: TLabel;
    memoSelectExplanation: TMemo;
    btnUpdate: TButton;
    lblSection: TLabel;
    lblVertex: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject); override;
    procedure edCellValueKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure memoExplanationKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnUpdateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure FormShow(Sender: TObject);
    procedure virttreecomboDataSetsDropDownTreeGetNodeDataSize(
      Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure virttreecomboDataSetsDropDownTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure virttreecomboDataSetsChange(Sender: TObject);
    procedure virttreecomboDataSetsClosedUp(Sender: TObject);
    procedure virttreecomboDataSetsDropDownTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure virttreecomboDataSetsEnter(Sender: TObject);
  private
    FSelectedScreenObject: TScreenObject;
    FColumn: Integer;
    FRow: Integer;
    FLayer: Integer;
    // @name is implemented as a TObjectList.
    FDataSetDummyObjects: TList;
    FSelectedVirtNode: PVirtualNode;
    FShouldClick: Boolean;
    property SelectedVirtNode: PVirtualNode read FSelectedVirtNode;
    procedure UpdatedSelectedObject;
    procedure UpdateScreenObjectInfo(const Column, Row, Layer: Integer; const Location: TPoint2D);
    procedure UpdateSelectedData(Layer, Row, Column: integer);
    procedure GetSelectedDataArray(var OtherDataSet: TDataArray);
    { Private declarations }
  public
    procedure UpdateValue(const Layer, Row, Column: integer;
      const DataSetName, CellValue: string; Explanation: string;
      const Location: TPoint2D);
    procedure UpdateDataSets;
    { Public declarations }
  end;

procedure UpdateFrmGridValue;

var
  frmGridValue: TfrmGridValue;

implementation

uses Clipbrd, CustomModflowWriterUnit, AbstractGridUnit, frmGoPhastUnit, 
  GoPhastTypes, GIS_Functions, RbwParser, Contnrs, ClassificationUnit;

resourcestring
  StrSelectedObject = 'Selected object';
  StrHigher3rdDimension = 'Higher 3rd dimension coordinate';
  StrLower3rdDimension = 'Lower 3rd dimension coordinate';
  StrNotAssigned = ': (not assigned)';
  StrHigher = 'Higher ';
  StrLower = 'Lower ';
  StrPriorVertex = 'Prior vertex number ';

{$R *.dfm}

procedure UpdateFrmGridValue;
begin
  if (frmGridValue <> nil) and frmGridValue.Visible then
  begin
    frmGridValue.UpdateDataSets;
  end;
end;

{ TfrmGridValue }

procedure TfrmGridValue.btnUpdateClick(Sender: TObject);
var
  OtherDataSet: TDataArray;
begin
  inherited;
  GetSelectedDataArray(OtherDataSet);
  if (OtherDataSet <> nil) then
  begin
    frmGoPhast.PhastModel.DataArrayManager.CacheDataArrays;
    OtherDataSet.Initialize;
    frmGoPhast.PhastModel.DataArrayManager.AddDataSetToCache(OtherDataSet);
  end;
  UpdateSelectedData(FLayer, FRow, FColumn);
end;

procedure TfrmGridValue.edCellValueKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if ((Key = Ord('C')) or (Key = Ord('c'))) and (ssCtrl in Shift) then
  begin
    edCellValue.CopyToClipboard;
  end;
end;

procedure TfrmGridValue.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  frmGridValue := nil;
  Action := caFree;
end;

procedure TfrmGridValue.FormCreate(Sender: TObject);
begin
  inherited;
  AdjustFormPosition(dpLeft);
  FDataSetDummyObjects := TObjectList.Create;
end;

procedure TfrmGridValue.FormDestroy(Sender: TObject);
begin
  inherited;
  FDataSetDummyObjects.Free;
end;

procedure TfrmGridValue.FormShow(Sender: TObject);
begin
  inherited;
  UpdateDataSets;
  // virttreecomboDataSets thows an exception if it is not on the
  // active page when TfrmGridValue is created.
  Assert(pcDataDisplay.ActivePageIndex = 1);
  pcDataDisplay.ActivePageIndex := 0;
end;

procedure TfrmGridValue.memoExplanationKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if ((Key = Ord('C')) or (Key = Ord('c'))) and (ssCtrl in Shift) then
  begin
    memoExplanation.CopyToClipboard;
  end;
end;

procedure TfrmGridValue.UpdateSelectedData(Layer, Row, Column: integer);
var
  OtherDataSet: TDataArray;
begin
  btnUpdate.Enabled := False;
  GetSelectedDataArray(OtherDataSet);
  if OtherDataSet = nil then
  begin
    edSelectValue.Text := '';
    memoSelectExplanation.Text := '';
    Exit;
  end;

  if not OtherDataSet.UpToDate then
  begin
    edSelectValue.Text := '';
    memoSelectExplanation.Text := 'Data set values need to be updated.';
    btnUpdate.Enabled := True;
    Exit;
  end;

  if (Layer < 0) or (Row < 0) or (Column < 0) then
  begin
    edSelectValue.Text := '';
    memoSelectExplanation.Text := 'The mouse is not over the grid.';
    Exit;
  end;


  case OtherDataSet.Orientation of
    dsoTop: Layer := 0;
    dsoFront: Row := 0;
    dsoSide: Column := 0;
    dso3D: ; // do nothing
    else Assert(False);
  end;

  if Layer > OtherDataSet.LayerCount then
  begin
    Layer := OtherDataSet.LayerCount -1;
  end;
  if Row > OtherDataSet.RowCount then
  begin
    Row := OtherDataSet.RowCount -1;
  end;
  if Column > OtherDataSet.ColumnCount then
  begin
    Column := OtherDataSet.ColumnCount -1;
  end;

  if not OtherDataSet.IsValue[Layer, Row, Column] then
  begin
    edSelectValue.Text := '';
    memoSelectExplanation.Text := 'The data set does not have a value '
      + 'at the current location.';
    Exit;
  end;

  case OtherDataSet.DataType of
    rdtDouble: edSelectValue.Text := FloatToStr(
      OtherDataSet.RealData[Layer, Row, Column]);
    rdtInteger: edSelectValue.Text := IntToStr(
      OtherDataSet.IntegerData[Layer, Row, Column]);
    rdtBoolean:
      begin
        if OtherDataSet.BooleanData[Layer, Row, Column] then
        begin
          edSelectValue.Text := 'True';
        end
        else
        begin
          edSelectValue.Text := 'False';
        end;
      end;
    rdtString: edSelectValue.Text :=
      OtherDataSet.StringData[Layer, Row, Column];
  end;
  memoSelectExplanation.Text :=
    OtherDataSet.Annotation[Layer, Row, Column];
end;

procedure TfrmGridValue.UpdateValue(const Layer, Row, Column: integer;
  const DataSetName, CellValue: string; Explanation: string;
  const Location: TPoint2D);
var
  Grid: TCustomGrid;
  DataArray: TDataArray;
  EvaluatedAt: TEvaluatedAt;
  ColumnWidth: Double;
  RowWidth: Double;
  LayerHeight: Double;
begin
  FColumn := Column;
  FRow := Row;
  FLayer := Layer;
  lblLayer.Caption := 'Layer: ' + IntToStr(Layer+1);
  lblRow.Caption := 'Row: ' + IntToStr(Row+1);
  lblColumn.Caption := 'Column: ' + IntToStr(Column+1);
  lblDataSet.Caption := DataSetName;
  edCellValue.Text := CellValue;
  if (Explanation <> StrNoValueAssigned) and (Pos(StrNoValueAssigned, Explanation) > 0) then
  begin
    Explanation := StringReplace(Explanation, StrNoValueAssigned, '', []);
  end;
  memoExplanation.Text := Explanation;

  Grid := frmGoPhast.Grid;
  if (Grid <> nil) and (Grid.LayerCount >= 1)
    and (Grid.RowCount >= 1) and (Grid.ColumnCount >= 1) then
  begin
    if DataSetName <> '' then
    begin
      DataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(DataSetName)
    end
    else
    begin
      DataArray := nil;
    end;
    if DataArray = nil then
    begin
      EvaluatedAt := eaBlocks;
    end
    else
    begin
      EvaluatedAt := DataArray.EvaluatedAt;
    end;
    GlobalEvaluatedAt := EvaluatedAt;
    ColumnWidth := GetColumnWidth(Column);
    RowWidth := GetRowWidth(Row);
    LayerHeight := GetLayerHeight(Column, Row, Layer);

    lblLayerHeight.Caption := 'Layer height: ' + FloatToStr(LayerHeight);
    lblRowWidth.Caption := 'Row width:'  + FloatToStr(RowWidth);
    lblColumnWidth.Caption := 'Column width: ' + FloatToStr(ColumnWidth);
  end
  else
  begin
    lblLayerHeight.Caption := 'Layer height:';
    lblRowWidth.Caption := 'Row width:';
    lblColumnWidth.Caption := 'Column width:';
  end;
  
  UpdateScreenObjectInfo(Column, Row, Layer, Location);
  UpdateSelectedData(Layer, Row, Column);

end;

procedure TfrmGridValue.virttreecomboDataSetsChange(Sender: TObject);
begin
  inherited;
  UpdateTreeComboText(SelectedVirtNode, virttreecomboDataSets);
  UpdateSelectedData(FLayer, FRow, FColumn);
end;

procedure TfrmGridValue.virttreecomboDataSetsClosedUp(Sender: TObject);
begin
  inherited;
  if FShouldClick then
  begin
    FShouldClick := False;
    MouseClick;
  end;
end;

procedure TfrmGridValue.virttreecomboDataSetsDropDownTreeChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  inherited;
  SelectOnlyLeaves(Node, virttreecomboDataSets, Sender, FSelectedVirtNode);
end;

procedure TfrmGridValue.virttreecomboDataSetsDropDownTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

procedure TfrmGridValue.virttreecomboDataSetsDropDownTreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmGridValue.virttreecomboDataSetsEnter(Sender: TObject);
begin
  inherited;
  FShouldClick := True;
end;

procedure TfrmGridValue.GetSelectedDataArray(var OtherDataSet: TDataArray);
begin
  OtherDataSet := frmGoPhast.PhastModel.DataArrayManager.
    GetDataSetByName(virttreecomboDataSets.Text);
end;

procedure TfrmGridValue.UpdateScreenObjectInfo
  (const Column, Row, Layer: Integer; const Location: TPoint2D);
var
  Value: Double;
  DirectionText: string;
  Segment: TCellElementSegment;
  procedure AssignHigherElevLabel(const ExtraText: string);
  var
    Indicies: array[0..2] of Integer;
    Grid: TCustomGrid;
    VarIndex: Integer;
    MaxCount: Integer;
    VarLabel: string;
    FoundValue: Boolean;
    LayRowColIndex: Integer;
  begin
    if cbShowThirdDValues.Checked then
    begin
      if FSelectedScreenObject.
        IsHigher3DElevationAssigned(Column, Row, Layer) then
      begin
        Value := FSelectedScreenObject.
          Higher3DElevations[Layer, Row, Column];
        lblHigher3rdDimensionCoordinate.Caption :=
          ExtraText + DirectionText + ': ' + FloatToStr(Value);
      end
      else
      begin
        Indicies[0] := Layer;
        Indicies[1] := Row;
        Indicies[2] := Column;

        Grid := frmGoPhast.Grid;

        VarIndex := -1;
        MaxCount := 0;
        VarLabel := '';
        case FSelectedScreenObject.ViewDirection of
          vdTop:
            begin
              VarIndex := 0;
              MaxCount := Grid.LayerCount;
              VarLabel := 'Layer ';
            end;
          vdFront:
            begin
              VarIndex := 1;
              MaxCount := Grid.RowCount;
              VarLabel := 'Row ';
            end;
          vdSide:
            begin
              VarIndex := 2;
              MaxCount := Grid.ColumnCount;
              VarLabel := 'Column ';
            end;
          else
            Assert(False);
        end;

        FoundValue := False;
        for LayRowColIndex := 0 to MaxCount do
        begin
          Indicies[VarIndex] := LayRowColIndex;
          if FSelectedScreenObject.
            IsHigher3DElevationAssigned(Indicies[2], Indicies[1], Indicies[0]) then
          begin
            Value := FSelectedScreenObject.
              Higher3DElevations[Indicies[0], Indicies[1], Indicies[2]];
            lblHigher3rdDimensionCoordinate.Caption :=
              ExtraText + DirectionText + ': ' + FloatToStr(Value)
              + ' on ' + VarLabel + IntToStr(LayRowColIndex+1);
            FoundValue := True;
            break;
          end
        end;
        if not FoundValue then
        begin
          lblHigher3rdDimensionCoordinate.Caption :=
            ExtraText + DirectionText + StrNotAssigned;
        end;
      end;
    end
    else
    begin
      lblHigher3rdDimensionCoordinate.Caption := ExtraText + DirectionText;
    end;
  end;
  procedure AssignLowerElevLabel(const ExtraText: string);
  var
    Indicies: array[0..2] of Integer;
    Grid: TCustomGrid;
    VarIndex: Integer;
    MaxCount: Integer;
    VarLabel: string;
    FoundValue: Boolean;
    LayRowColIndex: Integer;
  begin
    if cbShowThirdDValues.Checked then
    begin
      if FSelectedScreenObject.
        IsLower3DElevationAssigned(Column, Row, Layer) then
      begin
        Value := FSelectedScreenObject.
          Lower3DElevations[Layer, Row, Column];
        lblLower3rdDimensionCoordinate.Caption :=
          ExtraText + DirectionText + ': ' + FloatToStr(Value);
      end
      else
      begin
        Indicies[0] := Layer;
        Indicies[1] := Row;
        Indicies[2] := Column;

        Grid := frmGoPhast.Grid;

        VarIndex := -1;
        MaxCount := 0;
        VarLabel := '';
        case FSelectedScreenObject.ViewDirection of
          vdTop:
            begin
              VarIndex := 0;
              MaxCount := Grid.LayerCount;
              VarLabel := 'Layer ';
            end;
          vdFront:
            begin
              VarIndex := 1;
              MaxCount := Grid.RowCount;
              VarLabel := 'Row ';
            end;
          vdSide:
            begin
              VarIndex := 2;
              MaxCount := Grid.ColumnCount;
              VarLabel := 'Column ';
            end;
          else
            Assert(False);
        end;

        FoundValue := False;
        for LayRowColIndex := 0 to MaxCount do
        begin
          Indicies[VarIndex] := LayRowColIndex;
          if FSelectedScreenObject.
            IsLower3DElevationAssigned(Indicies[2], Indicies[1], Indicies[0]) then
          begin
            Value := FSelectedScreenObject.
              Lower3DElevations[Indicies[0], Indicies[1], Indicies[2]];
            lblLower3rdDimensionCoordinate.Caption :=
              ExtraText + DirectionText + ': ' + FloatToStr(Value)
              + ' on ' + VarLabel + IntToStr(LayRowColIndex+1);
            FoundValue := True;
            break;
          end
        end;
        if not FoundValue then
        begin
          lblLower3rdDimensionCoordinate.Caption :=
            ExtraText + DirectionText + StrNotAssigned;
        end;
      end;
    end
    else
    begin
      lblLower3rdDimensionCoordinate.Caption := ExtraText + DirectionText;
    end;
  end;
begin
  if (frmGoPhast.PhastModel.SelectedScreenObjectCount = 1) then
  begin
    if (FSelectedScreenObject = nil)
      or not FSelectedScreenObject.Selected then
    begin
      UpdatedSelectedObject;
    end;
    lblSelectedObject.Caption := StrSelectedObject
      + ': ' + FSelectedScreenObject.Name;

    Segment := FSelectedScreenObject.Segments.ClosestSegment(Location, 1);
    case FSelectedScreenObject.ViewDirection of
      vdTop:
        begin
          DirectionText := 'Z-coordinate';
          if (Segment <> nil) and (Segment.Col = Column)
            and (Segment.Row = Row) then
          begin
            lblVertex.Caption := StrPriorVertex + IntToStr(Segment.VertexIndex + 1);
            lblSection.Caption := 'Section ' + IntToStr(Segment.SectionIndex + 1);
          end
          else
          begin
            lblVertex.Caption := StrPriorVertex +' = ?';
            lblSection.Caption := 'Section = ?';
          end;
        end;
      vdFront:
        begin
          DirectionText := 'Y-coordinate';
          if (Segment <> nil) and (Segment.Col = Column)
            and (Segment.Layer = Layer) then
          begin
            lblVertex.Caption := StrPriorVertex + IntToStr(Segment.VertexIndex + 1);
            lblSection.Caption := 'Section ' + IntToStr(Segment.SectionIndex + 1);
          end
          else
          begin
            lblVertex.Caption := StrPriorVertex +' = ?';
            lblSection.Caption := 'Section = ?';
          end;
        end;
      vdSide:
        begin
          DirectionText := 'X-coordinate';
          if (Segment <> nil)
            and (Segment.Row = Row) and (Segment.Layer = Layer) then
          begin
            lblVertex.Caption := StrPriorVertex + IntToStr(Segment.VertexIndex + 1);
            lblSection.Caption := 'Section ' + IntToStr(Segment.SectionIndex + 1);
          end
          else
          begin
            lblVertex.Caption := StrPriorVertex +' = ?';
            lblSection.Caption := 'Section = ?';
          end;
        end;
    else
      Assert(False);
    end;


    case FSelectedScreenObject.ElevationCount of
      ecZero:
        begin
          cbShowThirdDValues.Enabled := False;
          lblHigher3rdDimensionCoordinate.Caption := '';
          lblLower3rdDimensionCoordinate.Caption := '';
        end;
      ecOne:
        begin
          cbShowThirdDValues.Enabled := True;
          lblLower3rdDimensionCoordinate.Caption := '';
          AssignHigherElevLabel('');
        end;
      ecTwo:
        begin
          cbShowThirdDValues.Enabled := True;
          AssignHigherElevLabel(StrHigher);
          AssignLowerElevLabel(StrLower);
        end;
    else
      Assert(False);
    end;
  end
  else if frmGoPhast.PhastModel.SelectedScreenObjectCount > 1 then
  begin
    lblSelectedObject.Caption := StrSelectedObject + ': (multiple objects)';
    lblHigher3rdDimensionCoordinate.Caption := StrHigher3rdDimension;
    lblLower3rdDimensionCoordinate.Caption := StrLower3rdDimension;
    lblVertex.Caption := StrPriorVertex;
    lblSection.Caption := 'Section';
  end
  else
  begin
    lblSelectedObject.Caption := StrSelectedObject + ': (none)';
    lblHigher3rdDimensionCoordinate.Caption := StrHigher3rdDimension;
    lblLower3rdDimensionCoordinate.Caption := StrLower3rdDimension;
    lblVertex.Caption := StrPriorVertex;
    lblSection.Caption := 'Section';
  end;
end;

procedure TfrmGridValue.UpdateDataSets;
var
  VirtNoneNode: PVirtualNode;
begin
  virttreecomboDataSets.Tree.Clear;
  VirtNoneNode := virttreecomboDataSets.Tree.AddChild(nil);
  virttreecomboDataSets.Tree.Selected[VirtNoneNode] := True;

  FillVirtualStringTreeWithDataSets(virttreecomboDataSets.Tree,
    FDataSetDummyObjects, nil, nil);
end;

procedure TfrmGridValue.UpdatedSelectedObject;
var
  ScreenObject: TScreenObject;
  Index: Integer;
begin
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if ScreenObject.Selected then
    begin
      FSelectedScreenObject := ScreenObject;
      break;
    end;
  end;
end;

end.
