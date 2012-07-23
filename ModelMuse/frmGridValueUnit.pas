unit frmGridValueUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ScreenObjectUnit,
  ComCtrls, DataSetUnit, VirtualTrees, FastGEO, GoPhastTypes, SsButtonEd,
  RbwStringTreeCombo, Grids, RbwDataGrid4, ExtCtrls;

type
  TPathLineColumn = (plcLabel, plcFirst, plcLast, plcClosest);
  TPathlineRow = (plrLabel, plrX, plrY, plrZ, plrXPrime, plrYPrime, plrLocalZ,
    plrTime, plrColumn, plrRow, plrLayer, plrTimeStep);
  TEndPointColumn = (epcLabel, epcStart, epcEnd);
  TEndPointRow = (eprLabel, eprZone, eprColumn, eprRow, eprLayer, eprX, eprY,
    eprZ, eprXPrime, eprYPrime, eprLocalZ, eprTimeStep);


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
    lblSelectValue: TLabel;
    edSelectValue: TEdit;
    lblSelectExplanation: TLabel;
    memoSelectExplanation: TMemo;
    btnUpdate: TButton;
    lblSection: TLabel;
    lblVertex: TLabel;
    lblModel: TLabel;
    comboModel: TComboBox;
    virttreecomboDataSets: TRbwStringTreeCombo;
    tabPathline: TTabSheet;
    rdgPathline: TRbwDataGrid4;
    tabEndPoint: TTabSheet;
    rdgEndPoints: TRbwDataGrid4;
    pnlEndPoints: TPanel;
    lbledtReleaseTime: TLabeledEdit;
    lbledtTerminationCode: TLabeledEdit;
    lbledtTrackingTime: TLabeledEdit;
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
      var CellText: string);
    procedure comboModelChange(Sender: TObject);
    procedure virttreecomboDataSetsChange(Sender: TObject);
    procedure virttreecomboDataSets1TreeGetNodeDataSize(
      Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure virttreecomboDataSets1TreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure virttreecomboDataSets1TreeInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure virttreecomboDataSetsTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
  private
    FSelectedScreenObject: TScreenObject;
    FColumn: Integer;
    FRow: Integer;
    FLayer: Integer;
    // @name is implemented as a TObjectList.
    FDataSetDummyObjects: TList;
    FSelectedVirtNode: PVirtualNode;
    FViewDirection: TViewDirection;
    FPriorLocation: TPoint2D;
    FPriorEndPointLocation: TPoint2D;
    procedure DisplayEndPointData(const Location: TPoint2D);
    procedure InitializeEndpointGrid;
    property SelectedVirtNode: PVirtualNode read FSelectedVirtNode;
    procedure UpdatedSelectedObject;
    procedure UpdateScreenObjectInfo(const Column, Row, Layer: Integer;
      Location: TPoint2D; Model: TBaseModel);
    procedure UpdateSelectedData(Layer, Row, Column: integer);
    procedure GetSelectedDataArray(var OtherDataSet: TDataArray);
    procedure InitializePathlineGrid;
    procedure DisplayPathlineData(const Location: TPoint2D);
    function DiscretizationDefined: Boolean;
    { Private declarations }
  public
    procedure UpdateValue(const Layer, Row, Column: integer;
      const DataSetName, CellValue: string; Explanation: string;
      const Location: TPoint2D; ViewDirection: TViewDirection);
    procedure UpdateDataSets;
    { Public declarations }
  end;

procedure UpdateFrmGridValue;

var
  frmGridValue: TfrmGridValue;

implementation

uses Clipbrd, CustomModflowWriterUnit, AbstractGridUnit, frmGoPhastUnit, 
  GIS_Functions, RbwParser, Contnrs, ClassificationUnit,
  PhastModelUnit, PathlineReader, QuadtreeClass, ZoomBox2, InteractiveTools,
  frmSutraLayersUnit, SutraMeshUnit;

resourcestring
  StrSelectedObject = 'Selected object';
  StrHigher3rdDimension = 'Higher 3rd dimension coordinate';
  StrLower3rdDimension = 'Lower 3rd dimension coordinate';
  StrNotAssigned = ': (not assigned)';
  StrHigher = 'Higher ';
  StrLower = 'Lower ';
  StrPriorVertex = 'Prior vertex number %s';
  StrParentModel = 'Parent model';
  StrDataSetValuesNeed = 'Data set values need to be updated.';
  StrTheMouseIsNotOve = 'The mouse is not over the grid.';
  StrTheDataSetDoesNo = 'The data set does not have a value at the current l' +
  'ocation.';
  StrLayerD = 'Layer: %d';
  StrRowD = 'Row: %d';
  StrColumn = 'Column: %d';
  StrLayerHeightG = 'Layer height: %g';
  StrRowWidthG = 'Row width: %g';
  StrColumnWidthG = 'Column width: %g';
  StrLayerHeight = 'Layer height:';
  StrRowWidth = 'Row width:';
  StrColumnWidth = 'Column width:';
  StrLayer = 'Layer';
  StrRow = 'Row';
  StrColumn1 = 'Column';
  Str0s1s2gOn = '%0:s %1:s: %2:g on %3:s %4:d';
  StrZcoordinate = 'Z-coordinate';
  StrYcoordinate = 'Y-coordinate';
  StrXcoordinate = 'X-coordinate';
  StrSection = 'Section = ?';
  StrSectionD = 'Section %d';
  StrSelectedObjectMu = 'Selected object: (multiple objects)';
  StrSection1 = 'Section';
  StrSelectedObjectNo = 'Selected object: (none)';
  StrFirst = 'First';
  StrLast = 'Last';
  StrClosest = 'Closest';
  StrX = 'X';
  StrY = 'Y';
  StrZ = 'Z';
  StrXPrime = 'X''';
  StrYPrime = 'Y''';
  StrLocalZ = 'Local Z';
  StrTime = 'Time';
  StrTimeStep = 'Time step';
  StrStart = 'Start';
  StrEnd = 'End';
  StrZone = 'Zone';

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

procedure TfrmGridValue.comboModelChange(Sender: TObject);
var
  LocalModel: TCustomModel;
begin
  inherited;
  LocalModel := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;
  if LocalModel <> nil then
  begin
    frmGoPhast.PhastModel.SelectedModel := LocalModel;
    UpdateCurrentModel(LocalModel);
  end;
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
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  inherited;
  AdjustFormPosition(dpLeft);
  FDataSetDummyObjects := TObjectList.Create;

  case frmGoPhast.ModelSelection of
    msPhast, msModflow, msModflowNWT {$IFDEF SUTRA}, msSutra {$ENDIF}:
      begin
        comboModel.Items.AddObject(StrParentModel, frmGoPhast.PhastModel)
      end;
    msModflowLGR:
      begin
        comboModel.Items.AddObject(StrParentModel, frmGoPhast.PhastModel);
        for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
        begin
          ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
          comboModel.Items.AddObject(ChildModel.ModelName, ChildModel);
        end;
      end;
    else
      Assert(False);
  end;
  comboModel.ItemIndex := comboModel.Items.IndexOfObject(
    frmGoPhast.PhastModel.SelectedModel);

  InitializePathlineGrid;
  InitializeEndpointGrid;
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
//  Assert(pcDataDisplay.ActivePageIndex = 1);
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
    memoSelectExplanation.Text := StrDataSetValuesNeed;
    btnUpdate.Enabled := True;
    Exit;
  end;

  if (Layer < 0) or (Row < 0) or (Column < 0) then
  begin
    edSelectValue.Text := '';
    memoSelectExplanation.Text := StrTheMouseIsNotOve;
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
    memoSelectExplanation.Text := StrTheDataSetDoesNo;
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
  const Location: TPoint2D; ViewDirection: TViewDirection);
var
  DataArray: TDataArray;
  EvaluatedAt: TEvaluatedAt;
  ColumnWidth: Double;
  RowWidth: Double;
  LayerHeight: Double;
  Model: TBaseModel;
//  DiscretizationDefined: Boolean;
begin
  Model := frmGoPhast.PhastModel.SelectedModel;
  FViewDirection := ViewDirection;
  FColumn := Column;
  FRow := Row;
  FLayer := Layer;
  lblLayer.Caption := Format(StrLayerD, [(Layer+1)]);
  lblRow.Caption := Format(StrRowD, [(Row+1)]);
  lblColumn.Caption := Format(StrColumn, [(Column+1)]);
  lblDataSet.Caption := DataSetName;
  edCellValue.Text := CellValue;
  if (Explanation <> StrNoValueAssigned)
    and (Pos(StrNoValueAssigned, Explanation) > 0) then
  begin
    Explanation := StringReplace(Explanation, StrNoValueAssigned, '', []);
  end;
  memoExplanation.Text := Explanation;
  if DiscretizationDefined then
  begin
    if DataSetName <> '' then
    begin
      DataArray := frmGoPhast.PhastModel.DataArrayManager.
        GetDataSetByName(DataSetName)
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

    lblLayerHeight.Caption := Format(StrLayerHeightG, [LayerHeight]);
    lblRowWidth.Caption := Format(StrRowWidthG, [RowWidth]);
    lblColumnWidth.Caption := Format(StrColumnWidthG, [ColumnWidth]);
  end
  else
  begin
    lblLayerHeight.Caption := StrLayerHeight;
    lblRowWidth.Caption := StrRowWidth;
    lblColumnWidth.Caption := StrColumnWidth;
  end;
  
  UpdateScreenObjectInfo(Column, Row, Layer, Location, Model);
  UpdateSelectedData(Layer, Row, Column);
  DisplayPathlineData(Location);
  DisplayEndPointData(Location);
end;

procedure TfrmGridValue.virttreecomboDataSetsChange(Sender: TObject);
begin
  inherited;
  UpdateTreeComboText(SelectedVirtNode, virttreecomboDataSets);
  UpdateSelectedData(FLayer, FRow, FColumn);
end;

procedure TfrmGridValue.virttreecomboDataSets1TreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

procedure TfrmGridValue.virttreecomboDataSets1TreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmGridValue.virttreecomboDataSets1TreeInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  CellText: string;
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmGridValue.virttreecomboDataSetsDropDownTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

procedure TfrmGridValue.virttreecomboDataSetsDropDownTreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmGridValue.virttreecomboDataSetsTreeChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  inherited;
  SelectOnlyLeaves(Node, virttreecomboDataSets, Sender, FSelectedVirtNode);
end;

procedure TfrmGridValue.GetSelectedDataArray(var OtherDataSet: TDataArray);
begin
  if DiscretizationDefined then
  begin
    OtherDataSet := frmGoPhast.PhastModel.SelectedModel.DataArrayManager.
      GetDataSetByName(virttreecomboDataSets.Text);
  end
  else
  begin
    OtherDataSet := nil;
  end;
end;

procedure TfrmGridValue.UpdateScreenObjectInfo
  (const Column, Row, Layer: Integer; Location: TPoint2D;
  Model: TBaseModel);
var
  Value: Double;
  DirectionText: string;
  Segment: TCellElementSegment;
  LocalModel: TCustomModel;
  temp: TFloat;
  procedure GetDirectionVariables(var VarIndex, MaxCount: Integer;
    var VarLabel: string);
  var
    Grid: TCustomModelGrid;
  {$IFDEF SUTRA}
    Mesh: TSutraMesh3D;
  {$ENDIF}
  begin
    VarIndex := -1;
    MaxCount := 0;
    VarLabel := '';
    case frmGoPhast.ModelSelection of
      msPhast, msModflow, msModflowLGR, msModflowNWT:
        begin
          Grid := frmGoPhast.Grid;

          case FSelectedScreenObject.ViewDirection of
            vdTop:
              begin
                VarIndex := 0;
                MaxCount := Grid.LayerCount;
                VarLabel := StrLayer;
              end;
            vdFront:
              begin
                VarIndex := 1;
                MaxCount := Grid.RowCount;
                VarLabel := StrRow;
              end;
            vdSide:
              begin
                VarIndex := 2;
                MaxCount := Grid.ColumnCount;
                VarLabel := StrColumn1;
              end;
            else
              Assert(False);
          end;
        end;
      {$IFDEF SUTRA}
      msSutra:
        begin
          Mesh := frmGoPhast.PhastModel.Mesh;
          case FSelectedScreenObject.ViewDirection of
            vdTop:
              begin
                VarIndex := 0;
                MaxCount := Mesh.LayerCount;
                VarLabel := StrLayer;
              end;
            vdFront:
              begin
                VarIndex := 1;
                MaxCount := 1;
                VarLabel := StrRow;
              end;
            vdSide:
              begin
                VarIndex := 2;
                MaxCount := Mesh.Mesh2D.Nodes.Count;
                VarLabel := StrColumn1;
              end;
          else
            Assert(False);
          end;
        end;
      {$ENDIF}
    else
      Assert(False);
    end;
  end;
  procedure AssignHigherElevLabel(const ExtraText: string);
  var
    Indicies: array[0..2] of Integer;
    VarIndex: Integer;
    MaxCount: Integer;
    VarLabel: string;
    FoundValue: Boolean;
    LayRowColIndex: Integer;
  begin
    if cbShowThirdDValues.Checked then
    begin
      if FSelectedScreenObject.
        IsHigher3DElevationAssigned(Column, Row, Layer, LocalModel) then
      begin
        Value := FSelectedScreenObject.
          Higher3DElevations[LocalModel][Layer, Row, Column];
        lblHigher3rdDimensionCoordinate.Caption :=
          ExtraText + DirectionText + ': ' + FloatToStr(Value);
      end
      else
      begin
        Indicies[0] := Layer;
        Indicies[1] := Row;
        Indicies[2] := Column;
        GetDirectionVariables(VarIndex, MaxCount, VarLabel);
//        VarIndex := -1;
//        MaxCount := 0;
//        VarLabel := '';
//        case frmGoPhast.ModelSelection of
//          msPhast, msModflow, msModflowLGR, msModflowNWT:
//            begin
//              Grid := frmGoPhast.Grid;
//
//              case FSelectedScreenObject.ViewDirection of
//                vdTop:
//                  begin
//                    VarIndex := 0;
//                    MaxCount := Grid.LayerCount;
//                    VarLabel := StrLayer;
//                  end;
//                vdFront:
//                  begin
//                    VarIndex := 1;
//                    MaxCount := Grid.RowCount;
//                    VarLabel := StrRow;
//                  end;
//                vdSide:
//                  begin
//                    VarIndex := 2;
//                    MaxCount := Grid.ColumnCount;
//                    VarLabel := StrColumn1;
//                  end;
//                else
//                  Assert(False);
//              end;
//            end;
//          {$IFDEF SUTRA}
//          msSutra:
//            begin
//              Mesh := frmGoPhast.PhastModel.SutraMesh;
//              case FSelectedScreenObject.ViewDirection of
//                vdTop:
//                  begin
//                    VarIndex := 0;
//                    MaxCount := Mesh.LayerCount;
//                    VarLabel := StrLayer;
//                  end;
//                vdFront:
//                  begin
//                    VarIndex := 1;
//                    MaxCount := 1;
//                    VarLabel := StrRow;
//                  end;
//                vdSide:
//                  begin
//                    VarIndex := 2;
//                    MaxCount := Mesh.Mesh2D.Nodes.Count;
//                    VarLabel := StrColumn1;
//                  end;
//              else
//                Assert(False);
//              end;
//            end;
//          {$ENDIF}
//        else
//          Assert(False);
//        end;


        FoundValue := False;
        for LayRowColIndex := 0 to MaxCount do
        begin
          Indicies[VarIndex] := LayRowColIndex;
          if FSelectedScreenObject.
            IsHigher3DElevationAssigned(Indicies[2], Indicies[1],
            Indicies[0], LocalModel) then
          begin
            Value := FSelectedScreenObject.
              Higher3DElevations[LocalModel][Indicies[0], Indicies[1], Indicies[2]];
            lblHigher3rdDimensionCoordinate.Caption := Format(Str0s1s2gOn,
              [ExtraText, DirectionText, Value, VarLabel, LayRowColIndex+1]);
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
    VarIndex: Integer;
    MaxCount: Integer;
    VarLabel: string;
    FoundValue: Boolean;
    LayRowColIndex: Integer;
  begin
    if cbShowThirdDValues.Checked then
    begin
      if FSelectedScreenObject.
        IsLower3DElevationAssigned(Column, Row, Layer, LocalModel) then
      begin
        Value := FSelectedScreenObject.
          Lower3DElevations[LocalModel][Layer, Row, Column];
        lblLower3rdDimensionCoordinate.Caption :=
          ExtraText + DirectionText + ': ' + FloatToStr(Value);
      end
      else
      begin
        Indicies[0] := Layer;
        Indicies[1] := Row;
        Indicies[2] := Column;

        GetDirectionVariables(VarIndex, MaxCount, VarLabel);
//        Grid := frmGoPhast.Grid;
//
//        VarIndex := -1;
//        MaxCount := 0;
//        VarLabel := '';
//        case FSelectedScreenObject.ViewDirection of
//          vdTop:
//            begin
//              VarIndex := 0;
//              MaxCount := Grid.LayerCount;
//              VarLabel := StrLayer;
//            end;
//          vdFront:
//            begin
//              VarIndex := 1;
//              MaxCount := Grid.RowCount;
//              VarLabel := StrRow;
//            end;
//          vdSide:
//            begin
//              VarIndex := 2;
//              MaxCount := Grid.ColumnCount;
//              VarLabel := StrColumn1;
//            end;
//          else
//            Assert(False);
//        end;

        FoundValue := False;
        for LayRowColIndex := 0 to MaxCount do
        begin
          Indicies[VarIndex] := LayRowColIndex;
          if FSelectedScreenObject.
            IsLower3DElevationAssigned(Indicies[2], Indicies[1],
            Indicies[0], LocalModel) then
          begin
            Value := FSelectedScreenObject.
              Lower3DElevations[LocalModel][Indicies[0], Indicies[1], Indicies[2]];
            lblLower3rdDimensionCoordinate.Caption := Format(Str0s1s2gOn,
              [ExtraText, DirectionText, Value, VarLabel, LayRowColIndex+1]);
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
  LocalModel := frmGoPhast.PhastModel.SelectedModel;
  if (frmGoPhast.PhastModel.SelectedScreenObjectCount = 1) then
  begin
    if (FSelectedScreenObject = nil)
      or not FSelectedScreenObject.Selected then
    begin
      UpdatedSelectedObject;
    end;
    lblSelectedObject.Caption := StrSelectedObject
      + ': ' + FSelectedScreenObject.Name;

    if FViewDirection = FSelectedScreenObject.ViewDirection then
    begin
      if FViewDirection = vdSide then
      begin
        temp := Location.x;
        Location.x := Location.y;
        Location.y := temp;
      end;
      Segment := FSelectedScreenObject.Segments[Model].ClosestSegment(Location, 1);
      case FSelectedScreenObject.ViewDirection of
        vdTop:
          begin
            DirectionText := StrZcoordinate;
            if (Segment <> nil) and (Segment.Col = Column)
              and (Segment.Row = Row) then
            begin
              lblVertex.Caption := Format(StrPriorVertex,
                ['= ' + IntToStr(Segment.VertexIndex + 1)]);
              lblSection.Caption := Format(StrSectionD, [Segment.SectionIndex + 1]);
            end
            else
            begin
              lblVertex.Caption := Format(StrPriorVertex, ['= ?']);
              lblSection.Caption := StrSection;
            end;
          end;
        vdFront:
          begin
            DirectionText := StrYcoordinate;
            if (Segment <> nil) and (Segment.Col = Column)
              and (Segment.Layer = Layer) then
            begin
              lblVertex.Caption := Format(StrPriorVertex,
                ['= ' + IntToStr(Segment.VertexIndex + 1)]);
              lblSection.Caption := Format(StrSectionD, [Segment.SectionIndex + 1]);
            end
            else
            begin
              lblVertex.Caption := Format(StrPriorVertex, ['= ?']);
              lblSection.Caption := StrSection;
            end;
          end;
        vdSide:
          begin
            DirectionText := StrXcoordinate;
            if (Segment <> nil)
              and (Segment.Row = Row) and (Segment.Layer = Layer) then
            begin
              lblVertex.Caption := Format(StrPriorVertex,
                ['= ' + IntToStr(Segment.VertexIndex + 1)]);
              lblSection.Caption := Format(StrSectionD, [Segment.SectionIndex + 1]);
            end
            else
            begin
              lblVertex.Caption := Format(StrPriorVertex, ['= ?']);
              lblSection.Caption := StrSection;
            end;
          end;
      else
        Assert(False);
      end;
    end
    else
    begin
      lblVertex.Caption := '';
      lblSection.Caption := '';
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
    lblSelectedObject.Caption := StrSelectedObjectMu;
    lblHigher3rdDimensionCoordinate.Caption := StrHigher3rdDimension;
    lblLower3rdDimensionCoordinate.Caption := StrLower3rdDimension;
    lblVertex.Caption := Format(StrPriorVertex, ['']);
    lblSection.Caption := StrSection1;
  end
  else
  begin
    lblSelectedObject.Caption := StrSelectedObjectNo;
    lblHigher3rdDimensionCoordinate.Caption := StrHigher3rdDimension;
    lblLower3rdDimensionCoordinate.Caption := StrLower3rdDimension;
    lblVertex.Caption := Format(StrPriorVertex, ['']);
    lblSection.Caption := StrSection1;
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

function TfrmGridValue.DiscretizationDefined: Boolean;
var
  Grid: TCustomModelGrid;
  {$IFDEF SUTRA}
  Mesh: TSutraMesh3D;
  {$ENDIF}
begin
  result := False;
  case frmGoPhast.ModelSelection of
    msPhast, msModflow, msModflowLGR, msModflowNWT:
      begin
        Grid := frmGoPhast.Grid;
        result := (Grid <> nil) and (Grid.LayerCount >= 1)
          and (Grid.RowCount >= 1) and (Grid.ColumnCount >= 1);
      end;
    {$IFDEF SUTRA}
    msSutra:
      begin
        Mesh := frmGoPhast.PhastModel.Mesh;
        result := (Mesh <> nil) and (Mesh.Mesh2D.Nodes.Count > 0);
      end;
    {$ENDIF}
  else
    Assert(False);
  end;
end;

procedure TfrmGridValue.DisplayEndPointData(const Location: TPoint2D);
var
  AnEndPoint: TEndPoint;
  APointer: Pointer;
  Y: TFloat;
  X: TFloat;
  EndPointQuadTree: TRbwQuadTree;
  EndPoints: TEndPointReader;
  DisplayPoint: Boolean;
  ZoomBox: TQRbwZoomBox2;
//  ColIndex: Integer;
  RowIndex: Integer;
  ALayer: Integer;
  ColIndex: Integer;
begin
  if (FPriorEndPointLocation.x = Location.x)
    and (FPriorEndPointLocation.y = Location.Y)then
  begin
    Exit;
  end;
  {$IFDEF SUTRA}
  if frmGoPhast.ModelSelection = msSutra then
  begin
    tabEndPoint.TabVisible := False;
    Exit;
  end;
  {$ENDIF}
  FPriorEndPointLocation := Location;
  EndPoints := frmGoPhast.PhastModel.EndPoints;
  EndPointQuadTree := nil;
  if EndPoints.Visible then
  begin
    case FViewDirection of
      vdTop:
        EndPointQuadTree := EndPoints.TopQuadTree;
      vdFront:
        EndPointQuadTree := EndPoints.FrontQuadTree;
      vdSide:
        EndPointQuadTree := EndPoints.SideQuadTree;
    else
      Assert(False);
    end;
  end;
  tabEndPoint.TabVisible := EndPoints.Visible and (EndPointQuadTree.Count > 0);
  if tabEndPoint.TabVisible then
  begin
    X := Location.X;
    Y := Location.Y;
    EndPointQuadTree.FirstNearestPoint(X, Y, APointer);
    AnEndPoint := APointer;
    Assert(AnEndPoint <> nil);



    DisplayPoint := False;

    case EndPoints.DisplayLimits.WhereToPlot of
      wtpStart:
        begin
          case FViewDirection of
            vdTop:
              begin
                DisplayPoint := (Abs(FColumn+1 - AnEndPoint.StartColumn) <= 1)
                  and (Abs(FRow+1 - AnEndPoint.StartRow) <= 1);
              end;
            vdFront:
              begin
                ALayer := frmGoPhast.PhastModel.
                  ModflowLayerToDataSetLayer(AnEndPoint.StartLayer);
                DisplayPoint := (Abs(FColumn+1 - AnEndPoint.StartColumn) <= 1)
                  and (Abs(FLayer - ALayer) <= 1);
              end;
            vdSide:
              begin
                ALayer := frmGoPhast.PhastModel.
                  ModflowLayerToDataSetLayer(AnEndPoint.StartLayer);
                DisplayPoint := (Abs(FLayer - ALayer) <= 1)
                  and (Abs(FRow+1 - AnEndPoint.StartRow) <= 1);
              end;
            else Assert(False);
          end;
        end;
      wtpEnd:
        begin
          case FViewDirection of
            vdTop:
              begin
                DisplayPoint := (Abs(FColumn+1 - AnEndPoint.EndColumn) <= 1)
                  and (Abs(FRow+1 - AnEndPoint.EndRow) <= 1);
              end;
            vdFront:
              begin
                ALayer := frmGoPhast.PhastModel.
                  ModflowLayerToDataSetLayer(AnEndPoint.EndLayer);
                DisplayPoint := (Abs(FColumn+1 - AnEndPoint.EndColumn) <= 1)
                  and (Abs(FLayer - ALayer) <= 1);
              end;
            vdSide:
              begin
                ALayer := frmGoPhast.PhastModel.
                  ModflowLayerToDataSetLayer(AnEndPoint.EndLayer);
                DisplayPoint := (Abs(FLayer - ALayer) <= 1)
                  and (Abs(FRow+1 - AnEndPoint.endRow) <= 1);
              end;
            else Assert(False);
          end;
        end;
      else
        Assert(False);
    end;


    if not DisplayPoint then
    begin
      ZoomBox := nil;
      case FViewDirection of
        vdTop: ZoomBox := frmGoPhast.frameTopView.ZoomBox;
        vdFront: ZoomBox := frmGoPhast.frameFrontView.ZoomBox;
        vdSide: ZoomBox := frmGoPhast.framesideView.ZoomBox;
        else Assert(False);
      end;
      DisplayPoint :=
        (Abs(ZoomBox.XCoord(X) - ZoomBox.XCoord(Location.X)) <= SelectionWidth)
        and (Abs(ZoomBox.YCoord(Y) - ZoomBox.YCoord(Location.Y)) <= SelectionWidth);
    end;

    if DisplayPoint then
    begin
      lbledtReleaseTime.Text := FloatToStr(AnEndPoint.ReleaseTime);
      lbledtTerminationCode.Text := IntToStr(AnEndPoint.TerminationCode);
      lbledtTrackingTime.Text := FloatToStr(AnEndPoint.TrackingTime);

      rdgEndPoints.BeginUpdate;
      try
        begin
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprZone)] :=
            IntToStr(AnEndPoint.StartZoneCode);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprColumn)] :=
            IntToStr(AnEndPoint.StartColumn);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprRow)] :=
            IntToStr(AnEndPoint.StartRow);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprLayer)] :=
            IntToStr(AnEndPoint.StartLayer);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprX)] :=
            FloatToStr(AnEndPoint.StartX);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprY)] :=
            FloatToStr(AnEndPoint.StartY);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprZ)] :=
            FloatToStr(AnEndPoint.StartZ);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprXPrime)] :=
            FloatToStr(AnEndPoint.StartXPrime);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprYPrime)] :=
            FloatToStr(AnEndPoint.StartYPrime);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprLocalZ)] :=
            FloatToStr(AnEndPoint.StartLocalZ);
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprTimeStep)] :=
            IntToStr(AnEndPoint.StartTimeStep);

          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprZone)] :=
            IntToStr(AnEndPoint.EndZoneCode);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprColumn)] :=
            IntToStr(AnEndPoint.EndColumn);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprRow)] :=
            IntToStr(AnEndPoint.EndRow);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprLayer)] :=
            IntToStr(AnEndPoint.EndLayer);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprX)] :=
            FloatToStr(AnEndPoint.EndX);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprY)] :=
            FloatToStr(AnEndPoint.EndY);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprZ)] :=
            FloatToStr(AnEndPoint.EndZ);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprXPrime)] :=
            FloatToStr(AnEndPoint.EndXPrime);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprYPrime)] :=
            FloatToStr(AnEndPoint.EndYPrime);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprLocalZ)] :=
            FloatToStr(AnEndPoint.EndLocalZ);
          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprTimeStep)] :=
            IntToStr(AnEndPoint.EndTimeStep);
        end;
      finally
        rdgEndPoints.EndUpdate;
      end;
    end
    else
    begin
      rdgEndPoints.BeginUpdate;
      try
        for ColIndex := 1 to rdgEndPoints.ColCount - 1 do
        begin
          for RowIndex := 1 to rdgEndPoints.RowCount - 1 do
          begin
            rdgEndPoints.Cells[ColIndex, RowIndex] := '';
          end;
        end;
      finally
        rdgEndPoints.EndUpdate;
      end;
    end;
  end;
end;

procedure TfrmGridValue.DisplayPathlineData(const Location: TPoint2D);
var
  PathLinePoint: TPathLinePoint;
  APointer: Pointer;
  Y: TFloat;
  X: TFloat;
  PathQuadTree: TRbwQuadTree;
  PathLines: TPathLineReader;
  DisplayPoint: Boolean;
  ZoomBox: TQRbwZoomBox2;
  ColIndex: Integer;
  RowIndex: Integer;
  PathLine: TPathLine;
  FirstPoint: TPathLinePoint;
  LastPoint: TPathLinePoint;
  List: TList;
  APathLinePoint: TPathLinePoint;
  ALayer: Integer;
begin
  if (FPriorLocation.x = Location.x)
    and (FPriorLocation.y = Location.Y)then
  begin
    Exit;
  end;
  {$IFDEF SUTRA}
  if frmGoPhast.ModelSelection = msSutra then
  begin
    tabPathline.TabVisible := False;
    Exit;
  end;
  {$ENDIF}
  FPriorLocation := Location;
  PathLines := frmGoPhast.PhastModel.PathLines;
  PathQuadTree := nil;
  if PathLines.Visible then
  begin
    case FViewDirection of
      vdTop:
        PathQuadTree := PathLines.TopQuadTree;
      vdFront:
        PathQuadTree := PathLines.FrontQuadTree;
      vdSide:
        PathQuadTree := PathLines.SideQuadTree;
    else
      Assert(False);
    end;
  end;
  tabPathline.TabVisible := PathLines.Visible and (PathQuadTree.Count > 0);
  if tabPathline.TabVisible then
  begin
    X := Location.X;
    Y := Location.Y;
    PathQuadTree.FirstNearestPoint(X, Y, APointer);
    PathLinePoint := APointer;
    Assert(PathLinePoint <> nil);

    DisplayPoint := False;
    case FViewDirection of
      vdTop:
        begin
          DisplayPoint := (Abs(FColumn+1 - PathLinePoint.Column) <= 1)
            and (Abs(FRow+1 - PathLinePoint.Row) <= 1);
        end;
      vdFront:
        begin
          ALayer := frmGoPhast.PhastModel.
            ModflowLayerToDataSetLayer(PathLinePoint.Layer);
          DisplayPoint := (Abs(FColumn+1 - PathLinePoint.Column) <= 1)
            and (Abs(FLayer - ALayer) <= 1);
        end;
      vdSide:
        begin
          ALayer := frmGoPhast.PhastModel.
            ModflowLayerToDataSetLayer(PathLinePoint.Layer);
          DisplayPoint := (Abs(FLayer - ALayer) <= 1)
            and (Abs(FRow+1 - PathLinePoint.Row) <= 1);
        end;
      else Assert(False);
    end;

    if not DisplayPoint then
    begin
      ZoomBox := nil;
      case FViewDirection of
        vdTop: ZoomBox := frmGoPhast.frameTopView.ZoomBox;
        vdFront: ZoomBox := frmGoPhast.frameFrontView.ZoomBox;
        vdSide: ZoomBox := frmGoPhast.framesideView.ZoomBox;
        else Assert(False);
      end;
      DisplayPoint :=
        (Abs(ZoomBox.XCoord(X) - ZoomBox.XCoord(Location.X)) <= SelectionWidth)
        and (Abs(ZoomBox.YCoord(Y) - ZoomBox.YCoord(Location.Y)) <= SelectionWidth);
    end;

    if DisplayPoint then
    begin
      PathLine := PathLinePoint.ParentLine;
      FirstPoint :=PathLine.Points[0];
      LastPoint :=PathLine.Points[PathLine.Points.Count -1];
      List := TList.Create;
      try
        List.Add(nil);
        List.Add(FirstPoint);
        List.Add(LastPoint);
        List.Add(PathLinePoint);
        rdgPathline.BeginUpdate;
        try
          for ColIndex := Ord(plcFirst) to Ord(plcClosest) do
          begin
            APathLinePoint := List[ColIndex];
            rdgPathline.Cells[ColIndex, Ord(plrX)] :=
              FloatToStr(APathLinePoint.X);
            rdgPathline.Cells[ColIndex, Ord(plrY)] :=
              FloatToStr(APathLinePoint.Y);
            rdgPathline.Cells[ColIndex, Ord(plrZ)] :=
              FloatToStr(APathLinePoint.Z);
            rdgPathline.Cells[ColIndex, Ord(plrXPrime)] :=
              FloatToStr(APathLinePoint.XPrime);
            rdgPathline.Cells[ColIndex, Ord(plrYPrime)] :=
              FloatToStr(APathLinePoint.YPrime);
            rdgPathline.Cells[ColIndex, Ord(plrLocalZ)] :=
              FloatToStr(APathLinePoint.LocalZ);
            rdgPathline.Cells[ColIndex, Ord(plrTime)] :=
              FloatToStr(APathLinePoint.AbsoluteTime);
            rdgPathline.Cells[ColIndex, Ord(plrColumn)] :=
              IntToStr(APathLinePoint.Column);
            rdgPathline.Cells[ColIndex, Ord(plrRow)] :=
              IntToStr(APathLinePoint.Row);
            rdgPathline.Cells[ColIndex, Ord(plrLayer)] :=
              IntToStr(APathLinePoint.Layer);
            rdgPathline.Cells[ColIndex, Ord(plrTimeStep)] :=
              IntToStr(APathLinePoint.TimeStep);
          end;
        finally
          rdgPathline.EndUpdate;
        end;
      finally
        List.Free;
      end;
    end
    else
    begin
      rdgPathline.BeginUpdate;
      try
        for ColIndex := 1 to rdgPathline.ColCount - 1 do
        begin
          for RowIndex := 1 to rdgPathline.RowCount - 1 do
          begin
            rdgPathline.Cells[ColIndex, RowIndex] := '';
          end;
        end;
      finally
        rdgPathline.EndUpdate;
      end;
    end;
  end;
end;

procedure TfrmGridValue.InitializePathlineGrid;
begin
  rdgPathline.Cells[Ord(plcFirst), 0] := StrFirst;
  rdgPathline.Cells[Ord(plcLast), 0] := StrLast;
  rdgPathline.Cells[Ord(plcClosest), 0] := StrClosest;
  rdgPathline.Cells[0, Ord(plrX)] := StrX;
  rdgPathline.Cells[0, Ord(plrY)] := StrY;
  rdgPathline.Cells[0, Ord(plrZ)] := StrZ;
  rdgPathline.Cells[0, Ord(plrXPrime)] := StrXPrime;
  rdgPathline.Cells[0, Ord(plrYPrime)] := StrYPrime;
  rdgPathline.Cells[0, Ord(plrLocalZ)] := StrLocalZ;
  rdgPathline.Cells[0, Ord(plrTime)] := StrTime;
  rdgPathline.Cells[0, Ord(plrColumn)] := StrColumn1;
  rdgPathline.Cells[0, Ord(plrRow)] := StrRow;
  rdgPathline.Cells[0, Ord(plrLayer)] := StrLayer;
  rdgPathline.Cells[0, Ord(plrTimeStep)] := StrTimeStep;
end;

procedure TfrmGridValue.InitializeEndpointGrid;
begin
  rdgEndPoints.Cells[Ord(epcStart), 0] := StrStart;
  rdgEndPoints.Cells[Ord(epcEnd), 0] := StrEnd;
  rdgEndPoints.Cells[0, Ord(eprZone)] := StrZone;
  rdgEndPoints.Cells[0, Ord(eprX)] := StrX;
  rdgEndPoints.Cells[0, Ord(eprY)] := StrY;
  rdgEndPoints.Cells[0, Ord(eprZ)] := StrZ;
  rdgEndPoints.Cells[0, Ord(eprXPrime)] := StrXPrime;
  rdgEndPoints.Cells[0, Ord(eprYPrime)] := StrYPrime;
  rdgEndPoints.Cells[0, Ord(eprLocalZ)] := StrLocalZ;
  rdgEndPoints.Cells[0, Ord(eprColumn)] := StrColumn1;
  rdgEndPoints.Cells[0, Ord(eprRow)] := StrRow;
  rdgEndPoints.Cells[0, Ord(eprLayer)] := StrLayer;
  rdgEndPoints.Cells[0, Ord(eprTimeStep)] := StrTimeStep;
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
