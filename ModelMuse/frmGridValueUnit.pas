unit frmGridValueUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ScreenObjectUnit,
  ComCtrls, DataSetUnit, VirtualTrees, FastGEO, GoPhastTypes, SsButtonEd,
  RbwStringTreeCombo, Grids, RbwDataGrid4, ExtCtrls, JvExExtCtrls,
  JvExtComponent, JvRollOut;

type
  TPathLineColumn = (plcLabel, plcFirst, plcLast, plcClosest);
  TPathlineRow = (plrLabel, plrNumber, plrX, plrY, plrZ, plrXPrime, plrYPrime, plrLocalZ,
    plrTime, plrColumn, plrRow, plrLayer, plrTimeStep, plrGroup);
  TEndPointColumn = (epcLabel, epcStart, epcEnd);
  TEndPointRow = (eprLabel, eprNumber, eprZone, eprColumn, eprRow, eprLayer, eprX, eprY,
    eprZ, eprXPrime, eprYPrime, eprLocalZ, eprTimeStep, eprParticleGroup);


  TfrmGridValue = class(TfrmCustomGoPhast)
    btnHelp: TBitBtn;
    btnClose: TBitBtn;
    pnlTabs: TPanel;
    jvrltCurrentData: TJvRollOut;
    lblModel: TLabel;
    comboModel: TComboBox;
    lblLayer: TLabel;
    lblLayerHeight: TLabel;
    lblRow: TLabel;
    lblRowWidth: TLabel;
    lblColumn: TLabel;
    lblColumnWidth: TLabel;
    lblDataSet: TLabel;
    lblCellValue: TLabel;
    edCellValue: TEdit;
    lblExplanation: TLabel;
    memoExplanation: TMemo;
    lblSelectedObject: TLabel;
    lblVertex: TLabel;
    lblSection: TLabel;
    cbShowThirdDValues: TCheckBox;
    lblHigher3rdDimensionCoordinate: TLabel;
    lblLower3rdDimensionCoordinate: TLabel;
    jvrltAllDataSets: TJvRollOut;
    jvrltPathline: TJvRollOut;
    jvrltEndPoint: TJvRollOut;
    virttreecomboDataSets: TRbwStringTreeCombo;
    lblSelectValue: TLabel;
    edSelectValue: TEdit;
    lblSelectExplanation: TLabel;
    memoSelectExplanation: TMemo;
    btnUpdate: TButton;
    rdgPathline: TRbwDataGrid4;
    pnlEndPoints: TPanel;
    lbledtReleaseTime: TLabeledEdit;
    lbledtTerminationCode: TLabeledEdit;
    lbledtTrackingTime: TLabeledEdit;
    rdgEndPoints: TRbwDataGrid4;
    spl1: TSplitter;
    pnlPathLength: TPanel;
    lblLength: TLabel;
    edLength: TEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject); override;
    procedure edCellValueKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure memoExplanationKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnUpdateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure FormShow(Sender: TObject);
    procedure comboModelChange(Sender: TObject);
    procedure virttreecomboDataSetsChange(Sender: TObject);
    procedure virttreecomboDataSetsTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure virttreecomboDataSetsTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure virttreecomboDataSetsTreeInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure virttreecomboDataSetsTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure jvrltEndPointCollapse(Sender: TObject);
    procedure jvrltEndPointExpand(Sender: TObject);
    procedure jvrltPathlineCollapse(Sender: TObject);
    procedure jvrltPathlineExpand(Sender: TObject);
    procedure FormResize(Sender: TObject);
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
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure UpdateScreenObjectInfo(const Column, Row, Layer: Integer;
      Location: TPoint2D; Model: TBaseModel);
    procedure UpdateSelectedData(Layer, Row, Column: integer);
    procedure GetSelectedDataArray(var OtherDataSet: TDataArray);
    procedure InitializePathlineGrid;
    procedure DisplayPathlineData(const Location: TPoint2D);
    function DiscretizationDefined: Boolean;
    procedure GetWidthForModpathPanels(var AvailableWidth: Integer);
    { Private declarations }
  public
    procedure UpdateValue(const Layer, Row, Column: integer;
      const DataSetName, CellValue: string; Explanation: string;
      const Location: TPoint2D; ViewDirection: TViewDirection;
      EvaluatedAt: TEvaluatedAt);
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
  frmSutraLayersUnit, SutraMeshUnit, UndoItems;

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
  StrGroup = 'Group';
  StrNumber = 'Number';
  StrElementD = 'Element: %d';
  StrNodeD = 'Node: %d';
  StrYouMustColorTheG = 'You must color the grid with a data set evaluated a' +
  't %s to see this data set''s values.';
  StrElements = 'elements';
  StrNodes = 'nodes';

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
    msPhast, msModflow, msModflowNWT
      {$IFDEF FMP}, msModflowFmp {$ENDIF}
      , msModflowCfp, msSutra22:
      begin
        comboModel.Items.AddObject(StrParentModel, frmGoPhast.PhastModel)
      end;
    msModflowLGR, msModflowLGR2:
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

procedure TfrmGridValue.FormResize(Sender: TObject);
begin
  inherited;
    rdgPathline.Width := jvrltPathline.ClientWidth;
end;

procedure TfrmGridValue.FormShow(Sender: TObject);
begin
  inherited;
  UpdateDataSets;
  // virttreecomboDataSets thows an exception if it is not on the
  // active page when TfrmGridValue is created.
//  Assert(pcDataDisplay.ActivePageIndex = 1);
  jvrltCurrentData.Collapsed := False;
  jvrltAllDataSets.Collapsed := True;
  jvrltPathline.Collapsed := True;
  jvrltEndPoint.Collapsed := True;

//  pcDataDisplay.ActivePageIndex := 0;
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

  if GlobalEvaluatedAt <> OtherDataSet.EvaluatedAt then
  begin
    edSelectValue.Text := '';
    case GlobalEvaluatedAt of
      eaBlocks:
        begin
          memoSelectExplanation.Text := Format(StrYouMustColorTheG, [StrElements]);
        end;
      eaNodes:
        begin
          memoSelectExplanation.Text := Format(StrYouMustColorTheG, [StrNodes]);
        end;
      else Assert(False);
    end;

    Exit;
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
  const Location: TPoint2D; ViewDirection: TViewDirection;
  EvaluatedAt: TEvaluatedAt);
var
  DataArray: TDataArray;

  ColumnWidth: Double;
  RowWidth: Double;
  LayerHeight: Double;
  Model: TBaseModel;
  Mesh: TSutraMesh3D;
  AnElement3D: TSutraElement3D;
  ANode3D: TSutraNode3D;
  AnElement2D: TSutraElement2D;
  ANode2D: TSutraNode2D;
//  DiscretizationDefined: Boolean;
begin
  Model := frmGoPhast.PhastModel.SelectedModel;
  FViewDirection := ViewDirection;
  FColumn := Column;
  FRow := Row;
  FLayer := Layer;
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
    if DataArray <> nil then
//    begin
//      EvaluatedAt := eaBlocks;
//    end
//    else
    begin
      EvaluatedAt := DataArray.EvaluatedAt;
    end;

    if Model.ModelSelection <> msSutra22 then
    begin
      lblLayer.Caption := Format(StrLayerD, [(Layer+1)]);
      lblRow.Caption := Format(StrRowD, [(Row+1)]);
      lblColumn.Caption := Format(StrColumn, [(Column+1)]);
    end
    else
    begin
      lblLayer.Caption := Format(StrLayerD, [(Layer+1)]);
      lblColumn.Caption := '';
      Mesh := (Model as TPhastModel).SutraMesh;
      if Mesh.MeshType = mt3D then
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              AnElement3D := Mesh.ElementArray[Layer,Column];
              lblRow.Caption := Format(StrElementD, [AnElement3D.ElementNumber+1]);
            end;
          eaNodes:
            begin
              ANode3D := Mesh.NodeArray[Layer,Column];
              lblRow.Caption := Format(StrNodeD, [ANode3D.Number+1]);
            end;
          else Assert(False);
        end;
      end
      else
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              AnElement2D := Mesh.Mesh2D.Elements[Column];
              lblRow.Caption := Format(StrElementD, [AnElement2D.ElementNumber+1]);
            end;
          eaNodes:
            begin
              ANode2D := Mesh.Mesh2D.Nodes[Column];
              lblRow.Caption := Format(StrNodeD, [ANode2D.Number+1]);
            end;
          else Assert(False);
        end;
      end;
    end;
    GlobalEvaluatedAt := EvaluatedAt;
    ColumnWidth := GetColumnWidth(Column);
    RowWidth := GetRowWidth(Row);
    LayerHeight := GetLayerHeight(Column, Row, Layer);

    lblLayerHeight.Caption := Format(StrLayerHeightG, [LayerHeight]);
    if Model.ModelSelection <> msSutra22 then
    begin
      lblRowWidth.Caption := Format(StrRowWidthG, [RowWidth]);
      lblColumnWidth.Caption := Format(StrColumnWidthG, [ColumnWidth]);
    end
    else
    begin
      lblRowWidth.Caption := '';
      lblColumnWidth.Caption := '';
    end;
  end
  else
  begin
    lblLayer.Caption := Format(StrLayerD, [(Layer+1)]);
    lblRow.Caption := Format(StrRowD, [(Row+1)]);
    lblColumn.Caption := Format(StrColumn, [(Column+1)]);
    lblLayerHeight.Caption := StrLayerHeight;
    if Model.ModelSelection <> msSutra22 then
    begin
      lblRowWidth.Caption := StrRowWidth;
      lblColumnWidth.Caption := StrColumnWidth;
    end
    else
    begin
      lblRowWidth.Caption := '';
      lblColumnWidth.Caption := '';
    end;
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

procedure TfrmGridValue.virttreecomboDataSetsTreeChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  inherited;
  SelectOnlyLeaves(Node, virttreecomboDataSets, Sender, FSelectedVirtNode);
end;

procedure TfrmGridValue.virttreecomboDataSetsTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

procedure TfrmGridValue.virttreecomboDataSetsTreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmGridValue.virttreecomboDataSetsTreeInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  CellText: string;
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
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
  ASegment: TCellElementSegment;
  LocalModel: TCustomModel;
  temp: TFloat;
//  LocalAnisotropy: double;
  Segments: TCellElementSegmentList;
  SegmentIndex: Integer;
  procedure GetDirectionVariables(var VarIndex, MaxCount: Integer;
    var VarLabel: string);
  var
    Grid: TCustomModelGrid;
    Mesh: TSutraMesh3D;
  begin
    VarIndex := -1;
    MaxCount := 0;
    VarLabel := '';
    case frmGoPhast.ModelSelection of
      msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT
        {$IFDEF FMP}, msModflowFmp {$ENDIF}
        , msModflowCfp:
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
      msSutra22:
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
    else
      Assert(False);
    end;
  end;
  procedure AssignHigherElevLabel(const ExtraText: string);
  var
    Indices: array[0..2] of Integer;
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
        Indices[0] := Layer;
        Indices[1] := Row;
        Indices[2] := Column;
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
          Indices[VarIndex] := LayRowColIndex;
          if FSelectedScreenObject.
            IsHigher3DElevationAssigned(Indices[2], Indices[1],
            Indices[0], LocalModel) then
          begin
            Value := FSelectedScreenObject.
              Higher3DElevations[LocalModel][Indices[0], Indices[1], Indices[2]];
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
    Indices: array[0..2] of Integer;
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
        Indices[0] := Layer;
        Indices[1] := Row;
        Indices[2] := Column;

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
          Indices[VarIndex] := LayRowColIndex;
          if FSelectedScreenObject.
            IsLower3DElevationAssigned(Indices[2], Indices[1],
            Indices[0], LocalModel) then
          begin
            Value := FSelectedScreenObject.
              Lower3DElevations[LocalModel][Indices[0], Indices[1], Indices[2]];
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
//      case FViewDirection of
//        vdTop: LocalAnisotropy := 1;
//        vdFront: LocalAnisotropy := TUndoVerticalExaggeration.GetOldVE;
//        vdSide: LocalAnisotropy := 1/TUndoVerticalExaggeration.GetOldVE;
//        else Assert(False);
//      end;

      Segment := nil;
      Segments := FSelectedScreenObject.Segments[Model];
      for SegmentIndex := 0 to Segments.Count - 1 do
      begin
        ASegment := Segments[SegmentIndex];
        case FSelectedScreenObject.ViewDirection of
          vdTop:
            begin
              if (ASegment.Col = Column)
                and (ASegment.Row = Row) then
              begin
                Segment := ASegment;
                break;
              end;
            end;
          vdFront:
            begin
              if (ASegment.Col = Column)
                and (ASegment.Layer = Layer) then
              begin
                Segment := ASegment;
                break;
              end;
            end;
          vdSide:
            begin
              if (ASegment.Row = Row) and (ASegment.Layer = Layer) then
              begin
                Segment := ASegment;
                break;
              end
            end;
          else Assert(False);
        end;
      end;

//      Segment := FSelectedScreenObject.Segments[Model].ClosestSegment(Location, LocalAnisotropy);
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

procedure TfrmGridValue.GetWidthForModpathPanels(var AvailableWidth: Integer);
begin
  AvailableWidth := ClientWidth - spl1.Width;
  if not jvrltCurrentData.Collapsed then
  begin
    AvailableWidth := AvailableWidth - jvrltCurrentData.Width;
  end
  else
  begin
    AvailableWidth := AvailableWidth - 22;
  end;
  if not jvrltAllDataSets.Collapsed then
  begin
    AvailableWidth := AvailableWidth - jvrltAllDataSets.Width;
  end
  else
  begin
    AvailableWidth := AvailableWidth - 22;
  end;
end;

function TfrmGridValue.DiscretizationDefined: Boolean;
var
  Grid: TCustomModelGrid;
  Mesh: TSutraMesh3D;
begin
  result := False;
  case frmGoPhast.ModelSelection of
    msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT
      {$IFDEF FMP}, msModflowFmp {$ENDIF}
      , msModflowCfp:
      begin
        Grid := frmGoPhast.Grid;
        result := (Grid <> nil) and (Grid.LayerCount >= 1)
          and (Grid.RowCount >= 1) and (Grid.ColumnCount >= 1);
      end;
    msSutra22:
      begin
        Mesh := frmGoPhast.PhastModel.Mesh;
        result := (Mesh <> nil) and (Mesh.Mesh2D.Nodes.Count > 0);
      end;
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
  LocalModel: TCustomModel;
begin
  if (FPriorEndPointLocation.x = Location.x)
    and (FPriorEndPointLocation.y = Location.Y)then
  begin
    Exit;
  end;
  if frmGoPhast.ModelSelection = msSutra22 then
  begin
    jvrltEndPoint.Visible := False;
    Exit;
  end;
  FPriorEndPointLocation := Location;
  LocalModel := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;
  EndPoints := LocalModel.EndPoints;
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
  jvrltEndPoint.Visible := EndPoints.Visible and (EndPointQuadTree.Count > 0);
  if jvrltEndPoint.Visible then
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
                ALayer := LocalModel.
                  ModflowLayerToDataSetLayer(AnEndPoint.StartLayer);
                DisplayPoint := (Abs(FColumn+1 - AnEndPoint.StartColumn) <= 1)
                  and (Abs(FLayer - ALayer) <= 1);
              end;
            vdSide:
              begin
                ALayer := LocalModel.
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
                ALayer := LocalModel.
                  ModflowLayerToDataSetLayer(AnEndPoint.EndLayer);
                DisplayPoint := (Abs(FColumn+1 - AnEndPoint.EndColumn) <= 1)
                  and (Abs(FLayer - ALayer) <= 1);
              end;
            vdSide:
              begin
                ALayer := LocalModel.
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
          rdgEndPoints.Cells[Ord(epcStart), Ord(eprNumber)] :=
            IntToStr(AnEndPoint.ParticleNumber);
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
          if AnEndPoint is TEndPointV6 then
          begin
            rdgEndPoints.Cells[Ord(epcStart), Ord(eprParticleGroup)] :=
              IntToStr(TEndPointV6(AnEndPoint).ParticleGroup);
          end
          else
          begin
            rdgEndPoints.Cells[Ord(epcStart), Ord(eprParticleGroup)] := '';
          end;

          rdgEndPoints.Cells[Ord(epcEnd), Ord(eprNumber)] :=
            IntToStr(AnEndPoint.ParticleNumber);
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

          if AnEndPoint is TEndPointV6 then
          begin
            rdgEndPoints.Cells[Ord(epcEnd), Ord(eprParticleGroup)] :=
              IntToStr(TEndPointV6(AnEndPoint).ParticleGroup);
          end
          else
          begin
            rdgEndPoints.Cells[Ord(epcEnd), Ord(eprParticleGroup)] := '';
          end;
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
  PathLine: TCustomPathLine;
  FirstPoint: TPathLinePoint;
  LastPoint: TPathLinePoint;
  List: TList;
  APathLinePoint: TPathLinePoint;
  ALayer: Integer;
  LocalModel: TCustomModel;
begin
  if (FPriorLocation.x = Location.x)
    and (FPriorLocation.y = Location.Y)then
  begin
    Exit;
  end;
  if frmGoPhast.ModelSelection = msSutra22 then
  begin
    jvrltPathline.Visible := False;
    Exit;
  end;
  FPriorLocation := Location;
  LocalModel := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;
  PathLines := LocalModel.PathLines;
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
  jvrltPathline.Visible := PathLines.Visible and (PathQuadTree.Count > 0);
  if jvrltPathline.Visible then
  begin
    X := Location.X;
    Y := Location.Y;
    PathQuadTree.FirstNearestPoint(X, Y, APointer);
    DisplayPoint := False;
//    case PathLines.ModpathVersion of
//      pv5:
//        begin
          PathLinePoint := APointer;
          Assert(PathLinePoint <> nil);
          case FViewDirection of
            vdTop:
              begin
                DisplayPoint := (Abs(FColumn+1 - PathLinePoint.Column) <= 1)
                  and (Abs(FRow+1 - PathLinePoint.Row) <= 1);
              end;
            vdFront:
              begin
                ALayer := LocalModel.
                  ModflowLayerToDataSetLayer(PathLinePoint.Layer);
                DisplayPoint := (Abs(FColumn+1 - PathLinePoint.Column) <= 1)
                  and (Abs(FLayer - ALayer) <= 1);
              end;
            vdSide:
              begin
                ALayer := LocalModel.
                  ModflowLayerToDataSetLayer(PathLinePoint.Layer);
                DisplayPoint := (Abs(FLayer - ALayer) <= 1)
                  and (Abs(FRow+1 - PathLinePoint.Row) <= 1);
              end;
            else Assert(False);
          end;
//        end;
//      pv6_0:
//        begin
//          PathLinePointV6 := APointer;
//          Assert(PathLinePointV6 <> nil);
//          case FViewDirection of
//            vdTop:
//              begin
//                DisplayPoint := (Abs(FColumn+1 - PathLinePointV6.Column) <= 1)
//                  and (Abs(FRow+1 - PathLinePointV6.Row) <= 1);
//              end;
//            vdFront:
//              begin
//                ALayer := frmGoPhast.PhastModel.
//                  ModflowLayerToDataSetLayer(PathLinePointV6.Layer);
//                DisplayPoint := (Abs(FColumn+1 - PathLinePointV6.Column) <= 1)
//                  and (Abs(FLayer - ALayer) <= 1);
//              end;
//            vdSide:
//              begin
//                ALayer := frmGoPhast.PhastModel.
//                  ModflowLayerToDataSetLayer(PathLinePointV6.Layer);
//                DisplayPoint := (Abs(FLayer - ALayer) <= 1)
//                  and (Abs(FRow+1 - PathLinePointV6.Row) <= 1);
//              end;
//            else Assert(False);
//          end;
//        end;
//      else
//        Assert(False);
//    end;


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
      edLength.Text := FloatToStr(PathLine.Length);
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
            rdgPathline.Cells[ColIndex, Ord(plrNumber)] :=
              IntToStr(PathLine.Index+1);

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
            if APathLinePoint is TPathLinePointV6 then
            begin
              rdgPathline.Cells[ColIndex, Ord(plrGroup)] :=
                IntToStr(TPathLinePointV6(APathLinePoint).ParticleGroup);
            end
            else
            begin
              rdgPathline.Cells[ColIndex, Ord(plrGroup)] := '';
            end;
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
  rdgPathline.Cells[0, Ord(plrNumber)] := StrNumber;
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
  rdgPathline.Cells[0, Ord(plrGroup)] := StrGroup;
end;

procedure TfrmGridValue.jvrltEndPointCollapse(Sender: TObject);
var
  AvailableWidth: Integer;
begin
  inherited;
   GetWidthForModpathPanels(AvailableWidth);
  if AvailableWidth > 0  then
  begin
    if not jvrltPathline.Collapsed then
    begin
      jvrltPathline.Width := AvailableWidth-22;
    end;
  end;
//  if jvrltPathline.Collapsed then
//  begin
//    jvrltEndPoint.Align := alLeft;
//  end
//  else
//  begin
//    jvrltEndPoint.Align := alRight;
//    Application.ProcessMessages;
//    jvrltPathline.Align := alClient;
//  end;
end;

procedure TfrmGridValue.jvrltEndPointExpand(Sender: TObject);
var
  AvailableWidth: Integer;
begin
  inherited;
//  jvrltPathline.Align := alLeft;
//    Application.ProcessMessages;
  GetWidthForModpathPanels(AvailableWidth);

  if AvailableWidth > 0 then
  begin
    if jvrltPathline.Collapsed then
    begin
      jvrltEndPoint.Width := AvailableWidth-22
    end
    else
    begin
      jvrltEndPoint.Width := AvailableWidth div 2;
      jvrltPathline.Width := AvailableWidth div 2;
    end;

  end;
//  jvrltEndPoint.Align := alClient;
end;

procedure TfrmGridValue.jvrltPathlineCollapse(Sender: TObject);
var
  AvailableWidth: Integer;
begin
  inherited;
  GetWidthForModpathPanels(AvailableWidth);
  if AvailableWidth > 0  then
  begin
    if not jvrltEndPoint.Collapsed then
    begin
      jvrltEndPoint.Width := AvailableWidth-22;
    end;
  end;
//  jvrltPathline.Align := alLeft;
//  if jvrltEndPoint.Collapsed then
//  begin
//    jvrltEndPoint.Align := alLeft;
//  end
//  else
//  begin
//    jvrltEndPoint.Align := alClient;
//  end;
//  Application.ProcessMessages;
//  jvrltEndPoint.Left := jvrltPathline.Left + 22;
end;

procedure TfrmGridValue.jvrltPathlineExpand(Sender: TObject);
var
  AvailableWidth: Integer;
begin
  inherited;
//  jvrltPathline.Align := alClient;
//  jvrltEndPoint.Align := alRight;
  GetWidthForModpathPanels(AvailableWidth);
  if AvailableWidth > 0  then
  begin
    if jvrltEndPoint.Collapsed then
    begin
      jvrltPathline.Width := AvailableWidth -22;
    end
    else
    begin
      jvrltPathline.Width := AvailableWidth div 2;
      jvrltEndPoint.Width := AvailableWidth div 2;
    end;
    rdgPathline.Width := jvrltPathline.ClientWidth;
  end;
end;

procedure TfrmGridValue.InitializeEndpointGrid;
begin
  rdgEndPoints.Cells[Ord(epcStart), 0] := StrStart;
  rdgEndPoints.Cells[Ord(epcEnd), 0] := StrEnd;
  rdgEndPoints.Cells[0, Ord(eprNumber)] := StrNumber;
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
  rdgEndPoints.Cells[0, Ord(eprParticleGroup)] := StrGroup;
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
