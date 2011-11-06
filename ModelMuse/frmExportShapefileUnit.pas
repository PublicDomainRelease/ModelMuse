unit frmExportShapefileUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, JvToolEdit, Mask, JvExMask,
  JvSpin, Grids, RbwDataGrid4, ComCtrls, ImgList, Contnrs, ExtCtrls, 
  ShapefileUnit, AbstractGridUnit, GoPhastTypes, XBase1;

type
  TfrmExportShapefile = class(TfrmCustomGoPhast)
    tvExportItems: TTreeView;
    rdgTime: TRbwDataGrid4;
    seTimeCount: TJvSpinEdit;
    jfeElements: TJvFilenameEdit;
    jfeNodes: TJvFilenameEdit;
    jfeHorizontalFlowBarrier: TJvFilenameEdit;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    lblExportItems: TLabel;
    lblTime: TLabel;
    lblTimeCount: TLabel;
    lblElements: TLabel;
    lblNodes: TLabel;
    lblHorizontalFlowBarrier: TLabel;
    ilCheckImages: TImageList;
    rgExportObjectType: TRadioGroup;
    rgHfbDimensions: TRadioGroup;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure tvExportItemsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure seTimeCountChange(Sender: TObject);
    procedure rdgTimeSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure btnOKClick(Sender: TObject);
  private
    FEdgeEdits: TList;
    Value: Extended;
    ShapeFileName: string;
    LocalGrid: TCustomModelGrid;
    LayerLimit: Integer;
    Fields: TStringList;
    Names: TStringList;
    ShapeDataBase: TXBase;
    ShapeType: Integer;
    ShapeFileWriter: TShapefileGeometryWriter;
    procedure GetDataSets;
    procedure GetBoundaryConditions;
    procedure UpdateParentNodeStates;
    procedure UpdateEnabledControls;
    procedure GetData;
    procedure InitializeNodeStates;
    procedure InitializeFileNames;
    procedure ReadSelectedTimes;
    procedure SetData;
    procedure ExportNodeShapes(DataSets, TimeLists: TList);
    procedure ExportElementShapes(DataSets, TimeLists: TList);
    procedure ExportHfbShapes(Edits: TList);
    procedure Assign2DShapeGeometry(Shape: TShapeObject; ColIndex,
      RowIndex: Integer; LocalGrid: TCustomModelGrid; EvaluatedAt: TEvaluatedAt);
    function GetShapeType: Integer;
    // A side effect of @name is to add the data sets in
    // the @link(TCustomTimeList)s in "TimeLists" to "DataSets".
    procedure GetFieldNames(Names, Fields: TStringList;
      LayerLimit: Integer; TimeLists, DataSets: TList);
    procedure InitializeControls;
    procedure Assign2DID_Fields(ID, ColIndex, RowIndex: Integer;
      ShapeDataBase: TXBase);
    procedure Assign2DDataSetValuesToDataBase(DataSets: TList;
        DataSetIndex: Integer; Names: TStringList; LayerLimit, ColIndex,
        RowIndex: integer; ShapeDataBase: TXBase);
    procedure Export2DNodeShapes(DataSets: TList);
    procedure Export3DNodeShapes(DataSets: TList);
    procedure Assign3DShapeGeometry(Shape: TShapeObject; ColIndex,
      RowIndex, LayerIndex: Integer; LocalGrid: TCustomModelGrid; EvaluatedAt: TEvaluatedAt);
    procedure Assign3DID_Fields(ID, ColIndex, RowIndex, LayerIndex: Integer;
      ShapeDataBase: TXBase);
    procedure Assign3DDataSetValuesToDataBase(DataSets: TList;
      DataSetIndex: Integer; Names: TStringList; ColIndex,
      RowIndex, LayerIndex: integer; ShapeDataBase: TXBase);
    procedure Export2DElementShapes(DataSets: TList);
    procedure Export3DElementShapes(DataSets: TList);
    { Private declarations }
  public
    { Public declarations }
  end;

  procedure InitializeDataBase(const ShapeFileName: string;
    ShapeDataBase: TXBase; Fields: TStringList);

var
  frmExportShapefile: TfrmExportShapefile;

implementation

uses Math, DataSetUnit, ClassificationUnit, PhastModelUnit, frmGoPhastUnit,
  PhastDataSets, RealListUnit, ModflowTimeUnit,
  TimeUnit, FastGEO, RbwParser, EdgeDisplayUnit, ModelMuseUtilities,
  frameCustomColorUnit;

{$R *.dfm}

function ConvertPoint(Point: TPoint2D): TShapePoint; overload;
begin
  result.X := Point.x;
  result.Y := Point.y;
end;

function ConvertPoint(Point: T3DRealPoint): TShapePoint; overload;
begin
  result.X := Point.x;
  result.Y := Point.y;
end;

procedure TfrmExportShapefile.GetDataSets;
var
  Index: Integer;
  Node: TTreeNode;
  SelectedDataArray: TDataArray;
  ClassificationList: TStringList;
  LayerGroupsDataSets: TList;
  DataSetList: TClassificationList;
  LayerGroupList: TClassificationList;
  DataSet: TDataArray;
  ClassificationObject: TDataSetClassification;
  SelectedName: string;
  Position: integer;
  HydrogeologicUnitNames: TStringList;
  HufDataArrays: TClassificationList;
  DataArrayManager: TDataArrayManager;
begin
  { TODO : Nearly the same code is use in TfrmFormulaUnit, TFrmGridColor,
  TfrmScreenObjectProperties, and TfrmDataSets. Find a way to combine them. }
  SelectedDataArray := frmGoPhast.Grid.ThreeDDataSet;
  if SelectedDataArray = nil then
  begin
    SelectedName := '';
  end
  else
  begin
    SelectedName := SelectedDataArray.Name;
  end;
  ClassificationList := TStringList.Create;
  try

    HydrogeologicUnitNames := TStringList.Create;
    HufDataArrays := TClassificationList.Create;
    LayerGroupsDataSets := TList.Create;
    DataSetList:= TClassificationList.Create;
    LayerGroupList := TClassificationList.Create;
    try
      frmGoPhast.PhastModel.HydrogeologicUnits.FillDataArrayNames(
        HydrogeologicUnitNames);
      HydrogeologicUnitNames.CaseSensitive := False;
      for Index := 0 to HydrogeologicUnitNames.Count - 1 do
      begin
        HufDataArrays.Add(nil);
      end;

      frmGoPhast.PhastModel.GetLayerGroupDataSets(LayerGroupsDataSets);
      for Index := 0 to LayerGroupsDataSets.Count - 1 do
      begin
        LayerGroupList.Add(nil);
      end;

      DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
      for Index := 0 to DataArrayManager.DataSetCount - 1 do
      begin
        DataSet := DataArrayManager.DataSets[Index];
        ClassificationObject := TDataSetClassification.Create(DataSet);
        DataSetList.Add(ClassificationObject);
        Position := LayerGroupsDataSets.IndexOf(DataSet);
        if Position >= 0 then
        begin
          LayerGroupList[Position] := ClassificationObject;
        end;
        Position := HydrogeologicUnitNames.IndexOf(DataSet.Name);
        if Position >= 0 then
        begin
          HufDataArrays[Position] := ClassificationObject;
        end;
      end;

      ClassifyListedObjects(ClassificationList, DataSetList,
        [LayerGroupList, HufDataArrays]);

      CreateClassifiedNodes(ClassificationList, 0, tvExportItems,
        SelectedName);

      for Index := 0 to tvExportItems.Items.Count - 1 do
      begin
        Node := tvExportItems.Items[Index];
        ClassificationObject := Node.Data;
        if ClassificationObject <> nil then
        begin
          Node.Data := ClassificationObject.DataArray;
        end;
      end;
    finally
      LayerGroupList.Free;
      for Index := 0 to DataSetList.Count - 1 do
      begin
        DataSetList[Index].Free;
      end;
      DataSetList.Free;
      LayerGroupsDataSets.Free;
      HufDataArrays.Free;
      HydrogeologicUnitNames.Free;
    end;
  finally
    ClassificationList.Free;
  end;
end;

procedure TfrmExportShapefile.tvExportItemsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  procedure UpdateChildNodeStates(Node: TTreeNode);
  var
    ChildNode: TTreeNode;
  begin
    ChildNode := Node.GetFirstChild;
    while ChildNode <> nil do
    begin
      ChildNode.StateIndex := Node.StateIndex;
      UpdateChildNodeStates(ChildNode);
      ChildNode := ChildNode.getNextSibling;
    end;
  end;
begin
  inherited;
  if htOnStateIcon in tvExportItems.GetHitTestInfoAt(X, Y) then
  begin
    case tvExportItems.Selected.StateIndex of
      1:
        begin
          tvExportItems.Selected.StateIndex := 2;
        end;
      2:
        begin
          tvExportItems.Selected.StateIndex := 1;
        end;
      3:
        begin
          tvExportItems.Selected.StateIndex := 2;
        end;
      else Assert(False);
    end;
    Node := tvExportItems.Selected;
    UpdateChildNodeStates(Node);
    UpdateParentNodeStates;
    UpdateEnabledControls;
  end;
end;

procedure TfrmExportShapefile.InitializeFileNames;
var
  NewFileName: string;
begin
  NewFileName := ChangeFileExt(frmGoPhast.sdSaveDialog.FileName, '');;
  jfeElements.FileName := NewFileName + '_E.shp';
  jfeNodes.FileName := NewFileName + '_N.shp';
  jfeHorizontalFlowBarrier.FileName := NewFileName + '_HFB.shp';
end;

procedure TfrmExportShapefile.ReadSelectedTimes;
var
  StressPeriod: TModflowStressPeriod;
  TimeItem: TTimeItem;
  Index: Integer;
  RealList: TRealList;
begin
  RealList := TRealList.Create;
  try
    RealList.Sorted := True;
    case frmGoPhast.PhastModel.ModelSelection of
      msPhast:
        begin
          RealList.Add(0);
          for Index := 0 to frmGoPhast.PhastModel.Times.Count - 1 do
          begin
            TimeItem := frmGoPhast.PhastModel.Times.Items[Index] as TTimeItem;
            RealList.AddUnique(TimeItem.EndingTime);
          end;
        end;
      msModflow, msModflowLGR, msModflowNWT:
        begin
          for Index := 0 to
            frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
          begin
            StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods[Index];
            RealList.AddUnique(StressPeriod.StartTime);
            RealList.AddUnique(StressPeriod.EndTime);
          end;
        end;
    else
      Assert(False);
    end;
    for Index := 0 to RealList.Count - 1 do
    begin
      rdgTime.Columns[0].PickList.Add(FloatToStr(RealList[Index]));
    end;
  finally
    RealList.Free;
  end;
end;

procedure TfrmExportShapefile.seTimeCountChange(Sender: TObject);
begin
  inherited;
  rdgTime.RowCount := seTimeCount.AsInteger + 1;
end;

procedure TfrmExportShapefile.GetData;
begin
  GetDataSets;
  GetBoundaryConditions;
  InitializeNodeStates;
  UpdateEnabledControls;
  InitializeFileNames;
  ReadSelectedTimes;
end;

procedure TfrmExportShapefile.InitializeNodeStates;
var
  Index: Integer;
  Node: TTreeNode;
begin
  if tvExportItems.Selected <> nil then
  begin
    tvExportItems.Selected.StateIndex := 2;
  end;
  for Index := 0 to tvExportItems.Items.Count - 1 do
  begin
    Node := tvExportItems.Items[Index];
    if (not Node.HasChildren) and (Node.StateIndex <> 2) then
    begin
      Node.StateIndex := 1;
    end;
  end;
  UpdateParentNodeStates;
end;

procedure TfrmExportShapefile.rdgTimeSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  if seTimeCount.AsInteger <> rdgTime.RowCount -1 then
  begin
    seTimeCount.AsInteger := rdgTime.RowCount -1;
  end;
end;

procedure TfrmExportShapefile.ExportNodeShapes(DataSets, TimeLists: TList);
begin
  if (DataSets.Count = 0) and (TimeLists.Count = 0) then
  begin
    Exit;
  end;

  ShapeFileName := jfeNodes.FileName;
  if FileExists(ShapeFileName) and (MessageDlg(ShapeFileName
    + ' already exists.  Do you want to replace it?',
    mtInformation, [mbYes, mbNo], 0) <> mrYes) then
  begin
    Exit;
  end;

  LocalGrid := frmGoPhast.Grid;
  LayerLimit := LocalGrid.LayerCount+1;

  Fields := TStringList.Create;
  Names := TStringList.Create;
  try
    GetFieldNames(Names, Fields, LayerLimit, TimeLists, DataSets);
    // DataSets now contains TDataArrays from
    // TimeLists for all the specified times.

    ShapeDataBase := TXBase.Create(nil);
    try
      InitializeDataBase(ShapeFileName, ShapeDataBase, Fields);

      ShapeType := GetShapeType;
      if ShapeType  in [stPolygon, stPoint] then
      begin
        Export2DNodeShapes(DataSets);
      end
      else
      begin
        Export3DNodeShapes(DataSets);
      end;
    finally
      ShapeDataBase.Free;
    end;
  finally
    Names.Free;
    Fields.Free;
  end;
end;

procedure TfrmExportShapefile.ExportElementShapes(DataSets, TimeLists: TList);
begin
  if (DataSets.Count = 0) and (TimeLists.Count = 0) then
  begin
    Exit;
  end;

  ShapeFileName := jfeElements.FileName;
  if FileExists(ShapeFileName) and (MessageDlg(ShapeFileName
    + ' already exists.  Do you want to replace it?',
    mtInformation, [mbYes, mbNo], 0) <> mrYes) then
  begin
    Exit;
  end;

  LocalGrid := frmGoPhast.Grid;
  LayerLimit := LocalGrid.LayerCount;

  Fields := TStringList.Create;
  Names := TStringList.Create;
  try
    GetFieldNames(Names, Fields, LayerLimit, TimeLists, DataSets);
    // DataSets now contains TDataArrays from
    // TimeLists for all the specified times.

    ShapeDataBase := TXBase.Create(nil);
    try
      InitializeDataBase(ShapeFileName, ShapeDataBase, Fields);
      ShapeType := GetShapeType;
      if ShapeType  in [stPolygon, stPoint] then
      begin
        Export2DElementShapes(DataSets);
      end
      else
      begin
        Export3DElementShapes(DataSets);
      end;
    finally
      ShapeDataBase.Free;
    end;
  finally
    Fields.Free;
    Names.Free;
  end;
end;

procedure TfrmExportShapefile.ExportHfbShapes(Edits: TList);
var
  Fields: TStringList;
  EditDisplayIndex: Integer;
  Edit: TEdgeDisplayEdit;
  Field: string;
  ShapeDataBase: TXBase;
  ShapeFileName: string;
  ShapeType: Integer;
  ShapeFileWriter: TShapefileGeometryWriter;
  EdgeDisplay: TCustomModflowGridEdgeDisplay;
  EdgeIndex: Integer;
  Edge: TCustomModflowGridEdgeFeature;
  Shape: TShapeObject;
  Names: TStringList;
  IndexFileName: string;
  ID: integer;
  Element: T3DElementCoordinates;
  StartingIndex: Integer;
  PointIndex: Integer;
  ArrayPosition: Integer;
begin
  if (Edits.Count = 0) then
  begin
    Exit;
  end;

  ShapeFileName := jfeHorizontalFlowBarrier.FileName;
  if FileExists(ShapeFileName) and (MessageDlg(ShapeFileName
    + ' already exists.  Do you want to replace it?',
    mtInformation, [mbYes, mbNo], 0) <> mrYes) then
  begin
    Exit;
  end;

  LocalGrid := frmGoPhast.Grid;

  Names := TStringList.Create;
  Fields := TStringList.Create;
  try
    Fields.Add('COL1=N');
    Fields.Add('COL2=N');
    Fields.Add('ROW1=N');
    Fields.Add('ROW2=N');
    Fields.Add('LAYER=N');
    Fields.Add('ID=N');
    EdgeDisplay := nil;
    for EditDisplayIndex := 0 to Edits.Count - 1 do
    begin
      Edit := Edits[EditDisplayIndex];
      if EditDisplayIndex = 0 then
      begin
        EdgeDisplay := Edit.Edge;
      end
      else
      begin
        Assert(EdgeDisplay = Edit.Edge);
      end;
      Field := Edit.Edge.RealDescription[Edit.DataIndex];
      if Length(Field) > 10 then
      begin
        SetLength(Field, 10);
      end;
      Field := UpperCase(Field);
      Field := StringReplace(Field, ' ', '_', [rfReplaceAll]);
      Names.Add(Field);
      Field := Field + '=N18,10';
      Fields.Add(Field);
    end;
    EdgeDisplay.UpdateData;
    ShapeDataBase := TXBase.Create(nil);
    try
      InitializeDataBase(ShapeFileName, ShapeDataBase, Fields);

      ShapeType := stPolyLine;
      case rgHfbDimensions.ItemIndex of
        0:
          begin
            ShapeType := stPolyLine;
          end;
        1:
          begin
            ShapeType := stMultiPatch;
          end;
        else
          Assert(False);
      end;

      ShapeFileWriter := TShapefileGeometryWriter.Create(ShapeType, True);
      try
        ID := 0;
        for EdgeIndex := 0 to EdgeDisplay.Count - 1 do
        begin
          Inc(ID);
          Edge := EdgeDisplay[EdgeIndex];

          Shape := TShapeObject.Create;
          Shape.FShapeType := ShapeType;
          ShapeFileWriter.AddShape(Shape);
          Shape.FNumParts := 1;
          SetLength(Shape.FParts, 1);
          Shape.FParts[0] := 0;

          case ShapeType of
            stPolyLine:
              begin
                SetLength(Shape.FPartTypes, 0);
                Shape.FNumPoints := 2;
                SetLength(Shape.FPoints, 2);
                Shape.FPoints[0] := ConvertPoint(Edge.StartingLocation);
                Shape.FPoints[1] := ConvertPoint(Edge.EndingLocation);
              end;
            stMultiPatch:
              begin
                SetLength(Shape.FPartTypes, 1);
                Shape.FPartTypes[0] := ptTriangleStrip;
                Shape.FNumPoints := 6;
                SetLength(Shape.FPoints, Shape.FNumPoints);
                SetLength(Shape.FZArray, Shape.FNumPoints);
                SetLength(Shape.FMArray, Shape.FNumPoints);
                Element := LocalGrid.ElementCoordinates[
                  Edge.Col1, Edge.Row1, Edge.Layer];
//                StartingIndex := -1;
                if Edge.Col1 = Edge.Col2 then
                begin
                  if Edge.Row1 < Edge.Row2 then
                  begin
                    StartingIndex := 4;
                  end
                  else
                  begin
                    StartingIndex := 0;
                  end;
                end
                else
                begin
                  if Edge.Col1 < Edge.Col2 then
                  begin
                    StartingIndex := 2;
                  end
                  else
                  begin
                    StartingIndex := 6;
                  end;
                  Assert(Edge.Row1 = Edge.Row2);
                end;

                for PointIndex := 0 to 2 do
                begin
                  ArrayPosition := PointIndex+StartingIndex;
                  if ArrayPosition = Length(Element.TopEdge) then
                  begin
                    ArrayPosition := 0;
                  end;
                  Shape.FPoints[PointIndex*2] :=
                    ConvertPoint(Element.TopEdge[ArrayPosition]);
                  Shape.FPoints[PointIndex*2+1] :=
                     ConvertPoint(Element.BottomEdge[ArrayPosition]);
                  Shape.FZArray[PointIndex*2] :=
                    Element.TopEdge[ArrayPosition].Z;
                  Shape.FZArray[PointIndex*2+1] :=
                     Element.BottomEdge[ArrayPosition].Z;
                  Shape.FMArray[PointIndex*2] := 0;
                  Shape.FMArray[PointIndex*2+1] := 0;
                end;
              end;
            else
              Assert(False);
          end;

          ShapeDataBase.AppendBlank;
          ShapeDataBase.UpdFieldInt('COL1', Edge.Col1+1);
          ShapeDataBase.UpdFieldInt('COL2', Edge.Col2+1);
          ShapeDataBase.UpdFieldInt('ROW1', Edge.Row1+1);
          ShapeDataBase.UpdFieldInt('ROW2', Edge.Row2+1);
          ShapeDataBase.UpdFieldInt('LAYER', Edge.Layer+1);
          ShapeDataBase.UpdFieldInt('ID', ID);
          for EditDisplayIndex := 0 to Edits.Count - 1 do
          begin
            Edit := Edits[EditDisplayIndex];
            ShapeDataBase.UpdFieldNum(AnsiString(Names[EditDisplayIndex]),
              Edge.RealValue[Edit.DataIndex]);
          end;
          ShapeDataBase.PostChanges;

        end;
        IndexFileName := ChangeFileExt(ShapeFileName, '.shx');
        ShapeFileWriter.WriteToFile(ShapeFileName, IndexFileName);

      finally
        ShapeFileWriter.Free;
      end;
    finally
      ShapeDataBase.Free;
    end;
  finally
    Fields.Free;
    Names.Free;
  end;
end;

procedure TfrmExportShapefile.Assign2DShapeGeometry(Shape: TShapeObject;
  ColIndex: Integer; RowIndex: Integer;  LocalGrid: TCustomModelGrid;
  EvaluatedAt: TEvaluatedAt);
var
  APoint: TPoint2D;
begin
  Shape.FMArray := nil;
  Shape.FZArray := nil;
  case Shape.FShapeType of
    stPolygon:
      begin
        Shape.FNumParts := 1;
        SetLength(Shape.FParts, 1);
        Shape.FParts[0] := 0;
        SetLength(Shape.FPartTypes, 1);
        Shape.FPartTypes[0] := ptOuterRing;
        Shape.FNumPoints := 5;
        SetLength(Shape.FPoints, 5);
        case EvaluatedAt of
          eaBlocks: APoint := LocalGrid.TwoDElementCorner(ColIndex, RowIndex);
          eaNodes: APoint := LocalGrid.TwoDCellCorner(ColIndex, RowIndex);
          else Assert(False);
        end;
        
        Shape.FPoints[0] := ConvertPoint(APoint);
        Shape.FPoints[4] := Shape.FPoints[0];
        case EvaluatedAt of
          eaBlocks: APoint :=
            LocalGrid.TwoDElementCorner(ColIndex + 1, RowIndex + 1);
          eaNodes: APoint :=
            LocalGrid.TwoDCellCorner(ColIndex + 1, RowIndex + 1);
          else Assert(False);
        end;
        Shape.FPoints[2] := ConvertPoint(APoint);
        // The points must be in clockwise order.
        case LocalGrid.RowDirection of
          rdSouthToNorth:
            begin
              case EvaluatedAt of
                eaBlocks: APoint :=
                  LocalGrid.TwoDElementCorner(ColIndex + 1, RowIndex);
                eaNodes: APoint :=
                  LocalGrid.TwoDCellCorner(ColIndex + 1, RowIndex);
                else Assert(False);
              end;
              Shape.FPoints[3] := ConvertPoint(APoint);
              case EvaluatedAt of
                eaBlocks: APoint :=
                  LocalGrid.TwoDElementCorner(ColIndex, RowIndex + 1);
                eaNodes: APoint :=
                  LocalGrid.TwoDCellCorner(ColIndex, RowIndex + 1);
                else Assert(False);
              end;
              Shape.FPoints[1] := ConvertPoint(APoint);
            end;
          rdNorthToSouth:
            begin
              case EvaluatedAt of
                eaBlocks: APoint :=
                  LocalGrid.TwoDElementCorner(ColIndex + 1, RowIndex);
                eaNodes: APoint :=
                  LocalGrid.TwoDCellCorner(ColIndex + 1, RowIndex);
                else Assert(False);
              end;
              Shape.FPoints[1] := ConvertPoint(APoint);
              case EvaluatedAt of
                eaBlocks: APoint :=
                  LocalGrid.TwoDElementCorner(ColIndex, RowIndex + 1);
                eaNodes: APoint :=
                  LocalGrid.TwoDCellCorner(ColIndex, RowIndex + 1);
                else Assert(False);
              end;
              Shape.FPoints[3] := ConvertPoint(APoint);
            end;
        else
          Assert(False);
        end;
      end;
    stPoint:
      begin
        Shape.FNumPoints := 1;
        SetLength(Shape.FPoints, 1);
        case EvaluatedAt of
          eaBlocks: APoint := LocalGrid.TwoDElementCenter(ColIndex, RowIndex);
          eaNodes: APoint := LocalGrid.TwoDElementCorner(ColIndex, RowIndex);
          else Assert(False);
        end;
        Shape.FPoints[0] := ConvertPoint(APoint);
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmExportShapefile.Assign3DShapeGeometry(Shape: TShapeObject;
  ColIndex, RowIndex, LayerIndex: Integer;  LocalGrid: TCustomModelGrid;
  EvaluatedAt: TEvaluatedAt);
var
  APoint: T3DRealPoint;
  Cell: T3DCellCoordinates;
  Element: T3DElementCoordinates;
  MIndex: Integer;
  PointIndex: Integer;
begin
  Shape.FMArray := nil;
  Shape.FZArray := nil;
  case Shape.FShapeType of
    stPointZ:
      begin
        Shape.FNumParts := 0;
        SetLength(Shape.FParts, 0);
        Shape.FNumPoints := 1;
        SetLength(Shape.FPoints, 1);
        SetLength(Shape.FZArray, 1);
        SetLength(Shape.FMArray, 1);
        case EvaluatedAt of
          eaBlocks: APoint := LocalGrid.RotatedThreeDElementCenter(ColIndex, RowIndex, LayerIndex);
          eaNodes: APoint := LocalGrid.RotatedThreeDElementCorner(ColIndex, RowIndex, LayerIndex);
          else Assert(False);
        end;
        
        Shape.FPoints[0] := ConvertPoint(APoint);
        Shape.FZArray[0] := APoint.Z;
        Shape.FMArray[0] := 0;;
      end;
    stMultiPatch:
      begin
        case EvaluatedAt of
          eaBlocks:
            begin
              Shape.FNumParts := 3;
              SetLength(Shape.FParts, Shape.FNumParts);
              SetLength(Shape.FPartTypes, Shape.FNumParts);
              Shape.FNumPoints := 38;
              SetLength(Shape.FPoints, Shape.FNumPoints);
              SetLength(Shape.FZArray, Shape.FNumPoints);
              SetLength(Shape.FMArray, Shape.FNumPoints);

              Shape.FParts[0] := 0;
              Shape.FPartTypes[0] := ptTriangleFan;
              Shape.FParts[1] := 10;
              Shape.FPartTypes[1] := ptTriangleFan;
              Shape.FParts[2] := 20;
              Shape.FPartTypes[2] := ptTriangleStrip;

              Element := LocalGrid.ElementCoordinates[ColIndex, RowIndex, LayerIndex];

              Shape.FPoints[0] := ConvertPoint(Element.TopCenter);
              Shape.FZArray[0] := Element.TopCenter.Z;
              for PointIndex := 0 to 7 do
              begin
                Shape.FPoints[PointIndex+1] := ConvertPoint(Element.TopEdge[PointIndex]);
                Shape.FZArray[PointIndex+1] := Element.TopEdge[PointIndex].Z;
              end;
              Shape.FPoints[9] := ConvertPoint(Element.TopEdge[0]);
              Shape.FZArray[9] := Element.TopEdge[0].Z;

              Shape.FPoints[10] := ConvertPoint(Element.BottomCenter);
              Shape.FZArray[10] := Element.BottomCenter.Z;
              for PointIndex := 0 to 7 do
              begin
                Shape.FPoints[PointIndex+11] := ConvertPoint(Element.BottomEdge[PointIndex]);
                Shape.FZArray[PointIndex+11] := Element.BottomEdge[PointIndex].Z;
              end;
              Shape.FPoints[19] := ConvertPoint(Element.BottomEdge[0]);
              Shape.FZArray[19] := Element.BottomEdge[0].Z;

              for PointIndex := 0 to 7 do
              begin
                Shape.FPoints[PointIndex*2+20] := ConvertPoint(Element.TopEdge[PointIndex]);
                Shape.FZArray[PointIndex*2+20] := Element.TopEdge[PointIndex].Z;
                Shape.FPoints[PointIndex*2+21] := ConvertPoint(Element.BottomEdge[PointIndex]);
                Shape.FZArray[PointIndex*2+21] := Element.BottomEdge[PointIndex].Z;
              end;
              Shape.FPoints[36] := ConvertPoint(Element.TopEdge[0]);
              Shape.FZArray[36] := Element.TopEdge[0].Z;
              Shape.FPoints[37] := ConvertPoint(Element.BottomEdge[0]);
              Shape.FZArray[37] := Element.BottomEdge[0].Z;
            end;
          eaNodes:
            begin
              Shape.FNumParts := 1;
              SetLength(Shape.FParts, 1);
              SetLength(Shape.FPartTypes, 1);
              Shape.FNumPoints := 14;
              SetLength(Shape.FPoints, Shape.FNumPoints);
              SetLength(Shape.FZArray, Shape.FNumPoints);
              SetLength(Shape.FMArray, Shape.FNumPoints);

              Shape.FParts[0] := 0;
              Shape.FPartTypes[0] := ptTriangleStrip;

              Cell := LocalGrid.CellCoordinates[ColIndex, RowIndex, LayerIndex];

              Shape.FPoints[0] := ConvertPoint(Cell.Col1_Row1_Lay1);
              Shape.FZArray[0] := Cell.Col1_Row1_Lay1.Z;

              Shape.FPoints[1] := ConvertPoint(Cell.Col2_Row1_Lay1);
              Shape.FZArray[1] := Cell.Col2_Row1_Lay1.Z;

              Shape.FPoints[2] := ConvertPoint(Cell.Col1_Row2_Lay1);
              Shape.FZArray[2] := Cell.Col1_Row2_Lay1.Z;

              Shape.FPoints[3] := ConvertPoint(Cell.Col2_Row2_Lay1);
              Shape.FZArray[3] := Cell.Col2_Row2_Lay1.Z;

              Shape.FPoints[4] := ConvertPoint(Cell.Col2_Row2_Lay2);
              Shape.FZArray[4] := Cell.Col2_Row2_Lay2.Z;

              Shape.FPoints[5] := ConvertPoint(Cell.Col2_Row1_Lay1);
              Shape.FZArray[5] := Cell.Col2_Row1_Lay1.Z;

              Shape.FPoints[6] := ConvertPoint(Cell.Col2_Row1_Lay2);
              Shape.FZArray[6] := Cell.Col2_Row1_Lay2.Z;

              Shape.FPoints[7] := ConvertPoint(Cell.Col1_Row1_Lay1);
              Shape.FZArray[7] := Cell.Col1_Row1_Lay1.Z;

              Shape.FPoints[8] := ConvertPoint(Cell.Col1_Row1_Lay2);
              Shape.FZArray[8] := Cell.Col1_Row1_Lay2.Z;

              Shape.FPoints[9] := ConvertPoint(Cell.Col1_Row2_Lay1);
              Shape.FZArray[9] := Cell.Col1_Row2_Lay1.Z;

              Shape.FPoints[10] := ConvertPoint(Cell.Col1_Row2_Lay2);
              Shape.FZArray[10] := Cell.Col1_Row2_Lay2.Z;

              Shape.FPoints[11] := ConvertPoint(Cell.Col2_Row2_Lay2);
              Shape.FZArray[11] := Cell.Col2_Row2_Lay2.Z;

              Shape.FPoints[12] := ConvertPoint(Cell.Col1_Row1_Lay2);
              Shape.FZArray[12] := Cell.Col1_Row1_Lay2.Z;

              Shape.FPoints[13] := ConvertPoint(Cell.Col2_Row1_Lay2);
              Shape.FZArray[13] := Cell.Col2_Row1_Lay2.Z;
            end;
          else
            Assert(False);
        end;
        for MIndex := 0 to Length(Shape.FMArray) - 1 do
        begin
          Shape.FMArray[MIndex] := 0;
        end;
      end;
  else
    Assert(False);
  end;
end;

function TfrmExportShapefile.GetShapeType: Integer;
begin
  result := stNull;
  case rgExportObjectType.ItemIndex of
    0:
      result := stPolygon;
    1:
      result := stPoint;
    2:
      result := stMultiPatch;
    3:
      result := stPointZ;
  else
    Assert(False);
  end;
end;

procedure TfrmExportShapefile.GetFieldNames(Names, Fields: TStringList;
  LayerLimit: Integer; TimeLists, DataSets: TList);
var
  TimeRoot: AnsiString;
  DataSetIndex: Integer;
  TimeValue: Double;
  TimeList: TCustomTimeList;
  TimeListIndex: Integer;
  TimeCharacters: Integer;
  TimeIndex: Integer;
  RealList: TRealList;
  FieldName: AnsiString;
  LayerIndex: Integer;
  FieldFormat: AnsiString;
  RootName: AnsiString;
  DataArray: TDataArray;
  Index: Integer;
  LayerCharacters: Integer;
  SuffixInt: integer;
  SuffixStr: AnsiString;
begin
  ShapeType := GetShapeType;
  Fields.Add('COLUMN=N');
  Fields.Add('ROW=N');
  if (ShapeType  in [stPolygon, stPoint]) then
  begin
    LayerCharacters := Trunc(Log10(LocalGrid.LayerCount + 1)) + 1;
  end
  else
  begin
    Fields.Add('LAYER=N');
    LayerCharacters := 0;
  end;
  Fields.Add('ID=N');
  LocalGrid := frmGoPhast.Grid;
  for Index := 0 to DataSets.Count - 1 do
  begin
    DataArray := DataSets[Index];
    DataArray.Initialize;
    DataArray.CacheData;
    frmGoPhast.PhastModel.DataArrayManager.CacheDataArrays;
    RootName := AnsiString(UpperCase(DataArray.Name));
    case DataArray.Orientation of
      dsoTop:
        begin
          if Length(RootName) > 10 then
          begin
            SetLength(RootName, 10);
          end;

          SuffixInt := 1;
          while Names.IndexOf(string(RootName)) >= 0 do
          begin
            SuffixStr := AnsiString(IntToStr(SuffixInt));
            Inc(SuffixInt);
            RootName := Copy(RootName, 1, 10-Length(SuffixStr))
              + SuffixStr;
          end;

          Names.AddObject(string(RootName), DataArray);
          case DataArray.DataType of
            rdtDouble:
              FieldFormat := 'N18,10';
            rdtInteger:
              FieldFormat := 'N';
            rdtBoolean:
              FieldFormat := 'N';
            rdtString:
              FieldFormat := 'C18';
            else
              Assert(False);
          end;
          FieldName := RootName;
          FieldName := FixShapeFileFieldName(FieldName);
          Fields.AddObject(string(FieldName + '=' + FieldFormat), DataArray);
        end;
      dsoFront, dsoSide, dso3D:
        begin
          if Length(RootName) > 10 then
          begin
            SetLength(RootName, 10);
          end;
          if LayerCharacters > 0 then
          begin
            if Length(RootName) > 9 - LayerCharacters then
            begin
              SetLength(RootName, 9 - LayerCharacters);
            end;
            SuffixInt := 1;
            while Names.IndexOf(string(RootName)) >= 0 do
            begin
              SuffixStr := AnsiString(IntToStr(SuffixInt));
              Inc(SuffixInt);
              RootName := AnsiString(Copy(string(RootName), 1, 9 - LayerCharacters-Length(SuffixStr)))
                + SuffixStr;
            end;
          end;
          Names.AddObject(string(RootName), DataArray);
          case DataArray.DataType of
            rdtDouble:
              FieldFormat := 'N18,10';
            rdtInteger:
              FieldFormat := 'N';
            rdtBoolean:
              FieldFormat := 'N';
            rdtString:
              FieldFormat := 'C18';
          end;
          if LayerCharacters = 0 then
          begin
            FieldName := RootName;
            FieldName := FixShapeFileFieldName(FieldName);
            Fields.AddObject(String(FieldName + '=' + FieldFormat), DataArray);
          end
          else
          begin
            for LayerIndex := 1 to LayerLimit do
            begin
              FieldName := RootName + 'L' + AnsiString(IntToStr(LayerIndex));
              FieldName := FixShapeFileFieldName(FieldName);
              Fields.AddObject(String(FieldName + '=' + FieldFormat), DataArray);
            end;
          end;
        end;
      else Assert(False);
    end;
  end;
  if TimeLists.Count > 0 then
  begin
    RealList := TRealList.Create;
    try
      for TimeIndex := 1 to rdgTime.RowCount - 1 do
      begin
        if TryStrToFloat(rdgTime.Cells[0, TimeIndex], Value) then
        begin
          RealList.Add(Value);
        end;
      end;
      TimeCharacters := Trunc(Log10(RealList.Count)) + 1;
      for TimeListIndex := 0 to TimeLists.Count - 1 do
      begin
        TimeList := TimeLists[TimeListIndex];
        TimeList.Initialize;
      end;
      for TimeListIndex := 0 to TimeLists.Count - 1 do
      begin
        TimeList := TimeLists[TimeListIndex];
        if TimeList.Count = 0 then
        begin
          Continue;
        end;
        for TimeIndex := 0 to RealList.Count - 1 do
        begin
          TimeValue := RealList[TimeIndex];
          DataSetIndex := TimeList.FirstTimeGreaterThan(TimeValue) - 1;
          DataArray := TimeList.Items[DataSetIndex];
          DataSets.Add(DataArray);
          RootName := AnsiString(UpperCase(TimeList.Name));
          RootName := AnsiString(StringReplace(string(RootName), ' ', '_', [rfReplaceAll]));
          if Length(RootName) > 8 - LayerCharacters - TimeCharacters then
          begin
            SetLength(RootName, 8 - LayerCharacters - TimeCharacters);
          end;
          TimeRoot := RootName + 'T' + AnsiString(IntToStr(TimeIndex + 1));
          Names.AddObject(string(TimeRoot), DataArray);
          case DataArray.DataType of
            rdtDouble:
              FieldFormat := 'N18,10';
            rdtInteger:
              FieldFormat := 'N';
            rdtBoolean:
              FieldFormat := 'N';
            rdtString:
              FieldFormat := 'C18';
          end;
          if LayerCharacters = 0 then
          begin
            FieldName := TimeRoot;
            FieldName := FixShapeFileFieldName(FieldName);
            Fields.AddObject(string(FieldName + '=' + FieldFormat), DataArray);
          end
          else
          begin
            for LayerIndex := 1 to LayerLimit do
            begin
              FieldName := TimeRoot + 'L' + AnsiString(IntToStr(LayerIndex));
              FieldName := FixShapeFileFieldName(FieldName);
              Fields.AddObject(string(FieldName + '=' + FieldFormat), DataArray);
            end;
          end;
        end;
      end;
    finally
      RealList.Free;
    end;
  end;
end;

procedure InitializeDataBase(const ShapeFileName: string;
  ShapeDataBase: TXBase; Fields: TStringList);
var
  DataBaseFileName: string;
begin
  DataBaseFileName := ChangeFileExt(ShapeFileName, '.dbf');
  if FileExists(DataBaseFileName) then
  begin
    DeleteFile(DataBaseFileName);
  end;
  ShapeDataBase.DBFCreate(DataBaseFileName, Fields);
  ShapeDataBase.FileName := DataBaseFileName;
  ShapeDataBase.Active := True;
  ShapeDataBase.GotoBOF;
end;

procedure TfrmExportShapefile.Assign3DDataSetValuesToDataBase(DataSets: TList;
  DataSetIndex: Integer; Names: TStringList; ColIndex,
  RowIndex, LayerIndex: integer; ShapeDataBase: TXBase);
var
  DataArray: TDataArray;
  RootName: AnsiString;
  Col: Integer;
  Row: Integer;
  Layer: Integer;
  FieldName: AnsiString;
begin
  DataArray := DataSets[DataSetIndex];
  RootName := AnsiString(Names[DataSetIndex]);
  Col := ColIndex;
  Row := RowIndex;
  Layer := LayerIndex;
  case DataArray.Orientation of
    dsoTop: Layer := 0;
    dsoFront: Row := 0;
    dsoSide: Col := 0;
    dso3D: ; // do nothing
    else Assert(False);
  end;
  FieldName := RootName;
  if DataArray.IsValue[Layer, Row, Col] then
  begin
    case DataArray.DataType of
      rdtDouble:
        begin
          ShapeDataBase.UpdFieldNum(FieldName,
            DataArray.RealData[Layer, Row, Col]);
        end;
      rdtInteger:
        begin
          ShapeDataBase.UpdFieldInt(FieldName,
            DataArray.IntegerData[Layer, Row, Col]);
        end;
      rdtBoolean:
        begin
          if DataArray.BooleanData[Layer, Row, Col] then
          begin
            ShapeDataBase.UpdFieldInt(FieldName, 1);
          end
          else
          begin
            ShapeDataBase.UpdFieldInt(FieldName, 0);
          end;
        end;
      rdtString:
        begin
          ShapeDataBase.UpdFieldStr(FieldName,
            AnsiString(DataArray.StringData[Layer, Row, Col]));
        end;
      else Assert(False);
    end;
  end;
end;

procedure TfrmExportShapefile.Export3DElementShapes(DataSets: TList);
var
  ID: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Shape: TShapeObject;
  DataSetIndex: Integer;
  DataArray: TDataArray;
  IndexFileName: string;
  LayerIndex: Integer;
begin
  ShapeFileWriter := TShapefileGeometryWriter.Create(ShapeType, True);
  try
    ShapeFileWriter.Capacity := LocalGrid.RowCount * LocalGrid.ColumnCount * LocalGrid.LayerCount;
    Assert(LocalGrid.ColumnDirection = cdWestToEast);
    ID := 0;
    for LayerIndex := 0 to LocalGrid.LayerCount - 1 do
    begin
      for RowIndex := 0 to LocalGrid.RowCount - 1 do
      begin
        for ColIndex := 0 to LocalGrid.ColumnCount - 1 do
        begin
          Inc(ID);
          Shape := TShapeObject.Create;
          Shape.FShapeType := ShapeType;
          ShapeFileWriter.AddShape(Shape);
          Assign3DShapeGeometry(Shape, ColIndex, RowIndex, LayerIndex, LocalGrid, eaBlocks);
          Assign3DID_Fields(ID, ColIndex, RowIndex, LayerIndex, ShapeDataBase);
          ShapeDataBase.PostChanges;
        end;
      end;
    end;
    for DataSetIndex := 0 to DataSets.Count - 1 do
    begin
      ShapeDataBase.GotoBOF;
      for LayerIndex := 0 to LocalGrid.LayerCount - 1 do
      begin
        for RowIndex := 0 to LocalGrid.RowCount - 1 do
        begin
          for ColIndex := 0 to LocalGrid.ColumnCount - 1 do
          begin
            Assign3DDataSetValuesToDataBase(DataSets, DataSetIndex, Names, ColIndex, RowIndex, LayerIndex, ShapeDataBase);
            ShapeDataBase.PostChanges;
            if (RowIndex < LocalGrid.RowCount - 1)
              or (ColIndex < LocalGrid.ColumnCount - 1)
              or (LayerIndex < LocalGrid.LayerCount - 1) then
            begin
              ShapeDataBase.GotoNext;
            end;
          end;
        end;
      end;
      frmGoPhast.PhastModel.DataArrayManager.CacheDataArrays;
      DataArray := DataSets[DataSetIndex];
      DataArray.CacheData;
    end;
    IndexFileName := ChangeFileExt(ShapeFileName, '.shx');
    ShapeFileWriter.WriteToFile(ShapeFileName, IndexFileName);
  finally
    ShapeFileWriter.Free;
  end;
end;


procedure TfrmExportShapefile.Export2DElementShapes(DataSets: TList);
var
  ID: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Shape: TShapeObject;
  DataSetIndex: Integer;
  DataArray: TDataArray;
  IndexFileName: string;
begin
  ShapeFileWriter := TShapefileGeometryWriter.Create(ShapeType, True);
  try
    ShapeFileWriter.Capacity := LocalGrid.RowCount * LocalGrid.ColumnCount;
    Assert(LocalGrid.ColumnDirection = cdWestToEast);
    ID := 0;
    for RowIndex := 0 to LocalGrid.RowCount - 1 do
    begin
      for ColIndex := 0 to LocalGrid.ColumnCount - 1 do
      begin
        Inc(ID);
        Shape := TShapeObject.Create;
        Shape.FShapeType := ShapeType;
        ShapeFileWriter.AddShape(Shape);
        Assign2DShapeGeometry(Shape, ColIndex, RowIndex, LocalGrid, eaBlocks);
        Assign2DID_Fields(ID, ColIndex, RowIndex, ShapeDataBase);
        ShapeDataBase.PostChanges;
      end;
    end;
    for DataSetIndex := 0 to DataSets.Count - 1 do
    begin
      ShapeDataBase.GotoBOF;
      for RowIndex := 0 to LocalGrid.RowCount - 1 do
      begin
        for ColIndex := 0 to LocalGrid.ColumnCount - 1 do
        begin
          Assign2DDataSetValuesToDataBase(DataSets, DataSetIndex, Names, LayerLimit, ColIndex, RowIndex, ShapeDataBase);
          ShapeDataBase.PostChanges;
          if (RowIndex < LocalGrid.RowCount - 1)
            or (ColIndex < LocalGrid.ColumnCount - 1) then
          begin
            ShapeDataBase.GotoNext;
          end;
        end;
      end;
      frmGoPhast.PhastModel.DataArrayManager.CacheDataArrays;
      DataArray := DataSets[DataSetIndex];
      DataArray.CacheData;
    end;
    IndexFileName := ChangeFileExt(ShapeFileName, '.shx');
    ShapeFileWriter.WriteToFile(ShapeFileName, IndexFileName);
  finally
    ShapeFileWriter.Free;
  end;
end;

procedure TfrmExportShapefile.Assign2DDataSetValuesToDataBase(DataSets: TList;
  DataSetIndex: Integer; Names: TStringList; LayerLimit, ColIndex,
  RowIndex: integer; ShapeDataBase: TXBase);
var
  DataArray: TDataArray;
  RootName: AnsiString;
  LayerIndex: Integer;
  Col: Integer;
  Row: Integer;
  Layer: Integer;
  FieldName: AnsiString;
begin
  DataArray := DataSets[DataSetIndex];
  RootName := AnsiString(Names[DataSetIndex]);
  if DataArray.Orientation = dsoTop then
  begin
    LayerLimit := 1;
  end;
  for LayerIndex := 0 to LayerLimit - 1 do
  begin
    Col := ColIndex;
    Row := RowIndex;
    Layer := LayerIndex;
    case DataArray.Orientation of
      dsoTop: Layer := 0;
      dsoFront: Row := 0;
      dsoSide: Col := 0;
      dso3D: ; // do nothing
      else Assert(False);
    end;
    if DataArray.Orientation = dsoTop then
    begin
      FieldName := RootName;
    end
    else
    begin
      FieldName := RootName + 'L' + AnsiString(IntToStr(Layer+1));
    end;
    FieldName := FixShapeFileFieldName(FieldName);
    if DataArray.IsValue[Layer, Row, Col] then
    begin
      case DataArray.DataType of
        rdtDouble:
          begin
            ShapeDataBase.UpdFieldNum(FieldName,
              DataArray.RealData[Layer, Row, Col]);
          end;
        rdtInteger:
          begin
            ShapeDataBase.UpdFieldInt(FieldName,
              DataArray.IntegerData[Layer, Row, Col]);
          end;
        rdtBoolean:
          begin
            if DataArray.BooleanData[Layer, Row, Col] then
            begin
              ShapeDataBase.UpdFieldInt(FieldName, 1);
            end
            else
            begin
              ShapeDataBase.UpdFieldInt(FieldName, 0);
            end;
          end;
        rdtString:
          begin
            ShapeDataBase.UpdFieldStr(FieldName,
              AnsiString(DataArray.StringData[Layer, Row, Col]));
          end;
        else Assert(False);
      end;
    end;
  end;
end;

procedure TfrmExportShapefile.Export3DNodeShapes(DataSets: TList);
var
  ID: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Shape: TShapeObject;
  DataSetIndex: Integer;
  DataArray: TDataArray;
  IndexFileName: string;
  LayerIndex: Integer;
begin
  ShapeFileWriter := TShapefileGeometryWriter.Create(ShapeType, True);
  try
    ShapeFileWriter.Capacity := (LocalGrid.RowCount + 1) * (LocalGrid.ColumnCount + 1) * (LocalGrid.LayerCount + 1);
    Assert(LocalGrid.ColumnDirection = cdWestToEast);
    ID := 0;
    for LayerIndex := 0 to LocalGrid.LayerCount do
    begin
      for RowIndex := 0 to LocalGrid.RowCount do
      begin
        for ColIndex := 0 to LocalGrid.ColumnCount do
        begin
          Inc(ID);
          Shape := TShapeObject.Create;
          Shape.FShapeType := ShapeType;
          ShapeFileWriter.AddShape(Shape);
          Assign3DShapeGeometry(Shape, ColIndex, RowIndex, LayerIndex, LocalGrid, eaNodes);
          Assign3DID_Fields(ID, ColIndex, RowIndex, LayerIndex, ShapeDataBase);
          ShapeDataBase.PostChanges;
        end;
        //            ShapeDataBase.GotoNext;
      end;
    end;
    for DataSetIndex := 0 to DataSets.Count - 1 do
    begin
      ShapeDataBase.GotoBOF;
      for LayerIndex := 0 to LocalGrid.LayerCount do
      begin
        for RowIndex := 0 to LocalGrid.RowCount do
        begin
          for ColIndex := 0 to LocalGrid.ColumnCount do
          begin
            Assign3DDataSetValuesToDataBase(DataSets, DataSetIndex, Names, ColIndex, RowIndex, LayerIndex, ShapeDataBase);
            ShapeDataBase.PostChanges;
            if (RowIndex < LocalGrid.RowCount)
              or (ColIndex < LocalGrid.ColumnCount)
              or (LayerIndex < LocalGrid.LayerCount) then
            begin
              ShapeDataBase.GotoNext;
            end;
          end;
        end;
      end;
      frmGoPhast.PhastModel.DataArrayManager.CacheDataArrays;
      DataArray := DataSets[DataSetIndex];
      DataArray.CacheData;
    end;
    IndexFileName := ChangeFileExt(ShapeFileName, '.shx');
    ShapeFileWriter.WriteToFile(ShapeFileName, IndexFileName);
  finally
    ShapeFileWriter.Free;
  end;
end;

procedure TfrmExportShapefile.Export2DNodeShapes(DataSets: TList);
var
  ID: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Shape: TShapeObject;
  DataSetIndex: Integer;
  DataArray: TDataArray;
  IndexFileName: string;
begin
  ShapeFileWriter := TShapefileGeometryWriter.Create(ShapeType, True);
  try
    ShapeFileWriter.Capacity := (LocalGrid.RowCount + 1) * (LocalGrid.ColumnCount + 1);
    Assert(LocalGrid.ColumnDirection = cdWestToEast);
    ID := 0;
    for RowIndex := 0 to LocalGrid.RowCount do
    begin
      for ColIndex := 0 to LocalGrid.ColumnCount do
      begin
        Inc(ID);
        Shape := TShapeObject.Create;
        Shape.FShapeType := ShapeType;
        ShapeFileWriter.AddShape(Shape);
        Assign2DShapeGeometry(Shape, ColIndex, RowIndex, LocalGrid, eaNodes);
        Assign2DID_Fields(ID, ColIndex, RowIndex, ShapeDataBase);
        ShapeDataBase.PostChanges;
      end;
      //            ShapeDataBase.GotoNext;
    end;
    for DataSetIndex := 0 to DataSets.Count - 1 do
    begin
      ShapeDataBase.GotoBOF;
      for RowIndex := 0 to LocalGrid.RowCount do
      begin
        for ColIndex := 0 to LocalGrid.ColumnCount do
        begin
          Assign2DDataSetValuesToDataBase(DataSets, DataSetIndex, Names, LayerLimit, ColIndex, RowIndex, ShapeDataBase);
          ShapeDataBase.PostChanges;
          if (RowIndex < LocalGrid.RowCount) or (ColIndex < LocalGrid.ColumnCount) then
          begin
            ShapeDataBase.GotoNext;
          end;
        end;
      end;
      frmGoPhast.PhastModel.DataArrayManager.CacheDataArrays;
      DataArray := DataSets[DataSetIndex];
      DataArray.CacheData;
    end;
    IndexFileName := ChangeFileExt(ShapeFileName, '.shx');
    ShapeFileWriter.WriteToFile(ShapeFileName, IndexFileName);
  finally
    ShapeFileWriter.Free;
  end;
end;

procedure TfrmExportShapefile.InitializeControls;
begin
  rdgTime.Cells[0, 0] := 'Times';
  rdgTime.Cells[0, 1] := '0';

  case frmGoPhast.ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast:
      begin
        lblElements.Caption := '&Element Shapefile name';
        lblNodes.Caption := '&Node Shapefile name';
      end;
    msModflow, msModflowLGR, msModflowNWT:
      begin
        lblElements.Caption := '&Cell Shapefile name';
        lblNodes.Caption := '&Cell-Corner Shapefile name';
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmExportShapefile.Assign2DID_Fields(ID, ColIndex, RowIndex: Integer;
  ShapeDataBase: TXBase);
begin
  ShapeDataBase.AppendBlank;
  ShapeDataBase.UpdFieldInt('ROW', RowIndex + 1);
  ShapeDataBase.UpdFieldInt('COLUMN', ColIndex + 1);
  ShapeDataBase.UpdFieldInt('ID', ID);
end;

procedure TfrmExportShapefile.Assign3DID_Fields(ID, ColIndex, RowIndex, LayerIndex: Integer;
  ShapeDataBase: TXBase);
begin
  ShapeDataBase.AppendBlank;
  ShapeDataBase.UpdFieldInt('ROW', RowIndex + 1);
  ShapeDataBase.UpdFieldInt('COLUMN', ColIndex + 1);
  ShapeDataBase.UpdFieldInt('LAYER', LayerIndex + 1);
  ShapeDataBase.UpdFieldInt('ID', ID);
end;

procedure TfrmExportShapefile.SetData;
var
  Index: Integer;
  Node: TTreeNode;
  AnObject: TObject;
  NodeDataSets: TList;
  NodeTimeLists: TList;
  ElementDataSets: TList;
  ElementTimeLists: TList;
  HfbEdits: TList;
  DataArray: TDataArray;
  TimeList: TCustomTimeList;
begin
  Screen.Cursor := crHourGlass;
  try
    NodeDataSets := TList.Create;
    NodeTimeLists := TList.Create;
    ElementDataSets := TList.Create;
    ElementTimeLists := TList.Create;
    HfbEdits:= TList.Create;
    try
      for Index := 0 to tvExportItems.Items.Count - 1 do
      begin
        Node := tvExportItems.Items[Index];
        if Node.StateIndex = 2 then
        begin
          AnObject := Node.Data;
          if AnObject = nil then
          begin
            Continue;
          end
          else if (AnObject is TDataArray) then
          begin
            DataArray := TDataArray(AnObject);
            case DataArray.EvaluatedAt of
              eaBlocks: ElementDataSets.Add(DataArray);
              eaNodes: NodeDataSets.Add(DataArray);
              else Assert(False);
            end;
          end
          else if (AnObject is TCustomTimeList) then
          begin
            TimeList := TCustomTimeList(AnObject);
            if TimeList is TPhastTimeList then
            begin
              NodeTimeLists.Add(TimeList);
            end
            else
            begin
              ElementTimeLists.Add(TimeList);
            end;
          end
          else
          begin
            Assert(AnObject is TEdgeDisplayEdit);
            HfbEdits.Add(AnObject)
          end;
        end;
      end;
      ExportNodeShapes(NodeDataSets, NodeTimeLists);
      ExportElementShapes(ElementDataSets, ElementTimeLists);
      ExportHfbShapes(HfbEdits);
    finally
      ElementDataSets.Free;
      ElementTimeLists.Free;
      NodeDataSets.Free;
      NodeTimeLists.Free;
      HfbEdits.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmExportShapefile.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData
end;

procedure TfrmExportShapefile.FormCreate(Sender: TObject);
begin
  inherited;
  FEdgeEdits := TObjectList.Create;
  InitializeControls;
  GetData;
end;

procedure TfrmExportShapefile.FormDestroy(Sender: TObject);
begin
  inherited;
  FEdgeEdits.Free;
end;

procedure TfrmExportShapefile.GetBoundaryConditions;
var
  Node: TTreeNode;
  DataSetIndex: Integer;
  ClassificationNode: TTreeNode;
  TimeList: TCustomTimeList;
  List: TStringList;
  ClassificationPosition: Integer;
  DataSet: TDataArray;
  Index: Integer;
  DataSetClassifications: TStringList;
  PhastBoundaryRootNode: TTreeNode;
  SelectedTimeList: TCustomTimeList;
  SelectedDataArray: TDataArray;
  EdgeEdit: TEdgeDisplayEdit;
  DataArrayManager: TDataArrayManager;
begin
  SelectedDataArray := frmGoPhast.Grid.ThreeDDataSet;
  SelectedTimeList := frmGoPhast.PhastModel.ThreeDTimeList;

  PhastBoundaryRootNode := tvExportItems.Items.AddChild(nil,
    StrBoundaryConditions);
  DataSetClassifications := TStringList.Create;
  try
    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    for Index := 0 to DataArrayManager.BoundaryDataSetCount - 1 do
    begin
      DataSet := DataArrayManager.BoundaryDataSets[Index];
      ClassificationPosition :=
        DataSetClassifications.IndexOf(DataSet.Classification);
      if ClassificationPosition < 0 then
      begin
        List := TStringList.Create;
        DataSetClassifications.AddObject(DataSet.Classification, List);
      end
      else
      begin
        List := DataSetClassifications.
          Objects[ClassificationPosition] as TStringList;
      end;
      List.AddObject(DataSet.Name, DataSet);
    end;
    FEdgeEdits.Clear;
    List := TStringList.Create;
    DataSetClassifications.AddObject('MODFLOW Horizontal Flow Barrier', List);
    for Index := 0 to frmGoPhast.PhastModel.
      HfbDisplayer.RealValueTypeCount - 1 do
    begin
      EdgeEdit := TEdgeDisplayEdit.Create;
      FEdgeEdits.Add(EdgeEdit);
      EdgeEdit.DataIndex := Index;
      EdgeEdit.Edge := frmGoPhast.PhastModel.HfbDisplayer;
      List.AddObject(EdgeEdit.Edge.RealDescription[Index], EdgeEdit);
    end;
    for Index := 0 to frmGoPhast.PhastModel.TimeListCount - 1 do
    begin
      TimeList := frmGoPhast.PhastModel.TimeLists[Index];
      if TimeList.UsedByModel then
      begin
        ClassificationPosition := DataSetClassifications.IndexOf(
          TimeList.Classification);
        if ClassificationPosition < 0 then
        begin
          List := TStringList.Create;
          DataSetClassifications.AddObject(TimeList.Classification, List);
        end
        else
        begin
          List := DataSetClassifications.
            Objects[ClassificationPosition] as TStringList;
        end;
        List.AddObject(TimeList.Name, TimeList);
      end;
    end;
    DataSetClassifications.Sort;
    for Index := 0 to DataSetClassifications.Count - 1 do
    begin
      ClassificationNode := tvExportItems.Items.
        AddChild(PhastBoundaryRootNode, DataSetClassifications[Index]);
      ClassificationNode.Data := nil;
      List := DataSetClassifications.Objects[Index] as TStringList;
      List.Sort;
      for DataSetIndex := 0 to List.Count - 1 do
      begin
        Node := tvExportItems.Items.
          AddChild(ClassificationNode, List[DataSetIndex]);
        Node.Data := List.Objects[DataSetIndex];
        if (Node.Data = SelectedDataArray)
          or (Node.Data = SelectedTimeList) then
        begin
          tvExportItems.Selected := Node;
        end;
      end;
    end;
  finally
    for Index := 0 to DataSetClassifications.Count - 1 do
    begin
      DataSetClassifications.Objects[Index].Free;
    end;
    DataSetClassifications.Free;
  end;
end;

procedure TfrmExportShapefile.UpdateEnabledControls;
var
  Index: Integer;
  Node: TTreeNode;
  AnObject: TObject;
  ShouldEnableElement: Boolean;
  ShouldEnableNode: Boolean;
  ShouldEnableHfb: Boolean;
  ShouldEnableTime: Boolean;
begin
  ShouldEnableElement := False;
  ShouldEnableNode := False;
  ShouldEnableHfb := False;
  ShouldEnableTime := False;
  for Index := 0 to tvExportItems.Items.Count - 1 do
  begin
    Node := tvExportItems.Items[Index];
    if Node.StateIndex = 2 then
    begin
      AnObject := Node.Data;
      if AnObject = nil then
      begin
        Continue;
      end
      else if (AnObject is TDataArray) then
      begin
        case TDataArray(AnObject).EvaluatedAt of
          eaBlocks: ShouldEnableElement := True;
          eaNodes: ShouldEnableNode := True;
          else Assert(False);
        end;
      end
      else if (AnObject is TCustomTimeList) then
      begin
        ShouldEnableTime := True;
        if AnObject is TPhastTimeList then
        begin
          ShouldEnableNode := True;
        end
        else
        begin
          ShouldEnableElement := True;
        end;
      end
      else
      begin
        Assert(AnObject is TEdgeDisplayEdit);
        ShouldEnableHfb := True;
      end;
    end;
  end;
  jfeElements.Enabled := ShouldEnableElement;
  jfeNodes.Enabled := ShouldEnableNode;
  jfeHorizontalFlowBarrier.Enabled := ShouldEnableHfb;
  rdgTime.Enabled := ShouldEnableTime;
  if ShouldEnableTime then
  begin
    rdgTime.Color := clWindow;
  end
  else
  begin
    rdgTime.Color := clBtnFace;
  end;
  seTimeCount.Enabled := ShouldEnableTime;
  rgExportObjectType.Enabled := (ShouldEnableElement or ShouldEnableNode);
  btnOK.Enabled := (ShouldEnableElement or ShouldEnableNode or ShouldEnableHfb);
end;

procedure TfrmExportShapefile.UpdateParentNodeStates;
var
  ChildNode: TTreeNode;
  Stack: TStack;
  Index: Integer;
  Node: TTreeNode;
begin
  Stack := TStack.Create;
  try
    for Index := 0 to tvExportItems.Items.Count - 1 do
    begin
      Node := tvExportItems.Items[Index];
      if Node.HasChildren then
      begin
        Stack.Push(Node);
      end;
    end;
    while Stack.Count > 0 do
    begin
      Node := Stack.Pop;
      ChildNode := Node.GetFirstChild;
      Node.StateIndex := ChildNode.StateIndex;
      if Node.StateIndex = 3 then
      begin
        Continue;
      end;
      ChildNode := ChildNode.getNextSibling;
      while ChildNode <> nil do
      begin
        if Node.StateIndex <> ChildNode.StateIndex then
        begin
          Node.StateIndex := 3;
          break;
        end;
        ChildNode := ChildNode.getNextSibling;
      end;
    end;
  finally
    Stack.Free;
  end;
end;

end.
