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
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure tvExportItemsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure seTimeCountChange(Sender: TObject);
    procedure rdgTimeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdgTimeSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure btnOKClick(Sender: TObject);
  private
    FEdgeEdits: TList;
    Value: Extended;
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
    procedure AssignShapeGeometry(Shape: TShapeObject; ColIndex,
      RowIndex: Integer; LocalGrid: TCustomGrid; EvaluatedAt: TEvaluatedAt);
    function GetShapeType: Integer;
    // A side effect of @name is to add the data sets in
    // the @link(TCustomTimeList)s in "TimeLists" to "DataSets".
    procedure GetFieldNames(Names, Fields: TStringList;
      LayerLimit: Integer; TimeLists, DataSets: TList);
    procedure InitializeControls;
    procedure AssignID_Fields(ID, ColIndex, RowIndex: Integer;
      ShapeDataBase: TXBase);
    procedure AssignDataSetValuesToDataBase(DataSets: TList;
        DataSetIndex: Integer; Names: TStringList; LayerLimit, ColIndex,
        RowIndex: integer; ShapeDataBase: TXBase);
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
  frmGridColorUnit, PhastDataSets, RealListUnit, ModflowTimeUnit,
  TimeUnit, FastGEO, RbwParser, EdgeDisplayUnit;

{$R *.dfm}

function ConvertPoint(Point: TPoint2D): TShapePoint;
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

      for Index := 0 to frmGoPhast.PhastModel.DataSetCount - 1 do
      begin
        DataSet := frmGoPhast.PhastModel.DataSets[Index];
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
      msModflow:
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

procedure TfrmExportShapefile.rdgTimeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ([ssShift, ssCtrl] * Shift) = [] then
  begin
    rdgTime.Options := rdgTime.Options + [goEditing];
  end
  else
  begin
    rdgTime.Options := rdgTime.Options - [goEditing];
  end;

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
var
  RowIndex: Integer;
  ColIndex: Integer;
  Shape : TShapeObject;
  ShapeType: longint;
  ShapeFileWriter: TShapefileGeometryWriter;
  ShapeFileName: string;
  IndexFileName: string;
  LocalGrid: TCustomGrid;
  Names: TStringList;
  ShapeDataBase: TXBase;
  LayerLimit: integer;
  Fields: TStringList;
  ID: integer;
  DataSetIndex: Integer;
  DataArray: TDataArray;
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
      ShapeFileWriter := TShapefileGeometryWriter.Create(ShapeType, True);
      try
        ShapeFileWriter.Capacity :=
          (LocalGrid.RowCount+1) * (LocalGrid.ColumnCount+1);
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
            AssignShapeGeometry(Shape, ColIndex, RowIndex, LocalGrid, eaNodes);

            AssignID_Fields(ID, ColIndex, RowIndex, ShapeDataBase);
            ShapeDataBase.PostChanges;
//            ShapeDataBase.GotoNext;
          end;
        end;
        for DataSetIndex := 0 to DataSets.Count - 1 do
        begin
          ShapeDataBase.GotoBOF;
          for RowIndex := 0 to LocalGrid.RowCount do
          begin
            for ColIndex := 0 to LocalGrid.ColumnCount do
            begin
              AssignDataSetValuesToDataBase(DataSets, DataSetIndex, Names, LayerLimit,
                ColIndex, RowIndex, ShapeDataBase);
              ShapeDataBase.PostChanges;
              if (RowIndex < LocalGrid.RowCount)
                or (ColIndex < LocalGrid.ColumnCount) then
              begin
                ShapeDataBase.GotoNext;
              end;
            end;
          end;
          frmGoPhast.PhastModel.CacheDataArrays;
          DataArray := DataSets[DataSetIndex];
          DataArray.CacheData;
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
    Names.Free;
    Fields.Free;
  end;
end;

procedure TfrmExportShapefile.ExportElementShapes(DataSets, TimeLists: TList);
var
  RowIndex: Integer;
  ColIndex: Integer;
  Shape : TShapeObject;
  ShapeType: longint;
  ShapeFileWriter: TShapefileGeometryWriter;
  ShapeFileName: string;
  IndexFileName: string;
  LocalGrid: TCustomGrid;
  LayerLimit: Integer;
  Fields: TStringList;
  Names: TStringList;
  ShapeDataBase: TXBase;
  ID: Integer;
  DataSetIndex: Integer;
  DataArray: TDataArray;
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
            AssignShapeGeometry(Shape, ColIndex, RowIndex, LocalGrid, eaBlocks);

            AssignID_Fields(ID, ColIndex, RowIndex, ShapeDataBase);
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
              AssignDataSetValuesToDataBase(DataSets, DataSetIndex, Names, LayerLimit,
                ColIndex, RowIndex, ShapeDataBase);
              ShapeDataBase.PostChanges;
              if (RowIndex < LocalGrid.RowCount - 1)
                or (ColIndex < LocalGrid.ColumnCount - 1) then
              begin
                ShapeDataBase.GotoNext;
              end;
            end;
          end;
          frmGoPhast.PhastModel.CacheDataArrays;
          DataArray := DataSets[DataSetIndex];
          DataArray.CacheData;
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
          SetLength(Shape.FPartTypes, 0);
          Shape.FNumPoints := 2;
          SetLength(Shape.FPoints, 2);
          Shape.FPoints[0] := ConvertPoint(Edge.StartingLocation);
          Shape.FPoints[1] := ConvertPoint(Edge.EndingLocation);

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
            ShapeDataBase.UpdFieldNum(Names[EditDisplayIndex],
              Edge.RealValue[Edit.DataIndex]);
          end;
          ShapeDataBase.PostChanges;
//          ShapeDataBase.GotoNext;

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

procedure TfrmExportShapefile.AssignShapeGeometry(Shape: TShapeObject; 
  ColIndex: Integer; RowIndex: Integer;  LocalGrid: TCustomGrid; 
  EvaluatedAt: TEvaluatedAt);
var
  Index: Integer;
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
        Shape.FBoundingBox.XMin := Shape.FPoints[0].X;
        Shape.FBoundingBox.XMax := Shape.FPoints[0].X;
        Shape.FBoundingBox.YMin := Shape.FPoints[0].Y;
        Shape.FBoundingBox.YMax := Shape.FPoints[0].Y;
        for Index := 1 to 4 do
        begin
          if Shape.FBoundingBox.XMin > Shape.FPoints[Index].X then
          begin
            Shape.FBoundingBox.XMin := Shape.FPoints[Index].X;
          end;
          if Shape.FBoundingBox.XMax < Shape.FPoints[Index].X then
          begin
            Shape.FBoundingBox.XMax := Shape.FPoints[Index].X;
          end;
          if Shape.FBoundingBox.YMin > Shape.FPoints[Index].Y then
          begin
            Shape.FBoundingBox.YMin := Shape.FPoints[Index].Y;
          end;
          if Shape.FBoundingBox.YMax < Shape.FPoints[Index].Y then
          begin
            Shape.FBoundingBox.YMax := Shape.FPoints[Index].Y;
          end;
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
        Shape.FBoundingBox.XMin := Shape.FPoints[0].X;
        Shape.FBoundingBox.XMax := Shape.FPoints[0].X;
        Shape.FBoundingBox.YMin := Shape.FPoints[0].Y;
        Shape.FBoundingBox.YMax := Shape.FPoints[0].Y;
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
  else
    Assert(False);
  end;
end;

procedure TfrmExportShapefile.GetFieldNames(Names, Fields: TStringList;
  LayerLimit: Integer; TimeLists, DataSets: TList);
var
  TimeRoot: string;
  DataSetIndex: Integer;
  TimeValue: Double;
  TimeList: TCustomTimeList;
  TimeListIndex: Integer;
  TimeCharacters: Integer;
  TimeIndex: Integer;
  RealList: TRealList;
  FieldName: string;
  LayerIndex: Integer;
  FieldFormat: string;
  RootName: string;
  DataArray: TDataArray;
  Index: Integer;
  LayerCharacters: Integer;
  LocalGrid: TCustomGrid;
  SuffixInt: integer;
  SuffixStr: string;
begin
  Fields.Add('COLUMN=N');
  Fields.Add('ROW=N');
  Fields.Add('ID=N');
  LocalGrid := frmGoPhast.Grid;
  LayerCharacters := Trunc(Log10(LocalGrid.LayerCount + 1)) + 1;
  for Index := 0 to DataSets.Count - 1 do
  begin
    DataArray := DataSets[Index];
    DataArray.Initialize;
    DataArray.CacheData;
    frmGoPhast.PhastModel.CacheDataArrays;
    RootName := UpperCase(DataArray.Name);
    case DataArray.Orientation of
      dsoTop:
        begin
          if Length(RootName) > 10 then
          begin
            SetLength(RootName, 10);
          end;

          SuffixInt := 1;
          while Names.IndexOf(RootName) >= 0 do
          begin
            SuffixStr := IntToStr(SuffixInt);
            Inc(SuffixInt);
            RootName := Copy(RootName, 1, 10-Length(SuffixStr))
              + SuffixStr;
          end;

          Names.AddObject(RootName, DataArray);
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
          Fields.AddObject(FieldName + '=' + FieldFormat, DataArray);
        end;
      dsoFront, dsoSide, dso3D:
        begin
          if Length(RootName) > 9 - LayerCharacters then
          begin
            SetLength(RootName, 9 - LayerCharacters);
          end;
          SuffixInt := 1;
          while Names.IndexOf(RootName) >= 0 do
          begin
            SuffixStr := IntToStr(SuffixInt);
            Inc(SuffixInt);
            RootName := Copy(RootName, 1, 9 - LayerCharacters-Length(SuffixStr))
              + SuffixStr;
          end;
          Names.AddObject(RootName, DataArray);
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
          for LayerIndex := 1 to LayerLimit do
          begin
            FieldName := RootName + 'L' + IntToStr(LayerIndex);
            Fields.AddObject(FieldName + '=' + FieldFormat, DataArray);
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
        for TimeIndex := 0 to RealList.Count - 1 do
        begin
          TimeValue := RealList[TimeIndex];
          DataSetIndex := TimeList.FirstTimeGreaterThan(TimeValue) - 1;
          DataArray := TimeList.Items[DataSetIndex];
          DataSets.Add(DataArray);
          RootName := UpperCase(TimeList.Name);
          RootName := StringReplace(RootName, ' ', '_', [rfReplaceAll]); 
          if Length(RootName) > 8 - LayerCharacters - TimeCharacters then
          begin
            SetLength(RootName, 8 - LayerCharacters - TimeCharacters);
          end;
          TimeRoot := RootName + 'T' + IntToStr(TimeIndex + 1);
          Names.AddObject(TimeRoot, DataArray);
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
          for LayerIndex := 1 to LayerLimit do
          begin
            FieldName := TimeRoot + 'L' + IntToStr(LayerIndex);
            Fields.AddObject(FieldName + '=' + FieldFormat, DataArray);
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

procedure TfrmExportShapefile.AssignDataSetValuesToDataBase(DataSets: TList;
  DataSetIndex: Integer; Names: TStringList; LayerLimit, ColIndex,
  RowIndex: integer; ShapeDataBase: TXBase);
var
  DataArray: TDataArray;
  RootName: string;
  LayerIndex: Integer;
  Col: Integer;
  Row: Integer;
  Layer: Integer;
  FieldName: string;
begin
  DataArray := DataSets[DataSetIndex];
  RootName := Names[DataSetIndex];
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
      FieldName := RootName + 'L' + IntToStr(Layer+1);
    end;
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
              DataArray.StringData[Layer, Row, Col]);
          end;
        else Assert(False);
      end;
    end;
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
    msModflow:
      begin
        lblElements.Caption := '&Cell Shapefile name';
        lblNodes.Caption := '&Cell-Corner Shapefile name';
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmExportShapefile.AssignID_Fields(ID, ColIndex, RowIndex: Integer;
  ShapeDataBase: TXBase);
begin
  ShapeDataBase.AppendBlank;
  ShapeDataBase.UpdFieldInt('ROW', RowIndex + 1);
  ShapeDataBase.UpdFieldInt('COLUMN', ColIndex + 1);
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
begin
  SelectedDataArray := frmGoPhast.Grid.ThreeDDataSet;
  SelectedTimeList := frmGoPhast.PhastModel.ThreeDTimeList;

  PhastBoundaryRootNode := tvExportItems.Items.AddChild(nil,
    StrBoundaryConditions);
  DataSetClassifications := TStringList.Create;
  try
    for Index := 0 to frmGoPhast.PhastModel.BoundaryDataSetCount - 1 do
    begin
      DataSet := frmGoPhast.PhastModel.BoundaryDataSets[Index];
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
