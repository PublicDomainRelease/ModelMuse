unit ContourExport;

interface

uses Classes, FastGeo, QuadtreeClass, RealListUnit, AbstractGridUnit,
  DataSetUnit, SysUtils, GoPhastTypes, ContourUnit, ValueArrayStorageUnit;

type
  TPointList = class(TObject)
  private
    FPoints: array of TPoint2D;
  strict private
    FCount: integer;
    procedure Grow;
    function GetCapacity: integer;
    function GetPoint(Index: integer): TPoint2D;
    procedure SetCapacity(const Value: integer);
    procedure SetPoint(Index: integer; const Value: TPoint2D);
  public
    function Add(Point: TPoint2D): integer;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read FCount;
    property Points[Index: integer]: TPoint2D read GetPoint write SetPoint;
    procedure Insert(Position: integer; Point: TPoint2D);
  end;

  TContourExtractor = class(TCustomContourCreator)
  private
    FQuadTree: TRbwQuadTree;
    // @name contains instances of @link(TPointList).
    // @name is instantiated as a TObjectList.
    // @name is filled in @link(ImportSegments).
    FPointLists: TList;
    FEpsilon: Real;
    FModelGrid: TCustomModelGrid;
    // @name is the event handler for TContourCreator.OnExtractSegments
    procedure ImportSegments(Sender: TObject; const Segments: TLine2DArray);
    procedure InitializeQuadTree;
    procedure InitializeEpsilon;
  public
    procedure CreateShapes(ValueList: TValueArrayStorage; DataArray: TDataArray;
      FileName: string);
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
  end;

procedure GlobalImportSegments(Sender: TObject;
  const Segments: TLine2DArray; Epsilon: Real;
  QuadTree: TRbwQuadTree; PointLists: TList);

procedure GlobalInitializeQuadTree(var QuadTree: TRbwQuadTree;
  ModelGrid: TCustomModelGrid);

procedure GlobalInitializeEpsilon(var Epsilon: Real; ModelGrid: TCustomModelGrid);

implementation

uses
  PhastModelUnit, frmGoPhastUnit, Contnrs, Math,
  ShapefileUnit, XBase1, frmExportShapefileUnit, RbwParser, ModelMuseUtilities;

procedure GlobalImportSegments(Sender: TObject;
  const Segments: TLine2DArray; Epsilon: Real;
  QuadTree: TRbwQuadTree; PointLists: TList);
var
  Index: Integer;
  Segment: TLine2D;
  Line1, Line2, Line3: TPointList;
  X1, Y1: double;
  X2, Y2: double;
  APoint: TPoint2D;
  PointIndex: Integer;
  APointer: Pointer;
  function SamePoint(X1, X2, Y1, Y2: double): boolean;
  begin
    result := Sqr(X1-X2) + Sqr(Y1-Y2) < Epsilon;
  end;
begin
  for Index := 0 to Length(Segments) - 1 do
  begin
    Segment := Segments[Index];
    if SamePoint(Segment[1].x, Segment[2].x, Segment[1].y, Segment[2].y) then
    begin
      Continue;
    end;
    X1 := Segment[1].x;
    Y1 := Segment[1].y;
    if QuadTree.Count > 0 then
    begin
      QuadTree.FirstNearestPoint(X1, Y1, APointer);
      Line1 := APointer;
    end
    else
    begin
      Line1 := nil;
    end;
    if Line1 <> nil then
    begin
      if not SamePoint(X1, Segment[1].x, Y1, Segment[1].Y) then
      begin
        Line1 := nil;
      end;
    end;

    X2 := Segment[2].x;
    Y2 := Segment[2].y;
    if QuadTree.Count > 0 then
    begin
      QuadTree.FirstNearestPoint(X2, Y2, APointer);
      Line2 := APointer;
    end
    else
    begin
      Line2 := nil;
    end;
    if Line2 <> nil then
    begin
      if not SamePoint(X2, Segment[2].x, Y2, Segment[2].Y) then
      begin
        Line2 := nil;
      end;
    end;
    if (Line1 = nil) and (Line2 = nil) then
    begin
      Line1 := TPointList.Create;
      PointLists.Add(Line1);
      Line1.Add(Segment[1]);
      Line1.Add(Segment[2]);
      QuadTree.AddPoint(Segment[1].x, Segment[1].y, Line1);
      QuadTree.AddPoint(Segment[2].x, Segment[2].y, Line1);
    end
    else if (Line1 = nil) then
    begin
      APoint := Line2.Points[Line2.Count-1];
      if SamePoint(Segment[2].x, APoint.x, Segment[2].y, APoint.y) then
      begin
        Line2.Add(Segment[1]);
      end
      else
      begin
        APoint := Line2.Points[0];
        Assert(SamePoint(Segment[2].x, APoint.x, Segment[2].y, APoint.y));
        Line2.Insert(0, Segment[1]);
      end;
      QuadTree.RemovePoint(X2, Y2, Line2);
      QuadTree.AddPoint(Segment[1].x, Segment[1].y, Line2);
    end
    else if (Line2 = nil) then
    begin
      APoint := Line1.Points[Line1.Count-1];
      if SamePoint(Segment[1].x, APoint.x, Segment[1].y, APoint.y) then
      begin
        Line1.Add(Segment[2]);
      end
      else
      begin
        APoint := Line1.Points[0];
        Assert(SamePoint(Segment[1].x, APoint.x, Segment[1].y, APoint.y));
        Line1.Insert(0, Segment[2]);
      end;
      QuadTree.RemovePoint(X1, Y1, Line1);
      QuadTree.AddPoint(Segment[2].x, Segment[2].y, Line1);
    end
    else if (Line2 = Line1) then
    begin
      APoint := Line1.Points[Line1.Count-1];
      if SamePoint(Segment[1].x, APoint.x, Segment[1].y, APoint.y) then
      begin
        Line1.Add(Segment[2]);
      end
      else
      begin
        APoint := Line1.Points[0];
        Assert(SamePoint(Segment[1].x, APoint.x, Segment[1].y, APoint.y));
        Line1.Insert(0, Segment[2]);
      end;
      QuadTree.RemovePoint(X1, Y1, Line1);
      QuadTree.RemovePoint(X2, Y2, Line1);
    end
    else
    begin
      APoint := Line1.Points[Line1.Count-1];
      if   (SamePoint(Segment[1].x, APoint.x, Segment[1].y, APoint.y))
        or (SamePoint(Segment[2].x, APoint.x, Segment[2].y, APoint.y)) then
      begin
        // Add to end of line 1
        if Line1.Capacity < Line1.Count + Line2.Count then
        begin
          Line1.Capacity := Line1.Count + Line2.Count
        end;
        APoint := Line2.Points[Line2.Count-1];
        if   (SamePoint(Segment[1].x, APoint.x, Segment[1].y, APoint.y))
          or (SamePoint(Segment[2].x, APoint.x, Segment[2].y, APoint.y)) then
        begin
          APoint := Line2.Points[0];
          QuadTree.RemovePoint(APoint.x, APoint.y, Line2);
          // Add end of line 2 to end of line 1
          for PointIndex := Line2.Count - 1 downto 0 do
          begin
            Line1.Add(Line2.Points[PointIndex]);
          end;
        end
        else
        begin
          APoint := Line2.Points[Line2.Count - 1];
          QuadTree.RemovePoint(APoint.x, APoint.y, Line2);
          // Join beginning of line 2 to end of line 1
          for PointIndex := 0 to Line2.Count - 1 do
          begin
            Line1.Add(Line2.Points[PointIndex]);
          end;
        end;
        QuadTree.RemovePoint(X1, Y1, Line1);
        QuadTree.RemovePoint(X2, Y2, Line2);
        QuadTree.AddPoint(APoint.x, APoint.y, Line1);
        PointLists.Remove(Line2);
      end
      else
      begin
        APoint := Line2.Points[Line2.Count-1];
        if   (SamePoint(Segment[1].x, APoint.x, Segment[1].y, APoint.y))
          or (SamePoint(Segment[2].x, APoint.x, Segment[2].y, APoint.y)) then
        begin
          // Add beginning of line 1 to end of line2
          if Line2.Capacity < Line1.Count + Line2.Count then
          begin
            Line2.Capacity := Line1.Count + Line2.Count;
          end;
          APoint := Line1.Points[Line1.Count - 1];
          QuadTree.RemovePoint(APoint.x, APoint.y, Line1);
          for PointIndex := 0 to Line1.Count - 1 do
          begin
            Line2.Add(Line1.Points[PointIndex]);
          end;
          QuadTree.RemovePoint(X1, Y1, Line1);
          QuadTree.RemovePoint(X2, Y2, Line2);
          QuadTree.AddPoint(APoint.x, APoint.y, Line2);
          PointLists.Remove(Line1);
        end
        else
        begin
          // Join beginning of line 1 to beginning of line 2
          Line3 := TPointList.Create;
          PointLists.Add(Line3);
          Line3.Capacity := Line1.Count + Line2.Count;
          for PointIndex := Line1.Count - 1 downto 0 do
          begin
            Line3.Add(Line1.Points[PointIndex]);
          end;
          for PointIndex := 0 to Line2.Count - 1 do
          begin
            Line3.Add(Line2.Points[PointIndex]);
          end;
          QuadTree.RemovePoint(X1, Y1, Line1);
          QuadTree.RemovePoint(X2, Y2, Line2);

          APoint := Line1.Points[Line1.Count - 1];
          QuadTree.RemovePoint(APoint.x, APoint.y, Line1);
          QuadTree.AddPoint(APoint.x, APoint.y, Line3);

          APoint := Line2.Points[Line2.Count - 1];
          QuadTree.RemovePoint(APoint.x, APoint.y, Line2);
          QuadTree.AddPoint(APoint.x, APoint.y, Line3);
          PointLists.Remove(Line1);
          PointLists.Remove(Line2);
        end;
      end;
    end;
  end;
end;

procedure GlobalInitializeQuadTree(var QuadTree: TRbwQuadTree;
  ModelGrid: TCustomModelGrid);
var
  CornerPoint: TPoint2d;
  XMin, XMax, YMin, YMax: double;
begin
  if QuadTree = nil then
  begin
    QuadTree := TRbwQuadTree.Create(nil);

    CornerPoint := ModelGrid.TwoDElementCorner(0,0);
    XMin := CornerPoint.x;
    XMax := XMin;
    YMin := CornerPoint.y;
    YMax := YMin;

    CornerPoint := ModelGrid.TwoDElementCorner(ModelGrid.ColumnCount,0);
    XMin := Min(XMin, CornerPoint.x);
    XMax := Max(XMax, CornerPoint.x);
    YMin := Min(YMin, CornerPoint.y);
    YMax := Max(YMax, CornerPoint.y);

    CornerPoint := ModelGrid.TwoDElementCorner(ModelGrid.ColumnCount,ModelGrid.RowCount);
    XMin := Min(XMin, CornerPoint.x);
    XMax := Max(XMax, CornerPoint.x);
    YMin := Min(YMin, CornerPoint.y);
    YMax := Max(YMax, CornerPoint.y);

    CornerPoint := ModelGrid.TwoDElementCorner(0,ModelGrid.RowCount);
    XMin := Min(XMin, CornerPoint.x);
    XMax := Max(XMax, CornerPoint.x);
    YMin := Min(YMin, CornerPoint.y);
    YMax := Max(YMax, CornerPoint.y);

    QuadTree.XMin := XMin;
    QuadTree.XMax := XMax;
    QuadTree.YMin := YMin;
    QuadTree.YMax := YMax;
  end
  else
  begin
    QuadTree.Clear;
  end;
end;


{ TPointList }

function TPointList.Add(Point: TPoint2D): integer;
begin
  if FCount = Capacity then
  begin
    Grow;
  end;
  FPoints[FCount] := Point;
  result := FCount;
  Inc(FCount);
end;

function TPointList.GetCapacity: integer;
begin
  result := Length(FPoints);
end;

function TPointList.GetPoint(Index: integer): TPoint2D;
begin
  result := FPoints[Index];
end;

procedure TPointList.Grow;
var
  Delta: Integer;
  LocalCapacity: integer;
begin
  LocalCapacity := Capacity;
  if LocalCapacity < 16 then
  begin
    Delta := 4;
  end
  else
  begin
    Delta := LocalCapacity  div 4;
  end;
  Capacity := LocalCapacity + Delta;
end;

procedure TPointList.Insert(Position: integer; Point: TPoint2D);
var
  Index: Integer;
begin
  if Count = Capacity then
  begin
    Grow;
  end;
  if Position = Count then
  begin
    Add(Point);
  end
  else
  begin
    for Index := FCount - 1 downto Position do
    begin
      FPoints[Index+1] := FPoints[Index];
    end;
    FPoints[Position] := Point;
    Inc(FCount);
  end;
end;

procedure TPointList.SetCapacity(const Value: integer);
begin
  SetLength(FPoints, Value);
end;

procedure TPointList.SetPoint(Index: integer; const Value: TPoint2D);
begin
  FPoints[Index] := Value;
end;

{ TContourExtractor }

constructor TContourExtractor.Create(Model: TBaseModel);
begin
  FModelGrid := (Model as TPhastModel).Grid;
end;

procedure TContourExtractor.CreateShapes(ValueList: TValueArrayStorage;
  DataArray: TDataArray; FileName: string);
var
  ContourCreator: TContourCreator;
  ValueIndex: Integer;
  PointListIndex: Integer;
  PointList: TPointList;
  PointIndex: Integer;
  PointsForShape: TList;
  ShapeFileWriter: TShapefileGeometryWriter;
  Shape: TShapeObject;
  PPoint: TPoint2DPtr;
  Fields: TStringList;
  FieldName: AnsiString;
  ShapeDataBase: TXBase;
  FieldDescription: AnsiString;
  MinValue, MaxValue: double;
  DSValues: TStringList;
  FieldFormat: AnsiString;
  Contours: TContours;
begin
  ActiveDataSet := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(rsActive);
  DataSet := DataArray;
  ViewDirection := vdTop;
  Grid := FModelGrid.ContourGrid(DataArray.EvaluatedAt,
    frmGoPhast.PhastModel.ModelSelection, vdTop, FModelGrid.SelectedLayer);
  Assert(Grid <> nil);
  Assert(ValueList.Count > 0);

  Contours :=  DataSet.Contours;
  DSValues := TStringList.Create;
  try
    AssignGridValues(MinValue, MaxValue, FModelGrid.SelectedLayer, DSValues, vdTop);

    ShapeDataBase := TXBase.Create(nil);
    try
      Fields := TStringList.Create;
      try
        FieldName := AnsiString(UpperCase(Copy(DataArray.Name, 1, 10)));
        FieldName := FixShapeFileFieldName(FieldName);
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
        FieldDescription := FieldName + '=' + FieldFormat;
        Fields.Add(string(FieldDescription));
        InitializeDataBase(FileName, ShapeDataBase, Fields);
      finally
        Fields.Free;
      end;

      FPointLists:= TObjectList.Create;
      ContourCreator:= TContourCreator.Create;
      ShapeFileWriter := TShapefileGeometryWriter.Create(stPolyLine, True);
      try
        InitializeEpsilon;
        ContourCreator.EvaluatedAt := DataArray.EvaluatedAt;
        ContourCreator.Grid := Grid;
        ContourCreator.OnExtractSegments := ImportSegments;

        Assert(ValueList.Count = Length(Contours.ContourValues));
        for ValueIndex := 0 to ValueList.Count - 1 do
        begin
          ContourCreator.Value := Contours.ContourValues[ValueIndex];

          InitializeQuadTree;
          FPointLists.Clear;
          ContourCreator.ExtractContour;

          if (FPointLists.Count> 0) then
          begin
            ShapeFileWriter.Capacity := ShapeFileWriter.Capacity
              + FPointLists.Count;
            for PointListIndex := 0 to FPointLists.Count - 1 do
            begin
              PointsForShape := TList.Create;
              try
                PointList := FPointLists[PointListIndex];
                for PointIndex := 0 to PointList.Count - 1 do
                begin
                  if (PointIndex > 0) and (PointIndex < PointList.Count - 1) then
                  begin
                    if not Collinear(PointList.Points[PointIndex-1],
                      PointList.Points[PointIndex],
                      PointList.Points[PointIndex+1]) then
                    begin
                      PointsForShape.Add(@PointList.FPoints[PointIndex])
                    end;
                  end
                  else
                  begin
                    PointsForShape.Add(@PointList.FPoints[PointIndex])
                  end;
                end;
                if PointsForShape.Count > 0 then
                begin
                  Shape := TShapeObject.Create;
                  try
                    Shape.FNumPoints := PointsForShape.Count;
                    Shape.FShapeType := stPolyLine;
                    SetLength(Shape.FPoints, PointsForShape.Count);
                    Shape.FNumParts := 1;
                    SetLength(Shape.FParts, 1);
                    Shape.FParts[0] := 0;
                    SetLength(Shape.FPartTypes, 0);
                    for PointIndex := 0 to PointsForShape.Count - 1 do
                    begin
                      PPoint := PointsForShape[PointIndex];
                      Shape.FPoints[PointIndex].X := PPoint.x;
                      Shape.FPoints[PointIndex].Y := PPoint.y;
                    end;
                    ShapeFileWriter.AddShape(Shape);

                    ShapeDataBase.AppendBlank;
                    case DataArray.DataType of
                      rdtDouble:
                        ShapeDataBase.UpdFieldNum(FieldName, ValueList.RealValues[ValueIndex]);
                      rdtInteger:
                        ShapeDataBase.UpdFieldInt(FieldName, ValueList.IntValues[ValueIndex]);
                      rdtBoolean:
                        if ValueList.BooleanValues[ValueIndex] then
                        begin
                          ShapeDataBase.UpdFieldInt(FieldName, 1);
                        end
                        else
                        begin
                          ShapeDataBase.UpdFieldInt(FieldName, 0);
                        end;
                      rdtString:
                        ShapeDataBase.UpdFieldStr(FieldName, AnsiString(ValueList.StringValues[ValueIndex]));
                      else
                        Assert(False);
                    end;

                    ShapeDataBase.PostChanges;
                  except
                    Shape.Free;
                    raise;
                  end;
                end;
              finally
                PointsForShape.Free;
              end;
            end;
          end;
        end;
        ShapeFileWriter.WriteToFile(FileName, ChangeFileExt(FileName, '.shx'));
      finally
        ShapeFileWriter.Free;
        ContourCreator.Free;
        FPointLists.Free;
      end;
    finally
      ShapeDataBase.Active := False;
      ShapeDataBase.Free;
    end;
  finally
    DSValues.Free;
  end;
end;

destructor TContourExtractor.Destroy;
begin
  FQuadTree.Free;
  inherited;
end;

procedure TContourExtractor.ImportSegments(Sender: TObject;
  const Segments: TLine2DArray);
begin
  GlobalImportSegments(Sender, Segments, FEpsilon, FQuadTree, FPointLists);
end;

procedure TContourExtractor.InitializeEpsilon;
//var
//  Index: Integer;
begin
  GlobalInitializeEpsilon(FEpsilon, FModelGrid);
//  FEpsilon := FModelGrid.ColumnWidth[0];
//  for Index := 1 to FModelGrid.ColumnCount - 1 do
//  begin
//    FEpsilon := Min(FEpsilon, FModelGrid.ColumnWidth[Index]);
//  end;
//  for Index := 0 to FModelGrid.RowCount - 1 do
//  begin
//    FEpsilon := Min(FEpsilon, FModelGrid.RowWidth[Index]);
//  end;
//  FEpsilon := Sqr(FEpsilon / 4);
end;

procedure TContourExtractor.InitializeQuadTree;
begin
  GlobalInitializeQuadTree(FQuadTree, FModelGrid);
end;

procedure GlobalInitializeEpsilon(var Epsilon: Real; ModelGrid: TCustomModelGrid);
var
  Index: Integer;
begin
  Epsilon := ModelGrid.ColumnWidth[0];
  for Index := 1 to ModelGrid.ColumnCount - 1 do
  begin
    Epsilon := Min(Epsilon, ModelGrid.ColumnWidth[Index]);
  end;
  for Index := 0 to ModelGrid.RowCount - 1 do
  begin
    Epsilon := Min(Epsilon, ModelGrid.RowWidth[Index]);
  end;
  Epsilon := Sqr(Epsilon / 4);
end;


end.
