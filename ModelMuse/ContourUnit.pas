{@name is used to create a series of contour lines
based on a grid of data values.
@author(Richard B. Winston <rbwinst@usgs.gov>)
}
unit ContourUnit;

interface

uses Classes, Graphics, FastGeo, GR32, ZoomBox2, GoPhastTypes, DataSetUnit,
  ColorSchemes, QuadTreeClass, Generics.Collections;

const
  DefaultLineThickness = 2;
  DefaultMajorLineThickness = 4;

type
  T2DGridPoint = record
    P: TPoint2D;
    Value: TFloat;
    Active: boolean;
  end;

  P2DGridPoint = ^T2DGridPoint;
  TGridSquare = record
    // Points to 4 grid points (square)
    GridPoint : Array [0..3] of P2DGridPoint;
  end;

  T2DGrid = array of array of T2DGridPoint;
  TSquares = array of array of TGridSquare;

  TExtractSegmentEvent = procedure (Sender: TObject;
    const Segments: TLine2DArray) of Object;

  TLabel = class(TObject)
    X: integer;
    Y: integer;
    Value: string;
  end;

  TLabelObjectList = TObjectList<TLabel>;

  TContourCreator = class(TObject)
  private
    FBitMap: TBitmap32;
    FColor: TColor32;
    FGrid: T2DGrid;
    FLineThickness: single;
    FValue: TFloat;
    FZoomBox: TQRbwZoomBox2;
    // See @link(OnExtractSegments).
    // During execution of DrawContour, @name is temporarily set to
    // @link(ConvertAndDrawSegments).
    FOnExtractSegments: TExtractSegmentEvent;
    FEvaluatedAt: TEvaluatedAt;
    FLabelLocations: TRbwQuadTree;
    FLabels: TLabelObjectList;
    procedure ExtractSegments(const GridSquare: TGridSquare);
    procedure ConvertAndDrawSegments(Sender: TObject;
      const SegmentArray: TLine2DArray);
  public
    property BitMap: TBitmap32 read FBitMap write FBitMap;
    property EvaluatedAt: TEvaluatedAt read FEvaluatedAt write FEvaluatedAt;
    property Color: TColor32 read FColor write FColor;
    property Grid: T2DGrid read FGrid write FGrid;
    property LineThickness: single read FLineThickness write FLineThickness;
    property Value: TFloat read FValue write FValue;
    property ZoomBox: TQRbwZoomBox2 read FZoomBox write FZoomBox;
    property LabelLocations: TRbwQuadTree read FLabelLocations write FLabelLocations;
    property Labels: TLabelObjectList read FLabels write FLabels;
    // While @link(ExtractContour) is being executed, @name is called
    // multiple times.  The contents of Segments will be one or more
    // segments in the contour being extracted.
    property OnExtractSegments: TExtractSegmentEvent read FOnExtractSegments
      write FOnExtractSegments;
    procedure DrawContour;
    procedure ExtractContour;
  end;

  TCustomContourCreator = class(TObject)
  private
    FActiveDataSet: TDataArray;
    FDataSet: TDataArray;
    FViewDirection: TViewDirection;
    FGrid: T2DGrid;
    procedure EvaluateMinMaxLgr(out MaxValue, MinValue: Double;
      DSValues: TStringList; 
      ViewDirection: TViewDirection);
  protected
    // @name set Active for the selected column, row, or layer based
    // on @link(ActiveDataSet)
    procedure EvaluateActive(var Active: T3DBooleanDataSet;
      AnActiveDataSet: TDataArray);
    {If @link(DataSet).DataType is rdtString, DSValues will contain a sorted
    list of the unigue values in @link(DataSet).  Otherwise,
    MaxValue and MinValue will be set to the maximum and minimum values in
    @link(DataSet) where Active is true.}
    procedure EvaluateMinMax(out MaxValue, MinValue: Double;
      DSValues: TStringList; Active: T3DBooleanDataSet; ADataArray: TDataArray;
      SelectedColRowLayer: Integer);
      // @name calls @link(EvaluateActive) and @link(EvaluateMinMax)
      // and then assigns values to @link(FGrid).
    procedure AssignGridValues(out MinValue, MaxValue: double;
      SelectedColRowLayer: integer; DSValues: TStringList; ViewDirection: TViewDirection);
  public
    property ActiveDataSet: TDataArray read FActiveDataSet write FActiveDataSet;
    property DataSet: TDataArray read FDataSet write FDataSet;
    property ViewDirection: TViewDirection read FViewDirection
      write FViewDirection;
    property Grid: T2DGrid read FGrid write FGrid;
  end;

  TMultipleContourCreator = class(TCustomContourCreator)
  private
    FBitMap: TBitmap32;
    FZoomBox: TQRbwZoomBox2;
    FLabelLocations: TRbwQuadTree;
    FLabels: TLabelObjectList;
    // @name calls @link(TContourCreator.DrawContour) for each memeber of
    // ContourValues.
    procedure CreateAndDrawContours(const ContourValues,
      LineThicknesses: TOneDRealArray; const ContourColors: TArrayOfColor32);
    // @name initializes the @link(TDataArray)s and then calls
    // @link(AssignGridValues) and @link(CreateAndDrawContours).
    // @name is called if @link(TContours.SpecifyContours
    // DataSet.Contours.SpecifyContours) is true;
    procedure DrawContours(const ContourValues, LineThicknesses: TOneDRealArray;
      const ContourColors: TArrayOfColor32; SelectedColRowLayer: integer;
        ViewDirection: TViewDirection); overload;
    // @name updates MinValue and MaxValue base on limits
    // in @link(DataSet).ContourLimits.
    procedure GetSpecifiedMinMax(var MinValue: Double; var MaxValue: Double;
      DSValues: TStringList);
    // @name returns the values need to define the contour values.
    procedure GetContouringParameters(var RequiredSize: Integer;
      MinValue: Double; MaxValue: Double; var DesiredSpacing: Double;
      var SmallestContour: Double; var LargestContour: Double);
    // @name initializes ContourValues.
    procedure GetContourValues(LargestContour, SmallestContour: Double;
      RequiredSize: Integer; var ContourValues: TOneDRealArray);
    // @name initializes LineThicknesses and ContourColors.
    procedure GetContourColorsAndThicknesses(DesiredSpacing: Double;
      RequiredSize: Integer; var LineThicknesses: TOneDRealArray;
      var ContourColors: TArrayOfColor32; ContourValues: TOneDRealArray;
      ColorParameters: TColorParameters);
  public
    Constructor Create;
    Destructor Destroy; override;
    property BitMap: TBitmap32 read FBitMap write FBitMap;
    property ZoomBox: TQRbwZoomBox2 read FZoomBox write FZoomBox;
    procedure DrawContours(SelectedColRowLayer: integer;
      ColorParameters: TColorParameters; ViewDirection: TViewDirection); overload;
  end;


implementation

uses Math, RbwParser, BigCanvasMethods, PhastModelUnit,
  SysUtils, Types, frmGoPhastUnit, frmDisplayDataUnit;

function Interpolate(const C1, C2 : TPoint2D; Val1, Val2 : TFloat;
  ContourValue: TFloat) : TPoint2D;
var mu : TFloat;
begin
  if (Val1 = ContourValue) then
    Result := C1
  else
  if (Val2 = ContourValue) then
    Result := C2
  else
  begin
    mu := (ContourValue - Val1) / (Val2 - Val1);
    Result.x := C1.x + mu * (C2.x - C1.x);
    Result.y := C1.y + mu * (C2.y - C1.y);
  end;
end;

type TPointArray4 = array[0..3] of TPoint2D;

{ TContourCreator }

procedure TContourCreator.ConvertAndDrawSegments(Sender: TObject;
  const SegmentArray: TLine2DArray);
const
  LabelSpacing = 100;
var
  SegmentI: array[0..3] of TPoint;
  Index: Integer;
  CenterX: Integer;
  CenterY: Integer;
  PointX: double;
  PointY: double;
  Data: Pointer;
//  ALabel: string;
  ASize: tagSIZE;
  LabelObject: TLabel;
begin
  for Index := 0 to Length(SegmentArray) - 1 do
  begin
    SegmentI[Index * 2].X := ZoomBox.XCoord(SegmentArray[Index, 1].x);
    SegmentI[Index * 2].y := ZoomBox.YCoord(SegmentArray[Index, 1].y);
    SegmentI[Index * 2 + 1].X := ZoomBox.XCoord(SegmentArray[Index, 2].x);
    SegmentI[Index * 2 + 1].y := ZoomBox.YCoord(SegmentArray[Index, 2].y);
  end;
  for Index := 0 to Length(SegmentArray) - 1 do
  begin
    DrawBigPolyline32(Bitmap, Color, LineThickness,
      SegmentI, True, False, Index * 2, 2);
    if LabelLocations <> nil then
    begin
      Assert(Labels <> nil);
      CenterX := (SegmentI[Index * 2].X + SegmentI[Index * 2 + 1].X) div 2;
      CenterY := (SegmentI[Index * 2].Y + SegmentI[Index * 2 + 1].Y) div 2;
      if (CenterX > 0) and (CenterY > 0)
        and (CenterX < LabelLocations.XMax) and (CenterY < LabelLocations.YMax) then
      begin
        PointX := CenterX;
        PointY := CenterY;

        if LabelLocations.Count > 0 then
        begin
          LabelLocations.FirstNearestPoint(PointX, PointY, Data);
        end;
        if (LabelLocations.Count = 0) or (Data <> nil) then
        begin
          if (LabelLocations.Count = 0)
            or (Distance(CenterX, CenterY, PointX, PointY) > LabelSpacing) then
          begin
            LabelLocations.AddPoint(CenterX, CenterY, Pointer(1));

            LabelObject := TLabel.Create;
            LabelObject.Value := FloatToStrF(Value, ffGeneral, 7, 0);
            ASize := Bitmap.TextExtent(LabelObject.Value);
            LabelObject.X := CenterX - ASize.cx div 2;
            LabelObject.Y := CenterY - ASize.cy div 2;
            Labels.Add(LabelObject);
          end;
        end;
      end;
    end;
  end;
end;

procedure TContourCreator.DrawContour;
var
  Temp: TExtractSegmentEvent;
begin
  Assert(Assigned(BitMap));
  Assert(Assigned(ZoomBox));
  Temp := OnExtractSegments;
  try
    OnExtractSegments := ConvertAndDrawSegments;
    ExtractContour;
  finally
    OnExtractSegments := Temp;
  end;
end;

procedure TContourCreator.ExtractContour;
var
  cy: Integer;
  cx: Integer;
  Squares: TSquares;
  GridSize2: Integer;
  GridSize1: Integer;
begin
  Assert(Assigned(Grid));
  Assert(Assigned(OnExtractSegments));

  GridSize1 := Length(Grid);
  if GridSize1 > 0 then
  begin
    GridSize2 := Length(Grid[0]);
    if GridSize2 > 0 then
    begin
      SetLength(Squares, GridSize1 - 1, GridSize2 - 1);
      for cx := 0 to GridSize1 - 2 do
      begin
        for cy := 0 to GridSize2 - 2 do
        begin
          Squares[cx, cy].GridPoint[0] := @Grid[cx, cy];
          Squares[cx, cy].GridPoint[1] := @Grid[cx + 1, cy];
          Squares[cx, cy].GridPoint[2] := @Grid[cx, cy + 1];
          Squares[cx, cy].GridPoint[3] := @Grid[cx + 1, cy + 1];
        end;
      end;
      case EvaluatedAt of
        eaBlocks:
          begin
            for cx := 0 to GridSize1 - 2 do
            begin
              for cy := 0 to GridSize2 - 2 do
              begin
                ExtractSegments(Squares[cx, cy]);
              end;
            end;
          end;
        eaNodes:
          begin
            for cx := 1 to GridSize1 - 3 do
            begin
              for cy := 1 to GridSize2 - 3 do
              begin
                ExtractSegments(Squares[cx, cy]);
              end;
            end;
          end;
        else Assert(False);
      end;
    end;
  end;
end;

procedure TContourCreator.ExtractSegments(const GridSquare: TGridSquare);
var
  TemDist: Extended;
  Index: Integer;
  MinDistance: Extended;
  MinIndex: Integer;
  LineIndex: Integer;
  Segments: TPointArray4;
  Count: Integer;
  SegmentArray: TLine2DArray;
begin
  // Determine the index into the edge table which tells
  // us which vertices are inside/outside the metaballs
  LineIndex := 0;
  if GridSquare.GridPoint[0]^.Active
    and GridSquare.GridPoint[1]^.Active
    and GridSquare.GridPoint[2]^.Active
    and GridSquare.GridPoint[3]^.Active
    then
  begin
    if GridSquare.GridPoint[0]^.Value < Value then
      LineIndex := LineIndex or 1;
    if GridSquare.GridPoint[1]^.Value < Value then
      LineIndex := LineIndex or 2;
    if GridSquare.GridPoint[2]^.Value < Value then
      LineIndex := LineIndex or 4;
    if GridSquare.GridPoint[3]^.Value < Value then
      LineIndex := LineIndex or 8;
  end;
  Count := 0;
  case LineIndex of
    0, 15:
      begin
        Count := 0;
      end;
    1, 14:
      begin
        Count := 2;
        Segments[0] := Interpolate(
          GridSquare.GridPoint[0].P,
          GridSquare.GridPoint[1].P,
          GridSquare.GridPoint[0].Value,
          GridSquare.GridPoint[1].Value,
          value);
        Segments[1] := Interpolate(
          GridSquare.GridPoint[0].P,
          GridSquare.GridPoint[2].P,
          GridSquare.GridPoint[0].Value,
          GridSquare.GridPoint[2].Value,
          value);
      end;
    2, 13:
      begin
        Count := 2;
        Segments[0] := Interpolate(
          GridSquare.GridPoint[1].P,
          GridSquare.GridPoint[0].P,
          GridSquare.GridPoint[1].Value,
          GridSquare.GridPoint[0].Value,
          value);
        Segments[1] := Interpolate(
          GridSquare.GridPoint[1].P,
          GridSquare.GridPoint[3].P,
          GridSquare.GridPoint[1].Value,
          GridSquare.GridPoint[3].Value,
          value);
      end;
    3, 12:
      begin
        Count := 2;
        Segments[0] := Interpolate(
          GridSquare.GridPoint[0].P,
          GridSquare.GridPoint[2].P,
          GridSquare.GridPoint[0].Value,
          GridSquare.GridPoint[2].Value,
          value);
        Segments[1] := Interpolate(
          GridSquare.GridPoint[1].P,
          GridSquare.GridPoint[3].P,
          GridSquare.GridPoint[1].Value,
          GridSquare.GridPoint[3].Value,
          value);
      end;
    4, 11:
      begin
        Count := 2;
        Segments[0] := Interpolate(
          GridSquare.GridPoint[2].P,
          GridSquare.GridPoint[0].P,
          GridSquare.GridPoint[2].Value,
          GridSquare.GridPoint[0].Value,
          value);
        Segments[1] := Interpolate(
          GridSquare.GridPoint[2].P,
          GridSquare.GridPoint[3].P,
          GridSquare.GridPoint[2].Value,
          GridSquare.GridPoint[3].Value,
          value);
      end;
    5, 10:
      begin
        Count := 2;
        Segments[0] := Interpolate(
          GridSquare.GridPoint[0].P,
          GridSquare.GridPoint[1].P,
          GridSquare.GridPoint[0].Value,
          GridSquare.GridPoint[1].Value,
          value);
        Segments[1] := Interpolate(
          GridSquare.GridPoint[3].P,
          GridSquare.GridPoint[2].P,
          GridSquare.GridPoint[3].Value,
          GridSquare.GridPoint[2].Value,
          value);
      end;
    6, 9:
      begin
        Count := 4;
        MinIndex := 0;
        MinDistance := Abs(Value - GridSquare.GridPoint[0].Value);
        for Index := 1 to 3 do
        begin
          TemDist := Abs(Value - GridSquare.GridPoint[Index].Value);
          if TemDist < MinDistance then
          begin
            MinDistance := TemDist;
            MinIndex := Index;
          end;
        end;
        case MinIndex of
          0, 3:
            begin
              Segments[0] := Interpolate(
                GridSquare.GridPoint[2].P,
                GridSquare.GridPoint[0].P,
                GridSquare.GridPoint[2].Value,
                GridSquare.GridPoint[0].Value,
                value);
              Segments[1] := Interpolate(
                GridSquare.GridPoint[0].P,
                GridSquare.GridPoint[1].P,
                GridSquare.GridPoint[0].Value,
                GridSquare.GridPoint[1].Value,
                value);
              Segments[2] := Interpolate(
                GridSquare.GridPoint[1].P,
                GridSquare.GridPoint[3].P,
                GridSquare.GridPoint[1].Value,
                GridSquare.GridPoint[3].Value,
                value);
              Segments[3] := Interpolate(
                GridSquare.GridPoint[2].P,
                GridSquare.GridPoint[3].P,
                GridSquare.GridPoint[2].Value,
                GridSquare.GridPoint[3].Value,
                value);
            end;
          1, 2:
            begin
              Segments[0] := Interpolate(
                GridSquare.GridPoint[0].P,
                GridSquare.GridPoint[1].P,
                GridSquare.GridPoint[0].Value,
                GridSquare.GridPoint[1].Value,
                value);
              Segments[1] := Interpolate(
                GridSquare.GridPoint[1].P,
                GridSquare.GridPoint[3].P,
                GridSquare.GridPoint[1].Value,
                GridSquare.GridPoint[3].Value,
                value);
              Segments[2] := Interpolate(
                GridSquare.GridPoint[2].P,
                GridSquare.GridPoint[3].P,
                GridSquare.GridPoint[2].Value,
                GridSquare.GridPoint[3].Value,
                value);
              Segments[3] := Interpolate(
                GridSquare.GridPoint[2].P,
                GridSquare.GridPoint[0].P,
                GridSquare.GridPoint[2].Value,
                GridSquare.GridPoint[0].Value,
                value);
            end;
        else
          Assert(False);
        end;
      end;
    7, 8:
      begin
        Count := 2;
        Segments[0] := Interpolate(
          GridSquare.GridPoint[3].P,
          GridSquare.GridPoint[2].P,
          GridSquare.GridPoint[3].Value,
          GridSquare.GridPoint[2].Value,
          value);
        Segments[1] := Interpolate(
          GridSquare.GridPoint[3].P,
          GridSquare.GridPoint[1].P,
          GridSquare.GridPoint[3].Value,
          GridSquare.GridPoint[1].Value,
          value);
      end;
  else
    begin
      Assert(False);
    end;
  end;
  if Count > 0 then
  begin
    SetLength(SegmentArray, Count div 2);
    for Index := 0 to (Count div 2) - 1 do
    begin
      SegmentArray[Index,1] := Segments[Index*2];
      SegmentArray[Index,2] := Segments[Index*2+1];
    end;
    OnExtractSegments(self, SegmentArray);
  end;
end;

{ TMultipleContourCreator }

procedure TMultipleContourCreator.DrawContours(const ContourValues,
  LineThicknesses: TOneDRealArray; const ContourColors: TArrayOfColor32;
  SelectedColRowLayer: integer; ViewDirection: TViewDirection);
var
  DSValues: TStringList;
  MinValue, MaxValue: double;
begin
  Assert(Assigned(ActiveDataSet));
  Assert(Assigned(DataSet));
  Assert(Assigned(BitMap));
  Assert(Assigned(Grid));
  Assert(Assigned(ZoomBox));
  Assert(Length(ContourValues) = Length(ContourColors));
  Assert(Length(ContourValues) = Length(LineThicknesses));

  DataSet.Initialize;
  ActiveDataSet.Initialize;

  DSValues := TStringList.Create;
  try
    AssignGridValues(MinValue, MaxValue, SelectedColRowLayer, DSValues,
      ViewDirection);
  finally
    DSValues.Free;
  end;
  Assert(Length(ContourValues) = Length(LineThicknesses));
  Assert(Length(ContourValues) = Length(ContourColors));
  CreateAndDrawContours(ContourValues, LineThicknesses, ContourColors);
end;

procedure TCustomContourCreator.AssignGridValues(out MinValue, MaxValue: double;
  SelectedColRowLayer: integer; DSValues: TStringList; ViewDirection: TViewDirection);
var
  Active: T3DBooleanDataSet;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  Column: Integer;
  Row: Integer;
  Layer: Integer;
  DSCol: Integer;
  DSRow: Integer;
  DSLayer: Integer;
  Value: Double;
  Position: Integer;
  Index: Integer;
  ColumnLimit, RowLimit, LayerLimit: integer;
begin
  EvaluateMinMaxLgr(MaxValue, MinValue, DSValues, ViewDirection);
  EvaluateActive(Active, ActiveDataSet);

  LayerLimit := -1;
  RowLimit := -1;
  ColumnLimit := -1;
  case DataSet.EvaluatedAt of
    eaBlocks:
      begin
        ColumnLimit := DataSet.ColumnCount;
        RowLimit := DataSet.RowCount;
        LayerLimit := DataSet.LayerCount;
      end;
    eaNodes:
      begin
        ColumnLimit := DataSet.ColumnCount+1;
        RowLimit := DataSet.RowCount+1;
        LayerLimit := DataSet.LayerCount+1;
      end;
    else Assert(False);
  end;
  case DataSet.Orientation of
    dsoTop: LayerLimit := 1;
    dsoFront: RowLimit := 1;
    dsoSide: ColumnLimit := 1;
    dso3D: ;  // do nothing
    else Assert(False);
  end;

  for ColIndex := 0 to ColumnLimit - 1 do
  begin
    if (ViewDirection = vdSide) and (ColIndex <> SelectedColRowLayer)
      and (DataSet.Orientation <> dsoSide) then
    begin
      Continue;
    end;
    for RowIndex := 0 to RowLimit - 1 do
    begin
      if (ViewDirection = vdFront) and (RowIndex <> SelectedColRowLayer)
        and (DataSet.Orientation <> dsoFront) then
      begin
        Continue;
      end;
      for LayerIndex := 0 to LayerLimit - 1 do
      begin
        if (ViewDirection = vdTop) and (LayerIndex <> SelectedColRowLayer)
          and (DataSet.Orientation <> dsoTop) then
        begin
          Continue;
        end;
        Column := -1;
        Row := -1;
        Layer := -1;
        case ViewDirection of
          vdTop:
            begin
              Column := ColIndex;
              Row := RowIndex;
              Layer := SelectedColRowLayer;
            end;
          vdFront:
            begin
              Column := ColIndex;
              Row := SelectedColRowLayer;
              Layer := LayerIndex;
            end;
          vdSide:
            begin
              Column := SelectedColRowLayer;
              Row := RowIndex;
              Layer := LayerIndex;
            end;
          else Assert(False);
        end;
        if Active[Column,Row,Layer]
          and DataSet.ContourGridValueOK(LayerIndex, RowIndex, ColIndex) then
        begin
          DSCol := Column;
          DSRow := Row;
          DSLayer := Layer;

          case DataSet.Orientation of
            dsoTop:
              begin
                DSLayer := 0;
              end;
            dsoFront:
              begin
                DSRow := 0;
              end;
            dsoSide:
              begin
                DSCol := 0;
              end;
            dso3D: ; // do nothing
            else Assert(False);
          end;

          Value := 0;
          case DataSet.DataType of
            rdtDouble:
              begin
                Value := DataSet.RealData[DSLayer,DSRow,DSCol];
                if DataSet.ContourLimits.LogTransform then
                begin
                  Assert(Value > 0);
                  Value := Log10(Value);
                end;
              end;
            rdtInteger:
              begin
                Value := DataSet.IntegerData[DSLayer,DSRow,DSCol];
              end;
            rdtBoolean:
              begin
                Value := Ord(DataSet.BooleanData[DSLayer,DSRow,DSCol]);
              end;
            rdtString:
              begin
                Value := DSValues.IndexOf(DataSet.StringData[DSLayer,DSRow,DSCol]);
              end;
            else Assert(False);
          end;

          case ViewDirection of
            vdTop:
              begin
                Grid[ColIndex+1,RowIndex+1].Value := Value;
                Grid[ColIndex+1,RowIndex+1].Active := True;
              end;
            vdFront:
              begin
                Grid[ColIndex+1,LayerIndex+1].Value := Value;
                Grid[ColIndex+1,LayerIndex+1].Active := True;
              end;
            vdSide:
              begin
                Grid[RowIndex+1,LayerIndex+1].Value := Value;
                Grid[RowIndex+1,LayerIndex+1].Active := True;
              end;
          end;

        end
        else
        begin
          case ViewDirection of
            vdTop:
              begin
                Grid[ColIndex+1,RowIndex+1].Active := False;
              end;
            vdFront:
              begin
                Grid[ColIndex+1,LayerIndex+1].Active := False;
              end;
            vdSide:
              begin
                Grid[RowIndex+1,LayerIndex+1].Active := False;
              end;
          end;
        end;
      end;
    end;
  end;

  Position := Length(Grid[0])-1;
  for Index := 1 to Length(Grid) - 2 do
  begin
    Grid[Index,0].Value := Grid[Index,1].Value;
    Grid[Index,0].Active := Grid[Index,1].Active;

    Grid[Index,Position].Value := Grid[Index,Position-1].Value;
    Grid[Index,Position].Active := Grid[Index,Position-1].Active;
  end;

  Position := Length(Grid)-1;
  for Index := 0 to Length(Grid[0]) - 1 do
  begin
    Grid[0,Index].Value := Grid[1,Index].Value;
    Grid[0,Index].Active := Grid[1,Index].Active;

    Grid[Position,Index].Value := Grid[Position-1,Index].Value;
    Grid[Position,Index].Active := Grid[Position-1,Index].Active;
  end;
end;

destructor TMultipleContourCreator.Destroy;
begin
  FLabels.Free;
  FLabelLocations.Free;
  inherited;
end;

procedure TMultipleContourCreator.DrawContours(SelectedColRowLayer: integer;
  ColorParameters: TColorParameters; ViewDirection: TViewDirection);
var
  DSValues: TStringList;
  ContourValues: TOneDRealArray;
  LineThicknesses: TOneDRealArray;
  ContourColors: TArrayOfColor32;
  MaxValue: double;
  MinValue: double;
  Index: Integer;
  Contours: TContours;
  DesiredSpacing: Double;
  SmallestContour: double;
  LargestContour: double;
  RequiredSize: integer;
begin
  Assert(Assigned(ActiveDataSet));
  Assert(Assigned(DataSet));
  Assert(Assigned(BitMap));
  Assert(Assigned(Grid));
  Assert(Assigned(ZoomBox));

  try
    Contours :=  DataSet.Contours;
    if (Contours <> nil) and Contours.SpecifyContours then
    begin
      if Contours.AutomaticColors and (Length(Contours.ContourValues) > 0) then
      begin
        MinValue := Contours.ContourValues[0];
        MaxValue := Contours.ContourValues[Length(Contours.ContourValues) -1];
        if MaxValue > MinValue then
        begin
          for Index := 0 to Length(Contours.ContourValues) - 2 do
          begin
            Contours.ContourColors[Index] :=
              Color32(ColorParameters.FracToColor(
              (MaxValue - Contours.ContourValues[Index])/(MaxValue - MinValue)));
          end;
          Contours.ContourColors[Length(Contours.ContourValues) - 1] :=
            Color32(ColorParameters.FracToColor(0));
        end
        else
        begin
          Contours.Count := 0;
        end;
      end;
      // Make copies of arrays so they don't get
      // altered in DrawContours.
      // Even though they are passed as const variables,
      // ContourColors can be altered in DataSet.Initialize.
      ContourValues := Contours.ContourValues;
      LineThicknesses := Contours.LineThicknesses;
      ContourColors := Contours.ContourColors;

      DrawContours(ContourValues, LineThicknesses,
        ContourColors, SelectedColRowLayer, ViewDirection);
      Exit;
    end;

    DataSet.Initialize;
    ActiveDataSet.Initialize;

    DSValues := TStringList.Create;
    try
      AssignGridValues(MinValue, MaxValue, SelectedColRowLayer, DSValues,
        ViewDirection);
      GetSpecifiedMinMax(MinValue, MaxValue, DSValues);

      if MaxValue > MinValue then
      begin
        if DataSet.DataType = rdtBoolean then
        begin
          SetLength(ContourValues, 1);
          SetLength(ContourColors, 1);
          SetLength(LineThicknesses, 1);
          ContourValues[0] := 0.5;
          LineThicknesses[0] := DefaultLineThickness;
          ContourColors[0] := Color32(ColorParameters.FracToColor(0.5));
        end
        else
        begin
          GetContouringParameters(RequiredSize, MinValue, MaxValue,
            DesiredSpacing, SmallestContour, LargestContour);
          GetContourValues(LargestContour, SmallestContour, RequiredSize,
            ContourValues);
          GetContourColorsAndThicknesses(DesiredSpacing, RequiredSize,
            LineThicknesses, ContourColors, ContourValues, ColorParameters);
        end;
      end
      else
      begin
        SetLength(LineThicknesses,0);
        SetLength(ContourColors,0);
        SetLength(ContourValues,0);
      end;

      CreateAndDrawContours(ContourValues, LineThicknesses, ContourColors);
      Contours := TContours.Create;
      try
        Assert(Length(ContourValues) = Length(LineThicknesses));
        Assert(Length(ContourValues) = Length(ContourColors));
        Contours.ContourValues := ContourValues;
        Contours.LineThicknesses := LineThicknesses;
        Contours.ContourColors := ContourColors;
        Contours.ContourStringValues := DSValues;
        DataSet.Contours := Contours;
      finally
        Contours.Free;
      end;
    finally
      DSValues.Free;
    end;
  finally
//    if frmContourData <> nil then
//    begin
//      frmContourData.UpdateContours;
//    end;
    if frmDisplayData <> nil then
    begin
      frmDisplayData.frameContourData.UpdateContours;
    end;

  end;
end;

procedure TMultipleContourCreator.GetContourColorsAndThicknesses(
  DesiredSpacing: Double; RequiredSize: Integer;
  var LineThicknesses: TOneDRealArray; var ContourColors: TArrayOfColor32;
  ContourValues: TOneDRealArray; ColorParameters: TColorParameters);
var
  ContourIndicator: Double;
  Index: Integer;
begin
  SetLength(ContourColors, RequiredSize);
  SetLength(LineThicknesses, RequiredSize);
  for Index := 0 to Length(ContourValues) - 2 do
  begin
    ContourIndicator := ContourValues[Index] / DesiredSpacing / 5;
    if Abs(Round(ContourIndicator) - ContourIndicator) < 0.01 then
    begin
      LineThicknesses[Index] := DefaultMajorLineThickness;
    end
    else
    begin
      LineThicknesses[Index] := DefaultLineThickness;
    end;
    ContourColors[Index] := Color32(ColorParameters.FracToColor(1 - (Index / (Length(ContourValues) - 1))));
  end;
  ContourIndicator := ContourValues[Length(ContourValues) - 1] / DesiredSpacing / 5;
  if Abs(Round(ContourIndicator) - ContourIndicator) < 0.01 then
  begin
    LineThicknesses[Length(ContourValues) - 1] := DefaultMajorLineThickness;
  end
  else
  begin
    LineThicknesses[Length(ContourValues) - 1] := DefaultLineThickness;
  end;
  ContourColors[Length(ContourValues) - 1] := Color32(ColorParameters.FracToColor(0));
end;

procedure TMultipleContourCreator.GetContourValues(LargestContour,
  SmallestContour: Double; RequiredSize: Integer;
  var ContourValues: TOneDRealArray);
var
  Index: Integer;
begin
  SetLength(ContourValues, RequiredSize);
  for Index := 0 to Length(ContourValues) - 2 do
  begin
    ContourValues[Index] := SmallestContour
      + Index * (LargestContour - SmallestContour)
      / (Length(ContourValues) - 1);
  end;
  ContourValues[Length(ContourValues) - 1] := LargestContour;
end;

procedure TMultipleContourCreator.GetContouringParameters(var RequiredSize: Integer; MinValue: Double; MaxValue: Double; var DesiredSpacing: Double; var SmallestContour: Double; var LargestContour: Double);
var
  UsedMin: Double;
  UsedMax: Double;
begin
  if DataSet.ContourLimits.LogTransform then
  begin
    UsedMin := Log10(MinValue);
    UsedMax := Log10(MaxValue);
  end
  else
  begin
    UsedMin := MinValue;
    UsedMax := MaxValue;
  end;
  DesiredSpacing := (UsedMax - UsedMin) / 20;
  DesiredSpacing := Power(10, Trunc(Log10(DesiredSpacing)));
  SmallestContour := Round(UsedMin / DesiredSpacing) * DesiredSpacing;
  while (SmallestContour > UsedMin) do
  begin
    SmallestContour := SmallestContour - DesiredSpacing;
  end;
  while (SmallestContour < UsedMin) do
  begin
    SmallestContour := SmallestContour + DesiredSpacing;
  end;
  LargestContour := Round(UsedMax / DesiredSpacing) * DesiredSpacing;
  while (LargestContour < UsedMax) do
  begin
    LargestContour := LargestContour + DesiredSpacing;
  end;
  while (LargestContour > UsedMax) do
  begin
    LargestContour := LargestContour - DesiredSpacing;
  end;
  RequiredSize := Round((LargestContour - SmallestContour) / DesiredSpacing) + 1;
end;

procedure TMultipleContourCreator.GetSpecifiedMinMax(var MinValue: Double; var MaxValue: Double; DSValues: TStringList);
var
  StringValue: string;
  Position: Integer;
begin
  case DataSet.DataType of
    rdtDouble:
      begin
        if DataSet.ContourLimits.UpperLimit.UseLimit then
        begin
          MaxValue := DataSet.ContourLimits.UpperLimit.RealLimitValue;
        end;
        if DataSet.ContourLimits.LowerLimit.UseLimit then
        begin
          MinValue := DataSet.ContourLimits.LowerLimit.RealLimitValue;
        end;
      end;
    rdtInteger:
      begin
        if DataSet.ContourLimits.UpperLimit.UseLimit then
        begin
          MaxValue := DataSet.ContourLimits.UpperLimit.IntegerLimitValue;
        end;
        if DataSet.ContourLimits.LowerLimit.UseLimit then
        begin
          MinValue := DataSet.ContourLimits.LowerLimit.IntegerLimitValue;
        end;
      end;
    rdtString:
      begin
        if DataSet.ContourLimits.UpperLimit.UseLimit then
        begin
          StringValue := DataSet.ContourLimits.UpperLimit.StringLimitValue;
          MaxValue := DSValues.IndexOf(StringValue);
          if MaxValue < 0 then
          begin
            Position := DSValues.Add(StringValue);
            MaxValue := Position - 0.5;
            DSValues.Delete(Position);
          end;
        end;
        if DataSet.ContourLimits.LowerLimit.UseLimit then
        begin
          StringValue := DataSet.ContourLimits.LowerLimit.StringLimitValue;
          MinValue := DSValues.IndexOf(StringValue);
          if MinValue < 0 then
          begin
            Position := DSValues.Add(StringValue);
            MinValue := Position + 0.5;
            DSValues.Delete(Position);
          end;
        end;
      end;
    rdtBoolean:
      begin
        MinValue := 0;
        MaxValue := 1;
      end;
  else
    Assert(False);
  end;
end;

procedure TCustomContourCreator.EvaluateMinMaxLgr(out MaxValue, MinValue: Double;
  DSValues: TStringList; 
  ViewDirection: TViewDirection);
var
  LocalPhastModel: TPhastModel;
  Active: T3DBooleanDataSet;
  AnActiveDataSet: TDataArray;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildMaxValue: Double;
  ChildMinValue: Double;
begin
  if DataSet.Model is TPhastModel then
  begin
    LocalPhastModel := TPhastModel(DataSet.Model);
  end
  else
  begin
    LocalPhastModel := (DataSet.Model as TChildModel).ParentModel as TPhastModel;
  end;

  AnActiveDataSet := LocalPhastModel.DataArrayManager.GetDataSetByName(rsActive);
  EvaluateActive(Active, AnActiveDataSet);
  case ViewDirection of
    vdTop:
      begin
        EvaluateMinMax(MaxValue, MinValue, DSValues, Active,
          LocalPhastModel.Grid.TopContourDataSet,
          LocalPhastModel.Grid.SelectedLayer);
      end;
    vdFront:
      begin
        EvaluateMinMax(MaxValue, MinValue, DSValues, Active,
          LocalPhastModel.Grid.FrontContourDataSet,
          LocalPhastModel.Grid.SelectedRow);
      end;
    vdSide:
      begin
        EvaluateMinMax(MaxValue, MinValue, DSValues, Active,
          LocalPhastModel.Grid.SideContourDataSet,
          LocalPhastModel.Grid.SelectedColumn);
      end;
    else
      Assert(False);
  end;

  if LocalPhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to LocalPhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalPhastModel.ChildModels[ChildIndex].ChildModel;
      AnActiveDataSet := ChildModel.DataArrayManager.GetDataSetByName(rsActive);
      EvaluateActive(Active, AnActiveDataSet);
      case ViewDirection of
        vdTop:
          begin
            EvaluateMinMax(ChildMaxValue, ChildMinValue, DSValues, Active,
              ChildModel.Grid.TopContourDataSet,
              ChildModel.Grid.SelectedLayer);
          end;
        vdFront:
          begin
            EvaluateMinMax(ChildMaxValue, ChildMinValue, DSValues, Active,
              ChildModel.Grid.FrontContourDataSet,
              ChildModel.Grid.SelectedRow);
          end;
        vdSide:
          begin
            EvaluateMinMax(ChildMaxValue, ChildMinValue, DSValues, Active,
              ChildModel.Grid.SideContourDataSet,
              ChildModel.Grid.SelectedColumn);
          end;
        else
          Assert(False);
      end;
      if MinValue > ChildMinValue then
      begin
        MinValue := ChildMinValue;
      end;
      if MaxValue < ChildMaxValue then
      begin
        MaxValue := ChildMaxValue;
      end;
    end;
  end;
end;

procedure TCustomContourCreator.EvaluateMinMax(out MaxValue, MinValue: Double;
  DSValues: TStringList; Active: T3DBooleanDataSet; ADataArray: TDataArray;
  SelectedColRowLayer: Integer);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  FoundFirst: Boolean;
  ActiveColumn, ActiveRow, ActiveLayer: integer;
begin
  ADataArray.Initialize;
  DSValues.Sorted := True;
  DSValues.Duplicates := dupIgnore;
  MinValue := 0;
  MaxValue := 0;
  FoundFirst := false;
  for ColIndex := 0 to ADataArray.ColumnCount - 1 do
  begin
    if ADataArray.Orientation = dsoSide then
    begin
      ActiveColumn := SelectedColRowLayer;
    end
    else
    begin
      ActiveColumn := ColIndex;
    end;
    for RowIndex := 0 to ADataArray.RowCount - 1 do
    begin
      if ADataArray.Orientation = dsoFront then
      begin
        ActiveRow := SelectedColRowLayer;
      end
      else
      begin
        ActiveRow := RowIndex;
      end;
      for LayerIndex := 0 to ADataArray.LayerCount - 1 do
      begin
        if ADataArray.Orientation = dsoTop then
        begin
          ActiveLayer := SelectedColRowLayer;
        end
        else
        begin
          ActiveLayer := LayerIndex;
        end;
        if Active[ActiveColumn, ActiveRow, ActiveLayer]
          and ADataArray.ContourGridValueOK(LayerIndex, RowIndex, ColIndex) then
        begin
          if ADataArray.DataType = rdtString then
          begin
            FoundFirst := True;
            DSValues.Add(ADataArray.StringData[LayerIndex, RowIndex, ColIndex]);
          end
          else if not FoundFirst then
          begin
            case ADataArray.DataType of
              rdtDouble:
                begin
                  MinValue := ADataArray.RealData[LayerIndex, RowIndex, ColIndex];
                  MaxValue := MinValue;
                  FoundFirst := True;
                end;
              rdtInteger:
                begin
                  MinValue := ADataArray.IntegerData[LayerIndex, RowIndex, ColIndex];
                  MaxValue := MinValue;
                  FoundFirst := True;
                end;
              rdtBoolean:
                begin
                  MinValue := Ord(ADataArray.BooleanData[LayerIndex, RowIndex, ColIndex]);
                  MaxValue := MinValue;
                  FoundFirst := True;
                end;
            else
              Assert(False);
            end;
          end
          else
          begin
            case ADataArray.DataType of
              rdtDouble:
                begin
                  if MinValue > ADataArray.RealData[LayerIndex, RowIndex, ColIndex] then
                  begin
                    MinValue := ADataArray.RealData[LayerIndex, RowIndex, ColIndex];
                  end
                  else if MaxValue < ADataArray.RealData[LayerIndex, RowIndex, ColIndex] then
                  begin
                    MaxValue := ADataArray.RealData[LayerIndex, RowIndex, ColIndex];
                  end;
                end;
              rdtInteger:
                begin
                  if MinValue > ADataArray.IntegerData[LayerIndex, RowIndex, ColIndex] then
                  begin
                    MinValue := ADataArray.IntegerData[LayerIndex, RowIndex, ColIndex];
                  end
                  else if MaxValue < ADataArray.IntegerData[LayerIndex, RowIndex, ColIndex] then
                  begin
                    MaxValue := ADataArray.IntegerData[LayerIndex, RowIndex, ColIndex];
                  end;
                end;
              rdtBoolean:
                begin
                  if MinValue > Ord(ADataArray.BooleanData[LayerIndex, RowIndex, ColIndex]) then
                  begin
                    MinValue := Ord(ADataArray.BooleanData[LayerIndex, RowIndex, ColIndex]);
                  end
                  else if MaxValue < Ord(ADataArray.BooleanData[LayerIndex, RowIndex, ColIndex]) then
                  begin
                    MaxValue := Ord(ADataArray.BooleanData[LayerIndex, RowIndex, ColIndex]);
                  end;
                end;
            else
              Assert(False);
            end;
          end;
        end;
      end;
    end;
  end;
  if ADataArray.DataType = rdtString then
  begin
    MinValue := 0;
    MaxValue := DSValues.Count - 1;
  end;
end;

procedure TCustomContourCreator.EvaluateActive(var Active: T3DBooleanDataSet;
  AnActiveDataSet: TDataArray);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  AnActiveDataSet.Initialize;
  case DataSet.EvaluatedAt of
    eaBlocks:
      begin
        SetLength(Active, AnActiveDataSet.ColumnCount,
          AnActiveDataSet.RowCount, AnActiveDataSet.LayerCount);
      end;
    eaNodes:
      begin
        SetLength(Active, AnActiveDataSet.ColumnCount + 1,
          AnActiveDataSet.RowCount + 1, AnActiveDataSet.LayerCount + 1);
        for ColIndex := 0 to AnActiveDataSet.ColumnCount do
        begin
          for RowIndex := 0 to AnActiveDataSet.RowCount do
          begin
            for LayerIndex := 0 to AnActiveDataSet.LayerCount do
            begin
              Active[ColIndex, RowIndex, LayerIndex] := False;
            end;
          end;
        end;
      end;
  else
    Assert(False);
  end;
  for ColIndex := 0 to AnActiveDataSet.ColumnCount - 1 do
  begin
    for RowIndex := 0 to AnActiveDataSet.RowCount - 1 do
    begin
      for LayerIndex := 0 to AnActiveDataSet.LayerCount - 1 do
      begin
        case DataSet.EvaluatedAt of
          eaBlocks:
            begin
              Active[ColIndex, RowIndex, LayerIndex] :=
                AnActiveDataSet.BooleanData[LayerIndex, RowIndex, ColIndex];
            end;
          eaNodes:
            begin
              if AnActiveDataSet.BooleanData[LayerIndex, RowIndex, ColIndex] then
              begin
                Active[ColIndex, RowIndex, LayerIndex] := True;
                Active[ColIndex + 1, RowIndex, LayerIndex] := True;
                Active[ColIndex, RowIndex + 1, LayerIndex] := True;
                Active[ColIndex + 1, RowIndex + 1, LayerIndex] := True;
                Active[ColIndex, RowIndex, LayerIndex + 1] := True;
                Active[ColIndex + 1, RowIndex, LayerIndex + 1] := True;
                Active[ColIndex, RowIndex + 1, LayerIndex + 1] := True;
                Active[ColIndex + 1, RowIndex + 1, LayerIndex + 1] := True;
              end;
            end;
        else
          Assert(False);
        end;
      end;
    end;
  end;
end;

constructor TMultipleContourCreator.Create;
begin
  inherited;
  FLabelLocations := TRbwQuadTree.Create(nil);
  FLabels := TLabelObjectList.Create;
end;

procedure TMultipleContourCreator.CreateAndDrawContours(
  const ContourValues, LineThicknesses: TOneDRealArray;
  const ContourColors: TArrayOfColor32);
var
  ContourIndex: Integer;
  ContourCreator: TContourCreator;
  AValue: Double;
  LabelIndex: Integer;
  ALabel: TLabel;
begin
  Assert(Length(ContourValues) = Length(LineThicknesses));
  Assert(Length(ContourValues) = Length(ContourColors));
  ContourCreator := TContourCreator.Create;
  try
    ContourCreator.BitMap := BitMap;
    ContourCreator.Grid := Grid;
    ContourCreator.ZoomBox := ZoomBox;
    ContourCreator.EvaluatedAt := DataSet.EvaluatedAt;
    FLabelLocations.Clear;
    FLabels.Clear;
    FLabelLocations.XMin := 0;
    FLabelLocations.YMin := 0;
    FLabelLocations.XMax := ZoomBox.Width;;
    FLabelLocations.YMax := ZoomBox.Height;
    for ContourIndex := 0 to Length(ContourValues) - 1 do
    begin
      AValue := ContourValues[ContourIndex];
      ContourCreator.Value := AValue;
      ContourCreator.Color := ContourColors[ContourIndex];
      ContourCreator.LineThickness := LineThicknesses[ContourIndex];
      if frmGoPhast.PhastModel.ShowContourLabels then
      begin
        ContourCreator.LabelLocations := FLabelLocations;
        ContourCreator.Labels := FLabels;
      end
      else
      begin
        ContourCreator.LabelLocations := nil;
        ContourCreator.Labels := nil;
      end;
      ContourCreator.DrawContour;
    end;
    for LabelIndex := 0 to FLabels.Count - 1 do
    begin
      ALabel := FLabels[LabelIndex];
      FBitMap.Textout(ALabel.X, ALabel.Y, ALabel.Value);
    end;
  finally
    ContourCreator.Free;
  end;
end;

end.
