unit ModflowGridUnit;

interface

uses
  Types, // included to allow inlining or "Point" function.
  SysUtils, Classes, Controls, Graphics, Forms,
  GR32, // TBitmap32 and TFloatRect are declared in GR32.
  GoPhastTypes, AbstractGridUnit, ZoomBox2, OpenGL12x;

type
  TModflowGrid = class(TCustomGrid)
  private
    FLayerElevations: TThreeDRealArray;
    // @name contains the locations of the cell corners, cell centers
    // on the top and bottom surfaces, and the centers of the edges of the
    // top and bottom surfaces.  It does not contain the location of the
    // center of the cell.  It is used for drawing the cell in
    // front, side and 3D Views.
    // The points are in the grid coordinate system not the
    // model coordinate system.
    //
    // Dimensions: 2*ColCount+1, 2*RowCount+1, LayerCount+1.
    CellPoints: T3DRealPointArray3;
    FCellElevationsNeedUpdating: boolean;
    FCellPointsNeedUpdating: boolean;
    procedure SetLayerElevations(const Value: TThreeDRealArray);
    // @name calculates @link(CellPoints).
    procedure UpdateCellPoints;
    procedure IdentifyDividedUnitBoundaries(
      out LayerBoundaries: TOneDIntegerArray; out DividedUnits: Boolean);
    procedure SetLayerLineWidth(var LineWidth: single; LayerIndex: Integer;
      var UnitIndex: Integer; DividedUnits: Boolean;
      LayerBoundaries: TOneDIntegerArray);
    function GetLayerElevations: TThreeDRealArray;
    procedure Write1DArray(const Comment: string; const AnArray: TOneDRealArray;
      const DiscretizationWriter: TObject; const Reverse: boolean);
    procedure WriteALayerArray(const DiscretizationWriter: TObject;
      const LayerIndex: integer; const Comment: string);
    function IsLayerUniform(const LayerIndex: integer): boolean;
    procedure CheckSizeRatios(const AnArray: TOneDRealArray;
      const WarningRoot: string);
    procedure GetMinAndMax(const AnArray: TOneDRealArray; out MinValue,
      MaxValue: double; out MinIndex, MaxIndex: integer);
  protected
    // @name creates an OpenGL display list using @link(FCellsGLIndex)
    // to show colored grid cells or elements.
    procedure RecordColoredGrid; override;
    // @name creates an OpenGL display list using @link(FrontGridGLIndex)
    // to show the grid on the front view of the model.
    procedure RecordFront; override;
    // @name creates an OpenGL display list using @link(GridShellGLIndex)
    // to show the grid shell.
    procedure RecordShell; override;
    // @name creates an OpenGL display list using @link(SideGridGLIndex)
    // to show the grid on the side view of the model.
    procedure RecordSide; override;
    // @name creates an OpenGL display list using @link(TopGridGLIndex)
    // to show the grid on the top view of the model.
    procedure RecordTop; override;
    {@name draws a front view of the grid on TBitmap32.}
    procedure DrawFront(const BitMap: TBitmap32;
      const ZoomBox: TQRbwZoomBox2); override;
    {@name draws a side view of the grid on TBitmap32.}
    procedure DrawSide(const BitMap: TBitmap32;
      const ZoomBox: TQRbwZoomBox2); override;
    // @name sets Elevations to a 3D array of real numbers.  Each
    // number in the array is the elevation of one corner of a cell or
    // element.
    //
    // This procedure would probably have to be modified or replaced for
    // grids such as that in MODFLOW in which elevations can vary
    // within a layer.
    procedure GetCellCornerElevations(const EvalAt: TEvaluatedAt;
      out Elevations: TThreeDRealArray); override;
    // See @link(CellElevation).
    function GetCellElevation(const Column, Row, Layer: integer): real;
      override;
    // See @link(CellThickness).
    function GetCellThickness(const Column, Row, Layer: integer): real;
      override;
    // See @link(CellElevation).
    procedure SetCellElevation(const Column, Row, Layer: integer;
      const Value: real); override;
    // See @link(CellThickness).
    procedure SetCellThickness(const Column, Row, Layer: integer;
      const Value: real); override;
    function GetTwoDCellElevations(const Col, Row: integer): TOneDRealArray; override;
  public
    procedure UpdateCellElevations;
    procedure NotifyGridChanged(Sender: TObject);
    {Copies the properties of Source into self.  Only those properties that
     normally would be saved to file are copied.}
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    function ThreeDElementCorner(const Column, Row, Layer: integer):
      T3DRealPoint; override;
    // @name is the elevations of the top and bottom of the model layers
    // @name is accessed as @name[Col, Row, Layer].
    property LayerElevations: TThreeDRealArray read GetLayerElevations
      write SetLayerElevations;
    // @name has the cell outlines for the MODFLOW cells as
    // viewed from the front.  The dimensions of the result will be
    // @link(ColumnCount)*2+1, @link(LayerCount)+1
    // The outline for a cell will be the points at
    // @orderedList(
    //   @item([Column*2, Layer])
    //   @item([Column*2+1, Layer])
    //   @item([Column*2+2, Layer])
    //   @item([Column*2, Layer+1])
    //   @item([Column*2+1, Layer+1])
    //   @item([Column*2+2, Layer+1])
    // )
    function FrontCellPoints(Row: integer): T2DRealPointArray;
    // @name has the cell outlines for the MODFLOW cells as
    // viewed from the side.  The dimensions of the result will be
    // @link(RowCount)*2+1, @link(LayerCount)+1
    // The outline for a cell will be the points at
    // @orderedList(
    //   @item([Row*2, Layer])
    //   @item([Row*2+1, Layer])
    //   @item([Row*2+2, Layer])
    //   @item([Row*2, Layer+1])
    //   @item([Row*2+1, Layer+1])
    //   @item([Row*2+2, Layer+1])
    // )
    function SideCellPoints(Col: integer): T2DRealPointArray;
    function HighestElevation: real; override;
    function LowestElevation: real; override;
    function GetContainingLayer(ACol, ARow: integer; const AZPosition: real): integer; override;
    function NearestLayerPosition(ACol, ARow: integer; const AZPosition: real;
      const First: integer = -1; const Last: integer = -1): integer;
    procedure WriteDELR(const DiscretizationWriter: TObject);
    procedure WriteDELC(const DiscretizationWriter: TObject);
    procedure WriteTOP(const DiscretizationWriter: TObject);
    procedure WriteBOTM(const DiscretizationWriter: TObject;
      const Model: TObject);
    procedure CheckColumnWidths;
    procedure CheckRowHeights;
    procedure CheckRowToColumnRatios;
    Procedure CheckElevations;
  end;

implementation

uses GR32_Polygons, BigCanvasMethods, frmGoPhastUnit, SparseArrayUnit,
  ModelMuseUtilities, LayerStructureUnit, DataSetUnit, FastGEO,
  ModflowDiscretizationWriterUnit, PhastModelUnit, frmErrorsAndWarningsUnit, 
  IsosurfaceUnit;

procedure ReadReal2DArray(const Reader: TReader;
  var Positions: TTwoDRealArray; const Count1, Count2: integer);
var
  Index: integer;
  CurrentCount: integer;
begin
  with Reader do
  begin
    if Count1 = 0 then
    begin
      CurrentCount := 4;
      SetLength(Positions, CurrentCount);
      ReadListBegin;
      Index := 0;
      while not EndOfList do
      begin
        ReadRealArray(Reader, Positions[Index], Count2);
        Inc(Index);
        if Index = CurrentCount then
        begin
          CurrentCount := CurrentCount * 2;
          SetLength(Positions, CurrentCount);
        end;
      end;
      ReadListEnd;
      SetLength(Positions, Index);
    end
    else
    begin
      SetLength(Positions, Count1);
      ReadListBegin;
      for Index := 0 to Count1 - 1 do
      begin
        ReadRealArray(Reader, Positions[Index], Count2);
      end;
      ReadListEnd;
    end;
  end;
end;

procedure ReadReal3DArray(const Reader: TReader;
  var Positions: TThreeDRealArray; const Count1, Count2, Count3: integer);
var
  Index: integer;
  CurrentCount: integer;
begin
  with Reader do
  begin
    if Count1 = 0 then
    begin
      CurrentCount := 4;
      SetLength(Positions, CurrentCount);
      ReadListBegin;
      Index := 0;
      while not EndOfList do
      begin
        ReadReal2DArray(Reader, Positions[Index], Count2, Count3);
        Inc(Index);
        if Index = CurrentCount then
        begin
          CurrentCount := CurrentCount * 2;
          SetLength(Positions, CurrentCount);
        end;
      end;
      ReadListEnd;
      SetLength(Positions, Index);
    end
    else
    begin
      SetLength(Positions, Count1);
      ReadListBegin;
      for Index := 0 to Count1 - 1 do
      begin
        ReadReal2DArray(Reader, Positions[Index], Count2, Count3);
      end;
      ReadListEnd;
    end;
  end;
end;

procedure WriteReal2DArray(const Writer: TWriter;
  const Positions: TTwoDRealArray);
var
  Count: integer;
  Index: integer;
begin
  with Writer do
  begin
    Count := Length(Positions);
    WriteListBegin;
    for Index := 0 to Count - 1 do
    begin
      WriteRealArray(Writer, Positions[Index]);
    end;
    WriteListEnd;
  end;
end;

procedure WriteReal3DArray(const Writer: TWriter;
  const Positions: TThreeDRealArray);
var
  Count: integer;
  Index: integer;
begin
  with Writer do
  begin
    Count := Length(Positions);
    WriteListBegin;
    for Index := 0 to Count - 1 do
    begin
      WriteReal2DArray(Writer, Positions[Index]);
    end;
    WriteListEnd;
  end;
end;

{ TModflowGrid }

procedure TModflowGrid.Assign(Source: TPersistent);
var
  SourceGrid: TModflowGrid;
begin
  if Source is TModflowGrid then
  begin
    SourceGrid := TModflowGrid(Source);
    LayerElevations := SourceGrid.LayerElevations;
  end;
  inherited Assign(Source);
end;

procedure TModflowGrid.UpdateCellElevations;
var
  LayerGroupIndex: integer;
  LayerGroup: TLayerGroup;
  DataArray: TDataArray;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: integer;
  UnitBottomIndex, UnitTopIndex: integer;
  Fraction: real;
  UnitBottom, UnitTop, UnitHeight: Real;
  FractionIndex: Integer;
begin
  if not FCellElevationsNeedUpdating then Exit;
  if (ColumnCount <= 0) or (RowCount <= 0) or (LayerCount <= 0) then
  begin
    Exit;
  end;

  Assert(frmGoPhast.PhastModel.LayerStructure.LayerCount = LayerCount);
  BeginLayerChange;
  try
    LayerIndex := -1;
    UnitTopIndex := 0;
    SetLength(FLayerElevations, ColumnCount, RowCount, LayerCount+1);

    for LayerGroupIndex := 0 to
      frmGoPhast.PhastModel.LayerStructure.Count -1 do
    begin
      LayerGroup := frmGoPhast.PhastModel.LayerStructure.
        Items[LayerGroupIndex] as TLayerGroup;
      UnitBottomIndex := LayerIndex + LayerGroup.LayerCount;
      DataArray := frmGoPhast.PhastModel.GetDataSetByName(LayerGroup.DataArrayName);
      DataArray.Initialize;
      for ColIndex := 0 to ColumnCount - 1 do
      begin
        for RowIndex := 0 to RowCount - 1 do
        begin
          CellElevation[ColIndex,RowIndex,UnitBottomIndex] :=
            DataArray.RealData[0, RowIndex, ColIndex];
        end;
      end;
      frmGoPhast.PhastModel.AddDataSetToCache(DataArray);
      frmGoPhast.PhastModel.CacheDataArrays;
      Inc(LayerIndex);
      if LayerGroup.Simulated then
      begin
        for FractionIndex := 0 to LayerGroup.LayerCollection.Count - 1 do
        begin
          Fraction := (LayerGroup.LayerCollection.Items[FractionIndex]
            as TLayerFraction).Fraction;
          for ColIndex := 0 to ColumnCount - 1 do
          begin
            for RowIndex := 0 to RowCount - 1 do
            begin
              UnitTop := CellElevation[ColIndex,RowIndex,UnitTopIndex];
              UnitBottom := CellElevation[ColIndex,RowIndex,UnitBottomIndex];
              UnitHeight := UnitTop - UnitBottom;

              CellElevation[ColIndex,RowIndex,LayerIndex] :=
                UnitHeight*Fraction + UnitBottom;
            end;
          end;
          Inc(LayerIndex);
        end;
      end;
      UnitTopIndex := UnitBottomIndex;
      Assert(LayerIndex = UnitTopIndex);
    end;
  finally
    EndLayerChange;
  end;
  FCellElevationsNeedUpdating := False;
end;

procedure TModflowGrid.UpdateCellPoints;
var
  ColIndex: Integer;
  ColWeight1: Real;
  ColWeight2: Real;
  ColCenter: Real;
  ColEdge: Real;
  LayerIndex: Integer;
  RowIndex: Integer;
  RowWeight1: Real;
  RowWeight2: Real;
  RCenter: Real;
  RowEdge: Real;
  CIndex1, CIndex2: integer;
  RIndex1, RIndex2: integer;
begin
  if not FCellPointsNeedUpdating then Exit;
  CellPoints := nil;
  if (ColumnCount <= 0) or (RowCount <= 0) or (LayerCount <= 0) then
  begin
    Exit;
  end;
  UpdateCellElevations;
  SetLength(CellPoints, ColumnCount*2+1, RowCount*2+1, LayerCount+1);

  for ColIndex := 0 to ColumnCount do
  begin
    if ColIndex < ColumnCount then
    begin
      CIndex1:= ColIndex;
    end
    else
    begin
      CIndex1:= ColIndex-1;
    end;
    ColWeight2 := ColumnWidth[CIndex1];

    if ColIndex > 0 then
    begin
      CIndex2 := ColIndex-1;
    end
    else
    begin
      CIndex2 := 0;
    end;
    ColWeight1 := ColumnWidth[CIndex2];

    if ColIndex < ColumnCount then
    begin
      ColCenter := ColumnCenter(ColIndex);
    end
    else
    begin
      ColCenter := -1000000;
    end;
    ColEdge := ColumnPosition[ColIndex];

    for RowIndex := 0 to RowCount do
    begin
      if RowIndex < RowCount then
      begin
        RIndex1 := RowIndex;
      end
      else
      begin
        RIndex1 := RowIndex-1;
      end;
      RowWeight2 := RowWidth[RIndex1];

      if RowIndex > 0 then
      begin
        RIndex2 := RowIndex-1;
      end
      else
      begin
        RIndex2 := 0;
      end;
      RowWeight1 := RowWidth[RIndex2];

      if RowIndex < RowCount then
      begin
        RCenter := RowCenter(RowIndex);
      end
      else
      begin
        RCenter := -1000000;
      end;
      RowEdge := RowPosition[RowIndex];

      for LayerIndex := 0 to LayerCount do
      begin
        if ColIndex < ColumnCount then
        begin
          if RowIndex < RowCount then
          begin
            // Cell center
            CellPoints[ColIndex*2+1,RowIndex*2+1,LayerIndex].X := ColCenter;
            CellPoints[ColIndex*2+1,RowIndex*2+1,LayerIndex].Y := RCenter;
            CellPoints[ColIndex*2+1,RowIndex*2+1,LayerIndex].Z :=
              CellElevation[ColIndex,RowIndex,LayerIndex];
          end;
          // column center
          CellPoints[ColIndex*2+1,RowIndex*2,LayerIndex].X := ColCenter;
          CellPoints[ColIndex*2+1,RowIndex*2,LayerIndex].Y := RowEdge;
          CellPoints[ColIndex*2+1,RowIndex*2,LayerIndex].Z :=
            ((CellElevation[ColIndex,RIndex1,LayerIndex]*RowWeight1)
            + (CellElevation[ColIndex,RIndex2,LayerIndex]*RowWeight2))
            /(RowWeight1+RowWeight2);
        end;
        if RowIndex < RowCount then
        begin
          // Row center
          CellPoints[ColIndex*2,RowIndex*2+1,LayerIndex].X := ColEdge;
          CellPoints[ColIndex*2,RowIndex*2+1,LayerIndex].Y := RCenter;
          CellPoints[ColIndex*2,RowIndex*2+1,LayerIndex].Z :=
            ((CellElevation[CIndex1,RowIndex,LayerIndex]*ColWeight1)
            + (CellElevation[CIndex2,RowIndex,LayerIndex]*ColWeight2))
            /(ColWeight1+ColWeight2);
        end;
        // cell corner
        CellPoints[ColIndex*2,RowIndex*2,LayerIndex].X :=ColEdge;
        CellPoints[ColIndex*2,RowIndex*2,LayerIndex].Y :=RowEdge;
        CellPoints[ColIndex*2,RowIndex*2,LayerIndex].Z :=
          (((CellElevation[CIndex1,RIndex1,LayerIndex]*ColWeight1
          +CellElevation[CIndex2,RIndex1,LayerIndex]*ColWeight2)
          /(ColWeight1+ColWeight2))*RowWeight1
          +
          ((CellElevation[CIndex1,RIndex2,LayerIndex]*ColWeight1
          +CellElevation[CIndex2,RIndex2,LayerIndex]*ColWeight2)
          /(ColWeight1+ColWeight2))*RowWeight2)
          /(RowWeight1+RowWeight2);
      end;
    end;
  end;
  FCellPointsNeedUpdating := False;
end;

procedure TModflowGrid.WriteBOTM(const DiscretizationWriter: TObject;
  const Model: TObject);
var
  PhastModel: TPhastModel;
  LayerIndex: integer;
begin
  PhastModel := Model as TPhastModel;
  for LayerIndex := 1 to LayerCount do
  begin
    WriteALayerArray(DiscretizationWriter, LayerIndex, 'BOTM, '
      + PhastModel.LayerStructure.ModflowLayerBottomDescription(LayerIndex-1));
  end;
end;

procedure TModflowGrid.Write1DArray(const Comment: string;
  const AnArray: TOneDRealArray; const DiscretizationWriter: TObject;
  const Reverse: boolean);
var
  DisWriter: TModflowDiscretizationWriter;
  Index: Integer;
  NewLineNeeded: boolean;
begin
  DisWriter := DiscretizationWriter as TModflowDiscretizationWriter;
  if IsUniform(AnArray) then
  begin
    DisWriter.WriteString('CONSTANT');
    if Reverse then
    begin
      DisWriter.WriteFloat(-AnArray[0]);
    end
    else
    begin
      DisWriter.WriteFloat(AnArray[0]);
    end;
    DisWriter.WriteString(' # ' + Comment);
    DisWriter.NewLine;
  end
  else
  begin
    NewLineNeeded := False;
    DisWriter.WriteU2DRELHeader(Comment);
    for Index := 0 to Length(AnArray) - 1 do
    begin
      if Reverse then
      begin
        DisWriter.WriteFloat(-AnArray[Index]);
      end
      else
      begin
        DisWriter.WriteFloat(AnArray[Index]);
      end;
      if ((Index + 1) mod 10) = 0 then
      begin
        DisWriter.NewLine;
        NewLineNeeded := False;
      end
      else
      begin
        NewLineNeeded := True;
      end;
    end;
    if NewLineNeeded then
    begin
      DisWriter.NewLine;
    end;
  end;
end;

procedure TModflowGrid.WriteDELC(const DiscretizationWriter: TObject);
begin
  Write1DArray('DELC', RowWidths, DiscretizationWriter, True);
end;

procedure TModflowGrid.WriteDELR(const DiscretizationWriter: TObject);
begin
  Write1DArray('DELR', ColWidths, DiscretizationWriter, False);
end;

function TModflowGrid.IsLayerUniform(const LayerIndex: integer): boolean;
var
  Elevations: TThreeDRealArray;
  TestValue: double;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  Elevations := LayerElevations;
  result := True;
  TestValue := Elevations[0,0,LayerIndex];
  for RowIndex := 0 to RowCount - 1 do
  begin
    for ColIndex := 0 to ColumnCount - 1 do
    begin
      result := Elevations[ColIndex,RowIndex,LayerIndex] = TestValue;
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TModflowGrid.WriteALayerArray(const DiscretizationWriter: TObject;
  const LayerIndex: integer; const Comment: string);
var
  DisWriter: TModflowDiscretizationWriter;
  Elevations: TThreeDRealArray;
  RowIndex: Integer;
  ColIndex: Integer;
  NewLineNeeded: boolean;
begin
  Elevations := LayerElevations;
  DisWriter := DiscretizationWriter as TModflowDiscretizationWriter;
  if IsLayerUniform(LayerIndex) then
  begin
    DisWriter.WriteString('CONSTANT');
    DisWriter.WriteFloat(Elevations[0,0,LayerIndex]);
    DisWriter.WriteString(' # ' + Comment);
    DisWriter.NewLine;
  end
  else
  begin
    DisWriter.WriteU2DRELHeader(Comment);
    for RowIndex := 0 to RowCount - 1 do
    begin
      NewLineNeeded := False;
      for ColIndex := 0 to ColumnCount - 1 do
      begin
        DisWriter.WriteFloat(Elevations[ColIndex, RowIndex, LayerIndex]);
        if ((ColIndex + 1) mod 10) = 0 then
        begin
          DisWriter.NewLine;
          NewLineNeeded := False;
        end
        else
        begin
          NewLineNeeded := True;
        end;
      end;
      if NewLineNeeded then
      begin
        DisWriter.NewLine;
      end;
    end;
  end;
end;

procedure TModflowGrid.WriteTOP(const DiscretizationWriter: TObject);
begin
  WriteALayerArray(DiscretizationWriter, 0, 'TOP');
end;

function TModflowGrid.FrontCellPoints(Row: integer): T2DRealPointArray;
var
  ColIndex: Integer;
  LayerIndex: Integer;
begin
  result := nil;
  UpdateCellPoints;
  if CellPoints = nil then
  begin
    Exit;
  end;
  SetLength(result, ColumnCount*2+1, LayerCount+1);
  for ColIndex := 0 to ColumnCount*2 do
  begin
    for LayerIndex := 0 to LayerCount do
    begin
      result[ColIndex, LayerIndex].X :=
        CellPoints[ColIndex, Row*2+1, LayerIndex].X;
      result[ColIndex, LayerIndex].Y :=
        CellPoints[ColIndex, Row*2+1, LayerIndex].Z;
    end;
  end;
end;

procedure TModflowGrid.NotifyGridChanged(Sender: TObject);
begin
  if frmGoPhast.PhastModel <> nil then
  begin
    frmGoPhast.PhastModel.InvalidateSegments;
  end;
  FCellElevationsNeedUpdating:= True;
  FCellPointsNeedUpdating:= True;
  NeedToRecalculateCellColors;
  frmGoPhast.InvalidateAllViews;
end;

procedure TModflowGrid.CheckSizeRatios(const AnArray: TOneDRealArray;
  const WarningRoot: string);
var
  Index: Integer;
  WarningString: string;
  ErrorString: string;
  Ratio: double;
begin
  ErrorString := 'The width of one or more ' + WarningRoot   + ' is zero.';
  for Index := 0 to Length(AnArray) - 1 do
  begin
    if AnArray[Index] = 0 then
    begin
      frmErrorsAndWarnings.AddError(ErrorString, IntToStr(Index+1));
    end;
  end;

  WarningString := 'The ratio between the widths of two adjacent '
    + WarningRoot + ' exceeds the recommended maximum of 1.5';
  for Index := 1 to Length(AnArray) - 1 do
  begin
    if (AnArray[Index-1] <> 0) and (AnArray[Index] <> 0) then
    begin
      Ratio := AnArray[Index-1]/AnArray[Index];
      if (Ratio > 1.5) or (Ratio < 1/1.5) then
      begin
        frmErrorsAndWarnings.AddWarning(WarningString,
          IntToStr(Index) + ', ' + IntToStr(Index+1));
      end;
    end;
  end;
end;

procedure TModflowGrid.CheckColumnWidths;
begin
  CheckSizeRatios(ColWidths, 'columns');
end;

procedure TModflowGrid.CheckElevations;
var
  Elevations: TThreeDRealArray;
  RowIndex: Integer;
  ColIndex: Integer;
  LayerIndex: integer;
  ErrorString: string;
  Active: TDataArray;
begin
  ErrorString := 'The top of one or more cells is below its bottom.';
  Elevations := LayerElevations;
  Active := frmGoPhast.PhastModel.GetDataSetByName(rsActive);
  Active.Initialize;
  for LayerIndex := 1 to LayerCount - 1 do
  begin
    for RowIndex := 0 to RowCount - 1 do
    begin
      for ColIndex := 0 to ColumnCount - 1 do
      begin
        if (Elevations[ColIndex, RowIndex, LayerIndex] >
          Elevations[ColIndex, RowIndex, LayerIndex-1])
          and Active.BooleanData[LayerIndex-1, RowIndex, ColIndex] then
        begin
          frmErrorsAndWarnings.AddError(ErrorString,
            'Column = ' + IntToStr(ColIndex+1)
            + '; Row = ' + IntToStr(RowIndex+1)
            + '; Layer = ' + IntToStr(LayerIndex));
        end;
      end;
    end;
  end;
  frmGoPhast.PhastModel.AddDataSetToCache(Active);
  frmGoPhast.PhastModel.CacheDataArrays;
end;

procedure TModflowGrid.CheckRowHeights;
begin
  CheckSizeRatios(RowWidths, 'rows');
end;

procedure TModflowGrid.GetMinAndMax(const AnArray: TOneDRealArray;
  Out MinValue, MaxValue: double; out MinIndex, MaxIndex: integer);
var
  Index: integer;
begin
  Assert(Length(AnArray) > 0);
  MinValue := AnArray[0];
  MaxValue := AnArray[0];
  MinIndex:= 0;
  MaxIndex := 0;
  for Index := 1 to Length(AnArray) - 1 do
  begin
    if MaxValue < AnArray[Index] then
    begin
      MaxValue := AnArray[Index];
      MaxIndex := Index;
    end;
    if MinValue > AnArray[Index] then
    begin
      MinValue := AnArray[Index];
      MinIndex := Index;
    end;
  end;
end;

procedure TModflowGrid.CheckRowToColumnRatios;
var
  MinCol, MaxCol, MinRow, MaxRow: double;
  MinColIndex, MaxColIndex, MinRowIndex, MaxRowIndex: integer;
  WarningString: string;
begin
  GetMinAndMax(ColWidths, MinCol, MaxCol, MinColIndex, MaxColIndex);
  GetMinAndMax(RowWidths, MaxRow, MinRow, MaxRowIndex, MinRowIndex);
  MaxRow := -MaxRow;
  MinRow := -MinRow;
  WarningString := 'One or more cells have ratios of row to column width that '
    + 'exceed the recommended maximum of 10.';
  if (MinCol <> 0) and (MaxRow/MinCol > 10) then
  begin
    frmErrorsAndWarnings.AddWarning(WarningString,
      'Column ' + IntToStr(MinColIndex+1) + ', '
      + 'Row ' + IntToStr(MaxRowIndex+1));
  end;
  if (MinRow <> 0) and (MaxCol/MinRow > 10) then
  begin
    frmErrorsAndWarnings.AddWarning(WarningString,
      'Column ' + IntToStr(MaxColIndex+1) + ', '
      + 'Row ' + IntToStr(MinRowIndex+1));
  end;
end;

constructor TModflowGrid.Create;
begin
  inherited;
  FCellElevationsNeedUpdating:= True;
  FCellPointsNeedUpdating:= True;
  LayerDirection := ldTopToBottom;
  RowDirection := rdNorthToSouth;
end;

procedure TModflowGrid.DrawFront(const BitMap: TBitmap32;
  const ZoomBox: TQRbwZoomBox2);
var
  FrontPoints: T2DRealPointArray;
  Points: array of TPoint;
  LayerIndex: Integer;
  APoint: TPoint2D;
  LineWidth: single;
  ColIndex: Integer;
  DividedUnits: boolean;
  LayerBoundaries: TOneDIntegerArray;
  UnitIndex: Integer;
  CellOutline: TPointArray;
  AColor, NewColor: TColor;
  PriorLayer: integer;
  ColumnIndex: integer;
  ColumnLimit, LayerLimit: integer;
  LocalEvalAt: TEvaluatedAt;
  LocalLineColor: TColor32;
  P: TPolygon32;
  MultiplePolygons: boolean;
  function Convert2D_FrontPoint(const APoint: TPoint2D): TPoint;
  begin
    result.X := ZoomBox.XCoord(APoint.X);
    result.Y := ZoomBox.YCoord(APoint.Y);
  end;
begin
  // for the time being, don't worry about coloring the grid.
  FrontPoints := FrontCellPoints(SelectedRow);
  if FrontPoints = nil then
  begin
    Exit;
  end;
  P := nil;
  MultiplePolygons := False;

  if (FrontDataSet <> nil) and (FrontDataSet.EvaluatedAt = eaBlocks) then
  begin
    case FrontDataSet.EvaluatedAt of
      eaBlocks:
        begin
          ColumnLimit := ColumnCount - 1;
          LayerLimit := LayerCount - 1;
        end;
      eaNodes:
        begin
          ColumnLimit := ColumnCount;
          LayerLimit := LayerCount;
        end;
    else
      begin
        Assert(False);
        ColumnLimit := -1;
        LayerLimit := -1;
      end;
    end;

    SetLength(CellOutline, 6);
    for ColumnIndex := 0 to ColumnLimit do
    begin
      if LayerCount > 0 then
      begin
        PriorLayer := 0;
        AColor := FrontCellColors[ColumnIndex, 0];
        NewColor := AColor;
        for LayerIndex := 1 to LayerLimit do
        begin
          NewColor := FrontCellColors[ColumnIndex, LayerIndex];
          if (NewColor <> AColor) then
          begin
            if AColor <> clWhite then
            begin
              case FrontDataSet.EvaluatedAt of
                eaBlocks:
                  begin
                    CellOutline[0] := Convert2D_FrontPoint(FrontPoints[ColumnIndex*2,PriorLayer]);
                    CellOutline[1] := Convert2D_FrontPoint(FrontPoints[ColumnIndex*2+1,PriorLayer]);
                    CellOutline[2] := Convert2D_FrontPoint(FrontPoints[ColumnIndex*2+2,PriorLayer]);
                    CellOutline[3] := Convert2D_FrontPoint(FrontPoints[ColumnIndex*2+2,LayerIndex]);
                    CellOutline[4] := Convert2D_FrontPoint(FrontPoints[ColumnIndex*2+1,LayerIndex]);
                    CellOutline[5] := Convert2D_FrontPoint(FrontPoints[ColumnIndex*2,LayerIndex]);
                  end;
                eaNodes:
                  begin
                    Assert(False);
                  end;
              else
                begin
                  Assert(False);
                end;
              end;
              DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
                0, CellOutline, P, MultiplePolygons, True);
            end;
            AColor := NewColor;
            PriorLayer := LayerIndex;

          end;
        end;

        if NewColor <> clWhite then
        begin
          case FrontDataSet.EvaluatedAt of
            eaBlocks:
              begin
                CellOutline[0] := Convert2D_FrontPoint(FrontPoints[ColumnIndex*2,PriorLayer]);
                CellOutline[1] := Convert2D_FrontPoint(FrontPoints[ColumnIndex*2+1,PriorLayer]);
                CellOutline[2] := Convert2D_FrontPoint(FrontPoints[ColumnIndex*2+2,PriorLayer]);
                CellOutline[3] := Convert2D_FrontPoint(FrontPoints[ColumnIndex*2+2,LayerCount]);
                CellOutline[4] := Convert2D_FrontPoint(FrontPoints[ColumnIndex*2+1,LayerCount]);
                CellOutline[5] := Convert2D_FrontPoint(FrontPoints[ColumnIndex*2,LayerCount]);
              end;
            eaNodes:
              begin
                Assert(False);
              end;
          else
            begin
              Assert(False);
            end;
          end;
          DrawBigPolygon32(BitMap, Color32(NewColor), Color32(NewColor),
            0, CellOutline, P, MultiplePolygons, True);
        end;
      end;
    end;
  end;

  SetLength(Points, Length(FrontPoints));

  IdentifyDividedUnitBoundaries(LayerBoundaries, DividedUnits);
  SetLocalEvalAt(vdFront, LocalEvalAt);

  UnitIndex := 0;
  for LayerIndex := 0 to Length(FrontPoints[0]) - 1 do
  begin
    SetLayerLineWidth(LineWidth, LayerIndex, UnitIndex,
      DividedUnits, LayerBoundaries);
    SetLayerLineColor(LayerIndex, LocalEvalAt, LocalLineColor, LineWidth);
    if Not DrawInteriorGridLines2D and (LayerIndex <> 0)
      and (LayerIndex <> (Length(FrontPoints[0]) - 1))
      and (LocalLineColor = clBlack32) then
    begin
      Continue;
    end;
    for ColIndex := 0 to Length(FrontPoints) - 1 do
    begin
      APoint := FrontPoints[ColIndex,LayerIndex];
      Points[ColIndex] := Convert2D_FrontPoint(APoint);
    end;
    DrawBigPolyline32(BitMap, LocalLineColor, LineWidth, Points, True);
  end;
  SetLength(Points, Length(FrontPoints[0]));
  for ColIndex := 0 to ColumnCount do
  begin
    if (ColIndex mod 10 = 0) or (ColIndex = ColumnCount) then
    begin
      LineWidth := ThickGridLineThickness;
    end
    else
    begin
      LineWidth := OrdinaryGridLineThickness;
    end;
    SetColumnLineColor(ColIndex, LocalEvalAt, LocalLineColor, LineWidth);
    if Not DrawInteriorGridLines2D and (ColIndex <> 0)
      and (ColIndex <> ColumnCount)
      and (LocalLineColor = clBlack32) then
    begin
      Continue;
    end;
    for LayerIndex := 0 to Length(FrontPoints[0]) - 1 do
    begin
      APoint := FrontPoints[ColIndex*2,LayerIndex];
      Points[LayerIndex] := Convert2D_FrontPoint(APoint);
    end;
    DrawBigPolyline32(BitMap, LocalLineColor, LineWidth, Points, True);
  end;
  DrawFrontContours(ZoomBox, BitMap);
end;

function TModflowGrid.SideCellPoints(Col: integer): T2DRealPointArray;
var
  RowIndex: Integer;
  LayerIndex: Integer;
begin
  result := nil;
  UpdateCellPoints;
  if CellPoints = nil then
  begin
    Exit;
  end;
  SetLength(result, RowCount*2+1, LayerCount+1);
  for RowIndex := 0 to RowCount*2 do
  begin
    for LayerIndex := 0 to LayerCount do
    begin
      result[RowIndex, LayerIndex].X :=
        CellPoints[Col*2+1, RowIndex, LayerIndex].Y;
      result[RowIndex, LayerIndex].Y :=
        CellPoints[Col*2+1, RowIndex, LayerIndex].Z;
    end;
  end;
end;


procedure TModflowGrid.DrawSide(const BitMap: TBitmap32;
  const ZoomBox: TQRbwZoomBox2);
var
  SidePoints: T2DRealPointArray;
  Points: array of TPoint;
  LayerIndex: Integer;
  APoint: TPoint2D;
  LineWidth: single;
  RowIndex: Integer;
  LayerBoundaries: TOneDIntegerArray;
  DividedUnits: Boolean;
  UnitIndex: Integer;
  CellOutline: TPointArray;
  AColor, NewColor: TColor;
  PriorLayer: integer;
  RowLimit, LayerLimit: integer;
  LocalEvalAt: TEvaluatedAt;
  LocalLineColor: TColor32;
  P: TPolygon32;
  MultiplePolygons: boolean;
  function Convert2D_SidePoint(const APoint: TPoint2D): TPoint;
  begin
    result.X := ZoomBox.XCoord(APoint.Y);
    result.Y := ZoomBox.YCoord(APoint.X);
  end;
begin
  // for the time being, don't worry about coloring the grid.
  SidePoints := SideCellPoints(SelectedColumn);
  if SidePoints = nil then
  begin
    Exit;
  end;
  P := nil;
  MultiplePolygons := False;

  if (SideDataSet <> nil) and (SideDataSet.EvaluatedAt = eaBlocks) then
  begin
    case SideDataSet.EvaluatedAt of
      eaBlocks:
        begin
          RowLimit := RowCount - 1;
          LayerLimit := LayerCount - 1;
        end;
      eaNodes:
        begin
          RowLimit := RowCount;
          LayerLimit := LayerCount;
        end;
    else
      begin
        Assert(False);
        RowLimit := -1;
        LayerLimit := -1;
      end;
    end;

    SetLength(CellOutline, 6);
    for RowIndex := 0 to RowLimit do
    begin
      if LayerCount > 0 then
      begin
        PriorLayer := 0;
        AColor := SideCellColors[RowIndex, 0];
        NewColor := AColor;
        for LayerIndex := 1 to LayerLimit do
        begin
          NewColor := SideCellColors[RowIndex, LayerIndex];
          if (NewColor <> AColor) then
          begin
            if AColor <> clWhite then
            begin
              case SideDataSet.EvaluatedAt of
                eaBlocks:
                  begin
                    CellOutline[0] := Convert2D_SidePoint(SidePoints[RowIndex*2,PriorLayer]);
                    CellOutline[1] := Convert2D_SidePoint(SidePoints[RowIndex*2+1,PriorLayer]);
                    CellOutline[2] := Convert2D_SidePoint(SidePoints[RowIndex*2+2,PriorLayer]);
                    CellOutline[3] := Convert2D_SidePoint(SidePoints[RowIndex*2+2,LayerIndex]);
                    CellOutline[4] := Convert2D_SidePoint(SidePoints[RowIndex*2+1,LayerIndex]);
                    CellOutline[5] := Convert2D_SidePoint(SidePoints[RowIndex*2,LayerIndex]);
                  end;
                eaNodes:
                  begin
                    Assert(False);
                  end;
              else
                begin
                  Assert(False);
                end;
              end;
              DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
                0, CellOutline, P, MultiplePolygons, True);
            end;
            AColor := NewColor;
            PriorLayer := LayerIndex;
          end;
        end;

        if NewColor <> clWhite then
        begin
          case SideDataSet.EvaluatedAt of
            eaBlocks:
              begin
                CellOutline[0] := Convert2D_SidePoint(SidePoints[RowIndex*2,PriorLayer]);
                CellOutline[1] := Convert2D_SidePoint(SidePoints[RowIndex*2+1,PriorLayer]);
                CellOutline[2] := Convert2D_SidePoint(SidePoints[RowIndex*2+2,PriorLayer]);
                CellOutline[3] := Convert2D_SidePoint(SidePoints[RowIndex*2+2,LayerCount]);
                CellOutline[4] := Convert2D_SidePoint(SidePoints[RowIndex*2+1,LayerCount]);
                CellOutline[5] := Convert2D_SidePoint(SidePoints[RowIndex*2,LayerCount]);
              end;
            eaNodes:
              begin
                Assert(False);
              end;
          else
            begin
              Assert(False);
            end;
          end;
          DrawBigPolygon32(BitMap, Color32(NewColor), Color32(NewColor),
            0, CellOutline, P, MultiplePolygons, True);
        end;
      end;
    end;
  end;

  SetLength(Points, Length(SidePoints));
  IdentifyDividedUnitBoundaries(LayerBoundaries, DividedUnits);
  SetLocalEvalAt(vdSide, LocalEvalAt);

  UnitIndex := 0;
  for LayerIndex := 0 to Length(SidePoints[0]) - 1 do
  begin
    SetLayerLineWidth(LineWidth, LayerIndex, UnitIndex,
      DividedUnits, LayerBoundaries);
    SetLayerLineColor(LayerIndex, LocalEvalAt, LocalLineColor, LineWidth);
    if Not DrawInteriorGridLines2D and (LayerIndex <> 0)
      and (LayerIndex <> LayerCount)
      and (LocalLineColor = clBlack32) then
    begin
      Continue;
    end;
    for RowIndex := 0 to Length(SidePoints) - 1 do
    begin
      APoint := SidePoints[RowIndex,LayerIndex];
      Points[RowIndex] :=
      Convert2D_SidePoint(APoint);
    end;
    DrawBigPolyline32(BitMap, LocalLineColor, LineWidth, Points, True);
  end;
  SetLength(Points, Length(SidePoints[0]));
  for RowIndex := 0 to RowCount do
  begin
    if (RowIndex mod 10 = 0) or (RowIndex = RowCount) then
    begin
      LineWidth := ThickGridLineThickness;
    end
    else
    begin
      LineWidth := OrdinaryGridLineThickness;
    end;
    SetRowLineColor(RowIndex, LocalEvalAt, LocalLineColor, LineWidth);
    if Not DrawInteriorGridLines2D and (RowIndex <> 0)
      and (RowIndex <> RowCount)
      and (LocalLineColor = clBlack32) then
    begin
      Continue;
    end;
    for LayerIndex := 0 to Length(SidePoints[0]) - 1 do
    begin
      APoint := SidePoints[RowIndex*2,LayerIndex];
      Points[LayerIndex] :=
      Convert2D_SidePoint(APoint);
    end;
    DrawBigPolyline32(BitMap, LocalLineColor, LineWidth, Points, True);
  end;
  DrawSideContours(ZoomBox, BitMap);
end;

procedure TModflowGrid.GetCellCornerElevations(const EvalAt: TEvaluatedAt;
  out Elevations: TThreeDRealArray);
var
  CCount, RCount, LCount: integer;
  ColIndex, RowIndex, LayerIndex: integer;
  CIndex, RIndex: integer;
  Elev1, Elev2, Elev3, Elev4: double;
  Weight1, Weight2, Weight3, Weight4: double;
begin
  case EvalAt of
    eaBlocks:
      begin
        CCount := ColumnCount + 1;
        RCount := RowCount + 1;
        LCount := LayerCount + 1;
        SetLength(Elevations, CCount, RCount, LCount);
        for ColIndex := 0 to CCount - 1 do
        begin
          if ColIndex = ColumnCount then
          begin
            CIndex := ColumnCount-1;
          end
          else
          begin
            CIndex := ColIndex;
          end;
          for RowIndex := 0 to RCount - 1 do
          begin
            if RowIndex = RowCount then
            begin
              RIndex := RowCount-1;
            end
            else
            begin
              RIndex := RowIndex;
            end;
            for LayerIndex := 0 to LCount - 1 do
            begin
              Elev1 := CellElevation[CIndex, RIndex, LayerIndex];
              Weight1 := ColumnWidth[CIndex];
              Weight3 := RowWidth[RIndex];
              if ColIndex > 0 then
              begin
                Elev2 := CellElevation[ColIndex-1, RIndex, LayerIndex];
                Weight2 := ColumnWidth[ColIndex-1];
              end
              else
              begin
                Elev2 := Elev1;
                Weight2 := Weight1;
              end;
              if RowIndex > 0 then
              begin
                Elev3 := CellElevation[CIndex, RowIndex-1, LayerIndex];
                Weight4 := RowWidth[RowIndex-1];
              end
              else
              begin
                Elev3 := Elev1;
                Weight4 := Weight3;
              end;
              if (ColIndex > 0) and (RowIndex > 0) then
              begin
                Elev4 := CellElevation[ColIndex-1, RowIndex-1, LayerIndex];
              end
              else
              begin
                Elev4 := Elev3;
              end;
              // bilinear interpolation.
              Elevations[ColIndex, RowIndex, LayerIndex] :=
                ((Weight3*(Elev1*Weight1 + Elev2*Weight2)/(Weight1 + Weight2))
                + (Weight4*(Elev3*Weight1 + Elev4*Weight2)/(Weight1 + Weight2)))
                / (Weight3 + Weight4);
            end;
          end;
        end;
      end;
    eaNodes:
      begin
        CCount := ColumnCount + 2;
        RCount := RowCount + 2;
        LCount := LayerCount + 2;
        SetLength(Elevations, CCount, RCount, LCount);
        // The corners of the interior _cells_ will just be the elevations.
        for ColIndex := 1 to CCount - 2 do
        begin
          for RowIndex := 1 to RCount - 2 do
          begin
            for LayerIndex := 0 to LCount - 1 do
            begin
              Elevations[ColIndex, RowIndex, LayerIndex] :=
                CellElevation[ColIndex-1, RowIndex-1, LayerIndex];
            end;
          end;
        end;
        // use linear interpolation along edges.
        for ColIndex := 1 to CCount - 2 do
        begin
          Weight1 := ColumnWidth[ColIndex];
          Weight2 := ColumnWidth[ColIndex-1];
          for LayerIndex := 0 to LCount - 1 do
          begin
            Elevations[ColIndex, 0, LayerIndex] :=
              (CellElevation[ColIndex, 0, LayerIndex]*Weight1
              + CellElevation[ColIndex-1, 0, LayerIndex]*Weight2)
              /(Weight1+Weight2);
            Elevations[ColIndex, RowCount + 1, LayerIndex] :=
              (CellElevation[ColIndex, RowCount-1, LayerIndex]*Weight1
              + CellElevation[ColIndex-1, RowCount-1, LayerIndex]*Weight2)
              /(Weight1+Weight2);
          end;
        end;
        for RowIndex := 1 to RCount - 2 do
        begin
          Weight1 := RowWidth[RowIndex];
          Weight2 := RowWidth[RowIndex-1];
          for LayerIndex := 0 to LCount - 1 do
          begin
            Elevations[0, RowIndex, LayerIndex] :=
              (CellElevation[0, RowIndex, LayerIndex]*Weight1
              + CellElevation[0, RowIndex-1, LayerIndex]*Weight2)
              /(Weight1+Weight2);
            Elevations[ColumnCount+1, RowIndex, LayerIndex] :=
              (CellElevation[ColumnCount-1, RowIndex, LayerIndex]*Weight1
              + CellElevation[ColumnCount-1, RowIndex-1, LayerIndex]*Weight2)
              /(Weight1+Weight2);
          end;
        end;
        // corners of mesh elevations are the elevations of the nearest cells.
        for LayerIndex := 0 to LCount - 1 do
        begin
          Elevations[0, 0, LayerIndex] := CellElevation[0, 0, LayerIndex];
          Elevations[ColumnCount, 0, LayerIndex] :=
            CellElevation[ColumnCount-1, 0, LayerIndex];
          Elevations[0, RowCount, LayerIndex] :=
            CellElevation[0, RowCount-1, LayerIndex];
          Elevations[ColumnCount, RowCount, LayerIndex] :=
            CellElevation[ColumnCount-1, RowCount-1, LayerIndex];
        end;
      end;
  else
    Assert(False);
  end;
end;

function TModflowGrid.GetCellElevation(const Column, Row, Layer: integer): real;
begin
  if (ColumnCount > 0) and (RowCount > 0) and (LayerCount > 0)  then
  begin
    if (FLayerElevations = nil)
      or (Length(FLayerElevations) <> ColumnCount)
      or (Length(FLayerElevations[0]) <> RowCount)
      or (Length(FLayerElevations[0,0]) <> LayerCount+1)
      then
    begin
      SetLength(FLayerElevations,ColumnCount,RowCount,LayerCount+1);
    end;
  end;
  Assert((Column >= 0) and (Row >= 0) and (Layer >= 0));
  Assert((Column < ColumnCount) and (Row < RowCount) and (Layer < LayerCount+1));
  result := FLayerElevations[Column, Row, Layer];
end;

function TModflowGrid.GetCellThickness(const Column, Row, Layer: integer): real;
begin
  result := FLayerElevations[Column, Row, Layer]
    - FLayerElevations[Column, Row, Layer+1];
end;

function TModflowGrid.GetContainingLayer(ACol, ARow: integer;
  const AZPosition: real): integer;
begin
  result := GetContainingColumnOrRow(GetTwoDCellElevations(ACol, ARow), AZPosition);
end;

function TModflowGrid.GetLayerElevations: TThreeDRealArray;
begin
  UpdateCellElevations;
  result := FLayerElevations;
end;

function TModflowGrid.GetTwoDCellElevations(const Col,
  Row: integer): TOneDRealArray;
begin
  result := LayerElevations[Col, Row];
end;

function TModflowGrid.HighestElevation: real;
var
  ColIndex, RowIndex: integer;
begin
  UpdateCellElevations;
  if (ColumnCount > 0) and (RowCount > 0) and (LayerCount > 0) then
  begin
    result := CellElevation[0,0,0];
    for ColIndex := 0 to ColumnCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        if result < CellElevation[ColIndex,RowIndex,0] then
        begin
          result := CellElevation[ColIndex,RowIndex,0]
        end;
      end;
    end;
  end
  else
  begin
    result := 1;
  end;
end;

procedure TModflowGrid.RecordColoredGrid;
var
  CellColors: TCellColors;
  LayerLength, RowLength, ColLength: integer;
  LayerIndex, RowIndex, ColIndex: integer;
  Red, Green, Blue: TGLubyte;
  X, Y, Z: single;
//  XPositions, YPositions: TOneDRealArray;
//  ZPositions: TThreeDRealArray;
//  Index: integer;
  APoint: T3DRealPoint;
  procedure DrawLeftSide;
  begin
    glBegin(GL_TRIANGLE_FAN);
    // Top Center point
    APoint := CellPoints[ColIndex*2,RowIndex*2+1,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Top Front point
    APoint := CellPoints[ColIndex*2,RowIndex*2+2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Front point
    APoint := CellPoints[ColIndex*2,RowIndex*2+2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Center point
    APoint := CellPoints[ColIndex*2,RowIndex*2+1,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Back point
    APoint := CellPoints[ColIndex*2,RowIndex*2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Top Back point
    APoint := CellPoints[ColIndex*2,RowIndex*2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);
    glEnd;
  end;
  procedure DrawRightSide;
  begin
    glBegin(GL_TRIANGLE_FAN);
    // Top Center point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+1,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Top Front point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Front point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Center point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+1,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Back point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Top Back point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);
    glEnd;
  end;
  procedure DrawBackSide;
  begin
    glBegin(GL_TRIANGLE_FAN);
    // Top Center point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Top Right point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Right point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Center point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Left point
    APoint := CellPoints[ColIndex*2,  RowIndex*2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Top Left point
    APoint := CellPoints[ColIndex*2,  RowIndex*2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);
    glEnd;
  end;
  procedure DrawFrontSide;
  begin
    glBegin(GL_TRIANGLE_FAN);
    // Top Center point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2+2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Top Right point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Right point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Center point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2+2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Bottom Left point
    APoint := CellPoints[ColIndex*2  ,RowIndex*2+2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Top Left point
    APoint := CellPoints[ColIndex*2  ,RowIndex*2+2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);
    glEnd;
  end;
  procedure DrawTopSide;
  begin
    glBegin(GL_TRIANGLE_FAN);
    // Center point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2+1,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Right center point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+1,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Right back point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Center back point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Left back point
    APoint := CellPoints[ColIndex*2,RowIndex*2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Left point
    APoint := CellPoints[ColIndex*2,RowIndex*2+1,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Left front point
    APoint := CellPoints[ColIndex*2,RowIndex*2+2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // center front point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2+2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // right front point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+2,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Right center point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+1,LayerIndex];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);
    glEnd;
  end;
  procedure DrawBottomSide;
  begin
    glBegin(GL_TRIANGLE_FAN);
    // Center point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2+1,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Right center point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+1,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Right back point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Center back point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Left back point
    APoint := CellPoints[ColIndex*2,RowIndex*2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Left point
    APoint := CellPoints[ColIndex*2,RowIndex*2+1,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Left front point
    APoint := CellPoints[ColIndex*2,RowIndex*2+2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // center front point
    APoint := CellPoints[ColIndex*2+1,RowIndex*2+2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // right front point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+2,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);

    // Right center point
    APoint := CellPoints[ColIndex*2+2,RowIndex*2+1,LayerIndex+1];
    X := APoint.X;
    Y := APoint.Y;
    Z := APoint.Z;
    glVertex3f(X, Y, Z);
    glEnd;
  end;
var
  SelectedLayer, SelectedRow, SelectedColumn: integer;
begin
  Screen.Cursor := crHourGlass;
  try
    Assert(ThreeDDataSet <> nil);

    UpdateCellPoints;
    if CellPoints = nil then
    begin
      Exit;
    end;

    if frmGoPhast.PhastModel.ThreeDTimeList <> nil then
    begin
      SelectedLayer := frmGoPhast.Grid.SelectedLayer;
      SelectedRow := frmGoPhast.Grid.SelectedRow;
      SelectedColumn := frmGoPhast.Grid.SelectedColumn;
      try
        frmGoPhast.PhastModel.UpdateThreeDTimeDataSet(frmGoPhast.PhastModel.ThreeDTimeList,
          frmGoPhast.PhastModel.ThreeDDisplayTime);
        if frmGoPhast.PhastModel.TopTimeList <> nil then
        begin
          frmGoPhast.PhastModel.UpdateTopTimeDataSet(frmGoPhast.PhastModel.TopTimeList,
            frmGoPhast.PhastModel.TopDisplayTime);
        end;
        if frmGoPhast.PhastModel.FrontTimeList <> nil then
        begin
          frmGoPhast.PhastModel.UpdateFrontTimeDataSet(frmGoPhast.PhastModel.FrontTimeList,
            frmGoPhast.PhastModel.FrontDisplayTime);
        end;
        if frmGoPhast.PhastModel.SideTimeList <> nil then
        begin
          frmGoPhast.PhastModel.UpdateSideTimeDataSet(frmGoPhast.PhastModel.SideTimeList,
            frmGoPhast.PhastModel.SideDisplayTime);
        end;
      finally
        frmGoPhast.Grid.SelectedLayer := SelectedLayer;
        frmGoPhast.Grid.SelectedRow := SelectedRow;
        frmGoPhast.Grid.SelectedColumn := SelectedColumn;
      end;
    end
    else
    begin
      ThreeDDataSet.Initialize;
    end;

    if ThreeDDataSet = nil then
    begin
      Exit;
    end;

    glNewList(CellsGLIndex, GL_COMPILE);
    Assert(ThreeDDataSet.EvaluatedAt = eaBlocks);
    LayerLength := ThreeDDataSet.LayerCount;
    RowLength := ThreeDDataSet.RowCount;
    ColLength := ThreeDDataSet.ColumnCount;

    SetLength(CellColors, LayerLength, RowLength, ColLength);

    Update3DCellColors(CellColors);

    for LayerIndex := 0 to LayerLength - 1 do
    begin
      for RowIndex := 0 to RowLength - 1 do
      begin
        for ColIndex := 0 to ColLength - 1 do
        begin
          if CellColors[LayerIndex, RowIndex, ColIndex] = clWhite then
            Continue;

          ExtractColorComponents(CellColors[LayerIndex, RowIndex, ColIndex],
            Red, Green, Blue);

          glColor3ub(Red, Green, Blue);

          if (ColIndex = ColLength - 1) or
            (CellColors[LayerIndex, RowIndex, ColIndex + 1] = clWhite) then
          begin
            DrawRightSide;
          end;
          if (ColIndex = 0) or
            (CellColors[LayerIndex, RowIndex, ColIndex - 1] = clWhite) then
          begin
            DrawLeftSide;
          end;
          if (RowIndex = RowLength - 1) or
            (CellColors[LayerIndex, RowIndex + 1, ColIndex] = clWhite) then
          begin
            DrawFrontSide;
          end;
          if (RowIndex = 0) or
            (CellColors[LayerIndex, RowIndex - 1, ColIndex] = clWhite) then
          begin
            DrawBackSide;
          end;
          if (LayerIndex = LayerLength - 1) or
            (CellColors[LayerIndex + 1, RowIndex, ColIndex] = clWhite) then
          begin
            DrawBottomSide;
          end;
          if (LayerIndex = 0) or
            (CellColors[LayerIndex - 1, RowIndex, ColIndex] = clWhite) then
          begin
            DrawTopSide;
          end;
        end;
      end;
    end;
    glEndList;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TModflowGrid.RecordFront;
var
  X, Y, Z: single;
  ColumnIndex, RowIndex, LayerIndex: integer;
  APoint: T3DRealPoint;
  LayerBoundaries: TOneDIntegerArray;
  DividedUnits: Boolean;
  LineWidth: single;
  UnitIndex: Integer;
  GridLine: TGLCoordArray;
  PointIndex: integer;
begin
  glEnableClientState(GL_VERTEX_ARRAY);
  glNewList(FrontGridGLIndex, GL_COMPILE);
  try
    // Draw grid lines on selected Row.
    UpdateCellPoints;
    if CellPoints = nil then
    begin
      Exit;
    end;

    IdentifyDividedUnitBoundaries(LayerBoundaries, DividedUnits);

    RowIndex := DisplayRow;
    if (RowIndex >= 0) and (RowIndex <= RowCount) then
    begin
      for ColumnIndex := 0 to ColumnCount do
      begin
        if (ColumnIndex = ColumnCount) or (ColumnIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        glBegin(GL_LINES);
        LayerIndex := 0;
        APoint := CellPoints[ColumnIndex*2,RowIndex*2,LayerIndex];
        X := APoint.X;
        Y := APoint.Y;
        Z := APoint.Z;
        glVertex3f(X, Y, Z);
        LayerIndex := LayerCount;
        APoint := CellPoints[ColumnIndex*2,RowIndex*2,LayerIndex];
        X := APoint.X;
        Y := APoint.Y;
        Z := APoint.Z;
        glVertex3f(X, Y, Z);
        glEnd;
      end;

      SetLength(GridLine, ColumnCount*2+1);
      glVertexPointer(3, GL_FLOAT, 0, GridLine);
      UnitIndex := 0;
      for LayerIndex := 0 to LayerCount do
      begin
        SetLayerLineWidth(LineWidth, LayerIndex, UnitIndex,
          DividedUnits, LayerBoundaries);
        if LineWidth <> ThinLine then
        begin
          LineWidth := ThickLine;
        end;
        glLineWidth(LineWidth);

        PointIndex := 0;
//        glBegin(GL_LINE_STRIP);
        for ColumnIndex := 0 to ColumnCount do
        begin
          APoint := CellPoints[ColumnIndex*2,RowIndex*2,LayerIndex];
//          X := APoint.X;
//          Y := APoint.Y;
//          Z := APoint.Z;
//          glVertex3f(X, Y, Z);

          GridLine[PointIndex].X := APoint.X;
          GridLine[PointIndex].Y := APoint.Y;
          GridLine[PointIndex].Z := APoint.Z;
          Inc(PointIndex);

          if ColumnIndex < ColumnCount then
          begin
            APoint := CellPoints[ColumnIndex*2+1,RowIndex*2,LayerIndex];
//            X := APoint.X;
//            Y := APoint.Y;
//            Z := APoint.Z;
//            glVertex3f(X, Y, Z);

            GridLine[PointIndex].X := APoint.X;
            GridLine[PointIndex].Y := APoint.Y;
            GridLine[PointIndex].Z := APoint.Z;
            Inc(PointIndex);
          end;
        end;
//        glEnd;
        glDrawArrays(GL_LINE_STRIP, 0, PointIndex);
      end;
    end;
  finally
    glEndList;
  end;
end;

procedure TModflowGrid.RecordShell;
var
  X, Y, Z: single;
  ColumnIndex, RowIndex, LayerIndex: integer;
  APoint: T3DRealPoint;
begin
  glNewList(GridShellGLIndex, GL_COMPILE);
  try
    UpdateCellPoints;
    if CellPoints = nil then
    begin
      Exit;
    end;
    glLineWidth(ThinLine);
    glColor3f(0.0, 0.0, 0.0);
    // Draw left side;
    glBegin(GL_LINE_LOOP);
    ColumnIndex := 0;
    LayerIndex := 0;
    for RowIndex := 0 to Length(CellPoints[0]) -1 do
    begin
      APoint := CellPoints[ColumnIndex,RowIndex,LayerIndex];
      X := APoint.X;
      Y := APoint.Y;
      Z := APoint.Z;
      glVertex3f(X, Y, Z);
    end;
    LayerIndex := Length(CellPoints[0,0])-1;
    for RowIndex := Length(CellPoints[0]) -1 downto 0 do
    begin
      APoint := CellPoints[ColumnIndex,RowIndex,LayerIndex];
      X := APoint.X;
      Y := APoint.Y;
      Z := APoint.Z;
      glVertex3f(X, Y, Z);
    end;
    glEnd;

    // Draw right side;
    glBegin(GL_LINE_LOOP);
    ColumnIndex := Length(CellPoints)-1;
    LayerIndex := 0;
    for RowIndex := 0 to Length(CellPoints[0]) -1 do
    begin
      APoint := CellPoints[ColumnIndex,RowIndex,LayerIndex];
      X := APoint.X;
      Y := APoint.Y;
      Z := APoint.Z;
      glVertex3f(X, Y, Z);
    end;
    LayerIndex := Length(CellPoints[0,0])-1;
    for RowIndex := Length(CellPoints[0]) -1 downto 0 do
    begin
      APoint := CellPoints[ColumnIndex,RowIndex,LayerIndex];
      X := APoint.X;
      Y := APoint.Y;
      Z := APoint.Z;
      glVertex3f(X, Y, Z);
    end;
    glEnd;

    // Draw front side;
    glBegin(GL_LINE_LOOP);
    LayerIndex := 0;
    RowIndex := 0;
    for ColumnIndex := 0 to Length(CellPoints) -1 do
    begin
      APoint := CellPoints[ColumnIndex,RowIndex,LayerIndex];
      X := APoint.X;
      Y := APoint.Y;
      Z := APoint.Z;
      glVertex3f(X, Y, Z);
    end;
    LayerIndex := Length(CellPoints[0,0])-1;
    for ColumnIndex := Length(CellPoints) -1 downto 0 do
    begin
      APoint := CellPoints[ColumnIndex,RowIndex,LayerIndex];
      X := APoint.X;
      Y := APoint.Y;
      Z := APoint.Z;
      glVertex3f(X, Y, Z);
    end;
    glEnd;

    // Draw back side;
    glBegin(GL_LINE_LOOP);
    LayerIndex := 0;
    RowIndex := Length(CellPoints[0])-1;;
    for ColumnIndex := 0 to Length(CellPoints) -1 do
    begin
      APoint := CellPoints[ColumnIndex,RowIndex,LayerIndex];
      X := APoint.X;
      Y := APoint.Y;
      Z := APoint.Z;
      glVertex3f(X, Y, Z);
    end;
    LayerIndex := Length(CellPoints[0,0])-1;
    for ColumnIndex := Length(CellPoints) -1 downto 0 do
    begin
      APoint := CellPoints[ColumnIndex,RowIndex,LayerIndex];
      X := APoint.X;
      Y := APoint.Y;
      Z := APoint.Z;
      glVertex3f(X, Y, Z);
    end;
    glEnd;
  finally
    glEndList;
  end;
end;

procedure TModflowGrid.RecordSide;
var
  X, Y, Z: single;
  ColumnIndex, RowIndex, LayerIndex: integer;
  APoint: T3DRealPoint;
  LayerBoundaries: TOneDIntegerArray;
  DividedUnits: Boolean;
  LineWidth: single;
  UnitIndex: Integer;
  GridLine: TGLCoordArray;
  PointIndex: integer;
begin
  glEnableClientState(GL_VERTEX_ARRAY);
  glNewList(SideGridGLIndex, GL_COMPILE);
  try
    // Draw grid lines on selected column.
    UpdateCellPoints;
    if CellPoints = nil then
    begin
      Exit;
    end;
    IdentifyDividedUnitBoundaries(LayerBoundaries, DividedUnits);

    ColumnIndex := DisplayColumn;
    if (ColumnIndex >= 0) and (ColumnIndex <= ColumnCount) then
    begin
//    Vertical lines
      for RowIndex := 0 to RowCount do
      begin
        if (RowIndex = RowCount) or (RowIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        glBegin(GL_LINES);

        LayerIndex := 0;
        APoint := CellPoints[ColumnIndex*2,RowIndex*2,LayerIndex];
        X := APoint.X;
        Y := APoint.Y;
        Z := APoint.Z;
        glVertex3f(X, Y, Z);
        LayerIndex := LayerCount;
        APoint := CellPoints[ColumnIndex*2,RowIndex*2,LayerIndex];
        X := APoint.X;
        Y := APoint.Y;
        Z := APoint.Z;
        glVertex3f(X, Y, Z);
        glEnd;
      end;
      // "Horizontal" lines
      SetLength(GridLine, RowCount*2+1);
      glVertexPointer(3, GL_FLOAT, 0, GridLine);
      UnitIndex := 0;
      for LayerIndex := 0 to LayerCount do
      begin
        SetLayerLineWidth(LineWidth, LayerIndex, UnitIndex,
          DividedUnits, LayerBoundaries);
        if LineWidth <> ThinLine then
        begin
          LineWidth := ThickLine;
        end;
        glLineWidth(LineWidth);
//        glBegin(GL_LINE_STRIP);
        PointIndex := 0;
        for RowIndex := 0 to RowCount do
        begin
          APoint := CellPoints[ColumnIndex*2,RowIndex*2,LayerIndex];
//          X := APoint.X;
//          Y := APoint.Y;
//          Z := APoint.Z;
//          glVertex3f(X, Y, Z);

          GridLine[PointIndex].X := APoint.X;
          GridLine[PointIndex].Y := APoint.Y;
          GridLine[PointIndex].Z := APoint.Z;
          Inc(PointIndex);

          if RowIndex < RowCount then
          begin
            APoint := CellPoints[ColumnIndex*2,RowIndex*2+1,LayerIndex];
//            X := APoint.X;
//            Y := APoint.Y;
//            Z := APoint.Z;
//            glVertex3f(X, Y, Z);

            GridLine[PointIndex].X := APoint.X;
            GridLine[PointIndex].Y := APoint.Y;
            GridLine[PointIndex].Z := APoint.Z;
            Inc(PointIndex);
          end;
        end;
//        glEnd;
        glDrawArrays(GL_LINE_STRIP, 0, PointIndex);
      end;
    end;
  finally
    glEndList;
  end;
end;

procedure TModflowGrid.RecordTop;
var
//  X, Y, Z: single;
  ColumnIndex, RowIndex, LayerIndex: integer;
  APoint: T3DRealPoint;
  GridLine: TGLCoordArray;
  PointIndex: integer;
begin
  glEnableClientState(GL_VERTEX_ARRAY);
  glNewList(TopGridGLIndex, GL_COMPILE);
  try
  // Draw grid lines on selected layer.
    UpdateCellPoints;
    if CellPoints = nil then
    begin
      Exit;
    end;

    LayerIndex := DisplayLayer;
    if (LayerIndex >= 0) and (LayerIndex <= LayerCount) then
    begin
      SetLength(GridLine, RowCount*2+1);
      glVertexPointer(3, GL_FLOAT, 0, GridLine);
      for ColumnIndex := 0 to ColumnCount do
      begin
        if (ColumnIndex = ColumnCount) or (ColumnIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        PointIndex := 0;
//        glBegin(GL_LINE_STRIP);
        for RowIndex := 0 to RowCount do
        begin
          APoint := CellPoints[ColumnIndex*2,RowIndex*2,LayerIndex];
//          X := APoint.X;
//          Y := APoint.Y;
//          Z := APoint.Z;
//          glVertex3f(X, Y, Z);

          GridLine[PointIndex].X := APoint.X;
          GridLine[PointIndex].Y := APoint.Y;
          GridLine[PointIndex].Z := APoint.Z;
          Inc(PointIndex);

          if RowIndex < RowCount then
          begin
            APoint := CellPoints[ColumnIndex*2,RowIndex*2+1,LayerIndex];
//            X := APoint.X;
//            Y := APoint.Y;
//            Z := APoint.Z;
//            glVertex3f(X, Y, Z);

            GridLine[PointIndex].X := APoint.X;
            GridLine[PointIndex].Y := APoint.Y;
            GridLine[PointIndex].Z := APoint.Z;
            Inc(PointIndex);
          end;
        end;
//        glEnd;
        glDrawArrays(GL_LINE_STRIP, 0, PointIndex);
      end;

      SetLength(GridLine, ColumnCount*2+1);
      glVertexPointer(3, GL_FLOAT, 0, GridLine);
      for RowIndex := 0 to RowCount do
      begin
        if (RowIndex = RowCount) or (RowIndex mod 10 = 0) then
        begin
          glLineWidth(ThickLine);
        end
        else
        begin
          glLineWidth(ThinLine);
        end;
        PointIndex := 0;
//        glBegin(GL_LINE_STRIP);
        for ColumnIndex := 0 to ColumnCount do
        begin
          APoint := CellPoints[ColumnIndex*2,RowIndex*2,LayerIndex];
//          X := APoint.X;
//          Y := APoint.Y;
//          Z := APoint.Z;
//          glVertex3f(X, Y, Z);

          GridLine[PointIndex].X := APoint.X;
          GridLine[PointIndex].Y := APoint.Y;
          GridLine[PointIndex].Z := APoint.Z;
          Inc(PointIndex);

          if ColumnIndex < ColumnCount then
          begin
            APoint := CellPoints[ColumnIndex*2+1,RowIndex*2,LayerIndex];
//            X := APoint.X;
//            Y := APoint.Y;
//            Z := APoint.Z;
//            glVertex3f(X, Y, Z);

            GridLine[PointIndex].X := APoint.X;
            GridLine[PointIndex].Y := APoint.Y;
            GridLine[PointIndex].Z := APoint.Z;
            Inc(PointIndex);
          end;
        end;
//        glEnd;
        glDrawArrays(GL_LINE_STRIP, 0, PointIndex);
      end;
    end;

  finally
    glEndList;
  end;
end;

procedure TModflowGrid.SetCellElevation(const Column, Row, Layer: integer;
  const Value: real);
begin
  FLayerElevations[Column, Row, Layer] := Value;
  LayersChanged;
end;

procedure TModflowGrid.SetCellThickness(const Column, Row, Layer: integer;
  const Value: real);
var
  Delta: double;
begin
  Delta := Value - CellThickness[Column, Row, Layer];
  if Delta <> 0 then
  begin
    LayerElevations[Column, Row, Layer+1] := LayerElevations[Column, Row, Layer+1] + Delta;
    LayersChanged;
  end;
end;

procedure TModflowGrid.SetLayerElevations(const Value: TThreeDRealArray);
var
  FirstLength: integer;
  SecondLength: integer;
  ThirdLength: integer;
begin
  if Value = nil then
  begin
    FirstLength := 0;
  end
  else
  begin
    FirstLength := Length(Value);
  end;
  if FirstLength <= 0 then
  begin
    SecondLength := 0
  end
  else
  begin
    SecondLength := Length(Value[0]);
  end;
  if SecondLength <= 0 then
  begin
    ThirdLength := 0
  end
  else
  begin
    ThirdLength := Length(Value[0,0]);
  end;
  LayerCount := ThirdLength-1;
  FLayerElevations := Value;
  SetLength(FLayerElevations, FirstLength, SecondLength, ThirdLength);
  LayersChanged;
end;

function TModflowGrid.ThreeDElementCorner(const Column, Row,
  Layer: integer): T3DRealPoint;
begin
  { TODO : Implement this? }
  Assert(False);
end;

procedure TModflowGrid.SetLayerLineWidth(var LineWidth: single;
  LayerIndex: Integer; var UnitIndex: Integer; DividedUnits: Boolean;
  LayerBoundaries: TOneDIntegerArray);
begin
  if DividedUnits then
  begin
    if LayerBoundaries[UnitIndex] = LayerIndex then
    begin
      LineWidth := ThickGridLineThickness;
      Inc(UnitIndex);
    end
    else
    begin
      LineWidth := OrdinaryGridLineThickness;
    end;
  end
  else
  begin
    if (LayerIndex mod 10 = 0) or (LayerIndex = LayerCount) then
    begin
      LineWidth := ThickGridLineThickness;
    end
    else
    begin
      LineWidth := OrdinaryGridLineThickness;
    end;
  end;
end;

procedure TModflowGrid.IdentifyDividedUnitBoundaries(
  out LayerBoundaries: TOneDIntegerArray; out DividedUnits: Boolean);
var
  LayerGroup: TLayerGroup;
  UnitIndex2: Integer;
  UnitCount: Integer;
begin
  with frmGoPhast.PhastModel do
  begin
    DividedUnits := LayerStructure.LayerCount + 1 <> LayerStructure.Count;
  end;
  if DividedUnits then
  begin
    SetLength(LayerBoundaries, frmGoPhast.PhastModel.LayerStructure.Count);
    UnitCount := -1;
    for UnitIndex2 := 0 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
    begin
      LayerGroup := frmGoPhast.PhastModel.LayerStructure.Items[UnitIndex2] as TLayerGroup;
      UnitCount := UnitCount + LayerGroup.LayerCount;
      LayerBoundaries[UnitIndex2] := UnitCount;
    end;
  end
  else
  begin
    SetLength(LayerBoundaries, 0);
  end;
end;

function TModflowGrid.LowestElevation: real;
var
  ColIndex, RowIndex: integer;
begin
  UpdateCellElevations;
  if (ColumnCount > 0) and (RowCount > 0) and (LayerCount > 0) then
  begin
    result := CellElevation[0,0,LayerCount];
    for ColIndex := 0 to ColumnCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        if result > CellElevation[ColIndex,RowIndex,LayerCount] then
        begin
          result := CellElevation[ColIndex,RowIndex,LayerCount]
        end;
      end;
    end;
  end
  else
  begin
    result := 1;
  end;
end;

function TModflowGrid.NearestLayerPosition(ACol, ARow: integer;
  const AZPosition: real; const First: integer = -1; const Last: integer = -1): integer;
begin
  result := NearestColumnOrRow(GetTwoDCellElevations(ACol, ARow), AZPosition, First, Last);
end;

end.