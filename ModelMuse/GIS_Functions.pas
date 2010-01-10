{@abstract(@name defines GIS functions that can be used with a TRbwParser
  from within GoPhast.)  Some of the functions require access to
  global variables.  Most of these are set through "Update" procedures
  but one (@link(GlobalEvaluatedAt)) is declared in the interface section.

  For a list of the functions, see the User's Guide.}
unit GIS_Functions;

interface

uses SysUtils, Classes, RbwParser, ScreenObjectUnit, GoPhastTypes;

type
  TActiveOnLayer = class(TExpression)
  protected
    function GetVariablesUsed: TStringList; override;
  public
    {
      @Name returns True if Variable is used by the @Link(TExpression)
      or if the variable is named "Active".
    }
    function UsesVariable(const Variable: TCustomVariable): boolean; override;
  end;

  TSpecifiedHeadOnLayer = class(TExpression)
  protected
    function GetVariablesUsed: TStringList; override;
  public
    {
      @Name returns True if Variable is used by the @Link(TExpression)
      or if the variable is named "Specified_Head".
    }
    function UsesVariable(const Variable: TCustomVariable): boolean; override;

  end;

  // @name adds a series of (mostly) GIS function to Parser.
// The functions are defined in the initialization section.
// In addition, one descendant of TSelectExpression is defined in the
// implementation section and added to RbwParser.SpecialImplentorList.
// @param(Parser is the TRbwParser to which the GIS functions will be added.)
procedure AddGIS_Functions(const Parser: TRbwParser;
  ModelSelection: TModelSelection);

// @name updates a series of global values related to location.
procedure UpdateGlobalLocations(const Col, Row, Layer: integer;
  const EvaluatedAt: TEvaluatedAt);

// @name stores AScreenObject in a global variable and resets the global
// variable for TCellElementSegment to nil.
procedure UpdateCurrentScreenObject(const AScreenObject: TScreenObject);

// @name stores ASegment in a global variable and also calls
// @link(UpdateCurrentSection).
procedure UpdateCurrentSegment(const ASegment: TCellElementSegment);

// @name stores SectionIndex in a global variable.
procedure UpdateCurrentSection(const SectionIndex: integer);

{ TODO : Move this to the implentation section. }
var
  // @name is the @link(TEvaluatedAt) of the current context.  This
  // should probably be defined in the implementation section.
  GlobalEvaluatedAt: TEvaluatedAt;

const
  // @name is the name of a function added in @link(AddGIS_Functions).
  rsListRealValue = 'ListRealValue';
  // @name is the name of a function added in @link(AddGIS_Functions).
  rsListIntegerValue = 'ListIntegerValue';
  rsObjectImportedValuesR = 'ObjectImportedValuesR';
  rsObjectImportedValuesI = 'ObjectImportedValuesI';
  rsObjectImportedValuesB = 'ObjectImportedValuesB';
  rsObjectImportedValuesT = 'ObjectImportedValuesT';
  StrImportedHigherElev = 'Imported Higher Elevations';
  StrImportedLowerEleva = 'Imported Lower Elevations';
  StrImportedElevations = 'Imported Elevations';

function GetColumnWidth(Column: Integer): Double;
function GetRowWidth(Row: Integer): Double;
function GetLayerHeight(Col, Row, Lay: Integer): Double;

implementation

uses frmGoPhastUnit, DataSetUnit, FastGEO, LayerStructureUnit, PhastModelUnit,
  ValueArrayStorageUnit, HufDefinition, OrderedCollectionUnit,
  ModflowPackageSelectionUnit, Math;

var  
  SpecialImplementors: TList;

type
  TNodeInterpolateExpression = class(TSelectExpression)
  public
    procedure Evaluate; override;
  end;


var
  NodeDistancesSet: boolean;
  FNodeDistances: array of double;

  GlobalX, GlobalY, GlobalZ: real;
  GlobalXPrime, GlobalYPrime: real;
  GlobalColumn, GlobalRow, GlobalLayer: integer;
  GlobalCurrentScreenObject: TScreenObject;
  GlobalCurrentSegment: TCellElementSegment;
  GlobalSection: integer;

  XFunction: TFunctionRecord;
  YFunction: TFunctionRecord;
  ZFunction: TFunctionRecord;
  XPrimeFunction: TFunctionRecord;
  YPrimeFunction: TFunctionRecord;
  ColumnFunction: TFunctionRecord;
  ElevationToLayerFunction: TFunctionRecord;
  ElevationToModelLayerFunction: TFunctionRecord;
  RowFunction: TFunctionRecord;
  LayerFunction: TFunctionRecord;
  ColumnWidthFunction: TFunctionRecord;
  RowWidthFunction: TFunctionRecord;
  LayerHeightFunction: TFunctionRecord;
  BlockAreaTopFunction: TFunctionRecord;
  BlockAreaFrontFunction: TFunctionRecord;
  BlockAreaSideFunction: TFunctionRecord;
  BlockVolumeFunction: TFunctionRecord;
  ColumnPositionFunction: TFunctionRecord;
  RowPositionFunction: TFunctionRecord;
  LayerPositionFunction: TFunctionRecord;
  ColumnCountFunction: TFunctionRecord;
  RowCountFunction: TFunctionRecord;
  LayerCountFunction: TFunctionRecord;
  ObjectLengthFunction: TFunctionRecord;
  ObjectAreaFunction: TFunctionRecord;
  ObjectIntersectLengthFunction: TFunctionRecord;
  ObjectSectionIntersectLengthFunction: TFunctionRecord;
  ObjectIntersectAreaFunction: TFunctionRecord;
  ObjectNameFunction: TFunctionRecord;

  NodeXFunction: TFunctionRecord;
  NodeYFunction: TFunctionRecord;
  NodeZFunction: TFunctionRecord;
  NodeDistanceFunction: TFunctionRecord;
  CurrentNodeXFunction: TFunctionRecord;
  CurrentNodeYFunction: TFunctionRecord;
  CurrentNodeZFunction: TFunctionRecord;
  CurrentSegmentLengthFunction: TFunctionRecord;
  CurrentSectionIndexFunction: TFunctionRecord;
  FractionOfObjectLengthFunction: TFunctionRecord;
  NodeCountFunction: TFunctionRecord;

  ListRealValueFunction: TFunctionRecord;
  ListIntegerValueFunction: TFunctionRecord;
  ModflowLayerSimulatedFunction: TFunctionRecord;
  ModflowHufKx: TFunctionRecord;

  ImportedValuesRFunction: TFunctionRecord;
  ImportedValuesIFunction: TFunctionRecord;
  ImportedValuesBFunction: TFunctionRecord;
  ImportedValuesTFunction: TFunctionRecord;

  NodeInterpolate: TFunctionClass;
  NodeInterpolateSpecialImplementor: TSpecialImplementor;

  ActiveOnLayer: TFunctionClass;
  ActiveOnLayerSpecialImplementor: TSpecialImplementor;

  SpecifiedHeadOnLayer: TFunctionClass;
  SpecifiedHeadOnLayerSpecialImplementor: TSpecialImplementor;

procedure AddGIS_Functions(const Parser: TRbwParser;
  ModelSelection: TModelSelection);
  procedure AddItem (Item: TFunctionRecord; ShouldAdd: boolean);
  var
    Index: integer;
  begin
    Index := Parser.Functions.IndexOf(Item.Name);
    if (Index > 0) and not ShouldAdd then
    begin
      Parser.Functions.Delete(Index);
    end;
    if (Index < 0) and ShouldAdd then
    begin
      Parser.Functions.Add(Item);
    end;
  end;
var
  Index: Integer;
  Item: TSpecialImplementor;
begin
  Parser.SpecialImplementorList.Clear;
  for Index := 0 to SpecialImplementors.Count - 1 do
  begin
    Item := SpecialImplementors[Index];
    Parser.SpecialImplementorList.Add(Item);
  end;
  // Make sure the GIS functions are available in Parser.
  AddItem(XFunction, True);
  AddItem(YFunction, True);
  AddItem(ZFunction, True);
  AddItem(XPrimeFunction, True);
  AddItem(YPrimeFunction, True);
  AddItem(ColumnFunction, True);
  AddItem(ElevationToLayerFunction, True);
  AddItem(ElevationToModelLayerFunction, True);
  AddItem(RowFunction, True);
  AddItem(LayerFunction, True);
  AddItem(ColumnWidthFunction, True);
  AddItem(RowWidthFunction, True);
  AddItem(LayerHeightFunction, True);
  AddItem(BlockAreaTopFunction, True);
  AddItem(BlockAreaFrontFunction, True);
  AddItem(BlockAreaSideFunction, True);
  AddItem(BlockVolumeFunction, True);
  AddItem(ColumnCountFunction, True);
  AddItem(RowCountFunction, True);
  AddItem(LayerCountFunction, True);
  AddItem(ColumnPositionFunction, True);
  AddItem(RowPositionFunction, True);
  AddItem(LayerPositionFunction, True);
  AddItem(ObjectLengthFunction, True);
  AddItem(ObjectAreaFunction, True);
  AddItem(ObjectIntersectLengthFunction, True);
  AddItem(ObjectSectionIntersectLengthFunction, True);
  AddItem(ObjectIntersectAreaFunction, True);
  AddItem(ObjectNameFunction, True);
  AddItem(NodeXFunction, True);
  AddItem(NodeYFunction, True);
  AddItem(NodeZFunction, True);
  AddItem(NodeDistanceFunction, True);
  AddItem(CurrentNodeXFunction, True);
  AddItem(CurrentNodeYFunction, True);
  AddItem(CurrentNodeZFunction, True);
  AddItem(CurrentSegmentLengthFunction, True);
  AddItem(CurrentSectionIndexFunction, True);
  AddItem(FractionOfObjectLengthFunction, True);
  AddItem(NodeCountFunction, True);
  AddItem(ListRealValueFunction, True);
  AddItem(ListIntegerValueFunction, True);
  AddItem(ModflowLayerSimulatedFunction, True);
  AddItem(ModflowHufKx, True);
  AddItem(ImportedValuesRFunction, True);
  AddItem(ImportedValuesIFunction, True);
  AddItem(ImportedValuesBFunction, True);
  AddItem(ImportedValuesTFunction, True);
end;

function NodeDistances(const Index: integer): double;
var
  PointIndex: integer;
  CumulativeDistance: double;
  APoint: TPoint2D;
  PriorPoint: TPoint2D;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    if not NodeDistancesSet then
    begin
      SetLength(FNodeDistances, GlobalCurrentScreenObject.Count);
      if GlobalCurrentScreenObject.Count > 0 then
      begin
        CumulativeDistance := 0;
        FNodeDistances[0] := 0;
        PriorPoint := GlobalCurrentScreenObject.Points[0];
        for PointIndex := 1 to GlobalCurrentScreenObject.Count - 1 do
        begin
          APoint := GlobalCurrentScreenObject.Points[PointIndex];
          CumulativeDistance := CumulativeDistance
            + Sqrt(Sqr(APoint.X - PriorPoint.X)
            + Sqr(APoint.Y - PriorPoint.Y));
          FNodeDistances[PointIndex] := CumulativeDistance;
          PriorPoint := APoint;
        end;
      end;
      NodeDistancesSet := True;
    end;
    if (Index < 0) or (Index >= Length(FNodeDistances)) then
    begin
      result := 0;
    end
    else
    begin
      result := FNodeDistances[Index];
    end;
  end;
end;

procedure UpdateGlobalLocations(const Col, Row, Layer: integer;
  const EvaluatedAt: TEvaluatedAt);
var
  CC2D: TPoint2D;
  CC3D: T3DRealPoint;
begin
  GlobalColumn := Col + 1;
  GlobalRow := Row + 1;
  GlobalLayer := Layer + 1;

  case EvaluatedAt of
    eaBlocks:
      begin
        CC2D := frmGoPhast.Grid.TwoDElementCenter(Col, Row);
        GlobalX := CC2D.X;
        GlobalY := CC2D.Y;

        CC3D := frmGoPhast.Grid.ThreeDElementCenter(Col, Row, Layer);
        GlobalZ := CC3D.Z;
        GlobalXPrime := CC3D.X;
        GlobalYPrime := CC3D.Y;
      end;
    eaNodes:
      begin
        CC2D := frmGoPhast.Grid.TwoDElementCorner(Col, Row);
        GlobalX := CC2D.X;
        GlobalY := CC2D.Y;

        CC3D := frmGoPhast.Grid.ThreeDElementCorner(Col, Row, Layer);
        GlobalZ := CC3D.Z;
        GlobalXPrime := CC3D.X;
        GlobalYPrime := CC3D.Y;
      end;
  else
    Assert(False);
  end;

end;

procedure UpdateCurrentSection(const SectionIndex: integer);
begin
  GlobalSection := SectionIndex;
end;

procedure UpdateCurrentSegment(const ASegment: TCellElementSegment);
begin
  GlobalCurrentSegment := ASegment;
  if GlobalCurrentSegment = nil then
  begin
    UpdateCurrentSection(-1);
  end
  else
  begin
    UpdateCurrentSection(ASegment.SectionIndex);
  end;
end;

procedure UpdateCurrentScreenObject(const AScreenObject: TScreenObject);
begin
  UpdateCurrentSegment(nil);
  GlobalCurrentScreenObject := AScreenObject;
  NodeDistancesSet := False;
end;

function _XNodePosition(Values: array of pointer): double;
var
  Index: integer;
begin
  result := 0;
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    Index := PInteger(Values[0])^ - 1;
    if (Index < 0) or (Index >= GlobalCurrentScreenObject.Count) then
    begin
      result := 0;
    end
    else
    begin
      case GlobalCurrentScreenObject.ViewDirection of
        vdTop, vdFront:
          begin
            result := GlobalCurrentScreenObject.Points[Index].X;
          end;
        vdSide:
          begin
            result := 0;
          end;
      else
        Assert(False);
      end;
    end;
  end;
end;

function _YNodePosition(Values: array of pointer): double;
var
  Index: integer;
begin
  result := 0;
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    Index := PInteger(Values[0])^ - 1;
    if (Index < 0) or (Index >= GlobalCurrentScreenObject.Count) then
    begin
      result := 0;
    end
    else
    begin
      case GlobalCurrentScreenObject.ViewDirection of
        vdTop:
          begin
            result := GlobalCurrentScreenObject.Points[Index].Y;
          end;
        vdFront:
          begin
            result := 0;
          end;
        vdSide:
          begin
            result := GlobalCurrentScreenObject.Points[Index].Y
          end;
      else
        Assert(False);
      end;
    end;
  end;
end;

function _ZNodePosition(Values: array of pointer): double;
var
  Index: integer;
begin
  result := 0;
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    Index := PInteger(Values[0])^ - 1;
    if (Index < 0) or (Index >= GlobalCurrentScreenObject.Count) then
    begin
      result := 0;
    end
    else
    begin
      case GlobalCurrentScreenObject.ViewDirection of
        vdTop:
          begin
            result := 0;
          end;
        vdFront:
          begin
            result := GlobalCurrentScreenObject.Points[Index].Y;
          end;
        vdSide:
          begin
            result := GlobalCurrentScreenObject.Points[Index].X;
          end;
      else
        Assert(False);
      end;
    end;
  end;
end;

function _NodeDistances(Values: array of pointer): double;
var
  Index: integer;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    Index := PInteger(Values[0])^ - 1;
    result := NodeDistances(Index);
  end;
end;

function _CurrentXNodePosition(Values: array of pointer): double;
begin
  result := 0;
  if GlobalCurrentSegment = nil then
  begin
    Exit;
  end
  else
  begin
    case GlobalCurrentScreenObject.ViewDirection of
      vdTop, vdFront:
        begin
          result := GlobalCurrentSegment.X1;
        end;
      vdSide:
        begin
          result := 0;
        end;
    else
      Assert(False);
    end;
  end;
end;

function _CurrentYNodePosition(Values: array of pointer): double;
begin
  result := 0;
  if GlobalCurrentSegment = nil then
  begin
    Exit;
  end
  else
  begin
    case GlobalCurrentScreenObject.ViewDirection of
      vdTop:
        begin
          result := GlobalCurrentSegment.Y1;
        end;
      vdFront:
        begin
          Result:= 0;
        end;
      vdside:
        begin
          result := GlobalCurrentSegment.X1;
        end;
    else
      Assert(False);
    end;
  end;
end;

function _CurrentZNodePosition(Values: array of pointer): double;
begin
  result := 0;
  if GlobalCurrentSegment = nil then
  begin
    Exit;
  end
  else
  begin
    case GlobalCurrentScreenObject.ViewDirection of
      vdTop:
        begin
          Result:= 0;
        end;
      vdFront, vdSide:
        begin
          result := GlobalCurrentSegment.Y1;
        end;
    else
      Assert(False);
    end;
  end;
end;

function _CurrentSectionIndex(Values: array of pointer): integer;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GlobalSection+1;
  end;
end;

function _CurrentSegmentLength(Values: array of pointer): double;
var
  Point1, Point2: TPoint2D;
begin
  if (GlobalCurrentSegment = nil) or (GlobalCurrentScreenObject = nil)
    or (GlobalCurrentScreenObject.Count <= 1) then
  begin
    result := 0;
  end
  else
  begin
    Point1 := GlobalCurrentScreenObject.Points[GlobalCurrentSegment.VertexIndex];
    Point2 := GlobalCurrentScreenObject.Points[
      GlobalCurrentSegment.VertexIndex + 1];
    result := Sqrt(Sqr(Point1.X - Point2.X) + Sqr(Point1.Y - Point2.Y));
  end;
end;

function _ListDataSetRealValue(Values: array of pointer): double;
var
  DataSetName: string;
  Item: TRealDataListItem;
  Index: integer;
begin
  result := 0;
  if (GlobalCurrentScreenObject = nil)
    or (GlobalCurrentScreenObject.Count <= 1)
    or not (GlobalCurrentScreenObject is TMultiValueScreenObject) then
  begin
    Exit;
  end
  else
  begin
    DataSetName := PString(Values[0])^;
    with TMultiValueScreenObject(GlobalCurrentScreenObject) do
    begin
      Item := RealValues.GetItemByName(DataSetName) as TRealDataListItem;
      if Item <> nil then
      begin
        Index := Item.ValueIndex(GlobalColumn - 1, GlobalRow - 1, GlobalLayer -
          1);
        if Index >= 0 then
        begin
          result := Item.Values[Index];
        end;
      end;
    end;
  end;
end;

function _ListDataSetIntegerValue(Values: array of pointer): integer;
var
  DataSetName: string;
  Item: TIntegerDataListItem;
  Index: integer;
begin
  result := 0;
  if (GlobalCurrentScreenObject = nil)
    or (GlobalCurrentScreenObject.Count <= 1)
    or not (GlobalCurrentScreenObject is TMultiValueScreenObject) then
  begin
    Exit;
  end
  else
  begin
    DataSetName := PString(Values[0])^;
    with TMultiValueScreenObject(GlobalCurrentScreenObject) do
    begin
      Item := IntegerValues.GetItemByName(DataSetName) as TIntegerDataListItem;
      if Item <> nil then
      begin
        Index := Item.ValueIndex(GlobalColumn - 1, GlobalRow - 1, GlobalLayer -
          1);
        if Index >= 0 then
        begin
          result := Item.Values[Index];
        end;
      end;
    end;
  end;
end;

function _FractionOfObjectLength(Values: array of pointer): double;
var
  Point1, Point2: TPoint2D;
begin
  if (GlobalCurrentSegment = nil) or (GlobalCurrentScreenObject = nil)
    or (GlobalCurrentScreenObject.Count <= 1) then
  begin
    result := 0;
  end
  else
  begin
    Point1 := GlobalCurrentScreenObject.Points[GlobalCurrentSegment.VertexIndex];
    if (GlobalCurrentSegment.VertexIndex = 0)
      and (GlobalCurrentSegment.X1 = Point1.X)
      and (GlobalCurrentSegment.Y1 = Point1.Y) then
    begin
      result := 0;
      Exit;
    end
    else if (GlobalCurrentSegment.VertexIndex =
      GlobalCurrentScreenObject.Count - 2) then
    begin
      Point2 := GlobalCurrentScreenObject.Points[
        GlobalCurrentScreenObject.Count - 1];
      if (GlobalCurrentSegment.X2 = Point2.X)
        and (GlobalCurrentSegment.Y2 = Point2.Y) then
      begin
        result := 1;
        Exit;
      end;
    end;
    if GlobalCurrentSegment.EndPosition = epLast then
    begin
      Point2.X := GlobalCurrentSegment.X2;
      Point2.Y := GlobalCurrentSegment.Y2;
    end
    else
    begin
      Point2.X := (GlobalCurrentSegment.X1 + GlobalCurrentSegment.X2) / 2;
      Point2.Y := (GlobalCurrentSegment.Y1 + GlobalCurrentSegment.Y2) / 2;
    end;

    result := (Sqrt(Sqr(Point1.X - Point2.X) + Sqr(Point1.Y - Point2.Y))
      + NodeDistances(GlobalCurrentSegment.VertexIndex))
      / GlobalCurrentScreenObject.ScreenObjectLength;
  end;
end;

function _ObjectNodeCount(Values: array of pointer): double;
begin
  result := 0;
  if (GlobalCurrentScreenObject = nil) then
  begin
  end
  else
  begin
    result := GlobalCurrentScreenObject.Count
  end;
end;

function _X(Values: array of pointer): double;
begin
  Result := GlobalX;
end;

function _Y(Values: array of pointer): double;
begin
  Result := GlobalY;
end;

function _Z(Values: array of pointer): double;
begin
  Result := GlobalZ;
end;

function _XPrime(Values: array of pointer): double;
begin
  Result := GlobalXPrime;
end;

function _YPrime(Values: array of pointer): double;
begin
  Result := GlobalYPrime;
end;

function _Column(Values: array of pointer): integer;
begin
  Result := GlobalColumn;
end;

function _Row(Values: array of pointer): integer;
begin
  Result := GlobalRow;
end;

function _ElevationToModelLayer(Values: array of pointer): integer;
var
  Elevation: double;
  GroupIndex: Integer;
  NonSimulatedUnits: Integer;
  LayerCount: Integer;
  Group: TLayerGroup;
begin
  result := -1;
  Elevation := PDouble(Values[0])^;
  case frmGoPhast.PhastModel.ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast:
      begin
        case GlobalEvaluatedAt of
          eaBlocks:
            begin
              result := frmGoPhast.PhastModel.PhastGrid.
                GetContainingLayer(GlobalColumn-1, GlobalRow-1, Elevation)
                +1;
            end;
          eaNodes:
            begin
              if Elevation < frmGoPhast.PhastModel.
                PhastGrid.LowestElevation then
              begin
                result := 0;
              end
              else if Elevation > frmGoPhast.PhastModel.
                PhastGrid.HighestElevation then
              begin
                result := frmGoPhast.PhastModel.PhastGrid.LayerCount + 2
              end
              else
              begin
                result := frmGoPhast.PhastModel.
                  PhastGrid.NearestLayerCenter(Elevation);
                result := frmGoPhast.PhastModel.LayerStructure.
                  DataSetLayerToModflowLayer(result);
              end;
            end;
          else Assert(False);
        end;
      end;
    msModflow:
      begin
        Assert(GlobalEvaluatedAt = eaBlocks);
        result := frmGoPhast.PhastModel.ModflowGrid.
          GetContainingLayer(GlobalColumn-1, GlobalRow-1, Elevation);
        NonSimulatedUnits := 0;
        LayerCount:= 0;
        for GroupIndex := 1 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
        begin
          Group := frmGoPhast.PhastModel.LayerStructure[GroupIndex];
          if Group.Simulated then
          begin
            LayerCount := LayerCount + Group.LayerCount;
          end
          else
          begin
            Inc(NonSimulatedUnits);
            Inc(LayerCount);
          end;
          if LayerCount > result then
          begin
            break;
          end;
        end;
        result := result - NonSimulatedUnits + 1;
      end;
    else Assert(False);
  end;
end;

function _ElevationToLayer(Values: array of pointer): integer;
var
  Elevation: double;
begin
  result := -1;
  Elevation := PDouble(Values[0])^;
  case frmGoPhast.PhastModel.ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast:
      begin
        case GlobalEvaluatedAt of
          eaBlocks:
            begin
              result := frmGoPhast.PhastModel.PhastGrid.
                GetContainingLayer(GlobalColumn-1, GlobalRow-1, Elevation)
                +1;
            end;
          eaNodes:
            begin
              if Elevation < frmGoPhast.PhastModel.
                PhastGrid.LowestElevation then
              begin
                result := 0;
              end
              else if Elevation > frmGoPhast.PhastModel.
                PhastGrid.HighestElevation then
              begin
                result := frmGoPhast.PhastModel.PhastGrid.LayerCount + 2
              end
              else
              begin
                result := frmGoPhast.PhastModel.
                  PhastGrid.NearestLayerCenter(Elevation)+1;
              end;
            end;
          else Assert(False);
        end;
      end;
    msModflow:
      begin
        Assert(GlobalEvaluatedAt = eaBlocks);
        result := frmGoPhast.PhastModel.ModflowGrid.
          GetContainingLayer(GlobalColumn-1, GlobalRow-1, Elevation);
      end;
    else Assert(False);
  end;
  Inc(result);
end;

function _Layer(Values: array of pointer): integer;
begin
  Result := GlobalLayer;
end;

function GetColumnWidth(Column: Integer): Double;
begin
  case GlobalEvaluatedAt of
    eaBlocks:
      begin
        if (Column < 0) or (Column >= frmGoPhast.Grid.ColumnCount) then
        begin
          Result := 0;
        end
        else
        begin
          Result := frmGoPhast.Grid.ColumnWidth[Column];
        end;
      end;
    eaNodes:
      begin
        if (Column < 0) or (Column > frmGoPhast.PhastGrid.ColumnCount) then
        begin
          Result := 0;
        end
        else
        begin
          if (Column = 0) then
          begin
            result := frmGoPhast.PhastGrid.ColumnWidth[Column] / 2;
          end
          else if (Column = frmGoPhast.PhastGrid.ColumnCount) then
          begin
            result := frmGoPhast.PhastGrid.ColumnWidth[Column - 1] / 2;
          end
          else
          begin
            result := (frmGoPhast.PhastGrid.ColumnPosition[Column + 1] - frmGoPhast.PhastGrid.ColumnPosition[Column - 1]) / 2;
          end;
        end;
      end;
  else
    begin
      Assert(False);
      result := 0;
    end;
  end;
end;

function _ColumnWidth(Values: array of pointer): double;
var
  Column: integer;
begin
  if Values[0] <> nil then
  begin
    Column := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Column := GlobalColumn - 1;
  end;
  Result := GetColumnWidth(Column);
end;

function GetRowWidth(Row: Integer): Double;
begin
  case GlobalEvaluatedAt of
    eaBlocks:
      begin
        if (Row < 0) or (Row >= frmGoPhast.Grid.RowCount) then
        begin
          Result := 0;
        end
        else
        begin
          Result := frmGoPhast.Grid.RowWidth[Row];
        end;
      end;
    eaNodes:
      begin
        if (Row < 0) or (Row > frmGoPhast.PhastGrid.RowCount) then
        begin
          Result := 0;
        end
        else
        begin
          if (Row = 0) then
          begin
            result := frmGoPhast.PhastGrid.RowWidth[Row] / 2;
          end
          else if (Row = frmGoPhast.PhastGrid.RowCount) then
          begin
            result := frmGoPhast.PhastGrid.RowWidth[Row - 1] / 2;
          end
          else
          begin
            result := (frmGoPhast.PhastGrid.RowPosition[Row + 1] - frmGoPhast.PhastGrid.RowPosition[Row - 1]) / 2;
          end;
        end;
      end;
  else
    begin
      Assert(False);
      result := 0;
    end;
  end;
end;

function _RowWidth(Values: array of pointer): double;
var
  Row: integer;
begin
  if Values[0] <> nil then
  begin
    Row := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Row := GlobalRow - 1;
  end;
  Result := GetRowWidth(Row);
end;

function _SimulatedModflowLayer(Values: array of pointer): boolean;
var
  Layer: integer;
begin
  if Values[0] <> nil then
  begin
    Layer := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Layer := GlobalLayer - 1;
  end;
  result := frmGoPhast.PhastModel.LayerStructure.IsLayerSimulated(Layer);
end;

procedure ExtractColRowLayer(var Lay: Integer; var Row: Integer; var Col: Integer; Values: array of Pointer);
begin
  if Values[2] <> nil then
  begin
    Col := PInteger(Values[0])^ - 1;
    Row := PInteger(Values[1])^ - 1;
    Lay := PInteger(Values[2])^ - 1;
  end
  else if Values[1] <> nil then
  begin
    Col := PInteger(Values[0])^ - 1;
    Lay := PInteger(Values[1])^ - 1;
    Row := GlobalRow - 1;
  end
  else if Values[0] <> nil then
  begin
    Lay := PInteger(Values[0])^ - 1;
    Col := GlobalColumn - 1;
    Row := GlobalRow - 1;
  end
  else
  begin
    Lay := GlobalLayer - 1;
    Col := GlobalColumn - 1;
    Row := GlobalRow - 1;
  end;
end;

function GetLayerHeight(Col, Row, Lay: Integer): Double;
begin
  result := 0;
  case frmGoPhast.ModelSelection of
    msPhast:
      begin
        case GlobalEvaluatedAt of
          eaBlocks:
            begin
              if (Lay < 0) or (Lay >= frmGoPhast.PhastGrid.LayerCount) then
              begin
                Result := 0;
              end
              else
              begin
                Result := frmGoPhast.PhastGrid.LayerThickness[Lay];
              end;
            end;
          eaNodes:
            begin
              if (Lay < 0) or (Lay > frmGoPhast.PhastGrid.LayerCount) then
              begin
                Result := 0;
              end
              else
              begin
                if (Lay = 0) then
                begin
                  result := frmGoPhast.PhastGrid.LayerThickness[Lay] / 2;
                end
                else if (Lay = frmGoPhast.PhastGrid.LayerCount) then
                begin
                  result := frmGoPhast.PhastGrid.LayerThickness[Lay - 1] / 2;
                end
                else
                begin
                  result := (frmGoPhast.PhastGrid.LayerElevation[Lay + 1] - frmGoPhast.PhastGrid.LayerElevation[Lay - 1]) / 2;
                end;
              end;
            end;
        else
          begin
            Assert(False);
            result := 0;
          end;
        end;
      end;
    msModflow:
      begin
        if (Lay < 0) or (Lay > frmGoPhast.ModflowGrid.LayerCount - 1) or (Row < 0) or (Row > frmGoPhast.ModflowGrid.RowCount - 1) or (Col < 0) or (Col > frmGoPhast.ModflowGrid.ColumnCount - 1) then
        begin
          Result := 0;
        end
        else
        begin
          Result := frmGoPhast.ModflowGrid.CellThickness[Col, Row, Lay];
        end;
      end;
  else
    Assert(False);
  end;
end;

function _LayerHeight(Values: array of pointer): double;
var
  Col: integer;
  Row: integer;
  Lay: integer;
begin
  ExtractColRowLayer(Lay, Row, Col, Values);
  result := GetLayerHeight(Col, Row, Lay);
end;

function _BlockAreaTop(Values: array of pointer): double;
begin
  result := _ColumnWidth([Values[0]]) * _RowWidth([Values[1]]);
end;

function _BlockAreaFront(Values: array of pointer): double;
var
  Col, Row, Layer: integer;
  CellPoints: T2DRealPointArray;
  CellOutline: TPolygon2D;
begin
  if frmGoPhast.ModelSelection = msPhast then
  begin
    if Values[2] = nil then
    begin
      result := _ColumnWidth([Values[0]]) * _LayerHeight([Values[1]]);
    end
    else
    begin
      result := _ColumnWidth([Values[0]]) * _LayerHeight([Values[2]]);
    end;
  end
  else if frmGoPhast.ModelSelection = msModflow then
  begin
    if Values[2] = nil then
    begin
      Row := GlobalRow - 1;

      if Values[0] <> nil then
      begin
        Col := PInteger(Values[0])^ - 1;
      end
      else
      begin
        Col := GlobalColumn - 1;
      end;

      if Values[1] <> nil then
      begin
        Layer := PInteger(Values[1])^ - 1;
      end
      else
      begin
        Layer := GlobalLayer - 1;
      end;
    end
    else
    begin
      Col := PInteger(Values[0])^ - 1;
      Row := PInteger(Values[1])^ - 1;
      Layer := PInteger(Values[2])^ - 1;
    end;
    CellPoints := frmGoPhast.ModflowGrid.FrontCellPoints(Row);
    SetLength(CellOutline, 6);
    CellOutline[5] := CellPoints[Col*2,Layer];
    CellOutline[4] := CellPoints[Col*2+1,Layer];
    CellOutline[3] := CellPoints[Col*2+2,Layer];
    CellOutline[2] := CellPoints[Col*2+2,Layer+1];
    CellOutline[1] := CellPoints[Col*2+1,Layer+1];
    CellOutline[0] := CellPoints[Col*2,Layer+1];
    result := Area(CellOutline);
  end
  else
  begin
    result := 0;
    Assert(False);
  end;
end;

function _BlockAreaSide(Values: array of pointer): double;
var
  Col, Row, Layer: integer;
  CellPoints: T2DRealPointArray;
  CellOutline: TPolygon2D;
begin
  if frmGoPhast.ModelSelection = msPhast then
  begin
    if Values[2] = nil then
    begin
      result := _RowWidth([Values[0]]) * _LayerHeight([Values[1]]);
    end
    else
    begin
      result := _RowWidth([Values[0]]) * _LayerHeight([Values[2]]);
    end;
  end
  else if frmGoPhast.ModelSelection = msModflow then
  begin
    if Values[2] = nil then
    begin
      Col := GlobalColumn - 1;

      if Values[0] <> nil then
      begin
        Row := PInteger(Values[0])^ - 1;
      end
      else
      begin
        Row := GlobalRow - 1;
      end;
      
      if Values[1] <> nil then
      begin
        Layer := PInteger(Values[1])^ - 1;
      end
      else
      begin
        Layer := GlobalLayer - 1;
      end;
    end
    else
    begin
      Col := PInteger(Values[0])^ - 1;
      Row := PInteger(Values[1])^ - 1;
      Layer := PInteger(Values[2])^ - 1;
    end;
    CellPoints := frmGoPhast.ModflowGrid.SideCellPoints(Col);
    SetLength(CellOutline, 6);
    CellOutline[0] := CellPoints[Row*2,Layer];
    CellOutline[1] := CellPoints[Row*2+1,Layer];
    CellOutline[2] := CellPoints[Row*2+2,Layer];
    CellOutline[3] := CellPoints[Row*2+2,Layer+1];
    CellOutline[4] := CellPoints[Row*2+1,Layer+1];
    CellOutline[5] := CellPoints[Row*2,Layer+1];
    result := Area(CellOutline);
  end
  else
  begin
    result := 0;
    Assert(False);
  end;
end;

function _BlockVolume(Values: array of pointer): double;
var
  Col: Integer;
  Row: Integer;
  Lay: Integer;
begin
  ExtractColRowLayer(Lay, Row, Col, Values);
  result := GetColumnWidth(Col) * GetRowWidth(Row) *
    GetLayerHeight(Col, Row, Lay);
end;

function _ColumnPosition(Values: array of pointer): double;
var
  Column: integer;
begin
  if Values[0] <> nil then
  begin
    Column := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Column := GlobalColumn - 1;
  end;
  if (Column < 0) or (Column > frmGoPhast.Grid.ColumnCount) then
  begin
    Result := 0;
  end
  else
  begin
    Result := frmGoPhast.Grid.ColumnPosition[Column];
  end;
end;

function _RowPosition(Values: array of pointer): double;
var
  Row: integer;
begin
  if Values[0] <> nil then
  begin
    Row := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Row := GlobalRow - 1;
  end;
  if (Row < 0) or (Row > frmGoPhast.Grid.RowCount) then
  begin
    Result := 0;
  end
  else
  begin
    Result := frmGoPhast.Grid.RowPosition[Row];
  end;
end;

function GetLayerPosition(const Lay, Row, Col: Integer): Double;
begin
  result := 0;
  case frmGoPhast.ModelSelection of
    msPhast:
      begin
        if (Lay < 0) or (Lay > frmGoPhast.PhastGrid.LayerCount) then
        begin
          Result := 0;
        end
        else
        begin
          Result := frmGoPhast.PhastGrid.LayerElevation[Lay];
        end;
      end;
    msModflow:
      begin
        if (Lay < 0) or (Lay > frmGoPhast.ModflowGrid.LayerCount)
         or (Row < 0) or (Row > frmGoPhast.ModflowGrid.RowCount-1)
         or (Col < 0) or (Col > frmGoPhast.ModflowGrid.ColumnCount-1) then
        begin
          Result := 0;
        end
        else
        begin
          Result := frmGoPhast.ModflowGrid.CellElevation[Col, Row, Lay];
        end;
      end;
  else
    Assert(False);
  end;
end;

function _LayerPosition(Values: array of pointer): double;
var
  Col: integer;
  Row: integer;
  Lay: integer;
begin
  ExtractColRowLayer(Lay, Row, Col, Values);
  Result := GetLayerPosition(Lay, Row, Col);
end;

function _LayerCount(Values: array of pointer): integer;
begin
  Result := frmGoPhast.Grid.LayerCount + 1;
end;

function _RowCount(Values: array of pointer): integer;
begin
  Result := frmGoPhast.Grid.RowCount + 1;
end;

function _ColumnCount(Values: array of pointer): integer;
begin
  Result := frmGoPhast.Grid.ColumnCount + 1;
end;

function _ObjectLength(Values: array of pointer): double;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GlobalCurrentScreenObject.ScreenObjectLength;
  end;
end;

function _ObjectArea(Values: array of pointer): double;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GlobalCurrentScreenObject.ScreenObjectArea;
  end;
end;

function _ObjectIntersectLength(Values: array of pointer): double;
var
  Column, Row, Layer: integer;
  ArrayLength: integer;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    ArrayLength := Length(Values);
    if (ArrayLength >= 1) and (Values[0] <> nil) then
    begin
      Column := PInteger(Values[0])^ - 1;
    end
    else
    begin
      Column := GlobalColumn - 1;
    end;
    if (ArrayLength >= 2) and (Values[1] <> nil) then
    begin
      Row := PInteger(Values[1])^ - 1;
    end
    else
    begin
      Row := GlobalRow - 1;
    end;
    if (ArrayLength >= 3) and (Values[2] <> nil) then
    begin
      Layer := PInteger(Values[2])^ - 1;
    end
    else
    begin
      Layer := GlobalLayer - 1;
    end;
    result := GlobalCurrentScreenObject.ObjectIntersectLength(Column, Row,
      Layer);
  end;
end;

function _ObjectIntersectSectionLength(Values: array of pointer): double;
var
  Section, Column, Row, Layer: integer;
  ArrayLength: integer;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    ArrayLength := Length(Values);
    if (ArrayLength >= 1) and (Values[0] <> nil) then
    begin
      Section := PInteger(Values[0])^ - 1;
    end
    else
    begin
      Section := GlobalSection;
    end;
    if (ArrayLength >= 2) and (Values[1] <> nil) then
    begin
      Column := PInteger(Values[1])^ - 1;
    end
    else
    begin
      Column := GlobalColumn - 1;
    end;
    if (ArrayLength >= 3) and (Values[2] <> nil) then
    begin
      Row := PInteger(Values[2])^ - 1;
    end
    else
    begin
      Row := GlobalRow - 1;
    end;
    if (ArrayLength >= 4) and (Values[3] <> nil) then
    begin
      Layer := PInteger(Values[3])^ - 1;
    end
    else
    begin
      Layer := GlobalLayer - 1;
    end;
    result := GlobalCurrentScreenObject.ObjectSectionIntersectLength(Column, Row,
      Layer, Section);
  end;
end;

function _ObjectIntersectArea(Values: array of pointer): double;
var
  Column, Row, Layer: integer;
  ArrayLength: integer;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := 0;
  end
  else
  begin
    ArrayLength := Length(Values);
    if (ArrayLength >= 1) and (Values[0] <> nil) then
    begin
      Column := PInteger(Values[0])^ - 1;
    end
    else
    begin
      Column := GlobalColumn - 1;
    end;
    if (ArrayLength >= 2) and (Values[1] <> nil) then
    begin
      Row := PInteger(Values[1])^ - 1;
    end
    else
    begin
      Row := GlobalRow - 1;
    end;
    if (ArrayLength >= 3) and (Values[2] <> nil) then
    begin
      Layer := PInteger(Values[2])^ - 1;
    end
    else
    begin
      Layer := GlobalLayer - 1;
    end;
    result := GlobalCurrentScreenObject.ObjectIntersectArea(Column, Row, Layer);
  end;
end;

function _ObjectName(Values: array of pointer): string;
begin
  if GlobalCurrentScreenObject = nil then
  begin
    result := '';
  end
  else
  begin
    result := GlobalCurrentScreenObject.Name;
  end;
end;

function _NodeInterpolate(Values: array of pointer): double;
begin
  result := 0;
  Assert(False);
end;

function EvaluateBooleanDataSetOnLayer(const DataSetName: string;
  Values: array of pointer): boolean;
var
  DataArray: TDataArray;
  ArrayLength: integer;
  Column, Row, Layer: integer;
begin
  DataArray := frmGoPhast.PhastModel.GetDataSetByName(DataSetName);
  Assert(DataArray <> nil);
  DataArray.Initialize;
  ArrayLength := Length(Values);
  Assert(ArrayLength = 1);
  Layer := PInteger(Values[0])^ - 1;
  if Layer < 0 then
  begin
    result := False;
  end
  else
  begin
    Column := GlobalColumn - 1;
    Row := GlobalRow - 1;
    result := DataArray.BooleanData[Layer, Row, Column];
  end;
end;

function _ActiveOnLayer(Values: array of pointer): boolean;
var
  Layer: Integer;
  DataArray: TDataArray;
  ArrayLength: Integer;
begin
  DataArray := frmGoPhast.PhastModel.GetDataSetByName(rsActive);
  Assert(DataArray <> nil);
  DataArray.Initialize;
  ArrayLength := Length(Values);
  Assert(ArrayLength = 1);
  Layer := PInteger(Values[0])^ - 1;
  if Layer >= DataArray.LayerCount then
  begin
    result := False;
    Exit;
  end;
  result := EvaluateBooleanDataSetOnLayer(rsActive, Values);
end;

function _SpecifiedHeadOnLayer(Values: array of pointer): boolean;
begin
  result := EvaluateBooleanDataSetOnLayer(rsModflowSpecifiedHead, Values);
end;

procedure GetImportedValues(var ImportedValues: TValueArrayStorage;
  const Values: array of Pointer);
var
  ImportedName: string;
begin
  if (Length(Values) > 0) and (Values[0] <> nil) then
  begin
    ImportedName := PString(Values[0])^;
    if ImportedName = StrImportedHigherElev then
    begin
      ImportedValues :=
        GlobalCurrentScreenObject.ImportedHigherSectionElevations;
    end
    else if ImportedName = StrImportedLowerEleva then
    begin
      ImportedValues :=
        GlobalCurrentScreenObject.ImportedLowerSectionElevations;
    end
    else if ImportedName = StrImportedElevations then
    begin
      ImportedValues :=
        GlobalCurrentScreenObject.ImportedSectionElevations;
    end
    else
    begin
      ImportedValues := GlobalCurrentScreenObject.
        ImportedValues.ValuesByName(ImportedName);
    end;
  end
  else
  begin
    ImportedValues := GlobalCurrentScreenObject.CurrentValues;
  end;
end;

function _ImportedScreenObjectValuesR(Values: array of pointer): double;
var
  Index: integer;
  ImportedValues: TValueArrayStorage;
begin
  result := 0;
  if GlobalCurrentScreenObject <> nil then
  begin
    GetImportedValues(ImportedValues, Values);
    if ImportedValues <> nil then
    begin
      Index := GlobalSection;
      Assert(Index >= 0);
      Assert(Index < ImportedValues.Count);
      Assert(ImportedValues.DataType= rdtDouble);
      result := ImportedValues.RealValues[Index];
    end;
  end;
end;

function _ImportedScreenObjectValuesI(Values: array of pointer): integer;
var
  Index: integer;
  ImportedValues: TValueArrayStorage;
begin
  result := 0;
  if GlobalCurrentScreenObject <> nil then
  begin
    GetImportedValues(ImportedValues, Values);
    if ImportedValues <> nil then
    begin
      Index := GlobalSection;
      Assert(Index >= 0);
      Assert(Index < ImportedValues.Count);
      Assert(ImportedValues.DataType= rdtInteger);
      result := ImportedValues.IntValues[Index];
    end;
  end;
end;

function _ImportedScreenObjectValuesB(Values: array of pointer): boolean;
var
  Index: integer;
  ImportedValues: TValueArrayStorage;
begin
  result := False;
  if GlobalCurrentScreenObject <> nil then
  begin
    GetImportedValues(ImportedValues, Values);
    if ImportedValues <> nil then
    begin
      Index := GlobalSection;
      Assert(Index >= 0);
      Assert(Index < ImportedValues.Count);
      Assert(ImportedValues.DataType= rdtBoolean);
      result := ImportedValues.BooleanValues[Index];
    end;
  end;
end;

function _ImportedScreenObjectValuesT(Values: array of pointer): string;
var
  Index: integer;
  ImportedValues: TValueArrayStorage;
begin
  result := '';
  if GlobalCurrentScreenObject <> nil then
  begin
    GetImportedValues(ImportedValues, Values);
    if ImportedValues <> nil then
    begin
      Index := GlobalSection;
      Assert(Index >= 0);
      Assert(Index < ImportedValues.Count);
      Assert(ImportedValues.DataType= rdtString);
      result := ImportedValues.StringValues[Index];
    end;
  end;
end;

procedure UpdateCellValue(var CellValue: Double; Parameter: TModflowParameter;
  Param: THufUsedParameter; PhastModel: TPhastModel; Column: Integer; Row: Integer);
var
  Multiplier: Double;
  MultiplierArray: TDataArray;
  ZoneArray: TDataArray;
//  UseParam: Boolean;
begin
//  UseParam := True;
  if Param.UseZone then
  begin
    ZoneArray := PhastModel.GetDataSetByName(Param.ZoneDataSetName);
    Assert(ZoneArray <> nil);
    ZoneArray.Initialize;
    if not ZoneArray.BooleanData[0, Row, Column] then
    begin
      Exit;
    end;
  end;
  if Param.UseMultiplier then
  begin
    MultiplierArray := PhastModel.GetDataSetByName(Param.MultiplierDataSetName);
    Assert(MultiplierArray <> nil);
    MultiplierArray.Initialize;
    Multiplier := MultiplierArray.RealData[0, Row, Column];
  end
  else
  begin
    Multiplier := 1;
  end;
  CellValue := CellValue + Multiplier * Parameter.Value;
end;

function _HufKx(Values: array of pointer): double;
var
  Layer: Integer;
  Row: Integer;
  Column: Integer;
  LayerTop: Double;
  LayerBottom: Double;
  HufUnitIndex: Integer;
  HydrogeologicUnits: THydrogeologicUnits;
  HufUnit: THydrogeologicUnit;
  PhastModel: TPhastModel;
  ThicknessDataArray: TDataArray;
  HufThickness: Double;
  TopDataArray: TDataArray;
  HufTop: Double;
  HufBottom: Double;
  HufK: double;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
  ZoneArray: TDataArray;
  MultiplierArray: TDataArray;
  Multiplier: Double;
  CumulativeHufThickness: double;
  InitialHeadDataArray: TDataArray;
  InitialHead: Double;
  LayerGroup: TLayerGroup;
  KDEP_Used: Boolean;
  RefSurfDataArray: TDataArray;
  GroundSurface: Double;
  ModelTopArray: TDataArray;
  DepthToTop: Double;
  DepthToBottom: Double;
  Lambda: double;
  KDepMult: double;
//  AValue: Double;
begin
// This function returns the hydraulic conductivity of a cell as calculated
// using the method in the HUF package. At present, it accounts
// for KDEP but not for LVDA.

  if(Length(Values) >= 1) and (Values[0] <> nil) then
  begin
    Layer := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Layer := GlobalLayer-1;
  end;
  if(Length(Values) >= 2) and (Values[1] <> nil) then
  begin
    Row := PInteger(Values[1])^ - 1;
  end
  else
  begin
    Row := GlobalRow-1;
  end;
  if(Length(Values) >= 3) and (Values[2] <> nil) then
  begin
    Column := PInteger(Values[2])^ - 1;
  end
  else
  begin
    Column := GlobalColumn-1;
  end;

  PhastModel := frmGoPhast.PhastModel;

  result := 0;
  if not PhastModel.LayerStructure.IsLayerSimulated(Layer) then
  begin
    Exit;
  end;

  LayerTop := GetLayerPosition (Layer, Row, Column);
  LayerBottom := GetLayerPosition (Layer+1, Row, Column);

  LayerGroup := PhastModel.LayerStructure.GetLayerGroupByLayer(Layer);
  if LayerGroup.AquiferType <> 0 then
  begin
    InitialHeadDataArray := PhastModel.GetDataSetByName(rsModflow_Initial_Head);
    InitialHeadDataArray.Initialize;
    InitialHead := InitialHeadDataArray.RealData[Layer, Row, Column];
    if InitialHead < LayerTop then
    begin
      LayerTop := InitialHead;
    end;
  end;

  if LayerTop <= LayerBottom then
  begin
    Exit;
  end;
  
  HydrogeologicUnits := PhastModel.HydrogeologicUnits;
  CumulativeHufThickness := 0.;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufK := 0.;
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    ThicknessDataArray := PhastModel.GetDataSetByName(
      HufUnit.ThickessDataArrayName);
    Assert(ThicknessDataArray <> nil);
    ThicknessDataArray.Initialize;
    HufThickness := ThicknessDataArray.RealData[0, Row, Column];
    if HufThickness > 0 then
    begin
      TopDataArray := PhastModel.GetDataSetByName(
        HufUnit.TopDataArrayName);
      Assert(TopDataArray <> nil);
      TopDataArray.Initialize;
      HufTop := TopDataArray.RealData[0, Row, Column];
      if HufTop > LayerBottom then
      begin
        HufBottom := HufTop - HufThickness;
        if HufBottom < LayerTop then
        begin
          if HufBottom < LayerBottom then
          begin
            HufBottom := LayerBottom;
          end;
          if HufTop > LayerTop then
          begin
            HufTop := LayerTop
          end;
          HufThickness := HufTop - HufBottom;
          Lambda := 0.;
          if HufThickness > 0 then
          begin
            CumulativeHufThickness := CumulativeHufThickness + HufThickness;
            KDEP_Used := False;
            for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
            begin
              Param := HufUnit.HufUsedParameters[ParamIndex];
              Parameter := Param.Parameter;
              if Parameter.ParameterType = ptHUF_HK then
              begin
                UpdateCellValue(HufK, Parameter, Param, PhastModel, Column, Row);
              end
              else if Parameter.ParameterType = ptHUF_KDEP then
              begin
                KDEP_Used := True;
                UpdateCellValue(Lambda, Parameter, Param, PhastModel, Column, Row);
              end;
            end;

            KDepMult := 1.;
            if KDEP_Used then
            begin
              if PhastModel.ModflowPackages.HufPackage.ReferenceChoice
                = hrcReferenceLayer then
              begin
                RefSurfDataArray := PhastModel.GetDataSetByName(
                  StrHufReferenceSurface);
                RefSurfDataArray.Initialize;
                GroundSurface := RefSurfDataArray.RealData[0,Row,Column];
              end
              else
              begin
                ModelTopArray := PhastModel.GetDataSetByName(StrModelTop);
                ModelTopArray.Initialize;
                GroundSurface := ModelTopArray.RealData[0,Row,Column];
              end;
              DepthToTop := GroundSurface - HufTop;
              DepthToBottom := GroundSurface - HufBottom;
              if Abs(2*(DepthToTop-DepthToBottom)/
                (DepthToBottom+DepthToTop)) < 1E-6 then
              begin
                KDepMult := 1.;
              end
              else
              begin
                KDepMult := Power(10.,(-Lambda*DepthToBottom))
                  - Power(10.,(-Lambda*DepthToTop));
                KDepMult := KDepMult / (-Lambda*Ln(10.0)*(DepthToBottom-DepthToTop))
              end;
            end;
            // result is in terms of transmissivity at this point.
            result := result + HufK * KDepMult *HufThickness;
          end;
        end;
      end;
    end;
  end;
  if CumulativeHufThickness > 0 then
  begin
    // convert transmissivity back to hydraulic conductivity.
    result := result/CumulativeHufThickness;
  end;
end;

{ TNodeInterpolateExpression }

procedure TNodeInterpolateExpression.Evaluate;
var
  ArrayLength: integer;
  AVariable: TConstant;
  Value1, Value2: double;
  Point1, Point2: TPoint2D;
  SegmentLength: double;
  PositionDistance: double;
  IsFirstPoint: Boolean;
  IsLastPoint: Boolean;
  function EvaluateLength(const X1, Y1, X2, Y2: double): double;
  begin
    result := Sqrt(Sqr(X1 - X2) + Sqr(Y1 - Y2));
  end;
begin
  if ShouldEvaluate then
  begin
    if (GlobalCurrentSegment = nil) or (GlobalCurrentScreenObject = nil) then
    begin
      PDouble(FResult)^ := 0;
      Exit;
    end;

    ArrayLength := Length(Data);
    Assert(ArrayLength >= 2);
    if GlobalCurrentSegment.VertexIndex >= ArrayLength - 1 then
    begin
      Assert(Data[ArrayLength - 1].DataType in [rdtDouble, rdtInteger]);
      AVariable := Data[ArrayLength - 1].Datum;
      if AVariable is TExpression then
      begin
        TExpression(AVariable).Evaluate;
      end;
      PDouble(FResult)^ := AVariable.DoubleResult;
      Exit;
    end;

    Assert(Data[GlobalCurrentSegment.VertexIndex].DataType in [rdtDouble,
      rdtInteger]);
    AVariable := Data[GlobalCurrentSegment.VertexIndex].Datum;
    if AVariable is TExpression then
    begin
      TExpression(AVariable).Evaluate;
    end;
    Value1 := AVariable.DoubleResult;
    Point1 := GlobalCurrentScreenObject.Points[GlobalCurrentSegment.VertexIndex];
    IsFirstPoint := False;
    case GlobalCurrentScreenObject.ViewDirection of
      vdTop, vdFront:
        begin
          IsFirstPoint := (Point1.X = GlobalCurrentSegment.X1)
            and (Point1.Y = GlobalCurrentSegment.Y1)
        end;
      vdSide:
        begin
          IsFirstPoint := (Point1.Y = GlobalCurrentSegment.X1)
            and (Point1.X = GlobalCurrentSegment.Y1)
        end;
      else Assert(False);
    end;
    if IsFirstPoint then
    begin
      PDouble(FResult)^ := Value1;
      Exit;
    end;

    Assert(Data[GlobalCurrentSegment.VertexIndex + 1].DataType in [rdtDouble,
      rdtInteger]);
    AVariable := Data[GlobalCurrentSegment.VertexIndex + 1].Datum;
    if AVariable is TExpression then
    begin
      TExpression(AVariable).Evaluate;
    end;
    Value2 := AVariable.DoubleResult;
    Point2 := GlobalCurrentScreenObject.Points[
      GlobalCurrentSegment.VertexIndex + 1];
    IsLastPoint := False;
    case GlobalCurrentScreenObject.ViewDirection of
      vdTop, vdFront:
        begin
          IsLastPoint := (Point2.X = GlobalCurrentSegment.X2)
            and (Point2.Y = GlobalCurrentSegment.Y2);
        end;
      vdSide:
        begin
          IsLastPoint := (Point2.Y = GlobalCurrentSegment.X2)
            and (Point2.X = GlobalCurrentSegment.Y2);
        end;
      else Assert(False);
    end;
    if IsLastPoint then
    begin
      PDouble(FResult)^ := Value2;
      Exit;
    end;

    SegmentLength := EvaluateLength(Point1.X, Point1.Y, Point2.X, Point2.Y);

    case GlobalCurrentScreenObject.ViewDirection of
      vdTop, vdFront:
        begin
          Point2.X := (GlobalCurrentSegment.X1 + GlobalCurrentSegment.X2) / 2;
          Point2.Y := (GlobalCurrentSegment.Y1 + GlobalCurrentSegment.Y2) / 2;
        end;
      vdSide:
        begin
          Point2.Y := (GlobalCurrentSegment.X1 + GlobalCurrentSegment.X2) / 2;
          Point2.X := (GlobalCurrentSegment.Y1 + GlobalCurrentSegment.Y2) / 2;
        end;
      else Assert(False);
    end;
    PositionDistance := EvaluateLength(Point1.X, Point1.Y, Point2.X, Point2.Y);

    PDouble(FResult)^ := PositionDistance / SegmentLength * (Value2 - Value1) +
      Value1;
  end;
end;

{ TActiveOnLayer }

function TActiveOnLayer.GetVariablesUsed: TStringList;
begin
  result := inherited GetVariablesUsed;
  result.Add(rsActive);
end;

function TActiveOnLayer.UsesVariable(const Variable: TCustomVariable): boolean;
begin
  result := inherited UsesVariable(Variable)
    or (Variable.Name = UpperCase(rsActive));
end;

{ TSpecifiedHeadOnLayer }

function TSpecifiedHeadOnLayer.GetVariablesUsed: TStringList;
begin
  result := inherited GetVariablesUsed;
  result.Add(rsModflowSpecifiedHead);
end;

function TSpecifiedHeadOnLayer.UsesVariable(
  const Variable: TCustomVariable): boolean;
begin
  result := inherited UsesVariable(Variable)
    or (Variable.Name = UpperCase(rsModflowSpecifiedHead));
end;

initialization
  SpecialImplementors := TList.Create;

  XFunction.ResultType := rdtDouble;
  XFunction.RFunctionAddr := _X;
  SetLength(XFunction.InputDataTypes, 0);
  XFunction.OptionalArguments := 0;
  XFunction.CanConvertToConstant := False;
  XFunction.Name := 'X';
  XFunction.Prototype := 'GIS|X';

  YFunction.ResultType := rdtDouble;
  YFunction.RFunctionAddr := _Y;
  SetLength(YFunction.InputDataTypes, 0);
  YFunction.OptionalArguments := 0;
  YFunction.CanConvertToConstant := False;
  YFunction.Name := 'Y';
  YFunction.Prototype := 'GIS|Y';

  ZFunction.ResultType := rdtDouble;
  ZFunction.RFunctionAddr := _Z;
  SetLength(ZFunction.InputDataTypes, 0);
  ZFunction.OptionalArguments := 0;
  ZFunction.CanConvertToConstant := False;
  ZFunction.Name := 'Z';
  ZFunction.Prototype := 'GIS|Z';

  XPrimeFunction.ResultType := rdtDouble;
  XPrimeFunction.RFunctionAddr := _XPrime;
  SetLength(XPrimeFunction.InputDataTypes, 0);
  XPrimeFunction.OptionalArguments := 0;
  XPrimeFunction.CanConvertToConstant := False;
  XPrimeFunction.Name := 'X_Prime';
  XPrimeFunction.Prototype := 'GIS|X_Prime';

  YPrimeFunction.ResultType := rdtDouble;
  YPrimeFunction.RFunctionAddr := _YPrime;
  SetLength(YPrimeFunction.InputDataTypes, 0);
  YPrimeFunction.OptionalArguments := 0;
  YPrimeFunction.CanConvertToConstant := False;
  YPrimeFunction.Name := 'Y_Prime';
  YPrimeFunction.Prototype := 'GIS|Y_Prime';

  ColumnFunction.ResultType := rdtInteger;
  ColumnFunction.IFunctionAddr := _Column;
  SetLength(ColumnFunction.InputDataTypes, 0);
  ColumnFunction.OptionalArguments := 0;
  ColumnFunction.CanConvertToConstant := False;
  ColumnFunction.Name := 'Column';
  ColumnFunction.Prototype := 'Grid|Column';

  ElevationToModelLayerFunction.ResultType := rdtInteger;
  ElevationToModelLayerFunction.IFunctionAddr := _ElevationToModelLayer;
  SetLength(ElevationToModelLayerFunction.InputDataTypes, 1);
  ElevationToModelLayerFunction.InputDataTypes[0] := rdtDouble;
  ElevationToModelLayerFunction.OptionalArguments := 0;
  ElevationToModelLayerFunction.CanConvertToConstant := False;
  ElevationToModelLayerFunction.Name := 'ElevationToModelLayer';
  ElevationToModelLayerFunction.Prototype := 'Grid|ElevationToModelLayer(Elevation)';

  ElevationToLayerFunction.ResultType := rdtInteger;
  ElevationToLayerFunction.IFunctionAddr := _ElevationToLayer;
  SetLength(ElevationToLayerFunction.InputDataTypes, 1);
  ElevationToLayerFunction.InputDataTypes[0] := rdtDouble;
  ElevationToLayerFunction.OptionalArguments := 0;
  ElevationToLayerFunction.CanConvertToConstant := False;
  ElevationToLayerFunction.Name := 'ElevationToLayer';
  ElevationToLayerFunction.Prototype := 'Grid|ElevationToLayer(Elevation)';

  RowFunction.ResultType := rdtInteger;
  RowFunction.IFunctionAddr := _Row;
  SetLength(RowFunction.InputDataTypes, 0);
  RowFunction.OptionalArguments := 0;
  RowFunction.CanConvertToConstant := False;
  RowFunction.Name := 'Row';
  RowFunction.Prototype := 'Grid|Row';

  LayerFunction.ResultType := rdtInteger;
  LayerFunction.IFunctionAddr := _Layer;
  SetLength(LayerFunction.InputDataTypes, 0);
  LayerFunction.OptionalArguments := 0;
  LayerFunction.CanConvertToConstant := False;
  LayerFunction.Name := 'Layer';
  LayerFunction.Prototype := 'Grid|Layer';

  ColumnWidthFunction.ResultType := rdtDouble;
  ColumnWidthFunction.RFunctionAddr := _ColumnWidth;
  SetLength(ColumnWidthFunction.InputDataTypes, 1);
  ColumnWidthFunction.InputDataTypes[0] := rdtInteger;
  ColumnWidthFunction.OptionalArguments := 1;
  ColumnWidthFunction.CanConvertToConstant := False;
  ColumnWidthFunction.Name := 'ColumnWidth';
  ColumnWidthFunction.Prototype := 'Grid|ColumnWidth({Column})';

  RowWidthFunction.ResultType := rdtDouble;
  RowWidthFunction.RFunctionAddr := _RowWidth;
  SetLength(RowWidthFunction.InputDataTypes, 1);
  RowWidthFunction.InputDataTypes[0] := rdtInteger;
  RowWidthFunction.OptionalArguments := 1;
  RowWidthFunction.CanConvertToConstant := False;
  RowWidthFunction.Name := 'RowWidth';
  RowWidthFunction.Prototype := 'Grid|RowWidth({Row})';

  LayerHeightFunction.ResultType := rdtDouble;
  LayerHeightFunction.RFunctionAddr := _LayerHeight;
  SetLength(LayerHeightFunction.InputDataTypes, 3);
  LayerHeightFunction.InputDataTypes[0] := rdtInteger;
  LayerHeightFunction.InputDataTypes[1] := rdtInteger;
  LayerHeightFunction.InputDataTypes[2] := rdtInteger;
  LayerHeightFunction.OptionalArguments := 3;
  LayerHeightFunction.CanConvertToConstant := False;
  LayerHeightFunction.Name := 'LayerHeight';
  LayerHeightFunction.Prototype := 'Grid|LayerHeight({{Col, Row,} Layer})';

  BlockAreaTopFunction.ResultType := rdtDouble;
  BlockAreaTopFunction.RFunctionAddr := _BlockAreaTop;
  SetLength(BlockAreaTopFunction.InputDataTypes, 2);
  BlockAreaTopFunction.InputDataTypes[0] := rdtInteger;
  BlockAreaTopFunction.InputDataTypes[1] := rdtInteger;
  BlockAreaTopFunction.OptionalArguments := 2;
  BlockAreaTopFunction.CanConvertToConstant := False;
  BlockAreaTopFunction.Name := 'BlockAreaTop';
  BlockAreaTopFunction.Prototype := 'Grid|BlockAreaTop({Column, Row})';

  BlockAreaFrontFunction.ResultType := rdtDouble;
  BlockAreaFrontFunction.RFunctionAddr := _BlockAreaFront;
  SetLength(BlockAreaFrontFunction.InputDataTypes, 3);
  BlockAreaFrontFunction.InputDataTypes[0] := rdtInteger;
  BlockAreaFrontFunction.InputDataTypes[1] := rdtInteger;
  BlockAreaFrontFunction.InputDataTypes[2] := rdtInteger;
  BlockAreaFrontFunction.OptionalArguments := 3;
  BlockAreaFrontFunction.CanConvertToConstant := False;
  BlockAreaFrontFunction.Name := 'BlockAreaFront';
  BlockAreaFrontFunction.Prototype :=
    'Grid|BlockAreaFront({Column, {Row,} Layer})';

  BlockAreaSideFunction.ResultType := rdtDouble;
  BlockAreaSideFunction.RFunctionAddr := _BlockAreaSide;
  SetLength(BlockAreaSideFunction.InputDataTypes, 3);
  BlockAreaSideFunction.InputDataTypes[0] := rdtInteger;
  BlockAreaSideFunction.InputDataTypes[1] := rdtInteger;
  BlockAreaSideFunction.InputDataTypes[2] := rdtInteger;
  BlockAreaSideFunction.OptionalArguments := 3;
  BlockAreaSideFunction.CanConvertToConstant := False;
  BlockAreaSideFunction.Name := 'BlockAreaSide';
  BlockAreaSideFunction.Prototype := 'Grid|BlockAreaSide({{Col,} Row, Layer})';

  BlockVolumeFunction.ResultType := rdtDouble;
  BlockVolumeFunction.RFunctionAddr := _BlockVolume;
  SetLength(BlockVolumeFunction.InputDataTypes, 3);
  BlockVolumeFunction.InputDataTypes[0] := rdtInteger;
  BlockVolumeFunction.InputDataTypes[1] := rdtInteger;
  BlockVolumeFunction.InputDataTypes[2] := rdtInteger;
  BlockVolumeFunction.OptionalArguments := 3;
  BlockVolumeFunction.CanConvertToConstant := False;
  BlockVolumeFunction.Name := 'BlockVolume';
  BlockVolumeFunction.Prototype := 'Grid|BlockVolume({{Column, Row,} Layer})';

  ColumnPositionFunction.ResultType := rdtDouble;
  ColumnPositionFunction.RFunctionAddr := _ColumnPosition;
  SetLength(ColumnPositionFunction.InputDataTypes, 1);
  ColumnPositionFunction.InputDataTypes[0] := rdtInteger;
  ColumnPositionFunction.OptionalArguments := 1;
  ColumnPositionFunction.CanConvertToConstant := False;
  ColumnPositionFunction.Name := 'ColumnBoundaryPosition';
  ColumnPositionFunction.Prototype := 'Grid|ColumnBoundaryPosition({Column})';

  RowPositionFunction.ResultType := rdtDouble;
  RowPositionFunction.RFunctionAddr := _RowPosition;
  SetLength(RowPositionFunction.InputDataTypes, 1);
  RowPositionFunction.InputDataTypes[0] := rdtInteger;
  RowPositionFunction.OptionalArguments := 1;
  RowPositionFunction.CanConvertToConstant := False;
  RowPositionFunction.Name := 'RowBoundaryPosition';
  RowPositionFunction.Prototype := 'Grid|RowBoundaryPosition({Row})';

  LayerPositionFunction.ResultType := rdtDouble;
  LayerPositionFunction.RFunctionAddr := _LayerPosition;
  SetLength(LayerPositionFunction.InputDataTypes, 3);
  LayerPositionFunction.InputDataTypes[0] := rdtInteger;
  LayerPositionFunction.InputDataTypes[1] := rdtInteger;
  LayerPositionFunction.InputDataTypes[2] := rdtInteger;
  LayerPositionFunction.OptionalArguments := 3;
  LayerPositionFunction.CanConvertToConstant := False;
  LayerPositionFunction.Name := 'LayerBoundaryPosition';
  LayerPositionFunction.Prototype :=
    'Grid|LayerBoundaryPosition({{Column, Row,} Layer})';

  ColumnCountFunction.ResultType := rdtInteger;
  ColumnCountFunction.IFunctionAddr := _ColumnCount;
  SetLength(ColumnCountFunction.InputDataTypes, 0);
  ColumnCountFunction.OptionalArguments := 0;
  ColumnCountFunction.CanConvertToConstant := False;
  ColumnCountFunction.Name := 'ColumnCount';
  ColumnCountFunction.Prototype := 'Grid|ColumnCount';

  RowCountFunction.ResultType := rdtInteger;
  RowCountFunction.IFunctionAddr := _RowCount;
  SetLength(RowCountFunction.InputDataTypes, 0);
  RowCountFunction.OptionalArguments := 0;
  RowCountFunction.CanConvertToConstant := False;
  RowCountFunction.Name := 'RowCount';
  RowCountFunction.Prototype := 'Grid|RowCount';

  LayerCountFunction.ResultType := rdtInteger;
  LayerCountFunction.IFunctionAddr := _LayerCount;
  SetLength(LayerCountFunction.InputDataTypes, 0);
  LayerCountFunction.OptionalArguments := 0;
  LayerCountFunction.CanConvertToConstant := False;
  LayerCountFunction.Name := 'LayerCount';
  LayerCountFunction.Prototype := 'Grid|LayerCount';

  ObjectLengthFunction.ResultType := rdtDouble;
  ObjectLengthFunction.RFunctionAddr := _ObjectLength;
  SetLength(ObjectLengthFunction.InputDataTypes, 0);
  ObjectLengthFunction.OptionalArguments := 0;
  ObjectLengthFunction.CanConvertToConstant := False;
  ObjectLengthFunction.Name := StrObjectLength;
  ObjectLengthFunction.Prototype := 'Object|ObjectLength';

  ObjectAreaFunction.ResultType := rdtDouble;
  ObjectAreaFunction.RFunctionAddr := _ObjectArea;
  SetLength(ObjectAreaFunction.InputDataTypes, 0);
  ObjectAreaFunction.OptionalArguments := 0;
  ObjectAreaFunction.CanConvertToConstant := False;
  ObjectAreaFunction.Name := StrObjectArea;
  ObjectAreaFunction.Prototype := 'Object|ObjectArea';

  ObjectIntersectLengthFunction.ResultType := rdtDouble;
  ObjectIntersectLengthFunction.RFunctionAddr := _ObjectIntersectLength;
  SetLength(ObjectIntersectLengthFunction.InputDataTypes, 3);
  ObjectIntersectLengthFunction.InputDataTypes[0] := rdtInteger;
  ObjectIntersectLengthFunction.InputDataTypes[1] := rdtInteger;
  ObjectIntersectLengthFunction.InputDataTypes[2] := rdtInteger;
  ObjectIntersectLengthFunction.OptionalArguments := 3;
  ObjectIntersectLengthFunction.CanConvertToConstant := False;
  ObjectIntersectLengthFunction.Name := StrObjectIntersectLength;
  ObjectIntersectLengthFunction.Prototype :=
    'Object|ObjectIntersectLength({Column, Row, Layer})';

  ObjectSectionIntersectLengthFunction.ResultType := rdtDouble;
  ObjectSectionIntersectLengthFunction.RFunctionAddr := _ObjectIntersectSectionLength;
  SetLength(ObjectSectionIntersectLengthFunction.InputDataTypes, 4);
  ObjectSectionIntersectLengthFunction.InputDataTypes[0] := rdtInteger;
  ObjectSectionIntersectLengthFunction.InputDataTypes[1] := rdtInteger;
  ObjectSectionIntersectLengthFunction.InputDataTypes[2] := rdtInteger;
  ObjectSectionIntersectLengthFunction.InputDataTypes[3] := rdtInteger;
  ObjectSectionIntersectLengthFunction.OptionalArguments := 4;
  ObjectSectionIntersectLengthFunction.CanConvertToConstant := False;
  ObjectSectionIntersectLengthFunction.Name := StrObjectSectionIntersectLength;
  ObjectSectionIntersectLengthFunction.Prototype :=
    'Object|ObjectSectionIntersectLength({Section, Column, Row, Layer})';

  ObjectIntersectAreaFunction.ResultType := rdtDouble;
  ObjectIntersectAreaFunction.RFunctionAddr := _ObjectIntersectArea;
  SetLength(ObjectIntersectAreaFunction.InputDataTypes, 3);
  ObjectIntersectAreaFunction.InputDataTypes[0] := rdtInteger;
  ObjectIntersectAreaFunction.InputDataTypes[1] := rdtInteger;
  ObjectIntersectAreaFunction.InputDataTypes[2] := rdtInteger;
  ObjectIntersectAreaFunction.OptionalArguments := 3;
  ObjectIntersectAreaFunction.CanConvertToConstant := False;
  ObjectIntersectAreaFunction.Name := StrObjectIntersectArea;
  ObjectIntersectAreaFunction.Prototype :=
    'Object|ObjectIntersectArea({Column, Row, Layer})';

  ObjectNameFunction.ResultType := rdtString;
  ObjectNameFunction.SFunctionAddr := _ObjectName;
  SetLength(ObjectNameFunction.InputDataTypes, 0);
  ObjectNameFunction.OptionalArguments := 0;
  ObjectNameFunction.CanConvertToConstant := False;
  ObjectNameFunction.Name := 'ObjectName';
  ObjectNameFunction.Prototype :=
    'Object|ObjectName';

  NodeXFunction.ResultType := rdtDouble;
  NodeXFunction.RFunctionAddr := _XNodePosition;
  SetLength(NodeXFunction.InputDataTypes, 1);
  NodeXFunction.InputDataTypes[0] := rdtInteger;
  NodeXFunction.OptionalArguments := 0;
  NodeXFunction.CanConvertToConstant := False;
  NodeXFunction.Name := 'ObjectVertexX';
  NodeXFunction.Prototype := 'Object|ObjectVertexX(VertexIndex)';
  SetLength(NodeXFunction.Synonyms, 1);
  NodeXFunction.Synonyms[0] := 'ObjectNodeX';

  NodeYFunction.ResultType := rdtDouble;
  NodeYFunction.RFunctionAddr := _YNodePosition;
  SetLength(NodeYFunction.InputDataTypes, 1);
  NodeYFunction.InputDataTypes[0] := rdtInteger;
  NodeYFunction.OptionalArguments := 0;
  NodeYFunction.CanConvertToConstant := False;
  NodeYFunction.Name := 'ObjectVertexY';
  NodeYFunction.Prototype := 'Object|ObjectVertexY(VertexIndex)';
  SetLength(NodeYFunction.Synonyms, 1);
  NodeYFunction.Synonyms[0] := 'ObjectNodeY';

  NodeZFunction.ResultType := rdtDouble;
  NodeZFunction.RFunctionAddr := _ZNodePosition;
  SetLength(NodeZFunction.InputDataTypes, 1);
  NodeZFunction.InputDataTypes[0] := rdtInteger;
  NodeZFunction.OptionalArguments := 0;
  NodeZFunction.CanConvertToConstant := False;
  NodeZFunction.Name := 'ObjectVertexZ';
  NodeZFunction.Prototype := 'Object|ObjectVertexZ(VertexIndex)';
  SetLength(NodeZFunction.Synonyms, 1);
  NodeZFunction.Synonyms[0] := 'ObjectNodeZ';



  NodeDistanceFunction.ResultType := rdtDouble;
  NodeDistanceFunction.RFunctionAddr := _NodeDistances;
  SetLength(NodeDistanceFunction.InputDataTypes, 1);
  NodeDistanceFunction.InputDataTypes[0] := rdtInteger;
  NodeDistanceFunction.OptionalArguments := 0;
  NodeDistanceFunction.CanConvertToConstant := False;
  NodeDistanceFunction.Name := 'ObjectVertexDistance';
  NodeDistanceFunction.Prototype := 'Object|ObjectVertexDistance(VertexIndex)';
  SetLength(NodeDistanceFunction.Synonyms, 1);
  NodeDistanceFunction.Synonyms[0] := 'ObjectNodeDistance';

  CurrentNodeXFunction.ResultType := rdtDouble;
  CurrentNodeXFunction.RFunctionAddr := _CurrentXNodePosition;
  CurrentNodeXFunction.OptionalArguments := 0;
  CurrentNodeXFunction.CanConvertToConstant := False;
  CurrentNodeXFunction.Name := 'ObjectCurrentVertexX';
  CurrentNodeXFunction.Prototype := 'Object|ObjectCurrentVertexX';
  SetLength(CurrentNodeXFunction.Synonyms, 1);
  CurrentNodeXFunction.Synonyms[0] := 'ObjectCurrentNodeX';

  CurrentNodeYFunction.ResultType := rdtDouble;
  CurrentNodeYFunction.RFunctionAddr := _CurrentYNodePosition;
  CurrentNodeYFunction.OptionalArguments := 0;
  CurrentNodeYFunction.CanConvertToConstant := False;
  CurrentNodeYFunction.Name := 'ObjectCurrentVertexY';
  CurrentNodeYFunction.Prototype := 'Object|ObjectCurrentVertexY';
  SetLength(CurrentNodeYFunction.Synonyms, 1);
  CurrentNodeYFunction.Synonyms[0] := 'ObjectCurrentNodeY';

  CurrentNodeZFunction.ResultType := rdtDouble;
  CurrentNodeZFunction.RFunctionAddr := _CurrentZNodePosition;
  CurrentNodeZFunction.OptionalArguments := 0;
  CurrentNodeZFunction.CanConvertToConstant := False;
  CurrentNodeZFunction.Name := 'ObjectCurrentVertexZ';
  CurrentNodeZFunction.Prototype := 'Object|ObjectCurrentVertexZ';
  SetLength(CurrentNodeZFunction.Synonyms, 1);
  CurrentNodeZFunction.Synonyms[0] := 'ObjectCurrentNodeZ';

  CurrentSegmentLengthFunction.ResultType := rdtDouble;
  CurrentSegmentLengthFunction.RFunctionAddr := _CurrentSegmentLength;
  CurrentSegmentLengthFunction.OptionalArguments := 0;
  CurrentSegmentLengthFunction.CanConvertToConstant := False;
  CurrentSegmentLengthFunction.Name := 'ObjectCurrentSegmentLength';
  CurrentSegmentLengthFunction.Prototype := 'Object|ObjectCurrentSegmentLength';

  CurrentSectionIndexFunction.ResultType := rdtInteger;
  CurrentSectionIndexFunction.IFunctionAddr := _CurrentSectionIndex;
  CurrentSectionIndexFunction.OptionalArguments := 0;
  CurrentSectionIndexFunction.CanConvertToConstant := False;
  CurrentSectionIndexFunction.Name := 'ObjectCurrentSectionIndex';
  CurrentSectionIndexFunction.Prototype := 'Object|ObjectCurrentSectionIndex';

  FractionOfObjectLengthFunction.ResultType := rdtDouble;
  FractionOfObjectLengthFunction.RFunctionAddr := _FractionOfObjectLength;
  FractionOfObjectLengthFunction.OptionalArguments := 0;
  FractionOfObjectLengthFunction.CanConvertToConstant := False;
  FractionOfObjectLengthFunction.Name := 'FractionOfObjectLength';
  FractionOfObjectLengthFunction.Prototype := 'Object|FractionOfObjectLength';

  NodeCountFunction.ResultType := rdtDouble;
  NodeCountFunction.RFunctionAddr := _ObjectNodeCount;
  NodeCountFunction.OptionalArguments := 0;
  NodeCountFunction.CanConvertToConstant := False;
  NodeCountFunction.Name := 'ObjectVertexCount';
  NodeCountFunction.Prototype := 'Object|ObjectVertexCount';
  SetLength(NodeCountFunction.Synonyms, 1);
  NodeCountFunction.Synonyms[0] := 'ObjectNodeCount';

  ImportedValuesRFunction.ResultType := rdtDouble;
  ImportedValuesRFunction.RFunctionAddr := _ImportedScreenObjectValuesR;
  SetLength(ImportedValuesRFunction.InputDataTypes, 1);
  ImportedValuesRFunction.InputDataTypes[0] := rdtString;
  ImportedValuesRFunction.OptionalArguments := 1;
  ImportedValuesRFunction.CanConvertToConstant := False;
  ImportedValuesRFunction.Name := rsObjectImportedValuesR;
  ImportedValuesRFunction.Prototype := 'Object|' + rsObjectImportedValuesR
    + '({Key})';
  ImportedValuesRFunction.Hidden := False;

  ImportedValuesIFunction.ResultType := rdtInteger;
  ImportedValuesIFunction.IFunctionAddr := _ImportedScreenObjectValuesI;
  SetLength(ImportedValuesIFunction.InputDataTypes, 1);
  ImportedValuesIFunction.InputDataTypes[0] := rdtString;
  ImportedValuesIFunction.OptionalArguments := 1;
  ImportedValuesIFunction.CanConvertToConstant := False;
  ImportedValuesIFunction.Name := rsObjectImportedValuesI;
  ImportedValuesIFunction.Prototype := 'Object|' + rsObjectImportedValuesI
    + '({Key})';
  ImportedValuesIFunction.Hidden := False;

  ImportedValuesBFunction.ResultType := rdtBoolean;
  ImportedValuesBFunction.BFunctionAddr := _ImportedScreenObjectValuesB;
  SetLength(ImportedValuesBFunction.InputDataTypes, 1);
  ImportedValuesBFunction.InputDataTypes[0] := rdtString;
  ImportedValuesBFunction.OptionalArguments := 1;
  ImportedValuesBFunction.CanConvertToConstant := False;
  ImportedValuesBFunction.Name := rsObjectImportedValuesB;
  ImportedValuesBFunction.Prototype := 'Object|' + rsObjectImportedValuesB
    + '({Key})';
  ImportedValuesBFunction.Hidden := False;

  ImportedValuesTFunction.ResultType := rdtString;
  ImportedValuesTFunction.SFunctionAddr := _ImportedScreenObjectValuesT;
  SetLength(ImportedValuesTFunction.InputDataTypes, 1);
  ImportedValuesTFunction.InputDataTypes[0] := rdtString;
  ImportedValuesTFunction.OptionalArguments := 1;
  ImportedValuesTFunction.CanConvertToConstant := False;
  ImportedValuesTFunction.Name := rsObjectImportedValuesT;
  ImportedValuesTFunction.Prototype := 'Object|' + rsObjectImportedValuesT
    + '({Key})';
  ImportedValuesTFunction.Hidden := False;

  ListRealValueFunction.ResultType := rdtDouble;
  ListRealValueFunction.RFunctionAddr := _ListDataSetRealValue;
  SetLength(ListRealValueFunction.InputDataTypes, 1);
  ListRealValueFunction.InputDataTypes[0] := rdtString;
  ListRealValueFunction.OptionalArguments := 0;
  ListRealValueFunction.CanConvertToConstant := False;
  ListRealValueFunction.Name := rsListRealValue;
  ListRealValueFunction.Prototype := 'Object|' + rsListRealValue +
    '("DataSetName")';
  ListRealValueFunction.Hidden := True;

  ListIntegerValueFunction.ResultType := rdtInteger;
  ListIntegerValueFunction.IFunctionAddr := _ListDataSetIntegerValue;
  SetLength(ListIntegerValueFunction.InputDataTypes, 1);
  ListIntegerValueFunction.InputDataTypes[0] := rdtString;
  ListIntegerValueFunction.OptionalArguments := 0;
  ListIntegerValueFunction.CanConvertToConstant := False;
  ListIntegerValueFunction.Name := rsListIntegerValue;
  ListIntegerValueFunction.Prototype := 'Object|' + rsListIntegerValue +
    '("DataSetName")';
  ListIntegerValueFunction.Hidden := True;

  ModflowLayerSimulatedFunction.ResultType := rdtBoolean;
  ModflowLayerSimulatedFunction.BFunctionAddr := _SimulatedModflowLayer;
  SetLength(ModflowLayerSimulatedFunction.InputDataTypes, 1);
  ModflowLayerSimulatedFunction.InputDataTypes[0] := rdtInteger;
  ModflowLayerSimulatedFunction.OptionalArguments := 1;
  ModflowLayerSimulatedFunction.CanConvertToConstant := False;
  ModflowLayerSimulatedFunction.Name := 'SimulatedLayer';
  ModflowLayerSimulatedFunction.Prototype := 'MODFLOW|SimulatedLayer({Layer})';

  ModflowHufKx.ResultType := rdtDouble;
  ModflowHufKx.RFunctionAddr := _HufKx;
  SetLength(ModflowHufKx.InputDataTypes, 3);
  ModflowHufKx.InputDataTypes[0] := rdtInteger;
  ModflowHufKx.InputDataTypes[1] := rdtInteger;
  ModflowHufKx.InputDataTypes[2] := rdtInteger;
  ModflowHufKx.OptionalArguments := 3;
  ModflowHufKx.CanConvertToConstant := False;
  ModflowHufKx.Name := 'HufKx';
  ModflowHufKx.Prototype := 'MODFLOW|HufKx({Layer, Row, Column})';
  ModflowHufKx.Hidden := True;

  NodeInterpolate := TFunctionClass.Create;
  NodeInterpolate.InputDataCount := 3;
  NodeInterpolate.OptionalArguments := -1;
  NodeInterpolate.RFunctionAddr := _NodeInterpolate;
  NodeInterpolate.Name := 'VertexInterpolate';
  NodeInterpolate.Prototype := 'Object|VertexInterpolate(Value1, Value2, ...)';
  NodeInterpolate.Synonyms.Add('NodeInterpolate');
  NodeInterpolate.InputDataTypes[0] := rdtDouble;
  NodeInterpolate.InputDataTypes[1] := rdtDouble;
  NodeInterpolate.InputDataTypes[2] := rdtDouble;
  NodeInterpolate.AllowConversionToConstant := False;

  NodeInterpolateSpecialImplementor := TSpecialImplementor.Create;
  NodeInterpolateSpecialImplementor.FunctionClass := NodeInterpolate;
  NodeInterpolateSpecialImplementor.Implementor := TNodeInterpolateExpression;
  SpecialImplementors.Add(NodeInterpolateSpecialImplementor);

  ActiveOnLayer := TFunctionClass.Create;
  ActiveOnLayer.InputDataCount := 1;
  ActiveOnLayer.OptionalArguments := 0;
  ActiveOnLayer.BFunctionAddr := _ActiveOnLayer;
  ActiveOnLayer.Name := 'ActiveOnLayer';
  ActiveOnLayer.Prototype := 'Grid|ActiveOnLayer(Layer)';
  ActiveOnLayer.InputDataTypes[0] := rdtInteger;
  ActiveOnLayer.AllowConversionToConstant := False;

  ActiveOnLayerSpecialImplementor := TSpecialImplementor.Create;
  ActiveOnLayerSpecialImplementor.FunctionClass := ActiveOnLayer;
  ActiveOnLayerSpecialImplementor.Implementor := TActiveOnLayer;
  SpecialImplementors.Add(ActiveOnLayerSpecialImplementor);

  SpecifiedHeadOnLayer := TFunctionClass.Create;
  SpecifiedHeadOnLayer.InputDataCount := 1;
  SpecifiedHeadOnLayer.OptionalArguments := 0;
  SpecifiedHeadOnLayer.BFunctionAddr := _SpecifiedHeadOnLayer;
  SpecifiedHeadOnLayer.Name := 'SpecifiedHeadOnLayer';
  SpecifiedHeadOnLayer.Prototype := 'MODFLOW|SpecifiedHeadOnLayer(Layer)';
  SpecifiedHeadOnLayer.InputDataTypes[0] := rdtInteger;
  SpecifiedHeadOnLayer.AllowConversionToConstant := False;

  SpecifiedHeadOnLayerSpecialImplementor := TSpecialImplementor.Create;
  SpecifiedHeadOnLayerSpecialImplementor.FunctionClass := SpecifiedHeadOnLayer;
  SpecifiedHeadOnLayerSpecialImplementor.Implementor := TSpecifiedHeadOnLayer;
  SpecialImplementors.Add(SpecifiedHeadOnLayerSpecialImplementor);

finalization
  NodeInterpolate.Free;
  NodeInterpolateSpecialImplementor.Free;

  ActiveOnLayer.Free;
  ActiveOnLayerSpecialImplementor.Free;

  SpecifiedHeadOnLayer.Free;
  SpecifiedHeadOnLayerSpecialImplementor.Free;

  SpecialImplementors.Free;

end.
