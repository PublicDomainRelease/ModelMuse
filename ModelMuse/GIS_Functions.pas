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

  TBcfVcont = class(TExpression)
  protected
    function GetVariablesUsed: TStringList; override;
  public
    {
      @Name returns True if Variable is used by the @Link(TExpression)
      or if the variable is named "Kz", "Confining_Bed_Kz",
      or one of the variables for grid layer elevations.
    }
    function UsesVariable(const Variable: TCustomVariable): boolean; override;
  end;

  TCustomHufExpression = class(TExpression)
  public
    function UsesVariable(const Variable: TCustomVariable): boolean; override;
  end;

  THufKx = class(TCustomHufExpression)
  protected
    function GetVariablesUsed: TStringList; override;
  end;

  THufKy = class(THufKx)
  protected
    function GetVariablesUsed: TStringList; override;
  end;

  THufKz = class(THufKx)
  protected
    function GetVariablesUsed: TStringList; override;
  end;

  THufSS = class(TCustomHufExpression)
  protected
    function GetVariablesUsed: TStringList; override;
  end;

  THufSY = class(TCustomHufExpression)
  protected
    function GetVariablesUsed: TStringList; override;
  end;

  THufSYTP = class(TCustomHufExpression)
  protected
    function GetVariablesUsed: TStringList; override;
  end;
{
 @name adds a series of (mostly) GIS function to Parser.
 The functions are defined in the initialization section.
 In addition, several descendants of TSelectExpression
 or TExpression are defined in the
 implementation section and added to RbwParser.SpecialImplentorList.
 @param(Parser is the TRbwParser to which the GIS functions will be added.)
}
procedure AddGIS_Functions(const Parser: TRbwParser;
  ModelSelection: TModelSelection; EvalAt: TEvaluatedAt);

{ TODO -cRefactor : Consider replacing Model with an interface. }
//
procedure UpdateCurrentModel(const AModel: TBaseModel);

    { TODO -cRefactor : Consider replacing Model with an interface. }
// @name updates a series of global values related to location.
procedure UpdateGlobalLocations(const Col, Row, Layer: integer;
  const EvaluatedAt: TEvaluatedAt; Model: TBaseModel);

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
  StrBcfVCONT = 'BcfVCONT';
  StrHufKx = 'GetHufKx';
  StrHufKy = 'GetHufKy';
  StrHufKz = 'GetHuf_Interlayer_Kz';
  StrHufSs = 'GetHufSs';
  StrHufAverageSy = 'GetHuf_Average_Sy';
  StrHufSy = 'GetHufSy';
  StrHufSytp = 'GetHufSytp';
  StrLayerHeight = 'LayerHeight';
  StrInterpolatedVertexValues = 'InterpolatedVertexValue';
  StrVertexInterpolate = 'VertexInterpolate';
  StrNodeInterpolate = 'NodeInterpolate';
  StrGridNumber = 'GridNumber';
  StrGridName = 'GridName';
  StrParentLayer = 'ParentLayer';
  StrParentRow = 'ParentRow';
  StrParentColumn = 'ParentColumn';
  ObjectCurrentSegmentAngle = 'ObjectCurrentSegmentAngle';
  ObjectDegrees = 'ObjectCurrentSegmentAngleDegrees';
  ObjectDegreesLimited = 'ObjectCurrentSegmentAngleLimitedDegrees';

  ObjectCurSegLength = 'ObjectCurrentSegmentLength';
  ObjectCurrentVertexX = 'ObjectCurrentVertexX';
  ObjectCurrentVertexY = 'ObjectCurrentVertexY';
  ObjectCurrentVertexZ = 'ObjectCurrentVertexZ';


function GetColumnWidth(Column: Integer): Double;
function GetRowWidth(Row: Integer): Double;
function GetLayerHeight(Col, Row, Lay: Integer): Double;

function GetLayerPosition(const Lay, Row, Col: Integer;
  var InvalidIndex: boolean): Double;
function GetLayerCenter(const Lay, Row, Col:  integer): double;

    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
function CurrentModel: TBaseModel;

implementation

uses frmGoPhastUnit, DataSetUnit, FastGEO, LayerStructureUnit, PhastModelUnit,
  ValueArrayStorageUnit, HufDefinition, OrderedCollectionUnit,
  ModflowPackageSelectionUnit, Math, ModflowGridUnit, ModflowParameterUnit,
  frmErrorsAndWarningsUnit, SutraMeshUnit, AbstractGridUnit;

resourcestring
  StrInSVANIParamete = 'In %s, VANI parameters are defined even though that ' +
  'hydrogeologic unit used vertical hydraulic conductivity and vertical anis' +
  'otropy. The VANI parameters will be ignored.';
  StrLayerDRowDCo = 'Layer %0:d, Row %1:d, Column %2:d';
  StrTheDataAccessedTh = 'The data accessed through %s are not real numbers.';
  StrTheDataAccessedThInt = 'The data accessed through %s are not integers.';
  StrTheDataAccessedThBoole = 'The data accessed through %s are not booleans' +
  '.';
  StrTheDataAccessedThStr = 'The data accessed through %s are not text.';
  StrTheSFunctionCan = 'The %s function can only be used with objects.';
  StrThe0sFunctionIs = 'The %0:s function is used but no values have been as' +
  'signed to individual nodes.';
  StrInvalidKeyIn0s = 'Invalid key in %0:s function.';
  StrObject0sInvali = 'Object: %0:s; invalid key: %1:s';
  StrNoImportedDataExi = 'No imported data exists in the object "%s" '
  + 'for the following name(s).';
  StrBecauseHorizontalH = 'Because horizontal hyraulic conductivity is zero ' +
  'in the following cells (Layer, Row, Column), vertical conductivity can no' +
  't be calculated. (This message can be ignored for inactive cells.)';
  StrTheSFunctionDoes = 'The %s function does not apply to locations that are' +
  ' not intersected by an object.';
  StrProblemEvaluating = 'Problem evaluating %s';
  StrThereAreTooFewIm = 'There are too few imported values for %0:s in %1:s.';

var  
  SpecialImplementors: TList;

type
  TNodeInterpolateExpression = class(TSelectExpression)
  public
    procedure Evaluate; override;
  end;

  TGlobalValues = record
    GlobalX, GlobalY, GlobalZ: real;
    GlobalXPrime, GlobalYPrime: real;
    GlobalColumn, GlobalRow, GlobalLayer: integer;
    GlobalCurrentScreenObject: TScreenObject;
    GlobalCurrentSegment: TCellElementSegment;
    GlobalSection: integer;
    { TODO -cRefactor : Consider replacing GlobalCurrentModel with an interface. }
    //
    GlobalCurrentModel: TBaseModel;
    GlobalEvaluatedAt: TEvaluatedAt;
  end;


var
  NodeDistancesSet: boolean;
  FNodeDistances: array of double;

    { TODO -cRefactor : Consider replacing GlobalCurrentModel with an interface. }
    //
  GlobalCurrentModel: TBaseModel;
  GlobalX, GlobalY, GlobalZ: real;
  GlobalXPrime, GlobalYPrime: real;
  GlobalColumn, GlobalRow, GlobalLayer: integer;
  GlobalCurrentScreenObject: TScreenObject;
  GlobalCurrentSegment: TCellElementSegment;
  GlobalSection: integer;
//  GlobalEvalAt: TEvaluatedAt;


  GlobalStack: array of TGlobalValues;

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
  ColumnCenterFunction: TFunctionRecord;
  RowCenterFunction: TFunctionRecord;
  LayerCenterFunction: TFunctionRecord;
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
  CurrentSegmentAngleFunction: TFunctionRecord;
  CurrentSegmentAngleDegreesFunction: TFunctionRecord;
  CurrentSegmentAngleLimitedegreesFunction: TFunctionRecord;
  CurrentSectionIndexFunction: TFunctionRecord;
  FractionOfObjectLengthFunction: TFunctionRecord;
  NodeCountFunction: TFunctionRecord;
  InterpolationedValuesFunction: TFunctionRecord;
  VertexValuesFunction: TFunctionRecord;

  ListRealValueFunction: TFunctionRecord;
  ListIntegerValueFunction: TFunctionRecord;
  ModflowLayerSimulatedFunction: TFunctionRecord;
//  ModflowHufKx: TFunctionRecord;

  ImportedValuesRFunction: TFunctionRecord;
  ImportedValuesIFunction: TFunctionRecord;
  ImportedValuesBFunction: TFunctionRecord;
  ImportedValuesTFunction: TFunctionRecord;

  GridNumberFunction: TFunctionRecord;
  GridNameFunction: TFunctionRecord;
  ParentLayerFunction: TFunctionRecord;
  ParentRowFunction: TFunctionRecord;
  ParentColumnFunction: TFunctionRecord;

  NodeInterpolate: TFunctionClass;
  NodeInterpolateSpecialImplementor: TSpecialImplementor;

  ActiveOnLayer: TFunctionClass;
  ActiveOnLayerSpecialImplementor: TSpecialImplementor;

//  HighestActiveLayer: TFunctionClass;
//  HighestActiveLayerSpecialImplementor: TSpecialImplementor;

  SpecifiedHeadOnLayer: TFunctionClass;
  SpecifiedHeadOnLayerSpecialImplementor: TSpecialImplementor;

  BcfVcont: TFunctionClass;
  BcfVcontSpecialImplementor: TSpecialImplementor;

  HufKx: TFunctionClass;
  HufKxSpecialImplementor: TSpecialImplementor;

  HufKy: TFunctionClass;
  HufKySpecialImplementor: TSpecialImplementor;

  HufKz: TFunctionClass;
  HufKzSpecialImplementor: TSpecialImplementor;

  HufSS: TFunctionClass;
  HufSSSpecialImplementor: TSpecialImplementor;

  HufAverageSY: TFunctionClass;
  HufAverageSYSpecialImplementor: TSpecialImplementor;

  HufSY: TFunctionClass;
  HufSYSpecialImplementor: TSpecialImplementor;

  HufSYTP: TFunctionClass;
  HufSYTPSpecialImplementor: TSpecialImplementor;

  InvalidNames: TStringList;

function CurrentModel: TBaseModel;
begin
  result := GlobalCurrentModel;
end;

procedure PushGlobalStack;
var
  Position : integer;
begin
  Position := Length(GlobalStack);
  SetLength(GlobalStack, Position + 1);

  GlobalStack[Position].GlobalX := GlobalX;
  GlobalStack[Position].GlobalY := GlobalY;
  GlobalStack[Position].GlobalZ := GlobalZ;
  GlobalStack[Position].GlobalXPrime := GlobalXPrime;
  GlobalStack[Position].GlobalYPrime := GlobalYPrime;
  GlobalStack[Position].GlobalColumn := GlobalColumn;
  GlobalStack[Position].GlobalRow := GlobalRow;
  GlobalStack[Position].GlobalLayer := GlobalLayer;
  GlobalStack[Position].GlobalCurrentScreenObject := GlobalCurrentScreenObject;
  GlobalStack[Position].GlobalCurrentSegment := GlobalCurrentSegment;
  GlobalStack[Position].GlobalSection := GlobalSection;
  GlobalStack[Position].GlobalCurrentModel := GlobalCurrentModel;
  GlobalStack[Position].GlobalEvaluatedAt := GlobalEvaluatedAt;
end;

procedure PopGlobalStack;
var
  Position: Integer;
begin
  Position := Length(GlobalStack) -1;

  GlobalX := GlobalStack[Position].GlobalX;
  GlobalY := GlobalStack[Position].GlobalY;
  GlobalZ := GlobalStack[Position].GlobalZ;
  GlobalXPrime := GlobalStack[Position].GlobalXPrime;
  GlobalYPrime := GlobalStack[Position].GlobalYPrime;
  GlobalColumn := GlobalStack[Position].GlobalColumn;
  GlobalRow := GlobalStack[Position].GlobalRow;
  GlobalLayer := GlobalStack[Position].GlobalLayer;
  GlobalCurrentScreenObject := GlobalStack[Position].GlobalCurrentScreenObject;
  GlobalCurrentSegment := GlobalStack[Position].GlobalCurrentSegment;
  GlobalSection := GlobalStack[Position].GlobalSection;
  GlobalCurrentModel := GlobalStack[Position].GlobalCurrentModel;
  GlobalEvaluatedAt := GlobalStack[Position].GlobalEvaluatedAt;

  SetLength(GlobalStack, Position);
end;

procedure AddGIS_Functions(const Parser: TRbwParser;
  ModelSelection: TModelSelection; EvalAt: TEvaluatedAt);
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
  Parser.SpecialImplementorList.Capacity := SpecialImplementors.Count;
  for Index := 0 to SpecialImplementors.Count - 1 do
  begin
    Item := SpecialImplementors[Index];
    case EvalAt of
      eaBlocks: 
        begin
          Parser.SpecialImplementorList.Add(Item);
        end;
      eaNodes:
        begin
          if Item = ActiveOnLayerSpecialImplementor then
          begin
            if frmGoPhast.ModelSelection = msSutra22 then
            begin
              Parser.SpecialImplementorList.Add(Item);
            end;
          end
          else
          begin
            Parser.SpecialImplementorList.Add(Item);
          end;
        end;
      else Assert(False);
    end;
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
  AddItem(ColumnCenterFunction, True);
  AddItem(RowCenterFunction, True);
  AddItem(LayerCenterFunction, True);
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
  AddItem(CurrentSegmentAngleFunction, True);
  AddItem(CurrentSegmentAngleDegreesFunction, True);
  AddItem(CurrentSegmentAngleLimitedegreesFunction, True);
  AddItem(CurrentSectionIndexFunction, True);
  AddItem(FractionOfObjectLengthFunction, True);
  AddItem(NodeCountFunction, True);
  AddItem(ListRealValueFunction, True);
  AddItem(ListIntegerValueFunction, True);
  AddItem(ModflowLayerSimulatedFunction, True);
//  AddItem(ModflowHufKx, True);
  AddItem(ImportedValuesRFunction, True);
  AddItem(ImportedValuesIFunction, True);
  AddItem(ImportedValuesBFunction, True);
  AddItem(ImportedValuesTFunction, True);
  AddItem(InterpolationedValuesFunction, True);
  AddItem(VertexValuesFunction, True);
  AddItem(GridNumberFunction, True);
  AddItem(GridNameFunction, True);
  AddItem(ParentLayerFunction, True);
  AddItem(ParentRowFunction, True);
  AddItem(ParentColumnFunction, True);
end;

procedure GetCellIndicies(var Column, Row, Layer: Integer;
  Values: array of Pointer; FirstIndex: integer = 0);
begin
  // This function returns the hydraulic conductivity of a cell as calculated
  // using the method in the HUF package. At present, it accounts
  // for KDEP but not for LVDA.
  if (Length(Values) >= FirstIndex+1) and (Values[FirstIndex] <> nil) then
  begin
    Layer := PInteger(Values[FirstIndex])^ - 1;
  end
  else
  begin
    Layer := GlobalLayer - 1;
  end;
  if (Length(Values) >= FirstIndex+2) and (Values[FirstIndex+1] <> nil) then
  begin
    Row := PInteger(Values[FirstIndex+1])^ - 1;
  end
  else
  begin
    Row := GlobalRow - 1;
  end;
  if (Length(Values) >= FirstIndex+3) and (Values[FirstIndex+2] <> nil) then
  begin
    Column := PInteger(Values[FirstIndex+2])^ - 1;
  end
  else
  begin
    Column := GlobalColumn - 1;
  end;
end;

function NodeDistances(const Index: integer): double;
var
  PointIndex: integer;
  CumulativeDistance: double;
  APoint: TPoint2D;
  PriorPoint: TPoint2D;
  SectionIndex: Integer;
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
        SectionIndex := 0;
        for PointIndex := 1 to GlobalCurrentScreenObject.Count - 1 do
        begin
          APoint := GlobalCurrentScreenObject.Points[PointIndex];
          if GlobalCurrentScreenObject.SectionStart[SectionIndex] <> PointIndex then
          begin
            CumulativeDistance := CumulativeDistance
              + Sqrt(Sqr(APoint.X - PriorPoint.X)
              + Sqr(APoint.Y - PriorPoint.Y));
          end;
          FNodeDistances[PointIndex] := CumulativeDistance;
          PriorPoint := APoint;
          if GlobalCurrentScreenObject.SectionEnd[SectionIndex] = PointIndex then
          begin
            Inc(SectionIndex);
          end;
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
  const EvaluatedAt: TEvaluatedAt; Model: TBaseModel);
var
  CC2D: TPoint2D;
  CC3D: T3DRealPoint;
  LocalModel: TCustomModel;
  Node: TSutraNode2D;
  Node3D: TSutraNode3D;
  Element: TSutraElement2D;
  ECenter: TPoint2D;
  Element3D: TSutraElement3D;
begin
  GlobalColumn := Col + 1;
  GlobalRow := Row + 1;
  GlobalLayer := Layer + 1;
  GlobalEvaluatedAt := EvaluatedAt;
  UpdateCurrentModel(Model);
  LocalModel := Model as TCustomModel;

  if LocalModel.ModelSelection = msSutra22 then
  begin
    case EvaluatedAt of
      eaBlocks:
        begin
          Element := LocalModel.Mesh.Mesh2D.Elements[Col];
          ECenter := Element.Center;
          GlobalX := ECenter.X;
          GlobalY := ECenter.Y;
          if LocalModel.Mesh.MeshType in [mt2D, mtProfile] then
          begin
            GlobalZ := 0;
          end
          else
          begin
            Element3D := LocalModel.Mesh.ElementArray[Layer,Col];
            GlobalZ := Element3D.CenterElevation;
          end;
        end;
      eaNodes:
        begin
          Node := LocalModel.Mesh.Mesh2D.Nodes[Col];
          GlobalX := Node.X;
          GlobalY := Node.Y;
          if LocalModel.Mesh.MeshType in [mt2D, mtProfile] then
          begin
            GlobalZ := 0;
          end
          else
          begin
            if LocalModel.Mesh.Nodes.Count > 0 then
            begin
              Node3D :=  LocalModel.Mesh.NodeArray[Layer,Col];
              GlobalZ := Node3D.Z;
            end
            else
            begin
              GlobalZ := 0;
            end;
          end;
        end;
      else Assert(False);
    end;
    GlobalXPrime := GlobalX;
    GlobalYPrime := GlobalY;
  end
  else
  begin
    case EvaluatedAt of
      eaBlocks:
        begin
          CC2D := LocalModel.TwoDElementCenter(Col, Row);
          GlobalX := CC2D.X;
          GlobalY := CC2D.Y;

          CC3D := LocalModel.Grid.ThreeDElementCenter(Col, Row, Layer);
          GlobalZ := CC3D.Z;
          GlobalXPrime := CC3D.X;
          GlobalYPrime := CC3D.Y;
        end;
      eaNodes:
        begin
          CC2D := LocalModel.Grid.TwoDElementCorner(Col, Row);
          GlobalX := CC2D.X;
          GlobalY := CC2D.Y;

          CC3D := LocalModel.Grid.ThreeDElementCorner(Col, Row, Layer);
          GlobalZ := CC3D.Z;
          GlobalXPrime := CC3D.X;
          GlobalYPrime := CC3D.Y;
        end;
    else
      Assert(False);
    end;
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

procedure UpdateCurrentModel(const AModel: TBaseModel);
begin
  Assert(AModel is TCustomModel);
  GlobalCurrentModel := AModel;
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
          if GlobalCurrentSegment.EndPosition = epLast then
          begin
            result := GlobalCurrentSegment.X2;
          end
          else
          begin
            result := GlobalCurrentSegment.X1;
          end;
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
          if GlobalCurrentSegment.EndPosition = epLast then
          begin
            result := GlobalCurrentSegment.Y2;
          end
          else
          begin
            result := GlobalCurrentSegment.Y1;
          end;
        end;
      vdFront:
        begin
          Result:= 0;
        end;
      vdside:
        begin
          if GlobalCurrentSegment.EndPosition = epLast then
          begin
            result := GlobalCurrentSegment.X2;
          end
          else
          begin
            result := GlobalCurrentSegment.X1;
          end;
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
      vdFront:
        begin
          if GlobalCurrentSegment.EndPosition = epLast then
          begin
            result := GlobalCurrentSegment.Y2;
          end
          else
          begin
            result := GlobalCurrentSegment.Y1;
          end;
        end;
      vdSide:
        begin
          if GlobalCurrentSegment.EndPosition = epLast then
          begin
            result := GlobalCurrentSegment.X2;
          end
          else
          begin
            result := GlobalCurrentSegment.X1;
          end;
//          result := GlobalCurrentSegment.Y1;
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

function _CurrentSegmentAngle(Values: array of pointer): double;
var
  Point1, Point2: TPoint2D;
  LocalGrid : TCustomModelGrid;
begin
  if (GlobalCurrentSegment = nil) or (GlobalCurrentScreenObject = nil)
    or (GlobalCurrentScreenObject.Count <= 1) then
  begin
    result := 0;
  end
  else
  begin
    result := 0;
    if (GlobalCurrentSegment.X1 = GlobalCurrentSegment.X2)
      and (GlobalCurrentSegment.Y1 = GlobalCurrentSegment.Y2) then
    begin
      Exit;
    end;

    Point1 := GlobalCurrentSegment.StartPoint;
    Point2 := GlobalCurrentSegment.EndPoint;

//    Point1 := GlobalCurrentScreenObject.Points[GlobalCurrentSegment.VertexIndex];
//    Point2 := GlobalCurrentScreenObject.Points[
//      GlobalCurrentSegment.VertexIndex + 1];

    case GlobalCurrentScreenObject.ViewDirection of
      vdTop, vdFront:
        begin
          result := ArcTan2(Point2.y - Point1.y, Point2.x - Point1.x);
        end;
      vdSide:
        begin
          result := ArcTan2(Point2.x - Point1.x, Point2.y - Point1.y);
        end;
      else
        Assert(False);
    end;

    if GlobalCurrentScreenObject.ViewDirection = vdTop then
    begin
      LocalGrid := (GlobalCurrentModel as TCustomModel).Grid;
      if LocalGrid <> nil then
      begin
        result := result - LocalGrid.GridAngle;
      end;
    end;
  end;
end;

function _CurrentSegmentAngleDegrees(Values: array of pointer): double;
begin
  result := _CurrentSegmentAngle(Values)*180/Pi;
end;

function _CurrentSegmentAngleLimitedDegrees(Values: array of pointer): double;
begin
  result := _CurrentSegmentAngleDegrees(Values);
  while result > 90 do
  begin
    result := result -180;
  end;
  while result < -90 do
  begin
    result := result +180;
  end;
end;

function _CurrentSegmentLength(Values: array of pointer): double;
var
  Point1, Point2: TPoint2D;
begin
  result := 0;
  if (GlobalCurrentSegment = nil) or (GlobalCurrentScreenObject = nil)
    or (GlobalCurrentScreenObject.Count <= 1) then
  begin
    Exit;
  end
  else
  begin
    if GlobalCurrentScreenObject.SectionLength[
      GlobalCurrentSegment.SectionIndex] = 1 then
    begin
      Exit;
    end;
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

function _VertexValue(Values: array of pointer): double;
var
  VertexValueName: string;
  DefaultValue: double;
  LocalPPV: TPointPositionValues;
  Index: Integer;
  Item: TPointValuesItem;
  VVIndex: Integer;
  AValue: Double;
  Point1: TPoint2D;
  CurrentSegmentStartPoint: TPoint2D;
  LocalEpsilon: double;
  Point2: TPoint2D;
  CurrentSegmentEndPoint: TPoint2D;
  SegmentPosition: Integer;
  SegmentStart: Integer;
  Segments: TCellElementSegmentList;
  ASegment: TCellElementSegment;
  SegmentEnd: Integer;
  SegmentIndex: Integer;
  function NearlyTheSame(const A, B: real): boolean;
  begin
    result := A = B;
    if not result then
    begin
      result := Abs(A - B) < LocalEpsilon;
    end;
  end;
begin
  Assert(Length(Values) >= 2);
  VertexValueName := PString(Values[0])^;
  DefaultValue := PDouble(Values[1])^;
  result := DefaultValue;
  if (GlobalCurrentSegment = nil) or (GlobalCurrentScreenObject = nil)
    or (GlobalCurrentScreenObject.PointPositionValues = nil) then
  begin
    Exit;
  end
  else
  begin
    Segments := GlobalCurrentScreenObject.Segments[GlobalCurrentModel];
    SegmentPosition := -1;
    for SegmentIndex := 0 to Segments.Count - 1 do
    begin
      ASegment := Segments[SegmentIndex];
      if GlobalCurrentSegment.IsSame(ASegment) then
      begin
        SegmentPosition := SegmentIndex;
        Break;
      end;
    end;
//    SegmentPosition := Segments.IndexOf(GlobalCurrentSegment);
    Assert(SegmentPosition >= 0);
    SegmentStart := SegmentPosition;
    for SegmentIndex := SegmentPosition-1 downto 0 do
    begin
      ASegment := Segments[SegmentIndex];
      if (ASegment.Col = GlobalCurrentSegment.Col)
        and (ASegment.Row = GlobalCurrentSegment.Row)
        and (ASegment.Layer = GlobalCurrentSegment.Layer)
        then
      begin
        SegmentStart := SegmentIndex;
      end
      else
      begin
        Break;
      end;
    end;

    SegmentEnd := SegmentPosition;
    for SegmentIndex := SegmentPosition +1 to Segments.Count - 1 do
    begin
      ASegment := Segments[SegmentIndex];
      if (ASegment.Col = GlobalCurrentSegment.Col)
        and (ASegment.Row = GlobalCurrentSegment.Row)
        and (ASegment.Layer = GlobalCurrentSegment.Layer)
        then
      begin
        SegmentEnd := SegmentIndex;
      end
      else
      begin
        Break;
      end;
    end;

    LocalEpsilon := GlobalCurrentSegment.SegmentLength/100000;
    LocalPPV := GlobalCurrentScreenObject.PointPositionValues;
    for Index := LocalPPV.Count - 1 downto 0 do
    begin
      Item := LocalPPV.Items[Index] as TPointValuesItem;
      VVIndex := Item.IndexOfName(VertexValueName);
      if VVIndex >= 0 then
      begin
        AValue := Item.Value[VVIndex];
        for SegmentIndex := SegmentEnd downto SegmentStart do
        begin
          ASegment := Segments[SegmentIndex];
          if Item.Position = ASegment.VertexIndex+1 then
          begin
            Point2 := GlobalCurrentScreenObject.Points[
              ASegment.VertexIndex+1];
            CurrentSegmentEndPoint.X := ASegment.X2;
            CurrentSegmentEndPoint.Y := ASegment.Y2;
            if frmGoPhast.Grid <> nil then
            begin
              CurrentSegmentEndPoint := frmGoPhast.Grid.
                RotateFromGridCoordinatesToRealWorldCoordinates(CurrentSegmentEndPoint);
            end;
            if NearlyTheSame(CurrentSegmentEndPoint.X, Point2.X)
              and NearlyTheSame(CurrentSegmentEndPoint.Y, Point2.Y) then
            begin
               result := AValue;
               Exit;
            end;
          end;
        end;

        for SegmentIndex := SegmentEnd downto SegmentStart do
        begin
          ASegment := Segments[SegmentIndex];
          if Item.Position = ASegment.VertexIndex then
          begin
            Point1 := GlobalCurrentScreenObject.Points[
              ASegment.VertexIndex];
            CurrentSegmentStartPoint.X := ASegment.X1;
            CurrentSegmentStartPoint.Y := ASegment.Y1;
            if frmGoPhast.Grid <> nil then
            begin
              CurrentSegmentStartPoint := frmGoPhast.Grid.
                RotateFromGridCoordinatesToRealWorldCoordinates(CurrentSegmentStartPoint);
            end;
            if NearlyTheSame(CurrentSegmentStartPoint.X, Point1.X)
              and NearlyTheSame(CurrentSegmentStartPoint.Y, Point1.Y) then
            begin
              result := AValue;
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function _InterpolatedVertexValues(Values: array of pointer): double;
var
  VertexValueName: string;
  LocalPPV: TPointPositionValues;
  Index: Integer;
  Item: TPointValuesItem;
  VVIndex: Integer;
  AValue: Double;
  BeforeValue: Double;
  AfterValue: Double;
  BeforePosition: Integer;
  AfterPosition: Integer;
  Distance1: Double;
  Distance2: Double;
  Point1, Point2: TPoint2D;
  SegmentDistance: double;
  CurrentSegmentStartPoint: TPoint2D;
  CurrentSegmentEndPoint: TPoint2D;
  LocalEpsilon: double;
  function NearlyTheSame(const A, B: real): boolean;
  begin
    result := A = B;
    if not result then
    begin
      result := Abs(A - B) < LocalEpsilon;
    end;
  end;
begin
  if (GlobalCurrentSegment = nil) or (GlobalCurrentScreenObject = nil)
    or (GlobalCurrentScreenObject.PointPositionValues = nil) then
  begin
    result := 0;
    if  (GlobalCurrentScreenObject = nil) then
    begin
      frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
        Format(StrTheSFunctionCan,
        [ StrInterpolatedVertexValues]), '');
      frmErrorsAndWarnings.Show;
    end
    else if (GlobalCurrentSegment = nil) then
    begin
      frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,Format(
        StrTheSFunctionDoes, [StrInterpolatedVertexValues]),
        GlobalCurrentScreenObject.Name, GlobalCurrentScreenObject);
      frmErrorsAndWarnings.Show;
    end
    else
    begin
      frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
        Format(StrThe0sFunctionIs, [StrInterpolatedVertexValues]),
        Format(StrObject0sInvali, [GlobalCurrentScreenObject.Name,
        VertexValueName]), GlobalCurrentScreenObject);
      frmErrorsAndWarnings.Show;
    end;
  end
  else
  begin
    Assert(Length(Values) >= 1);
    VertexValueName := PString(Values[0])^;
    LocalPPV := GlobalCurrentScreenObject.PointPositionValues;

    BeforeValue := 0;
    BeforePosition := -1;
    AfterValue := 0;
    AfterPosition := -1;

    for Index := 0 to LocalPPV.Count - 1 do
    begin
      Item := LocalPPV.Items[Index] as TPointValuesItem;
      VVIndex := Item.IndexOfName(VertexValueName);
      if VVIndex >= 0 then
      begin
        AValue := Item.Value[VVIndex];
        if Item.Position <= GlobalCurrentSegment.VertexIndex then
        begin
          BeforeValue := AValue;
          BeforePosition := Item.Position;
        end
        else
        begin
          AfterValue := AValue;
          AfterPosition := Item.Position;
          break;
        end;
      end;
    end;

    if BeforePosition = -1 then
    begin
      if AfterPosition = -1 then
      begin
        result := 0;
        frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
          Format(StrInvalidKeyIn0s, [StrInterpolatedVertexValues]),
          Format(StrObject0sInvali, [GlobalCurrentScreenObject.Name,
          VertexValueName]), GlobalCurrentScreenObject);
        frmErrorsAndWarnings.Show;
      end
      else
      begin
        result := AfterValue;
      end;
    end
    else
    begin
      LocalEpsilon := GlobalCurrentSegment.SegmentLength/100000;
      if AfterPosition = -1 then
      begin
        result := BeforeValue;
      end
      else
      begin
        Point1 := GlobalCurrentScreenObject.Points[
          GlobalCurrentSegment.VertexIndex];
        CurrentSegmentStartPoint.X := GlobalCurrentSegment.X1;
        CurrentSegmentStartPoint.Y := GlobalCurrentSegment.Y1;
        if frmGoPhast.Grid <> nil then
        begin
          CurrentSegmentStartPoint := frmGoPhast.Grid.
            RotateFromGridCoordinatesToRealWorldCoordinates(CurrentSegmentStartPoint);
        end;
        if (BeforePosition = GlobalCurrentSegment.VertexIndex)
          and NearlyTheSame(CurrentSegmentStartPoint.X, Point1.X)
          and NearlyTheSame(CurrentSegmentStartPoint.Y, Point1.Y) then
        begin
          result := BeforeValue;
          Exit;
        end;

        Point2 := GlobalCurrentScreenObject.Points[
          GlobalCurrentSegment.VertexIndex+1];
        CurrentSegmentEndPoint.X := GlobalCurrentSegment.X2;
        CurrentSegmentEndPoint.Y := GlobalCurrentSegment.Y2;
        if frmGoPhast.Grid <> nil then
        begin
          CurrentSegmentEndPoint := frmGoPhast.Grid.
            RotateFromGridCoordinatesToRealWorldCoordinates(CurrentSegmentEndPoint);
        end;
        if (AfterPosition = GlobalCurrentSegment.VertexIndex+1)
          and NearlyTheSame(CurrentSegmentEndPoint.X, Point2.X)
          and NearlyTheSame(CurrentSegmentEndPoint.Y, Point2.Y) then
        begin
           result := AfterValue;
           Exit;
        end;

        Distance1 := NodeDistances(BeforePosition);
        Distance2 := NodeDistances(AfterPosition);
        if Distance1 = Distance2 then
        begin
          result := (BeforeValue + AfterValue)/2;
        end
        else
        begin
          Point2.X := (CurrentSegmentStartPoint.X + CurrentSegmentEndPoint.X) / 2;
          Point2.Y := (CurrentSegmentStartPoint.Y + CurrentSegmentEndPoint.Y) / 2;
          SegmentDistance := (Sqrt(Sqr(Point1.X - Point2.X) + Sqr(Point1.Y - Point2.Y))
            + NodeDistances(GlobalCurrentSegment.VertexIndex));
          result := (SegmentDistance - Distance1)/(Distance2 - Distance1)
            * (AfterValue - BeforeValue) + BeforeValue;
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

function _ObjectNodeCount(Values: array of pointer): integer;
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

function RotatedSutraLocation: TPoint2D;
var
  Temp: TPoint2D;
  SutraAngle: Double;
begin
    Result.X := GlobalX;
    Result.Y := GlobalY;
    SutraAngle := GlobalCurrentScreenObject.SutraAngle;
    temp.X := Cos(-SutraAngle) * Result.X - Sin(-SutraAngle) * Result.Y;
    temp.Y := Sin(-SutraAngle) * Result.X + Cos(-SutraAngle) * Result.Y;
    Result := temp;
end;

function _XPrime(Values: array of pointer): double;
begin
  if (frmGoPhast.ModelSelection = msSutra22)
    and (GlobalCurrentScreenObject <> nil)
    and (GlobalCurrentScreenObject.ViewDirection = vdFront)
    and (GlobalCurrentScreenObject.SutraAngle <> 0)
    then
  begin
    result := RotatedSutraLocation.X;
  end
  else
  begin
    Result := GlobalXPrime;
  end;
end;

function _YPrime(Values: array of pointer): double;
begin
  if (frmGoPhast.ModelSelection = msSutra22)
    and (GlobalCurrentScreenObject <> nil)
    and (GlobalCurrentScreenObject.ViewDirection = vdFront)
    and (GlobalCurrentScreenObject.SutraAngle <> 0)
    then
  begin
    result := RotatedSutraLocation.Y;
  end
  else
  begin
    Result := GlobalYPrime;
  end;
end;

function _Column(Values: array of pointer): integer;
begin
  Result := GlobalColumn;
end;

function _Row(Values: array of pointer): integer;
begin
  Result := GlobalRow;
end;

function _ColumnCenter(Values: array of pointer): double;
var
  Col: Integer;
  LocalGrid : TCustomModelGrid;
begin
  if Values[0] <> nil then
  begin
    Col := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Col := GlobalColumn - 1;
  end;
  LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
  if (Col < 0) or (LocalGrid = nil) or (Col > LocalGrid.ColumnCount-1) then
  begin
    result := 0;
  end
  else
  begin
    result := LocalGrid.ColumnCenter(Col);
  end;
end;

function _RowCenter(Values: array of pointer): double;
var
  Row: Integer;
  LocalGrid : TCustomModelGrid;
begin
  if Values[0] <> nil then
  begin
    Row := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Row := GlobalRow - 1;
  end;
  LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
  if (Row < 0) or (LocalGrid = nil) or (Row > LocalGrid.RowCount-1) then
  begin
    result := 0;
  end
  else
  begin
    result := LocalGrid.RowCenter(Row);
  end;
end;

function _ElevationToLayer(Values: array of pointer): integer;
var
  Elevation: double;
  Mesh: TSutraMesh3D;
  LayerIndex: Integer;
  Element: TSutraElement3D;
  Node: TSutraNode3D;
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
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp:
      begin
        Assert(GlobalEvaluatedAt = eaBlocks);
        result := TCustomModel(GlobalCurrentModel).ModflowGrid.
          GetContainingLayer(GlobalColumn-1, GlobalRow-1, Elevation);
      end;
    msSutra22:
      begin
        Mesh := TCustomModel(GlobalCurrentModel).Mesh;
        Result := 0;
        if (Mesh = nil) or (Mesh.MeshType in [mt2D, mtProfile]) then
        begin
          Exit;
        end;
        case GlobalEvaluatedAt of
          eaBlocks:
            begin
              for LayerIndex := 0 to Mesh.LayerCount - 1 do
              begin
                Element:= Mesh.ElementArray[LayerIndex, GlobalColumn-1];
                if Element.Active
                  and (Elevation <= Element.UpperElevation)
                  and (Elevation >= Element.LowerElevation) then
                begin
                  result := LayerIndex+1;
                  Exit;
                end;
              end;
            end;
          eaNodes:
            begin
              for LayerIndex := 0 to Mesh.LayerCount do
              begin
                Node := Mesh.NodeArray[LayerIndex, GlobalColumn-1];
                if Node.Active
                  and (Elevation <= Node.Top)
                  and (Elevation >= Node.Bottom) then
                begin
                  result := LayerIndex+1;
                  Exit;
                end;
              end;
            end;
          else
            Assert(False);
        end;
      end;
    else Assert(False);
  end;
  Inc(result);
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
//                result := frmGoPhast.PhastModel.LayerStructure.
//                  DataSetLayerToModflowLayer(result);
              end;
            end;
          else Assert(False);
        end;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp:
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
    msSutra22:
      begin
        Result := _ElevationToLayer(Values);
      end;
    else Assert(False);
  end;
end;

function _Layer(Values: array of pointer): integer;
begin
  Result := GlobalLayer;
end;

function GetColumnWidth(Column: Integer): Double;
var
  LocalGrid : TCustomModelGrid;
begin
  if frmGoPhast.ModelSelection = msSutra22 then
  begin
    result := 0;
    Exit;
  end;
  LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
  case GlobalEvaluatedAt of
    eaBlocks:
      begin
        if (Column < 0) or (Column >= LocalGrid.ColumnCount) then
        begin
          Result := 0;
        end
        else
        begin
          Result := LocalGrid.ColumnWidth[Column];
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
            result := (frmGoPhast.PhastGrid.ColumnPosition[Column + 1]
              - frmGoPhast.PhastGrid.ColumnPosition[Column - 1]) / 2;
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
var
  LocalGrid : TCustomModelGrid;
begin
  if frmGoPhast.ModelSelection = msSutra22 then
  begin
    result := 0;
    Exit;
  end;
  LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
  case GlobalEvaluatedAt of
    eaBlocks:
      begin
        if (Row < 0) or (Row >= LocalGrid.RowCount) then
        begin
          Result := 0;
        end
        else
        begin
          Result := LocalGrid.RowWidth[Row];
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
            result := (frmGoPhast.PhastGrid.RowPosition[Row + 1]
              - frmGoPhast.PhastGrid.RowPosition[Row - 1]) / 2;
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
  Result := True;
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if Values[0] <> nil then
  begin
    Layer := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Layer := GlobalLayer - 1;
  end;
  result := frmGoPhast.PhastModel.IsLayerSimulated(Layer);
end;

procedure ExtractColRowLayer(var Lay, Row, Col: Integer;
  Values: array of Pointer);
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
var
  Mesh: TSutraMesh3D;
  Element: TSutraElement3D;
  Node: TSutraNode3D;
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
                  result := (frmGoPhast.PhastGrid.LayerElevation[Lay + 1]
                    - frmGoPhast.PhastGrid.LayerElevation[Lay - 1]) / 2;
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
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp:
      begin
        if (Lay < 0) or (Lay > TCustomModel(GlobalCurrentModel).ModflowGrid.LayerCount - 1)
          or (Row < 0) or (Row > TCustomModel(GlobalCurrentModel).ModflowGrid.RowCount - 1)
          or (Col < 0) or (Col > TCustomModel(GlobalCurrentModel).ModflowGrid.ColumnCount - 1) then
        begin
          Result := 0;
        end
        else
        begin
          Result := TCustomModel(GlobalCurrentModel).ModflowGrid.CellThickness[Col, Row, Lay];
        end;
      end;
    msSutra22:
      begin
        Result := 0;
        Mesh := TCustomModel(GlobalCurrentModel).Mesh;
        if Mesh.MeshType in [mt2D, mtProfile] then
        begin
          Exit;
        end;
        case GlobalEvaluatedAt of
          eaBlocks:
            begin
              Element := Mesh.ElementArray[Lay, Col];
              Result := Element.UpperElevation - Element.LowerElevation;
            end;
          eaNodes:
            begin
              Node := Mesh.NodeArray[Lay, Col];
              Result := Node.Top - Node.Bottom;
            end;
          else
            Assert(False);
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
var
  Column: Integer;
  Mesh: TSutraMesh3D;
begin
  result := 0;
  case frmGoPhast.ModelSelection of
    msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp:
      begin
        result := _ColumnWidth([Values[0]]) * _RowWidth([Values[1]]);
      end;
    msSutra22:
      begin
        if (frmGoPhast.PhastModel <> nil)
          and (frmGoPhast.PhastModel.Mesh <> nil) then
        begin
          Mesh := frmGoPhast.PhastModel.Mesh;
        end
        else
        begin
          Exit;
        end;
        if Values[0] <> nil then
        begin
          Column := PInteger(Values[0])^ - 1;
        end
        else
        begin
          Column := GlobalColumn - 1;
        end;
        if Mesh.Mesh2D.Nodes.Count = 0 then
        begin
          Exit;
        end;
        case GlobalEvaluatedAt of
          eaBlocks:
            begin
              result := Mesh.Mesh2D.Elements[Column].ElementArea
            end;
          eaNodes:
            begin
              result := Mesh.Mesh2D.Nodes[Column].CellArea
            end;
          else
            Assert(False);
        end;
      end;
    else
      Assert(False);
  end
end;

function _BlockAreaFront(Values: array of pointer): double;
var
  Col, Row, Layer: integer;
  CellPoints: T2DRealPointArray;
  CellOutline: TPolygon2D;
  Mesh: TSutraMesh3D;
  Limits: TLimitsArray;
  Polygons: TCellElementPolygons2D;
  Angle: Extended;
begin
  case frmGoPhast.ModelSelection of
    msPhast:
      begin
        if Values[2] = nil then
        begin
          result := _ColumnWidth([Values[0]]) * _LayerHeight([Values[1]]);
        end
        else
        begin
          result := _ColumnWidth([Values[0]]) * _LayerHeight([Values[2]]);
        end;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp:
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
        CellPoints := TCustomModel(GlobalCurrentModel).ModflowGrid.FrontCellPoints(Row);
        SetLength(CellOutline, 6);
        CellOutline[5] := CellPoints[Col*2,Layer];
        CellOutline[4] := CellPoints[Col*2+1,Layer];
        CellOutline[3] := CellPoints[Col*2+2,Layer];
        CellOutline[2] := CellPoints[Col*2+2,Layer+1];
        CellOutline[1] := CellPoints[Col*2+1,Layer+1];
        CellOutline[0] := CellPoints[Col*2,Layer+1];
        result := Area(CellOutline);
      end;
    msSutra22:
      begin
        result := 0;
        Mesh := frmGoPhast.PhastModel.Mesh;
        if Mesh = nil then
        begin
          Exit;
        end;
        if Mesh.MeshType in [mt2D, mtProfile] then
        begin
          Exit;
        end;
        if Values[2] = nil then
        begin
//          Row := GlobalRow - 1;

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
//          Row := PInteger(Values[1])^ - 1;
          Layer := PInteger(Values[2])^ - 1;
        end;

        if GlobalCurrentScreenObject = nil then
        begin
          Angle := 0.;
        end
        else
        begin
          Angle := GlobalCurrentScreenObject.SutraAngle;
        end;

        Polygons := Mesh.FrontPolygons(Angle, GlobalEvaluatedAt, Limits);
        result := Abs(Area(Polygons[Layer, Col]));

      end
    else
      begin
        result := 0;
        Assert(False);
      end;
  end;
end;

function _BlockAreaSide(Values: array of pointer): double;
var
  Col, Row, Layer: integer;
  CellPoints: T2DRealPointArray;
  CellOutline: TPolygon2D;
begin
  case frmGoPhast.ModelSelection of
    msPhast:
      begin
        if Values[2] = nil then
        begin
          result := _RowWidth([Values[0]]) * _LayerHeight([Values[1]]);
        end
        else
        begin
          result := _RowWidth([Values[0]]) * _LayerHeight([Values[2]]);
        end;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp:
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
        CellPoints := TCustomModel(GlobalCurrentModel).ModflowGrid.SideCellPoints(Col);
        SetLength(CellOutline, 6);
        CellOutline[0] := CellPoints[Row*2,Layer];
        CellOutline[1] := CellPoints[Row*2+1,Layer];
        CellOutline[2] := CellPoints[Row*2+2,Layer];
        CellOutline[3] := CellPoints[Row*2+2,Layer+1];
        CellOutline[4] := CellPoints[Row*2+1,Layer+1];
        CellOutline[5] := CellPoints[Row*2,Layer+1];
        result := Area(CellOutline);
      end;
    msSutra22:
      begin
        result := 0;
      end
    else
      begin
        result := 0;
        Assert(False);
      end;
  end;
end;

function _BlockVolume(Values: array of pointer): double;
var
  Col: Integer;
  Row: Integer;
  Lay: Integer;
  Mesh: TSutraMesh3D;
  AnElement: TSutraElement3D;
  AnNode: TSutraNode3D;
begin
  result := 0;
  ExtractColRowLayer(Lay, Row, Col, Values);
  case frmGoPhast.ModelSelection of
    msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp:
      begin
        result := GetColumnWidth(Col) * GetRowWidth(Row) *
          GetLayerHeight(Col, Row, Lay);
      end;
    msSutra22:
      begin
        result := 0;
        Mesh := frmGoPhast.PhastModel.Mesh;
        if Mesh = nil then
        begin
          Exit;
        end;
        case Mesh.MeshType of
          mt2D, mtProfile: Exit;
          mt3D:
            begin
              case GlobalEvaluatedAt of
                eaBlocks:
                  begin
                    AnElement := Mesh.ElementArray[Lay,Col];
                    result := AnElement.Volume;
                  end;
                eaNodes:
                  begin
                    AnNode := Mesh.NodeArray[Lay,Col];
                    result := AnNode.Volume;
                  end;
                else
                  Assert(False);
              end;
            end;
          else
            Assert(False);
        end;
      end;
    else
      Assert(False);
  end;
end;

function _ColumnPosition(Values: array of pointer): double;
var
  Column: integer;
  LocalGrid : TCustomModelGrid;
begin
  if frmGoPhast.ModelSelection = msSutra22 then
  begin
    result := 0;
    Exit;
  end;
  LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
  if Values[0] <> nil then
  begin
    Column := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Column := GlobalColumn - 1;
  end;
  if (Column < 0) or (Column > LocalGrid.ColumnCount) then
  begin
    Result := 0;
  end
  else
  begin
    Result := LocalGrid.ColumnPosition[Column];
  end;
end;

function _RowPosition(Values: array of pointer): double;
var
  Row: integer;
  LocalGrid : TCustomModelGrid;
begin
  if frmGoPhast.ModelSelection = msSutra22 then
  begin
    result := 0;
    Exit;
  end;
  LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
  if Values[0] <> nil then
  begin
    Row := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Row := GlobalRow - 1;
  end;
  if (Row < 0) or (Row > LocalGrid.RowCount) then
  begin
    Result := 0;
  end
  else
  begin
    Result := LocalGrid.RowPosition[Row];
  end;
end;

function GetLayerPosition(const Lay, Row, Col: Integer;
  var InvalidIndex: boolean): Double;
var
  LocalGrid: TModflowGrid;
  Mesh: TSutraMesh3D;
  Element: TSutraElement3D;
  Node: TSutraNode3D;
begin
  result := 0;
  InvalidIndex := False;
  case frmGoPhast.ModelSelection of
    msPhast:
      begin
        if (Lay < 0) or (Lay > frmGoPhast.PhastGrid.LayerCount) then
        begin
          Result := 0;
          InvalidIndex := True;
        end
        else
        begin
          Result := frmGoPhast.PhastGrid.LayerElevation[Lay];
        end;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp:
      begin
        LocalGrid := TCustomModel(GlobalCurrentModel).ModflowGrid;
        LocalGrid.UpdateCellElevations;
        if (Lay < 0) or (Lay > LocalGrid.LayerCount)
         or (Row < 0) or (Row > LocalGrid.RowCount-1)
         or (Col < 0) or (Col > LocalGrid.ColumnCount-1) then
        begin
          Result := 0;
          InvalidIndex := True;
        end
        else
        begin
          Result := LocalGrid.CellElevation[Col, Row, Lay];
        end;
      end;
    msSutra22:
      begin
        Mesh := TCustomModel(GlobalCurrentModel).Mesh;
        if Mesh.MeshType in [mt2D, mtProfile] then
        begin
          result := 0;
          Exit;
        end;
        case GlobalEvaluatedAt of
          eaBlocks:
            begin
              if (Lay < 0) or (Lay > Mesh.LayerCount)
               or (Row <> 0)
               or (Col < 0) or (Col >= Mesh.Mesh2D.Elements.Count) then
              begin
                Result := 0;
                InvalidIndex := True;
              end
              else
              begin
                if Lay = Mesh.LayerCount then
                begin
                  Element := Mesh.ElementArray[Lay-1, Col];
                  result := Element.LowerElevation;
                end
                else
                begin
                  Element := Mesh.ElementArray[Lay, Col];
                  result := Element.UpperElevation;
                end;
              end;
            end;
          eaNodes:
            begin
              if (Lay < 0) or (Lay > Mesh.LayerCount)
               or (Row <> 0)
               or (Col < 0) or (Col >= Mesh.Mesh2D.Nodes.Count) then
              begin
                Result := 0;
                InvalidIndex := True;
              end
              else
              begin
                Node := Mesh.NodeArray[Lay,Col];
                Result := Node.Z;
              end;
            end;
          else Assert(False);
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
  DummyInvalidIndex: boolean;
begin
  ExtractColRowLayer(Lay, Row, Col, Values);
  Result := GetLayerPosition(Lay, Row, Col, DummyInvalidIndex);
end;

function GetLayerCenter(const Lay, Row, Col:  integer): double;
var
  BelowValue: Double;
  AboveValue: Double;
  InvalidIndex: boolean;
  Mesh: TSutraMesh3D;
  Element: TSutraElement3D;
  Node: TSutraNode3D;
begin
  result := 0;
  case GlobalCurrentModel.ModelSelection of
    msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp:
      begin
        BelowValue := GetLayerPosition(Lay, Row, Col, InvalidIndex);
        if InvalidIndex then
        begin
          result := 0;
        end
        else
        begin
          AboveValue := GetLayerPosition(Lay+1, Row, Col, InvalidIndex);
          if InvalidIndex then
          begin
            result := 0;
          end
          else
          begin
            result := (BelowValue + AboveValue)/2;
          end;
        end;
      end;
    msSutra22:
      begin
        Mesh := (GlobalCurrentModel as TCustomModel).Mesh;
        result := 0;
        if Mesh.MeshType in [mt2D, mtProfile] then
        begin
          Exit;
        end;
        case GlobalEvaluatedAt of
          eaBlocks:
            begin
              Element:= Mesh.ElementArray[Lay, Col];
              result := Element.CenterElevation;
            end;
          eaNodes:
            begin
              Node:= Mesh.NodeArray[Lay, Col];
              result := Node.Z;
            end;
          else
            Assert(False);
        end;
      end;
    else
      Assert(False);
  end;
end;

function _LayerCenter(Values: array of pointer): double;
var
  Col: integer;
  Row: integer;
  Lay: integer;
begin
  ExtractColRowLayer(Lay, Row, Col, Values);
  result := GetLayerCenter(Lay, Row, Col);
end;

function _LayerCount(Values: array of pointer): integer;
var
  LocalModel: TCustomModel;
  LocalGrid: TCustomModelGrid;
  LocalMesh: TSutraMesh3D;
begin
  LocalModel := TCustomModel(GlobalCurrentModel);
  LocalGrid := LocalModel.Grid;
  if LocalGrid <> nil then
  begin
    Result := LocalGrid.LayerCount + 1;
  end
  else
  begin
    LocalMesh := LocalModel.Mesh;
    if (LocalMesh = nil) or (LocalMesh.MeshType in [mt2D, mtProfile]) then
    begin
      result := 1;
    end
    else
    begin
      result := LocalMesh.LayerCount+1;
    end;
  end;
end;

function _RowCount(Values: array of pointer): integer;
var
  LocalGrid: TCustomModelGrid;
  Mesh: TSutraMesh3D;
begin
  LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
  if LocalGrid = nil then
  begin
    Mesh := TCustomModel(GlobalCurrentModel).Mesh;
    if Mesh = nil then
    begin
      result := 0;
    end
    else
    begin
      result := 1;
    end;
  end
  else
  begin
    Result := LocalGrid.RowCount + 1;
  end;
end;

function _ColumnCount(Values: array of pointer): integer;
var
  LocalGrid: TCustomModelGrid;
  Mesh: TSutraMesh3D;
begin
  LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
  if LocalGrid = nil then
  begin
    result := 0;
    Mesh := TCustomModel(GlobalCurrentModel).Mesh;
    if Mesh = nil then
    begin
      Exit;
    end
    else
    begin
      case GlobalEvaluatedAt of
        eaBlocks:
          begin
            result := Mesh.Mesh2D.Elements.Count;
          end;
        eaNodes:
          begin
            result := Mesh.Mesh2D.Nodes.Count;
          end;
      end;
    end;
  end
  else
  begin
    Result := LocalGrid.ColumnCount + 1;
  end;
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
      Layer, GlobalCurrentModel);
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
      Layer, Section, GlobalCurrentModel);
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
    result := GlobalCurrentScreenObject.ObjectIntersectArea(
      Column, Row, Layer, GlobalCurrentModel);
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
  DataArray := TCustomModel(GlobalCurrentModel).
    DataArrayManager.GetDataSetByName(DataSetName);
  Assert(DataArray <> nil);
  PushGlobalStack;
  try
    DataArray.Initialize;
  finally
    PopGlobalStack;
  end;
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
  Column: Integer;
  Mesh: TSutraMesh3D;
begin
  result := False;
  if TCustomModel(GlobalCurrentModel).Grid <> nil then
  begin
    DataArray := TCustomModel(GlobalCurrentModel).
      DataArrayManager.GetDataSetByName(rsActive);
    Assert(DataArray <> nil);
    PushGlobalStack;
    try
      DataArray.Initialize;
    finally
      PopGlobalStack;
    end;
    ArrayLength := Length(Values);
    Assert(ArrayLength = 1);
    Layer := PInteger(Values[0])^ - 1;
    if Layer >= DataArray.LayerCount then
    begin
      result := False;
      Exit;
    end;
    result := EvaluateBooleanDataSetOnLayer(rsActive, Values);
  end
  else if TCustomModel(GlobalCurrentModel).Mesh <> nil then
  begin
    Mesh := TCustomModel(GlobalCurrentModel).Mesh;
    if Mesh = nil then
    begin
      result := False;
      Exit;
    end;
    ArrayLength := Length(Values);
    Assert(ArrayLength = 1);
    Layer := PInteger(Values[0])^ - 1;
    if Layer < 0 then
    begin
      result := False;
    end
    else
    begin
      if Mesh.MeshType in [mt2D, mtProfile] then
      begin
        result := Layer = 0;
        Exit;
      end;
      Column := GlobalColumn - 1;
      case GlobalEvaluatedAt of
        eaBlocks:
          begin
            if Layer < Mesh.LayerCount then
            begin
              result := Mesh.ElementArray[Layer,Column].Active;
            end
            else
            begin
              result := False;
            end;
          end;
        eaNodes:
          begin
            if Layer <= Mesh.LayerCount then
            begin
              result := Mesh.NodeArray[Layer,Column].Active;
            end
            else
            begin
              result := False;
            end;
          end;
      end;
    end;
  end;
end;

function _SpecifiedHeadOnLayer(Values: array of pointer): boolean;
begin
  result := False;
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  result := EvaluateBooleanDataSetOnLayer(rsModflowSpecifiedHead, Values);
end;

procedure GetImportedValues(var ImportedValues: TValueArrayStorage;
  const Values: array of Pointer; var ImportedName: string);
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
    ImportedName := '';
    ImportedValues := GlobalCurrentScreenObject.CurrentValues;
  end;
end;

function _ImportedScreenObjectValuesR(Values: array of pointer): double;
var
  Index: integer;
  ImportedValues: TValueArrayStorage;
  ImportedName: string;
  ErrorMessage: string;
begin
  result := 0;
  if GlobalCurrentScreenObject <> nil then
  begin
    GetImportedValues(ImportedValues, Values, ImportedName);
    if ImportedValues = nil then
    begin
      if InvalidNames.IndexOf(ImportedName) < 0 then
      begin
        InvalidNames.Add(ImportedName);
        frmErrorsAndWarnings.AddError(GlobalCurrentModel,
          Format(StrNoImportedDataExi, [GlobalCurrentScreenObject.Name]),
          ImportedName, GlobalCurrentScreenObject);
      end;
    end
    else
    begin
      Index := GlobalSection;
      if Index < 0 then
      begin
        Exit;
      end;
      if Index >= ImportedValues.Count then
      begin
        frmErrorsAndWarnings.AddError(GlobalCurrentModel,
          Format(StrProblemEvaluating, [rsObjectImportedValuesR]),
          Format(StrThereAreTooFewIm,
          [ImportedName, GlobalCurrentScreenObject.Name]),
          GlobalCurrentScreenObject);
        Exit;
      end;
      Assert(Index < ImportedValues.Count);
      if ImportedValues.DataType <> rdtDouble then
      begin
        ErrorMessage := Format(StrTheDataAccessedTh, [rsObjectImportedValuesR]);
        if ImportedName <> '' then
        begin
          ErrorMessage := ErrorMessage + ' (' + ImportedName + ')';
        end;
        raise Exception.Create(ErrorMessage);
      end;
      result := ImportedValues.RealValues[Index];
    end;
  end;
end;

function _ImportedScreenObjectValuesI(Values: array of pointer): integer;
var
  Index: integer;
  ImportedValues: TValueArrayStorage;
  ErrorMessage: string;
  ImportedName: string;
begin
  result := 0;
  if GlobalCurrentScreenObject <> nil then
  begin
    GetImportedValues(ImportedValues, Values, ImportedName);
    if ImportedValues = nil then
    begin
      if InvalidNames.IndexOf(ImportedName) < 0 then
      begin
        InvalidNames.Add(ImportedName);
        frmErrorsAndWarnings.AddError(GlobalCurrentModel,
          Format(StrNoImportedDataExi, [GlobalCurrentScreenObject.Name]),
          ImportedName, GlobalCurrentScreenObject);
      end;
    end
    else
    begin
      Index := GlobalSection;
      if Index < 0 then
      begin
        Exit;
      end;
      if Index >= ImportedValues.Count then
      begin
        frmErrorsAndWarnings.AddError(GlobalCurrentModel,
          Format(StrProblemEvaluating, [rsObjectImportedValuesI]),
          Format(StrThereAreTooFewIm,
          [ImportedName, GlobalCurrentScreenObject.Name]), GlobalCurrentScreenObject);
        Exit;
      end;
      Assert(Index < ImportedValues.Count);
      if ImportedValues.DataType <> rdtInteger then
      begin
        ErrorMessage := Format(StrTheDataAccessedThInt, [rsObjectImportedValuesI]);
        if ImportedName <> '' then
        begin
          ErrorMessage := ErrorMessage + ' (' + ImportedName + ')';
        end;
        raise Exception.Create(ErrorMessage);
      end;
      Assert(ImportedValues.DataType= rdtInteger);
      result := ImportedValues.IntValues[Index];
    end;
  end;
end;

function _ImportedScreenObjectValuesB(Values: array of pointer): boolean;
var
  Index: integer;
  ImportedValues: TValueArrayStorage;
  ErrorMessage: string;
  ImportedName: string;
begin
  result := False;
  if GlobalCurrentScreenObject <> nil then
  begin
    GetImportedValues(ImportedValues, Values, ImportedName);
    if ImportedValues = nil then
    begin
      if InvalidNames.IndexOf(ImportedName) < 0 then
      begin
        InvalidNames.Add(ImportedName);
        frmErrorsAndWarnings.AddError(GlobalCurrentModel,
          Format(StrNoImportedDataExi, [GlobalCurrentScreenObject.Name]),
          ImportedName, GlobalCurrentScreenObject);
      end;
    end
    else
    begin
      Index := GlobalSection;
      if Index < 0 then
      begin
        Exit;
      end;
      if Index >= ImportedValues.Count then
      begin
        frmErrorsAndWarnings.AddError(GlobalCurrentModel,
          Format(StrProblemEvaluating, [rsObjectImportedValuesB]),
          Format(StrThereAreTooFewIm,
          [ImportedName, GlobalCurrentScreenObject.Name]), GlobalCurrentScreenObject);
        Exit;
      end;
      Assert(Index < ImportedValues.Count);
      if ImportedValues.DataType <> rdtBoolean then
      begin
        ErrorMessage := Format(StrTheDataAccessedThBoole,
          [rsObjectImportedValuesB]);
        if ImportedName <> '' then
        begin
          ErrorMessage := ErrorMessage + ' (' + ImportedName + ')';
        end;
        raise Exception.Create(ErrorMessage);
      end;
      result := ImportedValues.BooleanValues[Index];
    end;
  end;
end;

function _ImportedScreenObjectValuesT(Values: array of pointer): string;
var
  Index: integer;
  ImportedValues: TValueArrayStorage;
  ErrorMessage: string;
  ImportedName: string;
begin
  result := '';
  if GlobalCurrentScreenObject <> nil then
  begin
    GetImportedValues(ImportedValues, Values, ImportedName);
    if ImportedValues = nil then
    begin
      if InvalidNames.IndexOf(ImportedName) < 0 then
      begin
        InvalidNames.Add(ImportedName);
        frmErrorsAndWarnings.AddError(GlobalCurrentModel,
          Format(StrNoImportedDataExi, [GlobalCurrentScreenObject.Name]),
          ImportedName, GlobalCurrentScreenObject);
      end;
    end
    else
    begin
      Index := GlobalSection;
      if Index < 0 then
      begin
        Exit;
      end;
      if Index >= ImportedValues.Count then
      begin
        frmErrorsAndWarnings.AddError(GlobalCurrentModel,
          Format(StrProblemEvaluating, [rsObjectImportedValuesT]),
          Format(StrThereAreTooFewIm,
          [ImportedName, GlobalCurrentScreenObject.Name]), GlobalCurrentScreenObject);
        Exit;
      end;
      Assert(Index < ImportedValues.Count);
      if ImportedValues.DataType <> rdtString then
      begin
        ErrorMessage := Format(StrTheDataAccessedThStr,
          [rsObjectImportedValuesT]);
        if ImportedName <> '' then
        begin
          ErrorMessage := ErrorMessage + ' (' + ImportedName + ')';
        end;
        raise Exception.Create(ErrorMessage);
      end;
      result := ImportedValues.StringValues[Index];
    end;
  end;
end;

function GetDataSetValue(Column: Integer; Row: Integer;Layer: Integer;
  const DataSetName: string): Double;
var
  DataArray: TDataArray;
  AModel: TCustomModel;
begin
  AModel := TCustomModel(GlobalCurrentModel);
  DataArray := AModel.DataArrayManager.GetDataSetByName(DataSetName);
  Assert(DataArray <> nil);
  PushGlobalStack;
  try
    DataArray.Initialize;
  finally
    PopGlobalStack;
  end;
  result := DataArray.RealData[Layer, Row, Column];
end;


procedure UpdateCellValue(var CellValue: Double;
  Param: THufUsedParameter; PhastModel: TCustomModel; Column, Row: Integer;
  var Updated: boolean);
var
  Multiplier: Double;
  ZoneArray: TDataArray;
  Parameter: TModflowParameter;
begin
  if Param.UseZone then
  begin
    ZoneArray := TCustomModel(GlobalCurrentModel).DataArrayManager.GetDataSetByName(Param.ZoneDataSetName);
    Assert(ZoneArray <> nil);
    PushGlobalStack;
    try
      ZoneArray.Initialize;
    finally
      PopGlobalStack;
    end;
    if not ZoneArray.BooleanData[0, Row, Column] then
    begin
      Exit;
    end;
  end;
  if Param.UseMultiplier then
  begin
    Multiplier := GetDataSetValue(Column, Row, 0, Param.MultiplierDataSetName);
  end
  else
  begin
    Multiplier := 1;
  end;
  Parameter := Param.Parameter;
  CellValue := CellValue + Multiplier * Parameter.Value;
  Updated := True;
end;

function _BcfGetVcont(Values: array of pointer): double;
var
  Layer: Integer;
  Row: Integer;
  Column: Integer;
  AModel: TCustomModel;
  KzDataArray: TDataArray;
  TopLayer: Integer;
  KzNonSimDataArray: TDataArray;
  BottomLayer: Integer;
  LayerIndex: Integer;
  Grid: TModflowGrid;
  LayerThickness: Double;
  VK: Double;
  ASum: double;
begin
  result := 0;
  AModel := TCustomModel(GlobalCurrentModel);
  if not (AModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  GetCellIndicies(Column, Row, Layer, Values);
  Grid := AModel.ModflowGrid;
  if (Grid = nil) or (Grid.LayerCount -1 <= Layer)
    or (Layer < 0) then
  begin
    // VCont for bottom layer is always zero.
    Exit;
  end;

  while not AModel.IsLayerSimulated(Layer)
    and (Layer >= 0) do
  begin
    Dec(Layer);
  end;

  KzNonSimDataArray := nil;
  TopLayer := Layer;
  Inc(Layer);
  if not AModel.IsLayerSimulated(Layer) then
  begin
    KzNonSimDataArray := AModel.DataArrayManager.GetDataSetByName(rsModflow_CBKz);
    Assert(KzNonSimDataArray <> nil);
    PushGlobalStack;
    try
      KzNonSimDataArray.Initialize;
    finally
      PopGlobalStack;
    end;
    Inc(Layer);
  end;
  BottomLayer := Layer;

  KzDataArray := AModel.DataArrayManager.GetDataSetByName(rsKz);
  Assert(KzDataArray <> nil);
  PushGlobalStack;
  try
    KzDataArray.Initialize;
  finally
    PopGlobalStack;
  end;

  ASum := 0.0;
  for LayerIndex := TopLayer to BottomLayer do
  begin
    LayerThickness := GetLayerHeight(Column, Row, LayerIndex);
    if LayerThickness <= 0 then
    begin
      Exit;
    end;
    if AModel.IsLayerSimulated(LayerIndex) then
    begin
      VK := KzDataArray.RealData[LayerIndex, Row, Column];
      LayerThickness := LayerThickness/2;
    end
    else
    begin
      VK := KzNonSimDataArray.RealData[LayerIndex, Row, Column];
    end;
    if VK <= 0 then
    begin
      Exit;
    end;
    ASum := ASum + LayerThickness/VK;
  end;
  if ASum > 0 then
  begin
    result := 1/ASum;
  end;

end;

function HguHorizontalAnisotrpy(HufUnit: THydrogeologicUnit; Column, Row: integer): double;
var
  HorizontalAnisotropy: double;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
  AModel: TCustomModel;
  HufThickness: Double;
  HaniUsed: boolean;
begin
  result := 1;
  AModel := TCustomModel(GlobalCurrentModel);
  HufThickness := GetDataSetValue(Column, Row, 0, HufUnit.ThickessDataArrayName);
  if HufThickness <= 0 then
  begin
    Exit;
  end;

  HaniUsed := False;
  HorizontalAnisotropy := 0.;
  for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
  begin
    Param := HufUnit.HufUsedParameters[ParamIndex];
    Parameter := Param.Parameter;
    if Parameter.ParameterType = ptHUF_HANI then
    begin
      UpdateCellValue(HorizontalAnisotropy, Param, AModel, Column, Row, HaniUsed);
    end
  end;
  if not HaniUsed then
  begin
    HorizontalAnisotropy := HufUnit.HorizontalAnisotropy;
  end;
  result := HorizontalAnisotropy;

end;

function HguTransmissivity(HufUnit: THydrogeologicUnit; Column, Row: integer;
  IntervalTop, IntervalBottom: double): double;
var
  KDEP_Used: Boolean;
  HufK: double;
  Lambda: double;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
  AModel: TCustomModel;
  KDepMult: double;
  GroundSurface: Double;
  HufTop: Double;
  DepthToTop: Double;
  HufThickness: Double;
  HufBottom: Double;
  DepthToBottom: Double;
  Kx_Used: Boolean;
begin
  result := 0;
  AModel := TCustomModel(GlobalCurrentModel);
  HufThickness := GetDataSetValue(Column, Row, 0, HufUnit.ThickessDataArrayName);
  if HufThickness <= 0 then
  begin
    Exit;
  end;

  KDEP_Used := False;
  Kx_Used := False;
  HufK := 0.;
  Lambda := 0.;
  for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
  begin
    Param := HufUnit.HufUsedParameters[ParamIndex];
    Parameter := Param.Parameter;
    if Parameter.ParameterType = ptHUF_HK then
    begin
      UpdateCellValue(HufK, Param, AModel, Column, Row, Kx_Used);
    end
    else if Parameter.ParameterType = ptHUF_KDEP then
    begin
      UpdateCellValue(Lambda, Param, AModel, Column, Row, KDEP_Used);
    end;
  end;
  if not Kx_Used then
  begin
    Exit;
  end;

  KDepMult := 1.;
  if KDEP_Used then
  begin
    if AModel.ModflowPackages.HufPackage.ReferenceChoice
      = hrcReferenceLayer then
    begin
      GroundSurface := GetDataSetValue(Column, Row, 0, StrHufReferenceSurface);
    end
    else
    begin
      GroundSurface := GetDataSetValue(Column, Row, 0, kModelTop);
    end;

    HufTop := GetDataSetValue(Column, Row, 0, HufUnit.TopDataArrayName);

    HufBottom := HufTop - HufThickness;

    if IntervalTop < HufTop then
    begin
      HufTop := IntervalTop;
    end;

    if IntervalBottom > HufBottom then
    begin
      HufBottom := IntervalBottom;
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
  result := HufK * KDepMult *HufThickness;

end;

procedure GetLayerTopAndBottom(Head: double; Layer, Row, Column: Integer;
  var LayerBottom, LayerTop: Double; UseHead: boolean = True);
var
  LayerGroup: TLayerGroup;
  PhastModel: TPhastModel;
  DummyInvalidIndex: boolean;
  GLayer: Integer;
begin
  PhastModel := frmGoPhast.PhastModel;
  if GlobalCurrentModel <> PhastModel then
  begin
    GLayer := (GlobalCurrentModel as TChildModel).ChildLayerToParentLayer(Layer);
  end
  else
  begin
    GLayer := Layer;
  end;
  LayerTop := GetLayerPosition(Layer, Row, Column, DummyInvalidIndex);
  LayerBottom := GetLayerPosition(Layer + 1, Row, Column, DummyInvalidIndex);
  LayerGroup := PhastModel.LayerStructure.GetLayerGroupByLayer(GLayer);
  if (LayerGroup.AquiferType <> 0) and UseHead then
  begin
    if Head < LayerTop then
    begin
      LayerTop := Head;
    end;
  end;
end;

function HufUnitParam(HufUnit: THydrogeologicUnit; Column, Row: integer;
  Paramtype: TParameterType; var Updated: boolean): double;
var
  CellValue: double;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
  AModel: TCustomModel;
  HufThickness: Double;
begin
  result := 0;
  AModel := TCustomModel(GlobalCurrentModel);
  HufThickness := GetDataSetValue(Column, Row, 0, HufUnit.ThickessDataArrayName);
  if HufThickness <= 0 then
  begin
    Exit;
  end;

  CellValue := 0.;
  for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
  begin
    Param := HufUnit.HufUsedParameters[ParamIndex];
    Parameter := Param.Parameter;
    if Parameter.ParameterType = Paramtype then
    begin
      UpdateCellValue(CellValue, Param, AModel, Column, Row, Updated);
    end
  end;

  // result will be divided by total cell thickness later.
  result := CellValue //* HufThickness;
end;

function _GetHufSytp(Values: array of pointer): double;
var
  Column: Integer;
  Row: Integer;
  Index: Integer;
  SteadyParameters: TModflowSteadyParameters;
  AParam: TModflowSteadyParameter;
  DataArrayManager: TDataArrayManager;
  ZoneDataArray: TDataArray;
  AValue: Double;
  MultiplierDataArray: TDataArray;
begin
  result := 0;
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if (Length(Values) >= 1) and (Values[0] <> nil) then
  begin
    Row := PInteger(Values[0])^ - 1;
  end
  else
  begin
    Row := GlobalRow - 1;
  end;
  if (Length(Values) >= 2) and (Values[1] <> nil) then
  begin
    Column := PInteger(Values[1])^ - 1;
  end
  else
  begin
    Column := GlobalColumn - 1;
  end;
//  GetCellIndicies(Dummy, Column, Row,  Values);
  SteadyParameters := frmGoPhast.PhastModel.ModflowSteadyParameters;
  DataArrayManager := TCustomModel(GlobalCurrentModel).DataArrayManager;
  for Index := 0 to SteadyParameters.Count - 1 do
  begin
    AParam := SteadyParameters[Index];
    if AParam.ParameterType = ptHUF_SYTP then
    begin
      if AParam.UseZone then
      begin
        ZoneDataArray := DataArrayManager.GetDataSetByName(AParam.ZoneName);
        PushGlobalStack;
        try
          ZoneDataArray.Initialize;
        finally
          PopGlobalStack;
        end;
        if not ZoneDataArray.BooleanData[0, Row, Column] then
        begin
          Continue;
        end;
      end;
      AValue := AParam.Value;
      if AParam.UseMultiplier then
      begin
        MultiplierDataArray := DataArrayManager.GetDataSetByName(AParam.MultiplierName);
        PushGlobalStack;
        try
          MultiplierDataArray.Initialize;
        finally
          PopGlobalStack;
        end;
        AValue := AValue * MultiplierDataArray.RealData[0, Row, Column];
      end;
      result := result + AValue;
    end;
  end;
end;


function _GetHufSy(Values: array of pointer): double;
var
  Head: double;
  Column: Integer;
  Row: Integer;
  Layer: Integer;
  AModel: TCustomModel;
  LayerBottom: Double;
  LayerTop: Double;
  HydrogeologicUnits: THydrogeologicUnits;
  HufUnitIndex: Integer;
  HufUnit: THydrogeologicUnit;
  HufThickness: Double;
  HufTop: Double;
  HufBottom: Double;
  Updated: Boolean;
begin
  Assert(Length(Values) >= 1);
  Head := PDouble(Values[0])^;
  GetCellIndicies(Column, Row, Layer, Values, 1);

  AModel := TCustomModel(GlobalCurrentModel);

  result := 0;
  if not (AModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if not AModel.IsLayerSimulated(Layer) then
  begin
    Exit;
  end;
  GetLayerTopAndBottom(Head, Layer, Row, Column, LayerBottom, LayerTop);

  if LayerTop <= LayerBottom then
  begin
    Exit;
  end;

  HydrogeologicUnits := AModel.HydrogeologicUnits;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    HufThickness := GetDataSetValue(Column, Row, 0, HufUnit.ThickessDataArrayName);
    if HufThickness > 0 then
    begin
      HufTop := GetDataSetValue(Column, Row, 0, HufUnit.TopDataArrayName);
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
          if HufThickness > 0 then
          begin
            if (HufTop >= Head) and (Head >= HufBottom) then
            begin
              result := HufUnitParam(HufUnit, Column, Row, ptHUF_SY, Updated);
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;

end;

function HufKHorizontal(Values: array of pointer;
  UseHorizontalAnisotropy: boolean): double;
var
  Layer: Integer;
  Row: Integer;
  Column: Integer;
  LayerTop: Double;
  LayerBottom: Double;
  HufUnitIndex: Integer;
  HydrogeologicUnits: THydrogeologicUnits;
  HufUnit: THydrogeologicUnit;
  AModel: TCustomModel;
  HufThickness: Double;
  HufTop: Double;
  HufBottom: Double;
  CumulativeHufThickness: double;
  HorizontalAnisotropy: double;
  Head: double;
  HufLayerThickness: Double;
begin
  result := 0;
  AModel := TCustomModel(GlobalCurrentModel);
  if not (AModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;

  // does not account for LVDA.  SGWF2HUF7VDHT?

  // affected by simulated layer
  // affected by rsModflow_Initial_Head if any layer type is not equal to zero.
  // affected by tops and thicknesses of all HUF units.
  // affected by tops and bottoms of all layers.
  // affected by zone arrays and multiplier arrays of ptHUF_HK
  // and ptHUF_KDEP parameters
  // may be affected by StrHufReferenceSurface either
  // or kModelTop depending on
  // AModel.ModflowPackages.HufPackage.ReferenceChoice
  // and whether or not KDEP is used.

  Assert(Length(Values) >= 1);
  Head := PDouble(Values[0])^;

  GetCellIndicies(Column, Row, Layer, Values, 1);

  if not AModel.IsLayerSimulated(Layer) then
  begin
    Exit;
  end;
  GetLayerTopAndBottom(Head, Layer, Row, Column, LayerBottom, LayerTop);

  if LayerTop <= LayerBottom then
  begin
    Exit;
  end;

  HydrogeologicUnits := AModel.HydrogeologicUnits;
  CumulativeHufThickness := 0.;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    HufThickness := GetDataSetValue(Column, Row, 0,
      HufUnit.ThickessDataArrayName);
    if HufThickness > 0 then
    begin
      HufTop := GetDataSetValue(Column, Row, 0, HufUnit.TopDataArrayName);
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
          HufLayerThickness := HufTop - HufBottom;
          if HufLayerThickness > 0 then
          begin
            CumulativeHufThickness := CumulativeHufThickness + HufLayerThickness;
            // result is in terms of transmissivity at this point.
            if UseHorizontalAnisotropy then
            begin
              HorizontalAnisotropy := HguHorizontalAnisotrpy(
                HufUnit, Column, Row);
            end
            else
            begin
              HorizontalAnisotropy := 1.;
            end;
            result := result + HorizontalAnisotropy*
              HguTransmissivity(HufUnit, Column, Row, LayerTop, LayerBottom)
              *HufLayerThickness/HufThickness;
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

function _HufKx(Values: array of pointer): double;
begin
  result := HufKHorizontal(Values, False);
  // does not account for LVDA.  SGWF2HUF7VDHT?

  // affected by simulated layer
  // affected by rsModflow_Initial_Head if any layer type is not equal to zero.
  // affected by tops and thicknesses of all HUF units.
  // affected by tops and bottoms of all layers.
  // affected by zone arrays and multiplier arrays of ptHUF_HK
  // and ptHUF_KDEP parameters
  // may be affected by StrHufReferenceSurface either
  // or kModelTop depending on
  // PhastModel.ModflowPackages.HufPackage.ReferenceChoice
  // and whether or not KDEP is used.
end;

function _HufAverageKY(Values: array of pointer): double;
begin
  result := HufKHorizontal(Values, True);
  // does not account for LVDA.  SGWF2HUF7VDHT?

  // affected by simulated layer
  // affected by rsModflow_Initial_Head if any layer type is not equal to zero.
  // affected by tops and thicknesses of all HUF units.
  // affected by tops and bottoms of all layers.
  // affected by zone arrays and multiplier arrays of ptHUF_HK
  // and ptHUF_KDEP parameters
  // may be affected by StrHufReferenceSurface either
  // or kModelTop depending on
  // PhastModel.ModflowPackages.HufPackage.ReferenceChoice
  // and whether or not KDEP is used.
  // affected by zone arrays and multiplier arrays of ptHUF_HANI
  // parameters
end;

function _HufKZ(Values: array of pointer): double;
var
  Column: Integer;
  Row: Integer;
  Layer: Integer;
  AModel: TCustomModel;
  UpperLayerBottom: Double;
  UpperLayerTop: Double;
  LowerLayerBottom: Double;
  LowerLayerTop: Double;
  IntervalTop: Double;
  IntervalBottom: Double;
  HydrogeologicUnits: THydrogeologicUnits;
  CumulativeHufThickness: double;
  HufUnitIndex: Integer;
  HufUnit: THydrogeologicUnit;
  HufThickness: Double;
  HufTop: Double;
  HufBottom: Double;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
  VK: double;
  Vani_Used: Boolean;
  Vani: double;
  VK_Used: Boolean;
  UpperHead: double;
  HufIntervalThickness: Double;
  ZeroResultErrorDisplayed: Boolean;
begin
  result := 0;
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;

  // affected by everything in _HufKx plus the
  // by zone arrays and multiplier arrays of ptHUF_VK and ptHUF_VANI
  // parameters
  // affected by HufUnit.VK_Method.

  ZeroResultErrorDisplayed := False;
  Assert(Length(Values) >= 1);
  UpperHead := PDouble(Values[0])^;
//  LowerHead := PDouble(Values[2])^;
  GetCellIndicies(Column, Row, Layer, Values, 1);

  AModel := TCustomModel(GlobalCurrentModel);

  if not AModel.IsLayerSimulated(Layer) then
  begin
    Exit;
  end;

  if (AModel.ModflowGrid.LayerCount -1 <= Layer)
    or (Layer < 0) then
  begin
    // no vertical hydraulic conductivity is defined
    // below the middle of the bottom layer.
    Exit;
  end;

  GetLayerTopAndBottom(UpperHead, Layer, Row, Column, UpperLayerBottom, UpperLayerTop);

  if UpperLayerTop <= UpperLayerBottom then
  begin
    Exit;
  end;

  GetLayerTopAndBottom(UpperHead, Layer+1, Row, Column,
    LowerLayerBottom, LowerLayerTop, False);

  if LowerLayerTop <= LowerLayerBottom then
  begin
    Exit;
  end;

  IntervalTop := (UpperLayerTop + UpperLayerBottom)/2;
  IntervalBottom := (LowerLayerTop + LowerLayerBottom)/2;

  HydrogeologicUnits := AModel.HydrogeologicUnits;
  CumulativeHufThickness := 0.;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    HufThickness := GetDataSetValue(Column, Row, 0, HufUnit.ThickessDataArrayName);
    if HufThickness > 0 then
    begin
      HufTop := GetDataSetValue(Column, Row, 0, HufUnit.TopDataArrayName);
      if HufTop > IntervalBottom then
      begin
        HufBottom := HufTop - HufThickness;
        if HufBottom < IntervalTop then
        begin
          if HufBottom < IntervalBottom then
          begin
            HufBottom := IntervalBottom;
          end;
          if HufTop > IntervalTop then
          begin
            HufTop := IntervalTop
          end;
          HufIntervalThickness := HufTop - HufBottom;
          if HufIntervalThickness > 0 then
          begin
            CumulativeHufThickness := CumulativeHufThickness + HufIntervalThickness;

            VK := 0.;
            Vani := 0.;
            Vani_Used := False;
            VK_Used := False;

            for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
            begin
              Param := HufUnit.HufUsedParameters[ParamIndex];
              Parameter := Param.Parameter;
              if Parameter.ParameterType = ptHUF_VK then
              begin
                UpdateCellValue(VK, Param, AModel, Column, Row, VK_Used);
              end
              else if Parameter.ParameterType = ptHUF_VANI then
              begin
                UpdateCellValue(Vani, Param, AModel, Column, Row, Vani_Used);
              end;
            end;

            case HufUnit.VK_Method of
              vkVK:
                begin
                  if Vani_Used then
                  begin
                    frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
                      Format(StrInSVANIParamete, [HufUnit.HufName]),
                      Format(StrLayerDRowDCo, [Layer + 1, Row+1, Column+1]));
                  end;
                  // do nothing.
                end;
              vkVANI:
                begin
                  if not Vani_Used then
                  begin
                    Vani := HufUnit.VerticalAnisotropy;
                  end;
                  if Vani <= 0 then
                  begin
                    Vani := 1;
                  end;
                  VK := HguTransmissivity(HufUnit, Column, Row, IntervalTop, IntervalBottom)
                    /HufThickness/Vani;


//                  result := result +
//                    HguTransmissivity(HufUnit, Column, Row)/Vani;
                end;
              else Assert(False);
            end;
            if VK = 0 then
            begin
//              result := 0;
              if HufIntervalThickness > 0 then
              begin
                frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
                  StrBecauseHorizontalH,
                  Format(StrLayerDRowDCo, [Layer + 1, Row+1, Column+1]));
                ZeroResultErrorDisplayed := True;
              end;
            end
            else
            begin
              result := result + HufIntervalThickness/VK;
            end;
          end;
        end;
      end;
    end;
  end;
  if CumulativeHufThickness > 0 then
  begin
    if result = 0 then
    begin
      if not ZeroResultErrorDisplayed then
      begin
        frmErrorsAndWarnings.AddWarning(GlobalCurrentModel,
          StrBecauseHorizontalH,
          Format(StrLayerDRowDCo, [Layer + 1, Row+1, Column+1]));
      end;
    end
    else
    begin
      result := CumulativeHufThickness/result;
    end;
  end;
end;

{function HufUnitParam(HufUnit: THydrogeologicUnit; Column, Row: integer;
  Paramtype: TParameterType; var Updated: boolean): double;
var
  CellValue: double;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
  PhastModel: TPhastModel;
  HufThickness: Double;
begin
  result := 0;
  PhastModel := frmGoPhast.PhastModel;
  HufThickness := GetDataSetValue(Column, Row, 0, HufUnit.ThickessDataArrayName);
  if HufThickness <= 0 then
  begin
    Exit;
  end;

  CellValue := 0.;
  for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
  begin
    Param := HufUnit.HufUsedParameters[ParamIndex];
    Parameter := Param.Parameter;
    if Parameter.ParameterType = Paramtype then
    begin
      UpdateCellValue(CellValue, Param, PhastModel, Column, Row, Updated);
    end
  end;

  // result will be divided by total cell thickness later.
  result := CellValue //* HufThickness;
end; }

function HufAveragParam(Values: array of pointer;
  Paramtype: TParameterType): double;
var
  Layer: Integer;
  Row: Integer;
  Column: Integer;
  LayerTop: Double;
  LayerBottom: Double;
  HufUnitIndex: Integer;
  HydrogeologicUnits: THydrogeologicUnits;
  HufUnit: THydrogeologicUnit;
  AModel: TCustomModel;
  HufThickness: Double;
  HufTop: Double;
  HufBottom: Double;
  CumulativeHufThickness: double;
  Updated: boolean;
  Head: double;
begin
  // affected by simulated layer
  // affected by tops and thicknesses of all HUF units.
  // affected by tops and bottoms of all layers.
  // affected by zone arrays and multiplier arrays of Paramtype

  Assert(Length(Values) >= 1);
  Head := PDouble(Values[0])^;
  GetCellIndicies(Column, Row, Layer, Values, 1);

  AModel := TCustomModel(GlobalCurrentModel);

  result := 0;
  if not AModel.IsLayerSimulated(Layer) then
  begin
    Exit;
  end;
  GetLayerTopAndBottom(Head, Layer, Row, Column, LayerBottom, LayerTop);

  if LayerTop <= LayerBottom then
  begin
    Exit;
  end;

  HydrogeologicUnits := AModel.HydrogeologicUnits;
  CumulativeHufThickness := 0.;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    HufThickness := GetDataSetValue(Column, Row, 0, HufUnit.ThickessDataArrayName);
    if HufThickness > 0 then
    begin
      HufTop := GetDataSetValue(Column, Row, 0, HufUnit.TopDataArrayName);
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
          if HufThickness > 0 then
          begin
            CumulativeHufThickness := CumulativeHufThickness + HufThickness;
            result := result + HufThickness*
              HufUnitParam(HufUnit, Column, Row, Paramtype, Updated);
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

function _HufSS(Values: array of pointer): double;
begin
  result := 0;
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;

  result := HufAveragParam(Values, ptHUF_SS);
end;

function _HufAverageSY(Values: array of pointer): double;
begin
  result := 0;
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  result := HufAveragParam(Values, ptHUF_SY);
end;

function _GridNumber(Values: array of pointer): integer;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := 0;
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if GlobalCurrentModel = frmGoPhast.PhastModel then
  begin
    result := 1;
  end
  else
  begin
    if frmGoPhast.PhastModel.LgrUsed then
    begin
      for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
      begin
        ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel = GlobalCurrentModel then
        begin
          result := ChildIndex + 2;
          Exit;
        end;
      end;
    end;
  end;
end;

function _GridName(Values: array of pointer): string;
var
  ChildModel: TChildModel;
begin
  result := '';
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if GlobalCurrentModel = frmGoPhast.PhastModel then
  begin
    result := 'Parent Grid';
  end
  else
  begin
    if frmGoPhast.PhastModel.LgrUsed and (GlobalCurrentModel <> nil) then
    begin
      ChildModel := GlobalCurrentModel as TChildModel;
      result := ChildModel.ModelName;
    end;
  end;
end;

function _ParentLayer(Values: array of pointer): integer;
begin
  result := _Layer(Values);
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if GlobalCurrentModel <> frmGoPhast.PhastModel then
  begin
    result := (GlobalCurrentModel as TChildModel).
      ChildLayerToParentLayer(result-1)+1;
  end;
end;

function _ParentRow(Values: array of pointer): integer;
begin
  result := _Row(Values);
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if GlobalCurrentModel <> frmGoPhast.PhastModel then
  begin
    result := (GlobalCurrentModel as TChildModel).
      ChildRowToParentRow(result-1)+1;
  end;
end;

function _ParentColumn(Values: array of pointer): integer;
begin
  result := _Column(Values);
  if not (frmGoPhast.PhastModel.ModelSelection in ModflowSelection) then
  begin
    Exit;
  end;
  if GlobalCurrentModel <> frmGoPhast.PhastModel then
  begin
    result := (GlobalCurrentModel as TChildModel).
      ChildColToParentCol(result-1)+1;
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
  LocalGrid: TCustomModelGrid;
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
    Point1 := GlobalCurrentScreenObject.Points[
      GlobalCurrentSegment.VertexIndex];
    LocalGrid := TCustomModel(GlobalCurrentModel).Grid;
    if LocalGrid <> nil then
    begin
      Point1 := LocalGrid.
        RotateFromRealWorldCoordinatesToGridCoordinates(Point1);
    end;
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
    if LocalGrid <> nil then
    begin
      Point2 := LocalGrid.
        RotateFromRealWorldCoordinatesToGridCoordinates(Point2);
    end;
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
var
  LayerIndex: Integer;
  SutraLayerStructure: TSutraLayerStructure;
  LayerGroup: TSutraLayerGroup;
begin
  result := inherited GetVariablesUsed;
  if frmGoPhast.ModelSelection in ModelsWithGrid then
  begin
    result.Add(rsActive);
  end
  else if frmGoPhast.ModelSelection = msSutra22 then
  begin
    if (frmGoPhast.PhastModel <> nil)
      and (frmGoPhast.PhastModel.SutraLayerStructure <> nil) then
    begin
      SutraLayerStructure := frmGoPhast.PhastModel.SutraLayerStructure;
      for LayerIndex := 0 to SutraLayerStructure.Count - 1 do
      begin
        LayerGroup := SutraLayerStructure[LayerIndex];
        result.Add(LayerGroup.DataArrayName);
      end;
    end;
  end;
end;

function TActiveOnLayer.UsesVariable(const Variable: TCustomVariable): boolean;
var
  SutraLayerStructure: TSutraLayerStructure;
  LayerIndex: Integer;
  LayerGroup: TSutraLayerGroup;
begin
  if frmGoPhast.ModelSelection in ModflowSelection then
  begin
    result := inherited UsesVariable(Variable)
      or (Variable.Name = UpperCase(rsActive));
  end
  else
  begin
    result := inherited UsesVariable(Variable);
    if not result then
    begin
    if (frmGoPhast.PhastModel <> nil)
      and (frmGoPhast.PhastModel.SutraLayerStructure <> nil) then
    begin
      SutraLayerStructure := frmGoPhast.PhastModel.SutraLayerStructure;
      for LayerIndex := 0 to SutraLayerStructure.Count - 1 do
      begin
        LayerGroup := SutraLayerStructure[LayerIndex];

        result := (Variable.Name = UpperCase(LayerGroup.DataArrayName));
        if result then
        begin
          exit;
        end;
      end;
    end;
    end;
  end;
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

{ TBcfVcont }

function TBcfVcont.GetVariablesUsed: TStringList;
var
  Index: Integer;
  GeoUnit: TLayerGroup;
begin
  result := inherited GetVariablesUsed;
  result.Add(rsKz);
  if TCustomModel(GlobalCurrentModel).LayerStructure.NonSimulatedLayersPresent then
  begin
    result.Add(rsModflow_CBKz);
  end;
  for Index := 0 to TCustomModel(GlobalCurrentModel).LayerStructure.Count - 1 do
  begin
    GeoUnit := TCustomModel(GlobalCurrentModel).LayerStructure[Index];
    result.Add(GeoUnit.DataArrayName);
  end;
end;

function TBcfVcont.UsesVariable(const Variable: TCustomVariable): boolean;
var
  Index: Integer;
  GeoUnit: TLayerGroup;
begin
  result := inherited UsesVariable(Variable)
    or SameText(Variable.Name, rsKz)
    or (SameText(Variable.Name, rsModflow_CBKz)
    and TCustomModel(GlobalCurrentModel).LayerStructure.NonSimulatedLayersPresent);
  if not result then
  begin
    for Index := 0 to TCustomModel(GlobalCurrentModel).LayerStructure.Count - 1 do
    begin
      GeoUnit := TCustomModel(GlobalCurrentModel).LayerStructure[Index];
      result := SameText(Variable.Name, GeoUnit.DataArrayName);
      if result then
      begin
        Exit;
      end;
    end;
  end;
end;

{ THufKx }

function THufKx.GetVariablesUsed: TStringList;
var
  AModel: TCustomModel;
  HydrogeologicUnits: THydrogeologicUnits;
  HufUnitIndex: Integer;
  HufUnit: THydrogeologicUnit;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
  KDEP_Used: Boolean;
  Index: Integer;
  GeoUnit: TLayerGroup;
  InitialHeadUsed: Boolean;
begin
  result := inherited GetVariablesUsed;

  AModel := TCustomModel(GlobalCurrentModel);

  InitialHeadUsed := False;
  for Index := 0 to TCustomModel(GlobalCurrentModel).LayerStructure.Count - 1 do
  begin
    GeoUnit := TCustomModel(GlobalCurrentModel).LayerStructure[Index];
    result.Add(GeoUnit.DataArrayName);
    if GeoUnit.AquiferType <> 0 then
    begin
      InitialHeadUsed := True;
    end;
  end;

  if InitialHeadUsed then
  begin
    result.Add(rsModflow_Initial_Head);
  end;

  KDEP_Used := False;
  HydrogeologicUnits := AModel.HydrogeologicUnits;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    result.Add(HufUnit.TopDataArrayName);
    result.Add(HufUnit.ThickessDataArrayName);
    for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
    begin
      Param := HufUnit.HufUsedParameters[ParamIndex];
      Parameter := Param.Parameter;
      if Parameter.ParameterType = ptHUF_HK then
      begin
        if Param.UseZone then
        begin
          result.Add(Param.ZoneDataSetName);
        end;
        if Param.UseMultiplier then
        begin
          result.Add(Param.MultiplierDataSetName);
        end;
      end
      else if Parameter.ParameterType = ptHUF_KDEP then
      begin
        KDEP_Used := True;
        if Param.UseZone then
        begin
          result.Add(Param.ZoneDataSetName);
        end;
        if Param.UseMultiplier then
        begin
          result.Add(Param.MultiplierDataSetName);
        end;
      end;
    end;
  end;

  if KDEP_Used then
  begin
    case AModel.ModflowPackages.HufPackage.ReferenceChoice of
      hrcModelTop:
        begin
//          kModelTop added previously.
//          result.Add(kModelTop);
        end;
      hrcReferenceLayer:
        begin
          result.Add(StrHufReferenceSurface);
        end;
      else Assert(False);
    end;
  end;

end;

{
function THufKx.UsesVariable(const Variable: TCustomVariable): boolean;
var
  List: TStringList;
begin
  List := GetVariablesUsed;
  result := List.IndexOf(Variable.Name) >= 0;
//  result := inherited UsesVariable(Variable)
//    or SameText(Variable.Name, rsModflow_Initial_Head)
//    or (SameText(Variable.Name, rsModflow_CBKz)
//    and frmGoPhast.PhastModel.LayerStructure.NonSimulatedLayersPresent);
//  if not result then
//  begin
//    for Index := 0 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
//    begin
//      GeoUnit := frmGoPhast.PhastModel.LayerStructure[Index];
//      result := SameText(Variable.Name, GeoUnit.DataArrayName);
//      if result then
//      begin
//        Exit;
//      end;
//    end;
//  end;
end;
}

{ THufKy }

function THufKy.GetVariablesUsed: TStringList;
var
  AModel: TCustomModel;
  HydrogeologicUnits: THydrogeologicUnits;
  HufUnitIndex: Integer;
  HufUnit: THydrogeologicUnit;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
begin
  result := inherited GetVariablesUsed;
  AModel := TCustomModel(GlobalCurrentModel);
  HydrogeologicUnits := AModel.HydrogeologicUnits;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
    begin
      Param := HufUnit.HufUsedParameters[ParamIndex];
      Parameter := Param.Parameter;
      if Parameter.ParameterType = ptHUF_HANI then
      begin
        if Param.UseZone then
        begin
          result.Add(Param.ZoneDataSetName);
        end;
        if Param.UseMultiplier then
        begin
          result.Add(Param.MultiplierDataSetName);
        end;
      end;
    end;
  end;
end;

{ THufKz }

function THufKz.GetVariablesUsed: TStringList;
var
  AModel: TCustomModel;
  HydrogeologicUnits: THydrogeologicUnits;
  HufUnitIndex: Integer;
  HufUnit: THydrogeologicUnit;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
begin
  result := inherited GetVariablesUsed;
  AModel := TCustomModel(GlobalCurrentModel);
  HydrogeologicUnits := AModel.HydrogeologicUnits;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
    begin
      Param := HufUnit.HufUsedParameters[ParamIndex];
      Parameter := Param.Parameter;
      if Parameter.ParameterType in [ptHUF_VK, ptHUF_VANI] then
      begin
        if Param.UseZone then
        begin
          result.Add(Param.ZoneDataSetName);
        end;
        if Param.UseMultiplier then
        begin
          result.Add(Param.MultiplierDataSetName);
        end;
      end;
    end;
  end;
end;

{ TCustomHufExpression }

function TCustomHufExpression.UsesVariable(
  const Variable: TCustomVariable): boolean;
var
  List: TStringList;
begin
  List := GetVariablesUsed;
  result := List.IndexOf(Variable.Name) >= 0;
end;

{ THufSS }

function THufSS.GetVariablesUsed: TStringList;
var
  AModel: TCustomModel;
  Index: Integer;
  GeoUnit: TLayerGroup;
  HydrogeologicUnits: THydrogeologicUnits;
  HufUnitIndex: Integer;
  HufUnit: THydrogeologicUnit;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
  InitialHeadUsed: Boolean;
begin
  result := inherited GetVariablesUsed;

  AModel := TCustomModel(GlobalCurrentModel);
  InitialHeadUsed := False;
  for Index := 0 to AModel.LayerStructure.Count - 1 do
  begin
    GeoUnit := AModel.LayerStructure[Index];
    result.Add(GeoUnit.DataArrayName);
    if GeoUnit.AquiferType <> 0 then
    begin
      InitialHeadUsed := True;
    end;
  end;

  if InitialHeadUsed then
  begin
    result.Add(rsModflow_Initial_Head);
  end;

  HydrogeologicUnits := AModel.HydrogeologicUnits;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    result.Add(HufUnit.TopDataArrayName);
    result.Add(HufUnit.ThickessDataArrayName);
    for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
    begin
      Param := HufUnit.HufUsedParameters[ParamIndex];
      Parameter := Param.Parameter;
      if Parameter.ParameterType = ptHUF_SS then
      begin
        if Param.UseZone then
        begin
          result.Add(Param.ZoneDataSetName);
        end;
        if Param.UseMultiplier then
        begin
          result.Add(Param.MultiplierDataSetName);
        end;
      end;
    end;
  end;
end;

{ THufSY }

function THufSY.GetVariablesUsed: TStringList;
var
  AModel: TCustomModel;
  Index: Integer;
  GeoUnit: TLayerGroup;
  HydrogeologicUnits: THydrogeologicUnits;
  HufUnitIndex: Integer;
  HufUnit: THydrogeologicUnit;
  ParamIndex: Integer;
  Param: THufUsedParameter;
  Parameter: TModflowParameter;
  InitialHeadUsed: Boolean;
begin
  result := inherited GetVariablesUsed;

  AModel := TCustomModel(GlobalCurrentModel);
  InitialHeadUsed := False;
  for Index := 0 to AModel.LayerStructure.Count - 1 do
  begin
    GeoUnit := AModel.LayerStructure[Index];
    result.Add(GeoUnit.DataArrayName);
    if GeoUnit.AquiferType <> 0 then
    begin
      InitialHeadUsed := True;
    end;
  end;

  if InitialHeadUsed then
  begin
    result.Add(rsModflow_Initial_Head);
  end;

  HydrogeologicUnits := AModel.HydrogeologicUnits;
  for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := HydrogeologicUnits[HufUnitIndex];
    result.Add(HufUnit.TopDataArrayName);
    result.Add(HufUnit.ThickessDataArrayName);
    for ParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
    begin
      Param := HufUnit.HufUsedParameters[ParamIndex];
      Parameter := Param.Parameter;
      if Parameter.ParameterType = ptHUF_SY then
      begin
        if Param.UseZone then
        begin
          result.Add(Param.ZoneDataSetName);
        end;
        if Param.UseMultiplier then
        begin
          result.Add(Param.MultiplierDataSetName);
        end;
      end;
    end;
  end;
end;

{ THufSYTP }

function THufSYTP.GetVariablesUsed: TStringList;
var
  AtModel: TCustomModel;
  SteadyParameters: TModflowSteadyParameters;
  Index: Integer;
  AParam: TModflowSteadyParameter;
begin
  result := inherited GetVariablesUsed;

  AtModel := TCustomModel(GlobalCurrentModel);
  SteadyParameters := AtModel.ModflowSteadyParameters;
  for Index := 0 to SteadyParameters.Count - 1 do
  begin
    AParam := SteadyParameters[Index];
    if AParam.ParameterType = ptHUF_SYTP then
    begin
      if AParam.UseZone then
      begin
        result.Add(AParam.ZoneName)
      end;
      if AParam.UseMultiplier then
      begin
        result.Add(AParam.MultiplierName)
      end;
    end;
  end;
end;

resourcestring
  StrGIS = 'GIS|';
  StrGridOrMesh = 'Grid or Mesh|';
  StrObject = 'Object|';
  StrMODFLOW = 'MODFLOW|';
  StrModflowLgr = 'MODFLOW-LGR|';

initialization
  SpecialImplementors := TList.Create;

  XFunction.ResultType := rdtDouble;
  XFunction.RFunctionAddr := _X;
  SetLength(XFunction.InputDataTypes, 0);
  XFunction.OptionalArguments := 0;
  XFunction.CanConvertToConstant := False;
  XFunction.Name := 'X';
  XFunction.Prototype := StrGIS+'X';

  YFunction.ResultType := rdtDouble;
  YFunction.RFunctionAddr := _Y;
  SetLength(YFunction.InputDataTypes, 0);
  YFunction.OptionalArguments := 0;
  YFunction.CanConvertToConstant := False;
  YFunction.Name := 'Y';
  YFunction.Prototype := StrGIS+'Y';

  ZFunction.ResultType := rdtDouble;
  ZFunction.RFunctionAddr := _Z;
  SetLength(ZFunction.InputDataTypes, 0);
  ZFunction.OptionalArguments := 0;
  ZFunction.CanConvertToConstant := False;
  ZFunction.Name := 'Z';
  ZFunction.Prototype := StrGIS+'Z';

  XPrimeFunction.ResultType := rdtDouble;
  XPrimeFunction.RFunctionAddr := _XPrime;
  SetLength(XPrimeFunction.InputDataTypes, 0);
  XPrimeFunction.OptionalArguments := 0;
  XPrimeFunction.CanConvertToConstant := False;
  XPrimeFunction.Name := 'X_Prime';
  XPrimeFunction.Prototype := StrGIS+'X_Prime';

  YPrimeFunction.ResultType := rdtDouble;
  YPrimeFunction.RFunctionAddr := _YPrime;
  SetLength(YPrimeFunction.InputDataTypes, 0);
  YPrimeFunction.OptionalArguments := 0;
  YPrimeFunction.CanConvertToConstant := False;
  YPrimeFunction.Name := 'Y_Prime';
  YPrimeFunction.Prototype := StrGIS+'Y_Prime';

  ColumnFunction.ResultType := rdtInteger;
  ColumnFunction.IFunctionAddr := _Column;
  SetLength(ColumnFunction.InputDataTypes, 0);
  ColumnFunction.OptionalArguments := 0;
  ColumnFunction.CanConvertToConstant := False;
  ColumnFunction.Name := 'Column';
  ColumnFunction.Prototype := StrGridOrMesh+'Column';

  ElevationToModelLayerFunction.ResultType := rdtInteger;
  ElevationToModelLayerFunction.IFunctionAddr := _ElevationToModelLayer;
  SetLength(ElevationToModelLayerFunction.InputDataTypes, 1);
  ElevationToModelLayerFunction.InputDataTypes[0] := rdtDouble;
  ElevationToModelLayerFunction.OptionalArguments := 0;
  ElevationToModelLayerFunction.CanConvertToConstant := False;
  ElevationToModelLayerFunction.Name := 'ElevationToModelLayer';
  ElevationToModelLayerFunction.Prototype := StrGridOrMesh+'ElevationToModelLayer(Elevation)';

  ElevationToLayerFunction.ResultType := rdtInteger;
  ElevationToLayerFunction.IFunctionAddr := _ElevationToLayer;
  SetLength(ElevationToLayerFunction.InputDataTypes, 1);
  ElevationToLayerFunction.InputDataTypes[0] := rdtDouble;
  ElevationToLayerFunction.OptionalArguments := 0;
  ElevationToLayerFunction.CanConvertToConstant := False;
  ElevationToLayerFunction.Name := 'ElevationToLayer';
  ElevationToLayerFunction.Prototype := StrGridOrMesh+'ElevationToLayer(Elevation)';

  RowFunction.ResultType := rdtInteger;
  RowFunction.IFunctionAddr := _Row;
  SetLength(RowFunction.InputDataTypes, 0);
  RowFunction.OptionalArguments := 0;
  RowFunction.CanConvertToConstant := False;
  RowFunction.Name := 'Row';
  RowFunction.Prototype := StrGridOrMesh+'Row';

  LayerFunction.ResultType := rdtInteger;
  LayerFunction.IFunctionAddr := _Layer;
  SetLength(LayerFunction.InputDataTypes, 0);
  LayerFunction.OptionalArguments := 0;
  LayerFunction.CanConvertToConstant := False;
  LayerFunction.Name := 'Layer';
  LayerFunction.Prototype := StrGridOrMesh+'Layer';

  ColumnWidthFunction.ResultType := rdtDouble;
  ColumnWidthFunction.RFunctionAddr := _ColumnWidth;
  SetLength(ColumnWidthFunction.InputDataTypes, 1);
  ColumnWidthFunction.InputDataTypes[0] := rdtInteger;
  ColumnWidthFunction.OptionalArguments := 1;
  ColumnWidthFunction.CanConvertToConstant := False;
  ColumnWidthFunction.Name := 'ColumnWidth';
  ColumnWidthFunction.Prototype := StrGridOrMesh+'ColumnWidth({Column})';

  RowWidthFunction.ResultType := rdtDouble;
  RowWidthFunction.RFunctionAddr := _RowWidth;
  SetLength(RowWidthFunction.InputDataTypes, 1);
  RowWidthFunction.InputDataTypes[0] := rdtInteger;
  RowWidthFunction.OptionalArguments := 1;
  RowWidthFunction.CanConvertToConstant := False;
  RowWidthFunction.Name := 'RowWidth';
  RowWidthFunction.Prototype := StrGridOrMesh+'RowWidth({Row})';

  LayerHeightFunction.ResultType := rdtDouble;
  LayerHeightFunction.RFunctionAddr := _LayerHeight;
  SetLength(LayerHeightFunction.InputDataTypes, 3);
  LayerHeightFunction.InputDataTypes[0] := rdtInteger;
  LayerHeightFunction.InputDataTypes[1] := rdtInteger;
  LayerHeightFunction.InputDataTypes[2] := rdtInteger;
  LayerHeightFunction.OptionalArguments := 3;
  LayerHeightFunction.CanConvertToConstant := False;
  LayerHeightFunction.Name := StrLayerHeight;
  LayerHeightFunction.Prototype := StrGridOrMesh+''+StrLayerHeight+'({{Col, Row,} Layer})';

  BlockAreaTopFunction.ResultType := rdtDouble;
  BlockAreaTopFunction.RFunctionAddr := _BlockAreaTop;
  SetLength(BlockAreaTopFunction.InputDataTypes, 2);
  BlockAreaTopFunction.InputDataTypes[0] := rdtInteger;
  BlockAreaTopFunction.InputDataTypes[1] := rdtInteger;
  BlockAreaTopFunction.OptionalArguments := 2;
  BlockAreaTopFunction.CanConvertToConstant := False;
  BlockAreaTopFunction.Name := 'BlockAreaTop';
  BlockAreaTopFunction.Prototype := StrGridOrMesh+'BlockAreaTop({Column, Row})';

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
    StrGridOrMesh+'BlockAreaFront({Column, {Row,} Layer})';

  BlockAreaSideFunction.ResultType := rdtDouble;
  BlockAreaSideFunction.RFunctionAddr := _BlockAreaSide;
  SetLength(BlockAreaSideFunction.InputDataTypes, 3);
  BlockAreaSideFunction.InputDataTypes[0] := rdtInteger;
  BlockAreaSideFunction.InputDataTypes[1] := rdtInteger;
  BlockAreaSideFunction.InputDataTypes[2] := rdtInteger;
  BlockAreaSideFunction.OptionalArguments := 3;
  BlockAreaSideFunction.CanConvertToConstant := False;
  BlockAreaSideFunction.Name := 'BlockAreaSide';
  BlockAreaSideFunction.Prototype := StrGridOrMesh+'BlockAreaSide({{Col,} Row, Layer})';

  BlockVolumeFunction.ResultType := rdtDouble;
  BlockVolumeFunction.RFunctionAddr := _BlockVolume;
  SetLength(BlockVolumeFunction.InputDataTypes, 3);
  BlockVolumeFunction.InputDataTypes[0] := rdtInteger;
  BlockVolumeFunction.InputDataTypes[1] := rdtInteger;
  BlockVolumeFunction.InputDataTypes[2] := rdtInteger;
  BlockVolumeFunction.OptionalArguments := 3;
  BlockVolumeFunction.CanConvertToConstant := False;
  BlockVolumeFunction.Name := 'BlockVolume';
  BlockVolumeFunction.Prototype := StrGridOrMesh+'BlockVolume({{Column, Row,} Layer})';

  ColumnPositionFunction.ResultType := rdtDouble;
  ColumnPositionFunction.RFunctionAddr := _ColumnPosition;
  SetLength(ColumnPositionFunction.InputDataTypes, 1);
  ColumnPositionFunction.InputDataTypes[0] := rdtInteger;
  ColumnPositionFunction.OptionalArguments := 1;
  ColumnPositionFunction.CanConvertToConstant := False;
  ColumnPositionFunction.Name := 'ColumnBoundaryPosition';
  ColumnPositionFunction.Prototype := StrGridOrMesh+'ColumnBoundaryPosition({Column})';

  RowPositionFunction.ResultType := rdtDouble;
  RowPositionFunction.RFunctionAddr := _RowPosition;
  SetLength(RowPositionFunction.InputDataTypes, 1);
  RowPositionFunction.InputDataTypes[0] := rdtInteger;
  RowPositionFunction.OptionalArguments := 1;
  RowPositionFunction.CanConvertToConstant := False;
  RowPositionFunction.Name := 'RowBoundaryPosition';
  RowPositionFunction.Prototype := StrGridOrMesh+'RowBoundaryPosition({Row})';

  LayerPositionFunction.ResultType := rdtDouble;
  LayerPositionFunction.RFunctionAddr := _LayerPosition;
  SetLength(LayerPositionFunction.InputDataTypes, 3);
  LayerPositionFunction.InputDataTypes[0] := rdtInteger;
  LayerPositionFunction.InputDataTypes[1] := rdtInteger;
  LayerPositionFunction.InputDataTypes[2] := rdtInteger;
  LayerPositionFunction.OptionalArguments := 3;
  LayerPositionFunction.CanConvertToConstant := False;
  LayerPositionFunction.Name := StrLayerBoundaryPosition;
  LayerPositionFunction.Prototype :=
    StrGridOrMesh+'' + StrLayerBoundaryPosition + '({{Column, Row,} Layer})';

  LayerCenterFunction.ResultType := rdtDouble;
  LayerCenterFunction.RFunctionAddr := _LayerCenter;
  SetLength(LayerCenterFunction.InputDataTypes, 3);
  LayerCenterFunction.InputDataTypes[0] := rdtInteger;
  LayerCenterFunction.InputDataTypes[1] := rdtInteger;
  LayerCenterFunction.InputDataTypes[2] := rdtInteger;
  LayerCenterFunction.OptionalArguments := 3;
  LayerCenterFunction.CanConvertToConstant := False;
  LayerCenterFunction.Name := 'LayerCenter';
  LayerCenterFunction.Prototype :=
    StrGridOrMesh+'LayerCenter({{Column, Row,} Layer})';

  ColumnCenterFunction.ResultType := rdtDouble;
  ColumnCenterFunction.RFunctionAddr := _ColumnCenter;
  SetLength(ColumnCenterFunction.InputDataTypes, 1);
  ColumnCenterFunction.InputDataTypes[0] := rdtInteger;
  ColumnCenterFunction.OptionalArguments := 1;
  ColumnCenterFunction.CanConvertToConstant := False;
  ColumnCenterFunction.Name := 'ColumnCenter';
  ColumnCenterFunction.Prototype :=
    StrGridOrMesh+'ColumnCenter({Column})';

  RowCenterFunction.ResultType := rdtDouble;
  RowCenterFunction.RFunctionAddr := _RowCenter;
  SetLength(RowCenterFunction.InputDataTypes, 1);
  RowCenterFunction.InputDataTypes[0] := rdtInteger;
  RowCenterFunction.OptionalArguments := 1;
  RowCenterFunction.CanConvertToConstant := False;
  RowCenterFunction.Name := 'RowCenter';
  RowCenterFunction.Prototype :=
    StrGridOrMesh+'RowCenter({Row})';

  ColumnCountFunction.ResultType := rdtInteger;
  ColumnCountFunction.IFunctionAddr := _ColumnCount;
  SetLength(ColumnCountFunction.InputDataTypes, 0);
  ColumnCountFunction.OptionalArguments := 0;
  ColumnCountFunction.CanConvertToConstant := False;
  ColumnCountFunction.Name := 'ColumnCount';
  ColumnCountFunction.Prototype := StrGridOrMesh+'ColumnCount';

  RowCountFunction.ResultType := rdtInteger;
  RowCountFunction.IFunctionAddr := _RowCount;
  SetLength(RowCountFunction.InputDataTypes, 0);
  RowCountFunction.OptionalArguments := 0;
  RowCountFunction.CanConvertToConstant := False;
  RowCountFunction.Name := 'RowCount';
  RowCountFunction.Prototype := StrGridOrMesh+'RowCount';

  LayerCountFunction.ResultType := rdtInteger;
  LayerCountFunction.IFunctionAddr := _LayerCount;
  SetLength(LayerCountFunction.InputDataTypes, 0);
  LayerCountFunction.OptionalArguments := 0;
  LayerCountFunction.CanConvertToConstant := False;
  LayerCountFunction.Name := 'LayerCount';
  LayerCountFunction.Prototype := StrGridOrMesh+'LayerCount';

  ObjectLengthFunction.ResultType := rdtDouble;
  ObjectLengthFunction.RFunctionAddr := _ObjectLength;
  SetLength(ObjectLengthFunction.InputDataTypes, 0);
  ObjectLengthFunction.OptionalArguments := 0;
  ObjectLengthFunction.CanConvertToConstant := False;
  ObjectLengthFunction.Name := StrObjectLength;
  ObjectLengthFunction.Prototype := StrObject+'ObjectLength';

  ObjectAreaFunction.ResultType := rdtDouble;
  ObjectAreaFunction.RFunctionAddr := _ObjectArea;
  SetLength(ObjectAreaFunction.InputDataTypes, 0);
  ObjectAreaFunction.OptionalArguments := 0;
  ObjectAreaFunction.CanConvertToConstant := False;
  ObjectAreaFunction.Name := StrObjectArea;
  ObjectAreaFunction.Prototype := StrObject+'ObjectArea';

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
    StrObject+'ObjectIntersectLength({Column, Row, Layer})';

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
    StrObject+'ObjectSectionIntersectLength({Section, Column, Row, Layer})';

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
    StrObject+'ObjectIntersectArea({Column, Row, Layer})';

  ObjectNameFunction.ResultType := rdtString;
  ObjectNameFunction.SFunctionAddr := _ObjectName;
  SetLength(ObjectNameFunction.InputDataTypes, 0);
  ObjectNameFunction.OptionalArguments := 0;
  ObjectNameFunction.CanConvertToConstant := False;
  ObjectNameFunction.Name := 'ObjectName';
  ObjectNameFunction.Prototype :=
    StrObject+'ObjectName';

  NodeXFunction.ResultType := rdtDouble;
  NodeXFunction.RFunctionAddr := _XNodePosition;
  SetLength(NodeXFunction.InputDataTypes, 1);
  NodeXFunction.InputDataTypes[0] := rdtInteger;
  NodeXFunction.OptionalArguments := 0;
  NodeXFunction.CanConvertToConstant := False;
  NodeXFunction.Name := 'ObjectVertexX';
  NodeXFunction.Prototype := StrObject+'ObjectVertexX(VertexIndex)';
  SetLength(NodeXFunction.Synonyms, 1);
  NodeXFunction.Synonyms[0] := 'ObjectNodeX';

  NodeYFunction.ResultType := rdtDouble;
  NodeYFunction.RFunctionAddr := _YNodePosition;
  SetLength(NodeYFunction.InputDataTypes, 1);
  NodeYFunction.InputDataTypes[0] := rdtInteger;
  NodeYFunction.OptionalArguments := 0;
  NodeYFunction.CanConvertToConstant := False;
  NodeYFunction.Name := 'ObjectVertexY';
  NodeYFunction.Prototype := StrObject+'ObjectVertexY(VertexIndex)';
  SetLength(NodeYFunction.Synonyms, 1);
  NodeYFunction.Synonyms[0] := 'ObjectNodeY';

  NodeZFunction.ResultType := rdtDouble;
  NodeZFunction.RFunctionAddr := _ZNodePosition;
  SetLength(NodeZFunction.InputDataTypes, 1);
  NodeZFunction.InputDataTypes[0] := rdtInteger;
  NodeZFunction.OptionalArguments := 0;
  NodeZFunction.CanConvertToConstant := False;
  NodeZFunction.Name := 'ObjectVertexZ';
  NodeZFunction.Prototype := StrObject+'ObjectVertexZ(VertexIndex)';
  SetLength(NodeZFunction.Synonyms, 1);
  NodeZFunction.Synonyms[0] := 'ObjectNodeZ';



  NodeDistanceFunction.ResultType := rdtDouble;
  NodeDistanceFunction.RFunctionAddr := _NodeDistances;
  SetLength(NodeDistanceFunction.InputDataTypes, 1);
  NodeDistanceFunction.InputDataTypes[0] := rdtInteger;
  NodeDistanceFunction.OptionalArguments := 0;
  NodeDistanceFunction.CanConvertToConstant := False;
  NodeDistanceFunction.Name := 'ObjectVertexDistance';
  NodeDistanceFunction.Prototype := StrObject+'ObjectVertexDistance(VertexIndex)';
  SetLength(NodeDistanceFunction.Synonyms, 1);
  NodeDistanceFunction.Synonyms[0] := 'ObjectNodeDistance';

  CurrentNodeXFunction.ResultType := rdtDouble;
  CurrentNodeXFunction.RFunctionAddr := _CurrentXNodePosition;
  CurrentNodeXFunction.OptionalArguments := 0;
  CurrentNodeXFunction.CanConvertToConstant := False;
  CurrentNodeXFunction.Name := ObjectCurrentVertexX;
  CurrentNodeXFunction.Prototype := StrObject+'' + ObjectCurrentVertexX;
  SetLength(CurrentNodeXFunction.Synonyms, 1);
  CurrentNodeXFunction.Synonyms[0] := 'ObjectCurrentNodeX';

  CurrentNodeYFunction.ResultType := rdtDouble;
  CurrentNodeYFunction.RFunctionAddr := _CurrentYNodePosition;
  CurrentNodeYFunction.OptionalArguments := 0;
  CurrentNodeYFunction.CanConvertToConstant := False;
  CurrentNodeYFunction.Name := ObjectCurrentVertexY;
  CurrentNodeYFunction.Prototype := StrObject+'' + ObjectCurrentVertexY;
  SetLength(CurrentNodeYFunction.Synonyms, 1);
  CurrentNodeYFunction.Synonyms[0] := 'ObjectCurrentNodeY';

  CurrentNodeZFunction.ResultType := rdtDouble;
  CurrentNodeZFunction.RFunctionAddr := _CurrentZNodePosition;
  CurrentNodeZFunction.OptionalArguments := 0;
  CurrentNodeZFunction.CanConvertToConstant := False;
  CurrentNodeZFunction.Name := ObjectCurrentVertexZ;
  CurrentNodeZFunction.Prototype := StrObject+'' + ObjectCurrentVertexZ;
  SetLength(CurrentNodeZFunction.Synonyms, 1);
  CurrentNodeZFunction.Synonyms[0] := 'ObjectCurrentNodeZ';

  CurrentSegmentAngleFunction.ResultType := rdtDouble;
  CurrentSegmentAngleFunction.RFunctionAddr := _CurrentSegmentAngle;
  CurrentSegmentAngleFunction.OptionalArguments := 0;
  CurrentSegmentAngleFunction.CanConvertToConstant := False;
  CurrentSegmentAngleFunction.Name := ObjectCurrentSegmentAngle;
  CurrentSegmentAngleFunction.Prototype := StrObject+''+ObjectCurrentSegmentAngle;

  CurrentSegmentAngleDegreesFunction.ResultType := rdtDouble;
  CurrentSegmentAngleDegreesFunction.RFunctionAddr := _CurrentSegmentAngleDegrees;
  CurrentSegmentAngleDegreesFunction.OptionalArguments := 0;
  CurrentSegmentAngleDegreesFunction.CanConvertToConstant := False;
  CurrentSegmentAngleDegreesFunction.Name :=ObjectDegrees;
  CurrentSegmentAngleDegreesFunction.Prototype := StrObject+'' + ObjectDegrees;

  CurrentSegmentAngleLimitedegreesFunction.ResultType := rdtDouble;
  CurrentSegmentAngleLimitedegreesFunction.RFunctionAddr := _CurrentSegmentAngleLimitedDegrees;
  CurrentSegmentAngleLimitedegreesFunction.OptionalArguments := 0;
  CurrentSegmentAngleLimitedegreesFunction.CanConvertToConstant := False;
  CurrentSegmentAngleLimitedegreesFunction.Name := ObjectDegreesLimited;
  CurrentSegmentAngleLimitedegreesFunction.Prototype := StrObject+''+ObjectDegreesLimited;

  CurrentSegmentLengthFunction.ResultType := rdtDouble;
  CurrentSegmentLengthFunction.RFunctionAddr := _CurrentSegmentLength;
  CurrentSegmentLengthFunction.OptionalArguments := 0;
  CurrentSegmentLengthFunction.CanConvertToConstant := False;
  CurrentSegmentLengthFunction.Name := ObjectCurSegLength;
  CurrentSegmentLengthFunction.Prototype := StrObject+'' + ObjectCurSegLength;

  CurrentSectionIndexFunction.ResultType := rdtInteger;
  CurrentSectionIndexFunction.IFunctionAddr := _CurrentSectionIndex;
  CurrentSectionIndexFunction.OptionalArguments := 0;
  CurrentSectionIndexFunction.CanConvertToConstant := False;
  CurrentSectionIndexFunction.Name := 'ObjectCurrentSectionIndex';
  CurrentSectionIndexFunction.Prototype := StrObject+'ObjectCurrentSectionIndex';

  FractionOfObjectLengthFunction.ResultType := rdtDouble;
  FractionOfObjectLengthFunction.RFunctionAddr := _FractionOfObjectLength;
  FractionOfObjectLengthFunction.OptionalArguments := 0;
  FractionOfObjectLengthFunction.CanConvertToConstant := False;
  FractionOfObjectLengthFunction.Name := 'FractionOfObjectLength';
  FractionOfObjectLengthFunction.Prototype := StrObject+'FractionOfObjectLength';

  InterpolationedValuesFunction.ResultType := rdtDouble;
  InterpolationedValuesFunction.RFunctionAddr := _InterpolatedVertexValues;
  SetLength(InterpolationedValuesFunction.InputDataTypes, 1);
  InterpolationedValuesFunction.InputDataTypes[0] := rdtString;
  InterpolationedValuesFunction.OptionalArguments := 1;
  InterpolationedValuesFunction.CanConvertToConstant := False;
  InterpolationedValuesFunction.Name := StrInterpolatedVertexValues;
  InterpolationedValuesFunction.Prototype := StrObject+''
    + StrInterpolatedVertexValues + '(Key)';
  InterpolationedValuesFunction.Hidden := False;

  VertexValuesFunction.ResultType := rdtDouble;
  VertexValuesFunction.RFunctionAddr := _VertexValue;
  SetLength(VertexValuesFunction.InputDataTypes, 2);
  VertexValuesFunction.InputDataTypes[0] := rdtString;
  VertexValuesFunction.InputDataTypes[1] := rdtDouble;
  VertexValuesFunction.OptionalArguments := 0;
  VertexValuesFunction.CanConvertToConstant := False;
  VertexValuesFunction.Name := 'VertexValue';
  VertexValuesFunction.Prototype := StrObject+''
    + 'VertexValue' + '(Key, DefaultValue)';
  VertexValuesFunction.Hidden := False;

  NodeCountFunction.ResultType := rdtInteger;
  NodeCountFunction.IFunctionAddr := _ObjectNodeCount;
  NodeCountFunction.OptionalArguments := 0;
  NodeCountFunction.CanConvertToConstant := False;
  NodeCountFunction.Name := 'ObjectVertexCount';
  NodeCountFunction.Prototype := StrObject+'ObjectVertexCount';
  SetLength(NodeCountFunction.Synonyms, 1);
  NodeCountFunction.Synonyms[0] := 'ObjectNodeCount';

  ImportedValuesRFunction.ResultType := rdtDouble;
  ImportedValuesRFunction.RFunctionAddr := _ImportedScreenObjectValuesR;
  SetLength(ImportedValuesRFunction.InputDataTypes, 1);
  ImportedValuesRFunction.InputDataTypes[0] := rdtString;
  ImportedValuesRFunction.OptionalArguments := 1;
  ImportedValuesRFunction.CanConvertToConstant := False;
  ImportedValuesRFunction.Name := rsObjectImportedValuesR;
  ImportedValuesRFunction.Prototype := StrObject+'' + rsObjectImportedValuesR
    + '({Key})';
  ImportedValuesRFunction.Hidden := False;

  ImportedValuesIFunction.ResultType := rdtInteger;
  ImportedValuesIFunction.IFunctionAddr := _ImportedScreenObjectValuesI;
  SetLength(ImportedValuesIFunction.InputDataTypes, 1);
  ImportedValuesIFunction.InputDataTypes[0] := rdtString;
  ImportedValuesIFunction.OptionalArguments := 1;
  ImportedValuesIFunction.CanConvertToConstant := False;
  ImportedValuesIFunction.Name := rsObjectImportedValuesI;
  ImportedValuesIFunction.Prototype := StrObject+'' + rsObjectImportedValuesI
    + '({Key})';
  ImportedValuesIFunction.Hidden := False;

  ImportedValuesBFunction.ResultType := rdtBoolean;
  ImportedValuesBFunction.BFunctionAddr := _ImportedScreenObjectValuesB;
  SetLength(ImportedValuesBFunction.InputDataTypes, 1);
  ImportedValuesBFunction.InputDataTypes[0] := rdtString;
  ImportedValuesBFunction.OptionalArguments := 1;
  ImportedValuesBFunction.CanConvertToConstant := False;
  ImportedValuesBFunction.Name := rsObjectImportedValuesB;
  ImportedValuesBFunction.Prototype := StrObject+'' + rsObjectImportedValuesB
    + '({Key})';
  ImportedValuesBFunction.Hidden := False;

  ImportedValuesTFunction.ResultType := rdtString;
  ImportedValuesTFunction.SFunctionAddr := _ImportedScreenObjectValuesT;
  SetLength(ImportedValuesTFunction.InputDataTypes, 1);
  ImportedValuesTFunction.InputDataTypes[0] := rdtString;
  ImportedValuesTFunction.OptionalArguments := 1;
  ImportedValuesTFunction.CanConvertToConstant := False;
  ImportedValuesTFunction.Name := rsObjectImportedValuesT;
  ImportedValuesTFunction.Prototype := StrObject+'' + rsObjectImportedValuesT
    + '({Key})';
  ImportedValuesTFunction.Hidden := False;

  ListRealValueFunction.ResultType := rdtDouble;
  ListRealValueFunction.RFunctionAddr := _ListDataSetRealValue;
  SetLength(ListRealValueFunction.InputDataTypes, 1);
  ListRealValueFunction.InputDataTypes[0] := rdtString;
  ListRealValueFunction.OptionalArguments := 0;
  ListRealValueFunction.CanConvertToConstant := False;
  ListRealValueFunction.Name := rsListRealValue;
  ListRealValueFunction.Prototype := StrObject+'' + rsListRealValue +
    '("DataSetName")';
  ListRealValueFunction.Hidden := True;

  ListIntegerValueFunction.ResultType := rdtInteger;
  ListIntegerValueFunction.IFunctionAddr := _ListDataSetIntegerValue;
  SetLength(ListIntegerValueFunction.InputDataTypes, 1);
  ListIntegerValueFunction.InputDataTypes[0] := rdtString;
  ListIntegerValueFunction.OptionalArguments := 0;
  ListIntegerValueFunction.CanConvertToConstant := False;
  ListIntegerValueFunction.Name := rsListIntegerValue;
  ListIntegerValueFunction.Prototype := StrObject+'' + rsListIntegerValue +
    '("DataSetName")';
  ListIntegerValueFunction.Hidden := True;

  ModflowLayerSimulatedFunction.ResultType := rdtBoolean;
  ModflowLayerSimulatedFunction.BFunctionAddr := _SimulatedModflowLayer;
  SetLength(ModflowLayerSimulatedFunction.InputDataTypes, 1);
  ModflowLayerSimulatedFunction.InputDataTypes[0] := rdtInteger;
  ModflowLayerSimulatedFunction.OptionalArguments := 1;
  ModflowLayerSimulatedFunction.CanConvertToConstant := False;
  ModflowLayerSimulatedFunction.Name := 'SimulatedLayer';
  ModflowLayerSimulatedFunction.Prototype := StrMODFLOW+'SimulatedLayer({Layer})';

  GridNumberFunction.ResultType := rdtInteger;
  GridNumberFunction.IFunctionAddr := _GridNumber;
  SetLength(GridNumberFunction.InputDataTypes, 0);
  GridNumberFunction.OptionalArguments := 0;
  GridNumberFunction.CanConvertToConstant := False;
  GridNumberFunction.Name := StrGridNumber;
  GridNumberFunction.Prototype := StrModflowLgr + StrGridNumber;
  GridNumberFunction.Hidden := False;

  GridNameFunction.ResultType := rdtString;
  GridNameFunction.SFunctionAddr := _GridName;
  SetLength(GridNameFunction.InputDataTypes, 0);
  GridNameFunction.OptionalArguments := 0;
  GridNameFunction.CanConvertToConstant := False;
  GridNameFunction.Name := StrGridName;
  GridNameFunction.Prototype := StrModflowLgr + StrGridName;
  GridNameFunction.Hidden := False;

  ParentLayerFunction.ResultType := rdtInteger;
  ParentLayerFunction.IFunctionAddr := _ParentLayer;
  SetLength(ParentLayerFunction.InputDataTypes, 0);
  ParentLayerFunction.OptionalArguments := 0;
  ParentLayerFunction.CanConvertToConstant := False;
  ParentLayerFunction.Name := StrParentLayer;
  ParentLayerFunction.Prototype := StrModflowLgr + StrParentLayer;
  ParentLayerFunction.Hidden := False;

  ParentRowFunction.ResultType := rdtInteger;
  ParentRowFunction.IFunctionAddr := _ParentRow;
  SetLength(ParentRowFunction.InputDataTypes, 0);
  ParentRowFunction.OptionalArguments := 0;
  ParentRowFunction.CanConvertToConstant := False;
  ParentRowFunction.Name := StrParentRow;
  ParentRowFunction.Prototype := StrModflowLgr + StrParentRow;
  ParentRowFunction.Hidden := False;

  ParentColumnFunction.ResultType := rdtInteger;
  ParentColumnFunction.IFunctionAddr := _ParentColumn;
  SetLength(ParentColumnFunction.InputDataTypes, 0);
  ParentColumnFunction.OptionalArguments := 0;
  ParentColumnFunction.CanConvertToConstant := False;
  ParentColumnFunction.Name := StrParentColumn;
  ParentColumnFunction.Prototype := StrModflowLgr + StrParentColumn;
  ParentColumnFunction.Hidden := False;

  NodeInterpolate := TFunctionClass.Create;
  NodeInterpolate.InputDataCount := 3;
  NodeInterpolate.OptionalArguments := -1;
  NodeInterpolate.RFunctionAddr := _NodeInterpolate;
  NodeInterpolate.Name := StrVertexInterpolate;
  NodeInterpolate.Prototype := StrObject+'' + StrVertexInterpolate + '(Value1, Value2, ...)';
  NodeInterpolate.Synonyms.Add(StrNodeInterpolate);
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
  ActiveOnLayer.Prototype := StrGridOrMesh+'ActiveOnLayer(Layer)';
  ActiveOnLayer.InputDataTypes[0] := rdtInteger;
  ActiveOnLayer.AllowConversionToConstant := False;

  ActiveOnLayerSpecialImplementor := TSpecialImplementor.Create;
  ActiveOnLayerSpecialImplementor.FunctionClass := ActiveOnLayer;
  ActiveOnLayerSpecialImplementor.Implementor := TActiveOnLayer;
  SpecialImplementors.Add(ActiveOnLayerSpecialImplementor);

//  HighestActiveLayer := TFunctionClass.Create;
//  HighestActiveLayer.InputDataCount := 0;
//  HighestActiveLayer.OptionalArguments := 0;
//  HighestActiveLayer.IFunctionAddr := _HighestActiveLayer;
//  HighestActiveLayer.Name := 'HighestActiveLayer';
//  HighestActiveLayer.Prototype := StrGridOrMesh+'HighestActiveLayer';
//  HighestActiveLayer.AllowConversionToConstant := False;
//
//  HighestActiveLayerSpecialImplementor := TSpecialImplementor.Create;
//  HighestActiveLayerSpecialImplementor.FunctionClass := HighestActiveLayer;
//  HighestActiveLayerSpecialImplementor.Implementor := TActiveOnLayer;
//  SpecialImplementors.Add(HighestActiveLayerSpecialImplementor);

  SpecifiedHeadOnLayer := TFunctionClass.Create;
  SpecifiedHeadOnLayer.InputDataCount := 1;
  SpecifiedHeadOnLayer.OptionalArguments := 0;
  SpecifiedHeadOnLayer.BFunctionAddr := _SpecifiedHeadOnLayer;
  SpecifiedHeadOnLayer.Name := 'SpecifiedHeadOnLayer';
  SpecifiedHeadOnLayer.Prototype := StrMODFLOW+'SpecifiedHeadOnLayer(Layer)';
  SpecifiedHeadOnLayer.InputDataTypes[0] := rdtInteger;
  SpecifiedHeadOnLayer.AllowConversionToConstant := False;

  SpecifiedHeadOnLayerSpecialImplementor := TSpecialImplementor.Create;
  SpecifiedHeadOnLayerSpecialImplementor.FunctionClass := SpecifiedHeadOnLayer;
  SpecifiedHeadOnLayerSpecialImplementor.Implementor := TSpecifiedHeadOnLayer;
  SpecialImplementors.Add(SpecifiedHeadOnLayerSpecialImplementor);

  BcfVcont := TFunctionClass.Create;
  BcfVcont.InputDataCount := 0;
  BcfVcont.OptionalArguments := 3;
  BcfVcont.RFunctionAddr := _BcfGetVcont;
  BcfVcont.Name := StrBcfVCONT;
  BcfVcont.Prototype := StrMODFLOW+'' + StrBcfVCONT + '({Layer, Row, Column})';
  BcfVcont.OptionalType := rdtInteger;
  BcfVcont.AllowConversionToConstant := False;

  BcfVcontSpecialImplementor := TSpecialImplementor.Create;
  BcfVcontSpecialImplementor.FunctionClass := BcfVcont;
  BcfVcontSpecialImplementor.Implementor := TBcfVcont;
  SpecialImplementors.Add(BcfVcontSpecialImplementor);

  HufKx := TFunctionClass.Create;
  HufKx.InputDataCount := 4;
  HufKx.OptionalArguments := 3;
  HufKx.RFunctionAddr := _HufKx;
  HufKx.Name := StrHufKx;
  HufKx.Prototype := StrMODFLOW+'' + StrHufKx + '(Head, {Layer, Row, Column})';
  HufKx.OptionalType := rdtInteger;
  HufKx.AllowConversionToConstant := False;
  HufKx.InputDataTypes[0] := rdtDouble;
  HufKx.InputDataTypes[1] := rdtInteger;
  HufKx.InputDataTypes[2] := rdtInteger;
  HufKx.InputDataTypes[3] := rdtInteger;

  HufKxSpecialImplementor := TSpecialImplementor.Create;
  HufKxSpecialImplementor.FunctionClass := HufKx;
  HufKxSpecialImplementor.Implementor := THufKx;
  SpecialImplementors.Add(HufKxSpecialImplementor);

  HufKy := TFunctionClass.Create;
  HufKy.InputDataCount := 4;
  HufKy.OptionalArguments := 3;
  HufKy.RFunctionAddr := _HufAverageKY;
  HufKy.Name := StrHufKy;
  HufKy.Prototype := StrMODFLOW+'' + StrHufKy + '(Head, {Layer, Row, Column})';
  HufKy.OptionalType := rdtInteger;
  HufKy.AllowConversionToConstant := False;
  HufKy.InputDataTypes[0] := rdtDouble;
  HufKy.InputDataTypes[1] := rdtInteger;
  HufKy.InputDataTypes[2] := rdtInteger;
  HufKy.InputDataTypes[3] := rdtInteger;

  HufKySpecialImplementor := TSpecialImplementor.Create;
  HufKySpecialImplementor.FunctionClass := HufKy;
  HufKySpecialImplementor.Implementor := THufKy;
  SpecialImplementors.Add(HufKySpecialImplementor);

  HufKz := TFunctionClass.Create;
  HufKz.InputDataCount := 4;
  HufKz.OptionalArguments := 3;
  HufKz.RFunctionAddr := _HufKz;
  HufKz.Name := StrHufKz;
  HufKz.Prototype := StrMODFLOW+'' + StrHufKz + '(Head, {Layer, Row, Column})';
  HufKz.OptionalType := rdtInteger;
  HufKz.AllowConversionToConstant := False;
  HufKz.InputDataTypes[0] := rdtDouble;
  HufKz.InputDataTypes[1] := rdtInteger;
  HufKz.InputDataTypes[2] := rdtInteger;
  HufKz.InputDataTypes[3] := rdtInteger;

  HufKzSpecialImplementor := TSpecialImplementor.Create;
  HufKzSpecialImplementor.FunctionClass := HufKz;
  HufKzSpecialImplementor.Implementor := THufKz;
  SpecialImplementors.Add(HufKzSpecialImplementor);

  HufSS := TFunctionClass.Create;
  HufSS.InputDataCount := 4;
  HufSS.OptionalArguments := 3;
  HufSS.RFunctionAddr := _HufSS;
  HufSS.Name := StrHufSs;
  HufSS.Prototype := StrMODFLOW+'' + StrHufSs + '(Head, {Layer, Row, Column})';
  HufSS.OptionalType := rdtInteger;
  HufSS.AllowConversionToConstant := False;
  HufSS.InputDataTypes[0] := rdtDouble;
  HufSS.InputDataTypes[1] := rdtInteger;
  HufSS.InputDataTypes[2] := rdtInteger;
  HufSS.InputDataTypes[3] := rdtInteger;

  HufSSSpecialImplementor := TSpecialImplementor.Create;
  HufSSSpecialImplementor.FunctionClass := HufSS;
  HufSSSpecialImplementor.Implementor := THufSS;
  SpecialImplementors.Add(HufSSSpecialImplementor);

  HufAverageSY := TFunctionClass.Create;
  HufAverageSY.InputDataCount := 4;
  HufAverageSY.OptionalArguments := 3;
  HufAverageSY.RFunctionAddr := _HufAverageSY;
  HufAverageSY.Name := StrHufAverageSy;
  HufAverageSY.Prototype := StrMODFLOW+'' + StrHufAverageSy + '(Head, {Layer, Row, Column})';
  HufAverageSY.OptionalType := rdtInteger;
  HufAverageSY.AllowConversionToConstant := False;
  HufAverageSY.InputDataTypes[0] := rdtDouble;
  HufAverageSY.InputDataTypes[1] := rdtInteger;
  HufAverageSY.InputDataTypes[2] := rdtInteger;
  HufAverageSY.InputDataTypes[3] := rdtInteger;

  HufAverageSYSpecialImplementor := TSpecialImplementor.Create;
  HufAverageSYSpecialImplementor.FunctionClass := HufAverageSY;
  HufAverageSYSpecialImplementor.Implementor := THufSY;
  SpecialImplementors.Add(HufAverageSYSpecialImplementor);

  HufSY := TFunctionClass.Create;
  HufSY.InputDataCount := 4;
  HufSY.OptionalArguments := 3;
  HufSY.RFunctionAddr := _GetHufSy;
  HufSY.Name := StrHufSy;
  HufSY.Prototype := StrMODFLOW+'' + StrHufSy + '(Head, {Layer, Row, Column})';
  HufSY.OptionalType := rdtInteger;
  HufSY.AllowConversionToConstant := False;
  HufSY.InputDataTypes[0] := rdtDouble;
  HufSY.InputDataTypes[1] := rdtInteger;
  HufSY.InputDataTypes[2] := rdtInteger;
  HufSY.InputDataTypes[3] := rdtInteger;

  HufSYSpecialImplementor := TSpecialImplementor.Create;
  HufSYSpecialImplementor.FunctionClass := HufSY;
  HufSYSpecialImplementor.Implementor := THufSY;
  SpecialImplementors.Add(HufSYSpecialImplementor);

  HufSYTP := TFunctionClass.Create;
  HufSYTP.InputDataCount := 2;
  HufSYTP.OptionalArguments := 2;
  HufSYTP.RFunctionAddr := _GetHufSytp;
  HufSYTP.Name := StrHufSytp;
  HufSYTP.Prototype := StrMODFLOW+'' + StrHufSytp + '({Row, Column})';
  HufSYTP.OptionalType := rdtInteger;
  HufSYTP.AllowConversionToConstant := False;
  HufSYTP.InputDataTypes[0] := rdtInteger;
  HufSYTP.InputDataTypes[1] := rdtInteger;

  HufSYTPSpecialImplementor := TSpecialImplementor.Create;
  HufSYTPSpecialImplementor.FunctionClass := HufSYTP;
  HufSYTPSpecialImplementor.Implementor := THufSYTP;
  SpecialImplementors.Add(HufSYTPSpecialImplementor);

  InvalidNames := TStringList.Create;
  InvalidNames.Sorted := True;
  InvalidNames.CaseSensitive := False;

finalization
  InvalidNames.Free;

  NodeInterpolate.Free;
  NodeInterpolateSpecialImplementor.Free;

  ActiveOnLayer.Free;
  ActiveOnLayerSpecialImplementor.Free;

//  HighestActiveLayer.Free;
//  HighestActiveLayerSpecialImplementor.Free;

  SpecifiedHeadOnLayer.Free;
  SpecifiedHeadOnLayerSpecialImplementor.Free;

  BcfVcont.Free;
  BcfVcontSpecialImplementor.Free;

  HufKx.Free;
  HufKxSpecialImplementor.Free;

  HufKy.Free;
  HufKySpecialImplementor.Free;

  HufKz.Free;
  HufKzSpecialImplementor.Free;

  HufSS.Free;
  HufSSSpecialImplementor.Free;

  HufAverageSY.Free;
  HufAverageSYSpecialImplementor.Free;

  HufSY.Free;
  HufSYSpecialImplementor.Free;

  HufSYTP.Free;
  HufSYTPSpecialImplementor.Free;

  SpecialImplementors.Free;

end.

