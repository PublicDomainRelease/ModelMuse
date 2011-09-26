unit frmExportShapefileObjectsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomSelectObjectsUnit, VirtualTrees, StdCtrls, Buttons,
  ExtCtrls, Contnrs, ScreenObjectUnit, DataSetUnit, ArgusDataEntry,
  ShapefileUnit, XBase1, ValueArrayStorageUnit;

type
  TFieldDefinition = record
    DataArray: TDataArray;
    FieldName: AnsiString;
    FieldType: AnsiChar;  // C = character, F = floating point, L = Logic, N = Number
  end;

  TBreakScreenObject = record
    BreakObject: boolean;
  end;

  TfrmExportShapefileObjects = class(TfrmCustomSelectObjects)
    Panel1: TPanel;
    lblObjects: TLabel;
    vstDataSets: TVirtualStringTree;
    lblDataArrays: TLabel;
    Splitter1: TSplitter;
    BitBtn1: TBitBtn;
    rdeMissingData: TRbwDataEntry;
    lblMissingData: TLabel;
    gbExportAs: TGroupBox;
    rbPoints: TRadioButton;
    rbMultipoint: TRadioButton;
    rbPolyline: TRadioButton;
    rbPolygons: TRadioButton;
    sdShapefile: TSaveDialog;
    XBaseShapeFile: TXBase;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure vstDataSetsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure FormResize(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure vstDataSetsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstDataSetsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    procedure vstObjectsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure btnCloseClick(Sender: TObject);
  protected
    function ShouldCheckBoxBeChecked(ScreenObject: TScreenObject): boolean;
      override;
    procedure HandleChecked(AScreenObject: TScreenObject); override;
    procedure HandleUnchecked(AScreenObject: TScreenObject); override;
    function CanSelect(ScreenObject: TScreenObject): boolean; override;
  private
    CurrentNodeName: string;
    FObjectOwner: TList;
    FSelectedDataSets: TList;
    FSelectedScreenObjects: TList;
    FSettingChecked: Boolean;
    FFieldDefinitions: array of TFieldDefinition;
    FBreakScreenObjects: array of TBreakScreenObject;
    FShapeType: Integer;
    FMissingValueString: string;
    FMissingValue: Integer;
    FShowWarning: boolean;
    FShapeFileWriter: TShapefileGeometryWriter;
    procedure GetData;
    procedure CenterLabels;
    procedure SetCheckedNodes(Sender: TBaseVirtualTree);
    procedure SetAllowableShapeTypes;
    procedure SetData;
    procedure DefinePointGeometry(AScreenObject: TScreenObject);
    procedure DefineMultipointGeometry(AScreenObject: TScreenObject;
      BreakObject: boolean);
    procedure DefinePolylineGeometry(AScreenObject: TScreenObject;
      BreakObject: boolean);
    procedure DefinePolygonGeometrySingleSection(AScreenObject: TScreenObject;
      BreakObject: boolean);
    procedure DefinePolygonGeometryMultipleSections(
      AScreenObject: TScreenObject);
    procedure SetFieldType(DataArrayIndex: Integer);
    procedure AssignFieldName(FieldNames: TStringList; DataArrayIndex: Integer);
    procedure FillFieldDefinitions(FieldDefinitions: TStringList);
    procedure CreateDataBase(FieldDefinitions: TStringList);
    procedure DefineShapeGeometry(AScreenObject: TScreenObject;
      BreakObject: boolean);
    procedure InitializeDataBase;
    procedure GetShapeType(var ShapeType: Integer);
    procedure AssignFieldValues(AScreenObject: TScreenObject;
      BreakObject: boolean);
    procedure InitializeBreakScreenObjects;
    function GetImportedValuesFromFormula(DataArray: TDataArray;
      ScreenObject: TScreenObject; Formula: string): TValueArrayStorage;
    procedure CreateShape(var Shape: TShapeObject);
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses frmCustomGoPhastUnit, ClassificationUnit, PhastModelUnit, FastGEO,
  ConvexHullUnit, GPC_Classes, gpc, RbwParser, StrUtils,
  frmErrorsAndWarningsUnit, GIS_Functions, ModelMuseUtilities, frmGoPhastUnit;

const
  StrFormulaTruncatedTo = 'Formula truncated to 254 characters';

type
  TPointDirection = (pdIncrement, pdDecrement);

  { TfrmExportShapefileObjects }

procedure TfrmExportShapefileObjects.btnCloseClick(Sender: TObject);
begin
  inherited;
  if sdShapefile.Execute then
  begin
    if FileExists(sdShapefile.FileName)
      or FileExists(ChangeFileExt(sdShapefile.FileName, '.shx'))
      or FileExists(ChangeFileExt(sdShapefile.FileName, '.dbf')) then
    begin
      if MessageDlg('Are you sure you want to overwrite the existing Shapefile?',
        mtWarning, [mbYes, mbNo], 0) <> mrYes then
      begin
        Exit;
      end;
    end;
    SetData
  end;
end;

function TfrmExportShapefileObjects.CanSelect(
  ScreenObject: TScreenObject): boolean;
var
  Index: Integer;
  DataArray: TDataArray;
begin
  result := False;
  for Index := 0 to FSelectedDataSets.Count - 1 do
  begin
    DataArray := FSelectedDataSets[Index];
    if ScreenObject.IndexOfDataSet(DataArray) >= 0 then
    begin
      result := True;
      Exit;
    end;
  end;
end;

procedure TfrmExportShapefileObjects.CenterLabels;
begin
  lblObjects.Left := vstObjects.Left
    + (vstObjects.Width - lblObjects.Width) div 2;
  lblDataArrays.Left := vstDataSets.Left
    + (vstDataSets.Width - lblDataArrays.Width) div 2;
end;

procedure TfrmExportShapefileObjects.SetCheckedNodes(Sender: TBaseVirtualTree);
var
  AScreenObject: TScreenObject;
  Index: Integer;
  ChildNode: PVirtualNode;
  Data: PMyRec;
  ANode: PVirtualNode;
begin
  ANode := Sender.GetFirst;
  while ANode <> nil do
  begin
    Data := Sender.GetNodeData(ANode);
    if (Data.ScreenObjects <> nil) and (Data.ScreenObjects.Count > 0) then
    begin
      ChildNode := Sender.GetFirstChild(ANode);
      for Index := 0 to Data.ScreenObjects.Count - 1 do
      begin
        Assert(ChildNode <> nil);
        AScreenObject := Data.ScreenObjects[Index];
        if FSelectedScreenObjects.IndexOf(AScreenObject) >= 0 then
        begin
          Sender.CheckState[ChildNode] := csCheckedNormal;
        end
        else
        begin
          Sender.CheckState[ChildNode] := csUncheckedNormal;
        end;
        ChildNode := Sender.GetNextSibling(ChildNode);
      end;
    end;
    ANode := Sender.GetNext(ANode);
  end;
end;


procedure TfrmExportShapefileObjects.SetData;
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
//  ShapeFileWriter: TShapefileGeometryWriter;
begin
  FShowWarning := False;
  frmErrorsAndWarnings.RemoveWarningGroup(frmGoPhast.PhastModel, StrFormulaTruncatedTo);

  try
    InitializeDataBase;
    GetShapeType(FShapeType);
    FMissingValueString := rdeMissingData.Text;
    FMissingValue := StrToInt(FMissingValueString);

    FShapeFileWriter := TShapefileGeometryWriter.Create(FShapeType, True);
    try
      for ObjectIndex := 0 to FSelectedScreenObjects.Count - 1 do
      begin
        AScreenObject := FSelectedScreenObjects[ObjectIndex];
        DefineShapeGeometry(AScreenObject,
          FBreakScreenObjects[ObjectIndex].BreakObject);
        AssignFieldValues(AScreenObject,
          FBreakScreenObjects[ObjectIndex].BreakObject);
      end;
      FShapeFileWriter.WriteToFile(sdShapefile.FileName,
        ChangeFileExt(sdShapefile.FileName, '.shx'));
    finally
      FShapeFileWriter.Free;
    end;
  finally
    XBaseShapeFile.Active := False;
  end;

  if FShowWarning then
  begin
    frmErrorsAndWarnings.Show;
  end;
end;

procedure TfrmExportShapefileObjects.DefinePointGeometry(
  AScreenObject: TScreenObject);
var
  APoint: TPoint2D;
  Shape: TShapeObject;
begin
  CreateShape(Shape);
  Shape.FNumPoints := 1;
  Shape.FNumParts := 0;
  SetLength(Shape.FPoints, 1);
  APoint := AScreenObject.Points[0];
  Shape.FPoints[0].X := APoint.x;
  Shape.FPoints[0].Y := APoint.y;
end;

procedure TfrmExportShapefileObjects.DefineMultipointGeometry(
  AScreenObject: TScreenObject; 
  BreakObject: boolean);
var
  APoint: TPoint2D;
  PointPosition: Integer;
  SectionIndex: Integer;
  SectionEnd: Integer;
  PointIndex: Integer;
  Shape: TShapeObject;
  PointCount: Integer;
begin
  if BreakObject then
  begin
    for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
    begin
      if AScreenObject.SectionClosed[SectionIndex] then
      begin
        SectionEnd := AScreenObject.SectionEnd[SectionIndex] - 1;
      end
      else
      begin
        SectionEnd := AScreenObject.SectionEnd[SectionIndex];
      end;
      CreateShape(Shape);
      PointCount := SectionEnd - AScreenObject.SectionStart[SectionIndex]+1;
      SetLength(Shape.FPoints, PointCount);
      SetLength(Shape.FParts, PointCount);
      PointPosition := 0;
      for PointIndex := AScreenObject.SectionStart[SectionIndex] to SectionEnd do
      begin
        APoint := AScreenObject.Points[PointIndex];
        Shape.FPoints[PointPosition].X := APoint.x;
        Shape.FPoints[PointPosition].Y := APoint.y;
        Shape.FParts[PointPosition] := PointPosition;
        Inc(PointPosition);
      end;
      Shape.FNumPoints := PointPosition;
      Shape.FNumParts := PointPosition;
      SetLength(Shape.FPoints, PointPosition);
      SetLength(Shape.FParts, PointPosition);
    end;
  end
  else
  begin
    CreateShape(Shape);
    SetLength(Shape.FPoints, AScreenObject.Count);
    SetLength(Shape.FParts, AScreenObject.Count);
    PointPosition := 0;
    for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
    begin
      if AScreenObject.SectionClosed[SectionIndex] then
      begin
        SectionEnd := AScreenObject.SectionEnd[SectionIndex] - 1;
      end
      else
      begin
        SectionEnd := AScreenObject.SectionEnd[SectionIndex];
      end;
      for PointIndex := AScreenObject.SectionStart[SectionIndex] to SectionEnd do
      begin
        APoint := AScreenObject.Points[PointIndex];
        Shape.FPoints[PointPosition].X := APoint.x;
        Shape.FPoints[PointPosition].Y := APoint.y;
        Shape.FParts[PointPosition] := PointPosition;
        Inc(PointPosition);
      end;
    end;
    Shape.FNumPoints := PointPosition;
    Shape.FNumParts := PointPosition;
    SetLength(Shape.FPoints, PointPosition);
    SetLength(Shape.FParts, PointPosition);
  end;
end;

procedure TfrmExportShapefileObjects.DefinePolylineGeometry(
  AScreenObject: TScreenObject; BreakObject: boolean);
var
  PointPosition: Integer;
  SectionIndex: Integer;
  PointIndex: Integer;
  APoint: TPoint2D;
  FirstPointInSection: Boolean;
  Shape: TShapeObject;
begin
  if BreakObject then
  begin
    for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
    begin
      CreateShape(Shape);
      SetLength(Shape.FPoints, AScreenObject.SectionLength[SectionIndex]);
      Shape.FNumParts := 1;
      SetLength(Shape.FParts, 1);
      Shape.FParts[0] := 0;
      PointPosition := 0;
      for PointIndex := AScreenObject.SectionStart[SectionIndex] to
        AScreenObject.SectionEnd[SectionIndex] do
      begin
        APoint := AScreenObject.Points[PointIndex];
        Shape.FPoints[PointPosition].X := APoint.x;
        Shape.FPoints[PointPosition].Y := APoint.y;
        Inc(PointPosition);
      end;
      Shape.FNumPoints := PointPosition;
      Assert(Length(Shape.FPoints) = PointPosition);
    end;
  end
  else
  begin
    CreateShape(Shape);
    SetLength(Shape.FPoints, AScreenObject.Count);
    PointPosition := 0;
    Shape.FNumParts := AScreenObject.SectionCount;
    SetLength(Shape.FParts, AScreenObject.SectionCount);
    for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
    begin
      FirstPointInSection := True;
      for PointIndex := AScreenObject.SectionStart[SectionIndex] to
        AScreenObject.SectionEnd[SectionIndex] do
      begin
        APoint := AScreenObject.Points[PointIndex];
        Shape.FPoints[PointPosition].X := APoint.x;
        Shape.FPoints[PointPosition].Y := APoint.y;
        if FirstPointInSection then
        begin
          Shape.FParts[SectionIndex] := PointPosition;
          FirstPointInSection := False;
        end;
        Inc(PointPosition);
      end;
    end;
    Shape.FNumPoints := PointPosition;
    Assert(Length(Shape.FPoints) = PointPosition);
  end;
end;

procedure TfrmExportShapefileObjects.DefinePolygonGeometrySingleSection(
  AScreenObject: TScreenObject;
  BreakObject: boolean);
var
  OutputPoints: TPolygon2D;
  APoint: TPoint2D;
  InputOrientation: Integer;
  InputPoints: TPolygon2D;
  PointIndex: Integer;
  PointPosition: Integer;
  CopyCount: Integer;
  Shape: TShapeObject;
  SectionIndex : integer;
begin
  if BreakObject then
  begin
    for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
    begin
      CreateShape(Shape);

      CopyCount := AScreenObject.SectionLength[SectionIndex] - 1;
      SetLength(InputPoints, CopyCount);

      AScreenObject.CopyPoints(InputPoints, 0,
        AScreenObject.SectionStart[SectionIndex], CopyCount);
      ConvexHull2(InputPoints, InputOrientation, OutputPoints);
//      PointPosition := 0;
      if InputOrientation = Clockwise then
      begin
        PointPosition := 0;
      end
      else
      begin
        PointPosition := AScreenObject.SectionLength[SectionIndex] - 1;
        Assert(InputOrientation = CounterClockwise);
      end;

      Shape.FNumPoints := AScreenObject.SectionLength[SectionIndex];
      SetLength(Shape.FPoints, AScreenObject.SectionLength[SectionIndex]);
      Shape.FNumParts := 1;
      SetLength(Shape.FParts, 1);
      Shape.FParts[0] := 0;
      for PointIndex := AScreenObject.SectionStart[SectionIndex] to
        AScreenObject.SectionEnd[SectionIndex] do
      begin
        APoint := AScreenObject.Points[PointIndex];
        Shape.FPoints[PointPosition].X := APoint.x;
        Shape.FPoints[PointPosition].Y := APoint.y;
        if InputOrientation = Clockwise then
        begin
          Inc(PointPosition);
        end
        else
        begin
          Dec(PointPosition);
        end;
      end;
//      Assert(Length(Shape.FPoints) = PointPosition);
    end;
  end
  else
  begin
    CreateShape(Shape);
    SetLength(InputPoints, AScreenObject.Count - 1);
    CopyCount := AScreenObject.Count - 1;
    AScreenObject.CopyPoints(InputPoints, 0, 0, CopyCount);
    ConvexHull2(InputPoints, InputOrientation, OutputPoints);
    if InputOrientation = Clockwise then
    begin
      PointPosition := 0;
    end
    else
    begin
      Assert(InputOrientation = CounterClockwise);
      PointPosition := AScreenObject.Count - 1;
    end;
    SetLength(Shape.FPoints, AScreenObject.Count);
    Shape.FNumParts := 1;
    SetLength(Shape.FParts, 1);
    Shape.FParts[0] := 0;
    for PointIndex := 0 to AScreenObject.Count - 1 do
    begin
      APoint := AScreenObject.Points[PointIndex];
      Shape.FPoints[PointPosition].X := APoint.x;
      Shape.FPoints[PointPosition].Y := APoint.y;
      if InputOrientation = Clockwise then
      begin
        Inc(PointPosition);
      end
      else
      begin
        Dec(PointPosition);
      end;
    end;
    Shape.FNumPoints := AScreenObject.Count;
  end;
end;

procedure TfrmExportShapefileObjects.DefinePolygonGeometryMultipleSections(
  AScreenObject: TScreenObject);
var
  ShapePoint: TShapePoint;
  PointDirection: TPointDirection;
  OutputPoints: TPolygon2D;
  InputDirection: Integer;
  InputPoints: TPolygon2D;
  SectionPosition: Integer;
  IntersectPolygon: TGpcPolygonClass;
  GpcPoint: Tgpc_vertex;
  APoint: TPoint2D;
  PointIndex: Integer;
  PointPosition: Integer;
  SectionIndex: Integer;
  GpcPolygon: TGpcPolygonClass;
  EmptyPolygon: TGpcPolygonClass;
  Shape: TShapeObject;
begin
  CreateShape(Shape);
  EmptyPolygon := TGpcPolygonClass.Create;
  GpcPolygon := TGpcPolygonClass.Create;
  try
    GpcPolygon.NumberOfContours := AScreenObject.SectionCount;
    for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
    begin
      PointPosition := 0;
      GpcPolygon.VertexCount[SectionIndex] :=
        AScreenObject.SectionEnd[SectionIndex]
        - AScreenObject.SectionStart[SectionIndex];
      // Skip the last point in a section;
      // it is a duplicate of the first point.
      for PointIndex := AScreenObject.SectionStart[SectionIndex] to
        AScreenObject.SectionEnd[SectionIndex] - 1 do
      begin
        APoint := AScreenObject.Points[PointIndex];
        GpcPoint.X := APoint.x;
        GpcPoint.Y := APoint.y;
        GpcPolygon.Vertices[SectionIndex, PointPosition] := GpcPoint;
        Inc(PointPosition);
      end;
    end;
    IntersectPolygon := TGpcPolygonClass.CreateFromOperation(
      GPC_DIFF, GpcPolygon, EmptyPolygon);
    try
      Shape.FNumPoints := IntersectPolygon.TotalVertexCount
        + IntersectPolygon.NumberOfContours;
      SetLength(Shape.FPoints, Shape.FNumPoints);
      Shape.FNumParts := IntersectPolygon.NumberOfContours;
      SetLength(Shape.FParts, IntersectPolygon.NumberOfContours);
      SectionPosition := 0;
      for SectionIndex := 0 to IntersectPolygon.NumberOfContours - 1 do
      begin
        SetLength(InputPoints, IntersectPolygon.VertexCount[SectionIndex]);
        for PointIndex := 0 to IntersectPolygon.VertexCount[SectionIndex] - 1 do
        begin
          GpcPoint := IntersectPolygon.Vertices[SectionIndex, PointIndex];
          InputPoints[PointIndex].x := GpcPoint.x;
          InputPoints[PointIndex].y := GpcPoint.y;
        end;
        ConvexHull2(InputPoints, InputDirection, OutputPoints);
//        PointDirection := pdIncrement;
        if IntersectPolygon.Holes[SectionIndex] then
        begin
          if InputDirection = ClockWise then
          begin
            PointDirection := pdDecrement;
          end
          else
          begin
            PointDirection := pdIncrement;
            Assert(InputDirection = counterClockWise);
          end;
        end
        else
        begin
          if InputDirection = ClockWise then
          begin
            PointDirection := pdIncrement;
          end
          else
          begin
            PointDirection := pdDecrement;
            Assert(InputDirection = counterClockWise);
          end;
        end;
        PointPosition := -1;
        case PointDirection of
          pdIncrement:
            PointPosition := 0;
          pdDecrement:
            PointPosition := IntersectPolygon.VertexCount[SectionIndex];
        else
          Assert(False);
        end;
        for PointIndex := 0 to IntersectPolygon.VertexCount[SectionIndex] - 1 do
        begin
          GpcPoint := IntersectPolygon.Vertices[SectionIndex, PointIndex];
          ShapePoint.X := GpcPoint.x;
          ShapePoint.Y := GpcPoint.y;
          Shape.FPoints[SectionPosition + PointPosition] := ShapePoint;
          case PointDirection of
            pdIncrement:
              Inc(PointPosition);
            pdDecrement:
              Dec(PointPosition);
          else
            Assert(False);
          end;
        end;
        GpcPoint := IntersectPolygon.Vertices[SectionIndex, 0];
        ShapePoint.X := GpcPoint.x;
        ShapePoint.Y := GpcPoint.y;
        Shape.FPoints[SectionPosition + PointPosition] := ShapePoint;
        Shape.FParts[SectionIndex] := SectionPosition;
        SectionPosition := SectionPosition
          + IntersectPolygon.VertexCount[SectionIndex] + 1;
      end;
    finally
      IntersectPolygon.Free;
    end;
  finally
    GpcPolygon.Free;
    EmptyPolygon.Free;
  end;
end;

procedure TfrmExportShapefileObjects.SetFieldType(DataArrayIndex: Integer);
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  DataArrayPosition: Integer;
  Formula: string;
  AValue: Double;
  AnInt: Integer;
  ImportedValues: TValueArrayStorage;
begin
  case FFieldDefinitions[DataArrayIndex].DataArray.DataType of
    rdtDouble:
      begin
        FFieldDefinitions[DataArrayIndex].FieldType := 'F';
      end;
    rdtInteger:
      begin
        FFieldDefinitions[DataArrayIndex].FieldType := 'N';
      end;
    rdtBoolean:
      begin
        FFieldDefinitions[DataArrayIndex].FieldType := 'N';
      end;
    rdtString:
      begin
        FFieldDefinitions[DataArrayIndex].FieldType := 'C';
      end;
  else
    Assert(False);
  end;
  if FFieldDefinitions[DataArrayIndex].DataArray.DataType <> rdtString then
  begin
    for ObjectIndex := 0 to FSelectedScreenObjects.Count - 1 do
    begin
      AScreenObject := FSelectedScreenObjects[ObjectIndex];
      DataArrayPosition := AScreenObject.IndexOfDataSet(
        FFieldDefinitions[DataArrayIndex].DataArray);
      if DataArrayPosition >= 0 then
      begin
        Formula := AScreenObject.DataSetFormulas[DataArrayPosition];
        if FBreakScreenObjects[ObjectIndex].BreakObject then
        begin
          ImportedValues := GetImportedValuesFromFormula(
            FFieldDefinitions[DataArrayIndex].DataArray,
            AScreenObject, Formula);
        end
        else
        begin
          ImportedValues := nil;
        end;
        if (ImportedValues = nil)
          or (ImportedValues.Count <> AScreenObject.SectionCount) then
        begin
          case FFieldDefinitions[DataArrayIndex].DataArray.DataType of
            rdtDouble:
              begin
                if not TryStrToFloat(Formula, AValue) then
                begin
                  FFieldDefinitions[DataArrayIndex].FieldType := 'C';
                  break;
                end;
              end;
            rdtInteger:
              begin
                if not TryStrToInt(Formula, AnInt) then
                begin
                  FFieldDefinitions[DataArrayIndex].FieldType := 'C';
                  break;
                end;
              end;
            rdtBoolean:
              begin
                if not SameText(Formula, 'True')
                  and not SameText(Formula, 'False') then
                begin
                  FFieldDefinitions[DataArrayIndex].FieldType := 'C';
                  break;
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

procedure TfrmExportShapefileObjects.AssignFieldName(FieldNames: TStringList;
  DataArrayIndex: Integer);
var
  FieldName: AnsiString;
  Root: AnsiString;
  SuffixValue: Integer;
  Suffix: AnsiString;
const
  MaximumFieldNameLength = 10;
begin
  FieldName := AnsiString(UpperCase(FFieldDefinitions[DataArrayIndex].DataArray.Name));
  if Length(FieldName) > MaximumFieldNameLength then
  begin
    SetLength(FieldName, MaximumFieldNameLength);
  end;
  FieldName := FixShapeFileFieldName(FieldName);
  if FieldNames.IndexOf(string(FieldName)) >= 0 then
  begin
    Root := FieldName;
    SuffixValue := 0;
    repeat
      Inc(SuffixValue);
      Suffix := AnsiString(IntToStr(SuffixValue));
      if Length(Root) + Length(Suffix) > MaximumFieldNameLength then
      begin
        SetLength(Root, MaximumFieldNameLength - Length(Suffix));
      end;
      FieldName := Root + Suffix;
      FieldName := FixShapeFileFieldName(FieldName);
    until (FieldNames.IndexOf(string(FieldName)) < 0);
  end;
  FFieldDefinitions[DataArrayIndex].FieldName := FieldName;
  FieldNames.Add(string(FieldName));
end;

procedure TfrmExportShapefileObjects.FillFieldDefinitions(
  FieldDefinitions: TStringList);
var
  FieldDefinition: AnsiString;
  FieldIndex: Integer;
begin
  for FieldIndex := 0 to FSelectedDataSets.Count - 1 do
  begin
    FieldDefinition := FFieldDefinitions[FieldIndex].FieldName + '=';
    case FFieldDefinitions[FieldIndex].FieldType of
      'C':
        begin
          FieldDefinition := FieldDefinition + 'C255';
        end;
      'F':
        begin
          FieldDefinition := FieldDefinition + 'N18,10';
        end;
      'L':
        begin
          FieldDefinition := FieldDefinition + 'N';
        end;
      'N':
        begin
          FieldDefinition := FieldDefinition + 'N';
        end;
    else
      Assert(False);
    end;
    FieldDefinitions.Add(string(FieldDefinition));
  end;
end;

procedure TfrmExportShapefileObjects.CreateDataBase(
  FieldDefinitions: TStringList);
var
  DataBaseFileName: string;
begin
  DataBaseFileName := ChangeFileExt(sdShapefile.FileName, '.dbf');
  if FileExists(DataBaseFileName) then
  begin
    DeleteFile(DataBaseFileName);
  end;
  XBaseShapeFile.DBFCreate(DataBaseFileName, FieldDefinitions);
  XBaseShapeFile.FileName := DataBaseFileName;
  XBaseShapeFile.Active := True;
  XBaseShapeFile.GotoBOF;
end;

procedure TfrmExportShapefileObjects.DefineShapeGeometry(
  AScreenObject: TScreenObject; 
  BreakObject: boolean);
begin
  case FShapeType of
    stPoint:
      begin
        Assert(not BreakObject);
        DefinePointGeometry(AScreenObject);
      end;
    stMultiPoint:
      begin
        DefineMultipointGeometry(AScreenObject, BreakObject);
      end;
    stPolyLine:
      begin
        DefinePolylineGeometry(AScreenObject, BreakObject);
      end;
    stPolygon:
      begin
        if BreakObject or (AScreenObject.SectionCount = 1) then
        begin
          DefinePolygonGeometrySingleSection(AScreenObject, BreakObject);
        end
        else
        begin
          DefinePolygonGeometryMultipleSections(AScreenObject);
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmExportShapefileObjects.InitializeBreakScreenObjects;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  DataArrayIndex: Integer;
  DataArray: TDataArray;
  Position: integer;
  Formula: string;
  ImportedValues: TValueArrayStorage;
begin
  SetLength(FBreakScreenObjects, FSelectedScreenObjects.Count);
  for ScreenObjectIndex := 0 to FSelectedScreenObjects.Count - 1 do
  begin
    FBreakScreenObjects[ScreenObjectIndex].BreakObject := False;
  end;
  for ScreenObjectIndex := 0 to FSelectedScreenObjects.Count - 1 do
  begin
    ScreenObject := FSelectedScreenObjects[ScreenObjectIndex];
    if ScreenObject.SectionCount > 1 then
    begin
      for DataArrayIndex := 0 to FSelectedDataSets.Count - 1 do
      begin
        DataArray := FSelectedDataSets[DataArrayIndex];
        Position := ScreenObject.IndexOfDataSet(DataArray);
        if Position >= 0 then
        begin
          Formula := ScreenObject.DataSetFormulas[Position];
          ImportedValues := GetImportedValuesFromFormula(DataArray,
            ScreenObject, Formula);
          if (ImportedValues <> nil)
            and (ImportedValues.Count = ScreenObject.SectionCount) then
          begin
            FBreakScreenObjects[ScreenObjectIndex].BreakObject := True;
            break;
          end;
        end;
      end;
    end;
  end;
end;

function TfrmExportShapefileObjects.GetImportedValuesFromFormula(
  DataArray: TDataArray; ScreenObject: TScreenObject;
  Formula: string): TValueArrayStorage;
var
  ImportedLength: Integer;
  ImportedPosition: Integer;
begin
  ImportedPosition := 0;
  ImportedLength := 0;
  case DataArray.DataType of
    rdtDouble:
      begin
        ImportedPosition := Pos(rsObjectImportedValuesR, Formula);
        if ImportedPosition <> 0 then
        begin
          ImportedLength := Length(rsObjectImportedValuesR);
        end
        else
        begin
          ImportedPosition := Pos(rsObjectImportedValuesI, Formula);
          if ImportedPosition <> 0 then
          begin
            ImportedLength := Length(rsObjectImportedValuesI);
          end;
        end;
      end;
    rdtInteger:
      begin
        ImportedPosition := Pos(rsObjectImportedValuesI, Formula);
        if ImportedPosition <> 0 then
        begin
          ImportedLength := Length(rsObjectImportedValuesI);
        end;
      end;
    rdtBoolean:
      begin
        ImportedPosition := Pos(rsObjectImportedValuesB, Formula);
        if ImportedPosition <> 0 then
        begin
          ImportedLength := Length(rsObjectImportedValuesB);
        end;
      end;
    rdtString:
      begin
        ImportedPosition := Pos(rsObjectImportedValuesT, Formula);
        if ImportedPosition <> 0 then
        begin
          ImportedLength := Length(rsObjectImportedValuesT);
        end;
      end;
  else
    Assert(False);
  end;
  result := nil;
  if ImportedPosition <> 0 then
  begin
    // Strip off the "ImportedValue?" and the quotes.
    Formula := Copy(Formula, ImportedLength + 1, MAXINT);
    if Length(Formula) >= 4 then
    begin
      if (Formula[1] = '(') and (Formula[Length(Formula)] = ')') then
      begin
        Formula := Copy(Formula, 2, Length(Formula) - 2);
        if (Formula[1] = '"') and (Formula[Length(Formula)] = '"') then
        begin
          Formula := Copy(Formula, 2, Length(Formula) - 2);
          result := ScreenObject.ImportedValues.ValuesByName(Formula);
        end;
      end;
    end;
  end;
end;

procedure TfrmExportShapefileObjects.CreateShape(var Shape: TShapeObject);
begin
  Shape := TShapeObject.Create;
  Shape.FShapeType := FShapeType;
  FShapeFileWriter.AddShape(Shape);
end;

procedure TfrmExportShapefileObjects.InitializeDataBase;
var
  FieldNames: TStringList;
  DataArrayIndex: Integer;
  DataArray: TDataArray;
  FieldDefinitions: TStringList;
begin
  FieldNames := TStringList.Create;
  try
    FieldNames.Sorted := True;
    FieldNames.CaseSensitive := False;
    InitializeBreakScreenObjects;
    SetLength(FFieldDefinitions, FSelectedDataSets.Count);
    for DataArrayIndex := 0 to FSelectedDataSets.Count - 1 do
    begin
      DataArray := FSelectedDataSets[DataArrayIndex];
      FFieldDefinitions[DataArrayIndex].DataArray := DataArray;
      SetFieldType(DataArrayIndex);
      AssignFieldName(FieldNames, DataArrayIndex);
    end;
    FieldDefinitions := TStringList.Create;
    try
      FillFieldDefinitions(FieldDefinitions);
      CreateDataBase(FieldDefinitions);
    finally
      FieldDefinitions.Free;
    end;
  finally
    FieldNames.Free;
  end;
end;

procedure TfrmExportShapefileObjects.GetShapeType(var ShapeType: Integer);
begin
  ShapeType := -1;
  if rbPoints.Checked then
  begin
    ShapeType := stPoint;
  end
  else if rbMultipoint.Checked then
  begin
    ShapeType := stMultiPoint;
  end
  else if rbPolyline.Checked then
  begin
    ShapeType := stPolyLine;
  end
  else if rbPolygons.Checked then
  begin
    ShapeType := stPolygon;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TfrmExportShapefileObjects.AssignFieldValues(
  AScreenObject: TScreenObject; BreakObject: boolean);
var
  IntValue: Integer;
  FloatValue: Double;
  Formula: AnsiString;
  APosition: Integer;
  DataArray: TDataArray;
  FieldIndex: Integer;
  SectionIndex: Integer;
  ImportedValues: TValueArrayStorage;
begin
  if BreakObject then
  begin
    for SectionIndex := 0 to AScreenObject.SectionCount - 1 do
    begin
      XBaseShapeFile.AppendBlank;
      for FieldIndex := 0 to Length(FFieldDefinitions) - 1 do
      begin
        DataArray := FFieldDefinitions[FieldIndex].DataArray;
        APosition := AScreenObject.IndexOfDataSet(DataArray);
        FloatValue := FMissingValue;
        IntValue := FMissingValue;
        Formula := AnsiString(FMissingValueString);
        if APosition < 0 then
        begin
          case DataArray.DataType of
            rdtString:
              begin
                Formula := AnsiString(FMissingValueString);
              end;
            rdtDouble:
              begin
                FloatValue := FMissingValue;
              end;
            rdtBoolean:
              begin
                IntValue := FMissingValue;
              end;
            rdtInteger:
              begin
                IntValue := FMissingValue;
              end;
          else
            Assert(False);
          end;
        end
        else
        begin
          Formula := AnsiString(AScreenObject.DataSetFormulas[APosition]);
          ImportedValues := GetImportedValuesFromFormula(DataArray, AScreenObject, string(Formula));
          if (ImportedValues <> nil)
            and (ImportedValues.Count = AScreenObject.SectionCount) then
          begin
            case DataArray.DataType of
              rdtDouble:
                begin
                  FloatValue := ImportedValues.RealValues[SectionIndex];
                  Formula := AnsiString(FortranFloatToStr(FloatValue));
                end;
              rdtInteger:
                begin
                  IntValue := ImportedValues.IntValues[SectionIndex];
                  Formula := AnsiString(IntToStr(IntValue));
                end;
              rdtBoolean:
                begin
                  if ImportedValues.BooleanValues[SectionIndex] then
                  begin
                    IntValue := 1;
                    Formula := '1';
                  end
                  else
                  begin
                    IntValue := 0;
                    Formula := '0';
                  end;
                end;
              rdtString:
                begin
                  Formula := AnsiString(ImportedValues.StringValues[SectionIndex]);
                  if Length(Formula) >= 2 then
                  begin
                    if (Formula[1] = '"')
                      and (Formula[Length(Formula)] = '"')
                      and (PosEx('"', String(Formula), 2) = Length(Formula)) then
                    begin
                      Formula := Copy(Formula, 2, Length(Formula) - 2);
                    end;
                  end;
                end;
            end;
          end
          else
          begin
            case DataArray.DataType of
              rdtDouble:
                begin
                  if FFieldDefinitions[FieldIndex].FieldType = 'F' then
                  begin
                    FloatValue := StrToFloat(String(Formula));
                  end;
                end;
              rdtInteger:
                begin
                  if FFieldDefinitions[FieldIndex].FieldType = 'N' then
                  begin
                    IntValue := StrToInt(String(Formula));
                  end;
                end;
              rdtBoolean:
                begin
                  if FFieldDefinitions[FieldIndex].FieldType = 'N' then
                  begin
                    if SameText(String(Formula), 'True') then
                    begin
                      IntValue := 1;
                    end
                    else
                    begin
                      IntValue := 0;
                    end;
                  end;
                end;
              rdtString:
                begin
                  if Length(Formula) >= 2 then
                  begin
                    if (Formula[1] = '"')
                      and (Formula[Length(Formula)] = '"')
                      and (PosEx('"', String(Formula), 2) = Length(Formula)) then
                    begin
                      Formula := Copy(Formula, 2, Length(Formula) - 2);
                    end;
                  end;
                end;
            else
              Assert(False);
            end;
          end;
        end;
        case FFieldDefinitions[FieldIndex].FieldType of
          'C':
            begin
              XBaseShapeFile.UpdFieldStr(
                FFieldDefinitions[FieldIndex].FieldName, Formula);
              if Length(Formula) > 254 then
              begin
                frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel,
                  StrFormulaTruncatedTo,
                  ' Data set = ' + DataArray.Name
                  + '; Object = ' + AScreenObject.Name);
                FShowWarning := True;
              end;
            end;
          'F':
            begin
              XBaseShapeFile.UpdFieldNum(
                FFieldDefinitions[FieldIndex].FieldName, FloatValue);
            end;
    //      'L':
    //        begin
    //          XBaseShapeFile.UpdFieldInt(
    //            FFieldDefinitions[FieldIndex].FieldName, IntValue);
    //        end;
          'N':
            begin
              XBaseShapeFile.UpdFieldInt(
                FFieldDefinitions[FieldIndex].FieldName, IntValue);
            end;
        else
          Assert(False);
        end;
      end;
      XBaseShapeFile.PostChanges;
      XBaseShapeFile.GotoNext;
    end;
  end
  else
  begin
    XBaseShapeFile.AppendBlank;
    for FieldIndex := 0 to Length(FFieldDefinitions) - 1 do
    begin
      DataArray := FFieldDefinitions[FieldIndex].DataArray;
      APosition := AScreenObject.IndexOfDataSet(DataArray);
      FloatValue := FMissingValue;
      IntValue := FMissingValue;
      if APosition < 0 then
      begin
        case DataArray.DataType of
          rdtString:
            begin
              Formula := AnsiString(FMissingValueString);
            end;
          rdtDouble:
            begin
              FloatValue := FMissingValue;
            end;
          rdtBoolean:
            begin
              IntValue := FMissingValue;
            end;
          rdtInteger:
            begin
              IntValue := FMissingValue;
            end;
        else
          Assert(False);
        end;
      end
      else
      begin
        Formula := AnsiString(AScreenObject.DataSetFormulas[APosition]);
        case DataArray.DataType of
          rdtString:
            begin
              if Length(Formula) >= 2 then
              begin
                if (Formula[1] = '"')
                  and (Formula[Length(Formula)] = '"')
                  and (PosEx('"', string(Formula), 2) = Length(Formula)) then
                begin
                  Formula := Copy(Formula, 2, Length(Formula) - 2);
                end;
              end;
            end;
          rdtDouble:
            begin
              if FFieldDefinitions[FieldIndex].FieldType = 'F' then
              begin
                FloatValue := StrToFloat(string(Formula));
              end;
            end;
          rdtBoolean:
            begin
              if FFieldDefinitions[FieldIndex].FieldType = 'N' then
              begin
                if SameText(string(Formula), 'True') then
                begin
                  IntValue := 1;
                end
                else
                begin
                  IntValue := 0;
                end;
              end;
            end;
          rdtInteger:
            begin
              if FFieldDefinitions[FieldIndex].FieldType = 'N' then
              begin
                IntValue := StrToInt(string(Formula));
              end;
            end;
        else
          Assert(False);
        end;
      end;
      case FFieldDefinitions[FieldIndex].FieldType of
        'C':
          begin
            XBaseShapeFile.UpdFieldStr(
              FFieldDefinitions[FieldIndex].FieldName, Formula);
            if Length(Formula) > 254 then
            begin
              frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel,
                StrFormulaTruncatedTo,
                ' Data set = ' + DataArray.Name
                + '; Object = ' + AScreenObject.Name);
              FShowWarning := True;
            end;
          end;
        'F':
          begin
            XBaseShapeFile.UpdFieldNum(
              FFieldDefinitions[FieldIndex].FieldName, FloatValue);
          end;
  //      'L':
  //        begin
  //          XBaseShapeFile.UpdFieldInt(
  //            FFieldDefinitions[FieldIndex].FieldName, IntValue);
  //        end;
        'N':
          begin
            XBaseShapeFile.UpdFieldInt(
              FFieldDefinitions[FieldIndex].FieldName, IntValue);
          end;
      else
        Assert(False);
      end;
    end;
    XBaseShapeFile.PostChanges;
    XBaseShapeFile.GotoNext;
  end;
end;

procedure TfrmExportShapefileObjects.SetAllowableShapeTypes;
var
  AScreenObject: TScreenObject;
  Index: Integer;
  PolygonOk: Boolean;
  PointOK: Boolean;
  MultiPointPreferred: boolean;
begin
  PointOK := True;
  PolygonOk := True;
  MultiPointPreferred := True;
  for Index := 0 to FSelectedScreenObjects.Count - 1 do
  begin
    AScreenObject := FSelectedScreenObjects[Index];
    if AScreenObject.Count > 1 then
    begin
      PointOK := False;
    end;
    if PolygonOk and not AScreenObject.AllSectionsClosed then
    begin
      PolygonOk := False;
    end;
    if AScreenObject.Count <> AScreenObject.SectionCount then
    begin
      MultiPointPreferred := False;
    end;
  end;
  rbPoints.Enabled := PointOK;
  rbPolygons.Enabled := PolygonOk;
  if PointOK then
  begin
    rbPoints.Checked := True;
  end
  else if PolygonOk then
  begin
    rbPolygons.Checked := True;
  end
  else if MultiPointPreferred then
  begin
    rbMultipoint.Checked := True;
  end
  else
  begin
    rbPolyline.Checked := True;
  end;
       
end;

procedure TfrmExportShapefileObjects.FormCreate(Sender: TObject);
begin
  inherited;
  vstDataSets.Width := (vstDataSets.Width + vstObjects.Width) div 2;
  CenterLabels;

  vstDataSets.NodeDataSize := SizeOf(TClassificationNodeData);
  FObjectOwner := TObjectList.Create;
  FSelectedDataSets := TList.Create;
  FSelectedScreenObjects := TList.Create;

  GetData;
end;

procedure TfrmExportShapefileObjects.FormDestroy(Sender: TObject);
begin
  inherited;
  FObjectOwner.Free;
  FSelectedDataSets.Free;
  FSelectedScreenObjects.Free;
end;

procedure TfrmExportShapefileObjects.FormResize(Sender: TObject);
begin
  inherited;
  CenterLabels;
end;

procedure TfrmExportShapefileObjects.GetData;
var
  Node: PVirtualNode;
begin
  inherited;
  FillVirtualStringTreeWithDataSets(vstDataSets, FObjectOwner, nil);
  Node := vstDataSets.GetFirst;
  While Node <> nil do
  begin
    if vstDataSets.HasChildren[Node] then
    begin
      Node.CheckType := ctTriStateCheckBox;
    end
    else
    begin
      Node.CheckType := ctCheckBox;
    end;
    Node := vstDataSets.GetNext(Node)
  end;
end;

procedure TfrmExportShapefileObjects.HandleChecked(
  AScreenObject: TScreenObject);
begin
  if FSelectedScreenObjects.IndexOf(AScreenObject) < 0 then
  begin
    FSelectedScreenObjects.Add(AScreenObject)
  end;
end;

procedure TfrmExportShapefileObjects.HandleUnchecked(
  AScreenObject: TScreenObject);
var
  Position: integer;
begin
  Position := FSelectedScreenObjects.IndexOf(AScreenObject);
  if Position >= 0 then
  begin
    FSelectedScreenObjects.Delete(Position);
  end;
end;

function TfrmExportShapefileObjects.ShouldCheckBoxBeChecked(
  ScreenObject: TScreenObject): boolean;
begin
  result := ScreenObject.Visible;
end;

procedure TfrmExportShapefileObjects.Splitter1Moved(Sender: TObject);
begin
  inherited;
  CenterLabels;
end;

procedure TfrmExportShapefileObjects.vstDataSetsChecked(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PClassificationNodeData;
  ClassificationObject: TDataSetClassification;
  DataArray: TDataArray;
  AllObjectData: PMyRec;
  Index: Integer;
begin
  inherited;
  FSettingChecked := True;
  Data := Sender.GetNodeData(Node);
  if Data.ClassificationObject <> nil then
  begin
    if Data.ClassificationObject is TDataSetClassification then
    begin
      ClassificationObject := TDataSetClassification(Data.ClassificationObject);
      DataArray := ClassificationObject.DataArray;
      Assert(DataArray <> nil);
      if Sender.CheckState[Node] in [csCheckedNormal, csCheckedPressed] then
      begin
        FSelectedDataSets.Add(DataArray);
      end
      else
      begin
        FSelectedDataSets.Remove(DataArray);
      end;
    end;
  end;
  if CurrentNodeName = Data.ClassificationObject.ClassificationName then
  begin
    inherited GetData;
    AllObjectData := vstObjects.GetNodeData(FvstAllObjectsNode);
    for Index := FSelectedScreenObjects.Count - 1 downto 0 do
    begin
      if AllObjectData.ScreenObjects.IndexOf(
        FSelectedScreenObjects[Index]) < 0 then
      begin
        FSelectedScreenObjects.Delete(Index);
      end;
    end;
    SetCheckedNodes(vstObjects);
    FSettingChecked := False;
  end;
end;

procedure TfrmExportShapefileObjects.vstDataSetsChecking(
  Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState;
  var Allowed: Boolean);
var
  Data: PClassificationNodeData;
begin
  inherited;
  if FSettingChecked then
  begin
    Exit;
  end;
  Data := Sender.GetNodeData(Node);
  if Data.ClassificationObject <> nil then
  begin
    CurrentNodeName := Data.ClassificationObject.ClassificationName;
    if CurrentNodeName <> '' then
    begin
      FSettingChecked := True;
    end;
  end;
end;

procedure TfrmExportShapefileObjects.vstDataSetsGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  Data: PClassificationNodeData;
begin
  inherited;
  if csDestroying in ComponentState then
  begin
    CellText := '';
    Exit;
  end;
  // A handler for the OnGetText event is always needed
  // as it provides the tree with the string data to display.
  // Note that we are now  using string instead of WideString.
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    CellText := Data.ClassificationObject.ClassificationName;
  end;
end;

procedure TfrmExportShapefileObjects.vstObjectsChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  inherited;
  if FSettingData or FSettingData2 or FSettingData3 then
  begin
    Exit;
  end;
  if (Node.Parent = nil) then
  begin
    Exit;
  end;
  if not FOkToDoCheck then
  begin
    Exit;
  end;
  FSettingData := True;
  Sender.BeginUpdate;
  try
    HandleCheckChange(Node, Sender);
    SetCheckedNodes(Sender);
    SetAllowableShapeTypes;
  finally
    Sender.EndUpdate;
    FSettingData := False;
  end;
end;

end.
