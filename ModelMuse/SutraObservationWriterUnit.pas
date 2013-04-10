unit SutraObservationWriterUnit;

interface

uses
  CustomModflowWriterUnit, Generics.Collections, PhastModelUnit, FastGEO,
  GoPhastTypes, SysUtils, SutraBoundariesUnit;

type
  TObsLocation = class(TObject)
  strict private
    FLocation: TPoint3d;
  private
    function GetCenter2D: TPoint2D;
    procedure SetCenter2D(const Value: TPoint2D);
  public
    property X: double read FLocation.x write FLocation.x;
    property Y: double read FLocation.y write FLocation.y;
    property Z: double read FLocation.Z write FLocation.Z;
    property Location: TPoint3d read FLocation write FLocation;
    property Center2D: TPoint2D read GetCenter2D write SetCenter2D;
  end;

  TObsLocationList = TObjectList<TObsLocation>;

  TObsGroup = class(TObject)
  private
    FLocations: TObsLocationList;
    FObsName: AnsiString;
    FObsSchedule: AnsiString;
    FTimeValues: TRealCollection;
    FObservationFormat: TObservationFormat;
    function GetObsName: AnsiString;
    procedure SetObsName(const Value: AnsiString);
    procedure SetTimeValues(const Value: TRealCollection);
    procedure SetObservationFormat(const Value: TObservationFormat);
  public
    constructor Create;
    destructor Destroy; override;
    property ObsName: AnsiString read GetObsName Write SetObsName;
    property ObsSchedule: AnsiString read FObsSchedule write FObsSchedule;
    property TimeValues: TRealCollection read FTimeValues write SetTimeValues;
    property ObservationFormat: TObservationFormat read FObservationFormat write SetObservationFormat;
  end;

  TObsGroupList = TObjectList<TObsGroup>;

  TSutraObservationWriter = class(TCustomFileWriter)
  private
    FObsGroups: TObsGroupList;
    FNOBS: Integer;
    FUsedFormats: TObservationFormats;
    procedure Evaluate;
    procedure WriteDataSet1;
    procedure WriteObservationGroups;
  public
    Constructor Create(AModel: TCustomModel;
      EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(FileName: string; out NOBS: integer);
  end;

implementation

uses
  ScreenObjectUnit, SparseDataSets, OctTreeClass,
  SutraMeshUnit, SutraOutputControlUnit, SutraFileWriterUnit;

{ TSutraObservationWriter }

constructor TSutraObservationWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FObsGroups := TObsGroupList.Create;
  FUsedFormats := [];
end;

destructor TSutraObservationWriter.Destroy;
begin
  FObsGroups.Free;
  inherited;
end;

procedure TSutraObservationWriter.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Observations: TSutraObservations;
  CellList: TCellAssignmentList;
  Mesh: TSutraMesh3D;
  CellIndex: Integer;
  ACell: TCellAssignment;
  AnObsGroup: TObsGroup;
  OctTree: TRbwOctTree;
  Obs: TObsLocation;
  X, Y, Z: double;
  AnElement: TSutraElement3D;
  ANode: TSutraNode3D;
  APointer: Pointer;
  AnElement2D: TSutraElement2D;
  ANode2D: TSutraNode2D;
//  OriginalY: Double;
//  OriginalZ: Double;
begin
  OctTree := TRbwOctTree.Create(nil);
  try
    FObsGroups.Clear;
    Mesh := Model.SutraMesh;
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      Observations := ScreenObject.SutraBoundaries.Observations;
      if Observations.Used then
      begin
        AnObsGroup := TObsGroup.Create;
        FObsGroups.Add(AnObsGroup);
        AnObsGroup.ObsName := Observations.ObservationName;
        AnObsGroup.ObservationFormat := Observations.ObservationFormat;
        AnObsGroup.ObsSchedule := AnsiString(Observations.ExportScheduleName);
        AnObsGroup.TimeValues := Observations.Times;

        CellList := TCellAssignmentList.Create;
        try
          ScreenObject.GetCellsToAssign(Mesh, '0', nil, nil, CellList,
            alAll, Model);
          if (ScreenObject.SectionCount = ScreenObject.Count)
            and (ScreenObject.ViewDirection = vdTop)
            and ((ScreenObject.ElevationCount = ecOne)
            or (Mesh.MeshType = mt2D))
          then
          begin
            // make observations at the exact locations of the points
            // in the objects.
            OctTree.Clear;

            for CellIndex := 0 to CellList.Count - 1 do
            begin
              ACell := CellList[CellIndex];

              if Mesh.MeshType = mt3D then
              begin
                case ScreenObject.EvaluatedAt of
                  eaBlocks:
                    begin
                      AnElement := Mesh.ElementArray[ACell.Layer, ACell.Column];
                      if not AnElement.Active then
                      begin
                        Continue;
                      end;
                    end;
                  eaNodes:
                    begin
                      ANode := Mesh.NodeArray[ACell.Layer, ACell.Column];
                      if not ANode.Active then
                      begin
                        Continue;
                      end;
                    end;
                end;
              end;

              X := ACell.SutraX;
              Y := ACell.SutraY;
              Z := ACell.SutraZ;
              Obs := nil;
              if OctTree.Count > 0 then
              begin
                OctTree.FirstNearestPoint(X, Y, Z, APointer);
                if (X <> ACell.SutraX) or (Y <> ACell.SutraY) or
                  (Z <> ACell.SutraZ) then
                begin
                  Obs := nil;
                end
                else
                begin
                  Obs := APointer;
                end;
              end;
              if Obs = nil then
              begin
                Obs := TObsLocation.Create;
                Obs.X := ACell.SutraX;
                Obs.Y := ACell.SutraY;
                Obs.Z := ACell.SutraZ;
                OctTree.AddPoint(Obs.X, Obs.Y, Obs.Z, Obs);
                AnObsGroup.FLocations.Add(Obs);
              end;
            end;
          end
          else
          begin
            // make observations at nodes or at the centers of elements.
            for CellIndex := 0 to CellList.Count - 1 do
            begin
              ACell := CellList[CellIndex];
              case ScreenObject.EvaluatedAt of
                eaBlocks:
                  begin
                    case Mesh.MeshType of
                      mt2D:
                        begin
                          AnElement2D := Mesh.Mesh2D.Elements[ACell.Column];
                          Obs := TObsLocation.Create;
                          AnObsGroup.FLocations.Add(Obs);
                          Obs.Center2D := AnElement2D.Center;
                        end;
                      mt3D:
                        begin
                          AnElement := Mesh.ElementArray[ACell.Layer, ACell.Column];
                          if AnElement.Active then
                          begin
                            Obs := TObsLocation.Create;
                            AnObsGroup.FLocations.Add(Obs);
                            Obs.Location := AnElement.CenterLocation;
                          end;
                        end;
                      else Assert(False);
                    end;
                  end;
                eaNodes:
                  begin
                    case Mesh.MeshType of
                      mt2D:
                        begin
                          ANode2D := Mesh.Mesh2D.Nodes[ACell.Column];
                          Obs := TObsLocation.Create;
                          AnObsGroup.FLocations.Add(Obs);
                          Obs.Center2D := ANode2D.Location;
                        end;
                      mt3D:
                        begin
                          ANode := Mesh.NodeArray[ACell.Layer, ACell.Column];
                          if ANode.Active then
                          begin
                            Obs := TObsLocation.Create;
                            AnObsGroup.FLocations.Add(Obs);
                            Obs.Location := ANode.NodeLocation
                          end;
                        end;
                      else Assert(False);
                    end;
                  end;
              else
                Assert(False);
              end;
            end;
          end;
        finally
          CellList.Free;
        end;
      end;
    end;
  finally
    OctTree.Free;
  end;
end;

procedure TSutraObservationWriter.WriteDataSet1;
var
  OutputControl: TSutraOutputControl;
  NOBLIN: Integer;
begin
  OutputControl := Model.SutraOutputControl;
  NOBLIN := OutputControl.MaxObsPerLine;
  WriteInteger(NOBLIN);
  NewLine;
end;

procedure TSutraObservationWriter.WriteFile(FileName: string; out NOBS: integer);
var
  ObsRoot: string;
begin
  Evaluate;

  FileName := ChangeFileExt(FileName, '.8d');
  OpenFile(FileName);
  try
    WriteDataSet1;
    WriteObservationGroups;
    ObsRoot := ChangeFileExt(FileName, '');
    if ofOBS in FUsedFormats then
    begin
      SutraFileWriter.AddFile(sftObs, ObsRoot);
    end;
    if ofOBC in FUsedFormats then
    begin
      SutraFileWriter.AddFile(sftObc, ObsRoot);
    end;
    Model.AddModelInputFile(FileName);
  finally
    CloseFile;
  end;
  NOBS := FNOBS;
end;

procedure TSutraObservationWriter.WriteObservationGroups;
const
  MaxObsNameLength = 40;
var
  GroupIndex: Integer;
  Group: TObsGroup;
  LocationIndex: Integer;
  ObsLocation: TObsLocation;
  OBSNAM: AnsiString;
//  ObsLength: Integer;
//  CharIndex: Integer;
  OBSSCH: AnsiString;
  OBSFMT: AnsiString;
  MeshType: TMeshType;
begin
  FNOBS := 0;
  MeshType := Model.SutraMesh.MeshType;
  for GroupIndex := 0 to FObsGroups.Count - 1 do
  begin
    Group := FObsGroups[GroupIndex];

    OBSNAM := Group.ObsName;
//    ObsLength := Length(OBSNAM);
    OBSNAM := '''' + OBSNAM + ''' ';

    OBSSCH := '''' + Group.ObsSchedule + ''' ';

    case Group.ObservationFormat of
      ofOBS: OBSFMT := ' ''OBS''';
      ofOBC: OBSFMT := ' ''OBC''';
      else Assert(False);
    end;
    Include(FUsedFormats, Group.ObservationFormat);

    for LocationIndex := 0 to Group.FLocations.Count - 1 do
    begin
      ObsLocation := Group.FLocations[LocationIndex];

      WriteString(OBSNAM);

      WriteFloat(ObsLocation.X);
      WriteFloat(ObsLocation.Y);
      if MeshType = mt3D then
      begin
        WriteFloat(ObsLocation.Z);
      end;

      WriteString(' ');
      WriteString(OBSSCH);
      WriteString(OBSFMT);
      NewLine;
      Inc(FNOBS);
    end;
  end;
  WriteString('-');
  NewLine;
end;

{ TObsGroup }

constructor TObsGroup.Create;
begin
  FLocations := TObsLocationList.Create;
  FTimeValues:= TRealCollection.Create(nil);
end;

destructor TObsGroup.Destroy;
begin
  FTimeValues.Free;
  FLocations.Free;
  inherited;
end;

function TObsGroup.GetObsName: AnsiString;
begin
  result := FObsName;
end;

procedure TObsGroup.SetObservationFormat(const Value: TObservationFormat);
begin
  FObservationFormat := Value;
end;

procedure TObsGroup.SetObsName(const Value: AnsiString);
const
  MaxObsNameLength = 40;
begin
  FObsName := Copy(Value, 1, MaxObsNameLength);
end;

procedure TObsGroup.SetTimeValues(const Value: TRealCollection);
begin
  FTimeValues.Assign(Value);
end;

{ TObsLocation }

function TObsLocation.GetCenter2D: TPoint2D;
begin
  Result.x := FLocation.x;
  Result.y := FLocation.y;
end;

procedure TObsLocation.SetCenter2D(const Value: TPoint2D);
begin
  FLocation.x := Value.x;
  FLocation.y := Value.y;
end;

end.
