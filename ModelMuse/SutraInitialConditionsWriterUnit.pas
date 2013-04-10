unit SutraInitialConditionsWriterUnit;

interface

uses
  CustomModflowWriterUnit, SysUtils, DataSetUnit, PhastModelUnit,
  Generics.Collections;

type
  TDataValue = class(TObject)
    Number: integer;
    Value: double;
  end;

  TDataValueList = TObjectList<TDataValue>;

  TSutraInitialConditionsWriter = class(TCustomFileWriter)
  private
    procedure WriteDataArray(DataArray: TDataArray);
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
  public
    Constructor Create(AModel: TCustomModel); reintroduce;
    procedure WriteFile(FileName: string);
  end;

implementation

uses
  GoPhastTypes, SutraOptionsUnit, SutraMeshUnit, Generics.Defaults,
  SutraFileWriterUnit;



{ TSutraInitialConditionsWriter }

constructor TSutraInitialConditionsWriter.Create(AModel: TCustomModel);
begin
  inherited Create(AModel, etExport);
end;

procedure TSutraInitialConditionsWriter.WriteDataArray(DataArray: TDataArray);
var
  List: TDataValueList;
  index: Integer;
  NodeIndex: Integer;
  Mesh2D: TSutraMesh2D;
  LayerIndex: Integer;
  Mesh3D: TSutraMesh3D;
  Node3D: TSutraNode3D;
  DataValue: TDataValue;
begin
  Assert(DataArray.EvaluatedAt = eaNodes);
  DataArray.Initialize;
  if DataArray.IsUniform = iuTrue then
  begin
    WriteString('''UNIFORM''');
    NewLine;
    WriteFloat(DataArray.UniformRealValue);
    NewLine;
  end
  else
  begin
    WriteString('''NONUNIFORM''');
    NewLine;
    List := TDataValueList.Create;
    try

      case Model.SutraMesh.MeshType of
        mt2D:
          begin
            Mesh2D := Model.SutraMesh.Mesh2D;
            for NodeIndex := 0 to Mesh2D.Nodes.Count - 1 do
            begin
              DataValue := TDataValue.Create;
              List.Add(DataValue);
              DataValue.Value := DataArray.RealData[0,0,NodeIndex];
              DataValue.Number := Mesh2D.Nodes[NodeIndex].Number;
            end;
          end;
        mt3D:
          begin
            Mesh3D := Model.SutraMesh;
            Mesh2D := Mesh3D.Mesh2D;
            for LayerIndex := 0 to Mesh3D.LayerCount do
            begin
              for NodeIndex := 0 to Mesh2D.Nodes.Count - 1 do
              begin
                Node3D := Mesh3D.NodeArray[LayerIndex, NodeIndex];
                if Node3D.Active then
                begin
                  DataValue := TDataValue.Create;
                  List.Add(DataValue);
                  DataValue.Value := DataArray.RealData[LayerIndex,0,NodeIndex];
                  DataValue.Number := Node3D.Number;
                end;
              end;
            end;
          end;
        else
          Assert(False);
      end;
      List.Sort(TComparer<TDataValue>.Construct(
        function (const L, R: TDataValue): integer
        begin
          result := L.Number - R.Number;
        end));
      for index := 0 to List.Count - 1 do
      begin
        DataValue := List[index];
        WriteFloat(DataValue.Value);
        if ((index + 1) mod 10) = 0 then
        begin
          NewLine;
        end;
      end;
      if (List.Count  mod 10) <> 0 then
      begin
        NewLine;
      end;
    finally
      List.Free;
    end;
  end;

  Model.DataArrayManager.AddDataSetToCache(DataArray);
  Model.DataArrayManager.CacheDataArrays;
end;

procedure TSutraInitialConditionsWriter.WriteDataSet0;
begin
  WriteCommentLine(File_Comment('SUTRA initial conditions file'));
end;

procedure TSutraInitialConditionsWriter.WriteDataSet1;
var
  TICS: double;
begin
  WriteCommentLine('Data set 1');

  TICS := (Model as TPhastModel).SutraTimeOptions.InitialTime;
  WriteFloat(TICS);
  NewLine;
end;

procedure TSutraInitialConditionsWriter.WriteDataSet2;
var
  InitialPressure: TDataArray;
begin
  WriteCommentLine('Data set 2');

  InitialPressure := nil;
  case (Model as TPhastModel).SutraOptions.TransportChoice of
    tcSolute, tcEnergy:
      InitialPressure := Model.DataArrayManager.GetDataSetByName(KInitialPressure);
    tcSoluteHead:
      InitialPressure := Model.DataArrayManager.GetDataSetByName(rsInitial_Head);
    else
      Assert(False);
  end;

  WriteDataArray(InitialPressure);
end;

procedure TSutraInitialConditionsWriter.WriteDataSet3;
var
  InitialU: TDataArray;
begin
  WriteCommentLine('Data set 3');

  InitialU := nil;
  case Model.SutraOptions.TransportChoice of
    tcSolute, tcSoluteHead: InitialU := Model.DataArrayManager.GetDataSetByName(
      KInitialConcentration);
    tcEnergy: InitialU := Model.DataArrayManager.GetDataSetByName(
      KInitialTemperature);
    else
      Assert(False);
  end;
  WriteDataArray(InitialU);
end;

procedure TSutraInitialConditionsWriter.WriteFile(FileName: string);
var
  Options: TSutraOptions;
begin
  Options := Model.SutraOptions;
  if (Options.StartType = stWarm) and (Options.RestartFileName <> '') then
  begin
    SutraFileWriter.AddFile(sftIcs, Options.RestartFileName);
  end
  else
  begin
    FileName := ChangeFileExt(FileName, '.ics');
    OpenFile(FileName);
    try
      WriteDataSet0;
      WriteDataSet1;
      WriteDataSet2;
      WriteDataSet3;
      SutraFileWriter.AddFile(sftIcs, FileName);
    finally
      CloseFile;
    end;
  end;

end;

end.
