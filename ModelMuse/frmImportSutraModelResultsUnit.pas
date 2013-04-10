unit frmImportSutraModelResultsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  CheckLst, Buttons, ExtCtrls, ReadSutraNodEleUnit, ScreenObjectUnit,
  frmImportShapefileUnit;

type
  TImportItems = (iiPressure, iiU, iiSaturation, iiXVel, iiYVel, iiZVel);
const
  FirstNodeItem = iiPressure;
  LastNodeItem = iiSaturation;
  FirstElementItem = iiXVel;
  LastElementItem = iiZVel;

type
  TUndoImportSutraResults = class(TUndoImportShapefile)
  protected
    function Description: string; override;
  end;

  TfrmImportSutraModelResults = class(TfrmCustomGoPhast)
    chklstTimeStepsToImport: TCheckListBox;
    lblTimeStepsToImport: TLabel;
    chklstDataToImport: TCheckListBox;
    lblDataToImport: TLabel;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    dlgOpenSutraFile: TOpenDialog;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
  private
    FNodeReader: TNodReader;
    FEleReader: TEleReader;
    FResultList: TStoredResultsList;
    procedure GetData;
    procedure SetData;
    procedure CreateNodeScreenObject(out ScreenObject: TScreenObject);
    procedure CreateElementScreenObject(out ScreenObject: TScreenObject);
    procedure CreateNodeDataSets(StepIndex: Integer; NewDataSets: TList);
    procedure CreateElementDataSets(StepIndex: Integer; NewDataSets: TList);
    procedure AssignNodeValues(NewDataSets: TList; AScreenObject: TScreenObject);
    procedure AssignElementValues(NewDataSets: TList; AScreenObject: TScreenObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmImportSutraModelResults: TfrmImportSutraModelResults;

var
  SutraNodeResults: string;
  SutraElementResults: string;

implementation

uses
  frmGoPhastUnit, SutraOptionsUnit, SutraMeshUnit, IntListUnit,
  SutraInputWriterUnit, UndoItems, GoPhastTypes, FastGEO, GIS_Functions,
  DataSetUnit, RbwParser, PhastModelUnit, frmSelectResultToImportUnit,
  ValueArrayStorageUnit;

{$R *.dfm}

resourcestring
  StrUnableToOpen0s = 'Unable to open %0:s. Error message = %1:s';
  StrOnlyNodAndEleF = 'Only .nod and .ele files may be selected.';
  StrConcentration = 'Concentration';
  StrTemperature = 'Temperature';
  StrTheNodAndEleFi = 'The .nod and .ele files contain no data.';
  StrTheNumbersOfNodes = 'The numbers of nodes or elements in the files do n' +
  'ot much the numbers of nodesl or element in the model. Do you want to att' +
  'empt to import the data anyway?';
  StrImportSUTRAModelR = 'import SUTRA model results';
  StrYouMustSpecifyAt = 'You must specify at least one type of data and one ' +
  'time step.';
  StrUnableToImportNod = 'Unable to import node data because the node locati' +
  'ons were not saved in the .nod file.';
  StrUnableToImportEle = 'Unable to import element data because the element locati' +
  'ons were not saved in the .nod file.';


{ TfrmImportSutraModelResults }

procedure TfrmImportSutraModelResults.btnOKClick(Sender: TObject);
var
  index: Integer;
  OK: Boolean;
begin
  inherited;
  OK := False;
  for index := 0 to chklstDataToImport.Items.Count - 1 do
  begin
    OK := chklstDataToImport.Checked[index];
    if OK then
    begin
      break;
    end;
  end;
  if OK then
  begin
    OK := False;
    for index := 0 to chklstTimeStepsToImport.Items.Count - 1 do
    begin
      OK := chklstTimeStepsToImport.Checked[index];
      if OK then
      begin
        break;
      end;
    end;
  end;
  if OK then
  begin
    SetData;
  end
  else
  begin
    Beep;
    MessageDlg(StrYouMustSpecifyAt, mtWarning, [mbOK], 0);
    ModalResult := mrNone;
  end;

end;

procedure TfrmImportSutraModelResults.FormCreate(Sender: TObject);
begin
  inherited;
  chklstDataToImport.CheckAll(cbChecked);
  case frmGoPhast.PhastModel.SutraOptions.TransportChoice of
    tcSolute, tcSoluteHead:
      begin
        chklstDataToImport.Items[Ord(iiU)] := StrConcentration;
      end;
    tcEnergy:
      begin
        chklstDataToImport.Items[Ord(iiU)] := StrTemperature;
      end;
    else Assert(False);
  end;
  if frmGoPhast.PhastModel.SutraMesh.MeshType = mt2D then
  begin
    chklstDataToImport.ItemEnabled[Ord(iiZVel)]  := False;
    chklstDataToImport.Checked[Ord(iiZVel)]  := False;
  end;
  GetData;
end;

procedure TfrmImportSutraModelResults.FormDestroy(Sender: TObject);
begin
  inherited;
  FNodeReader.Free;
  FEleReader.Free;
  FResultList.Free;
end;

procedure TfrmImportSutraModelResults.GetData;
var
  FileName: TFileName;
  NodeFileName: TFileName;
  ElementFileName: string;
  Extension: string;
  ItemIndex: Integer;
  Mesh: TSutraMesh3D;
  ShowWarning: Boolean;
begin
  if frmGoPhast.PhastModel.ModelFileName <> '' then
  begin
    dlgOpenSutraFile.FileName := ChangeFileExt(
      frmGoPhast.PhastModel.ModelFileName, '.nod');
  end;
  if dlgOpenSutraFile.Execute then
  begin
    FileName := dlgOpenSutraFile.FileName;
    Extension := LowerCase(ExtractFileExt(FileName));
    if Extension = '.nod' then
    begin
      NodeFileName := FileName;
      ElementFileName := ChangeFileExt(FileName, '.ele');
    end
    else if Extension = '.ele' then
    begin
      NodeFileName := ChangeFileExt(FileName, '.nod');
      ElementFileName := FileName;
    end
    else
    begin
      NodeFileName := '';
      ElementFileName := '';
      Beep;
      MessageDlg(StrOnlyNodAndEleF, mtError, [mbOK], 0);
      ModalResult := mrOk;
      Exit;
    end;
    try
      FNodeReader := TNodReader.Create(NodeFileName);
    except on E: EInOutError do
      begin
        Beep;
        MessageDlg(Format(StrUnableToOpen0s, [NodeFileName, E.message]),
          mtWarning, [mbOK], 0);
        ModalResult := mrOk;
        Exit;
      end;
    end;
    try
      FEleReader := TEleReader.Create(ElementFileName);
    except on E: EInOutError do
      begin
        Beep;
        MessageDlg(Format(StrUnableToOpen0s, [ElementFileName, E.message]),
          mtWarning, [mbOK], 0);
        ModalResult := mrOk;
        Exit;
      end;
    end;

    FResultList := TStoredResultsList.Create;
    FResultList.AddRange(FNodeReader.StoredResults);
    FResultList.AddRange(FEleReader.StoredResults);
    if FResultList.Count = 0 then
    begin
      Beep;
      MessageDlg(StrTheNodAndEleFi, mtWarning, [mbOK], 0);
      Exit;
    end;

    FResultList.Sort(TStoredResultsComparer.Construct(
      function (const L, R: TStoredResults): integer
       begin
         result := L.TimeStep - R.TimeStep;
       end));

    for ItemIndex := FResultList.Count - 1 downto 1 do
    begin
      if FResultList[ItemIndex].TimeStep = FResultList[ItemIndex-1].TimeStep then
      begin
        FResultList.Delete(ItemIndex);
      end;
    end;

    chklstTimeStepsToImport.Items.Capacity := FResultList.Count;
    for ItemIndex := 0 to FResultList.Count - 1 do
    begin
      chklstTimeStepsToImport.Items.Add(Format('Step: %0:d; Time: %1:g',
        [FResultList[ItemIndex].TimeStep, FResultList[ItemIndex].Time]));
    end;
    chklstTimeStepsToImport.Checked[chklstTimeStepsToImport.items.Count-1] :=
      True;

    ShowWarning := False;
    Mesh := frmGoPhast.PhastModel.SutraMesh;
    case Mesh.MeshType of
      mt2D:
        begin
          ShowWarning := (Mesh.Mesh2D.Nodes.Count <> FNodeReader.Count)
            or (Mesh.Mesh2D.Elements.Count <> FEleReader.Count)
        end;
      mt3D:
        begin
          ShowWarning := (Mesh.ActiveNodeCount <> FNodeReader.Count)
            or (Mesh.ActiveElementCount <> FEleReader.Count)
        end;
      else Assert(False);
    end;
    if ShowWarning then
    begin
      Beep;
      if (MessageDlg(StrTheNumbersOfNodes, mtWarning, [mbYes, mbNo], 0)
        <> mrYes) then
      begin
        ModalResult := mrOk;
        Exit;
      end;
    end;

  end
  else
  begin
    ModalResult := mrOk;
  end;
end;

procedure TfrmImportSutraModelResults.CreateNodeScreenObject(
  Out ScreenObject: TScreenObject);
var
  UndoCreateScreenObject: TCustomUndo;
  NeedLocations: Boolean;
  Mesh: TSutraMesh3D;
  NodeIndex: Integer;
  ANode2D: TSutraNode2D;
  NodeData: TNodeData;
  LayerIndex: Integer;
  ANode3D: TSutraNode3D;
  Nodes: TNodeDataList;
  APoint: TPoint2D;
  Z: Extended;
begin
  ScreenObject := nil;
  Mesh := frmGoPhast.PhastModel.SutraMesh;
  Nodes := TNodeDataList.Create;
  try
    NeedLocations := False;
    case Mesh.MeshType of
      mt2D:
        begin
          NeedLocations := (Mesh.Mesh2D.Nodes.Count <> FNodeReader.Count);
          if NeedLocations then
          begin
            if (Length(FNodeReader.X) = 0) or (Length(FNodeReader.Y) = 0)  then
            begin
              Beep;
              MessageDlg(StrUnableToImportNod, mtError, [mbOK], 0);
              Exit;
            end;
          end;
          Nodes.Capacity := Mesh.Mesh2D.Nodes.Count;
          for NodeIndex := 0 to Mesh.Mesh2D.Nodes.Count - 1 do
          begin
            ANode2D := Mesh.Mesh2D.Nodes[NodeIndex];
            NodeData := TNodeData.Create;
            Nodes.Add(NodeData);
            NodeData.Number := ANode2D.Number;
            NodeData.NREG := 0;
            NodeData.X := ANode2D.X;
            NodeData.Y := ANode2D.Y;
            NodeData.Z := 0;
            NodeData.Porosity := 0
          end;
        end;
      mt3D:
        begin
          NeedLocations := (Mesh.ActiveNodeCount <> FNodeReader.Count);
          if NeedLocations then
          begin
            if (Length(FNodeReader.X) = 0)
              or (Length(FNodeReader.Y) = 0)
              or (Length(FNodeReader.Z) = 0)  then
            begin
              Beep;
              MessageDlg(StrUnableToImportNod, mtError, [mbOK], 0);
              Exit;
            end;
          end;
          Nodes.Capacity := Mesh.ActiveNodeCount;
          for LayerIndex := 0 to Mesh.LayerCount do
          begin
            for NodeIndex := 0 to Mesh.Mesh2D.Nodes.Count - 1 do
            begin
              ANode3D := Mesh.NodeArray[LayerIndex,NodeIndex];
              if ANode3D.Active then
              begin
                NodeData := TNodeData.Create;
                Nodes.Add(NodeData);
                NodeData.Number := ANode3D.Number;
                NodeData.NREG := 0;
                NodeData.X := ANode3D.X;
                NodeData.Y := ANode3D.Y;
                NodeData.Z := ANode3D.Z;
                NodeData.Porosity := 0;
              end;
            end;
          end
        end;
      else Assert(False);
    end;

    if Nodes.Count > 0 then
    begin
      Nodes.Sort(TNodeDataComparer.Construct(
        function (const L, R: TNodeData): integer
        begin
          result := L.Number - R.Number;
        end));
    end;

    ScreenObject := TScreenObject.CreateWithViewDirection(
      frmGoPhast.PhastModel, vdTop,
      UndoCreateScreenObject, False);
    ScreenObject.SetPropertiesOfIntersectedCells := True;
    ScreenObject.EvaluatedAt := eaNodes;
    ScreenObject.Visible := False;
    case Mesh.MeshType of
      mt2D: ScreenObject.ElevationCount := ecZero;
      mt3D:
        begin
          ScreenObject.ElevationCount := ecOne;
          ScreenObject.ElevationFormula :=
            rsObjectImportedValuesR
            + '("' + StrImportedElevations + '")';

        end;
      else Assert(False);
    end;

    for NodeIndex := 0 to FNodeReader.Count - 1 do
    begin
      if (Length(FNodeReader.X) = 0) or not NeedLocations then
      begin
        // The X coordinates were NOT stored in the .node file
        APoint.X := Nodes[NodeIndex].X;
      end
      else
      begin
        // The X coordinates WERE stored in the .node file
        APoint.X := FNodeReader.X[NodeIndex];
      end;
      if (Length(FNodeReader.Y) = 0) or not NeedLocations then
      begin
        // The Y coordinates were NOT stored in the .node file
        APoint.Y := Nodes[NodeIndex].Y;
      end
      else
      begin
        // The Y coordinates WERE stored in the .node file
        APoint.Y := FNodeReader.Y[NodeIndex];
      end;
      ScreenObject.AddPoint(APoint, True);


      if ScreenObject.ElevationCount = ecOne then
      begin
        // 3D model
        if (Length(FNodeReader.Z) = 0) or not NeedLocations then
        begin
          // The Z coordinates were NOT stored in the .node file
          Z := Nodes[NodeIndex].Z;
        end
        else
        begin
          // The Z coordinates WERE stored in the .node file
          Z := FNodeReader.Z[NodeIndex];
        end;
        ScreenObject.ImportedSectionElevations.Add(Z);
      end;
    end;
  finally
    Nodes.Free;
  end;

end;

procedure TfrmImportSutraModelResults.CreateElementDataSets(StepIndex: Integer;
  NewDataSets: TList);
var
  index: TImportItems;
  NewName: string;
  DataSet: TDataArray;
  NewFormula: string;
  NewDataType: TRbwDataType;
begin
  for index := FirstElementItem to LastElementItem do
  begin
    if chklstDataToImport.Checked[Ord(index)] then
    begin
      case index of
        iiXVel:
          begin
            if Length(FEleReader.FXVelocity) = 0 then
            begin
              Continue;
            end;
          end;
        iiYVel:
          begin
            if Length(FEleReader.FYVelocity) = 0 then
            begin
              Continue;
            end;
          end;
        iiZVel:
          begin
            if Length(FEleReader.FZVelocity) = 0 then
            begin
              Continue;
            end;
          end;
        else
          Assert(False);
      end;
      NewName := GenerateNewName(chklstDataToImport.Items[Ord(index)] + '_'
        + IntToStr(FResultList[StepIndex].TimeStep));

      NewDataType := rdtDouble;
      NewFormula := '0.';

      DataSet := frmGoPhast.PhastModel.DataArrayManager.CreateNewDataArray(
        TDataArray, NewName, NewFormula, NewName, [], NewDataType,
        eaBlocks, dso3D, SutraElementResults);

      NewDataSets.Add(DataSet);
      frmGoPhast.PhastModel.UpdateDataArrayDimensions(DataSet);

      DataSet.Units := '';

    end;
  end;
end;

procedure TfrmImportSutraModelResults.CreateElementScreenObject(
  out ScreenObject: TScreenObject);
var
  UndoCreateScreenObject: TCustomUndo;
  NeedLocations: Boolean;
  Mesh: TSutraMesh3D;
  ElementIndex: Integer;
  AnElement2D: TSutraElement2D;
  ElementData: TElementData;
  LayerIndex: Integer;
  AnElement3D: TSutraElement3D;
  Elements: TElementDataList;
  APoint: TPoint2D;
  Z: Extended;
  CenterPoint: TPoint2D;
  CenterLocation: TPoint3D;
begin
  ScreenObject := nil;
  Mesh := frmGoPhast.PhastModel.SutraMesh;
  Elements := TElementDataList.Create;
  try
    NeedLocations := False;
    case Mesh.MeshType of
      mt2D:
        begin
          NeedLocations := (Mesh.Mesh2D.Elements.Count <> FEleReader.Count);
          if NeedLocations then
          begin
            if (Length(FEleReader.X) = 0) or (Length(FEleReader.Y) = 0)  then
            begin
              Beep;
              MessageDlg(StrUnableToImportEle, mtError, [mbOK], 0);
              Exit;
            end;
          end;
          Elements.Capacity := Mesh.Mesh2D.Elements.Count;
          for ElementIndex := 0 to Mesh.Mesh2D.Elements.Count - 1 do
          begin
            AnElement2D := Mesh.Mesh2D.Elements[ElementIndex];
            ElementData := TElementData.Create;
            Elements.Add(ElementData);
            ElementData.Number := AnElement2D.ElementNumber;
            CenterPoint := AnElement2D.Center;
            ElementData.X := CenterPoint.X;
            ElementData.Y := CenterPoint.Y;
            ElementData.Z := 0;
          end;
        end;
      mt3D:
        begin
          NeedLocations := (Mesh.ActiveElementCount <> FEleReader.Count);
          if NeedLocations then
          begin
            if (Length(FEleReader.X) = 0)
              or (Length(FEleReader.Y) = 0)
              or (Length(FEleReader.Z) = 0)  then
            begin
              Beep;
              MessageDlg(StrUnableToImportEle, mtError, [mbOK], 0);
              Exit;
            end;
          end;
          Elements.Capacity := Mesh.ActiveElementCount;
          for LayerIndex := 0 to Mesh.LayerCount-1 do
          begin
            for ElementIndex := 0 to Mesh.Mesh2D.Elements.Count - 1 do
            begin
              AnElement3D := Mesh.ElementArray[LayerIndex,ElementIndex];
              if AnElement3D.Active then
              begin
                ElementData := TElementData.Create;
                Elements.Add(ElementData);
                ElementData.Number := AnElement3D.ElementNumber;
                CenterLocation := AnElement3D.CenterLocation;
                ElementData.X := CenterLocation.X;
                ElementData.Y := CenterLocation.Y;
                ElementData.Z := CenterLocation.Z;
              end;
            end;
          end
        end;
      else Assert(False);
    end;

    if Elements.Count > 0 then
    begin
      Elements.Sort(TElementDataComparer.Construct(
        function (const L, R: TElementData): integer
        begin
          result := L.Number - R.Number;
        end));
    end;

    ScreenObject := TScreenObject.CreateWithViewDirection(
      frmGoPhast.PhastModel, vdTop,
      UndoCreateScreenObject, False);
    ScreenObject.SetPropertiesOfIntersectedCells := True;
    ScreenObject.EvaluatedAt := eaBlocks;
    ScreenObject.Visible := False;
    case Mesh.MeshType of
      mt2D: ScreenObject.ElevationCount := ecZero;
      mt3D:
        begin
          ScreenObject.ElevationCount := ecOne;
          ScreenObject.ElevationFormula :=
            rsObjectImportedValuesR
            + '("' + StrImportedElevations + '")';

        end;
      else Assert(False);
    end;

    for ElementIndex := 0 to FEleReader.Count - 1 do
    begin
      if (Length(FEleReader.X) = 0) or not NeedLocations then
      begin
        // The X coordinates were NOT stored in the .node file
        APoint.X := Elements[ElementIndex].X;
      end
      else
      begin
        // The X coordinates WERE stored in the .node file
        APoint.X := FEleReader.X[ElementIndex];
      end;
      if (Length(FEleReader.Y) = 0) or not NeedLocations then
      begin
        // The Y coordinates were NOT stored in the .node file
        APoint.Y := Elements[ElementIndex].Y;
      end
      else
      begin
        // The Y coordinates WERE stored in the .node file
        APoint.Y := FEleReader.Y[ElementIndex];
      end;
      ScreenObject.AddPoint(APoint, True);


      if ScreenObject.ElevationCount = ecOne then
      begin
        // 3D model
        if (Length(FEleReader.Z) = 0) or not NeedLocations then
        begin
          // The Z coordinates were NOT stored in the .node file
          Z := Elements[ElementIndex].Z;
        end
        else
        begin
          // The Z coordinates WERE stored in the .node file
          Z := FEleReader.Z[ElementIndex];
        end;
        ScreenObject.ImportedSectionElevations.Add(Z);
      end;
    end;
  finally
    Elements.Free;
  end;

end;

procedure TfrmImportSutraModelResults.CreateNodeDataSets(StepIndex: Integer;
  NewDataSets: TList);
var
  index: TImportItems;
  NewName: string;
  DataSet: TDataArray;
  NewFormula: string;
  NewDataType: TRbwDataType;
begin
  for index := FirstNodeItem to LastNodeItem do
  begin
    if chklstDataToImport.Checked[Ord(index)] then
    begin
      case index of
        iiPressure:
          begin
            if Length(FNodeReader.Pressure) = 0 then
            begin
              Continue;
            end;
          end;
        iiU:
          begin
            if Length(FNodeReader.U) = 0 then
            begin
              Continue;
            end;
          end;
        iiSaturation:
          begin
            if Length(FNodeReader.Saturation) = 0 then
            begin
              Continue;
            end;
          end;
        else
          Assert(False);
      end;
      NewName := GenerateNewName(chklstDataToImport.Items[Ord(index)] + '_'
        + IntToStr(FResultList[StepIndex].TimeStep));

      NewDataType := rdtDouble;
      NewFormula := '0.';

      DataSet := frmGoPhast.PhastModel.DataArrayManager.CreateNewDataArray(
        TDataArray, NewName, NewFormula, NewName, [], NewDataType,
        eaNodes, dso3D, SutraNodeResults);

      NewDataSets.Add(DataSet);
      frmGoPhast.PhastModel.UpdateDataArrayDimensions(DataSet);

      DataSet.Units := '';

    end;
  end;
end;

procedure TfrmImportSutraModelResults.AssignElementValues(NewDataSets: TList;
  AScreenObject: TScreenObject);
var
  DSIndex: Integer;
  DataArray: TDataArray;
  ValueArray: TOneDRealArray;
  Item: TValueArrayItem;
  ValueIndex: Integer;
  DataSetPosition: integer;
  ItemIndex: TImportItems;
begin
  DSIndex := 0;
  for ItemIndex := FirstElementItem to LastElementItem do
  begin
    if chklstDataToImport.Checked[Ord(ItemIndex)] then
    begin
      DataArray := nil;
      case ItemIndex of
        iiXVel:
          begin
            if Length(FEleReader.XVelocity) = 0 then
            begin
              Continue;
            end
            else
            begin
              ValueArray := FEleReader.XVelocity;
              DataArray := NewDataSets[DSIndex];
              Inc(DSIndex);
            end;
          end;
        iiYVel:
          begin
            if Length(FEleReader.YVelocity) = 0 then
            begin
              Continue;
            end
            else
            begin
              ValueArray := FEleReader.YVelocity;
              DataArray := NewDataSets[DSIndex];
              Inc(DSIndex);
            end;
          end;
        iiZVel:
          begin
            if Length(FEleReader.ZVelocity) = 0 then
            begin
              Continue;
            end
            else
            begin
              ValueArray := FEleReader.ZVelocity;
              DataArray := NewDataSets[DSIndex];
              Inc(DSIndex);
            end;
          end;
        else
          Assert(False);
      end;

      Item := AScreenObject.ImportedValues.Add as TValueArrayItem;
      Item.Name := DataArray.Name;
      Item.Values.DataType := DataArray.DataType;
      Item.Values.Count := FEleReader.Count;

      for ValueIndex := 0 to FEleReader.Count - 1 do
      begin
        Item.Values.RealValues[ValueIndex] := ValueArray[ValueIndex];
      end;

      DataSetPosition := AScreenObject.AddDataSet(DataArray);
      AScreenObject.DataSetFormulas[DataSetPosition] :=
        rsObjectImportedValuesR + '("' + DataArray.Name + '")';

    end;
  end
end;

procedure TfrmImportSutraModelResults.AssignNodeValues(NewDataSets: TList;
  AScreenObject: TScreenObject);
var
  DSIndex: Integer;
  DataArray: TDataArray;
  ValueArray: TOneDRealArray;
  Item: TValueArrayItem;
  ValueIndex: Integer;
  DataSetPosition: integer;
  ItemIndex: TImportItems;
begin
  DSIndex := 0;
  for ItemIndex := FirstNodeItem to LastNodeItem do
  begin
    if chklstDataToImport.Checked[Ord(ItemIndex)] then
    begin
      DataArray := nil;
      case ItemIndex of
        iiPressure:
          begin
            if Length(FNodeReader.Pressure) = 0 then
            begin
              Continue;
            end
            else
            begin
              ValueArray := FNodeReader.Pressure;
              DataArray := NewDataSets[DSIndex];
              Inc(DSIndex);
            end;
          end;
        iiU:
          begin
            if Length(FNodeReader.U) = 0 then
            begin
              Continue;
            end
            else
            begin
              ValueArray := FNodeReader.U;
              DataArray := NewDataSets[DSIndex];
              Inc(DSIndex);
            end;
          end;
        iiSaturation:
          begin
            if Length(FNodeReader.Saturation) = 0 then
            begin
              Continue;
            end
            else
            begin
              ValueArray := FNodeReader.Saturation;
              DataArray := NewDataSets[DSIndex];
              Inc(DSIndex);
            end;
          end;
        else
          Assert(False);
      end;

      Item := AScreenObject.ImportedValues.Add as TValueArrayItem;
      Item.Name := DataArray.Name;
      Item.Values.DataType := DataArray.DataType;
      Item.Values.Count := FNodeReader.Count;

      for ValueIndex := 0 to FNodeReader.Count - 1 do
      begin
        Item.Values.RealValues[ValueIndex] := ValueArray[ValueIndex];
      end;

      DataSetPosition := AScreenObject.AddDataSet(DataArray);
      AScreenObject.DataSetFormulas[DataSetPosition] :=
        rsObjectImportedValuesR + '("' + DataArray.Name + '")';

    end;
  end
end;

procedure TfrmImportSutraModelResults.SetData;
var
  index: Integer;
  StepList: TIntegerList;
  FirstResults: Boolean;
  NodeScreenObject: TScreenObject;
  NewDataSets: TList;
  AllNewDataSets: TList;
  DataSetIndex: integer;
  NewScreenObjects: TList;
  Undo: TUndoImportSutraResults;
  ElementScreenObject: TScreenObject;
begin
  NewDataSets := TList.Create;
  AllNewDataSets := TList.Create;
  StepList := TIntegerList.Create;
  NewScreenObjects := TList.Create;
  try
    Undo := TUndoImportSutraResults.Create;
    try
      if chklstDataToImport.Checked[Ord(iiPressure)]
        or chklstDataToImport.Checked[Ord(iiU)]
        or chklstDataToImport.Checked[Ord(iiSaturation)] then
      begin
        for index := 0 to Length(FNodeReader.StoredResults) - 1 do
        begin
          StepList.Add(FNodeReader.StoredResults[index].TimeStep);
        end;
        StepList.Sorted := True;
        Assert(FResultList.Count = chklstTimeStepsToImport.Items.Count);
        FirstResults := True;
        NodeScreenObject := nil;
        for index := 0 to FResultList.Count - 1 do
        begin
          if StepList.IndexOf(FResultList[index].TimeStep) >= 0 then
          begin
            if chklstTimeStepsToImport.Checked[index] then
            begin
              FNodeReader.ReadNextResults;
              NewDataSets.Clear;
              CreateNodeDataSets(index, NewDataSets);
              if NewDataSets.Count > 0 then
              begin
                for DataSetIndex := 0 to NewDataSets.Count -1 do
                begin
                  AllNewDataSets.Add(NewDataSets[DataSetIndex]);
                end;
                if FirstResults then
                begin
                  CreateNodeScreenObject(NodeScreenObject);
                  if NodeScreenObject = nil then
                  begin
                    Exit;
                  end;
                  NewScreenObjects.Add(NodeScreenObject);
                  FirstResults := False;
                end;

                AssignNodeValues(NewDataSets, NodeScreenObject);
              end;

            end
            else
            begin
              FNodeReader.SkipNextResults
            end;
          end;
        end;
      end;

      if chklstDataToImport.Checked[Ord(iiXVel)]
        or chklstDataToImport.Checked[Ord(iiYVel)]
        or chklstDataToImport.Checked[Ord(iiZVel)] then
      begin
        StepList.Clear;
        for index := 0 to Length(FEleReader.StoredResults) - 1 do
        begin
          StepList.Add(FEleReader.StoredResults[index].TimeStep);
        end;
        StepList.Sorted := True;
        Assert(FResultList.Count = chklstTimeStepsToImport.Items.Count);
        FirstResults := True;
        ElementScreenObject := nil;
        for index := 0 to FResultList.Count - 1 do
        begin
          if StepList.IndexOf(FResultList[index].TimeStep) >= 0 then
          begin
            if chklstTimeStepsToImport.Checked[index] then
            begin
              FEleReader.ReadNextResults;
              NewDataSets.Clear;
              CreateElementDataSets(index, NewDataSets);
              if NewDataSets.Count > 0 then
              begin
                for DataSetIndex := 0 to NewDataSets.Count -1 do
                begin
                  AllNewDataSets.Add(NewDataSets[DataSetIndex]);
                end;
                if FirstResults then
                begin
                  CreateElementScreenObject(ElementScreenObject);
                  if ElementScreenObject = nil then
                  begin
                    Exit;
                  end;
                  NewScreenObjects.Add(ElementScreenObject);
                  FirstResults := False;
                end;

                AssignElementValues(NewDataSets, ElementScreenObject);
              end;

            end
            else
            begin
              FEleReader.SkipNextResults
            end;
          end;
        end;
      end;


      Undo.StoreNewScreenObjects(NewScreenObjects);
      Undo.StoreNewDataSets(AllNewDataSets);
      frmGoPhast.UndoStack.Submit(Undo)
    finally

    end;
  finally
    AllNewDataSets.Free;
    NewDataSets.Free;
    StepList.Free;
    NewScreenObjects.Free;
  end;
end;

{ TUndoImportSutraResults }

function TUndoImportSutraResults.Description: string;
begin
  result := StrImportSUTRAModelR;
end;

initialization
  SutraNodeResults := StrModelResults + '|' + 'SUTRA Nodal Results';
  SutraElementResults := StrModelResults + '|' + 'SUTRA Element Results';

end.
