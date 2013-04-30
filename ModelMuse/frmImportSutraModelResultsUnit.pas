unit frmImportSutraModelResultsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  CheckLst, Buttons, ExtCtrls, ReadSutraNodEleUnit, ScreenObjectUnit,
  frmImportShapefileUnit, Generics.Collections, Generics.Defaults, DataSetUnit,
  GoPhastTypes;

type
  TImportItem = (iiPressure, iiU, iiSaturation, iiXVel, iiYVel, iiZVel);
  TImportItems = set of TImportItem;

  TColorContourItem = class(TObject)
    ImportChoice: TImportItem;
    TimeStep: integer;
  end;
  TColorContourList = TObjectList<TColorContourItem>;
  TColorContourItemComparer = TComparer<TColorContourItem>;

const
  FirstNodeItem = iiPressure;
  LastNodeItem = iiSaturation;
  FirstElementItem = iiXVel;
  LastElementItem = iiZVel;

type
  TUndoImportSutraResults = class(TUndoImportShapefile)
  strict private
    FDisplayDataSet: TDataArray;
    FDisplayChoice: TDisplayChoice;
    FOldTimeList: TCustomTimeList;
    FOldTopDataSet: TDataArray;
    FOld3DDataSet: TDataArray;
    FOldTopContourDataSet: TDataArray;
    FOld3DContourDataSet: TDataArray;
  private
    procedure SetDisplayChoice(const Value: TDisplayChoice);
    procedure SetDisplayDataSet(const Value: TDataArray);
  protected
    function Description: string; override;
  public
    constructor Create;
    property DisplayDataSet: TDataArray read FDisplayDataSet write SetDisplayDataSet;
    property DisplayChoice: TDisplayChoice read FDisplayChoice write SetDisplayChoice;
    procedure DoCommand; override;
    procedure Undo; override;
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
    rgDisplayChoice: TRadioGroup;
    lblColorMesh: TLabel;
    comboColorMesh: TComboBox;
    btnSelectAll: TButton;
    btnSelectNone: TButton;
    btnSelectAllTimes: TButton;
    btnDeselectAllTimes: TButton;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnSelectNoneClick(Sender: TObject);
    procedure btnSelectAllTimesClick(Sender: TObject);
    procedure btnDeselectAllTimesClick(Sender: TObject);
    procedure chklstDataToImportClick(Sender: TObject);
    procedure chklstTimeStepsToImportClick(Sender: TObject);
  private
    FNodeReader: TNodReader;
    FEleReader: TEleReader;
    FResultList: TStoredResultsList;
    FColorContourList: TColorContourList;
    FColorContourDataArray: TDataArray;
    F_CCItem: TColorContourItem;
    FNodeFileName: string;
    FElementFileName: string;
    procedure GetData;
    procedure SetData;
    procedure CreateNodeScreenObject(out ScreenObject: TScreenObject);
    procedure CreateElementScreenObject(out ScreenObject: TScreenObject);
    procedure CreateNodeDataSets(StepIndex: Integer; NewDataSets: TList);
    procedure CreateElementDataSets(StepIndex: Integer; NewDataSets: TList);
    procedure AssignNodeValues(NewDataSets: TList; AScreenObject: TScreenObject);
    procedure AssignElementValues(NewDataSets: TList; AScreenObject: TScreenObject);
    procedure UpdateColorContourList;
    procedure EnableOkButton;
  protected
    procedure Loaded; override;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmImportSutraModelResults: TfrmImportSutraModelResults;

var
//  SutraNodeResults: string;
//  SutraElementResults: string;
  SutraPressureResults: string;
  SutraUResults: string;
  SutraSaturationResults: string;
  SutraXVelocityResults: string;
  SutraYVelocityResults: string;
  SutraZVelocityResults: string;

implementation

uses
  frmGoPhastUnit, SutraOptionsUnit, SutraMeshUnit, IntListUnit,
  SutraInputWriterUnit, UndoItems, FastGEO, GIS_Functions,
  RbwParser, PhastModelUnit, frmSelectResultToImportUnit,
  ValueArrayStorageUnit, Math, frmDisplayDataUnit, frmGridValueUnit,
  VectorDisplayUnit, IOUtils;

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
  StrNone = 'none';
  StrReadFrom0sOn = 'read from: "%0:s" on %1:s'
    + sLineBreak + 'Time Step: %2:d.'
    + sLineBreak + 'Elapsed Time: %3:g.'
    + sLineBreak + 'File last modified on: %4:s';
  StrVelocityAtTimeSte = 'Velocity at time step %0:d imported on %1:s';


{ TfrmImportSutraModelResults }

procedure TfrmImportSutraModelResults.btnDeselectAllTimesClick(Sender: TObject);
begin
  inherited;
  chklstTimeStepsToImport.CheckAll(cbUnchecked);
  UpdateColorContourList;
end;

procedure TfrmImportSutraModelResults.btnOKClick(Sender: TObject);
var
  index: Integer;
  OK: Boolean;
  DisplayChoice: TDisplayChoice;
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
    DisplayChoice := TDisplayChoice(rgDisplayChoice.ItemIndex);
    Inc(DisplayChoices[DisplayChoice]);
  end
  else
  begin
    Beep;
    MessageDlg(StrYouMustSpecifyAt, mtWarning, [mbOK], 0);
    ModalResult := mrNone;
  end;

end;

procedure TfrmImportSutraModelResults.btnSelectAllClick(Sender: TObject);
begin
  inherited;
  chklstDataToImport.CheckAll(cbChecked);
  UpdateColorContourList;
end;

procedure TfrmImportSutraModelResults.btnSelectAllTimesClick(Sender: TObject);
begin
  inherited;
  chklstTimeStepsToImport.CheckAll(cbChecked);
  UpdateColorContourList;
end;

procedure TfrmImportSutraModelResults.btnSelectNoneClick(Sender: TObject);
begin
  inherited;
  chklstDataToImport.CheckAll(cbUnchecked);
  UpdateColorContourList;
end;

procedure TfrmImportSutraModelResults.chklstDataToImportClick(Sender: TObject);
begin
  inherited;
  UpdateColorContourList;
end;

procedure TfrmImportSutraModelResults.chklstTimeStepsToImportClick(
  Sender: TObject);
begin
  inherited;
  UpdateColorContourList;
end;

procedure TfrmImportSutraModelResults.FormCreate(Sender: TObject);
begin
  inherited;
  FColorContourList := TColorContourList.Create;
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
  if frmGoPhast.PhastModel.SutraMesh.MeshType in [mt2D, mtProfile] then
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
  FColorContourList.Free;
end;

procedure TfrmImportSutraModelResults.GetData;
var
  FileName: TFileName;
//  NodeFileName: TFileName;
//  ElementFileName: string;
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
      FNodeFileName := FileName;
      FElementFileName := ChangeFileExt(FileName, '.ele');
    end
    else if Extension = '.ele' then
    begin
      FNodeFileName := ChangeFileExt(FileName, '.nod');
      FElementFileName := FileName;
    end
    else
    begin
      FNodeFileName := '';
      FElementFileName := '';
      Beep;
      MessageDlg(StrOnlyNodAndEleF, mtError, [mbOK], 0);
      ModalResult := mrOk;
      Exit;
    end;
    try
      FNodeReader := TNodReader.Create(FNodeFileName);
    except on E: EInOutError do
      begin
        Beep;
        MessageDlg(Format(StrUnableToOpen0s, [FNodeFileName, E.message]),
          mtWarning, [mbOK], 0);
        ModalResult := mrOk;
        Exit;
      end;
    end;
    try
      FEleReader := TEleReader.Create(FElementFileName);
    except on E: EInOutError do
      begin
        Beep;
        MessageDlg(Format(StrUnableToOpen0s, [FElementFileName, E.message]),
          mtWarning, [mbOK], 0);
        ModalResult := mrOk;
        Exit;
      end;
    end;

    FResultList := TStoredResultsList.Create;
    FResultList.OwnsObjects := False;
    FResultList.AddRange(FNodeReader.StoredResults.ToArray);
    FResultList.AddRange(FEleReader.StoredResults.ToArray);
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
      chklstTimeStepsToImport.Items.AddObject(Format('Step: %0:d; Time: %1:g',
        [FResultList[ItemIndex].TimeStep, FResultList[ItemIndex].Time]),
        FResultList[ItemIndex]);
    end;
    chklstTimeStepsToImport.Checked[chklstTimeStepsToImport.items.Count-1] :=
      True;

    ShowWarning := False;
    Mesh := frmGoPhast.PhastModel.SutraMesh;
    case Mesh.MeshType of
      mt2D, mtProfile:
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

    UpdateColorContourList;
  end
  else
  begin
    ModalResult := mrOk;
  end;
end;

procedure TfrmImportSutraModelResults.Loaded;
begin
  inherited;
  Constraints.MinHeight := Height;
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
      mt2D, mtProfile:
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
    ScreenObject.ElevationCount := ecOne;
    case Mesh.MeshType of
      mt2D, mtProfile:
        begin
          ScreenObject.ElevationFormula := '0'
        end;
      mt3D:
        begin
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

procedure TfrmImportSutraModelResults.EnableOkButton;
var
  index: Integer;
  ShouldEnable: Boolean;
begin
  ShouldEnable := False;
  for index := 0 to chklstDataToImport.Items.Count - 1 do
  begin
    ShouldEnable := chklstDataToImport.Checked[index];
    if ShouldEnable then
    begin
      Break;
    end;
  end;
  if ShouldEnable then
  begin
    ShouldEnable := False;
    for index := 0 to chklstTimeStepsToImport.Items.Count - 1 do
    begin
      ShouldEnable := chklstTimeStepsToImport.Checked[index];
      if ShouldEnable then
      begin
        Break;
      end;
    end;
  end;
  btnOK.Enabled := ShouldEnable;
end;

procedure TfrmImportSutraModelResults.CreateElementDataSets(StepIndex: Integer;
  NewDataSets: TList);
var
  index: TImportItem;
  NewName: string;
  DataSet: TDataArray;
  NewFormula: string;
  NewDataType: TRbwDataType;
  MeshType: TMeshType;
  VItem: TVectorItem;
  Vectors: TVectorCollection;
  Classification: string;
  procedure AssignCommonProperties;
  var
    PriorItem: TVectorItem;
  begin
    VItem := Vectors.Add as TVectorItem;
    DataSet := NewDataSets[0];
    VItem.Vectors.XVelocityName := DataSet.Name;
    DataSet := NewDataSets[1];
    VItem.Vectors.YVelocityName := DataSet.Name;
    VItem.Description := Format(StrVelocityAtTimeSte,
      [FResultList[StepIndex].TimeStep, DateTimeToStr(Now)]);
    if Vectors.Count = 1 then
    begin
      VItem.Vectors.Color := clFuchsia;
    end
    else
    begin
      PriorItem := Vectors.Items[Vectors.Count-2] as TVectorItem;
      VItem.Vectors.Color := PriorItem.Vectors.Color;
    end;
  end;
begin
  Assert(NewDataSets.Count = 0);
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
            Classification := SutraXVelocityResults;
          end;
        iiYVel:
          begin
            if Length(FEleReader.FYVelocity) = 0 then
            begin
              Continue;
            end;
            Classification := SutraYVelocityResults;
          end;
        iiZVel:
          begin
            if Length(FEleReader.FZVelocity) = 0 then
            begin
              Continue;
            end;
            Classification := SutraZVelocityResults;
          end;
        else
          Assert(False);
      end;
      NewName := GenerateNewName(chklstDataToImport.Items[Ord(index)] + '_'
        + IntToStr(FResultList[StepIndex].TimeStep), nil, '_');

      NewDataType := rdtDouble;
      NewFormula := '0.';

      DataSet := frmGoPhast.PhastModel.DataArrayManager.CreateNewDataArray(
        TDataArray, NewName, NewFormula, NewName, [], NewDataType,
        eaBlocks, dso3D, Classification);

      DataSet.Comment := Format(StrReadFrom0sOn,
        [FElementFileName, DateTimeToStr(Now), FResultList[StepIndex].TimeStep,
        FResultList[StepIndex].Time,
        DateTimeToStr(TFile.GetLastWriteTime(FElementFileName))]);

      if (F_CCItem <> nil) and (F_CCItem.ImportChoice = index)
        and (F_CCItem.TimeStep = FResultList[StepIndex].TimeStep) then
      begin
        FColorContourDataArray  := DataSet;
      end;

      NewDataSets.Add(DataSet);
      frmGoPhast.PhastModel.UpdateDataArrayDimensions(DataSet);

      DataSet.Units := '';

    end;
  end;
  MeshType := frmGoPhast.PhastModel.SutraMesh.MeshType;
  Vectors := frmGoPhast.PhastModel.VelocityVectors;
  case MeshType of
    mt2D, mtProfile:
      begin
        if NewDataSets.Count = 2 then
        begin
          AssignCommonProperties;
        end;
      end;
    mt3D:
      begin
        if NewDataSets.Count = 3 then
        begin
          AssignCommonProperties;
//          VItem := frmGoPhast.PhastModel.VelocityVectors.Add as TVectorItem;
//          DataSet := NewDataSets[0];
//          VItem.Vectors.XVelocityName := DataSet.Name;
//          DataSet := NewDataSets[1];
//          VItem.Vectors.YVelocityName := DataSet.Name;
          DataSet := NewDataSets[2];
          VItem.Vectors.ZVelocityName := DataSet.Name;
//          VItem.Description := Format(StrVelocityAtTimeSte,
//            [FResultList[StepIndex].TimeStep, DateTimeToStr(Now)]);
//          if Vectors.Count = 1 then
//          begin
//            VItem.Vectors.Color := clFuchsia;
//          end
//          else
//          begin
//            PriorItem := Vectors[Vectors.Count-2];
//            VItem.Vectors.Color := PriorItem.Vectors.Color;
//          end;
        end;
      end;
    else
      Assert(False);
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
      mt2D, mtProfile:
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
    ScreenObject.ElevationCount := ecOne;
    case Mesh.MeshType of
      mt2D, mtProfile:
        begin
          ScreenObject.ElevationFormula := '0'
        end;
      mt3D:
        begin
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
  index: TImportItem;
  NewName: string;
  DataSet: TDataArray;
  NewFormula: string;
  NewDataType: TRbwDataType;
  Classification: string;
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
            Classification := SutraPressureResults;
          end;
        iiU:
          begin
            if Length(FNodeReader.U) = 0 then
            begin
              Continue;
            end;
            Classification := SutraUResults;
          end;
        iiSaturation:
          begin
            if Length(FNodeReader.Saturation) = 0 then
            begin
              Continue;
            end;
            Classification := SutraSaturationResults;
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
        eaNodes, dso3D, Classification);
      DataSet.Comment := Format(StrReadFrom0sOn,
        [FNodeFileName, DateTimeToStr(Now), FResultList[StepIndex].TimeStep,
        FResultList[StepIndex].Time,
        DateTimeToStr(TFile.GetLastWriteTime(FNodeFileName))]);

      if (F_CCItem <> nil) and (F_CCItem.ImportChoice = index)
        and (F_CCItem.TimeStep = FResultList[StepIndex].TimeStep) then
      begin
        FColorContourDataArray  := DataSet;
      end;

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
  ItemIndex: TImportItem;
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
  ItemIndex: TImportItem;
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
  DisplayChoice: TDisplayChoice;
begin
  FColorContourDataArray := nil;
  F_CCItem := comboColorMesh.Items.
    Objects[comboColorMesh.ItemIndex] as TColorContourItem;
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
        for index := 0 to FNodeReader.StoredResults.Count - 1 do
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
      StepList.Sorted := True;

      if chklstDataToImport.Checked[Ord(iiXVel)]
        or chklstDataToImport.Checked[Ord(iiYVel)]
        or chklstDataToImport.Checked[Ord(iiZVel)] then
      begin
        StepList.Clear;
        for index := 0 to FEleReader.StoredResults.Count - 1 do
        begin
          StepList.AddUnique(FEleReader.StoredResults[index].TimeStep);
        end;
//        StepList.Sorted := True;
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

      DisplayChoice := TDisplayChoice(rgDisplayChoice.ItemIndex);
      if FColorContourDataArray <> nil then
      begin
        case DisplayChoice of
          dcColor:
            begin
              frmGoPhast.acColoredGrid.Enabled := True;
              frmGoPhast.acColoredGrid.Checked := True;
              frmGoPhast.tb3DColors.Down := True;
            end;
          dcContour, dcNone:
            begin
              // do nothing
            end;
          else Assert(False);
        end;
      end;


      Undo.StoreNewScreenObjects(NewScreenObjects);
      Undo.StoreNewDataSets(AllNewDataSets);
      Undo.DisplayChoice := DisplayChoice;
      Undo.DisplayDataSet := FColorContourDataArray;
      frmGoPhast.UndoStack.Submit(Undo)
    except
      Undo.Free;
      raise
    end;
  finally
    AllNewDataSets.Free;
    NewDataSets.Free;
    StepList.Free;
    NewScreenObjects.Free;
  end;
end;

procedure TfrmImportSutraModelResults.UpdateColorContourList;
var
  ImportItems: TImportItems;
  Index: integer;
  TimeStep: Integer;
  ItemIndex: TImportItem;
  CCItem: TColorContourItem;
  SelectedText: TCaption;
  ItemText: string;
  IntList: TIntegerList;
  StoredResult: TStoredResults;
begin
  EnableOkButton;
  FColorContourList.Clear;
  try
    ImportItems := [];
    for Index := 0 to chklstDataToImport.Count - 1 do
    begin
      if chklstDataToImport.Checked[Index] then
      begin
        Include(ImportItems, TImportItem(Index));
      end;
    end;
    if ImportItems = [] then
    begin
      Exit;
    end;
    IntList:= TIntegerList.Create;
    try
      for Index := 0 to chklstTimeStepsToImport.Count - 1 do
      begin
        if chklstTimeStepsToImport.Checked[Index] then
        begin
          StoredResult := chklstTimeStepsToImport.Items.
            Objects[Index] as TStoredResults;
          IntList.Add(StoredResult.TimeStep);
        end;
      end;
      IntList.Sorted := True;
      if IntList.Count = 0 then
      begin
        Exit;
      end;
      if (ImportItems * [iiPressure, iiU, iiSaturation]) <> [] then
      begin
        for Index := 0 to FNodeReader.StoredResults.Count - 1 do
        begin
          TimeStep := FNodeReader.StoredResults[Index].TimeStep;

          if IntList.IndexOf(TimeStep) >= 0 then
          begin
            for ItemIndex := FirstNodeItem to LastNodeItem do
            begin
              if ItemIndex in ImportItems then
              begin
                CCItem := TColorContourItem.Create;
                FColorContourList.Add(CCItem);
                CCItem.ImportChoice := ItemIndex;
                CCItem.TimeStep := TimeStep;
              end;
            end;
          end;
        end;
      end;
      if (ImportItems * [iiXVel, iiYVel, iiZVel]) <> [] then
      begin
        for Index := 0 to FEleReader.StoredResults.Count - 1 do
        begin
          TimeStep := FEleReader.StoredResults[Index].TimeStep;
          if IntList.IndexOf(TimeStep) >= 0 then
          begin
            for ItemIndex := FirstElementItem to LastElementItem do
            begin
              if ItemIndex in ImportItems then
              begin
                CCItem := TColorContourItem.Create;
                FColorContourList.Add(CCItem);
                CCItem.ImportChoice := ItemIndex;
                CCItem.TimeStep := TimeStep;
              end;
            end;
          end;
        end;
      end;
    finally
      IntList.Free;
    end;
    FColorContourList.Sort(TColorContourItemComparer.Construct(
      function (const L, R: TColorContourItem): integer
       begin
         result := L.TimeStep - R.TimeStep;
         if result = 0 then
         begin
           result := Ord(L.ImportChoice) - Ord(R.ImportChoice);
         end;
       end));

  finally
    SelectedText := comboColorMesh.Text;
    comboColorMesh.Items.Clear;
    comboColorMesh.Items.Capacity := FColorContourList.Count+1;
    comboColorMesh.Items.Add(StrNone);
    for Index := 0 to FColorContourList.Count - 1 do
    begin
      CCItem := FColorContourList[Index];
      ItemText := chklstDataToImport.Items[Ord(CCItem.ImportChoice)];
      ItemText := Format('%0:s, TS: %1:d', [ItemText,CCItem.TimeStep]);
      comboColorMesh.Items.AddObject(ItemText, CCItem);
    end;
    comboColorMesh.ItemIndex := Max(0,
      comboColorMesh.Items.IndexOf(SelectedText));

  end;
end;

{ TUndoImportSutraResults }

constructor TUndoImportSutraResults.Create;
var
  SutraMesh: TSutraMesh3D;
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := frmGoPhast.PhastModel;
  SutraMesh := PhastModel.SutraMesh;
  FOldTimeList := PhastModel.ThreeDTimeList;
  FOld3DDataSet := PhastModel.ThreeDDataSet;
  FOldTopDataSet := PhastModel.TopDataSet;
  FOldTopContourDataSet := SutraMesh.TopContourDataSet;
  FOld3DContourDataSet := SutraMesh.ThreeDContourDataSet;
end;

function TUndoImportSutraResults.Description: string;
begin
  result := StrImportSUTRAModelR;
end;

procedure TUndoImportSutraResults.DoCommand;
var
  PhastModel: TPhastModel;
  SutraMesh: TSutraMesh3D;
begin
  inherited;
  if FDisplayDataSet = nil then
  begin
    Exit;
  end;
  case DisplayChoice of
    dcColor:
      begin
        PhastModel := frmGoPhast.PhastModel;
        PhastModel.ThreeDTimeList := nil;
        PhastModel.ThreeDDataSet := FDisplayDataSet;
        PhastModel.TopDataSet := FDisplayDataSet;

        UpdateFrmDisplayData;
        UpdateFrmGridValue;
      end;
    dcContour:
      begin
        PhastModel := frmGoPhast.PhastModel;
        SutraMesh := PhastModel.SutraMesh;
        SutraMesh.TopContourDataSet := FDisplayDataSet;
        SutraMesh.ThreeDContourDataSet := FDisplayDataSet;
        UpdateFrmDisplayData;
        UpdateFrmGridValue;
      end;
    dcNone:;// do nothing
    else Assert(False);
  end;
end;

procedure TUndoImportSutraResults.SetDisplayChoice(const Value: TDisplayChoice);
begin
  FDisplayChoice := Value;
end;

procedure TUndoImportSutraResults.SetDisplayDataSet(const Value: TDataArray);
begin
  FDisplayDataSet := Value;
end;

procedure TUndoImportSutraResults.Undo;
var
  PhastModel: TPhastModel;
  SutraMesh: TSutraMesh3D;
begin
  inherited;
  if (FDisplayDataSet = nil) or (DisplayChoice = dcNone) then
  begin
    Exit;
  end;

  PhastModel := frmGoPhast.PhastModel;
  SutraMesh := PhastModel.SutraMesh;
  PhastModel.ThreeDTimeList := FOldTimeList;
  PhastModel.ThreeDDataSet := FOld3DDataSet;
  PhastModel.TopDataSet := FOldTopDataSet;
  SutraMesh.TopContourDataSet := FOldTopContourDataSet;
  SutraMesh.ThreeDContourDataSet := FOld3DContourDataSet;

  UpdateFrmDisplayData;
  UpdateFrmGridValue;

end;

initialization
//  SutraNodeResults := StrModelResults + '|' + 'SUTRA Nodal Results';
//  SutraElementResults := StrModelResults + '|' + 'SUTRA Element Results';

  SutraPressureResults := StrModelResults + '|' + 'Pressure';
  SutraUResults := StrModelResults + '|' + 'U Values';
  SutraSaturationResults := StrModelResults + '|' + 'Saturation';

  SutraXVelocityResults := StrModelResults + '|' + 'X Velocity';
  SutraYVelocityResults := StrModelResults + '|' + 'Y Velocity';
  SutraZVelocityResults := StrModelResults + '|' + 'Z Velocity';

end.