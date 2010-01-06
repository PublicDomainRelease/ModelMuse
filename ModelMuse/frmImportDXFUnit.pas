{@abstract(The main purpose of @name is to define @link(TUndoImportDXFFile)
  which is used to import DXF files into GoPhast. It also defines
  @link(TUndoImportDXFFile) which can be used to undo the import.)}
unit frmImportDXFUnit;


interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Buttons, ExtCtrls,
  Grids, IntListUnit, ScreenObjectUnit, DXF_Structs, DXF_read, DXF_Utils,
  frmImportShapefileUnit;

type
  {@abstract(@name is the command used to import
    DXF files or reverse the import.)}
  TUndoImportDXFFile = class(TUndoImportShapefile)
  protected
    // @name describes what @classname does.
    function Description: string; override;
  end;

  {@abstract(@name is used to import DXF files into GoPhast.)
    See @link(TfrmGoPhast.miImportDXFFileClick).}
  TfrmImportDXF = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes @classname without doing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name displays help about @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TCheckBox;
    // @name indicates that the value of the imported @link(TScreenObject)s
    // should set the value of enclosed cells or elements in the related
    // @link(TDataArray).
    cbEnclosedCells: TCheckBox;
    // @name: TCheckBox;
    // @name indicates that the value of the imported @link(TScreenObject)s
    // should set the value of cells or elements in the related
    // @link(TDataArray) by interpolation.
    cbInterpolation: TCheckBox;
    // @name: TCheckBox;
    // @name indicates that the value of the imported @link(TScreenObject)s
    // should set the value of intersected cells or elements in the related
    // @link(TDataArray).
    cbIntersectedCells: TCheckBox;
    // @name: TComboBox;
    // @name is used to select the name of the @link(TDataArray) to be
    // affected by the imported @link(TScreenObject)s.
    comboDataSets: TComboBox;
    // @name: TComboBox;
    // @name is the name of the @link(TCustom2DInterpolater) that will
    // be used with a new @link(TDataArray).
    comboInterpolators: TComboBox;
    // @name: TLabel;
    // @name displays "Data Set".
    lblDataSet: TLabel;
    // @name: TLabel;
    // @name displays "Interpolator".
    lblInterpolator: TLabel;
    // @name: TOpenDialog;
    // @name is used to select the DXF file.
    OpenDialogDXF: TOpenDialog;
    // @name: TRadioGroup;
    // @name indicates whether a new data set will be evaluated at
    // elements or cells.
    rgEvaluatedAt: TRadioGroup;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name makes sure that at least one of the following checkboxes is
    // checked: @link(cbEnclosedCells), @link(cbIntersectedCells), and
    // @link(cbInterpolation).  If not, their fonts are changed to emphasize
    // them and @link(btnOK) is disabled.
    procedure cbEnclosedCellsClick(Sender: TObject);
    // @name enables or disables @link(comboInterpolators) depending
    // on whether a new @link(TDataArray) is to be created.
    // @link(comboInterpolators) will be enabled if
    // a new @link(TDataArray) is to be created.
    procedure comboDataSetsChange(Sender: TObject);
    // @name enables @link(cbInterpolation) if an interpolator
    // is specified.
    procedure comboInterpolatorsChange(Sender: TObject);
    // @name calls @link(GetInterpolators).
    procedure FormCreate(Sender: TObject); override;
    // @name frees @link(FDxfObject).
    procedure FormDestroy(Sender: TObject);
    // @name changes the captions of @link(cbEnclosedCells),
    // @link(cbIntersectedCells), and @link(cbInterpolation).
    procedure rgEvaluatedAtClick(Sender: TObject);
  private
    // @name: DXF_Object;
    // @name represents the contents of the DXF file.
    // DXF_Object is defined in DXF_Structs.
    FDxfObject: DXF_Object;
    FDxfName: string;
    // @name is used to transform the coordinates of P
    // based on OCS.
    function CoordConvert(P: Point3D; OCS: pMatrix): Point3D;
    // @name fills @link(comboDataSets) with the names of
    // @link(TDataArray)s that can be used the the imported
    // @link(TScreenObject)s.
    procedure GetDataSets;
    // @name fills @link(comboInterpolators) with a list of
    // @link(TCustom2DInterpolater)s.
    procedure GetInterpolators;
    // @name creates a new @link(TDataArray).
    procedure MakeNewDataSet;
    { Set the captions of @link(cbEnclosedCells), @link(cbIntersectedCells),
      and @link(cbInterpolation) based on @link(rgEvaluatedAt).ItemIndex.}
    procedure SetCheckBoxCaptions;
    // @name converts the entities from @link(FDxfObject) to
    // @link(TScreenObject)s.
    procedure SetData;
    // @name is used to update @link(frmProgress).
    procedure Think(const Sender: TObject; Message: string);
    { Private declarations }
  public
    // @name is used to open and read a DXF file.  It returns @true
    // if it was able to do so and the file contains something it can use.
    function GetData: boolean;
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit, GoPhastTypes, DataSetUnit, 
  RbwParser, UndoItems, frmProgressUnit, frmDataSetsUnits, ModelMuseUtilities, FastGEO;

{$R *.dfm}

procedure TfrmImportDXF.FormCreate(Sender: TObject);
begin
  inherited;
  cbEnclosedCellsClick(nil);
  SetCheckBoxCaptions;
  GetInterpolators;
end;

procedure TfrmImportDXF.Think(const Sender: TObject; Message: string);
begin
  frmProgress.ProgressLabelCaption := Message;
end;

function TfrmImportDXF.GetData: boolean;
begin
  inherited;
  rgEvaluatedAt.Items[Ord(eaBlocks)] := EvalAtToString(eaBlocks,
    frmGoPhast.PhastModel.ModelSelection, True, True);
  rgEvaluatedAt.Items[Ord(eaNodes)] := EvalAtToString(eaNodes,
    frmGoPhast.PhastModel.ModelSelection, True, True);
  rgEvaluatedAt.Enabled := frmGoPhast.PhastModel.ModelSelection = msPhast;

  result := OpenDialogDXF.Execute;
  if result then
  begin
    FDxfName := OpenDialogDXF.FileName;
    if not FileExists(FDxfName) then
    begin
      result := False;
      Beep;
      MessageDlg('The ".dxf" file "' + FDxfName + '" does not exist.',
        mtError, [mbOK], 0);
      Exit;
    end;
    Caption := Caption + ' - ' + FDxfName;
    GetDataSets;
    frmProgress.PopupParent := self;
    frmProgress.Show;
    try
      FDxfObject := DXF_Object.Create(name);
      FDxfObject.Progress := frmProgress.pbProgress;
      FDxfObject.OnThinking := Think;
      FDxfObject.ReadFile(FDxfName, frmProgress.memoMessages.Lines);
    finally
      frmProgress.Hide;
    end;

    result := FDxfObject.layer_lists.Count > 0;
    comboDataSets.ItemIndex := 0;
    comboInterpolators.ItemIndex := 4;
  end;
end;

procedure TfrmImportDXF.MakeNewDataSet;
var
  NewDataSetName: string;
  DataSet: TDataArray;
  AType: TInterpolatorType;
  Interpolator: TCustom2DInterpolater;

begin
  Assert(SizeOf(TObject) = SizeOf(TInterpolatorType));
  if comboDataSets.ItemIndex = 0 then
  begin
    NewDataSetName := ExtractFileName(OpenDialogDXF.FileName);
    NewDataSetName := ChangeFileExt(NewDataSetName, '');
    NewDataSetName := GenerateNewName(NewDataSetName + '_DXF_Z');

    DataSet := frmGoPhast.PhastModel.CreateNewDataArray(TDataArray,
      NewDataSetName, '0.', [], rdtDouble,
      TEvaluatedAt(rgEvaluatedAt.ItemIndex), dsoTop, 'Imported from DXF files');

    DataSet.OnDataSetUsed := frmGoPhast.PhastModel.ModelResultsRequired;
    DataSet.Units := '';

    if comboInterpolators.ItemIndex > 0 then
    begin
      AType := TInterpolatorType(comboInterpolators.Items.
        Objects[comboInterpolators.ItemIndex]);
      Interpolator := AType.Create(nil);
      try
        DataSet.TwoDInterpolator := Interpolator
      finally
        Interpolator.Free;
      end;
    end;

    DataSet.UpdateDimensions(frmGoPhast.Grid.LayerCount,
      frmGoPhast.Grid.RowCount, frmGoPhast.Grid.ColumnCount);

    comboDataSets.Items[0] := NewDataSetName;
    comboDataSets.Text := NewDataSetName;
    comboDataSets.ItemIndex := 0;
  end;
end;

function TfrmImportDXF.CoordConvert(P: Point3D; OCS: pMatrix): Point3D;
begin
  if OCS = nil then
  begin
    result := P;
  end
  else
  begin
    result := TransformPoint(OCS^, P);
  end;
end;

procedure TfrmImportDXF.SetData;
var
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  ScreenObjectList: TList;
  PointIndex: integer;
  DataSetName: string;
  Position: integer;
  DataSet: TDataArray;
  EntityCount: Integer;
  LayerIndex: integer;
  ALayer: DXF_Layer;
  EntityListIndex: integer;
  EList: Entity_List;
  EntityIndex: integer;
  Entity: DXF_Entity;
  PrimitiveList: TPrimitiveList;
  PrimitiveIndex: integer;
  Points: pointlist;
  InvalidPointCount: integer;
  Undo: TUndoImportDXFFile;
  Root: string;
  ExistingObjectCount: integer;
  function ConvertPoint(const DXF_Point: Point3D): TPoint2D;
  begin
    result.X := DXF_Point.X;
    result.Y := DXF_Point.Y;
  end;
begin
  SetLength(Points, 0);
  InvalidPointCount := 0;
  frmGoPhast.PhastModel.BeginScreenObjectUpdate;
  frmGoPhast.CanDraw := False;
  try
    MakeNewDataSet;
    EntityCount := 0;
    for LayerIndex := 0 to FDxfObject.layer_lists.Count - 1 do
    begin
      ALayer := FDxfObject.layer_lists[LayerIndex];
      for EntityListIndex := 0 to ALayer.entity_lists.Count - 1 do
      begin
        if ALayer.entity_names[EntityListIndex] <> Block_.ClassName then
        begin
          EList := ALayer.entity_lists[EntityListIndex];
          EntityCount := EntityCount + EList.entities.Count;
        end;
      end;
    end;
    frmProgress.Prefix := 'Object ';
    frmProgress.PopupParent := self;
    frmProgress.Show;
    frmProgress.pbProgress.Max := EntityCount;
    frmProgress.pbProgress.Position := 0;
    frmProgress.ProgressLabelCaption := '0 out of '
      + IntToStr(EntityCount) + '.';
    DataSetName := comboDataSets.Text;
//    Position := frmGoPhast.PhastModel.IndexOfDataSet(DataSetName);
  //  Assert(Position >= 0);
    DataSet := frmGoPhast.PhastModel.GetDataSetByName(DataSetName);
    Assert(DataSet <> nil);
    ScreenObjectList := TList.Create;
    //MultipleParts := false;
    try
      Undo := TUndoImportDXFFile.Create;
      try
        Root := TScreenObject.ValidName(
        ExtractFileRoot(OpenDialogDXF.FileName)+ '_');
        ScreenObjectList.Capacity := EntityCount;
        ExistingObjectCount :=
          frmGoPhast.PhastModel.NumberOfLargestScreenObjectsStartingWith(Root);
        for LayerIndex := 0 to FDxfObject.layer_lists.Count - 1 do
        begin
          ALayer := FDxfObject.layer_lists[LayerIndex];
          for EntityListIndex := 0 to ALayer.entity_lists.Count - 1 do
          begin
            if ALayer.entity_names[EntityListIndex] <> Block_.ClassName then
            begin
              EList := ALayer.entity_lists[EntityListIndex];
              for EntityIndex := 0 to EList.entities.Count - 1 do
              begin
                Entity := EList.entities[EntityIndex];
                SetLength(PrimitiveList, 0);
                Entity.GetCoordinates(PrimitiveList, CoordConvert, nil);
                for PrimitiveIndex := 0 to Length(PrimitiveList) - 1 do
                begin
                  Points := PrimitiveList[PrimitiveIndex];
                  if Length(Points) > 0 then
                  begin
                    AScreenObject :=
                      TScreenObject.CreateWithViewDirection(
                      frmGoPhast.PhastModel, vdTop,
                      UndoCreateScreenObject, False);
                    Inc(ExistingObjectCount);
                    AScreenObject.Name := Root + IntToStr(ExistingObjectCount);
                    AScreenObject.SetValuesOfEnclosedCells
                      := cbEnclosedCells.Checked;
                    AScreenObject.SetValuesOfIntersectedCells
                      := cbIntersectedCells.Checked;
                    AScreenObject.SetValuesByInterpolation
                      := cbInterpolation.Checked;
                    AScreenObject.ColorLine := Entity.colour <> clBlack;
                    AScreenObject.LineColor := Entity.colour;
                    AScreenObject.FillScreenObject := AScreenObject.ColorLine;
                    AScreenObject.FillColor := Entity.colour;
                    AScreenObject.ElevationCount := ecZero;
                    AScreenObject.Capacity := Length(Points);
                    try
                      for PointIndex := 0 to Length(Points) - 1 do
                      begin
                        AScreenObject.AddPoint(ConvertPoint(Points[PointIndex]), False);
                      end;
                      ScreenObjectList.Add(AScreenObject);
                      Position := AScreenObject.AddDataSet(DataSet);
                      Assert(Position >= 0);
                      AScreenObject.DataSetFormulas[Position]
                        := FloatToStr(Entity.p1.z);
                    except on E: EScreenObjectError do
                      begin
                        Inc(InvalidPointCount);
                        AScreenObject.Free;
                      end
                    end;
                  end;
                end;
                frmProgress.StepIt;
                Application.ProcessMessages;
              end;
            end;
          end;
        end;
        if ScreenObjectList.Count > 0 then
        begin
          Undo.StoreNewScreenObjects(ScreenObjectList);
          frmGoPhast.UndoStack.Submit(Undo);
          frmGoPhast.PhastModel.AddFileToArchive(FDxfName);
        end
        else
        begin
          Undo.Free
        end;
      except
        Undo.Free;
        raise;
      end;
    finally
      ScreenObjectList.Free;
      frmProgress.Hide;
    end;
  finally
    frmGoPhast.CanDraw := True;
    frmGoPhast.PhastModel.EndScreenObjectUpdate;
  end;
  if InvalidPointCount > 0 then
  begin
    Beep;
    MessageDlg(IntToStr(InvalidPointCount) + ' objects were invalid because they cross '
      + 'themselves and have been skipped.',
      mtWarning, [mbOK], 0);
  end;
end;

procedure TfrmImportDXF.btnOKClick(Sender: TObject);
begin
  inherited;
  Hide;
  SetData;
end;

procedure TfrmImportDXF.GetInterpolators;
var
  List: TList;
  Index: integer;
  AType: TInterpolatorType;
begin
  Assert(SizeOf(TObject) = SizeOf(TInterpolatorType));
  List := TList.Create;
  try
    AddInterpolatorsToList(List);
    comboInterpolators.Items.Add('none');
    for Index := 0 to List.Count - 1 do
    begin
      AType := List[Index];
      comboInterpolators.Items.AddObject(AType.InterpolatorName,
        TObject(AType));
    end;
  finally
    List.Free;
  end;
end;

procedure TfrmImportDXF.GetDataSets;
var
  EvalAt: TEvaluatedAt;
  DataSet: TDataArray;
  Index: integer;
begin
  EvalAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
  with comboDataSets.Items do
  begin
    Clear;
    AddObject(rsNewDataSet, nil);
    for Index := 0 to frmGoPhast.PhastModel.DataSetCount - 1 do
    begin
      DataSet := frmGoPhast.PhastModel.DataSets[Index];
      if (DataSet.EvaluatedAt = EvalAt)
        and (DataSet.Orientation = dsoTop)
        and (DataSet.DataType = rdtDouble) then
      begin
        AddObject(DataSet.Name, DataSet);
      end;
    end;
  end;
  comboDataSets.ItemIndex := 0;
  comboDataSetsChange(nil);
end;

procedure TfrmImportDXF.SetCheckBoxCaptions;
var
  NodeElemString: string;
begin
  NodeElemString := EvalAtToString(TEvaluatedAt(rgEvaluatedAt.ItemIndex),
       frmGoPhast.ModelSelection, True, False);
  cbEnclosedCells.Caption := rsSetValueOfEnclosed + NodeElemString;
  cbIntersectedCells.Caption := rsSetValueOfIntersected + NodeElemString;
  cbInterpolation.Caption := rsSetValueOf + NodeElemString + rsByInterpolation;
//  case rgEvaluatedAt.ItemIndex of
//    0:
//      begin
//        cbEnclosedCells.Caption := rsSetValueOfEnclosedElements;
//        cbIntersectedCells.Caption := rsSetValueOfIntersectedElements;
//        cbInterpolation.Caption := rsSetValueOfElementsByInterpolation;
//      end;
//    1:
//      begin
//        cbEnclosedCells.Caption := rsSetValueOfEnclosedNodes;
//        cbIntersectedCells.Caption := rsSetValueOfIntersectedNodes;
//        cbInterpolation.Caption := rsSetValueOfNodesByInterpolation;
//      end;
//  else
//    Assert(False);
//  end;
end;

procedure TfrmImportDXF.rgEvaluatedAtClick(Sender: TObject);
begin
  inherited;
  SetCheckBoxCaptions;
  GetDataSets;
end;

procedure TfrmImportDXF.FormDestroy(Sender: TObject);
begin
  inherited;
  FDxfObject.Free;
end;

procedure TfrmImportDXF.comboDataSetsChange(Sender: TObject);
begin
  inherited;
  comboInterpolators.Enabled := comboDataSets.Text = rsNewDataSet;
end;

procedure TfrmImportDXF.comboInterpolatorsChange(Sender: TObject);
begin
  inherited;
  cbInterpolation.Enabled := comboInterpolators.ItemIndex <> 0;
  if not cbInterpolation.Enabled then
  begin
    cbInterpolation.Checked := False;
  end;
end;

{ TUndoImportDXFFile }

function TUndoImportDXFFile.Description: string;
begin
  result := 'import DXF file';
end;

procedure TfrmImportDXF.cbEnclosedCellsClick(Sender: TObject);
begin
  inherited;
  EmphasizeCheckBoxes([cbEnclosedCells, cbIntersectedCells, cbInterpolation]);
  btnOK.Enabled := cbEnclosedCells.Checked or
    cbIntersectedCells.Checked or
    cbInterpolation.Checked;
end;

end.


