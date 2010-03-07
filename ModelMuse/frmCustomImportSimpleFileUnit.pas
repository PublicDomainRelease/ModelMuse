{@abstract(The main purpose of @name is to define
  @link(TfrmCustomImportSimpleFile)
  which is used as a base class for importing
  DXF and Surfer grid files into ModelMuse. )}
unit frmCustomImportSimpleFileUnit;


interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Buttons, ExtCtrls,
  Grids, IntListUnit, ScreenObjectUnit, DXF_Structs, DXF_read, DXF_Utils,
  frmImportShapefileUnit;

type

  {@abstract(@name is a base class used to import DXF and Surfer
    grid files into ModelMuse.)
    See @link(TfrmGoPhast.miImportDXFFileClick).}
  TfrmCustomImportSimpleFile = class(TfrmCustomGoPhast)
    // Clicking @name closes @classname without doing anything.
    btnCancel: TBitBtn;
    // Clicking @name displays help about @classname.
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    // @name indicates that the value of the imported @link(TScreenObject)s
    // should set the value of enclosed cells or elements in the related
    // @link(TDataArray).
    cbEnclosedCells: TCheckBox;
    // @name indicates that the value of the imported @link(TScreenObject)s
    // should set the value of cells or elements in the related
    // @link(TDataArray) by interpolation.
    cbInterpolation: TCheckBox;
    // @name indicates that the value of the imported @link(TScreenObject)s
    // should set the value of intersected cells or elements in the related
    // @link(TDataArray).
    cbIntersectedCells: TCheckBox;
    // @name is used to select the name of the @link(TDataArray) to be
    // affected by the imported @link(TScreenObject)s.
    comboDataSets: TComboBox;
    // @name is the name of the @link(TCustom2DInterpolater) that will
    // be used with a new @link(TDataArray).
    comboInterpolators: TComboBox;
    // @name displays "Data Set".
    lblDataSet: TLabel;
    // @name displays "Interpolator".
    lblInterpolator: TLabel;
    // @name is used to select the DXF file.
    OpenDialogFile: TOpenDialog;
    // @name indicates whether a new data set will be evaluated at
    // elements or cells.
    rgEvaluatedAt: TRadioGroup;
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
    // @name changes the captions of @link(cbEnclosedCells),
    // @link(cbIntersectedCells), and @link(cbInterpolation).
    procedure rgEvaluatedAtClick(Sender: TObject);
  protected
    procedure UpdateEvalAt;
    // @name fills @link(comboDataSets) with the names of
    // @link(TDataArray)s that can be used the the imported
    // @link(TScreenObject)s.
    procedure GetDataSets;
    // @name fills @link(comboInterpolators) with a list of
    // @link(TCustom2DInterpolater)s.
    procedure GetInterpolators;
    // @name creates a new @link(TDataArray).
    procedure MakeNewDataSet(NewDataSets: TList; Suffix, Classification: string);
    { Set the captions of @link(cbEnclosedCells), @link(cbIntersectedCells),
      and @link(cbInterpolation) based on @link(rgEvaluatedAt).ItemIndex.}
    procedure SetCheckBoxCaptions;
    { Protected declarations }
  public
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit, GoPhastTypes, DataSetUnit, 
  RbwParser, UndoItems, frmProgressUnit, frmDataSetsUnits, ModelMuseUtilities, FastGEO;

{$R *.dfm}

procedure TfrmCustomImportSimpleFile.FormCreate(Sender: TObject);
begin
  inherited;
  cbEnclosedCellsClick(nil);
  SetCheckBoxCaptions;
  GetInterpolators;
end;

procedure TfrmCustomImportSimpleFile.MakeNewDataSet(NewDataSets: TList; Suffix, Classification: string);
var
  NewDataSetName: string;
  DataSet: TDataArray;
  AType: TInterpolatorType;
  Interpolator: TCustom2DInterpolater;

begin
  Assert(SizeOf(TObject) = SizeOf(TInterpolatorType));
  if comboDataSets.ItemIndex = 0 then
  begin
    NewDataSetName := ExtractFileName(OpenDialogFile.FileName);
    NewDataSetName := ChangeFileExt(NewDataSetName, '');
    NewDataSetName := GenerateNewName(NewDataSetName + Suffix);

    DataSet := frmGoPhast.PhastModel.CreateNewDataArray(TDataArray,
      NewDataSetName, '0.', [], rdtDouble,
      TEvaluatedAt(rgEvaluatedAt.ItemIndex), dsoTop, Classification);

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
    NewDataSets.add(DataSet);

    comboDataSets.Items[0] := NewDataSetName;
    comboDataSets.Text := NewDataSetName;
    comboDataSets.ItemIndex := 0;
  end;
end;



procedure TfrmCustomImportSimpleFile.GetInterpolators;
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

procedure TfrmCustomImportSimpleFile.GetDataSets;
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

procedure TfrmCustomImportSimpleFile.SetCheckBoxCaptions;
var
  NodeElemString: string;
begin
  NodeElemString := EvalAtToString(TEvaluatedAt(rgEvaluatedAt.ItemIndex),
       frmGoPhast.ModelSelection, True, False);
  cbEnclosedCells.Caption := rsSetValueOfEnclosed + NodeElemString;
  cbIntersectedCells.Caption := rsSetValueOfIntersected + NodeElemString;
  cbInterpolation.Caption := rsSetValueOf + NodeElemString + rsByInterpolation;
end;

procedure TfrmCustomImportSimpleFile.UpdateEvalAt;
begin
  rgEvaluatedAt.Items[Ord(eaBlocks)] :=
    EvalAtToString(eaBlocks, frmGoPhast.PhastModel.ModelSelection, True, True);
  rgEvaluatedAt.Items[Ord(eaNodes)] :=
    EvalAtToString(eaNodes, frmGoPhast.PhastModel.ModelSelection, True, True);
  rgEvaluatedAt.Enabled :=
    frmGoPhast.PhastModel.ModelSelection = msPhast;
end;

procedure TfrmCustomImportSimpleFile.rgEvaluatedAtClick(Sender: TObject);
begin
  inherited;
  SetCheckBoxCaptions;
  GetDataSets;
end;

procedure TfrmCustomImportSimpleFile.comboDataSetsChange(Sender: TObject);
begin
  inherited;
  comboInterpolators.Enabled := comboDataSets.Text = rsNewDataSet;
end;

procedure TfrmCustomImportSimpleFile.comboInterpolatorsChange(Sender: TObject);
begin
  inherited;
  cbInterpolation.Enabled := comboInterpolators.ItemIndex <> 0;
  if not cbInterpolation.Enabled then
  begin
    cbInterpolation.Checked := False;
  end;
end;

procedure TfrmCustomImportSimpleFile.cbEnclosedCellsClick(Sender: TObject);
begin
  inherited;
  EmphasizeCheckBoxes([cbEnclosedCells, cbIntersectedCells, cbInterpolation]);
  btnOK.Enabled := cbEnclosedCells.Checked or
    cbIntersectedCells.Checked or
    cbInterpolation.Checked;
end;

end.


