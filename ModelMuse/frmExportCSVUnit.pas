unit frmExportCSVUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, VirtualTrees, DataSetUnit, StdCtrls, ExtCtrls,
  Buttons, GoPhastTypes;

type
  TfrmExportCSV = class(TfrmCustomGoPhast)
    sdSaveCSV: TSaveDialog;
    Panel1: TPanel;
    vstDataSets: TVirtualStringTree;
    rgOrientation: TRadioGroup;
    rgEvaluatedAt: TRadioGroup;
    btnHelp: TBitBtn;
    btnSave: TBitBtn;
    pnlModel: TPanel;
    lblModel: TLabel;
    comboModel: TComboBox;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure vstDataSetsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstDataSetsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure rgOrientationClick(Sender: TObject);
    procedure rgEvaluatedAtClick(Sender: TObject);
    procedure vstDataSetsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure sdSaveCSVTypeChange(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    FDataSets: TList;
    FFileStream: TFileStream;
    procedure GetData;
    procedure SetData;
    function SelectDataArrays(DataArray: TDataArray): boolean;
    procedure GetOrientationAndEvalAt(var EvaluatedAt: TEvaluatedAt;
      var Orientation: TDataSetOrientation);
    procedure WriteString(const Value: String);
    procedure NewLine;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmExportCSV: TfrmExportCSV;

implementation

uses
  Contnrs, ClassificationUnit, frmGoPhastUnit, PhastModelUnit, AbstractGridUnit,
  CustomModflowWriterUnit, RbwParser;

resourcestring
  StrYouMustDefineThe = 'You must define the grid before you can export data' +
  '.';
  StrNoDataSetsHaveBe = 'No data sets have been selected. Do you want to jus' +
  't export the coordinates?';
  StrXYColumn = '"X", "Y", "Column", "Row"';
  StrXYZColu = '"X", "Y", "Z", "Column", "Row", "Layer"';

{$R *.dfm}

{ TfrmExportCSV }

procedure TfrmExportCSV.btnSaveClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmExportCSV.FormCreate(Sender: TObject);
var
  Index: Integer;
  ChildModel: TChildModel;
begin
  inherited;
  FDataSets := TObjectList.Create;
  rgEvaluatedAt.Enabled := frmGoPhast.PhastModel.ModelSelection = msPhast;

  comboModel.AddItem(StrParentModel, frmGoPhast.PhastModel);
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for Index := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[Index].ChildModel;
      comboModel.AddItem(ChildModel.ModelName, ChildModel);
    end;
  end
  else
  begin
    pnlModel.Visible := False;
  end;
  comboModel.ItemIndex := 0;

  GetData;
end;

procedure TfrmExportCSV.FormDestroy(Sender: TObject);
begin
  FDataSets.Free;
  inherited;
end;

procedure TfrmExportCSV.GetData;
var
  Node: PVirtualNode;
begin
  vstDataSets.Clear;
  FillVirtualStringTreeWithDataSets(vstDataSets,
    FDataSets, nil, SelectDataArrays);
  Node := vstDataSets.GetFirst;
  while Assigned(Node) do
  begin
    if Node.ChildCount > 0 then
    begin
      Node.CheckType := ctTriStateCheckBox;
    end
    else
    begin
      Node.CheckType := ctCheckBox;
    end;
    Node := vstDataSets.GetNext(Node);
  end;
end;

procedure TfrmExportCSV.rgEvaluatedAtClick(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmExportCSV.rgOrientationClick(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmExportCSV.sdSaveCSVTypeChange(Sender: TObject);
begin
  inherited;
  case sdSaveCSV.FilterIndex of
    1:
      begin
        sdSaveCSV.DefaultExt := 'csv';
      end;
    2:
      begin
        sdSaveCSV.DefaultExt := '';
      end;
    else Assert(False);
  end;
end;

function TfrmExportCSV.SelectDataArrays(DataArray: TDataArray): boolean;
var
  Orientation: TDataSetOrientation;
  EvaluatedAt: TEvaluatedAt;
begin
  GetOrientationAndEvalAt(EvaluatedAt, Orientation);
  result := (DataArray.Orientation = Orientation)
    and (DataArray.EvaluatedAt = EvaluatedAt)
end;

procedure TfrmExportCSV.SetData;
var
  DataArrayList: TList;
  Node: PVirtualNode;
  NodeData: PClassificationNodeData;
  ADataArray: TDataArray;
  EvaluatedAt: TEvaluatedAt;
  Orientation: TDataSetOrientation;
  Index: Integer;
  Grid: TCustomModelGrid;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColumnIndex: Integer;
  Location: T3DRealPoint;
  DataArrayManager: TDataArrayManager;
  LocalModel: TCustomModel;
  function FreeFormattedReal(
    const Value: double): string;
  begin
    result := TCustomModflowWriter.FortranDecimal(Format('%.13e', [Value]));
  end;
  procedure WriteALine;
  var
    Index: integer;
  begin
    WriteString(FreeFormattedReal(Location.X) + ', ');
    WriteString(FreeFormattedReal(Location.Y));
    if (Orientation = dso3D) then
    begin
      WriteString(', ' + FreeFormattedReal(Location.Z));
    end;
    WriteString(', ' + IntToStr(ColumnIndex+1));
    WriteString(', ' + IntToStr(RowIndex+1));
    if (Orientation = dso3D) then
    begin
      WriteString(', ' + IntToStr(LayerIndex+1));
    end;
    for Index := 0 to DataArrayList.Count - 1 do
    begin
      ADataArray := DataArrayList[Index];
      case ADataArray.DataType of
        rdtDouble:
          begin
            WriteString(', ' + FreeFormattedReal(
              ADataArray.RealData[LayerIndex, RowIndex, ColumnIndex]));
          end;
        rdtInteger: 
          begin
            WriteString(', ' + IntToStr(
              ADataArray.IntegerData[LayerIndex, RowIndex, ColumnIndex]));
          end;
        rdtBoolean: 
          begin
            if ADataArray.BooleanData[LayerIndex, RowIndex, ColumnIndex] then
            begin
              WriteString(', True');
            end
            else
            begin
              WriteString(', False');
            end;
          end;
        rdtString: 
          begin
            WriteString(', "' +
              ADataArray.StringData[LayerIndex, RowIndex, ColumnIndex] + '"' );
          end;
        else Assert(False);
      end;
    end;
    NewLine;
  end;
begin
  LocalModel := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;
  Grid := LocalModel.Grid;
  if (Grid.ColumnCount = 0)
    or (Grid.RowCount = 0)
    or (Grid.LayerCount = 0) then
  begin
    MessageDlg(StrYouMustDefineThe, mtError, [mbOK], 0);
    Exit;
  end;
  Screen.Cursor := crHourGlass;
  DataArrayList := TList.Create;
  try
    Node := vstDataSets.GetFirst;
    while Assigned(Node) do
    begin
      if (Node.ChildCount = 0)
        and (Node.CheckState in [csCheckedNormal, csCheckedPressed]) then
      begin
        NodeData := vstDataSets.GetNodeData(Node);
        if Assigned(NodeData)
          and Assigned(NodeData.ClassificationObject) then
        begin
          if NodeData.ClassificationObject is TDataSetClassification then
          begin
            ADataArray := TDataSetClassification(
              NodeData.ClassificationObject).DataArray;
            if LocalModel <> frmGoPhast.PhastModel then
            begin
              ADataArray := LocalModel.DataArrayManager.
                GetDataSetByName(ADataArray.Name);
              Assert(ADataArray <> nil);
            end;
            DataArrayList.Add(ADataArray);
          end
        end;
      end;
      Node := vstDataSets.GetNext(Node);
    end;
    if DataArrayList.Count = 0 then
    begin
      if (MessageDlg(StrNoDataSetsHaveBe,
        mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
      begin
        Exit;
      end;
    end;
    if sdSaveCSV.Execute then
    begin
      GetOrientationAndEvalAt(EvaluatedAt, Orientation);
      try
        FFileStream := TFileStream.Create(sdSaveCSV.FileName,
          fmCreate or fmShareDenyWrite);
        try
          case Orientation of
            dsoTop:
              begin
                WriteString(StrXYColumn);
              end;
            dso3D:
              begin
                WriteString(StrXYZColu);
              end;
            else Assert(False)
          end;
          for Index := 0 to DataArrayList.Count - 1 do
          begin
            ADataArray := DataArrayList[Index];
            WriteString(', "' + ADataArray.Name + '"');
            ADataArray.Initialize;
          end;
          NewLine;

          case EvaluatedAt of
            eaBlocks:
              begin
                case Orientation of
                  dsoTop:
                    begin
                      LayerIndex := 0;
                      for RowIndex := 0 to Grid.RowCount - 1 do
                      begin
                        for ColumnIndex := 0 to Grid.ColumnCount - 1 do
                        begin
                          Location := Grid.
                            RotatedThreeDElementCenter(
                            ColumnIndex, RowIndex, 0);
                          WriteALine;
                        end;
                      end;
                    end;
                  dso3D:
                    begin
                      for LayerIndex := 0 to Grid.LayerCount - 1 do
                      begin
                        for RowIndex := 0 to Grid.RowCount - 1 do
                        begin
                          for ColumnIndex := 0 to Grid.ColumnCount - 1 do
                          begin
                            Location := Grid.
                              RotatedThreeDElementCenter(
                              ColumnIndex, RowIndex, LayerIndex);
                            WriteALine;
                          end;
                        end;
                      end;
                    end;
                  else Assert(False);
                end;
              end;
            eaNodes:
              begin
                case Orientation of
                  dsoTop:
                    begin
                      LayerIndex := 0;
                      for RowIndex := 0 to Grid.RowCount do
                      begin
                        for ColumnIndex := 0 to Grid.ColumnCount do
                        begin
                          Location := Grid.
                            RotatedThreeDElementCorner(
                            ColumnIndex, RowIndex, 0);
                          WriteALine;
                        end;
                      end;
                    end;
                  dso3D:
                    begin
                      for LayerIndex := 0 to Grid.LayerCount do
                      begin
                        for RowIndex := 0 to Grid.RowCount do
                        begin
                          for ColumnIndex := 0 to Grid.ColumnCount do
                          begin
                            Location := Grid.
                              RotatedThreeDElementCorner(
                              ColumnIndex, RowIndex, LayerIndex);
                            WriteALine;
                          end;
                        end;
                      end;
                    end;
                  else Assert(False);
                end;
              end;
            else Assert(False);
          end;
        finally
          FFileStream.Free;
        end;
      except on E: EFCreateError do
        begin
          Beep;
          MessageDlg(E.message, mtError, [mbOK], 0);
          Exit;
        end;
      end;
    end;
    if DataArrayList.Count > 0 then
    begin
      DataArrayManager := LocalModel.DataArrayManager;
      for Index := 0 to DataArrayList.Count - 1 do
      begin
        ADataArray := DataArrayList[Index];
        DataArrayManager.AddDataSetToCache(ADataArray);
      end;
      DataArrayManager.CacheDataArrays;
    end;
  finally
    Screen.Cursor := crDefault;
    DataArrayList.Free;
  end;
  ModalResult := mrOK;
end;
procedure TfrmExportCSV.GetOrientationAndEvalAt(var EvaluatedAt: TEvaluatedAt;
  var Orientation: TDataSetOrientation);
begin
  Orientation := dso3D;
  case rgOrientation.ItemIndex of
    0:
      begin
        Orientation := dsoTop;
      end;
    1:
      begin
        Orientation := dso3D;
      end;
  else
    Assert(False);
  end;
  Assert(rgEvaluatedAt.ItemIndex >= 0);
  EvaluatedAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
end;

procedure TfrmExportCSV.vstDataSetsChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  ChildNode: PVirtualNode;
begin
  inherited;
  if Node.CheckState in [csUncheckedNormal,
    csUncheckedPressed, csCheckedNormal, csCheckedPressed] then
  begin
    if Node.ChildCount > 0 then
    begin
      ChildNode := Node.FirstChild;
      while Assigned(ChildNode) do
      begin
        ChildNode.CheckState := Node.CheckState;
        ChildNode := ChildNode.NextSibling;
      end;
    end;
  end;
end;

procedure TfrmExportCSV.vstDataSetsGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

procedure TfrmExportCSV.vstDataSetsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmExportCSV.WriteString(const Value: String);
begin
  if Length(Value) > 0 then
  begin
    FFileStream.Write(Value[1], Length(Value)*SizeOf(Char));
  end;
end;

procedure TfrmExportCSV.NewLine;
begin
  WriteString(sLineBreak);
end;


end.
