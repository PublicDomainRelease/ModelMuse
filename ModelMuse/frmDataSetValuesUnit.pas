unit frmDataSetValuesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, VirtualTrees,
  ComCtrls, Grids, RbwDataGrid4, JvExStdCtrls, JvListBox, SsButtonEd,
  RbwStringTreeCombo;

type
  TfrmDataSetValues = class(TfrmCustomGoPhast)
    Panel1: TPanel;
    btnClose: TBitBtn;
    btnHelp: TBitBtn;
    pcDataSet: TPageControl;
    btnCopy: TButton;
    Panel2: TPanel;
    lbLayers: TJvListBox;
    lblLayer: TLabel;
    lblDataSet: TLabel;
    comboModel: TComboBox;
    lblModel: TLabel;
    treecomboDataSets: TRbwStringTreeCombo;
    comboOrientation: TComboBox;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure treecomboDataSetsDropDownTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure treecomboDataSetsDropDownTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure treecomboDataSetsChange(Sender: TObject);
    procedure treecomboDataSetsDropDownTreeGetNodeDataSize(
      Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure btnCopyClick(Sender: TObject);
    procedure lbLayersMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure comboModelChange(Sender: TObject);
    procedure treecomboDataSets1TreeInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure comboOrientationChange(Sender: TObject);
  private
    FSelectedVirtNode: PVirtualNode;
    // @name is implemented as a TObjectList.
    FDataSets: TList;
    FTempControls: TList;
    procedure GetData;
    procedure SetSelectedNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    { Private declarations }
  public
    property SelectedVirtNode: PVirtualNode read FSelectedVirtNode;
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses Contnrs, ClassificationUnit, frmGoPhastUnit, DataSetUnit, RbwParser,
  GoPhastTypes, PhastModelUnit;

resourcestring
  StrParentModel = 'Parent model';
  StrLayer = 'Layer';
  StrRow = 'Row';
  StrColumn = 'Column';

procedure TfrmDataSetValues.btnCopyClick(Sender: TObject);
var
  Grid: TRbwDataGrid4;
begin
  inherited;
  if pcDataSet.ActivePage <> nil then
  begin
    Grid := pcDataSet.ActivePage.Controls[0] as TRbwDataGrid4;
    Grid.Options := Grid.Options - [goEditing];
    Grid.SelectAll;
    Grid.CopySelectedCellsToClipboard;
    Grid.ClearSelection;
  end;
end;

procedure TfrmDataSetValues.comboModelChange(Sender: TObject);
begin
  inherited;
  treecomboDataSetsChange(nil);
end;

procedure TfrmDataSetValues.comboOrientationChange(Sender: TObject);
begin
  inherited;
  treecomboDataSetsChange(Sender);
end;

procedure TfrmDataSetValues.FormCreate(Sender: TObject);
begin
  inherited;
  FDataSets := TObjectList.Create;
  FSelectedVirtNode := nil;
  FTempControls := TObjectList.Create;
  GetData;
end;

procedure TfrmDataSetValues.FormDestroy(Sender: TObject);
begin
  inherited;
  FTempControls.Free;
  FDataSets.Free;
end;

procedure TfrmDataSetValues.GetData;
var
  Node: PVirtualNode;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  case frmGoPhast.ModelSelection of
    msPhast, msModflow, msModflowNWT:
      begin
        comboModel.Items.AddObject(StrParentModel, frmGoPhast.PhastModel)
      end;
    msModflowLGR:
      begin
        comboModel.Items.AddObject(StrParentModel, frmGoPhast.PhastModel);
        for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
        begin
          ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
          comboModel.Items.AddObject(ChildModel.ModelName, ChildModel);
        end;
      end;
    else
      Assert(False);
  end;
  comboModel.ItemIndex := comboModel.Items.IndexOfObject(frmGoPhast.PhastModel.SelectedModel);

  FillVirtualStringTreeWithDataSets(treecomboDataSets.Tree,
    FDataSets, nil);
  Node := treecomboDataSets.Tree.GetFirst;
  if Node <> nil then
  begin
    treecomboDataSets.Tree.Selected[Node] := True;
    treecomboDataSetsChange(nil);
  end;
end;

procedure TfrmDataSetValues.lbLayersMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if lbLayers.ItemIndex >= 0 then
  begin
    pcDataSet.ActivePageIndex := lbLayers.ItemIndex;
  end;
end;

procedure TfrmDataSetValues.treecomboDataSets1TreeInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  CellText: string;
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmDataSetValues.treecomboDataSetsChange(Sender: TObject);
var
  DataArray: TDataArray;
  LayerIndex: Integer;
  APage: TTabSheet;
  AGrid: TRbwDataGrid4;
  ColIndex: Integer;
  ColumnFormat: TRbwColumnFormat4;
  RowIndex: Integer;
  LocalModel: TCustomModel;
  RowIndex2: Integer;
  ColIndex2: Integer;
begin
  inherited;
  UpdateTreeComboText(SelectedVirtNode, treecomboDataSets);
  DataArray := frmGoPhast.PhastModel.DataArrayManager.
    GetDataSetByName(treecomboDataSets.Text);
  if DataArray = nil then
  begin
    Exit;
  end;

  LocalModel := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;
  if LocalModel <> frmGoPhast.PhastModel then
  begin
    DataArray := LocalModel.DataArrayManager.GetDataSetByName(DataArray.Name);
  end;

  FTempControls.Clear;
  lbLayers.Items.Clear;
  DataArray.Initialize;
  case DataArray.Orientation of
    dso3D:
      begin
        case comboOrientation.ItemIndex of
          0:
            begin
              // Layer
              lblLayer.Caption := StrLayer;
              for LayerIndex := 0 to DataArray.LayerCount - 1 do
              begin
                lbLayers.Items.Add(IntToStr(LayerIndex+1));
                APage := TTabSheet.Create(self);

                FTempControls.Add(APage);
                APage.PageControl := pcDataSet;
                APage.Caption := IntToStr(LayerIndex+1);

                AGrid := TRbwDataGrid4.Create(self);
                AGrid.Parent := APage;
                AGrid.Align := alClient;
                AGrid.ColCount := DataArray.ColumnCount + 1;
                AGrid.Options := AGrid.Options - [goEditing];
                AGrid.DefaultColWidth := 10;
                AGrid.ColorSelectedRow := False;
                AGrid.AutoMultiEdit := True;
                ColumnFormat := rcf4Real;
                case DataArray.DataType of
                  rdtDouble: ColumnFormat := rcf4Real;
                  rdtInteger: ColumnFormat := rcf4Integer;
                  rdtBoolean: ColumnFormat := rcf4Boolean;
                  rdtString: ColumnFormat := rcf4String;
                  else Assert(False);
                end;
                AGrid.Columns[0].AutoAdjustColWidths := True;
                for ColIndex := 1 to AGrid.ColCount - 1 do
                begin
                  AGrid.Columns[ColIndex].AutoAdjustColWidths := True;
                  AGrid.Columns[ColIndex].Format := ColumnFormat;
                  AGrid.Cells[ColIndex,0] := IntToStr(ColIndex);
                end;
                AGrid.RowCount := DataArray.RowCount + 1;
                AGrid.BeginUpdate;
                try
                  for RowIndex := 1 to AGrid.RowCount - 1 do
                  begin
                    AGrid.Cells[0,RowIndex] := IntToStr(RowIndex);
                  end;
                  for ColIndex := 0 to DataArray.ColumnCount - 1 do
                  begin
                    for RowIndex := 0 to DataArray.RowCount - 1 do
                    begin
                      case DataArray.DataType of
                        rdtDouble: AGrid.Cells[ColIndex+1,RowIndex+1] :=
                          FloatToStr(DataArray.RealData[LayerIndex, RowIndex, ColIndex]);
                        rdtInteger: AGrid.Cells[ColIndex+1,RowIndex+1] :=
                          IntToStr(DataArray.IntegerData[LayerIndex, RowIndex, ColIndex]);
                        rdtBoolean: AGrid.Checked[ColIndex+1,RowIndex+1] :=
                          DataArray.BooleanData[LayerIndex, RowIndex, ColIndex];
                        rdtString: AGrid.Cells[ColIndex+1,RowIndex+1] :=
                          DataArray.StringData[LayerIndex, RowIndex, ColIndex];
                      end;
                    end;
                  end;
                finally
                  AGrid.EndUpdate;
                end;
                APage.TabVisible := False;
              end
            end;
          1:
            begin
              // Row
              lblLayer.Caption := StrRow;
              for RowIndex := 0 to DataArray.RowCount - 1 do
              begin
                lbLayers.Items.Add(IntToStr(RowIndex+1));
                APage := TTabSheet.Create(self);

                FTempControls.Add(APage);
                APage.PageControl := pcDataSet;
                APage.Caption := IntToStr(RowIndex+1);

                AGrid := TRbwDataGrid4.Create(self);
                AGrid.Parent := APage;
                AGrid.Align := alClient;
                AGrid.ColCount := DataArray.ColumnCount + 1;
                AGrid.Options := AGrid.Options - [goEditing];
                AGrid.DefaultColWidth := 10;
                AGrid.ColorSelectedRow := False;
                AGrid.AutoMultiEdit := True;
                ColumnFormat := rcf4Real;
                case DataArray.DataType of
                  rdtDouble: ColumnFormat := rcf4Real;
                  rdtInteger: ColumnFormat := rcf4Integer;
                  rdtBoolean: ColumnFormat := rcf4Boolean;
                  rdtString: ColumnFormat := rcf4String;
                  else Assert(False);
                end;
                AGrid.Columns[0].AutoAdjustColWidths := True;
                for ColIndex := 1 to AGrid.ColCount - 1 do
                begin
                  AGrid.Columns[ColIndex].AutoAdjustColWidths := True;
                  AGrid.Columns[ColIndex].Format := ColumnFormat;
                  AGrid.Cells[ColIndex,0] := IntToStr(ColIndex);
                end;
                AGrid.RowCount := DataArray.LayerCount + 1;
                AGrid.BeginUpdate;
                try
                  for RowIndex2 := 1 to AGrid.RowCount - 1 do
                  begin
                    AGrid.Cells[0,RowIndex2] := IntToStr(RowIndex2);
                  end;
                  for ColIndex := 0 to DataArray.ColumnCount - 1 do
                  begin
                    for LayerIndex := 0 to DataArray.LayerCount - 1 do
                    begin
                      case DataArray.DataType of
                        rdtDouble: AGrid.Cells[ColIndex+1,LayerIndex+1] :=
                          FloatToStr(DataArray.RealData[LayerIndex, RowIndex, ColIndex]);
                        rdtInteger: AGrid.Cells[ColIndex+1,LayerIndex+1] :=
                          IntToStr(DataArray.IntegerData[LayerIndex, RowIndex, ColIndex]);
                        rdtBoolean: AGrid.Checked[ColIndex+1,LayerIndex+1] :=
                          DataArray.BooleanData[LayerIndex, RowIndex, ColIndex];
                        rdtString: AGrid.Cells[ColIndex+1,LayerIndex+1] :=
                          DataArray.StringData[LayerIndex, RowIndex, ColIndex];
                      end;
                    end;
                  end;
                finally
                  AGrid.EndUpdate;
                end;
                APage.TabVisible := False;
              end
            end;
          2:
            begin
              // Column
              lblLayer.Caption := StrColumn;
              for ColIndex := 0 to DataArray.ColumnCount - 1 do
              begin
                lbLayers.Items.Add(IntToStr(ColIndex+1));
                APage := TTabSheet.Create(self);

                FTempControls.Add(APage);
                APage.PageControl := pcDataSet;
                APage.Caption := IntToStr(ColIndex+1);

                AGrid := TRbwDataGrid4.Create(self);
                AGrid.Parent := APage;
                AGrid.Align := alClient;
                AGrid.ColCount := DataArray.RowCount + 1;
                AGrid.Options := AGrid.Options - [goEditing];
                AGrid.DefaultColWidth := 10;
                AGrid.ColorSelectedRow := False;
                AGrid.AutoMultiEdit := True;
                ColumnFormat := rcf4Real;
                case DataArray.DataType of
                  rdtDouble: ColumnFormat := rcf4Real;
                  rdtInteger: ColumnFormat := rcf4Integer;
                  rdtBoolean: ColumnFormat := rcf4Boolean;
                  rdtString: ColumnFormat := rcf4String;
                  else Assert(False);
                end;
                AGrid.Columns[0].AutoAdjustColWidths := True;
                for ColIndex2 := 1 to AGrid.ColCount - 1 do
                begin
                  AGrid.Columns[ColIndex2].AutoAdjustColWidths := True;
                  AGrid.Columns[ColIndex2].Format := ColumnFormat;
                  AGrid.Cells[ColIndex2,0] := IntToStr(ColIndex2);
                end;
                AGrid.RowCount := DataArray.LayerCount + 1;
                AGrid.BeginUpdate;
                try
                  for RowIndex := 1 to AGrid.RowCount - 1 do
                  begin
                    AGrid.Cells[0,RowIndex] := IntToStr(RowIndex);
                  end;
                  for RowIndex := 0 to DataArray.RowCount - 1 do
                  begin
                    for LayerIndex := 0 to DataArray.LayerCount - 1 do
                    begin
                      case DataArray.DataType of
                        rdtDouble: AGrid.Cells[RowIndex+1,LayerIndex+1] :=
                          FloatToStr(DataArray.RealData[LayerIndex, RowIndex, ColIndex]);
                        rdtInteger: AGrid.Cells[RowIndex+1,LayerIndex+1] :=
                          IntToStr(DataArray.IntegerData[LayerIndex, RowIndex, ColIndex]);
                        rdtBoolean: AGrid.Checked[RowIndex+1,LayerIndex+1] :=
                          DataArray.BooleanData[LayerIndex, RowIndex, ColIndex];
                        rdtString: AGrid.Cells[RowIndex+1,LayerIndex+1] :=
                          DataArray.StringData[LayerIndex, RowIndex, ColIndex];
                      end;
                    end;
                  end;
                finally
                  AGrid.EndUpdate;
                end;
                APage.TabVisible := False;
              end
            end;
          else Assert(False);
        end;
      end;
    dsoTop:
      begin
        lblLayer.Caption := StrLayer;
        for LayerIndex := 0 to DataArray.LayerCount - 1 do
        begin
          lbLayers.Items.Add(IntToStr(LayerIndex+1));
          APage := TTabSheet.Create(self);

          FTempControls.Add(APage);
          APage.PageControl := pcDataSet;
          APage.Caption := IntToStr(LayerIndex+1);

          AGrid := TRbwDataGrid4.Create(self);
          AGrid.Parent := APage;
          AGrid.Align := alClient;
          AGrid.ColCount := DataArray.ColumnCount + 1;
          AGrid.Options := AGrid.Options - [goEditing];
          AGrid.DefaultColWidth := 10;
          AGrid.ColorSelectedRow := False;
          AGrid.AutoMultiEdit := True;
          ColumnFormat := rcf4Real;
          case DataArray.DataType of
            rdtDouble: ColumnFormat := rcf4Real;
            rdtInteger: ColumnFormat := rcf4Integer;
            rdtBoolean: ColumnFormat := rcf4Boolean;
            rdtString: ColumnFormat := rcf4String;
            else Assert(False);
          end;
          AGrid.Columns[0].AutoAdjustColWidths := True;
          for ColIndex := 1 to AGrid.ColCount - 1 do
          begin
            AGrid.Columns[ColIndex].AutoAdjustColWidths := True;
            AGrid.Columns[ColIndex].Format := ColumnFormat;
            AGrid.Cells[ColIndex,0] := IntToStr(ColIndex);
          end;
          AGrid.RowCount := DataArray.RowCount + 1;
          AGrid.BeginUpdate;
          try
            for RowIndex := 1 to AGrid.RowCount - 1 do
            begin
              AGrid.Cells[0,RowIndex] := IntToStr(RowIndex);
            end;
            for ColIndex := 0 to DataArray.ColumnCount - 1 do
            begin
              for RowIndex := 0 to DataArray.RowCount - 1 do
              begin
                case DataArray.DataType of
                  rdtDouble: AGrid.Cells[ColIndex+1,RowIndex+1] :=
                    FloatToStr(DataArray.RealData[LayerIndex, RowIndex, ColIndex]);
                  rdtInteger: AGrid.Cells[ColIndex+1,RowIndex+1] :=
                    IntToStr(DataArray.IntegerData[LayerIndex, RowIndex, ColIndex]);
                  rdtBoolean: AGrid.Checked[ColIndex+1,RowIndex+1] :=
                    DataArray.BooleanData[LayerIndex, RowIndex, ColIndex];
                  rdtString: AGrid.Cells[ColIndex+1,RowIndex+1] :=
                    DataArray.StringData[LayerIndex, RowIndex, ColIndex];
                end;
              end;
            end;
          finally
            AGrid.EndUpdate;
          end;
          APage.TabVisible := False;
        end;
      end;
    dsoFront:
      begin
        lblLayer.Caption := StrRow;
        lbLayers.Items.Add('1');
        APage := TTabSheet.Create(self);

        FTempControls.Add(APage);
        APage.PageControl := pcDataSet;
        APage.Caption := IntToStr(1);

        AGrid := TRbwDataGrid4.Create(self);
        AGrid.Parent := APage;
        AGrid.Align := alClient;
        AGrid.ColCount := DataArray.ColumnCount + 1;
        AGrid.Options := AGrid.Options - [goEditing];
        AGrid.DefaultColWidth := 10;
        AGrid.ColorSelectedRow := False;
        AGrid.AutoMultiEdit := True;
        ColumnFormat := rcf4Real;
        case DataArray.DataType of
          rdtDouble: ColumnFormat := rcf4Real;
          rdtInteger: ColumnFormat := rcf4Integer;
          rdtBoolean: ColumnFormat := rcf4Boolean;
          rdtString: ColumnFormat := rcf4String;
          else Assert(False);
        end;
        AGrid.Columns[0].AutoAdjustColWidths := True;
        for ColIndex := 1 to AGrid.ColCount - 1 do
        begin
          AGrid.Columns[ColIndex].AutoAdjustColWidths := True;
          AGrid.Columns[ColIndex].Format := ColumnFormat;
          AGrid.Cells[ColIndex,0] := IntToStr(ColIndex);
        end;
        AGrid.RowCount := DataArray.LayerCount + 1;
        AGrid.BeginUpdate;
        try
          for RowIndex := 1 to AGrid.RowCount - 1 do
          begin
            AGrid.Cells[0,RowIndex] := IntToStr(RowIndex);
          end;
          for ColIndex := 0 to DataArray.ColumnCount - 1 do
          begin
            for LayerIndex := 0 to DataArray.LayerCount - 1 do
            begin
              case DataArray.DataType of
                rdtDouble: AGrid.Cells[ColIndex+1,LayerIndex+1] :=
                  FloatToStr(DataArray.RealData[LayerIndex, 0, ColIndex]);
                rdtInteger: AGrid.Cells[ColIndex+1,LayerIndex+1] :=
                  IntToStr(DataArray.IntegerData[LayerIndex, 0, ColIndex]);
                rdtBoolean: AGrid.Checked[ColIndex+1,LayerIndex+1] :=
                  DataArray.BooleanData[LayerIndex, 0, ColIndex];
                rdtString: AGrid.Cells[ColIndex+1,LayerIndex+1] :=
                  DataArray.StringData[LayerIndex, 0, ColIndex];
              end;
            end;
          end;
        finally
          AGrid.EndUpdate;
        end;
        APage.TabVisible := False;
      end;
    dsoSide:
      begin
        lblLayer.Caption := StrColumn;
        lbLayers.Items.Add('1');
        APage := TTabSheet.Create(self);

        FTempControls.Add(APage);
        APage.PageControl := pcDataSet;
        APage.Caption := IntToStr(1);

        AGrid := TRbwDataGrid4.Create(self);
        AGrid.Parent := APage;
        AGrid.Align := alClient;
        AGrid.ColCount := DataArray.RowCount + 1;
        AGrid.Options := AGrid.Options - [goEditing];
        AGrid.DefaultColWidth := 10;
        AGrid.ColorSelectedRow := False;
        AGrid.AutoMultiEdit := True;
        ColumnFormat := rcf4Real;
        case DataArray.DataType of
          rdtDouble: ColumnFormat := rcf4Real;
          rdtInteger: ColumnFormat := rcf4Integer;
          rdtBoolean: ColumnFormat := rcf4Boolean;
          rdtString: ColumnFormat := rcf4String;
          else Assert(False);
        end;
        AGrid.Columns[0].AutoAdjustColWidths := True;
        for ColIndex := 1 to AGrid.ColCount - 1 do
        begin
          AGrid.Columns[ColIndex].AutoAdjustColWidths := True;
          AGrid.Columns[ColIndex].Format := ColumnFormat;
          AGrid.Cells[ColIndex,0] := IntToStr(ColIndex);
        end;
        AGrid.RowCount := DataArray.LayerCount + 1;
        AGrid.BeginUpdate;
        try
          for RowIndex := 1 to AGrid.RowCount - 1 do
          begin
            AGrid.Cells[0,RowIndex] := IntToStr(RowIndex);
          end;
          for RowIndex := 0 to DataArray.RowCount - 1 do
          begin
            for LayerIndex := 0 to DataArray.LayerCount - 1 do
            begin
              case DataArray.DataType of
                rdtDouble: AGrid.Cells[RowIndex+1,LayerIndex+1] :=
                  FloatToStr(DataArray.RealData[LayerIndex, RowIndex, 0]);
                rdtInteger: AGrid.Cells[RowIndex+1,LayerIndex+1] :=
                  IntToStr(DataArray.IntegerData[LayerIndex, RowIndex, 0]);
                rdtBoolean: AGrid.Checked[RowIndex+1,LayerIndex+1] :=
                  DataArray.BooleanData[LayerIndex, RowIndex, 0];
                rdtString: AGrid.Cells[RowIndex+1,LayerIndex+1] :=
                  DataArray.StringData[LayerIndex, RowIndex, 0];
              end;
            end;
          end;
        finally
          AGrid.EndUpdate;
        end;
        APage.TabVisible := False;
      end;
    else Assert(False);
  end;
;
  pcDataSet.ActivePageIndex := 0;
  lbLayers.ItemIndex := 0;
  btnCopy.Enabled := True;
end;

procedure TfrmDataSetValues.treecomboDataSetsDropDownTreeChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  inherited;
  SetSelectedNode(Sender, Node);
end;

procedure TfrmDataSetValues.treecomboDataSetsDropDownTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

procedure TfrmDataSetValues.treecomboDataSetsDropDownTreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmDataSetValues.SetSelectedNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  SelectOnlyLeaves(Node, treecomboDataSets, Sender, FSelectedVirtNode);
end;

end.
