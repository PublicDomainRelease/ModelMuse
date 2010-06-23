unit frmDataSetValuesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, TntStdCtrls,
  TntExDropDownEdit, TntExDropDownVirtualStringTree, VirtualTrees, ComCtrls,
  Grids, RbwDataGrid4, JvExStdCtrls, JvListBox;

type
  TfrmDataSetValues = class(TfrmCustomGoPhast)
    Panel1: TPanel;
    btnClose: TBitBtn;
    btnHelp: TBitBtn;
    treecomboDataSets: TTntExDropDownVirtualStringTree;
    pcDataSet: TPageControl;
    btnCopy: TButton;
    Panel2: TPanel;
    lbLayers: TJvListBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure treecomboDataSetsDropDownTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure treecomboDataSetsDropDownTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure treecomboDataSetsChange(Sender: TObject);
    procedure treecomboDataSetsDropDownTreeGetNodeDataSize(
      Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure btnCopyClick(Sender: TObject);
    procedure lbLayersMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure treecomboDataSetsClosedUp(Sender: TObject);
    procedure treecomboDataSetsDropDownTreeEnter(Sender: TObject);
  private
    FSelectedVirtNode: PVirtualNode;
    // @name is implemented as a TObjectList.
    FDataSets: TList;
    FTempControls: TList;
    FShouldClick: Boolean;
    procedure GetData;
    procedure SetSelectedNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    { Private declarations }
  public
    property SelectedVirtNode: PVirtualNode read FSelectedVirtNode;
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses Contnrs, ClassificationUnit, frmGoPhastUnit, DataSetUnit, RbwParser;

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
begin
  FillVirtualStringTreeWithDataSets(treecomboDataSets.Tree,
    FDataSets, nil);
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

procedure TfrmDataSetValues.treecomboDataSetsChange(Sender: TObject);
var
  DataArray: TDataArray;
  LayerIndex: Integer;
  APage: TTabSheet;
  AGrid: TRbwDataGrid4;
  ColIndex: Integer;
  ColumnFormat: TRbwColumnFormat4;
  RowIndex: Integer;
begin
  inherited;
  UpdateTreeComboText(SelectedVirtNode, treecomboDataSets);
  DataArray := frmGoPhast.PhastModel.GetDataSetByName(treecomboDataSets.Text);
  if DataArray = nil then
  begin
    Exit;
  end;
  FTempControls.Clear;
  lbLayers.Items.Clear;
  DataArray.Initialize;
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
  pcDataSet.ActivePageIndex := 0;
  lbLayers.ItemIndex := 0;
  btnCopy.Enabled := True;
end;

procedure TfrmDataSetValues.treecomboDataSetsClosedUp(Sender: TObject);
begin
  inherited;
  if FShouldClick then
  begin
    MouseClick;
    FShouldClick := False;
  end;
end;

procedure TfrmDataSetValues.treecomboDataSetsDropDownTreeChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  inherited;
  SetSelectedNode(Sender, Node);
end;

procedure TfrmDataSetValues.treecomboDataSetsDropDownTreeEnter(Sender: TObject);
begin
  inherited;
  FShouldClick := True;
end;

procedure TfrmDataSetValues.treecomboDataSetsDropDownTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TClassificationNodeData);
end;

procedure TfrmDataSetValues.treecomboDataSetsDropDownTreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmDataSetValues.SetSelectedNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  SelectOnlyLeaves(Node, treecomboDataSets, Sender, FSelectedVirtNode);
end;

end.
