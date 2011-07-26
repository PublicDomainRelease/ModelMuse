{@abstract(@name defines @link(TframeHeadObservations).
@link(TframeHeadObservations)
is used in @link(TfrmScreenObjectProperties) to specify head observations.)

@author(Richard B. Winston <rbwinst@usgs.gov>))
}
unit frameHeadObservationsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, RbwDataGrid4, StdCtrls, Mask, JvExMask, JvSpin, ExtCtrls,
  UndoItemsScreenObjects, ModflowHobUnit, Math, ArgusDataEntry, JvExStdCtrls,
  JvCombobox, JvListComb, ComCtrls, JvExComCtrls, JvComCtrls;

type
  TframeHeadObservations = class(TFrame)
    Panel4: TPanel;
    seLayers: TJvSpinEdit;
    pnlCaption: TPanel;
    pnlName: TPanel;
    edObsName: TLabeledEdit;
    rgMultiObsMethod: TRadioGroup;
    Panel6: TPanel;
    btnDeleteLayer: TButton;
    btnInsertLayer: TButton;
    lblNumberOfLayers: TLabel;
    lblTreatment: TLabel;
    comboTreatment: TComboBox;
    pcData: TJvPageControl;
    tabTimes: TTabSheet;
    tabLayers: TTabSheet;
    rdgLayers: TRbwDataGrid4;
    rdeMultiLayerEdit: TRbwDataEntry;
    Panel5: TPanel;
    rdeMultiValueEdit: TRbwDataEntry;
    comboMultiStatFlag: TJvImageComboBox;
    Panel2: TPanel;
    lblNumberOfTimes: TLabel;
    seTimes: TJvSpinEdit;
    btnDeleteValue: TButton;
    btnInsertValue: TButton;
    rdgHeads: TRbwDataGrid4;
    procedure seTimesChange(Sender: TObject);
    procedure rdgHeadsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure seLayersChange(Sender: TObject);
    procedure rdgLayersSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdeMultiValueEditChange(Sender: TObject);
    procedure rdeMultiLayerEditChange(Sender: TObject);
    procedure rdgHeadsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgLayersSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure btnDeleteValueClick(Sender: TObject);
    procedure btnInsertValueClick(Sender: TObject);
    procedure btnDeleteLayerClick(Sender: TObject);
    procedure btnInsertLayerClick(Sender: TObject);
    procedure rdgHeadsExit(Sender: TObject);
    procedure rdgLayersExit(Sender: TObject);
    procedure edObsNameExit(Sender: TObject);
    procedure rdgHeadsHorizontalScroll(Sender: TObject);
    procedure comboMultiStatFlagChange(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure rdgHeadsColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure splitHeadsLayersMoved(Sender: TObject);
    procedure edObsNameChange(Sender: TObject);
    procedure comboTreatmentChange(Sender: TObject);
    procedure rgMultiObsMethodClick(Sender: TObject);
  private
    FDeletingTime: Boolean;
    FDeletingLayer: Boolean;
    FChanged: Boolean;
//    FHidingColumns: Boolean;
    FTimesCountChanged: Boolean;
    FLayerCountChanged: Boolean;
    procedure ClearGrid(Grid: TRbwDataGrid4);
    procedure AssignValuesToSelectedGridCells(const NewText: string;
      Grid: TRbwDataGrid4; const StartCol, EndCol: integer);
    procedure EnableMultiEditControl(Grid: TRbwDataGrid4; AControl: TControl;
      const StartCol, EndCol: integer);
    procedure DeleteSelectedRow(rdgGrid: TRbwDataGrid4; SpinEdit: TJvSpinEdit; DeleteButton: TButton);
    procedure UpdateSpinEdit(Grid: TRbwDataGrid4; SpinEdit: TJvSpinEdit);
    procedure InsertRow(Grid: TRbwDataGrid4; SpinEdit: TJvSpinEdit; DeleteButton: TButton);
    procedure EnableDeleteButton(DeleteButton: TButton; SpinEdit: TJvSpinEdit);
    procedure SetSpinCount(SpinEdit: TJvSpinEdit; Grid: TRbwDataGrid4);
    procedure LayoutMultiHeadEditControls;
    { Private declarations }
  public
//    procedure HideUcodeColumns;
    procedure GetData(List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    procedure InitializeControls;
    { Public declarations }
  end;

resourcestring
  StrTime = 'Time';
  StrObservedHead = 'Observed  Head';
  StrStatistic = 'Statistic';
  StrStatFlag = 'StatFlag';

implementation

uses
  GoPhastTypes, frmCustomGoPhastUnit, frmGoPhastUnit;

{$R *.dfm}

type
  THeadObsCol = (hocTime, hocHead, hocStatistic, hocStatFlag, hocComment);
  THeadLayers = (hlLayer, hlFraction);

{ TframeHeadObservations }

procedure TframeHeadObservations.btnDeleteLayerClick(Sender: TObject);
begin
  DeleteSelectedRow(rdgLayers, seLayers, btnDeleteLayer);
end;

procedure TframeHeadObservations.btnDeleteValueClick(Sender: TObject);
begin
  DeleteSelectedRow(rdgHeads, seTimes, btnDeleteValue);
end;

procedure TframeHeadObservations.btnInsertLayerClick(Sender: TObject);
begin
  InsertRow(rdgLayers, seLayers, btnDeleteLayer);
end;

procedure TframeHeadObservations.btnInsertValueClick(Sender: TObject);
begin
  InsertRow(rdgHeads, seTimes, btnDeleteValue);
end;

procedure TframeHeadObservations.ClearGrid(Grid: TRbwDataGrid4);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  for ColIndex := Grid.FixedCols to Grid.ColCount - 1 do
  begin
    for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
    begin
      Grid.Cells[ColIndex, RowIndex] := '';
    end;
  end;
end;

procedure TframeHeadObservations.comboMultiStatFlagChange(Sender: TObject);
begin
  AssignValuesToSelectedGridCells(comboMultiStatFlag.Text, rdgHeads,
    Ord(hocStatFlag), Ord(hocStatFlag));
end;

type TRbwDataGrid4Crack = class(TRbwDataGrid4);

procedure TframeHeadObservations.comboTreatmentChange(Sender: TObject);
var
  Purpose: TObservationPurpose;
  Index: Integer;
begin
  FChanged := True;
  if comboTreatment.ItemIndex >= 0 then
  begin
    Purpose := TObservationPurpose(comboTreatment.ItemIndex)
  end
  else
  begin
    Purpose := ofObserved;
  end;
  TRbwDataGrid4Crack(rdgHeads).HideEditor;
  case Purpose of
    ofObserved, ofInacative:
      begin
        rdgHeads.Columns[Ord(hocStatFlag)].PickList := ObservationStatFlagLabels;
        comboMultiStatFlag.Items.Assign(ObservationStatFlagLabels);
      end;
    ofPredicted:
      begin
        rdgHeads.Columns[Ord(hocStatFlag)].PickList := PredictionStatFlagLabels;
        comboMultiStatFlag.Items.Assign(PredictionStatFlagLabels);
        for Index := 1 to rdgHeads.RowCount - 1 do
        begin
          if (rdgHeads.Cells[Ord(hocStatFlag), Index] <> '')
            and (rdgHeads.ItemIndex[Ord(hocStatFlag), Index] < 0) then
          begin
            rdgHeads.ItemIndex[Ord(hocStatFlag), Index] := 0;
          end;
        end;
      end;
    else Assert(False);
  end;
end;

procedure TframeHeadObservations.GetData(List: TScreenObjectEditCollection);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Observations: THobBoundary;
  FoundFirst: Boolean;
  ItemIndex: Integer;
  HobItem: THobItem;
  LayerItem: TMultiHeadItem;
begin
  InitializeControls;
  rdgHeads.BeginUpdate;
  rdgLayers.BeginUpdate;
  try
    edObsName.Text := '';
    edObsName.Enabled := True;
    edObsName.Color := clWindow;
    ClearGrid(rdgHeads);
    ClearGrid(rdgLayers);
    seTimes.AsInteger := 0;
    seLayers.AsInteger := 0;
    FoundFirst := False;
    for Index := 0 to List.Count - 1 do
    begin
      Item := List.Items[Index];
      Observations := Item.ScreenObject.ModflowHeadObservations;
      if (Observations <> nil) and Observations.Used then
      begin
        if not FoundFirst then
        begin
          rgMultiObsMethod.ItemIndex := Ord(Observations.MultiObsMethod);
          edObsName.Text := Observations.ObservationName;
          seTimes.AsInteger := Observations.Values.Count;
          rgMultiObsMethod.Enabled := Observations.Values.Count > 1;
          for ItemIndex := 0 to Observations.Values.Count - 1 do
          begin
            HobItem := Observations.Values.HobItems[ItemIndex];
            rdgHeads.Cells[Ord(hocTime),ItemIndex +1] := FloatToStr(HobItem.Time);
            rdgHeads.Cells[Ord(hocHead),ItemIndex +1] := FloatToStr(HobItem.Head);
            rdgHeads.Cells[Ord(hocStatistic),ItemIndex +1] := FloatToStr(HobItem.Statistic);
            rdgHeads.Cells[Ord(hocStatFlag),ItemIndex +1] :=
              rdgHeads.Columns[Ord(hocStatFlag)].PickList[Ord(HobItem.StatFlag)];
            rdgHeads.Cells[Ord(hocComment),ItemIndex +1] := HobItem.Comment;
          end;
          seLayers.AsInteger := Observations.LayerFractions.Count;
          for ItemIndex := 0 to Observations.LayerFractions.Count - 1 do
          begin
            LayerItem := Observations.LayerFractions.MultiHeadItems[ItemIndex];
            rdgLayers.Cells[Ord(hlLayer),ItemIndex +1] := IntToStr(LayerItem.Layer);
            rdgLayers.Cells[Ord(hlFraction),ItemIndex +1] := FloatToStr(LayerItem.Proportion);
          end;
          comboTreatment.ItemIndex := Ord(Observations.Purpose);
          comboTreatmentChange(nil);
        end
        else
        begin
          edObsName.Text := '';
          edObsName.Enabled := False;
          edObsName.Color := Color;
          if rgMultiObsMethod.ItemIndex <> Ord(Observations.MultiObsMethod) then
          begin
            rgMultiObsMethod.ItemIndex := -1;
          end;
          if Observations.Values.Count > 1 then
          begin
            rgMultiObsMethod.Enabled := True;
          end;
          if seTimes.AsInteger <> Observations.Values.Count then
          begin
            ClearGrid(rdgHeads);
          end
          else
          begin
            for ItemIndex := 0 to Observations.Values.Count - 1 do
            begin
              HobItem := Observations.Values.HobItems[ItemIndex];
              if rdgHeads.Cells[Ord(hocTime),ItemIndex +1]
                <> FloatToStr(HobItem.Time) then
              begin
                rdgHeads.Cells[Ord(hocTime),ItemIndex +1] := '';
//                ClearGrid(rdgHeads);
//                break;
              end;
              if rdgHeads.Cells[Ord(hocHead),ItemIndex +1]
                <> FloatToStr(HobItem.Head) then
              begin
                rdgHeads.Cells[Ord(hocHead),ItemIndex +1] := '';
//                ClearGrid(rdgHeads);
//                break;
              end;
              if rdgHeads.Cells[Ord(hocStatistic),ItemIndex +1]
                <> FloatToStr(HobItem.Statistic) then
              begin
                rdgHeads.Cells[Ord(hocStatistic),ItemIndex +1] := '';
//                ClearGrid(rdgHeads);
//                break;
              end;
              if rdgHeads.Cells[Ord(hocStatFlag),ItemIndex +1]
                <> rdgHeads.Columns[Ord(hocStatFlag)].
                PickList[Ord(HobItem.StatFlag)] then
              begin
                rdgHeads.Cells[Ord(hocStatFlag),ItemIndex +1] := '';
//                ClearGrid(rdgHeads);
//                break;
              end;
              if rdgHeads.Cells[Ord(hocComment),ItemIndex +1]
                <> HobItem.Comment then
              begin
                rdgHeads.Cells[Ord(hocComment),ItemIndex +1] := '';
//                ClearGrid(rdgHeads);
//                break;
              end;
            end;
          end;
          if seLayers.AsInteger <> Observations.LayerFractions.Count then
          begin
            ClearGrid(rdgLayers);
          end
          else
          begin
            for ItemIndex := 0 to Observations.LayerFractions.Count - 1 do
            begin
              LayerItem := Observations.LayerFractions.MultiHeadItems[ItemIndex];
              if rdgLayers.Cells[Ord(hlLayer),ItemIndex +1]
                <> IntToStr(LayerItem.Layer) then
              begin
                rdgLayers.Cells[Ord(hlLayer),ItemIndex +1] := '';
//                ClearGrid(rdgLayers);
//                break;
              end;
              if rdgLayers.Cells[Ord(hlFraction),ItemIndex +1]
                <> FloatToStr(LayerItem.Proportion) then
              begin
                rdgLayers.Cells[Ord(hlFraction),ItemIndex +1] := '';
//                ClearGrid(rdgLayers);
//                break;
              end;
            end;
          end;
          if comboTreatment.ItemIndex <> Ord(Observations.Purpose) then
          begin
            comboTreatment.ItemIndex := -1;
            comboTreatmentChange(nil);
          end;
        end;
        FoundFirst := True;
      end;
    end;
  finally
    rdgHeads.EndUpdate;
    rdgLayers.EndUpdate;
  end;
//  HideUcodeColumns;
  FChanged := False;
  FTimesCountChanged := False;
  FLayerCountChanged := False;
end;

procedure TframeHeadObservations.InitializeControls;
var
  Column: TRbwColumn4;
  Index: Integer;
begin
  pcData.ActivePageIndex := 0;
  rdgHeads.Columns[Ord(hocStatFlag)].PickList := ObservationStatFlagLabels;
  comboMultiStatFlag.Items.Assign(ObservationStatFlagLabels);
  Column := rdgHeads.Columns[Ord(hocStatFlag)];
  Assert(comboMultiStatFlag.Items.Count = Column.PickList.Count);
  for Index := 0 to Column.PickList.Count - 1 do
  begin
    comboMultiStatFlag.Items[Index].Text := Column.PickList[Index];
  end;

  rdgHeads.Cells[Ord(hocTime),0] := StrTime;
  rdgHeads.Cells[Ord(hocHead),0] := StrObservedHead;
  rdgHeads.Cells[Ord(hocStatistic),0] := StrStatistic;
  rdgHeads.Cells[Ord(hocStatFlag),0] := StrStatFlag;
  rdgHeads.Cells[Ord(hocComment),0] := 'Comment';

  rdgLayers.Cells[Ord(hlLayer),0] := 'Layer';
  rdgLayers.Cells[Ord(hlFraction),0] := 'Weight';

  rgMultiObsMethod.ItemIndex := 1;
  comboTreatment.ItemIndex := 0;
  lblTreatment.Top := comboTreatment.Top - lblTreatment.Height - 2;

  LayoutMultiHeadEditControls;
//  HideUcodeColumns;
end;

procedure TframeHeadObservations.LayoutMultiHeadEditControls;
var
  Index: Integer;
  AColVisible: Boolean;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;  
  AColVisible := False;
  for Index := Ord(hocTime) to Ord(hocStatistic) do
  begin
    if rdgHeads.ColVisible[Index] then
    begin
      LayoutControls(rdgHeads, rdeMultiValueEdit, nil, Index);
      AColVisible := True;
      break;
    end;
  end;
  if not AColVisible then
  begin
    LayoutControls(rdgHeads, rdeMultiValueEdit, nil, 0);
  end;
  LayoutControls(rdgHeads, comboMultiStatFlag, nil, Ord(hocStatFlag));
end;

procedure TframeHeadObservations.SetSpinCount(SpinEdit: TJvSpinEdit; Grid: TRbwDataGrid4);
begin
  if Grid.RowCount = 2 then
  begin
    if (Grid.Cells[0, 1] = '')
      and (Grid.Cells[1, 1] = '')
      and (Grid.Cells[2, 1] = '')
      and (Grid.Cells[3, 1] = '') then
    begin
      SpinEdit.AsInteger := 0;
    end
    else
    begin
      SpinEdit.AsInteger := 1;
    end;
  end;
end;

procedure TframeHeadObservations.splitHeadsLayersMoved(Sender: TObject);
begin
  LayoutMultiHeadEditControls;
end;

procedure TframeHeadObservations.EnableDeleteButton(DeleteButton: TButton; SpinEdit: TJvSpinEdit);
begin
  DeleteButton.Enabled := (SpinEdit.AsInteger > 0);
end;

procedure TframeHeadObservations.InsertRow(Grid: TRbwDataGrid4; SpinEdit: TJvSpinEdit; DeleteButton: TButton);
begin
  FChanged := True;
  if SpinEdit.AsInteger = 0 then
  begin
    SpinEdit.AsInteger := 1;
  end
  else
  begin
    if (Grid.SelectedRow > 0) and (Grid.SelectedRow < Grid.RowCount) then
    begin
      Grid.InsertRow(Grid.SelectedRow);
      UpdateSpinEdit(Grid, SpinEdit);
      EnableDeleteButton(DeleteButton, SpinEdit);
    end;
  end;
end;

procedure TframeHeadObservations.UpdateSpinEdit(Grid: TRbwDataGrid4; SpinEdit: TJvSpinEdit);
begin
  FChanged := True;
  if Grid.RowCount > 2 then
  begin
    SpinEdit.AsInteger := Grid.RowCount - 1;
  end;
//  HideUcodeColumns;
end;

procedure TframeHeadObservations.DeleteSelectedRow(rdgGrid: TRbwDataGrid4;
  SpinEdit: TJvSpinEdit; DeleteButton: TButton);
var
  ColIndex: Integer;
begin
  FChanged := True;
  if (rdgGrid.SelectedRow > 0) and (rdgGrid.SelectedRow < rdgGrid.RowCount)
    and (rdgGrid.RowCount > 2) then
  begin
    rdgGrid.DeleteRow(rdgGrid.SelectedRow);
    UpdateSpinEdit(rdgGrid, SpinEdit);
  end
  else if (rdgGrid.SelectedRow = 1) and (rdgGrid.RowCount = 2) then
  begin
    for ColIndex := 0 to rdgGrid.ColCount - 1 do
    begin
      rdgGrid.Cells[ColIndex,1] := '';
    end;
    UpdateSpinEdit(rdgGrid, SpinEdit);
  end;
  EnableDeleteButton(DeleteButton, SpinEdit);
end;

procedure TframeHeadObservations.edObsNameChange(Sender: TObject);
begin
  FChanged := True;
end;

procedure TframeHeadObservations.edObsNameExit(Sender: TObject);
begin
  edObsName.Text := StringReplace(edObsName.Text, ' ', '_', [rfReplaceAll]);
end;

procedure TframeHeadObservations.EnableMultiEditControl(Grid: TRbwDataGrid4;
  AControl: TControl; const StartCol, EndCol: integer);
var
  ShouldEnable: Boolean;
  ColIndex: Integer;
  RowIndex: Integer;
  EnableCount: Integer;
begin
  EnableCount := 0;
  for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
  begin
    for ColIndex := StartCol to EndCol do
    begin
      ShouldEnable := Grid.IsSelectedCell(ColIndex, RowIndex);
      if ShouldEnable then
      begin
        Inc(EnableCount);
        if EnableCount >= 2 then
        begin
          break;
        end;
      end;
    end;
  end;
  ShouldEnable := EnableCount >= 2;
  AControl.Enabled := ShouldEnable;
end;

procedure TframeHeadObservations.AssignValuesToSelectedGridCells(
  const NewText: string; Grid: TRbwDataGrid4; const StartCol, EndCol: integer);
var
  ColIndex: Integer;
  RowIndex: Integer;
  TempText: string;
begin
  if Grid = nil then
  begin
    Exit;
  end;
  FChanged := True;
  for ColIndex := StartCol to EndCol do
  begin
    if Grid.Columns[ColIndex].Format = rcf4Integer then
    begin
      TempText := IntToStr(Round(StrToFloat(NewText)));
    end
    else
    begin
      TempText := NewText;
    end;
    for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
    begin
      if Grid.IsSelectedCell(ColIndex, RowIndex) then
      begin
        Grid.Cells[ColIndex, RowIndex] := TempText;
        if Assigned(Grid.OnSetEditText) then
        begin
          Grid.OnSetEditText(Grid, ColIndex, RowIndex, TempText);
        end;
      end;
    end;
  end;
end;


procedure TframeHeadObservations.FrameResize(Sender: TObject);
begin
  LayoutMultiHeadEditControls;
end;

procedure TframeHeadObservations.rdeMultiLayerEditChange(Sender: TObject);
begin
  AssignValuesToSelectedGridCells(rdeMultiLayerEdit.Text, rdgLayers,
    Ord(hlLayer), Ord(hlFraction));
end;

procedure TframeHeadObservations.rdeMultiValueEditChange(Sender: TObject);
begin
  AssignValuesToSelectedGridCells(rdeMultiValueEdit.Text, rdgHeads,
    Ord(hocTime), Ord(hocStatistic));
end;

procedure TframeHeadObservations.rdgHeadsColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  LayoutMultiHeadEditControls;
end;

procedure TframeHeadObservations.rdgHeadsExit(Sender: TObject);
begin
  SetSpinCount(seTimes, rdgHeads);  
end;

procedure TframeHeadObservations.rdgHeadsHorizontalScroll(Sender: TObject);
begin
  LayoutMultiHeadEditControls;
end;

procedure TframeHeadObservations.rdgHeadsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect := seTimes.AsInteger > 0;
  EnableMultiEditControl(rdgHeads, rdeMultiValueEdit,
    Ord(hocTime), Ord(hocStatistic));
  EnableMultiEditControl(rdgHeads, comboMultiStatFlag,
    Ord(hocStatFlag), Ord(hocStatFlag));
end;

procedure TframeHeadObservations.rdgHeadsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  if FDeletingTime then
  begin
    Exit;
  end;
  UpdateSpinEdit(rdgHeads, seTimes);
end;

procedure TframeHeadObservations.rdgLayersExit(Sender: TObject);
begin
  SetSpinCount(seLayers, rdgLayers); 
end;

procedure TframeHeadObservations.rdgLayersSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect := seLayers.AsInteger > 0;
  EnableMultiEditControl(rdgLayers, rdeMultiLayerEdit,
    Ord(hlLayer), Ord(hlFraction));
end;

procedure TframeHeadObservations.rdgLayersSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  if FDeletingLayer then
  begin
    Exit;
  end;
  UpdateSpinEdit(rdgLayers, seLayers);
end;

procedure TframeHeadObservations.rgMultiObsMethodClick(Sender: TObject);
begin
  FChanged := True;
end;

procedure TframeHeadObservations.seLayersChange(Sender: TObject);
begin
  FLayerCountChanged := True;
  FDeletingLayer := True;
  try
    if seLayers.AsInteger = 0 then
    begin
      rdgLayers.RowCount := 2;
      rdgLayers.Options := rdgLayers.Options - [goAlwaysShowEditor];
    end
    else
    begin
      rdgLayers.RowCount := seLayers.AsInteger + 1;
      rdgLayers.Options := rdgLayers.Options + [goAlwaysShowEditor];
    end;
    rdgLayers.Invalidate;
    EnableDeleteButton(btnDeleteLayer, seLayers);
  finally
    FDeletingLayer := False;
  end;
end;

procedure TframeHeadObservations.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Observations: THobBoundary;
  ObservationUsed: boolean;
  ValueCount: integer;
  RowIndex: Integer;
  Head, Time, Statistic: double;
  ObsHead: THobItem;
  Layer: integer;
  Fraction: double;
  ObsLayer: TMultiHeadItem;
  NewComment: string;
begin
  if not FChanged then
  begin
    Exit;
  end;
  for Index := 0 to List.Count - 1 do
  begin
    Item := List.Items[Index];
    Observations := Item.ScreenObject.ModflowHeadObservations;
    ObservationUsed := (Observations <> nil) and Observations.Used;

    if ClearAll then
    begin
      if ObservationUsed then
      begin
        Observations.Clear;
      end;
    end
    else if SetAll or ObservationUsed then
    begin
      if Observations = nil then
      begin
        Item.ScreenObject.CreateHeadObservations;
        Observations := Item.ScreenObject.ModflowHeadObservations;
      end;

      if edObsName.Text <> '' then
      begin
        Observations.ObservationName := edObsName.Text;
      end;
      if rgMultiObsMethod.ItemIndex >= 0 then
      begin
        Observations.MultiObsMethod :=
          TMultiObsMethod(rgMultiObsMethod.ItemIndex); 
      end;
      if comboTreatment.ItemIndex >= 0 then
      begin
        Observations.Purpose :=
          TObservationPurpose(comboTreatment.ItemIndex);
      end;
      ValueCount := 0;
      for RowIndex := 1 to rdgHeads.RowCount - 1 do
      begin
        if (TryStrToFloat(rdgHeads.Cells[Ord(hocTime), RowIndex], Time)
          or (rdgHeads.Cells[Ord(hocTime), RowIndex] = ''))
          and (TryStrToFloat(rdgHeads.Cells[Ord(hocHead), RowIndex], Head)
          or (rdgHeads.Cells[Ord(hocHead), RowIndex] = '')) then
        begin
          Inc(ValueCount);
        end;
      end;
      if FTimesCountChanged then
      begin
        while Observations.Values.Count < ValueCount do
        begin
          Observations.Values.Add;
        end;
        while Observations.Values.Count > ValueCount do
        begin
          Observations.Values.Delete(Observations.Values.Count-1);
        end;
      end;
      ValueCount := 0;
      for RowIndex := 1 to seTimes.AsInteger do
      begin
        if (TryStrToFloat(rdgHeads.Cells[Ord(hocTime), RowIndex], Time)
          or (rdgHeads.Cells[Ord(hocTime), RowIndex] = ''))
          and (TryStrToFloat(rdgHeads.Cells[Ord(hocHead), RowIndex], Head)
          or (rdgHeads.Cells[Ord(hocHead), RowIndex] = '')) then
        begin
          if ValueCount < Observations.Values.Count then
          begin
            ObsHead := Observations.Values.HobItems[ValueCount];
            if (rdgHeads.Cells[Ord(hocTime), RowIndex] <> '') then
            begin
              ObsHead.Time := Time;
            end;
            if (rdgHeads.Cells[Ord(hocHead), RowIndex] <> '') then
            begin
              ObsHead.Head := Head;
            end;
            if TryStrToFloat(rdgHeads.Cells[Ord(hocStatistic), RowIndex], Statistic) then
            begin
              ObsHead.Statistic := Statistic;
            end;
            if (rdgHeads.Cells[Ord(hocStatFlag), RowIndex] <> '') then
            begin
              ObsHead.StatFlag := TStatFlag(rdgHeads.Columns[Ord(hocStatFlag)].
                PickList.IndexOf(rdgHeads.Cells[Ord(hocStatFlag), RowIndex]));
            end;
            NewComment := rdgHeads.Cells[Ord(hocComment), RowIndex];
            if (List.Count = 1) or (NewComment <> '') then
            begin
              ObsHead.Comment := NewComment;
            end;
          end;
          Inc(ValueCount);
        end;
      end;

      ValueCount := 0;
      for RowIndex := 1 to seLayers.AsInteger do
      begin
        if TryStrToInt(rdgLayers.Cells[Ord(hlLayer), RowIndex], Layer)
          and TryStrToFloat(rdgLayers.Cells[Ord(hlFraction), RowIndex], Fraction) then
        begin
          Inc(ValueCount);
        end;
      end;
      if FLayerCountChanged then
      begin
        while Observations.LayerFractions.Count < ValueCount do
        begin
          Observations.LayerFractions.Add;
        end;
        while Observations.LayerFractions.Count > ValueCount do
        begin
          Observations.LayerFractions.Delete(Observations.LayerFractions.Count-1);
        end;
      end;
      ValueCount := 0;
      for RowIndex := 1 to rdgLayers.RowCount - 1 do
      begin
        if TryStrToInt(rdgLayers.Cells[Ord(hlLayer), RowIndex], Layer)
          and TryStrToFloat(rdgLayers.Cells[Ord(hlFraction), RowIndex], Fraction) then
        begin
          if ValueCount < Observations.LayerFractions.Count then
          begin
            ObsLayer := Observations.LayerFractions.MultiHeadItems[ValueCount];
            ObsLayer.Layer := Layer;
            ObsLayer.Proportion := Fraction;
          end;
          Inc(ValueCount);
        end;
      end;
    end;
  end;
end;

//procedure TframeHeadObservations.HideUcodeColumns;
//begin
//  if FHidingColumns then Exit;
//  FHidingColumns := True;
//  try
//    if not frmGoPhast.ShowUcodeInterface then
//    begin
//      rdgHeads.ColWidths[Ord(hocStatistic)] := 0;
//      rdgHeads.ColWidths[Ord(hocStatFlag)] := 0;
//    end;
//  finally
//    FHidingColumns := False;
//  end;
//end;

procedure TframeHeadObservations.seTimesChange(Sender: TObject);
var
  CharNumber: integer;
begin
  FTimesCountChanged := True;
  FChanged := True;
  FDeletingTime := True;
  try
    if seTimes.AsInteger = 0 then
    begin
      rdgHeads.RowCount := 2;
      rdgHeads.Options := rdgHeads.Options - [goAlwaysShowEditor];
    end
    else
    begin
      rdgHeads.RowCount := seTimes.AsInteger + 1;
      rdgHeads.Options := rdgHeads.Options + [goAlwaysShowEditor];

      if seTimes.AsInteger = 1 then
      begin
        edObsName.MaxLength := 12;
      end
      else
      begin
        CharNumber := Trunc(Log10(seTimes.AsInteger))+1;
        edObsName.MaxLength := 12-CharNumber;
      end;
    
      if Length(edObsName.Text) > edObsName.MaxLength then
      begin
        edObsName.Text := Copy(edObsName.Text, 1, edObsName.MaxLength);
      end;
    end;
    EnableDeleteButton(btnDeleteValue, seTimes);
    rgMultiObsMethod.Enabled := seTimes.AsInteger > 1;
    rdgHeads.Invalidate;
  finally
    FDeletingTime := False;
  end;
//  HideUcodeColumns;
end;

end.
