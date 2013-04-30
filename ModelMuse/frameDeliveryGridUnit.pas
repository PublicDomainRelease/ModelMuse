unit frameDeliveryGridUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameFormulaGridUnit, ExtCtrls,
  Grids, RbwDataGrid4, StdCtrls, Mask, JvExMask, JvSpin, Buttons,
  UndoItemsScreenObjects, Math;

type
  // @name is used for editing data set 33 in the Farm Process of MODFLOW-FMP
  TframeDeliveryGrid = class(TframeFormulaGrid)
    lblNumberOfDeliveryTypes: TLabel;
    seNumberOfDeliveryTypes: TJvSpinEdit;
    comboHowUsed: TComboBox;
    lblHowUsed: TLabel;
    procedure seNumberOfDeliveryTypesChange(Sender: TObject);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure seNumberChange(Sender: TObject);
    procedure lblNumberOfDeliveryTypesClick(Sender: TObject);
    procedure sbInsertClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure edFormulaChange(Sender: TObject);
    procedure comboHowUsedChange(Sender: TObject);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    FChanged: boolean;
    FChanging: Boolean;
    FOnChange: TNotifyEvent;
    procedure DoChange;
{$IFDEF FMP}
    procedure ClearGrid(Grid: TRbwDataGrid4);
    property Changing: Boolean read FChanging write FChanging;
    procedure CheckValidCell(Sender: TObject; ACol, ARow: Integer; var ValidCell: Boolean);
    procedure GetValidHowUsed(ColIndex, RowIndex: Integer; var ValidCell: Boolean);
{$ENDIF}
    { Private declarations }
  public
    property DataChanged: Boolean read FChanged;
    procedure InitializeControls;
    // ScreenObjectList contains only objects that define farms.
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure LayoutMultiRowEditControls; override;
    { Public declarations }
  end;

type
  TDeliveryTimeColumns = (dtcStart, dtcEnd);
  TDeliveryColumns = (dcVolume, dcRank, dcHowUsed, dcVirtualFarm);

var
  frameDeliveryGrid: TframeDeliveryGrid;

implementation

uses
  GoPhastTypes, ModflowTimeUnit, frmGoPhastUnit, ModflowFmpFarmUnit,
  Generics.Collections, frmCustomGoPhastUnit;

resourcestring
  StrOnlyTheAmountRequ = 'Take required amount (0)';
  StrSurplusDischargedT = 'Surplus discharged (1)';
  StrSurplusStoredInGr = 'Surplus stored (2)';
  StrVolumeNRDV = 'Volume (NRDV)';
  StrRankNRDR = 'Rank (NRDR)';
  StrHowUsedNRDU = 'How used (NRDU)';
  StrVirtualFarm0 = 'Virtual Farm (<0)';
  StrVirtualFarmNumber = 'Virtual farm number (NRDU)';

const
  DeliveryColumns = Ord(High(TDeliveryColumns))+1;

{$R *.dfm}

{$IFDEF FMP}
procedure TframeDeliveryGrid.CheckValidCell(Sender: TObject; ACol,
  ARow: Integer; var ValidCell: Boolean);
begin
  ValidCell := (ARow >= 1) and (ACol > Ord(dtcEnd))
    and (((ACol-2) mod DeliveryColumns) <> Ord(dcHowUsed));
end;

procedure TframeDeliveryGrid.ClearGrid(Grid: TRbwDataGrid4);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
  begin
    for ColIndex := Grid.FixedCols to Grid.ColCount - 1 do
    begin
      Grid.Cells[ColIndex,RowIndex] := ''
    end;
  end;
end;

{$ENDIF}

procedure TframeDeliveryGrid.comboHowUsedChange(Sender: TObject);
{$IFDEF FMP}
var
  ColIndex: Integer;
  RowIndex: Integer;
  TempOptions: TGridOptions;
  ValidCell: Boolean;
{$ENDIF}
begin
{$IFDEF FMP}
  for RowIndex := Grid.FixedRows to
    Grid.RowCount - 1 do
  begin
    for ColIndex := FirstFormulaColumn to Grid.ColCount - 1 do
    begin
      if Grid.IsSelectedCell(ColIndex, RowIndex) then
      begin
        GetValidHowUsed(ColIndex, RowIndex, ValidCell);
        if ValidCell then
        begin
          Grid.Cells[ColIndex, RowIndex] := comboHowUsed.Text;
          if Assigned(Grid.OnSetEditText) then
          begin
            Grid.OnSetEditText(
              Grid,ColIndex,RowIndex, comboHowUsed.Text);
          end;
        end;
      end;
    end;
  end;
  TempOptions := Grid.Options;
  try
    Grid.Options := [goEditing, goAlwaysShowEditor];
    Grid.UpdateEditor;
  finally
    Grid.Options := TempOptions;
  end;
{$ENDIF}
end;

procedure TframeDeliveryGrid.DoChange;
begin
{$IFDEF FMP}
  if Changing then
  begin
    Exit;
  end;
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
  FChanged := True;
{$ENDIF}
end;

procedure TframeDeliveryGrid.edFormulaChange(Sender: TObject);
begin
  inherited;
{$IFDEF FMP}
  DoChange;
{$ENDIF}
end;


procedure TframeDeliveryGrid.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
{$IFDEF FMP}
var
  ObjectIndex: Integer;
  AFarmItem: TScreenObjectEditItem;
  AFarm: TFarm;
  MaxCount: Integer;
  FirstFarm: TFarm;
  DelivItem: TDeliveryParamItem;
  OuterIndex: Integer;
  TimeIndex: Integer;
  TimeItem: TNonRoutedDeliveryParameterItem;
{$ENDIF}
begin
{$IFDEF FMP}
  Changing := True;
  try
    Assert(ScreenObjectList.Count > 0);
    MaxCount := 0;
    AFarmItem := ScreenObjectList[0];
    FirstFarm := AFarmItem.ScreenObject.ModflowFmpFarm;
    for ObjectIndex := 1 to ScreenObjectList.Count - 1 do
    begin
      AFarmItem := ScreenObjectList[ObjectIndex];
      AFarm := AFarmItem.ScreenObject.ModflowFmpFarm;
      if not FirstFarm.DeliveryParamCollection.IsSame(
        AFarm.DeliveryParamCollection) then
      begin
        ClearGrid(Grid);
        seNumberOfDeliveryTypes.AsInteger := 0;
        seNumberOfDeliveryTypes.OnChange(seNumberOfDeliveryTypes);
        seNumber.AsInteger := 0;
        seNumber.OnChange(seNumber);
        Exit;
      end;
    end;
    MaxCount := Max(MaxCount, FirstFarm.DeliveryParamCollection.Count);
    if MaxCount = 0 then
    begin
      ClearGrid(Grid);
      seNumberOfDeliveryTypes.AsInteger := 0;
      seNumberOfDeliveryTypes.OnChange(seNumberOfDeliveryTypes);
      seNumber.AsInteger := 0;
      seNumber.OnChange(seNumber);
      Exit;
    end;

    seNumberOfDeliveryTypes.AsInteger := MaxCount;
    Grid.BeginUpdate;
    try
      DelivItem := FirstFarm.DeliveryParamCollection[0];

      seNumber.AsInteger := DelivItem.DeliveryParam.Count;
      seNumber.OnChange(seNumber);
      for OuterIndex := 0 to FirstFarm.DeliveryParamCollection.Count - 1 do
      begin
        DelivItem := FirstFarm.DeliveryParamCollection[OuterIndex];
        for TimeIndex := 0 to DelivItem.DeliveryParam.Count - 1 do
        begin
          TimeItem := DelivItem.DeliveryParam[TimeIndex];
          Grid.Cells[Ord(dtcStart), TimeIndex+1] := FloatToStr(TimeItem.StartTime);
          Grid.Cells[Ord(dtcEnd), TimeIndex+1] := FloatToStr(TimeItem.EndTime);
          Grid.Cells[Ord(dcVolume) + OuterIndex*DeliveryColumns + 2, TimeIndex+1] := TimeItem.Volume;
          Grid.Cells[Ord(dcRank) + OuterIndex*DeliveryColumns + 2, TimeIndex+1] := TimeItem.Rank;
          Grid.ItemIndex[Ord(dcHowUsed) + OuterIndex*DeliveryColumns + 2, TimeIndex+1] := Ord(TimeItem.NonRoutedDeliveryType);
          Grid.Cells[Ord(dcVirtualFarm) + OuterIndex*DeliveryColumns + 2, TimeIndex+1] := TimeItem.VirtualFarm;
        end;
      end;
    finally
      Grid.EndUpdate;
    end;
  finally
    FChanged := False;
    Changing := False;
  end;
{$ENDIF}
end;

procedure TframeDeliveryGrid.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
{$IFDEF FMP}
var
  ShouldEnable: boolean;
  ColIndex, RowIndex: Integer;
{$ENDIF}
begin
  inherited;
{$IFDEF FMP}
  ShouldEnable := False;
  for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
  begin
    for ColIndex := FirstFormulaColumn to Grid.ColCount - 1 do
    begin
      ShouldEnable := Grid.IsSelectedCell(ColIndex,RowIndex);
      if ShouldEnable then
      begin
        GetValidHowUsed(ColIndex, RowIndex, ShouldEnable);
        if ShouldEnable then
        begin
          Break;
        end;
      end;
    end;
    if ShouldEnable then
    begin
      break;
    end;
  end;
  comboHowUsed.Enabled := ShouldEnable;
  lblHowUsed.Enabled := ShouldEnable;
{$ENDIF}
end;

procedure TframeDeliveryGrid.GridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  ColumnType: TDeliveryColumns;
  DelivType: TNonRoutedDeliveryType;
begin
  inherited;
  if ACol >= 2 then
  begin
    ColumnType := TDeliveryColumns((ACol-2) mod DeliveryColumns);
    if ColumnType = dcVirtualFarm then
    begin
      DelivType := TNonRoutedDeliveryType(Grid.ItemIndex[ACol-1,ARow]);
      CanSelect  := DelivType = nrdtVirtualFarm;
    end;
  end;
end;

procedure TframeDeliveryGrid.GridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
//  UpdateNextTimeCell(Grid, ACol, ARow);
  DoChange;
end;

procedure TframeDeliveryGrid.lblNumberOfDeliveryTypesClick(Sender: TObject);
begin
  inherited;
  DoChange;
//  FChanged := True;
end;

procedure TframeDeliveryGrid.sbDeleteClick(Sender: TObject);
begin
  inherited;
  DoChange;
//  FChanged := True;
end;

procedure TframeDeliveryGrid.sbInsertClick(Sender: TObject);
begin
  inherited;
  DoChange;
//  FChanged := True;
end;

procedure TframeDeliveryGrid.seNumberChange(Sender: TObject);
begin
  inherited;
  DoChange;
//  FChanged := True;
end;

procedure TframeDeliveryGrid.seNumberOfDeliveryTypesChange(Sender: TObject);

var
  PickList: TStringList;
  PriorColCount: Integer;
  ColIndex: Integer;
  ColumnType: TDeliveryColumns;
begin
  inherited;
  PriorColCount := Grid.ColCount;
  Assert(PriorColCount >= 2);
  Grid.ColCount := seNumberOfDeliveryTypes.AsInteger * DeliveryColumns + 2;

  PickList := TStringList.Create;
  try
    PickList.Add(StrOnlyTheAmountRequ);
    PickList.Add(StrSurplusDischargedT);
    PickList.Add(StrSurplusStoredInGr);
    PickList.Add(StrVirtualFarm0);
    comboHowUsed.Items := PickList;

    Grid.BeginUpdate;
    try
      for ColIndex := PriorColCount to Grid.ColCount - 1 do
      begin
        ColumnType := TDeliveryColumns((ColIndex-2) mod DeliveryColumns);
        case ColumnType of
          dcVolume:
            begin
              Grid.Cells[ColIndex,0] := StrVolumeNRDV;
              Grid.Columns[ColIndex].ButtonUsed := True;
              Grid.Columns[ColIndex].ButtonCaption := StrF;
              Grid.Columns[ColIndex].ButtonWidth := 35;
            end;
          dcRank:
            begin
              Grid.Cells[ColIndex,0] := StrRankNRDR;
              Grid.Columns[ColIndex].ButtonUsed := True;
              Grid.Columns[ColIndex].ButtonCaption := StrF;
              Grid.Columns[ColIndex].ButtonWidth := 35;
            end;
          dcHowUsed:
            begin
              Grid.Cells[ColIndex,0] := StrHowUsedNRDU;
              Grid.Columns[ColIndex].ComboUsed := True;
              Grid.Columns[ColIndex].LimitToList := True;
              Grid.Columns[ColIndex].PickList := PickList;
            end;
          dcVirtualFarm:
            begin
              Grid.Cells[ColIndex,0] := StrVirtualFarmNumber;
              Grid.Columns[ColIndex].ButtonUsed := True;
              Grid.Columns[ColIndex].ButtonCaption := StrF;
              Grid.Columns[ColIndex].ButtonWidth := 35;
            end;
          else
            Assert(False);
        end;
        Grid.Columns[ColIndex].AutoAdjustColWidths := True;
        Grid.Columns[ColIndex].AutoAdjustRowHeights := True;
        Grid.Columns[ColIndex].WordWrapCaptions := True;
        Grid.Columns[ColIndex].ButtonFont := Font;
      end;
    finally
      Grid.EndUpdate;
    end;
  finally
    PickList.Free;
  end;

  for ColIndex := PriorColCount to Grid.ColCount - 1 do
  begin
    Grid.Columns[ColIndex].AutoAdjustColWidths := False;
  end;
  LayoutMultiRowEditControls;

  DoChange;
//  FChanged := True;
end;

procedure TframeDeliveryGrid.SetData(List: TScreenObjectEditCollection);
{$IFDEF FMP}
var
  index: Integer;
  Item: TScreenObjectEditItem;
  Farm: TFarm;
  Delivery: TDeliveryParamCollection;
  StartTimes: TList<Double>;
  EndTimes: TList<Double>;
  Rows: TList<Integer>;
  StartTime: double;
  EndTime: double;
  RowIndex: Integer;
  DeliveryIndex: Integer;
  DeliveryItem: TDeliveryParamItem;
  ARow: Integer;
  ColStart: Integer;
  DeliveryTimeItem: TNonRoutedDeliveryParameterItem;
{$ENDIF}
begin
{$IFDEF FMP}
  for index := 0 to List.Count - 1 do
  begin
    Item := List[index];
    Farm := Item.ScreenObject.ModflowFmpFarm;
    if Farm <> nil then
    begin
      Delivery := Farm.DeliveryParamCollection;
      while Delivery.Count < seNumberOfDeliveryTypes.AsInteger do
      begin
        Delivery.Add;
      end;
      while Delivery.Count > seNumberOfDeliveryTypes.AsInteger do
      begin
        Delivery.Last.Free;
      end;
      StartTimes := TList<Double>.Create;
      EndTimes := TList<Double>.Create;
      Rows := TList<Integer>.Create;
      try
        for RowIndex := 1 to seNumber.AsInteger do
        begin
          if TryStrToFloat(Grid.Cells[Ord(dtcStart), RowIndex], StartTime)
            and TryStrToFloat(Grid.Cells[Ord(dtcEnd), RowIndex], EndTime) then
          begin
            Rows.Add(RowIndex);
            StartTimes.Add(StartTime);
            EndTimes.Add(EndTime);
          end;
        end;
        for DeliveryIndex := 0 to seNumberOfDeliveryTypes.AsInteger - 1 do
        begin
          DeliveryItem := Delivery[DeliveryIndex];
          ColStart := DeliveryIndex*DeliveryColumns+2;
          for RowIndex := 0 to Rows.Count-1 do
          begin
            ARow := Rows[RowIndex];
            if RowIndex < DeliveryItem.DeliveryParam.Count then
            begin
              DeliveryTimeItem := DeliveryItem.DeliveryParam[RowIndex];
            end
            else
            begin
              DeliveryTimeItem := DeliveryItem.DeliveryParam.Add;
            end;
            DeliveryTimeItem.StartTime := StartTimes[RowIndex];
            DeliveryTimeItem.EndTime := EndTimes[RowIndex];
            DeliveryTimeItem.Volume := Grid.Cells[ColStart + Ord(dcVolume),ARow];
            DeliveryTimeItem.Rank := Grid.Cells[ColStart + Ord(dcRank),ARow];
            DeliveryTimeItem.NonRoutedDeliveryType :=
              TNonRoutedDeliveryType(Grid.ItemIndex[ColStart + Ord(dcHowUsed),ARow]);
            if DeliveryTimeItem.NonRoutedDeliveryType = nrdtVirtualFarm then
            begin
              DeliveryTimeItem.VirtualFarm := Grid.Cells[ColStart + Ord(dcVirtualFarm),ARow];
            end;
          end;
          while DeliveryItem.DeliveryParam.Count > Rows.Count do
          begin
            DeliveryItem.DeliveryParam.Last.Free;
          end;
//  TDeliveryTimeColumns = (dtcStart, dtcEnd);
//  TDeliveryColumns = (dcVolume, dcRank, dcHowUsed);
        end;
      finally
        StartTimes.Free;
        EndTimes.Free;
        Rows.Free;
      end;

    end;
  end;
{$ENDIF}
end;

{$IFDEF FMP}
procedure TframeDeliveryGrid.GetValidHowUsed(ColIndex, RowIndex: Integer; 
  var ValidCell: Boolean);
begin
  ValidCell := (RowIndex >= 1) and (ColIndex > Ord(dtcEnd))
    and (((ColIndex - 2) mod DeliveryColumns) = Ord(dcHowUsed));
end;
{$ENDIF}

procedure TframeDeliveryGrid.InitializeControls;
{$IFDEF FMP}
var
  StressPeriods: TModflowStressPeriods;
{$ENDIF}
begin
{$IFDEF FMP}
  FirstFormulaColumn := Succ(Ord(dtcEnd));
  ClearGrid(Grid);
  OnValidCell := CheckValidCell;
  Grid.Cells[Ord(dtcStart), 0] := StrStartingTime;
  Grid.Cells[Ord(dtcEnd), 0] := StrEndingTime;
  StressPeriods := frmGoPhast.PhastModel.ModflowStressPeriods;
  StressPeriods.FillPickListWithStartTimes(Grid, Ord(dtcStart));
  StressPeriods.FillPickListWithEndTimes(Grid, Ord(dtcEnd));
  seNumberOfDeliveryTypes.AsInteger := 0;
  seNumber.AsInteger := 0;
  LayoutMultiRowEditControls;
{$ENDIF}

end;

procedure TframeDeliveryGrid.LayoutMultiRowEditControls;
{$IFDEF FMP}
var
  Column: integer;
//  Row: Integer;
  ColIndex: Integer;
  ValidCell: Boolean;
{$ENDIF}
begin
  inherited;
{$IFDEF FMP}
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  Column := Max(FirstFormulaColumn,Grid.LeftCol);
  for ColIndex := Column to Grid.ColCount - 1 do
  begin
    GetValidHowUsed(ColIndex,1,ValidCell);
//    ValidCell := (ARow >= 1) and (ACol > Ord(dtcEnd))
//      and (((ACol-2) mod 3) = Ord(dcHowUsed));
    if ValidCell then
    begin
      Column := ColIndex;
      break;
    end;
  end;
  LayoutControls(Grid, comboHowUsed, lblHowUsed,
    Column);
{$ENDIF}
end;

end.
