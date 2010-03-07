unit framePackageSubUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, Mask, JvExMask, JvSpin,
  ArgusDataEntry, JvToolEdit, Grids, RbwDataGrid4, ComCtrls, JvExStdCtrls,
  JvCombobox, JvListComb, Math, ModflowPackageSelectionUnit, Buttons;

type
  TSubFormatColumns = (sfcSubsidence, sfcCompactModelLayer,
    sfcCompactByInterbedSystem, sfcVerticalDisplacement,
    sfcNoDelayPreconsolidationHead, sfcDelayPreconsolidationHead);

  TSubOutputColumns = (socStartTime, socEndTime,
    socPrintSubsidence, socSaveSubsidence,
    socPrintCompactModelLayer, socSaveCompactModelLayer,
    socPrintCompactByInterbedSystem, socSaveCompactByInterbedSystem,
    socPrintVerticalDisplacement, socSaveVerticalDisplacement,
    socPrintNoDelayPreconsolidationHead, socSaveNoDelayPreconsolidationHead,
    socPrintDelayPreconsolidationHead, socSaveDelayPreconsolidationHead,
    socPrintDelayInterbedBudget);

  TframePackageSub = class(TframePackage)
    pcSub: TPageControl;
    tabControls: TTabSheet;
    seNumberOfNodes: TJvSpinEdit;
    lblNumberOfNodes: TLabel;
    rdeAccel1: TRbwDataEntry;
    lblAccel1: TLabel;
    rdeAccel2: TRbwDataEntry;
    lblAccel2: TLabel;
    rdeMinIterations: TRbwDataEntry;
    lblMinIterations: TLabel;
    cbSaveRestart: TCheckBox;
    lbReadRestart: TLabel;
    feReadRestart: TJvFilenameEdit;
    tabFormat: TTabSheet;
    rdgFormats: TRbwDataGrid4;
    comboMultiFomat: TJvImageComboBox;
    tabPrintSave: TTabSheet;
    cbMultiPrintSave: TCheckBox;
    rdgOutput: TRbwDataGrid4;
    seNumExportPeriods: TJvSpinEdit;
    lblNumExportPeriods: TLabel;
    sbAdd: TSpeedButton;
    sbInsert: TSpeedButton;
    sbDelete: TSpeedButton;
    comboOutputChoice: TJvImageComboBox;
    lblOutputChoice: TLabel;
    procedure comboMultiFomatChange(Sender: TObject);
    procedure cbMultiPrintSaveClick(Sender: TObject);
    procedure rdgFormatsColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdgOutputColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdgFormatsHorizontalScroll(Sender: TObject);
    procedure rdgOutputHorizontalScroll(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure rdgFormatsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdgOutputMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdgFormatsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdgOutputMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure seNumExportPeriodsChange(Sender: TObject);
    procedure sbAddClick(Sender: TObject);
    procedure sbInsertClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
    procedure rdgOutputEndUpdate(Sender: TObject);
  private
    FChangingRowCount: Boolean;
    procedure LayoutFormatControls;
    procedure LayoutPrintSaveControls;
    procedure ChangeGridOptions(DataGrid: TRbwDataGrid4; Shift: TShiftState);
    procedure EnableMultiEditControl(Control: TControl; DataGrid: TRbwDataGrid4);
    procedure EnableDeleteButton;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    procedure Loaded; override;
    { Public declarations }
  end;

var
  framePackageSub: TframePackageSub;

implementation

uses
  GoPhastTypes, frmCustomGoPhastUnit, frmGoPhastUnit, ModflowTimeUnit;

{$R *.dfm}

{ TframePackageSub }

procedure TframePackageSub.cbMultiPrintSaveClick(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  for RowIndex := rdgOutput.FixedRows to rdgOutput.RowCount - 1 do
  begin
    for ColIndex := Ord(socPrintSubsidence) to rdgOutput.ColCount - 1 do
    begin
      if rdgOutput.IsSelectedCell(ColIndex, RowIndex) then
      begin
        rdgOutput.Checked[ColIndex, RowIndex] := cbMultiPrintSave.Checked;
        if Assigned(rdgOutput.OnSetEditText) then
        begin
          rdgOutput.OnStateChange(rdgOutput, ColIndex, RowIndex, cbMultiPrintSave.State);
        end;
      end;
    end;
  end;
end;

procedure TframePackageSub.comboMultiFomatChange(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  for RowIndex := rdgFormats.FixedRows to rdgFormats.RowCount - 1 do
  begin
    for ColIndex := 0 to rdgFormats.ColCount - 1 do
    begin
      if rdgFormats.IsSelectedCell(ColIndex, RowIndex) then
      begin
        rdgFormats.Cells[ColIndex, RowIndex] := comboMultiFomat.Text;
        if Assigned(rdgFormats.OnSetEditText) then
        begin
          rdgFormats.OnSetEditText(rdgFormats, ColIndex, RowIndex, comboMultiFomat.Text);
        end;
      end;
    end;
  end;
end;

constructor TframePackageSub.Create(AOwner: TComponent);
begin
  inherited;
  rdgFormats.Cells[Ord(sfcSubsidence), 0] := 'Format for printing subsidence (Ifm1)';
  rdgFormats.Cells[Ord(sfcCompactModelLayer), 0] := 'Format for printing compaction by model layer (Ifm2)';
  rdgFormats.Cells[Ord(sfcCompactByInterbedSystem), 0] := 'Format for printing compaction by interbed system (Ifm3)';
  rdgFormats.Cells[Ord(sfcVerticalDisplacement), 0] := 'Format for printing vertical displacement (Ifm4)';
  rdgFormats.Cells[Ord(sfcNoDelayPreconsolidationHead), 0] := 'Format for printing no-delay preconsolidation head (Ifm5)';
  rdgFormats.Cells[Ord(sfcDelayPreconsolidationHead), 0] := 'Format for printing delay preconsolidation head (Ifm6)';

  rdgOutput.Cells[Ord(socStartTime), 0] := StrStartingTime;
  rdgOutput.Cells[Ord(socEndTime), 0] := StrEndingTime;
  rdgOutput.Cells[Ord(socPrintSubsidence), 0] := 'Print subsidence (Ifl1)';
  rdgOutput.Cells[Ord(socSaveSubsidence), 0] := 'Save subsidence (Ifl2)';
  rdgOutput.Cells[Ord(socPrintCompactModelLayer), 0] := 'Print compaction by model layer (Ifl3)';
  rdgOutput.Cells[Ord(socSaveCompactModelLayer), 0] := 'Save compaction by model layer (Ifl4)';
  rdgOutput.Cells[Ord(socPrintCompactByInterbedSystem), 0] := 'Print compaction by interbed system (Ifl5)';
  rdgOutput.Cells[Ord(socSaveCompactByInterbedSystem), 0] := 'Save compaction by interbed system (Ifl6)';
  rdgOutput.Cells[Ord(socPrintVerticalDisplacement), 0] := 'Print vertical displacement (Ifl7)';
  rdgOutput.Cells[Ord(socSaveVerticalDisplacement), 0] := 'Save vertical displacement (Ifl8)';
  rdgOutput.Cells[Ord(socPrintNoDelayPreconsolidationHead), 0] := 'Print critical head for no-delay interbeds (Ifl9)';
  rdgOutput.Cells[Ord(socSaveNoDelayPreconsolidationHead), 0] := 'Save critical head for no-delay interbeds (Ifl10)';
  rdgOutput.Cells[Ord(socPrintDelayPreconsolidationHead), 0] := 'Print critical head for delay interbeds (Ifl11)';
  rdgOutput.Cells[Ord(socSaveDelayPreconsolidationHead), 0] := 'Save critical head for delay interbeds (Ifl12)';
  rdgOutput.Cells[Ord(socPrintDelayInterbedBudget), 0] := 'Print volumetric budget for delay interbeds (Ifl13)';

  pcSub.ActivePageIndex := 0;
end;

procedure TframePackageSub.EnableMultiEditControl(Control: TControl; DataGrid: TRbwDataGrid4);
var
  RowIndex: Integer;
  ShouldEnable: Boolean;
  ColIndex: Integer;
begin
  ShouldEnable := False;
  for RowIndex := DataGrid.FixedRows to DataGrid.RowCount - 1 do
  begin
    for ColIndex := 2 to DataGrid.ColCount - 1 do
    begin
      ShouldEnable := DataGrid.IsSelectedCell(ColIndex, RowIndex);
      if ShouldEnable then
      begin
        break;
      end;
    end;
    if ShouldEnable then
    begin
      break;
    end;
  end;
  Control.Enabled := ShouldEnable;
end;

procedure TframePackageSub.ChangeGridOptions(DataGrid: TRbwDataGrid4; Shift: TShiftState);
begin
  if ([ssShift, ssCtrl] * Shift) = [] then
  begin
    DataGrid.Options := DataGrid.Options + [goEditing];
  end
  else
  begin
    DataGrid.Options := DataGrid.Options - [goEditing];
  end;
end;

procedure TframePackageSub.FrameResize(Sender: TObject);
begin
  inherited;
  LayoutFormatControls;
  LayoutPrintSaveControls;
end;

procedure TframePackageSub.GetData(Package: TModflowPackageSelection);
var
  SubPackage: TSubPackageSelection;
  TimeIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  Index: Integer;
  Item: TSubPrintItem;
begin
  inherited;
  pcSub.ActivePageIndex := 0;

  rdgOutput.Columns[Ord(socStartTime)].PickList.Clear;
  rdgOutput.Columns[Ord(socEndTime)].PickList.Clear;
  for TimeIndex := 0 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
  begin
    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods[TimeIndex];
    rdgOutput.Columns[Ord(socStartTime)].PickList.
      Add(FloatToStr(StressPeriod.StartTime));
    rdgOutput.Columns[Ord(socEndTime)].PickList.
      Add(FloatToStr(StressPeriod.EndTime));
  end;

  SubPackage := Package as TSubPackageSelection;
  seNumberOfNodes.AsInteger := SubPackage.NumberOfNodes;
  rdeAccel1.Text := FloatToStr(SubPackage.AccelerationParameter1);
  rdeAccel2.Text := FloatToStr(SubPackage.AccelerationParameter2);
  rdeMinIterations.Text := IntToStr(SubPackage.MinIterations);
  cbSaveRestart.Checked := SubPackage.SaveDelayRestart;
  feReadRestart.FileName := SubPackage.ReadDelayRestartFileName;
  comboOutputChoice.ItemIndex := Ord(SubPackage.BinaryOutputChoice);

  rdgFormats.ItemIndex[Ord(sfcSubsidence), 1] :=
    SubPackage.PrintFormats.SubsidenceFormat;
  rdgFormats.ItemIndex[Ord(sfcCompactModelLayer), 1] :=
    SubPackage.PrintFormats.CompactionByModelLayerFormat;
  rdgFormats.ItemIndex[Ord(sfcCompactByInterbedSystem), 1] :=
    SubPackage.PrintFormats.CompactionByInterbedSystemFormat;
  rdgFormats.ItemIndex[Ord(sfcVerticalDisplacement), 1] :=
    SubPackage.PrintFormats.VerticalDisplacementFormat;
  rdgFormats.ItemIndex[Ord(sfcNoDelayPreconsolidationHead), 1] :=
    SubPackage.PrintFormats.NoDelayPreconsolidationHeadFormat;
  rdgFormats.ItemIndex[Ord(sfcDelayPreconsolidationHead), 1] :=
    SubPackage.PrintFormats.DelayPreconsolidationHeadFormat;

  seNumExportPeriods.AsInteger := SubPackage.PrintChoices.Count;
  seNumExportPeriodsChange(nil);
  for Index := 0 to SubPackage.PrintChoices.Count - 1 do
  begin
    Item := SubPackage.PrintChoices[Index];
    rdgOutput.Cells[Ord(socStartTime),Index+1] := FloatToStr(Item.StartTime);
    rdgOutput.Cells[Ord(socEndTime),Index+1] := FloatToStr(Item.EndTime);
    rdgOutput.Checked[Ord(socPrintSubsidence),Index+1]
      := Item.PrintSubsidence;
    rdgOutput.Checked[Ord(socSaveSubsidence),Index+1]
      := Item.SaveSubsidence;
    rdgOutput.Checked[Ord(socPrintCompactModelLayer),Index+1]
      := Item.PrintCompactionByModelLayer;
    rdgOutput.Checked[Ord(socSaveCompactModelLayer),Index+1]
      := Item.SaveCompactionByModelLayer;
    rdgOutput.Checked[Ord(socPrintCompactByInterbedSystem),Index+1]
      := Item.PrintCompactionByInterbedSystem;
    rdgOutput.Checked[Ord(socSaveCompactByInterbedSystem),Index+1]
      := Item.SaveCompactionByInterbedSystem;
    rdgOutput.Checked[Ord(socPrintVerticalDisplacement),Index+1]
      := Item.PrintVerticalDisplacement;
    rdgOutput.Checked[Ord(socSaveVerticalDisplacement),Index+1]
      := Item.SaveVerticalDisplacement;
    rdgOutput.Checked[Ord(socPrintNoDelayPreconsolidationHead),Index+1]
      := Item.PrintCriticalHeadNoDelay;
    rdgOutput.Checked[Ord(socSaveNoDelayPreconsolidationHead),Index+1]
      := Item.SaveCriticalHeadNoDelay;
    rdgOutput.Checked[Ord(socPrintDelayPreconsolidationHead),Index+1]
      := Item.PrintCriticalHeadDelay;
    rdgOutput.Checked[Ord(socSaveDelayPreconsolidationHead),Index+1]
      := Item.SaveCriticalHeadDelay;
    rdgOutput.Checked[Ord(socPrintDelayInterbedBudget),Index+1]
      := Item.PrintDelayBudgets;
  end;
end;

procedure TframePackageSub.LayoutPrintSaveControls;
begin
  LayoutControls(rdgOutput, cbMultiPrintSave, nil,
    Max(Ord(socPrintSubsidence), rdgOutput.LeftCol), rdgOutput.Left);
  cbMultiPrintSave.Width := 200;
end;

procedure TframePackageSub.Loaded;
begin
  inherited;
  MoveControlsToTab(pcSub);
  rdgFormats.Height := tabFormat.Height - rdgFormats.Top - 20;
  seNumExportPeriods.Top := tabPrintSave.Height
    - seNumExportPeriods.Height -8;
  lblNumExportPeriods.Top := seNumExportPeriods.Top + 3;
  sbAdd.Top := tabPrintSave.Height
    - sbAdd.Height -8;
  sbInsert.Top := sbAdd.Top;
  sbDelete.Top := sbAdd.Top;
  rdgOutput.Height := tabPrintSave.Height - rdgOutput.Top -
    (tabPrintSave.Height - seNumExportPeriods.Top + 8)
end;

procedure TframePackageSub.LayoutFormatControls;
var
  Column: Integer;
begin
  Column := Max(Ord(sfcSubsidence), rdgFormats.LeftCol);
  LayoutControls(rdgFormats, comboMultiFomat, nil,
    Column, rdgFormats.Left);
  comboMultiFomat.Width := rdgFormats.ColWidths[Column];
end;

procedure TframePackageSub.rcSelectionControllerEnabledChange(Sender: TObject);
begin
  inherited;
  EnableDeleteButton;
end;

procedure TframePackageSub.rdgFormatsColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutFormatControls;
end;

procedure TframePackageSub.rdgFormatsHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutFormatControls;
end;

procedure TframePackageSub.rdgFormatsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ChangeGridOptions(rdgFormats, Shift);
end;

procedure TframePackageSub.rdgFormatsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  EnableMultiEditControl(comboMultiFomat, rdgFormats);
end;

procedure TframePackageSub.rdgOutputColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutPrintSaveControls;
end;

procedure TframePackageSub.rdgOutputEndUpdate(Sender: TObject);
begin
  inherited;
  if not FChangingRowCount then
  begin
    seNumExportPeriods.AsInteger := rdgOutput.RowCount -1;
  end;
end;

procedure TframePackageSub.rdgOutputHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutPrintSaveControls;
end;

procedure TframePackageSub.rdgOutputMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ChangeGridOptions(rdgOutput, Shift);
end;

procedure TframePackageSub.rdgOutputMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnableMultiEditControl(cbMultiPrintSave, rdgOutput);
end;

procedure TframePackageSub.sbAddClick(Sender: TObject);
begin
  inherited;
  seNumExportPeriods.AsInteger := seNumExportPeriods.AsInteger +1;
  seNumExportPeriodsChange(nil);
end;

procedure TframePackageSub.sbDeleteClick(Sender: TObject);
begin
  inherited;
  Assert(seNumExportPeriods.AsInteger > 0);
  if seNumExportPeriods.AsInteger = 1 then
  begin
    seNumExportPeriods.AsInteger := 0;
    seNumExportPeriodsChange(Sender);
  end
  else
  begin
    if (rdgOutput.Row >= 1) and (rdgOutput.Row < rdgOutput.RowCount) then
    begin
      rdgOutput.DeleteRow(rdgOutput.Row);
      seNumExportPeriods.AsInteger := seNumExportPeriods.AsInteger - 1;
      seNumExportPeriodsChange(nil);
    end;
  end;
end;

procedure TframePackageSub.sbInsertClick(Sender: TObject);
begin
  inherited;
  if seNumExportPeriods.AsInteger = 0 then
  begin
    sbAddClick(nil);
  end
  else
  begin
    if (rdgOutput.Row >= 1) and (rdgOutput.Row < rdgOutput.RowCount) then
    begin
      rdgOutput.InsertRow(rdgOutput.Row);
    end;
  end;
end;

procedure TframePackageSub.seNumExportPeriodsChange(Sender: TObject);
var
  NumRows: Integer;
begin
  inherited;
  FChangingRowCount := True;
  try
  NumRows := seNumExportPeriods.AsInteger + 1;
  if NumRows = 1 then
  begin
    NumRows := 2;
  end;
  rdgOutput.BeginUpdate;
  try
    rdgOutput.RowCount := NumRows;
  finally
    rdgOutput.EndUpdate;
  end;
  EnableDeleteButton;
  finally
    FChangingRowCount := False;
  end;
end;

procedure TframePackageSub.SetData(Package: TModflowPackageSelection);
var
  SubPackage: TSubPackageSelection;
  PrintChoiceCount: Integer;
  Index: Integer;
  StartTime: double;
  EndTime: double;
  Item: TSubPrintItem;
begin
  inherited;
  SubPackage := Package as TSubPackageSelection;
  SubPackage.NumberOfNodes := seNumberOfNodes.AsInteger;
  SubPackage.AccelerationParameter1 := StrToFloat(rdeAccel1.Text);
  SubPackage.AccelerationParameter2 := StrToFloat(rdeAccel2.Text);
  SubPackage.MinIterations := StrToInt(rdeMinIterations.Text);
  SubPackage.SaveDelayRestart := cbSaveRestart.Checked;
  SubPackage.ReadDelayRestartFileName := feReadRestart.FileName;
  SubPackage.BinaryOutputChoice :=
    TSubBinaryOutputChoice(comboOutputChoice.ItemIndex);

  SubPackage.PrintFormats.SubsidenceFormat :=
    rdgFormats.ItemIndex[Ord(sfcSubsidence), 1];
  SubPackage.PrintFormats.CompactionByModelLayerFormat :=
    rdgFormats.ItemIndex[Ord(sfcCompactModelLayer), 1];
  SubPackage.PrintFormats.CompactionByInterbedSystemFormat :=
    rdgFormats.ItemIndex[Ord(sfcCompactByInterbedSystem), 1];
  SubPackage.PrintFormats.VerticalDisplacementFormat :=
    rdgFormats.ItemIndex[Ord(sfcVerticalDisplacement), 1];
  SubPackage.PrintFormats.NoDelayPreconsolidationHeadFormat :=
    rdgFormats.ItemIndex[Ord(sfcNoDelayPreconsolidationHead), 1];
  SubPackage.PrintFormats.DelayPreconsolidationHeadFormat :=
    rdgFormats.ItemIndex[Ord(sfcDelayPreconsolidationHead), 1];

  PrintChoiceCount := 0;
  for Index := 1 to rdgOutput.RowCount - 1 do
  begin
    if TryStrToFloat(rdgOutput.Cells[Ord(socStartTime),Index], StartTime)
      and TryStrToFloat(rdgOutput.Cells[Ord(socEndTime),Index], EndTime) then
    begin
      Inc(PrintChoiceCount);
      if SubPackage.PrintChoices.Count < PrintChoiceCount then
      begin
        Item := SubPackage.PrintChoices.Add as TSubPrintItem;
      end
      else
      begin
        Item := SubPackage.PrintChoices[PrintChoiceCount-1];
      end;
      Item.StartTime := StartTime;
      Item.EndTime := EndTime;
      Item.PrintSubsidence :=
        rdgOutput.Checked[Ord(socPrintSubsidence),Index];
      Item.SaveSubsidence :=
        rdgOutput.Checked[Ord(socSaveSubsidence),Index];
      Item.PrintCompactionByModelLayer := 
        rdgOutput.Checked[Ord(socPrintCompactModelLayer),Index];
      Item.SaveCompactionByModelLayer :=
        rdgOutput.Checked[Ord(socSaveCompactModelLayer),Index];
      Item.PrintCompactionByInterbedSystem :=
        rdgOutput.Checked[Ord(socPrintCompactByInterbedSystem),Index];
      Item.SaveCompactionByInterbedSystem :=
        rdgOutput.Checked[Ord(socSaveCompactByInterbedSystem),Index];
      Item.PrintVerticalDisplacement :=
        rdgOutput.Checked[Ord(socPrintVerticalDisplacement),Index];
      Item.SaveVerticalDisplacement :=
        rdgOutput.Checked[Ord(socSaveVerticalDisplacement),Index];
      Item.PrintCriticalHeadNoDelay :=
        rdgOutput.Checked[Ord(socPrintNoDelayPreconsolidationHead),Index];
      Item.SaveCriticalHeadNoDelay :=
        rdgOutput.Checked[Ord(socSaveNoDelayPreconsolidationHead),Index];
      Item.PrintCriticalHeadDelay :=
        rdgOutput.Checked[Ord(socPrintDelayPreconsolidationHead),Index];
      Item.SaveCriticalHeadDelay :=
        rdgOutput.Checked[Ord(socSaveDelayPreconsolidationHead),Index];
      Item.PrintDelayBudgets :=
        rdgOutput.Checked[Ord(socPrintDelayInterbedBudget),Index];
    end;
  end;
  while SubPackage.PrintChoices.Count > PrintChoiceCount do
  begin
    SubPackage.PrintChoices.Delete(SubPackage.PrintChoices.Count-1);
  end;
end;

procedure TframePackageSub.EnableDeleteButton;
begin
  sbDelete.Enabled := (seNumExportPeriods.AsInteger >= 1)
    and rcSelectionController.Enabled;
end;

end.
