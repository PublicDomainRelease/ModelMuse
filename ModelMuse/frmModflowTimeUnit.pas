unit frmModflowTimeUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, Grids,
  RbwDataGrid4, ComCtrls, Mask, JvExMask, JvSpin, JvExStdCtrls, JvCombobox,
  JvListComb, ArgusDataEntry, ModflowTimeUnit, UndoItems,
  RequiredDataSetsUndoUnit;
         
type
  TfrmModflowTime = class(TfrmCustomGoPhast)
    pnlTop: TPanel;
    dgTime: TRbwDataGrid4;
    rdePeriodLength: TRbwDataEntry;
    rdeMaxFirstStepLength: TRbwDataEntry;
    rdeMultiplier: TRbwDataEntry;
    comboSteadyTransient: TJvImageComboBox;
    lblPeriodLength: TLabel;
    lblMaxFirstTimeStepLength: TLabel;
    lblMultiplier: TLabel;
    lblSteadyTransient: TLabel;
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    seNumPeriods: TJvSpinEdit;
    lblNumPeriods: TLabel;
    btnDelete: TButton;
    btnInsert: TButton;
    PageControl1: TPageControl;
    tabEdit: TTabSheet;
    comboTimeUnit: TJvComboBox;
    lblTimeUnit: TLabel;
    btnHelp: TBitBtn;
    procedure FormCreate(Sender: TObject); override;
    procedure dgTimeSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure dgTimeSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure seNumPeriodsChange(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure dgTimeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dgTimeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dgTimeColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdePeriodLengthChange(Sender: TObject);
    procedure rdeMaxFirstStepLengthChange(Sender: TObject);
    procedure rdeMultiplierChange(Sender: TObject);
    procedure comboSteadyTransientChange(Sender: TObject);
    procedure dgTimeBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure FormDestroy(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure dgTimeHorizontalScroll(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure dgTimeButtonClick(Sender: TObject; ACol, ARow: Integer);
  private
    FModflowStressPeriods: TModflowStressPeriods;
    FDeleting: Boolean;
    procedure GetData;
    procedure SetData;
    procedure FillEmptyCells;
    procedure LayoutMultiRowEditControls;
    procedure GetTimePeriodValues(ARow: Integer; var PerLength: Double;
      var MaxTimeStepLength: Double; var TimeStepMultiplier: Double);
    procedure SetDeleteButtonEnabled;
    procedure UpdateNumberOfTimeSteps(const ARow: integer);
  { Private declarations }
  public
    { Public declarations }
  end;

  TUndoModflowStressPeriods = class(TCustomCreateRequiredDataSetsUndo)
  private
    FNewStressPeriods: TModflowStressPeriods;
    FOldStressPeriods: TModflowStressPeriods;
    FNewTimeUnit: integer;
    FOldTimeUnit: integer;
  protected
    function Description: string; override;
  public
    constructor Create(var NewStressPeriods: TModflowStressPeriods;
      NewTimeUnit: integer);
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

implementation

{$R *.dfm}

uses Math, frmGoPhastUnit, frmTimeStepLengthCalculatorUnit, GoPhastTypes;

type
  TTimeColumn = (tcStressPeriod, tcStartTime, tcEndTime, tcLength,
    tcTimeFirstStep, tcMultiplier, tcSteady, tcDrawDownReference, tcSteps);

resourcestring
  StrStressPeriod = 'Stress period';
  StrLength = 'Length';
  StrMaxFirstTimeStep = 'Max first time step length';
  StrMultiplier = 'Multiplier';
  StrSSTR = 'Steady State/ Transient';
  StrNumberOfSteps = 'Number of steps (calculated)';
  StrDrawdownReference = 'Drawdown reference';

var
  MaxSteps: integer = 100;

procedure TfrmModflowTime.btnOKClick(Sender: TObject);
var
  Index: Integer;
  StressPeriods: TModflowStressPeriods;
  StressPeriod: TModflowStressPeriod;
  Steps: integer;
begin
  inherited;
  SetData;
  StressPeriods := frmGoPhast.PhastModel.ModflowStressPeriods;
  Steps := 0;
  for Index := 0 to StressPeriods.Count - 1 do
  begin
    StressPeriod := StressPeriods[Index];
    if StressPeriod.NumberOfSteps > Steps then
    begin
      Steps := StressPeriod.NumberOfSteps;
    end;
  end;
  if dgTime.Checked[Ord(tcDrawDownReference), 1] then
  begin
    if dgTime.Cells[Ord(tcSteady), 1] =
      dgTime.Columns[Ord(tcSteady)].PickList[1] then
    begin
      // Transient model
      Beep;
      MessageDlg('The first stress period is a '
        + 'transient stress period but it is also a reference '
        + 'stress period for calculating drawdown.  This might '
        + 'be a mistake.', mtWarning, [mbOK], 0);
    end
    else if dgTime.RowCount = 2 then
    begin
      Beep;
      MessageDlg('The first and only stress period is a '
        + 'also a reference '
        + 'stress period for calculating drawdown.  This might '
        + 'be a mistake because drawdown will always be calculated '
        + 'as zero under these conditions.', mtWarning, [mbOK], 0);
    end;
  end;
  if Steps > MaxSteps then
  begin
    Beep;
    MessageDlg('At least one stress period has '
      + IntToStr(Steps) + ' time steps. If this is not what you intend, '
      + 'you should fix the problem before trying to run the model.',
      mtWarning, [mbOK], 0);
  end;
end;

procedure TfrmModflowTime.SetDeleteButtonEnabled;
begin
  btnDelete.Enabled := dgTime.RowCount > 2;
end;

procedure TfrmModflowTime.btnDeleteClick(Sender: TObject);
begin
  inherited;
  dgTime.DeleteRow(dgTime.SelectedRow);
  seNumPeriods.AsInteger := seNumPeriods.AsInteger - 1;
  SetDeleteButtonEnabled;
end;

procedure TfrmModflowTime.btnInsertClick(Sender: TObject);
begin
  inherited;
  dgTime.InsertRow(dgTime.SelectedRow);
  seNumPeriods.AsInteger := seNumPeriods.AsInteger + 1;
  SetDeleteButtonEnabled;
end;

procedure TfrmModflowTime.dgTimeBeforeDrawCell(Sender: TObject; ACol,
  ARow: Integer);
var
  PerLength: double;
begin
  inherited;
  if (ARow >= dgTime.FixedRows) then
  begin
    case TTimeColumn(ACol) of
      tcLength:
        begin
          if tryStrToFloat(dgTime.Cells[Ord(tcLength), ARow], PerLength) then
          begin
            if PerLength <= 0 then
            begin
              dgTime.Canvas.Brush.Color := clRed;
            end;
          end;
        end;
      tcStartTime:
        begin
          if (ARow > 1) and
            (dgTime.Cells[Ord(tcStartTime), ARow] <>
              dgTime.Cells[Ord(tcEndTime), ARow-1]) then
          begin
            dgTime.Canvas.Brush.Color := clRed;
          end;
        end;
      tcEndTime:
        begin
          if (ARow < dgTime.RowCount-1) and
            (dgTime.Cells[Ord(tcStartTime), ARow+1] <>
              dgTime.Cells[Ord(tcEndTime), ARow]) then
          begin
            dgTime.Canvas.Brush.Color := clRed;
          end;
        end;
      tcDrawDownReference:
        begin
          if dgTime.Checked[Ord(tcDrawDownReference),ARow]
            and (dgTime.Cells[Ord(tcSteady),ARow] =
            dgTime.Columns[Ord(tcSteady)].PickList[1])  then
          begin
            dgTime.Canvas.Brush.Color := clYellow;
          end;
        end;
    end;
  end;

end;

procedure TfrmModflowTime.dgTimeButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  NumSteps: integer;
  PeriodLength, Multiplier: double;
  TimeStepLength: double;
begin
  inherited;
  NumSteps := StrToInt(dgTime.Cells[Ord(tcSteps),ARow]);
  PeriodLength := StrToFloat(dgTime.Cells[Ord(tcLength),ARow]);
  Multiplier := StrToFloat(dgTime.Cells[Ord(tcMultiplier),ARow]);
  if CalculateTimeStepLength(NumSteps, PeriodLength, Multiplier,
    TimeStepLength) then
  begin
    dgTime.Cells[Ord(tcMultiplier),ARow] := FloatToStr(Multiplier);
    dgTimeSetEditText(dgTime, Ord(tcMultiplier),ARow,
      dgTime.Cells[Ord(tcMultiplier),ARow]);

    dgTime.Cells[Ord(tcLength),ARow] := FloatToStr(PeriodLength);
    dgTimeSetEditText(dgTime, Ord(tcLength),ARow,
      dgTime.Cells[Ord(tcLength),ARow]);

    dgTime.Cells[Ord(tcTimeFirstStep),ARow] := FloatToStr(TimeStepLength);
    dgTimeSetEditText(dgTime, Ord(tcTimeFirstStep),ARow,
      dgTime.Cells[Ord(tcTimeFirstStep),ARow]);
  end;
end;

procedure TfrmModflowTime.dgTimeColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TfrmModflowTime.dgTimeHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TfrmModflowTime.dgTimeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ([ssShift, ssCtrl] * Shift) = [] then
  begin
    dgTime.Options := dgTime.Options + [goEditing];
  end
  else
  begin
    dgTime.Options := dgTime.Options - [goEditing];
  end;
end;

procedure TfrmModflowTime.dgTimeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnableMultiEditControl(dgTime, rdePeriodLength, Ord(tcLength));
  EnableMultiEditControl(dgTime, rdeMaxFirstStepLength, Ord(tcTimeFirstStep));
  EnableMultiEditControl(dgTime, rdeMultiplier, Ord(tcMultiplier));
  EnableMultiEditControl(dgTime, comboSteadyTransient, Ord(tcSteady));
end;

procedure TfrmModflowTime.dgTimeSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  inherited;
  if ACol= Ord(tcSteps) then
  begin
    CanSelect := False;
  end;
end;

procedure TfrmModflowTime.comboSteadyTransientChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(dgTime, Ord(tcSteady), comboSteadyTransient.Text);
end;

procedure TfrmModflowTime.rdeMultiplierChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(dgTime, Ord(tcMultiplier), rdeMultiplier.Text);
end;

procedure TfrmModflowTime.rdeMaxFirstStepLengthChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(dgTime, Ord(tcTimeFirstStep), rdeMaxFirstStepLength.Text);
end;

procedure TfrmModflowTime.rdePeriodLengthChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(dgTime, Ord(tcLength), rdePeriodLength.Text);
end;

procedure TfrmModflowTime.FillEmptyCells;
var
  RowIndex: integer;
  StartTime: Double;
  EndTime: Double;
  PeriodLength: double;
begin
  for RowIndex := 1 to dgTime.RowCount - 1 do
  begin
    if dgTime.Cells[Ord(tcStressPeriod), RowIndex] = '' then
    begin
      dgTime.Cells[Ord(tcStressPeriod), RowIndex] := IntToStr(RowIndex);
    end;

    if dgTime.Cells[Ord(tcStartTime), RowIndex] = '' then
    begin
      if RowIndex = 1 then
      begin
        dgTime.Cells[Ord(tcStartTime), RowIndex] := '0';
      end
      else
      begin
        dgTime.Cells[Ord(tcStartTime), RowIndex] :=
          dgTime.Cells[Ord(tcEndTime), RowIndex-1];
      end;

    end;

    if dgTime.Cells[Ord(tcEndTime), RowIndex] = '' then
    begin
      if TryStrToFloat(dgTime.Cells[Ord(tcStartTime), RowIndex], StartTime) then
      begin
        if (RowIndex = 1) or
          not TryStrToFloat(dgTime.Cells[Ord(tcLength), RowIndex-1],
          PeriodLength) then
        begin
          PeriodLength := 1
        end;
        dgTime.Cells[Ord(tcEndTime), RowIndex] :=
          FloatToStr(StartTime + PeriodLength);
      end
      else
      begin
        dgTime.Cells[Ord(tcEndTime), RowIndex] := '1';
      end;

    end;

    if dgTime.Cells[Ord(tcLength), RowIndex] = '' then
    begin
      if TryStrToFloat(dgTime.Cells[Ord(tcStartTime), RowIndex], StartTime)
        and TryStrToFloat(dgTime.Cells[Ord(tcEndTime), RowIndex], EndTime) then
      begin
        dgTime.Cells[Ord(tcLength), RowIndex]
          := FloatToStr(EndTime - StartTime);
      end
      else
      begin
        dgTime.Cells[Ord(tcLength), RowIndex] := '1';
      end;
    end;

    if dgTime.Cells[Ord(tcTimeFirstStep), RowIndex] = '' then
    begin
      if RowIndex > 1 then
      begin
        dgTime.Cells[Ord(tcTimeFirstStep), RowIndex] :=
          dgTime.Cells[Ord(tcTimeFirstStep), RowIndex-1];
      end
      else
      begin
        dgTime.Cells[Ord(tcTimeFirstStep), RowIndex] := '1';
      end;
    end;

    if dgTime.Cells[Ord(tcMultiplier), RowIndex] = '' then
    begin
      if RowIndex > 1 then
      begin
        dgTime.Cells[Ord(tcMultiplier), RowIndex] :=
          dgTime.Cells[Ord(tcMultiplier), RowIndex-1];
      end
      else
      begin
        dgTime.Cells[Ord(tcMultiplier), RowIndex] := '1';
      end;
    end;

    if dgTime.Cells[Ord(tcSteady), RowIndex] = '' then
    begin
      if RowIndex > 1 then
      begin
        dgTime.Cells[Ord(tcSteady), RowIndex] :=
          dgTime.Columns[Ord(tcSteady)].PickList[1];
      end
      else
      begin
        dgTime.Cells[Ord(tcSteady), RowIndex] :=
          dgTime.Columns[Ord(tcSteady)].PickList[0];
      end;
    end;

    UpdateNumberOfTimeSteps(RowIndex);
  end;
end;

procedure TfrmModflowTime.LayoutMultiRowEditControls;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;  
  LayoutControls(dgTime, rdePeriodLength, lblPeriodLength, Ord(tcLength));
  LayoutControls(dgTime, rdeMaxFirstStepLength, lblMaxFirstTimeStepLength, Ord(tcTimeFirstStep));
  LayoutControls(dgTime, rdeMultiplier, lblMultiplier, Ord(tcMultiplier));
  LayoutControls(dgTime, comboSteadyTransient, lblSteadyTransient, Ord(tcSteady));
end;

procedure TfrmModflowTime.seNumPeriodsChange(Sender: TObject);
begin
  inherited;
  dgTime.BeginUpdate;
  FDeleting := True;
  try
    // If the first and only stress period is a steady-state
    // stress period, then make it the reference period for
    // calculating drawdown when adding more stress periods.
    If (dgTime.RowCount = 2) then
    begin
      if dgTime.Cells[Ord(tcSteady),1] =
        dgTime.Columns[Ord(tcSteady)].PickList[0] then
      begin
        dgTime.Checked[Ord(tcDrawDownReference),1] := True; 
      end;
    end;

    dgTime.RowCount := seNumPeriods.AsInteger + 1;
    FillEmptyCells;
    SetDeleteButtonEnabled;
  finally
    FDeleting := False;
    dgTime.EndUpdate;
  end;
end;

procedure TfrmModflowTime.GetData;
var
  RowIndex: Integer;
  StressPeriod: TModflowStressPeriod;
begin
  comboTimeUnit.ItemIndex := frmGoPhast.PhastModel.ModflowOptions.TimeUnit;
  seNumPeriods.AsInteger := frmGoPhast.PhastModel.ModflowStressPeriods.Count;
//  dgTime.RowCount := frmGoPhast.PhastModel.ModflowStressPeriods.Count + 1;
  FillEmptyCells;
  for RowIndex := 1 to dgTime.RowCount - 1 do
  begin
    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods[RowIndex-1];
    dgTime.Cells[ord(tcStartTime), RowIndex]
      := FloatToStr(StressPeriod.StartTime);
    dgTime.Cells[ord(tcEndTime), RowIndex]
      := FloatToStr(StressPeriod.EndTime);
    dgTime.Cells[ord(tcLength), RowIndex]
      := FloatToStr(StressPeriod.PeriodLength);
    dgTime.Cells[ord(tcTimeFirstStep), RowIndex]
      := FloatToStr(StressPeriod.MaxLengthOfFirstTimeStep);
    dgTime.Cells[ord(tcMultiplier), RowIndex]
      := FloatToStr(StressPeriod.TimeStepMultiplier);
    dgTime.Cells[ord(tcSteady), RowIndex]
      := dgTime.Columns[Ord(tcSteady)].
      PickList[Ord(StressPeriod.StressPeriodType)];
    dgTime.Checked[ord(tcDrawDownReference), RowIndex]
      := StressPeriod.DrawDownReference;
    UpdateNumberOfTimeSteps(RowIndex);
  end;
end;

procedure TfrmModflowTime.SetData;
var
  Index: Integer;
  StressPeriod: TModflowStressPeriod;
  Value: double;
  IntValue: integer;
  Undo: TUndoModflowStressPeriods;
begin
  FModflowStressPeriods.Clear;

  for Index := 1 to dgTime.RowCount - 1 do
  begin
    StressPeriod := FModflowStressPeriods.Add as TModflowStressPeriod;

    if TryStrToFloat(dgTime.Cells[ord(tcStartTime), Index], Value) then
    begin
      StressPeriod.StartTime := Value;
    end
    else
    begin
      FModflowStressPeriods.Delete(FModflowStressPeriods.Count -1);
      Continue;
    end;

    if TryStrToFloat(dgTime.Cells[ord(tcEndTime), Index], Value) then
    begin
      StressPeriod.EndTime := Value;
    end
    else
    begin
      FModflowStressPeriods.Delete(FModflowStressPeriods.Count -1);
      Continue;
    end;

    if TryStrToFloat(dgTime.Cells[ord(tcLength), Index], Value) then
    begin
      StressPeriod.PeriodLength := Value;
    end
    else
    begin
      FModflowStressPeriods.Delete(FModflowStressPeriods.Count -1);
      Continue;
    end;

    if TryStrToFloat(dgTime.Cells[ord(tcTimeFirstStep), Index], Value) then
    begin
      StressPeriod.MaxLengthOfFirstTimeStep := Value;
    end
    else
    begin
      FModflowStressPeriods.Delete(FModflowStressPeriods.Count -1);
      Continue;
    end;

    if TryStrToFloat(dgTime.Cells[ord(tcMultiplier), Index], Value) then
    begin
      StressPeriod.TimeStepMultiplier := Value;
    end
    else
    begin
      FModflowStressPeriods.Delete(FModflowStressPeriods.Count -1);
      Continue;
    end;

    IntValue := dgTime.Columns[Ord(tcSteady)].PickList.IndexOf(
      dgTime.Cells[ord(tcSteady), Index]);
    Assert(IntValue in
      [Ord(Low(TStressPeriodType))..Ord(High(TStressPeriodType))]);
    StressPeriod.StressPeriodType := TStressPeriodType(IntValue);
    StressPeriod.DrawDownReference :=
      dgTime.Checked[Ord(tcDrawDownReference), Index]
  end;

  Undo:= TUndoModflowStressPeriods.Create(FModflowStressPeriods,
    comboTimeUnit.ItemIndex);
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmModflowTime.GetTimePeriodValues(ARow: Integer;
  var PerLength, MaxTimeStepLength, TimeStepMultiplier: Double);
begin
  if not TryStrToFloat(dgTime.Cells[Ord(tcLength), ARow], PerLength) then
  begin
    PerLength := 0;
  end;
  if not TryStrToFloat(dgTime.Cells[Ord(tcTimeFirstStep), ARow], MaxTimeStepLength) then
  begin
    MaxTimeStepLength := 1;
  end;
  if not TryStrToFloat(dgTime.Cells[Ord(tcMultiplier), ARow],
    TimeStepMultiplier) then
  begin
    TimeStepMultiplier := 1;
  end;
end;

procedure TfrmModflowTime.UpdateNumberOfTimeSteps(const ARow: integer);
var
  PerLength: double;
  MaxTimeStepLength: double;
  TimeStepMultiplier: double;
begin
  GetTimePeriodValues(ARow, PerLength, MaxTimeStepLength, TimeStepMultiplier);
  dgTime.Cells[Ord(tcSteps),ARow] := IntToStr(
    GetNumberOfTimeSteps(PerLength, MaxTimeStepLength, TimeStepMultiplier));
end;

procedure TfrmModflowTime.dgTimeSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  StartTime: double;
  EndTime: double;
  PeriodLength: double;
  procedure UpdatePeriodLength(Const ARow: integer);
  begin
    if not TryStrToFloat(dgTime.Cells[Ord(tcStartTime), ARow], StartTime) then
    begin
      StartTime := 0;
    end;
    if not TryStrToFloat(dgTime.Cells[Ord(tcEndTime), ARow], EndTime) then
    begin
      EndTime := 0;
    end;
    dgTime.Cells[Ord(tcLength), ARow] := FloatToStr(EndTime-StartTime);
    UpdateNumberOfTimeSteps(ARow);
  end;
begin
  inherited;
  if FDeleting then
  begin
    Exit;
  end;
  if dgTime.RowCount <> seNumPeriods.AsInteger + 1 then
  begin
    seNumPeriods.AsInteger := dgTime.RowCount -1;
  end;

  if Ord(tcStartTime) = ACol then
  begin
    UpdatePeriodLength(ARow);
    if ARow > 1 then
    begin
      dgTime.Cells[Ord(tcEndTime), ARow -1] := Value;
      UpdatePeriodLength(ARow-1);
    end;
  end;
  if Ord(tcEndTime) = ACol then
  begin
    UpdatePeriodLength(ARow);
    if ARow < dgTime.RowCount -1 then
    begin
      dgTime.Cells[Ord(tcStartTime), ARow +1] := Value;
      UpdatePeriodLength(ARow+1);
    end;
  end;
  if Ord(tcLength) = ACol then
  begin
    if not TryStrToFloat(dgTime.Cells[Ord(tcStartTime), ARow], StartTime) then
    begin
      StartTime := 0;
    end;
    if not TryStrToFloat(dgTime.Cells[Ord(tcLength), ARow], PeriodLength) then
    begin
      PeriodLength := 0;
    end;
    EndTime := StartTime + PeriodLength;
    dgTime.Cells[Ord(tcEndTime),ARow] := FloatToStr(EndTime);
    if ARow < dgTime.RowCount -1 then
    begin
      dgTime.Cells[Ord(tcStartTime), ARow +1] :=
        dgTime.Cells[Ord(tcEndTime),ARow];
      UpdatePeriodLength(ARow+1);
    end;
  end;
  UpdateNumberOfTimeSteps(ARow);
end;

procedure TfrmModflowTime.FormCreate(Sender: TObject);
begin
  inherited;
  FModflowStressPeriods:= TModflowStressPeriods.Create(nil);

  dgTime.ColWidths[Ord(tcTimeFirstStep)] := Round(dgTime.DefaultColWidth*1.6);

  dgTime.Cells[Ord(tcStressPeriod), 0] := StrStressPeriod;
  dgTime.Cells[Ord(tcStartTime), 0] := StrStartingTime;
  dgTime.Cells[Ord(tcEndTime), 0] := StrEndingTime;
  dgTime.Cells[Ord(tcLength), 0] := StrLength;
  dgTime.Cells[Ord(tcTimeFirstStep), 0] := StrMaxFirstTimeStep;
  dgTime.Cells[Ord(tcMultiplier), 0] := StrMultiplier;
  dgTime.Cells[Ord(tcSteady), 0] := StrSSTR;
  dgTime.Cells[Ord(tcDrawDownReference), 0] := StrDrawdownReference;
  dgTime.Cells[Ord(tcSteps), 0] := StrNumberOfSteps;

  lblPeriodLength.Caption := StrLength;
  lblMaxFirstTimeStepLength.Caption := StrMaxFirstTimeStep;
  lblMultiplier.Caption := StrMultiplier;
  lblSteadyTransient.Caption := StrSSTR;

  LayoutMultiRowEditControls;

  FillEmptyCells;
  GetData;
  SetDeleteButtonEnabled;
end;

procedure TfrmModflowTime.FormDestroy(Sender: TObject);
begin
  inherited;
  FModflowStressPeriods.Free;
end;

procedure TfrmModflowTime.FormResize(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

{ TUndoModflowStressPeriods }

constructor TUndoModflowStressPeriods.Create(
  var NewStressPeriods: TModflowStressPeriods; NewTimeUnit: integer);
begin
  inherited Create;
  FNewStressPeriods := NewStressPeriods;
  NewStressPeriods := nil;
  FOldStressPeriods:= TModflowStressPeriods.Create(nil);
  FOldStressPeriods.Assign(frmGoPhast.PhastModel.ModflowStressPeriods);

  FNewTimeUnit := NewTimeUnit;
  FOldTimeUnit := frmGoPhast.PhastModel.ModflowOptions.TimeUnit;
end;

function TUndoModflowStressPeriods.Description: string;
begin
  result := 'change stress periods';
end;

destructor TUndoModflowStressPeriods.Destroy;
begin
  FNewStressPeriods.Free;
  FOldStressPeriods.Free;
  inherited;
end;

procedure TUndoModflowStressPeriods.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.ModflowStressPeriods := FNewStressPeriods;
  frmGoPhast.PhastModel.ModflowOptions.TimeUnit := FNewTimeUnit;
  UpdatedRequiredDataSets;
end;

procedure TUndoModflowStressPeriods.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.ModflowStressPeriods.Assign(FOldStressPeriods);
  frmGoPhast.PhastModel.ModflowOptions.TimeUnit := FOldTimeUnit;
  UpdatedRequiredDataSets;
end;

end.
