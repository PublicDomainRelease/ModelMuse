unit frameSfrParamInstancesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Buttons, Mask, JvExMask, JvSpin, Grids, RbwDataGrid4,
  ExtCtrls;

type
  TSfrInstanceColumn = (sicStartTime, sicEndTime, sicInstanceName);

  TframeSfrParamInstances = class(TFrame)
    pnlSfrInstancesBottom: TPanel;
    rdgSfrParamInstances: TRbwDataGrid4;
    pnlLabel: TPanel;
    seInstanceCount: TJvSpinEdit;
    lblInstanceCount: TLabel;
    btnInsertFlowTableRow: TBitBtn;
    btnDeleteFlowTableRow: TBitBtn;
    procedure seInstanceCountChange(Sender: TObject);
    procedure btnInsertFlowTableRowClick(Sender: TObject);
    procedure btnDeleteFlowTableRowClick(Sender: TObject);
    procedure rdgSfrParamInstancesExit(Sender: TObject);
    procedure rdgSfrParamInstancesSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure rdgSfrParamInstancesEndUpdate(Sender: TObject);
  private
    ErrorFound: boolean;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TframeSfrParamInstances.btnDeleteFlowTableRowClick(Sender: TObject);
begin
  if rdgSfrParamInstances.SelectedRow > 0 then
  begin
    if rdgSfrParamInstances.RowCount > 2 then
    begin
      rdgSfrParamInstances.DeleteRow(rdgSfrParamInstances.SelectedRow);
    end;
    seInstanceCount.AsInteger := seInstanceCount.AsInteger -1;
    seInstanceCount.OnChange(nil);
  end;
end;

procedure TframeSfrParamInstances.btnInsertFlowTableRowClick(Sender: TObject);
begin
  if rdgSfrParamInstances.SelectedRow > 0 then
  begin
    rdgSfrParamInstances.InsertRow(rdgSfrParamInstances.SelectedRow);
    seInstanceCount.AsInteger := seInstanceCount.AsInteger +1;
    seInstanceCount.OnChange(nil);
  end;
end;

constructor TframeSfrParamInstances.Create(AOwner: TComponent);
begin
  inherited;
  rdgSfrParamInstances.Cells[Ord(sicStartTime), 0] := 'Start time';
  rdgSfrParamInstances.Cells[Ord(sicEndTime), 0] := 'End time';
  rdgSfrParamInstances.Cells[Ord(sicInstanceName), 0] := 'Instance Name';
end;

procedure TframeSfrParamInstances.rdgSfrParamInstancesEndUpdate(
  Sender: TObject);
begin
  seInstanceCount.AsInteger := rdgSfrParamInstances.RowCount -1;
end;

procedure TframeSfrParamInstances.rdgSfrParamInstancesExit(Sender: TObject);
const
  ValidFirstChar = ['a'..'z', 'A'..'Z', '_'];
  ValidChar = ['a'..'z', 'A'..'Z', '_', '0'..'9'];
var
  Index: Integer;
  AName: string;
  CharIndex: Integer;
  InstanceNames: TStringList;
  NewSelection: TGridRect;
begin
  if ErrorFound then
  begin
    // prevent infinite recursion.
    ErrorFound := False;
    Exit;
  end;
  // Ensure validity of instance names.
  for Index := 1 to rdgSfrParamInstances.RowCount - 1 do
  begin
    AName := rdgSfrParamInstances.Cells[Ord(sicInstanceName), Index];
    AName := Trim(AName);
    if Length(AName) > 0 then
    begin
      if not (AName[1] in ValidFirstChar) then
      begin
        AName[1] := '_'
      end;
      for CharIndex := 1 to Length(AName) do
      begin
        if not (AName[CharIndex] in ValidChar) then
        begin
          AName[CharIndex] := '_'
        end;
      end;
    end
    else
    begin
      AName := 'SP' + IntToStr(Index);
    end;
    rdgSfrParamInstances.Cells[Ord(sicInstanceName), Index] := AName;
  end;
  InstanceNames := TStringList.Create;
  try
    InstanceNames.Assign(rdgSfrParamInstances.Cols[Ord(sicInstanceName)]);
    InstanceNames.Delete(0);
    for Index := 0 to InstanceNames.Count - 1 do
    begin
      InstanceNames[Index] := UpperCase(InstanceNames[Index]);
    end;
    for Index := 0 to InstanceNames.Count - 1 do
    begin
      AName := InstanceNames[Index];
      if (AName <> '') and (InstanceNames.IndexOf(AName) <> Index) then
      begin
        NewSelection.Left := Ord(sicInstanceName);
        NewSelection.Right := NewSelection.Left;
        NewSelection.Top := Index+1;
        NewSelection.Bottom := NewSelection.Top;
        rdgSfrParamInstances.Selection := NewSelection;
        AName := rdgSfrParamInstances.Cells[Ord(sicInstanceName), Index+1];
        Beep;
        MessageDlg('The names of parameter instances must be unique. '
          + 'You should correct the name "' + AName + '."', mtError,
          [mbOK], 0);
        ErrorFound := True;
        rdgSfrParamInstances.SetFocus;
        Exit;
      end;
    end;
  finally
    InstanceNames.Free;
  end;
end;

procedure TframeSfrParamInstances.rdgSfrParamInstancesSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect := seInstanceCount.AsInteger > 0;
end;

procedure TframeSfrParamInstances.seInstanceCountChange(Sender: TObject);
var
  ColIndex: Integer;
begin
  if seInstanceCount.AsInteger = 0 then
  begin
    rdgSfrParamInstances.RowCount := 2;
    for ColIndex := 0 to rdgSfrParamInstances.ColCount - 1 do
    begin
      rdgSfrParamInstances.Cells[ColIndex,1] := '';
    end;
  end
  else
  begin
    rdgSfrParamInstances.RowCount := seInstanceCount.AsInteger + 1;
  end;
  btnDeleteFlowTableRow.Enabled := seInstanceCount.AsInteger > 0;
end;

end.
