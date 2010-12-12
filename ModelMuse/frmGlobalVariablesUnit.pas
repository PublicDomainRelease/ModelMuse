unit frmGlobalVariablesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, Mask, JvExMask,
  JvSpin, Grids, RbwDataGrid4, GlobalVariablesUnit, UndoItems;

type
  TGlobalVariableColumns = (gvName, gvType, gvValue, gvComment);

  TfrmGlobalVariables = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    rdgGlobalVariables: TRbwDataGrid4;
    seGlobalVariableCount: TJvSpinEdit;
    Label1: TLabel;
    btnDelete: TButton;
    procedure seGlobalVariableCountChange(Sender: TObject);
    procedure rdgGlobalVariablesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure rdgGlobalVariablesSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure rdgGlobalVariablesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdgGlobalVariablesEndUpdate(Sender: TObject);
    procedure rdgGlobalVariablesBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
  private
    FInitializingRow: Boolean;
    FNewGlobals: TGlobalVariables;
    VariableNames: TStringList;
    OkVariables: array of boolean;
    function GenerateNewName(Root: string = 'NewGlobalVariable';
      const CurrentRow: integer = -1): string;
    procedure GetData;
    procedure SetData;
    procedure InitializeNewRow(ARow: integer);
    procedure UpdateSpecialFormat(RowIndex: integer);
    function GenerateNewRoot(const Root: string): string;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoGlobalVariables = class(TCustomUndo)
  private
    FNewGlobalVariables: TGlobalVariables;
    FOldGlobalVariables: TGlobalVariables;
    FOldNames: TStringList;
    FNewNames: TStringList;
    procedure UpdateFormulas(NewNames: TStringList; OldNames: TStringList);
  public
    Constructor Create(var NewGlobals: TGlobalVariables;
      var OldNames, NewNames: TStringList);
    Destructor Destroy; override;
    function Description: string; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

implementation

uses RbwParser, frmGoPhastUnit, DataSetUnit, PhastModelUnit;

{$R *.dfm}

procedure TfrmGlobalVariables.rdgGlobalVariablesBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
begin
  inherited;
  if (ACol = Ord(gvName)) and (ARow >= rdgGlobalVariables.FixedRows) then
  begin
    if (ARow < Length(OkVariables)) and not OkVariables[ARow] then
    begin
      rdgGlobalVariables.Canvas.Brush.Color := clRed;
    end;
  end;
end;

procedure TfrmGlobalVariables.rdgGlobalVariablesEndUpdate(Sender: TObject);
var
  RowIndex: Integer;
  ShouldEnableOkButton: Boolean;
  DataArrayManager: TDataArrayManager;
begin
  inherited;
  if VariableNames = nil then
  begin
    Exit;
  end;
  if (rdgGlobalVariables.Cells[Ord(gvName),1] <> '') or (rdgGlobalVariables.RowCount > 2) then
  begin
    seGlobalVariableCount.AsInteger := rdgGlobalVariables.RowCount -1;
  end;
  VariableNames.Assign(rdgGlobalVariables.Cols[Ord(gvName)]);
  SetLength(OkVariables, VariableNames.Count);
  VariableNames[0] := '';
  VariableNames.CaseSensitive := False;
  ShouldEnableOkButton := True;
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  for RowIndex := 1 to VariableNames.Count - 1 do
  begin
    OkVariables[RowIndex] := (VariableNames[RowIndex] = '')
      or (VariableNames.IndexOf(VariableNames[RowIndex]) = RowIndex);
    if (DataArrayManager.IndexOfDataSet(VariableNames[RowIndex]) >= 0)
      or (DataArrayManager.IndexOfBoundaryDataSet(VariableNames[RowIndex]) >= 0) then
    begin
      OkVariables[RowIndex] := False;
    end;
    if not OkVariables[RowIndex] then
    begin
      ShouldEnableOkButton := False;
    end;
  end;
  btnOK.Enabled := ShouldEnableOkButton;
  rdgGlobalVariables.Invalidate;
end;

procedure TfrmGlobalVariables.rdgGlobalVariablesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ([ssShift, ssCtrl] * Shift) = [] then
  begin
    rdgGlobalVariables.Options := rdgGlobalVariables.Options + [goEditing];
  end
  else
  begin
    rdgGlobalVariables.Options := rdgGlobalVariables.Options - [goEditing];
  end;
end;

procedure TfrmGlobalVariables.rdgGlobalVariablesSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
//  CanSelect := seGlobalVariableCount.AsInteger >= 1;
end;

procedure TfrmGlobalVariables.rdgGlobalVariablesSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
var
  Column: TGlobalVariableColumns;
  NewValue: string;
begin
  inherited;
  if FInitializingRow then
  begin
    Exit;
  end;
  if (ARow > 0) then
  begin
    Column := TGlobalVariableColumns(ACol);
    case Column of
      gvName:
        begin
          NewValue := GenerateNewRoot(Value);
          if NewValue <> Value then
          begin
            rdgGlobalVariables.Cells[ACol, ARow] := NewValue
          end;
          if (Value <> '') then
          begin
            if rdgGlobalVariables.Cells[Ord(gvType), ARow] = '' then
            begin
              rdgGlobalVariables.ItemIndex[Ord(gvType), ARow] := 0;
              if rdgGlobalVariables.Cells[Ord(gvValue), ARow] = '' then
              begin
                rdgGlobalVariables.Cells[Ord(gvValue), ARow] := '0';
              end;
            end;
          end;
        end;
      gvType:
        begin
          UpdateSpecialFormat(ARow);
        end;
      gvValue:
        begin

        end;
      gvComment:
        begin

        end;
      else Assert(False);
    end;

  end;
end;

procedure TfrmGlobalVariables.seGlobalVariableCountChange(Sender: TObject);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  inherited;
  while seGlobalVariableCount.AsInteger > FNewGlobals.Count do
  begin
    FNewGlobals.Add;
  end;
  while seGlobalVariableCount.AsInteger < FNewGlobals.Count do
  begin
    FNewGlobals.Delete(FNewGlobals.Count-1);
  end;

  rdgGlobalVariables.BeginUpdate;
  try
    if seGlobalVariableCount.AsInteger >= 1 then
    begin
      rdgGlobalVariables.RowCount := seGlobalVariableCount.AsInteger +1;
    end
    else
    begin
      rdgGlobalVariables.RowCount := 2;
      for ColIndex := 0 to rdgGlobalVariables.ColCount - 1 do
      begin
        rdgGlobalVariables.Cells[ColIndex,1] := '';
      end;
    end;
    for RowIndex := 1 to seGlobalVariableCount.AsInteger do
    begin
      if rdgGlobalVariables.Cells[Ord(gvName), RowIndex] = '' then
      begin
        InitializeNewRow(RowIndex);
      end;
    end;
  finally
    rdgGlobalVariables.EndUpdate;
  end;
  btnDelete.Enabled := seGlobalVariableCount.AsInteger >= 1;
end;

procedure TfrmGlobalVariables.SetData;
var
  Undo: TUndoGlobalVariables;
  RowIndex: Integer;
  OK: boolean;
  ColIndex: Integer;
  Variable: TGlobalVariable;
  Format: TRbwDataType;
  ItemIndex: integer;
  OldNames: TStringList;
  NewNames: TStringList;
  OldName: TComponentName;
  NewName: string;
begin
  OldNames := TStringList.Create;
  NewNames := TStringList.Create;
  try
    ItemIndex := 0;
    for RowIndex := 1 to seGlobalVariableCount.AsInteger do
    begin
      OK:= True;
      for ColIndex := 0 to 1 do
      begin
        OK := OK and (rdgGlobalVariables.Cells[ColIndex,RowIndex] <> '');
      end;
      if OK then
      begin
        Format := TRbwDataType(rdgGlobalVariables.Columns[Ord(gvType)].
          PickList.IndexOf(rdgGlobalVariables.Cells[Ord(gvType),RowIndex]));
        if Format in [rdtDouble, rdtInteger] then
        begin
          Ok := (rdgGlobalVariables.Cells[Ord(gvValue),RowIndex] <> '')
        end;
        if Ok then
        begin
          Variable := FNewGlobals.Variables[ItemIndex];
          OldName := Variable.Name;
          NewName := rdgGlobalVariables.Cells[Ord(gvName),RowIndex];
          if (OldName <> NewName)
            and (OldName <> '')
            and (NewName <> '') then
          begin
            OldNames.Add(OldName);
            NewNames.Add(NewName);
          end;
          Variable.Name := NewName;
          Variable.Format := Format;
          case Variable.Format of
            rdtDouble:
              begin
                Variable.RealValue := StrToFloat(rdgGlobalVariables.
                  Cells[Ord(gvValue),RowIndex])
              end;
            rdtInteger:
              begin
                Variable.IntegerValue := StrToInt(rdgGlobalVariables.
                  Cells[Ord(gvValue),RowIndex])
              end;
            rdtBoolean:
              begin
                Variable.BooleanValue :=
                  rdgGlobalVariables.Checked[Ord(gvValue),RowIndex];
              end;
            rdtString:
              begin
                Variable.StringValue := rdgGlobalVariables.
                  Cells[Ord(gvValue),RowIndex];
              end;
            else Assert(False);
          end;
          Variable.Comment := rdgGlobalVariables.Cells[Ord(gvComment), RowIndex];
          Inc(ItemIndex);
        end
        else
        begin
          FNewGlobals.Delete(ItemIndex);
        end;
      end
      else
      begin
        FNewGlobals.Delete(ItemIndex);
      end;
    end;
    Undo := TUndoGlobalVariables.Create(FNewGlobals, OldNames, NewNames);
    frmGoPhast.UndoStack.Submit(Undo);
  finally
    NewNames.Free;
    OldNames.Free;
  end;
end;

procedure TfrmGlobalVariables.btnDeleteClick(Sender: TObject);
begin
  inherited;
  if (rdgGlobalVariables.Row > 0) then
  begin
    FNewGlobals.Delete(rdgGlobalVariables.Row-1);
    if seGlobalVariableCount.AsInteger > 1 then
    begin
      rdgGlobalVariables.DeleteRow(rdgGlobalVariables.Row);
    end;
    seGlobalVariableCount.AsInteger := seGlobalVariableCount.AsInteger-1;
  end;
end;

procedure TfrmGlobalVariables.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmGlobalVariables.FormCreate(Sender: TObject);
begin
  inherited;
  VariableNames := TStringList.Create;
  rdgGlobalVariables.Cells[0,0] := 'Name';
  rdgGlobalVariables.Cells[1,0] := 'Type';
  rdgGlobalVariables.Cells[2,0] := 'Value';
  rdgGlobalVariables.Cells[3,0] := 'Comment';
  FNewGlobals := TGlobalVariables.Create(nil);
  GetData;
end;

procedure TfrmGlobalVariables.FormDestroy(Sender: TObject);
begin
  inherited;
  FNewGlobals.Free;
  VariableNames.Free;
end;

function TfrmGlobalVariables.GenerateNewName(Root: string = 'NewGlobalVariable'; const
  CurrentRow: integer = -1): string;
var
  Names: TStringList;
  Index: integer;
begin
  Root := Trim(Root);
  if Root = '' then
  begin
    Root := 'NewGlobalVariable';
  end;

  // This function generates a name for a data set that is valid
  // and does not conflict with the names of any existing data sets.
  Names := TStringList.Create;
  try
    // The names of all the data sets are in rdgGlobalVariables.Cols[Ord(gvName)]
    Names.Assign(rdgGlobalVariables.Cols[Ord(gvName)]);
    // There may be names stored for rows that are no longer present.
    // Get rid of them.
    while Names.Count > rdgGlobalVariables.RowCount do
    begin
      Names.Delete(Names.Count - 1);
    end;
    if CurrentRow > 0 then
    begin
      Names.Delete(CurrentRow);
    end;
    // The name in the first row is a header for the names column
    // not the name of a data set so get rid of it.
    Names.Delete(0);
    // Get rid of any blanks.
    for Index := Names.Count - 1 downto 0 do
    begin
      if Names[Index] = '' then
      begin
        Names.Delete(Index);
      end;
    end;

    result := PhastModelUnit.GenerateNewName(Root, Names);
  finally
    Names.Free;
  end;
end;

procedure TfrmGlobalVariables.UpdateSpecialFormat(RowIndex: integer);
var
  Format: TRbwDataType;
  ItemIndex: integer;
begin
  rdgGlobalVariables.UseSpecialFormat[Ord(gvValue), RowIndex] := True;
  ItemIndex := rdgGlobalVariables.Columns[Ord(gvType)].Picklist.
      IndexOf(rdgGlobalVariables.Cells[Ord(gvType), RowIndex]);
  Format := TRbwDataType(ItemIndex);
  case Format of
    rdtDouble:
      begin
        rdgGlobalVariables.SpecialFormat[Ord(gvValue), RowIndex] := rcf4Real;
      end;
    rdtInteger:
      begin
        rdgGlobalVariables.SpecialFormat[Ord(gvValue), RowIndex] := rcf4Integer;
      end;
    rdtBoolean:
      begin
        rdgGlobalVariables.Cells[Ord(gvValue), RowIndex] := '';
        rdgGlobalVariables.SpecialFormat[Ord(gvValue), RowIndex] := rcf4Boolean;
      end;
    rdtString:
      begin
        rdgGlobalVariables.SpecialFormat[Ord(gvValue), RowIndex] := rcf4String;
      end;
    else Assert(False);
  end;
end;

function TfrmGlobalVariables.GenerateNewRoot(const Root: string): string;
begin
  result := Trim(Root);
  if result = '' then
  begin
    Exit;
  end;
  result := PhastModelUnit.GenerateNewRoot(result);
end;

procedure TfrmGlobalVariables.GetData;
var
  Index: Integer;
  GlobalVariable: TGlobalVariable;
  RowIndex: integer;
begin
  FNewGlobals.Assign(frmGoPhast.PhastModel.GlobalVariables);
  rdgGlobalVariables.BeginUpdate;
  try
    seGlobalVariableCount.AsInteger := frmGoPhast.PhastModel.GlobalVariables.Count;
    seGlobalVariableCountChange(nil);
    for Index := 0 to FNewGlobals.Count - 1 do
    begin
      RowIndex := Index+1;
      GlobalVariable := FNewGlobals[Index];
      rdgGlobalVariables.Cells[Ord(gvName), RowIndex] := GlobalVariable.Name;
      rdgGlobalVariables.Cells[Ord(gvType), RowIndex] :=
        rdgGlobalVariables.Columns[Ord(gvType)].PickList[Ord(GlobalVariable.Format)];
      UpdateSpecialFormat(RowIndex);
      case GlobalVariable.Format of
        rdtDouble:
          begin
            rdgGlobalVariables.Cells[Ord(gvValue), RowIndex] := FloatToStr(GlobalVariable.RealValue);
          end;
        rdtInteger:
          begin
            rdgGlobalVariables.Cells[Ord(gvValue), RowIndex] := IntToStr(GlobalVariable.IntegerValue);
          end;
        rdtBoolean:
          begin
            rdgGlobalVariables.Cells[Ord(gvValue), RowIndex] := '';
            rdgGlobalVariables.Checked[Ord(gvValue), RowIndex] := GlobalVariable.BooleanValue;
          end;
        rdtString:
          begin
            rdgGlobalVariables.Cells[Ord(gvValue), RowIndex] := GlobalVariable.StringValue;
          end;
        else Assert(False);
      end;
      rdgGlobalVariables.Cells[Ord(gvComment), RowIndex] := GlobalVariable.Comment;
    end;
  finally
    rdgGlobalVariables.EndUpdate;
  end;
end;

procedure TfrmGlobalVariables.InitializeNewRow(ARow: integer);
begin
  FInitializingRow := True;
  try
    rdgGlobalVariables.Cells[Ord(gvName),ARow] := GenerateNewName;
    rdgGlobalVariables.Cells[Ord(gvType),ARow] :=
      rdgGlobalVariables.Columns[Ord(gvType)].PickList[0];
    rdgGlobalVariables.Cells[Ord(gvValue),ARow] := '0';
    rdgGlobalVariables.UseSpecialFormat[Ord(gvValue),ARow] := True;
    rdgGlobalVariables.SpecialFormat[Ord(gvValue),ARow] := rcf4Real;
  finally
    FInitializingRow := False;
  end;
end;

{ TUndoGlobalVariables }

constructor TUndoGlobalVariables.Create(var NewGlobals: TGlobalVariables;
      var OldNames, NewNames: TStringList);
begin
  FNewGlobalVariables := NewGlobals;
  NewGlobals := nil;
  FOldGlobalVariables := TGlobalVariables.Create(nil);
  FOldGlobalVariables.Assign(frmGoPhast.PhastModel.GlobalVariables);
  FOldNames := OldNames;
  OldNames := nil;
  FNewNames := NewNames;
  NewNames := nil;
  FNewNames.CaseSensitive := False;
  FNewNames.Sort;
end;

function TUndoGlobalVariables.Description: string;
begin
  result := 'change global variables';
end;

destructor TUndoGlobalVariables.Destroy;
begin
  FOldNames.Free;
  FNewNames.Free;
  FNewGlobalVariables.Free;
  FOldGlobalVariables.Free;
  inherited;
end;

procedure TUndoGlobalVariables.DoCommand;
begin
  UpdateFormulas(FNewNames, FOldNames);
  frmGoPhast.PhastModel.GlobalVariables := FNewGlobalVariables;
  if FOldNames.Count > 0 then
  begin
    frmGoPhast.PhastModel.FormulaManager.RestoreSubscriptions;
  end;
end;

procedure TUndoGlobalVariables.Undo;
begin
  UpdateFormulas(FOldNames, FNewNames);
  frmGoPhast.PhastModel.GlobalVariables := FOldGlobalVariables;
  if FOldNames.Count > 0 then
  begin
    frmGoPhast.PhastModel.FormulaManager.RestoreSubscriptions;
  end;
end;

procedure TUndoGlobalVariables.UpdateFormulas(NewNames: TStringList; OldNames: TStringList);
//var
//  CompilerList: TList;
//  VarIndex: Integer;
//  VariableIndex: Integer;
//  Compiler: TRbwParser;
//  CompilerIndex: Integer;
begin
  Assert(OldNames.Count = NewNames.Count);
  if OldNames.Count > 0 then
  begin
    frmGoPhast.PhastModel.UpdateFormulas(OldNames, NewNames);
//    frmGoPhast.PhastModel.FormulaManager.RemoveSubscriptions(OldNames, NewNames);
//    CompilerList := TList.Create;
//    try
//      frmGoPhast.PhastModel.FillCompilerList(CompilerList);
//      for CompilerIndex := 0 to CompilerList.Count - 1 do
//      begin
//        Compiler := CompilerList[CompilerIndex];
//        for VariableIndex := 0 to OldNames.Count - 1 do
//        begin
//          VarIndex := Compiler.IndexOfVariable(OldNames[VariableIndex]);
//          Compiler.RenameVariable(VarIndex, NewNames[VariableIndex]);
//        end;
//      end;
//      frmGoPhast.PhastModel.FormulaManager.ResetFormulas;
//      for CompilerIndex := 0 to CompilerList.Count - 1 do
//      begin
//        Compiler := CompilerList[CompilerIndex];
//        for VariableIndex := 0 to OldNames.Count - 1 do
//        begin
//          VarIndex := Compiler.IndexOfVariable(NewNames[VariableIndex]);
//          Compiler.RenameVariable(VarIndex, OldNames[VariableIndex]);
//        end;
//      end;
//    finally
//      CompilerList.Free;
//    end;
  end;
end;

end.
