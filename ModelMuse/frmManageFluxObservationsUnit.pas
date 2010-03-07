unit frmManageFluxObservationsUnit;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  JvListBox, JvCtrls, JvComponent, JvExStdCtrls, JvExComCtrls, ComCtrls,
  JvComCtrls, frmCustomGoPhastUnit, Buttons, Mask, JvExMask, JvSpin, Grids,
  RbwDataGrid4, JvEdit, JvExExtCtrls, JvNetscapeSplitter, UndoItems,
  ModflowPackageSelectionUnit, FluxObservationUnit, RbwParser, JvCombobox,
  JvListComb, ArgusDataEntry;

type
  TFluxColumns = (fcName, fcTime, fcValue, fcStatistic, fcStatFlag, fcComment);
{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFDualLst.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of JvFDualLst.PAS at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

@author(Richard B. Winston <rbwinst@usgs.gov>)
}
  TfrmManageFluxObservations = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    pnlMain: TPanel;
    pcMain: TJvPageControl;
    tabObservationsTimes: TTabSheet;
    lblNumPeriods: TLabel;
    rdgFluxObsTimes: TRbwDataGrid4;
    seNumObsTimes: TJvSpinEdit;
    btnDelete: TButton;
    btnInsert: TButton;
    tabObjects: TTabSheet;
    SrcLabel: TLabel;
    DstLabel: TLabel;
    lblFactor: TLabel;
    SrcList: TJvListBox;
    IncBtn: TButton;
    IncAllBtn: TButton;
    ExclBtn: TButton;
    ExclAllBtn: TButton;
    DstList: TJvListBox;
    edFactorFormula: TJvEdit;
    btnFactorFormula: TButton;
    pnlTop: TPanel;
    edObservationName: TJvEdit;
    lblObservationName: TLabel;
    btnDeleteObservation: TButton;
    btnAddObservation: TButton;
    rparserThreeDFormulaElements: TRbwParser;
    rdeMultiValueEdit: TRbwDataEntry;
    comboMultiStatFlag: TJvImageComboBox;
    lblTreatment: TLabel;
    comboTreatment: TComboBox;
    tvFluxObservations: TTreeView;
    procedure IncBtnClick(Sender: TObject);
    procedure IncAllBtnClick(Sender: TObject);
    procedure ExclBtnClick(Sender: TObject);
    procedure ExclAllBtnClick(Sender: TObject);
    procedure SrcListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DstListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SrcListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DstListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SrcListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DstListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject); override;
    procedure ListClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnAddObservationClick(Sender: TObject);
    procedure btnDeleteObservationClick(Sender: TObject);
    procedure edObservationNameChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rdgFluxObsTimesExit(Sender: TObject);
    procedure seNumObsTimesChange(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure edFactorFormulaChange(Sender: TObject);
    procedure btnFactorFormulaClick(Sender: TObject);
    procedure edFactorFormulaExit(Sender: TObject);
    procedure rdgFluxObsTimesBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure rdgFluxObsTimesEndUpdate(Sender: TObject);
    procedure rdgFluxObsTimesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edObservationNameExit(Sender: TObject);
    procedure rdgFluxObsTimesHorizontalScroll(Sender: TObject);
    procedure tabObservationsTimesResize(Sender: TObject);
    procedure rdgFluxObsTimesColSize(Sender: TObject; ACol,
      PriorWidth: Integer);
    procedure rdeMultiValueEditChange(Sender: TObject);
    procedure comboMultiStatFlagChange(Sender: TObject);
    procedure rdgFluxObsTimesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure comboTreatmentChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvFluxObservationsChange(Sender: TObject; Node: TTreeNode);
  private
    FChobNode: TTreeNode;
    FChobObservations: TFluxObservationGroups;
    FSelectedGroup: TFluxObservationGroups;
    FSelectedObservation: TFluxObservationGroup;
    FChdScreenObjects: TList;
    FSettingObservation: Boolean;
    FUpdatingFormula: Boolean;
    FDrobObservations: TFluxObservationGroups;
    FDrnScreenObjects: TList;
    FGbobObservations: TFluxObservationGroups;
    FGhbScreenObjects: TList;
    FRvobObservations: TFluxObservationGroups;
    FRivScreenObjects: TList;
    FGbobNode: TTreeNode;
    FDrobNode: TTreeNode;
    FRvobNode: TTreeNode;
    FSettingTimeCount: Boolean;
    procedure SetSelectedObservation(const Value: TFluxObservationGroup);
    procedure AssignObsNames;
    procedure DisplayFactor;
    procedure UpdateFactor;
    procedure CreateVariables;
    property SelectedObservation: TFluxObservationGroup
      read FSelectedObservation write SetSelectedObservation;
    procedure GetData;
    procedure SetData;
    procedure ReadFluxObservations(Package: TModflowPackageSelection;
      FluxObservations: TFluxObservationGroups; var ParentNode: TTreeNode);
    procedure UpdateObjectsInSelectedObservation;
    procedure GetGlobalVariables;
    function CheckFormula(FunctionString: string; ShowError: boolean): boolean;
    procedure AssignFactor(NewFormula: string);
    procedure InitializeFirstRow;
    procedure CheckErrors;
    procedure LayoutMultiFluxEdits;
    procedure AssignValuesToSelectedGridCells(const NewText: string;
      Grid: TRbwDataGrid4; const StartCol, EndCol: integer);
    procedure EnableMultiEditControl(Grid: TRbwDataGrid4; AControl: TControl;
      const StartCol, EndCol: integer);
    procedure SetSelectedGroupAndObservation(TreeView: TTreeView);
    procedure HideUcodeColumns;
  public
    procedure SetButtons;
  end;

implementation

uses
  JvBoxProcs, frmGoPhastUnit, ScreenObjectUnit, Math, GIS_Functions, 
  GoPhastTypes, DataSetUnit, frmFormulaUnit, frmErrorsAndWarningsUnit;

{$R *.dfm}

var
  FPriorErrors: TStringList;
const
  // MODFLOW-2005 allows observation names to be up to 12 characters in length.
  // UCODE allows names up to a length of 20.
  MaxObservationNameLength = 12;

procedure TfrmManageFluxObservations.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := (SrcList.Items.Count = 0);
  DstEmpty := (DstList.Items.Count = 0);
  IncBtn.Enabled := not SrcEmpty and (SrcList.SelCount > 0);
  IncAllBtn.Enabled := not SrcEmpty;
  ExclBtn.Enabled := not DstEmpty and (DstList.SelCount > 0);
  ExclAllBtn.Enabled := not DstEmpty;
end;

procedure TfrmManageFluxObservations.HideUcodeColumns;
begin
  if not frmGoPhast.ShowUcodeInterface then
  begin
    rdgFluxObsTimes.ColWidths[Ord(fcStatistic)] := 0;
    rdgFluxObsTimes.ColWidths[Ord(fcStatFlag)] := 0;
  end;
end;

procedure TfrmManageFluxObservations.SetSelectedGroupAndObservation(TreeView: TTreeView);
begin
  btnDeleteObservation.Enabled := (TreeView.Selected <> nil) and (TreeView.Selected.Data <> FChobObservations) and (TreeView.Selected.Data <> FDrobObservations) and (TreeView.Selected.Data <> FGbobObservations) and (TreeView.Selected.Data <> FRvobObservations);
  rdgFluxObsTimesExit(nil);
  if (TreeView.Selected = nil) then
  begin
    SelectedObservation := nil;
    FSelectedGroup := nil;
  end
  else if (TreeView.Selected = FChobNode) then
  begin
    SelectedObservation := nil;
    FSelectedGroup := FChobObservations;
  end
  else if (TreeView.Selected = FDrobNode) then
  begin
    SelectedObservation := nil;
    FSelectedGroup := FDrobObservations;
  end
  else if (TreeView.Selected = FGbobNode) then
  begin
    SelectedObservation := nil;
    FSelectedGroup := FGbobObservations;
  end
  else if (TreeView.Selected = FRvobNode) then
  begin
    SelectedObservation := nil;
    FSelectedGroup := FRvobObservations;
  end
  else
  begin
    SelectedObservation := TreeView.Selected.Data;
    FSelectedGroup := TreeView.Selected.Parent.Data;
  end;
  DisplayFactor;
end;

procedure TfrmManageFluxObservations.LayoutMultiFluxEdits;
var
  AColVisible: Boolean;
  Index: Integer;
begin
  if (rdgFluxObsTimes = nil) or (rdeMultiValueEdit = nil)
     or (comboMultiStatFlag = nil) then
  begin
    Exit;
  end;
  AColVisible := False;
  for Index := Ord(fcTime) to Ord(fcStatFlag) do
  begin
    if not frmGoPhast.ShowUcodeInterface
      and (TFluxColumns(Index) in [fcStatistic, fcStatFlag]) then
    begin
      break;
    end;
    if rdgFluxObsTimes.ColVisible[Index] then
    begin
      LayoutControls(rdgFluxObsTimes, rdeMultiValueEdit, nil, Index,
        rdgFluxObsTimes.Margins.Left);
      rdeMultiValueEdit.Width := rdgFluxObsTimes.ColWidths[Index];
      AColVisible := True;
      break;
    end;
  end;
  if not AColVisible then
  begin
    rdeMultiValueEdit.Visible := False;
//    LayoutControls(rdgFluxObsTimes, rdeMultiValueEdit, nil, 0,
//      rdgFluxObsTimes.Margins.Left);
  end;
  LayoutControls(rdgFluxObsTimes, comboMultiStatFlag, nil, Ord(fcStatFlag),
    rdgFluxObsTimes.Margins.Left);
  comboMultiStatFlag.Width := rdgFluxObsTimes.ColWidths[Ord(fcStatFlag)];
  HideUcodeColumns;
end;

procedure TfrmManageFluxObservations.CheckErrors;
var
  ErrorIndex: Integer;
  ErrorMessages: TStringList;
  ErrorRoots: TStringList;
  Index: Integer;
begin
  for Index := 0 to FPriorErrors.Count - 1 do
  begin
    frmErrorsAndWarnings.RemoveErrorGroup(FPriorErrors[Index]);
  end;
  FPriorErrors.Clear;

  ErrorRoots := TStringList.Create;
  ErrorMessages := TStringList.Create;
  try
    FChobObservations.CheckObservationTimes(ErrorRoots,
      ErrorMessages, fkConstantHead);
    FDrobObservations.CheckObservationTimes(ErrorRoots,
      ErrorMessages, fkDrain);
    FGbobObservations.CheckObservationTimes(ErrorRoots,
      ErrorMessages, fkGeneralHead);
    FRvobObservations.CheckObservationTimes(ErrorRoots,
      ErrorMessages, fkRiver);
    Assert(ErrorRoots.Count = ErrorMessages.Count);
    for ErrorIndex := 0 to ErrorRoots.Count - 1 do
    begin
      FPriorErrors.Add(ErrorRoots[ErrorIndex]);
      frmErrorsAndWarnings.AddError(ErrorRoots[ErrorIndex],
        ErrorMessages[ErrorIndex]);
    end;
    if ErrorRoots.Count > 0 then
    begin
      frmErrorsAndWarnings.ShowAfterDelay;
    end;
  finally
    ErrorRoots.Free;
    ErrorMessages.Free;
  end;
end;

procedure TfrmManageFluxObservations.InitializeFirstRow;
begin
  if seNumObsTimes.AsInteger = 0 then
  begin
    rdgFluxObsTimes.Cells[Ord(fcName), 1] := '';
    rdgFluxObsTimes.Cells[Ord(fcTime), 1] := '';
    rdgFluxObsTimes.Cells[Ord(fcValue), 1] := '';
    rdgFluxObsTimes.Cells[Ord(fcStatistic), 1] := '';
    rdgFluxObsTimes.Cells[Ord(fcStatFlag), 1] := '';
    rdgFluxObsTimes.Cells[Ord(fcComment), 1] := '';

    rdgFluxObsTimes.Objects[Ord(fcName), 1] := nil;
  end;
end;

procedure TfrmManageFluxObservations.AssignFactor(NewFormula: string);
var
  FactorObject: TObservationFactor;
  ObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Index: Integer;
begin
  for Index := 0 to DstList.Items.Count - 1 do
  begin
    if DstList.Selected[Index] then
    begin
      ScreenObject := DstList.Items.Objects[Index] as TScreenObject;
      ObjectIndex := FSelectedObservation.ObservationFactors.
        IndexOfScreenObject(ScreenObject);
      if (ObjectIndex >= 0) then
      begin
        FactorObject := FSelectedObservation.ObservationFactors[ObjectIndex];
        FactorObject.Factor := NewFormula;
      end;
    end;
  end;
end;

function TfrmManageFluxObservations.CheckFormula(FunctionString: string;
  ShowError: boolean): boolean;
var
  CompiledFormula: TExpression;
begin
  result := True;
  try
    rparserThreeDFormulaElements.Compile(FunctionString);
  except on E: ErbwParserError do
    begin
      edFactorFormula.Color := clRed;
      if ShowError then
      begin
        Beep;
        MessageDlg('Error in formula: ' + E.Message, mtError, [mbOK], 0);
      end;
      result := False;
      Exit;
    end
  end;

  CompiledFormula := rparserThreeDFormulaElements.CurrentExpression;
  // check that the formula is OK.
  if not (CompiledFormula.ResultType in [rdtDouble, rdtInteger]) then
  begin
    edFactorFormula.Color := clRed;
    if ShowError then
    begin
      Beep;
      MessageDlg('Error: the formula is does not result in a real number',
        mtError, [mbOK], 0);
    end;
    result := False;
  end
  else
  begin
    edFactorFormula.Color := clWindow;
    if ShowError then
    begin
      FunctionString := CompiledFormula.Decompile;
      if FunctionString <> edFactorFormula.Text then
      begin
        edFactorFormula.Text := FunctionString;
        if Assigned(edFactorFormula.OnChange) then
        begin
          edFactorFormula.OnChange(edFactorFormula);
        end;
        if Assigned(edFactorFormula.OnExit) then
        begin
          edFactorFormula.OnExit(edFactorFormula);
        end;
      end;
    end;
  end;
end;

procedure TfrmManageFluxObservations.comboMultiStatFlagChange(Sender: TObject);
begin
  inherited;
  AssignValuesToSelectedGridCells(comboMultiStatFlag.Text, rdgFluxObsTimes,
    Ord(fcStatFlag), Ord(fcStatFlag));
end;

procedure TfrmManageFluxObservations.comboTreatmentChange(Sender: TObject);
begin
  inherited;
  if (FSelectedObservation <> nil) then
  begin
    FSelectedObservation.Purpose :=
      TObservationPurpose(comboTreatment.ItemIndex);
  end;
end;

procedure TfrmManageFluxObservations.ReadFluxObservations(
  Package: TModflowPackageSelection; FluxObservations: TFluxObservationGroups;
  var ParentNode: TTreeNode);
var
  Index: Integer;
  Item: TFluxObservationGroup;
  ANode: TTreeNode;
begin
  if Package.IsSelected then
  begin
    ParentNode := tvFluxObservations.Items.Add(nil, Package.PackageIdentifier);
    ParentNode.Data := FluxObservations;
    for Index := 0 to FluxObservations.Count - 1 do
    begin
      Item := FluxObservations[Index];
      ANode := tvFluxObservations.Items.AddChild(ParentNode,
        Item.ObservationName);
      ANode.Data := Item;
    end;
  end
  else
  begin
    ParentNode := nil;
  end;
end;

procedure TfrmManageFluxObservations.seNumObsTimesChange(Sender: TObject);
var
  Index: Integer;
  ObsTime: TFluxObservation;
  NewRowCount: Integer;
begin
  inherited;
  if FSettingTimeCount then
  begin
    Exit;
  end;
  FSettingTimeCount := True;
  try
    if seNumObsTimes.AsInteger > 0 then
    begin
      edObservationName.MaxLength :=
        MaxObservationNameLength -2 - Trunc(Log10(seNumObsTimes.AsInteger));
      if Length(edObservationName.Text) > edObservationName.MaxLength then
      begin
        edObservationName.Text := Copy(edObservationName.Text, 1,
          edObservationName.MaxLength);
        edObservationNameChange(nil);
      end;
    end
    else
    begin
      edObservationName.MaxLength := MaxObservationNameLength-2;
    end;
    rdgFluxObsTimes.Enabled := seNumObsTimes.Enabled;
    rdgFluxObsTimes.Invalidate;
    NewRowCount := Max(1, seNumObsTimes.AsInteger) + 1;
    if NewRowCount < rdgFluxObsTimes.RowCount then
    begin
      for Index := NewRowCount to rdgFluxObsTimes.RowCount - 1 do
      begin
        rdgFluxObsTimes.Cells[Ord(fcName),Index] := '';
        rdgFluxObsTimes.Cells[Ord(fcTime),Index] := '';
        rdgFluxObsTimes.Cells[Ord(fcValue),Index] := '';
        rdgFluxObsTimes.Cells[Ord(fcStatistic),Index] := '';
        rdgFluxObsTimes.Cells[Ord(fcStatFlag),Index] := '';
        rdgFluxObsTimes.Cells[Ord(fcComment),Index] := '';
        rdgFluxObsTimes.Objects[Ord(fcName),Index] := nil;
      end;
    end;
    if seNumObsTimes.AsInteger = 0 then
    begin
      rdgFluxObsTimes.Cells[Ord(fcName),1] := '';
      rdgFluxObsTimes.Cells[Ord(fcTime),1] := '';
      rdgFluxObsTimes.Cells[Ord(fcValue),1] := '';
      rdgFluxObsTimes.Cells[Ord(fcStatistic),1] := '';
      rdgFluxObsTimes.Cells[Ord(fcStatFlag),1] := '';
      rdgFluxObsTimes.Cells[Ord(fcComment),1] := '';
      rdgFluxObsTimes.Objects[Ord(fcName),1] := nil;
    end;
    rdgFluxObsTimes.RowCount := NewRowCount;
    btnDelete.Enabled := seNumObsTimes.Enabled
      and (seNumObsTimes.AsInteger >= 1);
    if not FSettingObservation and (FSelectedObservation <> nil) then
    begin
      for Index := 1 to seNumObsTimes.AsInteger do
      begin
        if rdgFluxObsTimes.Objects[Ord(fcName),Index] = nil then
        begin
          ObsTime := FSelectedObservation.ObservationTimes.Add;
          ObsTime.Index := Index-1;
          rdgFluxObsTimes.Objects[Ord(fcName),Index] := ObsTime;
          if rdgFluxObsTimes.Cells[Ord(fcTime),Index] = '' then
          begin
            ObsTime.Time := 0;
            rdgFluxObsTimes.Cells[Ord(fcTime),Index] := '0';
          end
          else
          begin
            ObsTime.Time := StrToFloat(rdgFluxObsTimes.Cells[Ord(fcTime),Index]);
          end;

          if rdgFluxObsTimes.Cells[Ord(fcValue),Index] = '' then
          begin
            ObsTime.ObservedValue := 0;
            rdgFluxObsTimes.Cells[Ord(fcValue),Index] := '0';
          end
          else
          begin
            ObsTime.ObservedValue := StrToFloat(rdgFluxObsTimes.Cells[Ord(fcValue),Index]);
          end;

          if rdgFluxObsTimes.Cells[Ord(fcStatistic),Index] = '' then
          begin
            ObsTime.Statistic := 0;
            rdgFluxObsTimes.Cells[Ord(fcStatistic),Index] := '0';
          end
          else
          begin
            ObsTime.Statistic := StrToFloat(rdgFluxObsTimes.Cells[Ord(fcStatistic),Index]);
          end;
          if rdgFluxObsTimes.Cells[Ord(fcStatFlag),Index] = '' then
          begin
            ObsTime.StatFlag := Low(TStatFlag);
            rdgFluxObsTimes.Cells[Ord(fcStatFlag),Index] :=
              rdgFluxObsTimes.Columns[Ord(fcStatFlag)].PickList[0];
          end
          else
          begin
            ObsTime.StatFlag := TStatFlag(rdgFluxObsTimes.Columns[Ord(fcStatFlag)].
              PickList.IndexOf(rdgFluxObsTimes.Cells[Ord(fcStatFlag),Index]));
          end;
          if rdgFluxObsTimes.Cells[Ord(fcComment),Index] = '' then
          begin
            ObsTime.Comment := '';
            rdgFluxObsTimes.Cells[Ord(fcComment),Index] := '';
          end
          else
          begin
            ObsTime.Comment := rdgFluxObsTimes.Cells[Ord(fcComment),Index];
          end;
        end;
      end;
      for Index := FSelectedObservation.ObservationTimes.Count-1
        downto seNumObsTimes.AsInteger do
      begin
        FSelectedObservation.ObservationTimes.Delete(Index);
      end;
      InitializeFirstRow;
    end;
    AssignObsNames;
  finally
    FSettingTimeCount := False;
  end;
  HideUcodeColumns;
end;

procedure TfrmManageFluxObservations.SetData;
var
  Undo: TUndoEditFluxObservations;
begin
  Undo := TUndoEditFluxObservations.Create;
  try
    Undo.AssignNewObservations(FChobObservations, FDrobObservations,
      FGbobObservations, FRvobObservations);
  except
    Undo.Free;
    raise;
  end;
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmManageFluxObservations.SetSelectedObservation(
  const Value: TFluxObservationGroup);
var
  Index: Integer;
  ScreenObject: TScreenObject;
  CurrentObjects: TList;
  AvailableList: TList;
  ObsTime: TFluxObservation;
  TimeString: string;
  MaxTimeStringLength: Integer;
begin
  edFactorFormulaExit(nil);
  if (FSelectedObservation <> Value) or (Value = nil) then
  begin
    FSettingObservation := True;
    try
      FSelectedObservation := Value;
      SrcList.Items.Clear;
      DstList.Items.Clear;
      if FSelectedObservation = nil then
      begin
        edObservationName.Enabled := False;
        comboTreatment.Enabled := False;
        SrcList.Enabled := False;
        DstList.Enabled := False;
        IncBtn.Enabled := False;
        IncAllBtn.Enabled := False;
        ExclBtn.Enabled := False;
        ExclAllBtn.Enabled := False;
        rdgFluxObsTimes.Enabled := False;
        rdgFluxObsTimes.Invalidate;
        seNumObsTimes.Enabled := False;
        btnInsert.Enabled := False;
        btnDelete.Enabled := False;
        rdgFluxObsTimes.Enabled := False;
        for Index := 1 to rdgFluxObsTimes.RowCount - 1 do
        begin
          rdgFluxObsTimes.Objects[Ord(fcName),Index] := nil;
        end;
        seNumObsTimes.AsInteger := 0;
      end
      else
      begin
        rdgFluxObsTimes.Enabled := True;
        edObservationName.Enabled := True;
        comboTreatment.Enabled := True;
        edObservationName.Text := FSelectedObservation.ObservationName;
        comboTreatment.ItemIndex := Ord(FSelectedObservation.Purpose);

        seNumObsTimes.Enabled := True;
        btnInsert.Enabled := True;
        SrcList.Enabled := True;
        DstList.Enabled := True;
        IncBtn.Enabled := True;
        IncAllBtn.Enabled := True;
        ExclBtn.Enabled := True;
        ExclAllBtn.Enabled := True;

        CurrentObjects := TList.Create;
        try
          for Index := 0 to FSelectedObservation.
            ObservationFactors.Count - 1 do
          begin
            ScreenObject := FSelectedObservation.ObservationFactors[Index].
              ScreenObject as TScreenObject;
            CurrentObjects.Add(ScreenObject);
          end;

          AvailableList := nil;
          if tvFluxObservations.Selected.Parent = FChobNode then
          begin
            AvailableList := FChdScreenObjects;
          end
          else if tvFluxObservations.Selected.Parent = FDrobNode then
          begin
            AvailableList := FDrnScreenObjects;
          end
          else if tvFluxObservations.Selected.Parent = FGbobNode then
          begin
            AvailableList := FGhbScreenObjects;
          end
          else if tvFluxObservations.Selected.Parent = FRvobNode then
          begin
            AvailableList := FRivScreenObjects;
          end
          else
          begin
            Assert(False);
          end;

          for Index := 0 to AvailableList.Count - 1 do
          begin
            ScreenObject := AvailableList[Index];
            if CurrentObjects.IndexOf(ScreenObject) >= 0 then
            begin
              DstList.Items.AddObject(ScreenObject.Name, ScreenObject);
              if ScreenObject.Selected then
              begin
                DstList.Selected[DstList.Count -1] := True;
              end;
            end
            else
            begin
              SrcList.Items.AddObject(ScreenObject.Name, ScreenObject);
              if ScreenObject.Selected then
              begin
                SrcList.Selected[SrcList.Count -1] := True;
              end;
            end;
          end;
        finally
          CurrentObjects.Free;
        end;

        for Index := 1 to rdgFluxObsTimes.RowCount - 1 do
        begin
          rdgFluxObsTimes.Objects[Ord(fcName),Index] := nil;
        end;
        seNumObsTimes.AsInteger := FSelectedObservation.ObservationTimes.Count;
        InitializeFirstRow;
        MaxTimeStringLength := Length(IntToStr(FSelectedObservation.ObservationTimes.Count));
        for Index := 0 to FSelectedObservation.ObservationTimes.Count - 1 do
        begin
          ObsTime := FSelectedObservation.ObservationTimes[Index];
          TimeString := IntToStr(Index+1);
          While Length(TimeString) < MaxTimeStringLength do
          begin
            TimeString := '0' + TimeString;
          end;
          rdgFluxObsTimes.Cells[Ord(fcName),Index+1] :=
            FSelectedObservation.ObservationName + '_' + TimeString;
          rdgFluxObsTimes.Cells[Ord(fcTime),Index+1] :=
            FloatToStr(ObsTime.Time);
          rdgFluxObsTimes.Cells[Ord(fcValue),Index+1] :=
            FloatToStr(ObsTime.ObservedValue);
          rdgFluxObsTimes.Cells[Ord(fcStatistic),Index+1] :=
            FloatToStr(ObsTime.Statistic);
          rdgFluxObsTimes.Cells[Ord(fcStatFlag),Index+1] :=
            rdgFluxObsTimes.Columns[Ord(fcStatFlag)].
            PickList[Ord(ObsTime.StatFlag)];
          rdgFluxObsTimes.Cells[Ord(fcComment),Index+1] :=
            ObsTime.Comment;
          rdgFluxObsTimes.Objects[Ord(fcName),Index+1] := ObsTime;
        end;
      end;
    finally
      FSettingObservation := False;
    end;
  end;
end;

procedure TfrmManageFluxObservations.IncBtnClick(Sender: TObject);
begin
  BoxMoveSelectedItems(SrcList, DstList);
  SetButtons;
  UpdateObjectsInSelectedObservation;
end;

procedure TfrmManageFluxObservations.IncAllBtnClick(Sender: TObject);
begin
  BoxMoveAllItems(SrcList, DstList);
  SetButtons;
  UpdateObjectsInSelectedObservation;
end;

procedure TfrmManageFluxObservations.ExclBtnClick(Sender: TObject);
begin
  BoxMoveSelectedItems(DstList, SrcList);
  SetButtons;
  UpdateObjectsInSelectedObservation;
end;

procedure TfrmManageFluxObservations.EnableMultiEditControl(Grid: TRbwDataGrid4;
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

procedure TfrmManageFluxObservations.ExclAllBtnClick(Sender: TObject);
begin
  BoxMoveAllItems(DstList, SrcList);
  SetButtons;
  UpdateObjectsInSelectedObservation;
end;

procedure TfrmManageFluxObservations.SrcListDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(SrcList, Source, X, Y, State, Accept, SrcList.Sorted);
  if State = dsDragLeave then
    (Source as TJvListBox).DragCursor := crDrag;
  if (State = dsDragEnter) and ((Source as TJvListBox).SelCount > 1) then
    (Source as TJvListBox).DragCursor := crMultiDrag;
end;

procedure TfrmManageFluxObservations.DstListDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(DstList, Source, X, Y, State, Accept, DstList.Sorted);
  if State = dsDragLeave then
    (Source as TJvListBox).DragCursor := crDrag;
  if (State = dsDragEnter) and ((Source as TJvListBox).SelCount > 1) then
    (Source as TJvListBox).DragCursor := crMultiDrag;
end;

procedure TfrmManageFluxObservations.SrcListDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  if Source = DstList then
    ExclBtnClick(SrcList)
  else
  if Source = SrcList then
  begin
    BoxMoveFocusedItem(SrcList, SrcList.ItemAtPos(Point(X, Y), True));
    UpdateObjectsInSelectedObservation;
  end;
end;

procedure TfrmManageFluxObservations.btnAddObservationClick(Sender: TObject);
var
  Observations: TFluxObservationGroups;
  ObservationGroup: TFluxObservationGroup;
  ANode: TTreeNode;
  ObsName: string;
  ParentNode : TTreeNode;
begin
  inherited;
  if tvFluxObservations.Selected = nil then
  begin
    if FChobNode <> nil then
    begin
      tvFluxObservations.Selected := FChobNode;
    end
    else if FGbobNode <> nil then
    begin
      tvFluxObservations.Selected := FGbobNode;
    end
    else if FDrobNode <> nil then
    begin
      tvFluxObservations.Selected := FDrobNode;
    end
    else if FRvobNode <> nil then
    begin
      tvFluxObservations.Selected := FRvobNode;
    end
  end;
  Assert(tvFluxObservations.Selected <> nil);
  if tvFluxObservations.Selected = FChobNode then
  begin
    ParentNode := tvFluxObservations.Selected;
  end
  else if tvFluxObservations.Selected = FGbobNode then
  begin
    ParentNode := tvFluxObservations.Selected;
  end
  else if tvFluxObservations.Selected = FDrobNode then
  begin
    ParentNode := tvFluxObservations.Selected;
  end
  else if tvFluxObservations.Selected = FRvobNode then
  begin
    ParentNode := tvFluxObservations.Selected;
  end
  else
  begin
    ParentNode := tvFluxObservations.Selected.Parent;
  end;

  if ParentNode = FChobNode then
  begin
    ObsName := 'Chob';
  end
  else if ParentNode = FGbobNode then
  begin
    ObsName := 'Gbob';
  end
  else if ParentNode = FDrobNode then
  begin
    ObsName := 'Drob';
  end
  else if ParentNode = FRvobNode then
  begin
    ObsName := 'Rvob';
  end
  else
  begin
    Assert(False);
  end;

  Observations := ParentNode.Data;
  ObservationGroup := Observations.Add;
  ANode := tvFluxObservations.Items.AddChild(ParentNode,
    ObservationGroup.ObservationName);
  ObservationGroup.ObservationName := ObsName
    + IntToStr(ParentNode.Count);
  ANode.Data := ObservationGroup;
  tvFluxObservations.Selected := ANode;
  SetSelectedGroupAndObservation(tvFluxObservations);

  HideUcodeColumns;
end;

procedure TfrmManageFluxObservations.btnDeleteClick(Sender: TObject);
begin
  inherited;
  if (rdgFluxObsTimes.SelectedRow >= 1)
    and (rdgFluxObsTimes.SelectedRow <= rdgFluxObsTimes.RowCount)then
  begin
    if seNumObsTimes.AsInteger > 1 then
    begin
      FSelectedObservation.ObservationTimes.
        Delete(rdgFluxObsTimes.SelectedRow-1);
      rdgFluxObsTimes.DeleteRow(rdgFluxObsTimes.SelectedRow);
    end
    else
    begin
      if rdgFluxObsTimes.Objects[Ord(fcName), rdgFluxObsTimes.SelectedRow] <> nil then
      begin
        FSelectedObservation.ObservationTimes.
          Delete(rdgFluxObsTimes.SelectedRow-1);
        rdgFluxObsTimes.Objects[Ord(fcName), rdgFluxObsTimes.SelectedRow] := nil;
      end;
    end;
    seNumObsTimes.AsInteger := seNumObsTimes.AsInteger -1;
    AssignObsNames;
  end;
  HideUcodeColumns;
end;

procedure TfrmManageFluxObservations.btnDeleteObservationClick(Sender: TObject);
var
  ParentNode: TTreeNode;
  Observations: TFluxObservationGroups;
  Item: TFluxObservationGroup;
  Index: Integer;
begin
  inherited;
  Assert((tvFluxObservations.Selected <> nil)
    and (tvFluxObservations.Selected <> FChobNode)
    and (tvFluxObservations.Selected <> FGbobNode)
    and (tvFluxObservations.Selected <> FDrobNode)
    and (tvFluxObservations.Selected <> FRvobNode));
  Item := tvFluxObservations.Selected.Data;
  ParentNode := tvFluxObservations.Selected.Parent;
  Assert(ParentNode <> nil);
  Observations := ParentNode.Data;
  Observations.Remove(Item);
  for Index := 1 to rdgFluxObsTimes.RowCount - 1 do
  begin
    rdgFluxObsTimes.Objects[Ord(fcName),Index] := nil;
  end;
  tvFluxObservations.Items.Delete(tvFluxObservations.Selected);
  SelectedObservation := nil;
  HideUcodeColumns;
end;

procedure TfrmManageFluxObservations.btnFactorFormulaClick(Sender: TObject);
var
  FirstScreenObject: TScreenObject;
  Index: Integer;
  Variable: TCustomValue;
  FunctionString: string;
  ObjectIndex: Integer;
  FactorObject: TObservationFactor;
begin
  inherited;
  FirstScreenObject := nil;
  for Index := 0 to DstList.Items.Count - 1 do
  begin
    if DstList.Selected[Index] then
    begin
      FirstScreenObject := DstList.Items.Objects[Index] as TScreenObject;
      break;
    end;
  end;
  if FirstScreenObject = nil then
  begin
    Exit;
  end;

  FunctionString := edFactorFormula.Text;
  if FunctionString = '' then
  begin
    ObjectIndex := FSelectedObservation.ObservationFactors.
      IndexOfScreenObject(FirstScreenObject);
    Assert(ObjectIndex >= 0);
    FactorObject := FSelectedObservation.ObservationFactors[ObjectIndex];
    FunctionString := FactorObject.Factor;
  end;

  with TfrmFormula.Create(nil) do
  begin
    try
      IncludeGIS_Functions;
      RemoveGetVCont;
      RemoveHufFunctions;
      PopupParent := self;

      for Index := 0 to rparserThreeDFormulaElements.VariableCount - 1 do
      begin
        Variable := rparserThreeDFormulaElements.Variables[Index];
        if rbFormulaParser.IndexOfVariable(Variable.Name) < 0 then
        begin
          rbFormulaParser.RegisterVariable(Variable);
        end;
      end;

      UpdateTreeList;
      Formula := FunctionString;

      ShowModal;
      if ResultSet then
      begin
        FunctionString := Formula;
      end
      else
      begin
        if FunctionString = '' then
          FunctionString := '0';
      end;
    finally
      Free;
    end;
  end;

  CheckFormula(FunctionString, True)
end;

procedure TfrmManageFluxObservations.AssignObsNames;
var
  Index: Integer;
  MaxStringLength: Integer;
  TimeString: string;
begin
  MaxStringLength := Length(IntToStr(seNumObsTimes.AsInteger));
  for Index := 1 to seNumObsTimes.AsInteger do
  begin
    TimeString := IntToStr(Index);
    While Length(TimeString) < MaxStringLength do
    begin
      TimeString := '0' + TimeString;
    end;
    rdgFluxObsTimes.Cells[Ord(fcName), Index] :=
      edObservationName.Text + '_' + TimeString;
  end;
  if seNumObsTimes.AsInteger = 0 then
  begin
    rdgFluxObsTimes.Cells[Ord(fcName), 1] := '';
  end;
end;

procedure TfrmManageFluxObservations.AssignValuesToSelectedGridCells(
  const NewText: string; Grid: TRbwDataGrid4; const StartCol, EndCol: integer);
var
  ColIndex: Integer;
  RowIndex: Integer;
  TempText: string;
begin
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

procedure TfrmManageFluxObservations.btnInsertClick(Sender: TObject);
begin
  inherited;
  if (rdgFluxObsTimes.SelectedRow >= 1)
    and (rdgFluxObsTimes.SelectedRow <= rdgFluxObsTimes.RowCount)then
  begin
    rdgFluxObsTimes.InsertRow(rdgFluxObsTimes.SelectedRow);
    seNumObsTimes.AsInteger := seNumObsTimes.AsInteger + 1;
  end;
  seNumObsTimesChange(Sender);
end;

procedure TfrmManageFluxObservations.DisplayFactor;
var
  FirstFormula: string;
  FoundFormula: boolean;
  Index: Integer;
  ScreenObject: TScreenObject;
  ObjectIndex: integer;
  FactorObject: TObservationFactor;
begin
  edFactorFormula.Enabled := DstList.Enabled and (DstList.SelCount > 0);
  btnFactorFormula.Enabled := edFactorFormula.Enabled;
  if edFactorFormula.Enabled then
  begin
    FoundFormula := False;
    FirstFormula := '';
    for Index := 0 to DstList.Items.Count - 1 do
    begin
      if DstList.Selected[Index] then
      begin
        ScreenObject := DstList.Items.Objects[Index] as TScreenObject;
        ObjectIndex := FSelectedObservation.ObservationFactors.
          IndexOfScreenObject(ScreenObject);
        Assert(ObjectIndex >= 0);
        FactorObject := FSelectedObservation.ObservationFactors[ObjectIndex];
        if FoundFormula then
        begin
          if FirstFormula <> FactorObject.Factor then
          begin
            FirstFormula := '';
          end;
        end
        else
        begin
          FirstFormula := FactorObject.Factor;
          FoundFormula := True;
        end;
      end;
    end;
    edFactorFormula.Text := FirstFormula;
  end;
end;

procedure TfrmManageFluxObservations.DstListDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  if Source = SrcList then
    IncBtnClick(DstList)
  else
  if Source = DstList then
  begin
    BoxMoveFocusedItem(DstList, DstList.ItemAtPos(Point(X, Y), True));
    UpdateObjectsInSelectedObservation;
  end;
end;

procedure TfrmManageFluxObservations.SrcListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  Incr: Integer;
begin
  if not SrcList.Sorted then
  begin
    if (ssCtrl in Shift) and ((Key = VK_DOWN) or (Key = VK_UP)) then
    begin
      if Key = VK_DOWN then
        Incr := 1
      else
        Incr := -1;
      BoxMoveFocusedItem(SrcList, SrcList.ItemIndex + Incr);
      Key := 0;
      UpdateObjectsInSelectedObservation;
    end;
  end;
end;

procedure TfrmManageFluxObservations.tabObservationsTimesResize(
  Sender: TObject);
begin
  inherited;
  LayoutMultiFluxEdits;
end;

procedure TfrmManageFluxObservations.tvFluxObservationsChange(Sender: TObject;
  Node: TTreeNode);
begin
  inherited;
  SetSelectedGroupAndObservation(tvFluxObservations);
  HideUcodeColumns;
end;

procedure TfrmManageFluxObservations.UpdateObjectsInSelectedObservation;
var
  Index: Integer;
  ScreenObject: TScreenObject;
  DestinationList: TList;
begin
  DestinationList := TList.Create;
  try
    for Index := 0 to DstList.Items.Count - 1 do
    begin
      DestinationList.Add(DstList.Items.Objects[Index]);
    end;

    for Index := FSelectedObservation.ObservationFactors.Count - 1 downto 0 do
    begin
      ScreenObject := FSelectedObservation.
        ObservationFactors.Items[Index].ScreenObject as TScreenObject;
      if DestinationList.IndexOf(ScreenObject) < 0 then
      begin
        FSelectedObservation.ObservationFactors.Delete(Index);
      end;
    end;

    for Index := 0 to DestinationList.Count - 1 do
    begin
      ScreenObject := DestinationList[Index];
      FSelectedObservation.AddObject(ScreenObject);
    end;
  finally
    DestinationList.Free;
  end;
  DisplayFactor;
end;

procedure TfrmManageFluxObservations.DstListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  Incr: Integer;
begin
  if not DstList.Sorted then
  begin
    if (ssCtrl in Shift) and ((Key = VK_DOWN) or (Key = VK_UP)) then
    begin
      if Key = VK_DOWN then
        Incr := 1
      else
        Incr := -1;
      BoxMoveFocusedItem(DstList, DstList.ItemIndex + Incr);
      UpdateObjectsInSelectedObservation;
      Key := 0;
    end;
  end;
end;

procedure TfrmManageFluxObservations.UpdateFactor;
var
  NewFormula: string;
begin
  if FUpdatingFormula then Exit;
  FUpdatingFormula := True;
  try
    NewFormula := edFactorFormula.Text;
    if (NewFormula = '') or not CheckFormula(NewFormula, False) then
    begin
      Exit;
    end;
    NewFormula := edFactorFormula.Text;
    AssignFactor(NewFormula);
  finally
    FUpdatingFormula := False;
  end;
end;

procedure TfrmManageFluxObservations.edFactorFormulaChange(Sender: TObject);
begin
  inherited;
  UpdateFactor;
end;

procedure TfrmManageFluxObservations.edFactorFormulaExit(Sender: TObject);
var
  NewFormula: string;
begin
  if FUpdatingFormula then Exit;
  FUpdatingFormula := True;
  try
    NewFormula := edFactorFormula.Text;
    if (NewFormula = '') or not CheckFormula(NewFormula, True) then
    begin
      Exit;
    end;
    NewFormula := edFactorFormula.Text;
    AssignFactor(NewFormula);
  finally
    FUpdatingFormula := False;
  end;
end;

procedure TfrmManageFluxObservations.edObservationNameChange(Sender: TObject);
begin
  inherited;
  if (FSelectedObservation <> nil) then
  begin
    Assert(tvFluxObservations.Selected.Data = FSelectedObservation);
    FSelectedObservation.ObservationName := edObservationName.Text;
    tvFluxObservations.Selected.Text := edObservationName.Text;
    AssignObsNames;
  end;
end;

procedure TfrmManageFluxObservations.edObservationNameExit(Sender: TObject);
begin
  inherited;
  edObservationName.Text := StringReplace(edObservationName.Text, ' ', '_', [rfReplaceAll]);
end;

procedure TfrmManageFluxObservations.CreateVariables;
var
  Index: Integer;
  DataArray: TDataArray;
begin
  for Index := 0 to frmGoPhast.PhastModel.DataSetCount - 1 do
  begin
    DataArray := frmGoPhast.PhastModel.DataSets[Index];
    if DataArray.EvaluatedAt = eaBlocks then
    begin
      case DataArray.DataType of
        rdtDouble: rparserThreeDFormulaElements.CreateVariable(DataArray.Name,
          DataArray.FullClassification, 0.0);
        rdtInteger: rparserThreeDFormulaElements.CreateVariable(DataArray.Name,
          DataArray.FullClassification, 0);
        rdtBoolean: rparserThreeDFormulaElements.CreateVariable(DataArray.Name,
          DataArray.FullClassification, False);
        rdtString: rparserThreeDFormulaElements.CreateVariable(DataArray.Name,
          DataArray.FullClassification, '');
      end;
    end;
  end;
end;

procedure TfrmManageFluxObservations.FormCreate(Sender: TObject);
begin
  FPriorErrors.Sorted := True;
  FPriorErrors.Duplicates := dupIgnore;

  FChobObservations := TFluxObservationGroups.Create(nil);
  FChdScreenObjects := TList.Create;

  FDrobObservations := TFluxObservationGroups.Create(nil);
  FDrnScreenObjects := TList.Create;

  FGbobObservations := TFluxObservationGroups.Create(nil);
  FGhbScreenObjects := TList.Create;

  FRvobObservations := TFluxObservationGroups.Create(nil);
  FRivScreenObjects := TList.Create;

  rdgFluxObsTimes.Cells[Ord(fcName),0] := 'Name';
  rdgFluxObsTimes.Cells[Ord(fcTime),0] := 'Time';
  rdgFluxObsTimes.Cells[Ord(fcValue),0] := 'Observed value';
  rdgFluxObsTimes.Cells[Ord(fcStatistic),0] := 'Statistic';
  rdgFluxObsTimes.Cells[Ord(fcStatFlag),0] := 'StatFlag';
  rdgFluxObsTimes.Cells[Ord(fcComment),0] := 'Comment';

  LayoutMultiFluxEdits;

  AddGIS_Functions(rparserThreeDFormulaElements,
    frmGoPhast.PhastModel.ModelSelection);
  GetGlobalVariables;
  CreateVariables;

  GetData;

  HideUcodeColumns;
end;

procedure TfrmManageFluxObservations.FormDestroy(Sender: TObject);
begin
  inherited;
  FChdScreenObjects.Free;
  FChobObservations.Free;
  FDrnScreenObjects.Free;
  FDrobObservations.Free;
  FGhbScreenObjects.Free;
  FGbobObservations.Free;
  FRivScreenObjects.Free;
  FRvobObservations.Free;
end;

procedure TfrmManageFluxObservations.FormResize(Sender: TObject);
begin
  { Delphi 5, 6, 7 and 2005 compatible code }
  IncBtn.Left := 4 + (tabObjects.Width - IncBtn.Width) div 2;
  IncAllBtn.Left := IncBtn.Left;
  ExclBtn.Left := IncBtn.Left;
  ExclAllBtn.Left := IncBtn.Left;
  SrcList.Width := (tabObjects.Width - (8 + 7 + IncBtn.Width + 7 + 8)) div 2;
  SrcLabel.Left := SrcList.Left;
  DstList.Width := SrcList.Width;
  DstList.Left := IncBtn.Left + IncBtn.Width + 7;
  DstLabel.Left := DstList.Left;
end;

procedure TfrmManageFluxObservations.FormShow(Sender: TObject);
begin
  inherited;
  ListClick(nil);
  rdgFluxObsTimes.Options := rdgFluxObsTimes.Options - [goEditing];
  rdgFluxObsTimes.Options := rdgFluxObsTimes.Options + [goEditing];

//  rdgFluxObsTimes.hi
end;

procedure TfrmManageFluxObservations.GetData;
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    if (ScreenObject.ModflowChdBoundary <> nil)
      and ScreenObject.ModflowChdBoundary.Used then
    begin
      FChdScreenObjects.Add(ScreenObject);
    end;
    if (ScreenObject.ModflowDrnBoundary <> nil)
      and ScreenObject.ModflowDrnBoundary.Used then
    begin
      FDrnScreenObjects.Add(ScreenObject);
    end;
    if (ScreenObject.ModflowGhbBoundary <> nil)
      and ScreenObject.ModflowGhbBoundary.Used then
    begin
      FGhbScreenObjects.Add(ScreenObject);
    end;
    if (ScreenObject.ModflowRivBoundary <> nil)
      and ScreenObject.ModflowRivBoundary.Used then
    begin
      FRivScreenObjects.Add(ScreenObject);
    end;
  end;

  FChobObservations.Assign(frmGoPhast.PhastModel.HeadFluxObservations);
  ReadFluxObservations(frmGoPhast.PhastModel.ModflowPackages.ChobPackage,
    FChobObservations, FChobNode);

  FDrobObservations.Assign(frmGoPhast.PhastModel.DrainObservations);
  ReadFluxObservations(frmGoPhast.PhastModel.ModflowPackages.DrobPackage,
    FDrobObservations, FDrobNode);

  FGbobObservations.Assign(frmGoPhast.PhastModel.GhbObservations);
  ReadFluxObservations(frmGoPhast.PhastModel.ModflowPackages.GbobPackage,
    FGbobObservations, FGbobNode);

  FRvobObservations.Assign(frmGoPhast.PhastModel.RiverObservations);
  ReadFluxObservations(frmGoPhast.PhastModel.ModflowPackages.RvobPackage,
    FRvobObservations, FRvobNode);

  SelectedObservation := nil;
end;

procedure TfrmManageFluxObservations.GetGlobalVariables;
var
  CompilerList: TList;
begin
  CompilerList := TList.Create;
  try
    CompilerList.Add(rparserThreeDFormulaElements);
    frmGoPhast.PhastModel.RefreshGlobalVariables(CompilerList);
  finally
    CompilerList.Free;
  end;
end;

procedure TfrmManageFluxObservations.ListClick(Sender: TObject);
begin
  SetButtons;
  DisplayFactor;
  HideUcodeColumns;
end;

procedure TfrmManageFluxObservations.OkBtnClick(Sender: TObject);
begin
  inherited;
  CheckErrors;
  SetData;
end;

procedure TfrmManageFluxObservations.rdeMultiValueEditChange(Sender: TObject);
begin
  inherited;
  AssignValuesToSelectedGridCells(rdeMultiValueEdit.Text, rdgFluxObsTimes,
    Ord(fcTime), Ord(fcStatistic));
end;

procedure TfrmManageFluxObservations.rdgFluxObsTimesBeforeDrawCell(
  Sender: TObject; ACol, ARow: Integer);
begin
  inherited;
  if not rdgFluxObsTimes.Enabled then
  begin
    rdgFluxObsTimes.Canvas.Brush.Color := clBtnFace;
  end;
  
end;

procedure TfrmManageFluxObservations.rdgFluxObsTimesColSize(Sender: TObject;
  ACol, PriorWidth: Integer);
begin
  inherited;
  LayoutMultiFluxEdits;
end;

procedure TfrmManageFluxObservations.rdgFluxObsTimesEndUpdate(Sender: TObject);
begin
  inherited;
  if not FSettingTimeCount then
  begin
    seNumObsTimes.AsInteger := rdgFluxObsTimes.RowCount -1;
    HideUcodeColumns;
  end;
end;

procedure TfrmManageFluxObservations.rdgFluxObsTimesExit(Sender: TObject);
var
  Index: Integer;
  ObsTime: TFluxObservation;
begin
  inherited;

  for Index := 1 to seNumObsTimes.AsInteger do
  begin
    if Index < rdgFluxObsTimes.RowCount then
    begin
      ObsTime := rdgFluxObsTimes.Objects[Ord(fcName),Index] as TFluxObservation;
      if ObsTime <> nil then
      begin
        ObsTime.Time := StrToFloat(rdgFluxObsTimes.Cells[Ord(fcTime),Index]);
        ObsTime.ObservedValue := StrToFloat(rdgFluxObsTimes.Cells[Ord(fcValue),Index]);
        ObsTime.Statistic := StrToFloat(rdgFluxObsTimes.Cells[Ord(fcStatistic),Index]);
        ObsTime.StatFlag := TStatFlag(rdgFluxObsTimes.Columns[Ord(fcStatFlag)].
          PickList.IndexOf(rdgFluxObsTimes.Cells[Ord(fcStatFlag),Index]));
        ObsTime.Comment := rdgFluxObsTimes.Cells[Ord(fcComment),Index];
      end;
    end;
  end;
end;

procedure TfrmManageFluxObservations.rdgFluxObsTimesHorizontalScroll(
  Sender: TObject);
begin
  inherited;
  LayoutMultiFluxEdits;
end;

procedure TfrmManageFluxObservations.rdgFluxObsTimesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ([ssShift, ssCtrl] * Shift) = [] then
  begin
    rdgFluxObsTimes.Options := rdgFluxObsTimes.Options + [goEditing];
  end
  else
  begin
    rdgFluxObsTimes.Options := rdgFluxObsTimes.Options - [goEditing];
  end;

end;

procedure TfrmManageFluxObservations.rdgFluxObsTimesSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  EnableMultiEditControl(rdgFluxObsTimes, rdeMultiValueEdit,
    Ord(fcTime), Ord(fcStatistic));
  EnableMultiEditControl(rdgFluxObsTimes, comboMultiStatFlag,
    Ord(fcStatFlag), Ord(fcStatFlag));
end;

initialization
  FPriorErrors:= TStringList.Create;

finalization
  FPriorErrors.Free;

end.
