{
May 22, 2006: introduced CustomizeControls.
}

{@abstract(The purpose of @name is to declare @link(TfrmCustomGoPhast),
  the ancestor of all TForms in GoPhast.)}
unit frmCustomGoPhastUnit;

interface

uses
  CommDlg, RbwDataGrid4, Spin, Windows, Forms, SysUtils, Types, Classes,
  Graphics, Controls, Dialogs, StdCtrls, Grids, HtmlHelpViewer, ImageDLLLoader,
  ICOLoader, JPEGLoader, PNGLoader, HIPSLoader, BMPLoader, PCXLoader, WMFLoader,
  LinarBitmap, FileUtils, ehshelprouter, JvSpin, VirtualTrees, DataSetUnit,
  ClassificationUnit, TntExDropDownVirtualStringTree;

type
  // @name is used in @link(TfrmCustomGoPhast.AdjustFormPosition)
  // to tell whether the form
  // should be to the right or to the left of the main form.
  TDesiredPosition = (dpLeft, dpRight, dpBottomLeft, dpBottomRight);

  {@abstract(@name is the ancestor of all TForms in GoPhast.)
   @name handles setting the color and font. @name also tries
   to keep from appearing at a location where it can't be seen.
   It also provides the @link(TfrmCustomGoPhast.ShowAForm) method. )}
  TfrmCustomGoPhast = class(TForm)
    // @name is the event handler for OnCreate.
    // It calls @link(CustomizeControls).
    procedure FormCreate(Sender: TObject); virtual;
    // @name is the eventhandler for OnShow.
    // It moves the form to try to get it completely on the screen
    // and calls @link(SetAppearance).
    procedure FormShow(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    function FormHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
  private
    function CallHelpRouter: boolean;
  protected
    {@name adjusts the position of Form to the left or right of the main form,
     if possible.  If there isn't room for it at the desired position it will
     try the other side.  If there isn't room there either, it will have it overlap
     the form on the desired side.  A side effect is that it sets Form.Position to
     poDesigned.}
    procedure AdjustFormPosition(DesiredPosition: TDesiredPosition);
    // @name checks that at least one of the TCheckBoxes in CheckBoxArray is
    // checked.  If not the color of all of them is set to clRed and all
    // have their fonts changed to bold.
    procedure EmphasizeCheckBoxes(const CheckBoxArray: array of TCheckBox);
    // @name checks if @link(GlobalFont) is nil. If not, it sets the
    // color, font, and icon used in the form
    // to be @link(GlobalFont), @link(GlobalColor) and Application.Icon.
    // @link(GlobalFont) and @link(GlobalColor) should be set in the
    // Applications OnCreate event handler.
    procedure SetAppearance;
    // @name changes the color of all TSpinEdits on the form depending
    // on whether or not the TSpinEdit is enabled or not.
//    procedure SetSpinColor;
  public
    procedure UpdateSubComponents(AComponent: TComponent);
    // @name changes all the cells in Grid that are in Column and that
    // are selected to NewText.
    procedure ChangeSelectedCellsInColumn(Grid: TRbwDataGrid4;
      const Column: integer; const NewText: string);
    // @name changes all the checked stated of all cells in Grid that
    // are in Column and that are selected to NewState.
    procedure ChangeSelectedCellsStateInColumn(Grid: TRbwDataGrid4;
      const Column: integer; const NewState: TCheckBoxState);
    // @name checks all the cells in Grid that are in Col.  If at least one
    // such cell is selected, AControl will be enabled. Otherwise, it will be
    // disabled.
    procedure EnableMultiEditControl(Grid: TRbwDataGrid4; AControl: TControl;
      Col: integer);
    // If the Application.MainForm has been assigned,
    // @name sets the color, font, and icon used in the form
    // to be the same as in Application.MainForm via a call to
    // @link(SetAppearance).
    procedure CustomizeControls;
    // @name creates an instance of FormClass and calls ShowModal
    // on that instance.
    function ShowAForm(const FormClass: TFormClass): integer;
  { Public declarations }
  end;

// @name sets the Left and Width of Control and ALabel so that they are
// lined up over Column in Grid. ALabel.Alignment should be taCenter.
procedure LayoutControls(Grid: TRbwDataGrid4; Control: TControl;
  ALabel: TLabel; Column: Integer; ControlOffSet: integer = 0);

Procedure UpdateDialogBoxFileName(Dialog: TOpenDialog; NewFileName: string);

Procedure FillVirtualStringTreeWithDataSets(Tree : TVirtualStringTree;
  ClassificationObjectOwnerList: TList; SelectedDataArray: TDataArray);

procedure FillDataSetLists(HufDataArrays: TClassificationList;
  LayerGroupList: TClassificationList;
  ClassificationObjects: TClassificationList;
  ClassificationObjectOwnerList: TList);
  
procedure UpdateTreeComboText(SelectedNode: PVirtualNode;
  TreeCombo: TTntExDropDownVirtualStringTree);

procedure GetNodeCaption(Node: PVirtualNode; var CellText: WideString;
  Sender: TBaseVirtualTree);

procedure SelectOnlyLeaves(Node: PVirtualNode;
  TreeCombo: TTntExDropDownVirtualStringTree; Sender: TBaseVirtualTree;
  var SelectedNode: PVirtualNode);

var
  GlobalFont: TFont = nil;
  GlobalColor: TColor = clBtnFace;
  HelpRouter: THelpRouter;


implementation

uses frmGoPhastUnit, PhastModelUnit;

{$R *.dfm}

type
  TStringGridCrack = class(TStringGrid);

Procedure UpdateDialogBoxFileName(Dialog: TOpenDialog; NewFileName: string);
const
  CB_FILENAME_ID = 1148;
begin
  SendMessage( GetParent(Dialog.Handle), CDM_SETCONTROLTEXT,
    CB_FILENAME_ID, LongInt(Pchar(ExtractFileName(NewFileName))));
end;

procedure TfrmCustomGoPhast.CustomizeControls;
begin
  if (Application.MainForm <> nil) and (self <> Application.MainForm) then
  begin
    SetAppearance;
  end;
end;

procedure TfrmCustomGoPhast.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
//  HelpKeyWord := 'files\' + HelpKeyWord + '.htm';
end;

function TfrmCustomGoPhast.CallHelpRouter: boolean;
var
  KeyWord: string;
  HelpControl: TControl;
begin
  KeyWord := HelpKeyword;
  HelpControl := ActiveControl;
  while (HelpControl <> nil)
    and (HelpControl <> self) do
  begin
    if HelpControl.HelpKeyword <> '' then
    begin
      KeyWord := HelpControl.HelpKeyword;
      break;
    end
    else
    begin
      HelpControl := HelpControl.Parent;
    end;
  end;
  result := HelpRouter.HelpJump('', KeyWord);
end;

function TfrmCustomGoPhast.FormHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  if (Command in [HELP_CONTEXT, HELP_INDEX, HELP_FORCEFILE, HH_DISPLAY_SEARCH, 15])
    {or (Command = HELP_COMMAND)} then
  begin
    // 15 = help contents.
    result := False;
    
  end
  else
  begin
    result := CallHelpRouter;
    CallHelp := False;
  end;
end;

procedure TfrmCustomGoPhast.btnHelpClick(Sender: TObject);
begin
  inherited;
  CallHelpRouter;
end;


function TfrmCustomGoPhast.ShowAForm(const FormClass: TFormClass): integer;
begin
  with FormClass.Create(nil) do
  begin
    try
      if ModalResult = mrNone then
      begin
        ShowModal;
      end;
      result :=ModalResult;
    finally
      Free;
    end;
  end;
end;

procedure TfrmCustomGoPhast.UpdateSubComponents(AComponent: TComponent);
var
  ComponentIndex: Integer;
  Component: TComponent;
  AStringGrid: TStringGrid;
  RowHeight: Integer;
  Grid: TCustomRBWDataGrid;
  ColGrid: TRbwDataGrid4;
  Index: Integer;
  RowGrid: TRbwRowDataGrid;
begin
  for ComponentIndex := 0 to AComponent.ComponentCount - 1 do
  begin
    Component := AComponent.Components[ComponentIndex];
    if Component is TStringGrid then
    begin
      AStringGrid := TStringGrid(Component);
      AStringGrid.Font := Font;
      // This is a work-around for a bug in CLX.
      TStringGridCrack(AStringGrid).ShowEditor;
      AStringGrid.Font.Size := AStringGrid.Font.Size + 1;
      AStringGrid.Font.Size := AStringGrid.Font.Size - 1;
      // Make sure the rows are tall enough to show the text.
      RowHeight := AStringGrid.Canvas.TextHeight('0');
      if RowHeight > AStringGrid.DefaultRowHeight then
      begin
        AStringGrid.DefaultRowHeight := RowHeight;
      end;
      AStringGrid.FixedColor := Color;
    end;
    if Component is TCustomRBWDataGrid then
    begin
      Grid := TCustomRBWDataGrid(Component);
      Grid.UnselectableColor := Color;
      if Grid is TRbwDataGrid4 then
      begin
        ColGrid := TRbwDataGrid4(Grid);
        for Index := 0 to ColGrid.ColCount - 1 do
        begin
          ColGrid.Columns[Index].ButtonFont := Font;
        end;
      end
      else
      begin
        RowGrid := Grid as TRbwRowDataGrid;
        for Index := 0 to RowGrid.RowCount - 1 do
        begin
          RowGrid.Rows[Index].ButtonFont := Font;
        end;
      end;
    end;
    UpdateSubComponents(Component);
  end;
end;

procedure TfrmCustomGoPhast.SetAppearance;
var
  AComponent: TComponent;
begin
  if GlobalFont <> nil then
  begin
    Font := GlobalFont;
    Color := GlobalColor;
//    Icon := Application.Icon;
    AComponent := self;
    UpdateSubComponents(AComponent);
  end;
end;

procedure TfrmCustomGoPhast.FormShow(Sender: TObject);
var
  // According to the documentation, the height of the form should
  // take into account the boder, menu, caption, etc.  However that
  // doesn't work correctly so call QWidget_frameGeometry to get the
  // correct form height.
  FrameHeight: integer;
  FrameWidth: integer;
  WorkAreaRect: TRect;
begin
{$IFDEF LINUX}
  //if not QWidget_isVisible(Widget) then Exit;
{$ENDIF}

{$IFDEF MSWINDOWS}
  // Because the CLX Screen.Height doesn't take the taskbar into account,
  // use the VCL Screen object instead under windows to determine the available
  // space on the screen.
  WorkAreaRect := Forms.Screen.WorkAreaRect;
{$ELSE}
{$IFDEF LINUX}
  // With Linux, the screen area and the available work area are the same.
  // Use the CLX screen object to determine them.
  WorkAreaRect.Top := 0;
  WorkAreaRect.Left := 0;
  WorkAreaRect.Right := Screen.Width;
  WorkAreaRect.Bottom := Screen.Height;
{$ELSE}
  Assert(False);
{$ENDIF}
{$ENDIF}

//  QWidget_frameGeometry(Handle, @AFrameGeom);

//  FrameHeight := AFrameGeom.Bottom - AFrameGeom.Top;
//  FrameWidth := AFrameGeom.Right - AFrameGeom.Left;
  FrameHeight := Height;
  FrameWidth := Width;

  // Try to make sure the form is never off the screen.
  // However, this does not resize the form to fit it on the screen.
  // If the form is wider or taller than the screen, some of it will
  // be off the screen.
  if Left + FrameWidth > WorkAreaRect.Right then
  begin
    Left := WorkAreaRect.Right - FrameWidth;
  end;
  if Top + FrameHeight > WorkAreaRect.Bottom then
  begin
    Top := WorkAreaRect.Bottom - FrameHeight;
  end;
  if Left < WorkAreaRect.Left then
  begin
    Left := WorkAreaRect.Left;
  end;
  if Top < WorkAreaRect.Top then
  begin
    Top := WorkAreaRect.Top;
  end;
  CustomizeControls;
end;

procedure TfrmCustomGoPhast.AdjustFormPosition(DesiredPosition: TDesiredPosition);
var
  NewLeft: integer;
  NewTop: integer;
  WorkAreaRect: TRect;
  FormHeight: integer;
  FormWidth: integer;
  MainFormWidth: integer;
  WorkAreaWidth: integer;
  WorkAreaHeight: integer;
  function RoomToLeft: boolean;
  begin
    result := Application.MainForm.Left - FormWidth >= 0;
  end;
  function RoomToRight: boolean;
  begin
    result := Application.MainForm.Left + MainFormWidth
      + FormWidth <= WorkAreaWidth;
  end;
begin
  Assert(Application.MainForm <> nil);
  
{$IFDEF LINUX}
  //if not QWidget_isVisible(Widget) then Exit;
{$ENDIF}

{$IFDEF MSWINDOWS}
  // Because the CLX Screen.Height doesn't take the taskbar into account,
  // use the VCL Screen object instead under windows to determine the available
  // space on the screen.
  WorkAreaRect := Application.MainForm.Monitor.WorkareaRect;
{$ELSE}
{$IFDEF LINUX}
  // With Linux, the screen area and the available work area are the same.
  // Use the CLX screen object to determine them.
  WorkAreaRect.Top := 0;
  WorkAreaRect.Left := 0;
  WorkAreaRect.Right := Screen.Width;
  WorkAreaRect.Bottom := Screen.Height;
{$ELSE}
  Assert(False);
{$ENDIF}
{$ENDIF}
  WorkAreaWidth := WorkAreaRect.Right - WorkAreaRect.Left;
  WorkAreaHeight := WorkAreaRect.Bottom - WorkAreaRect.Top;

  // The height of the form is the same as the Client height (a bug).
  // We need the full height instead.
//  QWidget_frameGeometry(Handle, @FormGeom);

//  FormHeight := FormGeom.Bottom - FormGeom.Top;
//  FormWidth := FormGeom.Right - FormGeom.Left;
  FormHeight := Height;
  FormWidth := Width;

//  QWidget_frameGeometry(Application.MainForm.Handle, @FormGeom);

//  MainFormHeight := FormGeom.Bottom - FormGeom.Top;
//  MainFormWidth := FormGeom.Right - FormGeom.Left;
//  MainFormHeight := Application.MainForm.Height;
  MainFormWidth := Application.MainForm.Width;

  // Change the desired position if there isn't enough room on the desired size
  // but there is enough room on the opposite side.
  case DesiredPosition of
    dpLeft:
      begin
        if not RoomToLeft and RoomToRight then
        begin
          DesiredPosition := dpRight;
        end;
      end;
    dpRight:
      begin
        if not RoomToRight and RoomToLeft then
        begin
          DesiredPosition := dpLeft;
        end;
      end;
    dpBottomLeft:
      begin
        if not RoomToLeft and RoomToRight then
        begin
          DesiredPosition := dpBottomRight;
        end;
      end;
    dpBottomRight:
      begin
        if not RoomToRight and RoomToLeft then
        begin
          DesiredPosition := dpBottomLeft;
        end;
      end;
  else
    Assert(False);
  end;

  // Get the new left coordinate of the form.
  NewLeft := 0;
  case DesiredPosition of
    dpLeft, dpBottomLeft:
      begin
        NewLeft := Application.MainForm.Left - FormWidth - 10;
        if NewLeft < 0 then
        begin
          NewLeft := 0;
        end;
      end;
    dpRight, dpBottomRight:
      begin
        NewLeft := Application.MainForm.Left + MainFormWidth + 10;
        if NewLeft + FormWidth > WorkAreaWidth then
        begin
          NewLeft := WorkAreaWidth - FormWidth;
        end;
      end;
    else
      Assert(False);
  end;

  NewTop := 0;
  case DesiredPosition of
    dpLeft, dpRight:
      begin
        NewTop := Application.MainForm.Top;
      end;
    dpBottomLeft, dpBottomRight:
      begin
        NewTop := Application.MainForm.Top
          + Application.MainForm.Height
          - FormHeight;
      end;
    else Assert(False);
  end;
  // Get the new top coordinate of the form.
  if NewTop + FormHeight > WorkAreaHeight then
  begin
    NewTop := WorkAreaHeight - FormHeight;
  end;

  if NewTop < 0 then
  begin
    NewTop := 0;
  end;

  // Set the forms position.
  Position := poDesigned;
  Left := NewLeft;
  Top := NewTop;
end;

procedure TfrmCustomGoPhast.EmphasizeCheckBoxes(
  const CheckBoxArray: array of TCheckBox);
var
  Index: integer;
  AnyChecked: boolean;
begin
  for Index := 0 to Length(CheckBoxArray) -1 do
  begin
    CheckBoxArray[Index].ParentFont := True;
  end;
  AnyChecked := False;
  for Index := 0 to Length(CheckBoxArray) -1 do
  begin
    AnyChecked := (CheckBoxArray[Index].State in [cbChecked, cbGrayed]);
    if AnyChecked then
    begin
      break;
    end;
  end;
  if not AnyChecked then
  begin
    for Index := 0 to Length(CheckBoxArray) -1 do
    begin
      CheckBoxArray[Index].Font.Color := clRed;
      CheckBoxArray[Index].Font.Style
        := CheckBoxArray[Index].Font.Style + [fsBold];
    end;
  end;
end;

//procedure TfrmCustomGoPhast.btnHelpClick(Sender: TObject);
//begin
//  inherited;
//  Application.HelpJump(HelpKeyword);
//end;

//procedure TfrmCustomGoPhast.SetSpinColor;
//var
//  Index: integer;
//  SpinEdit: TSpinEdit;
//begin
//  for Index := 0 to ComponentCount - 1 do
//  begin
//    if Components[Index] is TSpinEdit then
//    begin
//      SpinEdit := TSpinEdit(Components[Index]);
//      if SpinEdit.Enabled then
//      begin
//        SpinEdit.Color := clWindow;
//      end
//      else
//      begin
//        SpinEdit.Color := clBtnFace; 
//      end;
//    end;
//  end;
//end;

procedure LayoutControls(Grid: TRbwDataGrid4; Control: TControl;
      ALabel: TLabel; Column: Integer; ControlOffSet: integer = 0);
var
  Rect: TRect;
begin
  if Grid = nil then
  begin
    Exit;
  end;
  if (Control = nil) and (ALabel = nil) then
  begin
    Exit;
  end;
  Rect := Grid.CellRect(Column, 0);
  if (Rect.Left = 0) and (Rect.Right = 0) then
  begin
    if Control <> nil then
    begin
      Control.Visible := False;
    end;
    if ALabel <> nil then
    begin
      ALabel.Visible := False;
    end;
  end
  else
  begin
    if Control <> nil then
    begin
      Control.Visible := True;
    end;
    if ALabel <> nil then
    begin
      ALabel.Visible := True;
    end;
  end;
  if Control <> nil then
  begin
    Control.Left := Rect.Left + ControlOffSet;
    Control.Width := Grid.ColWidths[Column] - ControlOffSet;
  end;
  if ALabel <> nil then
  begin
    ALabel.Left := Rect.Left;
    ALabel.Width := Grid.ColWidths[Column];
  end;
end;

procedure TfrmCustomGoPhast.EnableMultiEditControl(Grid: TRbwDataGrid4;
  AControl: TControl; Col: integer);
var
  ShouldEnable: boolean;
  Index: Integer;
begin
  ShouldEnable := False;
  for Index := Grid.FixedRows to Grid.RowCount -1 do
  begin
    ShouldEnable := Grid.IsSelectedCell(Col,Index);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  AControl.Enabled := ShouldEnable;
end;

procedure TfrmCustomGoPhast.ChangeSelectedCellsInColumn(Grid: TRbwDataGrid4;
  const Column: integer; const NewText: string);
var
  Index: Integer;
  TempOptions: Grids.TGridOptions;
begin
  if Grid = nil then
  begin
    Exit;
  end;
  for Index := Grid.FixedRows to Grid.RowCount - 1 do
  begin
    if Grid.IsSelectedCell(Column, Index) then
    begin
      Grid.Cells[Column, Index] := NewText;
      if Assigned(Grid.OnSetEditText) then
      begin
        Grid.OnSetEditText(Grid,Column,Index, NewText);
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
end;

procedure TfrmCustomGoPhast.ChangeSelectedCellsStateInColumn(
  Grid: TRbwDataGrid4; const Column: integer; const NewState: TCheckBoxState);
var
  Index: Integer;
begin
  if Grid = nil then
  begin
    Exit;
  end;
  for Index := Grid.FixedRows to Grid.RowCount - 1 do
  begin
    if Grid.IsSelectedCell(Column, Index) then
    begin
      Grid.State[Column, Index] := NewState;
    end;
  end;
end;

procedure FillDataSetLists(HufDataArrays: TClassificationList;
  LayerGroupList: TClassificationList;
  ClassificationObjects: TClassificationList;
  ClassificationObjectOwnerList: TList);
var
  Index: Integer;
  DataSet: TDataArray;
  ClassificationObject: TDataSetClassification;
  Position: Integer;
  HydrogeologicUnitNames: TStringList;
  LayerGroupsDataSets: TList;
begin
  LayerGroupsDataSets := TList.Create;
  HydrogeologicUnitNames := TStringList.Create;
  try
    // HufDataArrays will be filled with TDataSetClassifications for the
    // TDataArrays used to define HUF data.
    frmGoPhast.PhastModel.HydrogeologicUnits.
      FillDataArrayNames(HydrogeologicUnitNames);
    HydrogeologicUnitNames.CaseSensitive := False;
    for Index := 0 to HydrogeologicUnitNames.Count - 1 do
    begin
      HufDataArrays.Add(nil);
    end;
    // LayerGroupList will be filled with TDataSetClassifications for the
    // TDataArrays used to define the layer geometry.
    frmGoPhast.PhastModel.GetLayerGroupDataSets(LayerGroupsDataSets);
    for Index := 0 to LayerGroupsDataSets.Count - 1 do
    begin
      LayerGroupList.Add(nil);
    end;
    // DataSetClassificationList will be filled with TDataSetClassifications
    // for the all the TDataArrays
    for Index := 0 to frmGoPhast.PhastModel.DataSetCount - 1 do
    begin
      DataSet := frmGoPhast.PhastModel.DataSets[Index];
      ClassificationObject := TDataSetClassification.Create(DataSet);
      ClassificationObjects.Add(ClassificationObject);
      ClassificationObjectOwnerList.Add(ClassificationObject);
      Position := LayerGroupsDataSets.IndexOf(DataSet);
      if Position >= 0 then
      begin
        LayerGroupList[Position] := ClassificationObject;
      end;
      Position := HydrogeologicUnitNames.IndexOf(DataSet.Name);
      if Position >= 0 then
      begin
        HufDataArrays[Position] := ClassificationObject;
      end;
    end;
  finally
    LayerGroupsDataSets.Free;
    HydrogeologicUnitNames.Free;
  end;
end;

Procedure FillVirtualStringTreeWithDataSets(Tree : TVirtualStringTree;
  ClassificationObjectOwnerList: TList; SelectedDataArray: TDataArray);
var
  ClassificationList: TStringList;
  ClassificationObjects: TClassificationList;
  LayerGroupList: TClassificationList;
  SelectedName: string;
  HufDataArrays: TClassificationList;
begin

  { TODO : Nearly the same code is use in TfrmFormulaUnit, TFrmGridColor,
  TfrmScreenObjectProperties, and TfrmDataSets. Find a way to combine them. }

  // ClassificationObjectOwnerList will be filled with ClassificationObjects
  // for all the TDataArrays.
  ClassificationObjectOwnerList.Clear;

  // get the name of the selected TDataArray.
  if SelectedDataArray = nil then
  begin
    SelectedName := '';
  end
  else
  begin
    SelectedName := SelectedDataArray.Name;
  end;

  // Create lists used for sorting the nodes.
  HufDataArrays := TClassificationList.Create;
  ClassificationObjects:= TClassificationList.Create;
  LayerGroupList := TClassificationList.Create;
  try
    FillDataSetLists(HufDataArrays,
      LayerGroupList,
      ClassificationObjects,
      ClassificationObjectOwnerList);

    ClassificationList := TStringList.Create;
    try

      ClassifyListedObjects(ClassificationList, ClassificationObjects,
        [LayerGroupList, HufDataArrays]);

      CreateClassifiedVirtualNodes(ClassificationList, 0,
        Tree, SelectedName, ClassificationObjectOwnerList);
    finally
      ClassificationList.Free;
    end;

  finally
    LayerGroupList.Free;
    ClassificationObjects.Free;
    HufDataArrays.Free;
  end;
end;

procedure GetNodeCaption(Node: PVirtualNode;
  var CellText: WideString; Sender: TBaseVirtualTree);
var
  ClassificationNodeData: PClassificationNodeData;
begin
  ClassificationNodeData := Sender.GetNodeData(Node);
  if not Assigned(ClassificationNodeData)
    or not Assigned(ClassificationNodeData.ClassificationObject) then
  begin
    CellText := 'none';
  end
  else
  begin
    CellText := ClassificationNodeData.ClassificationObject.ClassificationName;
  end;
end;

procedure UpdateTreeComboText(SelectedNode: PVirtualNode;
  TreeCombo: TTntExDropDownVirtualStringTree);
var
  CellText: WideString;
begin
  if TreeCombo.Tree.SelectedCount = 0 then
  begin
    if TreeCombo.Text <> '' then
    begin
      TreeCombo.Text := '';
    end;
  end
  else
  begin
    GetNodeCaption(SelectedNode, CellText, TreeCombo.Tree);
    TreeCombo.Text := CellText;
  end;
end;

procedure SelectOnlyLeaves(Node: PVirtualNode;
  TreeCombo: TTntExDropDownVirtualStringTree;
  Sender: TBaseVirtualTree; var SelectedNode: PVirtualNode);
var
  CellText: WideString;
begin
  if Sender.Selected[Node] and Sender.HasChildren[Node] then
  begin
    Sender.Selected[Node] := False;
    Sender.FocusedNode := nil;
    TreeCombo.Text := '';
  end;
  if Sender.Selected[Node] then
  begin
    SelectedNode := Node;
    GetNodeCaption(Node, CellText, Sender);
    TreeCombo.Text := CellText;
  end
  else
  begin
    SelectedNode := nil;
  end;
end;

initialization
  HelpRouter := THelpRouter.Create(Application);
  HelpRouter.HelpType := htHTMLhelp;
  HelpRouter.ValidateID := False;

finalization

end.
