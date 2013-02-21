{
May 22, 2006: introduced CustomizeControls.
}

{@abstract(The purpose of @name is to declare @link(TfrmCustomGoPhast),
  the ancestor of all TForms in ModelMuse.)}
unit frmCustomGoPhastUnit;

interface

uses
  EdgeDisplayUnit, CommDlg, RbwDataGrid4, Spin, Windows, Forms, SysUtils, Types,
  Classes, Graphics, Controls, Dialogs, StdCtrls, Grids, HtmlHelpViewer,
  JvSpin, VirtualTrees, DataSetUnit, ClassificationUnit, GLWin32Viewer,
  RbwStringTreeCombo, Mask, JvExMask;

type
  TEdgeDisplayEdit = class(TObject)
    Edge: TCustomModflowGridEdgeDisplay;
    DataIndex: integer;
  end;

  TBoundaryClassification = class(TClassificationObject)
  private
    FDataArray: TDataArray;
    FTimeList: TCustomTimeList;
    FEdgeDisplay: TEdgeDisplayEdit;
    FName: string;
    function GetClassifiedObject: TObject;
    function GetBoundaryType: TBoundaryType;
  public
    function ClassificationName: string; Override;
    function FullClassification: string; Override;
    Constructor Create(AnObject: TDataArray); overload;
    Constructor Create(AnObject: TCustomTimeList); overload;
    Constructor Create(const Name: string; AnObject: TEdgeDisplayEdit); overload;
    Constructor Create(const Name: string; AnObject: TObject); overload;
    property ClassifiedObject: TObject read GetClassifiedObject;
    property BoundaryType: TBoundaryType read GetBoundaryType;
  end;

  TCanSelectBoundary = function (BoundaryClassification: TBoundaryClassification): Boolean of object;

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
{$IF CompilerVersion < 23}
// Use this version for Delphi XE
   function FormHelp(Command: Word; Data:  Integer;
     var CallHelp: Boolean): Boolean;
{$ELSE}
// Use this version for Delphi XE2
   function FormHelp(Command: Word; Data:  NativeInt;
     var CallHelp: Boolean): Boolean;
{$IFEND}
    procedure FormDestroy(Sender: TObject); virtual;
  private
    FCallingHelp: Boolean;
    function CallHelpRouter: boolean;
    procedure DestroyGLSceneViewers(ParentComponent: TComponent);
    procedure EnsureFormVisible;
    procedure FixGridEditorPosition(AComponent: TComponent);
  protected
    procedure UpdateStringTreeViewCheckedState(TreeView: TVirtualStringTree;
      BaseNode: PVirtualNode; NewState: TCheckState);
    {@name adjusts the position of Form to the left or right of the main form,
     if possible.  If there isn't room for it at the desired position it will
     try the other side.  If there isn't room there either,
     it will have it overlap
     the form on the desired side.
     A side effect is that it sets Form.Position to
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
  public
    procedure MouseClick;
    procedure UpdateSubComponents(AComponent: TComponent);
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
    Col: integer); overload;
  procedure EnableMultiEditControl(Grid: TRbwDataGrid4; AControl: TControl;
    Cols: array of integer); overload;

  // @name sets the Left and Width of Control and ALabel so that they are
// lined up over Column in Grid. ALabel.Alignment should be taCenter.
procedure LayoutControls(Grid: TRbwDataGrid4; Control: TControl;
  ALabel: TLabel; Column: Integer; ControlOffSet: integer = 0);

Procedure UpdateDialogBoxFileName(Dialog: TOpenDialog; NewFileName: string);

type TDataSetAllowedEvent = function (DataArray: TDataArray): boolean of object;

Procedure FillVirtualStringTreeWithDataSets(Tree : TVirtualStringTree;
  ClassificationObjectOwnerList: TList; SelectedDataArray: TDataArray;
  DataSetAllowed: TDataSetAllowedEvent = nil);

procedure FillDataSetLists(HufDataArrays: TClassificationList;
  LayerGroupList: TClassificationList;
  ClassificationObjects: TClassificationList;
  ClassificationObjectOwnerList: TList;
  DataSetAllowed: TDataSetAllowedEvent = nil);

procedure GetNodeCaption(Node: PVirtualNode; var CellText: string;
  Sender: TBaseVirtualTree);

procedure SelectOnlyLeaves(Node: PVirtualNode;
  TreeCombo: TRbwStringTreeCombo; Sender: TBaseVirtualTree;
  var SelectedNode: PVirtualNode);

procedure FillComboWithModelNames(Combo: TComboBox);

procedure UpdateTreeComboText(SelectedNode: PVirtualNode;
  TreeCombo: TRbwStringTreeCombo);

procedure FillVirtStrTreeWithBoundaryConditions(
  SelectedDataArray: TDataArray;
  SelectedTimeList: TCustomTimeList;
  SelectedEdgeDisplay: TCustomModflowGridEdgeDisplay;
  LocalBoundaryClassifications: TList; EdgeEdits: TList;
  ATree: TVirtualStringTree;
  CanSelectBoundary: TCanSelectBoundary = nil);

function ShowHelp(const Keyword: string): boolean;

var
  GlobalFont: TFont = nil;
  GlobalColor: TColor = clBtnFace;

resourcestring
  StrNone = 'none';

implementation

uses SubscriptionUnit, GoPhastTypes, ModflowPackagesUnit,
  frmGoPhastUnit, PhastModelUnit, ModflowPackageSelectionUnit,
  frameCustomColorUnit, JvRollOut, Messages, ShellAPI;

{$R *.dfm}

type
  TStringGridCrack = class(TStringGrid);

// from http://delphi.about.com/cs/adptips2000/a/bltip0100_2.htm
function KillApp(const sCapt: PChar) : boolean;
var
  AppHandle:THandle;
begin
  AppHandle:=FindWindow(Nil, sCapt) ;
  if AppHandle <> 0 then
  begin
    Result:=PostMessage(AppHandle, WM_QUIT, 0, 0) ;
  end
  else
  begin
    Result := False;
  end;
end;


function ShowHelp(const Keyword: string): boolean;
//var
//  AppHandle:THandle;
begin
{
  AppHandle:=FindWindow(Nil, 'ModelMuse Help');
  if AppHandle = 0 then
  begin
    if Keyword = '' then
    begin
  //    Result := ShellExecute(AppHandle, 'open', 'HH', PChar(Application.HelpFile),
      Result := ShellExecute(0, 'open', 'HH', PChar(Application.HelpFile),
        nil, SW_SHOWNORMAL) > 32;
    end
    else
    begin
  //    Result := ShellExecute(AppHandle, 'open', 'HH',
      Result := ShellExecute(0, 'open', 'HH',
        PChar(Application.HelpFile + '::/' + Keyword + '.htm'),
        nil, SW_SHOWNORMAL) > 32;
    end;
  end
  else
  begin
    if Keyword = '' then
    begin
      Result := ShellExecute(AppHandle, 'explore', 'HH', PChar(Application.HelpFile),
//      Result := ShellExecute(0, 'open', 'HH', PChar(Application.HelpFile),
        nil, SW_SHOWNORMAL) > 32;
    end
    else
    begin
      Result := ShellExecute(AppHandle, 'explore', 'HH',
//      Result := ShellExecute(0, 'open', 'HH',
        PChar(Application.HelpFile + '::/' + Keyword + '.htm'),
        nil, SW_SHOWNORMAL) > 32;
    end;
  end;
}

  KillApp('ModelMuse Help');
  if Keyword = '' then
  begin
    Result := ShellExecute(0, 'open', 'HH', PChar(Application.HelpFile),
      nil, SW_SHOWNORMAL) > 32;
  end
  else
  begin
    Result := ShellExecute(0, 'open', 'HH',
      PChar(Application.HelpFile + '::/' + Keyword + '.htm'),
      nil, SW_SHOWNORMAL) > 32;
  end;


end;

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

procedure TfrmCustomGoPhast.DestroyGLSceneViewers(ParentComponent: TComponent);
var
  Index: Integer;
  AComponent: TComponent;
begin
   Exit;
  // It seems that some TGLSceneViewer's are causing errors when
  // the form is being destroyed.  This is an attempt
  // to get around that.
  for Index := ParentComponent.ComponentCount - 1 downto 0 do
  begin
    AComponent := ParentComponent.Components[Index];
    if AComponent is TGLSceneViewer then
    begin
      AComponent.Free;
    end
    else
    begin
      DestroyGLSceneViewers(AComponent);
    end;
  end;
end;

procedure TfrmCustomGoPhast.FormDestroy(Sender: TObject);
begin
  DestroyGLSceneViewers(self);
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
  result := not ShowHelp(KeyWord);
//  result := Application.HelpJump(KeyWord);
//  result := HelpRouter.HelpJump('', KeyWord);
end;

{$IF CompilerVersion < 23}
// Delphi XE
function TfrmCustomGoPhast.FormHelp(Command: Word; Data:  Integer;
  var CallHelp: Boolean): Boolean;
{$ELSE}
// Delphi XE2
function TfrmCustomGoPhast.FormHelp(Command: Word; Data:  NativeInt;
  var CallHelp: Boolean): Boolean;
{$IFEND}
begin
  if (Command in [HELP_CONTEXT, HELP_INDEX, HELP_FORCEFILE,
    HH_DISPLAY_SEARCH {, 15}])
    {or (Command = HELP_COMMAND)} then
  begin
    // 15 = help contents.
    result := False;
    
  end
  else
  begin
    if FCallingHelp then
    begin
      result := True;
      CallHelp := True;
    end
    else
    begin
      FCallingHelp := True;
      result := CallHelpRouter;
      CallHelp := False;
      FCallingHelp := False;
    end;
//    result := True;
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

procedure TfrmCustomGoPhast.FixGridEditorPosition(AComponent: TComponent);
var
  Grid: TCustomRBWDataGrid;
  AnotherComponent: TComponent;
  ComponentIndex: integer;
begin
  for ComponentIndex := 0 to AComponent.ComponentCount - 1 do
  begin
    AnotherComponent := AComponent.Components[ComponentIndex];
    if AnotherComponent is TCustomRBWDataGrid then
    begin
      Grid := TCustomRBWDataGrid(AnotherComponent);
      if goAlwaysShowEditor in Grid.Options then
      begin
        Grid.Options := Grid.Options - [goAlwaysShowEditor];
        Grid.Options := Grid.Options + [goAlwaysShowEditor];
      end;
    end
    else
    begin
      FixGridEditorPosition(AnotherComponent);
    end;
  end;
end;

procedure FillVirtStrTreeWithBoundaryConditions(
  SelectedDataArray: TDataArray;
  SelectedTimeList: TCustomTimeList;
  SelectedEdgeDisplay: TCustomModflowGridEdgeDisplay;
  LocalBoundaryClassifications: TList; EdgeEdits: TList;
  ATree: TVirtualStringTree;
  CanSelectBoundary: TCanSelectBoundary = nil);
var
  List: TStringList;
  ClassificationPosition: Integer;
  DataSet: TDataArray;
  Index: Integer;
  DataArrayManager: TDataArrayManager;
  Classifications: TStringList;
  NodeData: PClassificationNodeData;
  RootNode: PVirtualNode;
  ANode: PVirtualNode;
  NextNode: PVirtualNode;
  DummyRootClassification: TDummyClassification;
  ParentNode: PVirtualNode;
  AnObject: TObject;
  VirtualNode: PVirtualNode;
  BounddaryClassification: TBoundaryClassification;
  BoundaryIndex: Integer;
  VirtualClassificationNode: PVirtualNode;
  DummyClassification: TDummyClassification;
  TimeList: TCustomTimeList;
  EdgeEdit: TEdgeDisplayEdit;
  UsedByModel: Boolean;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildTimeList: TCustomTimeList;
  NodeDeleted: Boolean;
begin
  LocalBoundaryClassifications.Clear;
  DummyRootClassification := TDummyClassification.Create(StrBoundaryConditions);
  LocalBoundaryClassifications.Add(DummyRootClassification);
  RootNode := ATree.AddChild(nil);
  NodeData := ATree.GetNodeData(RootNode);
  NodeData.ClassificationObject := DummyRootClassification;
  Classifications := TStringList.Create;
  try
    if frmGoPhast.PhastModel.ModelSelection = msPhast then
    begin
      DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
      for Index := 0 to DataArrayManager.BoundaryDataSetCount - 1 do
      begin
        DataSet := DataArrayManager.BoundaryDataSets[Index];
        ClassificationPosition := Classifications.IndexOf(
          DataSet.Classification);
        if ClassificationPosition < 0 then
        begin
          List := TStringList.Create;
          Classifications.AddObject(DataSet.Classification, List);
        end
        else
        begin
          List := Classifications.Objects[ClassificationPosition]
            as TStringList;
        end;
        List.AddObject(DataSet.Name, DataSet);
      end;
    end;
    EdgeEdits.Clear;
    if (frmGoPhast.PhastModel.ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
      and frmGoPhast.PhastModel.HfbIsSelected then
    begin
      List := TStringList.Create;
      Classifications.AddObject('MODFLOW Horizontal Flow Barrier', List);
      for Index := 0 to frmGoPhast.PhastModel.
        HfbDisplayer.RealValueTypeCount - 1 do
      begin
        EdgeEdit := TEdgeDisplayEdit.Create;
        EdgeEdits.Add(EdgeEdit);
        EdgeEdit.DataIndex := Index;
        EdgeEdit.Edge := frmGoPhast.PhastModel.HfbDisplayer;
        List.AddObject(EdgeEdit.Edge.RealDescription[Index], EdgeEdit);
      end;
    end;
    for Index := 0 to frmGoPhast.PhastModel.TimeListCount - 1 do
    begin
      TimeList := frmGoPhast.PhastModel.TimeLists[Index];
      UsedByModel := TimeList.UsedByModel;
      if not UsedByModel and frmGoPhast.PhastModel.LgrUsed then
      begin
        for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
        begin
          ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
          if ChildModel <> nil then
          begin
            ChildTimeList := ChildModel.GetTimeListByName(TimeList.Name);
            if (ChildTimeList <> nil) and ChildTimeList.UsedByModel then
            begin
              UsedByModel := True;
              break;
            end;
          end;
        end;
      end;
      if UsedByModel then
      begin
        ClassificationPosition := Classifications.IndexOf(
          TimeList.Classification);
        if ClassificationPosition < 0 then
        begin
          List := TStringList.Create;
          Classifications.AddObject(TimeList.Classification, List);
        end
        else
        begin
          List := Classifications.Objects[ClassificationPosition]
            as TStringList;
        end;
        List.AddObject(TimeList.Name, TimeList);
      end;
    end;
    Classifications.Sort;
    for Index := 0 to Classifications.Count - 1 do
    begin
      DummyClassification := TDummyClassification.Create(Classifications[Index]);
      LocalBoundaryClassifications.Add(DummyClassification);
      VirtualClassificationNode := ATree.AddChild(RootNode);
      NodeData := ATree.GetNodeData(VirtualClassificationNode);
      NodeData.ClassificationObject := DummyClassification;
      List := Classifications.Objects[Index] as TStringList;
      List.Sort;
      for BoundaryIndex := 0 to List.Count - 1 do
      begin
        BounddaryClassification := TBoundaryClassification.Create(
          List[BoundaryIndex], List.Objects[BoundaryIndex]);
        if Assigned(CanSelectBoundary) then
        begin
          if not CanSelectBoundary(BounddaryClassification) then
          begin
            BounddaryClassification.Free;
            Continue;
          end;
        end;
        LocalBoundaryClassifications.Add(BounddaryClassification);
        VirtualNode := ATree.AddChild(VirtualClassificationNode);
        NodeData := ATree.GetNodeData(VirtualNode);
        NodeData.ClassificationObject := BounddaryClassification;
        AnObject := BounddaryClassification.ClassifiedObject;
        if AnObject is TEdgeDisplayEdit then
        begin
          EdgeEdit := TEdgeDisplayEdit(AnObject);
          ATree.Selected[VirtualNode] := (EdgeEdit.Edge = SelectedEdgeDisplay)
            and (EdgeEdit.DataIndex = SelectedEdgeDisplay.DataToPlot);
        end
        else
        begin
          ATree.Selected[VirtualNode] := (AnObject = SelectedDataArray)
            or (AnObject = SelectedTimeList);
        end;
        if ATree.Selected[VirtualNode] then
        begin
          ParentNode := VirtualNode.Parent;
          while ParentNode <> nil do
          begin
            ATree.Expanded[ParentNode] := True;
            if ParentNode = RootNode then
            begin
              break;
            end;
            ParentNode := ParentNode.Parent;
          end;
        end;
      end;
    end;

    repeat
      ANode := RootNode;
      NodeDeleted := False;
      while ANode <> nil do
      begin
        NextNode := ATree.GetNext(ANode);
        NodeData := ATree.GetNodeData(ANode);
        if NodeData.ClassificationObject is TDummyClassification then
        begin
          if not ATree.HasChildren[ANode] then
          begin
            if ANode = RootNode then
            begin
              RootNode := nil;
            end;
            ATree.DeleteNode(ANode);
            NodeDeleted := True;
          end;
        end;
        ANode:= NextNode;
      end;
    until not NodeDeleted;


  finally
    for Index := 0 to Classifications.Count - 1 do
    begin
      Classifications.Objects[Index].Free;
    end;
    Classifications.Free;
  end;
end;

procedure TfrmCustomGoPhast.EnsureFormVisible;
var
  FrameWidth: Integer;
  WorkAreaRect: TRect;
  FrameHeight: Integer;
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

procedure TfrmCustomGoPhast.UpdateStringTreeViewCheckedState(
  TreeView: TVirtualStringTree; BaseNode: PVirtualNode; NewState: TCheckState);
var
  ChildNode: PVirtualNode;
  ChildIndex: Integer;
begin
  ChildNode := nil;
  for ChildIndex := 0 to BaseNode.ChildCount - 1 do
  begin
    if ChildIndex = 0 then
    begin
      ChildNode := TreeView.GetFirstChild(BaseNode);
    end
    else
    begin
      ChildNode := TreeView.GetNextSibling(ChildNode);
    end;
    if TreeView.Selected[ChildNode] and (ChildNode.CheckState <> NewState) then
    begin
      ChildNode.CheckState := NewState;
      if Assigned(TreeView.OnChecked) then
      begin
        TreeView.OnChecked(TreeView, ChildNode);
      end;
    end;
    if ChildNode.ChildCount > 0 then
    begin
      UpdateStringTreeViewCheckedState(TreeView, ChildNode, NewState);
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
//      TStringGridCrack(AStringGrid).ShowEditor;
//      AStringGrid.Font.Size := AStringGrid.Font.Size + 1;
//      AStringGrid.Font.Size := AStringGrid.Font.Size - 1;
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
      if Component is TJvRollOut then
      begin
        TJvRollOut(Component).ButtonFont := Font;
      end;
    end;
    UpdateSubComponents(Component);
  end;
end;

procedure UpdateTreeComboText(SelectedNode: PVirtualNode;
  TreeCombo: TRbwStringTreeCombo);
var
  CellText: string;
begin
  if TreeCombo.Tree.SelectedCount = 0 then
  begin
    if TreeCombo.Text <> 'none' then
    begin
      TreeCombo.Text := 'none';
    end;
  end
  else
  begin
    GetNodeCaption(SelectedNode, CellText, TreeCombo.Tree);
    TreeCombo.Text := CellText;
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
begin
  GetFormatSettings;
  EnsureFormVisible;
  FixGridEditorPosition(Self);
end;

procedure TfrmCustomGoPhast.MouseClick;
begin
  Mouse_Event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
  Mouse_Event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
end;

procedure TfrmCustomGoPhast.AdjustFormPosition(
  DesiredPosition: TDesiredPosition);
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
  if Column >= Grid.ColCount then
  begin
    Exit;
  end;
  if Grid.RowCount = 0 then
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

procedure EnableMultiEditControl(Grid: TRbwDataGrid4;
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

procedure EnableMultiEditControl(Grid: TRbwDataGrid4; AControl: TControl;
  Cols: array of integer); overload;
var
  ShouldEnable: boolean;
  RowIndex: Integer;
  ColIndex: Integer;
  Col: Integer;
begin
  ShouldEnable := False;
  for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
  begin
    for ColIndex := 0 to Length(Cols) - 1 do
    begin
      Col := Cols[ColIndex];
      ShouldEnable := Grid.IsSelectedCell(Col,RowIndex);
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
  AControl.Enabled := ShouldEnable;
end;


procedure ChangeSelectedCellsInColumn(Grid: TRbwDataGrid4;
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

procedure ChangeSelectedCellsStateInColumn(
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
  ClassificationObjectOwnerList: TList;
  DataSetAllowed: TDataSetAllowedEvent = nil);
var
  Index: Integer;
  DataSet: TDataArray;
  ClassificationObject: TDataSetClassification;
  Position: Integer;
  HydrogeologicUnitNames: TStringList;
  LayerGroupsDataSets: TList;
  DataArrayManager: TDataArrayManager;
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
    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    for Index := 0 to DataArrayManager.DataSetCount - 1 do
    begin
      DataSet := DataArrayManager.DataSets[Index];
      if Assigned(DataSetAllowed) and not DataSetAllowed(DataSet) then
      begin
        Continue;
      end;
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
  ClassificationObjectOwnerList: TList; SelectedDataArray: TDataArray;
  DataSetAllowed: TDataSetAllowedEvent = nil);
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
    SelectedName := SelectedDataArray.DisplayName;
  end;

  // Create lists used for sorting the nodes.
  HufDataArrays := TClassificationList.Create;
  ClassificationObjects:= TClassificationList.Create;
  LayerGroupList := TClassificationList.Create;
  try
    FillDataSetLists(HufDataArrays,
      LayerGroupList,
      ClassificationObjects,
      ClassificationObjectOwnerList, DataSetAllowed);

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
  var CellText: string; Sender: TBaseVirtualTree);
var
  ClassificationNodeData: PClassificationNodeData;
begin
  ClassificationNodeData := Sender.GetNodeData(Node);
  if not Assigned(ClassificationNodeData)
    or not Assigned(ClassificationNodeData.ClassificationObject) then
  begin
    CellText := StrNone;
  end
  else
  begin
    CellText := ClassificationNodeData.ClassificationObject.ClassificationName;
  end;
end;

procedure SelectOnlyLeaves(Node: PVirtualNode;
  TreeCombo: TRbwStringTreeCombo; Sender: TBaseVirtualTree;
  var SelectedNode: PVirtualNode); overload;
var
  CellText: string;
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

procedure FillComboWithModelNames(Combo: TComboBox);
var
  ChildModel: TChildModel;
  Index: Integer;
begin
  Combo.Clear;
  Combo.AddItem(StrParentModel, frmGoPhast.PhastModel);
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for Index := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[Index].ChildModel;
      Combo.AddItem(ChildModel.ModelName, ChildModel);
    end;
  end;
  Combo.ItemIndex := 0;
end;


//initialization
//  HelpRouter := THelpRouter.Create(Application);
//  HelpRouter.HelpType := htHTMLhelp;
//  HelpRouter.ValidateID := False;

//finalization

{ TBoundaryClassification }

function TBoundaryClassification.ClassificationName: string;
begin
  if FDataArray <> nil then
  begin
    result := FDataArray.DisplayName;
    Assert(FTimeList = nil);
    Assert(FEdgeDisplay = nil);
  end
  else if FTimeList <> nil then
  begin
    result := FTimeList.Name;
    Assert(FEdgeDisplay = nil);
  end
  else
  begin
    result := FName;
  end;
end;

constructor TBoundaryClassification.Create(AnObject: TCustomTimeList);
begin
  FTimeList := AnObject;
  FDataArray := nil;
  FEdgeDisplay := nil;
end;

constructor TBoundaryClassification.Create(AnObject: TDataArray);
begin
  FDataArray := AnObject;
  FTimeList := nil;
  FEdgeDisplay := nil;
end;

constructor TBoundaryClassification.Create(const Name: string;
  AnObject: TObject);
begin
  FName := Name;
  if AnObject is TDataArray then
  begin
    Create(TDataArray(AnObject));
  end
  else if AnObject is TCustomTimeList then
  begin
    Create(TCustomTimeList(AnObject));
  end
  else if AnObject is TEdgeDisplayEdit then
  begin
    Create(Name, TEdgeDisplayEdit(AnObject));
  end
  else
  begin
    Assert(False);
  end;
end;

constructor TBoundaryClassification.Create(const Name: string;
  AnObject: TEdgeDisplayEdit);
begin
  FName := Name;
  FEdgeDisplay := AnObject;
  FDataArray := nil;
  FTimeList := nil;
end;

function TBoundaryClassification.FullClassification: string;
begin
  result := ''
end;

function TBoundaryClassification.GetBoundaryType: TBoundaryType;
begin
  if FEdgeDisplay <> nil then
  begin
    result := btMfHfb;
  end
  else
  begin
    result := FTimeList.BoundaryType;
  end;
end;

function TBoundaryClassification.GetClassifiedObject: TObject;
begin
  if FDataArray <> nil then
  begin
    result := FDataArray;
  end
  else if FTimeList <> nil then
  begin
    result := FTimeList;
  end
  else
  begin
    result := FEdgeDisplay;
    Assert(result <> nil);
  end;
end;

initialization

finalization
  KillApp('ModelMuse Help');

end.


