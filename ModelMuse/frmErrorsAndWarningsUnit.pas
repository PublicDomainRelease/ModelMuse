unit frmErrorsAndWarningsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, VirtualTrees;

type
  // @name is used to display error and warning messages that are generated
  // during export of a model.
  TfrmErrorsAndWarnings = class(TfrmCustomGoPhast)
    // @name is the TPanel at the bottom of the TForm.
    pnlBottom: TPanel;
    // @name closes the @classname.
    btnClose: TBitBtn;
    // @name is used to display the error and warning messages in a tree view.
    vstWarningsAndErrors: TVirtualStringTree;
    // @name is the Help button.
    btnHelp: TBitBtn;
    Timer1: TTimer;
    btnSave: TButton;
    sdSaveFileDlg: TSaveDialog;
    btnClear: TButton;
    // @name creates @link(ErrorChildNodes),
    // @link(Errors), @link(WarningChildNodes) and
    // @link(Warnings).
    // It also initializes the size of the record associated with
    // nodes in @link(vstWarningsAndErrors).
    procedure FormCreate(Sender: TObject); override;
    // @name destroys @link(ErrorChildNodes),
    // @link(Errors), @link(WarningChildNodes) and
    // @link(Warnings) and other data associated with the @classname..
    procedure FormDestroy(Sender: TObject); override;
    // @name determines the text to display in each cell.
    procedure vstWarningsAndErrorsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    // @name initializes all the nodes so they can be multi-line.
    procedure vstWarningsAndErrorsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    // @name determines the height of the current node.
    procedure vstWarningsAndErrorsMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    // @name is a list of the PVirtualNodes beneath @link(ErrorNode).
    ErrorChildNodes: TList;
    // @name is the node in @link(vstWarningsAndErrors) that has
    // beneath it all the error messages. @link(Errors) is associated with
    // @name through the data record associated with @name.
    ErrorNode: PVirtualNode;
    // @name is a list of the error messages generated during export
    // of a project.  Its Objects property contains lists of the cells
    // to which the error messages apply.
    Errors: TStringList;
    // @name is a list of the PVirtualNodes beneath @link(WarningNode).
    WarningChildNodes: TList;
    // @name is the node in @link(vstWarningsAndErrors) that has
    // beneath it all the warning messages. @link(Warnings) is associated with
    // @name through the data record associated with @name.
    WarningNode: PVirtualNode;
    // @name is a list of the warning messages generated during export
    // of a project.  Its Objects property contains lists of the cells
    // to which the warning messages apply.
    Warnings: TStringList;
    // @name is used to add an error or warning to @classname.
    // It first creates RootNode if it does not exist and associates
    // RootList with it.  Then if checks if an error message like
    // TypeOfErrorOrWarning has been created.  If not, it creates a
    // PVirtualNode for it (stored in Children)
    // and associates a TStringList with it.
    // The TStringList is also stored in RootList in its Objects property.
    // Finally, ErrorOrWarning is added to the TStringList and a new
    // PVirtualNode is created.
    procedure AddErrorOrWarning(RootList: TStringList;
      const TypeOfErrorOrWarning, ErrorOrWarning: string;
      var RootNode: PVirtualNode; Children: TList);
    // @name frees everything in the Objects property of List.
    procedure ClearABaseStringList(List: TStringList);
    // @name creates a new root node (PVirtualNode) and associates List
    // with it. @seealso(ErrorNode) @seealso(WarningNode)
    procedure InitializeRootNode(var Node: PVirtualNode; List: TStringList);
    procedure RemoveWarningOrErrorGroup(const TypeOfErrorOrWarning: string;
      RootList: TStringList; Children: TList);
    procedure CheckDeleteRootNode(var Node: PVirtualNode);
    { Private declarations }
  public
    function HasMessages: boolean;
    // @name is used to add a new error message to @classname.
    // Root is the type of error, Error is the specific cell to which the
    // error applies.
    Procedure AddError(const Root, Error: string);
    // @name is used to add a new warning message to @classname.
    // Root is the type of warning, Warning is the specific cell to which the
    // warning applies.
    Procedure AddWarning(const Root, Warning: string);
    // @name deletes the warning and error messages and clears
    // @link(vstWarningsAndErrors).
    Procedure Clear;
    procedure RemoveWarningGroup(const TypeOfWarning: string);
    procedure RemoveErrorGroup(const TypeOfError: string);
    procedure ShowAfterDelay;
    { Public declarations }
  end;

function frmErrorsAndWarnings: TfrmErrorsAndWarnings;

implementation

uses Math, frmProgressUnit;
{$R *.dfm}

var
  FfrmErrorsAndWarnings: TfrmErrorsAndWarnings = nil;

type
  PErrorWarningRec = ^TErrorWarningRec;
  TErrorWarningRec = record
    List: TStringList;
  end;

function frmErrorsAndWarnings: TfrmErrorsAndWarnings;
begin
  if FfrmErrorsAndWarnings = nil then
  begin
    FfrmErrorsAndWarnings := TfrmErrorsAndWarnings.Create(nil);
//    Application.CreateForm(TfrmErrorsAndWarnings, FfrmErrorsAndWarnings);
  end;
  FfrmErrorsAndWarnings.Handle;
  result := FfrmErrorsAndWarnings;
end;

{ TfrmErrorsAndWarnings }

procedure TfrmErrorsAndWarnings.AddErrorOrWarning(RootList: TStringList;
  const TypeOfErrorOrWarning, ErrorOrWarning: string;
  var RootNode: PVirtualNode; Children: TList);
var
  ChildNode: PVirtualNode;
  RootIndex: integer;
  Data: PErrorWarningRec;
  ErrorMessages: TStringList;
begin
  if RootNode = nil then
  begin
    InitializeRootNode(RootNode, RootList);
  end;

  RootIndex := RootList.IndexOf(TypeOfErrorOrWarning);
  if RootIndex >= 0 then
  begin
    ErrorMessages := RootList.Objects[RootIndex] as TStringList;
    ChildNode := Children[RootIndex];
  end
  else
  begin
    ErrorMessages := TStringList.Create;
    RootList.AddObject(TypeOfErrorOrWarning, ErrorMessages);
    ChildNode := vstWarningsAndErrors.AddChild(RootNode);
    Children.Add(ChildNode);
    Data := vstWarningsAndErrors.GetNodeData(ChildNode);
    Data.List := ErrorMessages;
    vstWarningsAndErrors.HasChildren[ChildNode] := True;
    if frmProgress <> nil then
    begin
      frmProgress.AddMessage(TypeOfErrorOrWarning);
    end;
  end;

  ErrorMessages.Add(ErrorOrWarning);
  vstWarningsAndErrors.ChildCount[ChildNode] :=
    vstWarningsAndErrors.ChildCount[ChildNode] + 1;
end;

procedure TfrmErrorsAndWarnings.InitializeRootNode(var Node: PVirtualNode;
  List: TStringList);
var
  Data: PErrorWarningRec;
begin
  vstWarningsAndErrors.RootNodeCount := vstWarningsAndErrors.RootNodeCount + 1;
  Node := vstWarningsAndErrors.RootNode.LastChild;
  Data := vstWarningsAndErrors.GetNodeData(Node);
  Data.List := List;
  vstWarningsAndErrors.HasChildren[Node] := True;
end;

procedure TfrmErrorsAndWarnings.RemoveWarningOrErrorGroup(
  const TypeOfErrorOrWarning: string; RootList: TStringList; Children: TList);
var
  RootIndex: Integer;
  ErrorMessages: TStringList;
  ChildNode: PVirtualNode;
begin
  RootIndex := RootList.IndexOf(TypeOfErrorOrWarning);
  if RootIndex >= 0 then
  begin
    ErrorMessages := RootList.Objects[RootIndex] as TStringList;
    ChildNode := Children[RootIndex];

    vstWarningsAndErrors.DeleteNode(ChildNode);
    RootList.Delete(RootIndex);
    Children.Delete(RootIndex);
    ErrorMessages.Free;
  end
end;

procedure TfrmErrorsAndWarnings.ShowAfterDelay;
begin
  Timer1.Enabled := True;
end;

procedure TfrmErrorsAndWarnings.Timer1Timer(Sender: TObject);
begin
  inherited;
  Timer1.Enabled := False;
  Show;
end;

procedure TfrmErrorsAndWarnings.RemoveErrorGroup(const TypeOfError: string);
begin
  RemoveWarningOrErrorGroup(TypeOfError, Errors, ErrorChildNodes);
  CheckDeleteRootNode(ErrorNode);
end;

procedure TfrmErrorsAndWarnings.CheckDeleteRootNode(var Node: PVirtualNode);
begin
  if (Node <> nil) and not vstWarningsAndErrors.HasChildren[Node] then
  begin
    vstWarningsAndErrors.DeleteNode(Node);
    Node := nil;
  end;
end;

procedure TfrmErrorsAndWarnings.RemoveWarningGroup(const TypeOfWarning: string);
begin
  RemoveWarningOrErrorGroup(TypeOfWarning, Warnings, WarningChildNodes);
  CheckDeleteRootNode(WarningNode);
end;

procedure TfrmErrorsAndWarnings.AddError(const Root, Error: string);
begin
  AddErrorOrWarning(Errors, Root, Error, ErrorNode, ErrorChildNodes);
end;

procedure TfrmErrorsAndWarnings.AddWarning(const Root, Warning: string);
begin
  AddErrorOrWarning(Warnings, Root, Warning, WarningNode, WarningChildNodes);
end;

procedure TfrmErrorsAndWarnings.btnClearClick(Sender: TObject);
begin
  inherited;
  Clear;
end;

procedure TfrmErrorsAndWarnings.btnSaveClick(Sender: TObject);
const
  TabChar = #9;
var
  ErrorsAndWarings: TStringList;
  Node: PVirtualNode;
  ALine: string;
  Index: Integer;
begin
  inherited;
  if sdSaveFileDlg.Execute then
  begin
    ErrorsAndWarings := TStringList.Create;
    try
      Node := vstWarningsAndErrors.GetFirst;
      While Node <> nil do
      begin
        ALine := '';
        for Index := 0 to vstWarningsAndErrors.GetNodeLevel(Node) do
        begin
          ALine := ALine + TabChar;
        end;
        ErrorsAndWarings.Add(ALine + vstWarningsAndErrors.Text[Node, 0]);
        Node := vstWarningsAndErrors.GetNext(Node);
      end;

      ErrorsAndWarings.SaveToFile(sdSaveFileDlg.FileName);
    finally
      ErrorsAndWarings.Free;
    end;
  end;
end;

procedure TfrmErrorsAndWarnings.ClearABaseStringList(List: TStringList);
var
  Index: Integer;
begin
  for Index := List.Count - 1 downto 0 do
  begin
    List.Objects[Index].Free;
  end;
  List.Clear;
end;

procedure TfrmErrorsAndWarnings.Clear;
begin
  vstWarningsAndErrors.Clear;
  ErrorNode := nil;
  WarningNode := nil;
  ClearABaseStringList(Errors);
  ClearABaseStringList(Warnings);
  ErrorChildNodes.Clear;
  WarningChildNodes.Clear;
end;

procedure TfrmErrorsAndWarnings.FormCreate(Sender: TObject);
begin
  inherited;
  vstWarningsAndErrors.NodeDataSize := SizeOf(TErrorWarningRec);
  Errors := TStringList.Create;
  Warnings := TStringList.Create;
  ErrorChildNodes:= TList.Create;
  WarningChildNodes:= TList.Create;
end;

procedure TfrmErrorsAndWarnings.FormDestroy(Sender: TObject);
var
  Node: PVirtualNode;
begin
  inherited;
  Clear;
  Errors.Free;
  Warnings.Free;
  ErrorChildNodes.Free;
  WarningChildNodes.Free;
  Node := vstWarningsAndErrors.GetFirst;
  while Node <> nil do
  begin
    vstWarningsAndErrors.DeleteNode(Node);
    Node := vstWarningsAndErrors.GetFirst;
  end;
end;

procedure TfrmErrorsAndWarnings.FormResize(Sender: TObject);
var
  Indent: integer;
begin
  inherited;
  // Using the Indent local variable here instead of
  // vstWarningsAndErrors.Indent avoids generating
  // a compiler warning.
  Indent := vstWarningsAndErrors.Indent;
  vstWarningsAndErrors.Header.Columns[0].Width :=
    vstWarningsAndErrors.ClientWidth - Indent;
end;

function TfrmErrorsAndWarnings.HasMessages: boolean;
begin
  result := (Errors.Count > 0) or (Warnings.Count > 0)
end;

procedure TfrmErrorsAndWarnings.vstWarningsAndErrorsGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);
var
  Data: PErrorWarningRec;
  ParentNode: PVirtualNode;
begin
  inherited;
  // A handler for the OnGetText event is always needed as it provides the tree with the string data to display.
  // Note that we are always using WideString.
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    if Node = ErrorNode then
    begin
      CellText := 'Errors';
    end
    else if Node = WarningNode then
    begin
      CellText := 'Warnings';
    end
    else
    begin
      ParentNode := Node.Parent;
      Data := Sender.GetNodeData(ParentNode);
      if Assigned(Data) and (Data.List <> nil) then
      begin
        CellText := Data.List[Node.Index];
      end;
    end;
  end;
end;

procedure TfrmErrorsAndWarnings.vstWarningsAndErrorsInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  inherited;
  Include(InitialStates, ivsMultiline);
end;

procedure TfrmErrorsAndWarnings.vstWarningsAndErrorsMeasureItem(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  var NodeHeight: Integer);
var
  CellCaption: WideString;
  Flags: UINT;
  Rect: TRect;
  TextString: string;
  Count: Integer;
  ANode: PVirtualNode;
begin
  inherited;
  Count := -2;
  ANode := Node;
  While (ANode <> nil) do
  begin
    Inc(Count);
    ANode := ANode.Parent;
  end;
  Count := Max(0, Count);
  vstWarningsAndErrorsGetText(Sender, Node, 0, ttNormal, CellCaption);
  TextString := CellCaption;

  Flags := DT_NOPREFIX or DT_Left or DT_CALCRECT or DT_WORDBREAK;

  Rect.Top := 0;
  Rect.Left := 4 + Count*27;
  Rect.Bottom := 300;
  Rect.Right := vstWarningsAndErrors.Header.Columns[0].Width - 40;

  TargetCanvas.Font := Font;
  NodeHeight := DrawText(TargetCanvas.Handle, PChar(TextString),
        Length(TextString), Rect, Flags) + 4;

  vstWarningsAndErrors.NodeHeight[Node] := NodeHeight;
end;

initialization

finalization
  FfrmErrorsAndWarnings.Free;

end.
