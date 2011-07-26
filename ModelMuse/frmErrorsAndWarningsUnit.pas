unit frmErrorsAndWarningsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, VirtualTrees,
  GoPhastTypes;

type
  TModelMessages = class(TObject)
  private
    FModel: TBaseModel;
    FChildNodes: TList;
    FNode: PVirtualNode;
    FMessages: TStringList;
  public
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
  end;

  TModelMessageList = class(TObject)
  private
    FList: TList;
    function GetModelMessages(Model: TBaseModel): TModelMessages;
  public
    Constructor Create;
    Destructor Destroy; override;
    property ModelMessages[Model: TBaseModel]: TModelMessages
      read GetModelMessages;
    procedure Clear;
  end;

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
    btnCopy: TButton;
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
      var CellText: String);
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
    procedure btnCopyClick(Sender: TObject);
  private
    // @name is a list of the PVirtualNodes beneath @link(ErrorNode).
    FErrorChildNodes: TList;
    // @name is the node in @link(vstWarningsAndErrors) that has
    // beneath it all the error messages. @link(Errors) is associated with
    // @name through the data record associated with @name.
    FErrorNode: PVirtualNode;
    // @name is a list of the error messages generated during export
    // of a project.  Its Objects property contains lists of the locations
    // to which the error messages apply.
    FErrorModels: TStringList;
    FErrorModelMessageList: TModelMessageList;

    // @name is a list of the PVirtualNodes beneath @link(WarningNode).
    FWarningChildNodes: TList;
    // @name is the node in @link(vstWarningsAndErrors) that has
    // beneath it all the warning messages. @link(Warnings) is associated with
    // @name through the data record associated with @name.
    FWarningNode: PVirtualNode;
    // @name is a list of the warning messages generated during export
    // of a project.  Its Objects property contains lists of the cells
    // to which the warning messages apply.
    FWarningModels: TStringList;
    FWarningModelMessageList: TModelMessageList;

    // @name is used to add an error or warning to @classname.
    // It first creates RootNode if it does not exist and associates
    // RootList with it.  Then if checks if an error message like
    // TypeOfErrorOrWarning has been created.  If not, it creates a
    // PVirtualNode for it (stored in Children)
    // and associates a TStringList with it.
    // The TStringList is also stored in RootList in its Objects property.
    // Finally, ErrorOrWarning is added to the TStringList and a new
    // PVirtualNode is created.
    procedure AddErrorOrWarning(Model: TBaseModel; RootList: TStringList;
      const TypeOfErrorOrWarning, ErrorOrWarning: string;
      var RootNode: PVirtualNode; Children: TList;
      ModelMessageList: TModelMessageList);
    procedure RemoveWarningOrErrorGroup(Model: TBaseModel;
      const TypeOfErrorOrWarning: string;
      RootList: TStringList; Children: TList;
      ModelMessageList: TModelMessageList);
    procedure CheckDeleteRootNode(Model: TBaseModel; var Node: PVirtualNode;
      RootList: TStringList; ModelMessageList: TModelMessageList);
    procedure GetErrorsAndWarnings(ErrorsAndWarings: TStringList);
    // @name frees everything in the Objects property of List.
//    procedure ClearABaseStringList(List: TStringList);

    // @name creates a new root node (PVirtualNode) and associates List
    // with it. @seealso(ErrorNode) @seealso(WarningNode)
    procedure InitializeRootNode(var Node: PVirtualNode; List: TStringList);
    { Private declarations }
  public
    function HasMessages: boolean;
    // @name is used to add a new error message to @classname.
    // Root is the type of error, Error is the specific cell to which the
    // error applies.
    Procedure AddError(Model: TBaseModel; const Root, Error: string);
    // @name is used to add a new warning message to @classname.
    // Root is the type of warning, Warning is the specific cell to which the
    // warning applies.
    Procedure AddWarning(Model: TBaseModel; const Root, Warning: string);
    // @name deletes the warning and error messages and clears
    // @link(vstWarningsAndErrors).
    Procedure Clear;
    procedure RemoveWarningGroup(Model: TBaseModel; const TypeOfWarning: string);
    procedure RemoveErrorGroup(Model: TBaseModel; const TypeOfError: string);
    procedure ShowAfterDelay;
    { Public declarations }
  end;

function frmErrorsAndWarnings: TfrmErrorsAndWarnings;

implementation

uses Math, frmProgressUnit, Clipbrd, contnrs;

resourcestring
  StrErrors = 'Errors';
  StrWarnings = 'Warnings';
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
  end;
  FfrmErrorsAndWarnings.Handle;
  result := FfrmErrorsAndWarnings;
end;

{ TfrmErrorsAndWarnings }

procedure TfrmErrorsAndWarnings.AddErrorOrWarning(Model: TBaseModel;
  RootList: TStringList;
  const TypeOfErrorOrWarning, ErrorOrWarning: string;
  var RootNode: PVirtualNode; Children: TList;
  ModelMessageList: TModelMessageList);
var
  ChildNode: PVirtualNode;
  RootIndex: integer;
  Data: PErrorWarningRec;
  ErrorMessages: TStringList;
  ModelMessages: TModelMessages;
begin
  Assert(Model <> nil);
  if RootNode = nil then
  begin
    InitializeRootNode(RootNode, RootList);
  end;

  ModelMessages := ModelMessageList.GetModelMessages(Model);
  if (ModelMessages.FNode = nil) then
  begin
    vstWarningsAndErrors.ChildCount[RootNode] := RootNode.ChildCount + 1;
    ModelMessages.FNode := RootNode.LastChild;
    Data := vstWarningsAndErrors.GetNodeData(ModelMessages.FNode);
    Data.List := ModelMessages.FMessages;
    vstWarningsAndErrors.HasChildren[ModelMessages.FNode] := True;
    RootList.Add(Model.DisplayName);
  end;

  RootIndex := ModelMessages.FMessages.IndexOf(TypeOfErrorOrWarning);
  if RootIndex >= 0 then
  begin
    ErrorMessages := ModelMessages.FMessages.Objects[RootIndex] as TStringList;
    ChildNode := ModelMessages.FChildNodes[RootIndex];
  end
  else
  begin
    ErrorMessages := TStringList.Create;
    ModelMessages.FMessages.AddObject(TypeOfErrorOrWarning, ErrorMessages);
    ChildNode := vstWarningsAndErrors.AddChild(ModelMessages.FNode);
    ModelMessages.FChildNodes.Add(ChildNode);
    Data := vstWarningsAndErrors.GetNodeData(ChildNode);
    Data.List := ErrorMessages;
    vstWarningsAndErrors.HasChildren[ChildNode] := True;
    if frmProgressMM <> nil then
    begin
      frmProgressMM.AddMessage(TypeOfErrorOrWarning);
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

procedure TfrmErrorsAndWarnings.RemoveWarningOrErrorGroup(Model: TBaseModel;
  const TypeOfErrorOrWarning: string; RootList: TStringList; Children: TList;
      ModelMessageList: TModelMessageList);
var
  RootIndex: Integer;
  ErrorMessages: TStringList;
  ChildNode: PVirtualNode;
  ModelMessages: TModelMessages;
begin
  Assert(Model <> nil);
  ModelMessages := ModelMessageList.GetModelMessages(Model);
  if (ModelMessages.FNode = nil) then
  begin
    Exit;
  end;
  RootIndex := ModelMessages.FMessages.IndexOf(TypeOfErrorOrWarning);
  if RootIndex >= 0 then
  begin
    ErrorMessages := ModelMessages.FMessages.Objects[RootIndex] as TStringList;
    ChildNode := ModelMessages.FChildNodes[RootIndex];

    vstWarningsAndErrors.DeleteNode(ChildNode);
    ModelMessages.FMessages.Delete(RootIndex);
    ModelMessages.FChildNodes.Delete(RootIndex);
    ErrorMessages.Free;
  end
//  RootIndex := RootList.IndexOf(TypeOfErrorOrWarning);
//  if RootIndex >= 0 then
//  begin
//    ErrorMessages := RootList.Objects[RootIndex] as TStringList;
//    ChildNode := Children[RootIndex];
//
//    vstWarningsAndErrors.DeleteNode(ChildNode);
//    RootList.Delete(RootIndex);
//    Children.Delete(RootIndex);
//    ErrorMessages.Free;
//  end
end;

procedure TfrmErrorsAndWarnings.ShowAfterDelay;
begin
  Timer1.Enabled := True;
end;

procedure TfrmErrorsAndWarnings.GetErrorsAndWarnings(ErrorsAndWarings: TStringList);
const
  TabChar = ''#9'';
var
  Index: Integer;
  ALine: string;
  Node: PVirtualNode;
begin
  Node := vstWarningsAndErrors.GetFirst;
  while Node <> nil do
  begin
    ALine := '';
    for Index := 0 to vstWarningsAndErrors.GetNodeLevel(Node) do
    begin
      ALine := ALine + TabChar;
    end;
    ErrorsAndWarings.Add(ALine + vstWarningsAndErrors.Text[Node, 0]);
    Node := vstWarningsAndErrors.GetNext(Node);
  end;
end;

procedure TfrmErrorsAndWarnings.Timer1Timer(Sender: TObject);
begin
  inherited;
  Timer1.Enabled := False;
  Show;
end;

procedure TfrmErrorsAndWarnings.RemoveErrorGroup(Model: TBaseModel;
  const TypeOfError: string);
begin
  RemoveWarningOrErrorGroup(Model, TypeOfError, FErrorModels,
    FErrorChildNodes, FErrorModelMessageList);
  CheckDeleteRootNode(Model, FErrorNode, FErrorModels, FErrorModelMessageList);
end;

procedure TfrmErrorsAndWarnings.CheckDeleteRootNode(Model: TBaseModel;
  var Node: PVirtualNode; RootList: TStringList; ModelMessageList: TModelMessageList);
var
  ModelMessages: TModelMessages;
  RootIndex: Integer;
begin
  Assert(Model <> nil);
  ModelMessages := ModelMessageList.GetModelMessages(Model);
  if (ModelMessages.FNode <> nil)
    and not vstWarningsAndErrors.HasChildren[ModelMessages.FNode] then
  begin
    vstWarningsAndErrors.DeleteNode(ModelMessages.FNode);
    ModelMessages.FNode := nil
  end;

  if ModelMessages.FNode = nil then
  begin
    RootIndex := RootList.IndexOf(Model.DisplayName);
    if RootIndex >= 0 then
    begin
      RootList.Delete(RootIndex);
    end;
  end;

  if (Node <> nil) and not vstWarningsAndErrors.HasChildren[Node] then
  begin

    vstWarningsAndErrors.DeleteNode(Node);
    Node := nil;
  end;
end;

procedure TfrmErrorsAndWarnings.RemoveWarningGroup(Model: TBaseModel;
  const TypeOfWarning: string);
begin
  RemoveWarningOrErrorGroup(Model, TypeOfWarning, FWarningModels,
    FWarningChildNodes, FWarningModelMessageList);
  CheckDeleteRootNode(Model, FWarningNode, FWarningModels, FWarningModelMessageList);
end;

procedure TfrmErrorsAndWarnings.AddError(Model: TBaseModel;
  const Root, Error: string);
begin
  AddErrorOrWarning(Model, FErrorModels, Root, Error, FErrorNode,
    FErrorChildNodes, FErrorModelMessageList);
end;

procedure TfrmErrorsAndWarnings.AddWarning(Model: TBaseModel;
  const Root, Warning: string);
begin
  AddErrorOrWarning(Model, FWarningModels, Root, Warning,
    FWarningNode, FWarningChildNodes, FWarningModelMessageList);
end;

procedure TfrmErrorsAndWarnings.btnClearClick(Sender: TObject);
begin
  inherited;
  Clear;
end;

procedure TfrmErrorsAndWarnings.btnCopyClick(Sender: TObject);
var
  ErrorsAndWarings: TStringList;
begin
  inherited;
  ErrorsAndWarings := TStringList.Create;
  try
    GetErrorsAndWarnings(ErrorsAndWarings);
    Clipboard.AsText := ErrorsAndWarings.Text;
  finally
    ErrorsAndWarings.Free;
  end;
end;

procedure TfrmErrorsAndWarnings.btnSaveClick(Sender: TObject);
var
  ErrorsAndWarings: TStringList;
begin
  inherited;
  if sdSaveFileDlg.Execute then
  begin
    ErrorsAndWarings := TStringList.Create;
    try
      GetErrorsAndWarnings(ErrorsAndWarings);
      ErrorsAndWarings.SaveToFile(sdSaveFileDlg.FileName);
    finally
      ErrorsAndWarings.Free;
    end;
  end;
end;

//procedure TfrmErrorsAndWarnings.ClearABaseStringList(List: TStringList);
//var
//  Index: Integer;
//begin
//  for Index := List.Count - 1 downto 0 do
//  begin
//    List.Objects[Index].Free;
//  end;
//  List.Clear;
//end;

procedure TfrmErrorsAndWarnings.Clear;
begin
  vstWarningsAndErrors.Clear;
  FErrorNode := nil;
  FWarningNode := nil;
  FErrorModels.Clear;
  FWarningModels.Clear;
//  ClearABaseStringList(FErrorModels);
//  ClearABaseStringList(FWarningModels);
  FErrorChildNodes.Clear;
  FWarningChildNodes.Clear;
  FWarningModelMessageList.Clear;
  FErrorModelMessageList.Clear;
end;

procedure TfrmErrorsAndWarnings.FormCreate(Sender: TObject);
begin
  inherited;
  vstWarningsAndErrors.NodeDataSize := SizeOf(TErrorWarningRec);
  FErrorModels := TStringList.Create;
  FWarningModels := TStringList.Create;
  FErrorModelMessageList:= TModelMessageList.Create;

  FErrorChildNodes:= TList.Create;
  FWarningChildNodes:= TList.Create;
  FWarningModelMessageList:= TModelMessageList.Create;
end;

procedure TfrmErrorsAndWarnings.FormDestroy(Sender: TObject);
//var
//  Node: PVirtualNode;
begin
  inherited;
  Clear;
  FErrorModels.Free;
  FWarningModels.Free;
  FErrorChildNodes.Free;
  FWarningChildNodes.Free;
  FErrorModelMessageList.Free;
  FWarningModelMessageList.Free;
//  Node := vstWarningsAndErrors.GetFirst;
  vstWarningsAndErrors.Clear;
//  while Node <> nil do
//  begin
//    vstWarningsAndErrors.DeleteNode(Node);
//    Node := vstWarningsAndErrors.GetFirst;
//  end;
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
  result := (FErrorModels.Count > 0) or (FWarningModels.Count > 0)
end;

procedure TfrmErrorsAndWarnings.vstWarningsAndErrorsGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  Data: PErrorWarningRec;
  ParentNode: PVirtualNode;
begin
  inherited;
  // A handler for the OnGetText event is always needed as it provides the tree with the string data to display.
  // Note that we are now using string instead of WideString.
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    if Node = FErrorNode then
    begin
      CellText := StrErrors;
    end
    else if Node = FWarningNode then
    begin
      CellText := StrWarnings;
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
  CellCaption: String;
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

{ TModelMessages }

constructor TModelMessages.Create(Model: TBaseModel);
begin
  FModel := Model;
  FMessages := TStringList.Create;
  FChildNodes:= TList.Create;
end;

destructor TModelMessages.Destroy;
var
  Index: Integer;
begin
  FChildNodes.Free;
  for Index := FMessages.Count - 1 downto 0 do
  begin
    FMessages.Objects[Index].Free;
  end;
  FMessages.Free;
  inherited;
end;

{ TModelMessageList }

procedure TModelMessageList.Clear;
begin
  FList.Clear;
end;

constructor TModelMessageList.Create;
begin
  FList := TObjectList.Create;
end;

destructor TModelMessageList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TModelMessageList.GetModelMessages(Model: TBaseModel): TModelMessages;
var
  Index: Integer;
  Item: TModelMessages;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    Item := FList[Index];
    if Item.FModel = Model then
    begin
      result := Item;
      Exit;
    end;
  end;
  result := TModelMessages.Create(Model);
  FList.Add(result);
end;

initialization

finalization
  FfrmErrorsAndWarnings.Free;

end.
