{@name defines classes and procedures that are used to place objects
 in a TTreeView or TVirtualStringTree component
 in a way that reflects their classification.

@author(Richard B. Winston <rbwinst@usgs.gov>)}
unit ClassificationUnit;

interface

uses SysUtils, StrUtils, Classes, ComCtrls, VirtualTrees;

Type
  {@name is an abstract base class used in @link(TClassificationList),
  and @link(ClassifyListedObjects) to arrange items in a
  hierarchical classification.
  Descendants include @link(TDataArrayEdit) @link(TVariableEdit),
  @link(TScreenObjectDataEdit) and @link(TDataSetClassification).}
  TClassificationObject = class(TObject)
  public
    function ClassificationName: string; virtual; abstract;
    function FullClassification: string; virtual; abstract;
  end;

  TDummyClassification = class(TClassificationObject)
  private
    FClassificationName: string;
  public
    function ClassificationName: string; override;
    function FullClassification: string; override;
    Constructor Create(const StoredClassification: string);
  end;

  // @name is a list of @link(TClassificationObject)s. @name does not own the
  // @link(TClassificationObject)s it contains.
  TClassificationList = class(TObject)
  private
    // @name is a private TList that stores @link(TClassificationObject)s.
    FList: TList;
    // @name returns @link(FList).Count.
    function GetCount: integer;
    // @name returns @link(FList)[Index].
    function GetItems(Index: integer): TClassificationObject;
    // @name sets @link(FList)[Index].
    procedure SetItems(Index: integer; const Value: TClassificationObject);
  public
    // @name calls @link(FList).Count.
    procedure Add(Item: TClassificationObject);
    // @name calls @link(FList).Count.
    property Count: integer read GetCount;
    // @name restores and sets @link(TClassificationObject)s.
    property Items[Index: integer]: TClassificationObject read GetItems
      write SetItems; default;
    // @name creates @link(FList).
    constructor Create;
    // @name destroys @link(FList).
    Destructor Destroy; override;
    // @name returns @link(FList).IndexOf(Item)
    Function IndexOf(Item: TClassificationObject): integer;
    // @name calls FList.Pack.
    procedure Pack;
  end;

  PClassificationNodeData = ^TClassificationNodeData;
  TClassificationNodeData = record
    ClassificationObject: TClassificationObject;
  end;

  
    {@name fills ClassificationList with the classifications of all
    the @link(TClassificationObject)s in ClassificationObjects
    suitable for storage in
    a TTreeView component. The number of tabs before
    each item indicate parent child relationships.  when the classification
    represents a @link(TClassificationObject) rather than just a parent node,
    the @link(TClassificationObject) is stored in the corresponding Objects
    property of ClassificationList.

    Each member of SpecialObjects is a group of @link(TClassificationObject)s
    that must be classified in a special order that is different from
    alphabetical order.
    }
procedure ClassifyListedObjects(
  const ClassificationList: TStringList; ClassificationObjects: TClassificationList;
  SpecialObjects: array of TClassificationList);

{@name creates nodes in TreeView based on Classifications.
  Classifications is typically filled by @link(ClassifyListedObjects).
  each object in Classifications.Objects must be a @link(TClassificationObject)
  or nil. The Data property of each node will be the corresponding
  @link(TClassificationObject).
  IndentationOffset is the level of indentation of the first node.
  TreeView is the TTTreeView to which nodes are added.
  If any of the @link(TClassificationObject) that are added have
  a @link(TClassificationObject.ClassificationName) that is the same as
  SelectedName, that node will be selected.}
procedure CreateClassifiedNodes(Classifications: TStringList;
  IndentationOffset: Integer; TreeView: TTreeView; const SelectedName: string);

procedure CreateClassifiedVirtualNodes(Classifications: TStringList;
  IndentationOffset: Integer; TreeView: TVirtualStringTree;
  const SelectedName: string; DummyObjects: TList);

function CompareStrings(List: TStringList; Index1, Index2: Integer): Integer;
  
implementation

uses IntListUnit, DataSetUnit;

function CompareStrings(List: TStringList; Index1, Index2: Integer): Integer;
const
  NumDigits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
var
  String1: string;
  String2: string;
  Digits1: string;
  Digits2: string;
  LastChar: Integer;
  Number1: Integer;
  Number2: Integer;
  procedure ExtractDigits(var AString, Digits: string);
  var
    CharIndex: Integer;
    AChar: Char;
  begin
    LastChar := Length(AString)+1;
    for CharIndex := Length(AString) downto 1 do
    begin
      AChar := AString[CharIndex];
      if not CharInSet(AChar, NumDigits) then
      begin
        break;
      end;
      LastChar := CharIndex;
    end;
    Digits := Copy(AString, LastChar, MAXINT);
    AString := Copy(AString, 1, LastChar-1);
  end;
begin
  if Index1 = Index2 then
  begin
    result := 0;
  end
  else
  begin
    String1 := List[Index1];
    String2 := List[Index2];
    ExtractDigits(String1, Digits1);
    ExtractDigits(String2, Digits2);
    result := AnsiCompareText(String1, String2);
    if result = 0 then
    begin
      if Digits1 = '' then
      begin
        result := -1
      end
      else if Digits2 = '' then
      begin
        result := 1
      end
      else
      begin
        Number1 := StrToInt(Digits1);
        Number2 := StrToInt(Digits2);
        result := Number1 - Number2;
      end;
    end;
  end;
end;

procedure ClassifyListedObjects(
  const ClassificationList: TStringList; ClassificationObjects: TClassificationList;
  SpecialObjects: array of TClassificationList);
var
  FullClassificationList: TStringList;
  LocalClassificationList: TStringList;
  TabIndex: Integer;
  CIndex: Integer;
  Classification: string;
  FullClassificaton: string;
  Position: Integer;
  LayerGroupPositions: TIntegerList;
  ClassificationObject: TClassificationObject;
  Index: Integer;
  SortedObjects: TStringList;
  SpecialGroupList: TClassificationList;
  GroupIndex: integer;
begin
  FullClassificationList := TStringList.Create;
  LocalClassificationList := TStringList.Create;
  SortedObjects := TStringList.Create;
  try
    for Index := 0 to ClassificationObjects.Count - 1 do
    begin
      ClassificationObject := ClassificationObjects[Index];
      SortedObjects.AddObject(
        ClassificationObject.FullClassification + '|'
        + ClassificationObject.ClassificationName, ClassificationObject);
    end;
    SortedObjects.CustomSort(CompareStrings);

    for GroupIndex := 0 to Length(SpecialObjects)-1 do
    begin
      SpecialGroupList := SpecialObjects[GroupIndex];
      if (SpecialGroupList.Count > 0) then
      begin
        LayerGroupPositions := TIntegerList.Create;
        try
          for Index := 0 to SpecialGroupList.Count - 1 do
          begin
            ClassificationObject := SpecialGroupList[Index];
            if ClassificationObject <> nil then
            begin
              Position := SortedObjects.IndexOfObject(ClassificationObject);
              Assert(Position >= 0);
              LayerGroupPositions.Add(Position);
//            end
//            else
//            begin
//              LayerGroupPositions.Add(-1);
            end;
          end;
          LayerGroupPositions.Sort;
          for Index := 0 to SpecialGroupList.Count - 1 do
          begin
            ClassificationObject := SpecialGroupList[Index];
            if ClassificationObject <> nil then
            begin
              Position := LayerGroupPositions[Index];
              SortedObjects[Position] :=
                ClassificationObject.FullClassification + '|'
                + ClassificationObject.ClassificationName;
              SortedObjects.Objects[Position] := ClassificationObject;
            end;
          end;
        finally
          LayerGroupPositions.Free;
        end;
      end;
    end;

    for Index := 0 to SortedObjects.Count - 1 do
    begin
      ClassificationObject := SortedObjects.Objects[Index]
        as TClassificationObject;
      FullClassificaton := ClassificationObject.FullClassification;
      FullClassificaton := ReplaceStr(FullClassificaton, '|', ''#13'');
      LocalClassificationList.Text := FullClassificaton;
      Classification := '';
      FullClassificaton := '';
      for CIndex := 0 to LocalClassificationList.Count - 1 do
      begin
        Classification := LocalClassificationList[CIndex];
        if CIndex = 0 then
        begin
          FullClassificaton := Classification;
        end
        else
        begin
          FullClassificaton := FullClassificaton + '|' + Classification;
        end;
        for TabIndex := 0 to CIndex - 1 do
        begin
          Classification := ''#9'' + Classification;
        end;
        if FullClassificationList.IndexOf(FullClassificaton) < 0 then
        begin
          ClassificationList.Add(Classification);
          FullClassificationList.Add(FullClassificaton);
        end;
        if CIndex = LocalClassificationList.Count - 1 then
        begin
          Classification := ClassificationObject.ClassificationName;
          for TabIndex := 0 to CIndex do
          begin
            Classification := ''#9'' + Classification;
          end;
          ClassificationList.AddObject(Classification, ClassificationObject);
        end;
      end;
    end;
  finally
    SortedObjects.Free;
    LocalClassificationList.Free;
    FullClassificationList.Free;
  end;
end;

procedure CreateClassifiedNodes(Classifications: TStringList;
  IndentationOffset: Integer; TreeView: TTreeView; const SelectedName: string);
var
  Node: TTreeNode;
  ClassifiedObject: TClassificationObject;
  ParentNode: TTreeNode;
  Indentation: Integer;
  TabCount: Integer;
  Classification: string;
  LocalIndex: Integer;
  NodeList: TList;
begin
  NodeList := TList.Create;
  try
    for LocalIndex := 0 to Classifications.Count - 1 do
    begin
      Classification := Classifications[LocalIndex];
      Assert(Length(Classification) > 0);
      TabCount := Length(Classification) - Length(Trim(Classification));
      Classification := Trim(Classification);
      Indentation := TabCount - IndentationOffset;
      Assert(Indentation >= 0);
      if NodeList.Count > Indentation then
      begin
        NodeList.Count := Indentation;
      end;
      if NodeList.Count > 0 then
      begin
        ParentNode := NodeList.Last;
      end
      else
      begin
        ParentNode := nil;
      end;
      ClassifiedObject := Classifications.Objects[LocalIndex]
        as TClassificationObject;
      if ClassifiedObject = nil then
      begin
        Node := TreeView.Items.AddChild(ParentNode, Classification);
        NodeList.Add(Node);
      end
      else
      begin
        Node := TreeView.Items.AddChildObject(ParentNode,
          ClassifiedObject.ClassificationName, ClassifiedObject);
        if ClassifiedObject.ClassificationName = SelectedName then
        begin
          TreeView.Selected := Node;
        end;
      end;
    end;
  finally
    NodeList.Free;
  end;
end;

procedure CreateClassifiedVirtualNodes(Classifications: TStringList;
  IndentationOffset: Integer; TreeView: TVirtualStringTree;
  const SelectedName: string; DummyObjects: TList);
var
  ClassifiedObject: TClassificationObject;
  ParentNode: PVirtualNode;
  Indentation: Integer;
  TabCount: Integer;
  Classification: string;
  LocalIndex: Integer;
  NodeList: TList;
  Dummy : TDummyClassification;
  Node: PVirtualNode;
  NodeData: PClassificationNodeData;
  SelectParentNode: PVirtualNode;
begin
  NodeList := TList.Create;
  try
    for LocalIndex := 0 to Classifications.Count - 1 do
    begin
      Classification := Classifications[LocalIndex];
      Assert(Length(Classification) > 0);
      TabCount := Length(Classification) - Length(Trim(Classification));
      Classification := Trim(Classification);
      Indentation := TabCount - IndentationOffset;
      Assert(Indentation >= 0);
      if NodeList.Count > Indentation then
      begin
        NodeList.Count := Indentation;
      end;
      if NodeList.Count > 0 then
      begin
        ParentNode := NodeList.Last;
      end
      else
      begin
        ParentNode := nil;
      end;
      ClassifiedObject := Classifications.Objects[LocalIndex]
        as TClassificationObject;
      if ClassifiedObject = nil then
      begin
        Dummy := TDummyClassification.Create(Classification);
        DummyObjects.Add(Dummy);
        Node := TreeView.AddChild(ParentNode);
        NodeList.Add(Node);
        NodeData := TreeView.GetNodeData(Node);
        NodeData.ClassificationObject := Dummy;
      end
      else
      begin
        Node := TreeView.AddChild(ParentNode);
        NodeData := TreeView.GetNodeData(Node);
        NodeData.ClassificationObject := ClassifiedObject;
        TreeView.Selected[Node] :=
          ClassifiedObject.ClassificationName = SelectedName ;
        if TreeView.Selected[Node] then
        begin
          SelectParentNode := Node.Parent;
          while SelectParentNode <> nil do
          begin
            if NodeList.IndexOf(SelectParentNode) >= 0 then
            begin
              TreeView.Expanded[SelectParentNode] := True;
              SelectParentNode := SelectParentNode.Parent;
            end
            else
            begin
              break;
            end;
          end;
        end;
      end;
    end;
  finally
    NodeList.Free;
  end;
end;


{ TClassificationList }

procedure TClassificationList.Add(Item: TClassificationObject);
begin
  FList.Add(Item);
end;

constructor TClassificationList.Create;
begin
  inherited;
  FList:= TList.Create;
end;

destructor TClassificationList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TClassificationList.GetCount: integer;
begin
  result := FList.Count;
end;

function TClassificationList.GetItems(Index: integer): TClassificationObject;
begin
  result := FList[Index];
end;

function TClassificationList.IndexOf(Item: TClassificationObject): integer;
begin
  result := FList.IndexOf(Item);
end;

procedure TClassificationList.Pack;
begin
  FList.Pack;
end;

procedure TClassificationList.SetItems(Index: integer;
  const Value: TClassificationObject);
begin
  FList[Index] := Value;
end;

{ TDummyClassification }

function TDummyClassification.ClassificationName: string;
begin
  result := FClassificationName;
end;

constructor TDummyClassification.Create(const StoredClassification: string);
begin
  inherited Create;
  FClassificationName := StoredClassification;
end;

function TDummyClassification.FullClassification: string;
begin
  result := '';
end;

{ TClassificationObject }

end.
