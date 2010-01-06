unit IntervalTree;

interface

uses SysUtils, Classes, GoPhastTypes;

Type
  TIntervalEvent = procedure(Subject: TObject;
    out LowerBoundary, UpperBoundary: double) of object;
  TCheckObjectEvent = procedure(Subject: TObject; Point: TOneDRealArray;
    var Accept: boolean) of object;
  TSearchEvent = procedure(Sender: TObject; Subject: TObject);


  TIntervalDefinition = record
    LowerBoundary: double;
    UpperBoundary: double;
    OnFindObjectInterval: TIntervalEvent;
//    OnCheckObject: TCheckObjectEvent;
  end;

  TIntDefArray = array of TIntervalDefinition;

  IIntervalTree = interface
    procedure Add(AnObject: TObject);
    procedure FindContainingObjects(Point, FullPoint: TOneDRealArray; List: TList);
    function ArraySize: integer;
  end;

  TRbwIntervalTree = class(TObject)
  private
    FIntervalTree : IIntervalTree;
  public
    Constructor Create(IntervalDefinitions: TIntDefArray);
    Destructor Destroy; override;
    procedure Add(AnObject: TObject);
    procedure FindContainingObjects(Point: TOneDRealArray; List: TList);
  end;

  TSearchInterval = record
    LowerBoundary: double;
    UpperBoundary: double;
//    OnFindObjectInterval: TSearchEvent;
  end;

  TSearchArray = array of TSearchInterval;

  TRangeTreeLeaf = class(TObject)
  private
    FIsLeft: boolean;
  public
    function GetCoordinate(Depth: integer): double; virtual; abstract;
  End;

  TRangeTreeLeafList = class(TObject)
  private
    FList: TList;
    function GetCapacity: integer;
    function GetItem(Index: Integer): TRangeTreeLeaf;
    procedure SetCapacity(const Value: integer);
    procedure SetItem(Index: Integer; const Value: TRangeTreeLeaf);
    function GetCount: integer;
    function First: TRangeTreeLeaf;
    function Last: TRangeTreeLeaf;
    function GetOwnsObjects: boolean;
    procedure SetOwnsObjects(const Value: boolean);
  protected
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    property OwnsObjects: boolean read GetOwnsObjects write SetOwnsObjects;
  public
    property Items[Index: Integer]: TRangeTreeLeaf read GetItem write SetItem; default;
    function CoordinateCount: integer; virtual; abstract;
    Constructor Create;
    Destructor Destroy; override;
    function Add(Item: TRangeTreeLeaf): integer;
    procedure Delete(Index: integer);
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount;
  end;

  TRangeTreeLeafLists = class(TObject)
  private
    FList: TList;
    function GetCount: integer;
    function GetItem(Index: Integer): TRangeTreeLeafList;
    procedure SetItem(Index: Integer; const Value: TRangeTreeLeafList);
    function GetCapacity: integer;
    procedure SetCapacity(const Value: integer);
  public
    property Items[Index: Integer]: TRangeTreeLeafList read GetItem write SetItem; default;
    function Add(Item: TRangeTreeLeafList): integer;
    property Count: integer read GetCount;
    Constructor Create;
    Destructor Destroy; override;
    property Capacity: integer read GetCapacity write SetCapacity;
  end;

  TRbwRangeTree = class;

  TInternalRangeTree = class(TObject)
  private
    FDepthIndex: integer;
    FLeftTree: TInternalRangeTree;
    FRightTree: TInternalRangeTree;
    FCenterTree: TInternalRangeTree;
    FLeafLists: TRangeTreeLeafLists;
    FParentTree: TRbwRangeTree;
    FBreakCoordinate: Double;
  public
    Constructor Create(ParentTree: TRbwRangeTree; LeafLists: TRangeTreeLeafLists; DepthIndex: integer);
    Destructor Destroy; override;
    procedure Search(const Intervals: TSearchArray);
    procedure EnsureCenterTree;
    procedure EnsureLeftRightTrees;
  end;

  TRbwRangeTree = class(TObject)
  private
    FInternalTree: TInternalRangeTree;
    FLeafList: TRangeTreeLeafList;
    FResultList: TList;
  public
    Constructor Create(List: TRangeTreeLeafList);
    Destructor Destroy; override;
    function Search(const Intervals: TSearchArray): TList;
  end;

implementation

uses Math, Contnrs;

{ TIntervalTree }

type
  TIntervalTree = class(TInterfacedObject, IIntervalTree)
  private
    FLeftTree: TIntervalTree;
    FRightTree: TIntervalTree;
    FCenterTree: TIntervalTree;
    FIntervalDefinitions: TIntDefArray;
    FCenterList: TList;
    FCenter: double;
    FContentsUpperLimit: double;
    FContentsLowerLimit: double;
    FFullPoint: TOneDRealArray;
  public
    function ArraySize: integer;
    Constructor InternalCreate(IntervalDefinitions: TIntDefArray);
    Destructor Destroy; override;
    procedure Add(AnObject: TObject);
    procedure FindContainingObjects(Point, FullPoint: TOneDRealArray; List: TList);
    class function Create(IntervalDefinitions: TIntDefArray): IIntervalTree;
  end;

  TInternalRangeTreeLeafList = class(TRangeTreeLeafList)
  private
    FMaxDepth: integer;
  public
    Constructor Create(MaximumDepth: integer);
    function CoordinateCount: integer; override;
  end;

procedure TIntervalTree.Add(AnObject: TObject);
var
  LowerBoundary, UpperBoundary: double;
  SubTreeIntervalDefinitions: TIntDefArray;
begin
  FIntervalDefinitions[0].OnFindObjectInterval(AnObject,
    LowerBoundary, UpperBoundary);
  Assert(LowerBoundary <= UpperBoundary);
  if LowerBoundary < UpperBoundary then
  begin
    if LowerBoundary > FCenter then
    begin
      if FRightTree = nil then
      begin
        SubTreeIntervalDefinitions := FIntervalDefinitions;
        // Make SubTreeIntervalDefinitions into a new array.
        SetLength(SubTreeIntervalDefinitions, Length(SubTreeIntervalDefinitions));
        SubTreeIntervalDefinitions[0].LowerBoundary := FCenter;
        FRightTree := TIntervalTree.InternalCreate(SubTreeIntervalDefinitions);
      end;
      FRightTree.Add(AnObject);
    end
    else if UpperBoundary < FCenter then
    begin
      if FLeftTree = nil then
      begin
        SubTreeIntervalDefinitions := FIntervalDefinitions;
        // Make SubTreeIntervalDefinitions into a new array.
        SetLength(SubTreeIntervalDefinitions, Length(SubTreeIntervalDefinitions));
        SubTreeIntervalDefinitions[0].UpperBoundary := FCenter;
        FLeftTree := TIntervalTree.InternalCreate(SubTreeIntervalDefinitions);
      end;
      FLeftTree.Add(AnObject);
    end
    else if Length(FIntervalDefinitions) > 1 then
    begin
      if FCenterTree = nil then
      begin
        SetLength(SubTreeIntervalDefinitions, Length(FIntervalDefinitions)-1);
        Move(FIntervalDefinitions[1], SubTreeIntervalDefinitions[0],
          Length(SubTreeIntervalDefinitions) * SizeOf(TIntervalDefinition));
        FCenterTree := TIntervalTree.InternalCreate(SubTreeIntervalDefinitions);
      end;
      FCenterTree.Add(AnObject);
      if FContentsUpperLimit < UpperBoundary then
      begin
        FContentsUpperLimit := UpperBoundary
      end;
      if FContentsLowerLimit > LowerBoundary then
      begin
        FContentsLowerLimit := LowerBoundary
      end;
    end
    else
    begin
      if FCenterList = nil then
      begin
        FCenterList := TList.Create;
      end;
      FCenterList.Add(AnObject);
      if FContentsUpperLimit < UpperBoundary then
      begin
        FContentsUpperLimit := UpperBoundary
      end;
      if FContentsLowerLimit > LowerBoundary then
      begin
        FContentsLowerLimit := LowerBoundary
      end;
    end;
  end else if Length(FIntervalDefinitions) > 1 then
  begin
    if (LowerBoundary > FCenter) and (FRightTree <> nil) then
    begin
      FRightTree.Add(AnObject);
    end
    else if (UpperBoundary < FCenter) and (FLeftTree <> nil) then
    begin
      FLeftTree.Add(AnObject);
    end
    else
    begin
      if FCenterTree = nil then
      begin
        SetLength(SubTreeIntervalDefinitions, Length(FIntervalDefinitions)-1);
        Move(FIntervalDefinitions[1], SubTreeIntervalDefinitions[0],
          Length(SubTreeIntervalDefinitions) * SizeOf(TIntervalDefinition));
        FCenterTree := TIntervalTree.InternalCreate(SubTreeIntervalDefinitions);
      end;
      FCenterTree.Add(AnObject);
      if FContentsUpperLimit < UpperBoundary then
      begin
        FContentsUpperLimit := UpperBoundary
      end;
      if FContentsLowerLimit > LowerBoundary then
      begin
        FContentsLowerLimit := LowerBoundary
      end;
    end;
  end
  else
  begin
    if FCenterList = nil then
    begin
      FCenterList := TList.Create;
    end;
    FCenterList.Add(AnObject);
    if FContentsUpperLimit < UpperBoundary then
    begin
      FContentsUpperLimit := UpperBoundary
    end;
    if FContentsLowerLimit > LowerBoundary then
    begin
      FContentsLowerLimit := LowerBoundary
    end;
  end;
end;

constructor TIntervalTree.InternalCreate(IntervalDefinitions: TIntDefArray);
begin
  FIntervalDefinitions := IntervalDefinitions;
  FCenter := (FIntervalDefinitions[0].LowerBoundary
    + FIntervalDefinitions[0].UpperBoundary) / 2;
  FContentsUpperLimit := FCenter;
  FContentsLowerLimit := FCenter;
end;

function TIntervalTree.ArraySize: integer;
begin
  result := Length(FIntervalDefinitions);
end;

class function TIntervalTree.Create(
  IntervalDefinitions: TIntDefArray): IIntervalTree;
begin
  result := TIntervalTree.InternalCreate(IntervalDefinitions);
end;

destructor TIntervalTree.Destroy;
begin
  FLeftTree.Free;
  FRightTree.Free;
  FCenterTree.Free;
  FCenterList.Free;
  inherited;
end;

procedure TIntervalTree.FindContainingObjects(Point, FullPoint: TOneDRealArray; List: TList);
var
  NewPoint: TOneDRealArray;
  Index: Integer;
  AnObject: TObject;
  LowerBoundary: Double;
  UpperBoundary: Double;
  OldCount: Integer;
//  Accept: Boolean;
begin
  FFullPoint := FullPoint;
  if Point[0] > FCenter then
  begin
    if FRightTree <> nil then
    begin
      FRightTree.FindContainingObjects(Point, FFullPoint, List);
    end;
  end
  else if Point[0] < FCenter then
  begin
    if FLeftTree <> nil then
    begin
      FLeftTree.FindContainingObjects(Point, FFullPoint, List);
    end;
  end;
  if (FContentsUpperLimit >= Point[0]) and (FContentsLowerLimit <= Point[0]) then
  begin
    if FCenterTree <> nil then
    begin

      SetLength(NewPoint, Length(Point)-1);
      Move(Point[1], NewPoint[0], Length(NewPoint)*SizeOf(double));
      OldCount := List.Count;
      FCenterTree.FindContainingObjects(NewPoint, FFullPoint, List);
      for Index := OldCount to List.Count - 1 do
      begin
        AnObject := List[Index];
        FIntervalDefinitions[0].OnFindObjectInterval(AnObject,
          LowerBoundary, UpperBoundary);
        if (LowerBoundary > Point[0]) or (UpperBoundary < Point[0]) then
        begin
          List[Index] := nil;
        end;
      end;
    end;
    if FCenterList <> nil then
    begin
      for Index := 0 to FCenterList.Count - 1 do
      begin
        AnObject := FCenterList[Index];
        FIntervalDefinitions[0].OnFindObjectInterval(AnObject,
          LowerBoundary, UpperBoundary);
        if (LowerBoundary <= Point[0]) and (UpperBoundary >= Point[0]) then
        begin
//          Accept := True;
//          if Assigned(FIntervalDefinitions[0].OnCheckObject) then
//          begin
//            FIntervalDefinitions[0].OnCheckObject(AnObject,
//              FFullPoint, Accept)
//          end;
//          if Accept then
//          begin
            List.Add(AnObject);
//          end;
        end;
      end;
    end;
  end;
end;

{ TRbwIntervalTree }

procedure TRbwIntervalTree.Add(AnObject: TObject);
begin
  FIntervalTree.Add(AnObject);
end;

constructor TRbwIntervalTree.Create(IntervalDefinitions: TIntDefArray);
var
  Index: Integer;
begin
  Assert(Length(IntervalDefinitions) > 0);
  SetLength(IntervalDefinitions, Length(IntervalDefinitions));
  for Index := 0 to Length(IntervalDefinitions) - 1 do
  begin
    Assert(Assigned(IntervalDefinitions[Index].OnFindObjectInterval));
    Assert(IntervalDefinitions[Index].LowerBoundary < IntervalDefinitions[Index].UpperBoundary);
  end;
  FIntervalTree := TIntervalTree.Create(IntervalDefinitions);
end;

destructor TRbwIntervalTree.Destroy;
begin
  FIntervalTree := nil;
  inherited;
end;

procedure TRbwIntervalTree.FindContainingObjects(Point: TOneDRealArray;
  List: TList);
begin
  Assert(Length(Point) = FIntervalTree.ArraySize);
  FIntervalTree.FindContainingObjects(Point, Point, List);
  List.Pack;
end;

var
  GlobalDepth: integer = -1;

function CompareLeaves(Item1, Item2: Pointer): Integer;
var
  Leaf1, Leaf2: TRangeTreeLeaf;
begin
  Leaf1 := Item1;
  Leaf2 := Item2;
  result := Sign(Leaf1.GetCoordinate(GlobalDepth)
    - Leaf2.GetCoordinate(GlobalDepth));
end;

{ TRangeTreeLeafList }

function TRangeTreeLeafList.Add(Item: TRangeTreeLeaf): integer;
begin
  result := FList.Add(Item);
end;

constructor TRangeTreeLeafList.Create;
begin
  inherited;
  FList:= TObjectList.Create;
  OwnsObjects := False;
end;

procedure TRangeTreeLeafList.Delete(Index: integer);
begin
  FList.Delete(Index);
end;

destructor TRangeTreeLeafList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TRangeTreeLeafList.First: TRangeTreeLeaf;
begin
  result := FList.First;
end;

function TRangeTreeLeafList.GetCapacity: integer;
begin
  result := FList.Capacity;
end;

function TRangeTreeLeafList.GetCount: integer;
begin
  result := FList.Count;
end;

function TRangeTreeLeafList.GetItem(Index: Integer): TRangeTreeLeaf;
begin
  result := FList.Items[Index];
end;

function TRangeTreeLeafList.GetOwnsObjects: boolean;
begin
  result := TObjectList(FList).OwnsObjects;
end;

function TRangeTreeLeafList.Last: TRangeTreeLeaf;
begin
  result := FList.Last;
end;

procedure TRangeTreeLeafList.Pack;
begin
  FList.Pack;
end;

procedure TRangeTreeLeafList.SetCapacity(const Value: integer);
begin
  FList.Capacity := Value;
end;

procedure TRangeTreeLeafList.SetItem(Index: Integer;
  const Value: TRangeTreeLeaf);
begin
  FList.Items[Index] := Value;
end;

procedure TRangeTreeLeafList.SetOwnsObjects(const Value: boolean);
begin
  TObjectList(FList).OwnsObjects := Value;
end;

procedure TRangeTreeLeafList.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

{ TRangeTreeLeafLists }

function TRangeTreeLeafLists.Add(Item: TRangeTreeLeafList): integer;
begin
  result := FList.Add(Item);
end;

constructor TRangeTreeLeafLists.Create;
begin
  inherited;
  FList:= TObjectList.Create;
end;

destructor TRangeTreeLeafLists.Destroy;
begin
  FList.Free;
  inherited;
end;

function TRangeTreeLeafLists.GetCapacity: integer;
begin
  result := FList.Capacity;
end;

function TRangeTreeLeafLists.GetCount: integer;
begin
  result := FList.Count;
end;

function TRangeTreeLeafLists.GetItem(Index: Integer): TRangeTreeLeafList;
begin
  result := FList[Index];
end;

procedure TRangeTreeLeafLists.SetCapacity(const Value: integer);
begin
  FList.Capacity := Value;
end;

procedure TRangeTreeLeafLists.SetItem(Index: Integer;
  const Value: TRangeTreeLeafList);
begin
  FList[Index] := Value;
end;

{ TInternalRangeTree }

constructor TInternalRangeTree.Create(ParentTree: TRbwRangeTree;
  LeafLists: TRangeTreeLeafLists; DepthIndex: integer);
begin
  FParentTree := ParentTree;
  FLeafLists := LeafLists;
  FDepthIndex := DepthIndex;
end;

destructor TInternalRangeTree.Destroy;
begin
  FLeafLists.Free;
  FLeftTree.Free;
  FRightTree.Free;
  FCenterTree.Free;
  inherited;
end;

procedure TInternalRangeTree.EnsureCenterTree;
var
  LeafLists: TRangeTreeLeafLists;
  ListIndex: Integer;
  List: TRangeTreeLeafList;
  NewList: TInternalRangeTreeLeafList;
  LeafIndex: Integer;
  Leaf: TRangeTreeLeaf;
begin
  if FCenterTree = nil then
  begin
    LeafLists := TRangeTreeLeafLists.Create;
    LeafLists.Capacity := FLeafLists.Count;
    for ListIndex := 0 to FLeafLists.Count - 1 do
    begin
      List := FLeafLists[ListIndex];

      NewList := TInternalRangeTreeLeafList.Create(List.CoordinateCount);
      LeafLists.Add(NewList);
      NewList.Capacity := List.Count;
      for LeafIndex := 0 to List.Count - 1 do
      begin
        Leaf := List[LeafIndex];
        NewList.Add(Leaf)
      end;
    end;
    FCenterTree := TInternalRangeTree.Create(FParentTree, LeafLists, Succ(FDepthIndex));
  end;
end;

procedure TInternalRangeTree.EnsureLeftRightTrees;
var
  List: TRangeTreeLeafList;
  BreakIndex: Integer;
  Index: Integer;
  Leaf: TRangeTreeLeaf;
  LeftLeafLists: TRangeTreeLeafLists;
  RightLeafLists: TRangeTreeLeafLists;
  ListIndex: Integer;
  NewLeftList: TRangeTreeLeafList;
  NewRightList: TRangeTreeLeafList;
  LeafIndex: Integer;
begin
  if FLeftTree = nil then
  begin
    List := FLeafLists[FDepthIndex];
    BreakIndex := List.Count div 2 -1;
    Leaf := List[BreakIndex];
    FBreakCoordinate := Leaf.GetCoordinate(FDepthIndex);
    for Index := 0 to BreakIndex do
    begin
      Leaf := List[Index];
      Leaf.FIsLeft := True;
    end;
    for Index := BreakIndex + 1 to List.Count - 1 do
    begin
      Leaf := List[Index];
      Leaf.FIsLeft := False;
    end;
    LeftLeafLists := TRangeTreeLeafLists.Create;
    LeftLeafLists.Capacity := FLeafLists.Count;
    if List.Count > 1 then
    begin
      RightLeafLists := TRangeTreeLeafLists.Create;
      RightLeafLists.Capacity := FLeafLists.Count;
    end
    else
    begin
      RightLeafLists := nil;
    end;
    for ListIndex := 0 to FLeafLists.Count - 1 do
    begin
      List := FLeafLists[ListIndex];

      NewLeftList := TInternalRangeTreeLeafList.Create(List.CoordinateCount);
      LeftLeafLists.Add(NewLeftList);
      NewLeftList.Capacity := BreakIndex + 1;

      if List.Count > 1 then
      begin
        NewRightList := TInternalRangeTreeLeafList.Create(List.CoordinateCount);
        RightLeafLists.Add(NewRightList);
        NewRightList.Capacity := BreakIndex + 1;
      end
      else
      begin
        NewRightList := nil;
      end;
      for LeafIndex := 0 to List.Count - 1 do
      begin
        Leaf := List[LeafIndex];
        if Leaf.FIsLeft then
        begin
          NewLeftList.Add(Leaf)
        end
        else
        begin
          NewRightList.Add(Leaf)
        end;
      end;
    end;
    FLeftTree := TInternalRangeTree.Create(FParentTree, LeftLeafLists, FDepthIndex);
    if RightLeafLists <> nil then
    begin
      FRightTree := TInternalRangeTree.Create(FParentTree, RightLeafLists, FDepthIndex);
    end;
  end;
end;

procedure TInternalRangeTree.Search(const Intervals: TSearchArray);
var
  List: TRangeTreeLeafList;
  FirstLeaf: TRangeTreeLeaf;
  LastLeaf: TRangeTreeLeaf;
  LeftCoordinate: Double;
  LeftContained: Boolean;
  LeafIndex: Integer;
  Interval: TSearchInterval;
  RightCoordinate: Double;
  RightContained: Boolean;
begin
  List := FLeafLists[FDepthIndex];
  Assert(List.Count > 0);

  FirstLeaf := List.First;
  Interval := Intervals[FDepthIndex];
  LeftContained := False;
  LeftCoordinate := FirstLeaf.GetCoordinate(FDepthIndex);
  if (Interval.LowerBoundary <= LeftCoordinate)
    and (LeftCoordinate <= Interval.UpperBoundary) then
  begin
    LeftContained := True;
  end;

  LastLeaf := List.Last;
  RightContained := False;
  RightCoordinate := LastLeaf.GetCoordinate(FDepthIndex);
  if (Interval.LowerBoundary <= RightCoordinate)
    and (RightCoordinate <= Interval.UpperBoundary) then
  begin
    RightContained := True;
  end;
  
  if LeftContained and RightContained then
  begin
    if FDepthIndex < List.CoordinateCount -1 then
    begin
      // Search Center tree
      FreeAndNil(FLeftTree);
      FreeAndNil(FRightTree);
      EnsureCenterTree;
      FCenterTree.Search(Intervals);
    end
    else
    begin
      for LeafIndex := 0 to List.Count - 1 do
      begin
        FParentTree.FResultList.Add(List[LeafIndex]);
      end;
    end;
  end
  else if (RightCoordinate >= Interval.LowerBoundary)
    or (LeftCoordinate <= Interval.UpperBoundary) then
  begin
    if LeftCoordinate <> RightCoordinate then
    begin
      FreeAndNil(FCenterTree);
      // search left and right tree
      EnsureLeftRightTrees;
      if (Interval.LowerBoundary <= FBreakCoordinate) then
      begin
        FLeftTree.Search(Intervals);
      end;
      if (FRightTree <> nil) and (FBreakCoordinate <= Interval.UpperBoundary) then
      begin
        FRightTree.Search(Intervals);
      end;
    end;
  end;
end;

{ TRbwRangeTree }

constructor TRbwRangeTree.Create(List: TRangeTreeLeafList);
var
  LeafLists: TRangeTreeLeafLists;
  ListIndex: Integer;
  LeafIndex: Integer;
  NewList: TRangeTreeLeafList;
begin
  FResultList := TList.Create;
  FLeafList := List;
  LeafLists:= TRangeTreeLeafLists.Create;
  for ListIndex := 0 to List.CoordinateCount - 1 do
  begin
    NewList := TInternalRangeTreeLeafList.Create(List.CoordinateCount);
    LeafLists.Add(NewList);
    NewList.Capacity := List.Count;
    for LeafIndex := 0 to List.Count - 1 do
    begin
      NewList.Add(List[LeafIndex]);
    end;
    GlobalDepth := ListIndex;
    NewList.Sort(CompareLeaves);
  end;
  FInternalTree := TInternalRangeTree.Create(self, LeafLists, 0);
end;

destructor TRbwRangeTree.Destroy;
begin
  FInternalTree.Free;
  FLeafList.Free;
  FResultList.Free;
  inherited;
end;

function TRbwRangeTree.Search(const Intervals: TSearchArray): TList;
var
  Index: Integer;
begin
  FResultList.Clear;
  for Index := 0 to Length(Intervals) - 1 do
  begin
    Assert(Intervals[Index].LowerBoundary <= Intervals[Index].UpperBoundary);
  end;
  FInternalTree.Search(Intervals);
  result := FResultList;
end;

{ TInternalRangeTreeLeafList }

constructor TInternalRangeTreeLeafList.Create(MaximumDepth: integer);
begin
  inherited Create;
  FMaxDepth := MaximumDepth;
end;

function TInternalRangeTreeLeafList.CoordinateCount: integer;
begin
  result := FMaxDepth;
end;

end.
