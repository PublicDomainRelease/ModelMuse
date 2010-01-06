unit frmCustomSelectObjectsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Contnrs, frmCustomGoPhastUnit, VirtualTrees, StdCtrls, Buttons,
  ExtCtrls, ScreenObjectUnit;

type
  TVisibleGroupState = (vgsUndefined, vgsUnChecked, vgsChecked, vgs3State);

  // @name is a record we use to store data in the nodes.
  // Since the application is responsible to manage all data including the node's caption
  // this record can be considered as minimal requirement in all VT applications.
  // Extend it to whatever your application needs.
  PMyRec = ^TMyRec;
  TMyRec = record
    Caption: WideString;
    Classification: WideString;
    ScreenObjects: TList;
    GroupState: TVisibleGroupState;
    IsDataSetNode: boolean;
  end;

  TfrmCustomSelectObjects = class(TfrmCustomGoPhast)
    // @name is the panel at the bottom of the form that holds the buttons.
    pnlBottom: TPanel;
    // @name is used to close the form.
    btnClose: TBitBtn;
    // @name is used to display help for the form.
    btnHelp: TBitBtn;
    // in @link(TfrmShowHideObjects) @name is used
    // to display and change the visibility of the
    // @link(TScreenObject)s.  The user can also select or edit objects
    // with it. The data are held in a TMyRec record associated with the
    // base nodes.
    // @seealso(TfrmShowHideObjects.vstObjectsChecked)
    // @seealso(TfrmShowHideObjects.pmSelectEdit)
    vstObjects: TVirtualStringTree;
    // @name is used to initialize data associated with each node.
    procedure vstObjectsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    // @name is used to free memory associated with each node.
    procedure vstObjectsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    // @name is used to return the text associated with a node.  This is
    // usually the name of a @link(TScreenObject) along with some additional
    // information.
    procedure vstObjectsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    // @name is used to enable or disable the activity of
    // @link(TfrmShowHideObjects.vstObjectsChecked) based on whether the check box that is
    // being checked is or is not the own the user originally changed.
    procedure vstObjectsStateChange(Sender: TBaseVirtualTree; Enter,
      Leave: TVirtualTreeStates);
    procedure vstObjectsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject);
  private
    // See @link(CanEdit).
    FCanEdit: boolean;
    { Private declarations }

  { Public declarations }
  protected
    FSettingData2: boolean;
    FvstModpathRoot: PVirtualNode;
    FvstModflowHfbNode: PVirtualNode;
    FvstModflowHobNode: PVirtualNode;
    FvstModflowGagNode: PVirtualNode;
    FvstModflowSfrNode: PVirtualNode;
    FvstModflowResNode: PVirtualNode;
    FvstModflowEtsNode: PVirtualNode;
    FvstModflowEvtNode: PVirtualNode;
    FvstModflowRchNode: PVirtualNode;
    FvstModflowDrtNode: PVirtualNode;
    FvstModflowDrnNode: PVirtualNode;
    FvstModflowRivNode: PVirtualNode;
    FvstModflowWellNode: PVirtualNode;
    FvstModflowLakNode: PVirtualNode;
    FvstModflowMnw2Node: PVirtualNode;
    FvstModflowGhbNode: PVirtualNode;
    FvstModflowUzfNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject)
    // that specifies a PHAST specified head boundary.
    FvstSpecifiedHeadNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject)
    // that specifies a PHAST specified flux boundary.
    FvstSpecifiedFluxNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject)
    // that specifies a PHAST well boundary.
    FvstWellNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject)
    // that specifies a MODFLOW CHD boundary.
    FvstModflowChdNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject)
    // that specifies the grid element size.
    FvstSizeNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject)
    // that specifies a PHAST river boundary.
    FvstRiverNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject)
    // that isn't used for anything.
    FvstOtherObjectsNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject)
    // that specifies a PHAST leaky boundary.
    FvstLeakyNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TDataArray).
    FvstDataSetRootNode: PVirtualNode;
    // @name holds a PVirtualNode for each MODFLOW boundary condition.
    FvstModflowBoundaryConditionsRoot: PVirtualNode;
    // @name holds a PVirtualNode for each PHAST boundary condition.
    FvstPhastBoundaryConditionsRoot: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject).
    FvstAllObjectsNode: PVirtualNode;
    // @name holds lists of
    // @link(TScreenObject)s that set particular
    // @link(TDataArray)s.
    FDataSetLists: TObjectList;
    // @name is used to enable or disable the action of
    // @link(TfrmShowHideObjects.vstObjectsChecked).  @SeeAlso(vstObjectsStateChange)
    FOkToDoCheck: boolean;
    // @name is used in @link(UpdateScreenObjects), @link(vstObjectsInitNode),
    //  and @link(TfrmShowHideObjects.vstObjectsChecked)
    // to prevent stack overflows.
    FSettingData: boolean;
    FSettingData3: boolean;
    FDrtList: TList;
    FHobList: TList;
    FDrnList: TList;
    FRivList: TList;
    // @name holds a list of all the @link(TScreenObject)s.
    FAllObjectsList: TList;
    FLakList: TList;
    FMnw2List: TList;
    // @name holds the lists of @link(TScreenObject)s that set MODFLOW GHB
    // boundary conditions.
    FGhbList: TList;
    // @name holds the lists of @link(TScreenObject)s that set MODFLOW Chd
    // boundary condtions.
    FChdList: TList;
    // @name holds the lists of @link(TScreenObject)s that set PHAST Well
    // boundary conditions.
    FWellList: TList;
    // @name holds the lists of @link(TScreenObject)s that set PHAST Specified Head
    // boundary conditions.
    FSpecifiedHeadList: TList;
    // @name holds the lists of @link(TScreenObject)s that set PHAST Specified Flux
    // boundary conditions.
    FSpecifiedFluxList: TList;
    // @name holds the lists of @link(TScreenObject)s that set the size
    // of elements in the grid.
    FSizeList: TList;
    // @name holds the lists of @link(TScreenObject)s that set PHAST River
    // boundary conditions.
    FRiverList: TList;
    FMfWellList: TList;
    FModpathList: TList;
    FUzfList: TList;
    // @name holds the lists of @link(TScreenObject)s that set PHAST Leaky
    // boundary conditions.
    FLeakyList: TList;
    FHfbList: TList;
    FSfrGagList: TList;
    FSfrList: TList;
    FResList: TList;
    FEtsList: TList;
    FEvtList: TList;
    // @name holds the lists of @link(TScreenObject)s that don't do anything.
    FOtherObjectsList: TList;
    FRchList: TList;
    function ShouldCheckBoxBeChecked(ScreenObject: TScreenObject): boolean;
      virtual; abstract;
    // @name adds one or more PVirtualNodes to @link(vstObjects)
    // for @link(TScreenObject AScreenObject).
    // It skips @link(TScreenObject TScreenObjects) that are deleted.
    procedure AddScreenObject(AScreenObject: TScreenObject;
      const DataSetList: TStringList);
    // @name removes all base nodes that have no children.
    procedure ClearBaseNodes(DataSetList: TStringList);
    // @name creates all the basal nodes of @link(vstObjects)
    // that will hold the nodes
    // that have data about the @link(TScreenObject TScreenObjects).
    // @name checks that the basal nodes don't already exist before creating
    // them.
    procedure CreateBaseNodes(DataSetList: TStringList);
    // @name sets @link(FvstAllObjectsNode),
    // @link(FvstSizeNode),
    // @link(FvstDataSetRootNode),
    // @link(FvstPhastBoundaryConditionsRoot),
    // @link(FvstModflowBoundaryConditionsRoot),
    // @link(FvstSpecifiedHeadNode),
    // @link(FvstSpecifiedFluxNode),
    // @link(FvstLeakyNode),
    // @link(FvstRiverNode),
    // @link(FvstWellNode), and
    // @link(FvstOtherObjectsNode), etc. to nil.
    procedure NilBaseNodes;
    procedure HandleChecked(AScreenObject: TScreenObject);virtual; abstract;
    procedure HandleUnchecked(AScreenObject: TScreenObject);virtual; abstract;
    procedure HandleChildren(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure SetCanEdit(const Value: boolean); virtual;
    // @name sorts the lists of @link(TScreenObject)s in a modified
    // alphabetical order.  In the modified order 'Object2" comes
    // before 'Object10' instead of after it.
    procedure SortScreenObjectLists;
    // @name sets whether the Node is checked, unchecked or mixed.
    procedure SetRootNodeStates(Node: PVirtualNode);
    procedure SetFirstNodeCheckState(var ChildData: PMyRec;
      ScreenObject: TScreenObject);
    procedure SetSubsequentNodesCheckState(var StateChanged: Boolean;
      ScreenObject: TScreenObject; var ChildData: PMyRec);
    // @name makes sure that the checked state of each node
    // reflects whether or not the associated @link(TScreenObject)
    // is visible or not.  Several nodes can be associated with a
    // single @link(TScreenObject) so TVirtualStringTree's check
    // propogation method can't completely handle this situation.
    procedure UpdateChildCheck(Node: PVirtualNode);
    procedure HandleCheckChange(Node: PVirtualNode; Sender: TBaseVirtualTree);
    // @name creates all the nodes in @link(vstObjects)
    procedure GetData;
    function CanSelect(ScreenObject: TScreenObject): boolean; virtual;
  public
    // @name is used to prevent access violations caused by attempting to
    // edit a @link(TScreenObject) while another one is already being edited.
    property CanEdit: boolean read FCanEdit write SetCanEdit;
    destructor Destroy; override;
    // @name updates the checked state of nodes of @link(vstObjects).
    // without adding or removing nodes.
    procedure UpdateDisplay;
    // @name adds or removes nodes as needed to make @link(vstObjects)
    // reflect the @link(TScreenObject TScreenObjects) that are present.
    procedure UpdateScreenObjects;
  end;

var
  frmCustomSelectObjects: TfrmCustomSelectObjects;

const
  StrAllObjects = 'All Objects';
  StrSetGridCellSize = 'Set Grid Cell Size';
  StrSetGridElementSize = 'Set Grid Element Size';
  StrDataSets = 'Data Sets';
  StrPhastBoundaryConditions = 'PHAST Boundary Conditions';
  StrSpecifiedHeadPhast = 'Specified Head';
  StrSpecifiedFlux = 'Specified Flux';
  StrLeakyBoundary = 'Leaky Boundary';
  StrRiverBoundary = 'River Boundary';
  StrWell = 'Well';
  StrModflowBoundaryConditions = 'MODFLOW Features';
  StrUnusedObjects = 'Unused Objects';

implementation

uses StrUtils, ModflowPackagesUnit, ModflowPackageSelectionUnit,
  GoPhastTypes, frmGoPhastUnit, PhastModelUnit, SubscriptionUnit,
  ModflowHfbUnit, ModflowSfrUnit, ModflowEvtUnit, ModflowGageUnit, DataSetUnit,
  ModpathParticleUnit, ModflowUzfUnit, ModflowHobUnit, ModflowRchUnit,
  ModflowEtsUnit, ModflowBoundaryUnit, ClassificationUnit;

{$R *.dfm}
procedure TfrmCustomSelectObjects.vstObjectsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data, ParentData: PMyRec;
  AScreenObject: TScreenObject;
  Packages: TModflowPackages;
  ScreenObjectCount: cardinal;
begin
  inherited;
  with Sender do
  begin
    Packages := frmGoPhast.PhastModel.ModflowPackages;
    Data := GetNodeData(Node);
    // Construct a node caption. This event is triggered once for each node but
    // appears asynchronously, which means when the node
    // is displayed not when it is added.

    if Node = FvstAllObjectsNode then
    begin
      Data.Caption := StrAllObjects;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstPhastBoundaryConditionsRoot then
    begin
      Data.Caption := StrPhastBoundaryConditions;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowBoundaryConditionsRoot then
    begin
      Data.Caption := StrModflowBoundaryConditions;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstDataSetRootNode then
    begin
      Data.Caption := StrDataSets;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstLeakyNode then
    begin
      Data.Caption := StrLeakyBoundary;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstOtherObjectsNode then
    begin
      Data.Caption := StrUnusedObjects;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstRiverNode then
    begin
      Data.Caption := StrRiverBoundary;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSizeNode then
    begin
      case frmGoPhast.ModelSelection of
        msUndefined:
          begin
            Assert(False);
          end;
        msPhast:
          begin
            Data.Caption := StrSetGridElementSize;
          end;
        msModflow:
          begin
            Data.Caption := StrSetGridCellSize;
          end;
        else Assert(False);
      end;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSpecifiedFluxNode then
    begin
      Data.Caption := StrSpecifiedFlux;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSpecifiedHeadNode then
    begin
      Data.Caption := StrSpecifiedHeadPhast;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstWellNode then
    begin
      Data.Caption := StrWell;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowChdNode then
    begin
      Data.Caption := Packages.ChdBoundary.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowGhbNode then
    begin
      Data.Caption := Packages.GhbBoundary.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowLakNode then
    begin
      Data.Caption := Packages.LakPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowMnw2Node then
    begin
      Data.Caption := Packages.Mnw2Package.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowWellNode then
    begin
      Data.Caption := Packages.WelPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowRivNode then
    begin
      Data.Caption := Packages.RivPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowDrnNode then
    begin
      Data.Caption := Packages.DrnPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowDrtNode then
    begin
      Data.Caption := Packages.DrtPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowRchNode then
    begin
      Data.Caption := Packages.RchPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowEvtNode then
    begin
      Data.Caption := Packages.EvtPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowEtsNode then
    begin
      Data.Caption := Packages.EtsPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowResNode then
    begin
      Data.Caption := Packages.ResPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowSfrNode then
    begin
      Data.Caption := Packages.SfrPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowGagNode then
    begin
      Data.Caption := 'GAGE: Gage package';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowUzfNode then
    begin
      Data.Caption := Packages.UzfPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowHobNode then
    begin
      Data.Caption := Packages.HobPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowHfbNode then
    begin
      Data.Caption := Packages.HfbPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModpathRoot then
    begin
      Data.Caption := Packages.ModPath.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end;

    If (ParentNode = nil) then
    begin
      Exit;
    end;

    ParentData := GetNodeData(ParentNode);
    if not Assigned(ParentData) or (ParentData.ScreenObjects = nil) then
    begin
      Exit;
    end;
    Assert(ParentData.ScreenObjects.Count >= 0);
    ScreenObjectCount := ParentData.ScreenObjects.Count;
    if (Node.Index >= ScreenObjectCount) then
    begin
      Exit;
    end;

    AScreenObject := ParentData.ScreenObjects[Node.Index];
    if AScreenObject <> nil then
    begin
      Node.CheckType := ctCheckBox;
      try
        FSettingData2 := True;
        if ShouldCheckBoxBeChecked(AScreenObject) then
        begin
          Sender.CheckState[Node] := csCheckedNormal;
        end
        else
        begin
          Sender.CheckState[Node] := csUncheckedNormal;
        end;
      finally
        FSettingData2 := False;
      end;
    end;

    If ParentNode.Parent = FvstDataSetRootNode then
    begin
      ParentNode.CheckType := ctTriStateCheckBox;
    end;
  end;
end;

procedure TfrmCustomSelectObjects.vstObjectsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PMyRec;
begin
  inherited;
  Data := Sender.GetNodeData(Node);
  // Explicitely free the string, the VCL cannot know that there is one but needs to free
  // it nonetheless. For more fields in such a record which must be freed use Finalize(Data^) instead touching
  // every member individually.
  if Assigned(Data) then
  begin
    Data.Caption := '';
    Data.Classification := '';
    if FDataSetLists <> nil then
    begin
      FDataSetLists.Remove(Data.ScreenObjects);
    end;
    Data.ScreenObjects := nil;
  end;
end;

procedure TfrmCustomSelectObjects.vstObjectsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  Data: PMyRec;
  AScreenObject: TScreenObject;
  ParentNode : PVirtualNode;
  Index, DataSetIndex: integer;
  ScreenObjectCount: cardinal;
  DSFormula: string;
begin
  inherited;
  if csDestroying in ComponentState then
  begin
    CellText := '';
    Exit;
  end;
  // A handler for the OnGetText event is always needed as it provides the tree with the string data to display.
  // Note that we are always using WideString.
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    CellText := Data.Caption;
  end;
  Data := Sender.GetNodeData(Node.Parent);
  if Assigned(Data) and (Data.ScreenObjects <> nil) then
  begin
    Assert(Data.ScreenObjects.Count >= 0);
    ScreenObjectCount := Data.ScreenObjects.Count;
    if Node.Index >= ScreenObjectCount then
    begin
      CellText := 'Error';
      Exit;
    end;
    AScreenObject := Data.ScreenObjects[Node.Index];
    CellText := AScreenObject.Name;
    ParentNode := Node.Parent;
    while (ParentNode <> nil) and (ParentNode.Parent <> FvstDataSetRootNode) do
    begin
      ParentNode := ParentNode.Parent;
    end;

    CellText := CellText + ' (' + AScreenObject.Methods;

    If (ParentNode <> nil) and (ParentNode.Parent = FvstDataSetRootNode) then
    begin
      for Index := 0 to frmGoPhast.PhastModel.DataSetCount -1 do
      begin
        if frmGoPhast.PhastModel.DataSets[Index].Name = Data.Caption then
        begin
          DataSetIndex := AScreenObject.
            IndexOfDataSet(frmGoPhast.PhastModel.DataSets[Index]);
          DSFormula := AScreenObject.DataSetFormulas[DataSetIndex];
          if Length(DSFormula) > 100 then
          begin
            DSFormula := Copy(DSFormula, 1, 100) + '...';
          end;
          CellText := CellText + '; ' + DSFormula;
          break;
        end;
      end;
    end;

    CellText := CellText  + ')'
  end;
end;

procedure TfrmCustomSelectObjects.vstObjectsStateChange(Sender: TBaseVirtualTree; Enter,
      Leave: TVirtualTreeStates);
begin
  inherited;
  FOkToDoCheck := not (tsCheckPropagation in Enter);
end;

procedure TfrmCustomSelectObjects.vstObjectsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
begin
  inherited;
  if not FSettingData and not FSettingData3 and (Node = FvstAllObjectsNode) then
  begin
    if Node.CheckState = csMixedNormal then
    begin
      NewState := csUncheckedNormal;
    end
    else if Node.CheckState = csMixedPressed then
    begin
      NewState := csUncheckedPressed;
    end;
  end;
end;

procedure TfrmCustomSelectObjects.AddScreenObject(AScreenObject: TScreenObject;
      const DataSetList: TStringList);
var
  DataSetIndex: integer;
  Position: integer;
  PutInOtherObjects: boolean;
  vstDataSetNode: PVirtualNode;
  DataArray: TDataArray;
  Procedure InitializeData(Node: PVirtualNode);
  var
    Data: PMyRec;
  begin
    Data := vstObjects.GetNodeData(Node);
    Data.ScreenObjects.Add(AScreenObject);
    PutInOtherObjects := False;
    if ShouldCheckBoxBeChecked(AScreenObject) then
    begin
      case Data.GroupState of
        vgsUndefined:
          begin
            Data.GroupState := vgsChecked;
          end;
        vgsUnChecked:
          begin
            Data.GroupState := vgs3State;
          end;
        vgsChecked:
          begin
            // do nothing
          end;
        vgs3State:
          begin
            // do nothing
          end;
        else
          begin
            Assert(False);
          end;
      end;
    end
    else
    begin
      case Data.GroupState of
        vgsUndefined:
          begin
            Data.GroupState := vgsUnChecked;
          end;
        vgsUnChecked:
          begin
            // do nothing
          end;
        vgsChecked:
          begin
            Data.GroupState := vgs3State;
          end;
        vgs3State:
          begin
            // do nothing
          end;
        else
          begin
            Assert(False);
          end;
      end;
    end;
    case Data.GroupState of
      vgsUndefined:
        begin
          vstObjects.CheckState[Node] := csUncheckedNormal;
        end;
      vgsUnChecked:
        begin
          vstObjects.CheckState[Node] := csUncheckedNormal;
        end;
      vgsChecked:
        begin
          vstObjects.CheckState[Node] := csCheckedNormal;
        end;
      vgs3State:
        begin
          vstObjects.CheckState[Node] := csMixedNormal;
        end;
      else
        begin
          Assert(False);
        end;
    end;
  end;
begin
  if AScreenObject.Deleted then
  begin
    Exit;
  end;
  if not CanSelect(AScreenObject) then
  begin
    Exit;
  end;

  vstObjects.BeginUpdate;
  try
    InitializeData(FvstAllObjectsNode);
    PutInOtherObjects := True;
    if AScreenObject.CellSizeUsed then
    begin
      InitializeData(FvstSizeNode);
    end;

    for DataSetIndex := 0 to AScreenObject.DataSetCount - 1 do
    begin
      DataArray := AScreenObject.DataSets[DataSetIndex];
      Position :=
        DataSetList.IndexOf(DataArray.FullClassification + '|' + DataArray.Name);
      if Position < 0 then
      begin
        // The data set has been deleted.
        Continue;
      end;

      vstDataSetNode := PVirtualNode(DataSetList.Objects[Position]);
      InitializeData(vstDataSetNode);
    end;

    case AScreenObject.BoundaryTypeUsed of
      btNone:
        begin
          // do nothing
        end;
      btSpecifiedHead:
        begin
          InitializeData(FvstSpecifiedHeadNode);
        end;
      btFlux:
        begin
          InitializeData(FvstSpecifiedFluxNode);
        end;
      btLeaky:
        begin
          InitializeData(FvstLeakyNode);
        end;
      btRiver:
        begin
          InitializeData(FvstRiverNode);
        end;
      btWell:
        begin
          InitializeData(FvstWellNode);
        end;
    else
      Assert(False);
    end;

    if (AScreenObject.ModflowChdBoundary <> nil)
      and AScreenObject.ModflowChdBoundary.Used then
    begin
      InitializeData(FvstModflowChdNode);
    end;

    if (AScreenObject.ModflowGhbBoundary <> nil)
      and AScreenObject.ModflowGhbBoundary.Used then
    begin
      InitializeData(FvstModflowGhbNode);
    end;

    if (AScreenObject.ModflowLakBoundary <> nil)
      and AScreenObject.ModflowLakBoundary.Used then
    begin
      InitializeData(FvstModflowLakNode);
    end;

    if (AScreenObject.ModflowMnw2Boundary <> nil)
      and AScreenObject.ModflowMnw2Boundary.Used then
    begin
      InitializeData(FvstModflowMnw2Node);
    end;

    if (AScreenObject.ModflowWellBoundary <> nil)
      and AScreenObject.ModflowWellBoundary.Used then
    begin
      InitializeData(FvstModflowWellNode);
    end;

    if (AScreenObject.ModflowRivBoundary <> nil)
      and AScreenObject.ModflowRivBoundary.Used then
    begin
      InitializeData(FvstModflowRivNode);
    end;

    if (AScreenObject.ModflowDrnBoundary <> nil)
      and AScreenObject.ModflowDrnBoundary.Used then
    begin
      InitializeData(FvstModflowDrnNode);
    end;

    if (AScreenObject.ModflowDrtBoundary <> nil)
      and AScreenObject.ModflowDrtBoundary.Used then
    begin
      InitializeData(FvstModflowDrtNode);
    end;

    if (AScreenObject.ModflowRchBoundary <> nil)
      and AScreenObject.ModflowRchBoundary.Used then
    begin
      InitializeData(FvstModflowRchNode);
    end;

    if (AScreenObject.ModflowResBoundary <> nil)
      and AScreenObject.ModflowResBoundary.Used then
    begin
      InitializeData(FvstModflowResNode);
    end;

    if (AScreenObject.ModflowEvtBoundary <> nil)
      and AScreenObject.ModflowEvtBoundary.Used then
    begin
      InitializeData(FvstModflowEvtNode);
    end;

    if (AScreenObject.ModflowEtsBoundary <> nil)
      and AScreenObject.ModflowEtsBoundary.Used then
    begin
      InitializeData(FvstModflowEtsNode);
    end;

    if (AScreenObject.ModflowSfrBoundary <> nil)
      and AScreenObject.ModflowSfrBoundary.Used then
    begin
      InitializeData(FvstModflowSfrNode);
    end;

    if (AScreenObject.ModflowStreamGage <> nil)
      and AScreenObject.ModflowStreamGage.Used then
    begin
      InitializeData(FvstModflowGagNode);
    end;

    if (AScreenObject.ModflowUzfBoundary <> nil)
      and AScreenObject.ModflowUzfBoundary.Used then
    begin
      InitializeData(FvstModflowUzfNode);
    end;

    if (AScreenObject.ModflowHeadObservations <> nil)
      and AScreenObject.ModflowHeadObservations.Used then
    begin
      InitializeData(FvstModflowHobNode);
    end;
    if (AScreenObject.ModflowHfbBoundary <> nil)
      and AScreenObject.ModflowHfbBoundary.Used then
    begin
      InitializeData(FvstModflowHfbNode);
    end;

    if (AScreenObject.ModpathParticles <> nil)
      and AScreenObject.ModpathParticles.Used then
    begin
      InitializeData(FvstModpathRoot);
    end;


    if PutInOtherObjects then
    begin
      InitializeData(FvstOtherObjectsNode);
    end;
  finally
    vstObjects.EndUpdate;
  end;
end;

function TfrmCustomSelectObjects.CanSelect(
  ScreenObject: TScreenObject): boolean;
begin
  result := True;
end;

procedure TfrmCustomSelectObjects.ClearBaseNodes(DataSetList: TStringList);
var
  DataSetIndex: integer;
  vstDataSetNode: PVirtualNode;
  GroupNode: PVirtualNode;
  NodeList: TList;
  NodeIndex: Integer;
  ParentNodes: TList;
  Node: PVirtualNode;
  ParentNode: PVirtualNode;
  NodeDeleted: Boolean;
  ChildCount: integer;
  procedure UpdateChildCount(var Node : PVirtualNode);
  var
    Data: PMyRec;
  begin
    Data := vstObjects.GetNodeData(Node);
    If Data.ScreenObjects <> nil then
    begin
      vstObjects.ChildCount[Node] := Data.ScreenObjects.Count;
    end;
  end;
  procedure vstCheckDeleteNode(var Node : PVirtualNode);
  begin
    UpdateChildCount(Node);
    if vstObjects.ChildCount[Node] = 0 then
    begin
      vstObjects.DeleteNode(Node);
      Node := nil;
    end;
  end;
begin
  UpdateChildCount(FvstAllObjectsNode);
  vstCheckDeleteNode(FvstOtherObjectsNode);
  vstCheckDeleteNode(FvstLeakyNode);
  vstCheckDeleteNode(FvstRiverNode);
  vstCheckDeleteNode(FvstSpecifiedFluxNode);
  vstCheckDeleteNode(FvstSpecifiedHeadNode);
  vstCheckDeleteNode(FvstWellNode);
  vstCheckDeleteNode(FvstPhastBoundaryConditionsRoot);

  vstCheckDeleteNode(FvstModflowGhbNode);
  vstCheckDeleteNode(FvstModflowLakNode);
  vstCheckDeleteNode(FvstModflowMnw2Node);
  vstCheckDeleteNode(FvstModflowChdNode);
  vstCheckDeleteNode(FvstModflowWellNode);
  vstCheckDeleteNode(FvstModflowRivNode);
  vstCheckDeleteNode(FvstModflowDrnNode);
  vstCheckDeleteNode(FvstModflowDrtNode);
  vstCheckDeleteNode(FvstModflowRchNode);
  vstCheckDeleteNode(FvstModflowResNode);
  vstCheckDeleteNode(FvstModflowEvtNode);
  vstCheckDeleteNode(FvstModflowEtsNode);
  vstCheckDeleteNode(FvstModflowSfrNode);
  vstCheckDeleteNode(FvstModflowGagNode);
  vstCheckDeleteNode(FvstModflowUzfNode);
  vstCheckDeleteNode(FvstModflowHobNode);
  vstCheckDeleteNode(FvstModflowHfbNode);

  vstCheckDeleteNode(FvstModflowBoundaryConditionsRoot);
  vstCheckDeleteNode(FvstModpathRoot);

  ParentNodes := TList.Create;
  try
    for DataSetIndex := DataSetList.Count -1 downto 0 do
    begin
      vstDataSetNode := PVirtualNode(DataSetList.Objects[DataSetIndex]);
      ParentNode := vstDataSetNode.Parent;
      UpdateChildCount(vstDataSetNode);
      if vstObjects.ChildCount[vstDataSetNode] = 0 then
      begin
        if ParentNodes.IndexOf(ParentNode) < 0 then
        begin
          ParentNodes.Add(ParentNode);
        end;
        vstObjects.DeleteNode(vstDataSetNode);
        DataSetList.Delete(DataSetIndex);
      end;
    end;

    NodeDeleted := True;
    while NodeDeleted do
    begin
      NodeDeleted := False;
      for NodeIndex := ParentNodes.Count - 1 downto 0 do
      begin
        Node := ParentNodes[NodeIndex];
        if vstObjects.ChildCount[Node] = 0 then
        begin
          ParentNode := Node.Parent;
          NodeDeleted := True;
          vstObjects.DeleteNode(Node);
          if (ParentNode <> FvstDataSetRootNode)
            and (ParentNodes.IndexOf(ParentNode) < 0) then
          begin
            ParentNodes.Add(ParentNode);
          end;
          ParentNodes.Delete(NodeIndex);
        end;
      end;
    end;
  finally
    ParentNodes.Free;
  end;

  NodeList := TList.Create;
  try
    ChildCount := vstObjects.ChildCount[FvstDataSetRootNode];
    for NodeIndex := 0 to ChildCount - 1 do
    begin
      if NodeIndex = 0 then
      begin
        GroupNode := vstObjects.GetFirstChild(FvstDataSetRootNode)
      end
      else
      begin
        GroupNode := vstObjects.GetNextSibling(GroupNode);
      end;
      NodeList.Add(GroupNode)
    end;

    for NodeIndex := 0 to NodeList.Count - 1 do
    begin
      GroupNode := NodeList[NodeIndex];
      vstCheckDeleteNode(GroupNode);
    end;
  finally
    NodeList.Free;
  end;

  vstCheckDeleteNode(FvstDataSetRootNode);
  vstCheckDeleteNode(FvstSizeNode);
end;

procedure TfrmCustomSelectObjects.CreateBaseNodes(DataSetList: TStringList);
var
  DataSetIndex: integer;
  ClassificationObject : TDataSetClassification;
  DataSet: TDataArray;
  vstDataSetNode: PVirtualNode;
  Data: PMyRec;
  ListOfObjects: TList;
  DataSetClassifications: TStringList;
  DataSetGroupNode: PVirtualNode;
  ClassificationPosition: integer;
  SortedClassifiedDataSets: TStringList;
  Classification: string;
  ALevelDescription: string;
  SeparatorPosition: integer;
  ParentNode: PVirtualNode;
  TempSeparatorPosition: Integer;
  PriorNode: PVirtualNode;
  ClassificationObjectOwnerList: TList;
  HufDataArrays: TClassificationList;
  ClassificationObjects: TClassificationList;
  LayerGroupList: TClassificationList;
  procedure InitializeNodeData(Node: PVirtualNode; List: TList);
  var
    Data: PMyRec;
  begin
    Data := vstObjects.GetNodeData(Node);
    Data.ScreenObjects := List;
    vstObjects.CheckType[Node] := ctTriStateCheckBox;
    Data.GroupState := vgsUndefined;
    Data.IsDataSetNode := False;
    if List <> nil then
    begin
      List.Clear;
    end;
  end;
  procedure FindOrCreateClassificationNode;
  begin
    ClassificationPosition :=
      DataSetClassifications.IndexOf(ALevelDescription);

    if ClassificationPosition < 0 then
    begin
      vstObjects.ChildCount[ParentNode] :=
        vstObjects.ChildCount[ParentNode] + 1;
      DataSetGroupNode := vstObjects.GetLastChild(ParentNode);
      DataSetClassifications.AddObject(
        ALevelDescription, TObject(DataSetGroupNode));
      InitializeNodeData(DataSetGroupNode, nil);
      Data := vstObjects.GetNodeData(DataSetGroupNode);
      Data.Classification := ALevelDescription;
      TempSeparatorPosition := Pos('|', ALevelDescription);
      while TempSeparatorPosition > 0 do
      begin
        ALevelDescription := Copy(ALevelDescription,
          Succ(TempSeparatorPosition), MAXINT);
        TempSeparatorPosition := Pos('|', ALevelDescription);
      end;

      Data.Caption := ALevelDescription;
      ParentNode := DataSetGroupNode;
    end
    else
    begin
      ParentNode := PVirtualNode(DataSetClassifications.
        Objects[ClassificationPosition]);
      DataSetGroupNode := ParentNode;
    end;
  end;
  procedure InitializeMF_BoundaryNode(var BC_Node, PriorNode: PVirtualNode;
    List: TList);
  begin
    if BC_Node = nil then
    begin
      BC_Node := vstObjects.InsertNode(PriorNode, amInsertAfter);
      vstObjects.ReinitNode(BC_Node, False);
    end;
    InitializeNodeData(BC_Node, List);
    PriorNode := BC_Node;
  end;
begin
  if FvstAllObjectsNode = nil then
  begin
    FvstAllObjectsNode := vstObjects.InsertNode(
      vstObjects.RootNode, amAddChildFirst);
    vstObjects.ReinitNode(FvstAllObjectsNode, False);
  end;
  InitializeNodeData(FvstAllObjectsNode, FAllObjectsList);

  if FvstSizeNode = nil then
  begin
    FvstSizeNode := vstObjects.InsertNode(FvstAllObjectsNode, amInsertAfter);
    vstObjects.ReinitNode(FvstSizeNode, False);
  end;
  InitializeNodeData(FvstSizeNode, FSizeList);

  if FvstDataSetRootNode = nil then
  begin
    FvstDataSetRootNode := vstObjects.InsertNode(FvstSizeNode, amInsertAfter);
    vstObjects.ReinitNode(FvstDataSetRootNode, False);
  end;
  InitializeNodeData(FvstDataSetRootNode, nil);

  if FvstPhastBoundaryConditionsRoot = nil then
  begin
    FvstPhastBoundaryConditionsRoot := vstObjects.InsertNode(
      FvstDataSetRootNode, amInsertAfter);
    vstObjects.ReinitNode(FvstPhastBoundaryConditionsRoot, False);
  end;
  InitializeNodeData(FvstPhastBoundaryConditionsRoot, nil);

  if FvstModflowBoundaryConditionsRoot = nil then
  begin
    FvstModflowBoundaryConditionsRoot := vstObjects.InsertNode(
      FvstPhastBoundaryConditionsRoot, amInsertAfter);
    vstObjects.ReinitNode(FvstModflowBoundaryConditionsRoot, False);
  end;
  InitializeNodeData(FvstModflowBoundaryConditionsRoot, nil);

  if FvstModpathRoot = nil then
  begin
    FvstModpathRoot := vstObjects.InsertNode(
      FvstModflowBoundaryConditionsRoot, amInsertAfter);
    vstObjects.ReinitNode(FvstModpathRoot, False);
  end;
  InitializeNodeData(FvstModpathRoot, FModpathList);

  if FvstOtherObjectsNode = nil then
  begin
    FvstOtherObjectsNode := vstObjects.InsertNode(
      FvstModflowBoundaryConditionsRoot, amInsertAfter);
    vstObjects.ReinitNode(FvstOtherObjectsNode, False);
  end;
  InitializeNodeData(FvstOtherObjectsNode, FOtherObjectsList);

  // create children of FvstPhastBoundaryConditionsRoot
  if FvstSpecifiedHeadNode = nil then
  begin
    FvstSpecifiedHeadNode := vstObjects.InsertNode(
      FvstPhastBoundaryConditionsRoot, amAddChildFirst);
    vstObjects.ReinitNode(FvstSpecifiedHeadNode, False);
  end;
  InitializeNodeData(FvstSpecifiedHeadNode, FSpecifiedHeadList);

  if FvstSpecifiedFluxNode = nil then
  begin
    FvstSpecifiedFluxNode := vstObjects.InsertNode(
      FvstSpecifiedHeadNode, amInsertAfter);
    vstObjects.ReinitNode(FvstSpecifiedFluxNode, False);
  end;
  InitializeNodeData(FvstSpecifiedFluxNode, FSpecifiedFluxList);

  if FvstLeakyNode = nil then
  begin
    FvstLeakyNode := vstObjects.InsertNode(
      FvstSpecifiedFluxNode, amInsertAfter);
    vstObjects.ReinitNode(FvstLeakyNode, False);
  end;
  InitializeNodeData(FvstLeakyNode, FLeakyList);

  if FvstRiverNode = nil then
  begin
    FvstRiverNode := vstObjects.InsertNode(
      FvstLeakyNode, amInsertAfter);
    vstObjects.ReinitNode(FvstRiverNode, False);
  end;
  InitializeNodeData(FvstRiverNode, FRiverList);

  if FvstWellNode = nil then
  begin
    FvstWellNode := vstObjects.InsertNode(FvstRiverNode, amInsertAfter);
    vstObjects.ReinitNode(FvstWellNode, False);
  end;
  InitializeNodeData(FvstWellNode, FWellList);

  // add children of FvstModflowBoundaryConditionsRoot
  if FvstModflowChdNode = nil then
  begin
    FvstModflowChdNode := vstObjects.InsertNode(
      FvstModflowBoundaryConditionsRoot, amAddChildFirst);
    vstObjects.ReinitNode(FvstModflowChdNode, False);
  end;
  InitializeNodeData(FvstModflowChdNode, FChdList);
  PriorNode := FvstModflowChdNode;

  InitializeMF_BoundaryNode(FvstModflowDrnNode, PriorNode, FDrnList);
  InitializeMF_BoundaryNode(FvstModflowDrtNode, PriorNode, FDrtList);
  InitializeMF_BoundaryNode(FvstModflowEtsNode, PriorNode, FEtsList);
  InitializeMF_BoundaryNode(FvstModflowEvtNode, PriorNode, FEvtList);
  InitializeMF_BoundaryNode(FvstModflowGhbNode, PriorNode, FGhbList);
  InitializeMF_BoundaryNode(FvstModflowLakNode, PriorNode, FLakList);

  InitializeMF_BoundaryNode(FvstModflowMnw2Node, PriorNode, FMnw2List);

  InitializeMF_BoundaryNode(FvstModflowRchNode, PriorNode, FRchList);
  InitializeMF_BoundaryNode(FvstModflowResNode, PriorNode, FResList);
  InitializeMF_BoundaryNode(FvstModflowRivNode, PriorNode, FRivList);
  InitializeMF_BoundaryNode(FvstModflowSfrNode, PriorNode, FSfrList);
  InitializeMF_BoundaryNode(FvstModflowGagNode, PriorNode, FSfrGagList);
  InitializeMF_BoundaryNode(FvstModflowUzfNode, PriorNode, FUzfList);
  InitializeMF_BoundaryNode(FvstModflowWellNode, PriorNode, FMfWellList);
  InitializeMF_BoundaryNode(FvstModflowHobNode, PriorNode, FHobList);
  InitializeMF_BoundaryNode(FvstModflowHfbNode, PriorNode, FHfbList);

  FDataSetLists.Clear;

  vstObjects.ChildCount[FvstDataSetRootNode] := 0;
  DataSetClassifications := TStringList.Create;
  try
    DataSetClassifications.AddObject('Data Sets', TObject(FvstDataSetRootNode));

    ClassificationObjectOwnerList := TObjectList.Create;
    try
  // Create lists used for sorting the nodes.
      HufDataArrays := TClassificationList.Create;
      ClassificationObjects:= TClassificationList.Create;
      LayerGroupList := TClassificationList.Create;
      try
        FillDataSetLists(HufDataArrays,
          LayerGroupList,
          ClassificationObjects,
          ClassificationObjectOwnerList);

        SortedClassifiedDataSets := TStringList.Create;
        try
          ClassifyListedObjects(SortedClassifiedDataSets, ClassificationObjects,
            [LayerGroupList, HufDataArrays]);
//          for DataSetIndex := 0 to frmGoPhast.PhastModel.DataSetCount - 1 do
//          begin
//            DataSet := frmGoPhast.PhastModel.DataSets[DataSetIndex];
//            SortedClassifiedDataSets.AddObject(
//              DataSet.FullClassification + '|' + DataSet.Name, DataSet);
//          end;
//          SortedClassifiedDataSets.CustomSort(CompareStrings);


          for DataSetIndex := 0 to SortedClassifiedDataSets.Count - 1 do
          begin
            Classification := SortedClassifiedDataSets[DataSetIndex];
            ClassificationObject := SortedClassifiedDataSets.Objects[DataSetIndex] as TDataSetClassification;
            if ClassificationObject <> nil then
            begin

              DataSet := ClassificationObject.DataArray;
              Classification := DataSet.FullClassification;
              ParentNode := FvstDataSetRootNode;
              Assert(Classification <> '');
              SeparatorPosition := Pos('|', Classification);
              while SeparatorPosition > 0 do
              begin
                ALevelDescription := Copy(Classification, 1, Pred(SeparatorPosition));

                FindOrCreateClassificationNode;

                SeparatorPosition := PosEx('|', Classification,
                  Succ(SeparatorPosition));
              end;
              ALevelDescription := Classification;
              FindOrCreateClassificationNode;

              vstObjects.ChildCount[DataSetGroupNode] :=
                vstObjects.ChildCount[DataSetGroupNode] + 1;

              vstDataSetNode := vstObjects.GetLastChild(DataSetGroupNode);

              Data := vstObjects.GetNodeData(vstDataSetNode);
              Data.Caption := DataSet.Name;

              ListOfObjects := TList.Create;
              FDataSetLists.Add(ListOfObjects);

              InitializeNodeData(vstDataSetNode, ListOfObjects);

              Data.IsDataSetNode := True;
              Data.Classification := Classification + '|' + DataSet.Name;

              DataSetList.AddObject(Data.Classification, TObject(vstDataSetNode));
            end;
          end;
        finally
          SortedClassifiedDataSets.Free;
        end;
      finally
        LayerGroupList.Free;
        ClassificationObjects.Free;
        HufDataArrays.Free;
      end;
    finally
      ClassificationObjectOwnerList.Free;
    end;
  finally
    DataSetClassifications.Free;
  end;
end;

destructor TfrmCustomSelectObjects.Destroy;
begin
  FDataSetLists.Free;
  FDataSetLists := nil;

  FAllObjectsList.Free;
  FLeakyList.Free;
  FOtherObjectsList.Free;
  FRiverList.Free;
  FSizeList.Free;
  FSpecifiedFluxList.Free;
  FSpecifiedHeadList.Free;
  FWellList.Free;
  FChdList.Free;
  FGhbList.Free;
  FLakList.Free;
  FMnw2List.Free;
  FMfWellList.Free;
  FRivList.Free;
  FDrnList.Free;
  FDrtList.Free;
  FRchList.Free;
  FEvtList.Free;
  FEtsList.Free;
  FResList.Free;
  FSfrGagList.Free;
  FSfrList.Free;
  FUzfList.Free;
  FHobList.Free;
  FHfbList.Free;
  FModpathList.Free;

  inherited;
end;

procedure TfrmCustomSelectObjects.FormCreate(Sender: TObject);
begin
  inherited;
  vstObjects.NodeDataSize := SizeOf(TMyRec);

  FDataSetLists:= TObjectList.Create;

  FAllObjectsList:= TList.Create;
  FLeakyList:= TList.Create;
  FOtherObjectsList:= TList.Create;
  FRiverList:= TList.Create;
  FSizeList:= TList.Create;
  FSpecifiedFluxList:= TList.Create;
  FSpecifiedHeadList:= TList.Create;
  FWellList:= TList.Create;
  FChdList := TList.Create;
  FGhbList := TList.Create;
  FLakList := TList.Create;
  FMnw2List := TList.Create;
  FMfWellList := TList.Create;
  FRivList := TList.Create;
  FDrnList := TList.Create;
  FDrtList := TList.Create;
  FRchList := TList.Create;
  FEvtList := TList.Create;
  FEtsList := TList.Create;
  FResList := TList.Create;
  FSfrList := TList.Create;
  FSfrGagList := TList.Create;
  FUzfList := TList.Create;
  FHobList := TList.Create;
  FHfbList := TList.Create;
  FModpathList := TList.Create;

  FCanEdit := True;

end;

procedure TfrmCustomSelectObjects.FormDestroy(Sender: TObject);
var
  Node: PVirtualNode;
begin
  inherited;
  Node := vstObjects.GetFirst;
  while Node <> nil do
  begin
    vstObjects.DeleteNode(Node);
    Node := vstObjects.GetFirst;
  end;
end;

procedure TfrmCustomSelectObjects.NilBaseNodes;
begin
  FvstAllObjectsNode := nil;
  FvstSizeNode := nil;
  FvstDataSetRootNode := nil;
  FvstPhastBoundaryConditionsRoot := nil;
  FvstModflowBoundaryConditionsRoot := nil;
  FvstSpecifiedHeadNode := nil;
  FvstSpecifiedFluxNode := nil;
  FvstLeakyNode := nil;
  FvstRiverNode := nil;
  FvstWellNode := nil;
  FvstOtherObjectsNode := nil;
  FvstModflowChdNode := nil;
  FvstModflowGhbNode := nil;
  FvstModflowLakNode := nil;
  FvstModflowMnw2Node := nil;
  FvstModflowWellNode := nil;
  FvstModflowRivNode := nil;
  FvstModflowDrnNode := nil;
  FvstModflowDrtNode := nil;
  FvstModflowRchNode := nil;
  FvstModflowEvtNode := nil;
  FvstModflowEtsNode := nil;
  FvstModflowResNode := nil;
  FvstModflowSfrNode := nil;
  FvstModflowUzfNode := nil;
  FvstModflowHobNode := nil;
  FvstModflowHfbNode := nil;
  FvstModflowGagNode := nil;
end;

procedure TfrmCustomSelectObjects.SetCanEdit(const Value: boolean);
begin
  FCanEdit := Value;
end;

procedure TfrmCustomSelectObjects.SetFirstNodeCheckState(var ChildData: PMyRec;
  ScreenObject: TScreenObject);
begin
  if ShouldCheckBoxBeChecked(ScreenObject) then
  begin
    ChildData.GroupState := vgsChecked;
  end
  else
  begin
    ChildData.GroupState := vgsUnChecked;
  end;
end;

procedure TfrmCustomSelectObjects.HandleChildren(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  ChildNode: PVirtualNode;
  ChildData: PMyRec;
  Index: integer;
  AScreenObject: TScreenObject;
begin
  ChildNode := Sender.GetFirstChild(Node);
  while ChildNode <> nil do
  begin
    ChildData := Sender.GetNodeData(ChildNode);
    if (ChildData <> nil) and (ChildData.ScreenObjects <> nil) then
    begin
      case Sender.CheckState[Node] of
        csUncheckedNormal:
          begin
            for Index := 0 to ChildData.ScreenObjects.Count - 1 do
            begin
              AScreenObject := ChildData.ScreenObjects[Index];
              HandleUnChecked(AScreenObject);
            end;
          end;
        csCheckedNormal:
          begin
            for Index := 0 to ChildData.ScreenObjects.Count - 1 do
            begin
              AScreenObject := ChildData.ScreenObjects[Index];
              HandleChecked(AScreenObject);
            end;
          end;
        csMixedNormal:
          begin
            // do nothing.
          end;
        else
          begin
            Assert(False);
          end;
      end;
    end
    else
    begin
      HandleChildren(Sender, ChildNode);
    end;
    ChildNode := Sender.GetNextSibling(ChildNode);
  end;
end;

procedure TfrmCustomSelectObjects.SortScreenObjectLists;
var
  Index: integer;
  List: TList;
begin
  FAllObjectsList.Sort(ScreenObjectCompare);
  FLeakyList.Sort(ScreenObjectCompare);
  FOtherObjectsList.Sort(ScreenObjectCompare);
  FRiverList.Sort(ScreenObjectCompare);
  FSizeList.Sort(ScreenObjectCompare);
  FSpecifiedFluxList.Sort(ScreenObjectCompare);
  FSpecifiedHeadList.Sort(ScreenObjectCompare);
  FWellList.Sort(ScreenObjectCompare);

  FChdList.Sort(ScreenObjectCompare);
  FGhbList.Sort(ScreenObjectCompare);
  FLakList.Sort(ScreenObjectCompare);
  FMnw2List.Sort(ScreenObjectCompare);
  FMfWellList.Sort(ScreenObjectCompare);
  FRivList.Sort(ScreenObjectCompare);
  FDrnList.Sort(ScreenObjectCompare);
  FDrtList.Sort(ScreenObjectCompare);
  FRchList.Sort(ScreenObjectCompare);
  FEvtList.Sort(ScreenObjectCompare);
  FEtsList.Sort(ScreenObjectCompare);
  FResList.Sort(ScreenObjectCompare);
  FSfrList.Sort(ScreenObjectCompare);
  FSfrGagList.Sort(ScreenObjectCompare);
  FUzfList.Sort(ScreenObjectCompare);
  FHobList.Sort(ScreenObjectCompare);
  FHfbList.Sort(ScreenObjectCompare);
  FModpathList.Sort(ScreenObjectCompare);
  for Index := 0 to FDataSetLists.Count - 1 do
  begin
    List := FDataSetLists[Index] as TList;
    List.Sort(ScreenObjectCompare);
  end;
end;

procedure TfrmCustomSelectObjects.SetRootNodeStates(Node: PVirtualNode);
var
  Index: integer;
  ChildNode: PVirtualNode;
  Data, ChildData: PMyRec;
  ChildCount: integer;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  StateChanged: Boolean;
begin
  if Node = nil then
  begin
    Exit;
  end;
  Data := vstObjects.GetNodeData(Node);
  ChildNode := nil;
  // Convert vstObjects.ChildCount[Node] from Cardinal to integer to
  // prevent Interger overflow when  vstObjects.ChildCount[Node] = 0.
  ChildCount := vstObjects.ChildCount[Node];
  for Index := 0 to ChildCount - 1 do
  begin
    if Index = 0 then
    begin
      ChildNode := vstObjects.GetFirstChild(Node);
    end
    else
    begin
      ChildNode := vstObjects.GetNextSibling(ChildNode);
    end;
    if vstObjects.ChildCount[ChildNode] > 0 then
    begin
      SetRootNodeStates(ChildNode);
    end;
    ChildData := vstObjects.GetNodeData(ChildNode);
    if ChildData.ScreenObjects <> nil then
    begin
      for ScreenObjectIndex := 0 to ChildData.ScreenObjects.Count - 1 do
      begin
        ScreenObject := ChildData.ScreenObjects[ScreenObjectIndex];
        if ScreenObjectIndex = 0 then
        begin
          SetFirstNodeCheckState(ChildData, ScreenObject);;
        end
        else
        begin
          StateChanged := False;
          SetSubsequentNodesCheckState(StateChanged, ScreenObject, ChildData);
          if StateChanged then
          begin
            break;
          end;
        end;
      end;
    end;
    if Index = 0 then
    begin
      Data.GroupState := ChildData.GroupState;
    end
    else
    begin
      if Data.GroupState <> ChildData.GroupState then
      begin
        Data.GroupState := vgs3State;
        break;
      end;
    end;
  end;
  case Data.GroupState of
    vgsUndefined:
      begin
        // do nothing
      end;
    vgsUnChecked:
      begin
        vstObjects.CheckState[Node] := csUncheckedNormal;
      end;
    vgsChecked:
      begin
        vstObjects.CheckState[Node] := csCheckedNormal;
      end;
    vgs3State:
      begin
        vstObjects.CheckState[Node] := csMixedNormal;
      end;
    else
      begin
        Assert(False);
      end;
  end;
end;

procedure TfrmCustomSelectObjects.SetSubsequentNodesCheckState(
  var StateChanged: Boolean; ScreenObject: TScreenObject;
  var ChildData: PMyRec);
begin
  if ShouldCheckBoxBeChecked(ScreenObject) then
  begin
    if ChildData.GroupState = vgsUnChecked then
    begin
      ChildData.GroupState := vgs3State;
      StateChanged := True;
    end;
  end
  else
  begin
    if ChildData.GroupState = vgsChecked then
    begin
      ChildData.GroupState := vgs3State;
      StateChanged := True;
    end;
  end;
end;

procedure TfrmCustomSelectObjects.UpdateChildCheck(Node: PVirtualNode);
var
  Index: integer;
  GroupState: TVisibleGroupState;
  ChildNode: PVirtualNode;
  Data, ChildData: PMyRec;
  AScreenObject: TScreenObject;
  ChildCount: integer;
begin
  if Node = nil then
  begin
    Exit;
  end;
  FSettingData3 := True;
  try
    GroupState := vgsUndefined;
    Data := vstObjects.GetNodeData(Node);
    ChildNode := nil;
    // Convert vstObjects.ChildCount[Node] from Cardinal to integer to
    // prevent Interger overflow when  vstObjects.ChildCount[Node] = 0.
    ChildCount := vstObjects.ChildCount[Node];
    Assert(Data.ScreenObjects.Count= ChildCount);
    for Index := 0 to ChildCount -1 do
    begin
      AScreenObject := Data.ScreenObjects[Index];
      if Index = 0 then
      begin
        ChildNode := vstObjects.GetFirstChild(Node);
        if ShouldCheckBoxBeChecked(AScreenObject) then
        begin
          GroupState := vgsChecked;
        end
        else
        begin
          GroupState := vgsUnChecked;
        end;
      end
      else
      begin
        ChildNode := vstObjects.GetNextSibling(ChildNode);
        if ShouldCheckBoxBeChecked(AScreenObject) then
        begin
          if GroupState <> vgsChecked then
          begin
            GroupState := vgs3State;
          end;
        end
        else
        begin
          if GroupState <> vgsUnChecked then
          begin
            GroupState := vgs3State;
          end;
        end;
      end;
      ChildData := vstObjects.GetNodeData(ChildNode);
      if ShouldCheckBoxBeChecked(AScreenObject) then
      begin
        vstObjects.CheckState[ChildNode] := csCheckedNormal;
        ChildData.GroupState := vgsChecked;
      end
      else
      begin
        vstObjects.CheckState[ChildNode] := csUncheckedNormal;
        ChildData.GroupState := vgsUnChecked;
      end;
    end;
    case GroupState of
      vgsUndefined:
        begin
          vstObjects.CheckState[Node] := csUncheckedNormal;
        end;
      vgsUnChecked:
        begin
          vstObjects.CheckState[Node] := csUncheckedNormal;
        end;
      vgsChecked:
        begin
          vstObjects.CheckState[Node] := csCheckedNormal;
        end;
      vgs3State:
        begin
          vstObjects.CheckState[Node] := csMixedNormal;
        end;
      else
      begin
        Assert(False);
      end;
    end;
    Data.GroupState := GroupState;
  finally
    FSettingData3 := False;
  end;
end;

procedure TfrmCustomSelectObjects.HandleCheckChange(Node: PVirtualNode; Sender: TBaseVirtualTree);
var
  ChildIndex: Integer;
  Data: PMyRec;
  Index: Integer;
  AScreenObject: TScreenObject;
  ParentData: PMyRec;
  ChildNode: PVirtualNode;
  ChildCount: Integer;
begin
  if (Node = FvstDataSetRootNode)
    or (Node = FvstPhastBoundaryConditionsRoot)
    or (Node = FvstModflowBoundaryConditionsRoot) then
  begin
    if Sender.CheckState[Node] <> csMixedNormal then
    begin
      ChildNode := nil;
      // Convert vstObjects.ChildCount[Node] from Cardinal to integer to
      // prevent Interger overflow when  vstObjects.ChildCount[Node] = 0.
      ChildCount := Sender.ChildCount[Node];
      for ChildIndex := 0 to ChildCount - 1 do
      begin
        if ChildIndex = 0 then
        begin
          ChildNode := Sender.GetFirstChild(Node);
        end
        else
        begin
          ChildNode := Sender.GetNextSibling(ChildNode);
        end;
        Data := Sender.GetNodeData(ChildNode);
        if Data.ScreenObjects <> nil then
        begin
          case Sender.CheckState[Node] of
            csUncheckedNormal:
              begin
                for Index := 0 to Data.ScreenObjects.Count - 1 do
                begin
                  AScreenObject := Data.ScreenObjects[Index];
                  HandleUnchecked(AScreenObject);
                end;
              end;
            csCheckedNormal:
              begin
                for Index := 0 to Data.ScreenObjects.Count - 1 do
                begin
                  AScreenObject := Data.ScreenObjects[Index];
                  HandleChecked(AScreenObject);
                end;
              end;
            csMixedNormal:
              begin
              end;
          else
            // do nothing.
            begin
              Assert(False);
            end;
          end;
        end
        else
        begin
          HandleChildren(Sender, ChildNode);
        end;
      end;
    end;
  end
  else
  begin
    Data := Sender.GetNodeData(Node);
    if (Data <> nil) and (Data.ScreenObjects <> nil) then
    begin
      case Sender.CheckState[Node] of
        csUncheckedNormal:
          begin
            for Index := 0 to Data.ScreenObjects.Count - 1 do
            begin
              AScreenObject := Data.ScreenObjects[Index];
              HandleUnchecked(AScreenObject);
            end;
          end;
        csCheckedNormal:
          begin
            for Index := 0 to Data.ScreenObjects.Count - 1 do
            begin
              AScreenObject := Data.ScreenObjects[Index];
              HandleChecked(AScreenObject);
            end;
          end;
        csMixedNormal:
          begin
          end;
      else
        // do nothing.
        begin
          Assert(False);
        end;
      end;
    end
    else
    begin
      HandleChildren(Sender, Node);
      ParentData := Sender.GetNodeData(Node.Parent);
      if (ParentData <> nil) and (ParentData.ScreenObjects <> nil) then
      begin
        AScreenObject := ParentData.ScreenObjects[Node.Index];
        case Sender.CheckState[Node] of
          csUncheckedNormal:
            begin
              HandleUnchecked(AScreenObject);
            end;
          csCheckedNormal:
            begin
              HandleChecked(AScreenObject);
            end;
        else
          begin
            Assert(False);
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmCustomSelectObjects.UpdateDisplay;
var
  Index, ChildIndex: integer;
  vstNode, ChildNode: PVirtualNode;
  ChildCount: integer;
  procedure UpdateDataSetChildren(Node: PVirtualNode);
  var
    ChildIndex, ChildCount: integer;
    ChildNode: PVirtualNode;
    ChildData: PMyRec;
  begin
    ChildCount := vstObjects.ChildCount[Node];
    ChildNode := nil;
    for ChildIndex := 0 to ChildCount - 1 do
    begin
      if ChildIndex = 0 then
      begin
        ChildNode := vstObjects.GetFirstChild(Node);
      end
      else
      begin
        ChildNode := vstObjects.GetNextSibling(ChildNode);
      end;
      ChildData := vstObjects.GetNodeData(ChildNode);
      if ChildData.IsDataSetNode then
      begin
        UpdateChildCheck(ChildNode);
        SetRootNodeStates(Node);
      end
      else
      begin
        UpdateDataSetChildren(ChildNode);
      end;
    end;
  end;
begin
  vstObjects.BeginUpdate;
  try
    vstNode := nil;
    for Index := 0 to vstObjects.RootNodeCount -1 do
    begin
      if Index = 0 then
      begin
        vstNode := vstObjects.GetFirstChild(vstObjects.RootNode);
      end
      else
      begin
        vstNode := vstObjects.GetNextSibling(vstNode);
      end;
      ChildNode := nil;
      if (vstNode = FvstPhastBoundaryConditionsRoot)
        or (vstNode = FvstModflowBoundaryConditionsRoot) then
      begin
        // Convert vstObjects.ChildCount[Node] from Cardinal to integer to
        // prevent Interger overflow when  vstObjects.ChildCount[Node] = 0.
        ChildCount := vstObjects.ChildCount[vstNode];
        for ChildIndex := 0 to ChildCount - 1 do
        begin
          if ChildIndex = 0 then
          begin
            ChildNode := vstObjects.GetFirstChild(vstNode);
          end
          else
          begin
            ChildNode := vstObjects.GetNextSibling(ChildNode);
          end;
          UpdateChildCheck(ChildNode);
        end;
        SetRootNodeStates(vstNode);
      end
      else if (vstNode = FvstDataSetRootNode) then
      begin
        UpdateDataSetChildren(vstNode);
      end
      else
      begin
        UpdateChildCheck(vstNode);
      end;
    end;
  finally
    vstObjects.EndUpdate;
  end;

  if FSettingData then
  begin
    Exit;
  end;
end;

procedure TfrmCustomSelectObjects.UpdateScreenObjects;
var
  AScreenObject: TScreenObject;
  Index: Integer;
  DataSetList: TStringList;
  SortedDataSetList: TStringList;
begin
  if FSettingData then
  begin
    Exit;
  end;
  FSettingData := True;
  try
    DataSetList := TStringList.Create;
    SortedDataSetList := TStringList.Create;
    try
      vstObjects.BeginUpdate;
      try
        CreateBaseNodes(DataSetList);
        SortedDataSetList.Assign(DataSetList);
        SortedDataSetList.Sorted := True;
        for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
        begin
          AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
          AddScreenObject(AScreenObject, SortedDataSetList);
        end;
        SortScreenObjectLists;
        ClearBaseNodes(DataSetList);
        UpdateChildCheck(FvstAllObjectsNode);
        UpdateChildCheck(FvstOtherObjectsNode);
        SetRootNodeStates(FvstPhastBoundaryConditionsRoot);
        SetRootNodeStates(FvstModflowBoundaryConditionsRoot);
        UpdateChildCheck(FvstModpathRoot);
        SetRootNodeStates(FvstDataSetRootNode);
      finally
        vstObjects.EndUpdate;
      end;
    finally
      SortedDataSetList.Free;
      DataSetList.Free;
    end;
  finally
    FSettingData := False;
  end;
end;

procedure TfrmCustomSelectObjects.GetData;
begin
  vstObjects.Clear;
  NilBaseNodes;
  UpdateScreenObjects;
end;

initialization
  // PVirtualNode will be cast to TObject.  This only works if they
  // are the same size.
  Assert(SizeOf(PVirtualNode) = SizeOf(TObject));

end.
