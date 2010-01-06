unit framePackageUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, JvExStdCtrls, JvCheckBox,
  ModflowPackageSelectionUnit, RbwController;

type
  TframePackage = class(TFrame)
    lblComments: TLabel;
    memoComments: TMemo;
    lblPackage: TLabel;
    rcSelectionController: TRbwController;
  private
    Node: TTreeNode;
    FSelected: boolean;
    FSelectionType: TSelectionType;
    FOnSelectedChange: TNotifyEvent;
    FCanSelect: boolean;
    procedure SetNodeStateIndex;
    procedure SetCanSelect(const Value: boolean);
  { Private declarations }
  protected
    procedure SetSelected(const Value: boolean); virtual;
    // @name moves the control on @classname to the first
    // tab of ParentPageControl.
    // @name is used in @link(TframeGMG).
    procedure MoveControlsToTab(ParentPageControl: TPageControl);
  public
    property CanSelect: boolean read FCanSelect write SetCanSelect;
    procedure GetData(Package: TModflowPackageSelection); virtual;
    procedure SetData(Package: TModflowPackageSelection); virtual;
    property Selected: boolean read FSelected write SetSelected;
    property SelectionType: TSelectionType read FSelectionType
      write FSelectionType;
    constructor Create(AOwner: TComponent); override;
  published
    property OnSelectedChange: TNotifyEvent read FOnSelectedChange
      write FOnSelectedChange;
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TframePackage }


constructor TframePackage.Create(AOwner: TComponent);
begin
  inherited;
  FCanSelect := True;
end;

procedure TframePackage.SetNodeStateIndex;
begin
  if Node <> nil then
  begin
    case SelectionType of
      stCheckBox:
        begin
          if CanSelect then
          begin
            Node.StateIndex := Ord(FSelected) + 1;
          end
          else
          begin
            Node.StateIndex := 6;
          end;
        end;
      stRadioButton:
        begin
          if CanSelect then
          begin
            Node.StateIndex := Ord(FSelected) + 4;
          end
          else
          begin
            Node.StateIndex := 7
          end;
        end;
    end;
    Node.TreeView.Invalidate;
  end;
end;

procedure TframePackage.GetData(Package: TModflowPackageSelection);
begin
  SelectionType := Package.SelectionType;
  Node := Package.Node;
  Selected := Package.IsSelected;
  memoComments.Lines := Package.Comments;
  lblPackage.Caption := Package.PackageIdentifier;
end;

procedure TframePackage.SetCanSelect(const Value: boolean);
begin
  if FCanSelect <> Value then
  begin
    FCanSelect := Value;
    SetNodeStateIndex;
  end;
end;

procedure TframePackage.SetData(Package: TModflowPackageSelection);
begin
  Package.IsSelected := Selected;
  Package.Comments := memoComments.Lines;
end;

procedure TframePackage.SetSelected(const Value: boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    if Assigned(FOnSelectedChange) then
    begin
      FOnSelectedChange(self);
    end;
  end;
  rcSelectionController.Enabled := Value;
  SetNodeStateIndex;
end;

procedure TframePackage.MoveControlsToTab(ParentPageControl: TPageControl);
var
  AControl: TControl;
  OldHeight: Integer;
  Index: Integer;
  DeltaHeight: Integer;
  ControlsToMoveDown: TList;
  DestinationTab: TTabSheet;
begin
  if not (csDesigning in ComponentState) then
  begin
    DestinationTab := ParentPageControl.Pages[0];
    ControlsToMoveDown := TList.Create;
    try
      for Index := 0 to DestinationTab.ControlCount - 1 do
      begin
        ControlsToMoveDown.Add(DestinationTab.Controls[Index]);
      end;
      OldHeight := ParentPageControl.Height;
      lblPackage.Parent := DestinationTab;
      lblComments.Parent := DestinationTab;
      memoComments.Parent := DestinationTab;
      ParentPageControl.Align := alClient;
      DeltaHeight := ParentPageControl.Height - OldHeight;
      for Index := 0 to ControlsToMoveDown.Count - 1 do
      begin
        AControl := ControlsToMoveDown[Index];
        AControl.Top := AControl.Top + DeltaHeight;
        AControl.Anchors := [akLeft, akBottom];
      end;
    finally
      ControlsToMoveDown.Free;
    end;
  end;
end;

end.
