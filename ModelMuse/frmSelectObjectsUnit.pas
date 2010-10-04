{@abstract(The main purpose of @name is to define @link(TfrmSelectObjects)
  which is used to select @link(TScreenObject)s by selecting
  them by name.)}
unit frmSelectObjectsUnit;

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, ComCtrls, Buttons, ExtCtrls;

{ TODO : Consider using a two panel selector (Tidwell pattern 13). }  

type
  {@abstract(@name is used to select @link(TScreenObject)s by selecting
    them by name.)}
  TfrmSelectObjects = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes @classname without changing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TButton;
    // See @link(btnSelectClick).
    btnSelectAll: TButton;
    // @name: TButton;
    // See @link(btnSelectClick).
    btnSelectNone: TButton;
    // @name: TButton;
    // See @link(btnToggleClick).
    btnToggle: TButton;
    // @name: TCheckBox;
    // @name controls whether or not @link(TScreenObject)s whose
    // @link(TScreenObject.Visible) property is @false will be
    // displayed.
    // See @link(cbIncludeHiddenObjectsClick) and @link(GetData).
    cbIncludeHiddenObjects: TCheckBox;
    // @name: TPageControl;
    // @name holds @link(tabFront), @link(tabSide), and @link(tabTop).
    pcObjects: TPageControl;
    // @name: TPanel;
    // @name holds the controls on the bottom of the @classname.
    pnlBottom: TPanel;
    // @name: TListView;
    // @name displays the @link(TScreenObject)s on the front view of the model.
    lvFront: TListView;
    // @name: TListView;
    // @name displays the @link(TScreenObject)s on the side view of the model.
    lvSide: TListView;
    // @name: TListView;
    // @name displays the @link(TScreenObject)s on the top view of the model.
    lvTop: TListView;
    // @name: TTabSheet;
    // @name holds @link(lvFront).
    tabFront: TTabSheet;
    // @name: TTabSheet;
    // @name holds @link(lvSide).
    tabSide: TTabSheet;
    // @name: TTabSheet;
    // @name holds @link(lvTop).
    tabTop: TTabSheet;
    edSearchTerm: TEdit;
    btnSelectByName: TButton;
    lblCount: TLabel;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name checks or unchecks all the checkboxes on the TListView
    // (@link(lvFront), @link(lvSide), or @link(lvTop)) that is on
    // the active page of @link(pcObjects).
    procedure btnSelectClick(Sender: TObject);
    // @name toggles the checked state of all the checkboxes on the TListView
    // (@link(lvFront), @link(lvSide), or @link(lvTop)) that is on
    // the active page of @link(pcObjects).
    procedure btnToggleClick(Sender: TObject);
    // @name calls @link(GetData).
    procedure cbIncludeHiddenObjectsClick(Sender: TObject);
    // @name initializes @classname and calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
    procedure btnSelectByNameClick(Sender: TObject);
    procedure lvTopDblClick(Sender: TObject);
    procedure UpdateCount;
    procedure lvTopChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    FGettingData: Boolean;
    // @name displays the @link(TScreenObject)s in
    // @link(lvFront), @link(lvSide), and @link(lvTop).
    // See @link(cbIncludeHiddenObjects).
    procedure GetData;
    // @name uses a @link(TUndoChangeSelection) to set the selected
    // @link(TScreenObject)s to the ones that are checked on the
    // active page of @link(pcObjects).
    procedure SetData;
    function GetCurrentListView: TListView;
//    function GetList: TListView;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit, ScreenObjectUnit, GoPhastTypes, UndoItemsScreenObjects;

{$R *.dfm}

{ TfrmSelectObjects }

procedure TfrmSelectObjects.GetData;
var
  Index: Integer;
  AScreenObject: TScreenObject;
  Item: TListItem;
  ListView: TListView;
  IncludeHiddenObjects: boolean;
  Objects: TStringList;
begin
  FGettingData := True;
  try
    IncludeHiddenObjects := cbIncludeHiddenObjects.Checked;
    lvTop.Items.Clear;
    lvFront.Items.Clear;
    lvSide.Items.Clear;
    Objects:= TStringList.Create;
    try
      for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
        if not AScreenObject.Deleted and (IncludeHiddenObjects or
          AScreenObject.Visible) then
        begin
          Objects.AddObject(AScreenObject.Name, AScreenObject);
        end;
      end;
      Objects.Sort;
      ListView := nil;
      for Index := 0 to Objects.Count - 1 do
      begin
        AScreenObject := Objects.Objects[Index] as TScreenObject;
        case AScreenObject.ViewDirection of
          vdTop: ListView := lvTop;
          vdFront: ListView := lvFront;
          vdSide: ListView := lvSide;
          else Assert(False);
        end;
        Item := ListView.Items.Add;
        Item.Caption := AScreenObject.Name;
        Item.Data := AScreenObject;
        Item.Checked := AScreenObject.Selected;
      end;
    finally
      Objects.Free;
    end;
    tabTop.TabVisible := lvTop.Items.Count <> 0;
    tabFront.TabVisible := lvFront.Items.Count <> 0;
    tabSide.TabVisible := lvSide.Items.Count <> 0;
    if not (tabTop.TabVisible or tabFront.TabVisible or tabSide.TabVisible) then
    begin
      MessageDlg('Your model has no objects.', mtInformation, [mbOK], 0);
      Exit;
    end;
  finally
    FGettingData := False;
  end;
  UpdateCount;
end;

procedure TfrmSelectObjects.lvTopChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  inherited;
  UpdateCount;
end;

procedure TfrmSelectObjects.lvTopDblClick(Sender: TObject);
var
  ListView: TListView;
  Item: TListItem;
  AScreenObject : TScreenObject;
  Index: Integer;
begin
  inherited;
   ListView := GetCurrentListView;
   if ListView <> nil then
   begin
     Item := ListView.Selected;
     if Item <> nil then
     begin
        AScreenObject := Item.Data;
        if AScreenObject <> nil then
        begin
          SelectAScreenObject(AScreenObject);
          frmGoPhast.EditScreenObjects;
          GetData;
          for Index := 0 to ListView.Items.Count - 1 do
          begin
            Item := ListView.Items[Index];
            if Item.Data = AScreenObject then
            begin
              ListView.Selected := Item;
              Item.MakeVisible(False);
              break;
            end;
          end;
        end;
     end;
   end;
end;

procedure TfrmSelectObjects.FormCreate(Sender: TObject);
begin
  inherited;
  pcObjects.ActivePageIndex := 0;
  GetData;
end;

procedure TfrmSelectObjects.SetData;
var
  ListView: TListView;
  Index: integer;
  Item: TListItem;
  Undo: TUndoChangeSelection;
  AScreenObject: TScreenObject;
begin
  ListView := GetCurrentListView;

  Undo := TUndoChangeSelection.Create;
  frmGoPhast.ResetSelectedScreenObjects;

  for Index := 0 to ListView.Items.Count - 1 do
  begin
    Item := ListView.Items[Index];
    if Item.Checked then
    begin
      AScreenObject := Item.Data;
      AScreenObject.Selected := True;
    end;
  end;

  Undo.SetPostSelection;

  if Undo.SelectionChanged then
  begin
    frmGoPhast.UndoStack.Submit(Undo);
  end
  else
  begin
    Undo.Free;
  end;
end;

procedure TfrmSelectObjects.UpdateCount;
var
  List: TListView;
  Index: Integer;
  Item: TListItem;
  Count: Integer;
begin
  if FGettingData then Exit;

  List := GetCurrentListView;
  Count := 0;
  for Index := 0 to List.Items.Count - 1 do
  begin
    Item := List.Items[Index];
    if Item.Checked then
    begin
      Inc(Count);
    end;
  end;
  lblCount.Caption := 'Selected objects = ' + IntToStr(Count)
end;

function TfrmSelectObjects.GetCurrentListView: TListView;
begin
  result := nil;
  case pcObjects.ActivePageIndex of
    0:
      // Top
      begin
        result := lvTop;
      end;
    1:
      // Front
      begin
        result := lvFront;
      end;
    2:
      // Side
      begin
        result := lvSide;
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmSelectObjects.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmSelectObjects.btnSelectClick(Sender: TObject);
var
  ListView: TListView;
  Index: integer;
  Item: TListItem;
  ShouldSelect: boolean;
begin
  inherited;
  ShouldSelect := Sender = btnSelectAll;
  ListView := GetCurrentListView;
  for Index := 0 to ListView.Items.Count - 1 do
  begin
    Item := ListView.Items[Index];
    Item.Checked := ShouldSelect;
  end;
  UpdateCount;
end;

procedure TfrmSelectObjects.btnToggleClick(Sender: TObject);
var
  ListView: TListView;
  Index: integer;
  Item: TListItem;
begin
  inherited;
  ListView := GetCurrentListView;
  for Index := 0 to ListView.Items.Count - 1 do
  begin
    Item := ListView.Items[Index];
    Item.Checked := not Item.Checked;
  end;
  UpdateCount
end;

procedure TfrmSelectObjects.cbIncludeHiddenObjectsClick(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmSelectObjects.btnSelectByNameClick(Sender: TObject);
var
  ListView: TListView;
  Index: integer;
  Item: TListItem;
  ShouldSelect: boolean;
begin
  inherited;
  ListView := GetCurrentListView;
  for Index := 0 to ListView.Items.Count - 1 do
  begin
    Item := ListView.Items[Index];
    ShouldSelect := Pos(edSearchTerm.Text, Item.Caption) > 0;
    Item.Checked := ShouldSelect;
  end;
  UpdateCount;
end;

end.

