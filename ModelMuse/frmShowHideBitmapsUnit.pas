{@abstract(The main purpose of @name is to define @link(TfrmShowHideBitmaps)
  which is used to show or hide imported
  bitmaps stored in @link(TCompressedBitmapItem)s.)}
unit frmShowHideBitmapsUnit;

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Buttons, ExtCtrls, CheckLst;

type
  {@abstract(@name is used to show or hide imported
    bitmaps stored in @link(TCompressedBitmapItem)s.)}
  TfrmShowHideBitmaps = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes @classname.
    btnClose: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // @name: TButton;
    // Clicking @name causes all the checkboxes in @link(clbBitmaps)
    // to be checked.
    // See @link(btnShowClick).
    btnShowAll: TButton;
    // @name: TButton;
    // Clicking @name causes none of the checkboxes in @link(clbBitmaps)
    // to be checked.
    // See @link(btnShowClick).
    btnShowNone: TButton;
    // @name: TButton;
    // See @link(btnToggleClick).
    btnToggle: TButton;
    // @name: TCheckListBox;
    // @name displays the names of the imported bitmaps.
    // The checked state of each bitmap reflects whether it is visible or not.
    // See @link(clbBitmapsClickCheck).
    clbBitmaps: TCheckListBox;
    // @name: TPanel;
    // @name holds the buttons on the bottom of @classname.
    pnlBottom: TPanel;
    // @name is the event-handler for clbBitmaps.OnClickCheck
    // Clicking on a checkbox in @link(clbBitmaps) causes the
    // @link(TCompressedBitmapItem.Visible) property of the
    // associated @link(TCompressedBitmapItem) to be toggled.
    procedure clbBitmapsClickCheck(Sender: TObject);
    // @name calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
    // @name shows or hides all the @link(TCompressedBitmapItem)s
    // depending on whether Sender is @link(btnShowAll) or @link(btnShowNone).
    procedure btnShowClick(Sender: TObject);
    // @name causes the checked state
    // of all the checkboxes in @link(clbBitmaps)
    // to be toggled.
    procedure btnToggleClick(Sender: TObject);
  private
    // @name fills @link(clbBitmaps) with the names of the
    //  imported bitmaps
    // and sets the checked state of each line depending
    // on the @link(TCompressedBitmapItem.Visible) property of the
    // @link(TCompressedBitmapItem).
    procedure GetData;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit, CompressedImageUnit;

{$R *.dfm}

{ TfrmShowHideBitmaps }

procedure TfrmShowHideBitmaps.GetData;
var
  BitmapIndex: integer;
  Item: TCompressedBitmapItem;
begin
  for BitmapIndex := 0 to frmGoPhast.PhastModel.Bitmaps.Count - 1 do
  begin
    Item := frmGoPhast.PhastModel.Bitmaps.Items[BitmapIndex] as
      TCompressedBitmapItem;
    clbBitmaps.Items.AddObject(Item.Name, Item);
    clbBitmaps.Checked[BitmapIndex] := Item.Visible;
  end;
end;

procedure TfrmShowHideBitmaps.clbBitmapsClickCheck(Sender: TObject);
var
  BitmapIndex: integer;
  Item: TCompressedBitmapItem;
begin
  inherited;
  for BitmapIndex := 0 to frmGoPhast.PhastModel.Bitmaps.Count - 1 do
  begin
    Item := frmGoPhast.PhastModel.Bitmaps.Items[BitmapIndex] as
      TCompressedBitmapItem;
    Item.Visible := clbBitmaps.Checked[BitmapIndex];
  end;
end;

procedure TfrmShowHideBitmaps.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmShowHideBitmaps.btnShowClick(Sender: TObject);
var
  Index: integer;
  ShouldCheck: boolean;
begin
  inherited;
  ShouldCheck := (Sender = btnShowAll);
  for Index := 0 to clbBitmaps.Items.Count - 1 do
  begin
    clbBitmaps.Checked[Index] := ShouldCheck;
  end;
  clbBitmapsClickCheck(nil);
end;

procedure TfrmShowHideBitmaps.btnToggleClick(Sender: TObject);
var
  Index: integer;
begin
  inherited;
  for Index := 0 to clbBitmaps.Items.Count - 1 do
  begin
    clbBitmaps.Checked[Index] := not clbBitmaps.Checked[Index];
  end;
  clbBitmapsClickCheck(nil);
end;

end.

