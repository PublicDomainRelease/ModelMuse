unit frmSelectObjectsForEditingUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomSelectObjectsUnit, VirtualTrees, StdCtrls, Buttons,
  ExtCtrls, ScreenObjectUnit;

type
  TfrmSelectObjectsForEditing = class(TfrmCustomSelectObjects)
    btnOK: TBitBtn;
    rgViewDirection: TRadioGroup;
    btnDelete: TBitBtn;
    procedure FormCreate(Sender: TObject); override;
    procedure vstObjectsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure btnOKClick(Sender: TObject);
    procedure rgViewDirectionClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    FListOfScreenObjects: TList;
    procedure SetData;
    procedure UpdateScreenObjectList;
    { Private declarations }
  protected
    function ShouldCheckBoxBeChecked(ScreenObject: TScreenObject): boolean; override;
    procedure HandleChecked(AScreenObject: TScreenObject); override;
    procedure HandleUnchecked(AScreenObject: TScreenObject); override;
    function CanSelect(ScreenObject: TScreenObject): boolean; override;
  public
    destructor Destroy; override;
    { Public declarations }
  end;

implementation

uses
  frmGoPhastUnit, frmScreenObjectPropertiesUnit, GoPhastTypes,
  UndoItemsScreenObjects;

{$R *.dfm}

procedure TfrmSelectObjectsForEditing.btnDeleteClick(Sender: TObject);
var
  ListOfScreenObjects: TScreenObjectList;
  Index: Integer;
  ScreenObject: TScreenObject;
  Undo: TUndoDeleteScreenObjects;
begin
  inherited;
  if FListOfScreenObjects.Count > 0 then
  begin
    ListOfScreenObjects:= TScreenObjectList.Create;
    try
      ListOfScreenObjects.Capacity := FListOfScreenObjects.Count;
      for Index := 0 to FListOfScreenObjects.Count - 1 do
      begin
        ScreenObject := FListOfScreenObjects[Index];
        ListOfScreenObjects.Add(ScreenObject);
      end;

      Undo := TUndoDeleteScreenObjects.Create(ListOfScreenObjects);
      frmGoPhast.UndoStack.Submit(Undo);
    finally
      ListOfScreenObjects.Free;
    end;
  end;
end;

procedure TfrmSelectObjectsForEditing.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

function TfrmSelectObjectsForEditing.CanSelect(
  ScreenObject: TScreenObject): boolean;
begin
  result := Ord(ScreenObject.ViewDirection) = rgViewDirection.ItemIndex;
end;

destructor TfrmSelectObjectsForEditing.Destroy;
begin
  FListOfScreenObjects.Free;
  inherited;
end;

procedure TfrmSelectObjectsForEditing.UpdateScreenObjectList;
var
  ScreenObject: TScreenObject;
  Index: Integer;
begin
  FListOfScreenObjects.Clear;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    if ScreenObject.Selected
      and (Ord(ScreenObject.ViewDirection) = rgViewDirection.ItemIndex) then
    begin
      FListOfScreenObjects.Add(ScreenObject);
    end;
  end;
end;

procedure TfrmSelectObjectsForEditing.FormCreate(Sender: TObject);
begin
  FListOfScreenObjects:= TList.Create;
  inherited;
  UpdateScreenObjectList;
  GetData;
end;

procedure TfrmSelectObjectsForEditing.HandleChecked(
  AScreenObject: TScreenObject);
begin
  if FListOfScreenObjects.IndexOf(AScreenObject)< 0 then
  begin
    FListOfScreenObjects.Add(AScreenObject)
  end;
end;

procedure TfrmSelectObjectsForEditing.HandleUnchecked(
  AScreenObject: TScreenObject);
begin
  FListOfScreenObjects.Remove(AScreenObject);
end;

procedure TfrmSelectObjectsForEditing.rgViewDirectionClick(Sender: TObject);
begin
  inherited;
  if FListOfScreenObjects <> nil then
  begin
    UpdateScreenObjectList;
    GetData;
  end;
end;

procedure TfrmSelectObjectsForEditing.SetData;
begin
  if not frmGoPhast.CanEdit then Exit;
  frmGoPhast.CanEdit := False;
  try
    if FListOfScreenObjects.Count > 0 then
    begin
      Assert(frmScreenObjectProperties <> nil);

      frmScreenObjectProperties.GetDataForMultipleScreenObjects(
        FListOfScreenObjects);
      frmScreenObjectProperties.ShowModal
    end;
  finally
    frmGoPhast.CanEdit := True;
  end;
end;

function TfrmSelectObjectsForEditing.ShouldCheckBoxBeChecked(
  ScreenObject: TScreenObject): boolean;
begin
  result := FListOfScreenObjects.IndexOf(ScreenObject) >= 0;
end;

procedure TfrmSelectObjectsForEditing.vstObjectsChecked(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  inherited;
  if FSettingData or FSettingData2 or FSettingData3 then
  begin
    Exit;
  end;
  if (Node.Parent = nil) then
  begin
    Exit;
  end;
  if not FOkToDoCheck then
  begin
    Exit;
  end;
  Screen.Cursor := crHourGlass;
  FSettingData := True;
  Sender.BeginUpdate;
  try
    HandleCheckChange(Node, Sender);
  finally
    Sender.EndUpdate;
    FSettingData := False;
    Screen.Cursor := crDefault;
  end;
end;

end.