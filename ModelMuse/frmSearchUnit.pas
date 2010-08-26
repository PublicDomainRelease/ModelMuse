{@abstract(The main purpose of @name is to define @link(TfrmSearch)
  which is used to search for @link(TScreenObject)s based on what
  the @link(TScreenObject)s do.)}
unit frmSearchUnit;      

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, CheckLst, ExtCtrls, Buttons, frmCustomSelectObjectsUnit,
  VirtualTrees, ScreenObjectUnit, UndoItemsScreenObjects;

type
  {@abstract(@name is used to search for @link(TScreenObject)s based on what
    the @link(TScreenObject)s do.)}
  TfrmSearch = class(TfrmCustomSelectObjects)
    rgDirecton: TRadioGroup;
    // @name calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
    procedure vstObjectsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure FormDestroy(Sender: TObject); override;
    procedure rgDirectonClick(Sender: TObject);
  private
    FListOfObjects: TList;
    FUndoChangeSelection: TUndoChangeSelection;
    // @name displays the criteria that can be used for
    // selecting @link(TScreenObject)s.
    procedure GetData;
    procedure RefreshListOfObjects;
    // @name selects @link(TScreenObject)s using the criteria
    // specified in @link(clbChoice) and @link(rgDirecton).
//    procedure SetData;
    { Private declarations }
  protected
    function ShouldCheckBoxBeChecked(ScreenObject: TScreenObject): boolean;
      override;
    procedure HandleChecked(AScreenObject: TScreenObject); override;
    procedure HandleUnchecked(AScreenObject: TScreenObject); override;
    function CanSelect(ScreenObject: TScreenObject): boolean; override;
  public
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit, DataSetUnit, 
  GoPhastTypes, frmGoToUnit;

{$R *.dfm}

{ TfrmSearch }

type
  TSearchChoice = (scCellSize, scSpecifiedHead, scFlux, scLeaky, scRiver,
    scWell);

procedure TfrmSearch.GetData;
//var
//  Index: integer;
//  DataSet: TDataArray;
begin
//  clbChoice.Items.Add('Cell Size');
//  clbChoice.Items.Add('Specified Head');
//  clbChoice.Items.Add('Flux');
//  clbChoice.Items.Add('Leaky');
//  clbChoice.Items.Add('River');
//  clbChoice.Items.Add('Well');
//
//  for Index := 0 to frmGoPhast.PhastModel.DataSetCount - 1 do
//  begin
//    DataSet := frmGoPhast.PhastModel.DataSets[Index];
//    clbChoice.Items.AddObject(DataSet.Name, DataSet);
//  end;


  vstObjects.Clear;
  NilBaseNodes;
  UpdateScreenObjects;
  RefreshListOfObjects;

end;

procedure TfrmSearch.HandleChecked(AScreenObject: TScreenObject);
begin
  inherited;
  if not AScreenObject.Selected then
  begin
    FListOfObjects.Add(AScreenObject);
//    FCount := FUndoShowHide.AddScreenObjectToChange(AScreenObject);
  end;
end;

procedure TfrmSearch.HandleUnchecked(AScreenObject: TScreenObject);
begin
  inherited;
  if AScreenObject.Selected then
  begin
    FListOfObjects.Remove(AScreenObject);
//    FCount := FUndoShowHide.AddScreenObjectToChange(AScreenObject);
  end;
end;

procedure TfrmSearch.rgDirectonClick(Sender: TObject);
begin
  inherited;
  GetData;
end;

//procedure TfrmSearch.SetData;
//var
//  Index: integer;
//  AScreenObject, FirstScreenObject: TScreenObject;
//  ViewDirection: TViewDirection;
//  BoundaryIndex: TSearchChoice;
//  GoToNextScreenObject: boolean;
//  DataSetIndex: integer;
//  DataSet: TDataArray;
//  Undo: TUndoChangeSelection;
//  XCoordinate, YCoordinate: double;
//begin
//  FirstScreenObject := nil;
//  ViewDirection := TViewDirection(rgDirecton.ItemIndex);
//
//  Undo := TUndoChangeSelection.Create;
//
//  for Index := frmGoPhast.PhastModel.ScreenObjectCount - 1 downto 0 do
//  begin
//    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
//
//    if AScreenObject.Deleted then
//    begin
//      Continue;
//    end;
//
//    if AScreenObject.ViewDirection <> ViewDirection then
//    begin
//      AScreenObject.Selected := False;
//      Continue;
//    end;
//
//    if clbChoice.Checked[Ord(scCellSize)] then
//    begin
//      if AScreenObject.CellSizeUsed then
//      begin
//        AScreenObject.Selected := True;
//        FirstScreenObject := AScreenObject;
//        Continue;
//      end;
//    end;
//
//    GoToNextScreenObject := False;
//    for BoundaryIndex := scSpecifiedHead to High(TSearchChoice) do
//    begin
//      if clbChoice.Checked[Ord(BoundaryIndex)] then
//      begin
//        if Ord(AScreenObject.BoundaryTypeUsed) = Ord(BoundaryIndex) then
//        begin
//          AScreenObject.Selected := True;
//          FirstScreenObject := AScreenObject;
//          GoToNextScreenObject := True;
//          break;
//        end;
//      end;
//    end;
//
//    if GoToNextScreenObject then
//    begin
//      Continue;
//    end;
//
//    for DataSetIndex := Ord(High(TSearchChoice)) + 1 to clbChoice.Items.Count - 1
//      do
//    begin
//      if clbChoice.Checked[DataSetIndex] then
//      begin
//        DataSet := clbChoice.Items.Objects[DataSetIndex] as TDataArray;
//        if AScreenObject.IndexOfDataSet(DataSet) >= 0 then
//        begin
//          AScreenObject.Selected := True;
//          FirstScreenObject := AScreenObject;
//          GoToNextScreenObject := True;
//          break;
//        end;
//      end;
//    end;
//
//    if GoToNextScreenObject then
//    begin
//      Continue;
//    end;
//
//    AScreenObject.Selected := False;
//  end;
//
//  if FirstScreenObject = nil then
//  begin
//    Beep;
//    MessageDlg('No objects were found that meet the selection criteria.',
//      mtInformation, [mbOK], 0);
//  end
//  else
//  begin
//    XCoordinate := FirstScreenObject.Points[0].X;
//    YCoordinate := FirstScreenObject.Points[0].Y;
//    case FirstScreenObject.ViewDirection of
//      vdTop:
//        begin
//          SetTopPosition(XCoordinate, YCoordinate);
//        end;
//      vdFront:
//        begin
//          SetFrontPosition(XCoordinate, YCoordinate);
//        end;
//      vdSide:
//        begin
//          SetSidePosition(YCoordinate, XCoordinate);
//        end;
//    else
//      Assert(False);
//    end;
//    ModalResult := mrOK;
//  end;
//
//  Undo.SetPostSelection;
//
//  if Undo.SelectionChanged then
//  begin
//    frmGoPhast.UndoStack.Submit(Undo);
//  end
//  else
//  begin
//    Undo.Free;
//  end;
//
//end;

function TfrmSearch.ShouldCheckBoxBeChecked(
  ScreenObject: TScreenObject): boolean;
begin
  result := ScreenObject.Selected;
end;

procedure TfrmSearch.vstObjectsChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Index: Integer;
  AScreenObject: TScreenObject;
  ShouldRefresh: boolean;
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
  ShouldRefresh := False;
  Screen.Cursor := crHourGlass;
  FSettingData := True;
  Sender.BeginUpdate;
  try
    FUndoChangeSelection := TUndoChangeSelection.Create;
    try
      HandleCheckChange(Node, Sender);
      if (FListOfObjects.Count >= 0) then
      begin
        frmGoPhast.PhastModel.ResetSelectedScreenObjects;
        for Index := 0 to FListOfObjects.Count - 1 do
        begin
          AScreenObject := FListOfObjects[Index];
          AScreenObject.Selected := True;
        end;
        FUndoChangeSelection.SetPostSelection;
        frmGoPhast.UndoStack.Submit(FUndoChangeSelection);
        ShouldRefresh := True;
      end
      else
      begin
        FreeAndNil(FUndoChangeSelection);
      end;
    except
      FreeAndNil(FUndoChangeSelection);
      raise;
    end;
  finally
    Sender.EndUpdate;
    FSettingData := False;
    Screen.Cursor := crDefault;
  end;
  if ShouldRefresh then
  begin
    UpdateScreenObjects;
    RefreshListOfObjects;
  end;
end;

procedure TfrmSearch.FormCreate(Sender: TObject);
begin
  inherited;
  FListOfObjects := TList.Create;
  GetData;
end;

procedure TfrmSearch.FormDestroy(Sender: TObject);
begin
  inherited;
  FListOfObjects.Free;
end;

function TfrmSearch.CanSelect(ScreenObject: TScreenObject): boolean;
begin
  result := not ScreenObject.Deleted and
    (Ord(ScreenObject.ViewDirection) = rgDirecton.ItemIndex);
end;

procedure TfrmSearch.RefreshListOfObjects;
var
  AScreenObject: TScreenObject;
  Index: Integer;
begin
  FListOfObjects.Clear;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if CanSelect(AScreenObject) and AScreenObject.Selected then
    begin
      FListOfObjects.Add(AScreenObject);
    end;
  end;
end;

end.

