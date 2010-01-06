{@abstract(The main purpose of @name is to define @link(TfrmSearch)
  which is used to search for @link(TScreenObject)s based on what
  the @link(TScreenObject)s do.)}
unit frmSearchUnit;      

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, CheckLst, ExtCtrls, Buttons;

type
  {@abstract(@name is used to search for @link(TScreenObject)s based on what
    the @link(TScreenObject)s do.)}
  TfrmSearch = class(TfrmCustomGoPhast)
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
    // See @link(btnToggleClick).
    btnToggle: TButton;
    // @name: TCheckListBox;
    // @name displays a list of data sets and properties that can be used
    // for searching for @link(TScreenObject)s.
    clbChoice: TCheckListBox;
    // @name: TPanel;
    // @name holds the buttons and TRadioGroup at the bottom of @classname.
    pnlBottom: TPanel;
    // @name: TRadioGroup;
    // @name displays the direction used when searching for
    // @link(TScreenObject)s.  The @link(TScreenObject.ViewDirection) of the
    // @link(TScreenObject) must match the value displayed in @name for the
    // @link(TScreenObject) to be selected.
    rgDirecton: TRadioGroup;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name toggles the state of all the check boxes in @link(clbChoice).
    procedure btnToggleClick(Sender: TObject);
    // @name calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
  private
    // @name displays the criteria that can be used for
    // selecting @link(TScreenObject)s.
    procedure GetData;
    // @name selects @link(TScreenObject)s using the criteria
    // specified in @link(clbChoice) and @link(rgDirecton).
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit, DataSetUnit, ScreenObjectUnit, UndoItemsScreenObjects,
  GoPhastTypes, frmGoToUnit;

{$R *.dfm}

{ TfrmSearch }

type
  TSearchChoice = (scCellSize, scSpecifiedHead, scFlux, scLeaky, scRiver,
    scWell);

procedure TfrmSearch.GetData;
var
  Index: integer;
  DataSet: TDataArray;
begin
  clbChoice.Items.Add('Cell Size');
  clbChoice.Items.Add('Specified Head');
  clbChoice.Items.Add('Flux');
  clbChoice.Items.Add('Leaky');
  clbChoice.Items.Add('River');
  clbChoice.Items.Add('Well');

  for Index := 0 to frmGoPhast.PhastModel.DataSetCount - 1 do
  begin
    DataSet := frmGoPhast.PhastModel.DataSets[Index];
    clbChoice.Items.AddObject(DataSet.Name, DataSet);
  end;

end;

procedure TfrmSearch.SetData;
var
  Index: integer;
  AScreenObject, FirstScreenObject: TScreenObject;
  ViewDirection: TViewDirection;
  BoundaryIndex: TSearchChoice;
  GoToNextScreenObject: boolean;
  DataSetIndex: integer;
  DataSet: TDataArray;
  Undo: TUndoChangeSelection;
  XCoordinate, YCoordinate: double;
begin
  FirstScreenObject := nil;
  ViewDirection := TViewDirection(rgDirecton.ItemIndex);

  Undo := TUndoChangeSelection.Create;

  for Index := frmGoPhast.PhastModel.ScreenObjectCount - 1 downto 0 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];

    if AScreenObject.ViewDirection <> ViewDirection then
    begin
      AScreenObject.Selected := False;
      Continue;
    end;

    if clbChoice.Checked[Ord(scCellSize)] then
    begin
      if AScreenObject.CellSizeUsed then
      begin
        AScreenObject.Selected := True;
        FirstScreenObject := AScreenObject;
        Continue;
      end;
    end;

    GoToNextScreenObject := False;
    for BoundaryIndex := scSpecifiedHead to High(TSearchChoice) do
    begin
      if clbChoice.Checked[Ord(BoundaryIndex)] then
      begin
        if Ord(AScreenObject.BoundaryTypeUsed) = Ord(BoundaryIndex) then
        begin
          AScreenObject.Selected := True;
          FirstScreenObject := AScreenObject;
          GoToNextScreenObject := True;
          break;
        end;
      end;
    end;

    if GoToNextScreenObject then
    begin
      Continue;
    end;

    for DataSetIndex := Ord(High(TSearchChoice)) + 1 to clbChoice.Items.Count - 1
      do
    begin
      if clbChoice.Checked[DataSetIndex] then
      begin
        DataSet := clbChoice.Items.Objects[DataSetIndex] as TDataArray;
        if AScreenObject.IndexOfDataSet(DataSet) >= 0 then
        begin
          AScreenObject.Selected := True;
          FirstScreenObject := AScreenObject;
          GoToNextScreenObject := True;
          break;
        end;
      end;
    end;

    if GoToNextScreenObject then
    begin
      Continue;
    end;

    AScreenObject.Selected := False;
  end;

  if FirstScreenObject = nil then
  begin
    Beep;
    MessageDlg('No objects were found that meet the selection criteria.',
      mtInformation, [mbOK], 0);
  end
  else
  begin
    XCoordinate := FirstScreenObject.Points[0].X;
    YCoordinate := FirstScreenObject.Points[0].Y;
    case FirstScreenObject.ViewDirection of
      vdTop:
        begin
          SetTopPosition(XCoordinate, YCoordinate);
        end;
      vdFront:
        begin
          SetFrontPosition(XCoordinate, YCoordinate);
        end;
      vdSide:
        begin
          SetSidePosition(YCoordinate, XCoordinate);
        end;
    else
      Assert(False);
    end;
    ModalResult := mrOK;
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

procedure TfrmSearch.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmSearch.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmSearch.btnToggleClick(Sender: TObject);
var
  Index: integer;
begin
  inherited;
  for Index := 0 to clbChoice.Items.Count - 1 do
  begin
    clbChoice.Checked[Index] := not clbChoice.Checked[Index];
  end;
end;

end.

