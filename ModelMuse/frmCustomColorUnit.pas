unit frmCustomColorUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, ComCtrls,
  Forms, Dialogs, frmCustomGoPhastUnit, GoPhastTypes, frameDisplayLimitUnit,
  StdCtrls, JvExStdCtrls, JvRichEdit, ExtCtrls, Mask, JvExMask, JvSpin,
  JvExControls, JvxSlider, Buttons, DataSetUnit, SubscriptionUnit,
  ClassificationUnit, frmGoPhastUnit, PhastModelUnit, TntStdCtrls,
  TntExDropDownEdit, TntExDropDownVirtualStringTree, VirtualTrees, JvExComCtrls,
  JvUpDown, Grids, RbwDataGrid4, RbwParser;

type
  TfrmCustomColor = class(TfrmCustomGoPhast)
    // @name displays "Data set or boundary condition".
    lblDataSet: TLabel;
    reComment: TJvRichEdit;
    // @name displays "Lower limit".
    lblLowerLimit: TLabel;
    // @name is used to set the maximum value used to color the grid.
    frameCheck3DMax: TframeDisplayLimit;
    // @name is used to set the minimum value used to color the grid.
    frameCheck3DMin: TframeDisplayLimit;
    // @name displays "Color scheme".
    lblColorScheme: TLabel;
    // @name displays a list of the different methods for converting
    // a value in the @link(TDataArray) used for coloring the @link(TPhastGrid)
    // to a color.
    comboColorScheme: TComboBox;
    // @name labels @link(seCycles).
    lblCycles: TLabel;
    // @name is used to give a preview of the color schemes listed in
    // @link(comboColorScheme).
    // See @link(pbColorSchemePaint).
    pbColorScheme: TPaintBox;
    seCycles: TJvSpinEdit;
    lblColorAdjustment: TLabel;
    jsColorExponent: TJvxSlider;
    seColorExponent: TJvSpinEdit;
    // Clicking @name @name displays help on the @classname.
    btnHelp: TBitBtn;
    // See @link(TfrmContourData.btnOKClick)
    // and @link(TfrmGridColor.btnOKClick).
    btnOK: TBitBtn;
    // Clicking @name closes the @classname without changing anything.
    btnCancel: TBitBtn;
    virttreecomboDataSets: TTntExDropDownVirtualStringTree;
    udDataSets: TJvUpDown;
    pcChoices: TPageControl;
    tabSelection: TTabSheet;
    tabFilters: TTabSheet;
    lblUpperLimit: TLabel;
    lblComment: TLabel;
    Panel1: TPanel;
    cbActiveOnly: TCheckBox;
    rdgValuesToIgnore: TRbwDataGrid4;
    lblValuesToIgnore: TLabel;
    seNumberOfValuesToIgnore: TJvSpinEdit;
    lblNumberOfValuesToIgnore: TLabel;
    cbLogTransform: TCheckBox;
    // @name gives a preview of the color scheme
    // selected in @link(comboColorScheme).
    procedure pbColorSchemePaint(Sender: TObject);
    // @name causes @link(pbColorScheme) to be redrawn.
    // See @link(pbColorSchemePaint).
    procedure comboColorSchemeChange(Sender: TObject);
    procedure seCyclesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure jsColorExponentChange(Sender: TObject);
    procedure seColorExponentChange(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject);
    procedure virttreecomboDataSetsDropDownTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure virttreecomboDataSetsDropDownTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure virttreecomboDataSetsChange(Sender: TObject);
    procedure udDataSetsChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure FormShow(Sender: TObject);
    procedure rdgValuesToIgnoreEndUpdate(Sender: TObject);
    procedure seNumberOfValuesToIgnoreChange(Sender: TObject);
  private
    // @name is implemented as a TObjectList.
    FDataSetDummyObjects: TList;
    FSelectedVirtNode: PVirtualNode;

  { Private declarations }
  protected
    // @name stores a list of the @link(TDataArray)s or boundary conditions
    // that can be displayed on the front view of the model.  The Objects
    // property of @name contains either the corresponding @link(TDataArray)
    // or the corresponding @link(TPhastTimeList).
    FFrontItems: TStringList;
    // @name stores a list of the @link(TDataArray)s or boundary conditions
    // that can be displayed on the side view of the model.  The Objects
    // property of @name contains either the corresponding @link(TDataArray)
    // or the corresponding @link(TPhastTimeList).
    FSideItems: TStringList;
    // @name stores a list of the @link(TDataArray)s or boundary conditions
    // that can be displayed on the top view of the model.  The Objects
    // property of @name contains either the corresponding @link(TDataArray)
    // or the corresponding @link(TPhastTimeList).
    FTopItems: TStringList;
    // @name fills @link(virttreecomboDataSets) with data about the things
    // that can be used to color the grid.
    // @name retrieves the data about the @link(TDataArray)
    // used to color the @link(TPhastGrid).
    function GetSelectedArray: TDataArray; virtual; abstract;
    procedure GetDataSets;
    procedure StoreDataSetsInLists;
    procedure FinalizeList(List: TStringList);
    procedure SetSelectedNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure RetrieveSelectedObject(var AnObject: TObject);
    procedure AssignLimits(DataType: TRbwDataType; Limits: TColoringLimits);
    procedure ReadLimits(DataType: TRbwDataType; Limits: TColoringLimits);
  public
    procedure ResetTreeText;
    property SelectedVirtNode: PVirtualNode read FSelectedVirtNode;
    { Public declarations }
  end;


implementation

uses
  Contnrs, frmGridColorUnit, Clipbrd, ModelMuseUtilities;

{$R *.dfm}

procedure TfrmCustomColor.AssignLimits(DataType: TRbwDataType;
  Limits: TColoringLimits);
var
  IntegerValue: Integer;
  SkipRealItem: TSkipReal;
  RealValue: Double;
  SkipIndex: Integer;
  SkipIntegerItem: TSkipInteger;
begin
  Limits.LowerLimit := frameCheck3DMin.Limit;
  Limits.UpperLimit := frameCheck3DMax.Limit;
  Limits.ActiveOnly := cbActiveOnly.Checked;
  case DataType of
    rdtDouble:
      begin
        Limits.RealValuesToSkip.Clear;
        for SkipIndex := 1 to seNumberOfValuesToIgnore.AsInteger do
        begin
          if TryStrToFloat(rdgValuesToIgnore.
            Cells[0, SkipIndex], RealValue) then
          begin
            SkipRealItem := Limits.RealValuesToSkip.Add as TSkipReal;
            SkipRealItem.RealValue := RealValue;
          end;
        end;
        Limits.LogTransform := cbLogTransform.Checked;
      end;
    rdtInteger:
      begin
        Limits.IntegerValuesToSkip.Clear;
        for SkipIndex := 1 to seNumberOfValuesToIgnore.AsInteger do
        begin
          if TryStrToInt(rdgValuesToIgnore.
            Cells[0, SkipIndex], IntegerValue) then
          begin
            SkipIntegerItem := Limits.IntegerValuesToSkip.Add as TSkipInteger;
            SkipIntegerItem.IntegerValue := IntegerValue;
          end;
        end;
        Limits.LogTransform := False;
      end;
    rdtBoolean:
      begin
        Limits.LogTransform := False;
      end;
    rdtString:
      begin
        Limits.StringValuesToSkip.Clear;
        for SkipIndex := 1 to seNumberOfValuesToIgnore.AsInteger do
        begin
          Limits.StringValuesToSkip.Add(rdgValuesToIgnore.Cells[0, SkipIndex]);
        end;
        Limits.LogTransform := False;
      end;
  else
    Assert(False);
  end;
  Limits.Update;
end;

procedure TfrmCustomColor.comboColorSchemeChange(Sender: TObject);
begin
  inherited;
  pbColorScheme.Invalidate;
end;

procedure TfrmCustomColor.jsColorExponentChange(Sender: TObject);
begin
  inherited;
  if Sender <> seColorExponent then
  begin
    seColorExponent.Value := jsColorExponent.Value / 100
  end;
  pbColorScheme.Invalidate;
end;

procedure TfrmCustomColor.pbColorSchemePaint(Sender: TObject);
var
  X: integer;
  Fraction: Real;
  AColor: TColor;
  ColorAdjustmentFactor: Real;
begin
  for X := 0 to pbColorScheme.Width - 1 do
  begin
    Fraction := 1 - X / pbColorScheme.Width;
    ColorAdjustmentFactor := seColorExponent.Value;

    AColor := FracAndSchemeToColor(comboColorScheme.ItemIndex,
      Fraction, ColorAdjustmentFactor, seCycles.AsInteger);

    with pbColorScheme.Canvas do
    begin
      Pen.Color := AColor;
      MoveTo(X, 0);
      LineTo(X, pbColorScheme.Height - 1);
    end;
  end;
end;

procedure TfrmCustomColor.rdgValuesToIgnoreEndUpdate(Sender: TObject);
var
  NewCount: integer;
begin
  inherited;
  NewCount := rdgValuesToIgnore.RowCount-1;
  if (NewCount = 1) and (rdgValuesToIgnore.Cells[0,1] = '') then
  begin
    NewCount := 0;
  end;
  seNumberOfValuesToIgnore.AsInteger := NewCount;
end;

procedure TfrmCustomColor.seColorExponentChange(Sender: TObject);
begin
  inherited;
  jsColorExponent.Value := Round(seColorExponent.Value * 100);
  pbColorScheme.Invalidate
end;

procedure TfrmCustomColor.seCyclesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if ((Key = Ord('C')) or (Key = Ord('c'))) and (ssCtrl in Shift) then
  begin
    ClipBoard.AsText := seCycles.SelText;
  end;
  if ((Key = Ord('V')) or (Key = Ord('V'))) and (ssCtrl in Shift) then
  begin
    seCycles.SelText := ClipBoard.AsText;
  end;
end;

procedure TfrmCustomColor.seNumberOfValuesToIgnoreChange(Sender: TObject);
var
  NewCount: Integer;
begin
  inherited;
  NewCount := seNumberOfValuesToIgnore.AsInteger;
  if NewCount = 0 then
  begin
    rdgValuesToIgnore.RowCount := 2;
    rdgValuesToIgnore.Color := clBtnFace;
    rdgValuesToIgnore.Cells[0,1] := '';
  end
  else
  begin
    rdgValuesToIgnore.RowCount := NewCount + 1;
    rdgValuesToIgnore.Color := clWindow;
  end;
end;

procedure TfrmCustomColor.virttreecomboDataSetsChange(Sender: TObject);
begin
  inherited;
  ResetTreeText;
end;

procedure TfrmCustomColor.virttreecomboDataSetsDropDownTreeChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  inherited;
  SetSelectedNode(Sender, Node);
end;

procedure TfrmCustomColor.virttreecomboDataSetsDropDownTreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmCustomColor.FormCreate(Sender: TObject);
begin
  inherited;
  FSelectedVirtNode := nil;
  FDataSetDummyObjects := TObjectList.Create;

  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  FTopItems := TStringList.Create;
  FFrontItems := TStringList.Create;
  FSideItems := TStringList.Create;
  frameCheck3DMax.Limit.DefaultBooleanLimitValue := True;
  udDataSets.Max := High(SmallInt);
  udDataSets.Min := Low(SmallInt);

  rdgValuesToIgnore.Cells[0,0] := 'Values to ignore';
  rdgValuesToIgnore.FixedRows := 1;

  pcChoices.ActivePageIndex := 0;
end;

procedure TfrmCustomColor.FormDestroy(Sender: TObject);
begin
  inherited;
  FTopItems.Free;
  FFrontItems.Free;
  FSideItems.Free;
  FDataSetDummyObjects.Free;
end;

procedure TfrmCustomColor.FormShow(Sender: TObject);
begin
  inherited;
  udDataSets.Left := virttreecomboDataSets.Left + virttreecomboDataSets.Width;
  udDataSets.Top := virttreecomboDataSets.Top;
  udDataSets.Height := virttreecomboDataSets.Height;

end;

procedure TfrmCustomColor.GetDataSets;
begin
  FillVirtualStringTreeWithDataSets(virttreecomboDataSets.Tree,
    FDataSetDummyObjects, GetSelectedArray);
end;

procedure TfrmCustomColor.StoreDataSetsInLists;
var
  DataSet: TDataArray;
  Index: Integer;
begin
  for Index := 0 to frmGoPhast.PhastModel.DataSetCount - 1 do
  begin
    DataSet := frmGoPhast.PhastModel.DataSets[Index];
    case DataSet.Orientation of
      dsoTop:
        begin
          FTopItems.AddObject(DataSet.Name, DataSet);
        end;
      dsoFront:
        begin
          FFrontItems.AddObject(DataSet.Name, DataSet);
        end;
      dsoSide:
        begin
          FSideItems.AddObject(DataSet.Name, DataSet);
        end;
      dso3D:
        begin
          FTopItems.AddObject(DataSet.Name, DataSet);
          FFrontItems.AddObject(DataSet.Name, DataSet);
          FSideItems.AddObject(DataSet.Name, DataSet);
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TfrmCustomColor.udDataSetsChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint; Direction: TUpDownDirection);
var
  NewSelection: PVirtualNode;
begin
  inherited;
  NewSelection := nil;
  if SelectedVirtNode <> nil then
  begin
    case Direction of
      updNone: ; // do nothing
      updUp:
        begin
          NewSelection := SelectedVirtNode.PrevSibling;
        end;
      updDown:
        begin
          NewSelection := SelectedVirtNode.NextSibling;
        end;
      else Assert(False);
    end;
  end;
  if NewSelection = nil then
  begin
    Beep;
  end
  else
  begin
    virttreecomboDataSets.Tree.Selected[NewSelection] := True;
    SetSelectedNode(virttreecomboDataSets.Tree, NewSelection);
    if Assigned(btnOK.OnClick) then
    begin
      btnOK.OnClick(nil);
    end;
  end;
end;

procedure TfrmCustomColor.FinalizeList(List: TStringList);
begin
  List.Sort;
  List.Sorted := False;
  List.Insert(0, 'none');
end;

procedure TfrmCustomColor.ResetTreeText;
begin
  UpdateTreeComboText(SelectedVirtNode, virttreecomboDataSets);
end;

procedure TfrmCustomColor.SetSelectedNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  SelectOnlyLeaves(Node, virttreecomboDataSets, Sender, FSelectedVirtNode);
end;

procedure TfrmCustomColor.RetrieveSelectedObject(var AnObject: TObject);
var
  NodeData: PClassificationNodeData;
begin
//  AnObject:= nil;
  if SelectedVirtNode = nil then
  begin
    AnObject := nil;
  end
  else
  begin
    NodeData := virttreecomboDataSets.Tree.GetNodeData(SelectedVirtNode);
    if Assigned(NodeData) and Assigned(NodeData.ClassificationObject) then
    begin
      if NodeData.ClassificationObject is TBoundaryClassification then
      begin
        AnObject := TBoundaryClassification(NodeData.ClassificationObject).ClassifiedObject;
      end
      else if NodeData.ClassificationObject is TDataSetClassification then
      begin
        AnObject := TDataSetClassification(NodeData.ClassificationObject).DataArray;
      end;
    end
    else
    begin
      AnObject := nil;
    end;
  end;
end;

procedure TfrmCustomColor.ReadLimits(DataType: TRbwDataType; Limits: TColoringLimits);
var
  SkipIntegerItem: TSkipInteger;
  SkipRealItem: TSkipReal;
  SkipIndex: Integer;
begin
  frameCheck3DMin.Limit := Limits.LowerLimit;
  frameCheck3DMax.Limit := Limits.UpperLimit;
  frameCheck3DMin.DataType := DataType;
  frameCheck3DMax.DataType := DataType;
  cbActiveOnly.Checked := Limits.ActiveOnly;
  case DataType of
    rdtDouble:
      begin
        rdgValuesToIgnore.Columns[0].Format := rcf4Real;
        rdgValuesToIgnore.BeginUpdate;
        try
          seNumberOfValuesToIgnore.AsInteger := Limits.RealValuesToSkip.Count;
          seNumberOfValuesToIgnoreChange(nil);
          for SkipIndex := 0 to Limits.RealValuesToSkip.Count - 1 do
          begin
            SkipRealItem := Limits.RealValuesToSkip.
              Items[SkipIndex] as TSkipReal;
            rdgValuesToIgnore.Cells[0, SkipIndex + 1] :=
              FloatToStr(SkipRealItem.RealValue);
          end;
          cbLogTransform.Checked := Limits.LogTransform;
        finally
          rdgValuesToIgnore.EndUpdate;
        end;
      end;
    rdtInteger:
      begin
        rdgValuesToIgnore.Columns[0].Format := rcf4Integer;
        rdgValuesToIgnore.BeginUpdate;
        try
          seNumberOfValuesToIgnore.AsInteger := Limits.IntegerValuesToSkip.Count;
          seNumberOfValuesToIgnoreChange(nil);
          for SkipIndex := 0 to Limits.IntegerValuesToSkip.Count - 1 do
          begin
            SkipIntegerItem := Limits.IntegerValuesToSkip.
              Items[SkipIndex] as TSkipInteger;
            rdgValuesToIgnore.Cells[0, SkipIndex + 1] :=
              IntToStr(SkipIntegerItem.IntegerValue);
          end;
          cbLogTransform.Checked := False;
        finally
          rdgValuesToIgnore.EndUpdate;
        end;
      end;
    rdtBoolean:
      begin
        cbLogTransform.Checked := False;
      end;
    rdtString:
      begin
        rdgValuesToIgnore.Columns[0].Format := rcf4String;
        rdgValuesToIgnore.BeginUpdate;
        try
          seNumberOfValuesToIgnore.AsInteger := Limits.StringValuesToSkip.Count;
          seNumberOfValuesToIgnoreChange(nil);
          for SkipIndex := 0 to Limits.StringValuesToSkip.Count - 1 do
          begin
            rdgValuesToIgnore.Cells[0, SkipIndex + 1] :=
              Limits.StringValuesToSkip[SkipIndex];
          end;
          cbLogTransform.Checked := False;
        finally
          rdgValuesToIgnore.EndUpdate;
        end;
      end;
  end;
end;

end.
