unit frmCustomColorUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, ComCtrls,
  Forms, Dialogs, frmCustomGoPhastUnit, GoPhastTypes, frameDisplayLimitUnit,
  StdCtrls, JvExStdCtrls, JvRichEdit, ExtCtrls, Mask, JvExMask, JvSpin,
  JvExControls, JvxSlider, Buttons, DataSetUnit, SubscriptionUnit,
  ClassificationUnit, frmGoPhastUnit, PhastModelUnit, VirtualTrees,
  JvExComCtrls, JvUpDown, Grids, RbwDataGrid4, RbwParser, ArgusDataEntry,
  LegendUnit, Math, SsButtonEd, RbwStringTreeCombo;

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
    lblEpsilon: TLabel;
    rdeEpsilon: TRbwDataEntry;
    tabLegend: TTabSheet;
    imLegend: TImage;
    Panel2: TPanel;
    lblMethod: TLabel;
    lblColorLegendRows: TLabel;
    comboMethod: TComboBox;
    seLegendRows: TJvSpinEdit;
    rdgLegend: TRbwDataGrid4;
    udDataSets: TJvUpDown;
    timerLegend: TTimer;
    rgUpdateLimitChoice: TRadioGroup;
    virttreecomboDataSets1: TRbwStringTreeCombo;
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
    procedure FormDestroy(Sender: TObject); override;
    procedure virttreecomboDataSetsDropDownTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure virttreecomboDataSetsDropDownTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure virttreecomboDataSetsChange(Sender: TObject);
    procedure udDataSetsChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure FormShow(Sender: TObject);
    procedure rdgValuesToIgnoreEndUpdate(Sender: TObject);
    procedure seNumberOfValuesToIgnoreChange(Sender: TObject);
    procedure virttreecomboDataSetsClosedUp(Sender: TObject);
    procedure virttreecomboDataSetsDropDownTreeEnter(Sender: TObject);
    procedure seLegendRowsChange(Sender: TObject);
    procedure comboMethodChange(Sender: TObject);
    procedure rdgLegendSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgLegendEndUpdate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure timerLegendTimer(Sender: TObject);
    procedure rdgLegendStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure virttreecomboDataSets1TreeInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  private
    // @name is implemented as a TObjectList.
    FDataSetDummyObjects: TList;
    FSelectedVirtNode: PVirtualNode;
    FShouldClick: Boolean;
    FUpdatingLegend: Boolean;
    FStartTime: TDateTime;
    procedure UpdateLegendAfterDelay;
    function GetLegendDataSource: TPersistent;
//    procedure ResetTreeText;
    procedure SetLegendDataSource(const Value: TPersistent);
    function CanColorDataSet(DataArray: TDataArray): boolean;

  { Private declarations }
  protected
    FStoredLegend: TLegend;
    FLegend: TLegend;
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
    procedure UpdateLegend;

  public
    property LegendDataSource: TPersistent read GetLegendDataSource
      write SetLegendDataSource;
    procedure ResetTreeText;
    property SelectedVirtNode: PVirtualNode read FSelectedVirtNode;
    { Public declarations }
  end;


implementation

uses
  Contnrs, frmGridColorUnit, Clipbrd, ModelMuseUtilities, frmProgressUnit;

{$R *.dfm}

const
  OneSecond = 1/24/3600;

procedure TfrmCustomColor.AssignLimits(DataType: TRbwDataType;
  Limits: TColoringLimits);
var
  IntegerValue: Integer;
  SkipRealItem: TSkipReal;
  RealValue: Double;
  SkipIndex: Integer;
  SkipIntegerItem: TSkipInteger;
begin
  if TryStrToFloat(rdeEpsilon.Text, RealValue) then
  begin
    Limits.Epsilon := RealValue
  end;
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

procedure TfrmCustomColor.comboMethodChange(Sender: TObject);
begin
  inherited;
  rdgLegend.Enabled := comboMethod.ItemIndex = 1;
  seLegendRows.Enabled := rdgLegend.Enabled;
  if rdgLegend.Enabled then
  begin
    rdgLegend.Color := clWindow;
  end
  else
  begin
    rdgLegend.Color := clBtnFace;
  end;
  UpdateLegend;
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

procedure TfrmCustomColor.rdgLegendEndUpdate(Sender: TObject);
begin
  inherited;
  seLegendRows.AsInteger := rdgLegend.RowCount -1;
end;

procedure TfrmCustomColor.rdgLegendSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  UpdateLegendAfterDelay;
//  UpdateLegend;
end;

procedure TfrmCustomColor.rdgLegendStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  inherited;
  UpdateLegendAfterDelay;
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

procedure TfrmCustomColor.seLegendRowsChange(Sender: TObject);
begin
  inherited;
  if rdgLegend.RowCount <> seLegendRows.AsInteger + 1 then
  begin
    rdgLegend.RowCount := seLegendRows.AsInteger + 1;
    UpdateLegendAfterDelay;
  end;
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

procedure TfrmCustomColor.virttreecomboDataSets1TreeInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  CellText: string;
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmCustomColor.virttreecomboDataSetsChange(Sender: TObject);
begin
  inherited;
  ResetTreeText;
end;

procedure TfrmCustomColor.virttreecomboDataSetsClosedUp(Sender: TObject);
begin
  inherited;
  if FShouldClick then
  begin
    FShouldClick := False;
    MouseClick;
  end;
end;

procedure TfrmCustomColor.virttreecomboDataSetsDropDownTreeChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  inherited;
  SetSelectedNode(Sender, Node);
end;

procedure TfrmCustomColor.virttreecomboDataSetsDropDownTreeEnter(
  Sender: TObject);
begin
  inherited;
  FShouldClick := True;
end;

procedure TfrmCustomColor.virttreecomboDataSetsDropDownTreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  inherited;
  GetNodeCaption(Node, CellText, Sender);
end;

procedure TfrmCustomColor.FormCreate(Sender: TObject);
begin
  inherited;
  reComment.DoubleBuffered := False;
//  FLegend := TLegend.Create(nil);
//  FLegend.ValueAssignmentMethod := vamAutomatic;

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

  rdgLegend.Cells[0,0] := 'Values';

  pcChoices.ActivePageIndex := 0;
end;

procedure TfrmCustomColor.FormDestroy(Sender: TObject);
begin
  inherited;
  FTopItems.Free;
  FFrontItems.Free;
  FSideItems.Free;
  FDataSetDummyObjects.Free;
  FStoredLegend.Free;
//  FLegend.Free;
end;

procedure TfrmCustomColor.FormResize(Sender: TObject);
begin
  inherited;
  UpdateLegend;
end;

procedure TfrmCustomColor.FormShow(Sender: TObject);
begin
  inherited;
  udDataSets.Left := virttreecomboDataSets1.Left + virttreecomboDataSets1.Width;
  udDataSets.Top := virttreecomboDataSets1.Top;
  udDataSets.Height := virttreecomboDataSets1.Height;

end;

function TfrmCustomColor.CanColorDataSet(DataArray: TDataArray): boolean;
begin
  result := False;
  case DataArray.EvaluatedAt of
    eaBlocks: result := True;
    eaNodes: result := frmGoPhast.PhastModel.ModelSelection = msPhast;
    else Assert(False);
  end;
end;

procedure TfrmCustomColor.GetDataSets;
begin
  FillVirtualStringTreeWithDataSets(virttreecomboDataSets1.Tree,
    FDataSetDummyObjects, GetSelectedArray, CanColorDataSet);
end;

function TfrmCustomColor.GetLegendDataSource: TPersistent;
begin
  if FLegend = nil then
  begin
    result := nil 
  end
  else
  begin
    result := FLegend.ValueSource;
  end;
end;

procedure TfrmCustomColor.StoreDataSetsInLists;
var
  DataSet: TDataArray;
  Index: Integer;
  DataArrayManager: TDataArrayManager;
begin
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  for Index := 0 to DataArrayManager.DataSetCount - 1 do
  begin
    DataSet := DataArrayManager.DataSets[Index];
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

procedure TfrmCustomColor.timerLegendTimer(Sender: TObject);
begin
  inherited;
  if Now - FStartTime > OneSecond then
  begin
    timerLegend.Enabled := False;
    UpdateLegend;
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
    virttreecomboDataSets1.Tree.Selected[NewSelection] := True;
    SetSelectedNode(virttreecomboDataSets1.Tree, NewSelection);
    if Assigned(btnOK.OnClick) then
    begin
      btnOK.OnClick(nil);
    end;
  end;
  udDataSets.ControlStyle := udDataSets.ControlStyle - [csCaptureMouse];

end;

procedure TfrmCustomColor.UpdateLegend;
var
  BitMap: TBitmap;
  Index: Integer;
  DummyRect: TRect;
begin
  if FUpdatingLegend or (csDestroying in ComponentState)
    or (frmGoPhast = nil) or (frmGoPhast.PhastModel = nil)
    or frmGoPhast.PhastModel.Clearing
    or (csDestroying in frmGoPhast.PhastModel.ComponentState) then
  begin
    Exit;
  end;

  FUpdatingLegend := True;
  try
    tabLegend.TabVisible := LegendDataSource <> nil;
    if tabLegend.TabVisible then
    begin
      if FStoredLegend <> nil then
      begin
        FLegend.Assign(FStoredLegend);
        Exit;
      end;
      FLegend.ValueAssignmentMethod :=
        TValueAssignmentMethod(comboMethod.ItemIndex + 1);
      rdgLegend.BeginUpdate;
      try
        case FLegend.ValueAssignmentMethod of
          vamNoLegend: Exit;
          vamAutomatic:
            begin
              FLegend.AutoAssignValues;
              case FLegend.Values.DataType of
                rdtDouble: rdgLegend.Columns[0].Format := rcf4Real;
                rdtInteger: rdgLegend.Columns[0].Format := rcf4Integer;
                rdtBoolean: rdgLegend.Columns[0].Format := rcf4Boolean;
                rdtString: rdgLegend.Columns[0].Format := rcf4String;
                else Assert(False);
              end;
              seLegendRows.AsInteger := FLegend.Values.Count;
              seLegendRowsChange(nil);
              for Index := 0 to FLegend.Values.Count - 1 do
              begin
                case FLegend.Values.DataType of
                  rdtDouble:
                    begin
                      if FLegend.ColoringLimits.LogTransform then
                      begin
                        rdgLegend.Cells[0,Index + 1] :=
                          FloatToStr(FLegend.Values.RealValues[Index]);
//                        rdgLegend.Cells[0,Index + 1] :=
//                          FloatToStr(Power(10,FLegend.Values.RealValues[Index]));
                      end
                      else
                      begin
                        rdgLegend.Cells[0,Index + 1] :=
                          FloatToStr(FLegend.Values.RealValues[Index]);
                      end;
                    end;
                  rdtInteger:
                    begin
                      rdgLegend.Cells[0,Index + 1] :=
                        IntToStr(FLegend.Values.IntValues[Index]);
                    end;
                  rdtBoolean:
                    begin
                      rdgLegend.Cells[0,Index + 1] := '';
                      rdgLegend.Checked[0,Index + 1] :=
                        FLegend.Values.BooleanValues[Index];
                    end;
                  rdtString:
                    begin
                      rdgLegend.Cells[0,Index + 1] :=
                        FLegend.Values.StringValues[Index];
                    end;
                  else Assert(False);
                end;
              end;
            end;
          vamManual:
            begin
              FLegend.Values.Count := seLegendRows.AsInteger;
              for Index := 0 to FLegend.Values.Count - 1 do
              begin
                case FLegend.Values.DataType of
                  rdtDouble:
                    begin
                      if cbLogTransform.Checked then
                      begin
                        FLegend.Values.RealValues[Index] :=
                          StrToFloatDef(rdgLegend.Cells[0,Index + 1], 0);
//                        FLegend.Values.RealValues[Index] := Log10(
//                          StrToFloatDef(rdgLegend.Cells[0,Index + 1], 1));
                      end
                      else
                      begin
                        FLegend.Values.RealValues[Index] :=
                          StrToFloatDef(rdgLegend.Cells[0,Index + 1], 0);
                      end;
                    end;
                  rdtInteger:
                    begin
                      FLegend.Values.IntValues[Index] :=
                        StrToIntDef(rdgLegend.Cells[0,Index + 1], 0);
                    end;
                  rdtBoolean:
                    begin
                      FLegend.Values.BooleanValues[Index] :=
                        rdgLegend.Checked[0,Index + 1];
                    end;
                  rdtString:
                    begin
                      FLegend.Values.StringValues[Index] :=
                        rdgLegend.Cells[0,Index + 1];
                    end;
                  else Assert(False);
                end;
              end;

            end;
          else Assert(False);
        end;
      finally
        rdgLegend.EndUpdate;
      end;

      FLegend.AssignFractions;
      BitMap := TBitMap.Create;
      try
        BitMap.Canvas.Font := Font;
        BitMap.Width := imLegend.Width;
        BitMap.Height := imLegend.Height;
        FLegend.Draw(BitMap.Canvas, 10, 10, DummyRect);
        imLegend.Picture.Assign(BitMap);
      finally
        BitMap.Free;
      end;
    end;
  finally
    FUpdatingLegend := False;
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
  UpdateTreeComboText(SelectedVirtNode, virttreecomboDataSets1);
end;

procedure TfrmCustomColor.UpdateLegendAfterDelay;
begin
  FStartTime := Now;
  timerLegend.Enabled := True;
end;

procedure TfrmCustomColor.SetLegendDataSource(const Value: TPersistent);
begin
  if FLegend <> nil then
  begin
    FLegend.ValueSource := Value;
  end;
end;

procedure TfrmCustomColor.SetSelectedNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  SelectOnlyLeaves(Node, virttreecomboDataSets1, Sender, FSelectedVirtNode);
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
    NodeData := virttreecomboDataSets1.Tree.GetNodeData(SelectedVirtNode);
    if Assigned(NodeData) and Assigned(NodeData.ClassificationObject) then
    begin
      if NodeData.ClassificationObject is TBoundaryClassification then
      begin
        AnObject := TBoundaryClassification(NodeData.ClassificationObject).ClassifiedObject;
      end
      else if NodeData.ClassificationObject is TDataSetClassification then
      begin
        AnObject := TDataSetClassification(NodeData.ClassificationObject).DataArray;
      end
      else
      begin
        AnObject := nil;
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
  rdeEpsilon.Text := FloatToStr(Limits.Epsilon);
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
