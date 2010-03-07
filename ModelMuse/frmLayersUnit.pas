unit frmLayersUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, ComCtrls, ExtCtrls, StdCtrls, Buttons, Grids,
  RbwDataGrid4, ArgusDataEntry, GoPhastTypes, LayerStructureUnit, ImgList,
  JvExStdCtrls, JvCombobox, JvListComb, UndoItems, RbwController, RbwEdit,
  RequiredDataSetsUndoUnit, JvCheckBox, Mask, JvExMask, JvSpin, frameSubBedsUnit,
  ModflowSubsidenceDefUnit;
                                 
type
  TfrmLayers = class(TfrmCustomGoPhast)
    Splitter1: TSplitter;
    pcLayerGroups: TPageControl;
    tabBasics: TTabSheet;
    Label1: TLabel;
    tabDiscretization: TTabSheet;
    rdeVDiscretization: TRbwDataEntry;
    Label2: TLabel;
    pbSubLayers: TPaintBox;
    Label3: TLabel;
    rdeGrowthRate: TRbwDataEntry;
    rgMethod: TRadioGroup;
    rdgSubLayerBoundaries: TRbwDataGrid4;
    pnlDiscritization: TPanel;
    Panel2: TPanel;
    pnlPaintboxParent: TPanel;
    Panel4: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Splitter2: TSplitter;
    Label4: TLabel;
    Panel3: TPanel;
    GridPanel1: TGridPanel;
    sbAddUnit: TSpeedButton;
    sbInsertUnit: TSpeedButton;
    sbDeleteUnit: TSpeedButton;
    GridPanel2: TGridPanel;
    sbInsertLine: TSpeedButton;
    sbMoveLine: TSpeedButton;
    sbDeleteLine: TSpeedButton;
    ilCombo: TImageList;
    Label5: TLabel;
    {
     @unorderedlist(
       @item(0, non-simulated)
       @item(1, confined)
       @item(2, convertible in LPF and HUF, Unconfined in BCF)
       @item(3, limited convertible in BCF with constant transmissivity)
       @item(4, fully convertible in BCF with variable transmissivity)
      )
    }
    comboAquiferType: TJvImageComboBox;
    lblInterblockMethod: TLabel;
    lbVertKMethod: TLabel;
    rconLayerType: TRbwController;
    comboInterblockMethod: TJvImageComboBox;
    comboVertKMethod: TJvImageComboBox;
    edName: TRbwEdit;
    cbComputeSaturatedThickness: TJvCheckBox;
    tvLayerGroups: TTreeView;
    ilTreeView: TImageList;
    lblAnisotropy: TLabel;
    rdeAnisotropy: TRbwDataEntry;
    tabNoDelay: TTabSheet;
    frameSubNoDelayBeds: TframeSubBeds;
    tabDelay: TTabSheet;
    frameSubDelayBeds: TframeSubBeds;
    procedure rdeVDiscretizationChange(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure rdeGrowthRateChange(Sender: TObject);
    procedure rgMethodClick(Sender: TObject);
    procedure pbSubLayersPaint(Sender: TObject);
    procedure rdgSubLayerBoundariesSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure pbSubLayersMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbSubLayersMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbSubLayersMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlPaintboxParentResize(Sender: TObject);
    procedure pbSubLayersMouseEnter(Sender: TObject);
    procedure pbSubLayersMouseLeave(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure rdeGrowthRateExit(Sender: TObject);
    procedure sbDeleteUnitClick(Sender: TObject);
    procedure sbAddUnitClick(Sender: TObject);
    procedure sbInsertUnitClick(Sender: TObject);
    procedure comboAquiferTypeChange(Sender: TObject);
    procedure comboInterblockMethodChange(Sender: TObject);
    procedure comboVertKMethodChange(Sender: TObject);
    procedure rdgSubLayerBoundariesSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure pcLayerGroupsChange(Sender: TObject);
    procedure cbComputeSaturatedThicknessClick(Sender: TObject);
    procedure rdgSubLayerBoundariesExit(Sender: TObject);
    procedure tvLayerGroupsChange(Sender: TObject; Node: TTreeNode);
    procedure rdeAnisotropyChange(Sender: TObject);
  private
    FLayerPositions: TOneDIntegerArray;
    FMovingLine: boolean;
    FLineBeingMoved: Integer;
    FMouseY: integer;
    FMouseInPaintBox: boolean;
    FLayerStructure: TLayerStructure;
    FSettingUnit: boolean;
    FSelectedUnits: TList;
    FSelectedTreeNodes: TList;
    FUseSaturatedThickness: Boolean;
    procedure UpdateSelectedUnitLayers;
    procedure EnableOkButton;
    procedure GetData;
    Procedure SetData;
    procedure SetSpacing(const GrowthRate: real;
      const GrowthMethod: TGrowthMethod; const SubLayers: integer;
      out Fractions: TRealArray);
    procedure UpdateStringGrid;
    procedure GetLayerPostions(const Fractions: TRealArray;
      out LayerPostions: TOneDIntegerArray);
    procedure UpdateLayerPositions;
    procedure SetPbCursor(X,Y: integer);
    function IsOnLine(Y: Integer; out WhichLine: integer): boolean;
    procedure StartMove(X, Y: Integer);
    procedure DeleteLine(Y: integer);
    procedure InsertLine(Y: integer);
    procedure MoveLine(Y: integer);
    function ConvertY(Y: integer): real;
    procedure RearrangeValuesInStringGrid;
    function InBox(X, Y: integer): boolean;
    procedure EnableGrowthRateControl;
    function AddNewUnit(Position: integer): TTreeNode;
    function FindImageIndex(LayerGroup: TLayerGroup): integer;
    procedure UpdateSelectedUnits;
    procedure SetControlValues;
    procedure EnableComputeSatThick;
    procedure EnableK_Methods;
    procedure SetUpLayerTypeOptions;
    procedure SetUpAveragingOptions;
    procedure EnableAnisotropy;
    procedure GetSubsidenceLayers(Sender: TObject;
      var SubLayers: TCustomSubLayer);
    procedure GetNewSubsidenceName(Sender: TObject; var NewName: string);
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoDefineLayers = class(TCustomCreateRequiredDataSetsUndo)
  private
    FNewLayerStructure: TLayerStructure;
    FOldLayerStructure: TLayerStructure;
    FNewDataSets: TList;
  protected
    function Description: string; override;
  public
    constructor Create(var NewLayerStructure: TLayerStructure);
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

implementation

uses Math, RealListUnit, CursorsFoiledAgain, frmGoPhastUnit,
  ModflowPackagesUnit;

{$R *.dfm}

function TfrmLayers.FindImageIndex(LayerGroup: TLayerGroup): integer;
begin
  if not LayerGroup.Simulated then
  begin
    result := 4;
  end
  else
  begin
    if frmGoPhast.PhastModel.ModflowPackages.BcfPackage.IsSelected then
    begin
      case LayerGroup.AquiferType of
        0: // confined
          begin
            result := 1;
          end;
        1: // unconfined
          begin
            if LayerGroup.Index = 1 then
            begin
              result := 3;
            end
            else
            begin
              result := 2;
            end;
          end;
        2: // limited convertible
          begin
            result := 5;
          end;
        3: // fully convertible
          begin
            result := 2;
          end;
        else
          begin
            result := -1;
          end;
      end;
    end
    else
    begin
      case LayerGroup.AquiferType of
        0: // confined
          begin
            result := 1;
          end;
        1,2,3: // convertible
          begin
            result := 2;
          end;
        else
          begin
            result := -1;
          end;
      end;
    end;
  end;
end;

procedure TfrmLayers.FormCreate(Sender: TObject);
begin
  inherited;
  pnlPaintboxParent.DoubleBuffered:= True;
  FSelectedUnits:= TList.Create;
  FSelectedTreeNodes:= TList.Create;
  pcLayerGroups.ActivePageIndex := 0;
  FLayerStructure:= TLayerStructure.Create(nil);
  rdgSubLayerBoundaries.Cells[0,0] := 'Layer boundary';
  frameSubNoDelayBeds.OnGetSelectedSubLayers := GetSubsidenceLayers;
  frameSubDelayBeds.OnGetSelectedSubLayers := GetSubsidenceLayers;
  frameSubNoDelayBeds.OnGetNewName := GetNewSubsidenceName;
  frameSubDelayBeds.OnGetNewName := GetNewSubsidenceName;
  GetData;
  EnableComputeSatThick;

end;

procedure TfrmLayers.FormDestroy(Sender: TObject);
begin
  FLayerStructure.Free;
  FSelectedUnits.Free;
  FSelectedTreeNodes.Free;
  inherited;
end;

procedure TfrmLayers.SetUpAveragingOptions;
var
  Packages: TModflowPackages;
  Item: TJvImageItem;
begin
  Packages := frmGoPhast.PhastModel.ModflowPackages;

  comboInterblockMethod.Items.Clear;

  Item := comboInterblockMethod.Items.Add;
  Item.Text := 'Harmonic mean (0)';

  if Packages.BcfPackage.IsSelected then
  begin
    Item := comboInterblockMethod.Items.Add;
    Item.Text := 'Arithmetic mean (1)';

    Item := comboInterblockMethod.Items.Add;
    Item.Text := 'Logarithmic mean (2)';

    Item := comboInterblockMethod.Items.Add;
    Item.Text := 'Arithmetic and logarithmic (3)';
  end
  else
  begin
    Item := comboInterblockMethod.Items.Add;
    Item.Text := 'Logarithmic mean (1)';

    Item := comboInterblockMethod.Items.Add;
    Item.Text := 'Arithmetic and logarithmic (2)';
  end;
end;

procedure TfrmLayers.EnableAnisotropy;
var

ShouldEnable: Boolean;
  Index: Integer;
  Group: TLayerGroup;
begin
  ShouldEnable :=  (FSelectedUnits.Count >= 1)
    and frmGoPhast.PhastModel.ModflowPackages.BcfPackage.IsSelected;
  if ShouldEnable then
  begin
    ShouldEnable := False;
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      Group := FSelectedUnits[Index];
      if Group.Simulated then
      begin
        ShouldEnable := True;
        break;
      end;
    end;
  end;
  rdeAnisotropy.Enabled := ShouldEnable
end;

procedure TfrmLayers.GetData;
var
  Index: integer;
  LayerGroup: TLayerGroup;
  NodeItem: TTreeNode;
begin
  tabNoDelay.TabVisible := frmGoPhast.PhastModel.
    ModflowPackages.SubPackage.IsSelected;
  tabDelay.TabVisible := frmGoPhast.PhastModel.
    ModflowPackages.SubPackage.IsSelected;

  SetUpLayerTypeOptions;
  SetUpAveragingOptions;

  FUseSaturatedThickness := frmGoPhast.PhastModel.
    ModflowPackages.LpfPackage.IsSelected
    and frmGoPhast.PhastModel.
    ModflowPackages.LpfPackage.UseSaturatedThickness;


  FLayerStructure.Assign(frmGoPhast.PhastModel.LayerStructure);
  for Index := 1 to FLayerStructure.Count - 1 do
  begin
    LayerGroup := FLayerStructure.Items[Index] as TLayerGroup;

    NodeItem := tvLayerGroups.Items.Add(nil, LayerGroup.AquiferName);
    NodeItem.Data := LayerGroup;

    NodeItem.StateIndex := FindImageIndex(LayerGroup);

  end;

  if tvLayerGroups.Items.Count > 0 then
  begin
    tvLayerGroups.Items[0].Selected := True;
  end;

  sbDeleteUnit.Enabled := FLayerStructure.Count > 1;
end;

procedure TfrmLayers.GetLayerPostions(const Fractions: TRealArray;
  Out LayerPostions: TOneDIntegerArray);
var
  Index: Integer;
begin
  SetLength(LayerPostions, Length(Fractions));
  for Index := 0 to Length(Fractions) - 1 do
  begin
    LayerPostions[Index] :=
      Round((pbSubLayers.Height-20)*(1-Fractions[Index])+10);
  end;
end;

procedure TfrmLayers.GetNewSubsidenceName(Sender: TObject;
  var NewName: string);
begin
  if Sender = frameSubNoDelayBeds then
  begin
    NewName := 'ND_SYS_' + IntToStr(FLayerStructure.NoDelayCount);
  end
  else if Sender = frameSubDelayBeds then
  begin
    NewName := 'D_SYS_' + IntToStr(FLayerStructure.DelayCount);
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TfrmLayers.GetSubsidenceLayers(Sender: TObject;
  var SubLayers: TCustomSubLayer);
var
  SelectedUnit: TLayerGroup;
begin
  SubLayers := nil;
  if (not FSettingUnit) and (FSelectedUnits.Count > 0) then
  begin
    Assert(FSelectedUnits.Count = 1);
    SelectedUnit := FSelectedUnits[0];
    if Sender = frameSubNoDelayBeds then
    begin
      SubLayers := SelectedUnit.SubNoDelayBedLayers;
    end
    else if Sender = frameSubDelayBeds then
    begin
      SubLayers := SelectedUnit.SubDelayBedLayers;
    end
    else
    begin
      Assert(False);
    end;
  end;
end;

procedure TfrmLayers.pnlPaintboxParentResize(Sender: TObject);
begin
  inherited;
  UpdateLayerPositions;
end;

procedure TfrmLayers.SetData;
var
  Undo: TUndoDefineLayers;
begin
  Undo := TUndoDefineLayers.Create(FLayerStructure);
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmLayers.SetPbCursor(X,Y: integer);
var
  Dummy: integer;
begin
  if FMovingLine then Exit;
  if not InBox(X,Y) then
  begin
    pbSubLayers.Cursor := crDefault;
  end
  else if sbInsertLine.Down then
  begin
    pbSubLayers.Cursor := crHorizontal;
  end
  else if IsOnLine(Y, Dummy) then
  begin
    if sbMoveLine.Down then
    begin
      pbSubLayers.Cursor := crMoveRow;
    end
    else if sbDeleteLine.Down then
    begin
      pbSubLayers.Cursor := crDelete;
    end
    else
    begin
      pbSubLayers.Cursor := crDefault;
    end;
  end
  else
  begin
    pbSubLayers.Cursor := crDefault;
  end;
end;

function TfrmLayers.IsOnLine(Y: Integer; out WhichLine: integer): boolean;
var
  Index: Integer;
begin
  result := False;
  WhichLine := -1;
  for Index := 0 to Length(FLayerPositions) - 1 do
  begin
    result := Abs(FLayerPositions[Index] - Y) <= 3;
    if result then
    begin
      WhichLine := Index;
      Exit;
    end;
  end;
end;

procedure TfrmLayers.tvLayerGroupsChange(Sender: TObject; Node: TTreeNode);
begin
  inherited;
  UpdateSelectedUnits;
  SetControlValues;
end;

procedure TfrmLayers.StartMove(X, Y: Integer);
begin
  FMovingLine := sbMoveLine.Down and IsOnLine(Y,FLineBeingMoved); 
end;

procedure TfrmLayers.pbSubLayersMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  StartMove(X, Y);
end;

procedure TfrmLayers.pbSubLayersMouseEnter(Sender: TObject);
begin
  inherited;
  FMouseInPaintBox := True;
end;

procedure TfrmLayers.pbSubLayersMouseLeave(Sender: TObject);
begin
  inherited;
  FMouseInPaintBox := False;
  pbSubLayers.Invalidate;
end;

procedure TfrmLayers.pbSubLayersMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  SetPbCursor(X,Y);
  FMouseY := Y;
  if FMovingLine or sbInsertLine.Down
   then
  begin
    pbSubLayers.Invalidate;
  end;
end;

procedure TfrmLayers.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmLayers.cbComputeSaturatedThicknessClick(Sender: TObject);
var
  Index: Integer;
  SelectedUnit: TLayerGroup;
begin
  inherited;
  if not FSettingUnit then
  begin
    cbComputeSaturatedThickness.AllowGrayed := False;
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      if SelectedUnit.Simulated
        and (SelectedUnit.AquiferType = 0) then
      begin
        SelectedUnit.UseStartingHeadForSaturatedThickness :=
          cbComputeSaturatedThickness.Checked;
      end;
    end;
  end;
end;

procedure TfrmLayers.comboAquiferTypeChange(Sender: TObject);
var
  Index: integer;
  SelectedUnit: TLayerGroup;
  SimulatedLayer: boolean;
  TreeNode: TTreeNode;
  ShowWarning: boolean;
begin
  inherited;
  if frmGoPhast.PhastModel.ModflowPackages.HufPackage.IsSelected then
  begin
    if comboAquiferType.ItemIndex = 0 then
    begin
      comboAquiferType.ItemIndex := 1;
    end;
  end;
  if not FSettingUnit then
  begin
    ShowWarning := False;
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      SelectedUnit.Simulated := comboAquiferType.ItemIndex > 0;
      if SelectedUnit.Simulated then
      begin
        if frmGoPhast.PhastModel.ModflowPackages.BcfPackage.IsSelected
          and (SelectedUnit.Index <> 1)
          and (comboAquiferType.ItemIndex = 2) then
        begin
          SelectedUnit.AquiferType := 3;
          ShowWarning := True;
          if FSelectedUnits.Count = 1 then
          begin
            comboAquiferType.ItemIndex := 4
          end;
        end
        else
        begin
          SelectedUnit.AquiferType := comboAquiferType.ItemIndex -1;
        end;
      end;
      TreeNode := FSelectedTreeNodes[Index];
      TreeNode.StateIndex := FindImageIndex(SelectedUnit);
    end;
    if ShowWarning then
    begin
      MessageDlg('Only the top layer can be unconfined.', mtWarning, [mbOK], 0);
    end;
  end;
  SimulatedLayer := comboAquiferType.ItemIndex > 0;
  tabDiscretization.TabVisible := SimulatedLayer;
  EnableK_Methods;
  rconLayerType.Enabled := SimulatedLayer;
  EnableComputeSatThick;
  EnableAnisotropy;
  EnableOkButton;
end;

procedure TfrmLayers.comboInterblockMethodChange(Sender: TObject);
var
  Index: integer;
  SelectedUnit: TLayerGroup;
begin
  inherited;
  if not FSettingUnit then
  begin
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      SelectedUnit.InterblockTransmissivityMethod :=
        comboInterblockMethod.ItemIndex;
    end;
  end;
end;

procedure TfrmLayers.comboVertKMethodChange(Sender: TObject);
var
  Index: integer;
  SelectedUnit: TLayerGroup;
begin
  inherited;
  if not FSettingUnit then
  begin
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      SelectedUnit.VerticalHydraulicConductivityMethod :=
        comboVertKMethod.ItemIndex;
    end;
  end;
end;

function TfrmLayers.ConvertY(Y: integer): real;
begin
  result := (pbSubLayers.Height - 10 - Y)/(pbSubLayers.Height - 20);
end;

procedure TfrmLayers.RearrangeValuesInStringGrid;
var
  Index: Integer;
  Fractions: TRealArray;
  LayerIndex: Integer;
  LayerGroup: TLayerGroup;
begin
  for LayerIndex := 0 to FSelectedUnits.Count - 1 do
  begin
    LayerGroup := FSelectedUnits[LayerIndex];
    rgMethod.ItemIndex := Integer(gmCustom);
    LayerGroup.GrowthMethod := gmCustom;
    SetSpacing(LayerGroup.GrowthRate, gmCustom,
      LayerGroup.LayerCount, Fractions);
    rdeVDiscretization.Text := IntToStr(Length(Fractions) + 1);
    rdeVDiscretizationChange(rdeVDiscretization);
    LayerGroup.LayerCollection.Clear;
    for Index := 0 to Length(Fractions)-1 do
    begin
      rdgSubLayerBoundaries.Cells[0, Index + 1]
        := FloatToStr(Fractions[Index]);
      (LayerGroup.LayerCollection.Add as TLayerFraction).Fraction :=
        Fractions[Index];
    end;
  end;
  pbSubLayers.Invalidate;
end;

procedure TfrmLayers.InsertLine(Y: integer);
var
  NumLayers: integer;
  LayerGroup: TLayerGroup;
  LayerIndex: integer;
begin
  rgMethod.ItemIndex := Integer(gmCustom);
  NumLayers := StrToInt(rdeVDiscretization.Text) + 1;
  rdeVDiscretization.Text := IntToStr(NumLayers);
  rdeVDiscretizationChange(rdeVDiscretization);
  rdgSubLayerBoundaries.Cells[0,rdgSubLayerBoundaries.RowCount-1]
    := FloatToStr(ConvertY(Y));
  for LayerIndex := 0 to FSelectedUnits.Count - 1 do
  begin
    LayerGroup := FSelectedUnits[LayerIndex];
    While LayerGroup.LayerCount < NumLayers do
    begin
      LayerGroup.LayerCollection.Add;
    end;
  end;
  RearrangeValuesInStringGrid;
  UpdateLayerPositions;
  UpdateSelectedUnitLayers;
  pbSubLayers.Invalidate;
end;

procedure TfrmLayers.MoveLine(Y: integer);
begin
  rdgSubLayerBoundaries.Cells[0, FLineBeingMoved+1]
    := FloatToStr(ConvertY(Y));
  RearrangeValuesInStringGrid;
  UpdateSelectedUnitLayers;
  pbSubLayers.Invalidate;
end;

procedure TfrmLayers.DeleteLine(Y: integer);
var
  LineToDelete: Integer;
  Index: integer;
begin
  if IsOnLine(Y, LineToDelete) then
  begin
    for Index := LineToDelete+1 to rdgSubLayerBoundaries.RowCount - 1 do
    begin
      rdgSubLayerBoundaries.Cells[0,Index] :=
        rdgSubLayerBoundaries.Cells[0,Index+1];
    end;
    rdgSubLayerBoundaries.Cells[0, rdgSubLayerBoundaries.RowCount-1] := '';
    RearrangeValuesInStringGrid;
    UpdateLayerPositions;
    UpdateSelectedUnitLayers;
    pbSubLayers.Invalidate;
  end;
end;

procedure TfrmLayers.edNameChange(Sender: TObject);
var
  SelectedUnit: TLayerGroup;
  TreeNode: TTreeNode;
begin
  inherited;
  if (not FSettingUnit) and (FSelectedUnits.Count > 0) then
  begin
    if edName.Text <> '' then
    begin
      Assert(FSelectedUnits.Count = 1);
      SelectedUnit := FSelectedUnits[0];
      SelectedUnit.AquiferName := edName.Text;
    end;
    TreeNode := FSelectedTreeNodes[0];
    TreeNode.Text := edName.Text;
  end;
end;

function TfrmLayers.InBox(X,Y: integer): boolean;
begin
  result := (X > 10) and (Y > 10) and
    (X < pbSubLayers.Width - 10) and (Y < pbSubLayers.Height - 10);
end;

procedure TfrmLayers.pbSubLayersMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if InBox(X,Y) then
  begin
    if sbInsertLine.Down then
    begin
      InsertLine(Y);
    end
    else if sbMoveLine.Down and FMovingLine then
    begin
      MoveLine(Y);
    end
    else if sbDeleteLine.Down then
    begin
      DeleteLine(Y);
    end;
    FMovingLine := False;
  end;
end;

procedure TfrmLayers.pbSubLayersPaint(Sender: TObject);
var
  Index: Integer;
begin
  inherited;
  pbSubLayers.Canvas.Brush.Color := clWhite;
  pbSubLayers.Canvas.Rectangle(0,0,pbSubLayers.Width, pbSubLayers.Height);
  pbSubLayers.Canvas.MoveTo(10,10);
  pbSubLayers.Canvas.LineTo(pbSubLayers.Width-10,10);
  pbSubLayers.Canvas.LineTo(pbSubLayers.Width-10,pbSubLayers.Height-10);
  pbSubLayers.Canvas.LineTo(10,pbSubLayers.Height-10);
  pbSubLayers.Canvas.LineTo(10,10);

  for Index := 0 to Length(FLayerPositions) - 1 do
  begin
    pbSubLayers.Canvas.MoveTo(10,FLayerPositions[Index]);
    pbSubLayers.Canvas.LineTo(pbSubLayers.Width-10,FLayerPositions[Index]);
  end;

  if (FMovingLine or sbInsertLine.Down) and FMouseInPaintBox then
  begin
    pbSubLayers.Canvas.Pen.Style := psDot;
    try
      pbSubLayers.Canvas.MoveTo(10,FMouseY);
      pbSubLayers.Canvas.LineTo(pbSubLayers.Width-10,FMouseY);
    finally
      pbSubLayers.Canvas.Pen.Style := psSolid;
    end;
  end;
end;

procedure TfrmLayers.pcLayerGroupsChange(Sender: TObject);
begin
  inherited;
  btnHelp.HelpKeyword := pcLayerGroups.ActivePage.HelpKeyword;
end;

procedure TfrmLayers.rdeAnisotropyChange(Sender: TObject);
var
  Index: Integer;
  SelectedUnit: TLayerGroup;
  AValue: double;
begin
  inherited;
  if (FSelectedUnits <> nil) and not FSettingUnit then
  begin
    if TryStrToFloat(rdeAnisotropy.Text, AValue) then
    begin
      for Index := 0 to FSelectedUnits.Count - 1 do
      begin
        SelectedUnit := FSelectedUnits[Index];
        SelectedUnit.HorizontalAnisotropy := AValue;
      end;
    end;
  end;
end;

procedure TfrmLayers.rdeGrowthRateChange(Sender: TObject);
begin
  inherited;
  if csLoading in ComponentState then Exit;
  UpdateStringGrid;
  UpdateLayerPositions;
  pbSubLayers.Invalidate;
end;

procedure TfrmLayers.rdeGrowthRateExit(Sender: TObject);
var
  Index: integer;
  SelectedUnit: TLayerGroup;
begin
  inherited;
  if not FSettingUnit then
  begin
    try
      if rdeGrowthRate.Text <> '' then
      begin
        for Index := 0 to FSelectedUnits.Count - 1 do
        begin
          SelectedUnit := FSelectedUnits[Index];
          SelectedUnit.GrowthRate := StrToFloat(rdeGrowthRate.Text);
        end;
        UpdateSelectedUnitLayers;
      end;
    except on E: EConvertError do
      begin
        // do nothing
      end;
    end;
  end;
end;

procedure TfrmLayers.EnableOkButton;
var
  Group: TLayerGroup;
  Group1, Group2: TLayerGroup;
  Index: Integer;
  ShouldShowMessage: boolean;
begin
  if FLayerStructure.Count = 0 then Exit;
  
  ShouldShowMessage := btnOK.Enabled;
  Group := FLayerStructure.Items[FLayerStructure.Count-1] as TLayerGroup;
  btnOK.Enabled := Group.Simulated;
  if btnOK.Enabled then
  begin
    Group := FLayerStructure.Items[1] as TLayerGroup;
    btnOK.Enabled := Group.Simulated;
    if btnOK.Enabled then
    begin
      for Index := 2 to FLayerStructure.Count - 2 do
      begin
        Group1 := FLayerStructure.Items[Index] as TLayerGroup;
        Group2 := FLayerStructure.Items[Index+1] as TLayerGroup;
        btnOK.Enabled := Group1.Simulated or Group2.Simulated;
        if not btnOK.Enabled then
        begin
          break;
          if ShouldShowMessage then
          begin
            Beep;
            MessageDlg('A non-simulated layer group can not be next to '
              + 'another non-simulated layer group.', mtError, [mbOK], 0);
          end;
        end;
      end;
    end
    else
    begin
      if ShouldShowMessage then
      begin
        Beep;
        MessageDlg('The top layer group must be simulated.',
          mtError, [mbOK], 0);
      end;
    end;
  end
  else
  begin
    if ShouldShowMessage then
    begin
      Beep;
      MessageDlg('The bottom layer group must be simulated.',
        mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfrmLayers.EnableGrowthRateControl;
var
  VDisc: integer;
begin
  if rdeVDiscretization.Text <> '' then
  begin
    VDisc := StrToInt(rdeVDiscretization.Text);
    rdeGrowthRate.Enabled := (VDisc > 1)
      and (TGrowthMethod(rgMethod.ItemIndex) in [gmUp..gmEdge]);
  end;
end;

function TfrmLayers.AddNewUnit(Position: integer): TTreeNode;
var
  LayerGroup: TLayerGroup;
  Index: Integer;
  TreeNode: TTreeNode;
  Sibling: TTreeNode;
begin
  for Index := 0 to FSelectedTreeNodes.Count - 1 do
  begin
    TreeNode := FSelectedTreeNodes[Index];
    TreeNode.Selected := False;
  end;
  FSelectedTreeNodes.Clear;
  FSelectedUnits.Clear;
  if FLayerStructure.Count = 0 then
  begin
    LayerGroup := FLayerStructure.Insert(0) as TLayerGroup;
    LayerGroup.AquiferName := StrModelTop;
  end;
  LayerGroup := FLayerStructure.Insert(Position+1) as TLayerGroup;

  if Position < tvLayerGroups.Items.Count then
  begin
    Sibling := tvLayerGroups.Items[Position];
  end
  else
  begin
    Sibling := nil;
  end;

  result := tvLayerGroups.Items.Insert(Sibling,
    LayerGroup.AquiferName);
  result.Data := LayerGroup;
  result.Selected := True;
  result.StateIndex := 1;

  sbDeleteUnit.Enabled := True;
  EnableOkButton;
  tvLayerGroups.Invalidate;
end;

procedure TfrmLayers.UpdateSelectedUnitLayers;
var
  Fractions: TRealArray;
  Index: integer;
  LayerFraction: TLayerFraction;
  SelectedUnit: TLayerGroup;
  GroupIndex: integer;
  LayerCount: integer;
begin
  if not FSettingUnit then
  begin
    for GroupIndex := 0 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[GroupIndex];
      if not TryStrToInt(rdeVDiscretization.Text, LayerCount) then
      begin
        LayerCount := SelectedUnit.LayerCount;
      end;

      SetSpacing(SelectedUnit.GrowthRate, SelectedUnit.GrowthMethod,
        LayerCount, Fractions);

      if Length(Fractions) <> SelectedUnit.LayerCollection.Count then
      begin
        SelectedUnit.LayerCollection.Clear;
      end;
      for Index := 0 to Length(Fractions) - 1 do
      begin
        if Index >= SelectedUnit.LayerCollection.Count then
        begin
          SelectedUnit.LayerCollection.Add;
        end;
        LayerFraction := SelectedUnit.LayerCollection.
          Items[Index] as TLayerFraction;
        LayerFraction.Fraction := Fractions[Length(Fractions)-Index-1];
      end;
    end;
  end;
end;

procedure TfrmLayers.UpdateSelectedUnits;
var
  StartIndex: integer;
  Index: Integer;
  NodeItem: TTreeNode;
begin
  if csDestroying in ComponentState then Exit;
  FSelectedUnits.Clear;
  FSelectedTreeNodes.Clear;
  if tvLayerGroups.Selected <> nil then
  begin
    NodeItem := tvLayerGroups.Selected;
    if NodeItem.Data <> nil then
    begin
      FSelectedUnits.Add(NodeItem.Data);
      FSelectedTreeNodes.Add(NodeItem);
    end;
    if tvLayerGroups.Selected <> nil then
    begin
      StartIndex := NodeItem.Index;
      for Index := Succ(StartIndex) to tvLayerGroups.Items.Count - 1 do
      begin
        NodeItem := tvLayerGroups.Items[Index];
        if NodeItem.Selected then
        begin
          if NodeItem.Data <> nil then
          begin
            FSelectedUnits.Add(NodeItem.Data);
            FSelectedTreeNodes.Add(NodeItem);
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmLayers.rdeVDiscretizationChange(Sender: TObject);
var
  VDisc: integer;
begin
  inherited;
  if csLoading in ComponentState then Exit;
  if rdeVDiscretization.Text <> '' then
  begin
    try
      VDisc := StrToInt(rdeVDiscretization.Text);
      rdgSubLayerBoundaries.Enabled := VDisc > 1;
      if rdgSubLayerBoundaries.Enabled then
      begin
        rdgSubLayerBoundaries.Color := clWindow;
      end
      else
      begin
        rdgSubLayerBoundaries.Color := clBtnFace;
      end;
      EnableGrowthRateControl;
      rgMethod.Enabled := VDisc > 1;
      if VDisc > 1 then
      begin
        rdgSubLayerBoundaries.RowCount := VDisc;
      end;
      UpdateStringGrid;
      UpdateLayerPositions;
      UpdateSelectedUnitLayers;
    except on E: EConvertError do
      begin         
        // ignore
      end;
    end;
    pbSubLayers.Invalidate;
  end
  else
  begin
    rdeGrowthRate.Enabled := False;
    rgMethod.Enabled := False;
  end;

  if not FSettingUnit then
  begin
    SetControlValues;
  end;
end;

procedure TfrmLayers.rdgSubLayerBoundariesExit(Sender: TObject);
begin
  inherited;
  UpdateStringGrid;
  UpdateSelectedUnitLayers;
end;

procedure TfrmLayers.rdgSubLayerBoundariesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  CanSelect := rgMethod.Enabled;
end;

procedure TfrmLayers.rdgSubLayerBoundariesSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  if not rgMethod.Enabled then
  begin
    Exit;
  end;
  rgMethod.ItemIndex := Integer(gmCustom);
  UpdateLayerPositions;
  if not FSettingUnit then
  begin
    UpdateSelectedUnitLayers;
  end;
  pbSubLayers.Invalidate;
end;

procedure TfrmLayers.rgMethodClick(Sender: TObject);
var
  Index: integer;
  SelectedUnit: TLayerGroup;
begin
  inherited;
  EnableGrowthRateControl;
  UpdateStringGrid;
  UpdateLayerPositions;
  if not FSettingUnit then
  begin
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      SelectedUnit.GrowthMethod := TGrowthMethod(rgMethod.ItemIndex);
    end;
    UpdateSelectedUnitLayers;
  end;
  pbSubLayers.Invalidate;
end;

procedure TfrmLayers.sbAddUnitClick(Sender: TObject);
begin
  inherited;
  AddNewUnit(tvLayerGroups.Items.Count);
end;

procedure TfrmLayers.sbDeleteUnitClick(Sender: TObject);
var
  Index: Integer;
  Item: TTreeNode;
  NewIndex: Integer;
begin
  inherited;
  NewIndex := 0;
  for Index := tvLayerGroups.Items.Count - 1 downto 0 do
  begin
    Item := tvLayerGroups.Items[Index];
    if Item.Selected then
    begin
      FLayerStructure.Delete(Index+1);
      NewIndex := Index-1;
      Item.Selected := False;
      tvLayerGroups.Items.Delete(Item);
    end;
  end;
  if NewIndex < 0 then
  begin
    NewIndex := tvLayerGroups.Items.Count - 1;
  end;
  tvLayerGroups.Items[NewIndex].Selected := True;
  sbDeleteUnit.Enabled := FLayerStructure.Count > 2;
  EnableOkButton;
end;

procedure TfrmLayers.sbInsertUnitClick(Sender: TObject);
var
  SelectIndex: integer;
begin
  inherited;
  if tvLayerGroups.Selected <> nil then
  begin
    SelectIndex := tvLayerGroups.Selected.Index;
    AddNewUnit(SelectIndex);
  end
  else
  begin
    sbAddUnitClick(nil);
  end;
end;

procedure TfrmLayers.SetControlValues;
var
  SelectedUnit: TLayerGroup;
  FirstUnit: TLayerGroup;
  Same: boolean;
  procedure AssignGrowthRate;
  var
    Index: integer;
  begin
    Same := True;
    for Index := 1 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      Same := FirstUnit.GrowthRate = SelectedUnit.GrowthRate;
      if not Same then
      begin
        break;
      end;
    end;
    if Same then
    begin
      rdeGrowthRate.Text := FloatToStr(FirstUnit.GrowthRate);
    end
    else
    begin
      rdeGrowthRate.Text := '';
    end;
  end;
  procedure AssignGrowthMethod;
  var
    Index: integer;
  begin
    Same := True;
    for Index := 1 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      Same := FirstUnit.GrowthMethod = SelectedUnit.GrowthMethod;
      if not Same then
      begin
        break;
      end;
    end;
    if Same then
    begin
      rgMethod.ItemIndex := Ord(FirstUnit.GrowthMethod);
    end
    else
    begin
      rgMethod.ItemIndex := -1;
    end;
  end;
  procedure InitializeSubsidenceGrid(Frame: TframeSubBeds);
  var
    ColIndex: Integer;
  begin
    Frame.rdgSubBed.BeginUpdate;
    try
      if FSelectedUnits.Count = 1 then
      begin
        if FirstUnit.LayerCollection.Count = 0 then
        begin
          Frame.rdgSubBed.ColCount := 1;
        end
        else
        begin
          Frame.rdgSubBed.ColCount := 2
            + FirstUnit.LayerCollection.Count+1;
          Frame.rdgSubBed.Cells[1,0] := 'Use in all layers';
          Frame.rdgSubBed.Columns[1].AutoAdjustRowHeights := True;
          Frame.rdgSubBed.Columns[1].WordWrapCaptions := True;
          for ColIndex := Ord(scUseAll) to
            Frame.rdgSubBed.ColCount - 1 do
          begin
            Frame.rdgSubBed.Columns[ColIndex].Format := rcf4Boolean;
          end;
          for ColIndex := Ord(fcFirst) to
            Frame.rdgSubBed.ColCount - 1 do
          begin
            Frame.rdgSubBed.ColWidths[ColIndex] := 10;
            Frame.rdgSubBed.Columns[ColIndex].AutoAdjustColWidths := True;
            Frame.rdgSubBed.Cells[ColIndex,0] := IntToStr(ColIndex-1);
          end;
        end;
        Frame.rdgSubBed.Cells[0,0] := 'Name'
      end;
    finally
      Frame.rdgSubBed.EndUpdate;
    end;
  end;
  procedure AssignDiscretization;
  var
    Index: integer;
  begin
    Same := True;
    for Index := 1 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      Same := FirstUnit.LayerCollection.Count = SelectedUnit.LayerCollection.Count;
      if not Same then
      begin
        break;
      end;
    end;
    if Same then
    begin
      rdeVDiscretization.Text := IntToStr(FirstUnit.
        LayerCollection.Count + 1);
    end
    else
    begin
      rdeVDiscretization.Text := '';
    end;
    rdeVDiscretizationChange(nil);
  end;
  procedure AssignAquiferType;
  var
    Index: integer;
  begin
    Same := True;
    for Index := 1 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      Same := FirstUnit.Simulated = SelectedUnit.Simulated;
      if not Same then
      begin
        break;
      end;
      if FirstUnit.Simulated then
      begin
        Same := FirstUnit.AquiferType = SelectedUnit.AquiferType;
        if not Same then
        begin
          break;
        end;
      end;
    end;
    if Same then
    begin
      if not FirstUnit.Simulated then
      begin
        comboAquiferType.ItemIndex := 0
      end
      else
      begin
        comboAquiferType.ItemIndex := FirstUnit.AquiferType + 1;
      end;
      comboAquiferTypeChange(comboAquiferType);
      tabDiscretization.TabVisible := comboAquiferType.ItemIndex > 0;
    end
    else
    begin
      comboAquiferType.ItemIndex := -1;
      tabDiscretization.TabVisible := FirstUnit.Simulated;
      if tabDiscretization.TabVisible then
      begin
        for Index := 1 to FSelectedUnits.Count - 1 do
        begin
          SelectedUnit := FSelectedUnits[Index];
          tabDiscretization.TabVisible := SelectedUnit.Simulated;
          if not tabDiscretization.TabVisible then
          begin
            break;
          end;
        end;
      end;
    end;
  end;
  procedure AssignTransmissivityMethod;
  var
    Index: integer;
  begin
    Same := True;
    for Index := 1 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      Same := FirstUnit.InterblockTransmissivityMethod
        = SelectedUnit.InterblockTransmissivityMethod;
      if not Same then
      begin
        break;
      end;
    end;
    if Same then
    begin
      comboInterblockMethod.ItemIndex :=
        FirstUnit.InterblockTransmissivityMethod;
    end
    else
    begin
      comboInterblockMethod.ItemIndex := -1;
    end;
  end;
  procedure AssignVerticalKMethod;
  var
    Index: integer;
  begin
    Same := True;
    for Index := 1 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      Same := FirstUnit.VerticalHydraulicConductivityMethod
        = SelectedUnit.VerticalHydraulicConductivityMethod;
      if not Same then
      begin
        break;
      end;
    end;
    if Same then
    begin
      comboVertKMethod.ItemIndex :=
        FirstUnit.VerticalHydraulicConductivityMethod;
    end
    else
    begin
      comboVertKMethod.ItemIndex := -1;
    end;
  end;
  procedure AssignCustomPostions;
  var
    Index: integer;
  begin
    if FirstUnit.GrowthMethod = gmCustom then
    begin
      Same := True;
      for Index := 1 to FSelectedUnits.Count - 1 do
      begin
        SelectedUnit := FSelectedUnits[Index];
        Same := (SelectedUnit.GrowthMethod = gmCustom) and
          SelectedUnit.LayerCollection.IsSame(FirstUnit.LayerCollection);;
        if not Same then
        begin
          break;
        end;
      end;
      if Same then
      begin
        for Index := 0 to FirstUnit.LayerCollection.Count - 1 do
        begin
          rdgSubLayerBoundaries.Cells[0,
            rdgSubLayerBoundaries.RowCount - Index]
            := FloatToStr((FirstUnit.LayerCollection.Items[Index]
            as TLayerFraction).Fraction);
        end;
        UpdateLayerPositions;
      end
      else
      begin
        for Index := 0 to SelectedUnit.LayerCollection.Count - 1 do
        begin
          rdgSubLayerBoundaries.Cells[0,
            rdgSubLayerBoundaries.RowCount - Index] := '';
        end;
      end;
    end
  end;
  procedure AssignComputeSaturatedThickness;
  var
    Index: integer;
  begin
    Same := True;
    for Index := 1 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      Same := FirstUnit.UseStartingHeadForSaturatedThickness
        = SelectedUnit.UseStartingHeadForSaturatedThickness;
      if not Same then
      begin
        break;
      end;
    end;
    if Same then
    begin
      cbComputeSaturatedThickness.Checked :=
        FirstUnit.UseStartingHeadForSaturatedThickness;
    end
    else
    begin
      cbComputeSaturatedThickness.AllowGrayed := True;
      cbComputeSaturatedThickness.State := cbGrayed;
    end;
  end;
  procedure AssignHorizontalAnisotropy;
  var
    Index: integer;
  begin
    Same := True;
    for Index := 1 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      Same := FirstUnit.HorizontalAnisotropy
        = SelectedUnit.HorizontalAnisotropy;
      if not Same then
      begin
        break;
      end;
    end;
    if Same then
    begin
      rdeAnisotropy.Text := FloatToStr(
        FirstUnit.HorizontalAnisotropy);
    end
    else
    begin
      rdeAnisotropy.Text := '';
    end;
  end;
  procedure AssignSubFrame(SubLayers: TCustomSubLayer; Frame: TframeSubBeds);
  var
    Index: Integer;
    ColIndex: Integer;
    ItemIndex: Integer;
    UseItem: TUseLayerNumberItem;
    AnItem: TCustomSubLayerItem;
  begin
    Frame.seCount.AsInteger := SubLayers.Count;
    Frame.seCountChange(nil);
    for Index := 0 to SubLayers.Count - 1 do
    begin
      AnItem := SubLayers.Items[Index] as TCustomSubLayerItem;
      Frame.rdgSubBed.Cells[Ord(scName), Index + 1] := AnItem.Name;
      if Frame.rdgSubBed.ColCount > 1 then
      begin
        Frame.rdgSubBed.Checked[Ord(scUseAll), Index + 1] :=
          AnItem.UseInAllLayers;
        if AnItem.UseInAllLayers then
        begin
          for ColIndex := Ord(fcFirst) to Frame.rdgSubBed.ColCount - 1 do
          begin
            Frame.rdgSubBed.Checked[ColIndex, Index + 1] := True;
          end;
        end
        else
        begin
          for ColIndex := Ord(fcFirst) to Frame.rdgSubBed.ColCount - 1 do
          begin
            Frame.rdgSubBed.Checked[ColIndex, Index + 1] := False;
          end;
          for ItemIndex := 0 to AnItem.UsedLayers.Count - 1 do
          begin
            UseItem := AnItem.UsedLayers[ItemIndex];
            ColIndex := UseItem.LayerNumber + 1;
            if ColIndex < Frame.rdgSubBed.ColCount then
            begin
              Frame.rdgSubBed.Checked[ColIndex, Index + 1] := True;
            end;
          end;
        end;
      end;
    end;
  end;
  procedure AssignNoDelayBeds;
  begin
    if FSelectedUnits.Count > 1 then
    begin
      tabNoDelay.TabVisible := False;
      Exit;
    end;
    tabNoDelay.TabVisible := frmGoPhast.PhastModel.
      ModflowPackages.SubPackage.IsSelected;;
    InitializeSubsidenceGrid(frameSubNoDelayBeds);
    AssignSubFrame(FirstUnit.SubNoDelayBedLayers, frameSubNoDelayBeds);
  end;
  procedure AssignDelayBeds;
  begin
    if FSelectedUnits.Count > 1 then
    begin
      tabDelay.TabVisible := False;
      Exit;
    end;
    tabDelay.TabVisible := frmGoPhast.PhastModel.
      ModflowPackages.SubPackage.IsSelected;;
    InitializeSubsidenceGrid(frameSubDelayBeds);
    AssignSubFrame(FirstUnit.SubDelayBedLayers, frameSubDelayBeds);
  end;
begin
  if csDestroying in ComponentState then Exit;

  FSettingUnit := True;
  edName.Enabled := FSelectedUnits.Count = 1;
  rdeGrowthRate.Enabled := FSelectedUnits.Count >= 1;
  rdeVDiscretization.Enabled := FSelectedUnits.Count >= 1;
  rgMethod.Enabled := FSelectedUnits.Count >= 1;
  comboAquiferType.Enabled := FSelectedUnits.Count >= 1;
  EnableK_Methods;
  EnableComputeSatThick;
  EnableAnisotropy;

  if FSelectedUnits.Count = 0 then
  begin
    Exit;
  end;
  FirstUnit := FSelectedUnits[0];

  try
    if FSelectedUnits.Count = 1 then
    begin
      edName.Text := FirstUnit.AquiferName;
    end
    else
    begin
      edName.Text := '';
    end;
    FirstUnit := FSelectedUnits[0];
    AssignGrowthRate;
    AssignDiscretization;
    AssignGrowthMethod;
    AssignAquiferType;
    AssignTransmissivityMethod;
    AssignVerticalKMethod;
    AssignCustomPostions;
    AssignComputeSaturatedThickness;
    AssignHorizontalAnisotropy;
    AssignNoDelayBeds;
    AssignDelayBeds;
  finally
    FSettingUnit := False;
  end;

end;

procedure TfrmLayers.EnableComputeSatThick;
var
  ConfinedLayer: Boolean;
  Index: Integer;
  SelectedUnit: TLayerGroup;
begin
  ConfinedLayer := False;
  if FUseSaturatedThickness and
    (FSelectedUnits.Count >= 1) then
  begin
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      if (SelectedUnit.AquiferType = 0) then
      begin
        ConfinedLayer := True;
        break;
      end;
    end;
  end;
  cbComputeSaturatedThickness.Enabled := ConfinedLayer
    and frmGoPhast.PhastModel.ModflowPackages.LpfPackage.IsSelected;
end;

procedure TfrmLayers.EnableK_Methods;
var
  SimulatedLayer: Boolean;
  ShouldEnable: Boolean;
begin
  if (FSelectedUnits.Count >= 1) then
  begin
    SimulatedLayer := comboAquiferType.ItemIndex > 0;
    ShouldEnable := SimulatedLayer
      and frmGoPhast.PhastModel.ModflowPackages.LpfPackage.IsSelected;
    comboVertKMethod.Enabled := ShouldEnable;
    ShouldEnable := SimulatedLayer
      and (frmGoPhast.PhastModel.ModflowPackages.LpfPackage.IsSelected
      or frmGoPhast.PhastModel.ModflowPackages.BcfPackage.IsSelected);
    comboInterblockMethod.Enabled := ShouldEnable;
  end
  else
  begin
    comboInterblockMethod.Enabled := False;
    comboVertKMethod.Enabled := False;
  end;
end;

procedure TfrmLayers.SetUpLayerTypeOptions;
var
  Packages: TModflowPackages;
  Item: TJvImageItem;
begin
  comboAquiferType.Items.Clear;
  Item := comboAquiferType.Items.Add;
  Item.Text := 'Non-simulated';
  Item.ImageIndex := 3;

  Item := comboAquiferType.Items.Add;
  Item.Text := 'Confined';
  Item.ImageIndex := 0;

  Packages := frmGoPhast.PhastModel.ModflowPackages;
  if Packages.BcfPackage.isSelected then
  begin
    Item := comboAquiferType.Items.Add;
    Item.Text := 'Unconfined';
    Item.ImageIndex := 2;

    Item := comboAquiferType.Items.Add;
    Item.Text := 'Limited convertible';
    Item.ImageIndex := 4;

    Item := comboAquiferType.Items.Add;
    Item.Text := 'Fully convertible';
    Item.ImageIndex := 1;
  end
  else if Packages.HufPackage.isSelected then
  begin
    Item := comboAquiferType.Items.Add;
    Item.Text := 'Convertible';
    Item.ImageIndex := 1;
  end
  else if Packages.LpfPackage.isSelected then
  begin
    Item := comboAquiferType.Items.Add;
    Item.Text := 'Convertible';
    Item.ImageIndex := 1;
  end
  else
  begin
    Assert(False);
  end;


  if Packages.HufPackage.IsSelected then
  begin
    comboAquiferType.Items[0].Brush.Color := clBtnFace;
  end
  else
  begin
    comboAquiferType.Items[0].Brush.Color := clWhite;
  end;
end;

procedure TfrmLayers.SetSpacing(const GrowthRate: real;
  const GrowthMethod: TGrowthMethod; const SubLayers: integer;
  out Fractions: TRealArray);
var
  Index: Integer;
  Sum: Real;
  CurrentLength: Real;
  StopIndex: Integer;
  RealList: TRealList;
  Value: Real;
  StartIndex: Integer;
begin
  SetLength(Fractions, 0);
  try
    if SubLayers > 1 then
    begin
      SetLength(Fractions, SubLayers-1);

      case GrowthMethod of
        gmUniform:
          begin
            for Index := 0 to SubLayers - 2 do
            begin
              Fractions[SubLayers - 2 - Index] := (Index+1)/SubLayers;
            end;
          end;
        gmUp:
          begin
            Sum := 1;
            CurrentLength := 1;
            for Index := 0 to SubLayers-2 do
            begin
              Fractions[SubLayers - 2 - Index] := Sum;
              CurrentLength := CurrentLength * GrowthRate;
              Sum := Sum + CurrentLength;
            end;
            for Index := 0 to SubLayers - 2 do
            begin
              Fractions[Index] := Fractions[Index]/ Sum;
            end;
          end;
        gmDown:
          begin
            Sum := 1;
            CurrentLength := 1;
            for Index := 0 to SubLayers-2 do
            begin
              Fractions[SubLayers - 2 - Index] := Sum;
              CurrentLength := CurrentLength / GrowthRate;
             Sum := Sum + CurrentLength;
            end;
            for Index := 0 to SubLayers - 2 do
            begin
              Fractions[Index] := Fractions[Index]/ Sum;
            end;
          end;
        gmMiddle, gmEdge:
          begin
            if Odd(SubLayers) then
            begin
              StopIndex := (SubLayers div 2);
            end
            else
            begin
              StopIndex := (SubLayers div 2)-1;
            end;

            Sum := 1;
            CurrentLength := 1;
            for Index := 0 to StopIndex-1 do
            begin
              Fractions[SubLayers - 2 - Index] := Sum;
              case GrowthMethod of
                gmMiddle:
                  begin
                    CurrentLength := CurrentLength * GrowthRate;
                  end;
                gmEdge:
                  begin
                    CurrentLength := CurrentLength / GrowthRate;
                  end;
                else Assert(False);
              end;
              Sum := Sum + CurrentLength;
            end;
            StartIndex := StopIndex;
            if not Odd(SubLayers) then
            begin
              Fractions[SubLayers - 2 - StartIndex] := Sum;
              Sum := Sum + CurrentLength;
              Inc(StartIndex);
            end;
            for Index := StartIndex to SubLayers-2 do
            begin
              Fractions[SubLayers - 2 - Index] := Sum;
              case GrowthMethod of
                gmMiddle:
                  begin
                    CurrentLength := CurrentLength / GrowthRate;
                  end;
                gmEdge:
                  begin
                    CurrentLength := CurrentLength * GrowthRate;
                  end;
                else Assert(False);
              end;
              Sum := Sum + CurrentLength;
            end;
            for Index := 0 to SubLayers - 2 do
            begin
              Fractions[Index] := Fractions[Index]/ Sum;
            end;
          end;
        gmCustom:
          begin
            RealList := TRealList.Create;
            try
              for Index := 0 to rdgSubLayerBoundaries.RowCount - 1 do
              begin
                try
                  if rdgSubLayerBoundaries.Cells[0,Index+1] <> '' then
                  begin
                    RealList.Add(StrToFloat(rdgSubLayerBoundaries.Cells[0,Index+1]));
                  end;
                except on E: EConvertError do
                  begin
                    // ignore
                  end;
                end;
              end;
              RealList.Sort;
              for Index := RealList.Count - 1 downto 0 do
              begin
                Value := RealList[Index];
                if (Value >= 1) or (Value <= 0) then
                begin
                  RealList.Delete(Index);
                  Continue;
                end;
                if Index > 0 then
                begin
                  if Value = RealList[Index-1] then
                  begin
                    RealList.Delete(Index);
                  end;
                end;
              end;
              SetLength(Fractions, RealList.Count);
              for Index := 0 to RealList.Count - 1 do
              begin
                Fractions[RealList.Count - 1-  Index] := RealList[Index];
              end;
            finally
              RealList.Free;
            end;
          end;
        else
          begin
            // multiple layers selected
            // do nothing
          end;
      end;
    end;
  except on E: EConvertError do
    begin
      // ignore
    end;
  end;
end;

procedure TfrmLayers.UpdateLayerPositions;
var
  Fractions: TRealArray;
  GrowthRate: double;
  LayerCount: integer;
begin
  if rgMethod.ItemIndex < 0 then
  begin
    Exit;
  end;
  if TryStrToFloat(rdeGrowthRate.Text, GrowthRate) then
  begin
    if rgMethod.ItemIndex >= 0 then
    begin
      if TryStrToInt(rdeVDiscretization.Text, LayerCount) then
      begin
        SetSpacing(GrowthRate, TGrowthMethod(rgMethod.ItemIndex),
          LayerCount, Fractions);
        GetLayerPostions(Fractions, FLayerPositions);
      end;
    end;
  end;
end;

procedure TfrmLayers.UpdateStringGrid;
var
  LayerCount: integer;
  Fractions: TRealArray;
  Index: Integer;
  GrowthRate: double;
begin
  if TryStrToFloat(rdeGrowthRate.Text, GrowthRate) then
  begin
    if rgMethod.ItemIndex >= 0 then
    begin
      if tryStrToInt(rdeVDiscretization.Text, LayerCount) then
      begin
        if LayerCount > 1 then
        begin
          SetSpacing(GrowthRate, TGrowthMethod(rgMethod.ItemIndex),
            LayerCount, Fractions);
          for Index := 0 to Length(Fractions) - 1 do
          begin
            rdgSubLayerBoundaries.Cells[0,Index+1]
              := FloatToStr(Fractions[Index]);
          end;
        end;
      end;
    end;
  end;
end;

{ TUndoDefineLayers }

constructor TUndoDefineLayers.Create(var NewLayerStructure: TLayerStructure);
begin
  inherited Create;
  FNewDataSets := TList.Create;

  FNewLayerStructure:= NewLayerStructure;
  // TUndoDefineLayers takes ownership of NewLayerStructure.
  NewLayerStructure := nil;
  FOldLayerStructure:= TLayerStructure.Create(nil);
  FOldLayerStructure.Assign(frmGoPhast.PhastModel.LayerStructure);
end;

function TUndoDefineLayers.Description: string;
begin
  result := 'change layer structure';
end;

destructor TUndoDefineLayers.Destroy;
begin
  FNewLayerStructure.Free;
  FOldLayerStructure.Free;
  FNewDataSets.Free;
  inherited;
end;

procedure TUndoDefineLayers.DoCommand;
begin
  frmGoPhast.CanDraw := False;
  try
    inherited;
    frmGoPhast.PhastModel.LayerStructure.NewDataSets := FNewDataSets;
    frmGoPhast.PhastModel.LayerStructure.ClearNewDataSets;
    frmGoPhast.PhastModel.LayerStructure.Assign(FNewLayerStructure);
    frmGoPhast.PhastModel.LayerStructure.NewDataSets := nil;
    UpdatedRequiredDataSets;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

procedure TUndoDefineLayers.Undo;
begin
  frmGoPhast.CanDraw := False;
  try
    inherited;
    frmGoPhast.PhastModel.LayerStructure.NewDataSets := FNewDataSets;
    frmGoPhast.PhastModel.LayerStructure.Assign(FOldLayerStructure);
    frmGoPhast.PhastModel.LayerStructure.RemoveNewDataSets;
    frmGoPhast.PhastModel.LayerStructure.NewDataSets := nil;
    UpdatedRequiredDataSets;
  finally
    frmGoPhast.CanDraw := True;
  end;
end;

end.
