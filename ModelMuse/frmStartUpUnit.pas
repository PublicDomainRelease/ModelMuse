{@abstract(The main purpose of @name is to define @link(TfrmStartUp)
  which is used to specify the grid for a new model or open an
  existing model.)}
unit frmStartUpUnit;

interface

uses
  Windows, SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, ExtCtrls,
  ComCtrls, Buttons, ArgusDataEntry, RbwEdit, Grids, RbwDataGrid4,
  frameInitialGridPositionUnit;
type
  {@abstract(@name is used to specify the grid for a new model or open an
    existing model.)}
  TfrmStartUp = class(TfrmCustomGoPhast)
    btnDontCreateGrid: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnNextClick).
    btnNext: TBitBtn;
    // @name: TGroupBox;
    // @name groups together the controls for creating an initial grid.
    gbInitialGrid: TGroupBox;
    // @name: TLabel;
    // @name displays "Number of nodes in X (column) direction".
    lblNumNodesX: TLabel;
    // @name: TLabel;
    // @name displays "Number of nodes in Y (row) direction".
    lblNumNodesY: TLabel;
    // @name: TLabel;
    // @name displays "Number of nodes in Z (layer) direction".
    lblNumNodesZ: TLabel;
    // @name: TLabel;
    // @name displays "Distance between X nodes".
    lblXDist: TLabel;
    // @name: TLabel;
    // @name displays "Distance between Y nodes".
    lblYDist: TLabel;
    // @name: TLabel;
    // @name displays "Distance between Z nodes".
    lblZDist: TLabel;
    // @name: TPageControl;
    // @name holds @link(tabModelChoice) and @link(tabInitialGrid).
    pcStartup: TPageControl;
    // @name: TPanel;
    // @name holds the buttons at the bottom of @classname.
    pnlBottom: TPanel;
    // @name: TRbwDataEntry;
    // @name is used to specify the default column width
    // (@link(TCustomModelGrid.ColumnWidth)).
    rdeColWidth: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the default layer height
    // (@link(TPhastGrid.LayerThickness)).
    rdeLayerHeight: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the number of column
    // boundaries in the @link(TPhastGrid).
    rdeNCol: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the number of layer
    // boundaries in the @link(TPhastGrid).
    rdeNLay: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the number of row
    // boundaries in the @link(TPhastGrid).
    rdeNRow: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the default row width
    // (@link(TCustomModelGrid.RowWidth)).
    rdeRowWidth: TRbwDataEntry;
    // @name: TRadioGroup;
    // @name is used to choose to open an existing model or create a new one.
    rgChoice: TRadioGroup;
    // @name: TTabSheet;
    // @name holds the controls for creating an initial grid.
    tabInitialGrid: TTabSheet;
    // @name: TTabSheet;
    // @name holds @link(rgChoice).
    tabModelChoice: TTabSheet;
    // @name is used to display help on this @classname.
    btnHelp: TBitBtn;
    tabInitialModflowGrid: TTabSheet;
    gbInitialGridModflow: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    rdeModflowRowWidth: TRbwDataEntry;
    rdeModflowColWidth: TRbwDataEntry;
    rdeModflowLayerCount: TRbwDataEntry;
    rdeModflowRowCount: TRbwDataEntry;
    rdeModflowColumnCount: TRbwDataEntry;
    rdgInitialLayers: TRbwDataGrid4;
    frameInitialGridPosition: TframeInitialGridPosition;
    // @name sets the vertical exaggeration of the model
    // but does not set up the grid.
    procedure btnDontCreateGridClick(Sender: TObject);
    // If @link(tabModelChoice) is the active page, @name either opens
    // a new model or advances to @link(tabInitialGrid) depending on the
    // choice in @link(rgChoice). @br
    // If @link(tabInitialGrid) is the active page, @name sets up the
    // grid using the values displayed on @link(tabInitialGrid) by
    // calling @link(SetUpPhastGrid).
    // If @link(tabInitialModflowGrid) is the active page, @name sets up the
    // grid using the values displayed on @link(tabInitialModflowGrid) by
    // calling @link(SetUpModflowGrid).
    procedure btnNextClick(Sender: TObject);
    // @name initializes @classname.
    procedure FormCreate(Sender: TObject); override;
    // @name sets the HelpKeyword property of @link(btnHelp) to the
    // active page in @link(pcStartup).
    procedure FormShow(Sender: TObject);
    procedure pcStartupChange(Sender: TObject);
    procedure rdeModflowLayerCountChange(Sender: TObject);
    procedure rdgInitialLayersSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgInitialLayersEndUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    // @name sets up the
    // @link(TPhastGrid) using the values displayed on @link(tabInitialGrid).
    procedure SetUpPhastGrid;
    // Set the vertical exaggeration.
    procedure SetExaggeration;
    procedure SetUpModflowGrid;
    procedure InitializeView(ModelXWidth, ModelYWidth, ModelHeight: Real);
    procedure SetUpModflowLayers(ColCount, RowCount: Integer;
      out ModelHeight: Real);
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses Math, Contnrs, frmGoPhastUnit, GoPhastTypes, frmGoToUnit, DataSetUnit,
  RbwParser, LayerStructureUnit, PhastModelUnit, ModelMuseUtilities;

resourcestring
  StrInitialGrid = 'Initial Grid';
  StrFinish = 'Finish';
  StrLayerGroupName = 'Layer group name';
  StrBottomElevation = 'Bottom elevation';
  StrUpperAquifer = 'Upper Aquifer';
  StrMiddleAquifer = 'Middle Aquifer';
  StrLowerAquifer = 'Lower Aquifer';


{$R *.dfm}

Const DefaultItemCount = 4;

procedure TfrmStartUp.btnNextClick(Sender: TObject);
var
  FileName: string;
begin
  inherited;
  case pcStartup.ActivePageIndex of
    0:
      begin
        case rgChoice.ItemIndex of
          0,1: // The user has chosen to create a new model. Go to the next page.
            begin
              Caption := StrInitialGrid;
              btnNext.Caption := StrFinish;
              btnDontCreateGrid.Visible := True;
              btnDontCreateGrid.Left := btnNext.Left - btnDontCreateGrid.Width - 8;
              btnHelp.Left := btnDontCreateGrid.Left - btnHelp.Width - 8;
              if rgChoice.ItemIndex = 0 then
              begin
                // new MODFLOW model.
                frameInitialGridPosition.Parent := gbInitialGridModflow;
                frmGoPhast.ModelSelection := msModflow;
                pcStartup.ActivePageIndex := 2;
              end
              else
              begin
                // new PHAST model
                frameInitialGridPosition.Parent := gbInitialGrid;
                frmGoPhast.ModelSelection := msPhast;;
                pcStartup.ActivePageIndex := 1;
              end;
              frmGoPhast.UpdateModelSelection;
            end;
          2: // The user has chosen to open an existing model.
            begin
              if frmGoPhast.odOpenDialog.Execute then
              begin
                ModalResult := mrOK;
                frmGoPhast.OpenAFile(frmGoPhast.odOpenDialog.FileName);
              end;
//              frmGoPhast.acFileOpenExecute(nil);
            end;
          3:
            begin
              // import a model
              Hide;
              ModalResult := mrOK;
              frmGoPhast.acFileNewModflowModelExecute(frmGoPhast.miModflow2005Model)
            end
        else
          begin
            Assert(rgChoice.ItemIndex >= DefaultItemCount);
            ModalResult := mrOK;
            FileName := frmGoPhast.
              MostRecentlyUsed.FileNames[rgChoice.ItemIndex-DefaultItemCount];
            frmGoPhast.OpenAFile(FileName);
          end;
        end;
        pcStartupChange(nil);
      end;
    1: // The user is creating a new model. Create the initial grid.
      begin
        SetUpPhastGrid;
        ModalResult := mrOK;
      end;
    2:  // The user is creating a new MODFLOW model.
      begin
        SetUpModflowGrid;
        ModalResult := mrOK;
      end
  else
    Assert(False);
  end;
end;

procedure TfrmStartUp.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if ModalResult <> mrOK then
  begin
    Application.Terminate;
  end;
end;

procedure TfrmStartUp.FormCreate(Sender: TObject);
var
  Index: integer;
  FileName: string;
  Control: TControl;
begin
  inherited;
  SetAppearance;
  Caption := StrModelName;
  Assert(DefaultItemCount = rgChoice.Items.Count);
  for Index := 0 to pcStartup.PageCount - 1 do
  begin
    pcStartup.Pages[Index].TabVisible := False;
  end;

  pcStartup.ActivePageIndex := 0;
  for Index := 0 to frmGoPhast.MostRecentlyUsed.FileNames.Count -1 do
  begin
    FileName := frmGoPhast.MostRecentlyUsed.FileNames[Index];
    rgChoice.Items.Add(ExtractFileName(FileName) + '    (' + FileName + ')');

    // This will cause TCustomRadioGroup.UpdateButtons to be called.
    rgChoice.WordWrap := not rgChoice.WordWrap;
    rgChoice.WordWrap := not rgChoice.WordWrap;

//    rgChoice.Items.Add(ExtractFileName(FileName));
//    Control := rgChoice.Controls[rgChoice.ControlCount-1];
    Control := rgChoice.Buttons[rgChoice.ControlCount-1];
    Control.Hint := FileName;
    Control.ShowHint := True;
  end;
  if rgChoice.Items.Count > DefaultItemCount then
  begin
    rgChoice.ItemIndex := DefaultItemCount;
  end;

  rdgInitialLayers.Cells[0,0] := StrLayerGroupName;
  rdgInitialLayers.Cells[1,0] := StrBottomElevation;
  rdgInitialLayers.Cells[0,1] := StrModelTop;
  rdgInitialLayers.Cells[1,1] := '0';
  rdgInitialLayers.Cells[0,2] := StrUpperAquifer;
  rdgInitialLayers.Cells[1,2] := '-10';
  rdgInitialLayers.Cells[0,3] := StrMiddleAquifer;
  rdgInitialLayers.Cells[1,3] := '-20';
  rdgInitialLayers.Cells[0,4] := StrLowerAquifer;
  rdgInitialLayers.Cells[1,4] := '-30';

  rdgInitialLayers.Col := 1;

end;

procedure TfrmStartUp.SetExaggeration;
begin
  // Set the vertical exaggeration.
  if Trim(frameInitialGridPosition.rdeExaggeration.Text) = '' then
  begin
    frmGoPhast.PhastModel.Exaggeration := frmGoPhast.DefaultVE;
  end
  else
  begin
    frmGoPhast.PhastModel.Exaggeration :=
      StrToFloat(frameInitialGridPosition.rdeExaggeration.Text);
  end;
end;

Type
  TLayerStorage = class
    Name: string;
    Elevation: real;
  end;

procedure TfrmStartUp.SetUpModflowGrid;
var
  Dimension: TOneDRealArray;
  Index: integer;
  XOrigin, YOrigin: double;
  Angle: double;
  XStart, YStart: double;
  OriginAngle: double;
  OriginDistance: double;
  ColWidth, RowWidth: double;
  ColCount, RowCount: integer;
  ModelYWidth: Real;
  ModelXWidth: Real;
  ModelHeight: Real;
begin
  // get some initial data.
  XOrigin := StrToFloat(frameInitialGridPosition.rdeX.Text);
  YOrigin := StrToFloat(frameInitialGridPosition.rdeY.Text);
  Angle := DegToRad(StrToFloat(frameInitialGridPosition.rdeAngle.Text));
  OriginDistance := Sqrt(Sqr(XOrigin) + Sqr(YOrigin));

  // Determine where the starting positions for the grid in the top view.
  if OriginDistance <> 0 then
  begin
    OriginAngle := ArcTan2(YOrigin, XOrigin);
    XStart := Cos(Angle - OriginAngle) * OriginDistance;
    YStart := -Sin(Angle - OriginAngle) * OriginDistance;
  end
  else
  begin
    XStart := 0;
    YStart := 0;
  end;

  frmGoPhast.ModflowGrid.GridAngle := Angle;

  // Set up the columns.
  ColWidth := StrToFloat(rdeModflowColWidth.Text);
  ColCount := StrToInt(rdeModflowColumnCount.Text);
  SetLength(Dimension, ColCount+1);
  for Index := 0 to ColCount do
  begin
    Dimension[Index] := Index * ColWidth + XStart;
  end;
  frmGoPhast.ModflowGrid.ColumnPositions := Dimension;

  // Set up the rows.
  RowWidth := StrToFloat(rdeModflowRowWidth.Text);
  RowCount := StrToInt(rdeModflowRowCount.Text);
  SetLength(Dimension, RowCount+1);
  for Index := 0 to RowCount do
  begin
    Dimension[Index] := YStart- Index * RowWidth;
  end;
  frmGoPhast.ModflowGrid.RowPositions := Dimension;
  try
    SetUpModflowLayers(ColCount, RowCount, ModelHeight);
  except on E: EOutOfMemory do
    begin
      Beep;
      MessageDlg(E.message, mtError, [mbOK], 0);
    end;
  end;
//  frmGoPhast.PhastModel.ClearNameChangeWarnings;

  ModelXWidth := ColCount * ColWidth;
  ModelYWidth := RowCount * RowWidth;

  // Set the selected layer.
  frmGoPhast.ModflowGrid.SelectedLayer := 0;
  frmGoPhast.PhastModel.DataArrayManager.CreateInitialDataSets;
  InitializeView(ModelXWidth, ModelYWidth, ModelHeight);
end;

procedure TfrmStartUp.InitializeView(ModelXWidth, ModelYWidth, ModelHeight: Real);
begin
  SetExaggeration;

  frmGoPhast.InitializeView(ModelXWidth, ModelYWidth, ModelHeight);
end;

procedure TfrmStartUp.SetUpModflowLayers(ColCount, RowCount: Integer;
  out ModelHeight: Real);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  NewLayerElevations: TThreeDRealArray;
  LayerCount: Integer;
  NewDataArray: TDataArray;
//  DataArrayIndex: Integer;
  LayerGroup: TLayerGroup;
  LayerStorage: TLayerStorage;
  Value: Real;
  Layers: TList;

  Local_Index: Integer;
  LayerName: string;
begin
  ModelHeight := 1;
  // Set up the layers.
  Layers := TObjectList.Create;
  try
    for Local_Index := 2 to rdgInitialLayers.RowCount - 1 do
    begin
      try
        Value := StrToFloat(rdgInitialLayers.Cells[1, Local_Index]);
        LayerName := Trim(rdgInitialLayers.Cells[0, Local_Index]);
        if LayerName = '' then
        begin
          Continue;
        end;
        LayerStorage := TLayerStorage.Create;
        Layers.Add(LayerStorage);
        LayerStorage.Name := LayerName;
        LayerStorage.Elevation := Value;
      except
        on E: EConvertError do
        begin
        end;
      end;
      // ignore
    end;
    Value := StrToFloat(rdgInitialLayers.Cells[1, 1]);
    LayerGroup := frmGoPhast.PhastModel.LayerStructure.Add as TLayerGroup;
    LayerGroup.AquiferName := StrModelTop;
    NewDataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(LayerGroup.DataArrayName);
    Assert(NewDataArray <> nil);
//    NewDataArray := frmGoPhast.PhastModel.DataSets[DataArrayIndex];
    NewDataArray.Formula := rdgInitialLayers.Cells[1, 1];
    LayerCount := Layers.Count + 1;
    SetLength(NewLayerElevations, ColCount, RowCount, LayerCount);
    for ColIndex := 0 to ColCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        NewLayerElevations[ColIndex, RowIndex, 0] := Value;
      end;
    end;
    for LayerIndex := 0 to Layers.Count - 1 do
    begin
      LayerStorage := Layers[LayerIndex];
      LayerGroup := frmGoPhast.PhastModel.LayerStructure.Add as TLayerGroup;
      LayerGroup.AquiferName := LayerStorage.Name;
      NewDataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(LayerGroup.DataArrayName);
      Assert(NewDataArray <> nil);
//      NewDataArray := frmGoPhast.PhastModel.DataSets[DataArrayIndex];
      NewDataArray.Formula := FortranFloatToStr(LayerStorage.Elevation);
      for ColIndex := 0 to ColCount - 1 do
      begin
        for RowIndex := 0 to RowCount - 1 do
        begin
          NewLayerElevations[ColIndex, RowIndex, LayerIndex + 1] := LayerStorage.Elevation;
        end;
      end;
      if LayerIndex = Layers.Count - 1 then
      begin
        ModelHeight := Value - LayerStorage.Elevation;
      end;
    end;
  finally
    Layers.Free;
  end;
  frmGoPhast.ModflowGrid.LayerElevations := NewLayerElevations;
  frmGoPhast.PhastModel.LayerStructure.AssignAssociatedInputDataSets;
//  frmGoPhast.PhastModel.ClearNameChangeWarnings;
end;

procedure TfrmStartUp.SetUpPhastGrid;
var
  Dimension: TOneDRealArray;
  Index: integer;
  XOrigin, YOrigin: double;
  Angle: double;
  XStart, YStart, ZStart: double;
  OriginAngle: double;
  OriginDistance: double;
  ColWidth, RowWidth, LayerHeight: double;
  ColCount, RowCount, LayerCount: integer;
  ModelXWidth: Real;
  ModelYWidth: Real;
  ModelHeight: Real;
begin
  // get some initial data.
  XOrigin := StrToFloat(frameInitialGridPosition.rdeX.Text);
  YOrigin := StrToFloat(frameInitialGridPosition.rdeY.Text);
  Angle := DegToRad(StrToFloat(frameInitialGridPosition.rdeAngle.Text));
  OriginDistance := Sqrt(Sqr(XOrigin) + Sqr(YOrigin));

  // Determine where the starting positions for the grid in the top view.
  if OriginDistance <> 0 then
  begin
    OriginAngle := ArcTan2(YOrigin, XOrigin);
    XStart := Cos(Angle - OriginAngle) * OriginDistance;
    YStart := -Sin(Angle - OriginAngle) * OriginDistance;
  end
  else
  begin
    XStart := 0;
    YStart := 0;
  end;
  ZStart := StrToFloat(frameInitialGridPosition.rdeZ.Text);

  frmGoPhast.PhastGrid.GridAngle := Angle;

  // Set up the columns.
  ColWidth := StrToFloat(rdeColWidth.Text);
  ColCount := StrToInt(rdeNCol.Text);
  SetLength(Dimension, ColCount);
  for Index := 0 to ColCount - 1 do
  begin
    Dimension[Index] := Index * ColWidth + XStart;
  end;
  frmGoPhast.PhastGrid.ColumnPositions := Dimension;

  // Set up the rows.
  RowWidth := StrToFloat(rdeRowWidth.Text);
  RowCount := StrToInt(rdeNRow.Text);
  SetLength(Dimension, RowCount);
  for Index := 0 to RowCount - 1 do
  begin
    Dimension[Index] := Index * RowWidth + YStart;
  end;
  frmGoPhast.PhastGrid.RowPositions := Dimension;

  // Set up the layers.
  LayerHeight := StrToFloat(rdeLayerHeight.Text);
  LayerCount := StrToInt(rdeNLay.Text);
  SetLength(Dimension, LayerCount);
  for Index := 0 to LayerCount - 1 do
  begin
    Dimension[Index] := Index * LayerHeight + ZStart;
  end;
  frmGoPhast.PhastGrid.LayerElevations := Dimension;

  // Set the selected layer.
  frmGoPhast.PhastGrid.SelectedLayer := LayerCount - 1;

  ModelXWidth := ColCount * ColWidth;
  ModelYWidth := RowCount * RowWidth;
  ModelHeight := LayerCount * LayerHeight;

  InitializeView(ModelXWidth, ModelYWidth, ModelHeight);
  frmGoPhast.PhastModel.DataArrayManager.CreateInitialDataSets;
end;

procedure TfrmStartUp.pcStartupChange(Sender: TObject);
begin
  inherited;
  HelpKeyword := pcStartup.ActivePage.HelpKeyword;

end;

procedure TfrmStartUp.rdeModflowLayerCountChange(Sender: TObject);
var
  Value: integer;
begin
  inherited;
  if rdgInitialLayers = nil then Exit;
  try
    if Trim(rdeModflowLayerCount.Text) <> '' then
    begin
      Value  := StrToInt(Trim(rdeModflowLayerCount.Text));
      if Value > 0 then
      begin
        rdgInitialLayers.RowCount := Value+2;
      end;
    end;
  except on EConvertError do
    begin
      // ignore
    end;
  end;
end;

procedure TfrmStartUp.rdgInitialLayersEndUpdate(Sender: TObject);
begin
  inherited;
  rdeModflowLayerCount.Text := IntToStr(rdgInitialLayers.RowCount-2);
end;

procedure TfrmStartUp.rdgInitialLayersSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  CanSelect := (ARow <> 1) or (ACol <> 0);
end;

procedure TfrmStartUp.FormShow(Sender: TObject);
begin
  inherited;
  HelpKeyword := pcStartup.ActivePage.HelpKeyword;
end;

procedure TfrmStartUp.btnDontCreateGridClick(Sender: TObject);
var
  ModelHeight: Real;
begin
  inherited;
  SetExaggeration;

  if frmGoPhast.ModelSelection = msModflow then
  begin
    SetUpModflowLayers(0, 0, ModelHeight);
  end;
  frmGoPhast.SynchronizeViews(vdTop);
  frmGoPhast.AdjustScales;
  ModalResult := mrOK;
end;

end.

