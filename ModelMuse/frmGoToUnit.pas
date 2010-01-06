{@abstract(The main purpose of @name is to define @link(TfrmGoTo)
  which is used to move the viewpoint to a selected position,
 cell, or @link(TScreenObject).)}
unit frmGoToUnit;

interface

uses
  UndoItemsScreenObjects, SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, ComCtrls, Buttons, CompressedImageUnit,
  ExtCtrls, Spin, ArgusDataEntry, Mask, JvExMask, JvSpin;

type
  {@abstract(@name is used to move the viewpoint to a selected position,
   cell, or @link(TScreenObject).)}
  TfrmGoTo = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes @classname without changing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TCheckBox;
    // @name indicates that the position on  the front view of the model
    // will be changed.
    // See @link(cbClick).
    cbFront: TCheckBox;
    // @name: TCheckBox;
    // @name indicates that the object being moved to will also be selected.
    cbSelectObject: TCheckBox;
    // @name: TCheckBox;
    // @name indicates that the position on  the side view of the model
    // will be changed.
    // See @link(cbClick).
    cbSide: TCheckBox;
    // @name: TCheckBox;
    // @name indicates that the position on  the top view of the model
    // will be changed.
    // See @link(cbClick).
    cbTop: TCheckBox;
    // @name: TLabel;
    // @name displays "Column".
    lblCol: TLabel;
    // @name: TLabel;
    // @name displays "Layer".
    lblLay: TLabel;
    // @name: TLabel;
    // @name displays "Row".
    lblRow: TLabel;
    // @name: TLabel;
    // @name displays "X".
    lblX: TLabel;
    // @name: TLabel;
    // @name displays "X'".
    lblXPrime: TLabel;
    // @name: TLabel;
    // @name displays "Y".
    lblY: TLabel;
    // @name: TLabel;
    // @name displays "Y'".
    lblYPrime: TLabel;
    // @name: TLabel;
    // @name displays "Z".
    lblZ: TLabel;
    // @name: TListView;
    // @name displays a list of @link(TScreenObject)s.
    lvScreenObjects: TListView;
    // @name: TPageControl;
    // @name show the options for moving to a new location.
    pcMain: TPageControl;
    // @name: TPanel;
    // @name holds the buttons at the bottom of the disk.
    pnlBottom: TPanel;
    // @name: TPanel;
    // @name holds @link(cbSelectObject).
    pnlObject: TPanel;
    // @name: TRbwDataEntry;
    // @name specifies the X-coordinate to move to.
    rdeX: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name specifies the X'-coordinate to move to.
    rdeXPrime: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name specifies the Y-coordinate to move to.
    rdeY: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name specifies the Y'-coordinate to move to.
    rdeYPrime: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name specifies the Z-coordinate to move to.
    rdeZ: TRbwDataEntry;
    // @name: TTabSheet;
    // @name holds controls for moving to a particular cell.
    tabCell: TTabSheet;
    // @name: TTabSheet;
    // @name holds controls for moving to a particular @link(TScreenObject).
    tabObject: TTabSheet;
    // @name: TTabSheet;
    // @name holds controls for moving to a position.
    tabPosition: TTabSheet;
    seCol: TJvSpinEdit;
    seRow: TJvSpinEdit;
    seLayer: TJvSpinEdit;
    tabImage: TTabSheet;
    lvImages: TListView;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name enables and disables controls depending on which of
    // @link(cbFront), @link(cbSide), and @link(cbTop) are checked.
    procedure cbClick(Sender: TObject);
    // @name initializes @classname and calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
    // @name enables moving to a particular cell if a grid has been defined.
    procedure FormShow(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
  private
    // @name stores information about the current position.
    procedure GetData;
    // @name moves the selected view or views to the selected
    // cell, object, or position.
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

{
  @name moves the top view of the model so that the point
  (XCoordinate,YCoordinate) is at the center of the view.
}
procedure SetTopPosition(const XCoordinate, YCoordinate: real);

{
  @name moves the front view of the model so that the point
  (XCoordinate,ZCoordinate) is at the center of the view.
}
procedure SetFrontPosition(const XCoordinate, ZCoordinate: real);

{
  @name moves the side view of the model so that the point
  (YCoordinate,ZCoordinate) is at the center of the view.
}
procedure SetSidePosition(const YCoordinate, ZCoordinate: real);

// @name moves the top view of the model to the cell at Column, Row.
procedure MoveToTopCell(const Column, Row: integer);

// @name moves the front view of the model to the cell at Column, Layer.
procedure MoveToFrontCell(const Column, Layer: integer);

// @name moves the side view of the model to the cell at Row, Layer.
procedure MoveToSideCell(const Row, Layer: integer);

procedure MoveToImage(BitMapItem: TCompressedBitmapItem);

implementation

uses frmGoPhastUnit, GoPhastTypes, AbstractGridUnit, ScreenObjectUnit,
  DataSetUnit, FastGEO;

{$R *.dfm}

procedure SetTopPosition(const XCoordinate, YCoordinate: real);
var
  DeltaX, DeltaY: double;
begin
  with frmGoPhast.frameTopView.ZoomBox do
  begin
    DeltaX := (X(Image32.Width) - X(0)) / 2;
    DeltaY := (Y(0) - Y(Image32.Height)) / 2;
    OriginX := XCoordinate - DeltaX;
    OriginY := YCoordinate - DeltaY;
    frmGoPhast.TopGridChanged := True;
    Image32.Invalidate;
    frmGoPhast.AdjustScales;
    frmGoPhast.SynchronizeViews(vdTop);
  end;
end;

procedure SetFrontPosition(const XCoordinate, ZCoordinate: real);
var
  DeltaX, DeltaY: double;
begin
  with frmGoPhast.frameFrontView.ZoomBox do
  begin
    DeltaX := (X(Image32.Width) - X(0)) / 2;
    DeltaY := (Y(0) - Y(Image32.Height)) / 2;
    OriginX := XCoordinate - DeltaX;
    OriginY := ZCoordinate - DeltaY;
    frmGoPhast.FrontGridChanged := True;
    Image32.Invalidate;
    frmGoPhast.AdjustScales;
    frmGoPhast.SynchronizeViews(vdFront);
  end;
end;

procedure SetSidePosition(const YCoordinate, ZCoordinate: real);
var
  DeltaX, DeltaY: double;
begin
  with frmGoPhast.frameSideView.ZoomBox do
  begin
    DeltaX := (X(Image32.Width) - X(0)) / 2;
    DeltaY := (Y(0) - Y(Image32.Height)) / 2;
    OriginX := ZCoordinate + DeltaX;
    OriginY := YCoordinate - DeltaY;
    frmGoPhast.SideGridChanged := True;
    Image32.Invalidate;
    frmGoPhast.AdjustScales;
    frmGoPhast.SynchronizeViews(vdSide);
  end;
end;

procedure MoveToTopCell(const Column, Row: integer);
var
  XCoordinate, YCoordinate: double;
  TopPoint: TPoint2D;
begin
  TopPoint := frmGoPhast.Grid.TwoDElementCenter(Column, Row);
  XCoordinate := TopPoint.X;
  YCoordinate := TopPoint.Y;
  SetTopPosition(XCoordinate, YCoordinate);
end;

procedure MoveToFrontCell(const Column, Layer: integer);
var
  XCoordinate, ZCoordinate: double;
  FrontPoint: T3DRealPoint;
begin
  FrontPoint := frmGoPhast.Grid.ThreeDElementCenter(Column,
    frmGoPhast.Grid.SelectedRow, Layer);
  XCoordinate := FrontPoint.X;
  ZCoordinate := FrontPoint.Z;
  SetFrontPosition(XCoordinate, ZCoordinate);
end;

procedure MoveToSideCell(const Row, Layer: integer);
var
  YCoordinate, ZCoordinate: double;
  SidePoint: T3DRealPoint;
begin
  SidePoint := frmGoPhast.Grid.ThreeDElementCenter(
    frmGoPhast.Grid.SelectedColumn, Row, Layer);
  YCoordinate := SidePoint.Y;
  ZCoordinate := SidePoint.Z;
  SetSidePosition(YCoordinate, ZCoordinate);
end;

{ TfrmGoTo }

procedure TfrmGoTo.GetData;
var
  APoint: TPoint2D;
  TopCell: T2DTopCell;
  FrontCell: T2DFrontCell;
  Index: Integer;
  AScreenObject: TScreenObject;
  Item: TListItem;
  TabVisible: boolean;
  AList: TList;
  Layer: integer;
  BitMapItem: TCompressedBitmapItem;
  procedure SetGridSpinEditValue(SE: TJvSpinEdit; NewValue: integer);
  begin
    if NewValue <= SE.MaxValue then
    begin
      SE.AsInteger := NewValue;
    end
    else
    begin
      SE.AsInteger := Round(SE.MaxValue);
    end;
  end;
begin
  Screen.Cursor := crHourGlass;
  try
    with frmGoPhast.frameTopView.ZoomBox do
    begin
      APoint.X := X(Image32.Width div 2);
      APoint.Y := Y(Image32.Height div 2);
      rdeX.Text := FloatToStr(APoint.X);
      rdeY.Text := FloatToStr(APoint.Y);
      APoint := frmGoPhast.Grid.
        RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
      rdeXPrime.Text := FloatToStr(APoint.X);
      rdeYPrime.Text := FloatToStr(APoint.Y);
    end;
    with frmGoPhast.frameFrontView.ZoomBox do
    begin
      rdeZ.Text := FloatToStr(Y(Image32.Height div 2));
    end;
    if (frmGoPhast.Grid.ColumnCount > 0)
      and (frmGoPhast.Grid.RowCount > 0)
      and (frmGoPhast.Grid.LayerCount > 0) then
    begin
      seCol.MaxValue := frmGoPhast.Grid.ColumnCount;
      seRow.MaxValue := frmGoPhast.Grid.RowCount;
      seLayer.MaxValue := frmGoPhast.Grid.LayerCount;
      seCol.MinValue := 1;
      seRow.MinValue := 1;
      seLayer.MinValue := 1;
  //    seColNew.MaxValue := frmGoPhast.Grid.ColumnCount;
  //    seColNew.MinValue := 1;
      TopCell := frmGoPhast.Grid.TopContainingCell(APoint, eaBlocks, False);
      SetGridSpinEditValue(seCol,TopCell.Col+1);
      SetGridSpinEditValue(seRow,TopCell.Row+1);
      with frmGoPhast.frameFrontView.ZoomBox do
      begin
        APoint.X := X(Image32.Width div 2);
        APoint.Y := Y(Image32.Height div 2);
      end;
      case frmGoPhast.ModelSelection of
        msUndefined: Assert(False);
        msPhast:
          begin
            FrontCell := frmGoPhast.PhastGrid.FrontContainingCell(APoint, eaBlocks);
            SetGridSpinEditValue(seLayer,FrontCell.Lay+1);
          end;
        msModflow:
          begin
            Layer := frmGoPhast.ModflowGrid.NearestLayerPosition(seCol.AsInteger-1,
              frmGoPhast.ModflowGrid.SelectedRow, APoint.Y);
            SetGridSpinEditValue(seLayer,Layer+1);
          end;
      end;
    end
    else
    begin
      tabCell.TabVisible := False;
    end;

    AList := TList.Create;
    try
      for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
        if not AScreenObject.Deleted then
        begin
          AList.Add(AScreenObject);
        end;
      end;

      TabVisible := AList.Count > 0;
      AList.Sort(ScreenObjectCompare);

      for Index := 0 to AList.Count - 1 do
      begin
        AScreenObject := AList[Index];
        Item := lvScreenObjects.Items.Add;
        Item.Caption := AScreenObject.Name;
        Item.Data := AScreenObject;
      end;
    finally
      AList.Free;
    end;
    tabObject.TabVisible := TabVisible;

    for Index := 0 to frmGoPhast.PhastModel.Bitmaps.Count - 1 do
    begin
      BitMapItem := frmGoPhast.PhastModel.Bitmaps.Items[Index]
        as TCompressedBitmapItem;
      if BitMapItem.Visible then
      begin
        Item := lvImages.Items.Add;
        Item.Caption := BitMapItem.Name;
        Item.Data := BitMapItem;
      end;
    end;
    tabImage.TabVisible := lvImages.Items.Count > 0;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmGoTo.SetData;
var
  XCoordinate, YCoordinate, ZCoordinate: double;
  Column, Row, Layer: integer;
  Item: TListItem;
  AScreenObject: TScreenObject;
  Undo: TUndoChangeSelection;
  UndoShowHide: TUndoShowHideScreenObject;
  BitMapItem: TCompressedBitmapItem;
begin
  Screen.Cursor := crHourGlass;
  try
    case pcMain.ActivePageIndex of
      0: // Position
        begin
          if cbTop.Checked then
          begin
            XCoordinate := StrToFloat(rdeX.Text);
            YCoordinate := StrToFloat(rdeY.Text);
            SetTopPosition(XCoordinate, YCoordinate);
          end;
          if cbFront.Checked then
          begin
            XCoordinate := StrToFloat(rdeXPrime.Text);
            ZCoordinate := StrToFloat(rdeZ.Text);
            SetFrontPosition(XCoordinate, ZCoordinate);
          end;
          if cbSide.Checked then
          begin
            YCoordinate := StrToFloat(rdeYPrime.Text);
            ZCoordinate := StrToFloat(rdeZ.Text);
            SetSidePosition(YCoordinate, ZCoordinate);
          end;

        end;
      1: //Cell
        begin
          Column := seCol.AsInteger - 1;
          Row := seRow.AsInteger - 1;
          Layer := seLayer.AsInteger - 1;
          if cbTop.Checked then
          begin
            MoveToTopCell(Column, Row);
          end;
          if cbFront.Checked then
          begin
            MoveToFrontCell(Column, Layer);
          end;
          if cbSide.Checked then
          begin
            MoveToSideCell(Row, Layer);
          end;

        end;
      2: // Object
        begin
          Item := lvScreenObjects.Selected;
          if Item <> nil then
          begin
            AScreenObject := Item.Data;
            if AScreenObject.Count > 0 then
            begin
              if cbSelectObject.Checked then
              begin
                Undo := TUndoChangeSelection.Create;

                frmGoPhast.ResetSelectedScreenObjects;
                AScreenObject.Selected := True;

                Undo.SetPostSelection;

                if Undo.SelectionChanged then
                begin
                  frmGoPhast.UndoStack.Submit(Undo);
                end
                else
                begin
                  Undo.Free;
                end;
              end
              else if not AScreenObject.Visible then
              begin
                UndoShowHide := TUndoShowHideScreenObject.Create;
                UndoShowHide.AddScreenObjectToChange(AScreenObject);
                frmGoPhast.UndoStack.Submit(UndoShowHide);
              end;

              XCoordinate := AScreenObject.Points[0].X;
              YCoordinate := AScreenObject.Points[0].Y;
              case AScreenObject.ViewDirection of
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
            end;
          end;
        end;
      3: // Object
        begin
          Item := lvImages.Selected;
          if Item <> nil then
          begin
            BitMapItem := Item.Data;
            MoveToImage(BitMapItem);
          end;
        end
    else
      Assert(False);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmGoTo.FormCreate(Sender: TObject);
begin
  inherited;
  pcMain.ActivePageIndex := 0;
  GetData;
end;

procedure TfrmGoTo.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmGoTo.cbClick(Sender: TObject);
begin
  inherited;
  rdeX.Enabled := cbTop.Checked;
  rdeY.Enabled := cbTop.Checked;
  rdeZ.Enabled := cbFront.Checked or cbSide.Checked;
  rdeXPrime.Enabled := cbFront.Checked;
  rdeYPrime.Enabled := cbSide.Checked;
  seCol.Enabled := cbTop.Checked or cbFront.Checked;
  seRow.Enabled := cbTop.Checked or cbSide.Checked;
  seLayer.Enabled := cbFront.Checked or cbSide.Checked;
end;

procedure TfrmGoTo.pcMainChange(Sender: TObject);
begin
  inherited;
  cbTop.Enabled := (pcMain.ActivePage <> tabObject)
    and (pcMain.ActivePage <> tabImage);
  cbFront.Enabled := cbTop.Enabled;
  cbSide.Enabled := cbTop.Enabled;
  HelpKeyWord := pcMain.ActivePage.HelpKeyword;
end;

procedure TfrmGoTo.FormShow(Sender: TObject);
begin
  inherited;
  tabCell.Visible := seCol.Enabled;
  case frmGoPhast.ModelSelection of
    msUndefined: Assert(False);
    msPhast: tabCell.Caption := 'Element';
    msModflow: tabCell.Caption := 'Block';
    else Assert(False);
  end;
end;

procedure MoveToImage(BitMapItem: TCompressedBitmapItem);
var
  X: double;
  Y: double;
  PointIndex: Integer;
  MeasPoint: TMeasurementPointItem;
begin
  if (BitMapItem = nil) or (BitMapItem.MeasurementPoints.Count = 0) then
  begin
    Exit;
  end;
  X := 0.0;
  Y := 0.0;
  for PointIndex := 0 to BitMapItem.MeasurementPoints.Count - 1 do
  begin
    MeasPoint := BitMapItem.MeasurementPoints.Items[PointIndex]
      as TMeasurementPointItem;
    X := X + MeasPoint.X;
    Y := Y + MeasPoint.Y;
  end;
  X := X/BitMapItem.MeasurementPoints.Count;
  Y := Y/BitMapItem.MeasurementPoints.Count;
  case BitMapItem.ViewDirection of
    vdTop: SetTopPosition(X,Y);
    vdFront: SetFrontPosition(X,Y);
    vdSide: SetSidePosition(Y,X);
    else Assert(False);
  end;
end;

end.
