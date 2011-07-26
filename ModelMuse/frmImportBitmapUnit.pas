{@abstract(The main purpose of @name is to define @link(TfrmImportBitmap)
  which is used to help import or edit bitmaps in GoPhast.)
  @name also defines @link(frmImportBitmap) which is used to
  communicate with @link(TfrmPixelPoint).}
unit frmImportBitmapUnit;

interface

uses
  Windows,
  GR32_Layers, // TPositionedLayer is declared in GR32_Layers.
  GR32, // TBitmap32, and TFloatRect are declared in GR32.
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Grids, RbwDataGrid4, 
  ExtCtrls, Buttons, CompressedImageUnit, ZoomBox2, {LinarBitmap,} Mask, JvExMask,
  JvSpin, pngimage;

type
  {@abstract(@name is used to help import or edit bitmaps in GoPhast.)
    See TfrmGoPhast.@link(TfrmGoPhast.miImportBitmapClick),
    TfrmGoPhast.@link(TfrmGoPhast.miEditBitmapsClick), and
    TfrmSelectImage.@link(TfrmSelectImage.btnOKClick).}
  TfrmImportBitmap = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes @classname without changing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    {@name is used to import ESRI *.wld files.
    See http://support.esri.com/index.cfm?fa=knowledgebase.techarticles.articleShow&d=16106

    The basic format is

    pixel-x,pixel-y real-world-x,real-world-y

    @unorderedList(
    @item(There is a space between each pair of coordinates.)
    @item(There is a comma (with no space) separating
    the X and X coordinates of each pair.)
    @item(There are two such points))
    }
    btnImportWorldFile: TButton;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TButton;
    // See @link(btnSelectImageClick).
    btnSelectImage: TButton;
    // @name: TCheckBox;
    // @name determines whether or not a bitmap will be visible or not.
    cbVisible: TCheckBox;
    // @name: TEdit;
    // @name holds the name for the bitmap.
    edName: TEdit;
    // @name: TLabel;
    // @name displays "Name".
    lblName: TLabel;
    // @name is used to select a .wld file.
    // @seealso(btnImportWorldFile).
    odWorldFiled: TOpenDialog;
    // @name: TOpenDialog;
    // @name is used to select the bitmap to import.
    OpenDialogBitmap: TOpenDialog;
    // @name: TPanel;
    // @name holds the buttons and other controls at the bottom of @classname.
    pnlBottom: TPanel;
    // @name: TRadioGroup;
    // @name determines whether the bitmap is to be displayed on the top,
    // front, or side view of the model.
    rgViewDirection: TRadioGroup;
    // @name: TSplitter;
    // @name is used to resize the relative areas of @link(dgPoints) and
    // @link(ZoomBox).
    Splitter1: TSplitter;
    dgPoints: TRbwDataGrid4;
    seNumRows: TJvSpinEdit;
    lblNumRows: TLabel;
    GridPanel1: TGridPanel;
    sbAddRow: TSpeedButton;
    sbInsertRow: TSpeedButton;
    sbDeleteRow: TSpeedButton;
    ScrollBox1: TScrollBox;
    ZoomBox: TQRbwZoomBox2;
    // @name imports a .wld file.  @seealso(btnImportWorldFile).
    procedure btnImportWorldFileClick(Sender: TObject);
    // Clicking @name causes the bitmap that is being dealt with to
    // either be imported into GoPhast or its properties to be edited.
    procedure btnOKClick(Sender: TObject);
    // @name uses @link(OpenDialogBitmap) to select a bitmap
    // which is then read into @link(FBitMap).
    procedure btnSelectImageClick(Sender: TObject);
    // @name causes @link(ZoomBox) to be redrawn and enables or disables
    // @link(btnOK).
    // See @link(DrawPointsOnBitMap32).
    procedure dgPointsExit(Sender: TObject);
    // @name causes @link(ZoomBox) to be redrawn and enables or disables
    // @link(btnOK).
    // See @link(DrawPointsOnBitMap32).
    procedure dgPointsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    // @name initializes @classname.
    procedure FormCreate(Sender: TObject); override;
    // @name destroys @link(FBitMap).
    procedure FormDestroy(Sender: TObject); override;
    // @name calls @link(LabelColumns).
    procedure rgViewDirectionClick(Sender: TObject);
    procedure ZoomBoxImage32MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure seNumRowsChange(Sender: TObject);
    procedure sbAddRowClick(Sender: TObject);
    procedure sbInsertRowClick(Sender: TObject);
    procedure sbDeleteRowClick(Sender: TObject);
    procedure dgPointsEndUpdate(Sender: TObject);
  private
    // @name is the bitmap that is being imported or edited.
    FBitMap: TBitmap;
    // @name: TCompressedBitmapItem;
    // @name is used to store @link(FBitMap) in GoPhast.
    FBitmapItem: TCompressedBitmapItem;
    FImageFileName: String;
    // @name enables or disables @link(btnOK) depending on whether or not
    // an image has been selected and
    // enough points have been specified.
    procedure EnableOKButton;
    // @name imports a .wld file. @seealso(btnImportWorldFile).
    procedure ImportWorldFile(const FileName: string);
    // @name labels the columns in @link(dgPoints) depending on the direction
    // from which the bitmap will be viewed.
    // See @link(rgViewDirection).
    procedure LabelColumns;
    // @name numbers the rows in @link(dgPoints).
    procedure NumberRows;
    // @name stores @link(FBitMap) in a new or existing @link(FBitmapItem)
    // along with the associated coordinates of pixels
    // and real-world coordinates.
    procedure SetData;
    procedure DrawPointsOnBitMap32(Sender: TObject; Buffer: TBitmap32);
    { Private declarations }
  public
    // @name adds a point to dgPoints.
    procedure AddPoint(const PixelX, PixelY: integer;
      const RealX, RealY: double);
    // @name is called when editing a bitmap to retrieve data from ABitmapItem.
    // @name is not called when importing a bitmap.
    procedure GetData(ABitmapItem: TCompressedBitmapItem);
    { Public declarations }
  end;

var
  // @name is the current instance of @link(TfrmImportBitmap).
  // @name is set in @link(TfrmImportBitmap.ZoomBoxImage32MouseUp) while
  // a @link(TfrmPixelPoint) is in use.
  frmImportBitmap: TfrmImportBitmap;

implementation

uses frmPixelPointUnit, GoPhastTypes, frmGoPhastUnit, BigCanvasMethods, 
  frmWorldFileTypeUnit, frmGoToUnit, jpeg, GraphicEx, Pcx;

{$R *.dfm}

type
  // @name identifies the columns in @link(TfrmImportBitmap.dgPoints
  // TfrmImportBitmap.dgPoints).
  TPixelColumns = (pcNone, pcPixelX, pcPixelY, pcRealWorldX, pcRealWorldY);

{ TfrmImportBitmap }

procedure TfrmImportBitmap.NumberRows;
var
  Row: integer;
begin
  for Row := 1 to dgPoints.RowCount - 1 do
  begin
    dgPoints.Cells[Ord(pcNone), Row] := IntToStr(Row);
  end;
end;

procedure TfrmImportBitmap.LabelColumns;
begin
  case TViewDirection(rgViewDirection.ItemIndex) of
    vdTop:
      begin
        dgPoints.Cells[Ord(pcPixelX), 0] := 'Pixel X';
        dgPoints.Cells[Ord(pcPixelY), 0] := 'Pixel Y';
        dgPoints.Cells[Ord(pcRealWorldX), 0] := 'X';
        dgPoints.Cells[Ord(pcRealWorldY), 0] := 'Y';
      end;
    vdFront:
      begin
        dgPoints.Cells[Ord(pcPixelX), 0] := 'Pixel X';
        dgPoints.Cells[Ord(pcPixelY), 0] := 'Pixel Z';
        dgPoints.Cells[Ord(pcRealWorldX), 0] := 'X';
        dgPoints.Cells[Ord(pcRealWorldY), 0] := 'Z';
      end;
    vdSide:
      begin
        dgPoints.Cells[Ord(pcPixelX), 0] := 'Pixel Z';
        dgPoints.Cells[Ord(pcPixelY), 0] := 'Pixel Y';
        dgPoints.Cells[Ord(pcRealWorldX), 0] := 'Z';
        dgPoints.Cells[Ord(pcRealWorldY), 0] := 'Y';
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmImportBitmap.FormCreate(Sender: TObject);
var
  ALayer: TPositionedLayer;
begin
  inherited;
  FImageFileName := '';
  dgPoints.ColWidths[Ord(pcNone)] := dgPoints.DefaultColWidth div 2;
  LabelColumns;
  NumberRows;

  ALayer := ZoomBox.Image32.Layers.Add(TPositionedLayer) as TPositionedLayer;
  ALayer.OnPaint := DrawPointsOnBitMap32;
end;

procedure TfrmImportBitmap.dgPointsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
var
  Index: Integer;
begin
  inherited;
  for Index := 1 to dgPoints.RowCount - 1 do
  begin
    dgPoints.Cells[0,Index] := IntToStr(Index);
  end;
  EnableOKButton;
  ZoomBox.Image32.Invalidate;
end;

procedure TfrmImportBitmap.dgPointsEndUpdate(Sender: TObject);
begin
  inherited;
  seNumRows.AsInteger := dgPoints.RowCount -1;
end;

procedure TfrmImportBitmap.dgPointsExit(Sender: TObject);
begin
  inherited;
  EnableOKButton;
  ZoomBox.Image32.Invalidate;
end;

procedure TfrmImportBitmap.EnableOKButton;
var
  ShouldEnable: boolean;
  Count: integer;
  Row: integer;
begin
  ShouldEnable := (FBitMap <> nil) and (dgPoints.RowCount >= 3);
  if ShouldEnable then
  begin
    Count := 0;
    for Row := 1 to dgPoints.RowCount -1 do
    begin
      if (Trim(dgPoints.Cells[Ord(pcPixelX),Row]) <> '')
        and (Trim(dgPoints.Cells[Ord(pcPixelY),Row]) <> '')
        and (Trim(dgPoints.Cells[Ord(pcRealWorldX),Row]) <> '')
        and (Trim(dgPoints.Cells[Ord(pcRealWorldY),Row]) <> '') then
      begin
        Inc(Count);
        ShouldEnable := Count >= 2;
        if ShouldEnable then
        begin
          break;
        end;
      end;
    end;
  end;
  btnOK.Enabled := ShouldEnable;
end;

procedure TfrmImportBitmap.btnSelectImageClick(Sender: TObject);
var
  WorldFile: string;
  Extension: string;
  MetaFile : TMetafile;
  WorldFileNames: TStringList;
  Index: Integer;
  jpegImage: TJPEGImage;
  png: TPngImage;
  TiffImage: TTIFFGraphic;
  procedure ShowError;
  begin
    FreeAndNil(FBitMap);
    Beep;
    MessageDlg('There was an error reading the file.  The file may be '
      + 'corrupt or there may be a bug in ModelMuse. Please contact '
      +  'rbwinst@usgs.gov for futher help with this problem.',
      mtError, [mbOK], 0);
  end;
begin
  inherited;
  if OpenDialogBitmap.Execute then
  begin
    FBitMap.Free;
    FBitMap := TBitmap.Create;

    FImageFileName := OpenDialogBitmap.FileName;
    Extension := ExtractFileExt(FImageFileName);
    if (CompareText(Extension, '.wmf') = 0)
      or (CompareText(Extension, '.emf') = 0) then
    begin
{ TODO : It would be better to use the metafile directly
instead of converting it to a bitmap so
there is no loss in resolution at higher magnifications. }
      MetaFile := TMetafile.Create;
      try
        try
          MetaFile.LoadFromFile(OpenDialogBitmap.FileName);
        except on E: Exception do
          begin
            ShowError;
            Exit;
          end;
        end;
        FBitMap.Height := Metafile.Height;
        FBitMap.Width := Metafile.Width;
        FBitMap.Canvas.Draw(0, 0, MetaFile) ;
      finally
        MetaFile.Free;
      end;
    end
    else if (CompareText(Extension, '.bmp') = 0) then
    begin
      FBitMap.LoadFromFile(FImageFileName);
    end
    else if (CompareText(Extension, '.jpg') = 0)
      or (CompareText(Extension, '.jpeg') = 0) then
    begin
      jpegImage := TJPEGImage.Create;
      try
        jpegImage.LoadFromFile(FImageFileName);
        FBitMap.Assign(jpegImage);
      finally
        jpegImage.Free
      end;
    end
    else if (CompareText(Extension, '.png') = 0) then
    begin
      png := TPngImage.Create;
      try
        png.LoadFromFile(FImageFileName);
        FBitMap.Assign(png);
      finally
        png.Free;
      end;
    end
    else if (CompareText(Extension, '.pcx') = 0) then
    begin
      LoadFromFileX(FImageFileName, FBitMap);
    end
    else if (CompareText(Extension, '.tif') = 0)
      or (CompareText(Extension, '.tiff') = 0) then
    begin
      TiffImage := TTIFFGraphic.Create;
      try
        TiffImage.LoadFromFile(FImageFileName);
        FBitMap.Assign(TiffImage);
      finally
        TiffImage.Free;
      end;
    end
    else
    begin
      Assert(False);
//      with TLinearBitmap.Create do
//      try
//        try
//          LoadFromFile(FImageFileName);
//        except on E: Exception do
//          begin
//            ShowError;
//            Exit;
//          end;
//        end;
//        AssignTo(FBitMap);
//      finally
//        Free;
//      end;
    end;

    ZoomBox.Image32.Bitmap.Assign(FBitMap);
    ZoomBox.Image32.Invalidate;
    ZoomBox.Width := FBitMap.Width;
    ZoomBox.Height := FBitMap.Height;

    if edName.Text = '' then
    begin
      edName.Text := ExtractFileName(OpenDialogBitmap.FileName);
    end;
    WorldFileNames := TStringList.Create;
    try
      WorldFileNames.Add(ChangeFileExt(OpenDialogBitmap.FileName, '.jgw'));
      WorldFileNames.Add(ChangeFileExt(OpenDialogBitmap.FileName, '.jpgw'));
      WorldFileNames.Add(ChangeFileExt(OpenDialogBitmap.FileName, '.tfw'));
      WorldFileNames.Add(ChangeFileExt(OpenDialogBitmap.FileName, '.tifw'));
      WorldFileNames.Add(ChangeFileExt(OpenDialogBitmap.FileName, '.wld'));
      for Index := 0 to WorldFileNames.Count - 1 do
      begin
        WorldFile := WorldFileNames[Index];
        if FileExists(WorldFile) then
        begin
          ImportWorldFile(WorldFile);
          break;
        end
      end;
    finally
      WorldFileNames.Free;
    end;

    EnableOKButton;
    if not btnOK.Enabled then
    begin
      MessageDlg('Click on the image and assign real-world coordinates to the '
        + 'points you clicked or fill in the information in the table.',
        mtInformation, [mbOK], 0);
    end;
  end;
end;

procedure TfrmImportBitmap.FormDestroy(Sender: TObject);
begin
  inherited;
  FBitMap.Free;
end;

procedure TfrmImportBitmap.GetData(ABitmapItem: TCompressedBitmapItem);
var
  Index: integer;
  MeasurementPoint: TMeasurementPointItem;
begin
  Caption := 'Edit BitMap';
  Assert(ABitmapItem <> nil);
  FBitmapItem := ABitmapItem;
  FBitMap.Free;
  FBitMap := TBitmap.Create;
  FBitMap.Assign(FBitmapItem.BitMap);
  ZoomBox.Image32.Bitmap.Assign(FBitMap);
  ZoomBox.Width := FBitMap.Width;
  ZoomBox.Height := FBitMap.Height;

  rgViewDirection.ItemIndex := Ord(FBitmapItem.ViewDirection);

  edName.Text := FBitmapItem.Name;
  dgPoints.RowCount := ABitmapItem.MeasurementPoints.Count + 1;
  for Index := 0 to ABitmapItem.MeasurementPoints.Count - 1 do
  begin
    MeasurementPoint := ABitmapItem.MeasurementPoints.Items[Index]
      as TMeasurementPointItem;
    dgPoints.Cells[Ord(pcPixelX), Index + 1] := IntToStr(MeasurementPoint.PixelX);
    dgPoints.Cells[Ord(pcPixelY), Index + 1] := IntToStr(MeasurementPoint.PixelY);
    dgPoints.Cells[Ord(pcRealWorldX), Index + 1] := FloatToStr(MeasurementPoint.X);
    dgPoints.Cells[Ord(pcRealWorldY), Index + 1] := FloatToStr(MeasurementPoint.Y);
  end;
  cbVisible.Checked := ABitmapItem.Visible;
  btnOK.Enabled := True;
end;

procedure TfrmImportBitmap.SetData;
var
  Index: integer;
  MeasurementPoint: TMeasurementPointItem;
  XPixel, YPixel: integer;
  X, Y: double;
  NewItem: boolean;
begin
  if FImageFileName <> '' then
  begin
    frmGoPhast.PhastModel.AddFileToArchive(FImageFileName);
  end;
  NewItem := False;
  if FBitmapItem = nil then
  begin
    FBitmapItem := frmGoPhast.PhastModel.Bitmaps.Add as TCompressedBitmapItem;
    NewItem := True;
  end;
  FBitmapItem.ViewDirection := TViewDirection(rgViewDirection.ItemIndex);
  FBitmapItem.BitMap.Assign(FBitMap);

  FBitmapItem.Name := edName.Text;
  FBitmapItem.Visible := cbVisible.Checked;

  FBitmapItem.MeasurementPoints.Clear;
  for Index := 1 to dgPoints.RowCount - 1 do
  begin
    if (dgPoints.Cells[Ord(pcPixelX), Index] = '')
      or (dgPoints.Cells[Ord(pcPixelY), Index] = '')
      or (dgPoints.Cells[Ord(pcRealWorldX), Index] = '')
      or (dgPoints.Cells[Ord(pcRealWorldY), Index] = '') then
    begin
      Continue;
    end;
    // initialize variables to prevent a compiler warning.
    XPixel := 0;
    YPixel := 0;
    X := 0;
    Y := 0;

    if not TryStrToInt(dgPoints.Cells[Ord(pcPixelX), Index], XPixel) then
    begin
      Continue;
    end;
    if not TryStrToInt(dgPoints.Cells[Ord(pcPixelY), Index], YPixel) then
    begin
      Continue;
    end;

    if not TryStrToFloat(dgPoints.Cells[Ord(pcRealWorldX), Index], X) then
    begin
      Continue;
    end;
    if not TryStrToFloat(dgPoints.Cells[Ord(pcRealWorldY), Index], Y) then
    begin
      Continue;
    end;

    MeasurementPoint := FBitmapItem.MeasurementPoints.Add
      as TMeasurementPointItem;
    MeasurementPoint.PixelX := XPixel;
    MeasurementPoint.PixelY := YPixel;
    MeasurementPoint.X := X;
    MeasurementPoint.Y := Y;
  end;
  case FBitmapItem.ViewDirection of
    vdTop:
      begin
        frmGoPhast.TopScreenObjectsChanged := True;
      end;
    vdFront:
      begin
        frmGoPhast.FrontScreenObjectsChanged := True;
      end;
    vdSide:
      begin
        frmGoPhast.SideScreenObjectsChanged := True;
      end;
  else
    Assert(False);
  end;
  if NewItem then
  begin
    MoveToImage(FBitmapItem);
  end;
  //frmGoPhast.Invalidate;
end;

procedure TfrmImportBitmap.sbAddRowClick(Sender: TObject);
begin
  inherited;
  seNumRows.AsInteger := seNumRows.AsInteger +1;
  seNumRows.OnChange(seNumRows);
end;

procedure TfrmImportBitmap.sbDeleteRowClick(Sender: TObject);
begin
  inherited;
  if (dgPoints.Row >= dgPoints.FixedRows)
    and (dgPoints.RowCount > 2) then
  begin
    dgPoints.DeleteRow(dgPoints.Row);
    seNumRows.AsInteger := dgPoints.RowCount-1;
    NumberRows;
  end;
end;

procedure TfrmImportBitmap.sbInsertRowClick(Sender: TObject);
begin
  inherited;
  if (dgPoints.Row >= dgPoints.FixedRows)
    and (dgPoints.Row < dgPoints.RowCount) then
  begin
    dgPoints.InsertRow(dgPoints.Row);
    seNumRows.AsInteger := dgPoints.RowCount-1;
    NumberRows;
  end;
end;

procedure TfrmImportBitmap.seNumRowsChange(Sender: TObject);
begin
  inherited;
  dgPoints.RowCount := seNumRows.AsInteger + 1;
  NumberRows;
end;

procedure TfrmImportBitmap.ZoomBoxImage32MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  frmPixelPoint: TfrmPixelPoint;
begin
  inherited;
  frmImportBitmap := self;
  try
    Application.CreateForm(TfrmPixelPoint, frmPixelPoint);
    try
      frmPixelPoint.PopupParent := self;
      frmPixelPoint.GetData(TViewDirection(rgViewDirection.ItemIndex), X, Y);
      if frmPixelPoint.ShowModal = mrOK then
      begin
        ZoomBox.Image32.Invalidate;
      end;
    finally
      frmPixelPoint.Free;
    end;
  finally
    frmImportBitmap := nil;
  end;
end;

procedure TfrmImportBitmap.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmImportBitmap.rgViewDirectionClick(Sender: TObject);
begin
  inherited;
  LabelColumns;
end;

Type
  TWorldFileType = (wftCAD, sftRaster);

procedure TfrmImportBitmap.ImportWorldFile(const FileName: string);
  function ExtractWord(Var AString: string): string;
  const
    Space = ' ';
    Comma = ',';
  var
    SpacePosition: integer;
    CommaPosition: integer;
  begin
    try
    SpacePosition := Pos(Space,AString);
    CommaPosition := Pos(Comma,AString);
    if (SpacePosition > 0) and (CommaPosition > 0) then
    begin
      if SpacePosition < CommaPosition then
      begin
        Result := Copy(AString, 1, SpacePosition-1);
        AString := Copy(AString, SpacePosition+1, MAXINT);
      end
      else
      begin
        Result := Copy(AString, 1, CommaPosition-1);
        AString := Copy(AString, CommaPosition+1, MAXINT);
      end;
    end
    else if (SpacePosition > 0) then
    begin
      Result := Copy(AString, 1, SpacePosition-1);
      AString := Copy(AString, SpacePosition+1, MAXINT);
    end
    else if (CommaPosition > 0) then
    begin
      Result := Copy(AString, 1, CommaPosition-1);
      AString := Copy(AString, CommaPosition+1, MAXINT);
    end
    else
    begin
      Result := AString;
      AString := '';
    end;
    finally
      result := Trim(Result);
    end;
  end;
var
  WorldFile: TStringList;
  LineIndex: integer;
  Line: string;
  PixelX: integer;
  PixelY: integer;
  RealWorldX: double;
  RealWorldY: double;
  Extension: string;
  FileType: TWorldFileType;
  A, B, C, D, E, F: double;
begin
  inherited;
  Extension := LowerCase(ExtractFileExt(FileName));
  if Extension = '.wld' then
  begin
    FileType := wftCAD;
  end
  else if (Extension = '.tfw')
    or (Extension = '.tifw')
    or (Extension = '.jgw')
    or (Extension = '.jpgw') then
  begin
    FileType := sftRaster;
  end
  else
  begin
    with TfrmWorldFileType.Create(nil) do
    begin
      try
        ShowModal;
        if rbRaster.Checked then
        begin
          FileType := sftRaster;
        end
        else if rbCAD.Checked then
        begin
          FileType := wftCAD;
        end
        else
        begin
          Exit;
        end;
      finally
        Free;
      end;
    end;
  end;
  WorldFile := TStringList.Create;
  try
    WorldFile.LoadFromFile(FileName);
    case FileType of
      wftCAD:
        begin
          dgPoints.BeginUpdate;
          try
            for LineIndex := 0 to WorldFile.Count -1 do
            begin
              Line := WorldFile[LineIndex];
              if Line <> '' then
              begin
                PixelX := Round(StrToFloat(ExtractWord(Line)));
                PixelY := Round(StrToFloat(ExtractWord(Line)));
                RealWorldX := StrToFloat(ExtractWord(Line));
                RealWorldY := StrToFloat(ExtractWord(Line));
                AddPoint(PixelX, PixelY, RealWorldX, RealWorldY);
              end;
            end;
          finally
            dgPoints.EndUpdate;
          end;
        end;
      sftRaster:
        begin
          Assert(WorldFile.Count >= 6);
          A := StrToFloat(Trim(WorldFile[0]));
          D := StrToFloat(Trim(WorldFile[1]));
          B := StrToFloat(Trim(WorldFile[2]));
          E := StrToFloat(Trim(WorldFile[3]));
          C := StrToFloat(Trim(WorldFile[4]));
          F := StrToFloat(Trim(WorldFile[5]));
          PixelX := 0;
          PixelY := 0;
          RealWorldX := A*PixelX + B*PixelY + C;
          RealWorldY := D*PixelX + E*PixelY + F;
          dgPoints.BeginUpdate;
          try
            AddPoint(PixelX, PixelY, RealWorldX, RealWorldY);
            if FBitMap <> nil then
            begin
              PixelX := FBitMap.Width-1;
              PixelY := FBitMap.Height-1;
              RealWorldX := A*PixelX + B*PixelY + C;
              RealWorldY := D*PixelX + E*PixelY + F;
              AddPoint(PixelX, PixelY, RealWorldX, RealWorldY);
            end;
          finally
            dgPoints.EndUpdate
          end;
        end;
      else Assert(False);
    end;
  finally
    WorldFile.Free;
  end;
end;

procedure TfrmImportBitmap.btnImportWorldFileClick(Sender: TObject);
begin
  inherited;
  if odWorldFiled.Execute then
  begin
    ImportWorldFile(odWorldFiled.FileName);
  end;
end;

procedure TfrmImportBitmap.AddPoint(const PixelX, PixelY: integer;
  const RealX, RealY: double);
var
  Row: integer;
begin
  if (dgPoints.Cells[Ord(pcPixelX), 1] = '')
    or (dgPoints.Cells[Ord(pcPixelY), 1] = '')
    or (dgPoints.Cells[Ord(pcRealWorldX), 1] = '')
    or (dgPoints.Cells[Ord(pcRealWorldY), 1] = '') then
  begin
    Row := 1;
  end
  else
  begin
    Row := dgPoints.RowCount;
    dgPoints.RowCount := Row + 1;
  end;
  dgPoints.Cells[Ord(pcPixelX), Row] := IntToStr(PixelX);
  dgPoints.Cells[Ord(pcPixelY), Row] := IntToStr(PixelY);
  dgPoints.Cells[Ord(pcRealWorldX), Row] := FloatToStr(RealX);
  dgPoints.Cells[Ord(pcRealWorldY), Row] := FloatToStr(RealY);
  NumberRows;
  EnableOKButton;
  ZoomBox.Image32.Invalidate;
end;

procedure TfrmImportBitmap.DrawPointsOnBitMap32(Sender: TObject;
  Buffer: TBitmap32);
var
  Row: integer;
  X, Y: integer;
begin
  Buffer.BeginUpdate;
  try
    for Row := 1 to dgPoints.RowCount - 1 do
    begin
      if (dgPoints.Cells[Ord(pcPixelX), Row] <> '')
        and (dgPoints.Cells[Ord(pcPixelY), Row] <> '') then
      begin
        try
          X := StrToInt(dgPoints.Cells[Ord(pcPixelX), Row]);
          Y := StrToInt(dgPoints.Cells[Ord(pcPixelY), Row]);
          DrawBigRectangle32(Buffer, clBlack32, clBlack32, 0,
            X - 3, Y - 3, X + 3, Y + 3);
        except on EConvertError do
          begin
            Continue;
          end
        end;
      end;
    end;
  finally
    Buffer.EndUpdate
  end;
end;

end.

