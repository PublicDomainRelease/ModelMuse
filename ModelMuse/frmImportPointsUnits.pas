{@name defines @link(TfrmImportPoints) which is used to import scattered point
 data into GoPhast.}
unit frmImportPointsUnits;

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, ExtCtrls, CheckLst, Grids,
  RbwDataGrid4, Buttons, ComCtrls, frmImportShapefileUnit, Spin, JvExControls,
  JvxCheckListBox, Mask, JvExMask, JvSpin;  

type
  {@abstract(@name is the command used to import
    points or reverse the import.)}
  TUndoImportPoints = class(TUndoImportShapefile)
  protected
    // @name describes what @classname does.
    function Description: string; override;
  end;

  {@name imports scattered point data into GoPhast.
  @unOrderedList(
  @Item(OnCreate = @link(FormCreate).)
  @Item(OnKeyUp = @link(FormKeyUp).)
  )
  }
  TfrmImportPoints = class(TfrmCustomGoPhast)
    // The Cancel button.
    btnCancel: TBitBtn;
    // @name is the Help button.
    btnHelp: TBitBtn;
    // The OK button.
    btnOK: TBitBtn;
    // @name is the button the user uses to open
    // a file that contains the point data.
    btnOpenFile: TBitBtn;
    // @name specifies whether the imported data points will set values
    //  of data sets by interpolation.
    cbInterpolation: TCheckBox;
    { TODO : Change name to remove "cell". }
    // @name specifies whether the imported data points will set
    // values of data sets at intersected cells.
    cbIntersectedCells: TCheckBox;
    // @name specifies a "root" that is used to create names for the imported
    // objects.
    edRoot: TEdit;
    // @name labels @link(edRoot).
    lblRoot: TLabel;
    // @name is the label for @link(seRows).
    lblRows: TLabel;
    // @name is used to select a file that contains the point data.
    OpenDialogImportFile: TOpenDialog;
    // The TPageControl that contains @link(tabControls) and @link(tabData).
    pcImportPoints: TPageControl;
    // @name is the bottom panel that holds the OK (@link(btnOK))
    // and Cancel (@link(btnCancel)) buttons.
    pnlBottom: TPanel;
    // @name is the panel at the bottom of @link(tabData) that holds
    // @link(seRows) and @link(btnOpenFile).
    pnlDataTabControls: TPanel;
    // The caption of @name is used to label @link(jvclbDataSets).
    pnlLabelDataSets: TPanel;
    // @name is the panel that holds @link(rgEvaluatedAt)
    // and @link(rgViewDirection).
    pnlRadioGroups: TPanel;
    // @name specifies how many elevation formulas will be used with each
    // imported point object.
    rgElevationCount: TRadioGroup;
    { @name indicates whether the points are to be evaluated
     at nodes or elements.
    @unOrderedList(
    @Item(OnClick = @link(rgEvaluatedAtClick).)
    )
    }
    rgEvaluatedAt: TRadioGroup;
    { @name indicates the direction from which the data will be viewed.
    @unOrderedList(
    @Item(OnClick = @link(rgViewDirectionClick).)
    )
    }
    rgViewDirection: TRadioGroup;
    // @name holds the controls used to specify properties of the imported
    // objects.
    tabControls: TTabSheet;
    // @name holds @link(dgData) and @link(pnlDataTabControls).
    tabData: TTabSheet;
    dgData: TRbwDataGrid4;
    { Specifies the data sets to be used and the coordinate directions.
    @unOrderedList(
    @Item(OnClickCheck = @link(jvclbDataSetsClickCheck).)
    )
    }
    jvclbDataSets: TJvxCheckListBox;
    cbImportAsSingleObject: TCheckBox;
    cbVisible: TCheckBox;
    seRows: TJvSpinEdit;
    // @name makes sure that at least one of the following checkboxes is
    // checked: @link(cbIntersectedCells), and
    // @link(cbInterpolation).  If not, their fonts are changed to emphasize
    // them and @link(btnOK) is disabled.
    procedure cbIntersectedCellsClick(Sender: TObject);
    // @name updates the columns in @link(dgData)
    procedure jvclbDataSetsClickCheck(Sender: TObject);
    // @name initializes the @classname.
    procedure FormCreate(Sender: TObject); override;
    // @name spreads the data in the keyboard into the cells of @link(dgData).
    // KeyPreview has to be @True for this to work.
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    // @name calls @link(UpdateDataSets);
    procedure rgEvaluatedAtClick(Sender: TObject);
    // @name calls @link(UpdateDataSets);
    procedure rgViewDirectionClick(Sender: TObject);
    // @name is used to prevent the columns specifying coordinates
    // from being moved.
    procedure dgDataColMoving(Sender: TObject; const Origin,
      Destination: Integer; var CanMove: Boolean);
    // @name calls @link(UpdateDimensionColumns) and @link(UpdateDataSets).
    procedure rgElevationCountClick(Sender: TObject);
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name reads data from a file into @link(dgData).
    procedure btnOpenFileClick(Sender: TObject);
    procedure seRowsChange(Sender: TObject);
    procedure dgDataSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure dgDataDistributeTextProgress(Sender: TObject; Position,
      Max: Integer);
  private
    // @name is the column that specifies the X coordinate.
    XCol: integer;
    // @name is the column that specifies the Y coordinate.
    YCol: integer;
    // @name is the column that specifies the Z coordinate.
    ZCol: integer;
    // @name is the column that specifies the first X formula.
    X1Col: integer;
    // @name is the column that specifies the first Y formula.
    Y1Col: integer;
    // @name is the column that specifies the first Z formula.
    Z1Col: integer;
    // @name is the column that specifies the second X formula.
    X2Col: integer;
    // @name is the column that specifies the second Y formula.
    Y2Col: integer;
    // @name is the column that specifies the second Z formula.
    Z2Col: integer;
    FImportFileName: string;
    StartTime: TDateTime;
    // @name updates the contents of @link(jvclbDataSets).
    procedure UpdateDataSets;
    // @name updates the column captions for the columns that specify
    // dimensions in @link(dgData).
    procedure UpdateDimensionColumns;
    // @name imports the data into GoPhast.
    procedure SetData;
    { Set the captions of @link(cbIntersectedCells)
      and @link(cbInterpolation) based on @link(rgEvaluatedAt).ItemIndex.}
    procedure SetCheckBoxCaptions;
    procedure EnableOkButton;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses Clipbrd, Contnrs, GoPhastTypes, frmGoPhastUnit, DataSetUnit, RbwParser,
  frmProgressUnit, UndoItems, ScreenObjectUnit, FastGEO, GIS_Functions,
  ValueArrayStorageUnit, PhastModelUnit;

{$R *.dfm}

resourcestring
  rsX = 'X';
  rsY = 'Y';
  rsZ = 'Z';
  rsX1 = 'X1';
  rsY1 = 'Y1';
  rsZ1 = 'Z1';
  rsX2 = 'X2';
  rsY2 = 'Y2';
  rsZ2 = 'Z2';

procedure TfrmImportPoints.seRowsChange(Sender: TObject);
begin
  inherited;
  dgData.RowCount := seRows.AsInteger + 1;
end;

procedure TfrmImportPoints.FormCreate(Sender: TObject);
begin
  inherited;
  rgEvaluatedAt.Items[Ord(eaBlocks)] := EvalAtToString(eaBlocks,
    frmGoPhast.PhastModel.ModelSelection, True, True);
  rgEvaluatedAt.Items[Ord(eaNodes)] := EvalAtToString(eaNodes,
    frmGoPhast.PhastModel.ModelSelection, True, True);
  rgEvaluatedAt.Enabled := frmGoPhast.PhastModel.ModelSelection = msPhast;
  if not rgEvaluatedAt.Enabled then
  begin
    rgEvaluatedAt.ItemIndex := 0;
  end;

  FImportFileName := '';
  cbIntersectedCellsClick(nil);
  SetCheckBoxCaptions;
  pcImportPoints.ActivePageIndex := 0;
  UpdateDimensionColumns;
  UpdateDataSets;
end;

procedure TfrmImportPoints.UpdateDataSets;
var
  Index: integer;
  EvalAt: TEvaluatedAt;
  DataSet: TDataArray;
  ViewDirection: TViewDirection;
  ShouldIncludeDataSet: boolean;
  DataArrayManager: TDataArrayManager;
begin
  jvclbDataSets.Items.Clear;
  jvclbDataSets.Color := clRed;
  EvalAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
  ViewDirection := TViewDirection(rgViewDirection.ItemIndex);
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  for Index := 0 to DataArrayManager.DataSetCount -1 do
  begin
    DataSet := DataArrayManager.DataSets[Index];
    if DataSet.EvaluatedAt = EvalAt then
    begin
      ShouldIncludeDataSet := false;
      case DataSet.Orientation of
        dsoTop:
          begin
            ShouldIncludeDataSet := ViewDirection = vdTop;
          end;
        dsoFront:
          begin
            ShouldIncludeDataSet := ViewDirection = vdFront;
          end;
        dsoSide:
          begin
            ShouldIncludeDataSet := ViewDirection = vdSide;
          end;
        dso3D:
          begin
            ShouldIncludeDataSet := rgElevationCount.ItemIndex > 0;
          end;
      else
        Assert(False);
      end;

      if ShouldIncludeDataSet then
      begin
        jvclbDataSets.Items.AddObject(DataSet.Name, DataSet);
      end;
    end;
  end;
  for Index := dgData.ColCount -1 downto rgElevationCount.ItemIndex + 2 do
  begin
    dgData.DeleteColumn(Index);
  end;
end;

procedure TfrmImportPoints.SetCheckBoxCaptions;
var
  NodeElemString: string;
begin
  NodeElemString := EvalAtToString(TEvaluatedAt(rgEvaluatedAt.ItemIndex),
       frmGoPhast.ModelSelection, True, False);
  cbIntersectedCells.Caption := rsSetValueOfIntersected + NodeElemString;
  cbInterpolation.Caption := rsSetValueOf + NodeElemString + rsByInterpolation;
//  case rgEvaluatedAt.ItemIndex of
//    0: // elements
//      begin
//        cbIntersectedCells.Caption := rsSetValueOfIntersectedElements;
//        cbInterpolation.Caption := rsSetValueOfElementsByInterpolation;
//      end;
//    1: // cells
//      begin
//        cbIntersectedCells.Caption := rsSetValueOfIntersectedNodes;
//        cbInterpolation.Caption := rsSetValueOfNodesByInterpolation;
//      end;
//  else
//    Assert(False);
//  end;
end;

procedure TfrmImportPoints.EnableOkButton;
var
  RowIndex: Integer;
begin
  if cbIntersectedCells.Checked or cbInterpolation.Checked then
  begin
    btnOK.Enabled := False;
    for RowIndex := 1 to dgData.RowCount - 1 do
    begin
      if (dgData.Cells[0,RowIndex] <> '')
        and (dgData.Cells[1,RowIndex] <> '') then
      begin
        btnOK.Enabled := True;
        break;
      end;
    end;
  end
  else
  begin
    btnOK.Enabled := False;
  end;
end;

procedure TfrmImportPoints.rgEvaluatedAtClick(Sender: TObject);
begin
  inherited;
  SetCheckBoxCaptions;
  UpdateDataSets;
end;

procedure TfrmImportPoints.jvclbDataSetsClickCheck(Sender: TObject);
var
  Captions: TStringList;
  Index: integer;
  ACaption: string;
  Position: Integer;
  DataSet: TDataArray;
  ItemChecked: boolean;
begin
  inherited;
  ItemChecked := False;
  Captions := TStringList.Create;
  try
    Captions.AddStrings(dgData.Rows[0]);
    for Index := jvclbDataSets.Items.Count -1 downto 0 do
    begin
      ACaption := jvclbDataSets.Items[Index];
      Position := Captions.IndexOf(ACaption);
      if jvclbDataSets.Checked[Index] then
      begin
        ItemChecked := True;
        if Position < 0 then
        begin
          dgData.ColCount := dgData.ColCount + 1;
          dgData.Columns[dgData.ColCount-1].AutoAdjustColWidths := True;
          dgData.Cells[dgData.ColCount-1, 0] := jvclbDataSets.Items[Index];
          DataSet := jvclbDataSets.Items.Objects[Index] as TDataArray;
          dgData.Objects[dgData.ColCount-1, 0] := DataSet;
          if DataSet = nil then
          begin
            dgData.Columns[dgData.ColCount-1].Format := rcf4Real;
          end
          else
          begin
            case DataSet.DataType of
              rdtDouble:
                begin
                  dgData.Columns[dgData.ColCount-1].Format := rcf4Real;
                end;
              rdtInteger:
                begin
                  dgData.Columns[dgData.ColCount-1].Format := rcf4Integer;
                end;
              rdtBoolean:
                begin
                  dgData.Columns[dgData.ColCount-1].Format := rcf4Boolean;
                end;
              rdtString:
                begin
                  dgData.Columns[dgData.ColCount-1].Format := rcf4String;
                end;
            else
              Assert(False);
            end;
          end;

        end;
      end
      else
      begin
        if Position >= 0 then
        begin
          dgData.DeleteColumn(Position);
        end;
      end;
    end;
  finally
    Captions.Free;
  end;
  if ItemChecked then
  begin
    jvclbDataSets.Color := clWindow;
  end
  else
  begin
    jvclbDataSets.Color := clRed;
  end;
end;

procedure TfrmImportPoints.rgViewDirectionClick(Sender: TObject);
begin
  inherited;
  case rgViewDirection.ItemIndex of
    0: rgElevationCount.Caption := 'Number of Z formulas';
    1: rgElevationCount.Caption := 'Number of Y formulas';
    2: rgElevationCount.Caption := 'Number of X formulas';
    else Assert(False);
  end;


  UpdateDimensionColumns;
  UpdateDataSets;
end;

procedure TfrmImportPoints.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Lines: TStringList;
  RequiredRows: integer;
begin
  inherited;
  If (Key = 86) and (Shift = [ssCtrl])
    and (ActiveControl is TRbwInplaceEdit4) then
  begin
    Lines := TStringList.Create;
    try
      Lines.Text := Clipboard.AsText;
      RequiredRows := dgData.Row + Lines.Count;
      if RequiredRows > dgData.RowCount then
      begin
        seRows.Value := RequiredRows -1;
      end;
      dgData.DistributeText(dgData.Col, dgData.Row, Clipboard.AsText)
    finally
      Lines.Free;
    end;
  end;
end;

procedure TfrmImportPoints.UpdateDimensionColumns;
var
  Col: integer;
  PriorCol: integer;
  ColIndex: integer;
  procedure UpdateColumn(const ColCaption: string;
    const ShouldBePresent: boolean; out SavedCol: integer);
  begin
    if ShouldBePresent then
    begin
      Col := PriorCol + 1;
      dgData.Cells[Col, 0] := ColCaption;
      PriorCol := Col;
      SavedCol := Col;
    end
    else
    begin
      SavedCol := -1;
    end;
  end;
begin
  Col := 0;
  PriorCol := -1;

  dgData.ColCount := rgElevationCount.ItemIndex + 2;
  for ColIndex := 0 to dgData.ColCount -1 do
  begin
    dgData.Columns[ColIndex].Format := rcf4Real;
    dgData.Columns[ColIndex].AutoAdjustColWidths := True;
  end;

  UpdateColumn(rsX, (rgViewDirection.ItemIndex in [0,1])
    or (rgElevationCount.ItemIndex = 1), XCol);
  UpdateColumn(rsX1, (rgViewDirection.ItemIndex = 2)
    and (rgElevationCount.ItemIndex = 2), X1Col);
  UpdateColumn(rsX2, (rgViewDirection.ItemIndex = 2)
    and (rgElevationCount.ItemIndex = 2), X2Col);

  UpdateColumn(rsY, (rgViewDirection.ItemIndex in [0,2])
    or (rgElevationCount.ItemIndex = 1), YCol);
  UpdateColumn(rsY1, (rgViewDirection.ItemIndex = 1)
    and (rgElevationCount.ItemIndex = 2), Y1Col);
  UpdateColumn(rsY2, (rgViewDirection.ItemIndex = 1)
    and (rgElevationCount.ItemIndex = 2), Y2Col);

  UpdateColumn(rsZ, (rgViewDirection.ItemIndex in [1,2])
    or (rgElevationCount.ItemIndex = 1), ZCol);
  UpdateColumn(rsZ1, (rgViewDirection.ItemIndex = 0)
    and (rgElevationCount.ItemIndex = 2), Z1Col);
  UpdateColumn(rsZ2, (rgViewDirection.ItemIndex = 0)
    and (rgElevationCount.ItemIndex = 2), Z2Col);
end;

procedure TfrmImportPoints.dgDataColMoving(Sender: TObject; const Origin,
  Destination: Integer; var CanMove: Boolean);
var
  RequiredColumns: integer;
begin
  inherited;
  RequiredColumns := rgElevationCount.ItemIndex + 2;
  CanMove := (Origin >= RequiredColumns) and (Destination >= RequiredColumns);
end;

procedure TfrmImportPoints.dgDataDistributeTextProgress(Sender: TObject;
  Position, Max: Integer);
const
  OneSecond = 1/24/3600;
begin
  inherited;

  if Now - StartTime > OneSecond then
  begin
    if not frmProgressMM.Visible then
    begin
      frmProgressMM.Caption := 'Reading Data';
    end;
    if Position < Max then
    begin
      frmProgressMM.pbProgress.Max := Max;
      frmProgressMM.pbProgress.Position := Position;
      frmProgressMM.Show;
      StartTime := Now;
      Application.ProcessMessages;
    end;
  end;
  if Position = Max then
  begin
    frmProgressMM.Hide;
    Application.ProcessMessages;
  end;
end;

procedure TfrmImportPoints.dgDataSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  seRows.Value := dgData.RowCount -1;
  EnableOkButton;
end;

procedure TfrmImportPoints.rgElevationCountClick(Sender: TObject);
begin
  inherited;
  UpdateDimensionColumns;
  UpdateDataSets;
end;

{ TUndoImportPoints }

function TUndoImportPoints.Description: string;
begin
  result := 'import points';
end;

procedure TfrmImportPoints.SetData;
var
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  ScreenObjectList: TList;
  Position: integer;
  DataSet: TDataArray;
  RowIndex: integer;
  Undo: TUndoImportPoints;
  Root: string;
  APoint: TPoint2D;
  AnXCol: integer;
  AYCol: integer;
  AZ1Col: integer;
  AZ2Col: integer;
  RequiredCols: integer;
  ColIndex: integer;
  AFormula: string;
  ExistingObjectCount: integer;
  LastValue: string;
  ElevValues1: TValueArrayStorage;
  ElevValues2: TValueArrayStorage;
  Item: TValueArrayItem;
  MultiValueList: TList;
  DataSetValues: TValueArrayStorage;
  ARealValue: double;
  AnIntValue: integer;
begin
  RequiredCols := rgElevationCount.ItemIndex + 2;
  Root := edRoot.Text + '_';
  if Trim(Root) = '' then
  begin
    Root := ObjectPrefix;
  end;
  ExistingObjectCount :=
    frmGoPhast.PhastModel.NumberOfLargestScreenObjectsStartingWith(Root);
  AnXCol := -1;
  AYCol := -1;
  AZ1Col := -1;
  AZ2Col := -1;
  case TViewDirection(rgViewDirection.ItemIndex) of
    vdTop:
      begin
        AnXCol := XCol;
        AYCol  := YCol;
        case TElevationCount(rgElevationCount.ItemIndex) of
          ecZero:
            begin
              AZ1Col := -1;
              AZ2Col := -1;
            end;
          ecOne:
            begin
              AZ1Col := ZCol;
              AZ2Col := -1;
            end;
          ecTwo:
            begin
              AZ1Col := Z1Col;
              AZ2Col := Z2Col;
            end;
        else
          Assert(False);
        end;
      end;
    vdFront:
      begin
        AnXCol := XCol;
        AYCol  := ZCol;
        case TElevationCount(rgElevationCount.ItemIndex) of
          ecZero:
            begin
              AZ1Col := -1;
              AZ2Col := -1;
            end;
          ecOne:
            begin
              AZ1Col := YCol;
              AZ2Col := -1;
            end;
          ecTwo:
            begin
              AZ1Col := Y1Col;
              AZ2Col := Y2Col;
            end;
        else
          Assert(False);
        end;
      end;
    vdSide:
      begin
        AnXCol := ZCol;
        AYCol  := YCol;
        case TElevationCount(rgElevationCount.ItemIndex) of
          ecZero:
            begin
              AZ1Col := -1;
              AZ2Col := -1;
            end;
          ecOne:
            begin
              AZ1Col := XCol;
              AZ2Col := -1;
            end;
          ecTwo:
            begin
              AZ1Col := X1Col;
              AZ2Col := X2Col;
            end;
        else
          Assert(False);
        end;
      end;
  else
    Assert(False);
  end;

  MultiValueList := TList.Create;
  ScreenObjectList := TList.Create;
  frmGoPhast.PhastModel.BeginScreenObjectUpdate;
  ElevValues1 := TValueArrayStorage.Create;
  ElevValues2 := TValueArrayStorage.Create;
  try
    Undo := TUndoImportPoints.Create;
    try
      ElevValues1.Count := seRows.AsInteger;
      ElevValues2.Count := seRows.AsInteger;
      if cbImportAsSingleObject.Checked then
      begin
        ScreenObjectList.Capacity := 1;
      end
      else
      begin
        ScreenObjectList.Capacity := seRows.AsInteger;
      end;
      frmProgressMM.Caption := 'Progress';
      frmProgressMM.pbProgress.Max := dgData.RowCount-1;
      frmProgressMM.pbProgress.Position := 0;
      frmProgressMM.PopupParent := frmGoPhast;
      frmProgressMM.Show;
      for RowIndex := 1 to dgData.RowCount - 1 do
      begin
        try
          begin
            APoint.X := StrToFloat(dgData.Cells[AnXCol, RowIndex]);
            APoint.Y := StrToFloat(dgData.Cells[AYCol, RowIndex]);
            if AZ1Col >= 0 then
            begin
              StrToFloat(dgData.Cells[AZ1Col, RowIndex]);
            end;
            if AZ2Col >= 0 then
            begin
              StrToFloat(dgData.Cells[AZ2Col, RowIndex]);
            end;
          end;
        except on EConvertError do
          Continue;
        end;
        if not cbImportAsSingleObject.Checked or (RowIndex = 1) then
        begin
          AScreenObject :=
            TScreenObject.CreateWithViewDirection(frmGoPhast.PhastModel,
            TViewDirection(rgViewDirection.ItemIndex),
            UndoCreateScreenObject, False);
          Inc(ExistingObjectCount);
          AScreenObject.Name := Root + IntToStr(ExistingObjectCount);
          AScreenObject.SetValuesOfEnclosedCells
            := False;
          AScreenObject.SetValuesOfIntersectedCells
            := cbIntersectedCells.Checked;
          AScreenObject.SetValuesByInterpolation
            := cbInterpolation.Checked;

          AScreenObject.EvaluatedAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
          if cbImportAsSingleObject.Checked then
          begin
            AScreenObject.Capacity := dgData.RowCount - 1;
          end
          else
          begin
            AScreenObject.Capacity := 1;
          end;
          AScreenObject.Visible := cbVisible.Checked;
          if cbImportAsSingleObject.Checked then
          begin
            for ColIndex := RequiredCols to dgData.ColCount -1 do
            begin
              DataSet := dgData.Objects[ColIndex, 0] as TDataArray;
              Item := AScreenObject.ImportedValues.Add as TValueArrayItem;
              Item.Name := DataSet.Name;
              Item.Values.DataType := DataSet.DataType;
              Item.Values.Count := dgData.RowCount - 1;
              MultiValueList.Add(Item.Values);
            end;
          end;
        end;

        AScreenObject.AddPoint(APoint, cbImportAsSingleObject.Checked);

        if not cbImportAsSingleObject.Checked or (RowIndex = 1) then
        begin
          AScreenObject.ElevationCount := TElevationCount(rgElevationCount.ItemIndex);
          if cbImportAsSingleObject.Checked then
          begin
            case AScreenObject.ElevationCount of
              ecZero:
                begin
                  // do nothing
                end;
              ecOne:
                begin
                  AScreenObject.ElevationFormula :=
                    rsObjectImportedValuesR
                    + '("' + StrImportedElevations + '")';
                end;
              ecTwo:
                begin
                  AScreenObject.LowerElevationFormula :=
                    rsObjectImportedValuesR
                    + '("' + StrImportedLowerEleva + '")';
                  AScreenObject.HigherElevationFormula :=
                    rsObjectImportedValuesR
                    + '("' + StrImportedHigherElev + '")';
                end;
            else
              Assert(False);
            end;
          end;
        end;
        case AScreenObject.ElevationCount of
          ecZero:
            begin
              // do nothing
            end;
          ecOne:
            begin
              if cbImportAsSingleObject.Checked then
              begin
                ElevValues1.RealValues[RowIndex-1] := StrToFloat(dgData.Cells[AZ1Col, RowIndex]);
              end
              else
              begin
                AScreenObject.ElevationFormula := dgData.Cells[AZ1Col, RowIndex];
              end;
            end;
          ecTwo:
            begin
              if cbImportAsSingleObject.Checked then
              begin
                ElevValues1.RealValues[RowIndex-1] := StrToFloat(dgData.Cells[AZ1Col, RowIndex]);
                ElevValues2.RealValues[RowIndex-1] := StrToFloat(dgData.Cells[AZ2Col, RowIndex]);
              end
              else
              begin
                AScreenObject.LowerElevationFormula := dgData.Cells[AZ1Col, RowIndex];
                AScreenObject.HigherElevationFormula := dgData.Cells[AZ2Col, RowIndex];
              end;
            end;
        else
          Assert(False);
        end;

        try
          DataSetValues := nil;
          for ColIndex := RequiredCols to dgData.ColCount -1 do
          begin
            DataSet := dgData.Objects[ColIndex, 0] as TDataArray;
            Position := AScreenObject.AddDataSet(DataSet);
            Assert(Position >= 0);

            if cbImportAsSingleObject.Checked then
            begin
              DataSetValues := MultiValueList[ColIndex-RequiredCols];
            end;
            case DataSet.DataType of
              rdtDouble:
                begin
                  ARealValue := StrToFloat(dgData.Cells[ColIndex,RowIndex]);
                  if cbImportAsSingleObject.Checked then
                  begin
                    DataSetValues.RealValues[RowIndex-1] := ARealValue;
                  end
                  else
                  begin
                    AScreenObject.DataSetFormulas[Position]
                      := dgData.Cells[ColIndex,RowIndex];
                  end;
                end;
              rdtInteger:
                begin
                  AnIntValue := StrToInt(dgData.Cells[ColIndex,RowIndex]);
                  if cbImportAsSingleObject.Checked then
                  begin
                    DataSetValues.IntValues[RowIndex-1] := AnIntValue;
                  end
                  else
                  begin
                    AScreenObject.DataSetFormulas[Position]
                      := dgData.Cells[ColIndex,RowIndex];
                  end;
                end;
              rdtBoolean:
                begin
                  if cbImportAsSingleObject.Checked then
                  begin
                    if dgData.Checked[ColIndex,RowIndex] then
                    begin
                      DataSetValues.BooleanValues[RowIndex-1] := True;
                    end
                    else
                    begin
                      DataSetValues.BooleanValues[RowIndex-1] := False;
                    end;
                  end
                  else
                  begin
                    if dgData.Checked[ColIndex,RowIndex] then
                    begin
                      AScreenObject.DataSetFormulas[Position]
                        := 'True';
                    end
                    else
                    begin
                      AScreenObject.DataSetFormulas[Position]
                        := 'False';
                    end;
                  end;
                end;
              rdtString:
                begin
                  AFormula := dgData.Cells[ColIndex,RowIndex];
                  if cbImportAsSingleObject.Checked then
                  begin
                    if Length(AFormula) > 0 then
                    begin
                      if AFormula[1] = '"' then
                      begin
                        AFormula := Copy(AFormula, 2, MAXINT);
                      end;
                    end;
                    if Length(AFormula) > 0 then
                    begin
                      if AFormula[Length(AFormula)] = '"' then
                      begin
                        AFormula := Copy(AFormula, 1, Length(AFormula)-1);
                      end;
                    end;
                    DataSetValues.StringValues[RowIndex-1] := AFormula;
                  end
                  else
                  begin
                    if Length(AFormula) > 0 then
                    begin
                      if AFormula[1] <> '"' then
                      begin
                        AFormula := '"' + AFormula;
                      end;
                      if AFormula[Length(AFormula)] <> '"' then
                      begin
                        AFormula := AFormula + '"';
                      end;
                    end
                    else
                    begin
                      AFormula := '""';
                    end;
                    AScreenObject.DataSetFormulas[Position]
                      := AFormula;
                  end;
                end;
            else
              Assert(False);
            end;
          end;
        except on EConvertError do
          begin
            FreeAndNil(AScreenObject);
            Undo.Free;
            if cbImportAsSingleObject.Checked then
            begin
              Beep;
              MessageDlg('Invalid data in row ' + IntToStr(RowIndex) + '.',
                mtError, [mbOK], 0);
              Exit;
            end
            else
            begin
              Continue;
            end;
          end;
        end;

        if not cbImportAsSingleObject.Checked or (RowIndex = 1) then
        begin
          ScreenObjectList.Add(AScreenObject);
        end;
        if RowIndex mod 100 = 0 then
        begin
          frmProgressMM.BringToFront;
          Application.ProcessMessages;
        end;
        frmProgressMM.StepIt;
      end;

      if cbImportAsSingleObject.Checked and (AScreenObject <> nil) then
      begin
        case AScreenObject.ElevationCount of
          ecZero:
            begin
              // do nothing
            end;
          ecOne:
            begin
              AScreenObject.ImportedSectionElevations := ElevValues1;
            end;
          ecTwo:
            begin
              AScreenObject.ImportedLowerSectionElevations := ElevValues1;
              AScreenObject.ImportedHigherSectionElevations := ElevValues2;
            end;
        else
          Assert(False);
        end;
        for ColIndex := RequiredCols to dgData.ColCount -1 do
        begin
          DataSet := dgData.Objects[ColIndex, 0] as TDataArray;
          Position := AScreenObject.AddDataSet(DataSet);
          Assert(Position >= 0);
          case DataSet.DataType of
            rdtDouble:
              begin
                AScreenObject.DataSetFormulas[Position] :=
                  rsObjectImportedValuesR + '("' + DataSet.Name + '")';
              end;
            rdtInteger:
              begin
                AScreenObject.DataSetFormulas[Position] :=
                  rsObjectImportedValuesI + '("' + DataSet.Name + '")';
              end;
            rdtBoolean:
              begin
                AScreenObject.DataSetFormulas[Position] :=
                  rsObjectImportedValuesB + '("' + DataSet.Name + '")';
              end;
            rdtString:
              begin
                AScreenObject.DataSetFormulas[Position] :=
                  rsObjectImportedValuesT + '("' + DataSet.Name + '")';
              end;
            else Assert(False);
          end;
        end;
      end;

      if ScreenObjectList.Count > 0 then
      begin
        Undo.StoreNewScreenObjects(ScreenObjectList);
        frmGoPhast.UndoStack.Submit(Undo);
        if FImportFileName <> '' then
        begin
          frmGoPhast.PhastModel.AddFileToArchive(FImportFileName);
        end;
      end
      else
      begin
        Undo.Free
      end;  
    except
      Undo.Free;
      raise;
    end;
  finally
    frmProgressMM.Hide;
    ScreenObjectList.Free;
    ElevValues1.Free;
    ElevValues2.Free;
    MultiValueList.Free;
    frmGoPhast.PhastModel.EndScreenObjectUpdate;
  end;
end;

procedure TfrmImportPoints.btnOKClick(Sender: TObject);
var
  RequiredCols: Integer;
  ColIndex: Integer;
  DataSet: TDataArray;
begin
  inherited;
  if cbInterpolation.Checked then
  begin

    RequiredCols := rgElevationCount.ItemIndex + 2;
    for ColIndex := RequiredCols to dgData.ColCount -1 do
    begin
      DataSet := dgData.Objects[ColIndex, 0] as TDataArray;
      if DataSet.TwoDInterpolator = nil then
      begin
        if (MessageDlg('You have choosen to set values of data sets '
          + 'using interpolation but interpolation is not used '
          + 'in one or more of the data sets for which you are '
          + 'importing values.  Do you want to import the data anyway?',
          mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
        begin
          break;
        end
        else
        begin
          ModalResult := mrNone;
          Exit;
        end;
      end;
    end;
  end;
  Hide;
  SetData;
end;

procedure TfrmImportPoints.btnOpenFileClick(Sender: TObject);
var
  Lines: TStringList;
  Index: Integer;
  DataSetSelected: boolean;
  ColIndex: Integer;
begin
  inherited;
  DataSetSelected := False;
  for Index := 0 to jvclbDataSets.Items.Count - 1 do
  begin
    DataSetSelected := jvclbDataSets.Checked[Index];
    if DataSetSelected then
    begin
      break;
    end;
  end;
  if not DataSetSelected then
  begin
    Beep;
    MessageDlg('You have not selected a data set on the "Controls" tab.',
      MtWarning, [mbOK], 0);
  end;
  if OpenDialogImportFile.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      Lines := TStringList.Create;
      try
        FImportFileName := OpenDialogImportFile.FileName;
        Lines.LoadFromFile(FImportFileName);
        for Index := Lines.Count - 1 downto 0 do
        begin
          if (Length(Lines[Index]) = 0) or (Lines[Index][1] = '#') then
          begin
            Lines.Delete(Index);
          end;
        end;
        seRows.Value := Lines.Count;
        dgData.BeginUpdate;
        try
          StartTime := Now;
          dgData.DistributeText(0,1, Lines.Text);
          frmProgressMM.Hide;
          for Index := dgData.RowCount - 1 downto 0 do
          begin
            for ColIndex := 0 to dgData.ColCount - 1 do
            begin
              if (dgData.Cells[ColIndex,Index] = '')
                and (dgData.Columns[ColIndex].Format in
                [rcf4Integer, rcf4Real]) then
              begin
                dgData.DeleteRow(Index);
                break;
              end;
            end;
          end;
          seRows.Value := dgData.RowCount - 1;
        finally
          dgData.EndUpdate;
        end;
      finally
        Lines.Free;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmImportPoints.cbIntersectedCellsClick(Sender: TObject);
begin
  inherited;
  EmphasizeCheckBoxes([cbIntersectedCells, cbInterpolation]);
  EnableOkButton;

end;

end.
