unit ModflowHydmodWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, ModflowSfrWriterUnit,
  ModflowHydmodUnit, Classes, PhastModelUnit, Contnrs, ModflowCellUnit,
  SysUtils;

type
  THydmodPackage = (hmpBAS, hmpIBS, hmpSub, hmpStr, hmpSFR);
  THydmodArr = (hmaHead, hmaDrawdown, hmaPreconsilidationHead, hmaCompaction,
    hmaSubsidence, hmaSfrStage, hmaInflow, hmaOutflow, hmaExchange);

  TModflowHydmodWriter = class;

  TCustomHydModItem = class(TObject)
    FPackage: THydmodPackage;
    FArray: THydmodArr;
    FAssignmentMethod: TAssignmentMethod;
    FLayer: integer;
    FHydLabel: string;
    Procedure Write(Writer: TModflowHydmodWriter); virtual; abstract;
  end;

  THydSfrModCell = class(TCustomHydModItem)
    FSegment: integer;
    FReach: integer;
    Procedure Write(Writer: TModflowHydmodWriter); override;
  end;

  THydModLocation = class(TCustomHydModItem)
    FXL: double;
    FYL: double;
    Procedure Write(Writer: TModflowHydmodWriter); override;
  end;

  TSegmentReach = class(TObject)
    FSegment: Integer;
    FReach: Integer;
  end;

  TModflowHydmodWriter = class(TCustomPackageWriter)
  private
    FSfrWriter: TModflowSFR_Writer;
    FLocations: TList;
    FNameOfFile: string;
    procedure Evaluate;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
  public
    Constructor Create(Model: TPhastModel); override;
    Destructor Destroy; override;
    procedure WriteFile(const AFileName: string;
      SfrWriter: TModflowSFR_Writer);
  end;


implementation

uses
  frmModflowNameFileUnit, ModflowUnitNumbers, ScreenObjectUnit,
  GoPhastTypes, frmProgressUnit, frmErrorsAndWarningsUnit, FastGEO, Forms;

{ TModflowHydmodWriter }

constructor TModflowHydmodWriter.Create(Model: TPhastModel);
begin
  inherited;
  FLocations:= TObjectList.Create;
end;

destructor TModflowHydmodWriter.Destroy;
begin
  FLocations.Free;
  inherited;
end;

procedure TModflowHydmodWriter.Evaluate;
var
  Index: Integer;
  ScreenObject: TScreenObject;
  HydmodData: THydmodData;
  CellList: TCellAssignmentList;
  AssignmentMethod: ModflowHydmodUnit.TAssignmentMethod;
  IntArray: TOneDIntegerArray;
  CellIndex: Integer;
  ACell: TCellAssignment;
  APoint: TPoint2D;
  X0: Real;
  Y0: Real;
  Location: THydModLocation;
  CheckArray: array of array of boolean;
  SegmentReachArray: array of array of array of TList;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  List: TList;
  ReachIndex: Integer;
  SegmentReach: TSegmentReach;
  SfrLocation: THydSfrModCell;
  procedure InitializeCheckArray;
  var
    RowIndex: Integer;
    ColIndex: Integer;
  begin
    for RowIndex := 0 to PhastModel.Grid.RowCount - 1 do
    begin
      for ColIndex := 0 to PhastModel.Grid.ColumnCount - 1 do
      begin
        CheckArray[RowIndex,ColIndex] := False;
      end;
    end;
  end;
  procedure InitializeSegmentReachArray;
  var
    SegmentIndex: Integer;
    Segment: TSegment;
    ReachIndex: Integer;
    Reach: TValueCell;
    List: TList;
    RowIndex: Integer;
    ColIndex: Integer;
    SegmentReach: TSegmentReach;
    LayerIndex: integer;
  begin
    if Length(SegmentReachArray) = 0 then
    begin
      SetLength(SegmentReachArray, PhastModel.Grid.LayerCount,
        PhastModel.Grid.RowCount, PhastModel.Grid.ColumnCount);
      for LayerIndex := 0 to PhastModel.Grid.LayerCount - 1 do
      begin
        for RowIndex := 0 to PhastModel.Grid.RowCount - 1 do
        begin
          for ColIndex := 0 to PhastModel.Grid.ColumnCount - 1 do
          begin
            SegmentReachArray[LayerIndex,RowIndex,ColIndex] := nil;
          end;
        end;
      end;
      for SegmentIndex := 0 to FSfrWriter.SegmentCount - 1 do
      begin
        Segment := FSfrWriter.Segments[SegmentIndex];
        for ReachIndex := 0 to Segment.ReachCount - 1 do
        begin
          Reach := Segment.Reaches[ReachIndex];
          List := SegmentReachArray[Reach.Layer, Reach.Row, Reach.Column];
          if List = nil then
          begin
            List := TObjectList.Create;
            SegmentReachArray[Reach.Layer, Reach.Row, Reach.Column] := List;
          end;
          SegmentReach := TSegmentReach.Create;
          List.Add(SegmentReach);
          SegmentReach.FSegment := Segment.NewSegmentNumber;
          SegmentReach.FReach := ReachIndex + 1;
        end;
      end;
    end
  end;
begin
  try
  SetLength(CheckArray, PhastModel.Grid.RowCount, PhastModel.Grid.ColumnCount);
  X0 := PhastModel.Grid.ColumnPosition[0];
  Y0 := PhastModel.Grid.RowPosition[PhastModel.Grid.RowCount];
  for Index := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := PhastModel.ScreenObjects[Index];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    HydmodData := ScreenObject.ModflowHydmodData;
    if (HydmodData <> nil) and HydmodData.Used then
    begin
      CellList := TCellAssignmentList.Create;
      try
        ScreenObject.GetCellsToAssign(PhastModel.Grid, '0',
          nil, nil, CellList, alAll);
        if (ScreenObject.ViewDirection = vdTop)
          and (ScreenObject.Count = ScreenObject.SectionCount)
          and (not HydmodData.SfrStage)
          and (not HydmodData.SfrInFlow)
          and (not HydmodData.SfrOutFlow)
          and (not HydmodData.SfrAquiferExchange) then
        begin
          AssignmentMethod := HydmodData.AssignmentMethod;
        end
        else
        begin
          AssignmentMethod := amCell;
          if AssignmentMethod <> HydmodData.AssignmentMethod then
          begin
            frmErrorsAndWarnings.AddWarning(
              'The interpolation assignment method in HYDMOD package '
              + 'is invalid for the following objects.  The cell method has '
              + 'been used instead.',
              ScreenObject.Name);
          end;
        end;
        case AssignmentMethod of
          amCell:
            begin
              if HydmodData.Head or HydmodData.Drawdown then
              begin
                for CellIndex := 0 to CellList.Count - 1 do
                begin
                  ACell := CellList[CellIndex];
                  APoint := PhastModel.Grid.UnrotatedTwoDElementCenter(
                    ACell.Column, ACell.Row);
                  if HydmodData.Head then
                  begin
                    Location := THydModLocation.Create;
                    FLocations.Add(Location);
                    Location.FXL := APoint.x - X0;
                    Location.FYL := APoint.y - Y0;
                    Location.FPackage := hmpBAS;
                    Location.FArray := hmaHead;
                    Location.FAssignmentMethod := amCell;
                    Location.FLayer := ACell.Layer + 1;
                    Location.FHydLabel := HydmodData.HydrographLabel;
                  end;
                  if HydmodData.Drawdown then
                  begin
                    Location := THydModLocation.Create;
                    FLocations.Add(Location);
                    Location.FXL := APoint.x - X0;
                    Location.FYL := APoint.y - Y0;
                    Location.FPackage := hmpBAS;
                    Location.FArray := hmaDrawdown;
                    Location.FAssignmentMethod := amCell;
                    Location.FLayer := ACell.Layer + 1;
                    Location.FHydLabel := HydmodData.HydrographLabel;
                  end;
                end;
              end;
              if PhastModel.ModflowPackages.SubPackage.IsSelected
                and (HydmodData.SubPreconsolidationHead
                or HydmodData.SubCompaction or HydmodData.SubSubsidence) then
              begin
                HydmodData.SubLayerNumbers(IntArray);
                InitializeCheckArray;
                for CellIndex := 0 to CellList.Count - 1 do
                begin
                  ACell := CellList[CellIndex];
                  if not CheckArray[ACell.Row, ACell.Column] then
                  begin
                    CheckArray[ACell.Row, ACell.Column] := True;
                    APoint := PhastModel.Grid.UnrotatedTwoDElementCenter(
                      ACell.Column, ACell.Row);
                    for LayerIndex := 0 to Length(IntArray) - 1 do
                    begin
                      if HydmodData.SubPreconsolidationHead then
                      begin
                        Location := THydModLocation.Create;
                        FLocations.Add(Location);
                        Location.FXL := APoint.x - X0;
                        Location.FYL := APoint.y - Y0;
                        Location.FPackage := hmpSub;
                        Location.FArray := hmaPreconsilidationHead;
                        Location.FAssignmentMethod := amCell;
                        Location.FLayer := IntArray[LayerIndex];
                        Location.FHydLabel := HydmodData.HydrographLabel;
                      end;
                      if HydmodData.SubCompaction then
                      begin
                        Location := THydModLocation.Create;
                        FLocations.Add(Location);
                        Location.FXL := APoint.x - X0;
                        Location.FYL := APoint.y - Y0;
                        Location.FPackage := hmpSub;
                        Location.FArray := hmaCompaction;
                        Location.FAssignmentMethod := amCell;
                        Location.FLayer := IntArray[LayerIndex];
                        Location.FHydLabel := HydmodData.HydrographLabel;
                      end;
                      if HydmodData.SubSubsidence then
                      begin
                        Location := THydModLocation.Create;
                        FLocations.Add(Location);
                        Location.FXL := APoint.x - X0;
                        Location.FYL := APoint.y - Y0;
                        Location.FPackage := hmpSub;
                        Location.FArray := hmaSubsidence;
                        Location.FAssignmentMethod := amCell;
                        Location.FLayer := IntArray[LayerIndex];
                        Location.FHydLabel := HydmodData.HydrographLabel;
                      end;
                    end;
                  end;
                end;
              end;
              if PhastModel.ModflowPackages.SfrPackage.IsSelected
                and (HydmodData.SfrStage or HydmodData.SfrInFlow
                or HydmodData.SfrOutFlow or HydmodData.SfrAquiferExchange) then
              begin
                InitializeSegmentReachArray;
                for CellIndex := 0 to CellList.Count - 1 do
                begin
                  ACell := CellList[CellIndex];
                  List := SegmentReachArray[ACell.Layer, ACell.Row, ACell.Column];
                  if List <> nil then
                  begin
                    APoint := PhastModel.Grid.UnrotatedTwoDElementCenter(
                      ACell.Column, ACell.Row);
                    for ReachIndex := 0 to List.Count - 1 do
                    begin
                      SegmentReach := List[ReachIndex];
                      if HydmodData.SfrStage then
                      begin
                        SfrLocation := THydSfrModCell.Create;
                        FLocations.Add(SfrLocation);
                        SfrLocation.FSegment := SegmentReach.FSegment;
                        SfrLocation.FReach := SegmentReach.FReach;
                        SfrLocation.FPackage := hmpSFR;
                        SfrLocation.FArray := hmaSfrStage;
                        SfrLocation.FAssignmentMethod := amCell;
                        SfrLocation.FLayer := ACell.Layer+1;
                        SfrLocation.FHydLabel := HydmodData.HydrographLabel;
                      end;
                      if HydmodData.SfrInFlow then
                      begin
                        SfrLocation := THydSfrModCell.Create;
                        FLocations.Add(SfrLocation);
                        SfrLocation.FSegment := SegmentReach.FSegment;
                        SfrLocation.FReach := SegmentReach.FReach;
                        SfrLocation.FPackage := hmpSFR;
                        SfrLocation.FArray := hmaInflow;
                        SfrLocation.FAssignmentMethod := amCell;
                        SfrLocation.FLayer := ACell.Layer+1;
                        SfrLocation.FHydLabel := HydmodData.HydrographLabel;
                      end;
                      if HydmodData.SfrOutFlow then
                      begin
                        SfrLocation := THydSfrModCell.Create;
                        FLocations.Add(SfrLocation);
                        SfrLocation.FSegment := SegmentReach.FSegment;
                        SfrLocation.FReach := SegmentReach.FReach;
                        SfrLocation.FPackage := hmpSFR;
                        SfrLocation.FArray := hmaOutflow;
                        SfrLocation.FAssignmentMethod := amCell;
                        SfrLocation.FLayer := ACell.Layer+1;
                        SfrLocation.FHydLabel := HydmodData.HydrographLabel;
                      end;
                      if HydmodData.SfrAquiferExchange then
                      begin
                        SfrLocation := THydSfrModCell.Create;
                        FLocations.Add(SfrLocation);
                        SfrLocation.FSegment := SegmentReach.FSegment;
                        SfrLocation.FReach := SegmentReach.FReach;
                        SfrLocation.FPackage := hmpSFR;
                        SfrLocation.FArray := hmaExchange;
                        SfrLocation.FAssignmentMethod := amCell;
                        SfrLocation.FLayer := ACell.Layer+1;
                        SfrLocation.FHydLabel := HydmodData.HydrographLabel;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          amInterpolate:
            begin
              if HydmodData.Head or HydmodData.Drawdown then
              begin
                for CellIndex := 0 to CellList.Count - 1 do
                begin
                  ACell := CellList[CellIndex];
                  if HydmodData.Head then
                  begin
                    Location := THydModLocation.Create;
                    FLocations.Add(Location);
                    Location.FXL := ACell.Segment.X1 - X0;
                    Location.FYL := ACell.Segment.Y1 - Y0;
                    Location.FPackage := hmpBAS;
                    Location.FArray := hmaHead;
                    Location.FAssignmentMethod := amInterpolate;
                    Location.FLayer := ACell.Layer + 1;
                    Location.FHydLabel := HydmodData.HydrographLabel;
                  end;
                  if HydmodData.Drawdown then
                  begin
                    Location := THydModLocation.Create;
                    FLocations.Add(Location);
                    Location.FXL := ACell.Segment.X1 - X0;
                    Location.FYL := ACell.Segment.Y1 - Y0;
                    Location.FPackage := hmpBAS;
                    Location.FArray := hmaDrawdown;
                    Location.FAssignmentMethod := amInterpolate;
                    Location.FLayer := ACell.Layer + 1;
                    Location.FHydLabel := HydmodData.HydrographLabel;
                  end;
                end;
              end;
              if PhastModel.ModflowPackages.SubPackage.IsSelected
                and (HydmodData.SubPreconsolidationHead
                or HydmodData.SubCompaction or HydmodData.SubSubsidence) then
              begin
                HydmodData.SubLayerNumbers(IntArray);
                InitializeCheckArray;
                for CellIndex := 0 to CellList.Count - 1 do
                begin
                  ACell := CellList[CellIndex];
                  if not CheckArray[ACell.Row, ACell.Column] then
                  begin
                    CheckArray[ACell.Row, ACell.Column] := True;
                    for LayerIndex := 0 to Length(IntArray) - 1 do
                    begin
                      if HydmodData.SubPreconsolidationHead then
                      begin
                        Location := THydModLocation.Create;
                        FLocations.Add(Location);
                        Location.FXL := ACell.Segment.X1 - X0;
                        Location.FYL := ACell.Segment.Y1 - Y0;
                        Location.FPackage := hmpSub;
                        Location.FArray := hmaPreconsilidationHead;
                        Location.FAssignmentMethod := amInterpolate;
                        Location.FLayer := IntArray[LayerIndex];
                        Location.FHydLabel := HydmodData.HydrographLabel;
                      end;
                      if HydmodData.SubCompaction then
                      begin
                        Location := THydModLocation.Create;
                        FLocations.Add(Location);
                        Location.FXL := ACell.Segment.X1 - X0;
                        Location.FYL := ACell.Segment.Y1 - Y0;
                        Location.FPackage := hmpSub;
                        Location.FArray := hmaCompaction;
                        Location.FAssignmentMethod := amInterpolate;
                        Location.FLayer := IntArray[LayerIndex];
                        Location.FHydLabel := HydmodData.HydrographLabel;
                      end;
                      if HydmodData.SubSubsidence then
                      begin
                        Location := THydModLocation.Create;
                        FLocations.Add(Location);
                        Location.FXL := ACell.Segment.X1 - X0;
                        Location.FYL := ACell.Segment.Y1 - Y0;
                        Location.FPackage := hmpSub;
                        Location.FArray := hmaSubsidence;
                        Location.FAssignmentMethod := amInterpolate;
                        Location.FLayer := IntArray[LayerIndex];
                        Location.FHydLabel := HydmodData.HydrographLabel;
                      end;
                    end;
                  end;
                end
              end;
              Assert((not HydmodData.SfrStage)
                and (not HydmodData.SfrInFlow)
                and (not HydmodData.SfrOutFlow)
                and (not HydmodData.SfrAquiferExchange));
            end;
        end;
      finally
        CellList.Free;
      end;
    end;
  end;

  finally
    if Length(SegmentReachArray) > 0 then
    begin
      for LayerIndex := 0 to PhastModel.Grid.LayerCount - 1 do
      begin
        for RowIndex := 0 to PhastModel.Grid.RowCount - 1 do
        begin
          for ColIndex := 0 to PhastModel.Grid.ColumnCount - 1 do
          begin
            SegmentReachArray[LayerIndex,RowIndex, ColIndex].Free;
          end;
        end;
      end;
    end;
  end;
end;

class function TModflowHydmodWriter.Extension: string;
begin
  result := '.hyd';
end;

function TModflowHydmodWriter.Package: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.HydmodPackage;
end;

procedure TModflowHydmodWriter.WriteDataSet1;
var
  NHYD, IHYDUN: integer;
  HYDNOH: double;
  HydModOutFileName: string;
begin
  NHYD := FLocations.Count;
  IHYDUN := PhastModel.UnitNumbers.UnitNumber(StrHydmodOut);

  HydModOutFileName := ExtractFileName(ChangeFileExt(FNameOfFile, '.hyd_out'));
  WriteToNameFile(StrDATABINARY, IHYDUN,
    HydModOutFileName, foOutput);

  HYDNOH := (Package as THydPackageSelection).HYDNOH;
  WriteInteger(NHYD);
  WriteInteger(IHYDUN);
  WriteFloat(HYDNOH);
  WriteString(' # Data Set 1: NHYD IHYDUN HYDNOH');
  NewLine;
end;

procedure TModflowHydmodWriter.WriteDataSet2;
var
  Index: Integer;
  Item: TCustomHydModItem;
begin
  for Index := 0 to FLocations.Count - 1 do
  begin
    Item := FLocations[Index];
    Item.Write(self);
  end;
end;

procedure TModflowHydmodWriter.WriteFile(const AFileName: string;
  SfrWriter: TModflowSFR_Writer);
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if PhastModel.PackageGeneratedExternally(StrHYD) then
  begin
    Exit;
  end;
  FSfrWriter := SfrWriter;
  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrHYD, PhastModel.UnitNumbers.UnitNumber(StrHYD), FNameOfFile, foInput);
  Evaluate;
  Application.ProcessMessages;
  if not frmProgress.ShouldContinue then
  begin
    Exit;
  end;
  OpenFile(FNameOfFile);
  try
    frmProgress.AddMessage('Writing HYDMOD Package input.');
    frmProgress.AddMessage('  Writing Data Set 1.');
    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Writing Data Set 2.');
    WriteDataSet2;
  finally
    CloseFile;
  end;
end;

{ THydSfrModCell }

procedure THydSfrModCell.Write(Writer: TModflowHydmodWriter);
var
  PCKG, ARR, INTYP , HYDLBL: string;
  KLAY: integer;
  XL, YL: integer;
begin
  case FPackage of
    hmpBAS: PCKG := 'BAS ';
    hmpIBS: PCKG := 'IBS ';
    hmpSub: PCKG := 'SUB ';
    hmpStr: PCKG := 'STR ';
    hmpSFR: PCKG := 'SFR ';
    else Assert(False);
  end;
  case FArray of
    hmaHead: ARR := 'HD ';
    hmaDrawdown: ARR := 'DD ';
    hmaPreconsilidationHead: ARR := 'HC ';
    hmaCompaction: ARR := 'CP ';
    hmaSubsidence: ARR := 'SB ';
    hmaSfrStage: ARR := 'ST ';
    hmaInflow: ARR := 'SI ';
    hmaOutflow: ARR := 'SO ';
    hmaExchange: ARR := 'SA ';
    else Assert(False);
  end;
  case self.FAssignmentMethod of
    amCell: INTYP := 'C ';
    amInterpolate:  INTYP := 'I ';
    else Assert(False);
  end;
  KLAY := FLayer;
  XL := FSegment;
  YL := FReach;
  HYDLBL := ' ' + FHydLabel;
  Writer.WriteString(PCKG);
  Writer.WriteString(ARR);
  Writer.WriteString(INTYP);
  Writer.WriteInteger(KLAY);
  Writer.WriteInteger(XL);
  Writer.WriteInteger(YL);
  Writer.WriteString(HYDLBL);
  Writer.WriteString(' # Data Set 2: PCKG ARR INTYP KLAY XL YL HYDLBL');
  Writer.NewLine;
end;

{ THydModLocation }

procedure THydModLocation.Write(Writer: TModflowHydmodWriter);
var
  PCKG, ARR, INTYP , HYDLBL: string;
  KLAY: integer;
  XL, YL: double;
begin
  case FPackage of
    hmpBAS: PCKG := 'BAS ';
    hmpIBS: PCKG := 'IBS ';
    hmpSub: PCKG := 'SUB ';
    hmpStr: PCKG := 'STR ';
    hmpSFR: PCKG := 'SFR ';
    else Assert(False);
  end;
  case FArray of
    hmaHead: ARR := 'HD ';
    hmaDrawdown: ARR := 'DD ';
    hmaPreconsilidationHead: ARR := 'HC ';
    hmaCompaction: ARR := 'CP ';
    hmaSubsidence: ARR := 'SB ';
    hmaSfrStage: ARR := 'ST ';
    hmaInflow: ARR := 'SI ';
    hmaOutflow: ARR := 'SO ';
    hmaExchange: ARR := 'SA ';
    else Assert(False);
  end;
  case self.FAssignmentMethod of
    amCell: INTYP := 'C ';
    amInterpolate:  INTYP := 'I ';
    else Assert(False);
  end;
  KLAY := FLayer;
  XL := FXl;
  YL := FYl;
  HYDLBL := ' ' + FHydLabel;
  Writer.WriteString(PCKG);
  Writer.WriteString(ARR);
  Writer.WriteString(INTYP);
  Writer.WriteInteger(KLAY);
  Writer.WriteFloat(XL);
  Writer.WriteFloat(YL);
  Writer.WriteString(HYDLBL);
  Writer.WriteString(' # Data Set 2: PCKG ARR INTYP KLAY XL YL HYDLBL');
  Writer.NewLine;
end;

end.
