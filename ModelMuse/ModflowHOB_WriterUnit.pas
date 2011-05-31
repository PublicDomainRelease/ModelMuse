unit ModflowHOB_WriterUnit;

interface

uses SysUtils, Math, Classes, Contnrs , PhastModelUnit, CustomModflowWriterUnit,
  ModflowPackageSelectionUnit, ModflowHobUnit, ModflowBoundaryDisplayUnit,
  GoPhastTypes;

type
  TLayerSort = class(TObject)
    Layer: integer;
    ActiveCells: integer;
    Proportion: double;
  end;

  TModflowHobWriter = class(TCustomPackageWriter)
  private
    NH: Integer;
    MOBS: Integer;
    MAXM: Integer;
    IUHOBSV: Integer;
    // @name contains the @link(THobBoundary)s that are used.
    FObservations: TList;
    FStartTime: Double;
    FEndTime: Double;
    IREFSP: Integer;
    procedure Evaluate(Purpose: TObservationPurpose);
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3to6(Index: integer);
    procedure WriteDataSet3(Observations: THobBoundary; CellList: TObsCellList);
    procedure WriteDataSet4(Observations: THobBoundary; CellList: TObsCellList);
    procedure WriteDataSet5(Observations: THobBoundary);
    procedure WriteDataSet6(Observations: THobBoundary);
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
  public
    Constructor Create(Model: TCustomModel); override;
    destructor Destroy; override;
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists;
      ParameterIndicies: TByteSet; Purpose: TObservationPurpose);
    procedure WriteFile(const AFileName: string; Purpose: TObservationPurpose);
  end;

implementation

uses ModflowUnitNumbers, ScreenObjectUnit, DataSetUnit,
  frmErrorsAndWarningsUnit, frmProgressUnit, Forms;

const
  ObsNameWarning = 'The following Head observation names may be valid for MODFLOW but they are not valid for UCODE.';
  MissingObsNameError = 'The head observation in the following objects do not have observations names assigned';

{ TModflowHobWriter }

constructor TModflowHobWriter.Create(Model: TCustomModel);
begin
  inherited;
  FObservations := TList.Create;
end;

destructor TModflowHobWriter.Destroy;
begin
  FObservations.Free;
  inherited;
end;

procedure TModflowHobWriter.Evaluate(Purpose: TObservationPurpose);
const
  InvalidEndObsTime = 'Observation time after end of simulation';
  InvalidStartObsTime = 'Observation time before beginning of simulation';
  HeadOffGrid = 'One or more head observation are not located on the grid and will be ignored';
  NoHeads = 'No head observations';
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Observations: THobBoundary;
  CellList: TObsCellList;
  ErrorMessage: string;
  ObsIndex: Integer;
  Item: THobItem;
  ObservationTimeCount: Integer;
  WrongObservationTypesDefined: boolean;
begin
  WrongObservationTypesDefined := False;
  FStartTime := Model.ModflowFullStressPeriods[0].StartTime;
  FEndTime := Model.ModflowFullStressPeriods[
    Model.ModflowFullStressPeriods.Count-1].EndTime;
  IUHOBSV := Model.UnitNumbers.UnitNumber(StrIUHOBSV);
  NH := 0;
  MOBS := 0;
  MAXM := 2;
  frmErrorsAndWarnings.RemoveWarningGroup(Model, ObsNameWarning);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, MissingObsNameError);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, HeadOffGrid);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, NoHeads);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, InvalidStartObsTime);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, InvalidEndObsTime);
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Observations := ScreenObject.ModflowHeadObservations;
    if (Observations <> nil) and Observations.Used then
    begin
      if Observations.Purpose = Purpose then
      begin
        Observations.EvaluateHeadObservations(Purpose);
        FObservations.Add(Observations);

        if Observations.CellListCount = 0 then
        begin
          ErrorMessage := 'Object: ' + ScreenObject.Name;
          frmErrorsAndWarnings.AddWarning(Model, HeadOffGrid, ErrorMessage);
          Continue;
        end;

        CellList := Observations.CellLists[0];
        if CellList.Count = 0 then
        begin
          ErrorMessage := 'Object: ' + ScreenObject.Name;
          frmErrorsAndWarnings.AddWarning(Model, HeadOffGrid, ErrorMessage);
          Continue;
        end;

        ObservationTimeCount := Observations.Values.
          CountObservationTimes(FStartTime, FEndTime);
        NH := NH + ObservationTimeCount;

        if CellList.Count > 1 then
        begin
          Inc(MOBS, Observations.Values.Count);
        end;
        if CellList.Count > MAXM then
        begin
          MAXM := CellList.Count;
        end;
        if ObservationTimeCount <> Observations.Values.Count then
        begin
          for ObsIndex := 0 to Observations.Values.Count - 1 do
          begin
            Item := Observations.Values.HobItems[ObsIndex];
            if Item.Time > FEndTime then
            begin
              ErrorMessage := 'Object: ' + ScreenObject.Name
                + '; Time: ' + FloatToStr(Item.Time);
              frmErrorsAndWarnings.AddError(Model,
                InvalidEndObsTime, ErrorMessage);
            end;
            if Item.Time < FStartTime then
            begin
              ErrorMessage := 'Object: ' + ScreenObject.Name
                + '; Time: ' + FloatToStr(Item.Time);
              frmErrorsAndWarnings.AddError(Model,
                InvalidStartObsTime, ErrorMessage);
            end;
          end;
        end;
      end
      else
      begin
        WrongObservationTypesDefined := True;
      end;
    end;
  end;
  if NH = 0 then
  begin
    if WrongObservationTypesDefined then
    begin
      frmErrorsAndWarnings.AddError(Model, NoHeads,
        'No valid head observations were defined. '
        + 'Check that "Model|Observation Type" is set to the '
        + 'correct value and that the observation type for '
        + 'each observation is set correctly.');
    end
    else
    begin
      frmErrorsAndWarnings.AddError(Model, NoHeads,
        'No valid head observations were defined.');
    end;
  end;
end;

class function TModflowHobWriter.Extension: string;
begin
  result := '.ob_hob';
end;

function TModflowHobWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.HobPackage;
end;

procedure TModflowHobWriter.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists; ParameterIndicies: TByteSet;
  Purpose: TObservationPurpose);
var
  DataArrayList: TList;
  TimeListIndex: Integer;
  DisplayTimeList: THobDisplayTimeList;
  TimeIndex: Integer;
//  CellList: TList;
  DataArray: TModflowBoundaryDisplayDataArray;
  ObsIndex: Integer;
  Obs: THobBoundary;
  ItemIndex: Integer;
  CellList: TObsCellList;
  Cell: THob_Cell;
  TimePosition: Integer;
  CellIndex: Integer;
begin
  // Quit if the package isn't used.
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrHeadObservationsError);
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  DataArrayList := TList.Create;
  try
    // evaluate all the data used in the package.
    Evaluate(Purpose);

    Assert(TimeLists.Count= 1);
    DisplayTimeList := TimeLists[0] as THobDisplayTimeList;
    for ObsIndex := 0 to FObservations.Count - 1 do
    begin
      Obs := FObservations[ObsIndex];
      for ItemIndex := 0 to Obs.Values.ObservationHeads.Count - 1 do
      begin
        CellList := Obs.Values.ObservationHeads[ItemIndex];
        if CellList.Count > 0 then
        begin
          Cell := CellList[0];
          TimePosition := DisplayTimeList.FirstTimeGreaterThan(Cell.Time);
          if TimePosition >= DisplayTimeList.Count then
          begin
            TimePosition := DisplayTimeList.Count-1;
          end;
          for TimeIndex := TimePosition downto Max(0, TimePosition-1) do
          begin
            if DisplayTimeList.Times[TimeIndex] = Cell.Time then
            begin
              TimePosition := TimeIndex;
              break;
            end;
          end;
          DataArray := DisplayTimeList[TimePosition]
            as TModflowBoundaryDisplayDataArray;
          for CellIndex := 0 to CellList.Count - 1 do
          begin
            Cell := CellList[CellIndex];
            DataArray.AddDataValue(Cell.HeadAnnotation, Cell.Head,
              Cell.Column, Cell.Row, Cell.Layer);
          end;
        end;
      end;
    end;
    // Mark all the data arrays and time lists as up to date.
    for TimeListIndex := 0 to TimeLists.Count - 1 do
    begin
      DisplayTimeList := TimeLists[TimeListIndex] as THobDisplayTimeList;
      for TimeIndex := 0 to DisplayTimeList.Count - 1 do
      begin
        DataArray := DisplayTimeList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.UpToDate := True;
      end;
      DisplayTimeList.SetUpToDate(True);
    end;
  finally
    DataArrayList.Free;
  end;

end;

procedure TModflowHobWriter.WriteDataSet1;
var
  HOBDRY: double;
begin
  HOBDRY := Model.ModflowPackages.HobPackage.DryHead;
  WriteInteger(NH);
  WriteInteger(MOBS);
  WriteInteger(MAXM);
  WriteInteger(IUHOBSV);
  WriteFloat(HOBDRY);
  WriteString(' # Data Set 1: NH MOBS MAXM IUHOBSV HOBDRY');
  NewLine;
end;

procedure TModflowHobWriter.WriteDataSet2;
const
  TOMULTH = 1;
begin
  WriteFloat(TOMULTH);
  WriteString(' # Data Set 2: TOMULTH');
  NewLine;
end;

function SortLayerSorts(Item1, Item2: Pointer): Integer;
var
  LayerSort1: TLayerSort;
  LayerSort2: TLayerSort;
begin
  LayerSort1 := Item1;
  LayerSort2 := Item2;
  result := LayerSort1.ActiveCells - LayerSort2.ActiveCells;
  if result = 1 then
  begin
    result := LayerSort1.Layer - LayerSort2.Layer;
  end;
end;

procedure TModflowHobWriter.WriteDataSet3to6(Index: integer);
var
  Observations: THobBoundary;
  CellList: TObsCellList;
begin
  Observations := FObservations[Index];
  if Observations.CellListCount > 0 then
  begin
    CellList := Observations.CellLists[0];
    if CellList.Count > 0 then
    begin
      WriteDataSet3(Observations, CellList);
      WriteDataSet4(Observations, CellList);
      WriteDataSet5(Observations);
      WriteDataSet6(Observations);
    end;
  end;
end;

procedure TModflowHobWriter.WriteFile(const AFileName: string; Purpose: TObservationPurpose);
var
  NameOfFile: string;
  OutFileName: string;
  Index: Integer;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrHOB) then
  begin
    Exit;
  end;
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrHeadObservationsError);
  frmProgressMM.AddMessage('Writing HOB Package input.');
  frmProgressMM.AddMessage('Evaluating data.');
  Evaluate(Purpose);
  NameOfFile := FileName(AFileName);
  WriteToNameFile(StrHOB, Model.UnitNumbers.UnitNumber(StrHOB), NameOfFile, foInput);
  if IUHOBSV <> 0 then
  begin
    OutFileName := ChangeFileExt(NameOfFile, '.hob_out');
    WriteToNameFile(StrDATA, IUHOBSV, OutFileName, foOutput);
  end;
  OpenFile(NameOfFile);
  try
    frmProgressMM.AddMessage('  Writing Data Set 0.');
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Set 1.');
    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Sets 3 to 6.');
    WriteDataSet2;
    for Index := 0 to FObservations.Count - 1 do
    begin
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      WriteDataSet3to6(Index);
    end;
  finally
    CloseFile;
  end;
end;

procedure TModflowHobWriter.WriteDataSet6(Observations: THobBoundary);
const
  IREFSP = 1;
var
  HOBS: Double;
  TOFFSET: Double;
  Item: THobItem;
  OBSNAM: string;
  ObsIndex: Integer;
  Comment: string;
begin
  if Observations.Values.Count > 1 then
  begin
    for ObsIndex := 0 to Observations.Values.Count - 1 do
    begin
      OBSNAM := Observations.ObservationName + '_' + IntToStr(ObsIndex + 1);
      if Length(OBSNAM) > 12 then
      begin
        OBSNAM := Observations.ObservationName + IntToStr(ObsIndex + 1);
      end;
      if Length(OBSNAM) > 12 then
      begin
        // The GUI is designed to prevent this from ever being required.
        SetLength(OBSNAM, 12);
      end;
      Item := Observations.Values.HobItems[ObsIndex];
      if (FStartTime <= Item.Time) and (Item.Time <= FEndTime) then
      begin
        TOFFSET := Item.Time - FStartTime;
        HOBS := Item.Head;
        WriteString(OBSNAM);
        WriteInteger(IREFSP);
        WriteFloat(TOFFSET);
        WriteFloat(HOBS);
        WriteString(' # Data Set 6: OBSNAM IREFSP TOFFSET HOBS');
        Comment := Item.Comment;
        if Comment <> '' then
        begin
          WriteString(' Comment = ' + Comment);
        end;
        NewLine;
      end;
    end;
  end;
end;

procedure TModflowHobWriter.WriteDataSet5(Observations: THobBoundary);
var
  ITT: Integer;
begin
  if IREFSP < 0 then
  begin
    ITT := Ord(Observations.MultiObsMethod) + 1;
    WriteInteger(ITT);
    WriteString(' # Data Set 5: ITT');
    NewLine;
  end;
end;

procedure TModflowHobWriter.WriteDataSet4(Observations: THobBoundary; CellList: TObsCellList);
var
  Total: Double;
  Item: TMultiHeadItem;
  ProportionIndex: Integer;
  SortIndex: Integer;
  LayerSort: TLayerSort;
  CellIndex: Integer;
  LayerSorter: TList;
  ActiveDataArray: TDataArray;
  Cell: THob_Cell;
  DeltaCol: Integer;
  DeltaRow: Integer;
  WarningMessage: string;
begin
  if CellList.Count > 1 then
  begin
    // When the absolute value of ROFF or COFF is less than 0.001,
    // MODFLOW-2000 and MODFLOW-2005 convert them to 0. 
    if Observations.Values.ObservationRowOffset < -0.001 then
    begin
      DeltaRow := -1;
    end
    else if Observations.Values.ObservationRowOffset > 0.001 then
    begin
      DeltaRow := 1;
    end
    else
    begin
      DeltaRow := 0;
    end;
    if Observations.Values.ObservationColumnOffset < -0.001 then
    begin
      DeltaCol := -1;
    end
    else if Observations.Values.ObservationColumnOffset > 0.001 then
    begin
      DeltaCol := 1;
    end
    else
    begin
      DeltaCol := 0;
    end;
    Cell := CellList[0];

    if (Cell.Row = 0) and (DeltaRow < 0) then
    begin
      DeltaRow := 0;
    end;
    if (Cell.Row = Model.Grid.RowCount - 1)
      and (DeltaRow > 0) then
    begin
      DeltaRow := 0;
    end;
    if (Cell.Column = 0) and (DeltaCol < 0) then
    begin
      DeltaCol := 0;
    end;
    if (Cell.Column = Model.Grid.ColumnCount - 1)
      and (DeltaCol > 0) then
    begin
      DeltaCol := 0;
    end;
    ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
    Assert(ActiveDataArray <> nil);
    ActiveDataArray.Initialize;
    LayerSorter := TObjectList.Create;
    try
      for CellIndex := 0 to CellList.Count - 1 do
      begin
        Cell := CellList[CellIndex];
        LayerSort := TLayerSort.Create;
        LayerSorter.Add(LayerSort);
        LayerSort.Layer := Cell.Layer;
        LayerSort.ActiveCells := 1;
        if not ActiveDataArray.BooleanData[Cell.Layer,
          Cell.Row, Cell.Column] then
        begin
          LayerSort.ActiveCells := -3;
        end;
        if DeltaRow <> 0 then
        begin
          if ActiveDataArray.BooleanData[Cell.Layer,
            Cell.Row + DeltaRow, Cell.Column] then
          begin
            LayerSort.ActiveCells := LayerSort.ActiveCells + 1;
          end;
        end;
        if DeltaCol <> 0 then
        begin
          if ActiveDataArray.BooleanData[Cell.Layer,
            Cell.Row, Cell.Column + DeltaCol] then
          begin
            LayerSort.ActiveCells := LayerSort.ActiveCells + 1;
          end;
        end;
        if (DeltaRow <> 0) and (DeltaCol <> 0) then
        begin
          if ActiveDataArray.BooleanData[Cell.Layer,
            Cell.Row + DeltaRow, Cell.Column + DeltaCol] then
          begin
            LayerSort.ActiveCells := LayerSort.ActiveCells + 1;
          end;
        end;
      end;
      for ProportionIndex := 0 to
        Observations.LayerFractions.Count - 1 do
      begin
        Item := Observations.LayerFractions[ProportionIndex];
        Item.Used := False;
      end;
      LayerSorter.Sort(SortLayerSorts);
      for SortIndex := 0 to LayerSorter.Count - 1 do
      begin
        LayerSort := LayerSorter[SortIndex];
        LayerSort.Proportion := 0;
        if LayerSort.ActiveCells > 0 then
        begin
          for ProportionIndex := 0 to
            Observations.LayerFractions.Count - 1 do
          begin
            Item := Observations.LayerFractions[ProportionIndex];
            if LayerSort.Layer+1 = Item.Layer then
            begin
              LayerSort.Proportion := Item.Proportion;
              Item.Used := True;
              break;
            end;
          end;
        end;
      end;
      Total := 0;
      for SortIndex := 0 to LayerSorter.Count - 1 do
      begin
        LayerSort := LayerSorter[SortIndex];
        Total := Total + LayerSort.Proportion;
      end;
      if Total > 0 then
      begin
        for SortIndex := 0 to LayerSorter.Count - 1 do
        begin
          LayerSort := LayerSorter[SortIndex];
          LayerSort.Proportion := LayerSort.Proportion / Total;
        end;
      end
      else
      begin
        for SortIndex := 0 to LayerSorter.Count - 1 do
        begin
          LayerSort := LayerSorter[SortIndex];
          LayerSort.Proportion := 1 / LayerSorter.Count;
        end;
      end;
      for SortIndex := 0 to LayerSorter.Count - 1 do
      begin
        LayerSort := LayerSorter[SortIndex];

        if LayerSort.Proportion = 0 then
        begin
          WarningMessage := 'In the head observation for '
            + (Observations.ScreenObject as TScreenObject).Name
            + ' the weight assigned to layer '
            + IntToStr(LayerSort.Layer+1) + ' is zero.';
          frmErrorsAndWarnings.AddWarning(Model, 'Head Observation Layer Weight = 0',
            WarningMessage);
        end;

        WriteInteger(LayerSort.Layer+1);
        WriteFloat(LayerSort.Proportion);
        if (((SortIndex + 1) mod 10) = 0)
          or (SortIndex = LayerSorter.Count - 1) then
        begin
          if (SortIndex = LayerSorter.Count - 1) then
          begin
            WriteString(' # Data Set 4: MLAY(1), PR(1), MLAY(2), '
              + 'PR(2), ..., MLAY(|LAYER|), PR(|LAYER|)');
          end;
          NewLine;
        end;
      end;
      for ProportionIndex := 0 to
        Observations.LayerFractions.Count - 1 do
      begin
        Item := Observations.LayerFractions[ProportionIndex];
        if not Item.Used then
        begin
          WarningMessage := 'In the head observation for '
            + (Observations.ScreenObject as TScreenObject).Name
            + ' a weight was assigned for layer '
            + IntToStr(Item.Layer)
            + ' but that layer is not part of the multilayer observation.';
          frmErrorsAndWarnings.AddWarning(Model, 'Head Observation Layer Weight incorrectly assigned',
            WarningMessage);
        end;
      end;
    finally
      LayerSorter.Free;
    end;
  end;
end;

procedure TModflowHobWriter.WriteDataSet3(Observations: THobBoundary; CellList: TObsCellList);
var
  ROFF: Double;
  COFF: Double;
  HOBS: Double;
  Cell: THob_Cell;
  OBSNAM: string;
  LAYER: Integer;
  ROW: Integer;
  COLUMN: Integer;
  TOFFSET: Double;
  Item: THobItem;
  ObservationTimeCount: Integer;
  ScreenObject: TScreenObject;
begin
  if CellList.Count = 0 then
  begin
    Exit;
  end;
  Cell := CellList[0];
  OBSNAM := Observations.ObservationName;
  if OBSNAM = '' then
  begin
    ScreenObject := Observations.ScreenObject as TScreenObject;
    frmErrorsAndWarnings.AddError(Model,
      MissingObsNameError, ScreenObject.Name);
  end;
  if not UcodeObsNameOK(OBSNAM) then
  begin
    ScreenObject := Observations.ScreenObject as TScreenObject;
    frmErrorsAndWarnings.AddWarning(Model, ObsNameWarning, OBSNAM
      + ' defined by object ' + ScreenObject.Name);
  end;
  if CellList.Count > 1 then
  begin
    LAYER := -CellList.Count;
  end
  else
  begin
    LAYER := Model.
      DataSetLayerToModflowLayer(Cell.Layer);
  end;
  ROW := Cell.Row+1;
  COLUMN := Cell.Column+1;
  ObservationTimeCount := Observations.Values.
    CountObservationTimes(FStartTime, FEndTime);
  if ObservationTimeCount = 1 then
  begin
    IREFSP := 1;
  end
  else
  begin
    IREFSP := -ObservationTimeCount;
  end;
  TOFFSET := Cell.Time - FStartTime;
  ROFF := Observations.Values.ObservationRowOffset;
  COFF := Observations.Values.ObservationColumnOffset;
  HOBS := Cell.Head;
  WriteString(OBSNAM);
  WriteInteger(LAYER);
  WriteInteger(ROW);
  WriteInteger(COLUMN);
  WriteInteger(IREFSP);
  WriteFloat(TOFFSET);
  WriteFloat(ROFF);
  WriteFloat(COFF);
  WriteFloat(HOBS);
  WriteString(' # Data Set 3: OBSNAM LAYER ROW COLUMN IREFSP TOFFSET ROFF COFF HOBS');
  Item := nil;
  if Observations.Values.Count > 0 then
  begin
    Item := Observations.Values.HobItems[0];
  end;
  if (Item <> nil) and (Item.Comment <> '') and (LAYER > 0) then
  begin
    WriteString(' Comment = ' + Item.Comment);
  end;
  NewLine;
end;

{ THobDisplayTimeList }

end.
