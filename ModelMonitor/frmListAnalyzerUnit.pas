unit frmListAnalyzerUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FileIndexUnit, Mask, JvExMask, JvSpin, ComCtrls,
  frameFileListHandlerUnit, ExtCtrls, Buttons, ImgList, Grids, RbwDataGrid4;

type
  TfrmMain = class(TForm)
    OpenDialog1: TOpenDialog;
    pgcIndex: TPageControl;
    tabIndex: TTabSheet;
    tabErrors: TTabSheet;
    tabWarnings: TTabSheet;
    frameWarning: TframeFileListHandler;
    frameErrors: TframeFileListHandler;
    frameListing: TframeFileListHandler;
    ProgressBar1: TProgressBar;
    Panel1: TPanel;
    Splitter1: TSplitter;
    btnOpenFile: TBitBtn;
    btnReadMoreLines: TBitBtn;
    btnReadEarlierLines: TBitBtn;
    spinLineCount: TJvSpinEdit;
    tmrReadFile: TTimer;
    lblLineCount: TLabel;
    ilTabFaces: TImageList;
    pgcDisplay: TPageControl;
    tabLines: TTabSheet;
    tabTable: TTabSheet;
    memoLines: TMemo;
    rdgTable: TRbwDataGrid4;
    procedure btnOpenFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmrReadFileTimer(Sender: TObject);
    procedure btnReadMoreLinesClick(Sender: TObject);
    procedure btnReadEarlierLinesClick(Sender: TObject);
    procedure frameListingredtIndexDblClick(Sender: TObject);
  private
    FListFile: TFileIndex;
    procedure ListFileProgress(Sender: TObject; PerMil: integer);
    procedure OpenFile(FileName: string);
    procedure DisplayObservations;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  ModflowIdentifiersUnit, Math, ErrorMessages, ExtractObservationsUnit;

{$R *.dfm}

type
  TObsColumns = (ocName, ocObserved, ocSimulated, ocDifference);

procedure TfrmMain.btnOpenFileClick(Sender: TObject);
var
  FileName: string;
begin
  if OpenDialog1.Execute then
  begin
    FileName := OpenDialog1.FileName;
    OpenFile(FileName);
  end;
end;

procedure TfrmMain.btnReadEarlierLinesClick(Sender: TObject);
begin
  case pgcIndex.ActivePageIndex of
    0:
      begin
        frameListing.InsertLines;
      end;
    1:
      begin
        frameWarning.InsertLines;
      end;
    2:
      begin
        frameErrors.InsertLines;
      end;
    else
      begin
        Assert(False);
      end;
  end;
end;

procedure TfrmMain.btnReadMoreLinesClick(Sender: TObject);
begin
  case pgcIndex.ActivePageIndex of
    0:
      begin
        frameListing.AddLines;
      end;
    1:
      begin
        frameWarning.AddLines;
      end;
    2:
      begin
        frameErrors.AddLines;
      end;
    else
      begin
        Assert(False);
      end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  frameWarning.Memo := memoLines;
  frameWarning.LineCountToRead := spinLineCount;

  frameErrors.Memo := memoLines;
  frameErrors.LineCountToRead := spinLineCount;

  frameListing.Memo := memoLines;
  frameListing.LineCountToRead := spinLineCount;

  if ParamCount > 0 then
  begin
    tmrReadFile.Enabled := True;
  end;

  Self.pgcDisplay.ActivePageIndex := 0;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FListFile.Free;
end;

procedure TfrmMain.frameListingredtIndexDblClick(Sender: TObject);
begin
  tabTable.TabVisible := False;
  tabLines.TabVisible := False;
  pgcDisplay.ActivePage := tabLines;
  frameListing.redtIndexDblClick(Sender);

  DisplayObservations;
end;

procedure TfrmMain.ListFileProgress(Sender: TObject; PerMil: integer);
begin
  ProgressBar1.Position := PerMil;
  Application.ProcessMessages;
end;

procedure TfrmMain.OpenFile(FileName: string);
var
  FoundID: Boolean;
  IdIndex: Integer;
  Divisor: Integer;
  ALine: string;
  LineIndex: Integer;
begin
  FListFile.Free;
  FListFile := TFileIndex.Create;
  frameWarning.ListFile := FListFile;
  frameErrors.ListFile := FListFile;
  frameListing.ListFile := FListFile;
  ProgressBar1.Max := 1000;
  FListFile.OnProgress := ListFileProgress;
  frameWarning.Initialize;
  frameErrors.Initialize;
  frameListing.Initialize;

  FListFile.FileName := FileName;
  Divisor := (FListFile.LineCount div 1000) + 1;
  ProgressBar1.Max := Divisor;
  ProgressBar1.Position := 0;

  frameListing.redtIndex.Lines.BeginUpdate;
  frameWarning.redtIndex.Lines.BeginUpdate;
  frameErrors.redtIndex.Lines.BeginUpdate;
  try
    for LineIndex := 0 to FListFile.LineCount - 1 do
    begin
      if (LineIndex mod 1000) = 0 then
      begin
        ProgressBar1.StepIt;
        Application.ProcessMessages;
      end;
      ALine := FListFile.ReadLine;
      for IdIndex := 0 to ErrorValues.Count - 1 do
      begin
        if Pos(ErrorValues[IdIndex], ALine) >= 1 then
        begin
          frameErrors.AddIndexLine(LineIndex, ALine);
          break;
        end;
      end;
      for IdIndex := 0 to WarningValues.Count - 1 do
      begin
        if Pos(WarningValues[IdIndex], ALine) >= 1 then
        begin
          frameWarning.AddIndexLine(LineIndex, ALine);
          break;
        end;
      end;
      if ((Pos(StressPeriodID1, ALine) >= 1) and (Pos(StressPeriodID2, ALine) >= 1)) then
      begin
        frameListing.AddIndexLine(LineIndex, ALine);
        Continue;
      end;
      if (Pos(StartNewTimeStep, ALine) >= 1) then
      begin
        frameListing.AddIndexLine(LineIndex, ALine);
        Continue;
      end;
      if ((Pos(IterationID1, ALine) >= 1) and (Pos(IterationID2, ALine) >= 1)) then
      begin
        frameListing.AddIndexLine(LineIndex, ALine);
        Continue;
      end;
      if ((Pos(ArrayID1, ALine) >= 1) and (Pos(ArrayID2, ALine) >= 1)) then
      begin
        frameListing.AddIndexLine(LineIndex, ALine);
        Continue;
      end;
      if (Pos(ArrayID3, ALine) >= 1) then
      begin
        frameListing.AddIndexLine(LineIndex, ALine);
        Continue;
      end;
      if (Pos(BudgetID, ALine) >= 1) then
      begin
        frameListing.AddIndexLine(LineIndex, ALine);
        Continue;
      end;
      if (Pos(TimeSummary, ALine) >= 1) then
      begin
        frameListing.AddIndexLine(LineIndex, ALine);
        Continue;
      end;
      FoundID := False;
      for IdIndex := 0 to PackageIdentifiers.Count - 1 do
      begin
        if Pos(PackageIdentifiers[IdIndex], ALine) >= 1 then
        begin
          frameListing.AddIndexLine(LineIndex, ALine);
          FoundID := True;
          break;
        end;
      end;
      if FoundID then
      begin
        Continue;
      end;
      for IdIndex := 0 to ObsIdentifiers.Count - 1 do
      begin
        if Pos(ObsIdentifiers[IdIndex], ALine) >= 1 then
        begin
          frameListing.AddIndexLine(LineIndex, ALine);
          FoundID := True;
          break;
        end;
      end;
      if FoundID then
      begin
        Continue;
      end;
      for IdIndex := 0 to BoundaryIdentifiers.Count - 1 do
      begin
        if Pos(BoundaryIdentifiers[IdIndex], ALine) >= 1 then
        begin
          frameListing.AddIndexLine(LineIndex, ALine);
          FoundID := True;
          break;
        end;
      end;
      if FoundID then
      begin
        Continue;
      end;
    end;
  finally
    frameListing.redtIndex.Lines.EndUpdate;
    frameWarning.redtIndex.Lines.EndUpdate;
    frameErrors.redtIndex.Lines.EndUpdate;
  end;
  pgcIndex.ActivePage := tabIndex;
  tabWarnings.TabVisible := frameWarning.redtIndex.Lines.Count > 0;
  if tabWarnings.TabVisible then
  begin
    pgcIndex.ActivePage := tabWarnings;
  end;
  tabErrors.TabVisible := frameErrors.redtIndex.Lines.Count > 0;
  if tabErrors.TabVisible then
  begin
    pgcIndex.ActivePage := tabErrors;
  end;
end;

procedure TfrmMain.DisplayObservations;
var
  FoundObservations: Boolean;
  ObservationLines: TStringList;
  SpacePos: Integer;
  ALine: string;
  LineIndex: Integer;
  Observations: TObsRecordArray;
  RowIndex: Integer;
  ColIndex: Integer;
  StartIndex: Integer;
begin
  FoundObservations := False;
  if frameListing.redtIndex.ActiveLineNo >= 0 then
  begin
    ALine := frameListing.redtIndex.Lines[frameListing.redtIndex.ActiveLineNo];
    SpacePos := Pos(' ', ALine);
    if SpacePos >= 1 then
    begin
      ALine := Trim(Copy(ALine, SpacePos + 1, MAXINT));
    end;
    if ObsIdentifiers.IndexOf(ALine) >= 0 then
    begin
      ObservationLines := TStringList.Create;
      try
        StartIndex := -1;
        for LineIndex := 0 to memoLines.Lines.Count - 1 do
        begin
          if Pos(ALine, memoLines.Lines[LineIndex]) >= 1 then
          begin
            StartIndex := LineIndex;
            Break;
          end;
        end;
        if StartIndex >= 0 then
        begin
          for LineIndex := StartIndex + 1 to memoLines.Lines.Count - 1 do
          begin
            if Pos('----', memoLines.Lines[LineIndex]) > 0 then
            begin
              StartIndex := LineIndex + 1;
              FoundObservations := StartIndex < memoLines.Lines.Count;
              break;
            end;
          end;
        end;
        if FoundObservations then
        begin
          FoundObservations := False;
          for LineIndex := StartIndex to memoLines.Lines.Count - 1 do
          begin
            ALine := Trim(memoLines.Lines[LineIndex]);
            if ALine = '' then
            begin
              FoundObservations := true;
              Break;
            end
            else
            begin
              ObservationLines.Add(ALine);
            end;
          end;
        end;
        if FoundObservations then
        begin
          ExtractObservations(ObservationLines, Observations);
          rdgTable.BeginUpdate;
          try
            rdgTable.ColCount := 4;
            rdgTable.RowCount := Length(Observations) + 1;
            for ColIndex := 0 to rdgTable.ColCount - 1 do
            begin
              rdgTable.Columns[ColIndex].AutoAdjustColWidths := True;
            end;
            rdgTable.Cells[Ord(ocName), 0] := 'Observations Name';
            rdgTable.Cells[Ord(ocObserved), 0] := 'Observed Value';
            rdgTable.Cells[Ord(ocSimulated), 0] := 'Simulated Value';
            rdgTable.Cells[Ord(ocDifference), 0] := 'Difference';
            for RowIndex := 1 to rdgTable.RowCount - 1 do
            begin
              rdgTable.Cells[Ord(ocName), RowIndex] := Observations[RowIndex - 1].Name;
              rdgTable.Cells[Ord(ocObserved), RowIndex] := FloatToStr(Observations[RowIndex - 1].ObservedValue);
              rdgTable.Cells[Ord(ocSimulated), RowIndex] := FloatToStr(Observations[RowIndex - 1].SimulatedValue);
              rdgTable.Cells[Ord(ocDifference), RowIndex] := FloatToStr(Observations[RowIndex - 1].Difference);
            end;
          finally
            rdgTable.EndUpdate;
          end;
          tabTable.TabVisible := True;
          tabLines.TabVisible := True;
        end;
      finally
        ObservationLines.Free;
      end;
    end;
  end;
end;

procedure TfrmMain.tmrReadFileTimer(Sender: TObject);
var
  FileName: string;
begin
  tmrReadFile.Enabled := False;
  if ParamCount > 0 then
  begin
    FileName := ParamStr(1);
    if FileExists(FileName) then
    begin
      OpenFile(ParamStr(1));
    end;
  end;

end;

end.
