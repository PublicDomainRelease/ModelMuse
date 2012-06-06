unit frmListAnalyzerUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FileIndexUnit, Mask, JvExMask, JvSpin, ComCtrls,
  frameFileListHandlerUnit, ExtCtrls, Buttons, ImgList, Grids, RbwDataGrid4,
  ArgusDataEntry, JvExExtCtrls, JvNetscapeSplitter, JvExStdCtrls;

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
    rdgTable: TRbwDataGrid4;
    btnIndex: TButton;
    Panel2: TPanel;
    lblFileCount: TLabel;
    rdeLineTarget: TRbwDataEntry;
    btnGoTo: TButton;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    sbLines: TScrollBar;
    btnAbort: TBitBtn;
    FindDialog1: TFindDialog;
    btnFind: TBitBtn;
    RichEditLines: TRichEdit;
    procedure btnOpenFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmrReadFileTimer(Sender: TObject);
    procedure btnReadMoreLinesClick(Sender: TObject);
    procedure btnReadEarlierLinesClick(Sender: TObject);
    procedure frameListingredtIndexDblClick(Sender: TObject);
    procedure btnGoToClick(Sender: TObject);
    procedure btnIndexClick(Sender: TObject);
    procedure sbLinesScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure memoLinesClick(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
  private
    FListFile: TFileIndex;
    FChangingPosition: Boolean;
    procedure ListFileProgress(Sender: TObject; PerMil: integer);
    procedure OpenFile(FileName: string);
    procedure DisplayObservations;
    procedure IndexFile;
    procedure FilePositionChanged(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  ModflowIdentifiersUnit, Math, ErrorMessages, ExtractObservationsUnit,
  BMSearch;

{$R *.dfm}

type
  TObsColumns = (ocName, ocObserved, ocSimulated, ocDifference);

procedure TfrmMain.btnAbortClick(Sender: TObject);
begin
  FListFile.Abort;
end;

procedure TfrmMain.btnFindClick(Sender: TObject);
begin
  FindDialog1.Execute;
end;

procedure TfrmMain.btnGoToClick(Sender: TObject);
var
  LineNumber: Integer;
begin
  LineNumber := StrToInt(rdeLineTarget.Text)-1;
  case pgcIndex.ActivePageIndex of
    0:
      begin
        frameListing.GoToLine(LineNumber);
      end;
    1:
      begin
        frameErrors.GoToLine(LineNumber);
      end;
    2:
      begin
        frameWarning.GoToLine(LineNumber);
      end;
  end;
end;

procedure TfrmMain.btnIndexClick(Sender: TObject);
begin
  IndexFile;
  btnIndex.Enabled := False;
  ProgressBar1.Position := 0;
end;

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
        frameErrors.InsertLines;
      end;
    2:
      begin
        frameWarning.InsertLines;
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
    2:
      begin
        frameErrors.AddLines;
      end;
    1:
      begin
        frameWarning.AddLines;
      end;
    else
      begin
        Assert(False);
      end;
  end;
end;

procedure TfrmMain.FilePositionChanged(Sender: TObject);
begin
  if FChangingPosition then
  begin
    Exit;
  end;
  FChangingPosition := True;
  try
    sbLines.Position := Round(FListFile.CurrentStartLine
      /FListFile.LineCount*sbLines.Max);
  finally
    FChangingPosition := False;
  end;
end;

procedure TfrmMain.FindDialog1Find(Sender: TObject);
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];
var
  FoundAt: LongInt;
  StartPos, ToEnd: Integer;
  mySearchTypes : TSearchTypes;
  myFindOptions : TFindOptions;
  NextLineIndex: integer;
  ALine: string;
  SearchTerm: string;
  StringB : TStringBuilder;
  Index: integer;
  StartLine: integer;
  WordPosition: integer;
begin
  mySearchTypes := [];
  with RichEditLines do
  begin
    if frMatchCase in FindDialog1.Options then
       mySearchTypes := mySearchTypes + [stMatchCase];
    if frWholeWord in FindDialog1.Options then
       mySearchTypes := mySearchTypes + [stWholeWord];
    { Begin the search after the current selection, if there is one. }
    { Otherwise, begin at the start of the text. }
    if SelLength <> 0 then
      StartPos := SelStart + SelLength
    else
      StartPos := 0;
    { ToEnd is the length from StartPos through the end of the 
      text in the rich edit control. }
    ToEnd := Length(Text) - StartPos;
    FoundAt := 
      FindText(FindDialog1.FindText, StartPos, ToEnd, mySearchTypes);
    if FoundAt <> -1 then
    begin
      SetFocus;
      SelStart := FoundAt;
      SelLength := Length(FindDialog1.FindText);
      memoLinesClick(nil);
      
    end
    else
    begin
      SearchTerm := FindDialog1.FindText;
      if not (frMatchCase in FindDialog1.Options) then
      begin
        SearchTerm := UpperCase(SearchTerm);
      end;

      StringB := TStringBuilder.Create;
      try
        NextLineIndex := FListFile.CurrentStartLine
          + RichEditLines.Lines.Count;
        
        while NextLineIndex < FListFile.LineCount do
        begin
          StartLine := NextLineIndex;
          StringB.Clear;
          for Index := 0 to 25 do
          begin
            ALine := FListFile[NextLineIndex];
            StringB.Append(ALine + sLineBreak);
            Inc(NextLineIndex);
            if NextLineIndex >= FListFile.LineCount then
            begin
              break;
            end;
          end;
          ALine := StringB.ToString;
          if not (frMatchCase in FindDialog1.Options) then
          begin
            ALine := UpperCase(ALine);
          end;
          WordPosition := BmPosSimple(SearchTerm, ALine);
          if WordPosition > 0 then
          begin
            if frWholeWord in FindDialog1.Options then
            begin
              if (WordPosition > 1) 
                and not (SearchTerm[WordPosition-1] in AlphaNumeric) then
              begin
                Continue;
              end;
            end;
            WordPosition := WordPosition + Length(SearchTerm)+1;
              if (WordPosition <= Length(ALine)) 
                and not (SearchTerm[WordPosition-1] in AlphaNumeric) then
              begin
                Continue;
              end;
            begin
              
            end;
            frameListing.GoToLine(StartLine);
            FindDialog1Find(Sender);
            Exit;
          end;
        end;
        Beep;
      finally
        StringB.Free;
      end;
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  frameWarning.Memo := RichEditLines;
  frameWarning.LineCountToRead := spinLineCount;

  frameErrors.Memo := RichEditLines;
  frameErrors.LineCountToRead := spinLineCount;

  frameListing.Memo := RichEditLines;
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

procedure TfrmMain.memoLinesClick(Sender: TObject);
begin
  lblFileCount.Caption := IntToStr(1 + RichEditLines.CaretPos.Y 
    + FListFile.CurrentStartLine);
end;

procedure TfrmMain.OpenFile(FileName: string);
var
  Divisor: Integer;
begin

  FListFile.Free;
  FListFile := TFileIndex.Create;
  frameWarning.ListFile := FListFile;
  frameErrors.ListFile := FListFile;
  frameListing.ListFile := FListFile;
  ProgressBar1.Max := 1000;
  FListFile.OnProgress := ListFileProgress;
  FListFile.OnStartLineChange := FilePositionChanged;
  frameWarning.Initialize;
  frameErrors.Initialize;
  frameListing.Initialize;

  FListFile.FileName := FileName;
  Divisor := (FListFile.LineCount div 1000) + 1;
  ProgressBar1.Max := Divisor;
  lblFileCount.Caption := IntToStr(FListFile.LineCount);
  rdeLineTarget.Max := FListFile.LineCount;
  btnIndex.Enabled := True;
  btnGoTo.Enabled := True;
  btnFind.Enabled := True;
  frameListing.GoToLine(0);
  progressbar1.Position := 0;
end;

procedure TfrmMain.sbLinesScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  if FChangingPosition then
  begin
    Exit;
  end;
  FChangingPosition := True;
  try
    frameListing.GoToLine(FListFile.LineCount div sbLines.Max * ScrollPos );
    lblFileCount.Caption := IntToStr(1 + FListFile.CurrentStartLine);
  finally
    FChangingPosition := False;
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
//  if frameListing.redtIndex.ActiveLineNo >= 0 then
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
        for LineIndex := 0 to RichEditLines.Lines.Count - 1 do
        begin
          if Pos(ALine, RichEditLines.Lines[LineIndex]) >= 1 then
          begin
            StartIndex := LineIndex;
            Break;
          end;
        end;
        if StartIndex >= 0 then
        begin
          for LineIndex := StartIndex + 1 to RichEditLines.Lines.Count - 1 do
          begin
            if Pos('----', RichEditLines.Lines[LineIndex]) > 0 then
            begin
              StartIndex := LineIndex + 1;
              FoundObservations := StartIndex < RichEditLines.Lines.Count;
              break;
            end;
          end;
        end;
        if FoundObservations then
        begin
          FoundObservations := False;
          for LineIndex := StartIndex to RichEditLines.Lines.Count - 1 do
          begin
            ALine := Trim(RichEditLines.Lines[LineIndex]);
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

procedure TfrmMain.IndexFile;
var
  AString: AnsiString;
  LineIndex: Integer;
  InnerLine: string;
  Lines: TStringList;
  StringB: TStringBuilder;
  InnerIndex: Integer;
  FoundID: Boolean;
  ALine: string;
  KeyLineFound: Boolean;
  IdIndex: Integer;
  StartIndex: Integer;
begin
  FListFile.GoToStart;
  StringB := TStringBuilder.Create;
  Lines := TStringList.Create;
  try
    ProgressBar1.Position := 0;
    StringB.Capacity := 200000;
    StartIndex := 0;
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
        StringB.Append(ALine);
        StringB.Append(sLineBreak);
        if (((LineIndex + 1) mod 25) = 0)
          or (LineIndex = FListFile.LineCount - 1) then
        begin
          AString := StringB.ToString;
          KeyLineFound := False;
          for IdIndex := 0 to ErrorValues.Count - 1 do
          begin
            if BmPosSimple(ErrorValues[IdIndex], AString) >= 1 then
            begin
              KeyLineFound := True;
              break;
            end;
          end;
          if not KeyLineFound then
          begin
            for IdIndex := 0 to WarningValues.Count - 1 do
            begin
              if BmPosSimple(WarningValues[IdIndex], AString) >= 1 then
              begin
                KeyLineFound := True;
                break;
              end;
            end;
          end;
          if not KeyLineFound then
          begin
            if ((BmPosSimple(StressPeriodID1, AString) >= 1) and (BmPosSimple(StressPeriodID2, AString) >= 1)) then
            begin
              KeyLineFound := True;
            end
            else if (BmPosSimple(StartNewTimeStep, AString) >= 1) then
            begin
              KeyLineFound := True;
            end
            else if ((BmPosSimple(IterationID1, AString) >= 1) and (BmPosSimple(IterationID2, AString) >= 1)) then
            begin
              KeyLineFound := True;
            end
            else if ((BmPosSimple(ArrayID1, AString) >= 1) and (BmPosSimple(ArrayID2, AString) >= 1)) then
            begin
              KeyLineFound := True;
            end
            else if (BmPosSimple(ArrayID3, AString) >= 1) then
            begin
              KeyLineFound := True;
            end
            else if (BmPosSimple(BudgetID, AString) >= 1) then
            begin
              KeyLineFound := True;
            end
            else if (BmPosSimple(TimeSummary, AString) >= 1) then
            begin
              KeyLineFound := True;
            end;
            if not KeyLineFound then
            begin
              for IdIndex := 0 to PackageIdentifiers.Count - 1 do
              begin
                if BmPosSimple(PackageIdentifiers[IdIndex], AString) >= 1 then
                begin
                  KeyLineFound := True;
                  break;
                end;
              end;
            end;
            if not KeyLineFound then
            begin
              for IdIndex := 0 to ObsIdentifiers.Count - 1 do
              begin
                if Pos(ObsIdentifiers[IdIndex], AString) >= 1 then
                begin
                  KeyLineFound := True;
                  break;
                end;
              end;
            end;
            if not KeyLineFound then
            begin
              for IdIndex := 0 to BoundaryIdentifiers.Count - 1 do
              begin
                if BmPosSimple(BoundaryIdentifiers[IdIndex], AString) >= 1 then
                begin
                  KeyLineFound := True;
                  break;
                end;
              end;
            end;
          end;
          if KeyLineFound then
          begin
            Lines.Text := AString;
            for InnerIndex := 0 to Lines.Count - 1 do
            begin
              InnerLine := Lines[InnerIndex];
              for IdIndex := 0 to ErrorValues.Count - 1 do
              begin
                if Pos(ErrorValues[IdIndex], InnerLine) >= 1 then
                begin
                  frameErrors.AddIndexLine(StartIndex + InnerIndex, InnerLine);
                  break;
                end;
              end;
              for IdIndex := 0 to WarningValues.Count - 1 do
              begin
                if Pos(WarningValues[IdIndex], InnerLine) >= 1 then
                begin
                  frameWarning.AddIndexLine(StartIndex + InnerIndex, InnerLine);
                  break;
                end;
              end;
              if ((Pos(StressPeriodID1, InnerLine) >= 1) and (Pos(StressPeriodID2, InnerLine) >= 1)) then
              begin
                frameListing.AddIndexLine(StartIndex + InnerIndex, InnerLine);
                Continue;
              end;
              if (Pos(StartNewTimeStep, InnerLine) >= 1) then
              begin
                frameListing.AddIndexLine(StartIndex + InnerIndex, InnerLine);
                Continue;
              end;
              if ((Pos(IterationID1, InnerLine) >= 1) and (Pos(IterationID2, InnerLine) >= 1)) then
              begin
                frameListing.AddIndexLine(StartIndex + InnerIndex, InnerLine);
                Continue;
              end;
              if ((Pos(ArrayID1, InnerLine) >= 1) and (Pos(ArrayID2, InnerLine) >= 1)) then
              begin
                frameListing.AddIndexLine(StartIndex + InnerIndex, InnerLine);
                Continue;
              end;
              if (Pos(ArrayID3, InnerLine) >= 1) then
              begin
                frameListing.AddIndexLine(StartIndex + InnerIndex, InnerLine);
                Continue;
              end;
              if (Pos(BudgetID, InnerLine) >= 1) then
              begin
                frameListing.AddIndexLine(StartIndex + InnerIndex, InnerLine);
                Continue;
              end;
              if (Pos(TimeSummary, InnerLine) >= 1) then
              begin
                frameListing.AddIndexLine(StartIndex + InnerIndex, InnerLine);
                Continue;
              end;
              FoundID := False;
              for IdIndex := 0 to PackageIdentifiers.Count - 1 do
              begin
                if Pos(PackageIdentifiers[IdIndex], InnerLine) >= 1 then
                begin
                  frameListing.AddIndexLine(StartIndex + InnerIndex, InnerLine);
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
                if Pos(ObsIdentifiers[IdIndex], InnerLine) >= 1 then
                begin
                  frameListing.AddIndexLine(StartIndex + InnerIndex, InnerLine);
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
                if Pos(BoundaryIdentifiers[IdIndex], InnerLine) >= 1 then
                begin
                  frameListing.AddIndexLine(StartIndex + InnerIndex, InnerLine);
                  FoundID := True;
                  break;
                end;
              end;
              if FoundID then
              begin
                Continue;
              end;
            end;
          end;
          StringB.Clear;
          StringB.Capacity := 200000;
          StartIndex := LineIndex + 1;
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
  finally
    StringB.Free;
    Lines.Free;
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
