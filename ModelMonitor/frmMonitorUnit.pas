unit frmMonitorUnit;

interface

{
  1.0.2.0 Fixed bug that caused ModelMonitor to work improperly when the
    decimal separator was not a period in the local language settings.
}

uses
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, JvExExtCtrls, JvImage, ImgList, JvImageList,
  TeEngine, Series, TeeProcs, Chart, ComCtrls, Buttons, JvNavigationPane,
  JvSplitter, JvSyncSplitter, JvExtComponent, JvSplit, Mask, JvExMask,
  JvToolEdit, JvExStdCtrls, JvRichEdit, JclFileUtils, AppEvnts, RealListUnit,
  JvHtControls;

type
  TStringFileStream = class(TFileStream)
  private
    FBuffer: array[0..1000] of char;
    TempLine: string;
    FEOF: boolean;
    function GetEOF: boolean;
  public
    function ReadLn: string;
    property EOF: boolean read GetEOF;
  end;
  
  TfrmMonitor = class(TForm)
    jilBigFaces: TJvImageList;
    timerReadOutput: TTimer;
    pnlLeft: TPanel;
    jimageStatus: TJvImage;
    imTabFaces: TImageList;
    AppEvents: TApplicationEvents;
    pnlMain: TPanel;
    pnlBottom: TPanel;
    btnRun: TBitBtn;
    pcMain: TPageControl;
    tabConfiguration: TTabSheet;
    lblModelName: TLabel;
    lblNameFile: TLabel;
    jvfeModelName: TJvFilenameEdit;
    jvfeNameFile: TJvFilenameEdit;
    tabListing: TTabSheet;
    tabResults: TTabSheet;
    chartPercentDiscrepancy: TChart;
    serCumulative: TLineSeries;
    serTimeStep: TLineSeries;
    tabMonitor: TTabSheet;
    lblMonitor: TLabel;
    reMonitor: TJvRichEdit;
    lblListingFile: TLabel;
    reListing: TJvRichEdit;
    timerStartFromCommandParameters: TTimer;
    lblModelDone: TLabel;
    tabAbout: TTabSheet;
    ImageLogo: TImage;
    lblDeveloperName: TLabel;
    lblGoPhast: TLabel;
    JvHTLabel1: TJvHTLabel;
    reReference: TRichEdit;
    JvHTLabel2: TJvHTLabel;
    lblVersion: TLabel;
    procedure btnRunClick(Sender: TObject);
    procedure timerReadOutputTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure jvfeModelNameChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AppEventsIdle(Sender: TObject; var Done: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure timerStartFromCommandParametersTimer(Sender: TObject);
  private
    FListingFile: string;
    FErrorPositions: TIntegerDynArray;
    FWarningPositions: TIntegerDynArray;
    FListTextReader: TStringFileStream;
    FMonitorTextReader: TStringFileStream;
    FLineCount: Integer;
    FShouldAbort: Boolean;
    FModelFinished: Boolean;
    FStartTime: Extended;
    FReading1: Boolean;
    FReading2: Boolean;
    {$IFDEF MakeBatchFile}
    FOutFile: string;
    {$ENDIF}
    FAlreadyStarted: Boolean;
    FPercentRate: TRealList;
    FPercentCumulative: TRealList;
    FActivated: Boolean;
    FStartPlotTime: TDateTime;
    FDone: Boolean;
    procedure GetListFile(var ListFile: string);
    procedure FindStart(RichEdit: TJvRichEdit; PositionInLine: integer;
      out SelStart: integer);
    procedure CreateFileReaders;
    procedure HandleListFileLine(ALine: string);
    procedure IndentifyProblem(ALine: string; var IsProblem: Boolean;
      NewIndex: Integer; var Positions: TIntegerDynArray;
      KeyTerms: TStringList);
    procedure HandleProblem(IsError: Boolean; AColor: TColor; Positions: TIntegerDynArray; EV: TStringList);
    procedure GetColor(NewIndex: Integer; Value: Double; var AColor: TColor);
    procedure StorePercentDiscrepancy(ALine: string);
    function WinExecAndWait32(FileName: string; Visibility: Integer): Longword;
    procedure HandleMonitorFileLine(ALine: string);
    procedure ReadCommandLine;
    function PlotPercentDiscrepancy : boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMonitor: TfrmMonitor;

implementation

uses {ShellApi,} ErrorMessages, forceforeground, JvVersionInfo;

resourcestring
  StrPERCENTDISCREPANCY = 'PERCENT DISCREPANCY =';
  StrMFOuttxt = 'MF_Out.txt';
  StrNormalTermination = 'Normal termination of simulation';
  StrFailureToConverge = 'Failure to converge';
  StrStopMonitoringMode = 'Stop monitoring model';
  StrStartModel = 'Re-Start model';
  StrFIRSTENTRYINNAME = 'FIRST ENTRY IN NAME FILE MUST BE "LIST".';
  StrFAILEDTOMEETSOLVE = 'FAILED TO MEET SOLVER CONVERGENCE CRITERIA';

{$R *.dfm}

// from http://www.swissdelphicenter.ch/torry/showcode.php?id=93                                 
function TfrmMonitor.WinExecAndWait32(FileName: string; Visibility: Integer): Longword;
var { by Pat Ritchey }
  zAppName: array[0..512] of Char;
  zCurDir: array[0..255] of Char;
  WorkDir: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  StrPCopy(zAppName, FileName);
  GetDir(0, WorkDir);
  StrPCopy(zCurDir, WorkDir);
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  StartupInfo.cb          := SizeOf(StartupInfo);
  StartupInfo.dwFlags     := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  if not CreateProcess(nil,
    zAppName, // pointer to command line string
    nil, // pointer to process security attributes
    nil, // pointer to thread security attributes
    False, // handle inheritance flag
    CREATE_NEW_CONSOLE or // creation flags
    NORMAL_PRIORITY_CLASS,
    nil, //pointer to new environment block
    nil, // pointer to current directory name
    StartupInfo, // pointer to STARTUPINFO
    ProcessInfo) // pointer to PROCESS_INF
    then Result := WAIT_FAILED
  else
  begin
    repeat
      if FShouldAbort then
      begin
        TerminateProcess(ProcessInfo.hProcess,0);
        break;
      end;
      if FListingFile = '' then
      begin
        break;
      end;
      Application.ProcessMessages;
      Sleep(50);
    until WaitForSingleObject(ProcessInfo.hProcess, 100) <> WAIT_TIMEOUT;
    if not GetExitCodeProcess(ProcessInfo.hProcess, Result) then
    begin
      RaiseLastOSError;
    end;
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end;
end; { WinExecAndWait32 }


procedure TfrmMonitor.AppEventsIdle(Sender: TObject;
  var Done: Boolean);
var
  ALine: string;
begin
  CreateFileReaders;
  FReading1 := True;
  try
    if FListTextReader = nil then
    begin
      Exit;
    end;

    while (FListingFile <> '')
      and ((FListTextReader.Position < FListTextReader.Size-1)
      or not FListTextReader.EOF) do
    begin
      if (FListingFile = '') then
      begin
        Exit;
      end;
      ALine := FListTextReader.ReadLn;
      if (ALine <> '') or not FListTextReader.EOF then
      begin
        HandleListFileLine(ALine);
      end;
      Application.ProcessMessages;
      FStartTime := Now;
    end;
    PlotPercentDiscrepancy;
    while (FOutFile <> '') and FileExists(FOutFile)
      and (FMonitorTextReader <> nil)
      and ((FMonitorTextReader.Position < FMonitorTextReader.Size-1)
      or not FMonitorTextReader.EOF)  do
    begin
      if (FOutFile = '') then
      begin
        Exit;
      end;
      ALine := FMonitorTextReader.ReadLn;
      if (ALine <> '') or not FMonitorTextReader.EOF then
      begin
        HandleMonitorFileLine(ALine);
      end;
      Application.ProcessMessages;
      FStartTime := Now;
    end;
    if FModelFinished then
    begin
        FListingFile := '';
        FOutFile := '';
        timerReadOutput.Enabled := False;
        FreeAndNil(FListTextReader);
        FreeAndNil(FMonitorTextReader);
        Exit;
    end;
    jimageStatus.Picture.Assign(jilBigFaces.Items[jimageStatus.Tag].Bitmap);
    Application.ProcessMessages;
  finally
    FReading1 := False;
  end;
end;

procedure TfrmMonitor.btnRunClick(Sender: TObject);
var
  CommandLine: string;
  FileDir: string;
  FileName: string;
  ListFile: string;
  NormalTermination: Boolean;
  Index: Integer;
  {$IFDEF MakeBatchFile}
  BatFile: TStringList;
  ProcessExitCode: Cardinal;
  {$ENDIF}
begin
  if timerReadOutput.Enabled then
  begin
    btnRun.Caption := StrStartModel;
    FShouldAbort := True;
    timerReadOutput.Enabled := False;
    btnRun.Glyph := jilBigFaces.Items[3].Bitmap;
    FListingFile := '';
  end
  else
  begin
    if not FileExists(jvfeModelName.FileName)
      or not FileExists(jvfeNameFile.FileName) then
    begin
      Beep;
      Exit;
    end;

    chartPercentDiscrepancy.LeftAxis.Automatic := False;
    chartPercentDiscrepancy.LeftAxis.Minimum := -1;
    chartPercentDiscrepancy.LeftAxis.Maximum := 1;

    FActivated := False;
    FModelFinished := False;
    FDone := False;
    btnRun.Glyph := jilBigFaces.Items[4].Bitmap;

    CommandLine := jvfeModelName.FileName;
    if Pos(' ', CommandLine) > 0 then
    begin
      CommandLine := '"' + CommandLine + '"';
    end;
    FileDir := ExtractFileDir(jvfeNameFile.FileName);
    FileName := ExtractFileName(jvfeNameFile.FileName);
    SetCurrentDir(FileDir);
    FileDir := IncludeTrailingPathDelimiter(FileDir);
    CommandLine := CommandLine + ' ' + FileName;
    {$IFDEF MakeBatchFile}
    FOutFile := FileDir + StrMFOuttxt;
    if FileExists(FOutFile) then
    begin
      DeleteFile(FOutFile);
    end;
    if LowerCase(ExtractFileExt(jvfeModelName.FileName)) <> '.bat' then
    begin
      CommandLine := CommandLine + ' >' + StrMFOuttxt;
      BatFile := TStringList.Create;
      try
        BatFile.Add(CommandLine);
        CommandLine := FileDir + 'MF_Run.bat';
        BatFile.SaveToFile(CommandLine);
      finally
        BatFile.Free;
      end;
    end;
    If Pos(' ', CommandLine) > 0 then
    begin
      CommandLine := '"' + CommandLine + '"';
    end;
    {$ENDIF}

    GetListFile(ListFile);
    if ListFile = '' then
    begin
      Beep;
      MessageDlg('No list file in name file.', mtError, [mbOK], 0);
      Exit;
    end;
    FListingFile := ExpandFileName(ListFile);

    btnRun.Caption := StrStopMonitoringMode;
    serCumulative.Clear;
    serTimeStep.Clear;
    jimageStatus.Tag := 0;
    jimageStatus.Picture.Assign(jilBigFaces.Items[0].Bitmap);

    tabListing.ImageIndex := 0;
    tabResults.ImageIndex := 0;
    tabMonitor.ImageIndex := 0;
    FLineCount := 0;
    FShouldAbort := False;

    reMonitor.Lines.Clear;
    reListing.Lines.Clear;

    lblMonitor.Caption := 'Screen output';
    lblMonitor.Font.Color := clBlack;
    lblMonitor.Font.Style := [];

    pcMain.ActivePage := tabResults;
    timerReadOutput.Enabled := True;
    ProcessExitCode := WinExecAndWait32(CommandLine, SW_SHOW);
    if ProcessExitCode <> 0 then
    begin
      if ProcessExitCode = WAIT_FAILED then
      begin
        raise Exception.Create('Error waiting for program to start.');
      end
      else
      begin
        raise Exception.Create('The program terminated with an error.');
      end;
    end;

    lblModelDone.Visible := True;

    while not FShouldAbort and (FMonitorTextReader <> nil) and
      ((FMonitorTextReader.Position < FMonitorTextReader.Size-1)
      or not FMonitorTextReader.EOF) do
    begin
      Application.ProcessMessages;
      Sleep(20);
    end;

    FDone := True;

    if FListingFile <> '' then
    begin
      while (FListTextReader = nil) or
        ((FListTextReader.Position < FListTextReader.Size-1)
        or not FListTextReader.EOF) do
      begin
        timerReadOutputTimer(nil);
      end;
      while (FMonitorTextReader = nil) or
        ((FMonitorTextReader.Position < FMonitorTextReader.Size-1)
        or not FMonitorTextReader.EOF) do
      begin
        timerReadOutputTimer(nil);
      end;
    end;

    NormalTermination := False;
    for Index := 0 to reMonitor.Lines.Count - 1 do
    begin
      if Pos(StrNormalTermination, reMonitor.Lines[Index]) > 0 then
      begin
        NormalTermination := True;
        break;
      end;
    end;
    
    if not FShouldAbort and not NormalTermination then
    begin
      lblMonitor.Caption := 'Screen output' + #13#10
        + 'Program failed to terminate normally';
      lblMonitor.Font.Color := clRed;
      lblMonitor.Font.Style := [fsBold];
      tabMonitor.ImageIndex := 2;
      jimageStatus.Tag := 2;
      jimageStatus.Picture.Assign(jilBigFaces.Items[jimageStatus.Tag].Bitmap);
    end;

    while not FShouldAbort and (FListTextReader <> nil) and
      ((FListTextReader.Position < FListTextReader.Size-1)
      or not FListTextReader.EOF) do
    begin
      Application.ProcessMessages;
      Sleep(20);
    end;
    lblModelDone.Visible := False;

    btnRun.Caption := StrStartModel;
    btnRun.Glyph := jilBigFaces.Items[3].Bitmap;
    FStartTime := Now;
    FModelFinished := True;
  end;
end;

procedure TfrmMonitor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  timerReadOutput.Enabled := False;
  AppEvents.OnIdle := nil;
  FListingFile := '';
  while FReading1 or FReading2 do
  begin
    // wait
  end;
  FreeAndNil(FListTextReader);
  FreeAndNil(FMonitorTextReader);
end;

type
  TCommandLineState = (clsUnknown, clsModelName, clsNameFile);

procedure TfrmMonitor.FormCreate(Sender: TObject);
var
  VerInfo: TJvVersionInfo;
begin
  VerInfo := TJvVersionInfo.Create(Application.ExeName);
  try
    lblVersion.Caption := 'Version: ' + VerInfo.FileVersion;
  finally
    VerInfo.Free;
  end;

  SetLength(FErrorPositions, ErrorValues.Count);
  SetLength(FWarningPositions, WarningValues.Count);

  jvfeModelNameChange(jvfeModelName);
  jvfeModelNameChange(jvfeNameFile);
  pcMain.ActivePageIndex := 0;
  FPercentRate := TRealList.Create;
  FPercentCumulative := TRealList.Create;
  FStartPlotTime  := 0;

  reReference.WordWrap := True;
end;

procedure TfrmMonitor.FormDestroy(Sender: TObject);
begin
  FPercentCumulative.Free;
  FPercentRate.Free;
  FListingFile := '';
  FreeAndNil(FListTextReader);
  FreeAndNil(FMonitorTextReader);
end;

procedure TfrmMonitor.FormShow(Sender: TObject);
begin
  timerStartFromCommandParameters.Enabled := True;
end;

procedure TfrmMonitor.jvfeModelNameChange(Sender: TObject);
var
  FileEdit: TJvFilenameEdit;
begin
  FileEdit := Sender as TJvFilenameEdit;
  if FileExists(FileEdit.FileName) then
  begin
    FileEdit.Color := clWindow;
  end
  else
  begin
    FileEdit.Color := clRed;
  end;
end;

procedure TfrmMonitor.FindStart(RichEdit: TJvRichEdit; PositionInLine: integer;
  out SelStart: integer);
var
  LineIndex: Integer;
begin
  SelStart := -1;
  for LineIndex := 0 to RichEdit.Lines.Count - 2 do
  begin
    SelStart := SelStart + Length(RichEdit.Lines[LineIndex]) + 1;
  end;
  SelStart := SelStart + PositionInLine;
end;

procedure TfrmMonitor.GetListFile(var ListFile: string);
var
  ALine: string;
  LineList: TStringList;
  NameFile: TStringList;
  Index: Integer;
begin
  ListFile := '';
  NameFile := TStringList.Create;
  try
    NameFile.LoadFromFile(jvfeNameFile.FileName);
    for Index := 0 to NameFile.Count - 1 do
    begin
      ALine := UpperCase(Trim(NameFile[Index]));
      if Pos('LIST', ALine) > 0 then
      begin
        LineList := TStringList.Create;
        try
          LineList.Delimiter := ' ';
          LineList.DelimitedText := ALine;
          if LineList.Count < 3 then
          begin
            Continue;
          end;
          if LineList[0] <> 'LIST' then
          begin
            Continue;
          end;
          ListFile := LineList[2];
          break;
        finally
          LineList.Free;
        end;
      end;
    end;
  finally
    NameFile.Free;
  end;
end;

procedure TfrmMonitor.CreateFileReaders;
begin
  if FListingFile = '' then
  begin
    FreeAndNil(FListTextReader);
  end;
  if (FListTextReader = nil) and FileExists(FListingFile) then
  begin
    FListTextReader := TStringFileStream.Create(FListingFile,
      fmOpenRead or fmShareDenyNone);
  end;
  if FOutFile = '' then
  begin
    FreeAndNil(FMonitorTextReader);
  end;
  if (FMonitorTextReader = nil) and FileExists(FOutFile) then
  begin
    FMonitorTextReader := TStringFileStream.Create(FOutFile,
      fmOpenRead or fmShareDenyNone);
  end;
end;

procedure TfrmMonitor.HandleMonitorFileLine(ALine: string);
var
  Position: integer;
  SelStart: Integer;
  OldDecimalSeparator: Char;
  OldThousandSeparator: Char;
begin
  if (FOutFile = '') then
  begin
    Exit;
  end;
  OldDecimalSeparator := DecimalSeparator;
  OldThousandSeparator := ThousandSeparator;
  try
    DecimalSeparator := '.';
    ThousandSeparator := ',';
    reMonitor.Lines.Add(ALine);
    Position := Pos(StrNormalTermination, ALine);
    if Position > 0 then
    begin
      FindStart(reMonitor, Position, SelStart);
      reMonitor.SetSelection(SelStart,
        SelStart + Length(StrNormalTermination), True);
      reMonitor.SelAttributes.BackColor := clGreen;
      reMonitor.SelAttributes.Color := clWhite;
      reMonitor.SetSelection(SelStart, SelStart, True);
    end
    else
    begin
      Position := Pos(StrFIRSTENTRYINNAME, ALine);
      if Position > 0 then
      begin
        FindStart(reMonitor, Position, SelStart);
        reMonitor.SetSelection(SelStart,
          SelStart + Length(StrFIRSTENTRYINNAME), True);
        reMonitor.SelAttributes.BackColor := clRed;
        reMonitor.SelAttributes.Color := clWhite;
        reMonitor.SetSelection(SelStart, SelStart, True);
        jimageStatus.Tag := 2;
        tabMonitor.ImageIndex := 2;
        jimageStatus.Picture.Assign(jilBigFaces.Items[jimageStatus.Tag].Bitmap);
      end;

      Position := Pos(StrFailureToConverge, ALine);
      if Position > 0 then
      begin
        FindStart(reMonitor, Position, SelStart);
        reMonitor.SetSelection(SelStart,
          SelStart + Length(StrFailureToConverge), True);
        reMonitor.SelAttributes.BackColor := clRed;
        reMonitor.SelAttributes.Color := clWhite;
        reMonitor.SetSelection(SelStart, SelStart, True);
        jimageStatus.Tag := 0;
        tabMonitor.ImageIndex := 2;
        jimageStatus.Picture.Assign(jilBigFaces.Items[jimageStatus.Tag].Bitmap);
      end;

      Position := Pos(StrFAILEDTOMEETSOLVE, ALine);
      if Position > 0 then
      begin
        FindStart(reMonitor, Position, SelStart);
        reMonitor.SetSelection(SelStart,
          SelStart + Length(StrFAILEDTOMEETSOLVE), True);
        reMonitor.SelAttributes.BackColor := clRed;
        reMonitor.SelAttributes.Color := clWhite;
        reMonitor.SetSelection(SelStart, SelStart, True);
        jimageStatus.Tag := 0;
        tabMonitor.ImageIndex := 2;
        jimageStatus.Picture.Assign(jilBigFaces.Items[jimageStatus.Tag].Bitmap);
      end;
    end;
  finally
    DecimalSeparator := OldDecimalSeparator;
    ThousandSeparator := OldThousandSeparator;
  end;
end;

procedure TfrmMonitor.ReadCommandLine;
var
  State: TCommandLineState;
  Index: Integer;
  Param: string;
  LowerCaseParam: string;
const
  ModelNameOption = '-m';
  NameFileOption = '-n';
begin
  if FAlreadyStarted then
  begin
    Exit;
  end;
  FAlreadyStarted := True;
  if ParamCount > 0 then
  begin
    State := clsUnknown;
    for Index := 0 to ParamCount - 1 do
    begin
      Param := ParamStr(Index + 1);
      LowerCaseParam := LowerCase(Param);
      if LowerCaseParam = ModelNameOption then
      begin
        State := clsModelName;
        Continue;
      end
      else if LowerCaseParam = NameFileOption then
      begin
        State := clsNameFile;
        Continue;
      end;
      case State of
        clsUnknown:
          begin
            if FileExists(Param) then
            begin
              jvfeModelName.FileName := Param;
              State := clsNameFile;
            end;
          end;
        clsModelName:
          begin
            if FileExists(Param) then
            begin
              jvfeModelName.FileName := Param;
              State := clsNameFile;
            end;
          end;
        clsNameFile:
          begin
            if FileExists(Param) then
            begin
              jvfeNameFile.FileName := Param;
              State := clsModelName;
            end;
          end;
      else
        Assert(False);
      end;
    end;
    if FileExists(jvfeModelName.FileName) and FileExists(jvfeNameFile.FileName) then
    begin
      btnRunClick(nil);
    end;
  end;
end;

procedure TfrmMonitor.HandleListFileLine(ALine: string);
var
  IsError: boolean;
  IsWarning: Boolean;
  OldDecimalSeparator: Char;
  OldThousandSeparator: Char;
begin
  OldDecimalSeparator := DecimalSeparator;
  OldThousandSeparator := ThousandSeparator;
  try
    DecimalSeparator := '.';
    ThousandSeparator := ',';

    Inc(FLineCount);
    if (FListingFile = '') then
    begin
      Exit;
    end;
    StorePercentDiscrepancy(ALine);

    ALine := IntToStr(FLineCount) + ': ' + ALine;
    IndentifyProblem(ALine, IsError, 2, FErrorPositions, ErrorValues);
    IndentifyProblem(ALine, IsWarning, 1, FWarningPositions, WarningValues);
    if IsError or IsWarning then
    begin
      reListing.Lines.Add(ALine);
      HandleProblem(IsWarning, clYellow, FWarningPositions, WarningValues);
      HandleProblem(IsError, clRed, FErrorPositions, ErrorValues);
    end;
  finally
    DecimalSeparator := OldDecimalSeparator;
    ThousandSeparator := OldThousandSeparator;
  end;
end;

procedure TfrmMonitor.IndentifyProblem(ALine: string; var IsProblem: Boolean;
  NewIndex: Integer; var Positions: TIntegerDynArray; KeyTerms: TStringList);
var
  Position: Integer;
  Index: Integer;
begin
  IsProblem := False;
  for Index := 0 to KeyTerms.Count - 1 do
  begin
    Position := Pos(KeyTerms[Index], ALine);
    Positions[Index] := Position;
    if Position > 0 then
    begin
      IsProblem := True;
      if NewIndex > jimageStatus.Tag then
      begin
        jimageStatus.Tag := NewIndex;
        jimageStatus.Picture.Assign(jilBigFaces.Items[jimageStatus.Tag].Bitmap);
      end;
      if NewIndex > tabListing.ImageIndex then
      begin
        tabListing.ImageIndex := NewIndex;
      end;
    end;
  end;
end;

procedure TfrmMonitor.HandleProblem(IsError: Boolean; AColor: TColor; Positions: TIntegerDynArray; EV: TStringList);
var
  SelStart: Integer;
  Index: Integer;
begin
  if IsError then
  begin
    for Index := 0 to EV.Count - 1 do
    begin
      if Positions[Index] > 0 then
      begin
        FindStart(reListing, Positions[Index], SelStart);
        reListing.SetSelection(SelStart, SelStart + Length(EV[Index]), True);
        reListing.SelAttributes.BackColor := AColor;
        if AColor = clRed then
        begin
          reListing.SelAttributes.Color := clWhite;
        end;
        reListing.SetSelection(SelStart, SelStart, True);
      end;
    end;
  end;
end;

procedure TfrmMonitor.GetColor(NewIndex: Integer; Value: Double; var AColor: TColor);
begin
  if Abs(Value) >= 1 then
  begin
    AColor := clRed;
    if jimageStatus.Tag < NewIndex then
    begin
      jimageStatus.Tag := NewIndex;
      jimageStatus.Picture.Assign(jilBigFaces.Items[jimageStatus.Tag].Bitmap);
    end;
    if tabResults.ImageIndex < NewIndex then
    begin
      tabResults.ImageIndex := NewIndex;
    end;
    chartPercentDiscrepancy.LeftAxis.Automatic := True;
  end;
end;

function TfrmMonitor.PlotPercentDiscrepancy: boolean;
const
  TimeOutTime = 1/24/3600;
var
  Index: Integer;
  Rate: Double;
  AColor: TColor;
begin
  result := False;
  if not FDone and (Now - FStartPlotTime < TimeOutTime) then
  begin
    Exit;
  end;
  result := True;
  FStartPlotTime := Now;
  if (FPercentRate.Count > 0) or (FPercentCumulative.Count > 0) then
  begin
    // Hide the series while it is being updated so
    // that the program doesn't spend too much time
    // trying to redraw the screen.
    serCumulative.Active := False;
    try
      for Index := 0 to FPercentCumulative.Count - 1 do
      begin
        Rate := FPercentCumulative[Index];
        AColor := clBlue;
        GetColor(2, Rate, AColor);
        serCumulative.AddXY(serCumulative.Count + 1, Rate, '', AColor);
      end;
    finally
      FPercentCumulative.Clear;
      serCumulative.Active := True;
    end;

    serTimeStep.Active := False;
    try
      for Index := 0 to FPercentRate.Count - 1 do
      begin
        Rate := FPercentRate[Index];
        AColor := clYellow;
        GetColor(2, Rate, AColor);
        serTimeStep.AddXY(serTimeStep.Count + 1, Rate, '', AColor);
      end;
    finally
      FPercentRate.Clear;
      serTimeStep.Active := True;
    end;
  end;
end;

procedure TfrmMonitor.StorePercentDiscrepancy(ALine: string);
var
  Position: Integer;
  TestLine: string;
  Num1: string;
  Num2: string;
  Cum: Double;
  Rate: Double;
begin
  Position := Pos(StrPERCENTDISCREPANCY, ALine);
  if Position > 0 then
  begin
    if not FActivated then
    begin
      FActivated := True;
      ForceForegroundWindow(Handle);
    end;

    TestLine := Trim(Copy(ALine, Position + Length(StrPERCENTDISCREPANCY), MAXINT));
    Position := Pos(StrPERCENTDISCREPANCY, TestLine);
    Assert(Position > 0);
    Num1 := Trim(Copy(TestLine, 1, Position - 1));
    Num2 := Trim(Copy(TestLine, Position + Length(StrPERCENTDISCREPANCY), MAXINT));
    Cum := StrToFloat(Num1);
    Rate := StrToFloat(Num2);

    FPercentRate.Add(Rate);
    FPercentCumulative.Add(Cum);
  end;
end;

procedure TfrmMonitor.timerReadOutputTimer(Sender: TObject);
const
  TimeOutTime = 1/24/3600;
var
  ALine: string;
  ATime: TDateTime;
begin
  if FReading2 then
  begin
    Exit;
  end;
  FReading2 := True;
  try
    CreateFileReaders;
    if (FListTextReader = nil) or (FMonitorTextReader = nil) then
    begin
      Beep;
      Exit;
    end;
    while (FListTextReader.Position < FListTextReader.Size-1)
      or not FListTextReader.EOF do
    begin
      if (FListingFile = '') then
      begin
        Exit;
      end;
      ALine := FListTextReader.ReadLn;
      if (ALine <> '') or not FListTextReader.EOF then
      begin
        HandleListFileLine(ALine);
      end;
      if PlotPercentDiscrepancy then
      begin
        // only update the plot once per second so the program doesn't spend
        // too much time trying to redraw the screen.
        Application.ProcessMessages;
        if not FDone and (FListTextReader.Size > 0)
          and (FMonitorTextReader.Size > 0)
          and (FListTextReader.Position/FListTextReader.Size
          > FMonitorTextReader.Position/FMonitorTextReader.Size) then
        begin
          break;
        end;
      end;
      Application.ProcessMessages;
    end;
    if FMonitorTextReader <> nil then
    begin
      ATime := Now;
      while (FMonitorTextReader.Position < FMonitorTextReader.Size-1)
        or not FMonitorTextReader.EOF do
      begin
        if (FOutFile = '') then
        begin
          Exit;
        end;
        ALine := FMonitorTextReader.ReadLn;
        if (ALine <> '') or not FMonitorTextReader.EOF then
        begin
          HandleMonitorFileLine(ALine);
        end;
        Application.ProcessMessages;
        if Now - ATime > TimeOutTime then
        begin
          break;
        end;
      end;
    end;
    jimageStatus.Picture.Assign(jilBigFaces.Items[jimageStatus.Tag].Bitmap);
    Application.ProcessMessages;
  finally
    FReading2 := False;
  end;
end;

procedure TfrmMonitor.timerStartFromCommandParametersTimer(Sender: TObject);
begin
  timerStartFromCommandParameters.Enabled := False;
  ReadCommandLine;
end;

{ TStringFileStream }

function TStringFileStream.GetEOF: boolean;
begin
  if TempLine <> '' then
  begin
    result := False;
  end
  else
  begin
    result := FEOF;
  end;
end;

function TStringFileStream.ReadLn: string;
var
  EndPos: integer;
begin
  if Position < Size -1 then
  begin
    FEOF := False;
  end;
  EndPos := Pos(#13#10, TempLine);
  if EndPos > 0 then
  begin
    result := Copy(TempLine, 1,EndPos-1);
    TempLine := Copy(TempLine,EndPos+2, MAXINT);
  end
  else if not FEOF then
  begin
    if Position < Size -1 then
    begin
      FillChar(FBuffer, SizeOf(FBuffer), 0) ;
      Read(FBuffer, SizeOf(FBuffer)-SizeOf(Char));
      TempLine := TempLine + FBuffer;
      result := ReadLn;
    end
    else if TempLine <> '' then
    begin
      result := TempLine;
      TempLine := '';
    end
    else
    begin
      FEOF := True;
      result := '';
    end;
  end
  else
  begin
    result := '';
  end;
end;

end.
