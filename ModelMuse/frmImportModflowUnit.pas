unit frmImportModflowUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, Mask, JvExMask, JvToolEdit,
  ArgusDataEntry, ComCtrls;

type
  TfrmImportModflow = class(TfrmCustomGoPhast)
    rdeX: TRbwDataEntry;
    Label1: TLabel;
    Label2: TLabel;
    rdeY: TRbwDataEntry;
    Label3: TLabel;
    rdeGridAngle: TRbwDataEntry;
    Label4: TLabel;
    edNameFile: TJvFilenameEdit;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    lblWarning: TLabel;
    sbStatusBar: TStatusBar;
    pbProgress: TProgressBar;
    cbOldStream: TCheckBox;
    procedure btnOKClick(Sender: TObject);
    procedure edNameFileChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FReadModflowInputProperly: Boolean;
    procedure HandleModflowConsolLine(const Text: string);
    procedure UpdateStatusBar(const Text: string);
    procedure ShowProgress(Position, Total: integer);
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses JclSysUtils, Modflow2005ImporterUnit, frmShowHideObjectsUnit,
  frmDisplayDataUnit, ModelMuseUtilities, StrUtils;

resourcestring
  StrTheMODFLOWNameFil = 'The MODFLOW Name file appears to be invalid';
  StrSWasNotFound = '%s was not found.';
  StrNoLISTFileWasFou = 'No LIST file was found in the MODFLOW Name file.';
  StrThereWasAnErrorR = 'There was an error reading the MODFLOW input files.' +
  '  Check %s for error messages.';
  StrTheListingFile = 'The listing file, "%s", was not found.';
  StrTheNameOfTheMODF = 'The name of the MODFLOW Name file can not contain a' +
  'ny spaces.';
  StrReadingStressPerio = 'Reading Stress Period %s';

procedure TfrmImportModflow.btnOKClick(Sender: TObject);
var
  ModflowImporterName: string;
  NameFile: TStringList;
  Index: Integer;
  ALine: string;
  LineContents: TStringList;
  ListFileName: string;
  CurrentDir: string;
  XOrigin: double;
  YOrigin: double;
  GridAngle: double;
  OldFile: string;
//  DelimPos: Integer;
begin
  inherited;
  Enabled := False;
  CurrentDir := GetCurrentDir;
  try
    ModflowImporterName := ExtractFileDir(Application.ExeName)
      + '\' + 'MF2005_Importer.exe';
    if not FileExists(ModflowImporterName) then
    begin
      Beep;
      MessageDlg(Format(StrSWasNotFound, [ModflowImporterName]), mtError, [mbOK], 0);
      Exit;
    end;

    if Pos(' ', ExtractFileName(edNameFile.FileName)) > 0 then
    begin
      Beep;
      MessageDlg(StrTheNameOfTheMODF, mtError, [mbOK], 0);
      Exit;
    end;

    XOrigin := StrToFloat(rdeX.Text);
    YOrigin := StrToFloat(rdeY.Text);
    GridAngle := StrToFloat(rdeGridAngle.Text) * Pi/180;

    OldFile := ExtractFileDir(edNameFile.FileName) + '\old.txt';
    if cbOldStream.Checked then
    begin
      With TStringList.Create do
      begin
        SaveToFile(OldFile);
        Free;
      end;
    end
    else
    begin
      DeleteFile(OldFile);
    end;

    ListFileName := '';
    NameFile := TStringList.Create;
    LineContents := TStringList.Create;
    try
      LineContents.Delimiter := ' ';
      try
        NameFile.LoadFromFile(edNameFile.FileName);
      except on EFOpenError do
        begin
          CantOpenFileMessage(edNameFile.FileName);
          Exit;
        end;
      end;
      for Index := 0 to NameFile.Count - 1 do
      begin
        ALine := NameFile[Index];
        if (Length(ALine) > 0) and (ALine[1] <> '#') then
        begin
          LineContents.DelimitedText := UpperCase(ALine);
          if LineContents.Count = 0 then
          begin
            Beep;
            MessageDlg(StrTheMODFLOWNameFil, mtError, [mbOK], 0);
            Exit;
          end;
          if Trim(LineContents[0]) = 'LIST' then
          begin
            LineContents.DelimitedText := ALine;
            if LineContents.Count < 3 then
            begin
              Beep;
              MessageDlg(StrTheMODFLOWNameFil, mtError, [mbOK], 0);
              Exit;
            end;
            ListFileName := LineContents[2];
            break;
          end;
        end;
      end;
    finally
      NameFile.Free;
      LineContents.Free;
    end;
    if ListFileName = '' then
    begin
      Beep;
      MessageDlg(StrNoLISTFileWasFou, mtError, [mbOK], 0);
      Exit;
    end;
    SetCurrentDir(ExtractFileDir(edNameFile.FileName));
//    if Copy(ListFileName,1,2) = '.\' then
//    begin
//      DelimPos := PosEx(PathDelim,ListFileName,3);
//      if DelimPos > 0 then
//      begin
//        ListFileName := Copy(ListFileName,DelimPos+1,MaxInt);
//      end;
//
//    end;
    ListFileName := ExpandFileName(ListFileName);

    FReadModflowInputProperly := False;
    Execute('"' + ModflowImporterName + '" '
      + ExtractFileName(edNameFile.FileName), HandleModflowConsolLine);
    if not FReadModflowInputProperly then
    begin
      Beep;
      MessageDlg(Format(StrThereWasAnErrorR, [ListFileName]),
        mtError, [mbOK], 0);
      Exit;
    end;
    if not FileExists(ListFileName) then
    begin
      Beep;
      MessageDlg(Format(StrTheListingFile, [ListFileName]),
        mtError, [mbOK], 0);
      Exit;
    end;
    sbStatusBar.SimpleText := '';
    FreeAndNil(frmShowHideObjects);
    FreeAndNil(frmDisplayData);
    ImportModflow2005(ListFileName, XOrigin, YOrigin, GridAngle,
      UpdateStatusBar, ShowProgress, mtParent);

  finally
    SetCurrentDir(CurrentDir);
    Enabled := True;
  end;
end;
procedure TfrmImportModflow.edNameFileChange(Sender: TObject);
begin
  inherited;
  btnOK.Enabled := FileExists(edNameFile.FileName);
end;

procedure TfrmImportModflow.FormShow(Sender: TObject);
begin
  inherited;
  SetAppearance;
  lblWarning.Width := Width - 16;
  lblWarning.Font.Style := [fsBold];
end;

procedure TfrmImportModflow.HandleModflowConsolLine(const Text: string);
const
  Normal = 'Normal termination of simulation';
  SP = 'Solving:  Stress period:';
  TS = 'Time step:';
var
  SpPos: Integer;
  TsPos: Integer;
  StressPeriod: string;
  SPStart: integer;
begin
  SpPos := Pos(SP, Text);
  TsPos := Pos(TS, Text);
  if (SpPos > 0) and (TsPos > 0) then
  begin
    SPStart := SpPos + Length(SP);
    StressPeriod := Trim(Copy(Text, SPStart, TsPos-SPStart));
    sbStatusBar.SimpleText := Format(StrReadingStressPerio, [StressPeriod]);
  end
  else
  begin
    if Trim(Text) <> '' then
    begin
      sbStatusBar.SimpleText := Text;
    end;
  end;
  Application.ProcessMessages;

  if Trim(Text) = Normal then
  begin
    FReadModflowInputProperly := True;
  end;
end;

procedure TfrmImportModflow.ShowProgress(Position, Total: integer);
begin
  if pbProgress.Max <> Total then
  begin
    pbProgress.Max := Total
  end;
  pbProgress.Position := Position;
  Application.ProcessMessages;
end;

procedure TfrmImportModflow.UpdateStatusBar(const Text: string);
begin
  sbStatusBar.SimpleText := Text;
  Application.ProcessMessages;
end;

end.
