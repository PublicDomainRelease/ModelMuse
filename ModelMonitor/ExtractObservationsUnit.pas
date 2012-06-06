unit ExtractObservationsUnit;

interface

uses
  Windows, Classes, SysUtils;

type
  TObsRecord = record
    Name: string;
    ObservedValue: double;
    SimulatedValue: double;
    Difference: double;
  end;

  TObsRecordArray = array of TObsRecord;

procedure ExtractObservations(Lines: TStrings; out Observations: TObsRecordArray);

function ReadHeadObsFromFile(FileName: string; out Observations: TObsRecordArray): boolean;

implementation

uses
  IOUtils, ModflowIdentifiersUnit, Dialogs;

function FortranStrToFloat(AString: string): Extended;
var
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    AString := StringReplace(AString, ',', '.', [rfReplaceAll, rfIgnoreCase]);
    AString := StringReplace(AString, 'd', 'e', [rfReplaceAll, rfIgnoreCase]);
    result := StrToFloat(AString);
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;


procedure ExtractObservations(Lines: TStrings; out Observations: TObsRecordArray);
var
  AnObsLine: TStringList;
  LineIndex: Integer;
begin
  SetLength(Observations, Lines.Count);
  AnObsLine := TStringList.Create;
  try
    AnObsLine.Delimiter := ' ';
    for LineIndex := 0 to Lines.Count - 1 do
    begin
      AnObsLine.DelimitedText := Lines[LineIndex];
      While (AnObsLine.Count > 4) do
      begin
        AnObsLine[0] := AnObsLine[0] + ' ' + AnObsLine[1];
        AnObsLine.Delete(1);
      end;
      Assert(AnObsLine.Count = 4);
      Observations[LineIndex].Name := AnObsLine[0];
      Observations[LineIndex].ObservedValue := FortranStrToFloat(AnObsLine[1]);
      Observations[LineIndex].SimulatedValue := FortranStrToFloat(AnObsLine[2]);
      Observations[LineIndex].Difference := FortranStrToFloat(AnObsLine[3]);
    end;
  finally
    AnObsLine.Free;
  end;
end;

function ReadObsFromFile(const FileName, ObsIdentifier: string;
  out Observations: TObsRecordArray): boolean;
var
  AFile: TStreamReader;
  ALine: string;
  ObservationLines: TStringList;
begin
  result := False;
  if TFile.Exists(FileName) then
  begin
    try
      try
        AFile := TFile.OpenText(FileName);
      except on E: EInOutError do
        begin
          Beep;
          MessageDlg(E.message, mtError, [mbOK], 0);
          Exit;
        end;
      end;
      while not AFile.EndOfStream do
      begin
        ALine := AFile.ReadLine;
        if Pos(ObsIdentifier, ALine) > 0 then
        begin
          ObservationLines := TStringList.Create;
          try
            while not AFile.EndOfStream do
            begin
              ALine := AFile.ReadLine;
              if Pos('----', ALine) > 0 then
              begin
                Break;
              end;
            end;
            while not AFile.EndOfStream do
            begin
              ALine := AFile.ReadLine;
              if Trim(ALine) = '' then
              begin
                break;
              end
              else
              begin
                ObservationLines.Add(ALine)
              end;
            end;
            ExtractObservations(ObservationLines, Observations);
            result := True;
            break;
          finally
            ObservationLines.Free;
          end;
        end;
      end;
    finally
      AFile.Free;
    end;
  end;
end;

function ReadHeadObsFromFile(FileName: string; out Observations: TObsRecordArray): boolean;
begin
  result := ReadObsFromFile(FileName, StrHEADANDDRAWDOWNOB, Observations);
end;

end.

