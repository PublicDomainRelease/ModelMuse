unit ModpathNameFileWriterUnit;

interface

uses SysUtils, Classes, PhastModelUnit;

Type
  TModpathNameFileWriter = class(TObject)
  public
    procedure WriteFile(const FileName: string; Model: TPhastModel);
  end;

implementation

uses ModpathMainFileWriterUnit, ModflowDiscretizationWriterUnit, 
  ModpathStartingLocationsWriter, ModflowPackageSelectionUnit,
  ModflowOutputControlUnit;

{ TModpathNameFileWriter }

procedure TModpathNameFileWriter.WriteFile(const FileName: string;
  Model: TPhastModel);
var
  NameFile: TStringList;
  AFileName: string;
  Options: TModpathSelection;
begin
  Options := Model.ModflowPackages.ModPath;
  
  NameFile := TStringList.Create;
  try
    AFileName := ExtractFileName(ChangeFileExt(FileName, '.mplst'));
    NameFile.Add('LIST 11 ' + AFileName);

    AFileName := ExtractFileName(ChangeFileExt(FileName,
      TModpathMainFileWriter.Extension));
    NameFile.Add('MAIN 12 ' + AFileName);

    AFileName := ExtractFileName(ChangeFileExt(FileName,
      TModflowDiscretizationWriter.Extension));
    NameFile.Add('DIS 13 ' + AFileName);

    AFileName := ExtractFileName(ChangeFileExt(FileName, '.cbf'));
    NameFile.Add('CBF 14 ' + AFileName);

    if Options.Binary then
    begin
      AFileName := ExtractFileName(ChangeFileExt(FileName, '.end_bin'));
    end
    else
    begin
      AFileName := ExtractFileName(ChangeFileExt(FileName, '.end'));
    end;
    NameFile.Add('ENDPOINT 15 ' + AFileName);
    case Options.OutputMode of
      mopEndpoints:
        begin
          // do nothing
        end;
      mopPathline:
        begin
          if Options.Binary then
          begin
            AFileName := ExtractFileName(ChangeFileExt(FileName, '.path_bin'));
          end
          else
          begin
            AFileName := ExtractFileName(ChangeFileExt(FileName, '.path'));
          end;
          NameFile.Add('PATHLINE 16 ' + AFileName);
        end;
      mopTimeSeries:
        begin
          if Options.Binary then
          begin
            AFileName := ExtractFileName(ChangeFileExt(FileName, '.ts_bin'));
          end
          else
          begin
            AFileName := ExtractFileName(ChangeFileExt(FileName, '.ts'));
          end;
          NameFile.Add('TIME-SERIES 17 ' + AFileName);
        end;
      else Assert(False);
    end;

    if Options.ShouldCreateTimeFile then
    begin
      AFileName := ExtractFileName(ChangeFileExt(FileName, '.tim'));
      NameFile.Add('TIME 18 ' + AFileName);
    end;

    AFileName := ExtractFileName(ChangeFileExt(FileName,
      TModpathStartingLocationsWriter.Extension));
    NameFile.Add('LOCATIONS 19 ' + AFileName);

    AFileName := ExtractFileName(ChangeFileExt(FileName, '.cbc'));
    NameFile.Add('BUDGET 20 ' + AFileName);

    if Model.ModflowOutputControl.HeadOC.SaveInExternalFile then
    begin
      case Model.ModflowOutputControl.HeadOC.OutputFileType of
        oftText:
          begin
            AFileName := ExtractFileName(ChangeFileExt(FileName, '.fhd'));
            NameFile.Add('HEAD 21 ' + AFileName);
          end;
        oftBinary:
          begin
            AFileName := ExtractFileName(ChangeFileExt(FileName, '.bhd'));
            NameFile.Add('HEAD(BINARY) 22 ' + AFileName);
          end;
        else Assert(False);
      end;
    end;

    if Model.ModflowOutputControl.DrawdownOC.SaveInExternalFile then
    begin
      case Model.ModflowOutputControl.DrawdownOC.OutputFileType of
        oftText:
          begin
            AFileName := ExtractFileName(ChangeFileExt(FileName, '.fdn'));
            NameFile.Add('DRAWDOWN 23 ' + AFileName);
          end;
        oftBinary:
          begin
            AFileName := ExtractFileName(ChangeFileExt(FileName, '.bdn'));
            NameFile.Add('DRAWDOWN(BINARY) 24 ' + AFileName);
          end;
        else Assert(False);
      end;
    end;

    NameFile.SaveToFile(FileName);
  finally
    NameFile.Free;
  end;
end;

end.
