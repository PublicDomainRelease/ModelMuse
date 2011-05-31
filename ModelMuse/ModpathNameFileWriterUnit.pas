unit ModpathNameFileWriterUnit;

interface

uses SysUtils, Classes, PhastModelUnit;

Type
  TModpathNameFileWriter = class(TObject)
  public
    procedure WriteFile(const FileName: string; Model: TCustomModel);
  end;


implementation

uses ModpathMainFileWriterUnit, ModflowDiscretizationWriterUnit, 
  ModpathStartingLocationsWriter, ModflowPackageSelectionUnit,
  ModflowOutputControlUnit, frmErrorsAndWarningsUnit;

{ TModpathNameFileWriter }

procedure TModpathNameFileWriter.WriteFile(const FileName: string;
  Model: TCustomModel);
const
  CbfFileExistsError = 'The following MODFLOW input or output files are '
    + 'required by MODPATH to run but they are not in the directory in which '
    + 'MODPATH is being run: "';
var
  NameFile: TStringList;
  AFileName: string;
  Options: TModpathSelection;
  procedure CheckFileExists(const AFileName: string);
  var
    FullFileName: string;
  begin
    FullFileName := ExpandFileName(AFileName);
    if not FileExists(FullFileName) then
    begin
      frmErrorsAndWarnings.AddError(Model,
        CbfFileExistsError + ExtractFilePath(FileName) + '".',
        AFileName);
    end;
  end;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, CbfFileExistsError);
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
    CheckFileExists(AFileName);

    if Model.ModflowStressPeriods.TransientModel then
    begin
      AFileName := ExtractFileName(ChangeFileExt(FileName, '.cbf'));
      NameFile.Add('CBF 14 ' + AFileName);
      // The response file will direct MODPATH to generated
      // the CBF file if it doesn't exist.
//      CheckFileExists(AFileName);
    end;

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

    AFileName := ExtractFileName(ChangeFileExt(FileName, StrCbcExt));
    NameFile.Add('BUDGET 20 ' + AFileName);
    CheckFileExists(AFileName);

    if Model.ModflowOutputControl.HeadOC.SaveInExternalFile then
    begin
      case Model.ModflowOutputControl.HeadOC.OutputFileType of
        oftText:
          begin
            AFileName := ExtractFileName(ChangeFileExt(FileName, StrFhd));
            NameFile.Add('HEAD 21 ' + AFileName);
            CheckFileExists(AFileName);
          end;
        oftBinary:
          begin
            AFileName := ExtractFileName(ChangeFileExt(FileName, StrBhd));
            NameFile.Add('HEAD(BINARY) 22 ' + AFileName);
            CheckFileExists(AFileName);
          end;
        else Assert(False);
      end;
    end;

    if Model.ModflowOutputControl.DrawdownOC.SaveInExternalFile then
    begin
      case Model.ModflowOutputControl.DrawdownOC.OutputFileType of
        oftText:
          begin
            AFileName := ExtractFileName(ChangeFileExt(FileName, StrFdn));
            NameFile.Add('DRAWDOWN 23 ' + AFileName);
            CheckFileExists(AFileName);
          end;
        oftBinary:
          begin
            AFileName := ExtractFileName(ChangeFileExt(FileName, StrBdn));
            NameFile.Add('DRAWDOWN(BINARY) 24 ' + AFileName);
            CheckFileExists(AFileName);
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
