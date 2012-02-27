unit ModflowMt3dmsLinkWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit;

type
  TModflowMt3dmsLinkWriter = class(TCustomPackageWriter)
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  ModflowUnitNumbers, frmProgressUnit, SysUtils;

{ TModflowMt3dmsLinkWriter }

class function TModflowMt3dmsLinkWriter.Extension: string;
begin
  result := '.lmt';
end;

function TModflowMt3dmsLinkWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.Mt3dBasic;
end;

procedure TModflowMt3dmsLinkWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
  FtlFileName: string;
begin
  if not Package.IsSelected then
  begin
    Exit;
  end;
  if Model.PackageGeneratedExternally(StrLMT6) then
  begin
    Exit;
  end;
  NameOfFile := FileName(AFileName);
  FtlFileName := ChangeFileExt(NameOfFile, '.ftl');
  WriteToNameFile(StrLMT6, Model.UnitNumbers.UnitNumber(StrLMT6),
    NameOfFile, foInput);
  WriteToNameFile(StrDATABINARY, Model.UnitNumbers.UnitNumber(StrFTL),
    FtlFileName, foOutput);

  OpenFile(NameOfFile);
  try
    frmProgressMM.AddMessage('Writing LMT6 Package input.');

    WriteString('OUTPUT_FILE_NAME ');
    WriteString(ExtractFileName(FtlFileName));
    NewLine;

    WriteString('OUTPUT_FILE_UNIT ');
    WriteInteger(Model.UnitNumbers.UnitNumber(StrFTL));
    NewLine;

    WriteString('OUTPUT_FIlE_HEADER Extended');
    NewLine;

    WriteString('OUTPUT_FILE_FORMAT Unformatted');
    NewLine;
  finally
    CloseFile;
  end;
end;

end.
