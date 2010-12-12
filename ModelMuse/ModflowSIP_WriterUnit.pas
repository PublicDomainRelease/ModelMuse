unit ModflowSIP_WriterUnit;

interface

uses SysUtils, CustomModflowWriterUnit, ModflowPackageSelectionUnit;

Type
  TSipWriter = class(TCustomSolverWriter)
  private
    procedure WriteDataSet1;
    procedure WriteDataSet2;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses ModflowUnitNumbers, PhastModelUnit, frmProgressUnit;

{ TSipWriter }

class function TSipWriter.Extension: string;
begin
  result := '.sip';
end;

function TSipWriter.Package: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.SipPackage;
end;

procedure TSipWriter.WriteDataSet1;
var
  SIP: TSIPPackageSelection;
begin
  SIP := PhastModel.ModflowPackages.SipPackage;
  WriteInteger(SIP.MXITER);
  WriteInteger(SIP.NPARM);
  WriteString(' # Data Set 1: MXITER NPARM');
  NewLine;
end;

procedure TSipWriter.WriteDataSet2;
var
  SIP: TSIPPackageSelection;
begin
  SIP := PhastModel.ModflowPackages.SipPackage;
  WriteFloat(SIP.ACCL.Value);
  WriteFloat(SIP.HCLOSE.Value);
  WriteInteger(SIP.IPCALC);
  WriteFloat(SIP.WSEED.Value);
  WriteInteger(SIP.IPRSIP);
  WriteString(' # Data Set 2: ACCL HCLOSE IPCALC WSEED IPRSIP');
  NewLine;
end;

procedure TSipWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if SolverFileGeneratedExternally then
  begin
    Exit;
  end;
  NameOfFile := FileName(AFileName);
  WriteToNameFile('SIP', PhastModel.UnitNumbers.UnitNumber(StrSIP), NameOfFile, foInput);
  OpenFile(NameOfFile);
  try
    frmProgressMM.AddMessage('Writing SIP Package input.');
    WriteDataSet0;
    WriteDataSet1;
    WriteDataSet2;
  finally
    CloseFile;
  end;
end;

end.
