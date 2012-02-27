program ListFileAnalyzer;

uses
  Forms,
  frmListAnalyzerUnit in 'frmListAnalyzerUnit.pas' {frmMain},
  FileIndexUnit in 'FileIndexUnit.pas',
  ModflowIdentifiersUnit in 'ModflowIdentifiersUnit.pas',
  ErrorMessages in 'ErrorMessages.pas',
  frameFileListHandlerUnit in 'frameFileListHandlerUnit.pas' {frameFileListHandler: TFrame},
  ExtractObservationsUnit in 'ExtractObservationsUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
