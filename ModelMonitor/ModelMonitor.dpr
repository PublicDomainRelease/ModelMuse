program ModelMonitor;

uses
  FastMM4 in '..\ModelMuse\FastMM4.pas',
  Forms,
  frmMonitorUnit in 'frmMonitorUnit.pas' {frmMonitor},
  ErrorMessages in 'ErrorMessages.pas',
  RealListUnit in '..\ModelMuse\RealListUnit.pas',
  forceforeground in 'forceforeground.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMonitor, frmMonitor);
  Application.Run;
end.
