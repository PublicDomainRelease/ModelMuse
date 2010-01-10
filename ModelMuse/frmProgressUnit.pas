{ @abstract(The main purpose of @name is to define @link(TfrmProgress) which
  is used to display a progress bar to the user with extra
  information displayed in @link(TfrmProgress.memoMessages)
  about what is happening.)}
unit frmProgressUnit;

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, ComCtrls,
  ExtCtrls, Buttons;

type
  { @abstract(@name is used to display a progress bar to the user with extra
    information displayed in @link(memoMessages) about what is happening.)}
  TfrmProgress = class(TfrmCustomGoPhast)
    // @name: TLabel;
    // @name displays the amount of progress.
    // See @link(StepIt) and @link(Prefix).
    lblProgress: TLabel;
    // @name: TMemo;
    // @name displays messages to the user.
    memoMessages: TMemo;
    // @name: TProgressBar;
    // @name displays the amount of progress.
    // See @link(StepIt).
    pbProgress: TProgressBar;
    // @name: TPanel;
    // @name holds the controls at the top of @classname.
    pnlTop: TPanel;
    btnAbort: TBitBtn;
    // @name initializes @classname.
    procedure FormShow(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
  private
    // @name: string;
    // See @link(Prefix).
    FPrefix: string;
    LastTime: TDateTime;
    LastMessageTime: TDateTime;
    FShouldContinue: Boolean;
    // See @link(Prefix).
    procedure SetPrefix(const Value: string);
    procedure SetProgressLabelCaption(const Value: string);
    function GetProgressLabelCaption: string;
    function GetShouldContinue: Boolean;
    { Private declarations }
  public
    property ShouldContinue: Boolean read GetShouldContinue write FShouldContinue;
    property ProgressLabelCaption: string read GetProgressLabelCaption
      write SetProgressLabelCaption;
    // @name is used in @link(StepIt) to help set the caption of
    // @link(lblProgress).
    // See @link(StepIt).
    property Prefix: string read FPrefix write SetPrefix;
    // @name advances @link(pbProgress) and sets the caption of
    // @link(lblProgress) based on @link(Prefix) and the amount of progress.
    procedure StepIt;
    procedure AddMessage(Const AMessage: string);
    { Public declarations }
  end;

var
  // @name is the instance of @link(TfrmProgress).
  frmProgress: TfrmProgress;
  frmFileProgress: TfrmProgress;

implementation

{$R *.dfm}

const
  HalfSecond = 1/24/3600/2;

{ TfrmProgress }

procedure TfrmProgress.StepIt;
begin
  pbProgress.StepIt;
  ProgressLabelCaption := Prefix + IntToStr(pbProgress.Position) + ' out of '
    + IntToStr(pbProgress.Max) + '.';
end;

procedure TfrmProgress.AddMessage(const AMessage: string);
begin
  memoMessages.Lines.Add(AMessage);
  if Now - LastMessageTime > HalfSecond then
  begin
    Application.ProcessMessages;
    LastMessageTime := Now;
  end;
end;

procedure TfrmProgress.btnAbortClick(Sender: TObject);
begin
  inherited;
  FShouldContinue := False;
  memoMessages.Lines.Add('Attempting to abort; please wait.');
end;

procedure TfrmProgress.FormShow(Sender: TObject);
begin
  inherited;
  LastTime := Now;
  LastMessageTime := Now;
  memoMessages.Clear;
  pbProgress.Position := 0;
  lblProgress.Caption := '';
end;

function TfrmProgress.GetProgressLabelCaption: string;
begin
  result := lblProgress.Caption;
end;

function TfrmProgress.GetShouldContinue: Boolean;
begin
  Application.ProcessMessages;
  Result := FShouldContinue;
end;

procedure TfrmProgress.SetPrefix(const Value: string);
begin
  FPrefix := Value;
end;

procedure TfrmProgress.SetProgressLabelCaption(const Value: string);
begin
  if Now - LastTime > HalfSecond then
  begin
    lblProgress.Caption := Value;
    LastTime := Now;
  end;
end;

end.