unit frmBatchFileAdditionsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, ExtCtrls, frameBatchFileLinesUnit, StdCtrls,
  Buttons, UndoItems;

type
  TfrmBatchFileAdditions = class(TfrmCustomGoPhast)
    frameBatchFileBefore: TframeBatchFileLines;
    Splitter1: TSplitter;
    frameBatchFileAfter: TframeBatchFileLines;
    Panel1: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
  private
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  TBatchFileAdditionsUndo = class(TCustomUndo)
  private
    FOldBeforeAdditions: TStrings;
    FOldAfterAdditions: TStrings;
    FNewBeforeAdditions: TStrings;
    FNewAfterAdditions: TStrings;
  protected
    function Description: string; override;
  public
    Constructor Create(NewBeforeAdditions, NewAfterAdditions: TStrings);
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

var
  frmBatchFileAdditions: TfrmBatchFileAdditions;

implementation

uses
  frmGoPhastUnit;

{$R *.dfm}

{ TfrmBatchFileAdditions }

procedure TfrmBatchFileAdditions.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmBatchFileAdditions.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmBatchFileAdditions.GetData;
begin
  frameBatchFileBefore.memoLines.Lines := frmGoPhast.PhastModel.BatchFileAdditionsBeforeModel;
  frameBatchFileAfter.memoLines.Lines := frmGoPhast.PhastModel.BatchFileAdditionsAfterModel;
end;

procedure TfrmBatchFileAdditions.SetData;
var
  Undo: TBatchFileAdditionsUndo;
begin
  Undo := TBatchFileAdditionsUndo.Create(frameBatchFileBefore.memoLines.Lines,
    frameBatchFileAfter.memoLines.Lines);
  frmGoPhast.UndoStack.Submit(Undo);
end;

{ TBatchFileAdditionsUndo }

constructor TBatchFileAdditionsUndo.Create(NewBeforeAdditions,
  NewAfterAdditions: TStrings);
begin
  inherited Create;
  FOldBeforeAdditions := TStringList.Create;
  FOldBeforeAdditions.Assign(frmGoPhast.PhastModel.BatchFileAdditionsBeforeModel);

  FOldAfterAdditions := TStringList.Create;
  FOldAfterAdditions.Assign(frmGoPhast.PhastModel.BatchFileAdditionsAfterModel);

  FNewBeforeAdditions := TStringList.Create;
  FNewBeforeAdditions.Assign(NewBeforeAdditions);

  FNewAfterAdditions := TStringList.Create;
  FNewAfterAdditions.Assign(NewAfterAdditions);
end;

function TBatchFileAdditionsUndo.Description: string;
begin

end;

destructor TBatchFileAdditionsUndo.Destroy;
begin
  FNewAfterAdditions.Free;
  FNewBeforeAdditions.Free;
  FOldAfterAdditions.Free;
  FOldBeforeAdditions.Free;
  inherited;
end;

procedure TBatchFileAdditionsUndo.DoCommand;
begin
  frmGoPhast.PhastModel.BatchFileAdditionsBeforeModel := FNewBeforeAdditions;
  frmGoPhast.PhastModel.BatchFileAdditionsAfterModel := FNewAfterAdditions;
end;

procedure TBatchFileAdditionsUndo.Undo;
begin
  frmGoPhast.PhastModel.BatchFileAdditionsBeforeModel := FOldBeforeAdditions;
  frmGoPhast.PhastModel.BatchFileAdditionsAfterModel := FOldAfterAdditions;
end;

end.
