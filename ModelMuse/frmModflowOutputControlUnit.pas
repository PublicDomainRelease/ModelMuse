unit frmModflowOutputControlUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, JvPageList,
  JvExControls, JvComponent, ComCtrls, JvExComCtrls, JvPageListTreeView,
  JvExStdCtrls, JvCheckBox, JvExExtCtrls, JvRadioGroup, frameOutputControlUnit,
  ArgusDataEntry, ehshelprouter, Mask, JvExMask, JvSpin, JvCombobox, JvListComb,
  JvNetscapeSplitter;

type
  TfrmModflowOutputControl = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    pltrPageNavigator: TJvPageListTreeView;
    jvPages: TJvPageList;
    jvspGeneral: TJvStandardPage;
    jvspHeads: TJvStandardPage;
    cbPrintInputArrays: TJvCheckBox;
    rgSaveCellFlows: TJvRadioGroup;
    cbPrintInputCellLists: TJvCheckBox;
    jvspDrawdown: TJvStandardPage;
    frameDrawdown: TframeOutputControl;
    frameHead: TframeOutputControl;
    cbCompact: TJvCheckBox;
    memoComments: TMemo;
    Comments: TLabel;
    jvspBudget: TJvStandardPage;
    comboFrequency: TJvImageComboBox;
    lblN: TLabel;
    spN: TJvSpinEdit;
    lblFrequency: TLabel;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    lblBudget: TLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure jvPagesChange(Sender: TObject);
  private
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmModflowOutputControl: TfrmModflowOutputControl;

implementation

uses ModflowOutputControlUnit, frmGoPhastUnit;

{$R *.dfm}

{ TfrmModflowOutputControl }

procedure TfrmModflowOutputControl.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmModflowOutputControl.FormCreate(Sender: TObject);
var
  Index: integer;
begin
  inherited;
  frameDrawdown.ParentFont := True;
  frameHead.ParentFont := True;
  // Create the links between nodes and pages in code because
  // they are too easily disrupted by editing the nodes in the form designer.
  for Index := 0 to pltrPageNavigator.Items.Count -1 do
  begin
    (pltrPageNavigator.Items[Index] as TJvPageIndexNode).PageIndex := Index;
  end;
  jvPages.ActivePage := jvspGeneral;

  GetData;
end;

procedure TfrmModflowOutputControl.GetData;
var
  OutputControl: TModflowOutputControl;
begin
  OutputControl := frmGoPhast.PhastModel.ModflowOutputControl;
  cbPrintInputArrays.Checked := OutputControl.PrintInputArrays;
  rgSaveCellFlows.ItemIndex := Ord(OutputControl.SaveCellFlows);
  cbPrintInputCellLists.Checked := OutputControl.PrintInputCellLists;
  cbCompact.Checked := OutputControl.Compact;
  frameHead.GetData(OutputControl.HeadOC);
  frameDrawdown.GetData(OutputControl.DrawdownOC);
  MemoComments.Lines.Assign(OutputControl.Comments);

  comboFrequency.ItemIndex := Ord(OutputControl.BudgetFrequencyChoice);
  spN.AsInteger := OutputControl.BudgetFrequency;
end;

procedure TfrmModflowOutputControl.jvPagesChange(Sender: TObject);
begin
  inherited;
  HelpKeyWord := jvPages.ActivePage.HelpKeyword;
end;

procedure TfrmModflowOutputControl.SetData;
var
  OutputControl: TModflowOutputControl;
begin
{ TODO : Need undo/redo here }
  OutputControl := frmGoPhast.PhastModel.ModflowOutputControl;
  OutputControl.PrintInputArrays := cbPrintInputArrays.Checked;
  OutputControl.SaveCellFlows := TCellSaveFormat(rgSaveCellFlows.ItemIndex);
  OutputControl.PrintInputCellLists := cbPrintInputCellLists.Checked;
  OutputControl.Compact := cbCompact.Checked;
  frameHead.SetData(OutputControl.HeadOC);
  frameDrawdown.SetData(OutputControl.DrawdownOC);
  OutputControl.Comments.Assign(MemoComments.Lines);
  OutputControl.BudgetFrequencyChoice :=
    TFrequencyChoice(comboFrequency.ItemIndex);
  OutputControl.BudgetFrequency := spN.AsInteger;
end;

end.
