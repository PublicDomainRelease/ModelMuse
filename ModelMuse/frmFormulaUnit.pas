{@abstract(The main purpose of @name is to define @link(TfrmFormula) which
  provides a way for the user to edit formulas)}
unit frmFormulaUnit;

interface

uses
  SysUtils, Types, Classes, StrUtils, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, Buttons, ExtCtrls, ComCtrls, RbwParser,
  JvExExtCtrls, JvNetscapeSplitter, JvExStdCtrls, RichEdit, JvRichEdit,
  ehshelprouter, ClassificationUnit;

type
  TVariableEdit = class(TClassificationObject)
  private
    FVariable: TCustomValue;
  public
    function ClassificationName: string; override;
    function FullClassification: string; override;
    property Variable: TCustomValue read FVariable;
    Constructor Create(AVariable: TCustomValue);
  end;

  TTreeNodeTextStorage = class(TObject)
  public
    OpenText: string;
    ClosedText: string;
  end;  

{ TODO : Consider showing web page at the bottom of the form with
formula help for the currently selected formula. Clicking a link in the web
page should bring up the normal help system.}
{ TODO :
Consider supporting ordinals in formulas.  The programer would have to
define the ordinal as a series of strings.  Also support keywords.  Then use
these capabilities for unit conversions:
Convert(Number from FromUnit to ToUnit)
Convert(5 from m to ft)
In this case "from" and "to" would be keywords.
The formula editor would need to display ordinals and maybe keywords.}
{ TODO :
'Consider finding a way to group data sets so not so
many will be displayed at one time. }
  { TODO : List all functions alphabetically under "Alphabetically". }

  {@abstract(@name provides a way for the user to edit formulas)}
  TfrmFormula = class(TfrmCustomGoPhast)
    // @name: TButton;
    // @name is used to insert "0" in the formula.
    btn0: TButton;
    // @name: TButton;
    // @name is used to insert "1" in the formula.
    btn1: TButton;
    // @name: TButton;
    // @name is used to insert "2" in the formula.
    btn2: TButton;
    // @name: TButton;
    // @name is used to insert "3" in the formula.
    btn3: TButton;
    // @name: TButton;
    // @name is used to insert "4" in the formula.
    btn4: TButton;
    // @name: TButton;
    // @name is used to insert "5" in the formula.
    btn5: TButton;
    // @name: TButton;
    // @name is used to insert "6" in the formula.
    btn6: TButton;
    // @name: TButton;
    // @name is used to insert "7" in the formula.
    btn7: TButton;
    // @name: TButton;
    // @name is used to insert "8" in the formula.
    btn8: TButton;
    // @name: TButton;
    // @name is used to insert "9" in the formula.
    btn9: TButton;
    // @name: TButton;
    // @name is used to insert "and" in the formula.
    btnAnd: TButton;
    // @name: TBitBtn;
    // @name closes the dialog box and sets the ModalResult to mrCancel.
    btnCancel: TBitBtn;
    // @name: TButton;
    // @name is used to insert ")" in the formula.
    btnCloseParen: TButton;
    // @name: TButton;
    // @name is used to insert "," in the formula.
    btnComma: TButton;
    // @name: TButton;
    // @name is used to insert "." in the formula.
    btnDecimal: TButton;
    // @name: TButton;
    // @name is used to insert "/" in the formula.
    btnDivide: TButton;
    // @name: TButton;
    // @name is used to insert "E" in the formula.
    btnE: TButton;
    // @name: TButton;
    // @name is used to insert "=" in the formula.
    btnEquals: TButton;
    // @name: TButton;
    // @name is used to insert "False" in the formula.
    btnFalse: TButton;
    // @name: TBitBtn;
    // See @link(btnFunctionHelpClick).
    btnFunctionHelp: TBitBtn;
    // @name: TButton;
    // @name is used to insert ">-" in the formula.
    btnGreaterOrEquals: TButton;
    // @name: TButton;
    // @name is used to insert ">" in the formula.
    btnGreaterThan: TButton;
    // @name: TBitBtn;
    // @name shows help for the dialog box.
    btnHelp: TBitBtn;
    // @name: TButton;
    // @name is used to insert "<=" in the formula.
    btnLessEquals: TButton;
    // @name: TButton;
    // @name is used to insert "<" in the formula.
    btnLessThan: TButton;
    // @name: TButton;
    // @name is used to insert "-" in the formula.
    btnMinus: TButton;
    // @name: TButton;
    // @name is used to insert "*" in the formula.
    btnMultiply: TButton;
    // @name: TButton;
    // @name is used to insert "not" in the formula.
    btnNot: TButton;
    // @name: TButton;
    // @name is used to insert "<>" in the formula.
    btnNotEqual: TButton;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TButton;
    // @name is used to insert "(" in the formula.
    btnOpenParen: TButton;
    // @name: TButton;
    // @name is used to insert "or" in the formula.
    btnOr: TButton;
    // @name: TButton;
    // @name is used to insert "+" in the formula.
    btnPlus: TButton;
    // @name: TButton;
    // @name is used to insert '"" in the formula.
    btnQuote: TButton;
    // @name: TButton;
    // @name is used to insert "True" in the formula.
    btnTrue: TButton;
    // @name: TButton;
    // @name is used to insert "xor" in the formula.
    btnXor: TButton;
    // @name: TPanel;
    // @name is the panel at the bottom of the dialog box.
    pnlBottom: TPanel;
    // @name: TPanel;
    // @name is the panel holding must of the buttons.
    pnlButtons: TPanel;
    // @name holds a label for @link(tvItems).
    pnlLabelItemTree: TPanel;
    // @name: TPanel;
    // @name holds @link(pnlButtons) and @link(jreFormula).
    // It is present to allow @link(Splitter) to work properly.
    pnlMain: TPanel;
    // @name holds @link(tvItems) and @link(pnlLabelItemTree).
    pnlRight: TPanel;
    // @name: TRbwParser;
    // @name is used to test the validity of the formula.
    rbFormulaParser: TRbwParser;
    // @name: TSplitter;
    // @name allows the user to resize parts of the dialog box.
    Splitter: TSplitter;
    // @name: TTimer;
    // @name is used to allow for a slight delay before selecting text
    // in @link(jreFormula).
    Timer: TTimer;
    // @name: TTreeView;
    // @name displays the @link(TDataArray)s and functions that can be used in
    // the formula.
    tvItems: TTreeView;
    tvFormulaDiagram: TTreeView;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    jreFormula: TJvRichEdit;
    gbLogicalOperators: TGroupBox;
    gbIntegerOperators: TGroupBox;
    // @name is used to insert "mod" in the formula.
    btnMod: TButton;
    // @name is used to insert "div" in the formula.
    btnDiv: TButton;
    gbNumbers: TGroupBox;
    gbOperators: TGroupBox;
    lbltems: TLabel;
    // @name shows help on the function that is selected in @link(tvItems).
    // See @link(tvItemsChange).
    procedure btnFunctionHelpClick(Sender: TObject);
    // @name checks that the formula is valid.  If so,
    // it closes the dialog box and sets the ModalResult to mrOK.
    // Otherwise, it disables the OK button.
    procedure btnOKClick(Sender: TObject);
    // @name is the OnClick event handler for most of the buttons in the
    // dialog box.  It inserts the caption of the button into the formula.
    procedure buttonClick(Sender: TObject);
    // @name initializes the @classname.
    procedure FormCreate(Sender: TObject); override;
    // @name calls @link(jreFormula).SetFocus;
    procedure FormShow(Sender: TObject);
    // @name enables the OK button.
    procedure jreFormulaChange(Sender: TObject);
    // @name determines which a token in the formula should be selected
    // and starts @link(Timer).  See @link(TimerSetSelection).

    procedure jreFormulaDblClick(Sender: TObject);
    // @name ensures that the splitter can't make @link(pnlMain)
    // go below its minimum width.
    procedure SplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    // @name selects the text specified in @link(jreFormulaDblClick) and
    // stops @link(Timer).
    procedure TimerSetSelection(Sender: TObject);
    procedure tvItemsChange(Sender: TObject; Node: TTreeNode);
    procedure tvItemsDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvFormulaDiagramCollapsed(Sender: TObject; Node: TTreeNode);
    procedure tvFormulaDiagramExpanded(Sender: TObject; Node: TTreeNode);
    // @name sets @link(FClickSelectionStart).
    procedure jreFormulaMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure jreFormulaSelectionChange(Sender: TObject);
  private
    // See @link(DataSetGroupName)
    FDataSetGroupName: string;
    // @name is the caption of the TTreeNode in @link(tvItems) that
    // was last clicked.  It is used to get context sensitive help on
    // Functions displayed in @link(tvItems).
    FFunctionHelpString: string;
    // @name: TTreeNode;
    // @name is the parent TTreeNode that holds all
    // the TTreeNodes for functions.
    FFunctions: TTreeNode;
    // @name: TButton;
    // @name is set to the previous button that was clicked.  It is used
    // to help determine whether there should be spaces before the
    // the text specified by the button.
    FLastButton: TButton;
    // @name: integer;
    // @name specifies the length of the text that
    // should be selected in @link(jreFormulaDblClick).
    FNewSelectionLength: integer;
    // @name: integer;
    // @name specifies the start of the text that
    // should be selected in @link(jreFormulaDblClick).
    FNewSelectionStart: integer;
    // @name: boolean;
    // See @link(ResultSet).
    FResultSet: boolean;
    FSelectedNode: TTreeNode;
    FClickSelectionStart: integer;
    FDiagramObjectStorage: TList;
    FUpdating: Boolean;
    FSetColor: Boolean;
    // See @link(Formula).
    function GetFormula: string;
    // @name inserts NewText into the formula at
    // the position of the selected text.
    procedure InsertText(const NewText: string);
    // See @link(Formula).
    procedure SetFormula(const Value: string);
    procedure DiagramFormula;
    procedure MatchEndingParen(PriorSelection: TCharRange);
    procedure MatchStartingParen(PriorSelection: TCharRange);
    procedure GetGlobalVariables;
    procedure RemoveSpecialImplementor(AClass: TClass);
    procedure CreateNodesForVariables;
    { Private declarations }
  public
    procedure IncludeGIS_Functions;
    procedure RemoveActiveOnLayer;
    procedure RemoveSpecifiedHeadOnLayer;
    // Name used in the TTreeNode that holds TCustomVariables in @link(tvItems).
    // By default, it is 'Data Sets'.
    property DataSetGroupName: string read FDataSetGroupName
      write FDataSetGroupName;
    // use @name to read or set the formula.
    property Formula: string read GetFormula write SetFormula;
    // @name sets @link(rbFormulaParser) so that only the functions
    // set in TRbwParser.Functions by default are present.
    // GIS functions and any other extra functions are removed.
    procedure RemoveGIS_Functions;
    // @name is set to true if the formula has been successfully changed.
    property ResultSet: boolean read FResultSet;
    // @name creates the nodes in @link(tvItems).
    procedure UpdateTreeList;
    { Public declarations }
  end;

implementation

uses Contnrs, GIS_Functions, IntListUnit, frmGoPhastUnit, GlobalVariablesUnit,
  PhastModelUnit, DataSetUnit;

{$R *.dfm}

var
  PriorSelectedText: string = '';

procedure TfrmFormula.InsertText(const NewText: string);
const
{$IFDEF LINUX}
  BreakChar = [#10];
{$ELSE}
  BreakChar = [#10,#13];
{$ENDIF}
var
  Start, sLength: integer;
  Index: integer;
  Position: integer;
  NewExpression: string;
  TextToSelect: string;
  NewStart: integer;
  FormulaText: string;
  SelLength: integer;
begin
  try
    SelLength := jreFormula.SelLength;
    if SelLength > 0 then
    begin
      FormulaText := jreFormula.Lines.Text;
      Start := jreFormula.SelStart + 1;
      Delete(FormulaText, Start, SelLength);
      Insert(NewText, FormulaText, Start);
      jreFormula.Text := FormulaText;
      // eliminate line breaks.
      Formula := Formula;

      Position := Pos('(', NewText);
      if (Position >= 1) and (NewText <> '(') then
      begin
        TextToSelect := '';
        sLength := 0;
        for Index := Position + 1 to Length(NewText) do
        begin
          if (NewText[Index] in [' ', ',', ')']) then
          begin
            sLength := Index - Position - 1;
            TextToSelect := Copy(NewText, Position + 1, sLength);
            break;
          end;
        end;
        If TextToSelect <> '' then
        begin
          NewExpression := jreFormula.Lines.Text;
          Start := Position + Start;
          Start := PosEx(TextToSelect, NewExpression, Start) -1;

          jreFormula.SelStart := Start;
          jreFormula.SelLength := sLength;

          While(jreFormula.SelText <> TextToSelect)
            and (jreFormula.SelStart + sLength + 1 < Length(NewExpression)) do
          begin
            NewStart := jreFormula.SelStart + 1;
            jreFormula.SelStart := NewStart;
            jreFormula.SelLength := sLength;
            if jreFormula.SelStart <> NewStart then
            begin
              NewStart := NewStart + 1;
              jreFormula.SelStart := NewStart;
              jreFormula.SelLength := sLength;
              if jreFormula.SelStart <> NewStart then
              begin
                break
              end;
            end;
          end
        end;
      end
      else
      begin
        jreFormula.SelStart := Start - 1 + Length(NewText);
        jreFormula.SelLength := 0;
      end;
    end
    else
    begin
      jreFormula.SelText := NewText;
    end;
  finally
    FocusControl(jreFormula);
  end;
end;

procedure TfrmFormula.jreFormulaMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FClickSelectionStart := jreFormula.SelStart;
end;

procedure TfrmFormula.jreFormulaSelectionChange(Sender: TObject);
var
  PriorSelection: TCharRange;
  NewSelection: TCharRange;
  SelText: string;
begin
  if FUpdating then Exit;
  FUpdating := True;
  try
    PriorSelection := jreFormula.GetSelection;
    try
      if FSetColor then
      begin
        jreFormula.SetSelection(0, MAXINT, False);
        jreFormula.SelAttributes.BackColor := clWindow;
        jreFormula.SetSelection(PriorSelection.cpMin, PriorSelection.cpMax, False);
        FSetColor := False;
      end;

      NewSelection := PriorSelection;
      if NewSelection.cpMin = NewSelection.cpMax then
      begin
        Inc(NewSelection.cpMax);
      end;
      SelText := jreFormula.GetTextRange(NewSelection.cpMin, NewSelection.cpMax);
      if SelText = '(' then
      begin
        MatchStartingParen(PriorSelection);
      end
      else if SelText = ')' then
      begin
        MatchEndingParen(PriorSelection);
      end;
    finally
      jreFormula.SetSelection(PriorSelection.cpMin, PriorSelection.cpMax, False);
    end;
  finally
    FUpdating := False;
  end;
end;

procedure TfrmFormula.buttonClick(Sender: TObject);
var
  //  AString: string;
  NewText: string;
  //  Start, sLength: integer;
  UseSpaces: boolean;
  Index: integer;
  //  Sel: TMemoSelection;
  SomeButtons: array[0..15] of TButton;
begin
  SomeButtons[0] := btn1;
  SomeButtons[1] := btn2;
  SomeButtons[2] := btn3;
  SomeButtons[3] := btn4;
  SomeButtons[4] := btn5;
  SomeButtons[5] := btn6;
  SomeButtons[6] := btn7;
  SomeButtons[7] := btn8;
  SomeButtons[8] := btn9;
  SomeButtons[9] := btn0;
  SomeButtons[10] := btnDecimal;
  SomeButtons[11] := btnOpenParen;
  SomeButtons[12] := btnCloseParen;
  SomeButtons[13] := btnComma;
  SomeButtons[14] := btnQuote;
  SomeButtons[15] := btnE;
  UseSpaces := True;
  for Index := Low(SomeButtons) to High(SomeButtons) do
  begin
    if Sender = SomeButtons[Index] then
    begin
      UseSpaces := False;
      break;
    end;
  end;

  if (FLastButton = btnE) and ((Sender = btnPlus) or (Sender = btnMinus)) then
  begin
    UseSpaces := False;
  end;

  if UseSpaces then
  begin
    NewText := ' ' + (Sender as TButton).Caption + ' ';
  end
  else
  begin
    NewText := (Sender as TButton).Caption;
  end;

  InsertText(NewText);
  FLastButton := Sender as TButton;
end;

procedure TfrmFormula.DiagramFormula;
const
  TAB_CHAR = #9;
var
  AFormula: string;
  DiagramList: TStringList;
  Index: Integer;
  TabCount: integer;
  NodeStack: TList;
  CharIndex: integer;
  Line: string;
  FunctionName: string;
  FunctionFormula: string;
  TabPosition: integer;
  Node: TTreeNode;
  NodeData: TTreeNodeTextStorage;
begin
  tvFormulaDiagram.Items.Clear;
  FDiagramObjectStorage.Clear;
  AFormula := Formula;
  try
    if rbFormulaParser.Compile(AFormula) >= 0 then
    begin
      DiagramList := TStringList.Create;
      try
        rbFormulaParser.CurrentExpression.Diagram(DiagramList);
        NodeStack := TList.Create;
        try
          for Index := 0 to DiagramList.Count - 1 do
          begin
            Line := DiagramList[Index];
            TabCount := 0;
            for CharIndex := 1 to Length(Line) do
            begin
              if Line[CharIndex] = TAB_CHAR then
              begin
                Inc(TabCount)
              end
              else
              begin
                break;
              end;
            end;
            Line := Copy(Line, TabCount+1, MaxInt);
            TabPosition := Pos(TAB_CHAR, Line);
            FunctionName := Copy(Line, 1, TabPosition-1);
            FunctionFormula := Copy(Line, TabPosition+1, MAXINT);
            if TabCount >= NodeStack.Count then
            begin
              if NodeStack.Count > 0 then
              begin
                Node := NodeStack[NodeStack.Count-1];
              end
              else
              begin
                Node := nil;
              end;
              Node := tvFormulaDiagram.Items.AddChild(Node, FunctionFormula);
              NodeStack.Add(Node);
            end
            else
            begin
              Node := NodeStack[TabCount];
              Node := tvFormulaDiagram.Items.Add(Node, FunctionFormula);
              NodeStack[TabCount] := Node;
              NodeStack.Count := TabCount+1;
            end;
            NodeData := TTreeNodeTextStorage.Create;
            FDiagramObjectStorage.Add(NodeData);
            NodeData.OpenText := FunctionName;
            NodeData.ClosedText := FunctionFormula;
            Node.Data := NodeData;
          end;
        finally
          NodeStack.Free;
        end;
      finally
        DiagramList.Free;
      end;
    end;
  except on E: ErbwParserError do
    begin
      // ignore
    end;
  end;
end;

function TfrmFormula.GetFormula: string;
var
  Index: integer;
begin
  result := '';
  for Index := 0 to jreFormula.Lines.Count - 1 do
  begin
    result := result + jreFormula.Lines[Index];
    if (Length(result) > 0) and (result[Length(result)] <> ' ') then
    begin
      result := result + ' ';
    end;
  end;
  result := Trim(Result);
end;

procedure TfrmFormula.jreFormulaDblClick(Sender: TObject);
type
  TBrace = (bNone, bStart, bStop);
const
  Separators: set of Char = [' ', ',', '(', ')', #10, #13, #9, '*', '/', '+',
  '-', '"'];
var
  LineNumber: integer;
  AString: string;
  Index: integer;
  Stop, Start: integer;
  function CurlyBrace: TBrace;
  var
    SelectedString: string;
    SelectedChar: char;
  begin
    result := bNone;
    SelectedString := Copy(AString, Start, 1);
    if Length(SelectedString) > 0 then
    begin
      SelectedChar := SelectedString[1];
      if SelectedChar = '{' then
      begin
        result := bStart;
      end
      else if SelectedChar = '}' then
      begin
        result := bStop;
      end;

    end;
  end;
begin
  inherited;
  if FClickSelectionStart >= 0 then
  begin
    AString := jreFormula.Lines.Text;
    Stop := Length(AString);
    Start := 1;

    case CurlyBrace of
      bNone:
        begin
          // Change stop so it represents the location of the first separator
          // after the selected position.
          for Index := FClickSelectionStart + 1 to Length(AString) + 1 do
          begin
            if Index <= Length(AString) then
            begin
              Stop := Index;
              if AString[Index] in Separators then
              begin
                break;
              end;
            end
            else
            begin
              Stop := Length(AString) + 1;
            end;
          end;
          // Change Start so it represents the first character
          // after the first separator before the selected position.
          for Index := FClickSelectionStart downto 1 do
          begin
            if Index <= Length(AString) then
            begin
              Start := Index;
              if AString[Index] in Separators then
              begin
                Start := Start + 1;
                break;
              end;
            end
            else
            begin
              Start := Length(AString);
            end;
          end;
        end;
      bStart:
        begin
          for Index := FClickSelectionStart + 1 to Length(AString) + 1 do
          begin
            // change Stop so that it represents the first
            // end curly brace after the selected position.
            if Index <= Length(AString) then
            begin
              Stop := Index;
              if AString[Index] = '}' then
              begin
                Stop := Stop + 1;
                break;
              end;
            end
            else
            begin
              Stop := Length(AString) + 1;
            end;
          end;
        end;
      bStop:
        begin
            // change Start so that it represents the first
            // start curly brace before the selected position.
          for Index := FClickSelectionStart downto 1 do
          begin
            if Index <= Length(AString) then
            begin
              Start := Index;
              if AString[Index] = '{' then
              begin
                break;
              end;
            end
            else
            begin
              Start := Length(AString);
            end;
          end;
        end;
    else Assert(False);
    end;

    LineNumber := 0;
    Index := PosEx(#13#10, AString, 1);
    while (Index < Start) and (Index > 0) do
    begin
      Inc(LineNumber);
      Index := PosEx(#13#10, AString, Index+2);
    end;
    Start := Start - LineNumber*2;
    Stop := Stop - LineNumber*2;

    // Set FNewSelectionStart and FNewSelectionLength for use in
    // TimerSetSelection.
    FNewSelectionStart := Start - 1;
    if Stop <> Start then
    begin
      FNewSelectionLength := Stop - Start;
    end
    else
    begin
      FNewSelectionLength := 1;
    end;
    // The selection can't be changed here so do it in TimerSetSelectionO.
    Timer.OnTimer := TimerSetSelection;
    Timer.Enabled := True;
  end;
end;

procedure TfrmFormula.SetFormula(const Value: string);
begin
  jreFormula.Lines.Clear;
  jreFormula.Lines.Add(Value);
  jreFormula.SelectAll;
end;

procedure TfrmFormula.TimerSetSelection(Sender: TObject);
begin
  inherited;
  Timer.Enabled := False;
  jreFormula.SelStart := FNewSelectionStart;
  jreFormula.SelLength := FNewSelectionLength;
end;

procedure TfrmFormula.GetGlobalVariables;
begin
  frmGoPhast.PhastModel.RegisterGlobalVariables(rbFormulaParser);
end;

procedure TfrmFormula.IncludeGIS_Functions;
begin
  AddGIS_Functions(rbFormulaParser, frmGoPhast.PhastModel.ModelSelection);
end;

procedure TfrmFormula.RemoveSpecialImplementor(AClass: TClass);
var
  Index: Integer;
  Item: TSpecialImplementor;
begin
  for Index := rbFormulaParser.SpecialImplementorList.Count - 1 downto 0 do
  begin
    Item := rbFormulaParser.SpecialImplementorList[Index];
    if Item.Implementor = AClass then
    begin
      rbFormulaParser.SpecialImplementorList.Delete(Index);
    end;
  end;
end;

procedure TfrmFormula.RemoveActiveOnLayer;
begin
  RemoveSpecialImplementor(TActiveOnLayer);
end;

procedure TfrmFormula.RemoveSpecifiedHeadOnLayer;
begin
  RemoveSpecialImplementor(TSpecifiedHeadOnLayer);
end;

procedure TfrmFormula.FormCreate(Sender: TObject);
begin
  inherited;
  DataSetGroupName := 'Data Sets';

  Constraints.MinWidth := Width;
  pnlMain.Constraints.MinWidth := pnlMain.Width;
  pnlButtons.Constraints.MinWidth := pnlButtons.Width;

  FDiagramObjectStorage := TObjectList.Create;
  GetGlobalVariables;
end;

procedure TfrmFormula.FormDestroy(Sender: TObject);
begin
  inherited;
  FDiagramObjectStorage.Free;
end;

procedure TfrmFormula.RemoveGIS_Functions;
begin
  rbFormulaParser.Functions.Clear;
  rbFormulaParser.Functions.Create;
end;

procedure TfrmFormula.UpdateTreeList;
var
  Index: integer;
  FClass: TFunctionClass;
  Prototype: string;
  ChildName: string;
  SeparatorPos: integer;
  ParentNode: TTreeNode;
  ChildNode: TTreeNode;
  FunctionNames: TStringList;
  Item: TSpecialImplementor;
begin
  FunctionNames := TStringList.Create;
  try
    FunctionNames.Sorted := True;
    FunctionNames.Duplicates := dupIgnore;
    for Index := 0 to OverloadedFunctionList.Count - 1 do
    begin
      FClass := OverloadedFunctionList[Index] as TFunctionClass;
      if not FClass.Hidden then
      begin
        FunctionNames.AddObject(FClass.Prototype, FClass);
      end;
    end;
    for Index := 0 to rbFormulaParser.SpecialImplementorList.Count - 1 do
    begin
      Item := rbFormulaParser.SpecialImplementorList[Index];
      FClass := Item.FunctionClass;
      if not FClass.Hidden then
      begin
        FunctionNames.AddObject(FClass.Prototype, FClass);
      end;
    end;
    for Index := 0 to rbFormulaParser.Functions.Count - 1 do
    begin
      FClass := rbFormulaParser.Functions.FunctionClass[Index];
      if not FClass.Hidden then
      begin
        FunctionNames.AddObject(FClass.Prototype, FClass);
      end;
    end;

    CreateNodesForVariables;

    FFunctions := tvItems.Items.Add(nil, 'Functions');
    for Index := 0 to FunctionNames.Count - 1 do
    begin
      ParentNode := FFunctions;
      FClass := FunctionNames.Objects[Index] as TFunctionClass;
      Prototype := FClass.Prototype;
      SeparatorPos := Pos('|', Prototype);
      while SeparatorPos > 0 do
      begin
        ChildName := Copy(Prototype, 1, SeparatorPos - 1);
        Prototype := Copy(Prototype, SeparatorPos + 1, MAXINT);
        ChildNode := ParentNode.getFirstChild;
        while ChildNode <> nil do
        begin
          if ChildNode.Text = ChildName then
          begin
            ParentNode := ChildNode;
            Break;
          end;
          ChildNode := ParentNode.GetNextChild(ChildNode);
        end;
        if ChildNode = nil then
        begin
          ParentNode := tvItems.Items.AddChild(ParentNode, ChildName);
        end;
        SeparatorPos := Pos('|', Prototype);
      end;

      tvItems.Items.AddChildObject(ParentNode, FClass.Name, FClass);
    end;
  finally
    FunctionNames.Free;
  end;
end;

procedure TfrmFormula.CreateNodesForVariables;
var
  Classifications: TStringList;
  AVariable: TCustomValue;
  Index: Integer;
  VariableList: TClassificationList;
  ClassifiedLayerList: TClassificationList;
  LayerList: TList;
  VarEdit: TVariableEdit;
  LayerStringList: TStringList;
  DataArray: TDataArray;
  Position: integer;
  Node: TTreeNode;
  HydrogeologicUnitNames: TStringList;
  HufDataArrays: TClassificationList;
begin
  { TODO : Nearly the same code is use in TfrmFormulaUnit, TFrmGridColor,
  TfrmScreenObjectProperties, and TfrmDataSets. Find a way to combine them. }
  if rbFormulaParser.VariableCount > 0 then
  begin
    HydrogeologicUnitNames := TStringList.Create;
    HufDataArrays := TClassificationList.Create;
    VariableList := TClassificationList.Create;
    ClassifiedLayerList := TClassificationList.Create;
    LayerList := TList.Create;
    LayerStringList := TStringList.Create;
    try
      frmGoPhast.PhastModel.HydrogeologicUnits.FillDataArrayNames(
        HydrogeologicUnitNames);
      HydrogeologicUnitNames.CaseSensitive := False;
      for Index := 0 to HydrogeologicUnitNames.Count - 1 do
      begin
        HufDataArrays.Add(nil);
      end;

      // The variables that represent data arrays used for defining
      // the MODFLOW layer boundaries should be sorted in order from
      // top to bottom rather than alphabetically.  Use ClassifiedLayerList
      // to sort them that way.
      frmGoPhast.PhastModel.GetLayerGroupDataSets(LayerList);
      for Index := 0 to LayerList.Count - 1 do
      begin
        DataArray := LayerList[Index];
        ClassifiedLayerList.Add(nil);
        LayerStringList.Add(DataArray.Name);
      end;

      for Index := 0 to rbFormulaParser.VariableCount - 1 do
      begin
        AVariable := rbFormulaParser.Variables[Index];
        VarEdit := TVariableEdit.Create(AVariable);
        VariableList.Add(VarEdit);
        Position := LayerStringList.IndexOf(VarEdit.ClassificationName);
        if Position >= 0 then
        begin
          ClassifiedLayerList[Position] := VarEdit;
        end;
        Position := HydrogeologicUnitNames.IndexOf(VarEdit.ClassificationName);
        if Position >= 0 then
        begin
          HufDataArrays[Position] := VarEdit;
        end;
      end;
      ClassifiedLayerList.Pack;
      HufDataArrays.Pack;
      Classifications := TStringList.Create;
      try
        ClassifyListedObjects(Classifications, VariableList,
          [ClassifiedLayerList, HufDataArrays]);

        CreateClassifiedNodes(Classifications, 0, tvItems, '');

        // In each node, replace the TVariableEdit with the
        // TCustomValue that it holds.
        for Index := 0 to tvItems.Items.Count - 1 do
        begin
          Node := tvItems.Items[Index];
          VarEdit := Node.Data;
          if VarEdit <> nil then
          begin
            Node.Data := VarEdit.Variable;
          end;
        end;
      finally
        Classifications.Free;
      end;
    finally
      for Index := 0 to VariableList.Count - 1 do
      begin
        VariableList[Index].Free;
      end;
      VariableList.Free;
      ClassifiedLayerList.Free;
      LayerList.Free;
      LayerStringList.Free;
      HufDataArrays.Free;
      HydrogeologicUnitNames.Free;
    end;
  end;
end;

procedure TfrmFormula.btnOKClick(Sender: TObject);
var
  AFormula: string;
begin
  inherited;
  AFormula := Formula;
  try
    if rbFormulaParser.Compile(AFormula) >= 0 then
    begin
      FResultSet := True;
    end;
    Close;
  except on E: ErbwParserError do
    begin
      btnOK.Enabled := False;
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
  if tvItems.Selected <> nil then
  begin
    PriorSelectedText := tvItems.Selected.Text;
  end;
end;

procedure TfrmFormula.jreFormulaChange(Sender: TObject);
begin
  inherited;
  btnOK.Enabled := True;
  DiagramFormula;
end;

procedure TfrmFormula.SplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  inherited;
//  Accept := Width - NewSize - Splitter.Width >= pnlMain.Constraints.MinWidth;
  Accept := NewSize >= pnlMain.Constraints.MinWidth;
//  if not Accept then Beep;
end;

procedure TfrmFormula.FormShow(Sender: TObject);
var
  Index: Integer;
  Item: TTreeNode;
begin
  inherited;
  if PriorSelectedText <> '' then
  begin
    for Index := 0 to tvItems.Items.Count - 1 do
    begin
      Item := tvItems.Items[Index];
      if Item.Text = PriorSelectedText then
      begin
        tvItems.Selected := Item;
        break;
      end;
    end;
  end;
  jreFormula.SetFocus;
end;

procedure TfrmFormula.btnFunctionHelpClick(Sender: TObject);
begin
  HelpRouter.HelpJump('', FFunctionHelpString);
//  Application.HelpJump('files\Functions.htm#'+ FFunctionHelpString);
  inherited;
end;

procedure TfrmFormula.tvFormulaDiagramCollapsed(Sender: TObject;
  Node: TTreeNode);
var
  NodeData: TTreeNodeTextStorage;
begin
  inherited;
  NodeData := Node.Data;
  Node.Text := NodeData.ClosedText;
end;

procedure TfrmFormula.tvFormulaDiagramExpanded(Sender: TObject;
  Node: TTreeNode);
var
  NodeData: TTreeNodeTextStorage;
begin
  inherited;
  NodeData := Node.Data;
  Node.Text := NodeData.OpenText;
end;

procedure TfrmFormula.tvItemsChange(Sender: TObject; Node: TTreeNode);
var
  IsFunction: boolean;
begin
  inherited;
  FSelectedNode := Node;
  FFunctionHelpString := Node.Text;
  IsFunction := False;
  Assert(FFunctions <> nil);
  while (Node <> nil) and (Node <> FFunctions) do
  begin
    Node := Node.Parent;
    if Node = FFunctions then
    begin
      IsFunction := True;
      break;
    end;
  end;
  btnFunctionHelp.Enabled := IsFunction;
end;

procedure TfrmFormula.tvItemsDblClick(Sender: TObject);
var
  AnObject: TObject;
  FClass: TFunctionClass;
  Value: TCustomValue;
  Prototype: string;
  SeparatorPos: integer;
begin
  inherited;
  Assert(FSelectedNode <> nil);
  AnObject := FSelectedNode.Data;
  if AnObject <> nil then
  begin
    if AnObject is TFunctionClass then
    begin
      FClass := FSelectedNode.Data;
      Prototype := FClass.Prototype;
      SeparatorPos := Pos('|', Prototype);
      while SeparatorPos > 0 do
      begin
        Prototype := Copy(Prototype, SeparatorPos + 1, MAXINT);
        SeparatorPos := Pos('|', Prototype);
      end;
      InsertText(Prototype);
    end
    else if AnObject is TCustomValue then
    begin
      Value := FSelectedNode.Data;
      InsertText(Value.Decompile);
    end;
  end;
end;

procedure TfrmFormula.MatchStartingParen(PriorSelection: TCharRange);
var
  Index: Integer;
  Level: Integer;
  StoredLevel: Integer;
  SelText: string;
  NewSelection: TCharRange;
  InString: boolean;
begin
  NewSelection.cpMin := 0;
  NewSelection.cpMax := 1;
  StoredLevel := -1;
  Level := 0;
  InString := False;
  for Index := 0 to MAXINT do
  begin
    SelText := jreFormula.GetTextRange(NewSelection.cpMin, NewSelection.cpMax);
    jreFormula.SetSelection(NewSelection.cpMin, NewSelection.cpMax, False);
    if SelText = '' then
    begin
      break;
    end
    else if (SelText = '"') and not InString then
    begin
      InString := True;
    end
    else if SelText = '"' then
    begin
      InString := False;
    end
    else if InString then
    begin
      if NewSelection.cpMin = PriorSelection.cpMin then
      begin
        break;
      end;
    end
    else if SelText = '(' then
    begin
      Inc(Level);
      if NewSelection.cpMin = PriorSelection.cpMin then
      begin
        jreFormula.SetSelection(NewSelection.cpMin, NewSelection.cpMax, False);
        jreFormula.SelAttributes.BackColor := clAqua;
        FSetColor := True;
        StoredLevel := Level;
      end;
    end
    else if SelText = ')' then
    begin
      if Level = StoredLevel then
      begin
        jreFormula.SetSelection(NewSelection.cpMin, NewSelection.cpMax, False);
        jreFormula.SelAttributes.BackColor := clAqua;
        break;
      end;
      Dec(Level);
    end;
    Inc(NewSelection.cpMin);
    Inc(NewSelection.cpMax);
  end;
end;

procedure TfrmFormula.MatchEndingParen(PriorSelection: TCharRange);
var
  Levels: TIntegerList;
  Starts: TIntegerList;
  StoredLevel: Integer;
  Level: Integer;
  Index: Integer;
  NewSelection: TCharRange;
  SelText: string;
  InString: boolean;
begin
  Levels := TIntegerList.Create;
  Starts := TIntegerList.Create;
  try
    NewSelection.cpMin := 0;
    NewSelection.cpMax := 1;
    StoredLevel := -1;
    Level := 0;
    InString := False;
    for Index := 0 to MAXINT do
    begin
      SelText := jreFormula.GetTextRange(NewSelection.cpMin, NewSelection.cpMax);
//      SelText := jreFormula.SelText;
      if SelText = '' then
      begin
        break;
      end
      else if (SelText = '"') and not InString then
      begin
        InString := True;
      end
      else if SelText = '"' then
      begin
        InString := False;
      end
      else if InString then
      begin
        if NewSelection.cpMin = PriorSelection.cpMin then
        begin
          break;
        end;
      end
      else if SelText = '(' then
      begin
        Inc(Level);
        Levels.Add(Level);
        Starts.Add(NewSelection.cpMin);
      end
      else if SelText = ')' then
      begin
        if NewSelection.cpMin = PriorSelection.cpMin then
        begin
          jreFormula.SetSelection(NewSelection.cpMin, NewSelection.cpMax, False);
          jreFormula.SelAttributes.BackColor := clAqua;
          StoredLevel := Level;
          FSetColor := True;
          break;
        end;
        Dec(Level);
      end;
      Inc(NewSelection.cpMin);
      Inc(NewSelection.cpMax);
    end;
    if StoredLevel >= 0 then
    begin
      for Index := Levels.Count - 1 downto 0 do
      begin
        if Levels[Index] = StoredLevel then
        begin
          jreFormula.SetSelection(Starts[Index], Starts[Index] + 1, False);
          jreFormula.SelAttributes.BackColor := clAqua;
          break;
        end;
      end;
    end;
  finally
    Levels.Free;
    Starts.Free;
  end;
end;

{ TVariableEdit }

function TVariableEdit.ClassificationName: string;
begin
  result := FVariable.Decompile;
end;

constructor TVariableEdit.Create(AVariable: TCustomValue);
begin
  inherited Create;
  FVariable := AVariable;
end;

function TVariableEdit.FullClassification: string;
begin
  result := FVariable.Classification;
end;

end.
