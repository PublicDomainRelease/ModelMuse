unit frameFileListHandlerUnit;

interface

uses
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, FileIndexUnit, JvSpin;

type
  TframeFileListHandler = class(TFrame)
    redtIndex: TRichEdit;
    procedure redtIndexDblClick(Sender: TObject);
  private
    FStartPosition: Integer;
    FLineNumbers: TIntList;
    FCount: integer;
    { Private declarations }
  public
    Memo: TMemo;
    ListFile: TFileIndex;
    LineCountToRead: TJvSpinEdit;
    procedure Initialize;
    procedure AddIndexLine(LineIndex: integer; ALine: string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddLines;
    procedure InsertLines;
    { Public declarations }
  end;

implementation

uses
  Math;

{$R *.dfm}

{ TFrame3 }

procedure TframeFileListHandler.AddLines;
var
  NewLines: TStringList;
  NumberOAfLinesToAdd: Integer;
  StartingPosition: Integer;
begin
  StartingPosition := FStartPosition + FCount;
  if StartingPosition >= ListFile.LineCount then
  begin
    Exit;
  end;
  NumberOAfLinesToAdd := LineCountToRead.AsInteger;
  NewLines := TStringList.Create;
  try
    ListFile.ReadLines(NewLines, StartingPosition, NumberOAfLinesToAdd);
    NumberOAfLinesToAdd := NewLines.Count;
    Memo.Lines.AddStrings(NewLines);
    FCount := FCount + NumberOAfLinesToAdd;
  finally
    NewLines.Free;
  end;
  Memo.SetFocus;
end;

constructor TframeFileListHandler.Create(AOwner: TComponent);
begin
  inherited;
  FLineNumbers:= TIntList.Create;
end;

destructor TframeFileListHandler.Destroy;
begin
  FLineNumbers.Free;
  inherited;
end;

procedure TframeFileListHandler.AddIndexLine(LineIndex: integer; ALine: string);
begin
  redtIndex.Lines.Add(IntToStr(LineIndex) + ' ' + Trim(ALine));
  FLineNumbers.Add(LineIndex);
end;

procedure TframeFileListHandler.Initialize;
begin
  redtIndex.Lines.Clear;
  FLineNumbers.Clear;
end;

procedure TframeFileListHandler.InsertLines;
var
  NumberOAfLinesToInsert: Integer;
  StartingPosition: Integer;
  NewLines: TStringList;
  LineIndex: Integer;
begin
  NumberOAfLinesToInsert := LineCountToRead.AsInteger;
  StartingPosition := FStartPosition - NumberOAfLinesToInsert;
  if StartingPosition < 0 then
  begin
    NumberOAfLinesToInsert := NumberOAfLinesToInsert + StartingPosition;
    StartingPosition := 0;
  end;
  NewLines := TStringList.Create;
  try
    ListFile.ReadLines(NewLines, StartingPosition, NumberOAfLinesToInsert);
    Memo.Lines.BeginUpdate;
    try
      for LineIndex := NewLines.Count - 1 downto 0 do
      begin
        Memo.Lines.Insert(0, NewLines[LineIndex]);
      end;
    finally
      Memo.Lines.EndUpdate;
    end;
    FStartPosition := StartingPosition;
    FCount := FCount + NumberOAfLinesToInsert;
  finally
    NewLines.Free;
  end;
  Memo.SetFocus;
end;

procedure TframeFileListHandler.redtIndexDblClick(Sender: TObject);
var
  StartIndex: Integer;
begin
  Assert(Memo <> nil);
  Assert(ListFile <> nil);
  Assert(LineCountToRead <> nil);
  StartIndex := FLineNumbers[redtIndex.ActiveLineNo];
  FStartPosition := Max(0, StartIndex - 10);
  FCount := LineCountToRead.AsInteger;
  Memo.Lines.BeginUpdate;
  try
    ListFile.ReadLines(Memo.Lines, FStartPosition, FCount);
  finally
    Memo.Lines.EndUpdate;
  end;
  FCount := Memo.Lines.Count;
  Memo.CaretPos := Point(0,StartIndex-FStartPosition);
  Memo.SelLength := 2;
  Memo.SetFocus;
end;

end.
