unit FileIndexUnit;

interface

uses
  Classes, IOUtils, Generics.Collections, SysUtils;

type
  TIntList = class (TList<Int64>);

  TOnProgress = procedure (Sender: TObject; PerMil: integer) of object;

  TFileIndex = class(TObject)
  private
    FFile: TStreamReader;
    FPositions: TIntList;
    FFileName: string;
    FOnProgress: TOnProgress;
    function GetLine(Index: integer): string;
    function GetLineCount: integer;
    procedure SetFileName(const Value: string);
  public
    Constructor Create;
    property FileName: string read FFileName write SetFileName;
    Destructor Destroy; override;
    property Line[Index: integer]: string read GetLine;
    procedure ReadLines(Lines: TStrings; StartLine, Count: integer);
    property LineCount: integer read GetLineCount;
    function ReadLine: string;
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
  end;

implementation

{ TFileIndex }

constructor TFileIndex.Create;
begin
  FPositions := TIntList.Create;
end;

destructor TFileIndex.Destroy;
begin
  FPositions.Free;
  FFile.Free;
  inherited;
end;

function TFileIndex.GetLine(Index: integer): string;
begin
  FFile.BaseStream.Position := FPositions[Index];
  FFile.DiscardBufferedData;
  result := FFile.ReadLine;
end;

function TFileIndex.GetLineCount: integer;
begin
  result := FPositions.Count;
end;

function TFileIndex.ReadLine: string;
begin
  result := FFile.ReadLine;
end;

procedure TFileIndex.ReadLines(Lines: TStrings; StartLine, Count: integer);
var
  Index: Integer;
begin
  Lines.Clear;
  Lines.Capacity := Count;
  FFile.BaseStream.Position := FPositions[StartLine];
  FFile.DiscardBufferedData;
  for Index := 0 to Count - 1 do
  begin
    Lines.Add(FFile.ReadLine);
    if FFile.EndOfStream then
    begin
      break;
    end;
  end;
end;

procedure TFileIndex.SetFileName(const Value: string);
var
  PriorPosition: Int64;
  ALine: string;
  LineEndLength: Int64;
  PerMil: Integer;
  NewPerMil: Int64;
  Divider: Int64;
begin
  LineEndLength := Length(sLineBreak);
  FFileName := Value;
  FreeAndNil(FFile);
  FPositions.Clear;
  FFile := TFile.OpenText(FileName);
  PriorPosition := 0;
  Divider := FFile.BaseStream.Size div 1000;
  PerMil := 0;
  while not FFile.EndOfStream do
  begin
    FPositions.Add(PriorPosition);
    ALine := FFile.ReadLine;
    PriorPosition  := PriorPosition + Length(ALine) + LineEndLength;
    if Assigned(OnProgress) and (Divider > 0) then
    begin
      NewPerMil := PriorPosition div Divider;
      if NewPerMil > PerMil then
      begin
        PerMil := NewPerMil;
        OnProgress(self, PerMil);
      end;
    end;
  end;
  FFile.BaseStream.Position := 0;
  FFile.DiscardBufferedData;
end;

end.
