{*******************************************************}
{       Rapware Delphi/Kylix XML basic parser           }
{  simple XML parser for XML 2 Binary 2 XML conversion  }
{                                                       }
{       Copyright (C) 2000,2001 Rapware                 }
{                                                       }
{       www.rapware.com   info@rapware.com              }
{*******************************************************}
unit rwXMLParser;
interface
uses
  sysutils,
  classes;

type
  TrwXMLParser = class(TObject)
  private
    // the stream which holds the XML formated DFM / component stream
    fInputStream: TStream;
    FElementName: string;
    FAttribs: TStringList;
    FData: String;
    FEndFound: boolean;
    FValidRead: boolean;
    FBuffer: PChar;
    FStreamSize: integer;
    FShowProgress: boolean;

    // property setter for the Attribs
    procedure SetAttribs(const Value: TStringList);
    // property setter for the ElementName of the last read Element
    procedure SetElementName(const Value: string);
    // property setter for the Data of the last read XML element
    procedure SetData(const Value: String);
    // property setter for if a end is found in the last read
    procedure SetEndFound(const Value: boolean);
    // property setter for if the last read was valid
    procedure SetValidRead(const Value: boolean);
    // removes the spaces from the current line
    procedure TrimSpaces;
    // Property Setter
    procedure SetStreamSize(const Value: integer);

    // Reads a validName form the stream
    function ReadNameStr: string;
    // Read a full Attribute from the stream
    procedure ReadAttrStr(var AAttrib : string; var isLast: boolean);
    function getNextChar: Char;
    function CheckNextChar: Char;
    procedure CharBack;
    procedure ReadData;
    procedure SetShowProgress(const Value: boolean);

  protected

  public
    // The constructor gets a Stream which hold the XML formatted
    // DFM / Component stream
    constructor Create(Stream: TStream);
    // The destructor cleans all temporary claimed memory.
    destructor Destroy; override;
    // Read the Next Element with all its Attribs and data if on the same line
    procedure ReadNextElement; virtual;
    // This function returns true if we are at the end of the stream
    function EOS: boolean;
    // Returns the value of Attribute
    function ValueOf(const aName: string): string;
    // Returns the Data as a Integer
    function IntegerData: Integer;
    // The Value can be a 64 bit integer
    function Int64Data: Int64;
    // Returns the Data as a Float
    function FloatData: Extended;

    // True if the last read was ended with a valid Element
    property ValidRead: boolean read FValidRead write SetValidRead;
    // The name of the last read Element
    property ElementName: string read FElementName write SetElementName;
    // All attributes of the last read Element are saved in a stringlist
    property Attribs: TStringList read FAttribs write SetAttribs;
    // The Data in the form of a string
    property Data: String read FData write SetData;
    // If the Element was closed then this property is true
    property EndFound: boolean read FEndFound write SetEndFound;
    // The actual size of the stream.
    property StreamSize: integer read FStreamSize write SetStreamSize;

    property ShowProgress: boolean read FShowProgress write SetShowProgress;
  end;

implementation


{ TrwXMLParser }
const
  BufferSize= 4096;

constructor TrwXMLParser.Create(Stream: TStream);
begin
  inherited create;
  FInputStream := Stream;
  FStreamSize := Stream.Size;
  GetMem(FBuffer,BufferSize);
  FAttribs := TStringList.Create;
  FShowProgress := false;
end;

destructor TrwXMLParser.Destroy;
begin
  FreeMem(FBuffer,BufferSize);
  FAttribs.Free;
  inherited;
end;

function TrwXMLParser.EOS : boolean;
begin
  result := (fInputStream.Position = fInputStream.Size);
end;

procedure TrwXMLParser.TrimSpaces;
var
  I: integer;
  C: Char;
begin
  I := fInputStream.Read(C,1);
  while (I=1) and ((C =' ') or (C < #32)) do
    I := fInputStream.Read(C,1);
  if I = 1 then
    fInputStream.Position := fInputStream.Position - 1;
end;

function TrwXMLParser.ReadNameStr: string;
var
  I: integer;
  C: Char;
begin
  I := fInputStream.Read(C,1);
  if I=1 then // throw away the "<"
    I := fInputStream.Read(C,1);
  if (I=1) and (C = '/') then
  begin
    EndFound := true;
    C := GetNextChar;
  end;

  while (I=1) and (C in ['a'..'z','A'..'Z','_','0'..'9']) do
  begin
    result := result + C;
    I := fInputStream.Read(C,1);
  end;
  fInputStream.Position := fInputStream.Position - 1;
end;


function  TrwXMLParser.getNextChar : Char;
var
  I: integer;
  C: Char;
begin
  I := fInputStream.Read(C,1);
  result := C;
  if I = 0 then
    raise Exception.Create('XML Stream read error');
end;

function  TrwXMLParser.CheckNextChar : Char;
var
  I: integer;
  C: Char;
begin
  I := fInputStream.Read(C,1);
  CharBack;
  result := C;
  if I = 0 then
    raise Exception.Create('XML Stream read error');
end;

procedure TrwXMLParser.CharBack;
begin
  fInputStream.Position := fInputStream.Position-1;
end;

procedure TrwXMLParser.ReadAttrStr(var aAttrib : string; var isLast: boolean);
var
  C: Char;
begin
  aAttrib := '';
  repeat // remove all spaces
    C := GetNextChar;
  until C <> ' ';
  if (C = '/') or (C = '>') then
  begin
    isLast := true;
    if C = '/' then
    begin
      C := GetNextChar;
      EndFound := (C = '>');
    end;
    Exit;
  end
  else
  begin
    repeat
      aAttrib := aAttrib + C;
      C := GetNextChar;
    until (C = '=') or (C < #32);
    if C < #32 then
      raise Exception.Create('XML stream read Error');
  end;
  while (C <> '"') do // remove all spaces
    C := GetNextChar;
  C := GetNextChar;
  aAttrib := aAttrib + '=';
  repeat
    aAttrib := aAttrib + C;
    C := GetNextChar;
    if C = '"' then
    begin
      while (C = '"') and (CheckNextChar = '"') do
      begin
        aAttrib := aAttrib + '"';
        GetNextChar;
        C := GetNextChar;
      end;
    end;
  until (C = '"') or (C < #32);       
  if C < #32 then
    raise Exception.Create('XML stream read Error');
end;


procedure TrwXMLParser.ReadData;
var
  C: Char;
begin
  Data := '';
  C := GetNextChar;
  while C <> '<' do
  begin
    Data := Data + C;
    C := GetNextChar;
  end;
  CharBack;
end;

procedure TrwXMLParser.ReadNextElement;
var
  isLast: boolean;
  aAttrib: String;
begin
  ValidRead := false;
  ElementName := '';
  Attribs.Clear;
  Data := '';
  EndFound := false;
  TrimSpaces;
  ElementName := ReadNameStr;
  IsLast := false;
  while not isLast do
  begin
    ReadAttrStr(aAttrib, isLast);
    if aAttrib > '' then
      Attribs.Add(aAttrib);
  end;
  if not EndFound then
    ReadData;
end;

procedure TrwXMLParser.SetAttribs(const Value: TStringList);
begin
  FAttribs := Value;
end;

procedure TrwXMLParser.SetData(const Value: String);
begin
  FData := Value;
end;

procedure TrwXMLParser.SetElementName(const Value: string);
begin
  FElementName := Value;
end;

procedure TrwXMLParser.SetEndFound(const Value: boolean);
begin
  FEndFound := Value;
end;

procedure TrwXMLParser.SetValidRead(const Value: boolean);
begin
  FValidRead := Value;
end;

procedure TrwXMLParser.SetStreamSize(const Value: integer);
begin
  FStreamSize := Value;
end;

function TrwXMLParser.ValueOf(const aName: string) : string;
begin
  result := Trim(Attribs.Values[aName]);
end;

function TrwXMLParser.IntegerData: Integer;
begin
  result := StrToInt(Trim(Data));
end;

function TrwXMLParser.Int64Data: Int64;
begin
  result := StrToInt64(Trim(Data));
end;

function TrwXMLParser.FloatData: Extended;
begin
  result := StrToFloat(Trim(Data));
end;

procedure TrwXMLParser.SetShowProgress(const Value: boolean);
begin
  FShowProgress := Value;
end;

end.

