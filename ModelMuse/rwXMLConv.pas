{*******************************************************}
{       Rapware Delphi/Kylix XML conversion library     }
{       XML to Binary DFM/XFM conversion                }
{                                                       }
{       Copyright (C) 2000,2001 Rapware                 }
{                                                       }
{       www.rapware.com   info@rapware.com              }
{*******************************************************}
unit rwXMLConv;
interface
uses
  rwXMLParser,
  SysUtils,
  Classes;

{$IFNDEF ver140}
  {$DEFINE D7}
{$ELSE}
  {$IFDEF LINUX}
    {$DEFINE D7}
  {$ENDIF}
{$ENDIF}

procedure rwObjectBinaryToXML(Input, Output: TStream);
procedure rwObjectXMLToBinary(Input, Output: TStream);

implementation
uses
{$IFDEF ver180}
  // Allow inlining of AnsiCompareText
  Windows,
{$ENDIF}
  TypInfo;

const
  CrLf = #13#10;

  scHeaderBegin           = '<%s type="%s" name="%s">';
  scHeaderBeginFlags      = '<%s type="%s" name="%s" order="%d">';
  scHeaderEnd             = '</%s>';
  scProperties            = 'properties';
  scPropertiesBegin       = '<properties>';
  scPropertiesEnd         = '</properties>';
  scPropertie             = 'propertie';
  scValueType             = 'vt';
  scPropertieBegin        = '<propertie name="%s" vt="%s">';

  scPropertieEnd          = '</propertie>';
  scComponents            = 'components';
  scComponentsBegin       = '<components>';
  scComponentsEnd         = '</components>';
  scObject                = 'object';
  scInherited             = 'inherited';
  scInline                = 'inline';
  scBinaryItem            = 'bin';
  scBinaryItemBegin       = '<bin>';
  scBinaryItemEnd         = '</bin>';
  scBinaryEnd             = '<bin/>';

  scList                  = 'list';
  scListBegin             = '<list>';
  scListEnd               = '</list>';
  scListItem              = 'li';
  scListItemBegin         = '<li vt="%s">';
  scListItemEnd           = '</li>';
  scCollection            = 'collection';
  scCollectionBegin       = '<collection>';
  scCollectionEnd         = '</collection>';
  scCollectionItem        = 'item';
  scCollectionItemBegin   = '<item>';
  scCollectionItemBeginValue = '<item value="%d"';
  scCollectionItemEnd     = '</item>';

function StringToXML(const S: String) : String;
begin
  Result := StringReplace(     S, '&',  '&amp;',   [rfReplaceAll]);
  Result := StringReplace(Result, '<',  '&lt;',    [rfReplaceAll]);
  Result := StringReplace(Result, '>',  '&gt;',    [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&apos;',  [rfReplaceAll]);
  Result := StringReplace(Result, '"',  '&quot;',  [rfReplaceAll]);
  Result := StringReplace(Result, CrLf,'&crlf;',   [rfReplaceAll]);
end;

function XMLToString(const S: String) : String;
begin
  Result := StringReplace(S     , '&crlf;',CrLf,  [rfReplaceAll]);
  Result := StringReplace(Result, '&quot;', '"',  [rfReplaceAll]);
  Result := StringReplace(Result, '&apos;', '''', [rfReplaceAll]);
  Result := StringReplace(Result, '&gt;',   '>',  [rfReplaceAll]);
  Result := StringReplace(Result, '&lt;',   '<',  [rfReplaceAll]);
  Result := StringReplace(Result, '&amp;',  '&',  [rfReplaceAll]);
end;

function StrToWideStr(aStr: string) : WideString;
var
  C: Char;
  I, X: integer;
  S: string;
  procedure Error;
  begin
    raise Exception.Create('Invalid XML String encoding');
  end;

begin
  result := '';
  X := Length(aStr);
  I := 0;
  while I < X do
  begin
    Inc(I);
    C := aStr[I];
    if C = '&' then
    begin
      Inc(I);
      S:= '';
      while (I < X) and (aStr[I] <> ';')do
      begin
        S := S + Upcase(aStr[I]);
        Inc(I);
      end;
      if (aStr[I] = ';') and (S > '') then
      begin
        case S[1] of
          '#': // Numeric value of string
              begin
                if copy(S,2,MaxInt) < '256' then
                  result := result + Chr(StrToInt(copy(S,2,MaxInt)))
                else
                  result := result + WideChar(StrToInt(copy(S,2,MaxInt)));
              end;
          'Q': if S = 'QUOT' then result := result + '"' else Error;
          'A': if S = 'APOS' then result := result + '''' else
               if S = 'AMP'  then result := result + '&' else
                Error;
          'G': if S = 'GT' then result := result + '>' else Error;
          'L': if S = 'LT' then result := result + '<' else Error;
          else
            Error;
        end;
      end
      else
        Error;
    end
    else
      result := result + aStr[I];
  end;
end;

function WideToStr(W: WideString) : string;
var
  C: Char;
  I: integer;
begin
  result := '';
  for I := 1 to Length(W) do
  begin
    if (Ord(W[i]) <= 255) then
    begin
      C := Char(Ord(W[I]));
      case C of
        #0..#31,#129..#255 :
          result := result + '&#' + IntToStr(Ord(C))+ ';';
        '&':
          result := result + '&amp;';
        '<':
          result := result + '&lt;';
        '>':
          result := result + '&gt;';
        '''':
          result := result + '&apos;';
        '"':
          result := result + '&quot;';
        else
          result := result + C;
      end;
    end
    else
      result := result + '&#' + IntToStr(Ord(W[i]))+ ';';
  end;
end;

function StrToXMLStr(aStr: string) : string;
var
  I: integer;
begin
  result := '';
  for I := 1 to Length(aStr) do
  begin
    case aStr[I] of
      #0..#31,#129..#255 :
        result := result + '&#' + IntToStr(Ord(aStr[I]))+ ';';
      '&':
        result := result + '&amp;';
      '<':
        result := result + '&lt;';
      '>':
        result := result + '&gt;';
      '''':
        result := result + '&apos;';
      '"':
        result := result + '&quot;';
      else
        result := result + aStr[I];
    end;
  end;
end;

function XMLStrToStr(aStr: string) : string;
var
  C: Char;
  I, X: integer;
  S: string;
  procedure Error;
  begin
    raise Exception.Create('Invalid XML String encoding');
  end;

begin
  result := '';
  X := Length(aStr);
  I := 0;
  while I < X do
  begin
    Inc(I);
    C := aStr[I];
    if C = '&' then
    begin
      Inc(I);
      S:= '';
      while (I < X) and (aStr[I] <> ';')do
      begin
        S := S + Upcase(aStr[I]);
        Inc(I);
      end;
      if (aStr[I] = ';') and (S > '') then
      begin
        case S[1] of
          '#': // Numeric value of string
              result := result + Chr(StrToInt(copy(S,2,MaxInt)));
          'Q': if S = 'QUOT' then result := result + '"' else Error;
          'A': if S = 'APOS' then result := result + '''' else
               if S = 'AMP'  then result := result + '&' else
                Error;
          'G': if S = 'GT' then result := result + '>' else Error;
          'L': if S = 'LT' then result := result + '<' else Error;
          else
            Error;
        end;
      end
      else
        Error;
    end
    else
      result := result + aStr[I];
  end;
end;


{ Rapware Binary to text conversion }

procedure rwObjectBinaryToXML(Input, Output: TStream);
var
  NestingLevel: Integer;
  SaveSeparator: Char;
  Reader: TReader;
  Writer: TWriter;

  procedure WriteIndent;
  const
    Blanks: array[0..1] of Char = '  ';
  var
    I: Integer;
  begin
    for I := 1 to NestingLevel do Writer.Write(Blanks, SizeOf(Blanks));
  end;

  procedure WriteStr(const S: string);
  begin
    if Length(S) > 0 then
    begin
      Writer.Write(S[1], Length(S));
    end;
  end;

  procedure WriteStrLn(const S: string);
  begin
    WriteStr(S + CrLf);
  end;

  procedure NewLine;
  begin
    WriteStr(CrLf);
    WriteIndent;
  end;


  procedure ConvertValue(Vt : TValueType); forward;

  function ConvertHeader: string;
  var
    ClassName, ObjectName, StyleName: string;
    Flags: TFilerFlags;
    Position: Integer;
  begin
    Result := '';
    Reader.ReadPrefix(Flags, Position);
    ClassName := Reader.ReadStr;
    ObjectName := Reader.ReadStr;
    WriteIndent;
    if ffInherited in Flags then
      StyleName := scInherited
    else if ffInline in Flags then
      StyleName := scInline
    else
      StyleName := scObject;
    Result := StyleName;
    if ffChildPos in Flags then
      WriteStr(Format(scHeaderBeginFlags,[StyleName, ClassName, ObjectName, Position]))
    else
      WriteStr(Format(scHeaderBegin,[StyleName, ClassName, ObjectName]));
    WriteStr(CrLf);
  end;

  procedure ConvertBinary;
  const
    BytesPerLine = 32;
  var
    MultiLine: Boolean;
    I: Integer;
    Count: Longint;
    Buffer: array[0..BytesPerLine - 1] of Char;
    Text: array[0..BytesPerLine * 2 - 1] of Char;
  begin
    Reader.ReadValue;
    Inc(NestingLevel);
    try
      Reader.Read(Count, SizeOf(Count));
      MultiLine := Count >= BytesPerLine;
      while Count > 0 do
      begin
        if MultiLine then NewLine;
        if Count >= 32 then I := 32 else I := Count;
        Reader.Read(Buffer, I);
        BinToHex(Buffer, Text, I);
        WriteStr(scBinaryItemBegin);
        Writer.Write(Text, I * 2);
        WriteStr(scBinaryItemEnd);
        Dec(Count, I);
      end;
      WriteStr(scBinaryEnd);
    finally
      Dec(NestingLevel);
      NewLine;
    end;
  end;

  procedure ConvertProperty; forward;

  procedure ConvertValue(Vt : TValueType);
  const
    LineLength = 64;
  var
    I: Integer;
    S: string;
    W: WideString;
    aVt : TValueType;
  begin
    case Vt of
      vaList:
        begin
          Reader.ReadValue;
          WriteStr(CrLf);
          Inc(NestingLevel);
          try
            WriteIndent;
            WriteStr(scListBegin);
            Inc(NestingLevel);
            try
              while not Reader.EndOfList do
              begin
                NewLine;
                aVt := Reader.NextValue;
                WriteStr(Format(scListItemBegin,[copy(GetEnumName(TypeInfo(TValueType),Integer(aVt)),3,MaxInt)]));
                ConvertValue(aVt);
                WriteStr(scListItemEnd);
              end;
              Reader.ReadListEnd;
            finally
              Dec(NestingLevel);
              NewLine;
              WriteStrLn(scListEnd);
            end;
          finally
            Dec(NestingLevel);
            WriteIndent;
          end;
        end;
      vaInt8, vaInt16, vaInt32:
        WriteStr(IntToStr(Reader.ReadInteger));
      vaExtended:
        WriteStr(FloatToStr(Reader.ReadFloat));
      vaSingle:
        WriteStr(FloatToStr(Reader.ReadSingle));
      vaCurrency:
        WriteStr(FloatToStr(Reader.ReadCurrency * 10000));
      vaDate:
        WriteStr(FloatToStr(Reader.ReadDate));
      vaWString:
        begin
          W := Reader.ReadWideString;
          WriteStr(WideToStr(W));
        end;
      vaString, vaLString, vaUTF8String:
        WriteStr(StrToXMLStr(Reader.ReadString));
      vaIdent, vaFalse, vaTrue, vaNil, vaNull:
        WriteStr(Reader.ReadIdent);
      vaBinary:
        ConvertBinary;
      vaSet:
        begin
          Reader.ReadValue;
          I := 0;
          while True do
          begin
            S := Reader.ReadStr;
            if S = '' then Break;
            if I > 0 then WriteStr(', ');
            WriteStr(S);
            Inc(I);
          end;
        end;
      vaCollection:
        begin
          Reader.ReadValue;
          Inc(NestingLevel);
          NewLine;
          WriteStr(scCollectionBegin);
          try
            Inc(NestingLevel);
            try
              WriteStr(CrLf);
              while not Reader.EndOfList do
              begin
                WriteIndent;
                if Reader.NextValue in [vaInt8, vaInt16, vaInt32] then
                  WriteStr(format(scCollectionItemBeginValue, [Reader.ReadInteger]))
                else
                  WriteStr(scCollectionItemBegin);
                WriteStr(CrLf);
                Reader.CheckValue(vaList);
                Inc(NestingLevel);
                while not Reader.EndOfList do ConvertProperty;
                Reader.ReadListEnd;
                Dec(NestingLevel);
                WriteIndent;
                WriteStrLn(scCollectionItemEnd);
              end;
              Reader.ReadListEnd;
            finally
              Dec(NestingLevel);
            end;
          finally
            WriteIndent;
            WriteStrLn(ScCollectionEnd);
            Dec(NestingLevel);
            WriteIndent;
          end;
        end;
      vaInt64:
        WriteStr(IntToStr(Reader.ReadInt64));
    end;
  end;

  procedure ConvertProperty;
  var
    PropName: string;
    Vt: TValueType;
  begin
    WriteIndent;
    PropName := Reader.ReadStr;
    Vt       := Reader.NextValue;
    WriteStr(Format(scPropertieBegin,[PropName,copy(GetEnumName(TypeInfo(TValueType),Integer(Vt)),3,MaxInt)]));
    try
      ConvertValue(Vt);
    finally
      WriteStr(Format(scPropertieEnd,[PropName]));
    end;
    WriteStr(CrLf);
  end;

  procedure ConvertObject;
  var
    StyleName: String;
  begin
    StyleName := ConvertHeader;

    Inc(NestingLevel);
    try
      WriteIndent;
      WriteStr(scPropertiesBegin);
      WriteStr(CrLf);
      Inc(NestingLevel);
      try
        // the embedded properties
        while not Reader.EndOfList do ConvertProperty;
        Reader.ReadListEnd;
      finally
        Dec(NestingLevel);
        WriteIndent;
        WriteStr(scPropertiesEnd);
        WriteStr(CrLf);
      end;

      WriteIndent;
      WriteStr(scComponentsBegin);
      WriteStr(CrLf);
      Inc(NestingLevel);
      try
        while not Reader.EndOfList do ConvertObject;
        Reader.ReadListEnd;
      finally
        Dec(NestingLevel);
        WriteIndent;
        WriteStr(scComponentsEnd);
        WriteStr(CrLf);
      end;

    finally
      Dec(NestingLevel);
    end;

    WriteIndent;
    WriteStr(Format(scHeaderEnd,[StyleName]));
    WriteStr(CrLf);
  end;

begin
  NestingLevel := 0;
  Reader := TReader.Create(Input, 4096);
  SaveSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  try
    Writer := TWriter.Create(Output, 4096);
    try
      Reader.ReadSignature;
      ConvertObject;
    finally
      Writer.Free;
    end;
  finally
    DecimalSeparator := SaveSeparator;
    Reader.Free;
  end;
end;

// TXMLWriter helper object because we can't use the TWrite with it's protected
// routines
type
  TXMLWriter = class(TWriter)
  public
    procedure WritePrefix(Flags: TFilerFlags; AChildPos: Integer);

  end;

procedure TXMLWriter.WritePrefix(Flags: TFilerFlags; AChildPos: Integer);
var
  Prefix: Byte;
begin
  if Flags <> [] then
  begin
    Prefix := $F0 or Byte(Flags);
    Write(Prefix, SizeOf(Prefix));
    if ffChildPos in Flags then WriteInteger(AChildPos);
  end;
end;

{ rapware XML to binary conversion }
type
  TBinaryBuffer = class(TObject)
  private
    FBinStream: TMemoryStream;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddDataStr(aStr: String);
    procedure CopyTo(Stream: TStream);
  end;

constructor TBinaryBuffer.Create;
begin
  inherited Create;
  FBinStream := TMemoryStream.Create;
end;

destructor TBinaryBuffer.Destroy;
begin
  FBinStream.Free;
  inherited;
end;

procedure TBinaryBuffer.CopyTo(Stream: TStream);
begin
  fBinStream.Position := 0;
  Stream.CopyFrom(fBinStream,fBinStream.Size);
end;

procedure TBinaryBuffer.AddDataStr(aStr: String);
var
  Count: Integer;
  Buffer: array[0..255] of Char;
begin
  Count := HexToBin(PChar(aStr), Buffer, SizeOf(Buffer));
  if Count = 0 then raise Exception.Create('Invalid Binary values in XML');
  FBinStream.Write(Buffer, Count);
end;

procedure rwObjectXMLToBinary(Input, Output: TStream);
var
  SaveSeparator: Char;
  Parser: TrwXMLParser;
  Writer: TXMLWriter;
  ValueType: TValueType;

  function ElementNameIs(aName: string): boolean;
  begin
    result := AnsiCompareText(aName, Parser.ElementName) = 0;
  end;

  procedure ConvertHeader;
  var
    ClassName, ObjectName, Order: string;
    Flags: TFilerFlags;
    Position: Integer;
    isInherited, isInline: boolean;
  begin
    isInherited := false;
    isInline    := false;
    if ElementNameIs(scObject) then
    begin
    end
    else if ElementNameIs(scInherited) then
      isInherited := true
    else if ElementNameIs(scInline) then
      isInline := true
    else
      raise Exception.Create('Not a vallid object XML stream');
    ClassName  := Parser.ValueOf('Type');
    ObjectName := Parser.ValueOf('Name');
    Order      := Parser.ValueOf('Order');
    Flags := [];
    if Order = '' then
      Position := -1
    else
      Position := StrToInt(Order);

    if IsInherited then
      Include(Flags, ffInherited);
    if IsInline then
      Include(Flags, ffInline);
    if Position >= 0 then
      Include(Flags, ffChildPos);
    Writer.WritePrefix(Flags, Position);
    Writer.WriteStr(ClassName);
    Writer.WriteStr(ObjectName);
  end;

  procedure ConvertProperty; forward;
  procedure ConvertObject; forward;
  procedure ConvertList; forward;
  procedure ConvertCollection; forward;
  procedure ConvertBinary; forward;

  procedure ConvertBinary;
  var
    Bin: TBinaryBuffer;
  begin
    Bin := TBinaryBuffer.Create;
    try
      Parser.ReadNextElement;
      if (not Parser.EndFound) and
         ((AnsiCompareText(scBinaryItem, Parser.ElementName) = 0)) then
      begin
        while (not Parser.EndFound) and
              (AnsiCompareText(scBinaryItem, Parser.ElementName) = 0) do
        begin
          Bin.AddDataStr(Trim(Parser.Data));
          Parser.ReadNextElement; // this must be the end of the ListItem
          if (not Parser.EndFound) or
             (not (AnsiCompareText(scBinaryItem, Parser.ElementName) = 0)) then
            raise Exception.Create('No Binary End found');
          Parser.ReadNextElement; // this must be the next item or the end of the list
        end;
        Writer.WriteBinary(Bin.CopyTo);
      end;
    finally
      Bin.Free;
    end;
  end;

  procedure WriteSet;
  var
    S, aVal: string;

    function NextStr(var aStr, aValue : string): boolean;
    var
      P: integer;
    begin
      aStr := Trim(aStr);
      P := pos(',',aStr);
      if P > 0 then
      begin
        aValue := copy(aStr,1,P-1);
        aStr   := copy(aStr,P+1,MaxInt);
        result := true;
      end
      else
      begin
        aValue := Trim(aStr);
        aStr := '';
        result := aValue <> '';
      end;
    end;

  begin
    S := trim(Parser.Data);
    while NextStr(S,aVal) do
      Writer.WriteStr(aVal);
  end;

  procedure ConvertValue;
  begin
    ValueType := TValueType(GetEnumValue(TypeInfo(TValueType),'va' +
                            Parser.ValueOf(scValueType)));

    case ValueType of
      vaInt64:
        Writer.WriteInteger(Parser.Int64Data);
      vaInt8, vaInt16, vaInt32:
        Writer.WriteInteger(Parser.IntegerData);
      vaList:
        ConvertList;
      vaSingle, vaExtended:
        {$IFDEF D7}
        Writer.WriteFloat(Parser.FloatData);
        {$ELSE}
        Writer.WriteSingle(Parser.FloatData);
        {$ENDIF}
      vaCurrency:
        Writer.WriteCurrency(Parser.FloatData / 10000);
      vaDate:
        Writer.WriteDate(Parser.FloatData);
      vaCollection:
        ConvertCollection;
      vaNull, vaFalse, vaTrue, vaNil:
        Writer.WriteValue(ValueType);
      vaIdent:
        begin
          Writer.WriteValue(vaIdent);
          Writer.WriteStr(trim(Parser.Data));
        end;
      vaSet:
        begin
          Writer.WriteValue(vaSet);
          WriteSet;
          Writer.WriteStr('');
        end;
      vaString, vaLString, vaUTF8String:
        begin
          Writer.WriteString(XMLStrToStr(Parser.Data));
        end;
      vaBinary:
        begin
          ConvertBinary;
        end;
      vaWString:
        Raise Exception.Create('Not yet implemented');
      else
        Raise Exception.Create('Unknown DataType');
    end;
  end;


  procedure ConvertCollection;
  begin
    Parser.ReadNextElement; // this must the begin of the collection
    if (Parser.EndFound) or
       (not (AnsiCompareText(scCollection, Parser.ElementName) = 0)) then
      raise Exception.Create('No Collection found');
    Writer.WriteValue(vaCollection);
    Parser.ReadNextElement;
    if not Parser.EndFound then
    begin
      while (not Parser.EndFound) and
            ((AnsiCompareText(scCollectionItem, Parser.ElementName) = 0)) do
      begin
        Writer.WriteListBegin;
        try
          Parser.ReadNextElement;
          while (not Parser.EndFound) and
                (AnsiCompareText(scPropertie, Parser.ElementName) = 0) do
          begin
            ConvertProperty;
            Parser.ReadNextElement;
          end;
          if (not Parser.EndFound) or
             (not (AnsiCompareText(scCollectionItem, Parser.ElementName) = 0)) then
            raise Exception.Create('No CollectionItem END found');
          Parser.ReadNextElement;
        finally
          Writer.WriteListEnd;
        end;
      end;
    end;
    Writer.WriteListEnd;
    // this must be the end of the collection
    if (not Parser.EndFound) or
       (not (AnsiCompareText(scCollection, Parser.ElementName) = 0)) then
      raise Exception.Create('No end of Collection found');
    Parser.EndFound := false;
  end;


  procedure ConvertList;
  begin
    Parser.ReadNextElement;
    if (Parser.EndFound) or
       (not (AnsiCompareText(scList, Parser.ElementName) = 0)) then
      raise Exception.Create('No List found');

    Writer.WriteListBegin;
    Parser.ReadNextElement; // this must be the end of the propertie

    if (not Parser.EndFound) and
       ((AnsiCompareText(scListItem, Parser.ElementName) = 0)) then
    begin
      while (not Parser.EndFound) and
            (AnsiCompareText(scListItem, Parser.ElementName) = 0) do
      begin
        ConvertValue;
        Parser.ReadNextElement; // this must be the end of the ListItem
        if (not Parser.EndFound) or
           (not (AnsiCompareText(scListItem, Parser.ElementName) = 0)) then
          raise Exception.Create('No ListItem End found');
        Parser.ReadNextElement; // this must be the next item or the end of the list
      end;
    end;

    Writer.WriteListEnd;
    if (not Parser.EndFound) or
       (not (AnsiCompareText(scList, Parser.ElementName) = 0)) then
      raise Exception.Create('No end of List found');
    Parser.EndFound := false;
  end;


  procedure ConvertProperty;
  var
    PropName: string;
  begin
    PropName := Parser.ValueOf('Name');
    if PropName = '' then
      raise Exception.Create('Name of Property Empty');
    Writer.WriteStr(PropName);
    ConvertValue;
    Parser.ReadNextElement; // this must be the end of the propertie
    if (not (Parser.EndFound)) or
       (not (AnsiCompareText(scPropertie, Parser.ElementName) = 0)) then
      raise Exception.Create('No propertie END found');
    Parser.EndFound := false;
  end;

  // Converts all propeties in the Properties block
  procedure ConvertProperties;
  begin
    repeat
      Parser.ReadNextElement;
      if ElementNameIs(scPropertie) then
        ConvertProperty;
    until (Parser.EndFound);
    if not (AnsiCompareText(scProperties, Parser.ElementName) = 0) then
      raise Exception.Create('Invalid XML object stream');
    Parser.EndFound := false;
    Writer.WriteListEnd;
  end;

  procedure ConvertComponents;
  begin
    repeat
      Parser.ReadNextElement;
      if ElementNameIs(scObject) or ElementNameIs(scInline) or
         ElementNameIs(scInherited) then
        ConvertObject;
    until (Parser.EndFound);
    if not (AnsiCompareText(scComponents, Parser.ElementName) = 0) then
      raise Exception.Create('Invalid XML object stream');
    Parser.EndFound := false;
    Writer.WriteListEnd;
  end;

  procedure ConvertObject;
  var
    ItemName: string;
  begin
    ConvertHeader;
    ItemName := Parser.ElementName;
    if not Parser.EndFound then
    begin
      repeat
        Parser.ReadNextElement;
        if ElementNameIs(scProperties) then
          ConvertProperties
        else if ElementNameIs(scComponents) then
        begin
          ConvertComponents;
        end;
      until (Parser.EndFound);
      if not (AnsiCompareText(ItemName, Parser.ElementName) = 0) then
        raise Exception.Create('Invalid XML object stream');
      Parser.EndFound := false;
    end;
  end;

begin
  Parser := TrwXMLParser.Create(Input);
  SaveSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  try
    Writer := TXMLWriter.Create(Output, 4096);
    try
      Writer.WriteSignature;
      Parser.ReadNextElement;
      ConvertObject;
      Writer.WriteListEnd;
    finally
      Writer.Free;
    end;
  finally
    DecimalSeparator := SaveSeparator;
    Parser.Free;
  end;
end;

end.
