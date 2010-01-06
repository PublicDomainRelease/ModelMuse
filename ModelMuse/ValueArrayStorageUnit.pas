unit ValueArrayStorageUnit;

interface

uses Classes, RbwParser;

type
  TValueArrayStorage = class(TPersistent)
  private
    FIntValues: array of integer;
    FRealValues: array of double;
    FBooleanValues: array of boolean;
    FStringValues: TStringList;
    FDataType: TRbwDataType;
    function GetBooleanValues(Index: integer): boolean;
    function GetCount: integer;
    function GetIntValues(Index: integer): integer;
    function GetRealValues(Index: integer): double;
    function GetStringValues(Index: integer): string;
    procedure SetBooleanValues(Index: integer; const Value: boolean);
    procedure SetCount(const Value: integer);
    procedure SetDataType(const Value: TRbwDataType);
    procedure SetIntValues(Index: integer; const Value: integer);
    procedure SetRealValues(Index: integer; const Value: double);
    procedure SetStringValues(Index: integer; Value: string);
    procedure ReadValues(Reader: TReader);
    procedure WriteValues(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property IntValues[Index: integer]: integer read GetIntValues write SetIntValues;
    property RealValues[Index: integer]: double read GetRealValues write SetRealValues;
    property BooleanValues[Index: integer]: boolean read GetBooleanValues write SetBooleanValues;
    property StringValues[Index: integer]: string read GetStringValues write SetStringValues;
    procedure Add; overload;
    procedure Add(Value: double); overload;
    procedure Add(Value: integer); overload;
    procedure Add(Value: boolean); overload;
    procedure Add(const Value: string); overload;
    procedure Delete(Index: integer);
    procedure Insert(Index: integer);
    procedure Clear;
  published
    property DataType: TRbwDataType read FDataType write SetDataType;
    property Count: integer read GetCount write SetCount;
  end;

  TValueArrayItem = class(TCollectionItem)
  private
    FName: string;
    FValues: TValueArrayStorage;
    procedure SetName(const Value: string);
    procedure SetValues(const Value: TValueArrayStorage);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Name: string read FName write SetName;
    property Values: TValueArrayStorage read FValues write SetValues;
  end;

  TValueCollection = class(TCollection)
  private
    FCachedName: string;
    FCachedIndex: integer;
    function GetItem(Index: Integer): TValueArrayItem;
    procedure SetItem(Index: Integer; const Value: TValueArrayItem);
  public
    constructor Create;
    property Items[Index: Integer]: TValueArrayItem read GetItem write SetItem;
    function ValuesByName(AName: string): TValueArrayStorage;
  end;

implementation

uses SysUtils;

{ TValueArrayStorage }

procedure TValueArrayStorage.ReadValues(Reader: TReader);
var
  Index: Integer;
begin
  Reader.ReadListBegin;
  Count := Reader.ReadInteger;
  for Index := 0 to Count - 1 do
  begin
//    Reader.ReadString;
    case DataType of
      rdtDouble: RealValues[Index] := Reader.ReadFloat;
      rdtInteger: IntValues[Index] := Reader.ReadInteger;
      rdtBoolean: BooleanValues[Index] := Reader.ReadBoolean;
      rdtString: StringValues[Index] := Reader.ReadString;
      else Assert(False);;
    end;
  end;
  Reader.ReadListEnd;
end;

procedure TValueArrayStorage.WriteValues(Writer: TWriter);
var
  Index: Integer;
begin
  Writer.WriteListBegin;
  Writer.WriteInteger(Count);
  for Index := 0 to Count - 1 do
  begin
//    Writer.WriteString('Value=');
    case DataType of
      rdtDouble: Writer.WriteFloat(RealValues[Index]);
      rdtInteger: Writer.WriteInteger(IntValues[Index]);
      rdtBoolean: Writer.WriteBoolean(BooleanValues[Index]);
      rdtString: Writer.WriteString(StringValues[Index]);
      else Assert(False);;
    end;
  end;
  Writer.WriteListEnd;
end;

procedure TValueArrayStorage.Add;
begin
  Count := Count + 1;
end;

procedure TValueArrayStorage.Add(Value: integer);
begin
  Assert(DataType = rdtInteger);
  Add;
  IntValues[Count-1] := Value;
end;

procedure TValueArrayStorage.Add(Value: double);
begin
  Assert(DataType = rdtDouble);
  Add;
  RealValues[Count-1] := Value;
end;

procedure TValueArrayStorage.Add(const Value: string);
begin
  Assert(DataType = rdtString);
  Add;
  StringValues[Count-1] := Value;
end;

procedure TValueArrayStorage.Add(Value: boolean);
begin
  Assert(DataType = rdtBoolean);
  Add;
  BooleanValues[Count-1] := Value;
end;

procedure TValueArrayStorage.Assign(Source: TPersistent);
var
  SourceStorage: TValueArrayStorage;
begin
  if Source = self then Exit;

  if Source is TValueArrayStorage then
  begin
    SourceStorage := TValueArrayStorage(Source);
    Count := 0;
    DataType := SourceStorage.DataType;
    case DataType of
      rdtDouble: FRealValues := SourceStorage.FRealValues;
      rdtInteger: FIntValues := SourceStorage.FIntValues;
      rdtBoolean: FBooleanValues := SourceStorage.FBooleanValues;
      rdtString: FStringValues.Assign(SourceStorage.FStringValues);
      else Assert(False);
    end;
    Count := SourceStorage.Count;
  end
  else
  begin
    inherited
  end;
end;

procedure TValueArrayStorage.Clear;
begin
  Count := 0;
end;

constructor TValueArrayStorage.Create;
begin
  FStringValues := TStringList.Create
end;

procedure TValueArrayStorage.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Values', ReadValues,
    WriteValues, True);
end;

procedure TValueArrayStorage.Delete(Index: integer);
var
  LocalCount: integer;
begin
  Assert(Index >= 0);
  LocalCount := Count;
  Assert(Index < LocalCount);
  if Index < LocalCount -1 then
  begin
    case DataType of
      rdtDouble:
        begin
          Move(FRealValues[Index+1], FRealValues[Index],
            (LocalCount-Index-1)*SizeOf(Double));
        end;
      rdtInteger:
        begin
          Move(FIntValues[Index+1], FIntValues[Index],
            (LocalCount-Index-1)*SizeOf(integer));
        end;
      rdtBoolean:
        begin
          Move(FBooleanValues[Index+1], FBooleanValues[Index],
            (LocalCount-Index-1)*SizeOf(boolean));
        end;
      rdtString:
        begin
          FStringValues.Delete(Index);
        end;
      else Assert(False);
    end;
  end;
  Count := Count -1;
end;

destructor TValueArrayStorage.Destroy;
begin
  FStringValues.Free;
  inherited;
end;

function TValueArrayStorage.GetBooleanValues(Index: integer): boolean;
begin
  result := FBooleanValues[Index];
end;

function TValueArrayStorage.GetCount: integer;
begin
  result := 0;
  case DataType of
    rdtDouble: result := length(FRealValues);
    rdtInteger: result := length(FIntValues);
    rdtBoolean: result := length(FBooleanValues);
    rdtString: result := FStringValues.Count;
    else Assert(False);
  end;
end;

function TValueArrayStorage.GetIntValues(Index: integer): integer;
begin
  result := FIntValues[Index];
end;

function TValueArrayStorage.GetRealValues(Index: integer): double;
begin
  result := FRealValues[Index];
end;

function TValueArrayStorage.GetStringValues(Index: integer): string;
begin
  result := FStringValues[Index];
end;

procedure TValueArrayStorage.Insert(Index: integer);
var
  LocalCount: integer;
begin
  Assert(Index >= 0);
  LocalCount := Count;
  Assert(Index <= LocalCount);
  Count := Count +1;
  if Index <= LocalCount -1 then
  begin
    case DataType of
      rdtDouble:
        begin
          Move(FRealValues[Index], FRealValues[Index+1],
            (LocalCount-Index)*SizeOf(Double));
        end;
      rdtInteger:
        begin
          Move(FIntValues[Index], FIntValues[Index+1],
            (LocalCount-Index)*SizeOf(integer));
        end;
      rdtBoolean:
        begin
          Move(FBooleanValues[Index], FBooleanValues[Index+1],
            (LocalCount-Index)*SizeOf(boolean));
        end;
      rdtString:
        begin
          FStringValues.Insert(Index, '');
        end;
      else Assert(False);
    end;
  end;
end;

procedure TValueArrayStorage.SetBooleanValues(Index: integer;
  const Value: boolean);
begin
  FBooleanValues[Index] := Value;
end;

procedure TValueArrayStorage.SetCount(const Value: integer);
begin
  Assert(Value >= 0);
  case DataType of
    rdtDouble: SetLength(FRealValues, Value);
    rdtInteger: SetLength(FIntValues, Value);
    rdtBoolean: SetLength(FBooleanValues, Value);
    rdtString:
      begin
        While (FStringValues.Count > Value) do
        begin
          FStringValues.Delete(FStringValues.Count-1);
        end;
        While (FStringValues.Count < Value) do
        begin
          FStringValues.Add('');
        end;
      end
    else Assert(False);
  end;
end;

procedure TValueArrayStorage.SetDataType(const Value: TRbwDataType);
var
  TempCount: integer;
begin
  if FDataType <> Value then
  begin
    TempCount := Count;
    Count := 0;
    FDataType := Value;
    Count := TempCount;
  end;
end;

procedure TValueArrayStorage.SetIntValues(Index: integer; const Value: integer);
begin
  FIntValues[Index] := Value;
end;

procedure TValueArrayStorage.SetRealValues(Index: integer;
  const Value: double);
begin
  FRealValues[Index] := Value;
end;

procedure TValueArrayStorage.SetStringValues(Index: integer;
  Value: string);
begin
  if Length(Value) > 0 then
  begin
    if Value[1] = '"' then
    begin
      Value := Copy(Value, 2, MAXINT);
    end;
  end;
  if Length(Value) > 0 then
  begin
    if Value[Length(Value)] = '"' then
    begin
      Value := Copy(Value, 1, Length(Value)-1);
    end;
  end;
  FStringValues[Index] := Value;
end;

{ TValueArrayItem }

procedure TValueArrayItem.Assign(Source: TPersistent);
var
  SourceValues: TValueArrayItem;
begin
  if Source is TValueArrayItem then
  begin
    SourceValues := TValueArrayItem(Source);
    Name := SourceValues.Name;
    Values := SourceValues.Values;
  end
  else
  begin
    inherited;
  end;
end;

constructor TValueArrayItem.Create(Collection: TCollection);
begin
  inherited;
  FValues := TValueArrayStorage.Create;
end;

destructor TValueArrayItem.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TValueArrayItem.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TValueArrayItem.SetValues(const Value: TValueArrayStorage);
begin
  FValues.Assign(Value);
end;

{ TValueCollection }

constructor TValueCollection.Create;
begin
  inherited Create(TValueArrayItem);
  FCachedIndex := -1;
end;

function TValueCollection.GetItem(Index: Integer): TValueArrayItem;
begin
  result := inherited Items[Index] as TValueArrayItem;
end;

procedure TValueCollection.SetItem(Index: Integer;
  const Value: TValueArrayItem);
begin
  inherited Items[Index] := Value;
end;

function TValueCollection.ValuesByName(AName: string): TValueArrayStorage;
var
  Index: Integer;
begin
  result := nil;

  if (FCachedName <> '') and SameText(FCachedName, AName) then
  begin
    result := Items[FCachedIndex].Values;
    Exit;
  end;
  
  for Index := 0 to Count - 1 do
  begin
    if SameText(Items[Index].Name, AName) then
    begin
      result := Items[Index].Values;
      FCachedName := AName;
      FCachedIndex := Index;
      break;
    end;
  end;
end;

initialization
  RegisterClass(TValueArrayStorage);

end.
