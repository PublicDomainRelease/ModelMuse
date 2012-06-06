unit HashTableFacadeUnit;

interface

uses {EZDSLHsh,} JclHashMaps;

type

  THashTableFacade = class(TObject)
  private
//    FHashTable: THashTable;
    FJclUnicodeStrHashMap: TJclUnicodeStrHashMap;
    FTableSize: integer;
    function GetIgnoreCase: boolean;
    function GetTableSize: integer;
    procedure SetIgnoreCase(const Value: boolean);
    procedure SetTableSize(Value: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Delete(const aKey : string);
    procedure Insert(const aKey : string; aData : pointer);
    function Search(const aKey : string; var aData : pointer) : boolean;
    property TableSize : integer read GetTableSize write SetTableSize;
    property IgnoreCase : boolean read GetIgnoreCase write SetIgnoreCase;
  end;

implementation

uses
  JclContainerIntf;

const
  MinTableSize = 11;   {arbitrary smallest table size}
  StartTableSize = 53; {arbitrary beginning table size}

type
  TPointerStorage = class(TObject)
    Data: Pointer;
  end;

function GetClosestPrime(N : longint) : longint;
{$I EZPrimes.inc}
const
  Forever = true;
var
  L, R, M : integer;
  RootN   : longint;
  IsPrime : boolean;
  DivisorIndex : integer;
begin
  {treat 2 as a special case}
  if (N = 2) then begin
    Result := N;
    Exit;
  end;
  {make the result equal to N, and if it's even, the next odd number}
  if Odd(N) then
    Result := N
  else
    Result := succ(N);
  {if the result is within our prime number table, use binary search
   to find the equal or next highest prime number}
  if (Result <= MaxPrime) then begin
    L := 0;
    R := pred(PrimeCount);
    while (L <= R) do begin
      M := (L + R) div 2;
      if (Result = Primes[M]) then
        Exit
      else if (Result < Primes[M]) then
        R := pred(M)
      else
        L := succ(M);
    end;
    Result := Primes[L];
    Exit;
  end;
  {the result is outside our prime number table range, use the
   standard method for testing primality (do any of the primes up to
   the root of the number divide it exactly?) and continue
   incrementing the result by 2 until it is prime}
  if (Result <= (MaxPrime * MaxPrime)) then begin
    while Forever do begin
      RootN := round(Sqrt(Result));
      DivisorIndex := 1; {ignore the prime number 2}
      IsPrime := true;
      while (DivisorIndex < PrimeCount) and (RootN > Primes[DivisorIndex]) do begin
        if ((Result div Primes[DivisorIndex]) * Primes[DivisorIndex] = Result) then begin
          IsPrime := false;
          Break;
        end;
        inc(DivisorIndex);
      end;
      if IsPrime then
        Exit;
      inc(Result, 2);
    end;
  end;
end;

{ THashTableFacade }

constructor THashTableFacade.Create;
begin
//  FHashTable := THashTable.Create(False);
  FJclUnicodeStrHashMap := TJclUnicodeStrHashMap.Create(StartTableSize, True);
  FTableSize := StartTableSize;
  IgnoreCase := False;
end;

procedure THashTableFacade.Delete(const aKey: string);
begin
//  FHashTable.Delete(aKey);
  FJclUnicodeStrHashMap.Extract(aKey).Free;
end;

destructor THashTableFacade.Destroy;
begin
  FJclUnicodeStrHashMap.Free;
//  FHashTable.Free;
  inherited;
end;

function THashTableFacade.GetIgnoreCase: boolean;
begin
//  result := FHashTable.IgnoreCase;
  Result := not FJclUnicodeStrHashMap.CaseSensitive;
end;

function THashTableFacade.GetTableSize: integer;
begin
//  result := FHashTable.TableSize;
  result := FTableSize;
end;

procedure THashTableFacade.Insert(const aKey: string; aData: pointer);
var
  ValueObject: TPointerStorage;
  OldHashMap: TJclUnicodeStrHashMap;
  It: IJclUnicodeStrIterator;
  Key: UnicodeString;
  OldSize: Integer;
begin
//  FHashTable.Insert(aKey, aData);
  ValueObject := TPointerStorage.Create;
  ValueObject.Data := aData;
  FJclUnicodeStrHashMap.PutValue(aKey, ValueObject);
  if FJclUnicodeStrHashMap.Size > TableSize * 2 div 3 then
  begin
    OldHashMap := FJclUnicodeStrHashMap;
    FJclUnicodeStrHashMap := TJclUnicodeStrHashMap.Create(GetClosestPrime(TableSize * 2), True);
    FJclUnicodeStrHashMap.CaseSensitive := OldHashMap.CaseSensitive;
    OldSize := OldHashMap.Size;
    It := OldHashMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      FJclUnicodeStrHashMap.PutValue(Key, OldHashMap.Extract(Key));
    end;
    Assert(FJclUnicodeStrHashMap.Size = OldSize);
    OldHashMap.Destroy;
  end;
end;

procedure THashTableFacade.SetIgnoreCase(const Value: boolean);
begin
//  FHashTable.IgnoreCase := Value;
  FJclUnicodeStrHashMap.CaseSensitive := not Value;
end;

function THashTableFacade.Search(const aKey: string;
  var aData: pointer): boolean;
var
  ValueObject: TPointerStorage;
begin
//  result := FHashTable.Search(aKey, aData);
  ValueObject := FJclUnicodeStrHashMap.GetValue(aKey) as TPointerStorage;
  result := ValueObject <> nil;
  if result then
  begin
    aData := ValueObject.Data;
  end
  else
  begin
    aData := nil;
  end;
end;

procedure THashTableFacade.SetTableSize(Value: integer);
begin
//  FHashTable.TableSize := Value;

 {force the hash table to be a prime at least MinTableSize, and if
   there's nothing to do, do it}
  Value := GetClosestPrime(Value);
  if (Value < MinTableSize) then
    Value := MinTableSize;
  if FTableSize <> Value then
  begin
    FJclUnicodeStrHashMap.SetCapacity(Value);
    FTableSize := Value;
  end;

end;

end.
