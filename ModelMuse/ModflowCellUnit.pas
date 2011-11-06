unit ModflowCellUnit;

interface

uses Windows, SysUtils, Classes, Contnrs, ZLib, GoPhastTypes;

type
  // @name defines a cell location.
  //  @longcode(
  //  TCellLocation = record
  //    Layer: integer;
  //    Row: integer;
  //    Column: integer;
  //  end;
  //  )
  // @member(Layer Layer is the layer in the grid to for this boundary.)
  // @member(Row Row is the row in the grid to for this boundary.)
  // @member(Column Column is the column in the grid to for this boundary.)
  TCellLocation = record
    Layer: integer;
    Row: integer;
    Column: integer;
    Section: integer;
    class operator Equal(ACell: TCellLocation; BCell: TCellLocation): boolean;
  end;

  TValueCell = class(TObject)
  private
    FIFace: TIface;
    FScreenObject: TObject;
    procedure SetScreenObject(const Value: TObject);
  protected
    function GetColumn: integer; virtual; abstract;
    function GetLayer: integer; virtual; abstract;
    function GetRow: integer; virtual; abstract;
    procedure SetColumn(const Value: integer); virtual; abstract;
    procedure SetLayer(const Value: integer); virtual; abstract;
    procedure SetRow(const Value: integer); virtual; abstract;
    function GetIntegerValue(Index: integer; AModel: TBaseModel): integer; virtual; abstract;
    function GetRealValue(Index: integer; AModel: TBaseModel): double; virtual; abstract;
    function GetRealAnnotation(Index: integer; AModel: TBaseModel): string; virtual; abstract;
    function GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string; virtual; abstract;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); virtual;
    procedure Restore(Decomp: TDecompressionStream;
      Annotations: TStringList); virtual;
    function GetSection: integer; virtual; abstract;
    procedure RecordStrings(Strings: TStringList); virtual; abstract;
  public
    Constructor Create; virtual; 
    // @name is the layer number for this cell. Valid values range from 0 to
    // the number of layers in the grid minus 1.
    property Layer: integer read GetLayer write SetLayer;
    // @name is the row number for this cell. Valid values range from 0 to
    // the number of rows in the grid minus 1.
    property Row: integer read GetRow write SetRow;
    // @name is the column number for this cell. Valid values range from 0 to
    // the number of columns in the grid minus 1.
    property Column: integer read GetColumn write SetColumn;
    property IntegerValue[Index: integer; AModel: TBaseModel]: integer read GetIntegerValue;
    property RealValue[Index: integer; AModel: TBaseModel]: double read GetRealValue;
    property RealAnnotation[Index: integer; AModel: TBaseModel]: string read GetRealAnnotation;
    property IntegerAnnotation[Index: integer; AModel: TBaseModel]: string read GetIntegerAnnotation;
    property IFace: TIface read FIFace write FIFace;
    // @name is the @link(TScreenObject) used to assign this
    // @classname.  @name is assigned in @link(TModflowBoundary.AssignCells).
    // @name is only assigned for boundary conditions that have
    // observations associated with them. (Drains, General head boundaries,
    // Rivers, and constant head cells.).
    property ScreenObject: TObject read FScreenObject write SetScreenObject;
    property Section: integer read GetSection;
    function IsIdentical(AnotherCell: TValueCell): boolean; virtual;
    function AreRealValuesIdentical(AnotherCell: TValueCell; DataIndex: integer): boolean;
    function AreIntegerValuesIdentical(AnotherCell: TValueCell; DataIndex: integer): boolean;
  end;

  TValueCellType = class of TValueCell;

  TValueCellList = class(TObjectList)
  private
    FCachedCount: integer;
    FCached: Boolean;
    FCleared: Boolean;
    FValueCellType: TValueCellType;
    FTempFileName: string;
    function GetCount: integer;
    procedure SetCount(const Value: integer);
    function GetItem(Index: integer): TValueCell;
    procedure SetItem(Index: integer; const Value: TValueCell);
    procedure Restore(Start: integer = 0);
  public
    function Add(Item: TValueCell): integer;
    procedure Cache;
    procedure CheckRestore;
    Constructor Create(ValueCellType: TValueCellType);
    property Count: integer read GetCount write SetCount;
    property Items[Index: integer]: TValueCell read GetItem write SetItem; default;
    Destructor Destroy; override;
    function AreRealValuesIdentical(AnotherList: TValueCellList; DataIndex: integer): boolean;
    function AreIntegerValuesIdentical(AnotherList: TValueCellList; DataIndex: integer): boolean;
  end;

procedure WriteCompInt(Stream: TStream; Value: integer);
procedure WriteCompReal(Stream: TStream; Value: double);
procedure WriteCompBoolean(Stream: TStream; Value: Boolean);
procedure WriteCompString(Stream: TStream; Value: string);
procedure WriteCompCell(Stream: TStream; Cell: TCellLocation);

function ReadCompInt(Stream: TStream): integer;
function ReadCompReal(Stream: TStream): double;
function ReadCompBoolean(Stream: TStream): Boolean;
function ReadCompStringSimple(Stream: TStream): string;
function ReadCompString(Stream: TStream; Annotations: TStringList): string;
function ReadCompCell(Stream: TStream): TCellLocation;

implementation

uses TempFiles, ScreenObjectUnit, frmGoPhastUnit;

const
  MaxCondensed = 100;

procedure WriteCompInt(Stream: TStream; Value: integer);
begin
  Stream.Write(Value, SizeOf(Value));
end;

function ReadCompInt(Stream: TStream): integer;
begin
  Stream.Read(result, SizeOf(result));
end;

procedure WriteCompReal(Stream: TStream; Value: double);
begin
  Stream.Write(Value, SizeOf(Value));
end;

function ReadCompReal(Stream: TStream): double;
begin
  Stream.Read(result, SizeOf(result));
end;

procedure WriteCompBoolean(Stream: TStream; Value: Boolean);
begin
  Stream.Write(Value, SizeOf(Value));
end;

function ReadCompBoolean(Stream: TStream): Boolean;
begin
  Stream.Read(result, SizeOf(result));
end;

procedure WriteCompString(Stream: TStream; Value: string);
var
  StringLength: integer;
begin
  StringLength := Length(Value);
  WriteCompInt(Stream, StringLength);
  Stream.WriteBuffer(Pointer(Value)^, StringLength * SizeOf(Char));
end;

function ReadCompStringSimple(Stream: TStream): string;
var
  CommentLength: Integer;
begin
  Stream.Read(CommentLength, SizeOf(CommentLength));
  SetString(result, nil, CommentLength);
  Stream.Read(Pointer(result)^, CommentLength * SizeOf(Char));
end;

function ReadCompString(Stream: TStream; Annotations: TStringList): string;
var
  StringPostion: integer;
begin
  result := ReadCompStringSimple(Stream);
  StringPostion := Annotations.IndexOf(result);
  if StringPostion < 0 then
  begin
    Annotations.Add(result);
  end
  else
  begin
    result := Annotations[StringPostion]
  end;
end;

procedure WriteCompCell(Stream: TStream; Cell: TCellLocation);
begin
  Stream.Write(Cell, SizeOf(Cell));
end;

function ReadCompCell(Stream: TStream): TCellLocation;
begin
  Stream.Read(result, SizeOf(result));
end;


{ TValueCellList }

function TValueCellList.Add(Item: TValueCell): integer;
begin
  if FCached and fCleared then
  begin
    Restore;
  end;
  FCached := False;
  result := inherited Add(Item);
end;

function TValueCellList.AreIntegerValuesIdentical(AnotherList: TValueCellList;
  DataIndex: integer): boolean;
var
  Index: Integer;
begin
  result := Count = AnotherList.Count;
  if result then
  begin
    for Index := 0 to Count - 1 do
    begin
      result := Items[Index].AreIntegerValuesIdentical(
        AnotherList.Items[Index], DataIndex);
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

function TValueCellList.AreRealValuesIdentical(AnotherList: TValueCellList;
  DataIndex: integer): boolean;
var
  Index: Integer;
begin
  result := Count = AnotherList.Count;
  if result then
  begin
    for Index := 0 to Count - 1 do
    begin
      result := Items[Index].AreRealValuesIdentical(
        AnotherList.Items[Index], DataIndex);
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TValueCellList.Cache;
var
//  TempFile: TTempFileStream;
  Compressor: TCompressionStream;
  Index: Integer;
  Cell: TValueCell;
  LocalCount: integer;
//  NewTempFileName: string;
  Strings: TStringList;
  StringIndex: Integer;
  TempFile: TMemoryStream;
begin
  { TODO : Investigate why this this if statement doesn't work. }
  if not FCached then
  begin
    LocalCount := inherited Count;
    if LocalCount > 0 then
    begin
      if FTempFileName = '' then
      begin
        FTempFileName := TempFileName;
      end;
      TempFile := TMemoryStream.Create;
      try
        Compressor := TCompressionStream.Create(clDefault, TempFile);
        try
          FCachedCount := {FCachedCount +} LocalCount;

          Strings := TStringList.Create;
          try
            Strings.Sorted := True;
            Strings.Duplicates := dupIgnore;
            for Index := 0 to LocalCount - 1 do
            begin
              Cell := inherited Items[Index] as TValueCell;
              Cell.RecordStrings(Strings);
            end;

            WriteCompInt(Compressor, Strings.Count);
            for StringIndex := 0 to Strings.Count - 1 do
            begin
              WriteCompString(Compressor, Strings[StringIndex])
            end;

            WriteCompInt(Compressor, LocalCount);
            for Index := 0 to LocalCount - 1 do
            begin
              Cell := inherited Items[Index] as TValueCell;
              Cell.Cache(Compressor, Strings);
            end;
          finally
            Strings.Free;
          end;
        finally
          Compressor.Free;
        end;
        ZipAFile(FTempFileName, TempFile);
      finally
        TempFile.Free;
      end;
      FCached := True;
    end;
  end;
  Clear;
  FCleared := True;
end;

procedure TValueCellList.CheckRestore;
begin
  if FCached and FCleared then
  begin
    Restore;
  end;
end;

constructor TValueCellList.Create(ValueCellType: TValueCellType);
begin
  inherited Create;
  FCached := False;
  FTempFileName := '';
  FValueCellType := ValueCellType;
end;

destructor TValueCellList.Destroy;
begin
  if FTempFileName <> '' then
  begin
    FreeMemory(FTempFileName);
  end;
  inherited;
end;

function TValueCellList.GetCount: integer;
begin
  if FCached and FCleared then
  begin
    result := FCachedCount;
  end
  else
  begin
    result := inherited Count;
  end;
end;

function TValueCellList.GetItem(Index: integer): TValueCell;
begin
  CheckRestore;
  result := inherited Items[Index] as TValueCell;
end;

procedure TValueCellList.Restore(Start: integer = 0);
var
//  TempFile: TTempFileStream;
  DecompressionStream: TDecompressionStream;
  ValueCell: TValueCell;
//  Index: Integer;
  LocalCount : integer;
//  CacheFileName: string;
  CellIndex: Integer;
//  SumOfLocalCounts: integer;
  Annotations: TStringList;
  StringIndex: Integer;
  StringCount: Integer;
  TempFile: TMemoryStream;
begin
  Assert(FCached);
  Assert(FCleared);

  Annotations := TStringList.Create;
  try
    if Capacity < FCachedCount then
    begin
      Capacity := FCachedCount;
    end;

    TempFile := TMemoryStream.Create;
    ExtractAFile(FTempFileName, TempFile);
    DecompressionStream := TDecompressionStream.Create(TempFile);
    try
      StringCount := ReadCompInt(DecompressionStream);
      Annotations.Clear;
      Annotations.Capacity := StringCount;
      for StringIndex := 0 to StringCount - 1 do
      begin
        Annotations.Add(ReadCompStringSimple(DecompressionStream));
      end;

      LocalCount := ReadCompInt(DecompressionStream);
      for CellIndex := 0 to LocalCount - 1 do
      begin
        ValueCell := FValueCellType.Create;
        inherited Add(ValueCell);
        ValueCell.Restore(DecompressionStream, Annotations);
      end;
    finally
      DecompressionStream.Free;
      TempFile.Free;
    end;

    FCleared := False;
  finally
    Annotations.Free;
  end;
end;

procedure TValueCellList.SetCount(const Value: integer);
begin
  if FCached and FCleared then
  begin
    Restore;
  end;
  inherited Count := Value;
end;

procedure TValueCellList.SetItem(Index: integer; const Value: TValueCell);
begin
  inherited Items[Index] := Value;
end;

{ TValueCell }

function TValueCell.AreIntegerValuesIdentical(AnotherCell: TValueCell;
  DataIndex: integer): boolean;
begin
  result := IntegerValue[DataIndex, nil] = AnotherCell.IntegerValue[DataIndex, nil];
end;

function TValueCell.AreRealValuesIdentical(AnotherCell: TValueCell;
  DataIndex: integer): boolean;
begin
  result := RealValue[DataIndex, nil] = AnotherCell.RealValue[DataIndex, nil];
end;

procedure TValueCell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  Comp.Write(FIface, SizeOf(FIface));
  if ScreenObject = nil then
  begin
    WriteCompString(Comp, '');
  end
  else
  begin
    WriteCompString(Comp, TScreenObject(ScreenObject).Name);
  end;
end;

constructor TValueCell.Create;
begin

end;

function TValueCell.IsIdentical(AnotherCell: TValueCell): boolean;
begin
  result := False;
end;

procedure TValueCell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
var
  ScreenObjectName: string;
begin
  Decomp.Read(FIFace, SizeOf(FIFace));
  ScreenObjectName := ReadCompStringSimple(Decomp);
  if ScreenObjectName = '' then
  begin
    ScreenObject := nil;
  end
  else
  begin
    ScreenObject := frmGoPhast.PhastModel.
      GetScreenObjectByName(ScreenObjectName);
    Assert(ScreenObject <> nil);
  end;
end;

procedure TValueCell.SetScreenObject(const Value: TObject);
begin
  if Value <> nil then
  begin
    Assert(Value is TScreenObject)
  end;
  FScreenObject := Value;
end;

{ TCellLocation }

class operator TCellLocation.Equal(ACell: TCellLocation;
  BCell: TCellLocation): boolean;
begin
  result :=
    (ACell.Layer = BCell.Layer)
    and (ACell.Row = BCell.Row)
    and (ACell.Column = BCell.Column)
end;

end.
