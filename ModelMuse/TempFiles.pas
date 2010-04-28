{@name is used to generate the names of new temporary files and
to delete those files when the program closes.  During initialization
of @name, an application-specific temporary directory will be created
if it does not already exist.  If any files are in the directory and
another instance of the program is not already running, the temporary
directory will be cleared.}
unit TempFiles;

interface

uses Windows, SysUtils, Classes;

{@name generates a name for a new temporary file in an application-specific
temporary directory.  When the program
closes, any file whose name matches a name generated by @name will
be deleted if it has not already been deleted.  }
function TempFileName: string;

// @name returns the name of a temporary directory where
// temporary files for an Application can be created.
// If the directory does not exist, @name will create it.
function GetAppSpecificTempDir: string;

type
  //  Mode must be fmOpenRead, fmOpenWrite, or fmOpenReadWrite
  TTempFileStream = class(TFileStream)
    constructor Create(const AFileName: string; Mode: Word);
  end;

procedure ZipAFile(const FileName: string; InStream: TMemoryStream);
procedure ExtractAFile(const FileName: string; OutStream: TMemoryStream);

implementation

uses RTLConsts, Contnrs, Forms;

var MaxItems: integer = 1000;

type
  TInt64Array = array of Int64;

  TBooleanItem = class(TObject)
    Value: boolean;
  end;

  TBooleanList = class(TObject)
  strict private
    FList: TList;
  private
    function GetCount: integer;
    function GetValue(Index: integer): boolean;
    procedure SetValue(Index: integer; const Value: boolean);
    procedure SetCapacity(const Value: integer);
    function GetCapacity: integer;
  public
    constructor Create;
    Destructor Destroy; override;
    property Values[Index: integer]: boolean read GetValue write SetValue; default;
    procedure Insert(Position: integer; AValue: boolean);
    property Count: integer read GetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
  end;

  TTempItems = class(TObject)
  strict private
    FArchiveName: string;
    FileListName: string;
    FFileList: TStringList;
    FStreamList: TList;
    FIsDirty: TBooleanList;
    FCanStore: boolean;
    FPositions: array of Int64;
    function GetIsDirty(Index: integer): boolean;
    procedure SetIsDirty(Index: integer; const Value: boolean);
    procedure RestoreStreams;
//    procedure ReadHeader(var Positions: TInt64Array; TempFileNames: TStringList; FileStream: TFileStream);
    procedure RestoreAStream(const FileName: string; OutStream: TMemoryStream);
    property IsDirty[Index: integer]: boolean read GetIsDirty write SetIsDirty;
  public
    procedure StoreStreams;
    Constructor Create(Const ArchiveName: string);
    Destructor Destroy; override;
    function StreamFromFileName(const FileName: string;
      RestoreIfCached: boolean): TMemoryStream;
    Procedure SetDirtyFile(const FileName: string);
    property CanStore: boolean read FCanStore write FCanStore;
  end;

var
  TemporaryFiles: TStringList;
  CurrentTempDir: string = '';
  DirCount: integer = 0;
  ErrorCount: integer = 0;
  Directories: TStringList;
  ZipFiles: TStringList;
  FilesToSave: TStringList;
  TempItemList: TList;
  CurrentTempItems: TTempItems = nil;
  PriorTempItems: TTempItems = nil;
  LastCreatedTempItems: TTempItems = nil;

function CreateZipFile(const DirName: string): string;
var
  Count: Integer;
  ADirectory: string;
  Position: Integer;
  TempItems: TTempItems;
begin
  result := IncludeTrailingPathDelimiter(DirName) + 'MM.tmp';
  if FileExists(result) then
  begin
    Count := 0;
    repeat
      Inc(Count);
      result := IncludeTrailingPathDelimiter(DirName) + 'MM'
        + IntToStr(Count) + '.tmp';
    until (not FileExists(result));
  end;
  ZipFiles.Add(result);
  TemporaryFiles.Add(result);
  ADirectory := IncludeTrailingPathDelimiter(ExtractFileDir(result));
  Position := Directories.IndexOf(ADirectory);
  TempItems := TTempItems.Create(result);
  TempItemList.Insert(Position, TempItems);
  if LastCreatedTempItems <> nil then
  begin
    LastCreatedTempItems.CanStore := True;
  end;
  LastCreatedTempItems := TempItems;
end;

function GetStandardDirName: string;
var
  ApplicationName: string;
  PathName: array[0..260] of Char;
begin
  if GetTempPath(MAX_PATH, @PathName) = 0 then
  begin
    RaiseLastOSError;
  end;
  result := IncludeTrailingPathDelimiter(Trim(PathName));
  ApplicationName := ExtractFileName(ParamStr(0));
  result := ExcludeTrailingPathDelimiter(result + ChangeFileExt(ApplicationName, ''));
end;

function UpdateCurrentDir: string;
var
  Count: Integer;
  BaseName: string;
begin
  BaseName := GetStandardDirName;
  result := BaseName;
  Count := 0;
  while DirectoryExists(result) do
  begin
    Inc(Count);
    result := BaseName + IntToStr(Count);
  end;
  result := IncludeTrailingPathDelimiter(result);
  CreateDir(result);
  Directories.Add(result);
  CreateZipFile(result);
  CurrentTempDir := result;
end;

// Get the name of an application-specific temporary directory.
// Create the directory if it does not already exist.
function GetAppSpecificTempDir: string;
begin
  if CurrentTempDir <> '' then
  begin
    result := CurrentTempDir;
    Exit;
  end;
  Result := UpdateCurrentDir;
end;


function TempFileName: string;
begin
  if (TemporaryFiles.Count > 0) and ((TemporaryFiles.Count mod MaxItems) = 0) then
  begin
    UpdateCurrentDir;
  end;

  CurrentTempDir := GetAppSpecificTempDir;
  result := CurrentTempDir + 'MM_' + IntToStr(TemporaryFiles.Count);
  TemporaryFiles.Add(result);
end;

procedure RemoveDirectories;
var
  Index: Integer;
begin
  for Index := 0 to Directories.Count - 1 do
  begin
    if DirectoryExists(Directories[Index]) then
    begin
      // If the directory contains files, RemoveDir will fail.
      RemoveDir(Directories[Index]);
    end;
  end;
end;

// delete all files that were generated by TempFileName if they have
// not already been deleted.
procedure DeleteFiles;
var
  Index: integer;
begin
  for Index := 0 to TemporaryFiles.Count - 1 do
  begin
    if FileExists(TemporaryFiles[Index]) then
    begin
      DeleteFile(TemporaryFiles[Index]);
    end;
  end;
  TemporaryFiles.Clear;
end;

var
  ShouldReleaseMutex: boolean = False;
  MutexHandle: THandle;

// Check if the program is already running.  If not, create a mutex
// that subsequent instances can use to check if another version is already
// running.
function AlreadyRunning: boolean;
var
  MutexName: string;
begin
  MutexName := ExtractFileName(ParamStr(0));
  if OpenMutex(MUTEX_ALL_ACCESS, False, PChar(MutexName)) <> 0 then
  begin
    result := True;
  end
  else
  begin
    result := False;
    MutexHandle := CreateMutex(nil, TRUE, PChar(MutexName));
    ShouldReleaseMutex := True;
  end;
end;

// Delete all files in the application-specific temporary directory.
procedure ClearAppSpecificTempDirectory;
var
  TempPath: string;
  F: TSearchRec;
  Files: TStringList;
  Index: Integer;
  FoundFile: boolean;
  FirstDir: string;
  Directories: TStringList;
  PathName: array[0..260] of Char;
  DirIndex: Integer;
begin
  Directories := TStringList.Create;
  try
    FirstDir := GetStandardDirName;

    if GetTempPath(MAX_PATH, @PathName) = 0 then
    begin
      RaiseLastOSError;
    end;
    TempPath := IncludeTrailingPathDelimiter(Trim(PathName));

    FoundFile := FindFirst(FirstDir + '*.*', faDirectory, F) = 0;
    try
      if FoundFile then
      begin
        Directories.Add(IncludeTrailingPathDelimiter(TempPath + F.Name));
        While FindNext(F) = 0 do
        begin
          Directories.Add(IncludeTrailingPathDelimiter(TempPath + F.Name));
        end;
      end;
    finally
      FindClose(F);
    end;


    Files := TStringList.Create;
    try
      for DirIndex := 0 to Directories.Count - 1 do
      begin
        TempPath := Directories[DirIndex];
        FoundFile := FindFirst(TempPath + '*.*', 0, F) = 0;
        try
          if FoundFile then
          begin
            Files.Add(TempPath + F.Name);
            While FindNext(F) = 0 do
            begin
              Files.Add(TempPath + F.Name);
            end;
          end;
        finally
          FindClose(F);
        end;
        for Index := 0 to Files.Count - 1 do
        begin
          if FileExists(Files[Index]) then
          begin
            DeleteFile(Files[Index]);
          end;
        end;
        Files.Clear;
        if DirectoryExists(TempPath) then
        begin
          RemoveDir(TempPath);
        end;
      end;
    finally
      Files.Free;
    end;
  finally
    Directories.Free;
  end;
end;

{ TTempFileStream }

constructor TTempFileStream.Create(const AFileName: string; Mode: Word);
const
  AccessModes: array[0..2] of DWORD =
    (GENERIC_READ, GENERIC_WRITE, GENERIC_READ or GENERIC_WRITE);
  ShareMode: DWORD = FILE_SHARE_DELETE or FILE_SHARE_READ or FILE_SHARE_WRITE;
  Flags : DWORD = FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE;
begin
  if not FileExists(AFileName) then
  begin
    Mode := fmCreate;
  end;
  inherited;
end;

function ZipfileName(const FileName: string): string;
var
  TempDir: string;
  Position: Integer;
begin
  TempDir := IncludeTrailingPathDelimiter(ExtractFileDir(FileName));
  Position := Directories.IndexOf(TempDir);
  result := ZipFiles[Position];
end;

function TempFileItem(const FileName: string): TTempItems;
var
  TempDir: string;
  Position: Integer;
begin
  TempDir := IncludeTrailingPathDelimiter(ExtractFileDir(FileName));
  Position := Directories.IndexOf(TempDir);
  result := TempItemList[Position];
end;

procedure UpdateCurrentTempItems(const FileName: string);
var
  TempItems: TTempItems;
begin
  TempItems := TempFileItem(FileName);
  Assert(TempItems <> nil);
  if (TempItems <> CurrentTempItems) then
  begin
    if TempItems <> PriorTempItems then
    begin
      if PriorTempItems <> nil then
      begin
        PriorTempItems.StoreStreams;
      end;
    end;
    PriorTempItems := CurrentTempItems;
    CurrentTempItems := TempItems;
  end;
end;

procedure ZipAFile(const FileName: string; InStream: TMemoryStream);
var
  StoredStream: TMemoryStream;
begin
  UpdateCurrentTempItems(FileName);

  StoredStream := CurrentTempItems.StreamFromFileName(FileName, False);
  StoredStream.Position := 0;
  InStream.Position := 0;
  InStream.SaveToStream(StoredStream);
  CurrentTempItems.SetDirtyFile(FileName);

//  Zipper := TAbZipper.Create(nil);
//  try
//    Zipper.FileName := ArchiveName;
//    Zipper.BaseDirectory := ExtractFileDir(ArchiveName);
//    Zipper.StoreOptions := Zipper.StoreOptions + [soReplace];
//    Zipper.DeleteFiles(FileName);
//    Zipper.AddFromStream(FileName, InStream);
//    Zipper.Save;
//  finally
//    Zipper.Free;
//  end;

end;

procedure ExtractAFile(const FileName: string; OutStream: TMemoryStream);
var
//  Unzipper: TAbUnZipper;
//  ArchiveName: string;
  StoredStream: TMemoryStream;
begin
  UpdateCurrentTempItems(FileName);

  StoredStream := CurrentTempItems.StreamFromFileName(FileName, True);
  StoredStream.Position := 0;
  OutStream.Position := 0;
  StoredStream.SaveToStream(OutStream);
//  CurrentTempItems.SetDirtyFile(FileName);

//  ArchiveName := ZipfileName(FileName);
//  FileName := ExtractFileName(FileName);
//  Unzipper := TAbUnZipper.Create(nil);
//  try
//    Unzipper.OpenArchive(ArchiveName);
//    Unzipper.BaseDirectory := ExtractFileDir(ArchiveName);
//    Unzipper.ExtractOptions := [];
//    Unzipper.ExtractToStream(FileName, OutStream);
//  finally
//    Unzipper.Free;
//  end;
end;


{ TTempItems }

constructor TTempItems.Create(Const ArchiveName: string);
begin
  FArchiveName := ArchiveName;
  FFileList := TStringList.Create;
  FFileList.Sorted := True;
  FFileList.Capacity := MaxItems;
  FStreamList := TObjectList.Create;
  FStreamList.Capacity := MaxItems;
  FIsDirty:= TBooleanList.Create;
  FIsDirty.Capacity := MaxItems;
end;

destructor TTempItems.Destroy;
begin
  FFileList.Free;
  FStreamList.Free;
  FIsDirty.Free;
  inherited;
end;

function TTempItems.GetIsDirty(Index: integer): boolean;
begin
  result := FIsDirty[Index];
end;

procedure TTempItems.RestoreAStream(const FileName: string; OutStream: TMemoryStream);
var
  FileStream: TFileStream;
//  FileStreamSize: Int64;
//  TempFileNames: TStringList;
//  Positions: TInt64Array;
  StreamIndex: Integer;
//  Count: Integer;
//  StreamIndex: Integer;
  StreamSize: Int64;
  ByteArray: TInt64Array;
//var
//  Unzipper: TAbUnZipper;
begin
  if FileExists(FArchiveName) then
  begin
    FileStream := TFileStream.Create(FArchiveName, fmOpenRead);
    try
//      FileStreamSize := FileStream.Size;
//      TempFileNames := TStringList.Create;
//      try
//        ReadHeader(Positions, TempFileNames, FileStream);
//        Count := Length(FPositions)-1;
//        StreamIndex := TempFileNames.IndexOf(FileName);
//        Assert(StreamIndex >= 0);

        StreamIndex := FFileList.IndexOf(FileName);
        Assert(StreamIndex >= 0);
        Assert(OutStream = FStreamList[StreamIndex]);
        if (OutStream.Size = 0) and not IsDirty[StreamIndex] then
        begin
//          if StreamIndex = Count - 1 then
//          begin
//            StreamSize := FileStreamSize - Positions[StreamIndex];
//          end
//          else
//          begin
            StreamSize := FPositions[StreamIndex + 1] - FPositions[StreamIndex];
//          end;
          SetLength(ByteArray, StreamSize);
          FileStream.Position := FPositions[StreamIndex];
          FileStream.Read(ByteArray[0], Length(ByteArray) * SizeOf(Byte));
          OutStream.Position := 0;
          OutStream.Write(ByteArray[0], Length(ByteArray) * SizeOf(Byte));
        end;

//      finally
//        TempFileNames.Free;
//      end;
    finally
      FileStream.Free;
    end;
  end;

//  Unzipper := TAbUnZipper.Create(nil);
//  try
//    Unzipper.OpenArchive(FArchiveName);
//    Unzipper.BaseDirectory := ExtractFileDir(FArchiveName);
//    Unzipper.ExtractOptions := [];
//    Unzipper.ExtractToStream(FileName, OutStream);
//  finally
//    Unzipper.Free;
//  end;
end;

procedure TTempItems.SetDirtyFile(const FileName: string);
var
  Position: Integer;
begin
  Position := FFileList.IndexOf(FileName);
  Assert(Position >= 0);
  IsDirty[Position] := True;
end;

procedure TTempItems.SetIsDirty(Index: integer; const Value: boolean);
begin
  FIsDirty[Index] := Value;
end;

procedure TTempItems.StoreStreams;
var
  FileIndex: Integer;
//  FileName: string;
  InStream: TMemoryStream;
  NeedToSave: Boolean;
  FileStream: TFileStream;
  Count: Integer;
  Position: Int64;
//  FileNameSize: Integer;
begin
  if not CanStore then
  begin
    Exit;
  end;
  NeedToSave := False;
  for FileIndex := 0 to FIsDirty.Count -1 do
  begin
    if IsDirty[FileIndex] then
    begin
      NeedToSave := True;
      break;
    end;
  end;
  if NeedToSave then
  begin
    if (FileListName <> '') and (FFileList.Count = 0) then
    begin
      FFileList.LoadFromFile(FileListName);
    end;
    try
      RestoreStreams;
      if FileExists(FArchiveName) then
      begin
        DeleteFile(FArchiveName);
      end;
      FileStream := TFileStream.Create(FArchiveName, fmCreate);
      try
        Count := FFileList.Count;
        SetLength(FPositions, Count+1);

//        FileStream.Write(Count, SizeOf(Count));

//        Position := 0;
//        for FileIndex := 0 to FFileList.Count - 1 do
//        begin
//          InStream := FStreamList[FileIndex];
//          FileStream.Write(Position, SizeOf(Position));
//
//          FileName := FFileList[FileIndex];
//          FileNameSize := Length(FileName);
//          FileStream.Write(FileNameSize, SizeOf(FileNameSize));
//          FileStream.WriteBuffer(Pointer(FileName)^,
//            Length(FileName) * SizeOf(Char));
//
//        end;

        Position := 0;  
        for FileIndex := 0 to FFileList.Count - 1 do
        begin
          FPositions[FileIndex] := Position;
          InStream := FStreamList[FileIndex];
          InStream.Position := 0;
          InStream.SaveToStream(FileStream);
          IsDirty[FileIndex] := False;
          Position := Position + InStream.Size;
          InStream.Clear;
        end;
        FPositions[Count] := Position;
      finally
        FileStream.Free;
      end
    finally
      if FileListName = '' then
      begin
        FileListName := TempFileName;
        FFileList.SaveToFile(FileListName);
      end;
      FFileList.Clear;
    end;
  end
  else
  begin
    for FileIndex := 0 to FIsDirty.Count - 1 do
    begin
      InStream := FStreamList[FileIndex];
      InStream.Clear;
    end;
    FFileList.Clear;
  end;
end;

function TTempItems.StreamFromFileName(const FileName: string; RestoreIfCached: boolean): TMemoryStream;
var
  Position: Integer;
begin
  if (FileListName <> '') and (FFileList.Count = 0) then
  begin
    FFileList.LoadFromFile(FileListName);
  end;
  Position := FFileList.IndexOf(FileName);
  if Position < 0 then
  begin
    Position := FFileList.Add(FileName);
    FStreamList.Insert(Position, TMemoryStream.Create);
    FIsDirty.Insert(Position, False);
  end;
  result := FStreamList[Position];
  if (result.Size = 0) and RestoreIfCached and not IsDirty[Position] then
  begin
    RestoreAStream(FileName, result);
  end;
end;

//procedure TTempItems.ReadHeader(var Positions: TInt64Array; TempFileNames: TStringList; FileStream: TFileStream);
//var
//  FileName: string;
//  FileNameSize: Integer;
//  Index: Integer;
//  Count: Integer;
//begin
//  FileStream.Read(Count, SizeOf(Count));
//  TempFileNames.Capacity := Count;
//  SetLength(Positions, Count);
//  for Index := 0 to Count - 1 do
//  begin
//    FileStream.Read(Positions[Index], SizeOf(Int64));
//    FileStream.Read(FileNameSize, SizeOf(FileNameSize));
//    SetString(FileName, nil, FileNameSize);
//    FileStream.Read(Pointer(FileName)^, FileNameSize * SizeOf(Char));
//    TempFileNames.Add(FileName);
//  end;
//  for Index := 0 to Count - 1 do
//  begin
//    Positions[Index] := Positions[Index] + FileStream.Position;
//  end;
//end;

procedure TTempItems.RestoreStreams;
var
//  Positions: TInt64Array;
  Count: Integer;
//  FileStreamSize: Int64;
  FileStream: TFileStream;
//  TempFileNames: TStringList;
  ByteArray: TInt64Array;
  StreamSize: Int64;
  InStream: TMemoryStream;
//  Position: Int64;
  StreamIndex: Integer;
begin
  if FileExists(FArchiveName) then
  begin
    FileStream := TFileStream.Create(FArchiveName, fmOpenRead);
    try
//      FileStreamSize := FileStream.Size;
//      TempFileNames := TStringList.Create;
//      try
//        ReadHeader(Positions, TempFileNames, FileStream);
        Count := Length(FPositions)-1;
        for StreamIndex := 0 to Count - 1 do
        begin
          InStream := FStreamList[StreamIndex];
          if (Instream.Size = 0) and not IsDirty[StreamIndex] then
          begin
            StreamSize := FPositions[StreamIndex + 1] - FPositions[StreamIndex];
            SetLength(ByteArray, StreamSize);
            FileStream.Position := FPositions[StreamIndex];
            FileStream.Read(ByteArray[0], Length(ByteArray) * SizeOf(Byte));
            Instream.Position := 0;
            Instream.Write(ByteArray[0], Length(ByteArray) * SizeOf(Byte));
          end;
        end;
//      finally
//        TempFileNames.Free;
//      end;
    finally
      FileStream.Free;
    end;
  end;
end;

{ TBooleanList }

constructor TBooleanList.Create;
begin
  FList := TObjectList.Create;
end;

destructor TBooleanList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TBooleanList.GetCapacity: integer;
begin
  result := FList.Capacity;
end;

function TBooleanList.GetCount: integer;
begin
  result := FList.Count;
end;

function TBooleanList.GetValue(Index: integer): boolean;
var
  Item: TBooleanItem;
begin
  Item := FList[Index];
  result := Item.Value;
end;

procedure TBooleanList.Insert(Position: integer; AValue: boolean);
var
  Item: TBooleanItem;
begin
  Item := TBooleanItem.Create;
  Item.Value := AValue;
  FList.Insert(Position, Item);
end;

procedure TBooleanList.SetCapacity(const Value: integer);
begin
  FList.Capacity := Value;
end;

procedure TBooleanList.SetValue(Index: integer; const Value: boolean);
var
  Item: TBooleanItem;
begin
  Item := FList[Index];
  Item.Value := Value;
end;

initialization
  if not AlreadyRunning then
  begin
    ClearAppSpecificTempDirectory;
  end;
  TemporaryFiles:= TStringList.Create;
  Directories := TStringList.Create;
  Directories.Sorted := True;
  ZipFiles := TStringList.Create;
  FilesToSave := TStringList.Create;
  TempItemList := TObjectList.Create;
  GetAppSpecificTempDir;

finalization
  TempItemList.Free;
  FilesToSave.Free;
  DeleteFiles;
  RemoveDirectories;
  ZipFiles.Free;
  TemporaryFiles.Free;
  Directories.Free;
  if ShouldReleaseMutex then
  begin
    ReleaseMutex(MutexHandle);
  end;

end.
