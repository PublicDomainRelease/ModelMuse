unit SutraFileWriterUnit;

interface

uses
  CustomModflowWriterUnit, PhastModelUnit, SysUtils,
  Generics.Collections;

type
  TSutraFileType = (sftInp, sftIcs, sftLst, sftRst, sftNod, sftEle,
    sftObs, sftObc, sftBcof, sftBcos, sftBcop, sftBcou, sftSmy);

  TSutraFileRecord = record
    FileName: string;
    UnitNumber: integer;
  end;

  TSutraFileObject = class(TObject)
    FileName: string;
    UnitNumber: integer;
  end;

  TSutraFileList = TObjectList<TSutraFileObject>;

  TSutraFileWriter = class(TCustomFileWriter)
  private
    FNextUnitNumber: Integer;
    FFiles: array[TSutraFileType] of TSutraFileRecord;
    FBoundaries: TSutraFileList;
    FFileRoot: string;
  public
    Constructor Create(AModel: TCustomModel; FileRoot: string); reintroduce;
    destructor Destroy; override;
    procedure AddFile(FileType: TSutraFileType; const FileName: string);
    procedure AddBoundaryFile(const FileName: string);
    procedure WriteFile;
  end;

var
  SutraFileWriter: TSutraFileWriter = nil;

implementation

{ TSutraFileWriter }

procedure TSutraFileWriter.AddBoundaryFile(const FileName: string);
var
  Item: TSutraFileObject;
begin
  Item := TSutraFileObject.Create;
  FBoundaries.Add(Item);
  Item.UnitNumber := FNextUnitNumber;
  Inc(FNextUnitNumber);
  Item.FileName := FileName;
end;

procedure TSutraFileWriter.AddFile(FileType: TSutraFileType;
  const FileName: string);
begin
  Assert(FFiles[FileType].UnitNumber = -1);
  FFiles[FileType].UnitNumber := FNextUnitNumber;
  Inc(FNextUnitNumber);
  FFiles[FileType].FileName := FileName;
end;

constructor TSutraFileWriter.Create(AModel: TCustomModel; FileRoot: string);
var
  FileType: TSutraFileType;
    SutraFileName: string;
begin
  inherited Create(AModel, etExport);
  FBoundaries := TSutraFileList.Create;
  FNextUnitNumber := 20;
  for FileType := Low(TSutraFileType) to High(TSutraFileType) do
  begin
    FFiles[FileType].UnitNumber := -1;
  end;
  FFileRoot := FileRoot;
  SutraFileName := ExtractFileDir(FileRoot);
  SutraFileName := IncludeTrailingPathDelimiter(SutraFileName);
  SutraFileName := SutraFileName + 'SUTRA.FIL';
  Model.AddModelInputFile(SutraFileName);

  OpenFile(SutraFileName);
end;

destructor TSutraFileWriter.Destroy;
begin
  CloseFile;
  FBoundaries.Free;
  inherited;
end;

procedure TSutraFileWriter.WriteFile;
var
  FileType: TSutraFileType;
  FileIndex: Integer;
  Item: TSutraFileObject;
//  SutraFileName: string;
begin
  AddFile(sftLst, ChangeFileExt(FFileRoot, '.lst'));
  AddFile(sftRst, ChangeFileExt(FFileRoot, '.rst'));
  AddFile(sftNod, ChangeFileExt(FFileRoot, '.nod'));
  AddFile(sftEle, ChangeFileExt(FFileRoot, '.ele'));
  AddFile(sftSmy, ChangeFileExt(FFileRoot, '.smy'));

//  SutraFileName := ExtractFileDir(FileRoot);
//  SutraFileName := IncludeTrailingPathDelimiter(SutraFileName);
//  SutraFileName := SutraFileName + 'SUTRA.FIL';
//
//  OpenFile(SutraFileName);
//  try
  for FileType := Low(TSutraFileType) to High(TSutraFileType) do
  begin
    if FFiles[FileType].UnitNumber > 0 then
    begin
      case FileType of
        sftInp: WriteString('''INP''');
        sftIcs: WriteString('''ICS''');
        sftLst: WriteString('''LST''');
        sftRst: WriteString('''RST''');
        sftNod: WriteString('''NOD''');
        sftEle: WriteString('''ELE''');
        sftObs: WriteString('''OBS''');
        sftObc: WriteString('''OBC''');
        sftBcof: WriteString('''BCOF''');
        sftBcos: WriteString('''BCOS''');
        sftBcop: WriteString('''BCOP''');
        sftBcou: WriteString('''BCOU''');
        sftSmy: WriteString('''SMY''');
        else Assert(False);
      end;
      WriteInteger(FFiles[FileType].UnitNumber);
      WriteString(' ''' + ExtractRelativePath(FFileRoot, FFiles[FileType].FileName) + '''');
      NewLine;
      case FileType of
        sftInp, sftIcs, sftLst, sftRst, sftNod, sftEle, sftBcof, sftBcos,
          sftBcop, sftBcou, sftSmy:
          begin
            Model.AddModelInputFile(FFiles[FileType].FileName);
          end;
        sftObs, sftObc:
          begin
            // do nothing.
          end
        else Assert(False);
      end;
    end;
  end;
  for FileIndex := 0 to FBoundaries.Count - 1 do
  begin
    Item := FBoundaries[FileIndex];
    WriteString('''BCS''');
    WriteInteger(Item.UnitNumber);
    WriteString(' ''' + ExtractRelativePath(FFileRoot, Item.FileName) + '''');
    NewLine;
    Model.AddModelInputFile(Item.FileName);
  end;
//  finally
//    CloseFile;
//  end;
end;

end.
