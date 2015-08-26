unit FootprintFileUnit;

interface

uses
  System.Classes, System.SysUtils, FootPrintUtilities, System.IOUtils, FastGEO;

type
  EFootPrintFileException = class(Exception);
  EUnrecognizedTag = class(EFootPrintFileException);
  ETagStartError = class(EFootPrintFileException);
  EFootPrintArrayError = class(EFootPrintFileException);
  EFootPrintColumnCountError = class(EFootPrintFileException);
  EFootPrintRowCountError = class(EFootPrintFileException);
  EFootPrintTagTerminatorError = class(EFootPrintFileException);
  EFootPrintMissingTag = class(EFootPrintFileException);
  EFootPrintWrongVersion = class(EFootPrintFileException);
  EFootPrintConvertError = class(EFootPrintFileException);
  EFootPrintHeaderError = class(EFootPrintFileException);


  TFootPrintFile = class(TObject)
  private
    const
    FileHeader = 'Well_Footprint_File';
    FileVersion = 1;
    FileFormatVersionTag = 'FileFormatVersion';
    NumberOfRowsTag = 'Number_Of_Rows';
    NumberOfColumnsTag = 'Number_Of_Columns';
    ClosureCriterionTag = 'Closure_Criterion';
    MaxIterationsTag = 'Max_Iterations';
    InitialDistributionTag = 'Initial_Distribution';
    RedistributionCriterionTag = 'Redistribution_Criterion';
    DepthRateIndexTag = 'Depth_Rate_Index';
    WithdrawalTag = 'Withdrawals';
    DirectTag = 'Direct';
    TextFileTag = 'Text_File';
    BinaryFileTag = 'Binary_File';
    CellSizeTag = 'Cell_Size';
    GridAngleTag = 'Grid_Angle';
    OutlineTag = 'Outline';
    ListingFileTag = 'Listing_File';
    AsciiOutputFileTag = 'Text_Results_File';
    BinaryOutputFileTag = 'Binary_Results_File';
    ActiveTag = 'Active';
    Indentation = '  ';
    procedure ReadToStartOfArrayInTextResultsFile(Reader: TStreamReader;
      TextFileName, ArrayName: string; var NumberOfRows, NumberOfColumns: integer);
    procedure ReadAsciRealArray(var AnArray: TTwoDRealArray; Reader: TStreamReader; NumberOfRows, NumberOfColumns: integer);
    procedure ReadAsciIntArray(var AnArray: TTwoDIntArray; Reader: TStreamReader; NumberOfRows, NumberOfColumns: integer);
    procedure ReadRealArray(var AnArray: TTwoDRealArray; StartTag: string;
      NumberOfRows, NumberOfColumns: integer; ArrayName: string = '');
    procedure CheckArrayIndicies(Row, Col: Integer);
    function GetDepthRateIndex(Row, Col: Integer): double;
    function GetWithdrawals(Row, Col: Integer): double;
    procedure SetCapacity(Row, Col: Integer; const Value: double);
    procedure SetWithdrawals(Row, Col: Integer; const Value: double);
    procedure SetCellSize(const Value: double);
    function GetDepthRateIndexArray: TTwoDRealArray;
    function GetWithdrawalsArray: TTwoDRealArray;
    procedure SetGridAngle(const Value: Double);
    procedure SetOutline(const Value: TPolygon2D);
    function GetOutline: TPolygon2D;
    function IsExpectedTerminator(ALine, ExpectedTag: string): boolean;
    procedure WriteOutline;
    procedure SetAsciiFileName(const Value: string);
    procedure SetBinaryFileName(const Value: string);
    procedure SetListingFileName(const Value: string);
    procedure ReadAsciiFileName;
    procedure ReadBinaryFilename;
    function GetAsciiFileName: string;
    function GetBinaryFileName: string;
    function GetListingFileName: string;
    function GetActive(Row, Col: Integer): Boolean;
    procedure SetActive(Row, Col: Integer; const Value: Boolean);
    procedure ReadBooleanArrayFromFile(var AnArray: TTwoDBooleanArray;
      StartTag: string);
    procedure ReadBooleanArray(var AnArray: TTwoDBooleanArray;
      Reader: TStreamReader);
    procedure WriteTaggedBooleanArray(Tag: string; Value: TTwoDBooleanArray);
    function GetActiveArray: TTwoDBooleanArray;
    procedure MoveToStartOfArrayInBinaryResultsFile(const ArrayName: string;
      BinaryFile: TFileStream; var NumberOfRows, NumberOfColumns: integer);
    var
    FSettingsFileWriter: TStreamWriter;
    FSettingsFileReader: TStreamReader;
    // version number of the file being read.
    FVersion: Integer;
    FFileName: string;
    FNumberOfRows: Integer;
    FNumberOfColumns: Integer;
    FMaxIterations: integer;
    FClosureCriterion: double;
    FRedistributionCriterion: Integer;
    FInitialDistribution: boolean;
    FDepthRateIndex: TTwoDRealArray;
    FWithdrawals: TTwoDRealArray;
    FActive: TTwoDBooleanArray;
    FCellSize: double;
    FGridAngle: double;
    FOutline: TPolygon2D;
    FBinaryFileName: string;
    FListingFileName: string;
    FAsciiFileName: string;
    procedure WriteTaggedInteger(Tag: string; Value: Integer);
    procedure WriteTagStart(Tag: string);
    procedure WriteTagTerminator(Tag: string);
    function ReadTagStart: string;
    procedure ReadTagTerminator(ExpectedTag: string);
    procedure ReadTaggedLine(ExpectedTag: string; var ALine: string);
    procedure ReadTagContents(ExpectedTag: string; var ALine: string);
    procedure ReadNumberOfRows;
    procedure ReadNumberOfColumns;
    procedure InitializeArrays;
    function FormatTagStart(Tag: string): string;
    function FormatTagTerminator(Tag: string): string;
    procedure SetNumberOfColumns(const Value: Integer);
    procedure SetNumberOfRows(const Value: Integer);
    function SameTag(ATag, ExpectedTag: string): boolean;
    procedure ReadHeader;
    procedure ReadVersion;
    procedure SetClosureCriterion(const Value: double);
    procedure SetInitialDistribution(const Value: boolean);
    procedure SetMaxIterations(const Value: integer);
    procedure SetRedistributionCriterion(const Value: Integer);
    procedure ReadClosureCriterion;
    procedure ReadMaximumNumberOfIterations;
    procedure ReadRedistributionCriterion;
    procedure ReadCellSize;
    procedure WriteTaggedReal(Tag: string; Value: double);
    procedure WriteTaggedBoolean(Tag: string; Value: Boolean);
    procedure WriteTaggedString(Tag: string; Value: string);
    procedure ReadInitialRedistribution;
    procedure WriteTaggedRealArray(Tag: string; Value: TTwoDRealArray);
    procedure ReadGridAngle;
    procedure ReadOutline;
    procedure ReadListingFile;
  public
    constructor Create;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    property NumberOfRows: Integer read FNumberOfRows write SetNumberOfRows;
    property NumberOfColumns: Integer read FNumberOfColumns
      write SetNumberOfColumns;
    property ClosureCriterion: double read FClosureCriterion
      write SetClosureCriterion;
    property CellSize: double read FCellSize write SetCellSize;
    property MaxIterations: integer read FMaxIterations write SetMaxIterations stored True;
    property InitialDistribution: boolean read FInitialDistribution
      write SetInitialDistribution;
    property RedistributionCriterion: Integer read FRedistributionCriterion
      write SetRedistributionCriterion;
    property DepthRateIndex[Row, Col: Integer]: double read GetDepthRateIndex
      write SetCapacity;
    property Withdrawals[Row, Col: Integer]: double read GetWithdrawals
      write SetWithdrawals;
    property Active[Row, Col: Integer]: Boolean read GetActive write SetActive;
    property DepthRateIndexArray: TTwoDRealArray read GetDepthRateIndexArray;
    property WithdrawalsArray: TTwoDRealArray read GetWithdrawalsArray;
    property ActiveArray: TTwoDBooleanArray read GetActiveArray;
    property Outline: TPolygon2D read GetOutline write SetOutline;
    // @name is measured counterclockwise in degrees.
    property GridAngle: Double read FGridAngle write SetGridAngle;
    property ListingFileName: string read GetListingFileName
      write SetListingFileName;
    property AsciiFileName: string read GetAsciiFileName write SetAsciiFileName;
    property BinaryFileName: string read GetBinaryFileName
      write SetBinaryFileName;
    function ListingFileFullPath: string;
    function AsciiFileFullPath: string;
    function BinaryFileFullPath: string;
    procedure ReadTextRealArray(TextFileName: string; var Reader: TStreamReader;
      var AnArray: TTwoDRealArray; ArrayName: string);
    procedure ReadBinaryRealArray(BinaryFileName:
      string; var BinaryFile: TFileStream; var AnArray: TTwoDRealArray;
      ArrayName: string);
    procedure ReadTextIntArray(TextFileName: string; var Reader: TStreamReader;
      var AnArray: TTwoDIntArray; ArrayName: string);
    procedure ReadBinaryIntArray(BinaryFileName: string;
      var BinaryFile: TFileStream; var AnArray: TTwoDIntArray; ArrayName: string);
  end;

const
  StrDistributedWithdraw = 'Distributed_Withdrawals';
  StrFootprintCode = 'Footprint_Code';
  StrFootprintBinaryFile = 'Footprint_Binary_File';

implementation

uses
  System.Generics.Collections;

const
  StrTagStart = '<';
  StrSlash = '/';
  StrTagTerminatorStart = StrTagStart + StrSlash;
  StrTagEnd = '>';


resourcestring
  StrSIsNotAFootprin = '%s is not a Footprint file';
//  StrTagTerminator = '/>';
  StrErrorReadingTheFo = 'Error Reading the following line.';
  StrItShouldBeATagB = 'It should be a tag but it isn''t.';
  StrErrorSettingTheNu = 'Error setting the number of %0:s to %1:d. The numb' +
  'er of %0:s must be greater than 0.';
  StrUnableToConvert = 'Unable to convert "%0:s" to %1:s in ' +
  '%2:s';
  StrUnrecognizedTag0 = 'Unrecognized tag "%0:s" in %1:s';
  StrTheEnd0sTagIs = 'The end %0:s tag is missing from %s';
  StrErrorReadingTheNu = 'Error reading the number of %0:s from %1:s. The nu' +
  'mber of %0:s should equal %2:d but in the file, it is %3:d.';
  StrErrorReadingCount = 'Error reading the number of %0:s from %1:s';

{ TFootPrintFile }

function TFootPrintFile.ListingFileFullPath: string;
var
  CurrentDir: string;
begin
  if FListingFileName <> '' then
  begin
    CurrentDir := GetCurrentDir;
    try
      SetCurrentDir(ExtractFileDir(FFileName));
      result := TPath.GetFullPath(FListingFileName)
    finally
      SetCurrentDir(CurrentDir)
    end;
  end
  else
  begin
    result := ''
  end;
end;

procedure TFootPrintFile.LoadFromFile(FileName: string);
var
  ATag: string;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  FFileName := FileName;
  FSettingsFileReader := TFile.OpenText(FFileName);
  try
    ReadHeader;
    ReadVersion;
    while not FSettingsFileReader.EndOfStream do
    begin
      ATag := ReadTagStart;
      if FSettingsFileReader.EndOfStream then
      begin
        break;
      end;

      if SameTag(ATag, NumberOfRowsTag) then
      begin
        ReadNumberOfRows;
      end
      else if SameTag(ATag, NumberOfColumnsTag) then
      begin
        ReadNumberOfColumns;
      end
      else if SameTag(ATag, ClosureCriterionTag) then
      begin
        ReadClosureCriterion;
      end
      else if SameTag(ATag, MaxIterationsTag) then
      begin
        ReadMaximumNumberOfIterations;
      end
      else if SameTag(ATag, InitialDistributionTag) then
      begin
        ReadInitialRedistribution;
      end
      else if SameTag(ATag, RedistributionCriterionTag) then
      begin
        ReadRedistributionCriterion;
      end
      else if SameTag(ATag, CellSizeTag) then
      begin
        ReadCellSize;
      end
      else if SameTag(ATag, GridAngleTag) then
      begin
        ReadGridAngle;
      end
      else if SameTag(ATag, OutlineTag) then
      begin
        ReadOutline;
      end
      else if SameTag(ATag, ListingFileTag) then
      begin
        ReadListingFile;
      end
      else if SameTag(ATag, AsciiOutputFileTag) then
      begin
        ReadAsciiFileName;
      end
      else if SameTag(ATag, BinaryOutputFileTag) then
      begin
        ReadBinaryFilename;
      end
      else if SameTag(ATag, DepthRateIndexTag) then
      begin
        ReadRealArray(FDepthRateIndex, DepthRateIndexTag, NumberOfRows, NumberOfColumns);
      end
      else if SameTag(ATag, WithdrawalTag) then
      begin
        ReadRealArray(FWithdrawals, WithdrawalTag, NumberOfRows, NumberOfColumns, StrDistributedWithdraw);
      end
      else if SameTag(ATag, ActiveTag) then
      begin
        ReadBooleanArrayFromFile(FActive, ActiveTag);
      end
      else
      begin
        raise EUnrecognizedTag.Create(
          Format(StrUnrecognizedTag0, [ATag, FFileName]));
      end;
    end;
  finally
    FSettingsFileReader.Free;
  end;
  for RowIndex := 0 to NumberOfRows - 1 do
  begin
    for ColIndex := 0 to NumberOfColumns - 1 do
    begin
      if not Active[RowIndex,ColIndex]  then
      begin
        Withdrawals[RowIndex,ColIndex] := 0;
      end;
    end;
  end;
end;

function TFootPrintFile.ReadTagStart: string;
var
  ALine: string;
begin
  repeat
    ALine := FSettingsFileReader.ReadLine;
  until ((Length(ALine) > 0) and (ALine[1] <> '#'))
    or FSettingsFileReader.EndOfStream;
  ALine := Trim(ALine);
  if FSettingsFileReader.EndOfStream then
  begin
    result := ALine;
  end
  else
  begin
    if (Length(ALine) > 2) and (ALine[1] = StrTagStart)
      and (ALine[Length(ALine)] = StrTagEnd)
      and (ALine[2] <> StrSlash) then
    begin
      result := Trim(UpperCase(Copy(ALine, 2, Length(ALine)-2)));
    end
    else
    begin
      raise ETagStartError.Create(StrErrorReadingTheFo
        + sLineBreak + ALine + sLineBreak
        + StrItShouldBeATagB);
    end;
  end;
end;

function TFootPrintFile.IsExpectedTerminator(
  ALine, ExpectedTag: string): boolean;
begin
  ALine := Trim(ALine);
  if (Length(ALine) >= 4) and (Copy(ALine, 1, 2) = StrTagTerminatorStart)
    and (ALine[Length(ALine)] = StrTagEnd) then
  begin
    ALine := Trim(Copy(ALine, 3, Length(ALine)-3));
    result := UpperCase(ExpectedTag) = UpperCase(ALine)
  end
  else
  begin
    result := False;
  end;
end;

procedure TFootPrintFile.ReadTagTerminator(ExpectedTag: string);
var
  ALine: string;
  FoundExpectedTag: Boolean;
begin
  ALine := FSettingsFileReader.ReadLine;
  FoundExpectedTag := IsExpectedTerminator(ALine, ExpectedTag);
//  if (Length(ALine) >= 4) and (Copy(ALine, 1, 2) = StrTagTerminatorStart)
//    and (ALine[Length(ALine)] = StrTagEnd) then
//  begin
//    ALine := Trim(Copy(ALine, 3, Length(ALine)-3));
//    FoundExpectedTag := UpperCase(ExpectedTag) = UpperCase(ALine)
//  end
//  else
//  begin
//    FoundExpectedTag := False;
//  end;
  if not FoundExpectedTag then
  begin
    raise EFootPrintTagTerminatorError.Create(Format(StrTheEnd0sTagIs,
      [ExpectedTag, FFileName]));
  end;
end;

function TFootPrintFile.SameTag(ATag, ExpectedTag: string): boolean;
begin
  result := Trim(UpperCase(ATag)) = Trim(UpperCase(ExpectedTag))
end;

procedure TFootPrintFile.WriteOutline;
var
  LineIndex: Integer;
begin
  WriteTagStart(OutlineTag);
  for LineIndex := 0 to Length(FOutline) - 1 do
  begin
    FSettingsFileWriter.Write(Indentation);
    FSettingsFileWriter.Write(FOutline[LineIndex].x);
    FSettingsFileWriter.Write(', ');
    FSettingsFileWriter.Write(FOutline[LineIndex].y);
    FSettingsFileWriter.WriteLine;
  end;
  WriteTagTerminator(OutlineTag);
end;

procedure TFootPrintFile.SaveToFile(FileName: string);
begin
  FFileName := FileName;
  FSettingsFileWriter := TFile.CreateText(FFileName);
  try
    FSettingsFileWriter.WriteLine(FileHeader);
    WriteTaggedInteger(FileFormatVersionTag, FileVersion);
    WriteTaggedInteger(NumberOfRowsTag, NumberOfRows);
    WriteTaggedInteger(NumberOfColumnsTag, NumberOfColumns);
    WriteTaggedReal(ClosureCriterionTag, ClosureCriterion);
    WriteTaggedReal(CellSizeTag, CellSize);
    WriteTaggedInteger(MaxIterationsTag, MaxIterations);
    WriteTaggedBoolean(InitialDistributionTag, InitialDistribution);
    WriteTaggedInteger(RedistributionCriterionTag, RedistributionCriterion);
    WriteTaggedReal(GridAngleTag, GridAngle);
    WriteOutline;
    WriteTaggedRealArray(DepthRateIndexTag, FDepthRateIndex);
    WriteTaggedRealArray(WithdrawalTag, FWithdrawals);
    WriteTaggedBooleanArray(ActiveTag, FActive);
    WriteTaggedString(ListingFileTag, FListingFileName);
    WriteTaggedString(AsciiOutputFileTag, FAsciiFileName);
    WriteTaggedString(BinaryOutputFileTag, FBinaryFileName);
  finally
    FSettingsFileWriter.Free;
  end;
end;

procedure TFootPrintFile.ReadAsciRealArray(var AnArray: TTwoDRealArray;
  Reader: TStreamReader; NumberOfRows, NumberOfColumns: integer);
var
  RowIndex: Integer;
  index: Integer;
  Splitter: TStringList;
  ColIndex: Integer;
begin
  Splitter := TStringList.Create;
  try
    for RowIndex := 0 to NumberOfRows - 1 do
    begin
      index := 0;
      Splitter.CommaText := Reader.ReadLine;
      for ColIndex := 0 to NumberOfColumns - 1 do
      begin
        if index >= Splitter.Count then
        begin
          Splitter.CommaText := Reader.ReadLine;
          index := 0;
        end;
        AnArray[RowIndex, ColIndex] := FortranStrToFloat(Splitter[index]);
        Inc(index);
        if ColIndex = NumberOfColumns - 1 then
        begin
          Assert(index = Splitter.Count);
        end;
      end;
    end;
  finally
    Splitter.Free;
  end;
end;

procedure TFootPrintFile.ReadBooleanArray(var AnArray: TTwoDBooleanArray;
  Reader: TStreamReader);
var
  RowIndex: Integer;
  index: Integer;
  Splitter: TStringList;
  ColIndex: Integer;
begin
  Splitter := TStringList.Create;
  try
    for RowIndex := 0 to NumberOfRows - 1 do
    begin
      index := 0;
      Splitter.CommaText := Reader.ReadLine;
      for ColIndex := 0 to NumberOfColumns - 1 do
      begin
        if index >= Splitter.Count then
        begin
          Splitter.CommaText := Reader.ReadLine;
          index := 0;
        end;
        AnArray[RowIndex, ColIndex] := StrToInt(Splitter[index]) <> 0;
        Inc(index);
        if ColIndex = NumberOfColumns - 1 then
        begin
          Assert(index = Splitter.Count);
        end;
      end;
    end;
  finally
    Splitter.Free;
  end;
end;

procedure TFootPrintFile.ReadBooleanArrayFromFile(
  var AnArray: TTwoDBooleanArray; StartTag: string);
var
  MethodTag: string;
  Reader: TStreamReader;
  TextFileName: string;
  CurrDir: string;
  BinaryFileName: string;
  RowIndex: Integer;
  ColIndex: Integer;
  BinaryFile: TFileStream;
  AnInt: longint;
begin
  MethodTag := ReadTagStart;
  if SameTag(MethodTag, DirectTag) then
  begin
    Reader := FSettingsFileReader;
    ReadBooleanArray(AnArray, Reader);
    ReadTagTerminator(DirectTag);
  end
  else if SameTag(MethodTag, TextFileTag) then
  begin
    TextFileName := Trim(FSettingsFileReader.ReadLine);
    CurrDir := GetcurrentDir;
    try
      SetCurrentDir(ExtractFileDir(FFileName));
      try
        Reader := TFile.OpenText(TextFileName);
      except
      {$IFDEF MSWINDOWS}
        on E: ENotSupportedException do
        begin
          raise EFootPrintFileException.Create(
            'Error opening "' + TextFileName + '". Check that the file name is valid');
        end;
      {$ENDIF}
        on E: EArgumentException do
        begin
          raise EFootPrintFileException.Create(
            'Error opening "' + TextFileName + '". Check that the file name is valid');
        end;
        on E: EDirectoryNotFoundException do
        begin
          raise EFootPrintFileException.Create(
            'Error opening "' + TextFileName + '". Check that the directory exists');
        end;
        on E: EFileNotFoundException do
        begin
          raise EFootPrintFileException.Create(
            'Error opening "' + TextFileName + '". Check that the file exists');
        end;
      end;
      try
        ReadBooleanArray(AnArray, Reader);
      finally
        Reader.Free;
      end;
    finally
      SetCurrentDir(CurrDir)
    end;
    ReadTagTerminator(TextFileTag);
  end
  else if SameTag(MethodTag, BinaryFileTag) then
  begin
    BinaryFileName := Trim(FSettingsFileReader.ReadLine);
    CurrDir := GetcurrentDir;
    try
      SetCurrentDir(ExtractFileDir(FFileName));
      try
        BinaryFile := TFile.Open(BinaryFileName, TFileMode.fmOpen);
      except
      {$IFDEF MSWINDOWS}
        on E: ENotSupportedException do
        begin
          raise EFootPrintFileException.Create(
            'Error opening "' + TextFileName + '". Check that the file name is valid');
        end;
      {$ENDIF}
        on E: EArgumentException do
        begin
          raise EFootPrintFileException.Create(
            'Error opening "' + TextFileName + '". Check that the file name is valid');
        end;
        on E: EDirectoryNotFoundException do
        begin
          raise EFootPrintFileException.Create(
            'Error opening "' + TextFileName + '". Check that the directory exists');
        end;
        on E: EFileNotFoundException do
        begin
          raise EFootPrintFileException.Create(
            'Error opening "' + TextFileName + '". Check that the file exists');
        end;
      end;
      try
        for RowIndex := 0 to NumberOfRows - 1 do
        begin
          for ColIndex := 0 to NumberOfColumns - 1 do
          begin
            AnInt := BinaryFile.Read(AnInt, SizeOf(AnInt));
            AnArray[RowIndex,ColIndex] := AnInt <> 0;
          end;
        end;
      finally
        BinaryFile.Free;
      end;
    finally
      SetCurrentDir(CurrDir)
    end;
    ReadTagTerminator(BinaryFileTag)
  end
  else
  begin
    raise EUnrecognizedTag.Create(
      Format(StrUnrecognizedTag0, [MethodTag, FFileName]));
  end;
  ReadTagTerminator(StartTag);
end;

procedure TFootPrintFile.MoveToStartOfArrayInBinaryResultsFile(
  const ArrayName: string; BinaryFile: TFileStream; var NumberOfRows, NumberOfColumns: integer);
var
  FileID: string;
  CharArray: array of Char;
  SizeOfDouble: longint;
  ACount: longint;
  FileText: string;
begin
  if  ArrayName <> '' then
  begin
    FileID := StrFootprintBinaryFile;
    SetLength(CharArray, Length(FileID));
    BinaryFile.Read(CharArray[0], Length(FileID)*SizeOf(Char));
    FileText := CharArrayToString(CharArray);
    // case sensitive
    if AnsiCompareStr(FileText,FileID) = 0 then
    begin
      BinaryFile.Read(SizeOfDouble, SizeOf(SizeOfDouble));
      Assert(SizeOfDouble = SizeOf(double));

      repeat
        BinaryFile.Read(ACount, SizeOf(ACount));
        SetLength(CharArray, ACount);
        BinaryFile.Read(CharArray[0], ACount*SizeOf(Char));

        BinaryFile.Read(ACount, SizeOf(ACount));
        if NumberOfRows < 0 then
        begin
          NumberOfRows := ACount;
        end;
        Assert(ACount = NumberOfRows);
        BinaryFile.Read(ACount, SizeOf(ACount));
        if NumberOfColumns < 0 then
        begin
          NumberOfColumns := ACount;
        end;
        Assert(ACount = NumberOfColumns);


        FileText := CharArrayToString(CharArray);
        // not case sensitive
        if  AnsiCompareText(FileText,ArrayName) = 0 then
        begin
          break;
        end;
        BinaryFile.Position := BinaryFile.Position
          + NumberOfRows*NumberOfColumns*SizeOf(Double);
      until BinaryFile.Size = BinaryFile.Position;
    end
    else
    begin
      BinaryFile.Position := 0;
    end;
  end
end;

procedure TFootPrintFile.ReadRealArray(var AnArray: TTwoDRealArray;
  StartTag: string; NumberOfRows, NumberOfColumns: integer; ArrayName: string = '');
var
  MethodTag: string;
  Reader: TStreamReader;
  TextFileName: string;
  CurrDir: string;
  BinaryFileName: string;
  BinaryFile: TFileStream;
begin
  MethodTag := ReadTagStart;
  if SameTag(MethodTag, DirectTag) then
  begin
    Reader := FSettingsFileReader;
    ReadAsciRealArray(AnArray, Reader, NumberOfRows, NumberOfColumns);
    ReadTagTerminator(DirectTag);
  end
  else if SameTag(MethodTag, TextFileTag) then
  begin
    TextFileName := Trim(FSettingsFileReader.ReadLine);
    CurrDir := GetcurrentDir;
    try
      SetCurrentDir(ExtractFileDir(FFileName));
      Reader := nil;
      try
        ReadTextRealArray(TextFileName, Reader, AnArray, ArrayName);
      finally
        Reader.Free;
      end;
    finally
      SetCurrentDir(CurrDir)
    end;
    ReadTagTerminator(TextFileTag);
  end
  else if SameTag(MethodTag, BinaryFileTag) then
  begin
    BinaryFileName := Trim(FSettingsFileReader.ReadLine);
    CurrDir := GetcurrentDir;
    try
      SetCurrentDir(ExtractFileDir(FFileName));
      BinaryFile := nil;
      try
        ReadBinaryRealArray(BinaryFileName, BinaryFile, AnArray, ArrayName );
      finally
        BinaryFile.Free;
      end;
    finally
      SetCurrentDir(CurrDir)
    end;
    ReadTagTerminator(BinaryFileTag)
  end
  else
  begin
    raise EUnrecognizedTag.Create(
      Format(StrUnrecognizedTag0, [MethodTag, FFileName]));
  end;
  ReadTagTerminator(StartTag);
end;

function TFootPrintFile.AsciiFileFullPath: string;
var
  CurrentDir: string;
begin
  if FAsciiFileName <> '' then
  begin
    CurrentDir := GetCurrentDir;
    try
      SetCurrentDir(ExtractFileDir(FFileName));
      result := TPath.GetFullPath(FAsciiFileName)
    finally
      SetCurrentDir(CurrentDir)
    end;
  end
  else
  begin
    Result := ''
  end;
end;

function TFootPrintFile.BinaryFileFullPath: string;
var
  CurrentDir: string;
begin
  if FBinaryFileName <> '' then
  begin
    CurrentDir := GetCurrentDir;
    try
      SetCurrentDir(ExtractFileDir(FFileName));
      result := TPath.GetFullPath(FBinaryFileName)
    finally
      SetCurrentDir(CurrentDir)
    end;
  end
  else
  begin
    Result := ''
  end;
end;

procedure TFootPrintFile.ReadTextIntArray(TextFileName: string;
  var Reader: TStreamReader; var AnArray: TTwoDIntArray; ArrayName: string);
var
  NumRow: Integer;
  NumCol: Integer;
begin
  try
    if Reader = nil then
    begin
      Reader := TFile.OpenText(TextFileName);
    end;
  except
    on E: ENotSupportedException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + TextFileName + '". Check that the file name is valid');
    end;
    on E: EArgumentException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + TextFileName + '". Check that the file name is valid');
    end;
    on E: EDirectoryNotFoundException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + TextFileName + '". Check that the directory exists');
    end;
    on E: EFileNotFoundException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + TextFileName + '". Check that the file exists');
    end;
  end;
  NumRow := NumberOfRows;
  NumCol := NumberOfColumns;
  ReadToStartOfArrayInTextResultsFile(Reader, TextFileName, ArrayName, NumRow, NumCol);
  SetLength(AnArray, NumRow, NumCol);
  ReadAsciIntArray(AnArray, Reader, NumRow, NumCol);
end;

procedure TFootPrintFile.ReadTextRealArray(TextFileName: string;
  var Reader: TStreamReader; var AnArray: TTwoDRealArray; ArrayName: string);
var
  NumRow: Integer;
  NumCol: Integer;
begin
  try
    if Reader = nil then
    begin
      Reader := TFile.OpenText(TextFileName);
    end;
  except
    on E: ENotSupportedException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + TextFileName + '". Check that the file name is valid');
    end;
    on E: EArgumentException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + TextFileName + '". Check that the file name is valid');
    end;
    on E: EDirectoryNotFoundException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + TextFileName + '". Check that the directory exists');
    end;
    on E: EFileNotFoundException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + TextFileName + '". Check that the file exists');
    end;
  end;
  NumRow := NumberOfRows;
  NumCol := NumberOfColumns;
  ReadToStartOfArrayInTextResultsFile(Reader, TextFileName, ArrayName, NumRow, NumCol);
  SetLength(AnArray, NumRow, NumCol);
  ReadAsciRealArray(AnArray, Reader, NumRow, NumCol);
end;

procedure TFootPrintFile.ReadBinaryRealArray(BinaryFileName: string;
  var BinaryFile: TFileStream; var AnArray: TTwoDRealArray; ArrayName: string);
var
  RowIndex: Integer;
  ColIndex: Integer;
  NumRows: Integer;
  NumCols: Integer;
begin
  try
    if BinaryFile = nil then
    begin
      BinaryFile := TFile.Open(BinaryFileName, TFileMode.fmOpen);
    end;
  except
    on E: ENotSupportedException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + BinaryFileName + '". Check that the file name is valid');
    end;
    on E: EArgumentException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + BinaryFileName + '". Check that the file name is valid');
    end;
    on E: EDirectoryNotFoundException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + BinaryFileName + '". Check that the directory exists');
    end;
    on E: EFileNotFoundException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + BinaryFileName + '". Check that the file exists');
    end;
  end;
  NumRows := NumberOfRows;
  NumCols := NumberOfColumns;
  MoveToStartOfArrayInBinaryResultsFile(ArrayName, BinaryFile, NumRows, NumCols);
  SetLength(AnArray, NumRows, NumCols);
  for RowIndex := 0 to NumRows - 1 do
  begin
    for ColIndex := 0 to NumCols - 1 do
    begin
      BinaryFile.Read(AnArray[RowIndex, ColIndex],
        SizeOf(AnArray[RowIndex, ColIndex]));
    end;
  end;
end;

procedure TFootPrintFile.ReadToStartOfArrayInTextResultsFile(
  Reader: TStreamReader; TextFileName, ArrayName: string; var NumberOfRows, NumberOfColumns: integer);
var
  ACount: Integer;
  ALine: string;
begin
  if ArrayName <> '' then
  begin
    ALine := Reader.ReadLine;
    if AnsiCompareText(ALine, ArrayName) = 0 then
    begin
      ALine := Reader.ReadLine;
      if TryStrToInt(ALine, ACount) then
      begin
        if NumberOfRows < 0 then
        begin
          NumberOfRows := ACount;
        end;
        if ACount <> NumberOfRows then
        begin
          raise EFootPrintRowCountError.Create(
            Format(StrErrorReadingTheNu,
            ['rows', TextFileName, NumberOfRows, ACount]));
        end;
      end
      else
      begin
        raise EFootPrintRowCountError.Create(Format(
          StrErrorReadingCount, ['rows', TextFileName]));
      end;
      ALine := Reader.ReadLine;
      if TryStrToInt(ALine, ACount) then
      begin
        if NumberOfColumns < 0 then
        begin
          NumberOfColumns := ACount;
        end;
        if ACount <> NumberOfColumns then
        begin
          raise EFootPrintColumnCountError.Create(
            Format(StrErrorReadingTheNu,
            ['columns', TextFileName, NumberOfRows, ACount]));
        end;
      end
      else
      begin
        raise EFootPrintColumnCountError.Create(Format(
          StrErrorReadingCount, ['columns', TextFileName]));
      end;
    end
    else
    begin
      Reader.DiscardBufferedData;
      Reader.BaseStream.Position := 0;
    end;
  end;
end;

procedure TFootPrintFile.CheckArrayIndicies(Row, Col: Integer);
begin
  if (Row < 0) or (Col < 0)
    or (Row >= NumberOfRows) or (Col >= NumberOfColumns) then
  begin
    raise EFootPrintArrayError.Create('Array index out of bounds');
  end;
end;

procedure TFootPrintFile.ReadInitialRedistribution;
var
  ALine: string;
  AValue: Boolean;
begin
  ReadTagContents(InitialDistributionTag, ALine);
  if TryStrToBool(Trim(ALine), AValue) then
  begin
    InitialDistribution := AValue;
  end
  else
  begin
    raise EFootPrintConvertError.Create(
      Format(StrUnableToConvert,
      [ALine, 'the initial redistribution', FFileName]));
  end;
end;

procedure TFootPrintFile.ReadListingFile;
var
  ALine: string;
begin
  ReadTagContents(ListingFileTag, ALine);
  ListingFileName := Trim(ALine);
end;

procedure TFootPrintFile.ReadAsciiFileName;
var
  ALine: string;
begin
  ReadTagContents(AsciiOutputFileTag, ALine);
  AsciiFileName := Trim(ALine);
end;

procedure TFootPrintFile.ReadAsciIntArray(var AnArray: TTwoDIntArray;
  Reader: TStreamReader; NumberOfRows, NumberOfColumns: integer);
var
  RowIndex: Integer;
  index: Integer;
  Splitter: TStringList;
  ColIndex: Integer;
begin
  Splitter := TStringList.Create;
  try
    for RowIndex := 0 to NumberOfRows - 1 do
    begin
      index := 0;
      Splitter.CommaText := Reader.ReadLine;
      for ColIndex := 0 to NumberOfColumns - 1 do
      begin
        if index >= Splitter.Count then
        begin
          Splitter.CommaText := Reader.ReadLine;
          index := 0;
        end;
        AnArray[RowIndex, ColIndex] := StrToInt(Splitter[index]);
        Inc(index);
        if ColIndex = NumberOfColumns - 1 then
        begin
          Assert(index = Splitter.Count);
        end;
      end;
    end;
  finally
    Splitter.Free;
  end;
end;

procedure TFootPrintFile.ReadBinaryFilename;
var
  ALine: string;
begin
  ReadTagContents(BinaryOutputFileTag, ALine);
  BinaryFileName := Trim(ALine);
end;


procedure TFootPrintFile.ReadBinaryIntArray(BinaryFileName: string;
  var BinaryFile: TFileStream; var AnArray: TTwoDIntArray;ArrayName: string);
var
  RowIndex: Integer;
  ColIndex: Integer;
  NumRows: Integer;
  NumCols: Integer;
begin
  try
    if BinaryFile = nil then
    begin
      BinaryFile := TFile.Open(BinaryFileName, TFileMode.fmOpen);
    end;
  except
    on E: ENotSupportedException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + BinaryFileName + '". Check that the file name is valid');
    end;
    on E: EArgumentException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + BinaryFileName + '". Check that the file name is valid');
    end;
    on E: EDirectoryNotFoundException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + BinaryFileName + '". Check that the directory exists');
    end;
    on E: EFileNotFoundException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + BinaryFileName + '". Check that the file exists');
    end;
  end;
  NumRows := NumberOfRows;
  NumCols := NumberOfColumns;
  MoveToStartOfArrayInBinaryResultsFile(ArrayName, BinaryFile, NumRows, NumCols);
  SetLength(AnArray, NumRows, NumCols);
  for RowIndex := 0 to NumRows - 1 do
  begin
    for ColIndex := 0 to NumCols - 1 do
    begin
      BinaryFile.Read(AnArray[RowIndex, ColIndex],
        SizeOf(AnArray[RowIndex, ColIndex]));
    end;
  end;
end;

procedure TFootPrintFile.ReadRedistributionCriterion;
var
  ALine: string;
  ANumber: Integer;
begin
  ReadTagContents(RedistributionCriterionTag, ALine);
  if TryStrToInt(Trim(ALine), ANumber) then
  begin
    RedistributionCriterion := ANumber;
  end
  else
  begin
    raise EFootPrintConvertError.Create(
      Format(StrUnableToConvert,
      [ALine, 'the redistribution criterion', FFileName]));
  end;
end;

procedure TFootPrintFile.ReadMaximumNumberOfIterations;
var
  ALine: string;
  ANumber: Integer;
begin
  ReadTagContents(MaxIterationsTag, ALine);
  if TryStrToInt(Trim(ALine), ANumber) then
  begin
    MaxIterations := ANumber;
  end
  else
  begin
    raise EFootPrintConvertError.Create(
      Format(StrUnableToConvert,
      [ALine, 'the maximum number of iterations', FFileName]));
  end;
end;

procedure TFootPrintFile.ReadCellSize;
var
  ALine: string;
  ANumber: Extended;
begin
  ReadTagContents(CellSizeTag, ALine);
  if TryFortranStrToFloat(Trim(ALine), ANumber) then
  begin
    CellSize := ANumber;
  end
  else
  begin
    raise EFootPrintConvertError.Create(
      Format(StrUnableToConvert, [ALine, 'the cell size', FFileName]));
  end;
end;

procedure TFootPrintFile.ReadClosureCriterion;
var
  ALine: string;
  ANumber: Extended;
begin
  ReadTagContents(ClosureCriterionTag, ALine);
  if TryFortranStrToFloat(Trim(ALine), ANumber) then
  begin
    ClosureCriterion := ANumber;
  end
  else
  begin
    raise EFootPrintConvertError.Create(
      Format(StrUnableToConvert, [ALine, 'the closure criterion', FFileName]));
  end;
end;

procedure TFootPrintFile.ReadGridAngle;
var
  ALine: string;
  ANumber: Extended;
begin
  ReadTagContents(GridAngleTag, ALine);
  if TryFortranStrToFloat(Trim(ALine), ANumber) then
  begin
    GridAngle := ANumber;
  end
  else
  begin
    raise EFootPrintConvertError.Create(
      Format(StrUnableToConvert, [ALine, 'the grid angle', FFileName]));
  end;
end;

procedure TFootPrintFile.ReadNumberOfColumns;
var
  ALine: string;
  ANumber: Integer;
begin
  ReadTagContents(NumberOfColumnsTag, ALine);
  if TryStrToInt(Trim(ALine), ANumber) then
  begin
    NumberOfColumns := ANumber;
  end
  else
  begin
    raise EFootPrintConvertError.Create(
      Format(StrUnableToConvert, [ALine, 'the number of columns', FFileName]));
  end;
end;

procedure TFootPrintFile.ReadNumberOfRows;
var
  ALine: string;
  ANumber: Integer;
begin
  ReadTagContents(NumberOfRowsTag, ALine);
  if TryStrToInt(Trim(ALine), ANumber) then
  begin
    NumberOfRows := ANumber;
  end
  else
  begin
    raise EFootPrintConvertError.Create(
      Format(StrUnableToConvert, [ALine, 'the number of rows', FFileName]));
  end;

end;

procedure TFootPrintFile.ReadOutline;
var
  Splitter: TStringList;
  ALine: string;
  FoundEnd: Boolean;
  index: Integer;
  IsX: Boolean;
  Value: Extended;
  APoint2D: TPoint2D;
  OutlineList: TList<TPoint2D>;
begin
  Splitter := TStringList.Create;
  OutlineList := TList<TPoint2D>.Create;
  try
    FoundEnd := False;
    index := 0;
    IsX := True;
    repeat
      if (not FoundEnd) then
      begin
        if index >= Splitter.Count then
        begin
          ALine := FSettingsFileReader.ReadLine;
          FoundEnd := IsExpectedTerminator(ALine, OutlineTag);
          if FoundEnd then
          begin
            Break;
          end;
          Splitter.CommaText := ALine;
          if Splitter.Count = 0 then
          begin
            Continue;
          end;
          index := 0;
        end;
        if TryFortranStrToFloat(Splitter[index], Value) then
        begin
          if IsX then
          begin
            APoint2D.x := Value;
          end
          else
          begin
            APoint2D.Y := Value;
            OutlineList.Add(APoint2D);
          end;
          IsX := not IsX;
          Inc(Index);
        end
        else
        begin
          raise EFootPrintConvertError.Create(Format(StrUnableToConvert,
            [ALine, 'A coordinate of the outline', FFileName]));
        end;
      end;
    until FoundEnd or FSettingsFileReader.EndOfStream;
    SetLength(FOutline, OutlineList.Count);
    for index := 0 to OutlineList.Count - 1 do
    begin
      FOutline[index] := OutlineList[index];
    end;
  finally
    Splitter.Free;
    OutlineList.Free;
  end;
end;

procedure TFootPrintFile.ReadTagContents(ExpectedTag: string;
  var ALine: string);
begin
  ALine := FSettingsFileReader.ReadLine;
  ReadTagTerminator(ExpectedTag);
end;

procedure TFootPrintFile.ReadTaggedLine(ExpectedTag: string; var ALine: string);
var
  ATag: string;
begin
  ATag := ReadTagStart;
  if not SameTag(ATag, ExpectedTag) then
  begin
    raise EFootPrintMissingTag.Create(
      Format('The %0:s tag is missing from %1:s', [ExpectedTag, FFileName]));
  end;
  ReadTagContents(ExpectedTag, ALine);
end;

procedure TFootPrintFile.ReadVersion;
var
  ALine: string;
  ExpectedTag: string;
begin
  ExpectedTag := FileFormatVersionTag;
  ReadTaggedLine(ExpectedTag, ALine);
  if TryStrToInt(Trim(ALine), FVersion) then
  begin
    if not (FVersion in [FileVersion]) then
    begin
      raise EFootPrintWrongVersion.Create(
        Format('Unrecognized file version %0:d in %1:s', [FVersion, FFileName]));
    end;
  end
  else
  begin
    raise EFootPrintConvertError.Create(
      Format('Unable to convert "%0:s" to a version number in %s', [ALine, FFileName]));
  end;
end;

procedure TFootPrintFile.ReadHeader;
var
  ALine: string;
begin
  ALine := FSettingsFileReader.ReadLine;
  if ALine <> FileHeader then
  begin
    raise EFootPrintHeaderError.Create(Format(StrSIsNotAFootprin, [FFileName]));
  end;
end;

procedure TFootPrintFile.InitializeArrays;
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  if (NumberOfRows > 0) and (NumberOfColumns > 0) then
  begin
    SetLength(FDepthRateIndex, NumberOfRows, NumberOfColumns);
    SetLength(FWithdrawals, NumberOfRows, NumberOfColumns);
    SetLength(FActive, NumberOfRows, NumberOfColumns);
    for RowIndex := 0 to NumberOfRows - 1 do
    begin
      for ColIndex := 0 to NumberOfColumns - 1 do
      begin
        FDepthRateIndex[RowIndex,ColIndex] := 0;
        FWithdrawals[RowIndex,ColIndex] := 0;
        FActive[RowIndex,ColIndex] := True;
      end;
    end;
  end;
end;

procedure TFootPrintFile.SetActive(Row, Col: Integer; const Value: Boolean);
begin
  CheckArrayIndicies(Row, Col);
  FActive[Row, Col] := Value;
end;

procedure TFootPrintFile.SetAsciiFileName(const Value: string);
begin
  FAsciiFileName := Value;
end;

procedure TFootPrintFile.SetBinaryFileName(const Value: string);
begin
  FBinaryFileName := Value;
end;

procedure TFootPrintFile.SetCapacity(Row, Col: Integer; const Value: double);
begin
  CheckArrayIndicies(Row, Col);
  FDepthRateIndex[Row, Col] := Value;
end;

procedure TFootPrintFile.SetCellSize(const Value: double);
begin
  FCellSize := Value;
end;

procedure TFootPrintFile.SetClosureCriterion(const Value: double);
begin
  FClosureCriterion := Value;
end;

procedure TFootPrintFile.SetGridAngle(const Value: Double);
begin
  FGridAngle := Value;
end;

procedure TFootPrintFile.SetInitialDistribution(const Value: boolean);
begin
  FInitialDistribution := Value;
end;

procedure TFootPrintFile.SetListingFileName(const Value: string);
begin
  FListingFileName := Value;
end;

procedure TFootPrintFile.SetMaxIterations(const Value: integer);
begin
  FMaxIterations := Value;
end;

procedure TFootPrintFile.SetNumberOfColumns(const Value: Integer);
begin
  if Value <= 0 then
  begin
    raise EFootPrintColumnCountError.Create(
      Format(StrErrorSettingTheNu, ['columns', Value]));
  end;
  if FNumberOfColumns <> Value then
  begin
    FNumberOfColumns := Value;
    InitializeArrays;
  end;
end;

procedure TFootPrintFile.SetNumberOfRows(const Value: Integer);
begin
  if Value <= 0 then
  begin
    raise EFootPrintRowCountError.Create(
      Format(StrErrorSettingTheNu, ['rows', Value]));
  end;
  if FNumberOfRows <> Value then
  begin
    FNumberOfRows := Value;
    InitializeArrays;
  end;
end;

procedure TFootPrintFile.SetOutline(const Value: TPolygon2D);
begin
  FOutline := Value;
  SetLength(FOutline, Length(FOutline));
end;

procedure TFootPrintFile.SetRedistributionCriterion(const Value: Integer);
begin
  FRedistributionCriterion := Value;
end;

procedure TFootPrintFile.SetWithdrawals(Row, Col: Integer; const Value: double);
begin
  CheckArrayIndicies(Row, Col);
  FWithdrawals[Row, Col] := Value;
end;

function TFootPrintFile.FormatTagTerminator(Tag: string): string;
begin
  Result := StrTagTerminatorStart + Tag + StrTagEnd;
end;


function TFootPrintFile.GetActive(Row, Col: Integer): Boolean;
begin
  CheckArrayIndicies(Row, Col);
  result := FActive[Row, Col];
end;

function TFootPrintFile.GetActiveArray: TTwoDBooleanArray;
begin
  result := FActive;
  SetLength(Result, NumberOfRows, NumberOfColumns);
end;

function TFootPrintFile.GetAsciiFileName: string;
begin
  result := FAsciiFileName;
end;

function TFootPrintFile.GetBinaryFileName: string;
begin
  result := FBinaryFileName;
end;

function TFootPrintFile.GetDepthRateIndex(Row, Col: Integer): double;
begin
  CheckArrayIndicies(Row, Col);
  result := FDepthRateIndex[Row, Col];
end;

function TFootPrintFile.GetDepthRateIndexArray: TTwoDRealArray;
begin
  Result := FDepthRateIndex;
  SetLength(Result, NumberOfRows, NumberOfColumns);
end;

function TFootPrintFile.GetListingFileName: string;
begin
  result := FListingFileName;
end;

function TFootPrintFile.GetOutline: TPolygon2D;
begin
  result := FOutline;
  SetLength(result, Length(result));
end;

function TFootPrintFile.GetWithdrawals(Row, Col: Integer): double;
begin
  CheckArrayIndicies(Row, Col);
  result := FWithdrawals[Row, Col];
end;

function TFootPrintFile.GetWithdrawalsArray: TTwoDRealArray;
begin
  Result := FWithdrawals;
  SetLength(Result, NumberOfRows, NumberOfColumns);
end;

procedure TFootPrintFile.WriteTagTerminator(Tag: string);
var
  TagTerminator: string;
begin
  TagTerminator := FormatTagTerminator(Tag);
  FSettingsFileWriter.WriteLine(TagTerminator);
end;

constructor TFootPrintFile.Create;
begin
  FNumberOfRows := -1;
  FNumberOfColumns := -1;
  FClosureCriterion := 0.01;
  FMaxIterations := 10000;
  FInitialDistribution := True;
  FRedistributionCriterion := 1;
  FCellSize := 0;
end;

function TFootPrintFile.FormatTagStart(Tag: string): string;
begin
  Result := StrTagStart + Tag + StrTagEnd;
end;

procedure TFootPrintFile.WriteTagStart(Tag: string);
var
  TagStart: string;
begin
  TagStart := FormatTagStart(Tag);
  FSettingsFileWriter.WriteLine(TagStart);
end;

procedure TFootPrintFile.WriteTaggedInteger(Tag: string; Value: Integer);
begin
  WriteTagStart(Tag);
  FSettingsFileWriter.Write(Indentation);
  FSettingsFileWriter.WriteLine(Value);
  WriteTagTerminator(Tag);
end;

procedure TFootPrintFile.WriteTaggedReal(Tag: string; Value: double);
begin
  WriteTagStart(Tag);
  FSettingsFileWriter.Write(Indentation);
  FSettingsFileWriter.WriteLine(FortranFloatToStr(Value));
  WriteTagTerminator(Tag);
end;

procedure TFootPrintFile.WriteTaggedBooleanArray(Tag: string;
  Value: TTwoDBooleanArray);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  WriteTagStart(Tag);
  FSettingsFileWriter.Write(Indentation);
  WriteTagStart(DirectTag);
  for RowIndex := 0 to Length(Value) - 1 do
  begin
    FSettingsFileWriter.Write(Indentation);
    FSettingsFileWriter.Write(Indentation);
    for ColIndex := 0 to Length(Value[RowIndex]) - 1 do
    begin
      FSettingsFileWriter.Write(Ord(Value[RowIndex,ColIndex]));
      if ColIndex < Length(Value[RowIndex]) - 1 then
      begin
        FSettingsFileWriter.Write(', ');
      end;
    end;
    FSettingsFileWriter.WriteLine;
  end;
  FSettingsFileWriter.Write(Indentation);
  WriteTagTerminator(DirectTag);
  WriteTagTerminator(Tag);
end;

procedure TFootPrintFile.WriteTaggedRealArray(Tag: string;
  Value: TTwoDRealArray);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  WriteTagStart(Tag);
  FSettingsFileWriter.Write(Indentation);
  WriteTagStart(DirectTag);
  for RowIndex := 0 to Length(Value) - 1 do
  begin
    FSettingsFileWriter.Write(Indentation);
    FSettingsFileWriter.Write(Indentation);
    for ColIndex := 0 to Length(Value[RowIndex]) - 1 do
    begin
      FSettingsFileWriter.Write(FortranFloatToStr(Value[RowIndex,ColIndex]));
      if ColIndex < Length(Value[RowIndex]) - 1 then
      begin
        FSettingsFileWriter.Write(', ');
      end;
    end;
    FSettingsFileWriter.WriteLine;
  end;
  FSettingsFileWriter.Write(Indentation);
  WriteTagTerminator(DirectTag);
  WriteTagTerminator(Tag);
end;

procedure TFootPrintFile.WriteTaggedString(Tag, Value: string);
begin
  if Value <> '' then
  begin
    WriteTagStart(Tag);
    FSettingsFileWriter.Write(Indentation);
    FSettingsFileWriter.WriteLine(Value);
    WriteTagTerminator(Tag);
  end;
end;

procedure TFootPrintFile.WriteTaggedBoolean(Tag: string; Value: Boolean);
begin
  WriteTagStart(Tag);
  FSettingsFileWriter.Write(Indentation);
  FSettingsFileWriter.WriteLine(Value);
  WriteTagTerminator(Tag);
end;

end.
