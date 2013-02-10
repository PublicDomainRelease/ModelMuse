{@abstract(@name contains a variety of miscellaneous routines involving
  OpenGL, math, and strings. )}
unit ModelMuseUtilities;

interface

uses
  Windows, SysUtils, Graphics, OpenGL12x, GoPhastTypes;

resourcestring
  StrSorryItLooksLike = 'Sorry. It looks like some other program has locked ' +
  '%s. You will need to close the other program.';

// @abstract(@name gets the red, green, and blue components from a TColor
// in a form suitable for use with OpenGL.)
procedure ExtractColorComponents(const AColor: TColor;
  out Red, Green, Blue: TGLubyte);

// @name extracts the file name without the drive, directory or extension.
function ExtractFileRoot(const FileName: string): string;
{
   @name converts a fraction between 0 and 1 to
   a color using the selected ColorSchemeIndex.

  @param(ColorSchemeIndex
0: @link(FracToSpectrum);

1: @link(FracToGreenMagenta) (reversed);

2: @link(FracToBlueRed) (reversed);

3: @link(FracToBlueDarkOrange) (reversed);

4: @link(FracToBlueGreen) (reversed);

5: @link(FracToBrownBlue) (reversed);

6: @link(FracToBlueGray) (reversed);

7: @link(FracToBlueOrange) (reversed);

8: @link(FracToBlue_OrangeRed) (reversed);

9: @link(FracToLightBlue_DarkBlue) (reversed);

10: @link(ModifiedSpectralScheme) (reversed); )
}

function FracAndSchemeToColor(const ColorSchemeIndex: integer;
  Fraction, ColorAdjustmentFactor: real; const Cycles: integer): TColor;

// @abstract(@name calculates the normal
// of the plane defined by v1, v2, and v3.)
procedure Normal(const v1, v2, v3: T3DRealPoint; out result: T3DRealPoint);

// @abstract(@name subtracts v2 from v1.)
procedure SubtractVectors(const v1, v2: T3DRealPoint; out result: T3DRealPoint);

function FortranFloatToStr(Value: Extended): string;

function FortranStrToFloat(AString: string): Extended;

function TitleCase(AString: string): string;
function AnsiTitleCase(AString: AnsiString): AnsiString;

// @name converts Value to a string that includes the thousands separator
// if appropriate.
function IntToStrFormatted(Value: integer): string;

procedure DSiTrimWorkingSet;

function QuoteFileName(AName: string): string;

function FixShapeFileFieldName(FieldName: AnsiString): AnsiString;

procedure RunAProgram(const CommandLine: string);

function FileLength(fileName : string) : Int64;

function IsWOW64: Boolean;

procedure CantOpenFileMessage(AFileName: string);

implementation

uses ColorSchemes, JvCreateProcess, AnsiStrings, StrUtils, Dialogs, Math;

resourcestring
  StrBadProcessHandle = 'Bad process handle';

function FileLength(fileName : string) : Int64;
 var
   sr : TSearchRec;
 begin
   if FindFirst(fileName, faAnyFile, sr ) = 0 then
   begin
      result := sr.Size;
   end
   else
   begin
      result := -1;
   end;

   FindClose(sr) ;
 end;

procedure RunAProgram(const CommandLine: string);
var
  Runner: TJvCreateProcess;
begin

  Runner := TJvCreateProcess.Create(nil);
  try
    Runner.CommandLine := CommandLine;
    try
      Runner.Run;
    except on E: EOSError do
      begin
        Beep;
        MessageDlg(E.message, mtError, [mbOK], 0);
      end;
    end;
  finally
    Runner.Free;
  end;
end;

procedure ExtractColorComponents(const AColor: TColor;
  out Red, Green, Blue: TGLubyte);
var
  Value: integer;
  v: longword;
begin
  Value := ColorToRGB(AColor);
  Assert(Value >= 0);
  v := Value;
  Red := (v shl 24) shr 24;
  v := v shr 8;
  Green := (v shl 24) shr 24;
  v := v shr 8;
  Blue := (v shl 24) shr 24;
end;

procedure CrossProduct(const v1, v2: T3DRealPoint; out result: T3DRealPoint);
begin
  result.X := v1.Y * v2.Z - v1.Z * v2.Y;
  result.Y := v1.Z * v2.X - v1.X * v2.Z;
  result.Z := v1.X * v2.Y - v1.Y * v2.X;
end;

procedure SubtractVectors(const v1, v2: T3DRealPoint; out result: T3DRealPoint);
begin
  result.X := v1.X - v2.X;
  result.Y := v1.Y - v2.Y;
  result.Z := v1.Z - v2.Z;
end;

procedure Normal(const v1, v2, v3: T3DRealPoint; out result: T3DRealPoint);
var
  Diff1, Diff2: T3DRealPoint;
  d: double;
begin
  SubtractVectors(v1, v2, Diff1);
  SubtractVectors(v2, v3, Diff2);
  CrossProduct(Diff1, Diff2, result);
  d := Sqrt(sqr(result.X) + sqr(result.Y) + sqr(result.Z));

  result.X := result.X / d;
  result.Y := result.Y / d;
  result.Z := result.Z / d;
end;

function FracAndSchemeToColor(const ColorSchemeIndex: integer;
  Fraction, ColorAdjustmentFactor: real; const Cycles: integer): TColor;
begin
  if Fraction <> 1 then
  begin
    Fraction := Frac(Fraction*Cycles);
  end;
  case ColorSchemeIndex of
    0: result := FracToSpectrum(Fraction, ColorAdjustmentFactor);
    1: result := FracToGreenMagenta(1 - Fraction, ColorAdjustmentFactor);
    2: result := FracToBlueRed(1 - Fraction, ColorAdjustmentFactor);
    3: result := FracToBlueDarkOrange(1 - Fraction, ColorAdjustmentFactor);
    4: result := FracToBlueGreen(1 - Fraction, ColorAdjustmentFactor);
    5: result := FracToBrownBlue(1 - Fraction, ColorAdjustmentFactor);
    6: result := FracToBlueGray(1 - Fraction, ColorAdjustmentFactor);
    7: result := FracToBlueOrange(1 - Fraction, ColorAdjustmentFactor);
    8: result := FracToBlue_OrangeRed(1 - Fraction, ColorAdjustmentFactor);
    9: result := FracToLightBlue_DarkBlue(1 - Fraction, ColorAdjustmentFactor);
    10: result := ModifiedSpectralScheme(1 - Fraction, ColorAdjustmentFactor);
    11: result := SteppedSequential(1 - Fraction, ColorAdjustmentFactor);
  else
    result := clWhite;
    Assert(False);
  end;
end;

function ExtractFileRoot(const FileName: string): string;
var
  DotPos: integer;
begin
  result := ExtractFileName(FileName);
  DotPos := Pos('.', result);
  If DotPos > 0 then
  begin
    result := Copy(result, 1, DotPos-1);
  end;
end;

function FortranFloatToStr(Value: Extended): string;
var
  OldDecimalSeparator: Char;
begin
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    result := FloatToStr(Value);
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;

function TitleCase(AString: string): string;
var
  Index: integer;
begin
  result := LowerCase(AString);
  if Length(result) > 0 then
  begin
    Result[1] := UpperCase(Result)[1];
    for Index := 1 to Length(result) - 1 do
    begin
      if (result[Index] = ' ') then
      begin
        result[Index+1] := UpperCase(result[Index+1])[1];
      end;
    end;
  end;
end;

function AnsiTitleCase(AString: AnsiString): AnsiString;
var
  Index: integer;
begin
  result := AnsiLowerCase(AString);
  if Length(result) > 0 then
  begin
    Result[1] := AnsiUpperCase(Result)[1];
    for Index := 1 to Length(result) - 1 do
    begin
      if result[Index] = ' ' then
      begin
        result[Index+1] := AnsiUpperCase(result[Index+1])[1];
      end;
    end;
  end;
end;

function FortranStrToFloat(AString: string): Extended;
var
  OldDecimalSeparator: Char;
  SignPos: Integer;
begin
  AString := Trim(AString);
  OldDecimalSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    AString := StringReplace(AString, ',', '.', [rfReplaceAll, rfIgnoreCase]);
    AString := StringReplace(AString, 'd', 'e', [rfReplaceAll, rfIgnoreCase]);
    SignPos := Max(PosEx('+', AString, 2), PosEx('-', AString, 2));
    if SignPos > 0 then
    begin
      if not CharInSet(AString[SignPos-1], ['e', 'E']) then
      begin
        Insert('E', AString, SignPos);
      end;
    end;
    result := StrToFloat(AString);
  finally
    FormatSettings.DecimalSeparator := OldDecimalSeparator;
  end;
end;


function IntToStrFormatted(Value: integer): string;
var
  Negative: Boolean;
  Digits: Integer;
  DigitString: string;
begin
  if Abs(Value) < 10000 then
  begin
    result := IntToStr(Value);
    Exit;
  end;
  Negative := Value < 0;
  result := '';
  Value := Abs(Value);
  while Value > 0 do
  begin
    Digits := Value mod 1000;
    Value := Value div 1000;
    if Value > 0 then
    begin
      DigitString := Format('%.3d', [Digits]);
      result := FormatSettings.ThousandSeparator + DigitString + result;
    end
    else
    begin
      DigitString := Format('%d', [Digits]);
      result := DigitString + result;
    end;
  end;
  if Negative then
  begin
    result := '-' + result;
  end;
end;

// http://stackoverflow.com/questions/2031577/can-memory-be-cleaned-up/2033393#2033393
procedure DSiTrimWorkingSet;
var
  hProcess: THandle;
begin
//  Exit;
  hProcess := OpenProcess(PROCESS_SET_QUOTA, false, GetCurrentProcessId);
  try
    SetProcessWorkingSetSize(hProcess, $FFFFFFFF, $FFFFFFFF);
  finally CloseHandle(hProcess); end;
end;

function QuoteFileName(AName: string): string;
begin
  if (Length(AName) > 0)
    and (AName[1] <> '"')
    and (Pos(' ', AName) > 0) then
  begin
    result := '"' + AName + '"';
  end
  else
  begin
    result := AName;
  end;
end;

function FixShapeFileFieldName(FieldName: AnsiString): AnsiString;
begin
  While (Length(FieldName) > 0) and (FieldName[Length(FieldName)] = '_') do
  begin
    SetLength(FieldName, Length(FieldName) -1);
  end;
  result := FieldName;
end;

function IsWOW64: Boolean;
// from http://www.delphipages.com/forum/showthread.php?t=206540
type
  TIsWow64Process = function(
    Handle: THandle;
    var Res: BOOL
  ): BOOL; stdcall;
var
  IsWow64Result: BOOL;
  IsWow64Process: TIsWow64Process;
begin
  IsWow64Process := GetProcAddress(
    GetModuleHandle('kernel32'), 'IsWow64Process'
  );
  if Assigned(IsWow64Process) then
  begin
    if not IsWow64Process(GetCurrentProcess, IsWow64Result) then
      raise Exception.Create(StrBadProcessHandle);
    Result := IsWow64Result;
  end
  else
    Result := False;
end;

procedure CantOpenFileMessage(AFileName: string);
begin
  Beep;
  MessageDlg(Format(StrSorryItLooksLike, [AFileName]), mtError, [mbOK], 0);
end;

end.

