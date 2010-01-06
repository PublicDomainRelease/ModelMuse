{@abstract(@name contains a variety of miscellaneous routines involving
  OpenGL, math, and strings. )}
unit ModelMuseUtilities;

interface

uses
  SysUtils, Graphics, OpenGL12x, GoPhastTypes;

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

function TitleCase(AString: string): string;

// @name converts Value to a string that includes the thousands separator
// if appropriate.
function IntToStrFormatted(Value: integer): string;

implementation

uses ColorSchemes;

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
  OldDecimalSeparator := DecimalSeparator;
  try
    DecimalSeparator := '.';
    result := FloatToStr(Value);
  finally
    DecimalSeparator := OldDecimalSeparator;
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
      if result[Index] = ' ' then
      begin
        result[Index+1] := UpperCase(result[Index+1])[1];
      end;
    end;
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
      result := ThousandSeparator + DigitString + result;
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

end.

