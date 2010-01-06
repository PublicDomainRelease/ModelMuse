// @abstract(@name registers @link(TRbwRuler) which draws a ruler.)
unit RbwRuler;

interface

uses
  Types, Windows, SysUtils, Classes, Controls, ExtCtrls;

type
  // See TRbwRuler.@link(TRbwRuler.RulerPosition).
  TRulerPosition = (rpLeft, rpBottom, rpRight, rpTop);
  // See TRbwRuler.@link(TRbwRuler.RulerStart).
  TStart = (sTopLeft, sBottomRight);
  // See TRbwRuler.@link(TRbwRuler.RulerTextPosition).
  TTextPosition = (tpOutside, tpInside);
  TOrientation = (orVertical, orHorizontal);

  TRbwRuler = class;

  {@abstract(See TRbwRuler.@link(TRbwRuler.RulerEnds).)}
  TRulerPositions = class(TPersistent)
  private
    // See @link(Upper).
    FHigh: integer;
    // See @link(Lower).
    FLow: integer;
    // @name is the @link(TRbwRuler) that owns and uses the @classname.
    FOwner: TRbwRuler;
    // See @link(Upper).
    procedure SetHigh(const Value: integer);
    // See @link(Lower).
    procedure SetLow(const Value: integer);
  public
    // If Source is a @classname, @name copies its
    // values of @link(Lower) and @link(Upper).
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create(Owner: TRbwRuler);
  published
    // @name is the position in pixels of the lower end or the ruler.
    property Lower: integer read FLow write SetLow;
    // @name is the position in pixels of the upper end or the ruler.
    property Upper: integer read FHigh write SetHigh;
  end;

  {@abstract(See TRbwRuler.@link(TRbwRuler.RulerValues).)}
  TRulerValues = class(TPersistent)
  private
    // See @link(Upper).
    FHigh: double;
    // See @link(Lower).
    FLow: double;
    // @name is the @link(TRbwRuler) that owns and uses the @classname.
    FOwner: TRbwRuler;
    // See @link(Upper).
    procedure SetHigh(const Value: double);
    // See @link(Lower).
    procedure SetLow(const Value: double);
  public
    // If Source is a @classname, @name copies its
    // values of @link(Lower) and @link(Upper).
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create(Owner: TRbwRuler);
  published
    // @name represents the value at the lower end of the ruler.
    property Lower: double read FLow write SetLow;
    // @name represents the value at the upper end of the ruler.
    property Upper: double read FHigh write SetHigh;
  end;

  // @abstract(@name draws a ruler.)  The orientation,
  // scaling, number format, and position of the ruler
  // can be controlled through properties.  Rotated text will only
  // work if a TrueType font is used.
  TRbwRuler = class(TPaintBox)
  private
    // See @link(RulerDesiredSpacing).
    FDesiredSpacing: integer;
    // See @link(RulerDigits).
    FDigits: integer;
    // See @link(RulerLinePosition).
    FLinePosition: integer;
    // See @link(RulerMajorTickLength).
    FMajorTickLength: integer;
    // See @link(RulerMinorTickLength).
    FMinorTickLength: integer;
    // See @link(RulerEnds).
    FPositions: TRulerPositions;
    // See @link(RulerPrecision).
    FPrecision: integer;
    // See @link(RulerPosition).
    FRulerPosition: TRulerPosition;
    // See @link(RulerStart).
    FStart: TStart;
    // See @link(RulerTextOffset).
    FTextOffset: integer;
    // See @link(RulerTextPosition).
    FTextPosition: TTextPosition;
    // See @link(RulerValues).
    FValues: TRulerValues;
    // @name draws the numbers on the ruler.
    procedure DrawLabel(const CurPositionR, Increment: double;
      const CurPositionI: integer; var FirstLabel: boolean;
      var LastLabelLT, LastLabelRB: integer; out LabelDrawn: boolean);
    // @name draws the main line of the ruler.
    procedure DrawMainLine;
    // @name draws minor tick lines on the ruler
    procedure DrawMinorTicks(const LowCoord, Increment: double;
      const Index, Spacing: integer; DrawIntermediates: boolean);
    // @name draws a tick line
    procedure DrawTick(const CurPositionI: integer;
      const IsMajorTick: boolean);
    // @name draws the ruler.
    procedure DrawRuler;
    // Get the pixel equivalent of the current number.
    function GetCurrentPosition(const CurPositionR: double): integer;
    // @name gets the length (which may be negative) of the
    // major ticks during drawing.
    function GetDrawingMajorTickLength: integer;
    // @name gets the length (which may be negative) of the
    // minor ticks during drawing.
    function GetDrawingMinorTickLength: integer;
    // @name gets the X or Y coordinate where the main line of the
    // ruler should be drawn.
    function GetLineDrawingPosition: integer;
    // See @link(RulerOrientation).
    function GetOrientation: TOrientation;
    // @name determines where the labels are drawn with respect
    // to the main line of the ruler.
    function GetTextDrawingPosition: TStart;
    // @name get the spacing between tick marks and Factor.
    procedure GetTickSpacingAndFactor(
      out Spacing: integer; out Factor: double);
    // @name is the ratio between the length that the ruler represents
    // and its length in pixels.
    function Multiplier: double;
    // @name converts Value to a string using @link(RulerPrecision) and
    // @Link(RulerDigits).  However, if Value is
    // within Increment/RoundFactor of 0, @name returns '0'.
    function RoundNumber(Value, Increment: double): string;
    // See @link(RulerDesiredSpacing).
    procedure SetDesiredSpacing(const Value: integer);
    // See @link(RulerDigits).
    procedure SetDigits(Value: integer);
    // See @link(RulerLinePosition).
    procedure SetLinePosition(const Value: integer);
    // See @link(RulerMajorTickLength).
    procedure SetMajorTickLength(const Value: integer);
    // See @link(RulerMinorTickLength).
    procedure SetMinorTickLength(const Value: integer);
    // See @link(RulerEnds).
    procedure SetPositions(const Value: TRulerPositions);
    // See @link(RulerPrecision).
    procedure SetPrecision(Value: integer);
    // See @link(RulerPosition).
    procedure SetRulerPosition(const Value: TRulerPosition);
    // See @link(RulerStart).
    procedure SetStart(const Value: TStart);
    // See @link(RulerTextOffset).
    procedure SetTextOffset(const Value: integer);
    // See @link(RulerTextPosition).
    procedure SetTextPosition(const Value: TTextPosition);
    // See @link(RulerValues).
    procedure SetValues(const Value: TRulerValues);
    { Private declarations }
  protected
    // @name calls inherited @name and then draws the ruler.
    procedure Paint; override;
    // @name controls whether the ruler is drawn in a horizontal
    // or vertical orientation. @name is determined by @link(RulerPosition).
    property RulerOrientation: TOrientation read GetOrientation;
    { Protected declarations }
  public
    // @name creates and instance of @classname
    constructor Create(AOwner: TComponent); override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    { @name draws Text at the position X,Y at the angle specified by Angle.
      X and Y are in pixels (despite being real numbers).  Angle is measured
      in degrees in a clockwise direction.  The font must be a TrueType
      Font for @name to work properly.}
    procedure PaintRotated(const X, Y, Angle: double; const Text: string);
      virtual;
    { Public declarations }
  published
    // @name is the desired spacing between major ticks in
    // pixels.
    // The actual spacing will, in most cases, be slightly different from
    // @name.
    property RulerDesiredSpacing: integer read FDesiredSpacing
      write SetDesiredSpacing;
    // @name specifies how many digits will
    // appear in the exponent portion of numbers on the ruler.
    property RulerDigits: integer read FDigits write SetDigits;
    // @name specifies the positions, in pixels,
    // of the starting and ending
    // ends of the main line of the ruler.
    property RulerEnds: TRulerPositions read FPositions write SetPositions;
    // @name specifies the distance of the main line of the ruler
    // from the left, top, right, or bottom edge in pixels.
    // Which one is determined by RulerPosition.
    property RulerLinePosition: integer read FLinePosition
      write SetLinePosition;
    // @name is the length of the major ticks in pixels.
    property RulerMajorTickLength: integer read FMajorTickLength
      write SetMajorTickLength;
    // @name is the length of the minor ticks in pixels.
    property RulerMinorTickLength: integer read FMinorTickLength
      write SetMinorTickLength;
    // @name specifies the orientation of the main line on the
    // ruler. @name controls @link(RulerOrientation).
    property RulerPosition: TRulerPosition read FRulerPosition
      write SetRulerPosition;
    // @name specifies how many digits appear in the
    // number in the ruler.
    property RulerPrecision: integer read FPrecision write SetPrecision;
    // @name specifies the end of the ruler
    // that has the lower value of the ruler.
    property RulerStart: TStart read FStart write SetStart;
    // @name specifies the distance from the main line of the
    // ruler to the labels on the ruler.
    property RulerTextOffset: integer read FTextOffset write SetTextOffset;
    // @name specifies whether the labels are inside or outside
    // the ruler.
    property RulerTextPosition: TTextPosition read FTextPosition
      write SetTextPosition;
    // @name specifies the values of the lower and upper end of the
    // ruler line. Thus the ruler represents a length equal to
    // TRulerValues.@link(TRulerValues.Upper) minus
    // TRulerValues.@link(TRulerValues.Lower)
    property RulerValues: TRulerValues read FValues write SetValues;
    { Published declarations }
  end;

  // @name registers @link(TRbwRuler).
procedure Register;

implementation

uses Math, Graphics;

const
  Epsilon = 1E-8;
  RoundFactor = 200;
  Ten = 10;

function Power10(const Value: double): double;
begin
  result := Power(Ten, Value); //exp(ln10*Value);
end;

procedure Register;
begin
  RegisterComponents('RBW', [TRbwRuler]);
end;

procedure GetRoundMinMax(var LowRange, HighRange: double; out Increment: double;
  const factor: double);
var
  Range: double;
  Multiplier: double;
begin
  if LowRange = HighRange then
    Exit;
  Range := HighRange - LowRange;
  Increment := Range / factor;
  if Range < 0 then
  begin
    Increment := Range;
    Exit;
  end;
  Increment := Power10(Int(Log10(Increment)));
  Multiplier := (LowRange / Increment);
  if (Multiplier <> Int(Multiplier)) then
  begin
    if LowRange > 0 then
    begin
      Multiplier := Int(Multiplier) + 1;
    end
    else
    begin
      Multiplier := Int(Multiplier);
    end;

  end;
  LowRange := Increment * Multiplier;
  Multiplier := Int(HighRange / Increment + Increment / 1E8);
  if HighRange < 0 then
  begin
    Multiplier := Multiplier - 1;
  end;
  HighRange := Increment * Multiplier;
end;

{ TRbwRuler }

constructor TRbwRuler.Create(AOwner: TComponent);
begin
  inherited;
  FDesiredSpacing := 100;
  RulerMinorTickLength := 10;
  RulerMajorTickLength := 20;
  FLinePosition := 40;
  FTextOffset := 5;
  FPrecision := 5;
  FDigits := 1;
  FTextPosition := tpOutside;
  FPositions := TRulerPositions.Create(self);
  FValues := TRulerValues.Create(self);
end;

destructor TRbwRuler.Destroy;
begin
  FPositions.Free;
  FValues.Free;
  inherited;
end;

function TRbwRuler.GetOrientation: TOrientation;
begin
  if (FRulerPosition = rpLeft) or (FRulerPosition = rpRight) then
  begin
    result := orVertical;
  end
  else
  begin
    result := orHorizontal;
  end;
end;

procedure TRbwRuler.Paint;
var
  AStyle: TPenStyle;
begin
  inherited;
  if (RulerEnds.Lower = RulerEnds.Upper)
    or (RulerValues.Lower = RulerValues.Upper) then
  begin
    Exit;
  end;
  AStyle := Canvas.Pen.Style;
  try
    Canvas.Pen.Style := psSolid;
    DrawRuler;
  finally
    Canvas.Pen.Style := AStyle
  end;
end;

function TRbwRuler.GetTextDrawingPosition: TStart;
begin
  if (RulerPosition = rpBottom) or (RulerPosition = rpRight) then
  begin
    if RulerTextPosition = tpOutside then
    begin
      result := sBottomRight;
    end
    else
    begin
      result := sTopLeft;
    end;
  end
  else
  begin
    if RulerTextPosition = tpOutside then
    begin
      result := sTopLeft;
    end
    else
    begin
      result := sBottomRight;
    end;
  end;
end;

function TRbwRuler.GetLineDrawingPosition: integer;
begin
  result := RulerLinePosition;
  if (RulerPosition = rpBottom) or (RulerPosition = rpRight) then
  begin
    if RulerPosition = rpBottom then
    begin
      result := Height - result;
    end
    else
    begin
      result := Width - result;
    end;
  end;
end;

function TRbwRuler.GetDrawingMajorTickLength: integer;
begin
  result := RulerMajorTickLength;
  if (RulerPosition = rpBottom) or (RulerPosition = rpRight) then
  begin
    result := -result;
  end;
end;

function TRbwRuler.GetDrawingMinorTickLength: integer;
begin
  result := RulerMinorTickLength;
  if (RulerPosition = rpBottom) or (RulerPosition = rpRight) then
  begin
    result := -result;
  end;
end;

procedure TRbwRuler.DrawMainLine;
var
  localLinePosition: integer;
begin
  localLinePosition := GetLineDrawingPosition;
  if RulerOrientation = orHorizontal then
  begin
    Canvas.MoveTo(RulerEnds.Lower, localLinePosition);
    Canvas.LineTo(RulerEnds.Upper, localLinePosition);
  end
  else
  begin
    Canvas.MoveTo(localLinePosition, RulerEnds.Lower);
    Canvas.LineTo(localLinePosition, RulerEnds.Upper);
  end;
end;

procedure TRbwRuler.GetTickSpacingAndFactor(
  out Spacing: integer; out Factor: double);
var
  LowCoord, HighCoord: double;
  Increment: double;
begin
  Factor := Ten;
  LowCoord := RulerValues.Lower;
  HighCoord := RulerValues.Upper;
  GetRoundMinMax(LowCoord, HighCoord, Increment, factor);
  Spacing := Round(Increment / Multiplier);
  while Spacing > RulerDesiredSpacing do
  begin
    Factor := Factor * Ten;
    LowCoord := RulerValues.Lower;
    HighCoord := RulerValues.Upper;
    GetRoundMinMax(LowCoord, HighCoord, Increment, factor);
    Spacing := Round(Increment / Multiplier);
  end;
end;

procedure TRbwRuler.DrawTick(const CurPositionI: integer;
  const IsMajorTick: boolean);
var
  localLinePosition: integer;
begin
  localLinePosition := GetLineDrawingPosition;
  if RulerOrientation = orHorizontal then
  begin
    Canvas.MoveTo(CurPositionI, localLinePosition);
    if IsMajorTick then
    begin
      Canvas.LineTo(CurPositionI, localLinePosition +
        GetDrawingMajorTickLength);
    end
    else
    begin
      Canvas.LineTo(CurPositionI, localLinePosition +
        GetDrawingMinorTickLength);
    end;
  end
  else
  begin
    Canvas.MoveTo(localLinePosition, CurPositionI);
    if IsMajorTick then
    begin
      Canvas.LineTo(localLinePosition + GetDrawingMajorTickLength,
        CurPositionI);
    end
    else
    begin
      Canvas.LineTo(localLinePosition + GetDrawingMinorTickLength,
        CurPositionI);
    end;
  end;
end;

procedure TRbwRuler.DrawMinorTicks(const LowCoord, Increment: double;
  const Index, Spacing: integer; DrawIntermediates: boolean);
var
  MinorTickIndex: integer;
  CurPositionR: double;
  CurPositionI: integer;
begin
  if DrawIntermediates and (Spacing * Ten >= RulerDesiredSpacing) then
  begin
    for MinorTickIndex := 1 to 9 do
    begin
      CurPositionR := LowCoord + Increment * Index
        + Increment / Ten * MinorTickIndex;
      if (CurPositionR > RulerValues.Lower - Increment / 20)
        and (CurPositionR < RulerValues.Upper + Increment / 20) then
      begin
        if RulerStart = sTopLeft then
        begin
          CurPositionI := RulerEnds.Lower + Round((CurPositionR -
            RulerValues.Lower) / Multiplier);
        end
        else
        begin
          CurPositionI := RulerEnds.Upper - Round((CurPositionR -
            RulerValues.Lower) / Multiplier);
        end;
        if (CurPositionI >= RulerEnds.Lower) and (CurPositionI <=
          RulerEnds.Upper) then
        begin
          DrawTick(CurPositionI, (MinorTickIndex = 5));
        end;
      end;
    end;
  end
  else if Spacing * Ten >= RulerDesiredSpacing then
  begin
    CurPositionR := LowCoord + Increment * Index
      + Increment / 2;
    if (CurPositionR > RulerValues.Lower - Increment / 20)
      and (CurPositionR < RulerValues.Upper + Increment / 20) then
    begin
      if RulerStart = sTopLeft then
      begin
        CurPositionI := RulerEnds.Lower + Round((CurPositionR -
          RulerValues.Lower) / Multiplier);
      end
      else
      begin
        CurPositionI := RulerEnds.Upper - Round((CurPositionR -
          RulerValues.Lower) / Multiplier);
      end;
      if (CurPositionI >= RulerEnds.Lower) and (CurPositionI <=
        RulerEnds.Upper) then
      begin
        DrawTick(CurPositionI, DrawIntermediates);
      end;
    end;
  end;
end;

procedure TRbwRuler.DrawLabel(const CurPositionR, Increment: double;
  const CurPositionI: integer; var FirstLabel: boolean;
  var LastLabelLT, LastLabelRB: integer; out LabelDrawn: boolean);
var
  LabelString: string;
  TextSize: TSize;
  LabelWH, Other: integer;
  LabelPosition: integer;
  ThisLabelLT, ThisLabelRB: integer;
  LocalTextPosition: TStart;
  localLinePosition: integer;
  BufferSize: integer;
begin
  LabelDrawn := False;
  LocalTextPosition := GetTextDrawingPosition;
  localLinePosition := GetLineDrawingPosition;
  LabelString := RoundNumber(CurPositionR, Increment);
  BufferSize := Canvas.TextWidth('0') div 2;

  TextSize := Canvas.TextExtent(LabelString);

  LabelWH := TextSize.cx;
  Other := TextSize.cy;

  if LocalTextPosition = sBottomRight then
  begin
    LabelPosition := Max(localLinePosition + RulerTextOffset,
      localLinePosition);
  end
  else
  begin
    LabelPosition := Min(localLinePosition - RulerTextOffset - Other,
      localLinePosition);
  end;

  ThisLabelLT := CurPositionI - LabelWH div 2;
  ThisLabelRB := ThisLabelLT + LabelWH;

  if FirstLabel or (ThisLabelRB + BufferSize < LastLabelLT)
    or (ThisLabelLT - BufferSize > LastLabelRB) then
  begin
    if RulerOrientation = orHorizontal then
    begin
      if (ThisLabelLT >= 0) and (ThisLabelRB < Width) then
      begin
        Canvas.TextOut(CurPositionI - LabelWH div 2,
          LabelPosition, LabelString);
        FirstLabel := False;
        LastLabelLT := ThisLabelLT;
        LastLabelRB := ThisLabelRB;
        LabelDrawn := True;
      end;
    end
    else
    begin
      if (ThisLabelLT >= 0) and (ThisLabelRB < Height) then
      begin
        PaintRotated(LabelPosition, CurPositionI + LabelWH / 2,
          -90, LabelString);
        FirstLabel := False;
        LastLabelLT := ThisLabelLT;
        LastLabelRB := ThisLabelRB;
        LabelDrawn := True;
      end;
    end;
  end;
end;

function TRbwRuler.GetCurrentPosition(const CurPositionR: double): integer;
begin
  if RulerStart = sTopLeft then
  begin
    result := RulerEnds.Lower + Round((CurPositionR - RulerValues.Lower) /
      Multiplier);
  end
  else
  begin
    result := RulerEnds.Upper - Round((CurPositionR - RulerValues.Lower) /
      Multiplier);
  end;
end;

function TRbwRuler.Multiplier: double;
begin
  Multiplier := (RulerValues.Upper - RulerValues.Lower)
    / (RulerEnds.Upper - RulerEnds.Lower);
end;

procedure TRbwRuler.DrawRuler;
var
  LowCoord, HighCoord: double;
  Increment: double;
  Index: integer;
  CurPositionR: double;
  CurPositionI: integer;
  factor: double;
  Spacing: integer;
  LastLabelLT, LastLabelRB: integer;
  FirstLabel: boolean;
  LabelDrawn: boolean;
  DrawIntermediates: boolean;
begin

  DrawMainLine;

  //Determine Tick Spacing
  GetTickSpacingAndFactor(Spacing, Factor);

  // Get the Increment.
  factor := factor / Ten;
  LowCoord := RulerValues.Lower;
  HighCoord := RulerValues.Upper;
  GetRoundMinMax(LowCoord, HighCoord, Increment, factor);

  // initialize variables.
  Index := 0;
  CurPositionR := LowCoord;
  FirstLabel := True;
  LastLabelLT := 0;
  LastLabelRB := 0;
  DrawIntermediates := True;
  while CurPositionR < RulerValues.Upper + Increment / 2 do
  begin
    CurPositionI := GetCurrentPosition(CurPositionR);
    // If the major tick is inside the ends of the ruler, draw and label it.
    if (CurPositionI >= RulerEnds.Lower) and (CurPositionI <= RulerEnds.Upper)
      then
    begin
      DrawLabel(CurPositionR, Increment, CurPositionI, FirstLabel,
        LastLabelLT, LastLabelRB, LabelDrawn);

      DrawIntermediates := DrawIntermediates and LabelDrawn;

      DrawTick(CurPositionI, LabelDrawn);
    end;

    // update variables
    Inc(Index);
    CurPositionR := LowCoord + Increment * Index;
  end;

  // initialize variables.
  Index := -1;
  CurPositionR := LowCoord;
  FirstLabel := True;
  LastLabelLT := 0;
  LastLabelRB := 0;
  while CurPositionR < RulerValues.Upper + Increment / 2 do
  begin
    DrawMinorTicks(LowCoord, Increment, Index, Spacing, DrawIntermediates);

    // update variables
    Inc(Index);
    CurPositionR := LowCoord + Increment * Index;
  end;
end;

procedure TRbwRuler.PaintRotated(const X, Y, Angle: double;
  const Text: string);
var
  LogFont: TLogFont;
  AFont: TFont;
begin
  AFont := TFont.Create;
  try
    AFont.Assign(Canvas.Font);

    GetObject(AFont.Handle, SizeOf(TLogFont), @LogFont);
    LogFont.lfEscapement := Round(-Angle * 10);
    LogFont.lfOrientation := Round(-Angle * 10);
    AFont.Handle := CreateFontIndirect(LogFont);
    Canvas.Font.Assign(AFont);
    Canvas.TextOut(Round(X), Round(Y), Text);
    DeleteObject(AFont.Handle);
  finally
    AFont.Free;
  end;
end;


{  Canvas.Start;
  try
    QPainter_save(Canvas.Handle);
    QPainter_translate(Canvas.Handle, X, Y);
    QPainter_rotate(Canvas.Handle, Angle);
    Canvas.TextOut(0, 0, Text);
  finally
    QPainter_restore(Canvas.Handle);
    Canvas.Stop;
  end;   }

function TRbwRuler.RoundNumber(Value, Increment: double): string;
begin
  if (Value + Increment / RoundFactor > 0)
    and (Value - Increment / RoundFactor < 0) then
  begin
    result := '0';
  end
  else
  begin
    result := FloatToStrF(Value, ffNumber, RulerPrecision, RulerDigits);
  end;
end;

procedure TRbwRuler.SetDesiredSpacing(const Value: integer);
begin
  if FDesiredSpacing <> Value then
  begin
    FDesiredSpacing := Value;
    Invalidate;
  end;
end;

procedure TRbwRuler.SetLinePosition(const Value: integer);
begin
  if FLinePosition <> Value then
  begin
    FLinePosition := Value;
    Invalidate;
  end;
end;

procedure TRbwRuler.SetMajorTickLength(const Value: integer);
begin
  if FMajorTickLength <> Value then
  begin
    FMajorTickLength := Value;
    Invalidate;
  end;
end;

procedure TRbwRuler.SetMinorTickLength(const Value: integer);
begin
  if FMinorTickLength <> Value then
  begin
    FMinorTickLength := Value;
    Invalidate;
  end;
end;

procedure TRbwRuler.SetPositions(const Value: TRulerPositions);
begin
  FPositions.Assign(Value);
  Invalidate;
end;

procedure TRbwRuler.SetRulerPosition(const Value: TRulerPosition);
begin
  if FRulerPosition <> Value then
  begin
    FRulerPosition := Value;
    Invalidate;
  end;
end;

procedure TRbwRuler.SetStart(const Value: TStart);
begin
  if FStart <> Value then
  begin
    FStart := Value;
    Invalidate;
  end;
end;

procedure TRbwRuler.SetTextOffset(const Value: integer);
begin
  if FTextOffset <> Value then
  begin
    FTextOffset := Value;
    Invalidate;
  end;
end;

procedure TRbwRuler.SetTextPosition(const Value: TTextPosition);
begin
  if FTextPosition <> Value then
  begin
    FTextPosition := Value;
    Invalidate;
  end;
end;

procedure TRbwRuler.SetValues(const Value: TRulerValues);
begin
  FValues.Assign(Value);
  Invalidate;
end;

procedure TRbwRuler.SetDigits(Value: integer);
begin
  if Value < 0 then
  begin
    Value := 0;
  end
  else if Value > 4 then
  begin
    Value := 4;
  end;
  if Value <> FDigits then
  begin
    FDigits := Value;
    Invalidate;
  end;
end;

procedure TRbwRuler.SetPrecision(Value: integer);
begin
  if Value < 1 then
  begin
    Value := 1;
  end
  else if Value > 15 then
  begin
    Value := 15;
  end;
  if Value <> FPrecision then
  begin
    FPrecision := Value;
    Invalidate;
  end;
end;

{ TRulerPosition }

procedure TRulerPositions.Assign(Source: TPersistent);
begin
  if Source is TRulerPositions then
  begin
    Lower := TRulerPositions(Source).Lower;
    Upper := TRulerPositions(Source).Upper;
  end
  else
  begin
    inherited;
  end;
end;

{ TRulerValues }

procedure TRulerValues.Assign(Source: TPersistent);
begin
  if Source is TRulerValues then
  begin
    Lower := TRulerValues(Source).Lower;
    Upper := TRulerValues(Source).Upper;
  end
  else
  begin
    inherited;
  end;
end;

constructor TRulerValues.Create(Owner: TRbwRuler);
begin
  inherited Create;
  FOwner := Owner;
  FHigh := 100;
end;

procedure TRulerValues.SetHigh(const Value: double);
begin
  if FHigh <> Value then
  begin
    FHigh := Value;
    FOwner.Invalidate;
  end;
end;

procedure TRulerValues.SetLow(const Value: double);
begin
  if FLow <> Value then
  begin
    FLow := Value;
    FOwner.Invalidate;
  end;
end;

constructor TRulerPositions.Create(Owner: TRbwRuler);
begin
  inherited Create;
  FOwner := Owner;
  FHigh := 100;
end;

procedure TRulerPositions.SetHigh(const Value: integer);
begin
  if FHigh <> Value then
  begin
    FHigh := Value;
    if (FOwner <> nil) and (csDesigning in FOwner.ComponentState) and (FLow >
      FHigh) then
    begin
      FLow := FHigh - 1;
    end;
    FOwner.Invalidate;
  end;
end;

procedure TRulerPositions.SetLow(const Value: integer);
begin
  if FLow <> Value then
  begin
    FLow := Value;
    if (FOwner <> nil) and (csDesigning in FOwner.ComponentState)
      and (FLow > FHigh) then
    begin
      FHigh := FLow + 1;
    end;
    FOwner.Invalidate;
  end;
end;

end.

