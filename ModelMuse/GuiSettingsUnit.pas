unit GuiSettingsUnit;

interface

uses Classes, Forms;
            
type
  TGuiSettings = class(TPersistent)
  private
    procedure SetWindowState(Value : TWindowState);
    function GetWindowState: TWindowState;
    procedure SetWidth(Value : integer);
    function GetWidth: integer;
    procedure SetTopY(Value : double);
    function GetTopY: double;
    procedure SetTopX(Value : double);
    function GetTopX: double;
    procedure SetTopViewWidth(Value : integer);
    function GetTopViewWidth: integer;
    procedure SetTopViewHeight(Value : integer);
    function GetTopViewHeight: integer;
    procedure SetTop(Value : integer);
    function GetTop: integer;
    procedure SetSideY(Value : double);
    function GetSideY: double;
    procedure SetSideX(Value : double);
    function GetSideX: double;
    procedure SetSideWidth(Value : integer);
    function GetSideWidth: integer;
    procedure SetMagnificationTop(Value : double);
    function GetMagnificationTop: double;
    procedure SetMagnificationSide(Value : double);
    function GetMagnificationSide: double;
    procedure SetMagnificationFront(Value : double);
    function GetMagnificationFront: double;
    procedure SetLeft(Value : integer);
    function GetLeft: integer;
    procedure SetFrontY(Value : double);
    function GetFrontY: double;
    procedure SetFrontX(Value : double);
    function GetFrontX: double;
    procedure SetHeight(Value : integer);
    function GetHeight: integer;
    procedure SetFrontHeight(Value : integer);
    function GetFrontHeight: integer;
    function GetFrontHorizontalDigits: integer;
    function GetFrontHorizontalPrecision: integer;
    function GetFrontVerticalDigits: integer;
    function GetFrontVerticalPrecision: integer;
    function GetSideHorizontalDigits: integer;
    function GetSideHorizontalPrecision: integer;
    function GetSideVerticalDigits: integer;
    function GetSideVerticalPrecision: integer;
    function GetTopHorizontalDigits: integer;
    function GetTopHorizontalPrecision: integer;
    function GetTopVerticalDigits: integer;
    function GetTopVerticalPrecision: integer;
    procedure SetFrontHorizontalDigits(const Value: integer);
    procedure SetFrontHorizontalPrecision(const Value: integer);
    procedure SetFrontVerticalDigits(const Value: integer);
    procedure SetFrontVerticalPrecision(const Value: integer);
    procedure SetSideHorizontalDigits(const Value: integer);
    procedure SetSideHorizontalPrecision(const Value: integer);
    procedure SetSideVerticalDigits(const Value: integer);
    procedure SetSideVerticalPrecision(const Value: integer);
    procedure SetTopHorizontalDigits(const Value: integer);
    procedure SetTopHorizontalPrecision(const Value: integer);
    procedure SetTopVerticalDigits(const Value: integer);
    procedure SetTopVerticalPrecision(const Value: integer);
    function GetFrontHorizontalDesiredSpacing: integer;
    function GetFrontVerticalDesiredSpacing: integer;
    function GetSideHorizontalDesiredSpacing: integer;
    function GetSideVerticalDesiredSpacing: integer;
    function GetTopHorizontalDesiredSpacing: integer;
    function GetTopVerticalDesiredSpacing: integer;
    procedure SetFrontHorizontalDesiredSpacing(const Value: integer);
    procedure SetFrontVerticalDesiredSpacing(const Value: integer);
    procedure SetSideHorizontalDesiredSpacing(const Value: integer);
    procedure SetSideVerticalDesiredSpacing(const Value: integer);
    procedure SetTopHorizontalDesiredSpacing(const Value: integer);
    procedure SetTopVerticalDesiredSpacing(const Value: integer);
  public
  published
    // @name stores the height in pixels of the front view of the model.
    property FrontHeight: integer read GetFrontHeight write SetFrontHeight;

    // @name is the height of the main form in pixels.
    property Height: integer read GetHeight write SetHeight;

    // @name stores the reference X-coordinate for the front view of the model.
    property FrontX: double read GetFrontX write SetFrontX;

    // @name stores the reference Y-coordinate for the front view of the model.
    property FrontY: double read GetFrontY write SetFrontY;

    // @name is the X-coordinate of the main form in pixels.
    property Left: integer read GetLeft write SetLeft;

    // @name is the magnification of the front view of the model.
    property MagnificationFront: double read GetMagnificationFront
      write SetMagnificationFront;

    // @name is the magnification of the side view of the model.
    property MagnificationSide: double read GetMagnificationSide
      write SetMagnificationSide;

    // @name is the magnification of the top view of the model.
    property MagnificationTop: double read GetMagnificationTop
      write SetMagnificationTop;

    // @name is the width in pixels of the side view of the model.
    property SideWidth: integer read GetSideWidth write SetSideWidth;

    // @name stores the reference X-coordinate for the side view of the model.
    property SideX: double read GetSideX write SetSideX;

    // @name stores the reference Y-coordinate for the front view of the model.
    property SideY: double read GetSideY write SetSideY;

    // @name is the Y-coordinate of the main form in pixels.
    property Top: integer read GetTop write SetTop;

    // @name is the height of the top view of the model in pixels
    property TopViewHeight: integer read GetTopViewHeight
      write SetTopViewHeight;

    // @name is the width of the top view of the model in pixels
    property TopViewWidth: integer read GetTopViewWidth write SetTopViewWidth;

    // @name stores the reference X-coordinate for the top view of the model.
    property TopX: double read GetTopX write SetTopX;

    // @name stores the reference Y-coordinate for the top view of the model.
    property TopY: double read GetTopY write SetTopY;

    // @name is the width of the main form in GoPhast in pixels.
    property Width: integer read GetWidth write SetWidth;

    // @name stores whether the model is maximized, minimized, or normal.
    property WindowState: TWindowState read GetWindowState write SetWindowState;

    property TopHorizontalDigits: integer read GetTopHorizontalDigits write SetTopHorizontalDigits;
    property TopHorizontalPrecision: integer read GetTopHorizontalPrecision write SetTopHorizontalPrecision;
    property TopHorizontalDesiredSpacing: integer read GetTopHorizontalDesiredSpacing write SetTopHorizontalDesiredSpacing;

    property TopVerticalDigits: integer read GetTopVerticalDigits write SetTopVerticalDigits;
    property TopVerticalPrecision: integer read GetTopVerticalPrecision write SetTopVerticalPrecision;
    property TopVerticalDesiredSpacing: integer read GetTopVerticalDesiredSpacing write SetTopVerticalDesiredSpacing;

    property FrontHorizontalDigits: integer read GetFrontHorizontalDigits write SetFrontHorizontalDigits;
    property FrontHorizontalPrecision: integer read GetFrontHorizontalPrecision write SetFrontHorizontalPrecision;
    property FrontHorizontalDesiredSpacing: integer read GetFrontHorizontalDesiredSpacing write SetFrontHorizontalDesiredSpacing;

    property FrontVerticalDigits: integer read GetFrontVerticalDigits write SetFrontVerticalDigits;
    property FrontVerticalPrecision: integer read GetFrontVerticalPrecision write SetFrontVerticalPrecision;
    property FrontVerticalDesiredSpacing: integer read GetFrontVerticalDesiredSpacing write SetFrontVerticalDesiredSpacing;

    property SideHorizontalDigits: integer read GetSideHorizontalDigits write SetSideHorizontalDigits;
    property SideHorizontalPrecision: integer read GetSideHorizontalPrecision write SetSideHorizontalPrecision;
    property SideHorizontalDesiredSpacing: integer read GetSideHorizontalDesiredSpacing write SetSideHorizontalDesiredSpacing;

    property SideVerticalDigits: integer read GetSideVerticalDigits write SetSideVerticalDigits;
    property SideVerticalPrecision: integer read GetSideVerticalPrecision write SetSideVerticalPrecision;
    property SideVerticalDesiredSpacing: integer read GetSideVerticalDesiredSpacing write SetSideVerticalDesiredSpacing;

  end;

implementation

uses frmGoPhastUnit;

function TGuiSettings.GetFrontHeight: integer;
begin
  result := frmGoPhast.pnlBottom.Height;
end;

procedure TGuiSettings.SetFrontHeight(Value : integer);
begin
  frmGoPhast.pnlBottom.Height := Value;
  if frmGoPhast.pnlBottom.Top > frmGoPhast.sbMain.Top then
  begin
    frmGoPhast.sbMain.Top := frmGoPhast.pnlBottom.Top +
      frmGoPhast.pnlBottom.Height;
  end;
  if frmGoPhast.splitHoriz.Top > frmGoPhast.pnlBottom.Top then
  begin
    frmGoPhast.splitHoriz.Top :=
      frmGoPhast.pnlBottom.Top - frmGoPhast.splitHoriz.Height;
  end;
end;

procedure TGuiSettings.SetFrontHorizontalDesiredSpacing(const Value: integer);
begin
  frmGoPhast.frameFrontView.rulHorizontal.RulerDesiredSpacing := Value;
end;

procedure TGuiSettings.SetFrontHorizontalDigits(const Value: integer);
begin
  frmGoPhast.frameFrontView.rulHorizontal.RulerDigits := Value;
end;

procedure TGuiSettings.SetFrontHorizontalPrecision(const Value: integer);
begin
  frmGoPhast.frameFrontView.rulHorizontal.RulerPrecision := Value;
end;

procedure TGuiSettings.SetFrontVerticalDesiredSpacing(const Value: integer);
begin
  frmGoPhast.frameFrontView.rulVertical.RulerDesiredSpacing := Value;
end;

procedure TGuiSettings.SetFrontVerticalDigits(const Value: integer);
begin
  frmGoPhast.frameFrontView.rulVertical.RulerDigits := Value;
end;

procedure TGuiSettings.SetFrontVerticalPrecision(const Value: integer);
begin
  frmGoPhast.frameFrontView.rulVertical.RulerPrecision := Value;
end;

function TGuiSettings.GetHeight: integer;
begin
  if frmGoPhast.WindowState = wsNormal then
  begin
    result := frmGoPhast.Height;
  end
  else
  begin
    result := frmGoPhast.OldHeight;
  end;
end;

procedure TGuiSettings.SetHeight(Value : integer);
begin
  frmGoPhast.Height := Value;
end;

function TGuiSettings.GetFrontHorizontalDesiredSpacing: integer;
begin
  result := frmGoPhast.frameFrontView.rulHorizontal.RulerDesiredSpacing;
end;

function TGuiSettings.GetFrontHorizontalDigits: integer;
begin
  result := frmGoPhast.frameFrontView.rulHorizontal.RulerDigits;
end;

function TGuiSettings.GetFrontHorizontalPrecision: integer;
begin
  result := frmGoPhast.frameFrontView.rulHorizontal.RulerPrecision;
end;

function TGuiSettings.GetFrontVerticalDesiredSpacing: integer;
begin
  result := frmGoPhast.frameFrontView.rulVertical.RulerDesiredSpacing;
end;

function TGuiSettings.GetFrontVerticalDigits: integer;
begin
  result := frmGoPhast.frameFrontView.rulVertical.RulerDigits;
end;

function TGuiSettings.GetFrontVerticalPrecision: integer;
begin
  result := frmGoPhast.frameFrontView.rulVertical.RulerPrecision;
end;

function TGuiSettings.GetFrontX: double;
begin
  result := frmGoPhast.frameFrontView.ZoomBox.OriginX;
end;

procedure TGuiSettings.SetFrontX(Value : double);
begin
  frmGoPhast.frameFrontView.ZoomBox.OriginX := Value;
end;

function TGuiSettings.GetFrontY: double;
begin
  result := frmGoPhast.frameFrontView.ZoomBox.OriginY;
end;

procedure TGuiSettings.SetFrontY(Value : double);
begin
  frmGoPhast.frameFrontView.ZoomBox.OriginY := Value;
end;

function TGuiSettings.GetLeft: integer;
begin
  result := frmGoPhast.Left;
end;

procedure TGuiSettings.SetLeft(Value : integer);
begin
  frmGoPhast.Left := Value;
end;

function TGuiSettings.GetMagnificationFront: double;
begin
  result := frmGoPhast.frameFrontView.ZoomBox.Magnification;
end;

procedure TGuiSettings.SetMagnificationFront(Value : double);
begin
  if Value = 0 then
  begin
    Value := 1;
  end;
  frmGoPhast.frameFrontView.ZoomBox.Magnification := Value;
end;

function TGuiSettings.GetMagnificationSide: double;
begin
  result := frmGoPhast.frameSideView.ZoomBox.Magnification;
end;

procedure TGuiSettings.SetMagnificationSide(Value : double);
begin
  if Value = 0 then
  begin
    Value := 1;
  end;
  frmGoPhast.frameSideView.ZoomBox.Magnification := Value;
end;

function TGuiSettings.GetMagnificationTop: double;
begin
  result := frmGoPhast.frameTopView.ZoomBox.Magnification;
end;

procedure TGuiSettings.SetMagnificationTop(Value : double);
begin
  if Value = 0 then
  begin
    Value := 1;
  end;
  frmGoPhast.frameTopView.ZoomBox.Magnification := Value;
end;

function TGuiSettings.GetSideHorizontalDesiredSpacing: integer;
begin
  result := frmGoPhast.frameSideView.rulHorizontal.RulerDesiredSpacing
end;

function TGuiSettings.GetSideHorizontalDigits: integer;
begin
  result := frmGoPhast.frameSideView.rulHorizontal.RulerDigits;
end;

function TGuiSettings.GetSideHorizontalPrecision: integer;
begin
  result := frmGoPhast.frameSideView.rulHorizontal.RulerPrecision;
end;

function TGuiSettings.GetSideVerticalDesiredSpacing: integer;
begin
  result := frmGoPhast.frameSideView.rulVertical.RulerDesiredSpacing;
end;

function TGuiSettings.GetSideVerticalDigits: integer;
begin
  result := frmGoPhast.frameSideView.rulVertical.RulerDigits;
end;

function TGuiSettings.GetSideVerticalPrecision: integer;
begin
  result := frmGoPhast.frameSideView.rulVertical.RulerPrecision;
end;

function TGuiSettings.GetSideWidth: integer;
begin
  result := frmGoPhast.frameSideView.Width;
end;

procedure TGuiSettings.SetSideHorizontalDesiredSpacing(const Value: integer);
begin
  frmGoPhast.frameSideView.rulHorizontal.RulerDesiredSpacing := Value;
end;

procedure TGuiSettings.SetSideHorizontalDigits(const Value: integer);
begin
  frmGoPhast.frameSideView.rulHorizontal.RulerDigits := Value;
end;

procedure TGuiSettings.SetSideHorizontalPrecision(const Value: integer);
begin
  frmGoPhast.frameSideView.rulHorizontal.RulerPrecision := Value;
end;

procedure TGuiSettings.SetSideVerticalDesiredSpacing(const Value: integer);
begin
  frmGoPhast.frameSideView.rulVertical.RulerDesiredSpacing := Value;
end;

procedure TGuiSettings.SetSideVerticalDigits(const Value: integer);
begin
  frmGoPhast.frameSideView.rulVertical.RulerDigits := Value;
end;

procedure TGuiSettings.SetSideVerticalPrecision(const Value: integer);
begin
  frmGoPhast.frameSideView.rulVertical.RulerPrecision := Value;
end;

procedure TGuiSettings.SetSideWidth(Value : integer);
begin
  frmGoPhast.frameSideView.Width := Value;
  frmGoPhast.frame3DView.Width := Value;
  if frmGoPhast.splitVertBottom.Left > frmGoPhast.frame3DView.Left then
  begin
    frmGoPhast.splitVertBottom.Left :=
      frmGoPhast.frame3DView.Left - frmGoPhast.splitVertBottom.Width;
  end;
  if frmGoPhast.splitVertTop.Left > frmGoPhast.frameSideView.Left then
  begin
    frmGoPhast.splitVertTop.Left :=
      frmGoPhast.frameSideView.Left - frmGoPhast.splitVertTop.Width;
  end;
end;

function TGuiSettings.GetSideX: double;
begin
  result := frmGoPhast.frameSideView.ZoomBox.OriginX;
end;

procedure TGuiSettings.SetSideX(Value : double);
begin
  frmGoPhast.frameSideView.ZoomBox.OriginX := Value;
end;

function TGuiSettings.GetSideY: double;
begin
  result := frmGoPhast.frameSideView.ZoomBox.OriginY;
end;

procedure TGuiSettings.SetSideY(Value : double);
begin
  frmGoPhast.frameSideView.ZoomBox.OriginY := Value;
end;

function TGuiSettings.GetTop: integer;
begin
  result := frmGoPhast.Top;
end;

function TGuiSettings.GetTopHorizontalDesiredSpacing: integer;
begin
  result := frmGoPhast.frameTopView.rulHorizontal.RulerDesiredSpacing
end;

function TGuiSettings.GetTopHorizontalDigits: integer;
begin
  result := frmGoPhast.frameTopView.rulHorizontal.RulerDigits;
end;

function TGuiSettings.GetTopHorizontalPrecision: integer;
begin
  result := frmGoPhast.frameTopView.rulHorizontal.RulerPrecision;
end;

procedure TGuiSettings.SetTop(Value : integer);
begin
  frmGoPhast.Top := Value;
end;

function TGuiSettings.GetTopVerticalDesiredSpacing: integer;
begin
  result := frmGoPhast.frameTopView.rulVertical.RulerDesiredSpacing;
end;

function TGuiSettings.GetTopVerticalDigits: integer;
begin
  result := frmGoPhast.frameTopView.rulVertical.RulerDigits;
end;

function TGuiSettings.GetTopVerticalPrecision: integer;
begin
  result := frmGoPhast.frameTopView.rulVertical.RulerPrecision;
end;

procedure TGuiSettings.SetTopHorizontalDesiredSpacing(const Value: integer);
begin
  frmGoPhast.frameTopView.rulHorizontal.RulerDesiredSpacing := Value;
end;

procedure TGuiSettings.SetTopHorizontalDigits(const Value: integer);
begin
  frmGoPhast.frameTopView.rulHorizontal.RulerDigits := Value;
end;

procedure TGuiSettings.SetTopHorizontalPrecision(const Value: integer);
begin
  frmGoPhast.frameTopView.rulHorizontal.RulerPrecision := Value;
end;

function TGuiSettings.GetTopViewHeight: integer;
begin
  result := frmGoPhast.frameTopView.Height;
end;

procedure TGuiSettings.SetTopVerticalDesiredSpacing(const Value: integer);
begin
  frmGoPhast.frameTopView.rulVertical.RulerDesiredSpacing := Value;
end;

procedure TGuiSettings.SetTopVerticalDigits(const Value: integer);
begin
  frmGoPhast.frameTopView.rulVertical.RulerDigits := Value;
end;

procedure TGuiSettings.SetTopVerticalPrecision(const Value: integer);
begin
  frmGoPhast.frameTopView.rulVertical.RulerPrecision := Value;
end;

procedure TGuiSettings.SetTopViewHeight(Value : integer);
begin
  frmGoPhast.frameTopView.Height := value;
end;

function TGuiSettings.GetTopViewWidth: integer;
begin
  result := frmGoPhast.frameTopView.Width;
end;

procedure TGuiSettings.SetTopViewWidth(Value : integer);
begin
  frmGoPhast.frameTopView.Width := value;
end;

function TGuiSettings.GetTopX: double;
begin
  result := frmGoPhast.frameTopView.ZoomBox.OriginX;
end;

procedure TGuiSettings.SetTopX(Value : double);
begin
  frmGoPhast.frameTopView.ZoomBox.OriginX := Value;
end;

function TGuiSettings.GetTopY: double;
begin
  result := frmGoPhast.frameTopView.ZoomBox.OriginY;
end;

procedure TGuiSettings.SetTopY(Value : double);
begin
  frmGoPhast.frameTopView.ZoomBox.OriginY := Value;
end;

function TGuiSettings.GetWidth: integer;
begin
  if frmGoPhast.WindowState = wsNormal then
  begin
    result := frmGoPhast.Width;
  end
  else
  begin
    result := frmGoPhast.OldWidth;
  end;

end;

procedure TGuiSettings.SetWidth(Value : integer);
begin
  frmGoPhast.Width := Value;
end;

function TGuiSettings.GetWindowState: TWindowState;
begin
  if frmGoPhast.WindowState = wsMinimized then
  begin
    result := wsNormal;
  end
  else
  begin
    result := frmGoPhast.WindowState;
  end;
end;

procedure TGuiSettings.SetWindowState(Value : TWindowState);
begin
  frmGoPhast.WindowState := Value;
end;

end.
