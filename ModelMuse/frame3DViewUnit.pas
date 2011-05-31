{@abstract(The main purpose of @name is to define @link(Tframe3DView) which
  is used to encapsulate the interaction with the 3D view
  of the @link(TPhastModel).)

@author(Richard B. Winston <rbwinst@usgs.gov>)
}
unit frame3DViewUnit;

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, GLWidget, arcball;

type
  {@abstract(@name is used to encapsulate the interaction with the 3D view
    of the @link(TPhastModel).)}
  Tframe3DView = class(TFrame)
    // @name is used to display a 3D view of the @link(TPhastModel).
    // See @link(glWidModelViewMouseDown),
    // @link(glWidModelViewMouseMove),
    // @link(glWidModelViewMouseUp),
    // @link(glWidModelViewRender), and
    // @link(glWidModelViewResize).
    glWidModelView: TGLWidget;
    // @name responds to OnMouseDown events in @link(glWidModelView)
    // by starting a pan, rotate, or zoom operation.
    procedure glWidModelViewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    // @name responds to OnMouseMove events in @link(glWidModelView)
    // by continuing a pan, rotate, or zoom operation.
    procedure glWidModelViewMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    // @name responds to OnMouseUp events in @link(glWidModelView)
    // by finishing a pan, rotate, or zoom operation.
    procedure glWidModelViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    // @name responds to OnRender events in @link(glWidModelView)
    // by drawing a 3D view of the model in @link(glWidModelView).
    procedure glWidModelViewRender(Sender: TObject);
    // @name responds to OnResize events in @link(glWidModelView)
    // by reinitializing some variables related to @link(glWidModelView).
    procedure glWidModelViewResize(Sender: TObject);
  private
    // @name is set to @True during panning operations.
    FPanning: boolean;
    // @name is used to handle rotation operations.
    FTheBall: TArcBall;
    // If the width of the @link(glWidModelView) is greater tha its height,
    // @name represents how far the center of the @link(glWidModelView)
    // is offset in the X direction from where it would be if it were square.
    FXOffset: integer;
    // @name represents how far the model has been panned in the X direction.
    FXPan: double;
    // @name represents how far the model had been panned in the X direction
    // at the beginning of a panning operations.
    FXPanStart: double;
    // @name is the X-coordinate of the position of the mouse in
    // pixels at the beginning of an operation.
    FXStart: integer;
    // If the height of the @link(glWidModelView) is greater tha its width,
    // @name represents how far the center of the @link(glWidModelView)
    // is offset in the Y direction from where it would be if it were square.
    FYOffset: integer;
    // @name represents how far the model has been panned in the Y direction.
    FYPan: double;
    // @name represents how far the model had been panned in the Y direction
    // at the beginning of a panning operations.
    FYPanStart: double;
    // @name is the Y-coordinate of the position of the mouse in
    // pixels at the beginning of an operation.
    FYStart: integer;
    // @name represents the factor by which the view of the model has
    // been increased or decreased.
    FZoomFactor: double;
    // @name is set to true while zooming in or out.
    FZooming: boolean;
    // @name represents the factor by which the view of the model has
    // been increased or decreased when the model is first viewed.
    FZScale: double;
    // @name stores @link(FZoomFactor) at the beginning of a zooming operation.
    FZStart: double;
    { Private declarations }
  public
    // @name creates an instance of @classname.
    constructor Create(AOwner: TComponent); override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name sets the orientation of the 3D view of model so that
    // it is seen from above with the magnification set up so the whole
    // model can be seen.
    procedure SetDefaultOrientation;
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses frmGoPhastUnit, CursorsFoiledAgain, Math, OpenGL12x, frmColorsUnit;

const
  PanFactor = 0.25;
  ZOffset = -50;
  ViewAngle = 45; // in degrees.
  nearPosition = 1;
  farPosition = 1E3;

{ Tframe3DView }

constructor Tframe3DView.Create(AOwner: TComponent);
begin
  inherited;
  FZoomFactor := 1;
  FTheBall := TArcBall.Create;
  FTheBall.Init(Min(glWidModelView.ClientWidth,
    glWidModelView.ClientHeight) div 2);
end;

destructor Tframe3DView.Destroy;
begin
  FTheBall.Free;
  inherited;
end;

procedure Tframe3DView.SetDefaultOrientation;
var
  Angle: double;
  TanAngle: double;
  Z1, Z2: single;
begin
  if frmGoPhast.PhastModel.Grid.CanDraw3D then
  begin
    Angle := ViewAngle / 2 / 180 * Pi;
    TanAngle := Tan(Angle);
    with frmGoPhast.Grid do
    begin
      Z1 := (ColumnPosition[ColumnCount] - ColumnPosition[0]) / 2;
      Z2 := (RowPosition[RowCount] - RowPosition[0]) / 2;
      {ZScale := (nearPosition - ZOffset +
        ((LayerElevation[LayerCount] - LayerElevation[0])
        /2/Model.Exaggeration))
        *TanAngle/Max(Z1, Z2); }

      FZScale := ((nearPosition - ZOffset) * TanAngle)
        / (Max(Z1, Z2) +
          Abs(HighestElevation - LowestElevation)
        * TanAngle * frmGoPhast.PhastModel.Exaggeration);

      FZScale := FZScale * 0.8;
    end;
  end;
  FXPan := 0;
  FYPan := 0;
  FZoomFactor := 1;
  FTheBall.RestoreDefaultOrientation;
  glWidModelView.Invalidate;
end;


procedure Tframe3DView.glWidModelViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not glWidModelView.Started then
    Exit;

  if Button = mbLeft then
  begin
    if ssShift in Shift then
    begin
      // Pan
      FXStart := X;
      FYStart := Y;
      FXPanStart := FXPan;
      FYPanStart := FYPan;
      FPanning := True;
      glWidModelView.Cursor := crHandFlat;
    end
    else
    begin
      // Rotate
      FTheBall.BeginDrag(X - FXOffset, Y - FYOffset);
      glWidModelView.Invalidate;
    end;
  end
  else if Button = mbRight then
  begin
    // Zoom
    FYStart := Y;
    FZStart := FZoomFactor;
    FZooming := True;
    glWidModelView.Cursor := crZoomByY;
  end;
end;

procedure Tframe3DView.glWidModelViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Factor: double;
begin
  if not glWidModelView.Started then
    Exit;
  if FZooming then
  begin
    if Y < 0 then
    begin
      Y := 0;
    end
    else if Y > glWidModelView.Height then
    begin
      Y := glWidModelView.Height;
    end;
    Factor := (Y - FYStart) / glWidModelView.Height;
    Factor := 1 - Factor;
    FZoomFactor := FZStart * Factor;
    //ZScale := ZStart*(1-Factor);//*ZFactor;
    glWidModelView.Invalidate;
  end
  else if FPanning then
  begin
    Factor := (FXStart - X) * PanFactor;
    FXPan := FXPanStart - Factor;
    Factor := (FYStart - Y) * PanFactor;
    FYPan := FYPanStart + Factor;
    glWidModelView.Invalidate;
  end
  else if FTheBall.Dragging then
  begin
    // rotating
    FTheBall.MouseMove(X - FXOffset, Y - FYOffset);
    glWidModelView.Invalidate;
  end;
end;

procedure Tframe3DView.glWidModelViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Factor: double;
begin
  if not glWidModelView.Started then
    Exit;
  if FZooming then
  begin
    if Y < 0 then
    begin
      Y := 0;
    end
    else if Y > glWidModelView.Height then
    begin
      Y := glWidModelView.Height;
    end;
    Factor := (Y - FYStart) / glWidModelView.Height;
    //ZScale := ZStart*(1-Factor);
    Factor := 1 - Factor;
    FZoomFactor := FZStart * Factor;

    glWidModelView.Invalidate;
    FZooming := False;
    glWidModelView.Cursor := crDefault;
  end
  else if FPanning then
  begin
    Factor := (FXStart - X) * PanFactor;
    FXPan := FXPanStart - Factor;
    Factor := (FYStart - Y) * PanFactor;
    FYPan := FYPanStart + Factor;
    glWidModelView.Invalidate;
    FPanning := False;
    glWidModelView.Cursor := crDefault;
  end
  else if FTheBall.Dragging then
  begin
    FTheBall.EndDrag(X - FXOffset, Y - FYOffset);
    glWidModelView.Invalidate;
  end;
end;

procedure Tframe3DView.glWidModelViewRender(Sender: TObject);
var
  errorCode: TGLuint;
  XMove, YMove, ZMove: single;
  light_position: array[0..3] of GLfloat;
  //  Colors: array[0..3] of TGLint;
  //const
  //  XY = 40.0;
  //  light_position : array[0..3] of GLfloat = ( 5.0, 40.0, -4.0, 2.0 );
  //  light_position : array[0..3] of GLfloat = ( XY, XY, 40.0, 0.0 );
  //  light_position2 : array[0..3] of GLfloat = ( -XY, -XY, 40.0, 0.0 );
begin
  if not glWidModelView.Started then
    Exit;

  light_position[0] := ColorValues.X;
  light_position[1] := ColorValues.Y;
  light_position[2] := ColorValues.Z;
  light_position[3] := 0;

  // White background
  glClearColor(1.0, 1.0, 1.0, 0.0);

  //  glShadeModel(GL_FLAT);
  glShadeModel(GL_SMOOTH);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLightfv(GL_LIGHT0, GL_POSITION, @light_position);

  if not frmGoPhast.CanDraw then
  begin
    Exit;
  end;

  if (frmGoPhast.PhastModel = nil)
    or (csReading in frmGoPhast.PhastModel.ComponentState) then
  begin
    Exit;
  end;

  if (frmGoPhast.PhastModel.Grid = nil)
    or not frmGoPhast.PhastModel.Grid.CanDraw3D then
  begin
    Exit;
  end;

  //  glEnable(GL_CULL_FACE);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  glPushMatrix;

  XMove := 0;
  YMove := 0;
  ZMove := 0;
  if frmGoPhast.PhastModel.Grid.CanDraw3D then
  begin
    with frmGoPhast.PhastModel.Grid do
    begin
      XMove := -(ColumnPosition[0] + ColumnPosition[ColumnCount]) / 2;
      YMove := -(RowPosition[0] + RowPosition[RowCount]) / 2;
      ZMove := -(CellElevation[ColumnCount div 2,RowCount div 2, 0]
        + CellElevation[ColumnCount div 2,RowCount div 2, LayerCount]) / 2;
    end;
  end;

  // The descriptions below are written as if they apply to the model
  // when they actually affect the coordinate system.  Because they
  // actually affect the coordinate system, the order in which they affect
  // the model is the reverse of the order they appear.  For example
  // XPan and YPan are applied last and moving the center of the model
  // to the origin is applied first.

  // XPan and YPan moves the model around on the screen.
  glTranslatef(FXPan, FYPan, 0);
  // ZOffset moves the model back behind the near cut-off position.
  glTranslatef(0, 0, ZOffset);
  // Rotate the model based on the grid angle after converting
  // from radians to degrees.
  glRotatef(frmGoPhast.PhastModel.Grid.GridAngle / Pi * 180, 0.0, 0.0, 1.0);
  // Respond to "rolling" of the model.
  glMultMatrixf(@FTheBall.Matrix);
  // Use ZScale to scale the model to a size that can be
  // confortably seen.
  // Apply the vertical exaggeration to the model.
  glScalef(FZScale * FZoomFactor, FZScale * FZoomFactor,
    FZScale * FZoomFactor * frmGoPhast.PhastModel.Exaggeration);
  // Move the center of the model to the origin so that all
  // "rolling" is relative to the center of the model.
  glTranslatef(XMove, YMove, ZMove);
  glPushMatrix;

  glEnable(GL_DEPTH_TEST);

  frmGoPhast.PhastModel.Grid.Draw3D;

  frmGoPhast.PhastModel.PathLines.Draw3D;
  frmGoPhast.PhastModel.EndPoints.Draw3D;
  frmGoPhast.PhastModel.TimeSeries.Draw3D;

  if frmGoPhast.tb3DObjects.Down then
  begin
    glPushMatrix;
    glRotatef(-frmGoPhast.PhastModel.Grid.GridAngle / Pi * 180, 0.0, 0.0, 1.0);
    frmGoPhast.PhastModel.DrawScreenObjects3D;
    glPopMatrix;
  end;
  glPopMatrix;

  glLineWidth(2);
  glPointSize(5);

  glColor3ub(0, 0, 0);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);

  //  glDisable(GL_LIGHTING);
  FTheBall.Render;
  //  glEnable(GL_LIGHTING);
  glEnable(GL_DEPTH_TEST);
  errorCode := glGetError;

  if errorCode <> GL_NO_ERROR then
    Beep;
  glPopMatrix;

  glFlush;

  errorCode := glGetError;

  if errorCode <> GL_NO_ERROR then
    Beep;
  //raise Exception.Create('Error in Render'#13 + gluErrorString(errorCode));
end;

procedure Tframe3DView.glWidModelViewResize(Sender: TObject);
var
  errorCode: TGLuint;
begin
  if not glWidModelView.Started then
    Exit;
  if FTheBall <> nil then
  begin
    FTheBall.Init(Min(glWidModelView.ClientWidth, glWidModelView.ClientHeight)
      div 2, glWidModelView.ClientWidth / glWidModelView.ClientHeight);
  end;
  glWidModelView.PerspectiveProjection(ViewAngle, nearPosition, farPosition);

  if glWidModelView.ClientWidth > glWidModelView.ClientHeight then
  begin
    FXOffset := (glWidModelView.ClientWidth
      - glWidModelView.ClientHeight) div 2;
    FYOffset := 0;
  end
  else
  begin
    FXOffset := 0;
    FYOffset := (glWidModelView.ClientHeight
      - glWidModelView.ClientWidth) div 2;
  end;

  errorCode := glGetError;
  if errorCode <> GL_NO_ERROR then
    raise Exception.Create('Error in Render'#13 + gluErrorString(errorCode));
end;

end.
