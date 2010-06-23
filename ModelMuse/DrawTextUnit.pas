unit DrawTextUnit;

interface

uses Windows, Classes, Graphics;

type
  TDrawItem = class(TPersistent)
  private
    FRect: TRect;
    FFont: TFont;
    FText: string;
    FSelected: boolean;
    FEditing: boolean;
    FOnChange: TNotifyEvent;
    procedure FontChanged(Sender: TObject);
    procedure SetFont(const Value: TFont);
    procedure SetRect(const Value: TRect);
    procedure SetText(const Value: string);
    procedure SetEditing(const Value: boolean);
    procedure SetSelected(const Value: boolean);
  protected
    procedure Changed;
  public
    Constructor Create;
    Destructor Destroy; override;
    property Selected: boolean read FSelected write SetSelected;
    property Editing: boolean read FEditing write SetEditing;
    procedure Draw(ACanvas: TCanvas);
  published
    property Text: string read FText write SetText;
    property Rect: TRect read FRect write SetRect;
    property Font: TFont read FFont write SetFont;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  Math;

procedure TDrawItem.Changed;
begin
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
end;

constructor TDrawItem.Create;
begin
  inherited;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
end;

destructor TDrawItem.Destroy;
begin
  Changed;
  FFont.Free;
  inherited;
end;

procedure TDrawItem.Draw(ACanvas: TCanvas);
var
  Lines: TStringList;
  LineIndex: Integer;
  Extent: TSize;
  ARect: TRect;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := Text;
    for LineIndex := 0 to Lines.Count - 1 do
    begin
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font := Font;
      Extent := ACanvas.TextExtent(Lines[LineIndex]);
      if not Editing then
      begin
        ACanvas.TextOut(Rect.Left, Rect.Top + Extent.cy * LineIndex,
          Lines[LineIndex]);
      end;
      if LineIndex = 0 then
      begin
        ARect := Rect;
        ARect.Right := Rect.Left + Extent.cx;
      end
      else
      begin
        ARect := Rect;
        ARect.Right := Max(Rect.Right, Rect.Left + Extent.cx);
      end;
      ARect.Bottom := Rect.Top + Extent.cy * (LineIndex + 1);
      Rect := ARect;
    end;
  finally
    Lines.Free;
  end;
  if Selected and not Editing then
  begin
    ACanvas.Pen.Style := psDot;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(Rect);
  end;
end;

procedure TDrawItem.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TDrawItem.SetEditing(const Value: boolean);
begin
  if FEditing <> Value then
  begin
    FEditing := Value;
    Changed;
  end;

end;

procedure TDrawItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TDrawItem.SetRect(const Value: TRect);
begin
  if not EqualRect(FRect, Value) then
  begin
    FRect := Value;
    Changed;
  end;

end;

procedure TDrawItem.SetSelected(const Value: boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    Changed;
  end;
end;

procedure TDrawItem.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

end.
