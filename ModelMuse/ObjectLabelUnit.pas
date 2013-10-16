unit ObjectLabelUnit;

interface

uses
  Types, Classes, Controls, Graphics;

type
  TObjectLabel = class(TPersistent)
  private
    FFont: TFont;
    FVisible: boolean;
    FCaption: TCaption;
    FOnChange: TNotifyEvent;
    FOffSet: TPoint;
    procedure SetCaption(const Value: TCaption);
    procedure SetFont(const Value: TFont);
    procedure SetOffSet(const Value: TPoint);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetVisible(const Value: boolean);
  protected
    procedure DoChange; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property Font: TFont read FFont write SetFont;
    // Instead of specifying the location of the label, a label offset
    // is specified. This will be the offset from the location of the
    // object with which @classname is associated.
    property OffSet: TPoint read FOffSet write SetOffSet;
    property Visible: boolean read FVisible write SetVisible;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

implementation

{ TObjectLabel }

procedure TObjectLabel.Assign(Source: TPersistent);
var
  SourceLabel: TObjectLabel;
begin
  if Source is TObjectLabel then
  begin
    SourceLabel := TObjectLabel(Source);
    Caption := SourceLabel.Caption;
    Font := SourceLabel.Font;
    OffSet := SourceLabel.OffSet;
    Visible := SourceLabel.Visible;
  end
  else
  begin
    inherited;
  end;
end;

constructor TObjectLabel.Create;
begin
  FFont := TFont.Create;
  FOffSet.Y := 20;
end;

destructor TObjectLabel.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TObjectLabel.DoChange;
begin
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
end;

procedure TObjectLabel.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    DoChange
  end;
end;

procedure TObjectLabel.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  DoChange;
end;

procedure TObjectLabel.SetOffSet(const Value: TPoint);
begin
  if (FOffSet.X <> Value.X) or (FOffSet.Y <> Value.Y) then
  begin
    FOffSet := Value;
    DoChange
  end;
end;

procedure TObjectLabel.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TObjectLabel.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    DoChange
  end;
end;

end.
