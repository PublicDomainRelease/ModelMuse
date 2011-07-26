unit RbwRadioGroup;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, ExtCtrls;

type
  TRbwRadioGroup = class(TRadioGroup)
  private
    FWordWrap: boolean;
    function GetItems: TStrings;
    procedure SetItems(const Value: TStrings);
    procedure SetWordWrap(const Value: boolean);
    procedure UpdateButtonWordWrap;
    { Private declarations }
  protected
    procedure Loaded; override;
    { Protected declarations }
  public
    property Items: TStrings read GetItems write SetItems;
    { Public declarations }
  published
    property WordWrap: boolean read FWordWrap write SetWordWrap;
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RBW', [TRbwRadioGroup]);
end;

{ TRbwRadioGroup }

function TRbwRadioGroup.GetItems: TStrings;
begin
  result := inherited Items;
end;

procedure TRbwRadioGroup.Loaded;
begin
  inherited;
  UpdateButtonWordWrap;
end;

procedure TRbwRadioGroup.SetItems(const Value: TStrings);
begin
  inherited Items := Value;
  UpdateButtonWordWrap;
end;

procedure TRbwRadioGroup.SetWordWrap(const Value: boolean);
begin
  FWordWrap := Value;
  UpdateButtonWordWrap;
end;

procedure TRbwRadioGroup.UpdateButtonWordWrap;
var
  Index: Integer;
begin
  if Items.Count <> ControlCount then
  begin
    Exit;
  end;
  for Index := 0 to Items.Count - 1 do
  begin
    Buttons[Index].WordWrap := WordWrap;
  end;
end;

end.
