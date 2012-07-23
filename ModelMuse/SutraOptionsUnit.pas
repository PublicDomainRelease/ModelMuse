unit SutraOptionsUnit;

interface

uses
  GoPhastTypes, Classes;

type
  TTransportChoice = (tcSolute, tcEnergy);

  TSutraOptions = class(TGoPhastPersistent)
  private
    FTransportChoice: TTransportChoice;
    procedure SetTransportChoice(const Value: TTransportChoice);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property TransportChoice: TTransportChoice read FTransportChoice
      write SetTransportChoice;
  end;

implementation

{ TSutraOptions }

procedure TSutraOptions.Assign(Source: TPersistent);
var
  SourceOptions: TSutraOptions;
begin
  if Source is TSutraOptions then
  begin
    SourceOptions := TSutraOptions(Source);
    TransportChoice := SourceOptions.TransportChoice;
  end
  else
  begin
    inherited;
  end;
end;

procedure TSutraOptions.SetTransportChoice(const Value: TTransportChoice);
begin
  if FTransportChoice <> Value then
  begin
    FTransportChoice := Value;
    InvalidateModel;
  end;
end;

end.
