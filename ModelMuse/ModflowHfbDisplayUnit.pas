unit ModflowHfbDisplayUnit;

interface

uses EdgeDisplayUnit;

type
  THfbDisplayer = class(TCustomModflowGridEdgeDisplay)
  protected
    function GetDescription(DataIndex: integer): string; override;
    function GetRealValueTypeCount: integer; override;
  public
    procedure SetUpToDate(const Value: boolean); override;
  end;

implementation

uses frmGoPhastUnit;

{ THfbDisplayer }

function THfbDisplayer.GetDescription(DataIndex: integer): string;
begin
  case DataIndex of
    0:
      begin
        result := 'Hydr Conductivity';
      end;
    1:
      begin
        result := 'Thickness';;
      end;
    2:
      begin
        result := 'Hydr Characteristic';
      end;
    else
      Assert(False);
  end;
end;

function THfbDisplayer.GetRealValueTypeCount: integer;
begin
  result := 3;
end;

procedure THfbDisplayer.SetUpToDate(const Value: boolean);
begin
  inherited;
  if not Value and (self = frmGoPhast.PhastModel.EdgeDisplay) then
  begin
    frmGoPhast.PhastModel.Grid.GridChanged;
  end;
end;

end.
