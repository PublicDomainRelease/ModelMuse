// @name defines a frame in which the user specifies the coordinates of
// a stream cross section in the SFR package.
unit frameCrossSectionUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, Grids, RbwDataGrid4;

type
  // @name represents the columns of @link(TframeCrossSection.dg8Point)
  // used to specify the X and Z coordinates of a cross section.
  TSfr8Point = (s8pX, s8pZ);

  // @name is a frame in which the user specifies the coordinates of
  // a stream cross section in the SFR package.
  TframeCrossSection = class(TFrame)
    // @name is the grid in which the user specifies the coordinates of
    // a stream cross section in the SFR package.
    dg8Point: TRbwDataGrid4;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  end;

implementation

{$R *.dfm}

resourcestring
  StrX = 'X';
  StrZ = 'Z';

{ TframeCrossSection }

constructor TframeCrossSection.Create(AOwner: TComponent);
begin
  inherited;
  dg8Point.Cells[Ord(s8pX),0] := StrX;
  dg8Point.Cells[Ord(s8pZ),0] := StrZ;
end;

end.
