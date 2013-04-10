unit frameMt3dBasicPkgUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, ExtCtrls, ArgusDataEntry,
  ModflowPackageSelectionUnit, frameGridUnit, Mt3dmsChemSpeciesUnit;

type
  TframeMt3dBasicPkg = class(TframePackage)
    edMassUnit: TLabeledEdit;
    rdeInactiveConcentration: TRbwDataEntry;
    lblInactiveConcentration: TLabel;
    rdeMinimumSaturatedFraction: TRbwDataEntry;
    lblMinimumSaturatedFraction: TLabel;
    pnlSpecies: TPanel;
    Splitter1: TSplitter;
    frameGridImmobile: TframeGrid;
    frameGridMobile: TframeGrid;
  private
    procedure GetMt3dComponents(Mt3DComponents: TCustomChemSpeciesCollection;
      AFrame: TframeGrid);
    procedure SetMt3dComponents(Mt3DComponents: TCustomChemSpeciesCollection;
      AFrame: TframeGrid);
    procedure FixNames(Names: TStringList; AFrame: TframeGrid);
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    procedure GetMt3dmsChemSpecies(
      MobileComponents: TMobileChemSpeciesCollection;
      ImmobileComponents: TChemSpeciesCollection);
    procedure SetMt3dmsChemSpecies(
      MobileComponents: TMobileChemSpeciesCollection;
      ImmobileComponents: TChemSpeciesCollection);
    { Public declarations }
  end;

var
  frameMt3dBasicPkg: TframeMt3dBasicPkg;

implementation

uses
  PhastModelUnit;

{$R *.dfm}

resourcestring
  StrMobileSpecies = 'Mobile Species';
  StrImmobileSpecies = 'Immobile Species';

{ TframeMt3dBasicPkg }

procedure TframeMt3dBasicPkg.GetData(Package: TModflowPackageSelection);
var
  BasicPackage: TMt3dBasic;
begin
  inherited;
  BasicPackage := Package as TMt3dBasic;
  edMassUnit.Text := BasicPackage.MassUnit;
  rdeInactiveConcentration.Text := FloatToStr(BasicPackage.InactiveConcentration);
  rdeMinimumSaturatedFraction.Text := FloatToStr(BasicPackage.MinimumSaturatedFraction);

  frameGridMobile.Grid.Cells[0,0] := StrMobileSpecies;
  frameGridImmobile.Grid.Cells[0,0] := StrImmobileSpecies;
end;

procedure TframeMt3dBasicPkg.SetData(Package: TModflowPackageSelection);
var
  BasicPackage: TMt3dBasic;
begin
  inherited;
  BasicPackage := Package as TMt3dBasic;
  BasicPackage.MassUnit := edMassUnit.Text;
  BasicPackage.InactiveConcentration := StrToFloat(rdeInactiveConcentration.Text);
  BasicPackage.MinimumSaturatedFraction := StrToFloat(rdeMinimumSaturatedFraction.Text);
end;

procedure TframeMt3dBasicPkg.FixNames(Names: TStringList; AFrame: TframeGrid);
var
  Index: Integer;
  AName: string;
begin
  for Index := 1 to AFrame.seNumber.AsInteger do
  begin
    AName := GenerateNewRoot(Trim(AFrame.Grid.Cells[0, Index]));
    if Names.IndexOf(AName) >= 0 then
    begin
      AFrame.Grid.Cells[0, Index] := '';
    end
    else
    begin
      Names.Add(AName);
    end;
  end;
end;

procedure TframeMt3dBasicPkg.SetMt3dComponents(
  Mt3DComponents: TCustomChemSpeciesCollection; AFrame: TframeGrid);
var
  ItemIndex: Integer;
  Index: Integer;
  Item: TChemSpeciesItem;
  AList: TList;
  GridCol: TStrings;
  ItemRow: integer;
begin
  AList := TList.Create;
  try
    for Index := 1 to AFrame.seNumber.AsInteger do
    begin
      if Trim(AFrame.Grid.Cells[0, Index]) <> '' then
      begin
        AList.Add(AFrame.Grid.Objects[0, Index]);
      end;
    end;
    GridCol := AFrame.Grid.Cols[0];
    for Index := Mt3DComponents.Count - 1 downto 0 do
    begin
      if AList.IndexOf(Mt3DComponents[Index]) < 0 then
      begin
        Item := Mt3DComponents[Index];
        ItemRow := GridCol.IndexOfObject(Item);
        if ItemRow >= 1 then
        begin
          AFrame.Grid.Objects[0, ItemRow] := nil;
        end;
        Mt3DComponents.Delete(Index);
      end;
    end;
    ItemIndex := 0;
    for Index := 1 to AFrame.seNumber.AsInteger do
    begin
      Item := AFrame.Grid.Objects[0, Index] as TChemSpeciesItem;
      if (Item = nil) and (Trim(AFrame.Grid.Cells[0, Index]) <> '') then
      begin
        Item := Mt3DComponents.Add;
      end;
      if Item <> nil then
      begin
        Item.Index := ItemIndex;
        Item.Name := Trim(AFrame.Grid.Cells[0, Index]);
        Inc(ItemIndex);
      end;
    end;
  finally
    AList.Free;
  end;
end;

procedure TframeMt3dBasicPkg.SetMt3dmsChemSpecies(
  MobileComponents: TMobileChemSpeciesCollection;
  ImmobileComponents: TChemSpeciesCollection);
var
  Names: TStringList;
begin
  Names := TStringList.Create;
  try
    Names.Sorted := True;
    Names.CaseSensitive := False;
    FixNames(Names, frameGridMobile);
    FixNames(Names, frameGridImmobile);
  finally
    Names.Free;
  end;

  SetMt3dComponents(MobileComponents, frameGridMobile);
  SetMt3dComponents(ImmobileComponents, frameGridImmobile);
end;

procedure TframeMt3dBasicPkg.GetMt3dComponents(
  Mt3DComponents: TCustomChemSpeciesCollection; AFrame: TframeGrid);
var
  Item: TChemSpeciesItem;
  Index: Integer;
begin
  AFrame.seNumber.AsInteger := Mt3DComponents.Count;
  AFrame.Grid.BeginUpdate;
  try
    if Mt3DComponents.Count > 0 then
    begin
      for Index := 0 to Mt3DComponents.Count - 1 do
      begin
        Item := Mt3DComponents[Index];
        AFrame.Grid.Cells[0, Index + 1] := Item.Name;
        AFrame.Grid.Objects[0, Index + 1] := Item;
      end;
    end
    else
    begin
      AFrame.Grid.Cells[0, 1] := '';
    end;
  finally
    AFrame.Grid.EndUpdate;
  end;
  AFrame.seNumber.AsInteger := Mt3DComponents.Count;
end;

procedure TframeMt3dBasicPkg.GetMt3dmsChemSpecies(
  MobileComponents: TMobileChemSpeciesCollection;
  ImmobileComponents: TChemSpeciesCollection);
begin
  GetMt3dComponents(MobileComponents, frameGridMobile);
  GetMt3dComponents(ImmobileComponents, frameGridImmobile);
end;

end.
