unit ModflowMnw1Unit;

interface

uses
  Windows, ZLib, SysUtils, Classes, ModflowBoundaryUnit, OrderedCollectionUnit,
  ModflowCellUnit, RbwParser, GoPhastTypes, FormulaManagerUnit, SubscriptionUnit;

{$IFDEF MNW1}
type
  // Rw  > 0, Rw  = 0, Rw  < 0
  TMnw1ConductanceMethod = (mcmRadius, mcmFixed, mcmConductance);

  // DD
  TMnw1WaterLevelLimitType = (mwlltAbsolute, mwlltRelative);

  // QCUT, Q-%CUT:
  TMnw1PumpingLimitType = (mpltNone, mpltAbsolute, mpltPercent);

  TMnw1CellRecord = record
    Cell: TCellLocation;
    // Qdes
    DesiredPumpingRate: double;
    DesiredPumpingRateAnnotation: string;
    // QWval
    WaterQuality: double;
    WaterQualityAnnotation: string;
    // Rw
    ConductanceMethod: TMnw1ConductanceMethod;
    // Rw
    WellRadius: double;
    WellRadiusAnnotation: string;
    // Rw
    Conductance: double;
    ConductanceAnnotation: string;
    // Skin
    SkinFactor: double;
    SkinFactorAnnotation: string;
    // DD
    WaterLevelLimitType: TMnw1WaterLevelLimitType;
    // Hlim
    LimitingWaterLevel: double;
    LimitingWaterLevelAnnotation: string;
    // Href
    ReferenceElevation: double;
    ReferenceElevationAnnotation: string;
    // Iwgrp
    WaterQualityGroup: integer;
    WaterQualityGroupAnnotation: string;
    // Cp:C
    NonLinearLossCoefficient: double;
    NonLinearLossCoefficientAnnotation: string;
    // QCUT, Q-%CUT:
    PumpingLimitType: TMnw1PumpingLimitType;
    // Qfrcmn
    MinimumPumpingRate: double;
    MinimumPumpingRateAnnotation: string;
    // Qfrcmx
    MaximumPumpingRate: double;
    MaximumPumpingRateAnnotation: string;
    // SITE: MNWsite
    Site: string;
    StartingTime: double;
    EndingTime: double;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TMnw1Array = array of TMnw1CellRecord;

  TMnw1Storage = class(TCustomBoundaryStorage)
  private
    FMnw1Array: TMnw1Array;
    function GetMnw1Array: TMnw1Array;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property Mnw1Array: TMnw1Array read GetMnw1Array;
  end;

  TMnw1Item = class (TCustomModflowBoundaryItem)
  private
    FSite: string;

    // Qdes
    FDesiredPumpingRate: TFormulaObject;
    // QWval
    FWaterQuality: TFormulaObject;
    // Rw
    FWellRadius: TFormulaObject;
    // Rw
    FConductance: TFormulaObject;
    // Skin
    FSkinFactor: TFormulaObject;
    // Hlim
    FLimitingWaterLevel: TFormulaObject;
    // Href
    FReferenceElevation: TFormulaObject;
    // Iwgrp
    FWaterQualityGroup: TFormulaObject;
    // Cp:C
    FNonLinearLossCoefficient: TFormulaObject;
    // Qfrcmn
    FMinimumPumpingRate: TFormulaObject;
    // Qfrcmx
    FMaximumPumpingRate: TFormulaObject;

    FConductanceMethod: TMnw1ConductanceMethod;
    FPumpingLimitType: TMnw1PumpingLimitType;
    FWaterLevelLimitType: TMnw1WaterLevelLimitType;
    function GetConductance: string;
    function GetDesiredPumpingRate: string;
    function GetLimitingWaterLevel: string;
    function GetMaximumPumpingRate: string;
    function GetMinimumPumpingRate: string;
    function GetNonLinearLossCoefficient: string;
    function GetReferenceElevation: string;
    function GetSkinFactor: string;
    function GetWaterQuality: string;
    function GetWaterQualityGroup: string;
    function GetWellRadius: string;
    procedure SetConductance(const Value: string);
    procedure SetConductanceMethod(const Value: TMnw1ConductanceMethod);
    procedure SetDesiredPumpingRate(const Value: string);
    procedure SetLimitingWaterLevel(const Value: string);
    procedure SetMaximumPumpingRate(const Value: string);
    procedure SetMinimumPumpingRate(const Value: string);
    procedure SetNonLinearLossCoefficient(const Value: string);
    procedure SetPumpingLimitType(const Value: TMnw1PumpingLimitType);
    procedure SetReferenceElevation(const Value: string);
    procedure SetSite(const Value: string);
    procedure SetSkinFactor(const Value: string);
    procedure SetWaterLevelLimitType(const Value: TMnw1WaterLevelLimitType);
    procedure SetWaterQuality(const Value: string);
    procedure SetWaterQualityGroup(const Value: string);
    procedure SetWellRadius(const Value: string);
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure InvalidateModel; override;
    function BoundaryFormulaCount: integer; override;
  public
    Destructor Destroy; override;
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent); override;
  published
    // Qdes
    property DesiredPumpingRate: string read GetDesiredPumpingRate
      write SetDesiredPumpingRate;
    // QWval
    property WaterQuality: string read GetWaterQuality write SetWaterQuality;
    // Rw
    property ConductanceMethod: TMnw1ConductanceMethod read FConductanceMethod write SetConductanceMethod;
    // Rw
    property WellRadius: string read GetWellRadius write SetWellRadius;
    // Rw
    property Conductance: string read GetConductance write SetConductance;
    // Skin
    property SkinFactor: string read GetSkinFactor write SetSkinFactor;
    // DD
    property WaterLevelLimitType: TMnw1WaterLevelLimitType read FWaterLevelLimitType write SetWaterLevelLimitType;
    // Hlim
    property LimitingWaterLevel: string read GetLimitingWaterLevel
      write SetLimitingWaterLevel;
    // Href
    property ReferenceElevation: string read GetReferenceElevation
      write SetReferenceElevation;
    // Iwgrp
    property WaterQualityGroup: string read GetWaterQualityGroup
      write SetWaterQualityGroup;
    // Cp:C
    property NonLinearLossCoefficient: string read GetNonLinearLossCoefficient
      write SetNonLinearLossCoefficient;
    // QCUT, Q-%CUT:
    property PumpingLimitType: TMnw1PumpingLimitType read FPumpingLimitType write SetPumpingLimitType;
    // Qfrcmn
    property MinimumPumpingRate: string read GetMinimumPumpingRate
      write SetMinimumPumpingRate;
    // Qfrcmx
    property MaximumPumpingRate: string read GetMaximumPumpingRate
      write SetMaximumPumpingRate;
    // SITE: MNWsite
    property Site: string read FSite write SetSite;
  end;

  TMnw1TimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the pumping rates for a series of
    // Wells over a series of time intervals.
//    FPumpingRateData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TWellCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateDesiredPumpingRate(Sender: TObject);
    procedure InvalidateWaterQuality(Sender: TObject);
    procedure InvalidateWellRadius(Sender: TObject);
    procedure InvalidateConductance(Sender: TObject);
    procedure InvalidateSkinFactor(Sender: TObject);
    procedure InvalidateLimitingWaterLevel(Sender: TObject);
    procedure InvalidateReferenceElevation(Sender: TObject);
    procedure InvalidateWaterQualityGroup(Sender: TObject);
    procedure InvalidateNonLinearLossCoefficient(Sender: TObject);
    procedure InvalidateMinimumPumpingRate(Sender: TObject);
    procedure InvalidateMaximumPumpingRate(Sender: TObject);
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TWellStorage.WellArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    procedure InvalidateModel; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel); override;
  end;

  TMnw1Cell = class(TValueCell)
  private
    Values: TMnw1CellRecord;
    StressPeriod: integer;
    function GetConductance: double;
    function GetConductanceMethod: TMnw1ConductanceMethod;
    function GetDesiredPumpingRate: double;
    function GetLimitingWaterLevel: double;
    function GetMaximumPumpingRate: double;
    function GetMinimumPumpingRate: double;
    function GetNonLinearLossCoefficient: double;
    function GetPumpingLimitType: TMnw1PumpingLimitType;
    function GetReferenceElevation: double;
    function GetSite: string;
    function GetSkinFactor: double;
    function GetWaterLevelLimitType: TMnw1WaterLevelLimitType;
    function GetWaterQuality: double;
    function GetWaterQualityGroup: integer;
    function GetWellRadius: double;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    procedure SetColumn(const Value: integer); override;
    procedure SetLayer(const Value: integer); override;
    procedure SetRow(const Value: integer); override;
    function GetIntegerValue(Index: integer; AModel: TBaseModel): integer; override;
    function GetRealValue(Index: integer; AModel: TBaseModel): double; override;
    function GetRealAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string; override;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    function GetSection: integer; override;
    procedure RecordStrings(Strings: TStringList); override;
  public
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
    // Qdes
    property DesiredPumpingRate: double read GetDesiredPumpingRate;
    // QWval
    property WaterQuality: double read GetWaterQuality;
    // Rw
    property ConductanceMethod: TMnw1ConductanceMethod read GetConductanceMethod;
    // Rw
    property WellRadius: double read GetWellRadius;
    // Rw
    property Conductance: double read GetConductance;
    // Skin
    property SkinFactor: double read GetSkinFactor;
    // DD
    property WaterLevelLimitType: TMnw1WaterLevelLimitType read GetWaterLevelLimitType;
    // Hlim
    property LimitingWaterLevel: double read GetLimitingWaterLevel;
    // Href
    property ReferenceElevation: double read GetReferenceElevation;
    // Iwgrp
    property WaterQualityGroup: integer read GetWaterQualityGroup;
    // Cp:C
    property NonLinearLossCoefficient: double read GetNonLinearLossCoefficient;
    // QCUT, Q-%CUT:
    property PumpingLimitType: TMnw1PumpingLimitType read GetPumpingLimitType;
    // Qfrcmn
    property MinimumPumpingRate: double read GetMinimumPumpingRate;
    // Qfrcmx
    property MaximumPumpingRate: double read GetMaximumPumpingRate;
    // SITE: MNWsite
    property Site: string read GetSite;
  end;

  TMnw1Boundary = class(TModflowBoundary)
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TWell_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  public
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    procedure InvalidateDisplay; override;
  end;

const
  DesiredPumpingRatePosition = 0;
  WaterQualityPosition = 1;
  WellRadiusPosition = 2;
  ConductancePosition = 3;
  SkinFactorPosition = 4;
  LimitingWaterLevelPosition = 5;
  ReferenceElevationPosition = 6;
  WaterQualityGroupPosition = 7;
  NonLinearLossCoefficientPosition = 8;
  MinimumPumpingRatePosition = 9;
  MaximumPumpingRatePosition = 10;

{$ENDIF}

implementation

{ TMnw1Item }
{$IFDEF MNW1}

procedure TMnw1Item.Assign(Source: TPersistent);
var
  MnwSource: TMnw1Item;
begin
  if Source is TMnw1Item then
  begin
    MnwSource := TMnw1Item(Source);
    DesiredPumpingRate := MnwSource.DesiredPumpingRate;
    WaterQuality := MnwSource.WaterQuality;
    ConductanceMethod := MnwSource.ConductanceMethod;
    WellRadius := MnwSource.WellRadius;
    Conductance := MnwSource.Conductance;
    SkinFactor := MnwSource.SkinFactor;
    WaterLevelLimitType := MnwSource.WaterLevelLimitType;
    LimitingWaterLevel := MnwSource.LimitingWaterLevel;
    ReferenceElevation := MnwSource.ReferenceElevation;
    WaterQualityGroup := MnwSource.WaterQualityGroup;
    NonLinearLossCoefficient := MnwSource.NonLinearLossCoefficient;
    PumpingLimitType := MnwSource.PumpingLimitType;
    MinimumPumpingRate := MnwSource.MinimumPumpingRate;
    MaximumPumpingRate := MnwSource.MaximumPumpingRate;
    Site := MnwSource.Site;
  end;
  inherited;
end;

procedure TMnw1Item.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TWellCollection;
  DesiredPumpingRateObserver: TObserver;
  WaterQualityObserver: TObserver;
  WellRadiusObserver: TObserver;
  ConductanceRateObserver: TObserver;
  SkinFactorObserver: TObserver;
  LimitingWaterLevelRateObserver: TObserver;
  ReferenceElevationObserver: TObserver;
  WaterQualityGroupObserver: TObserver;
  NonLinearLossCoefficientObserver: TObserver;
  MinimumPumpingRateObserver: TObserver;
  MaximumPumpingRateObserver: TObserver;
begin
  ParentCollection := Collection as TWellCollection;

  DesiredPumpingRateObserver := FObserverList[DesiredPumpingRatePosition];
  DesiredPumpingRateObserver.OnUpToDateSet := ParentCollection.InvalidateDesiredPumpingRate;

  WaterQualityObserver := FObserverList[WaterQualityPosition];
  WaterQualityObserver.OnUpToDateSet := ParentCollection.InvalidateWaterQuality;

  WellRadiusObserver := FObserverList[WellRadiusPosition];
  WellRadiusObserver.OnUpToDateSet := ParentCollection.InvalidateWellRadius;

  ConductanceRateObserver := FObserverList[ConductancePosition];
  ConductanceRateObserver.OnUpToDateSet := ParentCollection.InvalidateConductance;

  SkinFactorObserver := FObserverList[SkinFactorPosition];
  SkinFactorObserver.OnUpToDateSet := ParentCollection.InvalidateSkinFactor;

  LimitingWaterLevelRateObserver := FObserverList[LimitingWaterLevelPosition];
  LimitingWaterLevelRateObserver.OnUpToDateSet := ParentCollection.InvalidateLimitingWaterLevel;

  ReferenceElevationObserver := FObserverList[ReferenceElevationPosition];
  ReferenceElevationObserver.OnUpToDateSet := ParentCollection.InvalidateReferenceElevation;

  WaterQualityGroupObserver := FObserverList[WaterQualityGroupPosition];
  WaterQualityGroupObserver.OnUpToDateSet := ParentCollection.InvalidateWaterQualityGroup;

  NonLinearLossCoefficientObserver := FObserverList[NonLinearLossCoefficientPosition];
  NonLinearLossCoefficientObserver.OnUpToDateSet := ParentCollection.InvalidateNonLinearLossCoefficient;

  MinimumPumpingRateObserver := FObserverList[MinimumPumpingRatePosition];
  MinimumPumpingRateObserver.OnUpToDateSet := ParentCollection.InvalidateMinimumPumpingRate;

  MaximumPumpingRateObserver := FObserverList[MaximumPumpingRatePosition];
  MaximumPumpingRateObserver.OnUpToDateSet := ParentCollection.InvalidateMaximumPumpingRate;
end;

function TMnw1Item.BoundaryFormulaCount: integer;
begin
  result := 11;
end;

procedure TMnw1Item.CreateFormulaObjects;
begin
  FDesiredPumpingRate := CreateFormulaObject(dso3D);
  FWaterQuality := CreateFormulaObject(dso3D);
  FWellRadius := CreateFormulaObject(dso3D);
  FConductance := CreateFormulaObject(dso3D);
  FSkinFactor := CreateFormulaObject(dso3D);
  FLimitingWaterLevel := CreateFormulaObject(dso3D);
  FReferenceElevation := CreateFormulaObject(dso3D);
  FWaterQualityGroup := CreateFormulaObject(dso3D);
  FNonLinearLossCoefficient := CreateFormulaObject(dso3D);
  FMinimumPumpingRate := CreateFormulaObject(dso3D);
  FMaximumPumpingRate := CreateFormulaObject(dso3D);

end;

destructor TMnw1Item.Destroy;
begin

  inherited;
end;

function TMnw1Item.GetBoundaryFormula(Index: integer): string;
begin

end;

function TMnw1Item.GetConductance: string;
begin
  Result := FConductance.Formula;
  ResetItemObserver(ConductancePosition);
end;

function TMnw1Item.GetDesiredPumpingRate: string;
begin
  Result := FDesiredPumpingRate.Formula;
  ResetItemObserver(DesiredPumpingRatePosition);
end;

function TMnw1Item.GetLimitingWaterLevel: string;
begin
  Result := FLimitingWaterLevel.Formula;
  ResetItemObserver(LimitingWaterLevelPosition);
end;

function TMnw1Item.GetMaximumPumpingRate: string;
begin
  Result := FMaximumPumpingRate.Formula;
  ResetItemObserver(MaximumPumpingRatePosition);
end;

function TMnw1Item.GetMinimumPumpingRate: string;
begin
  Result := FMinimumPumpingRate.Formula;
  ResetItemObserver(MinimumPumpingRatePosition);
end;

function TMnw1Item.GetNonLinearLossCoefficient: string;
begin
  Result := FNonLinearLossCoefficient.Formula;
  ResetItemObserver(NonLinearLossCoefficientPosition);
end;

procedure TMnw1Item.GetPropertyObserver(Sender: TObject; List: TList);
begin
  inherited;

end;

function TMnw1Item.GetReferenceElevation: string;
begin
  Result := FReferenceElevation.Formula;
  ResetItemObserver(ReferenceElevationPosition);
end;

function TMnw1Item.GetSkinFactor: string;
begin
  Result := FSkinFactor.Formula;
  ResetItemObserver(SkinFactorPosition);
end;

function TMnw1Item.GetWaterQuality: string;
begin
  Result := FWaterQuality.Formula;
  ResetItemObserver(WaterQualityPosition);
end;

function TMnw1Item.GetWaterQualityGroup: string;
begin
  Result := FWaterQualityGroup.Formula;
  ResetItemObserver(WaterQualityGroupPosition);
end;

function TMnw1Item.GetWellRadius: string;
begin
  Result := FWellRadius.Formula;
  ResetItemObserver(WellRadiusPosition);
end;

procedure TMnw1Item.InvalidateModel;
begin
  inherited;

end;

function TMnw1Item.IsSame(AnotherItem: TOrderedItem): boolean;
begin

end;

procedure TMnw1Item.RemoveFormulaObjects;
begin
  inherited;

end;

procedure TMnw1Item.SetBoundaryFormula(Index: integer; const Value: string);
begin
  inherited;

end;

procedure TMnw1Item.SetConductance(const Value: string);
begin
  UpdateFormula(Value, ConductancePosition, FConductance);
end;

procedure TMnw1Item.SetConductanceMethod(const Value: TMnw1ConductanceMethod);
begin
  FConductanceMethod := Value;
end;

procedure TMnw1Item.SetDesiredPumpingRate(const Value: string);
begin
  UpdateFormula(Value, DesiredPumpingRatePosition, FDesiredPumpingRate);
end;

procedure TMnw1Item.SetLimitingWaterLevel(const Value: string);
begin
  UpdateFormula(Value, LimitingWaterLevelPosition, FLimitingWaterLevel);
end;

procedure TMnw1Item.SetMaximumPumpingRate(const Value: string);
begin
  UpdateFormula(Value, MaximumPumpingRatePosition, FMaximumPumpingRate);
end;

procedure TMnw1Item.SetMinimumPumpingRate(const Value: string);
begin
  UpdateFormula(Value, MinimumPumpingRatePosition, FMinimumPumpingRate);
end;

procedure TMnw1Item.SetNonLinearLossCoefficient(const Value: string);
begin
  UpdateFormula(Value, NonLinearLossCoefficientPosition, FNonLinearLossCoefficient);
end;

procedure TMnw1Item.SetPumpingLimitType(const Value: TMnw1PumpingLimitType);
begin
  FPumpingLimitType := Value;
end;

procedure TMnw1Item.SetReferenceElevation(const Value: string);
begin
  UpdateFormula(Value, ReferenceElevationPosition, FReferenceElevation);
end;

procedure TMnw1Item.SetSite(const Value: string);
begin
  FSite := Value;
end;

procedure TMnw1Item.SetSkinFactor(const Value: string);
begin
  UpdateFormula(Value, SkinFactorPosition, FSkinFactor);
end;

procedure TMnw1Item.SetWaterLevelLimitType(
  const Value: TMnw1WaterLevelLimitType);
begin
  FWaterLevelLimitType := Value;
end;

procedure TMnw1Item.SetWaterQuality(const Value: string);
begin
  UpdateFormula(Value, WaterQualityPosition, FWaterQuality);
end;

procedure TMnw1Item.SetWaterQualityGroup(const Value: string);
begin
  UpdateFormula(Value, WaterQualityGroupPosition, FWaterQualityGroup);
end;

procedure TMnw1Item.SetWellRadius(const Value: string);
begin
  UpdateFormula(Value, WellRadiusPosition, FWellRadius);
end;

{ TMnw1CellRecord }

procedure TMnw1CellRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompReal(Comp, DesiredPumpingRate);
  WriteCompInt(Comp, Strings.IndexOf(DesiredPumpingRateAnnotation));
  WriteCompReal(Comp, WaterQuality);
  WriteCompInt(Comp, Strings.IndexOf(WaterQualityAnnotation));
  WriteCompInt(Comp, Ord(ConductanceMethod));
  WriteCompReal(Comp, WellRadius);
  WriteCompInt(Comp, Strings.IndexOf(WellRadiusAnnotation));
  WriteCompReal(Comp, Conductance);
  WriteCompInt(Comp, Strings.IndexOf(ConductanceAnnotation));
  WriteCompReal(Comp, SkinFactor);
  WriteCompInt(Comp, Strings.IndexOf(SkinFactorAnnotation));
  WriteCompInt(Comp, Ord(WaterLevelLimitType));
  WriteCompReal(Comp, LimitingWaterLevel);
  WriteCompInt(Comp, Strings.IndexOf(LimitingWaterLevelAnnotation));
  WriteCompReal(Comp, ReferenceElevation);
  WriteCompInt(Comp, Strings.IndexOf(ReferenceElevationAnnotation));
  WriteCompInt(Comp, WaterQualityGroup);
  WriteCompInt(Comp, Strings.IndexOf(WaterQualityGroupAnnotation));
  WriteCompReal(Comp, NonLinearLossCoefficient);
  WriteCompInt(Comp, Strings.IndexOf(NonLinearLossCoefficientAnnotation));
  WriteCompInt(Comp, Ord(PumpingLimitType));
  WriteCompReal(Comp, MinimumPumpingRate);
  WriteCompInt(Comp, Strings.IndexOf(MinimumPumpingRateAnnotation));
  WriteCompReal(Comp, MaximumPumpingRate);
  WriteCompInt(Comp, Strings.IndexOf(MaximumPumpingRateAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(Site));
end;

procedure TMnw1CellRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(DesiredPumpingRateAnnotation);
  Strings.Add(WaterQualityAnnotation);
  Strings.Add(WellRadiusAnnotation);
  Strings.Add(ConductanceAnnotation);
  Strings.Add(SkinFactorAnnotation);
  Strings.Add(LimitingWaterLevelAnnotation);
  Strings.Add(ReferenceElevationAnnotation);
  Strings.Add(WaterQualityGroupAnnotation);
  Strings.Add(NonLinearLossCoefficientAnnotation);
  Strings.Add(MinimumPumpingRateAnnotation);
  Strings.Add(MaximumPumpingRateAnnotation);
  Strings.Add(Site);
end;

procedure TMnw1CellRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);

  DesiredPumpingRate := ReadCompReal(Decomp);
  DesiredPumpingRateAnnotation := Annotations[ReadCompInt(Decomp)];

  WaterQuality := ReadCompReal(Decomp);
  WaterQualityAnnotation := Annotations[ReadCompInt(Decomp)];

  ConductanceMethod := TMnw1ConductanceMethod(ReadCompInt(Decomp));

  WellRadius := ReadCompReal(Decomp);
  WellRadiusAnnotation := Annotations[ReadCompInt(Decomp)];

  Conductance := ReadCompReal(Decomp);
  ConductanceAnnotation := Annotations[ReadCompInt(Decomp)];

  SkinFactor := ReadCompReal(Decomp);
  SkinFactorAnnotation := Annotations[ReadCompInt(Decomp)];

  WaterLevelLimitType := TMnw1WaterLevelLimitType(ReadCompInt(Decomp));

  LimitingWaterLevel := ReadCompReal(Decomp);
  LimitingWaterLevelAnnotation := Annotations[ReadCompInt(Decomp)];

  ReferenceElevation := ReadCompReal(Decomp);
  ReferenceElevationAnnotation := Annotations[ReadCompInt(Decomp)];

  WaterQualityGroup := ReadCompInt(Decomp);
  WaterQualityGroupAnnotation := Annotations[ReadCompInt(Decomp)];

  NonLinearLossCoefficient := ReadCompReal(Decomp);
  NonLinearLossCoefficientAnnotation := Annotations[ReadCompInt(Decomp)];

  PumpingLimitType := TMnw1PumpingLimitType(ReadCompInt(Decomp));

  MinimumPumpingRate := ReadCompReal(Decomp);
  MinimumPumpingRateAnnotation := Annotations[ReadCompInt(Decomp)];

  MaximumPumpingRate := ReadCompReal(Decomp);
  MaximumPumpingRateAnnotation := Annotations[ReadCompInt(Decomp)];

  Site := Annotations[ReadCompInt(Decomp)];
end;


{ TMnw1Storage }

procedure TMnw1Storage.Clear;
begin
  SetLength(FMnw1Array, 0);
  FCleared := True;
end;

function TMnw1Storage.GetMnw1Array: TMnw1Array;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FMnw1Array;
end;

procedure TMnw1Storage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FMnw1Array, Count);
  for Index := 0 to Count - 1 do
  begin
    FMnw1Array[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TMnw1Storage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FMnw1Array);
    for Index := 0 to Count - 1 do
    begin
      FMnw1Array[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FMnw1Array[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TMnw1TimeListLink }

procedure TMnw1TimeListLink.CreateTimeLists;
begin
  inherited;

end;

destructor TMnw1TimeListLink.Destroy;
begin

  inherited;
end;

{ TWellCollection }

procedure TWellCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  inherited;

end;

function TWellCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
begin

end;

procedure TWellCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel);
begin
  inherited;

end;

procedure TWellCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
begin
  inherited;

end;

function TWellCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin

end;

procedure TWellCollection.InvalidateConductance(Sender: TObject);
begin

end;

procedure TWellCollection.InvalidateDesiredPumpingRate(Sender: TObject);
begin

end;

procedure TWellCollection.InvalidateLimitingWaterLevel(Sender: TObject);
begin

end;

procedure TWellCollection.InvalidateMaximumPumpingRate(Sender: TObject);
begin

end;

procedure TWellCollection.InvalidateMinimumPumpingRate(Sender: TObject);
begin

end;

procedure TWellCollection.InvalidateModel;
begin
  inherited;

end;

procedure TWellCollection.InvalidateNonLinearLossCoefficient(Sender: TObject);
begin

end;

procedure TWellCollection.InvalidateReferenceElevation(Sender: TObject);
begin

end;

procedure TWellCollection.InvalidateSkinFactor(Sender: TObject);
begin

end;

procedure TWellCollection.InvalidateWaterQuality(Sender: TObject);
begin

end;

procedure TWellCollection.InvalidateWaterQualityGroup(Sender: TObject);
begin

end;

procedure TWellCollection.InvalidateWellRadius(Sender: TObject);
begin

end;

class function TWellCollection.ItemClass: TBoundaryItemClass;
begin

end;

procedure TWellCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  inherited;

end;

{ TMnw1Cell }

procedure TMnw1Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;

end;

function TMnw1Cell.GetColumn: integer;
begin

end;

function TMnw1Cell.GetConductance: double;
begin

end;

function TMnw1Cell.GetConductanceMethod: TMnw1ConductanceMethod;
begin

end;

function TMnw1Cell.GetDesiredPumpingRate: double;
begin

end;

function TMnw1Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin

end;

function TMnw1Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin

end;

function TMnw1Cell.GetLayer: integer;
begin

end;

function TMnw1Cell.GetLimitingWaterLevel: double;
begin

end;

function TMnw1Cell.GetMaximumPumpingRate: double;
begin

end;

function TMnw1Cell.GetMinimumPumpingRate: double;
begin

end;

function TMnw1Cell.GetNonLinearLossCoefficient: double;
begin

end;

function TMnw1Cell.GetPumpingLimitType: TMnw1PumpingLimitType;
begin

end;

function TMnw1Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin

end;

function TMnw1Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin

end;

function TMnw1Cell.GetReferenceElevation: double;
begin

end;

function TMnw1Cell.GetRow: integer;
begin

end;

function TMnw1Cell.GetSection: integer;
begin

end;

function TMnw1Cell.GetSite: string;
begin

end;

function TMnw1Cell.GetSkinFactor: double;
begin

end;

function TMnw1Cell.GetWaterLevelLimitType: TMnw1WaterLevelLimitType;
begin

end;

function TMnw1Cell.GetWaterQuality: double;
begin

end;

function TMnw1Cell.GetWaterQualityGroup: integer;
begin

end;

function TMnw1Cell.GetWellRadius: double;
begin

end;

function TMnw1Cell.IsIdentical(AnotherCell: TValueCell): boolean;
begin

end;

procedure TMnw1Cell.RecordStrings(Strings: TStringList);
begin
  inherited;

end;

procedure TMnw1Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;

end;

procedure TMnw1Cell.SetColumn(const Value: integer);
begin
  inherited;

end;

procedure TMnw1Cell.SetLayer(const Value: integer);
begin
  inherited;

end;

procedure TMnw1Cell.SetRow(const Value: integer);
begin
  inherited;

end;

{ TMnw1Boundary }

procedure TMnw1Boundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
begin
  inherited;

end;

class function TMnw1Boundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin

end;

procedure TMnw1Boundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
begin
  inherited;

end;

procedure TMnw1Boundary.InvalidateDisplay;
begin
  inherited;

end;
{$ENDIF}

end.
