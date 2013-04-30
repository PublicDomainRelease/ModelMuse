unit ModflowCfpFixedUnit;

interface

uses Classes, RbwParser, GoPhastTypes, ModflowBoundaryUnit, SubscriptionUnit,
  FormulaManagerUnit, Contnrs, SysUtils;

type
  TCfpFixedBoundary = class(TModflowSteadyBoundary)
  private
    FFixedHead: TFormulaObject;
    FDiameterObserver: TObserver;
    function GetFixedHead: string;
    procedure SetFixedHead(const Value: string);
    function GetFixedHeadObserver: TObserver;
  protected
    procedure HandleChangedValue(Observer: TObserver); override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    function GetUsedObserver: TObserver; override;
    procedure CreateFormulaObjects; override;
    function BoundaryObserverPrefix: string; override;
    procedure CreateObservers; override;
    property FixedHeadObserver: TObserver read GetFixedHeadObserver;
  public
    Procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
  published
    property FixedHead: string read GetFixedHead write SetFixedHead;
  end;

implementation

uses
  PhastModelUnit, frmGoPhastUnit, ScreenObjectUnit;

const
  FixedHeadPosition = 0;

{ TCfpFixedBoundary }

procedure TCfpFixedBoundary.Assign(Source: TPersistent);
var
  SourceCfp: TCfpFixedBoundary;
begin
  if Source is TCfpFixedBoundary then
  begin
    SourceCfp := TCfpFixedBoundary(Source);
    FixedHead := SourceCfp.FixedHead;
  end;
  inherited;
end;

function TCfpFixedBoundary.BoundaryObserverPrefix: string;
begin
  result := 'CfpFixedBoundary_';
end;

constructor TCfpFixedBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  FixedHead := '0';
end;

procedure TCfpFixedBoundary.CreateFormulaObjects;
begin
  FFixedHead := CreateFormulaObject(dso3D);
end;

procedure TCfpFixedBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(FixedHeadObserver);
  end;
end;

destructor TCfpFixedBoundary.Destroy;
begin
  FixedHead := '0';
  inherited;
end;

function TCfpFixedBoundary.GetFixedHead: string;
begin
  Result := FFixedHead.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(FixedHeadPosition);
  end;
end;

function TCfpFixedBoundary.GetFixedHeadObserver: TObserver;
begin
  if FDiameterObserver = nil then
  begin
    CreateObserver('Cfp_FixedHead_', FDiameterObserver);
  end;
  result := FDiameterObserver;
end;

procedure TCfpFixedBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FFixedHead then
  begin
    List.Add(FObserverList[FixedHeadPosition]);
  end;

end;

function TCfpFixedBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('CFP_Fixed_Used_', FUsedObserver);
  end;
  result := FUsedObserver;
end;

procedure TCfpFixedBoundary.HandleChangedValue(Observer: TObserver);
begin
  // invalidate display here.
  { TODO -cCFP : Does this need to be finished?}
end;

procedure TCfpFixedBoundary.SetFixedHead(const Value: string);
begin
  UpdateFormula(Value, FixedHeadPosition, FFixedHead);
end;

end.
