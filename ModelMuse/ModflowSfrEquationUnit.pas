unit ModflowSfrEquationUnit;

interface

uses SysUtils, Classes, RbwParser, OrderedCollectionUnit, ModflowBoundaryUnit,
  FormulaManagerUnit, SubscriptionUnit;

type
  TSfrEquationRecord = record
    DepthCoefficient: double;
    DepthExponent: double;
    WidthCoefficient: double;
    WidthExponent: double;
    StartingTime: double;
    EndingTime: double;
    DepthCoefficientAnnotation: string;
    DepthExponentAnnotation: string;
    WidthCoefficientAnnotation: string;
    WidthExponentAnnotation: string;
  end;

  // @name represents a MODFLOW Streamflow Routing boundary for one time interval.
  // @name is stored by @link(TSfrCollection).
  TSfrEquationItem = class(TCustomModflowBoundaryItem)
  private
    FDepthCoefficient: TFormulaObject;
    FDepthExponent: TFormulaObject;
    FWidthCoefficient: TFormulaObject;
    FWidthExponent: TFormulaObject;
    procedure SetDepthExponent(const Value: string);
    procedure SetDepthCoefficient(const Value: string);
    procedure SetWidthCoefficient(const Value: string);
    procedure SetWidthExponent(const Value: string);
    function GetDepthCoefficient: string;
    function GetDepthExponent: string;
    function GetWidthCoefficient: string;
    function GetWidthExponent: string;
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // See @link(TCustomModflowBoundaryItem.BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(TCustomModflowBoundaryItem.BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function BoundaryFormulaCount: integer; override;
  public
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
  published
    property DepthCoefficient: string read GetDepthCoefficient
      write SetDepthCoefficient;
    property DepthExponent: string read GetDepthExponent
      write SetDepthExponent;
    property WidthCoefficient: string read GetWidthCoefficient
      write SetWidthCoefficient;
    property WidthExponent: string read GetWidthExponent
      write SetWidthExponent;
  end;

  // @name represents MODFLOW Streamflow Routing boundaries
  // for a series of time intervals.
  TSfrEquationCollection = class(TCustomNonSpatialBoundColl)
  private
    FTimeValues: array of TSfrEquationRecord;
    function GetChannelTimeValues(Index: integer): TSfrEquationRecord;
    procedure SetChannelTimeValues(Index: integer;
      const Value: TSfrEquationRecord);
  protected
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TMF_BoundItemClass; override;
  public
    procedure EvaluateBoundaries;
    property EquationTimeValues[Index: integer]: TSfrEquationRecord
      read GetChannelTimeValues write SetChannelTimeValues;
    function GetEquationTimeValuesFromTime(StartTime: double): TSfrEquationRecord;
  end;

implementation

uses Contnrs, DataSetUnit, ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit,
  ModflowSfrUnit, frmFormulaErrorsUnit, frmErrorsAndWarningsUnit, GoPhastTypes, 
  frmGoPhastUnit, ModflowSfrChannelUnit;

const
  DepthCoefficientPosition = 0;
  DepthExponentPosition = 1;
  WidthCoefficientPosition = 2;
  WidthExponentPosition = 3;

{ TSfrEquationItem }

procedure TSfrEquationItem.Assign(Source: TPersistent);
var
  Sfr: TSfrEquationItem;
begin
  if Source is TSfrEquationItem then
  begin
    Sfr := TSfrEquationItem(Source);
    DepthCoefficient := Sfr.DepthCoefficient;
    DepthExponent := Sfr.DepthExponent;
    WidthCoefficient := Sfr.WidthCoefficient;
    WidthExponent := Sfr.WidthExponent;
  end;
  inherited;
end;

procedure TSfrEquationItem.AssignObserverEvents(Collection: TCollection);
begin
  // do nothing
end;

function TSfrEquationItem.BoundaryFormulaCount: integer;
begin
  result := 4;
end;

constructor TSfrEquationItem.Create(Collection: TCollection);
begin
  inherited;
  DepthCoefficient := '0';
  DepthExponent := '0';
  WidthCoefficient := '0';
  WidthExponent := '0';
end;

procedure TSfrEquationItem.CreateFormulaObjects;
begin
  FDepthCoefficient := CreateFormulaObject(dso3D);
  FDepthExponent := CreateFormulaObject(dso3D);
  FWidthCoefficient := CreateFormulaObject(dso3D);
  FWidthExponent := CreateFormulaObject(dso3D);
end;

destructor TSfrEquationItem.Destroy;
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    BoundaryFormula[Index] := '0';
  end;
  inherited;
end;

function TSfrEquationItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    0:
      result := DepthCoefficient;
    1:
      result := DepthExponent;
    2:
      result := WidthCoefficient;
    3:
      result := WidthExponent;
    else Assert(False);
  end;
end;

function TSfrEquationItem.GetDepthCoefficient: string;
begin
  Result := FDepthCoefficient.Formula;
  ResetItemObserver(DepthCoefficientPosition);
end;

function TSfrEquationItem.GetDepthExponent: string;
begin
  Result := FDepthExponent.Formula;
  ResetItemObserver(DepthExponentPosition);
end;

procedure TSfrEquationItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FDepthCoefficient then
  begin
    List.Add(FObserverList[DepthCoefficientPosition]);
  end;
  if Sender = FDepthExponent then
  begin
    List.Add(FObserverList[DepthExponentPosition]);
  end;
  if Sender = FWidthCoefficient then
  begin
    List.Add(FObserverList[WidthCoefficientPosition]);
  end;
  if Sender = FWidthExponent then
  begin
    List.Add(FObserverList[WidthExponentPosition]);
  end;
end;

function TSfrEquationItem.GetWidthCoefficient: string;
begin
  Result := FWidthCoefficient.Formula;
  ResetItemObserver(WidthCoefficientPosition);
end;

function TSfrEquationItem.GetWidthExponent: string;
begin
  Result := FWidthExponent.Formula;
  ResetItemObserver(WidthExponentPosition);
end;

function TSfrEquationItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SfrChannel: TSfrEquationItem;
begin
  result := (AnotherItem is TSfrEquationItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    SfrChannel := TSfrEquationItem(AnotherItem);
    result := (DepthCoefficient = SfrChannel.DepthCoefficient)
      and (DepthExponent = SfrChannel.DepthExponent)
      and (WidthCoefficient = SfrChannel.WidthCoefficient)
      and (WidthExponent = SfrChannel.WidthExponent);
  end;
end;

procedure TSfrEquationItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FWidthExponent,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FWidthCoefficient,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FDepthExponent,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FDepthCoefficient,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
end;

procedure TSfrEquationItem.SetDepthExponent(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FDepthExponent.Formula <> Value then
  begin
    UpdateFormula(Value, DepthExponentPosition, FDepthExponent);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrDepthExponent(self);
      end;
    end;
  end;
end;

procedure TSfrEquationItem.SetWidthCoefficient(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FWidthCoefficient.Formula <> Value then
  begin
    UpdateFormula(Value, WidthCoefficientPosition, FWidthCoefficient);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrWidthCoefficient(self);
      end;
    end;
  end;
end;

procedure TSfrEquationItem.SetWidthExponent(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FWidthExponent.Formula <> Value then
  begin
    UpdateFormula(Value, WidthExponentPosition, FWidthExponent);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrWidthExponent(self);
      end;
    end;
  end;
end;

procedure TSfrEquationItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    0:
      DepthCoefficient := Value;
    1:
      DepthExponent := Value;
    2:
      WidthCoefficient := Value;
    3:
      WidthExponent := Value;
    else Assert(False);
  end;
end;

procedure TSfrEquationItem.SetDepthCoefficient(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FDepthCoefficient.Formula <> Value then
  begin
    UpdateFormula(Value, DepthCoefficientPosition, FDepthCoefficient);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrDepthCoefficient(self);
      end;
    end;
  end;
end;

{ TSfrEquationCollection }

procedure TSfrEquationCollection.EvaluateBoundaries;
var
  CurrentRecord: TSfrEquationRecord;
  CurrentItem: TSfrEquationItem;
  Compiler: TRbwParser;
  PhastModel: TPhastModel;
  SfrBound: TSfrBoundary;
  ICalc: integer;
  Formula: string;
  Expression: TExpression;
  ScrObj: TScreenObject;
  Index: integer;
begin
  SfrBound := BoundaryGroup as TSfrBoundary;
  if not (3 in SfrBound.ParamIcalc.IcalcSet) then
  begin
    Exit;
  end;
  ScrObj := ScreenObject as TScreenObject;
  PhastModel := Model as TPhastModel;
  SetLength(FTimeValues, Count);
  Compiler := PhastModel.rpThreeDFormulaCompiler;
  for Index := 0 to Count - 1 do
  begin
    CurrentItem := Items[Index] as TSfrEquationItem;
    CurrentRecord.StartingTime := CurrentItem.StartTime;
    CurrentRecord.EndingTime := CurrentItem.EndTime;
    ICALC := SfrBound.ParamIcalc.ICalc(CurrentItem.StartTime);
    if ICALC = 3 then
    begin
      Expression := nil;
      Formula := CurrentItem.DepthCoefficient;
      CurrentRecord.DepthCoefficientAnnotation := 'Assigned by '
        + ScrObj.Name + ' with formula = "' + Formula + '."';
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError(ScrObj.Name,
            '(depth coefficient for the SFR package)',
            Formula, E.Message);

          CurrentItem.DepthCoefficient := '0.';
          Formula := CurrentItem.DepthCoefficient;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.DepthCoefficient := Expression.DoubleResult;

      Formula := CurrentItem.DepthExponent;
      CurrentRecord.DepthExponentAnnotation := 'Assigned by '
        + ScrObj.Name + ' with formula = "' + Formula + '."';
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError(ScrObj.Name,
            '(depth exponent for the SFR package)',
            Formula, E.Message);

          CurrentItem.DepthExponent := '0.';
          Formula := CurrentItem.DepthCoefficient;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.DepthExponent := Expression.DoubleResult;

      Formula := CurrentItem.WidthCoefficient;
      CurrentRecord.WidthCoefficientAnnotation := 'Assigned by '
        + ScrObj.Name + ' with formula = "' + Formula + '."';
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError(ScrObj.Name,
            '(width coefficient for the SFR package)',
            Formula, E.Message);

          CurrentItem.WidthCoefficient := '0.';
          Formula := CurrentItem.DepthCoefficient;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.WidthCoefficient := Expression.DoubleResult;

      Formula := CurrentItem.WidthExponent;
      CurrentRecord.WidthExponentAnnotation := 'Assigned by '
        + ScrObj.Name + ' with formula = "' + Formula + '."';
      try
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
        // only global variables are used so there should be no need
        // to update the variables.
        Expression.Evaluate;
      except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError(ScrObj.Name,
            '(width exponent for the SFR package)',
            Formula, E.Message);

          CurrentItem.WidthExponent := '0.';
          Formula := CurrentItem.DepthCoefficient;
          Compiler.Compile(Formula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        end;
      end;
      CurrentRecord.WidthExponent := Expression.DoubleResult;
    end
    else
    begin
      CurrentRecord.DepthCoefficient := 0;
      CurrentRecord.DepthExponent := 0;
      CurrentRecord.WidthCoefficient := 0;
      CurrentRecord.WidthExponent := 0;
      CurrentRecord.DepthCoefficientAnnotation := 'DepthCoefficient not used';
      CurrentRecord.DepthExponentAnnotation := 'DepthExponent not used';
      CurrentRecord.WidthCoefficientAnnotation := 'WidthCoefficient not used';
      CurrentRecord.WidthExponentAnnotation := 'WidthExponent not used';
    end;

    FTimeValues[Index] := CurrentRecord;
  end;
end;

function TSfrEquationCollection.GetChannelTimeValues(
  Index: integer): TSfrEquationRecord;
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  result := FTimeValues[Index];
end;

function TSfrEquationCollection.GetEquationTimeValuesFromTime(
  StartTime: double): TSfrEquationRecord;
var
  Index: integer;
  ScreenObjectName: string;
  ErrorMessage: string;
begin
  Assert(Length(FTimeValues) > 0);
  result := FTimeValues[0];
  for Index := 0 to Length(FTimeValues) - 1 do
  begin
    if (FTimeValues[Index].StartingTime <= StartTime) then
    begin
      result := FTimeValues[Index];
      if (FTimeValues[Index].EndingTime > StartTime) then
      begin
        Exit;
      end;
    end;
  end;
  ScreenObjectName := (ScreenObject as TScreenObject).Name;
  ErrorMessage := Format(IDError, [ScreenObjectName, StartTime]);
//  ErrorMessage := 'Object = ' + ScreenObjectName
//    + '; Time = ' + FloatToStr(StartTime);
  frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
    StrIncompleteSFRData, ErrorMessage);
end;

class function TSfrEquationCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TSfrEquationItem;
end;

procedure TSfrEquationCollection.SetChannelTimeValues(Index: integer;
  const Value: TSfrEquationRecord);
begin
  Assert((Index >= 0) and (Index < Length(FTimeValues)));
  FTimeValues[Index] := Value;
end;

end.

