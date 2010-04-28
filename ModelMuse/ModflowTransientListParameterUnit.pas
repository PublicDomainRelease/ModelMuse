unit ModflowTransientListParameterUnit;

interface

uses SysUtils, Classes, OrderedCollectionUnit, ModflowParameterUnit;

type
  TModflowTransientListParameters = class;

  TModflowTransientListParameter = class(TModflowParameter)
  private
    function Collection: TModflowTransientListParameters;
  protected
    procedure SetParameterName(const Value: string); override;
    procedure SetParameterType(const Value: TParameterType); override;
    procedure SetValue(Value : double); override;
  public
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
  end;

  TModflowTransientListParameters = class(TEnhancedOrderedCollection)
  private
    function GetItems(Index: integer): TModflowTransientListParameter;
    procedure SetItems(Index: integer;
      const Value: TModflowTransientListParameter);
  public
    procedure UpdateDisplay(Value: TModflowTransientListParameter);
    constructor Create(Model: TObject);
    property Items[Index: integer]: TModflowTransientListParameter
      read GetItems write SetItems; default;
    function GetParamByName(
      const ParamName: string): TModflowTransientListParameter;
    function CountParam(ParameterType: TParameterType): integer;
  end;

implementation

uses PhastModelUnit, ScreenObjectUnit, ModflowBoundaryUnit,
  ModflowSfrParamIcalcUnit;

{ TModflowTransientListParameter }


procedure TModflowTransientListParameter.Assign(Source: TPersistent);
var
  SourceParam: TModflowTransientListParameter;
  ParmChanged: boolean;
begin
  if Source is TModflowTransientListParameter then
  begin
    SourceParam := TModflowTransientListParameter(Source);
    ParmChanged := not IsSame(SourceParam);
    if ParmChanged then
    begin
      Collection.UpdateDisplay(self);
    end;
    inherited;
    if ParmChanged then
    begin
      Collection.UpdateDisplay(SourceParam);
    end;
  end
  else
  begin
    inherited;
  end;
end;

function TModflowTransientListParameter.Collection: TModflowTransientListParameters;
begin
  result := inherited Collection as TModflowTransientListParameters;
end;

destructor TModflowTransientListParameter.Destroy;
begin
  (Collection as TModflowTransientListParameters).UpdateDisplay(self);
  inherited;
end;

procedure TModflowTransientListParameter.SetParameterName(const Value: string);
var
  Model: TPhastModel;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ParamIndex: integer;
  Boundary: TModflowParamBoundary;
  NameIndex: Integer;
  Item: TSfrParamIcalcItem;
  NewName: string;
begin
  NewName := CorrectParamName(Value);
  if FParameterName <> NewName then
  begin
    if ParameterType in [ptRCH,ptEVT,ptETS, ptCHD,ptGHB,ptQ,ptRIV,ptDRN,ptDRT,ptSFR] then
    begin
      if Collection.Model <> nil then
      begin
        Model := Collection.Model as TPhastModel;
        for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
        begin
          ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
          if ParameterType = ptSFR then
          begin
            if ScreenObject.ModflowSfrBoundary <> nil then
            begin
              for NameIndex := 0 to ScreenObject.ModflowSfrBoundary.
                ParamIcalc.Count - 1 do
              begin
                Item := ScreenObject.ModflowSfrBoundary.
                  ParamIcalc.Items[NameIndex];
                if Item.Param = FParameterName then
                begin
                  Item.Param := NewName
                end;
              end;
            end;
          end
          else
          begin
            Boundary := ScreenObject.GetMfBoundary(ParameterType);
            if Boundary <> nil then
            begin
              ParamIndex := Boundary.Parameters.IndexOfParam(self);
              if ParamIndex >= 0 then
              begin
                Boundary.Parameters[
                  ParamIndex].Param.ParamName := NewName;
              end;
            end;
          end;
        end;
      end;
    end;
    FParameterName := NewName;
    InvalidateModel;
  end;
end;

procedure TModflowTransientListParameter.SetParameterType(
  const Value: TParameterType);
begin
  inherited;
  (Collection as TModflowTransientListParameters).UpdateDisplay(self);
end;

procedure TModflowTransientListParameter.SetValue(Value: double);
begin
  inherited;
  (Collection as TModflowTransientListParameters).UpdateDisplay(self);
end;

{ TModflowTransientListParameters }

function TModflowTransientListParameters.CountParam(
  ParameterType: TParameterType): integer;
var
  Index: Integer;
  Item: TModflowTransientListParameter;
begin
  result := 0;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if Item.ParameterType = ParameterType then
    begin
      Inc(result);
    end;
  end;
end;

constructor TModflowTransientListParameters.Create(Model: TObject);
begin
  inherited Create(TModflowTransientListParameter, Model);
end;

function TModflowTransientListParameters.GetItems(
  Index: integer): TModflowTransientListParameter;
begin
  result := inherited Items[Index] as TModflowTransientListParameter;
end;

function TModflowTransientListParameters.GetParamByName(
  const ParamName: string): TModflowTransientListParameter;
var
  Index: Integer;
  Item: TModflowTransientListParameter;
begin
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if SameText(Item.ParameterName, ParamName) then
    begin
      result := Item;
      Exit;
    end;
  end;
end;

procedure TModflowTransientListParameters.UpdateDisplay(
  Value: TModflowTransientListParameter);
begin
  case Value.ParameterType of
    ptUndefined: ;
    ptLPF_HK, ptLPF_HANI, ptLPF_VK, ptLPF_VANI, ptLPF_SS, ptLPF_SY, ptLPF_VKCB:
      begin
        Assert(False);
      end;
    ptRCH:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfRchRate(self);
        end;
      end;
    ptEVT:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfEvtEvapRate(self);
        end;
      end;
    ptETS:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfEtsEvapRate(self);
        end;
      end;
    ptCHD:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfChdStartingHead(self);
          (Model as TPhastModel).InvalidateMfChdEndingHead(self);
        end;
      end;
    ptGHB:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfGhbConductance(self);
        end;
      end;
    ptQ:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfWellPumpage(self);
        end;
      end;
    ptRIV:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfRivConductance(self);
        end;
      end;
    ptDRN:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfDrnConductance(self);
        end;
      end;
    ptDRT: 
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfDrtConductance(self);
        end;
      end;
    ptSfr:
      begin
        if Model <> nil then
        begin
          (Model as TPhastModel).InvalidateMfSfrData(self);
        end;
      end;
    else Assert(False);
  end;
end;

procedure TModflowTransientListParameters.SetItems(Index: integer;
  const Value: TModflowTransientListParameter);
var
  ParmChanged: boolean;
begin
  ParmChanged := not Items[Index].IsSame(Value);
  if ParmChanged then
  begin
    UpdateDisplay(Items[Index]);
  end;
  inherited Items[Index] := Value;
  if ParmChanged then
  begin
    UpdateDisplay(Value);
  end;
end;

end.
