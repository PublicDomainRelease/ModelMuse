unit LayerStructureUnit;

interface

uses OrderedCollectionUnit, Classes, GoPhastTypes;

type
  TIntTypeMethod = (itmLaytype, itmLayavg, itmLayvka);

  TLayerCollection = class;

  TLayerFraction = class(TCollectionItem)
  private
    FFraction: real;
    procedure InvalidateModel;
    procedure SetFraction(const Value: real);
    function IsSame(AnotherLayerFraction: TLayerFraction): boolean;
  public
    function Collection: TLayerCollection;
    procedure Assign(Source: TPersistent); override;
  published
    property Fraction: real read FFraction write SetFraction;
  end;

  TLayerGroup = class;

  TLayerCollection  = class(TCollection)
  Private
    FLayerGroup: TLayerGroup;
    procedure InvalidateModel;
    procedure Sort;
  public
    function IsSame(AnotherLayerCollection: TLayerCollection): boolean;
    procedure Assign(Source: TPersistent); override;
    constructor Create(LayerGroup: TLayerGroup);
  end;

  TLayerStructure = class;

  TLayerGroup = class(TOrderedItem)
  private
    FDataArrayName: string;
    FAquiferName: string;
    FGrowthMethod: TGrowthMethod;
    FGrowthRate: Real;
    FLayerCollection: TLayerCollection;
    FSimulated: boolean;
    FAquiferType: integer;
    FInterblockTransmissivityMethod: integer;
    FVerticalHydraulicConductivityMethod: integer;
    FUseStartingHeadForSaturatedThickness: boolean;
    procedure SetDataArrayName(const NewName : string);
    procedure SetAquiferName(const Value : string);
    procedure SetGrowthMethod(const Value: TGrowthMethod);
    procedure SetGrowthRate(const Value: Real);
    procedure SetLayerCollection(const Value: TLayerCollection);
    procedure SetSimulated(const Value: boolean);
    procedure SetAquiferType(const Value: integer);
    procedure Loaded;
    procedure SetInterblockTransmissivityMethod(const Value: integer);
    procedure SetVerticalHydraulicConductivityMethod(const Value: integer);
    function Collection: TLayerStructure;
    procedure SetUseStartingHeadForSaturatedThickness(const Value: boolean);
    function GetSimulated: boolean;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    function LayerCount: integer;
    function ModflowLayerCount: integer;
    procedure WriteLAYCB(const DiscretizationWriter: TObject);
  published
    property AquiferName : string read FAquiferName write SetAquiferName;
    // @name can take on the following values:
    // @unorderedlist(
    //   @item(0, confined)
    //   @item 1, unconfined)
    //  )
    property AquiferType: integer read FAquiferType write SetAquiferType;
    property DataArrayName :  string read FDataArrayName write SetDataArrayName;
    property GrowthMethod: TGrowthMethod read FGrowthMethod
      write SetGrowthMethod;
    property GrowthRate: Real read FGrowthRate write SetGrowthRate;
    property LayerCollection: TLayerCollection read FLayerCollection
      write SetLayerCollection stored FSimulated;
    property Simulated: boolean read GetSimulated write SetSimulated;
    property InterblockTransmissivityMethod: integer
      read FInterblockTransmissivityMethod
      write SetInterblockTransmissivityMethod;
    property VerticalHydraulicConductivityMethod: integer
      read FVerticalHydraulicConductivityMethod
      write SetVerticalHydraulicConductivityMethod;
    property UseStartingHeadForSaturatedThickness: boolean
      read FUseStartingHeadForSaturatedThickness
      write SetUseStartingHeadForSaturatedThickness;
  end;

  TLayerStructure = class(TLayerOwnerCollection)
  Private
    function GetLayerGroup(const Index: integer): TLayerGroup;
    function IntegerArray(Method: TIntTypeMethod): TOneDIntegerArray;
  public
    procedure AssignAssociatedInputDataSets;
    procedure Assign(Source: TPersistent);override;
    constructor Create(Model: TObject);
    property LayerGroups[const Index: integer]: TLayerGroup
      read GetLayerGroup; default;
    function LayerCount: integer;
    procedure Loaded;
    function ModflowLayerCount: integer;
    function ModflowConfiningBedCount: integer;
    procedure WriteLAYCB(const DiscretizationWriter: TObject);
    function ModflowLayerBottomDescription(const LayerID: integer): string;
    // @name returns true if a layer in the grid is simulated
    // LayerID is zero-based.
    function IsLayerSimulated(const LayerID: integer): boolean;
    Function Laytyp: TOneDIntegerArray;
    Function Layavg: TOneDIntegerArray;
    function Chani: TOneDIntegerArray;
    Function Layvka: TOneDIntegerArray;
    // @name converts a MODFLOW model layer (starting at 1) to the
    // appropriate index in a 3D data array;
    Function ModflowLayerToDataSetLayer(ModflowLayer: integer): integer;
    function DataSetLayerToModflowLayer(DataSetLayer: integer): integer;
    //  @name returns the @link(TLayerGroup) that contains Layer.
    // Layer is zero-based.
    function GetLayerGroupByLayer(const Layer: integer): TLayerGroup;
  end;

resourcestring
  StrLayerDefinition = 'Layer Definition';

implementation

uses SysUtils, Math, RbwParser, PhastModelUnit, DataSetUnit,
  ModflowDiscretizationWriterUnit;

procedure TLayerGroup.Assign(Source: TPersistent);
var
  AnotherLayerGroup: TLayerGroup;
begin
  inherited;
  AnotherLayerGroup := Source as TLayerGroup;
  if not IsSame(AnotherLayerGroup) then
  begin
    // It is important for AquiferName to be assigned after DataArrayName
    // because if AquiferName has changed, DataArrayName will
    // need to be changed too and that change should not be overwritten
    // by another assignment to DataArrayName.
    if AnotherLayerGroup.DataArrayName <> '' then
    begin
      DataArrayName := AnotherLayerGroup.DataArrayName;
    end;
    AquiferName := AnotherLayerGroup.AquiferName;
    GrowthMethod := AnotherLayerGroup.GrowthMethod;
    GrowthRate := AnotherLayerGroup.GrowthRate;
    AquiferType := AnotherLayerGroup.AquiferType;
    Simulated := AnotherLayerGroup.Simulated;
    LayerCollection := AnotherLayerGroup.LayerCollection;
    InterblockTransmissivityMethod :=
      AnotherLayerGroup.InterblockTransmissivityMethod;
    VerticalHydraulicConductivityMethod :=
      AnotherLayerGroup.VerticalHydraulicConductivityMethod;
    UseStartingHeadForSaturatedThickness :=
      AnotherLayerGroup.UseStartingHeadForSaturatedThickness;
  end;
end;

function TLayerGroup.Collection: TLayerStructure;
begin
  result := inherited Collection as TLayerStructure;
end;

constructor TLayerGroup.Create(Collection: TCollection);
begin
  inherited;
  FLayerCollection:= TLayerCollection.Create(self);
  AquiferName := 'New Layer Group';
  FGrowthRate := 1.2;
  FSimulated := True;
end;

destructor TLayerGroup.Destroy;
var
  Model: TPhastModel;
//  DataArrayIndex: Integer;
  DataArray: TDataArray;
begin
  if Collection.Model <> nil then
  begin
    Model := Collection.Model as TPhastModel;
    if not (csDestroying in Model.ComponentState) then
    begin
      DataArray := Model.GetDataSetByName(FDataArrayName);
      if DataArray <> nil then
      begin
  //      DataArray := Model.DataSets[DataArrayIndex];
        DataArray.Lock := [];
      end;
    end;
  end;
  FLayerCollection.Free;
  inherited;
end;

function TLayerGroup.GetSimulated: boolean;
var
  PhastModel: TPhastModel;
begin
  result := FSimulated;
  if not result then
  begin
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil) then
      if (PhastModel.ModflowPackages <> nil) then
      if (PhastModel.ModflowPackages.HufPackage <> nil) then
      if PhastModel.ModflowPackages.HufPackage.IsSelected then
    begin
      result := True;
    end;
  end;
end;

function TLayerGroup.IsSame(AnotherItem: TOrderedItem): boolean;
var
  AnotherLayerGroup : TLayerGroup;
begin
  AnotherLayerGroup := AnotherItem as TLayerGroup;
  result := (AnotherLayerGroup.AquiferName = AquiferName)
    and (AnotherLayerGroup.DataArrayName = DataArrayName)
    and (AnotherLayerGroup.GrowthMethod = GrowthMethod)
    and (AnotherLayerGroup.GrowthRate = GrowthRate)
    and (AnotherLayerGroup.Simulated = Simulated)
    and (AnotherLayerGroup.AquiferType = AquiferType)
    and (AnotherLayerGroup.InterblockTransmissivityMethod =
      InterblockTransmissivityMethod)
    and (AnotherLayerGroup.VerticalHydraulicConductivityMethod =
      VerticalHydraulicConductivityMethod)
    and (AnotherLayerGroup.UseStartingHeadForSaturatedThickness =
      UseStartingHeadForSaturatedThickness)
    and AnotherLayerGroup.LayerCollection.IsSame(LayerCollection);
end;

function TLayerGroup.LayerCount: integer;
begin
  if Simulated then
  begin
    result := LayerCollection.Count + 1;
  end
  else
  begin
    result := 1;
  end;
end;

procedure TLayerGroup.Loaded;
var
  Model: TPhastModel;
//  DataArrayIndex: Integer;
  DataArray: TDataArray;
begin
  Model := Collection.Model as TPhastModel;
  DataArray := Model.GetDataSetByName(FDataArrayName);
  Assert( DataArray <> nil);
//  DataArray := Model.DataSets[DataArrayIndex];
  Model.TopGridObserver.TalksTo(DataArray);
  DataArray.TalksTo(Model.ThreeDGridObserver);
  Model.ThreeDGridObserver.StopsTalkingTo(DataArray);
end;

function TLayerGroup.ModflowLayerCount: integer;
begin
  if Simulated then
  begin
    result := LayerCollection.Count + 1;
  end
  else
  begin
    result := 0;
  end;
end;

procedure TLayerGroup.SetAquiferName(const Value : string);
begin
  Assert(Value <> '');
  if FAquiferName <> Value then
  begin
    if Collection.Model <> nil then
    begin
      if UpperCase(FAquiferName) = UpperCase(Value) then
      begin
        DataArrayName := StringReplace(DataArrayName,
          GenerateNewRoot(FAquiferName),GenerateNewRoot(Value), []);
      end
      else
      begin
        if Index = 0 then
        begin
          DataArrayName := GenerateNewName(Value);
        end
        else
        begin
          DataArrayName := GenerateNewName(Value + '_Bottom');
        end;
      end;
    end;
    FAquiferName := Value;
    InvalidateModel;
  end;
end;

procedure TLayerGroup.SetAquiferType(const Value: integer);
begin
  if FAquiferType <> Value then
  begin
    Assert(Value  in [0..3]);
    FAquiferType := Value;
    InvalidateModel;
  end;
end;

procedure TLayerGroup.SetDataArrayName(const NewName : string);
var
  Model: TPhastModel;
//  DataArrayIndex: integer;
  DataArray: TDataArray;
  UnitAbove, UnitBelow: TLayerGroup;
  NewFormula: string;
  Compiler: TRbwParser;
  Position: Integer;
begin
  if FDataArrayName <> NewName then
  begin
    if Collection.Model <> nil then
    begin
      Model := Collection.Model as TPhastModel;
      if not (csLoading in Model.ComponentState) then
      begin
        DataArray := Model.GetDataSetByName(FDataArrayName);
        if DataArray <> nil then
        begin
//          DataArray := Model.DataSets[DataArrayIndex];
          Model.TopGridObserver.StopsTalkingTo(DataArray);
          DataArray.StopsTalkingTo(Model.ThreeDGridObserver);
          DataArray.Name := NewName;
          Compiler := Model.GetCompiler(DataArray.Orientation,
            DataArray.EvaluatedAt);
          Position := Compiler.IndexOfVariable(FDataArrayName);
          if Position >= 0 then
          begin
            Compiler.RenameVariable(Position, NewName);
          end;
          Compiler := Model.GetCompiler(dso3D,
            DataArray.EvaluatedAt);
          Position := Compiler.IndexOfVariable(FDataArrayName);
          if Position >= 0 then
          begin
            Compiler.RenameVariable(Position, NewName);
          end;
        end
        else
        begin
          if Collection.Count = 1 then
          begin
            NewFormula := '0';
          end
          else if Index <= 0 then
          begin
            // No unit can be inserted above the top of the model.
            Assert(False);
          end
          else if Index = Collection.Count -1 then
          begin
            UnitAbove := Collection.Items[Index-1] as TLayerGroup;
            NewFormula := UnitAbove.DataArrayName + ' - 1';
          end
          else
          begin
            UnitAbove := Collection.Items[Index-1] as TLayerGroup;
            UnitBelow := Collection.Items[Index+1] as TLayerGroup;
            NewFormula := '(' + UnitAbove.DataArrayName + ' + '
              + UnitBelow.DataArrayName + ') / 2';
          end;

          DataArray := Model.CreateNewDataArray(TDataArray, NewName, NewFormula,
            [dcName, dcType, dcOrientation, dcEvaluatedAt], rdtDouble,
            eaBlocks, dsoTop, StrLayerDefinition);
          DataArray.OnDataSetUsed := Model.ModelLayerDataArrayUsed;

          Collection.AddOwnedDataArray(DataArray);
        end;
        Model.TopGridObserver.TalksTo(DataArray);
        DataArray.TalksTo(Model.ThreeDGridObserver);
        Model.ThreeDGridObserver.StopsTalkingTo(DataArray);

        DataArray.UpdateDimensions(Model.Grid.LayerCount,
          Model.Grid.RowCount, Model.Grid.ColumnCount);
      end;
    end;
    FDataArrayName := NewName;
    InvalidateModel;
  end;
end;


procedure TLayerGroup.SetGrowthMethod(const Value: TGrowthMethod);
begin
  if FGrowthMethod <> Value then
  begin
    FGrowthMethod := Value;
    InvalidateModel;
  end;
end;

procedure TLayerGroup.SetGrowthRate(const Value: Real);
begin
  if FGrowthRate <> Value then
  begin
    FGrowthRate := Value;
    InvalidateModel;
  end;
end;

procedure TLayerGroup.SetInterblockTransmissivityMethod(const Value: integer);
begin
  if FInterblockTransmissivityMethod <> Value then
  begin
    FInterblockTransmissivityMethod := Value;
    InvalidateModel;
  end;
end;

procedure TLayerGroup.SetLayerCollection(const Value: TLayerCollection);
begin
  FLayerCollection.Assign(Value);
end;

procedure TLayerGroup.SetSimulated(const Value: boolean);
var
  PhastModel: TPhastModel;
begin
  if FSimulated <> Value then
  begin
    FSimulated := Value;
    PhastModel := Model as TPhastModel;
    if PhastModel <> nil then
    begin
      PhastModel.InvalidateModflowBoundaries;
    end;
    InvalidateModel;
  end;
end;

procedure TLayerGroup.SetUseStartingHeadForSaturatedThickness(
  const Value: boolean);
begin
  if FUseStartingHeadForSaturatedThickness <> Value then
  begin
    FUseStartingHeadForSaturatedThickness := Value;
    InvalidateModel;
  end;
end;

procedure TLayerGroup.SetVerticalHydraulicConductivityMethod(
  const Value: integer);
begin
  if FVerticalHydraulicConductivityMethod <> Value then
  begin
    FVerticalHydraulicConductivityMethod := Value;
    InvalidateModel;
  end;
end;

procedure TLayerGroup.WriteLAYCB(const DiscretizationWriter: TObject);
var
  DisWriter: TModflowDiscretizationWriter;
  Index: integer;
begin
  DisWriter := DiscretizationWriter as TModflowDiscretizationWriter;
  if Simulated then
  begin
    for Index := 0 to LayerCollection.Count-1 do
    begin
      DisWriter.WriteInteger(0);
    end;
  end
  else
  begin
    DisWriter.WriteInteger(1);
  end;
end;

{ TLayerStructure }
procedure TLayerStructure.Assign(Source: TPersistent);
var
  PhastModel: TPhastModel;
begin
  inherited;
  if Model <> nil then
  begin
    PhastModel := Model as TPhastModel;
    PhastModel.Grid.LayerCount := LayerCount;
    PhastModel.InvalidateMfEtsEvapLayer(self);
    PhastModel.InvalidateMfEvtEvapLayer(self);
    PhastModel.InvalidateMfRchLayer(self);
    AssignAssociatedInputDataSets;
  end;
end;

function TLayerStructure.Chani: TOneDIntegerArray;
var
  Index: Integer;
begin
  SetLength(result, ModflowLayerCount);
  for Index := 0 to Length(result) - 1 do
  begin
    result[Index] := -1;
  end;
end;

constructor TLayerStructure.Create(Model: TObject);
begin
  inherited Create(TLayerGroup, Model);
end;

function TLayerStructure.GetLayerGroup(const Index: integer): TLayerGroup;
begin
  result := Items[Index] as TLayerGroup;
end;

function TLayerStructure.GetLayerGroupByLayer(
  const Layer: integer): TLayerGroup;
var
  HufUnitIndex: Integer;
  HufUnit: TLayerGroup;
  LocalLayerCount : integer;
begin
  Assert(Layer >= 0);
  result := nil;
  LocalLayerCount := 0;
  for HufUnitIndex := 1 to Count - 1 do
  begin
    HufUnit := LayerGroups[HufUnitIndex];
    LocalLayerCount := LocalLayerCount + HufUnit.LayerCount;
    if LocalLayerCount > Layer then
    begin
      result := HufUnit;
      Exit;
    end;
  end;
end;

function TLayerStructure.IsLayerSimulated(const LayerID: integer): boolean;
var
  LayerGroup: TLayerGroup;
  LayerGroupIndex: integer;
  LayerCount: integer;
begin
  Assert(LayerID >= 0);
  LayerCount := 0;
  result := False;
  for LayerGroupIndex := 1 to Count - 1 do
  begin
    LayerGroup := LayerGroups[LayerGroupIndex];
    LayerCount := LayerCount + LayerGroup.LayerCollection.Count +1;
    if LayerCount >= LayerID+1 then
    begin
      result := LayerGroup.Simulated;
      Exit;
    end;
  end;
end;

function TLayerStructure.IntegerArray(Method: TIntTypeMethod):
  TOneDIntegerArray;
var
  LayerCount: integer;
  GroupIndex: Integer;
  Group: TLayerGroup;
  LayerIndex: Integer;
  MFLayIndex: integer;
  PhastModel: TPhastModel;
begin
  PhastModel := Model as TPhastModel;
  LayerCount := ModflowLayerCount;
  SetLength(result, LayerCount);
  MFLayIndex := 0;
  for GroupIndex := 1 to Count - 1 do
  begin
    Group := LayerGroups[GroupIndex];
    if Group.Simulated then
    begin
      for LayerIndex := 0 to Group.ModflowLayerCount - 1 do
      begin
        Assert(MFLayIndex < LayerCount);
        case Method of
          itmLaytype:
            begin
              result[MFLayIndex] := Group.AquiferType;
              if (Group.AquiferType = 0) and
                Group.UseStartingHeadForSaturatedThickness
                and PhastModel.ModflowPackages.LpfPackage.isSelected
                and PhastModel.ModflowPackages.LpfPackage.UseSaturatedThickness then
              begin
                result[MFLayIndex] := -1;
              end;
            end;
          itmLayavg: result[MFLayIndex] := Group.InterblockTransmissivityMethod;
          itmLayvka: result[MFLayIndex] :=
            Group.VerticalHydraulicConductivityMethod;
          else Assert(False);
        end;
        Inc(MFLayIndex);
      end;
    end;
  end;
  Assert(MFLayIndex = LayerCount);
end;

function TLayerStructure.Layavg: TOneDIntegerArray;
begin
  result := IntegerArray(itmLayavg);
end;

function TLayerStructure.LayerCount: integer;
var
  Index: Integer;
  LayerGroup: TLayerGroup;
begin
  result := 0;
  // Skip the top of the model: it doesn't count.
  for Index := 1 to Count - 1 do
  begin
    LayerGroup := Items[Index] as TLayerGroup;
    result := result + LayerGroup.LayerCount;
  end;
end;

function TLayerStructure.Laytyp: TOneDIntegerArray;
begin
  result := IntegerArray(itmLaytype);
end;

function TLayerStructure.Layvka: TOneDIntegerArray;
begin
  result := IntegerArray(itmLayvka);
end;

procedure TLayerStructure.Loaded;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    LayerGroups[Index].Loaded;
  end;
  AssignAssociatedInputDataSets;
end;

function TLayerStructure.ModflowLayerBottomDescription(
  const LayerID: integer): string;
var
  LayerGroup: TLayerGroup;
  LayerGroupIndex: integer;
  LayerCount: integer;
  LayerNumber: integer;
begin
  Assert(LayerID >= 0);
  LayerCount := 0;
  result := '';
  LayerNumber := LayerID+1;
  for LayerGroupIndex := 1 to Count - 1 do
  begin
    LayerGroup := LayerGroups[LayerGroupIndex];
    LayerCount := LayerCount + LayerGroup.LayerCollection.Count +1;
    if LayerCount >= LayerID+1 then
    begin
      result := LayerGroup.AquiferName;
      if LayerGroup.LayerCollection.Count > 0 then
      begin
        result := result + ' Layer ' + IntToStr(LayerNumber);
      end;
      Exit;
    end;
    LayerNumber := LayerID+1 - LayerCount;
  end;
end;

function TLayerStructure.ModflowConfiningBedCount: integer;
var
  Index: Integer;
  LayerGroup: TLayerGroup;
begin
  result := 0;
  // Skip the top of the model: it doesn't count.
  for Index := 1 to Count - 1 do
  begin
    LayerGroup := Items[Index] as TLayerGroup;
    if not LayerGroup.Simulated then
    begin
      Inc(result);
    end;
  end;
end;

function TLayerStructure.ModflowLayerCount: integer;
var
  Index: Integer;
  LayerGroup: TLayerGroup;
begin
  result := 0;
  // Skip the top of the model: it doesn't count.
  for Index := 1 to Count - 1 do
  begin
    LayerGroup := Items[Index] as TLayerGroup;
    result := result + LayerGroup.ModflowLayerCount;
  end;
end;

function TLayerStructure.DataSetLayerToModflowLayer(
  DataSetLayer: integer): integer;
var
  GroupIndex: Integer;
  Group: TLayerGroup;
  GridLayerCount: integer;
begin
  GridLayerCount := 0;
  result := 0;
  for GroupIndex := 1 to Count - 1 do
  begin
    Group := LayerGroups[GroupIndex];
    if Group.Simulated then
    begin
      GridLayerCount := GridLayerCount + Group.ModflowLayerCount;
      result := result + Group.ModflowLayerCount;
    end
    else
    begin
      GridLayerCount := GridLayerCount + 1;
    end;
    if GridLayerCount >= DataSetLayer then
    begin
      result := result - (GridLayerCount-DataSetLayer)+1;
      Exit;
    end;
  end;
  Assert(False);
end;

procedure TLayerStructure.AssignAssociatedInputDataSets;
var
  Index: Integer;
  PhastModel: TPhastModel;
  Name: string;
  DataSet: TDataArray;
begin
  PhastModel := Model as TPhastModel;
  Assert(PhastModel <> nil);
  for Index := 0 to Count - 1 do
  begin
    Name := LayerGroups[Index].DataArrayName;
    DataSet := PhastModel.GetDataSetByName(Name);
    if Index = 0 then
    begin
      DataSet.AssociatedDataSets := 'MODFLOW DIS: Top';
    end
    else
    begin
      DataSet.AssociatedDataSets := 'MODFLOW DIS: BOTM';
    end;
  end;
end;

function TLayerStructure.ModflowLayerToDataSetLayer(
  ModflowLayer: integer): integer;
var
  GroupIndex: Integer;
  Group: TLayerGroup;
  LayerCount: integer;
begin
  Assert((ModflowLayer >= 1) and (ModflowLayer <= ModflowLayerCount));
  LayerCount := 0;
  result := -1;
  for GroupIndex := 1 to Count - 1 do
  begin
    Group := LayerGroups[GroupIndex];
    LayerCount := LayerCount + Group.ModflowLayerCount;
    if Group.Simulated then
    begin
      result := result + Group.ModflowLayerCount;
    end
    else
    begin
      result := result + 1;
    end;

    if LayerCount >= ModflowLayer then
    begin
      result := result - (LayerCount - ModflowLayer);
      Exit;
    end;
  end;
  Assert(False);
end;

procedure TLayerStructure.WriteLAYCB(const DiscretizationWriter: TObject);
var
  Index: Integer;
  LayerGroup: TLayerGroup;
  DisWriter: TModflowDiscretizationWriter;
  ItemIndex: Integer;
begin
  DisWriter := DiscretizationWriter as TModflowDiscretizationWriter;
  // Skip the top of the model: it doesn't count.
  for Index := 1 to Count - 1 do
  begin
//    LayerGroup.WriteLAYCB(DisWriter);
    LayerGroup := Items[Index] as TLayerGroup;
    if LayerGroup.Simulated then
    begin
      for ItemIndex := 0 to LayerGroup.LayerCollection.Count-1 do
      begin
        DisWriter.WriteInteger(0);
      end;
      if Index < Count - 1 then
      begin
        LayerGroup := Items[Index+1] as TLayerGroup;
        if LayerGroup.Simulated then
        begin
          DisWriter.WriteInteger(0);
        end
        else
        begin
          DisWriter.WriteInteger(1);
        end;
      end
      else
      begin
        DisWriter.WriteInteger(0);
      end;
    end;
  end;
  DisWriter.WriteString(' # LAYCB');
  DisWriter.NewLine;
end;

{ TLayerFraction }

procedure TLayerFraction.Assign(Source: TPersistent);
begin
  Fraction := (Source as TLayerFraction).Fraction;
end;

function TLayerFraction.Collection: TLayerCollection;
begin
  result := inherited Collection as TLayerCollection;
end;

procedure TLayerFraction.InvalidateModel;
begin
  Collection.InvalidateModel;
end;

function TLayerFraction.IsSame(AnotherLayerFraction: TLayerFraction): boolean;
begin
  result := Fraction = AnotherLayerFraction.Fraction;
end;

procedure TLayerFraction.SetFraction(const Value: real);
begin
  if FFraction <> Value then
  begin
    FFraction := Value;
    InvalidateModel;
  end;
end;

{ TLayerCollection }

procedure TLayerCollection.Assign(Source: TPersistent);
begin
  if not IsSame(Source as TLayerCollection) then
  begin
    inherited;
  end;
  Sort;
end;

constructor TLayerCollection.Create(LayerGroup: TLayerGroup);
begin
  inherited Create(TLayerFraction);
  Assert(LayerGroup <> nil);
  FLayerGroup := LayerGroup;
end;

procedure TLayerCollection.InvalidateModel;
begin
  FLayerGroup.InvalidateModel;
end;

function TLayerCollection.IsSame(
  AnotherLayerCollection: TLayerCollection): boolean;
var
  Index: Integer;
begin
  result := Count = AnotherLayerCollection.Count;
  if result then
  begin
    for Index := 0 to Count - 1 do
    begin
      result := (Items[Index] as TLayerFraction).IsSame(
        AnotherLayerCollection.Items[Index] as TLayerFraction);
      if not result then Exit;
    end;
  end;
end;

function CompareLayerFractions(Item1, Item2: Pointer): Integer;
var
  Frac1, Frac2: TLayerFraction;
begin
  Frac1 := Item1;
  Frac2 := Item2;
  result := Sign(Frac2.Fraction - Frac1.Fraction);
end;

procedure TLayerCollection.Sort;
var
  List: TList;
  Index: integer;
  Item: TLayerFraction;
begin
  List := TList.Create;
  try
    for Index := 0 to Count - 1 do
    begin
      List.Add(Items[Index]);
    end;
    List.Sort(CompareLayerFractions);
    for Index := 0 to List.Count - 1 do
    begin
      Item := List[Index];
      Item.Index := Index;
    end;
  finally
    List.Free;
  end;
end;

end.