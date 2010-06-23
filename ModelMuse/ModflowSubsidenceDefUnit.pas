unit ModflowSubsidenceDefUnit;

interface

uses
  Classes, GoPhastTypes, OrderedCollectionUnit, SysUtils;

type
  TUseLayerNumberItem = class(TOrderedItem)
  private
    FLayerNumber: integer;
    procedure SetLayerNumber(const Value: integer);
  public
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure Assign(Source: TPersistent); override;
  published
    property LayerNumber: integer read FLayerNumber write SetLayerNumber;
  end;

  TUseLayersCollection = class(TOrderedCollection)
  private
    function GetItem(Index: integer): TUseLayerNumberItem;
    procedure SetItem(Index: integer; const Value: TUseLayerNumberItem);
  public
    function Add: TUseLayerNumberItem;
    Constructor Create(Model: TObject);
    property Items[Index: integer]: TUseLayerNumberItem read GetItem
      write SetItem; default;
    function GetItemByLayerNumber(LayerNumber: integer): TUseLayerNumberItem;
  end;

  TCustomSubLayerItem = class(TOrderedItem)
  private
    FUsedLayers: TUseLayersCollection;
    FUseInAllLayers: boolean;
    FName: string;
    FAssociatedModelDataSetNames: TStringList;
    procedure SetName(const Value: string);
    procedure SetUsedLayers(const Value: TUseLayersCollection);
    procedure SetUseInAllLayers(const Value: boolean);
  protected
    FDataArrayTypes: TStringList;
    procedure UpdateArrayNames(NewNames: TStringList); virtual; abstract;
    procedure UpdateAssociatedDataSetNames(NewNames: TStringList);
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure SetDataArrayName(var OldName: string; NewName: string);
    procedure SetArrayNames(NewName: string);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  published
    property UseInAllLayers: boolean read FUseInAllLayers
      write SetUseInAllLayers;
    property UsedLayers: TUseLayersCollection read FUsedLayers
      write SetUsedLayers;
    property Name: string read FName write SetName;
  end;

  TSubNoDelayBedLayerItem = class(TCustomSubLayerItem)
  private
    FElasticSkeletalStorageCoefficientDataArrayName: string;
    FInelasticSkeletalStorageCoefficientDataArrayName: string;
    FPreconsolidationHeadDataArrayName: string;
    FInitialCompactionDataArrayName: string;
    procedure SetElasticSkeletalStorageCoefficientDataArrayName(
      const Value: string);
    procedure SetInelasticSkeletalStorageCoefficientDataArrayName(
      const Value: string);
    procedure SetInitialCompactionDataArrayName(const Value: string);
    procedure SetPreconsolidationHeadDataArrayName(const Value: string);
    procedure Loaded;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure UpdateArrayNames(NewNames: TStringList); override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Collection: TCollection); override;
  published
    // data set 5. HC
    property PreconsolidationHeadDataArrayName: string
      read FPreconsolidationHeadDataArrayName
      write SetPreconsolidationHeadDataArrayName;
    // data set 6. Sfe
    property ElasticSkeletalStorageCoefficientDataArrayName: string
      read FElasticSkeletalStorageCoefficientDataArrayName
      write SetElasticSkeletalStorageCoefficientDataArrayName;
    // data set 7. Sfv
    property InelasticSkeletalStorageCoefficientDataArrayName: string
      read FInelasticSkeletalStorageCoefficientDataArrayName
      write SetInelasticSkeletalStorageCoefficientDataArrayName;
    // data set 8. Com
    property InitialCompactionDataArrayName: string
      read FInitialCompactionDataArrayName
      write SetInitialCompactionDataArrayName;
  end;

  // @name is defined in order to gain access to protected methods of
  // @link(TLayerOwnerCollection);
  TCustomSubLayer = class(TLayerOwnerCollection);

  TSubNoDelayBedLayers = class(TCustomSubLayer)
  private
    function GetItem(Index: integer): TSubNoDelayBedLayerItem;
    procedure SetItem(Index: integer; const Value: TSubNoDelayBedLayerItem);
  public
    function Add: TSubNoDelayBedLayerItem;
    constructor Create(Model: TObject);
    property Items[Index: integer]: TSubNoDelayBedLayerItem read GetItem
      write SetItem; default;
    procedure Loaded;
  end;

  TSubDelayBedLayerItem = class(TCustomSubLayerItem)
  private
    FInterbedPreconsolidationHeadDataArrayName: string;
    FElasticSpecificStorageDataArrayName: string;
    FInterbedStartingCompactionDataArrayName: string;
    FVerticalHydraulicConductivityDataArrayName: string;
    FInterbedEquivalentThicknessDataArrayName: string;
    FInterbedStartingHeadDataArrayName: string;
    FInelasticSpecificStorageDataArrayName: string;
    FEquivNumberDataArrayName: string;
    procedure SetElasticSpecificStorageDataArrayName(const Value: string);
    procedure SetEquivNumberDataArrayName(const Value: string);
    procedure SetInelasticSpecificStorageDataArrayName(const Value: string);
    procedure SetInterbedEquivalentThicknessDataArrayName(const Value: string);
    procedure SetInterbedPreconsolidationHeadDataArrayName(const Value: string);
    procedure SetInterbedStartingCompactionDataArrayName(const Value: string);
    procedure SetInterbedStartingHeadDataArrayName(const Value: string);
    procedure SetVerticalHydraulicConductivityDataArrayName(const Value: string);
    procedure Loaded;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Collection: TCollection); override;
    procedure UpdateArrayNames(NewNames: TStringList); override;
  published
    // data set 4. RNB
    property EquivNumberDataArrayName: string read FEquivNumberDataArrayName
      write SetEquivNumberDataArrayName;
    // data set 9. DP
    property VerticalHydraulicConductivityDataArrayName: string
      read FVerticalHydraulicConductivityDataArrayName
      write SetVerticalHydraulicConductivityDataArrayName;
    // data set 9. DP
    property ElasticSpecificStorageDataArrayName: string
      read FElasticSpecificStorageDataArrayName
      write SetElasticSpecificStorageDataArrayName;
    // data set 9. DP
    property InelasticSpecificStorageDataArrayName: string
      read FInelasticSpecificStorageDataArrayName
      write SetInelasticSpecificStorageDataArrayName;
    // data set 10. Dstart
    property InterbedStartingHeadDataArrayName: string
      read FInterbedStartingHeadDataArrayName
      write SetInterbedStartingHeadDataArrayName;
    // data set 11. DHC
    property InterbedPreconsolidationHeadDataArrayName: string
      read FInterbedPreconsolidationHeadDataArrayName
      write SetInterbedPreconsolidationHeadDataArrayName;
    // data set 12. DCOM
    property InterbedStartingCompactionDataArrayName: string
      read FInterbedStartingCompactionDataArrayName
      write SetInterbedStartingCompactionDataArrayName;
    // data set 13. DZ
    property InterbedEquivalentThicknessDataArrayName: string
      read FInterbedEquivalentThicknessDataArrayName
      write SetInterbedEquivalentThicknessDataArrayName;
  end;

  TSubDelayBedLayers = class(TCustomSubLayer)
  private
    function GetItem(Index: integer): TSubDelayBedLayerItem;
    procedure SetItem(Index: integer; const Value: TSubDelayBedLayerItem);
  public
    function Add: TSubDelayBedLayerItem;
    constructor Create(Model: TObject);
    property Items[Index: integer]: TSubDelayBedLayerItem read GetItem
      write SetItem; default;
    procedure Loaded;
  end;

const
  StrSubSidence = 'Subsidence';

implementation

uses
  PhastModelUnit, DataSetUnit, RbwParser, IntListUnit;

{ TSubNoDelayBedLayers }

procedure TSubNoDelayBedLayerItem.Assign(Source: TPersistent);
var
  SubSource: TSubNoDelayBedLayerItem;
begin
  inherited;
  if Source is TSubNoDelayBedLayerItem then
  begin
    SubSource := TSubNoDelayBedLayerItem(Source);
    PreconsolidationHeadDataArrayName := SubSource.PreconsolidationHeadDataArrayName;
    ElasticSkeletalStorageCoefficientDataArrayName :=
      SubSource.ElasticSkeletalStorageCoefficientDataArrayName;
    InelasticSkeletalStorageCoefficientDataArrayName :=
      SubSource.InelasticSkeletalStorageCoefficientDataArrayName;
    InitialCompactionDataArrayName := SubSource.InitialCompactionDataArrayName;
  end;
end;

constructor TSubNoDelayBedLayerItem.Create(Collection: TCollection);
begin
  inherited;
  FDataArrayTypes.Add('No_Delay_Preconsolidation_Head');
  FDataArrayTypes.Add('No_Delay_Elastic_Skeletal_Storage_Coefficient');
  FDataArrayTypes.Add('No_Delay_Inelastic_Skeletal_Storage_Coefficient');
  FDataArrayTypes.Add('No_Delay_Initial_Compaction');

  FAssociatedModelDataSetNames.Add('MODFLOW SUB: HC (Data Set 5)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: Sfe (Data Set 6)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: Sfv (Data Set 7)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: Com (Data Set 8)');
end;

function TSubNoDelayBedLayerItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SubItem: TSubNoDelayBedLayerItem;
begin
  result := (AnotherItem is TSubNoDelayBedLayerItem)
    and inherited IsSame(AnotherItem);
  if result then
  begin
     SubItem := TSubNoDelayBedLayerItem(AnotherItem);
     result :=
       (PreconsolidationHeadDataArrayName = SubItem.PreconsolidationHeadDataArrayName)
       and (ElasticSkeletalStorageCoefficientDataArrayName =
         SubItem.ElasticSkeletalStorageCoefficientDataArrayName)
       and (InelasticSkeletalStorageCoefficientDataArrayName =
         SubItem.InelasticSkeletalStorageCoefficientDataArrayName)
       and (InitialCompactionDataArrayName = SubItem.InitialCompactionDataArrayName)
  end;
end;

procedure TSubNoDelayBedLayerItem.Loaded;
var
  PhastModel: TPhastModel;
  Names: TStringList;
  procedure UpdateTalksTo(Const ArrayName: string);
  var
    DataArray: TDataArray;
  begin
    DataArray := PhastModel.GetDataSetByName(ArrayName);
    Assert( DataArray <> nil);
    PhastModel.TopGridObserver.TalksTo(DataArray);
    DataArray.OnDataSetUsed := PhastModel.SubsidenceDataArrayUsed;
  end;
begin
  PhastModel := Model as TPhastModel;
  UpdateTalksTo(PreconsolidationHeadDataArrayName);
  UpdateTalksTo(ElasticSkeletalStorageCoefficientDataArrayName);
  UpdateTalksTo(InelasticSkeletalStorageCoefficientDataArrayName);
  UpdateTalksTo(InitialCompactionDataArrayName);

  Names := TStringList.Create;
  try
    Names.Add(PreconsolidationHeadDataArrayName);
    Names.Add(ElasticSkeletalStorageCoefficientDataArrayName);
    Names.Add(InelasticSkeletalStorageCoefficientDataArrayName);
    Names.Add(InitialCompactionDataArrayName);
    UpdateAssociatedDataSetNames(Names);
  finally
    Names.Free;
  end;
end;

procedure TSubNoDelayBedLayerItem.SetElasticSkeletalStorageCoefficientDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FElasticSkeletalStorageCoefficientDataArrayName, Value);
end;

procedure TSubNoDelayBedLayerItem.
  SetInelasticSkeletalStorageCoefficientDataArrayName(const Value: string);
begin
  SetDataArrayName(FInelasticSkeletalStorageCoefficientDataArrayName, Value);
end;

procedure TSubNoDelayBedLayerItem.SetInitialCompactionDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInitialCompactionDataArrayName, Value);
end;

procedure TSubNoDelayBedLayerItem.SetPreconsolidationHeadDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FPreconsolidationHeadDataArrayName, Value);
end;

procedure TSubNoDelayBedLayerItem.UpdateArrayNames(NewNames: TStringList);
begin
  Assert(NewNames.Count= 4);
  PreconsolidationHeadDataArrayName := NewNames[0];
  ElasticSkeletalStorageCoefficientDataArrayName := NewNames[1];
  InelasticSkeletalStorageCoefficientDataArrayName := NewNames[2];
  InitialCompactionDataArrayName := NewNames[3];

  UpdateAssociatedDataSetNames(NewNames);
end;

{ TSubDelayBedLayers }

procedure TSubDelayBedLayerItem.Assign(Source: TPersistent);
var
  SubSource: TSubDelayBedLayerItem;
begin
  inherited;
  if Source is TSubDelayBedLayerItem then
  begin
    SubSource := TSubDelayBedLayerItem(Source);
    EquivNumberDataArrayName := SubSource.EquivNumberDataArrayName;
    VerticalHydraulicConductivityDataArrayName :=
      SubSource.VerticalHydraulicConductivityDataArrayName;
    ElasticSpecificStorageDataArrayName :=
      SubSource.ElasticSpecificStorageDataArrayName;
    InelasticSpecificStorageDataArrayName :=
      SubSource.InelasticSpecificStorageDataArrayName;
    InterbedStartingHeadDataArrayName :=
      SubSource.InterbedStartingHeadDataArrayName;
    InterbedPreconsolidationHeadDataArrayName :=
      SubSource.InterbedPreconsolidationHeadDataArrayName;
    InterbedStartingCompactionDataArrayName :=
      SubSource.InterbedStartingCompactionDataArrayName;
    InterbedEquivalentThicknessDataArrayName :=
      SubSource.InterbedEquivalentThicknessDataArrayName;
  end;
end;

constructor TSubDelayBedLayerItem.Create(Collection: TCollection);
begin
  inherited;
  FDataArrayTypes.Add('Delay_Equivalent_Number');
  FDataArrayTypes.Add('Delay_Vertical_Hydraulic_Conductivity');
  FDataArrayTypes.Add('Delay_Elastic_Specific_Storage');
  FDataArrayTypes.Add('Delay_Inelastic_Specific_Storage');
  FDataArrayTypes.Add('Delay_Interbed_Starting_Head');
  FDataArrayTypes.Add('Delay_Interbed_Preconsolidation_Head');
  FDataArrayTypes.Add('Delay_Interbed_Starting_Compaction');
  FDataArrayTypes.Add('Delay_Interbed_Equivalent_Thickness');

  FAssociatedModelDataSetNames.Add('MODFLOW SUB: RNB (Data Set 4)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: DP (Data Set 9); NZ (Data Set 14)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: DP (Data Set 9); NZ (Data Set 14)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: DP (Data Set 9); NZ (Data Set 14)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: Dstart (Data Set 10)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: DHC (Data Set 11)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: DCOM (Data Set 12)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: DZ (Data Set 13)');
end;

function TSubDelayBedLayerItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SubItem: TSubDelayBedLayerItem;
begin
  result := (AnotherItem is TSubDelayBedLayerItem)
    and inherited IsSame(AnotherItem);
  if result then
  begin
     SubItem := TSubDelayBedLayerItem(AnotherItem);
     result :=
       (EquivNumberDataArrayName = SubItem.EquivNumberDataArrayName)
       and (VerticalHydraulicConductivityDataArrayName =
         SubItem.VerticalHydraulicConductivityDataArrayName)
       and (ElasticSpecificStorageDataArrayName =
         SubItem.ElasticSpecificStorageDataArrayName)
       and (InelasticSpecificStorageDataArrayName =
         SubItem.InelasticSpecificStorageDataArrayName)
       and (InterbedStartingHeadDataArrayName =
         SubItem.InterbedStartingHeadDataArrayName)
       and (InterbedPreconsolidationHeadDataArrayName =
         SubItem.InterbedPreconsolidationHeadDataArrayName)
       and (InterbedStartingCompactionDataArrayName =
         SubItem.InterbedStartingCompactionDataArrayName)
       and (InterbedEquivalentThicknessDataArrayName =
         SubItem.InterbedEquivalentThicknessDataArrayName)
  end;
end;

procedure TSubDelayBedLayerItem.Loaded;
var
  PhastModel: TPhastModel;
  Names: TStringList;
  procedure UpdateTalksTo(Const ArrayName: string);
  var
    DataArray: TDataArray;
  begin
    DataArray := PhastModel.GetDataSetByName(ArrayName);
    Assert( DataArray <> nil);
    PhastModel.TopGridObserver.TalksTo(DataArray);
    DataArray.OnDataSetUsed := PhastModel.SubsidenceDataArrayUsed;
  end;
begin
  PhastModel := Model as TPhastModel;
  UpdateTalksTo(EquivNumberDataArrayName);
  UpdateTalksTo(VerticalHydraulicConductivityDataArrayName);
  UpdateTalksTo(ElasticSpecificStorageDataArrayName);
  UpdateTalksTo(InelasticSpecificStorageDataArrayName);
  UpdateTalksTo(InterbedStartingHeadDataArrayName);
  UpdateTalksTo(InterbedPreconsolidationHeadDataArrayName);
  UpdateTalksTo(InterbedStartingCompactionDataArrayName);
  UpdateTalksTo(InterbedEquivalentThicknessDataArrayName);

  Names := TStringList.Create;
  try
    Names.Add(EquivNumberDataArrayName);
    Names.Add(VerticalHydraulicConductivityDataArrayName);
    Names.Add(ElasticSpecificStorageDataArrayName);
    Names.Add(InelasticSpecificStorageDataArrayName);
    Names.Add(InterbedStartingHeadDataArrayName);
    Names.Add(InterbedPreconsolidationHeadDataArrayName);
    Names.Add(InterbedStartingCompactionDataArrayName);
    Names.Add(InterbedEquivalentThicknessDataArrayName);
    UpdateAssociatedDataSetNames(Names);
  finally
    Names.Free;
  end;

end;

procedure TSubDelayBedLayerItem.SetElasticSpecificStorageDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FElasticSpecificStorageDataArrayName, Value);
end;

procedure TSubDelayBedLayerItem.SetEquivNumberDataArrayName(const Value: string);
begin
  SetDataArrayName(FEquivNumberDataArrayName, Value);
end;

procedure TSubDelayBedLayerItem.SetInelasticSpecificStorageDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInelasticSpecificStorageDataArrayName, Value);
end;

procedure TSubDelayBedLayerItem.SetInterbedEquivalentThicknessDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInterbedEquivalentThicknessDataArrayName, Value);
end;

procedure TSubDelayBedLayerItem.SetInterbedPreconsolidationHeadDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInterbedPreconsolidationHeadDataArrayName, Value);
end;

procedure TSubDelayBedLayerItem.SetInterbedStartingCompactionDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInterbedStartingCompactionDataArrayName, Value);
end;

procedure TSubDelayBedLayerItem.SetInterbedStartingHeadDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInterbedStartingHeadDataArrayName, Value);
end;

procedure TSubDelayBedLayerItem.SetVerticalHydraulicConductivityDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FVerticalHydraulicConductivityDataArrayName, Value);
end;

procedure TSubDelayBedLayerItem.UpdateArrayNames(NewNames: TStringList);
begin
  Assert(NewNames.Count= 8);
  EquivNumberDataArrayName := NewNames[0];
  VerticalHydraulicConductivityDataArrayName := NewNames[1];
  ElasticSpecificStorageDataArrayName := NewNames[2];
  InelasticSpecificStorageDataArrayName := NewNames[3];
  InterbedStartingHeadDataArrayName := NewNames[4];
  InterbedPreconsolidationHeadDataArrayName := NewNames[5];
  InterbedStartingCompactionDataArrayName := NewNames[6];
  InterbedEquivalentThicknessDataArrayName := NewNames[7];

  UpdateAssociatedDataSetNames(NewNames);
end;

{ TSubDelayBedLayers }

function TSubDelayBedLayers.Add: TSubDelayBedLayerItem;
begin
  result := inherited Add as TSubDelayBedLayerItem;
end;

constructor TSubDelayBedLayers.Create(Model: TObject);
begin
  inherited Create(TSubDelayBedLayerItem, Model);
end;

function TSubDelayBedLayers.GetItem(Index: integer): TSubDelayBedLayerItem;
begin
  result := inherited Items[Index] as TSubDelayBedLayerItem;
end;

procedure TSubDelayBedLayers.Loaded;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].Loaded;
  end;
end;

procedure TSubDelayBedLayers.SetItem(Index: integer;
  const Value: TSubDelayBedLayerItem);
begin
  inherited Items[Index] := Value;
end;

{ TSubNoDelayBedLayers }

function TSubNoDelayBedLayers.Add: TSubNoDelayBedLayerItem;
begin
  result := inherited Add as TSubNoDelayBedLayerItem;
end;

constructor TSubNoDelayBedLayers.Create(Model: TObject);
begin
  inherited Create(TSubNoDelayBedLayerItem, Model);
end;

function TSubNoDelayBedLayers.GetItem(Index: integer): TSubNoDelayBedLayerItem;
begin
  result := inherited Items[Index] as TSubNoDelayBedLayerItem;
end;

procedure TSubNoDelayBedLayers.Loaded;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].Loaded;
  end;
end;

procedure TSubNoDelayBedLayers.SetItem(Index: integer;
  const Value: TSubNoDelayBedLayerItem);
begin
  inherited Items[Index] := Value;
end;

{ TUseLayerNumberItem }

procedure TUseLayerNumberItem.Assign(Source: TPersistent);
begin
  if Source is TUseLayerNumberItem then
  begin
    LayerNumber := TUseLayerNumberItem(Source).LayerNumber;
  end;
  inherited;
end;

function TUseLayerNumberItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := LayerNumber = TUseLayerNumberItem(AnotherItem).LayerNumber;
end;

procedure TUseLayerNumberItem.SetLayerNumber(const Value: integer);
begin
  SetIntegerProperty(FLayerNumber, Value)
end;

{ TUseLayersCollection }

function TUseLayersCollection.Add: TUseLayerNumberItem;
begin
  result := inherited Add as TUseLayerNumberItem;
end;

constructor TUseLayersCollection.Create(Model: TObject);
begin
  inherited Create(TUseLayerNumberItem, Model);
end;

function TUseLayersCollection.GetItem(Index: integer): TUseLayerNumberItem;
begin
  result := inherited Items[Index] as TUseLayerNumberItem;
end;

function TUseLayersCollection.GetItemByLayerNumber(
  LayerNumber: integer): TUseLayerNumberItem;
var
  Index: Integer;
  Item: TUseLayerNumberItem;
begin
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if Item.LayerNumber = LayerNumber then
    begin
      result := Item;
      Exit;
    end;
  end;
end;

procedure TUseLayersCollection.SetItem(Index: integer;
  const Value: TUseLayerNumberItem);
begin
  inherited Items[Index] := Value;
end;

{ TCustomSubLayers }

procedure TCustomSubLayerItem.Assign(Source: TPersistent);
var
  SubSource: TCustomSubLayerItem;
begin
  if Source is TCustomSubLayerItem then
  begin
    SubSource := TCustomSubLayerItem(Source);
    Name := SubSource.Name;
    UsedLayers := SubSource.UsedLayers;
    UseInAllLayers := SubSource.UseInAllLayers;
  end;
  inherited;
end;

constructor TCustomSubLayerItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FAssociatedModelDataSetNames:= TStringList.Create;
  FDataArrayTypes := TStringList.Create;
  FUsedLayers := TUseLayersCollection.Create(
    (Collection as TOrderedCollection).Model);
end;

destructor TCustomSubLayerItem.Destroy;
begin
  FUsedLayers.Free;
  FDataArrayTypes.Free;
  FAssociatedModelDataSetNames.Free;
  inherited;
end;

function TCustomSubLayerItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SubItem: TCustomSubLayerItem;
begin
  result := AnotherItem is TCustomSubLayerItem;
  if result then
  begin
    SubItem := TCustomSubLayerItem(AnotherItem);
    result := (Name = SubItem.Name)
      and (UseInAllLayers = SubItem.UseInAllLayers)
      and UsedLayers.IsSame(SubItem.UsedLayers)
  end;
end;

procedure TCustomSubLayerItem.SetArrayNames(NewName: string);
var
  NewNames: TStringList;
  ANewName: string;
  Index: integer;
begin
  NewNames := TStringList.Create;
  try
    NewName := ValidName(NewName);
    NewNames.Capacity := FDataArrayTypes.Count;
    for Index := 0 to FDataArrayTypes.Count - 1 do
    begin
      ANewName := NewName + '_' + FDataArrayTypes[Index];
      NewNames.Add(ANewName);
    end;
    UpdateArrayNames(NewNames);
  finally
    NewNames.Free;
  end;
end;

procedure TCustomSubLayerItem.SetDataArrayName(var OldName: string;
  NewName: string);
var
  Model: TPhastModel;
  DataArray: TDataArray;
//  UnitAbove, UnitBelow: TLayerGroup;
  NewFormula: string;
  Compiler: TRbwParser;
  Position: Integer;
  LocalCollection: TCustomSubLayer;
begin
  if OldName <> NewName then
  begin
    LocalCollection := Collection as TCustomSubLayer;
    try
      if LocalCollection.Model <> nil then
      begin
        Model := LocalCollection.Model as TPhastModel;
        if not (csLoading in Model.ComponentState) then
        begin
          if OldName = '' then
          begin
            DataArray := nil;
          end
          else
          begin
            DataArray := Model.GetDataSetByName(OldName);
          end;
          if DataArray = nil then
          begin
            DataArray := Model.GetDataSetByName(NewName);
            if DataArray <> nil then
            begin
              DataArray.OnDataSetUsed := Model.SubsidenceDataArrayUsed;
              LocalCollection.AddOwnedDataArray(DataArray);
            end;
          end;
          if DataArray <> nil then
          begin
            // rename data array.
            Model.TopGridObserver.StopsTalkingTo(DataArray);
            DataArray.StopsTalkingTo(Model.ThreeDGridObserver);
            DataArray.Name := NewName;
            DataArray.Classification := StrSubSidence + '|' + Name;
            Compiler := Model.GetCompiler(DataArray.Orientation,
              DataArray.EvaluatedAt);
            Position := Compiler.IndexOfVariable(OldName);
            if Position >= 0 then
            begin
              Compiler.RenameVariable(Position, NewName);
            end;
            Compiler := Model.GetCompiler(dso3D,
              DataArray.EvaluatedAt);
            Position := Compiler.IndexOfVariable(OldName);
            if Position >= 0 then
            begin
              Compiler.RenameVariable(Position, NewName);
            end;
          end
          else
          begin
            // create a new data array.

            // First get formula for new layer.
            NewFormula := '0.';

            // create new data array.
            DataArray := Model.CreateNewDataArray(TDataArray, NewName, NewFormula,
              [dcName, dcType, dcOrientation, dcEvaluatedAt], rdtDouble,
              eaBlocks, dsoTop, StrSubSidence + '|' + Name);
            DataArray.OnDataSetUsed := Model.SubsidenceDataArrayUsed;

            LocalCollection.AddOwnedDataArray(DataArray);
          end;
          Model.TopGridObserver.TalksTo(DataArray);
          DataArray.TalksTo(Model.ThreeDGridObserver);
          Model.ThreeDGridObserver.StopsTalkingTo(DataArray);

          DataArray.UpdateDimensions(Model.Grid.LayerCount,
            Model.Grid.RowCount, Model.Grid.ColumnCount);
        end;
      end;
    finally
      OldName := NewName;
      InvalidateModel;
    end;
  end;
end;

procedure TCustomSubLayerItem.SetName(const Value: string);
begin
  SetCaseSensitiveStringProperty(FName, Value);
  SetArrayNames(FName);
end;


procedure TCustomSubLayerItem.SetUsedLayers(const Value: TUseLayersCollection);
begin
  FUsedLayers.Assign(Value);
end;

procedure TCustomSubLayerItem.SetUseInAllLayers(const Value: boolean);
begin
  SetBooleanProperty(FUseInAllLayers, Value);
end;

procedure TCustomSubLayerItem.UpdateAssociatedDataSetNames(NewNames: TStringList);
var
  LocalCollection: TCustomSubLayer;
  Model : TPhastModel;
  DataArray: TDataArray;
  Index: integer;
begin
  LocalCollection := Collection as TCustomSubLayer;
  if LocalCollection.Model <> nil then
  begin
    Model := LocalCollection.Model as TPhastModel;
    if not (csLoading in Model.ComponentState) then
    begin
      Assert(NewNames.Count = FAssociatedModelDataSetNames.Count);
      for Index := 0 to NewNames.Count - 1 do
      begin
        DataArray := Model.GetDataSetByName(NewNames[Index]);
        if DataArray <> nil then
        begin
          DataArray.AssociatedDataSets := FAssociatedModelDataSetNames[Index];
        end;
      end;
    end;
  end;

end;

end.
