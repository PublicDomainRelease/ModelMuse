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
    procedure SetDataArrayName(var StoredName: string; NewName: string;
      CreateDataArray: boolean);
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
    procedure SetVerticalHydraulicConductivityDataArrayName
      (const Value: string);
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

  TSwtWaterTableItem = class(TCustomSubLayerItem)
  private
    FWaterTableCompressibleThicknessDataArrayName: string;
    FWaterTableInitialVoidRatioDataArrayName: string;
    FWaterTableInitialElasticSkeletalSpecificStorageDataArrayName: string;
    FWaterTableInitialCompactionDataArrayName: string;
    FWaterTableCompressionIndexDataArrayName: string;
    FWaterTableRecompressionIndexDataArrayName: string;
    FWaterTableInitialInelasticSkeletalSpecificStorageDataArrayName: string;
    procedure SetWaterTableCompressibleThicknessDataArrayName(
      const Value: string);
    procedure SetWaterTableCompressionIndexDataArrayName(const Value: string);
    procedure SetWaterTableInitialCompactionDataArrayName(const Value: string);
    procedure SetWaterTableInitialElasticSkeletalSpecificStorageDataArrayName(
      const Value: string);
    procedure SetWaterTableInitialInelasticSkeletalSpecificStorageDataArrayName(
      const Value: string);
    procedure SetWaterTableInitialVoidRatioDataArrayName(const Value: string);
    procedure SetWaterTableRecompressionIndexDataArrayName(const Value: string);
    procedure Loaded;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Collection: TCollection); override;
    procedure UpdateArrayNames(NewNames: TStringList); override;
  published
    // data set 7 THICK.
    property WaterTableCompressibleThicknessDataArrayName: string
      read FWaterTableCompressibleThicknessDataArrayName
      write SetWaterTableCompressibleThicknessDataArrayName;
    // data set 8 Sse
    property WaterTableInitialElasticSkeletalSpecificStorageDataArrayName: string
      read FWaterTableInitialElasticSkeletalSpecificStorageDataArrayName
      write SetWaterTableInitialElasticSkeletalSpecificStorageDataArrayName;
    // data set 9 Ssv
    property WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName: string
      read FWaterTableInitialInelasticSkeletalSpecificStorageDataArrayName
      write SetWaterTableInitialInelasticSkeletalSpecificStorageDataArrayName;
    // data set 10 Cr
    property WaterTableRecompressionIndexDataArrayName: string
      read FWaterTableRecompressionIndexDataArrayName
      write SetWaterTableRecompressionIndexDataArrayName;
    // data set 11 Cc
    property WaterTableCompressionIndexDataArrayName: string
      read FWaterTableCompressionIndexDataArrayName
      write SetWaterTableCompressionIndexDataArrayName;
    // data set 12 Void
    property WaterTableInitialVoidRatioDataArrayName: string
      read FWaterTableInitialVoidRatioDataArrayName
      write SetWaterTableInitialVoidRatioDataArrayName;
    // data set 13 Sub
    property WaterTableInitialCompactionDataArrayName: string
      read FWaterTableInitialCompactionDataArrayName
      write SetWaterTableInitialCompactionDataArrayName;
  end;

  TWaterTableLayers = class(TCustomSubLayer)
  private
    function GetItem(Index: integer): TSwtWaterTableItem;
    procedure SetItem(Index: integer; const Value: TSwtWaterTableItem);
  public
    function Add: TSwtWaterTableItem;
    constructor Create(Model: TObject);
    property Items[Index: integer]: TSwtWaterTableItem read GetItem
      write SetItem; default;
    procedure Loaded;
  end;


const
  StrSubSidence = 'Subsidence';

implementation

uses
  PhastModelUnit, DataSetUnit, RbwParser, IntListUnit,
  ModflowPackageSelectionUnit;

{ TSubNoDelayBedLayers }

procedure TSubNoDelayBedLayerItem.Assign(Source: TPersistent);
var
  SubSource: TSubNoDelayBedLayerItem;
begin
  inherited;
  if Source is TSubNoDelayBedLayerItem then
  begin
    SubSource := TSubNoDelayBedLayerItem(Source);
    PreconsolidationHeadDataArrayName :=
      SubSource.PreconsolidationHeadDataArrayName;
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
       (PreconsolidationHeadDataArrayName =
         SubItem.PreconsolidationHeadDataArrayName)
       and (ElasticSkeletalStorageCoefficientDataArrayName =
         SubItem.ElasticSkeletalStorageCoefficientDataArrayName)
       and (InelasticSkeletalStorageCoefficientDataArrayName =
         SubItem.InelasticSkeletalStorageCoefficientDataArrayName)
       and (InitialCompactionDataArrayName =
         SubItem.InitialCompactionDataArrayName)
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

procedure TSubNoDelayBedLayerItem.
  SetElasticSkeletalStorageCoefficientDataArrayName(const Value: string);
begin
  SetDataArrayName(FElasticSkeletalStorageCoefficientDataArrayName,
    Value, True);
end;

procedure TSubNoDelayBedLayerItem.
  SetInelasticSkeletalStorageCoefficientDataArrayName(const Value: string);
begin
  SetDataArrayName(FInelasticSkeletalStorageCoefficientDataArrayName, Value,
    True);
end;

procedure TSubNoDelayBedLayerItem.SetInitialCompactionDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInitialCompactionDataArrayName, Value, True);
end;

procedure TSubNoDelayBedLayerItem.SetPreconsolidationHeadDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FPreconsolidationHeadDataArrayName, Value, True);
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
  SetDataArrayName(FElasticSpecificStorageDataArrayName, Value, True);
end;

procedure TSubDelayBedLayerItem.
  SetEquivNumberDataArrayName(const Value: string);
begin
  SetDataArrayName(FEquivNumberDataArrayName, Value, True);
end;

procedure TSubDelayBedLayerItem.SetInelasticSpecificStorageDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInelasticSpecificStorageDataArrayName, Value, True);
end;

procedure TSubDelayBedLayerItem.SetInterbedEquivalentThicknessDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInterbedEquivalentThicknessDataArrayName, Value, True);
end;

procedure TSubDelayBedLayerItem.SetInterbedPreconsolidationHeadDataArrayName(
  const Value: string);
var
  CreateDataArray: Boolean;
begin
  CreateDataArray := (Model <> nil) and ((Model as TPhastModel).ModflowPackages.
    SubPackage.ReadDelayRestartFileName = '');
  SetDataArrayName(FInterbedPreconsolidationHeadDataArrayName, Value,
    CreateDataArray);
end;

procedure TSubDelayBedLayerItem.SetInterbedStartingCompactionDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInterbedStartingCompactionDataArrayName, Value, True);
end;

procedure TSubDelayBedLayerItem.SetInterbedStartingHeadDataArrayName(
  const Value: string);
var
  CreateDataArray: Boolean;
begin
  CreateDataArray := (Model <> nil) and ((Model as TPhastModel).ModflowPackages.
    SubPackage.ReadDelayRestartFileName = '');
  SetDataArrayName(FInterbedStartingHeadDataArrayName, Value, CreateDataArray);
end;

procedure TSubDelayBedLayerItem.SetVerticalHydraulicConductivityDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FVerticalHydraulicConductivityDataArrayName, Value, True);
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

procedure TCustomSubLayerItem.SetDataArrayName(var StoredName: string;
  NewName: string; CreateDataArray: boolean);
var
  LocalModel: TPhastModel;
  DataArray: TDataArray;
  NewFormula: string;
  Compiler: TRbwParser;
  Position: Integer;
  LocalCollection: TCustomSubLayer;
begin
  LocalCollection := Collection as TCustomSubLayer;
  try
    if LocalCollection.Model <> nil then
    begin
      LocalModel := Model as TPhastModel;
      if not (csLoading in LocalModel.ComponentState) then
      begin
        if StoredName = '' then
        begin
          DataArray := nil;
        end
        else
        begin
          DataArray := LocalModel.GetDataSetByName(StoredName);
        end;
        if DataArray = nil then
        begin
          DataArray := LocalModel.GetDataSetByName(NewName);
          if DataArray <> nil then
          begin
            DataArray.OnDataSetUsed := LocalModel.SubsidenceDataArrayUsed;
            LocalCollection.AddOwnedDataArray(DataArray);
          end;
        end;
        if DataArray <> nil then
        begin
          // rename data array.
          LocalModel.TopGridObserver.StopsTalkingTo(DataArray);
          DataArray.StopsTalkingTo(LocalModel.ThreeDGridObserver);
          DataArray.Name := NewName;
          DataArray.Classification := StrSubSidence + '|' + Name;
          Compiler := LocalModel.GetCompiler(DataArray.Orientation,
            DataArray.EvaluatedAt);
          Position := Compiler.IndexOfVariable(StoredName);
          if Position >= 0 then
          begin
            Compiler.RenameVariable(Position, NewName);
          end;
          Compiler := LocalModel.GetCompiler(dso3D,
            DataArray.EvaluatedAt);
          Position := Compiler.IndexOfVariable(StoredName);
          if Position >= 0 then
          begin
            Compiler.RenameVariable(Position, NewName);
          end;
        end
        else if CreateDataArray then
        begin
          // create a new data array.

          // First get formula for new layer.
          NewFormula := '0.';

          // create new data array.
          DataArray := LocalModel.CreateNewDataArray(TDataArray, NewName,
            NewFormula, [dcName, dcType, dcOrientation, dcEvaluatedAt],
            rdtDouble, eaBlocks, dsoTop, StrSubSidence + '|' + Name);
          DataArray.OnDataSetUsed := LocalModel.SubsidenceDataArrayUsed;

          LocalCollection.AddOwnedDataArray(DataArray);
        end;
        if DataArray <> nil then
        begin
          LocalModel.TopGridObserver.TalksTo(DataArray);
          DataArray.TalksTo(LocalModel.ThreeDGridObserver);
          LocalModel.ThreeDGridObserver.StopsTalkingTo(DataArray);

          DataArray.UpdateDimensions(LocalModel.Grid.LayerCount,
            LocalModel.Grid.RowCount, LocalModel.Grid.ColumnCount);
//          DataArray.AssociatedDataSets := AssociatedDataSets;
        end;
      end;
    end;
  finally
    if StoredName <> NewName then
    begin
      StoredName := NewName;
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

procedure TCustomSubLayerItem.UpdateAssociatedDataSetNames(
  NewNames: TStringList);
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

{ TSubWaterTableItem }

procedure TSwtWaterTableItem.Assign(Source: TPersistent);
var
  SubItem: TSwtWaterTableItem;
begin
  inherited;
  if Source is TSwtWaterTableItem then
  begin
     SubItem := TSwtWaterTableItem(Source);
     WaterTableCompressibleThicknessDataArrayName :=
       SubItem.WaterTableCompressibleThicknessDataArrayName;
     WaterTableInitialElasticSkeletalSpecificStorageDataArrayName :=
       SubItem.WaterTableInitialElasticSkeletalSpecificStorageDataArrayName;
     WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName :=
       SubItem.WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName;
     WaterTableRecompressionIndexDataArrayName :=
       SubItem.WaterTableRecompressionIndexDataArrayName;
     WaterTableCompressionIndexDataArrayName :=
       SubItem.WaterTableCompressionIndexDataArrayName;
     WaterTableInitialVoidRatioDataArrayName :=
       SubItem.WaterTableInitialVoidRatioDataArrayName;
     WaterTableInitialCompactionDataArrayName :=
       SubItem.WaterTableInitialCompactionDataArrayName;
  end;
end;

constructor TSwtWaterTableItem.Create(Collection: TCollection);
begin
  inherited;
  FDataArrayTypes.Add('Compressible_Thickness');
  FDataArrayTypes.Add('Initial_Elastic_Skeletal_Specific_Storage');
  FDataArrayTypes.Add('Initial_Inelastic_Skeletal_Specific_Storage');
  FDataArrayTypes.Add('Recompression_Index');
  FDataArrayTypes.Add('Compression_Index');
  FDataArrayTypes.Add('Initial_Void_Ratio');
  FDataArrayTypes.Add('Initial_Compaction');

  FAssociatedModelDataSetNames.Add('MODFLOW SWT: THICK (Data Set 7)');
  FAssociatedModelDataSetNames.Add('MODFLOW SWT: Sse (Data Set 8)');
  FAssociatedModelDataSetNames.Add('MODFLOW SWT: Ssv (Data Set 9)');
  FAssociatedModelDataSetNames.Add('MODFLOW SWT: Cr (Data Set 10)');
  FAssociatedModelDataSetNames.Add('MODFLOW SWT: Cc (Data Set 11)');
  FAssociatedModelDataSetNames.Add('MODFLOW SWT: VOID (Data Set 12)');
  FAssociatedModelDataSetNames.Add('MODFLOW SWT: SUB (Data Set 13)');
end;

function TSwtWaterTableItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SubItem: TSwtWaterTableItem;
begin
  result := (AnotherItem is TSwtWaterTableItem)
    and inherited IsSame(AnotherItem);
  if result then
  begin
     SubItem := TSwtWaterTableItem(AnotherItem);
     result :=
       (WaterTableCompressibleThicknessDataArrayName =
         SubItem.WaterTableCompressibleThicknessDataArrayName)
       and (WaterTableInitialElasticSkeletalSpecificStorageDataArrayName =
         SubItem.WaterTableInitialElasticSkeletalSpecificStorageDataArrayName)
       and (WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName =
         SubItem.WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName)
       and (WaterTableRecompressionIndexDataArrayName =
         SubItem.WaterTableRecompressionIndexDataArrayName)
       and (WaterTableCompressionIndexDataArrayName =
         SubItem.WaterTableCompressionIndexDataArrayName)
       and (WaterTableInitialVoidRatioDataArrayName =
         SubItem.WaterTableInitialVoidRatioDataArrayName)
       and (WaterTableInitialCompactionDataArrayName =
         SubItem.WaterTableInitialCompactionDataArrayName)
  end;
end;

procedure TSwtWaterTableItem.Loaded;
var
  PhastModel: TPhastModel;
  Names: TStringList;
  procedure UpdateTalksTo(Const ArrayName: string);
  var
    DataArray: TDataArray;
  begin
    DataArray := PhastModel.GetDataSetByName(ArrayName);
    if( DataArray <> nil) then
    begin
      PhastModel.TopGridObserver.TalksTo(DataArray);
      DataArray.OnDataSetUsed := PhastModel.SubsidenceDataArrayUsed;
    end;
  end;
begin
  PhastModel := Model as TPhastModel;
  UpdateTalksTo(WaterTableCompressibleThicknessDataArrayName);
  UpdateTalksTo(WaterTableInitialElasticSkeletalSpecificStorageDataArrayName);
  UpdateTalksTo(WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName);
  UpdateTalksTo(WaterTableRecompressionIndexDataArrayName);
  UpdateTalksTo(WaterTableCompressionIndexDataArrayName);
  UpdateTalksTo(WaterTableInitialVoidRatioDataArrayName);
  UpdateTalksTo(WaterTableInitialCompactionDataArrayName);

  Names := TStringList.Create;
  try
    Names.Add(WaterTableCompressibleThicknessDataArrayName);
    Names.Add(WaterTableInitialElasticSkeletalSpecificStorageDataArrayName);
    Names.Add(WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName);
    Names.Add(WaterTableRecompressionIndexDataArrayName);
    Names.Add(WaterTableCompressionIndexDataArrayName);
    Names.Add(WaterTableInitialVoidRatioDataArrayName);
    Names.Add(WaterTableInitialCompactionDataArrayName);
    UpdateAssociatedDataSetNames(Names);
  finally
    Names.Free;
  end;

end;

procedure TSwtWaterTableItem.SetWaterTableCompressibleThicknessDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FWaterTableCompressibleThicknessDataArrayName, Value, True);
end;

procedure TSwtWaterTableItem.SetWaterTableCompressionIndexDataArrayName(
  const Value: string);
var
  CreateDataArray: Boolean;
begin
  CreateDataArray := (Model <> nil) and ((Model as TPhastModel).ModflowPackages.
    SwtPackage.CompressionSource = csCompressionReComp);
  SetDataArrayName(FWaterTableCompressionIndexDataArrayName, Value,
    CreateDataArray);
end;

procedure TSwtWaterTableItem.SetWaterTableInitialCompactionDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FWaterTableInitialCompactionDataArrayName, Value, True);
end;

procedure TSwtWaterTableItem.
  SetWaterTableInitialElasticSkeletalSpecificStorageDataArrayName(
  const Value: string);
var
  CreateDataArray: Boolean;
begin
  CreateDataArray := (Model <> nil) and ((Model as TPhastModel).ModflowPackages.
    SwtPackage.CompressionSource = csSpecificStorage);
  SetDataArrayName(
    FWaterTableInitialElasticSkeletalSpecificStorageDataArrayName, Value,
      CreateDataArray);
end;

procedure TSwtWaterTableItem.
  SetWaterTableInitialInelasticSkeletalSpecificStorageDataArrayName(
  const Value: string);
var
  CreateDataArray: Boolean;
begin
  CreateDataArray := (Model <> nil) and ((Model as TPhastModel).ModflowPackages.
    SwtPackage.CompressionSource = csSpecificStorage);
  SetDataArrayName(
    FWaterTableInitialInelasticSkeletalSpecificStorageDataArrayName, Value,
      CreateDataArray);
end;

procedure TSwtWaterTableItem.SetWaterTableInitialVoidRatioDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FWaterTableInitialVoidRatioDataArrayName, Value, True);
end;

procedure TSwtWaterTableItem.SetWaterTableRecompressionIndexDataArrayName(
  const Value: string);
var
  CreateDataArray: Boolean;
begin
  CreateDataArray := (Model <> nil) and ((Model as TPhastModel).ModflowPackages.
    SwtPackage.CompressionSource = csCompressionReComp);
  SetDataArrayName(FWaterTableRecompressionIndexDataArrayName, Value,
    CreateDataArray);
end;

procedure TSwtWaterTableItem.UpdateArrayNames(NewNames: TStringList);
begin
  Assert(NewNames.Count= 7);
  WaterTableCompressibleThicknessDataArrayName := NewNames[0];
  WaterTableInitialElasticSkeletalSpecificStorageDataArrayName := NewNames[1];
  WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName := NewNames[2];
  WaterTableRecompressionIndexDataArrayName := NewNames[3];
  WaterTableCompressionIndexDataArrayName := NewNames[4];
  WaterTableInitialVoidRatioDataArrayName := NewNames[5];
  WaterTableInitialCompactionDataArrayName := NewNames[6];

  UpdateAssociatedDataSetNames(NewNames);

end;

{ TWaterTableLayers }

function TWaterTableLayers.Add: TSwtWaterTableItem;
begin
  result := inherited Add as TSwtWaterTableItem
end;

constructor TWaterTableLayers.Create(Model: TObject);
begin
  inherited Create(TSwtWaterTableItem, Model);
end;

function TWaterTableLayers.GetItem(Index: integer): TSwtWaterTableItem;
begin
  result := inherited Items[Index] as TSwtWaterTableItem
end;

procedure TWaterTableLayers.Loaded;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].Loaded;
  end;
end;

procedure TWaterTableLayers.SetItem(Index: integer;
  const Value: TSwtWaterTableItem);
begin
  inherited Items[Index] := Value;
end;

end.

