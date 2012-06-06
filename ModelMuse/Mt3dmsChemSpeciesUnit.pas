unit Mt3dmsChemSpeciesUnit;

interface

uses
  OrderedCollectionUnit, ModflowBoundaryDisplayUnit, DataSetUnit, Classes,
  GoPhastTypes;

type
  TCustomChemSpeciesCollection = class;

  TChemSpeciesItem = class(TOrderedItem)
  private
    FName: string;
    FInitialConcDataArrayName: string;
    FSorbOrImmobInitialConcDataArrayName: string;
    FFirstSorbParamDataArrayName: string;
    FSecondSorbParamDataArrayName: string;
    FReactionRateDisolvedDataArrayName: string;
    FReactionRateSorbedDataArrayName: string;
    procedure SetName(const Value: string); virtual;
    procedure SetInitialConcDataArrayName(const NewName: string);
    function Collection: TCustomChemSpeciesCollection;
    procedure UpdateDataArray(OnDataSetUsed: TObjectUsedEvent;
      const OldDataArrayName, NewName, NewFormula, AssociatedDataSets: string; ShouldCreate: boolean);
    procedure SetSorbOrImmobInitialConcDataArrayName(const NewName: string);
    procedure SetFirstSorbParamDataArrayName(const NewName: string);
    procedure SetSecondSorbParamDataArrayName(const NewName: string);
    procedure SetReactionRateDisolvedDataArrayName(const NewName: string);
    procedure SetReactionRateSorbedDataArrayName(const NewName: string);
    procedure RenameDependents(NewName: string);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure SetIndex(Value: Integer); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Name: string read FName write SetName;
    // BTN package, SCONC
    property InitialConcDataArrayName: string read FInitialConcDataArrayName
      write SetInitialConcDataArrayName;
    // RCT package, SRCONC
    property SorbOrImmobInitialConcDataArrayName: string
      read FSorbOrImmobInitialConcDataArrayName
      write SetSorbOrImmobInitialConcDataArrayName;
    // RCT package, SP1
    property FirstSorbParamDataArrayName: string
      read FFirstSorbParamDataArrayName write SetFirstSorbParamDataArrayName;
    // RCT package, SP2
    property SecondSorbParamDataArrayName: string
      read FSecondSorbParamDataArrayName write SetSecondSorbParamDataArrayName;
    // RCT package, RC1
    property ReactionRateDisolvedDataArrayName: string
      read FReactionRateDisolvedDataArrayName
      write SetReactionRateDisolvedDataArrayName;
    // RCT package, RC2
    property ReactionRateSorbedDataArrayName: string
      read FReactionRateSorbedDataArrayName write
      SetReactionRateSorbedDataArrayName;
  end;

  TCustomChemSpeciesCollection= class(TEnhancedOrderedCollection)
  private
    function GetItem(Index: Integer): TChemSpeciesItem;
    procedure SetItem(Index: Integer; const Value: TChemSpeciesItem);
  public
    property Items[Index: Integer] : TChemSpeciesItem read GetItem
      write SetItem; default;
    function Add: TChemSpeciesItem;
    function Insert(Index: integer): TChemSpeciesItem;
    procedure UpdateDataArrays; virtual;
    procedure Loaded;
    function IndexOfName(const AName: string): integer;
  end;

  TChemSpeciesCollection = class(TCustomChemSpeciesCollection)
  public
    constructor Create(Model: TBaseModel);
  end;

  TMobileChemSpeciesItem = class(TChemSpeciesItem)
  private
    FDiffusionCoefDataArrayName: string;
    procedure SetDiffusionCoefDataArrayName(const NewName: string);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure SetName(const Value: string); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    // DSP Package, DMCOEF
    property DiffusionCoefDataArrayName: string read FDiffusionCoefDataArrayName
      write SetDiffusionCoefDataArrayName;
  end;

  TMobileChemSpeciesCollection = class(TCustomChemSpeciesCollection)
  private
    function GetItem(Index: Integer): TMobileChemSpeciesItem;
    procedure SetItem(Index: Integer; const Value: TMobileChemSpeciesItem);
  public
    constructor Create(Model: TBaseModel);
    property Items[Index: Integer] : TMobileChemSpeciesItem read GetItem
      write SetItem; default;
    function Add: TMobileChemSpeciesItem;
    procedure UpdateDataArrays; override;
  end;

implementation

uses
  PhastModelUnit, RbwParser, SysUtils, ModflowPackageSelectionUnit,
  frmGoPhastUnit, ScreenObjectUnit, Mt3dmsChemUnit, Mt3dmsTobUnit;

{ TChemSpeciesItem }

procedure TChemSpeciesItem.Assign(Source: TPersistent);
var
  SourceChem: TChemSpeciesItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TChemSpeciesItem then
  begin
    SourceChem := TChemSpeciesItem(Source);
    // first copy data array names
    FInitialConcDataArrayName := SourceChem.InitialConcDataArrayName;
    FSorbOrImmobInitialConcDataArrayName :=
      SourceChem.SorbOrImmobInitialConcDataArrayName;
    FFirstSorbParamDataArrayName :=
      SourceChem.FirstSorbParamDataArrayName;
    FSecondSorbParamDataArrayName :=
      SourceChem.SecondSorbParamDataArrayName;
    FReactionRateDisolvedDataArrayName :=
      SourceChem.ReactionRateDisolvedDataArrayName;
    FReactionRateSorbedDataArrayName :=
      SourceChem.ReactionRateSorbedDataArrayName;

    // then change the name of the chem species
    Name := SourceChem.Name;

    // then update the data array names
    FInitialConcDataArrayName := '';
    InitialConcDataArrayName := SourceChem.InitialConcDataArrayName;

    FSorbOrImmobInitialConcDataArrayName := '';
    SorbOrImmobInitialConcDataArrayName :=
      SourceChem.SorbOrImmobInitialConcDataArrayName;

    FFirstSorbParamDataArrayName := '';
    FirstSorbParamDataArrayName :=
      SourceChem.FirstSorbParamDataArrayName;

    FSecondSorbParamDataArrayName := '';
    SecondSorbParamDataArrayName :=
      SourceChem.SecondSorbParamDataArrayName;

    FReactionRateDisolvedDataArrayName := '';
    ReactionRateDisolvedDataArrayName :=
      SourceChem.ReactionRateDisolvedDataArrayName;

    FReactionRateSorbedDataArrayName := '';
    ReactionRateSorbedDataArrayName :=
      SourceChem.ReactionRateSorbedDataArrayName;
  end;
  inherited;
end;

procedure TChemSpeciesItem.UpdateDataArray(OnDataSetUsed: TObjectUsedEvent;
  const OldDataArrayName, NewName, NewFormula, AssociatedDataSets: string; ShouldCreate: boolean);
var
  DataArray: TDataArray;
  LocalModel: TPhastModel;
begin
  if Collection.Model <> nil then
  begin
    LocalModel := Collection.Model as TPhastModel;
    if not (csLoading in LocalModel.ComponentState) then
    begin
      DataArray := LocalModel.DataArrayManager.
        GetDataSetByName(OldDataArrayName);
      if DataArray <> nil then
      begin
        if DataArray.Name <> NewName then
        begin
          LocalModel.RenameDataArray(DataArray, NewName);
        end;
      end
      else
      begin
        DataArray := LocalModel.DataArrayManager.GetDataSetByName(NewName);
      end;
      if DataArray = nil then
      begin
        if ShouldCreate then
        begin
          // create a new data array.
          DataArray := LocalModel.DataArrayManager.CreateNewDataArray(
            TDataArray, NewName, NewFormula,
            [dcName, dcType, dcOrientation, dcEvaluatedAt],
            rdtDouble, eaBlocks, dso3D, StrMT3DMS);
        end;
      end;
      if DataArray <> nil then
      begin
        LocalModel.ThreeDGridObserver.TalksTo(DataArray);
        DataArray.UpdateDimensions(LocalModel.Grid.LayerCount,
          LocalModel.Grid.RowCount, LocalModel.Grid.ColumnCount);
        DataArray.OnDataSetUsed := OnDataSetUsed;
        DataArray.AssociatedDataSets := AssociatedDataSets;
      end;
    end;
  end;
end;

function TChemSpeciesItem.Collection: TCustomChemSpeciesCollection;
begin
  result := inherited Collection as TCustomChemSpeciesCollection;
end;

constructor TChemSpeciesItem.Create(Collection: TCollection);
var
  LocalModel: TCustomModel;
  SpeciesIndex: integer;
  ScreenObjectIndex: integer;
  ScreenObject: TScreenObject;
  ConcBoundary: TMt3dmsConcBoundary;
begin
  inherited;
  FName := 'default name';
  if (Model <> nil) and not (csLoading in Model.ComponentState) then
  begin
    LocalModel := Model as TCustomModel;
    SpeciesIndex := LocalModel.MobileComponents.IndexOf(self);
    if SpeciesIndex < 0 then
    begin
      SpeciesIndex := LocalModel.ImmobileComponents.IndexOf(self);
      Assert(SpeciesIndex >= 0);
      SpeciesIndex := LocalModel.MobileComponents.Count + SpeciesIndex
    end;
    for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
      ConcBoundary := ScreenObject.Mt3dmsConcBoundary;
      if ConcBoundary <> nil then
      begin
        ConcBoundary.InsertNewSpecies(SpeciesIndex, Name);
      end;
    end;
  end;
end;

destructor TChemSpeciesItem.Destroy;
var
  LocalModel: TCustomModel;
  SpeciesIndex: Integer;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ConcBoundary: TMt3dmsConcBoundary;
  TransObservations: TMt3dmsTransObservations;
begin
  if (Model <> nil) and not (csDestroying in Model.ComponentState) then
  begin
    LocalModel := Model as TCustomModel;
    SpeciesIndex := LocalModel.MobileComponents.IndexOf(self);
    if SpeciesIndex < 0 then
    begin
      SpeciesIndex := LocalModel.ImmobileComponents.IndexOf(self);
      Assert(SpeciesIndex >= 0);
      SpeciesIndex := LocalModel.MobileComponents.Count + SpeciesIndex
    end;
    for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
      ConcBoundary := ScreenObject.Mt3dmsConcBoundary;
      if ConcBoundary <> nil then
      begin
        ConcBoundary.DeleteSpecies(SpeciesIndex);
      end;
      TransObservations := ScreenObject.Mt3dmsTransObservations;
      if TransObservations <> nil then
      begin
        TransObservations.DeleteSpecies(Name);
      end;
    end;
  end;

  inherited;

//  if (Model <> nil) and not (csDestroying in Model.ComponentState) then
//  begin
//    LocalModel := Model as TCustomModel;
//    for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
//    begin
//      ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
//      ConcBoundary := ScreenObject.Mt3dmsConcBoundary;
//      if ConcBoundary <> nil then
//      begin
//        ConcBoundary.CreateTimeLists;
//      end;
//    end;
//  end;

end;

function TChemSpeciesItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  ChemItem: TChemSpeciesItem;
begin
  result := AnotherItem is TChemSpeciesItem;
  if result then
  begin
    ChemItem := TChemSpeciesItem(AnotherItem);
    result := (Name = ChemItem.Name)
      and (InitialConcDataArrayName = ChemItem.InitialConcDataArrayName)
      and (SorbOrImmobInitialConcDataArrayName = ChemItem.SorbOrImmobInitialConcDataArrayName)
      and (FirstSorbParamDataArrayName = ChemItem.FirstSorbParamDataArrayName)
      and (SecondSorbParamDataArrayName = ChemItem.SecondSorbParamDataArrayName)
      and (ReactionRateDisolvedDataArrayName = ChemItem.ReactionRateDisolvedDataArrayName)
      and (ReactionRateSorbedDataArrayName = ChemItem.ReactionRateSorbedDataArrayName)
  end;
end;

procedure TChemSpeciesItem.RenameDependents(NewName: string);
var
  LocalModel: TCustomModel;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  ConcBoundary: TMt3dmsConcBoundary;
  TransObservations: TMt3dmsTransObservations;
begin
  if FName <> NewName then
  begin
    LocalModel := Model as TCustomModel;
    if LocalModel <> nil then
    begin
      for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
        ConcBoundary := AScreenObject.Mt3dmsConcBoundary;
        if ConcBoundary <> nil then
        begin
          ConcBoundary.RenameSpecies(FName, NewName);
        end;
        TransObservations := AScreenObject.Mt3dmsTransObservations;
        if TransObservations <> nil then
        begin
          TransObservations.RenameSpecies(FName, NewName);
        end;
      end;
    end;
  end;
end;

procedure TChemSpeciesItem.SetFirstSorbParamDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Collection.Model as TPhastModel;
  if LocalModel <> nil then
  begin
    UpdateDataArray(LocalModel.Mt3dMsFirstSorbParamUsed,
      FFirstSorbParamDataArrayName, NewName, '1.', 'MT3DMS RCT package, SP1',
      LocalModel.AnyMt3dSorbParameter);
  end;
  SetCaseSensitiveStringProperty(FFirstSorbParamDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetIndex(Value: Integer);
var
  OldIndex: Integer;
  LocalModel: TCustomModel;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  ConcBoundary: TMt3dmsConcBoundary;
begin
  OldIndex := Index;
  inherited;
  if OldIndex <> Value then
  begin
    LocalModel := Model as TCustomModel;
    if LocalModel <> nil then
    begin
      if LocalModel.MobileComponents.IndexOf(self) < 0 then
      begin
        OldIndex := OldIndex + LocalModel.MobileComponents.Count;
        Value := Value + LocalModel.MobileComponents.Count;
      end;

      for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
        ConcBoundary := AScreenObject.Mt3dmsConcBoundary;
        if ConcBoundary <> nil then
        begin
          ConcBoundary.ChangeSpeciesPosition(OldIndex, Value)
        end;
      end;
    end;
  end;
end;

procedure TChemSpeciesItem.SetInitialConcDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Collection.Model as TPhastModel;

  if LocalModel <> nil then
  begin
    UpdateDataArray(LocalModel.Mt3dMsInitialConcUsed,
      FInitialConcDataArrayName, NewName, '0', 'MT3DMS BTN package, SCONC',
      True);
  end;

  SetCaseSensitiveStringProperty(FInitialConcDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetName(const Value: string);
const
  InitConcPrefix = 'Initial_Concentration_';
  SorbPrefix = 'Sorbed_Phase_Initial_Conc_';
  ImmobPrefix = 'Immobile_Phase_Initial_Conc_';
  FirstSorbParamPrefix = 'Sorption_Parameter1_';
  SecondSorbParamPrefix = 'Sorption_Parameter2_';
  RC1Prefix = 'Reaction_Rate_Dissolved_Phase_';
  RC2Prefix = 'Reaction_Rate_Sorbed_Phase_';
var
  LocalModel: TPhastModel;
begin
  Assert(Value <> '');
  // data array names may need to change even if the species name does not.
//  if FName <> Value then
  begin
    if UpperCase(FName) = UpperCase(Value) then
    begin
      InitialConcDataArrayName := StringReplace(InitialConcDataArrayName,
        GenerateNewRoot(FName),GenerateNewRoot(Value), []);
      SorbOrImmobInitialConcDataArrayName := StringReplace(
        SorbOrImmobInitialConcDataArrayName,
        GenerateNewRoot(FName),GenerateNewRoot(Value), []);
      FirstSorbParamDataArrayName := StringReplace(
        FirstSorbParamDataArrayName,
        GenerateNewRoot(FName),GenerateNewRoot(Value), []);
      SecondSorbParamDataArrayName := StringReplace(
        SecondSorbParamDataArrayName,
        GenerateNewRoot(FName),GenerateNewRoot(Value), []);
      ReactionRateDisolvedDataArrayName := StringReplace(
        ReactionRateDisolvedDataArrayName,
        GenerateNewRoot(FName),GenerateNewRoot(Value), []);
      ReactionRateSorbedDataArrayName := StringReplace(
        ReactionRateSorbedDataArrayName,
        GenerateNewRoot(FName),GenerateNewRoot(Value), []);
    end
    else
    begin
      InitialConcDataArrayName := GenerateNewRoot(InitConcPrefix + Value);
      LocalModel := Model as TPhastModel;
      if LocalModel = nil then
      begin
        LocalModel := frmGoPhast.PhastModel;
      end;
      if LocalModel.ModflowPackages.Mt3dmsChemReact.SorptionChoice
        = scFirstOrderKinetic then
      begin
        SorbOrImmobInitialConcDataArrayName :=
          GenerateNewRoot(SorbPrefix + Value);
      end
      else
      begin
        SorbOrImmobInitialConcDataArrayName :=
          GenerateNewRoot(ImmobPrefix + Value);
      end;
      FirstSorbParamDataArrayName :=
        GenerateNewRoot(FirstSorbParamPrefix + Value);
      SecondSorbParamDataArrayName :=
        GenerateNewRoot(SecondSorbParamPrefix + Value);
      ReactionRateDisolvedDataArrayName :=
        GenerateNewRoot(RC1Prefix + Value);
      ReactionRateSorbedDataArrayName :=
        GenerateNewRoot(RC2Prefix + Value);
    end;
    RenameDependents(Value);
    SetCaseSensitiveStringProperty(FName, Value);
  end;
end;

procedure TChemSpeciesItem.SetReactionRateDisolvedDataArrayName(
  const NewName: string);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Collection.Model as TPhastModel;

  if LocalModel <> nil then
  begin
    UpdateDataArray(LocalModel.Mt3dmsReactionRateDisolvedUsed,
      FReactionRateDisolvedDataArrayName, NewName, '1E-6', 'MT3DMS RCT package, RC1',
      LocalModel.AnyMt3dReactions);
  end;

  SetCaseSensitiveStringProperty(FReactionRateDisolvedDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetReactionRateSorbedDataArrayName(
  const NewName: string);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Collection.Model as TPhastModel;

  if LocalModel <> nil then
  begin
    UpdateDataArray(LocalModel.Mt3dmsReactionRateSorbedUsed,
      FReactionRateSorbedDataArrayName, NewName, '1E-6', 'MT3DMS RCT package, RC2',
      LocalModel.AnyMt3dReactions);
  end;

  SetCaseSensitiveStringProperty(FReactionRateSorbedDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetSecondSorbParamDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Collection.Model as TPhastModel;

  if LocalModel <> nil then
  begin
    UpdateDataArray(LocalModel.Mt3dMsSecondSorbParamUsed,
      FSecondSorbParamDataArrayName, NewName, '1.', 'MT3DMS RCT package, SP2',
      LocalModel.AnyMt3dSorbParameter);
  end;

  SetCaseSensitiveStringProperty(FSecondSorbParamDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetSorbOrImmobInitialConcDataArrayName(
  const NewName: string);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Collection.Model as TPhastModel;
  if LocalModel <> nil then
  begin
    UpdateDataArray(LocalModel.Mt3dMsSorbImmobInitialConcUsed,
      FSorbOrImmobInitialConcDataArrayName, NewName, '0', 'MT3DMS RCT package, SRCONC',
      LocalModel.AnyMt3dSorbImmobConc);
  end;
  SetCaseSensitiveStringProperty(FSorbOrImmobInitialConcDataArrayName, NewName);
end;

{ TConcentrationCollection }

function TCustomChemSpeciesCollection.Add: TChemSpeciesItem;
begin
  Result := inherited Add as TChemSpeciesItem;
end;

constructor TChemSpeciesCollection.Create(Model: TBaseModel);
begin
  inherited Create(TChemSpeciesItem, Model);
end;

function TCustomChemSpeciesCollection.GetItem(Index: Integer): TChemSpeciesItem;
begin
  result := inherited Items[Index] as TChemSpeciesItem
end;

function TCustomChemSpeciesCollection.IndexOfName(const AName: string): integer;
var
  index: Integer;
begin
  result := -1;
  for index := 0 to Count - 1 do
  begin
    if AnsiCompareText(Items[index].Name, AName) = 0 then
    begin
      result := index;
      exit;
    end;
  end;
end;

function TCustomChemSpeciesCollection.Insert(Index: integer): TChemSpeciesItem;
begin
  result := inherited Insert(Index) as TChemSpeciesItem;
end;

procedure TCustomChemSpeciesCollection.Loaded;
var
  index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[index].Name := Items[index].Name;
  end;
end;

procedure TCustomChemSpeciesCollection.SetItem(Index: Integer;
  const Value: TChemSpeciesItem);
begin
  inherited Items[Index] := Value;
end;


procedure TCustomChemSpeciesCollection.UpdateDataArrays;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    // ensure the data sets have been created if needed.
    Items[Index].Name := Items[Index].Name;
  end;
end;

{ TMobileChemSpeciesItem }

procedure TMobileChemSpeciesItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TMobileChemSpeciesItem then
  begin
    DiffusionCoefDataArrayName :=
      TMobileChemSpeciesItem(Source).DiffusionCoefDataArrayName;
  end;
  inherited;

end;

function TMobileChemSpeciesItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := inherited and (AnotherItem is TMobileChemSpeciesItem);
  if result then
  begin
    Result := DiffusionCoefDataArrayName =
      TMobileChemSpeciesItem(AnotherItem).DiffusionCoefDataArrayName
  end;
end;

procedure TMobileChemSpeciesItem.SetDiffusionCoefDataArrayName(
  const NewName: string);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Collection.Model as TPhastModel;

  if LocalModel <> nil then
  begin
    UpdateDataArray(LocalModel.ModDispDataArrayUsed,
      FDiffusionCoefDataArrayName, NewName, '0', 'MT3DMS DSP Package, DMCOEF',
      LocalModel.AnyDispersionMultiDiffusion);
  end;

  SetCaseSensitiveStringProperty(FDiffusionCoefDataArrayName, NewName);
end;

procedure TMobileChemSpeciesItem.SetName(const Value: string);
const
  Prefix = 'Diffusion_Coefficient_';
begin
  Assert(Value <> '');
  // data array names may need to change even if the species name does not.
//  if FName <> Value then
  begin
    if UpperCase(FName) = UpperCase(Value) then
    begin
      DiffusionCoefDataArrayName := StringReplace(DiffusionCoefDataArrayName,
        GenerateNewRoot(FName),GenerateNewRoot(Value), []);
    end
    else
    begin
      DiffusionCoefDataArrayName := GenerateNewRoot(Prefix + Value);
    end;
  end;
  inherited;
end;

{ TMobileChemSpeciesCollection }

function TMobileChemSpeciesCollection.Add: TMobileChemSpeciesItem;
begin
  result := inherited Add as TMobileChemSpeciesItem
end;

constructor TMobileChemSpeciesCollection.Create(Model: TBaseModel);
begin
  inherited Create(TMobileChemSpeciesItem, Model);
end;

function TMobileChemSpeciesCollection.GetItem(
  Index: Integer): TMobileChemSpeciesItem;
begin
  result := inherited Items[Index] as TMobileChemSpeciesItem;
end;

procedure TMobileChemSpeciesCollection.SetItem(Index: Integer;
  const Value: TMobileChemSpeciesItem);
begin
  inherited Items[Index] := Value;
end;

procedure TMobileChemSpeciesCollection.UpdateDataArrays;
var
  Index: Integer;
begin
  inherited;
  for Index := 0 to Count - 1 do
  begin
    // ensure the data sets have been created if needed.
    Items[Index].DiffusionCoefDataArrayName :=
      Items[Index].DiffusionCoefDataArrayName
  end;
end;

end.
