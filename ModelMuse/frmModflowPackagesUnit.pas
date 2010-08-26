{
  @name defines the form on which the user activates or deactivates
  MODFLOW packages.

  To add a new package, add a new page to
  @link(TfrmModflowPackages.jvplPackages)
  and put a @link(TFramePackage) or one of its descendants on it.
  Add additional controls if required.
  Add a new package to @link(TPhastModel.ModflowPackages
  frmGoPhast.PhastModel.ModflowPackages).
  Modfify @link(TfrmModflowPackages.GetData) and
  @link(TfrmModflowPackages.SetData) to use the new package and
  @link(TFramePackage). Modify TUndoChangePackageSelection.Changed to reflect
  the new package. Update @link(TModflowPackages.Assign),
  @link(TModflowPackages.Create), @link(TModflowPackages.Destroy),
  @link(TModflowPackages.Reset).
}
unit frmModflowPackagesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, JvPageList, JvExControls, ExtCtrls,
  JvExExtCtrls, JvNetscapeSplitter, ComCtrls, StdCtrls, JvExStdCtrls,
  JvCheckBox, framePackageUnit, Buttons, UndoItems, ModflowPackageSelectionUnit,
  ArgusDataEntry, framePcgUnit, ModflowPackagesUnit, Mask, JvExMask, JvSpin,
  RbwController, Grids, RbwDataGrid4, ModflowParameterUnit,
  ModflowTransientListParameterUnit, OrderedCollectionUnit,
  frameListParameterDefinitionUnit, frameArrayParameterDefinitionUnit,
  framePackageTransientLayerChoiceUnit, frameEtsPackageUnit, ImgList,
  framePackageResUnit, PhastModelUnit, GoPhastTypes, RbwParser,
  framePackageLAK_Unit, DataSetUnit, framePackageSFRUnit,
  framePackageLayerChoiceUnit, framePackageUZFUnit, frameGmgUnit, frameSipUnit,
  frameDe4Unit, JvExComCtrls, JvComCtrls, RequiredDataSetsUndoUnit,
  framePackageHobUnit, framePackageLpfUnit, frameModpathSelectionUnit,
  framePackageHufUnit, HufDefinition, framePackageMnw2Unit, framePackageSubUnit,
  frameZoneBudgetUnit, framePackageSwtUnit, framePkgHydmodUnit;

type
  TfrmModflowPackages = class(TfrmCustomGoPhast)
    tvPackages: TTreeView;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    jvplPackages: TJvPageList;
    jvspLPF: TJvStandardPage;
    jvspCHD: TJvStandardPage;
    framePkgCHD: TframePackage;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    jvspPCG: TJvStandardPage;
    framePCG: TframePCG;
    jvspGHB: TJvStandardPage;
    framePkgGHB: TframePackage;
    jvspWEL: TJvStandardPage;
    framePkgWEL: TframePackage;
    jvspRIV: TJvStandardPage;
    framePkgRIV: TframePackage;
    jvspDRN: TJvStandardPage;
    framePkgDRN: TframePackage;
    jvspDRT: TJvStandardPage;
    framePkgDRT: TframePackage;
    rbwLpfParamCountController: TRbwController;
    frameLpfParameterDefinition: TframeArrayParameterDefinition;
    tvLpfParameterTypes: TTreeView;
    JvNetscapeSplitter2: TJvNetscapeSplitter;
    frameChdParameterDefinition: TframeListParameterDefinition;
    frameDrnParameterDefinition: TframeListParameterDefinition;
    frameDrtParameterDefinition: TframeListParameterDefinition;
    frameGhbParameterDefinition: TframeListParameterDefinition;
    frameRivParameterDefinition: TframeListParameterDefinition;
    frameWelParameterDefinition: TframeListParameterDefinition;
    jvspRCH: TJvStandardPage;
    jvspEVT: TJvStandardPage;
    framePkgEVT: TframePackageTransientLayerChoice;
    frameEvtParameterDefinition: TframeListParameterDefinition;
    framePkgRCH: TframePackageTransientLayerChoice;
    frameRchParameterDefinition: TframeListParameterDefinition;
    jvspETS: TJvStandardPage;
    framePkgETS: TframeEtsPackage;
    frameEtsParameterDefinition: TframeListParameterDefinition;
    ilCheckImages: TImageList;
    jvspRES: TJvStandardPage;
    framePkgRES: TframePackageRes;
    jvspLAK: TJvStandardPage;
    framePkgLAK: TframePackageLAK;
    jvspSFR: TJvStandardPage;
    frameSFRParameterDefinition: TframeListParameterDefinition;
    framePkgSFR: TframePackageSFR;
    pcSFR: TJvPageControl;
    tabSfrGeneral: TTabSheet;
    tabSfrParameters: TTabSheet;
    jplSfrParameters: TJvPageList;
    splitSFR: TSplitter;
    jvspUZF: TJvStandardPage;
    framePkgUZF: TframePackageUZF;
    jvspGMG: TJvStandardPage;
    framePkgGMG: TframeGMG;
    jvspSIP: TJvStandardPage;
    framePkgSIP: TframeSIP;
    jvspDE4: TJvStandardPage;
    framePkgDE4: TframeDE4;
    jvspHOB: TJvStandardPage;
    framePkgHOB: TframePackageHob;
    jvspHFB: TJvStandardPage;
    framePkgHFB: TframePackage;
    frameHfbParameterDefinition: TframeListParameterDefinition;
    framePkgLPF: TframePackageLpf;
    jvspModpath: TJvStandardPage;
    frameModpath: TframeModpathSelection;
    jvspCHOB: TJvStandardPage;
    framePkgCHOB: TframePackage;
    jvspDROB: TJvStandardPage;
    jvspGBOB: TJvStandardPage;
    jvspRVOB: TJvStandardPage;
    framePkgDROB: TframePackage;
    framePkgGBOB: TframePackage;
    framePkgRVOB: TframePackage;
    JvNetscapeSplitter3: TJvNetscapeSplitter;
    jvspHUF: TJvStandardPage;
    framePkgHuf: TframePackageHuf;
    tvHufParameterTypes: TTreeView;
    JvNetscapeSplitter4: TJvNetscapeSplitter;
    JvNetscapeSplitter5: TJvNetscapeSplitter;
    frameHufParameterDefinition: TframeListParameterDefinition;
    rbwHufParamCountController: TRbwController;
    jvspMNW2: TJvStandardPage;
    framePkgMnw2: TframePackageMnw2;
    jvspBCF: TJvStandardPage;
    framePkgBCF: TframePackage;
    jvspSUB: TJvStandardPage;
    framePkgSUB: TframePackageSub;
    jvspZoneBudget: TJvStandardPage;
    frameZoneBudget: TframeZoneBudget;
    jvspSWT: TJvStandardPage;
    framePkgSwt: TframePackageSwt;
    jvspHydmod: TJvStandardPage;
    framePkgHydmod: TframePkgHydmod;
    procedure tvPackagesChange(Sender: TObject; Node: TTreeNode);
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure tvLpfParameterTypesChange(Sender: TObject; Node: TTreeNode);
    procedure frameParameterDefinition_seNumberOfParametersChange(
      Sender: TObject);
    procedure frameParameterDefinition_btnDeleteClick(Sender: TObject);
    procedure jvplPackagesChange(Sender: TObject);
    procedure tvPackagesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure framePkgSFRrcSelectionControllerEnabledChange(Sender: TObject);
    procedure framePkgSFRcbSfrUnsatflowClick(Sender: TObject);
    procedure framePkgSFRcbSfrLpfHydraulicCondClick(Sender: TObject);
    procedure framePkgSFRrgSfr2ISFROPTClick(Sender: TObject);
    procedure frameSFRParameterDefinitionseNumberOfParametersChange(
      Sender: TObject);
    procedure frameSFRParameterDefinitionbtnDeleteClick(Sender: TObject);
    procedure frameSFRParameterDefinitiondgParametersSelectCell(Sender: TObject;
      ACol, ARow: Integer; var CanSelect: Boolean);
    procedure frameSFRParameterDefinitiondgParametersSetEditText(
      Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure FormResize(Sender: TObject);
    procedure frameModpathrcSelectionControllerEnabledChange(Sender: TObject);
    procedure framePkgEVTrcSelectionControllerEnabledChange(Sender: TObject);
    procedure framePkgRCHrcSelectionControllerEnabledChange(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure tvHufParameterTypesChange(Sender: TObject; Node: TTreeNode);
    procedure framePkgHufrcSelectionControllerEnabledChange(Sender: TObject);
    procedure framePkgUZFrcSelectionControllerEnabledChange(Sender: TObject);
    procedure framePkgBCFrcSelectionControllerEnabledChange(Sender: TObject);
  private
    IsLoaded: boolean;
    CurrentParameterType: TParameterType;
    SteadyParameters: TModflowSteadyParameters;
    HufParameters: THufModflowParameters;
    TransientListParameters: TModflowTransientListParameters;
    PackageList: TList;
    FSfrParameterInstances: TSfrParamInstances;
    procedure AssignParameterToRow(ActiveGrid: TRbwDataGrid4; RowIndex: Integer;
      Parameter: TModflowParameter);
    procedure SetData;
    function NewParameterName: string;
    procedure FillLpfTree;
    function ParentFrame(Sender: TObject): TframeListParameterDefinition;
    procedure FillTransientGrids;
    procedure AddPackagesToList(Packages: TModflowPackages);
    procedure framePkgLPFSelectedChange(Sender: TObject);
    procedure EnableSfrParameters;
    procedure GetSfrParamInstances;
    procedure SetSfrParamInstances;
    procedure AdjustDroppedWidth(OwnerComponent: TComponent);
    procedure ReadPackages;
    procedure SetpcSFR_ClientBorderWidth;
    procedure CheckLpfParameters;
    function PackageUsed(const ID: string): boolean;
    procedure CheckSfrParameterInstances;
    procedure FillHfbGrid;
    procedure InitializeFrame(Frame: TframeListParameterDefinition);
    procedure EnableEvtModpathOption;
    procedure EnableRchModpathOption;
    procedure ChdSelectedChange(Sender: TObject);
    procedure DrnSelectedChange(Sender: TObject);
    procedure GhbSelectedChange(Sender: TObject);
    procedure RivSelectedChange(Sender: TObject);
    procedure FillHufTree;
    procedure UpdateFlowParamGrid(Node: TTreeNode;
      ParameterFrame: TframeListParameterDefinition;
      ParameterCollection: TCollection;
      ParameterFrameController: TRbwController);
    procedure ActivateHufReferenceChoice;
    procedure EnableUzfVerticalKSource;
    { Private declarations }
  public
    procedure GetData;
    { Public declarations }
  end;

  TUndoChangePackageSelection = class(TCustomUndoChangeParameters)
  private
    FOldPackages: TModflowPackages;
    FNewPackages: TModflowPackages;
    FOldSfrParameterInstances: TSfrParamInstances;
    FNewSfrParameterInstances: TSfrParamInstances;
    FOldHydroGeologicUnits: THydrogeologicUnits;
    FOldInterBlockTransmissivity: array of integer;
    FOldAquiferType: array of integer;
    procedure UpdateLayerGroupProperties(BcfPackage: TModflowPackageSelection);
  protected
    function Description: string; override;
  public
    Constructor Create(var NewSteadyParameters: TModflowSteadyParameters;
      var NewTransientParameters: TModflowTransientListParameters;
      var SfrParameterInstances: TSfrParamInstances;
      var NewHufModflowParameters: THufModflowParameters);
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

var
  frmModflowPackages: TfrmModflowPackages = nil;

implementation

uses Contnrs, JvListComb, frmGoPhastUnit, ScreenObjectUnit,
  ModflowConstantHeadBoundaryUnit, frmGridColorUnit, frmShowHideObjectsUnit,
  frameSfrParamInstancesUnit, LayerStructureUnit, frmErrorsAndWarningsUnit, 
  frmManageFluxObservationsUnit, ModflowSubsidenceDefUnit;

{$R *.dfm}

type
  TParameterColumns = (pcName, pcValue, pcUseZone, pcUseMultiplier);

procedure TfrmModflowPackages.AssignParameterToRow(ActiveGrid: TRbwDataGrid4;
  RowIndex: Integer; Parameter: TModflowParameter);
begin
  ActiveGrid.Objects[0, RowIndex] := Parameter;
  ActiveGrid.Cells[Ord(pcName), RowIndex] := Parameter.ParameterName;
  ActiveGrid.Cells[Ord(pcValue), RowIndex] := FloatToStr(Parameter.Value);
  if (Parameter is TModflowSteadyParameter)
    and not (Parameter.ParameterType = ptHFB) then
  begin
    ActiveGrid.Checked[Ord(pcUseZone), RowIndex] :=
      TModflowSteadyParameter(Parameter).UseZone;
    ActiveGrid.Checked[Ord(pcUseMultiplier), RowIndex] :=
      TModflowSteadyParameter(Parameter).UseMultiplier;
  end;
end;

function TfrmModflowPackages.PackageUsed(const ID: string): boolean;
var
  PackageIndex : Integer;
  Package: TModflowPackageSelection;
begin
  result := false;
  for PackageIndex := 0 to PackageList.Count - 1 do
  begin
    Package := PackageList[PackageIndex];
    if Package.PackageIdentifier = ID then
    begin
      result := Package.IsSelected;
      Exit;
    end;
  end;
end;

procedure TfrmModflowPackages.CheckSfrParameterInstances;
var
  PageIndex: Integer;
  Page: TJvCustomPage;
  Frame: TframeSfrParamInstances;
  ParameterName: string;
  InstanceIndex: Integer;
  InstanceName: string;
  StartTime: double;
  EndTime: double;
  InvalidParameterNames: TStringList;
  IsInvalid: boolean;
  ParamNameIndex: Integer;
begin
  // Don't check SFR parameters if the SFR package isn't used.
  if not PackageUsed(StrSFR_Identifier) then Exit;
  // Don't check SFR parameters if SFR parameters can't be used.
  if framePkgSFR.CalculateISFROPT <> 0 then Exit;

  InvalidParameterNames := TStringList.Create;
  try
    for PageIndex := 0 to jplSfrParameters.PageCount - 1 do
    begin
      Page := jplSfrParameters.Pages[PageIndex];
      Assert(Page.ControlCount= 1);
      Frame := Page.Controls[0] as TframeSfrParamInstances;
      ParameterName := frameSFRParameterDefinition.dgParameters.Cells[Ord(pcName), PageIndex+1];
      if ParameterName <> '' then
      begin
        IsInvalid := True;
        for InstanceIndex := 1 to Frame.seInstanceCount.AsInteger do
        begin
          InstanceName := Frame.rdgSfrParamInstances.Cells[Ord(sicInstanceName), InstanceIndex];

          if (InstanceName <> '')
            and TryStrToFloat(Frame.rdgSfrParamInstances.
              Cells[Ord(sicStartTime), InstanceIndex], StartTime)
            and TryStrToFloat(Frame.rdgSfrParamInstances.
              Cells[Ord(sicEndTime), InstanceIndex], EndTime) then
          begin
            IsInvalid := False;
            break;
          end;
        end;
        if IsInvalid then
        begin
          InvalidParameterNames.Add(ParameterName);
        end;
      end;
    end;

    frmErrorsAndWarnings.RemoveWarningGroup('SFR Parameters');
    if InvalidParameterNames.Count > 0 then
    begin
      for ParamNameIndex := 0 to InvalidParameterNames.Count - 1 do
      begin
        frmErrorsAndWarnings.AddWarning('SFR Parameters',
          'The SFR parameter named "' + InvalidParameterNames[ParamNameIndex]
          + '" can''t be used because no parameter instances are defined '
          + 'for it.');
      end;
      frmErrorsAndWarnings.Show;
    end;
  finally
    InvalidParameterNames.Free;
  end;
end;

procedure TfrmModflowPackages.CheckLpfParameters;
var
  ParamIndex: Integer;
  Param: TModflowSteadyParameter;
  VKCB_Defined: boolean;
  VK_Defined: boolean;
  VANI_Defined: boolean;
  SS_Defined: boolean;
  SY_Defined: boolean;
  Quasi3dUsed: boolean;
  VerticalHydraulicConductivityUsed: boolean;
  VerticalAnisotropyUsed: boolean;
  UnitIndex: Integer;
  LayerGroup: TLayerGroup;
  ShowErrors: boolean;
  SpecificStorageUsed: boolean;
  SpecificYieldUsed: boolean;
begin
  if not PackageUsed(StrLPF_Identifier) then Exit;

  VKCB_Defined := False;
  VK_Defined := False;
  VANI_Defined := False;
  SS_Defined := False;
  SY_Defined := False;
  for ParamIndex := 0 to SteadyParameters.Count - 1 do
  begin
    Param := SteadyParameters[ParamIndex];
    case Param.ParameterType of
      ptLPF_VK: VK_Defined := True;
      ptLPF_VANI: VANI_Defined := True;
      ptLPF_VKCB: VKCB_Defined := True;
      ptLPF_SS: SS_Defined := True;
      ptLPF_SY: SY_Defined := True;
    end;
  end;

  Quasi3dUsed := False;
  VerticalHydraulicConductivityUsed := False;
  VerticalAnisotropyUsed := False;
  if frmGoPhast.PhastModel.LayerStructure.ModflowLayerCount > 1 then
  begin
    // Skip the top of the model: it doesn't count.
    for UnitIndex := 1 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
    begin
      LayerGroup := frmGoPhast.PhastModel.LayerStructure[UnitIndex];
      if LayerGroup.Simulated then
      begin
        if LayerGroup.VerticalHydraulicConductivityMethod = 0 then
        begin
          VerticalHydraulicConductivityUsed := True;
        end
        else
        begin
          VerticalAnisotropyUsed := True;
        end;
      end
      else
      begin
        Quasi3dUsed := True;
      end;
    end;
  end;

  frmErrorsAndWarnings.RemoveWarningGroup('LPF Parameters');
  SpecificStorageUsed := False;
  SpecificYieldUsed := False;
  if frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel then
  begin
    SpecificStorageUsed := True;
// Skip the top of the model: it doesn't count.
    for UnitIndex := 1 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
    begin
      LayerGroup := frmGoPhast.PhastModel.LayerStructure[UnitIndex];
      if LayerGroup.Simulated then
      begin
        if LayerGroup.AquiferType <> 0 then
        begin
          SpecificYieldUsed := True;
        end
      end;
    end;
  end;

  ShowErrors := False;
  if VKCB_Defined and not Quasi3dUsed then
  begin
    ShowErrors := True;
    frmErrorsAndWarnings.AddWarning('LPF Parameters',
      'One or more VKCB parameters are defined in the LPF package '
      + 'but they won''t be used because'
      + ' all the layers are simulated.');
  end;

  if VK_Defined and not VerticalHydraulicConductivityUsed then
  begin
    ShowErrors := True;
    if frmGoPhast.PhastModel.LayerStructure.ModflowLayerCount = 1 then
    begin
      frmErrorsAndWarnings.AddWarning('LPF Parameters',
        'One or more VK parameters are defined in the LPF package '
        + 'but they won''t be used because'
        + ' there is only one layer in the model.');
    end
    else
    begin
      frmErrorsAndWarnings.AddWarning('LPF Parameters',
        'One or more VK parameters are defined in the LPF package '
        + 'but they won''t be used because'
        + ' vertical anisotropy is used for all the layers. '
        + 'Check the MODFLOW Layers dialog box if you want to use '
        + 'vertical anisotropy.');
    end;
  end;

  if VANI_Defined and not VerticalAnisotropyUsed then
  begin
    ShowErrors := True;
    if frmGoPhast.PhastModel.LayerStructure.ModflowLayerCount = 1 then
    begin
      frmErrorsAndWarnings.AddWarning('LPF Parameters',
        'One or more VANI parameters are defined in the LPF package '
        + 'but they won''t be used because'
        + ' there is only one layer in the model.');
    end
    else
    begin
      frmErrorsAndWarnings.AddWarning('LPF Parameters',
        'One or more VANI parameters are defined in the LPF package '
        + 'but they won''t be used because'
        + ' vertical hydraulic hydraulic conductivity is used for all the layers. '
        + 'Check the MODFLOW Layers dialog box if you want to use '
        + 'vertical anisotropy.');
    end;
  end;

  if SS_Defined and not SpecificStorageUsed then
  begin
    ShowErrors := True;
    frmErrorsAndWarnings.AddWarning('LPF Parameters',
      'One or more SS parameters are defined in the LPF package '
      + 'but they won''t be used because'
      + ' there are no transient stress periods in the model.');
  end;

  if SY_Defined and not SpecificYieldUsed then
  begin
    ShowErrors := True;
    if not frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel then
    begin
      frmErrorsAndWarnings.AddWarning('LPF Parameters',
        'One or more SY parameters are defined in the LPF package '
        + 'but they won''t be used because'
        + ' there are no transient stress periods in the model.');
    end
    else
    begin
      frmErrorsAndWarnings.AddWarning('LPF Parameters',
        'One or more SY parameters are defined in the LPF package '
        + 'but they won''t be used because'
        + ' all of the layers are confined.');
    end;
  end;

  if ShowErrors then
  begin
    frmErrorsAndWarnings.Show;
  end;

end;

procedure TfrmModflowPackages.btnOKClick(Sender: TObject);
var
  NeedToDefineFluxObservations: Boolean;
  ModflowPackages: TModflowPackages;
  SubPackage: TSubPackageSelection;
  SwtPackage: TSwtPackageSelection;
begin
  inherited;
  CheckLpfParameters;
  CheckSfrParameterInstances;

  ModflowPackages := frmGoPhast.PhastModel.ModflowPackages;
  NeedToDefineFluxObservations := False;
  if framePkgCHOB.Selected and
    not ModflowPackages.ChobPackage.IsSelected then
  begin
    NeedToDefineFluxObservations := True;
  end
  else if framePkgDROB.Selected and
    not ModflowPackages.DrobPackage.IsSelected then
  begin
    NeedToDefineFluxObservations := True;
  end
  else if framePkgGBOB.Selected and
    not ModflowPackages.GbobPackage.IsSelected then
  begin
    NeedToDefineFluxObservations := True;
  end
  else if framePkgRVOB.Selected and
    not ModflowPackages.RvobPackage.IsSelected then
  begin
    NeedToDefineFluxObservations := True;
  end;

  SetData;

  SubPackage := ModflowPackages.SubPackage;
  if SubPackage.IsSelected then
  begin
    SubPackage.PrintChoices.ReportErrors;
  end;

  SwtPackage := ModflowPackages.SwtPackage;
  if SwtPackage.IsSelected then
  begin
    SwtPackage.PrintChoices.ReportErrors;
  end;

  if NeedToDefineFluxObservations then
  begin
    Hide;
    ShowAForm(TfrmManageFluxObservations);
  end;

  if framePkgHuf.Selected
    and (frmGoPhast.PhastModel.HydrogeologicUnits.Count = 0) then
  begin
    frmGoPhast.miMF_HydrogeologicUnitsClick(nil);
  end;

  if (frmGoPhast.PhastModel.ModflowPackages.SubPackage.IsSelected
    and not frmGoPhast.PhastModel.LayerStructure.SubsidenceDefined)
    or (frmGoPhast.PhastModel.ModflowPackages.SwtPackage.IsSelected
    and not frmGoPhast.PhastModel.LayerStructure.SwtDefined) then
  begin
    frmGoPhast.acLayersExecute(nil);
  end;
end;

procedure TfrmModflowPackages.FillHufTree;
var
  PriorBaseNode: TTreeNode;
  ChildNode: TTreeNode;
  HkNode: TTreeNode;
begin
  tvHufParameterTypes.Items.Clear;
  
  PriorBaseNode := nil;

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    'HK (horizontal hydraulic conductivity)');
  ChildNode.Data := Pointer(ptHUF_HK);
  HkNode := ChildNode;

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    'HANI (horizontal anisotropy)');
  ChildNode.Data := Pointer(ptHUF_HANI);

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    'VK (vertical hydraulic conductivity)');
  ChildNode.Data := Pointer(ptHUF_VK);

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    'VANI (vertical anisotropy)');
  ChildNode.Data := Pointer(ptHUF_VANI);

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    'SS (specific storage)');
  ChildNode.Data := Pointer(ptHUF_SS);

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    'SY (specific yield)');
  ChildNode.Data := Pointer(ptHUF_SY);

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    'SYTP (storage coefficient for the top active cell)');
  ChildNode.Data := Pointer(ptHUF_SYTP);

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    'KDEP (hydraulic conductivity depth-dependence coefficient)');
  ChildNode.Data := Pointer(ptHUF_KDEP);

  ChildNode := tvHufParameterTypes.Items.Add(PriorBaseNode,
    'LVDA (horizontal anisotropy angle)');
  ChildNode.Data := Pointer(ptHUF_LVDA);

  tvHufParameterTypes.Selected := HkNode;
end;

procedure TfrmModflowPackages.FillLpfTree;
var
  PriorBaseNode: TTreeNode;
  ChildNode: TTreeNode;
  HkNode: TTreeNode;
begin
  tvLpfParameterTypes.Items.Clear;

  PriorBaseNode := nil;

  ChildNode := tvLpfParameterTypes.Items.Add(PriorBaseNode,
    'HK (horizontal hydraulic conductivity)');
  ChildNode.Data := Pointer(ptLPF_HK);
  HkNode := ChildNode;

  ChildNode := tvLpfParameterTypes.Items.Add(PriorBaseNode,
    'HANI (horizontal anisotropy)');
  ChildNode.Data := Pointer(ptLPF_HANI);

  ChildNode := tvLpfParameterTypes.Items.Add(PriorBaseNode,
    'VK (vertical hydraulic conductivity)');
  ChildNode.Data := Pointer(ptLPF_VK);

  ChildNode := tvLpfParameterTypes.Items.Add(PriorBaseNode,
    'VANI (vertical anisotropy)');
  ChildNode.Data := Pointer(ptLPF_VANI);

  ChildNode := tvLpfParameterTypes.Items.Add(PriorBaseNode,
    'SS (specific storage)');
  ChildNode.Data := Pointer(ptLPF_SS);

  ChildNode := tvLpfParameterTypes.Items.Add(PriorBaseNode,
    'SY (specific yield)');
  ChildNode.Data := Pointer(ptLPF_SY);

  ChildNode := tvLpfParameterTypes.Items.Add(PriorBaseNode,
    'VKCB (vertical hydraulic conductivity of confining layer)');
  ChildNode.Data := Pointer(ptLPF_VKCB);

  tvLpfParameterTypes.Selected := HkNode;
end;

procedure TfrmModflowPackages.framePkgBCFrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  EnableUzfVerticalKSource;
end;

procedure TfrmModflowPackages.framePkgEVTrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
//  framePkgEVT.rcSelectionControllerEnabledChange(nil);
  EnableEvtModpathOption;
end;

procedure TfrmModflowPackages.framePkgHufrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  ActivateHufReferenceChoice;
end;

procedure TfrmModflowPackages.framePkgLPFSelectedChange(Sender: TObject);
begin
  framePkgSFR.LpfUsed := framePkgLPF.Selected;
end;

procedure TfrmModflowPackages.ChdSelectedChange(Sender: TObject);
begin
  framePkgCHOB.CanSelect := framePkgCHD.Selected;
  if not framePkgCHOB.CanSelect then
  begin
    framePkgCHOB.Selected := False;
  end;
end;

procedure TfrmModflowPackages.DrnSelectedChange(Sender: TObject);
begin
  framePkgDROB.CanSelect := framePkgDRN.Selected;
  if not framePkgDROB.CanSelect then
  begin
    framePkgDROB.Selected := False;
  end;
end;

procedure TfrmModflowPackages.GhbSelectedChange(Sender: TObject);
begin
  framePkgGBOB.CanSelect := framePkgGHB.Selected;
  if not framePkgGBOB.CanSelect then
  begin
    framePkgGBOB.Selected := False;
  end;
end;

procedure TfrmModflowPackages.RivSelectedChange(Sender: TObject);
begin
  framePkgRVOB.CanSelect := framePkgRIV.Selected;
  if not framePkgRVOB.CanSelect then
  begin
    framePkgRVOB.Selected := False;
  end;
end;

procedure TfrmModflowPackages.framePkgRCHrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
//  framePkgRCH.rcSelectionControllerEnabledChange(nil);
  EnableRchModpathOption;
end;

procedure TfrmModflowPackages.EnableSfrParameters;
var
  OldActivePage: TTabSheet;
begin
  frameSFRParameterDefinition.Enabled :=
    framePkgSFR.rcSelectionController.Enabled
    and (framePkgSFR.CalculateISFROPT = 0);
  OldActivePage := pcSFR.ActivePage;
  tabSfrParameters.TabVisible := frameSFRParameterDefinition.Enabled;
  pcSFR.ActivePage := OldActivePage;
  if not frameSFRParameterDefinition.Enabled then
  begin
    frameSFRParameterDefinition.seNumberOfParameters.OnEnter(
      frameSFRParameterDefinition.seNumberOfParameters);
    frameSFRParameterDefinition.seNumberOfParameters.Value := 0;
    frameSFRParameterDefinition.seNumberOfParameters.OnChange(
      frameSFRParameterDefinition.seNumberOfParameters);
  end;
  tabSfrGeneral.TabVisible := tabSfrParameters.TabVisible;
  if not tabSfrGeneral.TabVisible then
  begin
    pcSFR.ActivePage := tabSfrGeneral;
  end;
  SetpcSFR_ClientBorderWidth;
end;

procedure TfrmModflowPackages.SetpcSFR_ClientBorderWidth;
begin
  if tabSfrParameters.TabVisible then
  begin
    pcSFR.ClientBorderWidth := 4;
  end
  else
  begin
    pcSFR.ClientBorderWidth := 0;
  end;
end;

procedure TfrmModflowPackages.framePkgSFRcbSfrLpfHydraulicCondClick(
  Sender: TObject);
begin
  inherited;
  EnableSfrParameters;
end;

procedure TfrmModflowPackages.framePkgSFRcbSfrUnsatflowClick(Sender: TObject);
begin
  inherited;
  EnableSfrParameters;
  framePkgSFR.rcSelectionControllerEnabledChange(Sender);
end;

procedure TfrmModflowPackages.framePkgSFRrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  framePkgSFR.rcSelectionControllerEnabledChange(Sender);
  EnableSfrParameters;
end;

procedure TfrmModflowPackages.framePkgSFRrgSfr2ISFROPTClick(Sender: TObject);
begin
  inherited;
  EnableSfrParameters;
end;

procedure TfrmModflowPackages.framePkgUZFrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  EnableUzfVerticalKSource;
end;

procedure TfrmModflowPackages.frameSFRParameterDefinitionbtnDeleteClick(
  Sender: TObject);
var
  PageIndex: integer;
  Page: TJvCustomPage;
  Frame: TframeSfrParamInstances;
begin
  inherited;
  frameParameterDefinition_btnDeleteClick(Sender);
  frameSFRParameterDefinition.btnDeleteClick(frameSFRParameterDefinition.btnDelete);
  PageIndex := frameSFRParameterDefinition.dgParameters.SelectedRow -1;
  frameSFRParameterDefinition.btnDeleteClick(frameSFRParameterDefinition.btnDelete);
  if (PageIndex >= 0) and (PageIndex < jplSfrParameters.PageCount) then
  begin
    Page := jplSfrParameters.Pages[PageIndex];
    Assert(Page.ControlCount= 1);
    Frame := Page.Controls[0] as TframeSfrParamInstances;
    Frame.Free;
    Page.Free;
  end;
end;

procedure TfrmModflowPackages.frameSFRParameterDefinitiondgParametersSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  PageIndex: integer;
begin
  inherited;
  frameSFRParameterDefinition.dgParametersSelectCell(Sender, ACol, ARow, CanSelect);
  PageIndex := frameSFRParameterDefinition.dgParameters.SelectedRow -1;
  if PageIndex < jplSfrParameters.PageCount then
  begin
    jplSfrParameters.ActivePageIndex := PageIndex;
  end;
end;

procedure TfrmModflowPackages.frameSFRParameterDefinitiondgParametersSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
var
  PageIndex: Integer;
  Page: TJvCustomPage;
  Frame: TframeSfrParamInstances;
begin
  inherited;
  frameSFRParameterDefinition.dgParametersSetEditText(Sender, ACol, ARow, Value);
  PageIndex := ARow -1;
  if PageIndex < jplSfrParameters.PageCount then
  begin
    Page := jplSfrParameters.Pages[PageIndex];
    Assert(Page.ControlCount= 1);
    Frame := Page.Controls[0] as TframeSfrParamInstances;
    Frame.pnlLabel.Caption := frameSFRParameterDefinition.
      dgParameters.Cells[Ord(pcName),PageIndex+1];
  end;
end;

procedure TfrmModflowPackages.frameSFRParameterDefinitionseNumberOfParametersChange(
  Sender: TObject);
var
  Page: TJvCustomPage;
  Frame: TframeSfrParamInstances;
  PageIndex: integer;
begin
  inherited;
  frameSFRParameterDefinition.seNumberOfParametersChange(Sender);
  frameParameterDefinition_seNumberOfParametersChange(Sender);
  while jplSfrParameters.PageCount <
    frameSFRParameterDefinition.seNumberOfParameters.AsInteger do
  begin
    Page := TJvCustomPage.Create(self);
    Page.PageList := jplSfrParameters;
    Frame := TframeSfrParamInstances.Create(self);
    Frame.Name := '';
    Frame.Parent := Page;
    Frame.pnlLabel.Caption := frameSFRParameterDefinition.
      dgParameters.Cells[Ord(pcName),jplSfrParameters.PageCount];
    Frame.btnDeleteFlowTableRow.Left := jplSfrParameters.Width
      - Frame.btnDeleteFlowTableRow.Width - 8;
    Frame.btnInsertFlowTableRow.Left := Frame.btnDeleteFlowTableRow.Left
      - Frame.btnInsertFlowTableRow.Width - 8;

    frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithStartTimes
      (Frame.rdgSfrParamInstances, Ord(sicStartTime));
    frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithEndTimes
      (Frame.rdgSfrParamInstances, Ord(sicEndTime));

    Frame.seInstanceCount.AsInteger := 0;
  end;
  while jplSfrParameters.PageCount >
    frameSFRParameterDefinition.seNumberOfParameters.AsInteger do
  begin
    Page := jplSfrParameters.Pages[jplSfrParameters.PageCount-1];
    Assert(Page.ControlCount= 1);
    Frame := Page.Controls[0] as TframeSfrParamInstances;
    Frame.Free;
    Page.Free;
  end;
  if frameSFRParameterDefinition.dgParameters.SelectedRow >= 1 then
  begin
    PageIndex := frameSFRParameterDefinition.dgParameters.SelectedRow-1
  end
  else
  begin
    PageIndex := 0;
  end;
  if PageIndex < jplSfrParameters.PageCount then
  begin
    jplSfrParameters.ActivePageIndex := PageIndex;
  end;
end;

procedure TfrmModflowPackages.FormCreate(Sender: TObject);
begin
  inherited;
  framePkgGMG.pcGMG.ActivePageIndex := 0;
end;

procedure TfrmModflowPackages.FormDestroy(Sender: TObject);
begin
  inherited;
  SteadyParameters.Free;
  HufParameters.Free;
  TransientListParameters.Free;
  PackageList.Free;
  FSfrParameterInstances.Free;
end;

procedure TfrmModflowPackages.FormResize(Sender: TObject);
begin
  inherited;
  AdjustDroppedWidth(self);
end;

procedure TfrmModflowPackages.frameModpathrcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  frameModpath.rcSelectionControllerEnabledChange(nil);
  EnableEvtModpathOption;
  EnableRchModpathOption;
end;

procedure TfrmModflowPackages.frameParameterDefinition_btnDeleteClick(
  Sender: TObject);
var
  ActiveFrame: TframeListParameterDefinition;
begin
  inherited;
  ActiveFrame := ParentFrame(Sender);
  ActiveFrame.btnDeleteClick(Sender);
  case CurrentParameterType of
    ptLPF_HK..ptLPF_VKCB, ptHFB, ptHUF_SYTP, ptHUF_LVDA:
      begin
        SteadyParameters.Remove(ActiveFrame.CurrentParameter);
      end;
    ptCHD..ptDRT, ptSFR, ptRCH, ptEVT, ptETS:
      begin
        TransientListParameters.Remove(ActiveFrame.CurrentParameter);
      end;
    ptHUF_HK..ptHUF_SY, ptHUF_KDEP:
      begin
        HufParameters.Remove(ActiveFrame.CurrentParameter);
      end
    else Assert(False);
  end;

end;

function TfrmModflowPackages.ParentFrame(Sender: TObject): TframeListParameterDefinition;
var
  Control: TControl;
begin
  result := nil;
  Control := Sender as TControl;
  while Control <> nil do
  begin
    if Control is TframeListParameterDefinition then
    begin
      result := TframeListParameterDefinition(Control);
      Exit;
    end;
    Control := Control.Parent;
  end;
end;

procedure TfrmModflowPackages.ActivateHufReferenceChoice;
begin
  framePkgHuf.rgElevationSurfaceChoice.Enabled :=
    framePkgHuf.rcSelectionController.Enabled
    and (HufParameters.CountParameters([ptHUF_KDEP]) > 0);
end;

procedure TfrmModflowPackages.frameParameterDefinition_seNumberOfParametersChange(
  Sender: TObject);
var
  RowIndex: Integer;
  Parameter: TModflowParameter;
  ActiveGrid: TRbwDataGrid4;
  ActiveFrame: TframeListParameterDefinition;
begin
  inherited;
  ActiveFrame := ParentFrame(Sender);
  if Sender = ActiveFrame.btnDelete then
  begin
    ActiveFrame.PriorNumberOfParameters :=
      ActiveFrame.seNumberOfParameters.AsInteger;
  end;
  ActiveGrid := ActiveFrame.dgParameters;
  ActiveFrame.seNumberOfParametersChange(nil);
  if not IsLoaded then
  begin
    ActiveFrame.PriorNumberOfParameters := ActiveFrame.seNumberOfParameters.AsInteger;
    Exit;
  end;

  // Create new parameters.
  for RowIndex := ActiveFrame.PriorNumberOfParameters+1 to
    ActiveFrame.seNumberOfParameters.AsInteger do
  begin
    Parameter := nil;
    case CurrentParameterType of
      ptLPF_HK..ptLPF_VKCB, ptHFB, ptHUF_SYTP, ptHUF_LVDA:
        begin
          Parameter := SteadyParameters.Add as TModflowParameter;
        end;
      ptCHD..ptSFR, ptRCH, ptEVT, ptETS:
        begin
          Parameter := TransientListParameters.Add as TModflowParameter;
        end;
      ptHUF_HK..ptHUF_SY, ptHUF_KDEP:
        begin
          Parameter := HufParameters.Add as TModflowParameter;
        end;
      else Assert(False);
    end;
    Parameter.ParameterType := CurrentParameterType;
    Parameter.ParameterName := NewParameterName;
    AssignParameterToRow(ActiveGrid, RowIndex, Parameter);
  end;

  // Get rid of old parameters.
  for RowIndex := ActiveFrame.PriorNumberOfParameters downto
    ActiveFrame.seNumberOfParameters.AsInteger+1 do
  begin
    Parameter := ActiveGrid.Objects[0, RowIndex] as TModflowParameter;
    if Parameter <> nil then
    begin
      
      if Parameter is TModflowSteadyParameter then
      begin
        SteadyParameters.Remove(Parameter);
      end
      else if Parameter is TModflowTransientListParameter then
      begin
        TransientListParameters.Remove(Parameter);
      end
      else
      begin
        Assert(Parameter is THufParameter);
        HufParameters.Remove(Parameter);
      end;
    end;
  end;

  ActiveFrame.PriorNumberOfParameters :=
    ActiveFrame.seNumberOfParameters.AsInteger;
  ActiveGrid.Invalidate;

  ActivateHufReferenceChoice;
end;

procedure TfrmModflowPackages.SetSfrParamInstances;
var
  PageIndex: integer;
  Page: TJvCustomPage;
  Frame: TframeSfrParamInstances;
  ParameterName: string;
  InstanceIndex: Integer;
  Item: TSfrParamInstance;
  InstanceName: string;
  StartTime, EndTime: double;
  ExistingInstances: TList;
  Index: integer;
  Position: integer;
begin
  ExistingInstances := TList.Create;
  try
    for PageIndex := 0 to jplSfrParameters.PageCount - 1 do
    begin
      Page := jplSfrParameters.Pages[PageIndex];
      Assert(Page.ControlCount= 1);
      Frame := Page.Controls[0] as TframeSfrParamInstances;
      ParameterName := frameSFRParameterDefinition.dgParameters.
        Cells[Ord(pcName), PageIndex+1];
      if ParameterName <> '' then
      begin
        for InstanceIndex := 1 to Frame.seInstanceCount.AsInteger do
        begin
          InstanceName := Frame.rdgSfrParamInstances.Cells[Ord(sicInstanceName), InstanceIndex];

          if (InstanceName <> '')
            and TryStrToFloat(Frame.rdgSfrParamInstances.
              Cells[Ord(sicStartTime), InstanceIndex], StartTime)
            and TryStrToFloat(Frame.rdgSfrParamInstances.
              Cells[Ord(sicEndTime), InstanceIndex], EndTime) then
          begin
            Item := Frame.rdgSfrParamInstances.
              Objects[Ord(sicInstanceName), InstanceIndex] as TSfrParamInstance;
            if Item <> nil then
            begin
              ExistingInstances.Add(Item);
            end;
          end;
        end;
      end;
    end;
    for Index := FSfrParameterInstances.Count - 1 downto 0 do
    begin
      Item := FSfrParameterInstances.Items[Index];
      if ExistingInstances.IndexOf(Item) < 0 then
      begin
        FSfrParameterInstances.Delete(Index);
      end;
    end;

    Position := 0;
    for PageIndex := 0 to jplSfrParameters.PageCount - 1 do
    begin
      Page := jplSfrParameters.Pages[PageIndex];
      Assert(Page.ControlCount= 1);
      Frame := Page.Controls[0] as TframeSfrParamInstances;
      ParameterName := frameSFRParameterDefinition.dgParameters.
        Cells[Ord(pcName), PageIndex+1];
      if ParameterName <> '' then
      begin
        for InstanceIndex := 1 to Frame.seInstanceCount.AsInteger do
        begin
          InstanceName := Frame.rdgSfrParamInstances.Cells[Ord(sicInstanceName), InstanceIndex];

          if (InstanceName <> '')
            and TryStrToFloat(Frame.rdgSfrParamInstances.
              Cells[Ord(sicStartTime), InstanceIndex], StartTime)
            and TryStrToFloat(Frame.rdgSfrParamInstances.
              Cells[Ord(sicEndTime), InstanceIndex], EndTime) then
          begin
            Item := Frame.rdgSfrParamInstances.
              Objects[Ord(sicInstanceName), InstanceIndex] as TSfrParamInstance;
            if Item = nil then
            begin
              Item := FSfrParameterInstances.Insert(Position) as TSfrParamInstance;
            end;
            Inc(Position);
            Item.ParameterName := ParameterName;
            Item.ParameterInstance := InstanceName;
            Item.StartTime := StartTime;
            Item.EndTime := EndTime;
          end;
        end;
      end;
    end;
  finally
    ExistingInstances.Free;
  end;
end;

procedure TfrmModflowPackages.AdjustDroppedWidth(OwnerComponent: TComponent);
var
  Combo: TJvImageComboBox;
  AComponent: TComponent;
  ComponentIndex: Integer;
begin
  for ComponentIndex := 0 to OwnerComponent.ComponentCount - 1 do
  begin
    AComponent := OwnerComponent.Components[ComponentIndex];
    if AComponent is TJvImageComboBox then
    begin
      Combo := TJvImageComboBox(AComponent);
      Combo.DroppedWidth := Combo.Width;
    end
    else if AComponent is TFrame then
    begin
      AdjustDroppedWidth(AComponent);
    end;
  end;
end;

procedure TfrmModflowPackages.GetSfrParamInstances;
var
  ParameterNames: TStringList;
  Index: integer;
  SfrParameterInstances: TSfrParamInstances;
  Item: TSfrParamInstance;
  PageIndex: integer;
  Page: TJvCustomPage;
  Frame: TframeSfrParamInstances;
  Row: integer;
begin
  if not frmGoPhast.PhastModel.ModflowPackages.SfrPackage.IsSelected then
  begin
    FSfrParameterInstances.Free;
    FSfrParameterInstances := TSfrParamInstances.Create(nil);
    Exit;
  end;

  Assert(jplSfrParameters.PageCount =
    frameSFRParameterDefinition.seNumberOfParameters.AsInteger);
  ParameterNames := TStringList.Create;
  try
    ParameterNames.Assign(
      frameSFRParameterDefinition.dgParameters.Cols[Ord(pcName)]);
    ParameterNames.Delete(0);

    SfrParameterInstances :=
      frmGoPhast.PhastModel.ModflowPackages.SfrPackage.ParameterInstances;
    FSfrParameterInstances.Free;
    FSfrParameterInstances := TSfrParamInstances.Create(nil);
    FSfrParameterInstances.Assign(SfrParameterInstances);

    for Index := 0 to FSfrParameterInstances.Count - 1 do
    begin
      Item := FSfrParameterInstances.Items[Index];
      PageIndex := ParameterNames.IndexOf(Item.ParameterName);
      Assert(PageIndex >= 0);
      Page := jplSfrParameters.Pages[PageIndex];
      Assert(Page.ControlCount= 1);
      Frame := Page.Controls[0] as TframeSfrParamInstances;
      Frame.seInstanceCount.AsInteger := Frame.seInstanceCount.AsInteger +1;
      Row := Frame.seInstanceCount.AsInteger;
      Frame.rdgSfrParamInstances.Cells[Ord(sicStartTime), Row] :=
        FloatToStr(Item.StartTime);
      Frame.rdgSfrParamInstances.Cells[Ord(sicEndTime), Row] :=
        FloatToStr(Item.EndTime);
      Frame.rdgSfrParamInstances.Cells[Ord(sicInstanceName), Row] :=
        Item.ParameterInstance;
      Frame.rdgSfrParamInstances.Objects[Ord(sicInstanceName), Row] := Item;
    end;
  finally
    ParameterNames.Free;
  end;
end;

procedure TfrmModflowPackages.ReadPackages;
var
  Index: Integer;
  APackage: TModflowPackageSelection;
  Frame: TframePackage;
  Page: TJvStandardPage;
  PriorNode: TTreeNode;
  TreeNodeList: TStringList;
  ParentNode: TTreeNode;
  ChildNode: TTreeNode;
  NodeIndex: integer;
  AControl: TControl;
  procedure AddNode(const Key, Caption: string; var PriorNode: TTreeNode);
  begin
    PriorNode := tvPackages.Items.Add(PriorNode, Caption);
    TreeNodeList.AddObject(Key, PriorNode);
  end;
  procedure AddChildNode(const Key, Caption: string; ParentNode: TTreeNode);
  var
    ChildNode: TTreeNode;
  begin
    ChildNode := tvPackages.Items.AddChild(ParentNode, Caption);
    TreeNodeList.AddObject(Key, ChildNode);
  end;
begin
  frameSFRParameterDefinition.seNumberOfParameters.AsInteger := 0;
  frameSFRParameterDefinitionseNumberOfParametersChange(
    frameSFRParameterDefinition.seNumberOfParameters);

  SteadyParameters.Assign(frmGoPhast.PhastModel.ModflowSteadyParameters);
  TransientListParameters.Assign(frmGoPhast.PhastModel.ModflowTransientParameters);
  HufParameters.Assign(frmGoPhast.PhastModel.HufParameters);

  AddPackagesToList(frmGoPhast.PhastModel.ModflowPackages);

  tvPackages.Items.Clear;
  TreeNodeList := TStringList.Create;
  try
    PriorNode := nil;
    AddNode(StrFlow, StrFlow, PriorNode);
    AddNode(StrBoundaryCondition, StrBoundaryCondition, PriorNode);
    AddChildNode(BC_SpecHead, StrSpecifiedHeadPackages, PriorNode);
    AddChildNode(BC_SpecifiedFlux, StrSpecifiedFlux, PriorNode);
    AddChildNode(BC_HeadDependentFlux, StrHeaddependentFlux, PriorNode);
    AddNode(StrSolver, StrSolver, PriorNode);
    AddNode(StrSubSidence, StrSubSidence, PriorNode);
    AddNode(StrObservations, StrObservations, PriorNode);
    AddNode(StrOutput, StrOutput, PriorNode);
    AddNode(StrPostProcessors, StrPostProcessors, PriorNode);

    for Index := 0 to PackageList.Count - 1 do
    begin
      APackage := PackageList[Index];
      Frame := APackage.Frame;
      Frame.NilNode;
    end;

    for Index := 0 to PackageList.Count - 1 do
    begin
      APackage := PackageList[Index];
      Frame := APackage.Frame;
      Assert(Frame <> nil);
      NodeIndex := TreeNodeList.IndexOf(APackage.Classification);
      Assert(NodeIndex >= 0);
      ParentNode := TreeNodeList.Objects[NodeIndex] as TTreeNode;
      ChildNode := tvPackages.Items.AddChild(ParentNode, APackage.PackageIdentifier);
      AControl := Frame;
      Page := nil;
      while AControl.Parent <> nil do
      begin
        AControl := AControl.Parent;
        if AControl is TJvStandardPage then
        begin
          Page := TJvStandardPage(AControl);
        end;
      end;
      Assert(Page <> nil);
      ChildNode.Data := Page;
      APackage.Node := ChildNode;
      Frame.GetData(APackage);
    end;
  finally
    TreeNodeList.Free;
  end;
  FillLpfTree;
  FillHufTree;
  FillTransientGrids;
  FillHfbGrid;

  GetSfrParamInstances;
end;

procedure TfrmModflowPackages.GetData;
begin
  IsLoaded := False;
  try
    PackageList.Free;
    PackageList := TList.Create;
    SteadyParameters.Free;
    SteadyParameters := TModflowSteadyParameters.Create(nil);
    HufParameters.Free;
    HufParameters := THufModflowParameters.Create(nil);
    TransientListParameters.Free;
    TransientListParameters := TModflowTransientListParameters.Create(nil);
    pcSFR.ActivePageIndex := 0;
    framePkgLPF.OnSelectedChange := framePkgLPFSelectedChange;
    framePkgCHD.OnSelectedChange :=  ChdSelectedChange;
    framePkgDRN.OnSelectedChange :=  DrnSelectedChange;
    framePkgGHB.OnSelectedChange :=  GhbSelectedChange;
    framePkgRIV.OnSelectedChange :=  RivSelectedChange;
    framePkgCHOB.CanSelect := False;
    framePkgDROB.CanSelect := False;
    framePkgGBOB.CanSelect := False;
    framePkgRVOB.CanSelect := False;
    ReadPackages;
    if frmGoPhast.PhastModel.ModflowPackages.LpfPackage.IsSelected then
    begin
      jvplPackages.ActivePage := jvspLPF;
    end
    else if frmGoPhast.PhastModel.ModflowPackages.HufPackage.IsSelected then
    begin
      jvplPackages.ActivePage := jvspHUF;
    end
    else if frmGoPhast.PhastModel.ModflowPackages.BcfPackage.IsSelected then
    begin
      jvplPackages.ActivePage := jvspBCF;
    end
    else
    begin
      jvplPackages.ActivePage := jvspLPF;
    end;
    jvplPackagesChange(nil);
    framePkgLPFSelectedChange(nil);
    HelpKeyword := 'MODFLOW_Packages_Dialog_Box';
    ChdSelectedChange(nil);
    DrnSelectedChange(nil);
    GhbSelectedChange(nil);
    RivSelectedChange(nil);
  finally
    IsLoaded := True;
  end;
end;

procedure TfrmModflowPackages.EnableUzfVerticalKSource;
begin
  framePkgUZF.comboVerticalKSource.Enabled :=
    framePkgUZF.rcSelectionController.Enabled
    and not framePkgBcf.rcSelectionController.Enabled;
  framePkgUZF.lblVerticalKSource.Enabled :=
    framePkgUZF.comboVerticalKSource.Enabled;
  if not framePkgUZF.comboVerticalKSource.Enabled then
  begin
    framePkgUZF.comboVerticalKSource.ItemIndex := 0;
  end;
end;

procedure TfrmModflowPackages.UpdateFlowParamGrid(Node: TTreeNode;
  ParameterFrame: TframeListParameterDefinition;
  ParameterCollection: TCollection; ParameterFrameController: TRbwController);
var
  Parameter: TModflowParameter;
  Index: Integer;
  ParamList: TList;
  ALabel: TLabel;
  RowIndex: Integer;
  ActiveGrid: TRbwDataGrid4;
  ActiveFrame: TframeListParameterDefinition;
const
  FormatStr = 'Number of %s parameters';
begin
  ActiveFrame := ParameterFrame;
  ActiveGrid := ParameterFrame.dgParameters;
  for RowIndex := 1 to ActiveGrid.RowCount - 1 do
  begin
    ActiveGrid.Objects[0, RowIndex] := nil;
  end;
  ALabel := ParameterFrame.lblNumParameters;
  CurrentParameterType := TParameterType(Node.Data);
  Assert(CurrentParameterType in [ptLPF_HK..ptLPF_VKCB, ptHUF_HK..ptHUF_LVDA]);
  case CurrentParameterType of
    ptLPF_HK:
      ALabel.Caption := Format(FormatStr, ['HK']);
    ptLPF_HANI:
      ALabel.Caption := Format(FormatStr, ['HANI']);
    ptLPF_VK:
      ALabel.Caption := Format(FormatStr, ['VK']);
    ptLPF_VANI:
      ALabel.Caption := Format(FormatStr, ['VANI']);
    ptLPF_SS:
      ALabel.Caption := Format(FormatStr, ['SS']);
    ptLPF_SY:
      ALabel.Caption := Format(FormatStr, ['SY']);
    ptLPF_VKCB:
      ALabel.Caption := Format(FormatStr, ['VKCB']);
    ptHUF_HK:
      ALabel.Caption := Format(FormatStr, ['HK']);
    ptHUF_HANI:
      ALabel.Caption := Format(FormatStr, ['HANI']);
    ptHUF_VK:
      ALabel.Caption := Format(FormatStr, ['VK']);
    ptHUF_VANI:
      ALabel.Caption := Format(FormatStr, ['VANI']);
    ptHUF_SS:
      ALabel.Caption := Format(FormatStr, ['SS']);
    ptHUF_SY:
      ALabel.Caption := Format(FormatStr, ['SY']);
    ptHUF_SYTP:
      ALabel.Caption := Format(FormatStr, ['SYTP']);
    ptHUF_KDEP:
      ALabel.Caption := Format(FormatStr, ['KDEP']);
    ptHUF_LVDA:
      ALabel.Caption := Format(FormatStr, ['LVDA']);
  else
    Assert(False);
  end;

  case CurrentParameterType of
    ptHUF_HK, ptHUF_HANI, ptHUF_VK, ptHUF_VANI,
      ptHUF_SS, ptHUF_SY, ptHUF_KDEP:
      begin
        ActiveGrid.ColCount := 2;
      end;
    ptHUF_SYTP, ptHUF_LVDA:
      begin
        ActiveGrid.ColCount := 4;
        ActiveGrid.Columns[Ord(pcUseZone)].Format := rcf4Boolean;
        ActiveGrid.Columns[Ord(pcUseMultiplier)].Format := rcf4Boolean;
        ParameterFrame.Loaded;
      end;
  end;

  ParameterFrameController.Enabled := CurrentParameterType <> ptUndefined;
  if ParameterFrameController.Enabled then
  begin
    ParamList := TList.Create;
    try
      for Index := 0 to ParameterCollection.Count - 1 do
      begin
        Parameter := ParameterCollection.Items[Index] as TModflowParameter;
        if Parameter.ParameterType = CurrentParameterType then
        begin
          ParamList.Add(Parameter);
        end;
      end;
      ActiveFrame.PriorNumberOfParameters := ParamList.Count;
      Assert(ActiveGrid <> nil);
      if ParamList.Count = 0 then
      begin
        ActiveGrid.RowCount := 2;
      end
      else
      begin
        ActiveGrid.RowCount := ParamList.Count + 1;
      end;
      for RowIndex := 1 to ParamList.Count do
      begin
        Parameter := ParamList[RowIndex - 1];
        AssignParameterToRow(ActiveGrid, RowIndex, Parameter);
      end;
      ParameterFrame.PriorNumberOfParameters := ParamList.Count;
      ParameterFrame.seNumberOfParameters.AsInteger := ParamList.Count;
      frameParameterDefinition_seNumberOfParametersChange(ParameterFrame.seNumberOfParameters);
    finally
      ParamList.Free;
    end;
  end;
end;

procedure TfrmModflowPackages.EnableRchModpathOption;
begin
  frameModpath.comboRchSource.Enabled :=
    frameModpath.rcSelectionController.Enabled
    and framePkgRCH.rcSelectionController.Enabled;
end;

procedure TfrmModflowPackages.EnableEvtModpathOption;
begin
  frameModpath.comboEvtSink.Enabled :=
    frameModpath.rcSelectionController.Enabled
    and framePkgEVT.rcSelectionController.Enabled;
end;

procedure TfrmModflowPackages.jvplPackagesChange(Sender: TObject);
begin
  inherited;
  if jvplPackages.ActivePage = jvspCHD then
  begin
    CurrentParameterType := ptCHD;
  end
  else if jvplPackages.ActivePage = jvspDRN then
  begin
    CurrentParameterType := ptDrn;
  end
  else if jvplPackages.ActivePage = jvspDRT then
  begin
    CurrentParameterType := ptDrt;
  end
  else if jvplPackages.ActivePage = jvspGHB then
  begin
    CurrentParameterType := ptGhb;
  end
  else if jvplPackages.ActivePage = jvspLPF then
  begin
    if tvLpfParameterTypes.Selected = nil then
    begin
      CurrentParameterType := ptUndefined;
      Assert(False);
    end
    else
    begin
      CurrentParameterType := TParameterType(tvLpfParameterTypes.Selected.Data);
    end;
  end
  else if jvplPackages.ActivePage = jvspHUF then
  begin
    if tvHufParameterTypes.Selected = nil then
    begin
      CurrentParameterType := ptUndefined;
      Assert(False);
    end
    else
    begin
      CurrentParameterType := TParameterType(tvHufParameterTypes.Selected.Data);
    end;
  end
  else if jvplPackages.ActivePage = jvspPCG then
  begin
    CurrentParameterType := ptUndefined;
  end
  else if jvplPackages.ActivePage = jvspRIV then
  begin
    CurrentParameterType := ptRiv;
  end
  else if jvplPackages.ActivePage = jvspWEL then
  begin
    CurrentParameterType := ptQ;
  end
  else if jvplPackages.ActivePage = jvspRCH then
  begin
    CurrentParameterType := ptRCH;
  end
  else if jvplPackages.ActivePage = jvspEVT then
  begin
    CurrentParameterType := ptEVT;
  end
  else if jvplPackages.ActivePage = jvspETS then
  begin
    CurrentParameterType := ptETS;
  end
  else if jvplPackages.ActivePage = jvspSFR then
  begin
    CurrentParameterType := ptSFR;
  end
  else if jvplPackages.ActivePage = jvspHFB then
  begin
    CurrentParameterType := ptHFB;
  end;

  HelpKeyword := jvplPackages.ActivePage.HelpKeyword;
end;

function TfrmModflowPackages.NewParameterName: string;
var
  Root: string;
  Index: Integer;
  Param: TModflowParameter;
  UpRoot, ParamUpRoot: string;
  CountString: string;
  Count, MaxCount: integer;
begin
  case CurrentParameterType of
    ptUndefined: Assert(False);
    ptLPF_HK: Root := 'HK_Par';
    ptLPF_HANI: Root := 'HANI_Par';
    ptLPF_VK: Root := 'VK_Par';
    ptLPF_VANI: Root := 'VANI_Par';
    ptLPF_SS: Root := 'SS_Par';
    ptLPF_SY: Root := 'SY_Par';
    ptLPF_VKCB: Root := 'VKCB_Par';
    ptRCH: Root := 'RCH_Par';
    ptEVT: Root := 'EVT_Par';
    ptETS: Root := 'ETS_Par';
    ptCHD: Root := 'CHD_Par';
    ptGHB: Root := 'GHB_Par';
    ptQ: Root := 'Q_Par';
    ptRIV: Root := 'RIV_Par';
    ptDRN: Root := 'DRN_Par';
    ptDRT: Root := 'DRT_Par';
    ptSFR: Root := 'SFR_Par';
    ptHFB: Root := 'HFB_Par';
    ptHUF_HK: Root := 'HK_Par';
    ptHUF_HANI: Root := 'HANI_Par';
    ptHUF_VK: Root := 'VK_Par';
    ptHUF_VANI: Root := 'VANI_Par';
    ptHUF_SS: Root := 'SS_Par';
    ptHUF_SY: Root := 'SY_Par';
    ptHUF_SYTP: Root := 'SYTP_Par';
    ptHUF_KDEP: Root := 'KDEP_Par';
    ptHUF_LVDA: Root := 'LVDA_Par';
    else Assert(False);
  end;
  UpRoot := UpperCase(Root);
  MaxCount := 0;
  case CurrentParameterType of
    ptUndefined: Assert(False);
    ptLPF_HK..ptLPF_VKCB, ptHFB, ptHUF_SYTP, ptHUF_LVDA:
      begin
        for Index := 0 to SteadyParameters.Count - 1 do
        begin
          Param := SteadyParameters[Index];
          ParamUpRoot := UpperCase(Param.ParameterName);
          if Pos(UpRoot, ParamUpRoot) > 0 then
          begin
            CountString := Copy(ParamUpRoot, Length(UpRoot)+1, MAXINT);
            if TryStrToInt(CountString, Count) then
            begin
              if Count > MaxCount then
              begin
                MaxCount := Count;
              end;
            end;
          end;
        end;
      end;
    ptCHD..ptSFR, ptRCH, ptEVT, ptETS:
      begin
        for Index := 0 to TransientListParameters.Count - 1 do
        begin
          Param := TransientListParameters[Index];
          ParamUpRoot := UpperCase(Param.ParameterName);
          if Pos(UpRoot, ParamUpRoot) > 0 then
          begin
            CountString := Copy(ParamUpRoot, Length(UpRoot)+1, MAXINT);
            if TryStrToInt(CountString, Count) then
            begin
              if Count > MaxCount then
              begin
                MaxCount := Count;
              end;
            end;
          end;
        end;
      end;
    ptHUF_HK..ptHUF_SY, ptHUF_KDEP:
      begin
        for Index := 0 to HufParameters.Count - 1 do
        begin
          Param := HufParameters.Items[Index] as TModflowParameter;
          ParamUpRoot := UpperCase(Param.ParameterName);
          if Pos(UpRoot, ParamUpRoot) > 0 then
          begin
            CountString := Copy(ParamUpRoot, Length(UpRoot)+1, MAXINT);
            if TryStrToInt(CountString, Count) then
            begin
              if Count > MaxCount then
              begin
                MaxCount := Count;
              end;
            end;
          end;
        end;
      end;
    else Assert(False);
  end;
  Inc(MaxCount);
  result := Root + IntToStr(MaxCount);
end;

procedure TfrmModflowPackages.SetData;
var
  Undo: TUndoChangePackageSelection;
  Index: Integer;
  APackage: TModflowPackageSelection;
  Frame: TframePackage;
begin
  SetSfrParamInstances;
  Undo := TUndoChangePackageSelection.Create(SteadyParameters,
    TransientListParameters, FSfrParameterInstances, HufParameters);

  AddPackagesToList(Undo.FNewPackages);

  for Index := 0 to PackageList.Count - 1 do
  begin
    APackage := PackageList[Index];
    Frame := APackage.Frame;
    Assert(Frame <> nil);
    Frame.SetData(APackage);
  end;

  frmGoPhast.UndoStack.Submit(Undo);
end;


procedure TfrmModflowPackages.tvPackagesChange(Sender: TObject;
  Node: TTreeNode);
var
  Page: TJvCustomPage;
begin
  inherited;
  if (Node <> nil) and (Node.Data <> nil) then
  begin
    Page := Node.Data;
    jvplPackages.ActivePage := Page;
  end;
end;

procedure TfrmModflowPackages.tvPackagesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Frame, SelectedFrame: TFramePackage;
  Node, ParentNode, ChildNode: TTreeNode;
  function NodeToFrame(Node: TTreeNode): TFramePackage;
  var
    Page, FramePage: TJvStandardPage;
    Frame: TFramePackage;
    Index: Integer;
    Component: TComponent;
    Control: TControl;
  begin
    result := nil;
    Page := Node.Data;
    for Index := 0 to ComponentCount - 1 do
    begin
      Component := Components[Index];
      if (Component is TFramePackage) then
      begin
        Frame := TFramePackage(Component);
        if (Frame.Parent = Page) then
        begin
          result := Frame;
          Exit;
        end
        else if Frame = framePkgSFR then
        begin
          Control := Frame;
          while Control.Parent <> nil do
          begin
            Control := Control.Parent;
            if Control is TJvStandardPage then
            begin
              FramePage := TJvStandardPage(Control);
              if FramePage = Page then
              begin
                result := Frame;
                Exit;
              end;
            end;
          end;
        end;
      end;
    end;
    Assert(result <> nil);
  end;
begin
  inherited;
  if htOnStateIcon in tvPackages.GetHitTestInfoAt(X, Y) then
  begin
    Node := tvPackages.GetNodeAt(X, Y);
    Frame := NodeToFrame(Node);
    case Frame.SelectionType of
      stCheckBox:
        begin
          Frame.Selected := not Frame.Selected and Frame.CanSelect;
        end;
      stRadioButton:
        begin
          if not Frame.Selected and Frame.CanSelect then
          begin
            ParentNode := Node.Parent;
            ChildNode := ParentNode.getFirstChild;
            while ChildNode <> nil do
            begin
              if ChildNode <> Node then
              begin
                SelectedFrame := NodeToFrame(ChildNode);
                if (SelectedFrame.SelectionType = stRadioButton)
                  and SelectedFrame.Selected then
                begin
                  SelectedFrame.Selected := False;
                end;
              end;
              ChildNode := ChildNode.getNextSibling;
            end;
            Frame.Selected := True;
          end;
        end;
      else Assert(False);
    end;
  end;
end;

procedure TfrmModflowPackages.InitializeFrame(
  Frame: TframeListParameterDefinition);
begin
  Frame.seNumberOfParameters.AsInteger := 0;
  frameParameterDefinition_seNumberOfParametersChange(
    Frame.seNumberOfParameters);
end;

procedure TfrmModflowPackages.FillHfbGrid;
var
  ParamIndex: Integer;
  Param: TModflowSteadyParameter;
  ActiveFrame: TframeListParameterDefinition;
  ActiveGrid: TRbwDataGrid4;
begin
  InitializeFrame(frameHfbParameterDefinition);
  for ParamIndex := 0 to SteadyParameters.Count - 1 do
  begin
    ActiveFrame := nil;
    Param := SteadyParameters[ParamIndex];
    case Param.ParameterType of
      ptLPF_HK, ptLPF_HANI, ptLPF_VK,
        ptLPF_VANI, ptLPF_SS, ptLPF_SY, ptLPF_VKCB, ptHUF_SYTP, ptHUF_LVDA: ;  // do nothing
      ptHFB: ActiveFrame := frameHFBParameterDefinition;
      else Assert(False);
    end;
    if ActiveFrame <> nil then
    begin
      ActiveGrid := ActiveFrame.dgParameters;
      ActiveFrame.seNumberOfParameters.AsInteger :=
        ActiveFrame.seNumberOfParameters.AsInteger + 1;
      if Assigned(ActiveFrame.seNumberOfParameters.OnChange) then
      begin
        ActiveFrame.seNumberOfParameters.OnChange(ActiveFrame.seNumberOfParameters);
      end;
      AssignParameterToRow(ActiveGrid, ActiveGrid.RowCount -1, Param);
    end;
  end;
end;

procedure TfrmModflowPackages.FillTransientGrids;
var
  ParamIndex: Integer;
  Param: TModflowTransientListParameter;
  ActiveFrame: TframeListParameterDefinition;
  ActiveGrid: TRbwDataGrid4;
begin
  InitializeFrame(frameChdParameterDefinition);
  InitializeFrame(frameGhbParameterDefinition);
  InitializeFrame(frameWelParameterDefinition);
  InitializeFrame(frameRivParameterDefinition);
  InitializeFrame(frameDrnParameterDefinition);
  InitializeFrame(frameDrtParameterDefinition);
  InitializeFrame(frameRchParameterDefinition);
  InitializeFrame(frameEvtParameterDefinition);
  InitializeFrame(frameEtsParameterDefinition);
  InitializeFrame(frameSfrParameterDefinition);

  for ParamIndex := 0 to TransientListParameters.Count - 1 do
  begin
    ActiveFrame := nil;
    Param := TransientListParameters[ParamIndex];
    case Param.ParameterType of
      ptCHD: ActiveFrame := frameChdParameterDefinition;
      ptGHB: ActiveFrame := frameGhbParameterDefinition;
      ptQ: ActiveFrame := frameWelParameterDefinition;
      ptRIV: ActiveFrame := frameRivParameterDefinition;
      ptDRN: ActiveFrame := frameDrnParameterDefinition;
      ptDRT: ActiveFrame := frameDrtParameterDefinition;
      ptRCH: ActiveFrame := frameRchParameterDefinition;
      ptEVT: ActiveFrame := frameEvtParameterDefinition;
      ptETS: ActiveFrame := frameEtsParameterDefinition;
      ptSFR: ActiveFrame := frameSfrParameterDefinition;
//      ptHFB: ActiveFrame := frameHFBParameterDefinition;
      else Assert(False);
    end;
    ActiveGrid := ActiveFrame.dgParameters;
    ActiveFrame.seNumberOfParameters.AsInteger :=
      ActiveFrame.seNumberOfParameters.AsInteger + 1;
    if Assigned(ActiveFrame.seNumberOfParameters.OnChange) then
    begin
      ActiveFrame.seNumberOfParameters.OnChange(ActiveFrame.seNumberOfParameters);
    end;
//    frameParameterDefinition_seNumberOfParametersChange(
//      ActiveFrame.seNumberOfParameters);
    AssignParameterToRow(ActiveGrid, ActiveGrid.RowCount -1, Param);
  end;
end;

procedure TfrmModflowPackages.AddPackagesToList(Packages: TModflowPackages);
begin
  PackageList.Clear;

  // add to list in alphabetical order.
  Packages.BcfPackage.Frame := framePkgBCF;
  PackageList.Add(Packages.BcfPackage);

  Packages.ChdBoundary.Frame := framePkgCHD;
  PackageList.Add(Packages.ChdBoundary);

  Packages.DrnPackage.Frame := framePkgDRN;
  PackageList.Add(Packages.DrnPackage);

  Packages.DrtPackage.Frame := framePkgDRT;
  PackageList.Add(Packages.DrtPackage);

  Packages.ETSPackage.Frame := framePkgETS;
  PackageList.Add(Packages.ETSPackage);

  Packages.EVTPackage.Frame := framePkgEVT;
  PackageList.Add(Packages.EVTPackage);

  Packages.GhbBoundary.Frame := framePkgGHB;
  PackageList.Add(Packages.GhbBoundary);

  Packages.LakPackage.Frame := framePkgLAK;
  PackageList.Add(Packages.LakPackage);

  Packages.LpfPackage.Frame := framePkgLPF;
  PackageList.Add(Packages.LpfPackage);

  Packages.HufPackage.Frame := framePkgHuf;
  PackageList.Add(Packages.HufPackage);

  Packages.PcgPackage.Frame := framePCG;
  PackageList.Add(Packages.PcgPackage);

  Packages.Mnw2Package.Frame := framePkgMnw2;
  PackageList.Add(Packages.Mnw2Package);

  Packages.RCHPackage.Frame := framePkgRCH;
  PackageList.Add(Packages.RCHPackage);

  Packages.ResPackage.Frame := framePkgRES;
  PackageList.Add(Packages.ResPackage);

  Packages.RivPackage.Frame := framePkgRIV;
  PackageList.Add(Packages.RivPackage);

  Packages.WelPackage.Frame := framePkgWEL;
  PackageList.Add(Packages.WelPackage);

  Packages.SfrPackage.Frame := framePkgSFR;
  PackageList.Add(Packages.SfrPackage);

  Packages.HfbPackage.Frame := framePkgHFB;
  PackageList.Add(Packages.HfbPackage);

  Packages.UzfPackage.Frame := framePkgUZF;
  PackageList.Add(Packages.UzfPackage);

  Packages.GmgPackage.Frame := framePkgGMG;
  PackageList.Add(Packages.GmgPackage);

  Packages.SipPackage.Frame := framePkgSIP;
  PackageList.Add(Packages.SipPackage);

  Packages.De4Package.Frame := framePkgDE4;
  PackageList.Add(Packages.De4Package);

  Packages.SubPackage.Frame := framePkgSub;
  PackageList.Add(Packages.SubPackage);

  Packages.SwtPackage.Frame := framePkgSwt;
  PackageList.Add(Packages.SwtPackage);

  Packages.HobPackage.Frame := framePkgHOB;
  PackageList.Add(Packages.HobPackage);

  Packages.ModPath.Frame := frameModpath;
  PackageList.Add(Packages.ModPath);

  Packages.ChobPackage.Frame := framePkgCHOB;
  PackageList.Add(Packages.ChobPackage);

  Packages.DrobPackage.Frame := framePkgDROB;
  PackageList.Add(Packages.DrobPackage);

  Packages.GbobPackage.Frame := framePkgGBOB;
  PackageList.Add(Packages.GbobPackage);

  Packages.RvobPackage.Frame := framePkgRVOB;
  PackageList.Add(Packages.RvobPackage);

  Packages.ZoneBudget.Frame := frameZoneBudget;
  PackageList.Add(Packages.ZoneBudget);

  Packages.HydmodPackage.Frame := framePkgHydmod;
  PackageList.Add(Packages.HydmodPackage);

end;

procedure TfrmModflowPackages.tvHufParameterTypesChange(Sender: TObject;
  Node: TTreeNode);
begin
  inherited;
  CurrentParameterType := TParameterType(Node.Data);
  if CurrentParameterType in [ptHUF_SYTP, ptHUF_LVDA] then
  begin
    UpdateFlowParamGrid(Node, frameHufParameterDefinition,
      SteadyParameters, rbwHufParamCountController);
  end
  else
  begin
    UpdateFlowParamGrid(Node, frameHufParameterDefinition,
      HufParameters, rbwHufParamCountController);
  end;
end;

procedure TfrmModflowPackages.tvLpfParameterTypesChange(Sender: TObject;
  Node: TTreeNode);
begin
  inherited;
  UpdateFlowParamGrid(Node, frameLpfParameterDefinition,
    SteadyParameters, rbwLpfParamCountController);
end;

{ TUndoChangePackageSelection }

constructor TUndoChangePackageSelection.Create(
  var NewSteadyParameters: TModflowSteadyParameters;
  var NewTransientParameters: TModflowTransientListParameters;
  var SfrParameterInstances: TSfrParamInstances;
  var NewHufModflowParameters: THufModflowParameters);
var
  Index: Integer;
  LayerGroup: TLayerGroup;
begin
  inherited Create(NewSteadyParameters, NewTransientParameters,
    NewHufModflowParameters);

  SetLength(FOldInterBlockTransmissivity, frmGoPhast.PhastModel.LayerStructure.Count);
  SetLength(FOldAquiferType, frmGoPhast.PhastModel.LayerStructure.Count);
  for Index := 0 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
  begin
    LayerGroup := frmGoPhast.PhastModel.LayerStructure[Index];
    FOldInterBlockTransmissivity[Index] := LayerGroup.InterblockTransmissivityMethod;
    FOldAquiferType[Index] := LayerGroup.AquiferType;
  end;

  FOldHydroGeologicUnits := THydrogeologicUnits.Create(nil);
  FOldHydroGeologicUnits.Assign(frmGoPhast.PhastModel.HydrogeologicUnits);

  FOldPackages := TModflowPackages.Create(nil);
  FNewPackages := TModflowPackages.Create(nil);
  FOldPackages.Assign(frmGoPhast.PhastModel.ModflowPackages);

  FNewSfrParameterInstances := SfrParameterInstances;
  // TUndoDefineLayers takes ownership of SfrParameterInstances.
  SfrParameterInstances := nil;

  FOldSfrParameterInstances := TSfrParamInstances.Create(nil);
  FOldSfrParameterInstances.Assign(
    frmGoPhast.PhastModel.ModflowPackages.SfrPackage.ParameterInstances);
end;

function TUndoChangePackageSelection.Description: string;
begin
  result := 'Change packages';
end;

destructor TUndoChangePackageSelection.Destroy;
begin
  FOldHydroGeologicUnits.Free;
  FOldPackages.Free;
  FNewPackages.Free;
  FOldSfrParameterInstances.Free;
  FNewSfrParameterInstances.Free;
  inherited;
end;

procedure TUndoChangePackageSelection.DoCommand;
begin
  frmGoPhast.PhastModel.ModflowPackages.SfrPackage.AssignParameterInstances := False;
  try
    UpdateLayerGroupProperties(FNewPackages.BcfPackage);
    frmGoPhast.PhastModel.ModflowPackages := FNewPackages;
  finally
    frmGoPhast.PhastModel.ModflowPackages.SfrPackage.AssignParameterInstances := True;
  end;
  frmGoPhast.PhastModel.ModflowPackages.SfrPackage.ParameterInstances := FNewSfrParameterInstances;
  inherited;
  frmGoPhast.EnableLinkStreams;
  frmGoPhast.EnableManageFlowObservations;
  frmGoPhast.EnableManageHeadObservations;
  frmGoPhast.EnableHufMenuItems;
end;

procedure TUndoChangePackageSelection.Undo;
var
  Index: Integer;
  LayerGroup: TLayerGroup;
begin
  frmGoPhast.PhastModel.ModflowPackages.SfrPackage.AssignParameterInstances := False;
  try
    for Index := 0 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
    begin
      LayerGroup := frmGoPhast.PhastModel.LayerStructure[Index];
      LayerGroup.InterblockTransmissivityMethod := FOldInterBlockTransmissivity[Index];
      LayerGroup.AquiferType := FOldAquiferType[Index];
    end;
    frmGoPhast.PhastModel.ModflowPackages := FOldPackages;
  finally
    frmGoPhast.PhastModel.ModflowPackages.SfrPackage.AssignParameterInstances := True;
  end;
  frmGoPhast.PhastModel.ModflowPackages.SfrPackage.ParameterInstances := FOldSfrParameterInstances;
  inherited;
  frmGoPhast.PhastModel.HydrogeologicUnits.Assign(FOldHydroGeologicUnits);
  frmGoPhast.EnableLinkStreams;
  frmGoPhast.EnableHufMenuItems;
  frmGoPhast.EnableManageFlowObservations;
  frmGoPhast.EnableManageHeadObservations;
end;

procedure TUndoChangePackageSelection.UpdateLayerGroupProperties(BcfPackage: TModflowPackageSelection);
var
  Index: Integer;
  LayerGroup: TLayerGroup;
begin
  if frmGoPhast.PhastModel.ModflowPackages.BcfPackage.IsSelected <> BcfPackage.IsSelected then
  begin
    if BcfPackage.IsSelected then
    begin
      for Index := 1 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
      begin
        LayerGroup := frmGoPhast.PhastModel.LayerStructure[Index];
        if LayerGroup.InterblockTransmissivityMethod >= 1 then
        begin
          LayerGroup.InterblockTransmissivityMethod :=
            LayerGroup.InterblockTransmissivityMethod + 1;
        end;
        if LayerGroup.AquiferType = 1 then
        begin
          LayerGroup.AquiferType := 3;
        end;
      end;
    end
    else
    begin
      for Index := 1 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
      begin
        LayerGroup := frmGoPhast.PhastModel.LayerStructure[Index];
        if LayerGroup.InterblockTransmissivityMethod >= 1 then
        begin
          LayerGroup.InterblockTransmissivityMethod :=
            LayerGroup.InterblockTransmissivityMethod - 1;
        end;
        if LayerGroup.AquiferType > 1 then
        begin
          LayerGroup.AquiferType := 1;
        end;
      end;
    end;
  end;
end;

end.
