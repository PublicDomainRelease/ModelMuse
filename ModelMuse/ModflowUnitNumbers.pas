unit ModflowUnitNumbers;

interface

uses SysUtils, Classes, GoPhastTypes;

const
  StrLIST = 'LIST';
  StrDIS = 'DIS';
  StrCBC = 'CBC';
  StrBAS = 'BAS6';
  StrLPF = 'LPF';
  StrHUF2 = 'HUF2';
  StrZONE = 'ZONE';
  StrMULT = 'MULT';
  StrCHD = 'CHD';
  StrPCG = 'PCG';
  StrGHB = 'GHB';
  StrWEL = 'WEL';
  StrRIV = 'RIV';
  StrDRN = 'DRN';
  StrDRT = 'DRT';
  StrRCH = 'RCH';
  StrEVT = 'EVT';
  StrETS = 'ETS';
  StrRES = 'RES';
  StrLAK = 'LAK';
  StrSFR = 'SFR';
  StrUZF = 'UZF';
  StrGMG = 'GMG';
  StrIUNITMHC = 'IUNITMHC';
  StrSIP = 'SIP';
  StrDE4 = 'DE4';
  StrHEAD = 'HEAD';
  StrDRAWDOWN = 'DRAWDOWN';
  StrOC = 'OC';
  StrGAG = 'GAGE';
  StrHOB = 'HOB';
  StrIUHOBSV = 'IUHOBSV';
  StrHFB = 'HFB6';
  StrUNIT = 'UNIT';
  StrDROB = 'DROB';
  StrIUDROBSV = 'IUDROBSV';
  StrGBOB = 'GBOB';
  StrIUGBOBSV = 'IUGBOBSV';
  StrRVOB = 'RVOB';
  StrIURVOBSV = 'IURVOBSV';
  StrCHOB = 'CHOB';
  StrIUCHOBSV = 'IUCHOBSV';
  StrPVAL = 'PVAL';
  StrIOHUFHEADS = 'IOHUFHEADS';
  StrIOHUFFLOWS = 'IOHUFFLOWS';
  StrKDEP = 'KDEP';
  StrLVDA = 'LVDA';
  StrISTCB2 = 'ISTCB2';
  StrMNW2 = 'MNW2';
  StrMNWI = 'MNWI';
  StrMNWI_Wells = 'MNWI_Wells';
  StrMNWI_SummarizeByWell = 'MNWI_SummarizeByWell';
  StrMNWI_SummarizeByNode = 'MNWI_SummarizeByNode';
  StrBCF = 'BCF6';
  StrSUB = 'SUB';
  StrSUBSaveRestart = 'SUB_SaveRestart';
  StrSUBReadRestart = 'SUB_ReadRestart';

  StrSubSUB_Out = 'SUB_SUB_OUT';
  StrSubCOM_ML_Out = 'SUB_COM_ML_OUT';
  StrSubCOM_IS_Out = 'SUB_COM_IS_OUT';
  StrSub_VD_Out = 'SUB_VD_OUT';
  StrSub_NDPCH_Out = 'SUB_NDPCH_OUT';
  StrSub_DPCH_Out = 'SUB_DPCH_OUT';
  StrZoneBudget = 'ZONEBUDGET';

  Solvers: array[0..3] of string = (StrPCG, StrGMG, StrSIP, StrDE4);
  FlowPackages: array[0..2] of string = (StrLPF, StrHUF2, StrBCF);


type
  TCustomUnitNumberItem = class(TCollectionItem)
  private
    FKey: string;
    FUnitNumber: integer;
    procedure SetKey(const Value: string);
    procedure SetUnitNumber(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Key: string read FKey write SetKey;
    property UnitNumber: integer read FUnitNumber write SetUnitNumber;
  end;

  TUnitNumberItem = class(TCustomUnitNumberItem)
  private
    FDescription: string;
    procedure SetDescription(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
    property Description: string read FDescription write SetDescription;
  end;

  TExternalFileItem = class(TCustomUnitNumberItem)
  private
    FFileName: string;
    procedure SetFileName(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property FileName: string read FFileName write SetFileName;
  end;

  TCustomUnitNumbers = class(TPhastCollection)
  public
    function IndexOf(Key: string): integer;
    function UnitNumber(const Key: string): integer;
  end;

  // @name stores instances of @link(TUnitNumberItem).
  // Each @link(TUnitNumberItem) associates a key with a
  // unit number and description. The unit numbers can be used when
  // exporting MODFLOW input files.
  TUnitNumbers = class(TCustomUnitNumbers)
  private
    procedure CreateDefaultItems;
    procedure RemoveObsoleteItems;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Model: TComponent);
  end;

  // @name stores instances of @link(TExternalFileItem).
  // Each @link(TExternalFileItem) associates a key with a
  // unit number, description, and file name. The file names are
  // the names that will be used for external files that will
  // be included in the name file.
  TExternalFiles = class(TCustomUnitNumbers)
  public
    constructor Create(Model: TComponent);
  end;

implementation

const
{
  unit numbers that were used previously.
  GlobalFileUnit = 10;
  SFR_PrintUnit = 30;
  UZF_GageStartUnit = 32;

  MODPATH reserves several numbers in the range
  80 to 99 for internal use. 
}
  CellFlowsUnitNumber = 9;
  // @name is the unit number in MODFLOW
  // on which the listing file will be printed.
  ListFileUnit = 11;
  DiscretizationUnit = 12;
  BasicUnit = 13;
  LPF_Unit = 14;
  ZoneUnit = 15;
  MultUnit = 16;
  CHD_Unit = 17;
  PCG_Unit = 18;
  GHB_Unit = 19;
  WEL_Unit = 20;
  RIV_Unit = 21;
  DRN_Unit = 22;
  DRT_Unit = 23;
  RCH_Unit = 24;
  EVT_Unit = 25;
  ETS_Unit = 26;
  RES_Unit = 27;
  LAK_Unit = 28;
  SFR_Unit = 29;
  UZF_Unit = 31;
  GMG_Unit = 33;
  GMG_HeadChangeUnit = 34;
  SIP_Unit = 35;
  DE4_Unit = 36;
  HeadDataUnit = 37;
  DrawdownDataUnit = 38;
  OC_Unit = 39;
  GAG_Unit = 40;
  HOB_Unit = 41;
  HOB_Output_Unit = 42;
  HFB_Unit = 43;
  DROB_Unit = 44;
  IUDROBSV = 45;
  GBOB_Unit = 46;
  IUGBOBSV = 47;
  RVOB_Unit = 48;
  IURVOBSV = 49;
  CHOB_Unit = 50;
  IUCHOBSV = 51;
  PVAL_Unit = 52;
  HUF_Unit = 53;
  IOHUFHEADS = 54;
  IOHUFFLOWS = 55;
  KDEP_Unit = 56;
  LVDA_Unit = 57;
  StreamFlows = 58;
  MNW2_Unit = 59;
  MNWI_Unit = 60;
  MNWI_Wells_Unit = 61;
  MNWI_SummarizeByWell_Unit = 62;
  MNWI_SummarizeByNode_Unit = 63;
  BcfUnit = 64;
  SubUnit = 65;
  SubUnitSave = 66;
  SubUnitRead = 67;

  SubSUB_Out = 110;
  SubCOM_ML_Out = 111;
  SubCOM_IS_Out = 112;
  Sub_VD_Out = 113;
  Sub_NDPCH_Out = 114;
  Sub_DPCH_Out = 115;


  // Unit numbers 70 - 95 are reserved for the user.
  // If the reserved unit numbers are changed, the documentation
  // for the MODFLOW Name File dialog box must be updated.

  GageOutputStartUnit = 105;


{ TUnitNumbers }

procedure TUnitNumbers.Assign(Source: TPersistent);
begin
  inherited;
  CreateDefaultItems;
  RemoveObsoleteItems;
end;

constructor TUnitNumbers.Create(Model: TComponent);
begin
  inherited Create(TUnitNumberItem, Model);
  CreateDefaultItems;
end;

procedure TUnitNumbers.CreateDefaultItems;
var
  Item: TUnitNumberItem;
  procedure AddItem(const Key: string; UnitNumber: integer;
    const Description: string = '');
  var
    Position: integer;
  begin
    Position:= IndexOf(Key);
    if Position < 0 then
    begin
      Item := Add as TUnitNumberItem;
      Item.Key := Key;
      Item.UnitNumber := UnitNumber;
    end
    else
    begin
      Item := Items[Position] as TUnitNumberItem;
    end;
    if Description = '' then
    begin
      Item.Description := Key;
    end
    else
    begin
      Item.Description := Description;
    end;
  end;
begin
  AddItem(StrLIST, ListFileUnit);
  AddItem(StrDIS, DiscretizationUnit);
  AddItem(StrCBC, CellFlowsUnitNumber, 'Cell-by-Cell Budget File');
  AddItem(StrBAS, BasicUnit);
  AddItem(StrLPF, LPF_Unit);
  AddItem(StrZONE, ZoneUnit);
  AddItem(StrMULT, MultUnit);
  AddItem(StrCHD, CHD_Unit);
  AddItem(StrPCG, PCG_Unit);
  AddItem(StrGHB, GHB_Unit);
  AddItem(StrWEL, WEL_Unit);
  AddItem(StrRIV, RIV_Unit);
  AddItem(StrDRN, DRN_Unit);
  AddItem(StrDRT, DRT_Unit);
  AddItem(StrRCH, RCH_Unit);
  AddItem(StrEVT, EVT_Unit);
  AddItem(StrETS, ETS_Unit);
  AddItem(StrRES, RES_Unit);
  AddItem(StrLAK, LAK_Unit);
  AddItem(StrSFR, SFR_Unit);
  AddItem(StrUZF, UZF_Unit);
  AddItem(StrGMG, GMG_Unit);
  AddItem(StrIUNITMHC, GMG_HeadChangeUnit, 'GMG Head Change Unit');
  AddItem(StrSIP, SIP_Unit);
  AddItem(StrDE4, DE4_Unit);
  AddItem(StrHEAD, HeadDataUnit, 'Head output file');
  AddItem(StrDRAWDOWN, DrawdownDataUnit, 'Drawdown output file');
  AddItem(StrOC, OC_Unit);
  AddItem(StrGAG, GAG_Unit);
  AddItem(StrHOB, HOB_Unit);
  AddItem(StrIUHOBSV, HOB_Output_Unit, 'Head Observations output file');
  AddItem(StrHFB, HFB_Unit);
  AddItem(StrUNIT, GageOutputStartUnit, 'First Gage Unit');
  AddItem(StrDROB, DROB_Unit);
  AddItem(StrIUDROBSV, IUDROBSV);
  AddItem(StrGBOB, GBOB_Unit);
  AddItem(StrIUGBOBSV, IUGBOBSV);
  AddItem(StrRVOB, RVOB_Unit);
  AddItem(StrIURVOBSV, IURVOBSV);
  AddItem(StrCHOB, CHOB_Unit);
  AddItem(StrIUCHOBSV, IUCHOBSV);
  AddItem(StrPVAL, PVAL_Unit);
  AddItem(StrHUF2, HUF_Unit);
  AddItem(StrIOHUFHEADS, IOHUFHEADS);
  AddItem(StrIOHUFFLOWS, IOHUFFLOWS);
  AddItem(StrKDEP, KDEP_Unit);
  AddItem(StrLVDA, LVDA_Unit);
  AddItem(StrISTCB2, StreamFlows);
  AddItem(StrMNW2, MNW2_Unit);
  AddItem(StrMNWI, MNWI_Unit);
  AddItem(StrMNWI_Wells, MNWI_Wells_Unit);
  AddItem(StrMNWI_SummarizeByWell, MNWI_SummarizeByWell_Unit);
  AddItem(StrMNWI_SummarizeByNode, MNWI_SummarizeByNode_Unit);
  AddItem(StrBCF, BcfUnit);
  AddItem(StrSUB, SubUnit);
  AddItem(StrSUBSaveRestart, SubUnitSave);
  AddItem(StrSUBReadRestart, SubUnitRead);

  AddItem(StrSubSUB_Out, SubSUB_Out);
  AddItem(StrSubCOM_ML_Out, SubCOM_ML_Out);
  AddItem(StrSubCOM_IS_Out, SubCOM_IS_Out);
  AddItem(StrSub_VD_Out, Sub_VD_Out);
  AddItem(StrSub_NDPCH_Out, Sub_NDPCH_Out);
  AddItem(StrSub_DPCH_Out, Sub_DPCH_Out);

end;

procedure TUnitNumbers.RemoveObsoleteItems;
  procedure RemoveItem(const Key: string);
  var
    Position: Integer;
  begin
    Position:= IndexOf(Key);
    if Position < 0 then
    begin
      Delete(Position);
    end;
  end;
begin
  // Currently there are no obsolete items so none are removed.
end;

{ TCustomUnitNumberItem }

procedure TCustomUnitNumberItem.Assign(Source: TPersistent);
var
  SourceItem: TCustomUnitNumberItem;
begin
  if Source is TCustomUnitNumberItem then
  begin
    SourceItem := TCustomUnitNumberItem(Source);
    Key := SourceItem.Key;
    UnitNumber := SourceItem.UnitNumber;
  end
  else
  begin
    inherited;
  end;
end;

procedure TCustomUnitNumberItem.SetKey(const Value: string);
begin
  if FKey <> Value then
  begin
    (Collection as TPhastCollection).InvalidateModel;
    FKey := Value;
  end;
end;

procedure TCustomUnitNumberItem.SetUnitNumber(const Value: integer);
begin
  if FUnitNumber <> Value then
  begin
    (Collection as TPhastCollection).InvalidateModel;
    FUnitNumber := Value;
  end;
end;

{ TUnitNumberItem }

procedure TUnitNumberItem.Assign(Source: TPersistent);
var
  SourceItem: TUnitNumberItem;
begin
  if Source is TUnitNumberItem then
  begin
    SourceItem := TUnitNumberItem(Source);
    Description := SourceItem.Description;
    inherited;
  end;
end;

procedure TUnitNumberItem.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

{ TExternalFiles }

constructor TExternalFiles.Create(Model: TComponent);
begin
  inherited Create(TExternalFileItem, Model);
end;

{ TExternalFileItem }

procedure TExternalFileItem.Assign(Source: TPersistent);
var
  SourceItem: TExternalFileItem;
begin
  if Source is TExternalFileItem then
  begin
    SourceItem := TExternalFileItem(Source);
    FileName := SourceItem.FileName;
  end;
  inherited;
end;

procedure TExternalFileItem.SetFileName(const Value: string);
begin
  if FFileName <> Value then
  begin
    (Collection as TPhastCollection).InvalidateModel;
    FFileName := Value;
  end;
end;

{ TCustomUnitNumbers }

function TCustomUnitNumbers.IndexOf(Key: string): integer;
var
  Index: Integer;
  Item: TCustomUnitNumberItem;
begin
  Key := UpperCase(Key);
  result := -1;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TCustomUnitNumberItem;
    if UpperCase(Item.Key) = Key then
    begin
      result := Index;
      Exit;
    end;
  end;
end;

function TCustomUnitNumbers.UnitNumber(const Key: string): integer;
var
  Item: TCustomUnitNumberItem;
begin
  result := IndexOf(Key);
  if result >= 0 then
  begin
    Item := Items[result] as TCustomUnitNumberItem;
    result := Item.UnitNumber;
  end;
end;

end.
