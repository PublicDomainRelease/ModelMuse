unit frmLinkStreamsUnit;

interface

uses
  Windows, Messages, Types, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ArgusDataEntry, ExtCtrls,
  UndoItems, ScreenObjectUnit, OrderedCollectionUnit;

type
  TStreamLinkageChangeItem = class(TCollectionItem)
  private
    FNewOutFlowSegment: integer;
    FOldOutFlowSegments: TIntegerDynArray;
    FScreenObject: TScreenObject;
    procedure SetNewOutFlowSegment(const Value: integer);
    procedure SetScreenObject(const Value: TScreenObject);
    function GetOutFlowSegment(Index: integer): integer;
  public
    procedure Assign(Source: TPersistent); override;
    property ScreenObject: TScreenObject read FScreenObject
      write SetScreenObject;
    property NewOutFlowSegment: integer read FNewOutFlowSegment
      write SetNewOutFlowSegment;
    property OldOutFlowSegments[Index: integer]: integer read GetOutFlowSegment;
  end;

  TStreamLinkageChangeCollection = class(TCollection)
  private
    FParameterType: TParameterType;
    function GetItems(Index: integer): TStreamLinkageChangeItem;
    procedure SetItems(Index: integer; const Value: TStreamLinkageChangeItem);
  public
    Constructor Create(ParameterType: TParameterType);
    property Items[Index: integer]: TStreamLinkageChangeItem read GetItems
      write SetItems;
    function Add: TStreamLinkageChangeItem;
  end;

  TUndoChangeStreamLinkages = class(TCustomUndo)
  private
    FLinkages: TStreamLinkageChangeCollection;
  public
    function Description: string; override;
    Constructor Create(Linkages: TStreamLinkageChangeCollection);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TfrmLinkStreams = class(TfrmCustomGoPhast)
    rgWhatToLink: TRadioGroup;
    lblTolerance: TLabel;
    rdeTolerance: TRbwDataEntry;
    cbKeepExistingLinkages: TCheckBox;
    btnApply: TBitBtn;
    btnClose: TBitBtn;
    btnHelp: TBitBtn;
    rgStreamtype: TRadioGroup;
    procedure GetLinkTolerance;
    procedure btnApplyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure GetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmLinkStreams: TfrmLinkStreams = nil;

implementation

uses frmGoPhastUnit, ModflowSfrParamIcalcUnit, ModflowStrUnit,
  ModflowBoundaryUnit;

resourcestring
  StrAssignStreamLinkag = 'assign stream linkages';
  StrNoOutflowSegmentN = 'No outflow segment numbers were changed.';

{$R *.dfm}

{ TStreamLinkageChangeItem }

procedure TStreamLinkageChangeItem.Assign(Source: TPersistent);
var
  SourceItem: TStreamLinkageChangeItem;
begin
  SourceItem := Source as TStreamLinkageChangeItem;
  ScreenObject := SourceItem.ScreenObject;
  NewOutFlowSegment := SourceItem.NewOutFlowSegment;
end;

function TStreamLinkageChangeItem.GetOutFlowSegment(Index: integer): integer;
begin
  result := FOldOutFlowSegments[Index];
end;

procedure TStreamLinkageChangeItem.SetNewOutFlowSegment(const Value: integer);
begin
  FNewOutFlowSegment := Value;
end;

procedure TStreamLinkageChangeItem.SetScreenObject(const Value: TScreenObject);
var
  ParamIcalc: TSfrParamIcalcCollection;
  Index: integer;
  ParamItem: TModflowParamItem;
  Values: TStrCollection;
begin
  FScreenObject := Value;
  case (Collection as TStreamLinkageChangeCollection).FParameterType of
    ptSFR:
      begin
        Assert((FScreenObject.ModflowSfrBoundary <> nil)
          and FScreenObject.ModflowSfrBoundary.Used);
        ParamIcalc := FScreenObject.ModflowSfrBoundary.ParamIcalc;
        SetLength(FOldOutFlowSegments, ParamIcalc.Count);
        for Index := 0 to ParamIcalc.Count - 1 do
        begin
          FOldOutFlowSegments[Index] := ParamIcalc.Items[Index].OutflowSegment;
        end;
      end;
    ptSTR:
      begin
        Assert((FScreenObject.ModflowStrBoundary <> nil)
          and FScreenObject.ModflowStrBoundary.Used);

        if FScreenObject.ModflowStrBoundary.Parameters.Count > 0 then
        begin
          ParamItem := FScreenObject.ModflowStrBoundary.Parameters[0];
          Values := ParamItem.Param as TStrCollection;
        end
        else
        begin
          Values := FScreenObject.ModflowStrBoundary.Values as TStrCollection;
        end;

        SetLength(FOldOutFlowSegments, Values.Count);
        for Index := 0 to Values.Count - 1 do
        begin
          FOldOutFlowSegments[Index] := (Values[Index] as TStrItem).OutflowSegment;
        end;
      end;
  end;
end;

{ TStreamLinkageChangeCollection }

function TStreamLinkageChangeCollection.Add: TStreamLinkageChangeItem;
begin
  result := inherited Add as TStreamLinkageChangeItem;
end;

constructor TStreamLinkageChangeCollection.Create(ParameterType: TParameterType);
begin
  inherited Create(TStreamLinkageChangeItem);
  Assert(ParameterType in [ptSFR, ptSTR]);
  FParameterType := ParameterType;
end;

function TStreamLinkageChangeCollection.GetItems(
  Index: integer): TStreamLinkageChangeItem;
begin
  result := inherited Items[Index] as TStreamLinkageChangeItem;
end;

procedure TStreamLinkageChangeCollection.SetItems(Index: integer;
  const Value: TStreamLinkageChangeItem);
begin
  inherited Items[Index] := Value;
end;

{ TUndoChangeStreamLinkages }

constructor TUndoChangeStreamLinkages.Create(
  Linkages: TStreamLinkageChangeCollection);
begin
  inherited Create;
  FLinkages:= TStreamLinkageChangeCollection.Create(Linkages.FParameterType);
  FLinkages.Assign(Linkages);
end;

function TUndoChangeStreamLinkages.Description: string;
begin
  result := StrAssignStreamLinkag;
end;

destructor TUndoChangeStreamLinkages.Destroy;
begin
  FLinkages.Free;
  inherited;
end;

procedure TUndoChangeStreamLinkages.DoCommand;
var
  Index: Integer;
  Item: TStreamLinkageChangeItem;
  ParamIcalc: TSfrParamIcalcCollection;
  PI_Index: Integer;
  ParamItem: TModflowParamItem;
  Values: TStrCollection;
  ItemIndex: Integer;
begin
  for Index := 0 to FLinkages.Count - 1 do
  begin
    Item := FLinkages.Items[Index];
    case FLinkages.FParameterType of
      ptSFR:
        begin
          ParamIcalc := Item.ScreenObject.ModflowSfrBoundary.ParamIcalc;
          for PI_Index := 0 to ParamIcalc.Count - 1 do
          begin
            ParamIcalc.Items[PI_Index].OutflowSegment := Item.NewOutFlowSegment;
          end;
        end;
      ptSTR:
        begin
          if Item.ScreenObject.ModflowStrBoundary.Parameters.Count > 0 then
          begin
            ParamItem := Item.ScreenObject.ModflowStrBoundary.Parameters[0];
            Values := ParamItem.Param as TStrCollection;
          end
          else
          begin
            Values := Item.ScreenObject.ModflowStrBoundary.Values as TStrCollection;
          end;

          for ItemIndex := 0 to Values.Count - 1 do
          begin
            (Values[ItemIndex] as TStrItem).OutflowSegment := Item.NewOutFlowSegment;
          end;
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TUndoChangeStreamLinkages.Undo;
var
  Index: Integer;
  Item: TStreamLinkageChangeItem;
  ParamIcalc: TSfrParamIcalcCollection;
  PI_Index: Integer;
  ParamItem: TModflowParamItem;
  Values: TStrCollection;
  ItemIndex: Integer;
begin
  for Index := 0 to FLinkages.Count - 1 do
  begin
    Item := FLinkages.Items[Index];
    case FLinkages.FParameterType of
      ptSFR:
        begin
          ParamIcalc := Item.ScreenObject.ModflowSfrBoundary.ParamIcalc;
          for PI_Index := 0 to ParamIcalc.Count - 1 do
          begin
            ParamIcalc.Items[PI_Index].OutflowSegment :=
              Item.OldOutFlowSegments[PI_Index];
          end;
        end;
      ptSTR:
        begin
          if Item.ScreenObject.ModflowStrBoundary.Parameters.Count > 0 then
          begin
            ParamItem := Item.ScreenObject.ModflowStrBoundary.Parameters[0];
            Values := ParamItem.Param as TStrCollection;
          end
          else
          begin
            Values := Item.ScreenObject.ModflowStrBoundary.Values as TStrCollection;
          end;

          for ItemIndex := 0 to Values.Count - 1 do
          begin
            (Values[ItemIndex] as TStrItem).OutflowSegment := Item.OldOutFlowSegments[ItemIndex];
          end;
        end;
    end;
  end;
end;

procedure TfrmLinkStreams.btnApplyClick(Sender: TObject);
var
  Linkages: TStreamLinkageChangeCollection;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  NearestLake, NearestStream: TScreenObject;
  Linkage: TStreamLinkageChangeItem;
  Tolerance: double;
  OutflowSegmentAssigned: boolean;
  ParamIcalc: TSfrParamIcalcCollection;
  PI_Index: Integer;
  OutFlowSegmentNeedsToChange: Boolean;
  ParameterType: TParameterType;
  Values: TStrCollection;
  ParamItem: TModflowParamItem;
  ItemIndex: Integer;
  StrItem: TStrItem;
begin
  inherited;
  Tolerance := StrToFloat(rdeTolerance.Text);
  ParameterType := ptUndefined;
  case rgStreamtype.ItemIndex of
    0: ParameterType := ptSFR;
    1: ParameterType := ptSTR;
    else Assert(False);
  end;
  Linkages := TStreamLinkageChangeCollection.Create(ParameterType);
  try
    for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      case ParameterType of
        ptSFR:
          begin
            if (ScreenObject.ModflowSfrBoundary = nil)
              or not ScreenObject.ModflowSfrBoundary.Used then
            begin
              Continue;
            end;
          end;
        ptSTR:
          begin
            if (ScreenObject.ModflowStrBoundary = nil)
              or not ScreenObject.ModflowStrBoundary.Used then
            begin
              Continue;
            end;
          end;
      end;
      if (rgWhatToLink.ItemIndex = 1) and not ScreenObject.Selected then
      begin
        Continue;
      end;
      if cbKeepExistingLinkages.Checked then
      begin
        OutflowSegmentAssigned := False;
        case ParameterType of
          ptSFR:
            begin
              ParamIcalc := ScreenObject.ModflowSfrBoundary.ParamIcalc;
              for PI_Index := 0 to ParamIcalc.Count - 1 do
              begin
                if ParamIcalc.Items[PI_Index].OutflowSegment <> 0 then
                begin
                  OutflowSegmentAssigned := True;
                  break;
                end;
              end;
            end;
          ptSTR:
            begin
              if ScreenObject.ModflowStrBoundary.Parameters.Count > 0 then
              begin
                ParamItem := ScreenObject.ModflowStrBoundary.Parameters[0];
                Values := ParamItem.Param as TStrCollection;
              end
              else
              begin
                Values := ScreenObject.ModflowStrBoundary.Values as TStrCollection;
              end;
              for ItemIndex := 0 to Values.Count - 1 do
              begin
                StrItem := Values[ItemIndex] as TStrItem;
                if StrItem.OutflowSegment > 0 then
                begin
                  OutflowSegmentAssigned := True;
                  break;
                end;
              end;
            end;
          else Assert(False);
        end;
        if OutflowSegmentAssigned then
        begin
          Continue;
        end;
      end;
      NearestStream := nil;
      NearestLake := nil;
      case ParameterType of
        ptSFR:
          begin
            frmGoPhast.PhastModel.LocateNearestLakeOrStream(ScreenObject,
                NearestLake, NearestStream, Tolerance);
          end;
        ptSTR:
          begin
            frmGoPhast.PhastModel.LocateNearestStrStream(ScreenObject,
                NearestStream, Tolerance);
          end;
        else Assert(False);
      end;
      if (NearestStream = nil) and (NearestLake = nil) then
      begin
        Continue;
      end
      else
      begin
        if NearestLake = nil then
        begin
          case ParameterType of
            ptSFR:
              begin
                Assert(NearestStream.ModflowSfrBoundary <> nil);
                OutFlowSegmentNeedsToChange := False;
                ParamIcalc := ScreenObject.ModflowSfrBoundary.ParamIcalc;
                for PI_index := 0 to ParamIcalc.Count - 1 do
                begin
                  if ParamIcalc.Items[PI_index].OutflowSegment <>
                    NearestStream.ModflowSfrBoundary.SegementNumber then
                  begin
                    OutFlowSegmentNeedsToChange := True;
                    break;
                  end;
                end;
                if OutFlowSegmentNeedsToChange then
                begin
                  Linkage := Linkages.Add;
                  Linkage.ScreenObject := ScreenObject;
                  Linkage.NewOutFlowSegment :=
                    NearestStream.ModflowSfrBoundary.SegementNumber;
                end;
              end;
            ptSTR:
              begin
                Assert(NearestStream.ModflowStrBoundary <> nil);
                OutFlowSegmentNeedsToChange := False;

                if ScreenObject.ModflowStrBoundary.Parameters.Count > 0 then
                begin
                  ParamItem := ScreenObject.ModflowStrBoundary.Parameters[0];
                  Values := ParamItem.Param as TStrCollection;
                end
                else
                begin
                  Values := ScreenObject.ModflowStrBoundary.Values as TStrCollection;
                end;

                for ItemIndex := 0 to Values.Count - 1 do
                begin
                  if (Values[ItemIndex] as TStrItem).OutflowSegment <>
                    NearestStream.ModflowStrBoundary.SegmentNumber then
                  begin
                    OutFlowSegmentNeedsToChange := True;
                    break;
                  end;
                end;
                if OutFlowSegmentNeedsToChange then
                begin
                  Linkage := Linkages.Add;
                  Linkage.ScreenObject := ScreenObject;
                  Linkage.NewOutFlowSegment :=
                    NearestStream.ModflowStrBoundary.SegmentNumber;
                end;

              end;
            else Assert(False);
          end;
        end
        else
        begin
          Assert(NearestLake.ModflowLakBoundary <> nil);
          OutFlowSegmentNeedsToChange := False;
          ParamIcalc := ScreenObject.ModflowSfrBoundary.ParamIcalc;
          for PI_index := 0 to ParamIcalc.Count - 1 do
          begin
            if ParamIcalc.Items[PI_index].OutflowSegment <>
              -NearestLake.ModflowLakBoundary.LakeID then
            begin
              OutFlowSegmentNeedsToChange := True;
              break;
            end;
          end;
          if OutFlowSegmentNeedsToChange then
          begin
            Linkage := Linkages.Add;
            Linkage.ScreenObject := ScreenObject;
            Linkage.NewOutFlowSegment :=
              -NearestLake.ModflowLakBoundary.LakeID;
          end;
        end;
      end;
    end;
    if Linkages.Count > 0 then
    begin
      frmGoPhast.UndoStack.Submit(TUndoChangeStreamLinkages.Create(Linkages));
    end
    else
    begin
      Beep;
      MessageDlg(StrNoOutflowSegmentN, mtInformation,
        [mbOK], 0);
    end;
  finally
    Linkages.Free;
  end;
end;

procedure TfrmLinkStreams.FormShow(Sender: TObject);
begin
  inherited;
  GetData;
  OnShow := nil;
end;

procedure TfrmLinkStreams.GetData;
begin
  GetLinkTolerance;
  rgStreamtype.Buttons[0].Enabled := frmGoPhast.PhastModel.SfrIsSelected;
  rgStreamtype.Buttons[1].Enabled := frmGoPhast.PhastModel.StrIsSelected;
  if not rgStreamtype.Buttons[0].Enabled then
  begin
    rgStreamtype.ItemIndex := 1;
  end;
end;

procedure TfrmLinkStreams.GetLinkTolerance;
var
  Tolerance: double;
  Index: Integer;
begin
  if frmGoPhast.PhastModel.ModflowGrid.RowCount > 0 then
  begin
    Tolerance := frmGoPhast.PhastModel.ModflowGrid.RowWidth[0];
    for Index := 0 to frmGoPhast.PhastModel.ModflowGrid.RowCount - 1 do
    begin
      if frmGoPhast.PhastModel.ModflowGrid.RowWidth[Index] < Tolerance then
      begin
        Tolerance := frmGoPhast.PhastModel.ModflowGrid.RowWidth[Index];
      end;
    end;
    for Index := 0 to frmGoPhast.PhastModel.ModflowGrid.ColumnCount - 1 do
    begin
      if frmGoPhast.PhastModel.ModflowGrid.ColumnWidth[Index] < Tolerance then
      begin
        Tolerance := frmGoPhast.PhastModel.ModflowGrid.ColumnWidth[Index];
      end;
    end;
    Tolerance := Tolerance /2;
  end
  else
  begin
    Tolerance := -1;
  end;
  rdeTolerance.Text := FloatToStr(Tolerance);
end;

end.
