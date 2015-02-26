unit frmLinkStreamsUnit;

interface

uses
  Windows, Messages, Types, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ArgusDataEntry, ExtCtrls,
  UndoItems, ScreenObjectUnit, OrderedCollectionUnit,
  Generics.Collections, ModflowSwrReachUnit;

type
  TLinkType = (ltSFR, ltSTR, ltSWR);

  TStreamLinkageChangeItem = class(TCollectionItem)
  private
    FNewOutFlowSegment: integer;
    FOldOutFlowSegments: TIntegerDynArray;
    FScreenObject: TScreenObject;
    FNewSwrReachConnections: TSwrConnections;
    FOldSwrReachConnections: TSwrConnections;
    procedure SetNewOutFlowSegment(const Value: integer);
    procedure SetScreenObject(const Value: TScreenObject);
    function GetOutFlowSegment(Index: integer): integer;
    procedure SetNewSwrReachConnections(const Value: TSwrConnections);
    procedure SetOldSwrReachConnections(const Value: TSwrConnections);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ScreenObject: TScreenObject read FScreenObject
      write SetScreenObject;
    property NewOutFlowSegment: integer read FNewOutFlowSegment
      write SetNewOutFlowSegment;
    property OldOutFlowSegments[Index: integer]: integer read GetOutFlowSegment;
    property NewSwrReachConnections: TSwrConnections
      read FNewSwrReachConnections write SetNewSwrReachConnections;
    property OldSwrReachConnections: TSwrConnections
      read FOldSwrReachConnections write SetOldSwrReachConnections;
  end;

  TStreamLinkageChangeCollection = class(TCollection)
  private
    FLinkType: TLinkType;
    function GetItems(Index: integer): TStreamLinkageChangeItem;
    procedure SetItems(Index: integer; const Value: TStreamLinkageChangeItem);
  public
    Constructor Create(LinkType: TLinkType);
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
    procedure rgStreamtypeClick(Sender: TObject);
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
  ModflowBoundaryUnit, DataSetUnit, PhastModelUnit;

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
  OldSwrReachConnections.Assign(SourceItem.OldSwrReachConnections);
  NewSwrReachConnections.Assign(SourceItem.NewSwrReachConnections);
end;

constructor TStreamLinkageChangeItem.Create(Collection: TCollection);
begin
  inherited;
  FNewSwrReachConnections := TSwrConnections.Create(nil);
  FOldSwrReachConnections := TSwrConnections.Create(nil);
end;

destructor TStreamLinkageChangeItem.Destroy;
begin
  FOldSwrReachConnections.Free;
  FNewSwrReachConnections.Free;
  inherited;
end;

function TStreamLinkageChangeItem.GetOutFlowSegment(Index: integer): integer;
begin
  result := FOldOutFlowSegments[Index];
end;

procedure TStreamLinkageChangeItem.SetNewOutFlowSegment(const Value: integer);
begin
  FNewOutFlowSegment := Value;
end;

procedure TStreamLinkageChangeItem.SetNewSwrReachConnections(
  const Value: TSwrConnections);
begin
  FNewSwrReachConnections.Assign(Value);
end;

procedure TStreamLinkageChangeItem.SetOldSwrReachConnections(
  const Value: TSwrConnections);
begin
  FOldSwrReachConnections.Assign(Value);
end;

procedure TStreamLinkageChangeItem.SetScreenObject(const Value: TScreenObject);
var
  ParamIcalc: TSfrParamIcalcCollection;
  Index: integer;
  ParamItem: TModflowParamItem;
  Values: TStrCollection;
begin
  FScreenObject := Value;
  case (Collection as TStreamLinkageChangeCollection).FLinkType of
    ltSFR:
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
    ltSTR:
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
    ltSWR:
      begin
        Assert((FScreenObject.ModflowSwrReaches <> nil)
          and FScreenObject.ModflowSwrReaches.Used);
        FOldSwrReachConnections.Assign(FScreenObject.ModflowSwrReaches.Connections);
      end
    else
      Assert(False)
  end;
end;

{ TStreamLinkageChangeCollection }

function TStreamLinkageChangeCollection.Add: TStreamLinkageChangeItem;
begin
  result := inherited Add as TStreamLinkageChangeItem;
end;

constructor TStreamLinkageChangeCollection.Create(LinkType: TLinkType);
begin
  inherited Create(TStreamLinkageChangeItem);
  Assert(LinkType in [ltSFR, ltSTR, ltSWR]);
  FLinkType := LinkType;
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
  FLinkages:= TStreamLinkageChangeCollection.Create(Linkages.FLinkType);
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
  ADataArray: TDataArray;
begin
  for Index := 0 to FLinkages.Count - 1 do
  begin
    Item := FLinkages.Items[Index];
    case FLinkages.FLinkType of
      ltSFR:
        begin
          ParamIcalc := Item.ScreenObject.ModflowSfrBoundary.ParamIcalc;
          for PI_Index := 0 to ParamIcalc.Count - 1 do
          begin
            ParamIcalc.Items[PI_Index].OutflowSegment := Item.NewOutFlowSegment;
          end;
        end;
      ltSTR:
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
      ltSWR:
        begin
          Item.ScreenObject.ModflowSwrReaches.Connections := Item.NewSwrReachConnections;
          ADataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(KSwrReach);
          ADataArray.Invalidate;
          frmGoPhast.frameTopView.ModelChanged := True;
          frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
        end
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
  ADataArray: TDataArray;
begin
  for Index := 0 to FLinkages.Count - 1 do
  begin
    Item := FLinkages.Items[Index];
    case FLinkages.FLinkType of
      ltSFR:
        begin
          ParamIcalc := Item.ScreenObject.ModflowSfrBoundary.ParamIcalc;
          for PI_Index := 0 to ParamIcalc.Count - 1 do
          begin
            ParamIcalc.Items[PI_Index].OutflowSegment :=
              Item.OldOutFlowSegments[PI_Index];
          end;
        end;
      ltSTR:
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
      ltSWR:
        begin
          Item.ScreenObject.ModflowSwrReaches.Connections := Item.OldSwrReachConnections;
          ADataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(KSwrReach);
          ADataArray.Invalidate;
          frmGoPhast.frameTopView.ModelChanged := True;
          frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
        end
      else
        Assert(False);
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
  LinkType: TLinkType;
  Values: TStrCollection;
  ParamItem: TModflowParamItem;
  ItemIndex: Integer;
  StrItem: TStrItem;
  NearbyReachObjects: TList<TScreenObject>;
  ConnectionIndex: Integer;
  AConnection: TSwrConnectionItem;
  ObjectIndex: Integer;
begin
  inherited;
  Tolerance := StrToFloat(rdeTolerance.Text);
  LinkType := TLinkType(rgStreamtype.ItemIndex);
  NearbyReachObjects := TList<TScreenObject>.Create;
  Linkages := TStreamLinkageChangeCollection.Create(LinkType);
  try
    for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      case LinkType of
        ltSFR:
          begin
            if (ScreenObject.ModflowSfrBoundary = nil)
              or not ScreenObject.ModflowSfrBoundary.Used then
            begin
              Continue;
            end;
          end;
        ltSTR:
          begin
            if (ScreenObject.ModflowStrBoundary = nil)
              or not ScreenObject.ModflowStrBoundary.Used then
            begin
              Continue;
            end;
          end;
        ltSWR:
          begin
            if (ScreenObject.ModflowSwrReaches = nil)
              or not ScreenObject.ModflowSwrReaches.Used then
            begin
              Continue;
            end;
          end;
        else
          Assert(False);
      end;
      if (rgWhatToLink.ItemIndex = 1) and not ScreenObject.Selected then
      begin
        Continue;
      end;
      if cbKeepExistingLinkages.Checked then
      begin
        OutflowSegmentAssigned := False;
        case LinkType of
          ltSFR:
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
          ltSTR:
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
          ltSWR:
            begin
              OutflowSegmentAssigned := False;
              // do nothing.
            end
          else Assert(False);
        end;
        if OutflowSegmentAssigned then
        begin
          Continue;
        end;
      end;
      NearestStream := nil;
      NearestLake := nil;
      case LinkType of
        ltSFR:
          begin
            frmGoPhast.PhastModel.LocateNearestLakeOrStream(ScreenObject,
                NearestLake, NearestStream, Tolerance);
          end;
        ltSTR:
          begin
            frmGoPhast.PhastModel.LocateNearestStrStream(ScreenObject,
                NearestStream, Tolerance);
          end;
        ltSWR:
          begin
            frmGoPhast.PhastModel.LocateNearestSwrReachObjects(ScreenObject,
              NearbyReachObjects, Tolerance);
            if rgWhatToLink.ItemIndex = 1 then
            begin
              for ObjectIndex := NearbyReachObjects.Count - 1 downto 0 do
              begin
                if not NearbyReachObjects[ObjectIndex].Selected then
                begin
                  NearbyReachObjects.Delete(ObjectIndex);
                end;
              end;
            end;
          end
        else Assert(False);
      end;
      if (NearestStream = nil) and (NearestLake = nil)
        and (NearbyReachObjects.Count = 0) then
      begin
        if (LinkType = ltSWR)
          and (ScreenObject.ModflowSwrReaches.Connections.Count > 0)
          and not cbKeepExistingLinkages.Checked then
        begin
          Linkage := Linkages.Add;
          Linkage.ScreenObject := ScreenObject;
          Linkage.NewSwrReachConnections.Clear;
        end;
        Continue;
      end
      else
      begin
        if NearestLake = nil then
        begin
          case LinkType of
            ltSFR:
              begin
                Assert(NearestStream.ModflowSfrBoundary <> nil);
                OutFlowSegmentNeedsToChange := False;
                ParamIcalc := ScreenObject.ModflowSfrBoundary.ParamIcalc;
                for PI_index := 0 to ParamIcalc.Count - 1 do
                begin
                  if ParamIcalc.Items[PI_index].OutflowSegment <>
                    NearestStream.ModflowSfrBoundary.SegmentNumber then
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
                    NearestStream.ModflowSfrBoundary.SegmentNumber;
                end;
              end;
            ltSTR:
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
            ltSWR:
              begin
                if cbKeepExistingLinkages.Checked then
                begin
                  for ConnectionIndex := 0 to ScreenObject.ModflowSwrReaches.Connections.Count - 1 do
                  begin
                    AConnection := ScreenObject.ModflowSwrReaches.Connections[ConnectionIndex];
                    if AConnection.Method = scmObject then
                    begin
                      for ObjectIndex := NearbyReachObjects.Count - 1 downto 0 do
                      begin
                        if NearbyReachObjects[ObjectIndex].Name = AConnection.ScreenObjectName then
                        begin
                          NearbyReachObjects.Delete(ObjectIndex);
                        end;
                      end;
                    end;
                  end;
                end;
                if NearbyReachObjects.Count > 0 then
                begin
                  Linkage := Linkages.Add;
                  Linkage.ScreenObject := ScreenObject;
                  Linkage.NewSwrReachConnections := ScreenObject.ModflowSwrReaches.Connections;
                  if not cbKeepExistingLinkages.Checked then
                  begin
                    Linkage.NewSwrReachConnections.Clear;
                  end;
                  for ObjectIndex := 0 to NearbyReachObjects.Count - 1 do
                  begin
                    AConnection := Linkage.NewSwrReachConnections.Add;
                    AConnection.Method := scmObject;
                    AConnection.ScreenObject := NearbyReachObjects[ObjectIndex];
                  end;
                end;
              end
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
    NearbyReachObjects.Free;
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
  rgStreamtype.Buttons[2].Enabled := frmGoPhast.PhastModel.SwrIsSelected;
  if not rgStreamtype.Buttons[0].Enabled then
  begin
    if rgStreamtype.Buttons[1].Enabled then
    begin
      rgStreamtype.ItemIndex := 1;
    end
    else
    begin
      rgStreamtype.ItemIndex := 2;
    end;
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

procedure TfrmLinkStreams.rgStreamtypeClick(Sender: TObject);
begin
  inherited;
  case TLinkType(rgStreamtype.ItemIndex) of
    ltSFR, ltSTR:
      begin
        rgWhatToLink.Items[0] := 'All streams';
        rgWhatToLink.Items[1] := 'Selected streams';
      end;
    ltSWR:
      begin
        rgWhatToLink.Items[0] := 'All reach objects';
        rgWhatToLink.Items[1] := 'Selected reach objects';
      end;
  end;
end;

end.
