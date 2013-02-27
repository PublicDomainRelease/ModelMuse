unit ModflowHeadObsResults;

interface

uses
  Windows, GR32, GoPhastTypes, ZoomBox2, SysUtils, Dialogs, Classes, Types,
  DataSetUnit, Graphics;

type
  THeadObsCollection = class;

  THeadObsItem = class(TPhastCollectionItem)
  private
    FName: string;
    FObservedValue: double;
    FSimulatedValue: double;
    FTime: double;
    FVisible: boolean;
    FX: double;
    FY: double;
    FHeadObsCollection: THeadObsCollection;
    FScreenObjectName: string;
    FOriginalOrder: Integer;
    procedure SetName(const Value: string);
    procedure SetObservedValue(const Value: double);
    procedure SetSimulatedValue(const Value: double);
    procedure SetTime(const Value: double);
    procedure SetVisible(const Value: boolean);
    procedure SetX(const Value: double);
    procedure SetY(const Value: double);
    procedure SetScreenObjectName(const Value: string);
    procedure SetOriginalOrder(const Value: Integer);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    procedure Draw(const BitMap: TBitmap32; const ZoomBox: TQRbwZoomBox2);
    function Residual: double;
  published
    property Name: string read FName write SetName;
    property ScreenObjectName: string read FScreenObjectName write SetScreenObjectName;
    property ObservedValue: double read FObservedValue write SetObservedValue;
    property SimulatedValue: double read FSimulatedValue write SetSimulatedValue;
    property Time: double read FTime write SetTime;
    property Visible: boolean read FVisible write SetVisible;
    property X: double read FX write SetX;
    property Y: double read FY write SetY;
    property OriginalOrder: Integer read FOriginalOrder write SetOriginalOrder;
  end;

  THeadObsCollection = class(TPhastCollection)
  private
    FFileName: string;
    FFileDate: TDateTime;
    FMaxTimeLimit: TColoringLimit;
    FMaxResidualLimit: TColoringLimit;
    FMinTimeLimit: TColoringLimit;
    FMinResidualLimit: TColoringLimit;
    FPositiveColor: TColor;
    FNegativeColor: TColor;
    FPositiveColor32: TColor32;
    FNegativeColor32: TColor32;
    FMaxResidual: double;
    FMaxSymbolSize: integer;
    FDisplayResiduals: boolean;
    procedure SetFileName(const Value: string);
    procedure SetFileDate(const Value: TDateTime);
    procedure SetMaxResidualLimit(const Value: TColoringLimit);
    procedure SetMaxTimeLimit(const Value: TColoringLimit);
    procedure SetMinResidualLimit(const Value: TColoringLimit);
    procedure SetMinTimeLimit(const Value: TColoringLimit);
    function GetHeadObs(Index: Integer): THeadObsItem;
    procedure UpdateVisibleItems(Sender: TObject);
    procedure SetNegativeColor(const Value: TColor);
    procedure SetPositiveColor(const Value: TColor);
    procedure SetMaxSymbolSize(const Value: integer);
    procedure SetDisplayResiduals(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure Draw(const BitMap: TBitmap32; const ZoomBox: TQRbwZoomBox2);
    property HeadObs[Index: Integer]: THeadObsItem read GetHeadObs; default;
    function ReadFromFile: boolean;
    procedure CalculateMaxResidual;
    property MaxResidual: double read FMaxResidual write FMaxResidual;
  published
    property FileName: string read FFileName write SetFileName;
    property FileDate: TDateTime read FFileDate write SetFileDate;
    property MaxResidualLimit: TColoringLimit read FMaxResidualLimit
      write SetMaxResidualLimit;
    property MinResidualLimit: TColoringLimit read FMinResidualLimit
      write SetMinResidualLimit;
    property MaxTimeLimit: TColoringLimit read FMaxTimeLimit
      write SetMaxTimeLimit;
    property MinTimeLimit: TColoringLimit read FMinTimeLimit
      write SetMinTimeLimit;
    property NegativeColor: TColor read FNegativeColor write SetNegativeColor default clRed;
    property PositiveColor: TColor read FPositiveColor write SetPositiveColor default clBlue;
    property MaxSymbolSize: integer read FMaxSymbolSize write SetMaxSymbolSize default 20;
    property Visible: boolean read FDisplayResiduals write SetDisplayResiduals default True;
  end;

implementation

uses
  StrUtils, ModelMuseUtilities, frmGoPhastUnit, ScreenObjectUnit,
  ModflowHobUnit, FastGEO, BigCanvasMethods, GR32_Polygons, Math,
  PhastModelUnit, frmErrorsAndWarningsUnit;

resourcestring
  StrTheFileFromWhich = 'The file from which you are attempting to read ' +
  'observed head data, %s, does not exist.';
  StrTheFileEmpty = 'The file from which you are attempting to read observed' +
  ' head data, %s, is empty.';
  StrWrongData = 'The file from which you are attempting to read observed he' +
  'ad data, %s, does not appear to contain observed head data.';
  StrTheObservationSI = 'The observation %s in the head observation results ' +
  'file is not among the observations defined in this model.';
  StrTheFollowingLines = 'The following lines of the Head Observation file c' +
  'ontain invalid values. Those lines have been skipped.';

const
  StrSIMULATEDEQUIVALEN = '"SIMULATED EQUIVALENT"   "OBSERVED VALUE"    "OBS' +
  'ERVATION NAME"';

{ THeadObsItem }

procedure THeadObsItem.Assign(Source: TPersistent);
var
  SourceItem: THeadObsItem;
begin
  if Source is THeadObsItem then
  begin
    SourceItem := THeadObsItem(Source);
    Name := SourceItem.Name;
    ScreenObjectName := SourceItem.ScreenObjectName;
    ObservedValue := SourceItem.ObservedValue;
    SimulatedValue := SourceItem.SimulatedValue;
    Time := SourceItem.Time;
    Visible := SourceItem.Visible;
    X := SourceItem.X;
    Y := SourceItem.Y;
    OriginalOrder := SourceItem.OriginalOrder;
  end
  else
  begin
    inherited;
  end;
end;

constructor THeadObsItem.Create(Collection: TCollection);
begin
  inherited;
  FHeadObsCollection := Collection as THeadObsCollection;
end;

function THeadObsItem.Residual: double;
begin
  Result := ObservedValue - SimulatedValue;
end;

procedure THeadObsItem.Draw(const BitMap: TBitmap32;
  const ZoomBox: TQRbwZoomBox2);
const
  MaxPoints = 12;
  PointsPerHalfCircle = MaxPoints div 2;
var
  XCenter: Integer;
  YCenter: Integer;
  Radius: Double;
  Points: TPointArray;
  PointIndex: Integer;
  Angle: double;
  Color: TColor32;
  APolygon: TPolygon32;
begin
  if not Visible then
  begin
    Exit;
  end;
  XCenter := ZoomBox.XCoord(X);
  YCenter := ZoomBox.YCoord(Y);
  if Residual > 0 then
  begin
    Color := FHeadObsCollection.FPositiveColor32;
  end
  else
  begin
    Color := FHeadObsCollection.FNegativeColor32;
  end;
  Radius :=
    Sqrt(Abs(Residual)/FHeadObsCollection.FMaxResidual)
    * (FHeadObsCollection.MaxSymbolSize / 2);
  SetLength(Points, MaxPoints);
  for PointIndex := 0 to MaxPoints - 1 do
  begin
    Angle := PointIndex * Pi / PointsPerHalfCircle;
    Points[PointIndex].X := Round(XCenter + Cos(Angle)*Radius);
    Points[PointIndex].Y := Round(YCenter + Sin(Angle)*Radius);
  end;
  APolygon := nil;
  DrawBigPolygon32(BitMap, Color, Color, 0.1, Points, APolygon, False, True);
end;

procedure THeadObsItem.SetName(const Value: string);
begin
  SetStringProperty(FName, Value);
end;

procedure THeadObsItem.SetObservedValue(const Value: double);
begin
  SetRealProperty(FObservedValue, Value);
end;

procedure THeadObsItem.SetOriginalOrder(const Value: Integer);
begin
  SetIntegerProperty(FOriginalOrder, Value);
end;

procedure THeadObsItem.SetScreenObjectName(const Value: string);
begin
  SetStringProperty(FScreenObjectName, Value);
end;

procedure THeadObsItem.SetSimulatedValue(const Value: double);
begin
  SetRealProperty(FSimulatedValue, Value);
end;

procedure THeadObsItem.SetTime(const Value: double);
begin
  SetRealProperty(FTime, Value);
end;

procedure THeadObsItem.SetVisible(const Value: boolean);
begin
  SetBooleanProperty(FVisible , Value);
end;

procedure THeadObsItem.SetX(const Value: double);
begin
  SetRealProperty(FX, Value);
end;

procedure THeadObsItem.SetY(const Value: double);
begin
  SetRealProperty(FY, Value);
end;

{ THeadObsCollection }

constructor THeadObsCollection.Create(Model: TBaseModel);
begin
  inherited Create(THeadObsItem, Model);
  FMaxTimeLimit := TColoringLimit.Create;
  FMaxResidualLimit := TColoringLimit.Create;
  FMinTimeLimit := TColoringLimit.Create;
  FMinResidualLimit := TColoringLimit.Create;

  FMaxTimeLimit.OnChange := UpdateVisibleItems;
  FMaxResidualLimit.OnChange := UpdateVisibleItems;
  FMinTimeLimit.OnChange := UpdateVisibleItems;
  FMinResidualLimit.OnChange := UpdateVisibleItems;

  NegativeColor := clRed;
  PositiveColor := clBlue;

  FMaxSymbolSize := 20;
  FDisplayResiduals := True;
end;

destructor THeadObsCollection.Destroy;
begin
  FMaxTimeLimit.Free;
  FMaxResidualLimit.Free;
  FMinTimeLimit.Free;
  FMinResidualLimit.Free;
  inherited;
end;

procedure THeadObsCollection.Draw(const BitMap: TBitmap32;
  const ZoomBox: TQRbwZoomBox2);
var
  ItemIndex: Integer;
  Item: THeadObsItem;
begin
  if (Count = 0) or (FMaxResidual = 0) then
  begin
    Exit;
  end;
  if Visible then
  begin
    for ItemIndex := 0 to Count - 1 do
    begin
      Item := HeadObs[ItemIndex];
      Item.Draw(BitMap, ZoomBox);
    end;
  end;
end;

function THeadObsCollection.GetHeadObs(Index: Integer): THeadObsItem;
begin
  Result := Items[Index] as THeadObsItem;
end;

function SortItems(Item1, Item2: Pointer): Integer;
var
  ObsItem1: THeadObsItem;
  ObsItem2: THeadObsItem;
begin
  ObsItem1 := Item1;
  ObsItem2 := Item2;
  result := Sign(Abs(ObsItem2.Residual) - Abs(ObsItem1.Residual));
end;

function THeadObsCollection.ReadFromFile: boolean;
var
  HeadFile: TStringList;
  LineIndex: Integer;
  Item: THeadObsItem;
  HeadObsObjects: TStringList;
  ObjectIndex: Integer;
  ScreenObject: TScreenObject;
  HeadObservations: THobBoundary;
  ObsIndex: integer;
  ObsItem: THobItem;
  ObsCollection: THobCollection;
  APoint: TPoint2D;
  StringSplitter: TStringList;
  Sorter: TList;
  Index: Integer;
  Val1: Extended;
  Val2: Extended;
  ShowErrors: Boolean;
begin
  result := False;
  ShowErrors := False;
  try
    if not FileExists(FileName) then
    begin
      Beep;
      MessageDlg(Format(StrTheFileFromWhich, [FileName]), mtError, [mbOK], 0);
      Exit;
    end;
    HeadFile := TStringList.Create;
    try
      try
        HeadFile.LoadFromFile(FileName);
      except on EFOpenError do
        begin
          CantOpenFileMessage(FileName);
          Exit;
        end;
      end;
      if HeadFile.Count = 0 then
      begin
        Beep;
        MessageDlg(Format(StrTheFileEmpty, [FileName]), mtError, [mbOK], 0);
        Exit;
      end;
      if HeadFile[0] <> StrSIMULATEDEQUIVALEN then
      begin
        Beep;
        MessageDlg(Format(StrWrongData, [FileName]), mtError, [mbOK], 0);
        Exit;
      end;
      Clear;
      HeadObsObjects := TStringList.Create;
      try
        for ObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
        begin
          ScreenObject := frmGoPhast.PhastModel.ScreenObjects[ObjectIndex];
          HeadObservations := ScreenObject.ModflowBoundaries.
            ModflowHeadObservations;
          if HeadObservations <> nil then
          begin
            for ObsIndex := 0 to HeadObservations.Values.Count - 1 do
            begin
              ObsItem := HeadObservations.Values.HobItems[ObsIndex];
              HeadObsObjects.AddObject(HeadObservations.GetItemObsName(ObsItem),
                ObsItem)
            end;
          end;
        end;
        HeadObsObjects.Sorted := True;

        frmErrorsAndWarnings.RemoveErrorGroup(frmGoPhast.PhastModel,
          StrTheFollowingLines);
        StringSplitter := TStringList.Create();
        try
          StringSplitter.Delimiter := ' ';
          for LineIndex := 1 to HeadFile.Count - 1 do
          begin
            StringSplitter.DelimitedText := HeadFile[LineIndex];
            Assert(StringSplitter.Count = 3);
            ObsIndex := HeadObsObjects.IndexOf(StringSplitter[2]);
            if ObsIndex < 0 then
            begin
              Beep;
              MessageDlg(Format(StrTheObservationSI, [StringSplitter[2]]),
                mtError, [mbOK], 0);
              Clear;
              Exit;
            end;
            Assert(ObsIndex >= 0);
            Val1 := 0;
            Val2 := 0;
            try
              Val1 := FortranStrToFloat(StringSplitter[0]);
              Val2 := FortranStrToFloat(StringSplitter[1]);
            except on EConvertError do
              begin
                frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
                  StrTheFollowingLines, IntToStr(LineIndex+1));
                ShowErrors := True;
                Continue;
              end;
            end;
            Item := Add as THeadObsItem;
            Item.OriginalOrder := Count-1;
            Item.SimulatedValue := Val1;
            Item.ObservedValue := Val2;
            Item.Name := StringSplitter[2];
            ObsItem := HeadObsObjects.Objects[ObsIndex] as THobItem;
            ObsCollection := ObsItem.Collection as THobCollection;
            ScreenObject := ObsCollection.ScreenObject as TScreenObject;
            Item.Time := ObsItem.Time;
            APoint := ScreenObject.Points[0];
            Item.X := APoint.x;
            Item.Y := APoint.y;
            Item.ScreenObjectName := ScreenObject.Name;
          end;
        finally
          StringSplitter.Free;
        end;
      finally
        HeadObsObjects.Free;
      end;
    finally
      HeadFile.Free;
    end;
    UpdateVisibleItems(nil);
    FileAge(FileName, FFileDate);
    Sorter := TList.Create;
    try
      for Index := 0 to Count - 1 do
      begin
        Sorter.Add(Items[Index])
      end;
      Sorter.Sort(SortItems);
      for Index := 0 to Sorter.Count - 1 do
      begin
        Item := Sorter[Index];
        Item.Index := Index;
      end;
    finally
      Sorter.Free;
    end;
  finally
    if ShowErrors then
    begin
      frmErrorsAndWarnings.ShowAfterDelay;
    end;
  end;
  result := True;
end;

procedure THeadObsCollection.Assign(Source: TPersistent);
var
  SourceHeadObs: THeadObsCollection;
begin
  if Source is THeadObsCollection then
  begin
    SourceHeadObs := THeadObsCollection(Source);
    FileName := SourceHeadObs.FileName;
    FileDate := SourceHeadObs.FileDate;
    MaxResidualLimit := SourceHeadObs.MaxResidualLimit;
    MinResidualLimit := SourceHeadObs.MinResidualLimit;
    MaxTimeLimit := SourceHeadObs.MaxTimeLimit;
    MinTimeLimit := SourceHeadObs.MinTimeLimit;
    NegativeColor := SourceHeadObs.NegativeColor;
    PositiveColor := SourceHeadObs.PositiveColor;
    MaxSymbolSize := SourceHeadObs.MaxSymbolSize;
    Visible := SourceHeadObs.Visible;
  end;
  inherited;
end;

procedure THeadObsCollection.CalculateMaxResidual;
var
  Item: THeadObsItem;
  ItemIndex: Integer;
begin
  FMaxResidual := 0;
  if Visible then
  begin
    for ItemIndex := 0 to Count - 1 do
    begin
      Item := HeadObs[ItemIndex];
      if Item.Visible and (Abs(Item.Residual) > FMaxResidual) then
      begin
        FMaxResidual := Abs(Item.Residual);
      end;
    end;
  end;
end;

procedure THeadObsCollection.UpdateVisibleItems(Sender: TObject);
var
  ItemIndex: Integer;
  Item: THeadObsItem;
  ItemResidual: Double;
begin
  for ItemIndex := 0 to Count - 1 do
  begin
    Item := HeadObs[ItemIndex];
    Item.Visible := True;
    ItemResidual := Item.Residual;
    if FMaxResidualLimit.UseLimit and (ItemResidual > FMaxResidualLimit.RealLimitValue) then
    begin
      Item.Visible := False;
    end;
    if FMinResidualLimit.UseLimit and (ItemResidual < FMinResidualLimit.RealLimitValue) then
    begin
      Item.Visible := False;
    end;
    if FMaxTimeLimit.UseLimit and (Item.Time > FMaxTimeLimit.RealLimitValue) then
    begin
      Item.Visible := False;
    end;
    if FMinTimeLimit.UseLimit and (Item.Time < FMinTimeLimit.RealLimitValue) then
    begin
      Item.Visible := False;
    end;
  end;
end;

procedure THeadObsCollection.SetDisplayResiduals(const Value: boolean);
begin
  if FDisplayResiduals <> Value then
  begin
    FDisplayResiduals := Value;
    InvalidateModel;
  end;
end;

procedure THeadObsCollection.SetFileDate(const Value: TDateTime);
begin
  FFileDate := Value;
end;

procedure THeadObsCollection.SetFileName(const Value: string);
begin
  if FFileName <> Value then
  begin
    FFileName := Value;
    InvalidateModel;
  end;
end;

procedure THeadObsCollection.SetMaxResidualLimit(
  const Value: TColoringLimit);
begin
  FMaxResidualLimit.Assign(Value);
end;

procedure THeadObsCollection.SetMaxSymbolSize(const Value: integer);
begin
  if FMaxSymbolSize <> Value then
  begin
    FMaxSymbolSize := Value;
    InvalidateModel;
  end;
end;

procedure THeadObsCollection.SetMaxTimeLimit(const Value: TColoringLimit);
begin
  FMaxTimeLimit.Assign(Value);
end;

procedure THeadObsCollection.SetMinResidualLimit(
  const Value: TColoringLimit);
begin
  FMinResidualLimit.Assign(Value);
end;

procedure THeadObsCollection.SetMinTimeLimit(const Value: TColoringLimit);
begin
  FMinTimeLimit.Assign(Value);
end;

procedure THeadObsCollection.SetNegativeColor(const Value: TColor);
begin
  if FNegativeColor <> Value then
  begin
    FNegativeColor := Value;
    FNegativeColor32 := Color32(FNegativeColor);
    InvalidateModel;
  end;
end;

procedure THeadObsCollection.SetPositiveColor(const Value: TColor);
begin
  if FPositiveColor <> Value then
  begin
    FPositiveColor := Value;
    FPositiveColor32 := Color32(FPositiveColor);
    InvalidateModel;
  end;
end;

end.
