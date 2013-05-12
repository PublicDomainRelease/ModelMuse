unit frmMeshInformationUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  ExtCtrls, Buttons, Mask, JvExMask, JvSpin, ComCtrls,
  Grids, RbwDataGrid4,
  {$IF CompilerVersion >= 23.0}
  // Delphi XE2 and up
  VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart;
  {$ELSE}
  // Delphi XE and earlier
  TeEngine, Series, TeeProcs, Chart;
  {$IFEND}

type
  TfrmMeshInformation = class(TfrmCustomGoPhast)
    pnl1: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    pnl2: TPanel;
    lblBandwidth: TLabel;
    lblNumberOfNodes: TLabel;
    lblNumberOfElements: TLabel;
    seBinSize: TJvSpinEdit;
    lblBinSize: TLabel;
    chtHistogram: TChart;
    serAngles: TBarSeries;
    rdgBadElements: TRbwDataGrid4;
    splitter: TSplitter;
    procedure seBinSizeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure rdgBadElementsButtonClick(Sender: TObject; ACol, ARow: Integer);
  private
    procedure UpdateHistogram;
    { Private declarations }
  public
    procedure GetData;
    { Public declarations }
  end;

var
  frmMeshInformation: TfrmMeshInformation = nil;

implementation

uses
  frmGoPhastUnit, SutraMeshUnit, IntListUnit, FastGEO, frmGoToUnit,
  GoPhastTypes;

resourcestring
  StrBandwidthD = 'Bandwidth: %d';
  StrNumberOfNodes = 'Number of nodes: %d';
  StrNumberOfElements = 'Number of elements: %d';
  StrElementNumber = 'Element number';

{$R *.dfm}

{ TfrmMeshInformation }

procedure TfrmMeshInformation.FormCreate(Sender: TObject);
begin
  inherited;
  rdgBadElements.Cells[0,0] := StrElementNumber;
end;

procedure TfrmMeshInformation.FormShow(Sender: TObject);
begin
  inherited;
  chtHistogram.Title.Font.Assign(Font);
  chtHistogram.LeftAxis.Title.Font.Assign(Font);
  chtHistogram.BottomAxis.Title.Font.Assign(Font);
  chtHistogram.LeftAxis.LabelsFont.Assign(Font);
  chtHistogram.BottomAxis.LabelsFont.Assign(Font);
  chtHistogram.LeftAxis.LabelsFont.Size :=
    chtHistogram.LeftAxis.LabelsFont.Size-2;
  chtHistogram.BottomAxis.LabelsFont.Size :=
    chtHistogram.BottomAxis.LabelsFont.Size-2;
end;

procedure TfrmMeshInformation.GetData;
var
  Mesh: TSutraMesh3D;
begin

  Mesh := frmGoPhast.PhastModel.Mesh;
  lblBandwidth.Caption := Format(StrBandwidthD, [Mesh.Bandwidth]);
  case Mesh.MeshType of
    mt2D, mtProfile:
      begin
        lblNumberOfNodes.Caption := Format(StrNumberOfNodes,
          [Mesh.Mesh2D.Nodes.Count]);
        lblNumberOfElements.Caption := Format(StrNumberOfElements,
          [Mesh.Mesh2D.Elements.Count]);
      end;
    mt3D:
      begin
        lblNumberOfNodes.Caption := Format(StrNumberOfNodes,
          [Mesh.ActiveNodeCount]);
        lblNumberOfElements.Caption := Format(StrNumberOfElements,
          [Mesh.ActiveElementCount]);
      end;
    else Assert(False);
  end;
  UpdateHistogram;
end;

procedure TfrmMeshInformation.rdgBadElementsButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  ElementNumber: Integer;
begin
  inherited;
  ElementNumber := StrToInt(rdgBadElements.Cells[ACol, ARow]) - 1;
  MoveToMesh(ElementNumber, mmtElement, [vdTop, vdFront]);
end;

procedure TfrmMeshInformation.seBinSizeChange(Sender: TObject);
begin
  inherited;
  UpdateHistogram
end;

procedure TfrmMeshInformation.UpdateHistogram;
var
  StartIndex: Integer;
  NodeIndex: Integer;
  Bin: Integer;
  LastBin: Integer;
  Angle: Double;
  Bins: TIntegerList;
  Node1: TPoint2D;
  BinTitles: TStringList;
  Node3: TPoint2D;
  AnElement2D: TSutraElement2D;
  ElementIndex: Integer;
  BinIndex: Integer;
  BinSize: Integer;
  Node2: TPoint2D;
  Mesh: TSutraMesh3D;
  BadElements: TIntegerList;
  BadElement: Boolean;
  LayerIndex: Integer;
  AnElement3D: TSutraElement3D;
  ElementNumberIndex: Integer;
begin

  Mesh := frmGoPhast.PhastModel.Mesh;
  BinSize := seBinSize.AsInteger;
  if BinSize <= 0 then
  begin
    Exit;
  end;
  LastBin := 0;
  Bins := TIntegerList.Create;
  BinTitles := TStringList.Create;
  BadElements := TIntegerList.Create;
  try
    for ElementIndex := 0 to Mesh.Mesh2D.Elements.Count - 1 do
    begin
      AnElement2D := Mesh.Mesh2D.Elements[ElementIndex];
      Node1 := AnElement2D.Nodes[2].Node.Location;
      Node2 := AnElement2D.Nodes[3].Node.Location;
      BadElement := False;
      for NodeIndex := 0 to AnElement2D.Nodes.Count - 1 do
      begin
        Node3 := AnElement2D.Nodes[NodeIndex].Node.Location;
        Angle := OrientedVertexAngle(Node1, Node2, Node3, CounterClockwise);
        Assert(Angle >= 0);
        if Angle >= 180 then
        begin
          BadElement := True;
        end;
        Bin := Trunc(Angle / BinSize);
        while Bin >= Bins.Count do
        begin
          Bins.Add(0);
          BinTitles.Add(Format('%0:d-%1:d', [LastBin, LastBin + BinSize]));
          LastBin := LastBin + BinSize;
        end;
        Bins[Bin] := Bins[Bin] + 1;
        Node1 := Node2;
        Node2 := Node3;
      end;
      if BadElement then
      begin
        if Mesh.MeshType = mt3D then
        begin
          for LayerIndex := 0 to Mesh.LayerCount - 1 do
          begin
            AnElement3D := Mesh.ElementArray[
              LayerIndex,AnElement2D.ElementNumber];
            if AnElement3D.Active then
            begin
              BadElements.Add(AnElement3D.ElementNumber+1);
              break;
            end;
          end;
        end
        else
        begin
          BadElements.Add(AnElement2D.ElementNumber+1);
        end;
      end;
    end;
    StartIndex := 0;
    while (StartIndex < Bins.Count) and (Bins[StartIndex] = 0) do
    begin
      Inc(StartIndex);
    end;
    serAngles.Clear;
    for BinIndex := StartIndex to Bins.Count - 1 do
    begin
      serAngles.AddBar(Bins[BinIndex], BinTitles[BinIndex], clTeeColor);
    end;
    rdgBadElements.Visible := BadElements.Count > 0;
    splitter.Visible := rdgBadElements.Visible;
    if splitter.Visible then
    begin
      if splitter.Left > rdgBadElements.Left then
      begin
        splitter.Left := rdgBadElements.Left;
      end;
    end;
    if BadElements.Count > 0 then
    begin
      rdgBadElements.BeginUpdate;
      try
        rdgBadElements.RowCount := BadElements.Count+1;
        for ElementNumberIndex := 0 to BadElements.Count - 1 do
        begin
          rdgBadElements.Cells[0,ElementNumberIndex+1] :=
            IntToStr(BadElements[ElementNumberIndex]);
        end;
      finally
        rdgBadElements.EndUpdate;
      end;
    end;
  finally
    Bins.Free;
    BinTitles.Free;
    BadElements.Free;
  end;
end;

end.
