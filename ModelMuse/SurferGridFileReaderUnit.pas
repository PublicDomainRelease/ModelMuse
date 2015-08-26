unit SurferGridFileReaderUnit;

// See http://www.geospatialdesigns.com/surfer6_format.htm,
// http://www.geospatialdesigns.com/surfer7_format.htm,
// and http://www.goldensoftware.com/index.php?option=com_fss&view=kb&prodid=2&kbartid=970&Itemid=182
// for the format of Surfer .grd files.

interface

uses SysUtils, Classes, RasterValuesAlongSegmentsUnit, FastGEO;

type
  TSurferFileType = (sft6, sft7, sftAscii);

  TGridData6Header = record
    ID: array [0..3] of AnsiChar;
    nx: Smallint;
    ny: Smallint;
    Xlo: double;
    Xhi: double;
    Ylo: double;
    Yhi: double;
    Zlo: double;
    Zhi: double;
  end;

  TSurferPoint = record
    X: double;
    Y: double;
    Z: double;
  end;

  TSurfer6Grid = record
    Header: TGridData6Header;
    Points: array of TSurferPoint;
  end;

  EGrdReadError = class(Exception);

  TSurfer7Header = record
    ID: longint;
    Size: longint;
  end;

  TGrid7Header = record
    nRow: longint;
    nCol: longint;
    xLL: double;
    yLL: double;
    xSize: double;
    ySize: double;
    zMin: double;
    zMax: double;
    // Rotation is not currently used.
    Rotation: double;
    BlankValue: double;
  end;

  TSurfer7Grid = record
    Header: TGrid7Header;
    Points: array of TSurferPoint;
  end;

  TSurferZArray = array of array of Double;

  TSurferRaster6 = class(TInterfacedObject, IRaster)
  private
    FHeader: TGridData6Header;
    FZ: TSurferZArray;
    FDeltaX: double;
    FDeltaY: double;
    function GetLowerLeft: TPoint2D;
    function GetXCount: integer;
    function GetYCount: integer;
    function GetXSpacing: Double;
    function GetYSpacing: Double;
    function GetZ(XIndex, YIndex: Integer): Double;
    function GetIgnore(XIndex, YIndex: Integer): Boolean;
    procedure SetCellSizes;
  public
    // @name is the lower left corner of the grid. The data point for the
    // lower left cells is at
    // (LowerLeft.x + (XSpacing/2), LowerLeft.y + (YSpacing/2)).
    // ESRI ASCII grid files follow this format. Surfer Grid files need to
    // be adjusted because they give the coordinates of the cell centers
    // directly.
    property LowerLeft: TPoint2D read GetLowerLeft;
    property XCount: Integer read GetXCount;
    property YCount: Integer read GetYCount;
    property XSpacing: Double read GetXSpacing;
    property YSpacing: Double read GetYSpacing;
    property Z[XIndex, YIndex: Integer]: double read GetZ;
    property Ignore[XIndex, YIndex: Integer]: Boolean read GetIgnore;
  end;

  TSurferRaster7 = class(TInterfacedObject, IRaster)
  private
    FHeader: TGrid7Header;
    FZ: TSurferZArray;
    function GetLowerLeft: TPoint2D;
    function GetXCount: integer;
    function GetYCount: integer;
    function GetXSpacing: Double;
    function GetYSpacing: Double;
    function GetZ(XIndex, YIndex: Integer): Double;
    function GetIgnore(XIndex, YIndex: Integer): Boolean;
  public
    // @name is the lower left corner of the grid. The data point for the
    // lower left cells is at
    // (LowerLeft.x + (XSpacing/2), LowerLeft.y + (YSpacing/2)).
    // ESRI ASCII grid files follow this format. Surfer Grid files need to
    // be adjusted because they give the coordinates of the cell centers
    // directly.
    property LowerLeft: TPoint2D read GetLowerLeft;
    property XCount: Integer read GetXCount;
    property YCount: Integer read GetYCount;
    property XSpacing: Double read GetXSpacing;
    property YSpacing: Double read GetYSpacing;
    property Z[XIndex, YIndex: Integer]: double read GetZ;
    property Ignore[XIndex, YIndex: Integer]: Boolean read GetIgnore;
  end;

function SurferFileType(const FileName: string): TSurferFileType;

procedure ReadSurfer6GrdFile(FileName: string; out SurferGrid: TSurfer6Grid); overload;
procedure ReadSurfer7GrdFile(FileName: string; out Surfer7Grid: TSurfer7Grid); overload;
procedure ReadSurferAsciiFile(FileName: string; out SurferGrid: TSurfer6Grid); overload;

procedure ReadSurfer6GrdFile(FileName: string; SurferRaster: TSurferRaster6); overload;
procedure ReadSurfer7GrdFile(FileName: string; SurferRaster: TSurferRaster7); overload;
procedure ReadSurferAsciiFile(FileName: string; SurferRaster: TSurferRaster6); overload;

implementation

resourcestring
  StrSIsNotASurferG = '%s is not a Surfer grid file.';
  StrSIsNotASurfer6 = '%s is not a Surfer 6 grid file.';
  StrErrorReadingS = 'Error reading %s.';
  StrSIsNotASurferA = '%s is not a Surfer ASCII grid file.';
  StrSIsNotASurfer7 = '%s is not a Surfer 7 grid file.';

function SurferFileType(const FileName: string): TSurferFileType;
var
  GrdFile: TFileStream;
  ID: array [0..3] of AnsiChar;
begin
  result := sft6;
  Assert(FileExists(FileName));
  GrdFile := TFileStream.Create(FileName,
    fmOpenRead or fmShareCompat or fmShareDenyWrite);
  try
    GrdFile.Read(ID[0], SizeOf(ID));
    if ID = 'DSBB' then
    begin
      result := sft6;
    end
    else if ID = 'DSRB' then
    begin
      result := sft7;
    end
    else if ID = 'DSAA' then
    begin
      result := sftAscii;
    end
    else
    begin
      raise EGrdReadError.Create(Format(StrSIsNotASurferG, [FileName]));
    end;
  finally
    GrdFile.Free;
  end;
end;

procedure ReadSurfer6GrdFile(FileName: string; out SurferGrid: TSurfer6Grid);
var
  GrdFile: TFileStream;
  ZValues: array of single;
  Index: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  XLength: Double;
  X: Array of Double;
  Y: Array of Double;
  YLength: Double;
begin
  Assert(FileExists(FileName));
  GrdFile := TFileStream.Create(FileName,
    fmOpenRead or fmShareCompat or fmShareDenyWrite);
  try
    GrdFile.Read(SurferGrid.Header, SizeOf(SurferGrid.Header));
    if SurferGrid.Header.ID <> 'DSBB' then
    begin
      raise EGrdReadError.Create(Format(StrSIsNotASurfer6, [FileName]));
    end;
    if (SurferGrid.Header.nx <= 1) or (SurferGrid.Header.ny <= 1) then
    begin
      raise EGrdReadError.Create(Format(StrErrorReadingS, [FileName]));
    end;
    SetLength(ZValues, SurferGrid.Header.nx * SurferGrid.Header.ny);
    GrdFile.Read(ZValues[0], Length(ZValues)*sizeof(single));

    XLength := SurferGrid.Header.Xhi - SurferGrid.Header.Xlo;
    SetLength(X, SurferGrid.Header.nx);
    for ColIndex := 0 to SurferGrid.Header.nx - 1 do
    begin
      X[ColIndex] := SurferGrid.Header.Xlo
        + XLength*ColIndex/(SurferGrid.Header.nx - 1);
    end;

    YLength := SurferGrid.Header.Yhi - SurferGrid.Header.Ylo;
    SetLength(Y, SurferGrid.Header.ny);
    for RowIndex := 0 to SurferGrid.Header.ny - 1 do
    begin
      Y[RowIndex] := SurferGrid.Header.Ylo
        + YLength*RowIndex/(SurferGrid.Header.ny - 1);
    end;

    SetLength(SurferGrid.Points, SurferGrid.Header.nx * SurferGrid.Header.ny);
    Index := 0;
    for RowIndex := 0 to SurferGrid.Header.ny - 1 do
    begin
      for ColIndex := 0 to SurferGrid.Header.nx - 1 do
      begin
        SurferGrid.Points[Index].X := X[ColIndex];
        SurferGrid.Points[Index].Y := Y[RowIndex];
        SurferGrid.Points[Index].Z := ZValues[Index];
        Inc(Index);
      end;
    end;
  finally
    GrdFile.Free;
  end;
end;

procedure ReadSurfer6GrdFile(FileName: string; SurferRaster: TSurferRaster6);
var
  GrdFile: TFileStream;
  ZValues: array of single;
  Index: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  Assert(FileExists(FileName));
  GrdFile := TFileStream.Create(FileName,
    fmOpenRead or fmShareCompat or fmShareDenyWrite);
  try
    GrdFile.Read(SurferRaster.FHeader, SizeOf(SurferRaster.FHeader));
    if SurferRaster.FHeader.ID <> 'DSBB' then
    begin
      raise EGrdReadError.Create(Format(StrSIsNotASurfer6, [FileName]));
    end;
    if (SurferRaster.FHeader.nx <= 1) or (SurferRaster.FHeader.ny <= 1) then
    begin
      raise EGrdReadError.Create(Format(StrErrorReadingS, [FileName]));
    end;
    SetLength(SurferRaster.FZ, SurferRaster.FHeader.ny, SurferRaster.FHeader.nx);
    SetLength(ZValues, SurferRaster.FHeader.ny* SurferRaster.FHeader.nx);
    GrdFile.Read(ZValues[0], Length(ZValues)*sizeof(single));

    Index := 0;
    for RowIndex := 0 to SurferRaster.FHeader.ny - 1 do
    begin
      for ColIndex := 0 to SurferRaster.FHeader.nx - 1 do
      begin
        SurferRaster.FZ[RowIndex, ColIndex] := ZValues[Index];
        Inc(Index);
      end;
    end;
    SurferRaster.SetCellSizes;
  finally
    GrdFile.Free;
  end;
end;

procedure ReadSurferAsciiFile(FileName: string; out SurferGrid: TSurfer6Grid);
var
  GrdFile: TextFile;
  ZValues: array of single;
  Index: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  XLength: Double;
  X: Array of Double;
  Y: Array of Double;
  YLength: Double;
begin
  Assert(FileExists(FileName));
  AssignFile(GrdFile, FileName);
  Reset(GrdFile);
  try
    for Index := 0 to 3 do
    begin
      Read(GrdFile, SurferGrid.Header.ID[Index]);
    end;
    ReadLn(GrdFile);
    ReadLn(GrdFile, SurferGrid.Header.nx, SurferGrid.Header.ny);
    ReadLn(GrdFile, SurferGrid.Header.Xlo, SurferGrid.Header.Xhi);
    ReadLn(GrdFile, SurferGrid.Header.Ylo, SurferGrid.Header.Yhi);
    ReadLn(GrdFile, SurferGrid.Header.Zlo, SurferGrid.Header.Zhi);
    if SurferGrid.Header.ID <> 'DSAA' then
    begin
      raise EGrdReadError.Create(Format(StrSIsNotASurferA, [FileName]));
    end;
    if (SurferGrid.Header.nx <= 1) or (SurferGrid.Header.ny <= 1) then
    begin
      raise EGrdReadError.Create(Format(StrErrorReadingS, [FileName]));
    end;
    SetLength(ZValues, SurferGrid.Header.nx * SurferGrid.Header.ny);
    for Index := 0 to SurferGrid.Header.nx*SurferGrid.Header.ny - 1 do
    begin
      Read(GrdFile, ZValues[Index]);
    end;

    XLength := SurferGrid.Header.Xhi - SurferGrid.Header.Xlo;
    SetLength(X, SurferGrid.Header.nx);
    for ColIndex := 0 to SurferGrid.Header.nx - 1 do
    begin
      X[ColIndex] := SurferGrid.Header.Xlo
        + XLength*ColIndex/(SurferGrid.Header.nx - 1);
    end;

    YLength := SurferGrid.Header.Yhi - SurferGrid.Header.Ylo;
    SetLength(Y, SurferGrid.Header.ny);
    for RowIndex := 0 to SurferGrid.Header.ny - 1 do
    begin
      Y[RowIndex] := SurferGrid.Header.Ylo
        + YLength*RowIndex/(SurferGrid.Header.ny - 1);
    end;

    SetLength(SurferGrid.Points, SurferGrid.Header.nx * SurferGrid.Header.ny);
    Index := 0;
    for RowIndex := 0 to SurferGrid.Header.ny - 1 do
    begin
      for ColIndex := 0 to SurferGrid.Header.nx - 1 do
      begin
        SurferGrid.Points[Index].X := X[ColIndex];
        SurferGrid.Points[Index].Y := Y[RowIndex];
        SurferGrid.Points[Index].Z := ZValues[Index];
        Inc(Index);
      end;
    end;
  finally
    CloseFile(GrdFile);
  end;
end;

procedure ReadSurferAsciiFile(FileName: string; SurferRaster: TSurferRaster6);
var
  GrdFile: TextFile;
  Index: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  Assert(FileExists(FileName));
  AssignFile(GrdFile, FileName);
  Reset(GrdFile);
  try
    for Index := 0 to 3 do
    begin
      Read(GrdFile, SurferRaster.FHeader.ID[Index]);
    end;
    ReadLn(GrdFile);
    ReadLn(GrdFile, SurferRaster.FHeader.nx, SurferRaster.FHeader.ny);
    ReadLn(GrdFile, SurferRaster.FHeader.Xlo, SurferRaster.FHeader.Xhi);
    ReadLn(GrdFile, SurferRaster.FHeader.Ylo, SurferRaster.FHeader.Yhi);
    ReadLn(GrdFile, SurferRaster.FHeader.Zlo, SurferRaster.FHeader.Zhi);
    if SurferRaster.FHeader.ID <> 'DSAA' then
    begin
      raise EGrdReadError.Create(Format(StrSIsNotASurferA, [FileName]));
    end;
    if (SurferRaster.FHeader.nx <= 1) or (SurferRaster.FHeader.ny <= 1) then
    begin
      raise EGrdReadError.Create(Format(StrErrorReadingS, [FileName]));
    end;
    SetLength(SurferRaster.FZ, SurferRaster.FHeader.ny, SurferRaster.FHeader.nx);
    for RowIndex := 0 to SurferRaster.FHeader.ny - 1 do
    begin
      for ColIndex := 0 to SurferRaster.FHeader.nx - 1 do
      begin
        Read(GrdFile, SurferRaster.FZ[RowIndex, ColIndex]);
      end;
    end;
    SurferRaster.SetCellSizes;
  finally
    CloseFile(GrdFile);
  end;
end;

procedure ReadSurfer7GrdFile(FileName: string; out Surfer7Grid: TSurfer7Grid);
var
  GrdFile: TFileStream;
  Header: TSurfer7Header;
  ZValues: array of double;
  X: array of double;
  Y: array of double;
  ColIndex: Integer;
  RowIndex: Integer;
  Index: Integer;
begin
  Assert(FileExists(FileName));
  GrdFile := TFileStream.Create(FileName,
    fmOpenRead or fmShareCompat or fmShareDenyWrite);
  try
    GrdFile.Read(Header, SizeOf(Header));
    if Header.ID <> $42525344 then
    begin
      raise EGrdReadError.Create(Format(StrSIsNotASurfer7, [FileName]));
    end;
    GrdFile.Position := GrdFile.Position + Header.Size;
    While GrdFile.Position < GrdFile.Size do
    begin
      GrdFile.Read(Header, SizeOf(Header));
      if Header.ID = $44495247 then
      begin
        // Grid section
        GrdFile.Read(Surfer7Grid.Header, SizeOf(Surfer7Grid.Header));

        // Data section must follow immediately after grid section;
        GrdFile.Read(Header, SizeOf(Header));
        Assert(Header.ID = $41544144);
        SetLength(ZValues, Surfer7Grid.Header.nRow * Surfer7Grid.Header.nCol);
        GrdFile.Read(ZValues[0], Length(ZValues)*sizeof(double));

        SetLength(X, Surfer7Grid.Header.nCol);
        for ColIndex := 0 to Surfer7Grid.Header.nCol - 1 do
        begin
          X[ColIndex] := Surfer7Grid.Header.xLL
            + ColIndex * Surfer7Grid.Header.xSize
        end;

        SetLength(Y, Surfer7Grid.Header.nRow);
        for RowIndex := 0 to Surfer7Grid.Header.nRow - 1 do
        begin
          Y[RowIndex] := Surfer7Grid.Header.yLL
            + RowIndex * Surfer7Grid.Header.ySize
        end;

        SetLength(Surfer7Grid.Points,
          Surfer7Grid.Header.nRow * Surfer7Grid.Header.nCol);
        Index := 0;
        for RowIndex := 0 to Surfer7Grid.Header.nRow - 1 do
        begin
          for ColIndex := 0 to Surfer7Grid.Header.nCol - 1 do
          begin
            Surfer7Grid.Points[Index].X := X[ColIndex];
            Surfer7Grid.Points[Index].Y := Y[RowIndex];
            Surfer7Grid.Points[Index].Z := ZValues[Index];
            Inc(Index);
          end;
        end;
        Exit;
      end
      else if Header.ID = $41544144 then
      begin
        // data section
        Assert(False);
      end
      else if Header.ID = $49544c46 then
      begin
        // Fault Info section
        GrdFile.Position := GrdFile.Position + Header.Size;
      end
      else
      begin
        Assert(False);
      end;
    end;
  finally
    GrdFile.Free;
  end;
  Assert(False);
end;

procedure ReadSurfer7GrdFile(FileName: string; SurferRaster: TSurferRaster7);
var
  GrdFile: TFileStream;
  Header: TSurfer7Header;
  ZValues: array of double;
  ColIndex: Integer;
  RowIndex: Integer;
  Index: Integer;
begin
  Assert(FileExists(FileName));
  GrdFile := TFileStream.Create(FileName,
    fmOpenRead or fmShareCompat or fmShareDenyWrite);
  try
    GrdFile.Read(Header, SizeOf(Header));
    if Header.ID <> $42525344 then
    begin
      raise EGrdReadError.Create(Format(StrSIsNotASurfer7, [FileName]));
    end;
    GrdFile.Position := GrdFile.Position + Header.Size;
    While GrdFile.Position < GrdFile.Size do
    begin
      GrdFile.Read(Header, SizeOf(Header));
      if Header.ID = $44495247 then
      begin
        // Grid section
        GrdFile.Read(SurferRaster.FHeader, SizeOf(SurferRaster.FHeader));

        // Data section must follow immediately after grid section;
        GrdFile.Read(Header, SizeOf(Header));
        Assert(Header.ID = $41544144);
        SetLength(ZValues, SurferRaster.FHeader.nRow * SurferRaster.FHeader.nCol);
        GrdFile.Read(ZValues[0], Length(ZValues)*sizeof(double));


        SetLength(SurferRaster.FZ, SurferRaster.FHeader.nRow,
          SurferRaster.FHeader.nCol);
        Index := 0;
        for RowIndex := 0 to SurferRaster.FHeader.nRow - 1 do
        begin
          for ColIndex := 0 to SurferRaster.FHeader.nCol - 1 do
          begin
            SurferRaster.FZ[RowIndex, ColIndex] := ZValues[Index];
            Inc(Index);
          end;
        end;
        Exit;
      end
      else if Header.ID = $41544144 then
      begin
        // data section
        Assert(False);
      end
      else if Header.ID = $49544c46 then
      begin
        // Fault Info section
        GrdFile.Position := GrdFile.Position + Header.Size;
      end
      else
      begin
        Assert(False);
      end;
    end;
  finally
    GrdFile.Free;
  end;
  Assert(False);
end;

{ TSurferRaster6 }

function TSurferRaster6.GetIgnore(XIndex, YIndex: Integer): Boolean;
const
  BlankValue = 1.70141e+38;
  Epsilon = 1e-8;
begin
  result := Abs(Z[XIndex, YIndex] - BlankValue)/BlankValue < Epsilon;
end;

function TSurferRaster6.GetLowerLeft: TPoint2D;
begin
  result.x := FHeader.Xlo - FDeltaX/2;
  result.y := FHeader.Ylo - FDeltaY/2;
end;

function TSurferRaster6.GetXCount: integer;
begin
  result := FHeader.nx;
end;

function TSurferRaster6.GetXSpacing: Double;
begin
  Result := FDeltaX;
end;

function TSurferRaster6.GetYCount: integer;
begin
  result := FHeader.ny;
end;

function TSurferRaster6.GetYSpacing: Double;
begin
  Result := FDeltaY;
end;

function TSurferRaster6.GetZ(XIndex, YIndex: Integer): Double;
begin
  result := FZ[YIndex, XIndex];
end;

procedure TSurferRaster6.SetCellSizes;
begin
  FDeltaX := (FHeader.Xhi - FHeader.Xlo)/(FHeader.nx-1);
  FDeltaY := (FHeader.Yhi - FHeader.Ylo)/(FHeader.nY-1);
end;

{ TSurferRaster7 }

function TSurferRaster7.GetIgnore(XIndex, YIndex: Integer): Boolean;
begin
  result := Z[XIndex, YIndex] = FHeader.BlankValue;
end;

function TSurferRaster7.GetLowerLeft: TPoint2D;
begin
  result.x := FHeader.xLL - FHeader.xSize/2;
  result.y := FHeader.yLL - FHeader.ySize/2;
end;

function TSurferRaster7.GetXCount: integer;
begin
  result := FHeader.nCol;
end;

function TSurferRaster7.GetXSpacing: Double;
begin
  result := FHeader.xSize;
end;

function TSurferRaster7.GetYCount: integer;
begin
  result := FHeader.nRow;
end;

function TSurferRaster7.GetYSpacing: Double;
begin
  result := FHeader.ySize;
end;

function TSurferRaster7.GetZ(XIndex, YIndex: Integer): Double;
begin
  result := FZ[YIndex,XIndex];
end;

end.
