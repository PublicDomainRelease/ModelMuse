unit SurferGridFileReaderUnit;

// See http://www.geospatialdesigns.com/surfer6_format.htm,
// http://www.geospatialdesigns.com/surfer7_format.htm,
// and http://www.goldensoftware.com/activekb/questions/368/How+do+I+convert+an+ESRI+ASCII+grid+file+to+a+Surfer+GRD+file%3F
// for the format of Surfer .grd files.

interface

uses SysUtils, Classes;

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


function SurferFileType(const FileName: string): TSurferFileType;
procedure ReadSurfer6GrdFile(FileName: string; out SurferGrid: TSurfer6Grid);
procedure ReadSurfer7GrdFile(FileName: string; out Surfer7Grid: TSurfer7Grid);
procedure ReadSurferAsciiFile(FileName: string; out SurferGrid: TSurfer6Grid);

implementation

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
      raise EGrdReadError.Create(FileName + ' is not a Surfer grid file.');
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
      raise EGrdReadError.Create(FileName + ' is not a Surfer 6 grid file.');
    end;
    if (SurferGrid.Header.nx <= 1) or (SurferGrid.Header.ny <= 1) then
    begin
      raise EGrdReadError.Create('Error reading ' + FileName + '.');
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
      raise EGrdReadError.Create(FileName + ' is not a Surfer ASCII grid file.');
    end;
    if (SurferGrid.Header.nx <= 1) or (SurferGrid.Header.ny <= 1) then
    begin
      raise EGrdReadError.Create('Error reading ' + FileName + '.');
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
      raise EGrdReadError.Create(FileName + ' is not a Surfer 7 grid file.');
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

end.
