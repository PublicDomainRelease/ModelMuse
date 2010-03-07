// This file is part of the source code of ModelMuse by
// @author(Richard B. Winston <rbwinst@usgs.gov>).
unit ReadModflowArrayUnit;

interface

uses SysUtils, Classes;

type
  TModflowFloat = single;
  TModflowDouble = double;
  TModflowDesc = Array[0..15] of Char;
  TModflowDesc2 = Array[0..17] of Char;
  TModflowSingleArray = array of array of TModflowFloat;
  TModflowDoubleArray = array of array of TModflowDouble;
  T3DTModflowArray = array of TModflowDoubleArray;

  TModflowPrecision = (mpSingle, mpDouble);

  TFileVariable = class(TObject)
    AFile: TextFile;
  end;

// AFile needs to be open before being passed to this procedure.
// The other variables will be returned from the procedure.
// Use CheckArrayPrecision to determine the precision before calling
// this procedure.
procedure ReadSinglePrecisionModflowBinaryRealArray(AFile: TFileStream;
  var KSTP, KPER: Integer; var PERTIM, TOTIM: TModflowDouble;
  var DESC: TModflowDesc; var NCOL, NROW, ILAY: Integer;
  var AnArray: TModflowDoubleArray);

// AFile needs to be open before being passed to this procedure.
// The other variables will be returned from the procedure.
// Use CheckArrayPrecision to determine the precision before calling
// this procedure.
procedure ReadDoublePrecisionModflowBinaryRealArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, ILAY: Integer; var AnArray: TModflowDoubleArray);

// F.AFile needs to be open before being passed to this procedure.
// The other variables will be returned from the procedure.
procedure ReadModflowAsciiRealArray(F: TFileVariable;
  var KSTP, KPER: Integer; var PERTIM, TOTIM: TModflowDouble;
  var DESC: TModflowDesc2; var NCOL, NROW, ILAY: Integer;
  var AnArray: TModflowDoubleArray);

// AFile needs to be open before being passed to this procedure.
// The other variables will be returned from the procedure.
// Use CheckBudgetPrecision to determine the precision before calling
// this procedure.
procedure ReadModflowSinglePrecFluxArray(AFile: TFileStream;
  var KSTP, KPER: Integer; var PERTIM, TOTIM: TModflowDouble;
  var DESC: TModflowDesc; var NCOL, NROW, NLAY: Integer;
  var AnArray: T3DTModflowArray;
  HufFormat: boolean
  );

// AFile needs to be open before being passed to this procedure.
// The other variables will be returned from the procedure.
// Use CheckBudgetPrecision to determine the precision before calling
// this procedure.
procedure ReadModflowDoublePrecFluxArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, NLAY: Integer; var AnArray: T3DTModflowArray;
  HufFormat: boolean
  );

// Use this function to check the precision of
// a binary head or drawdown file or other similar files.
// AFile should be at the beginning of the file when this
// function is called.  It will still be at the beginning
// when the function returns.
function CheckArrayPrecision(AFile: TFileStream): TModflowPrecision;

// Use this function to check the precision of a
// cell by cell flow file. AFile should be at the beginning of
// the file when this function is called.  It will still
// be at the beginning when the function returns.
function CheckBudgetPrecision(AFile: TFileStream; out HufFormat: boolean): TModflowPrecision;

implementation

function CheckArrayPrecision(AFile: TFileStream): TModflowPrecision;
var
  KSTP, KPER: Integer;
  PERTIM, TOTIM: TModflowFloat;
  DESC: TModflowDesc;
  Description : string;
  PERTIM_Double: TModflowDouble;
  TOTIM_Double: TModflowDouble;
  function ValidDescription: boolean;
  begin
    result := (Description = '            HEAD')
      or (Description = '        DRAWDOWN')
      or (Description = '      SUBSIDENCE')
      or (Description = '      COMPACTION')
      or (Description = '   CRITICAL HEAD')
      or (Description = '     HEAD IN HGU')
      or (Description = '      SUBSIDENCE')
      or (Description = 'NDSYS COMPACTION')
      or (Description = '  Z DISPLACEMENT')
      or (Description = ' D CRITICAL HEAD')
      or (Description = 'LAYER COMPACTION')
      or (Description = ' DSYS COMPACTION')
      or (Description = 'ND CRITICAL HEAD')
      or (Description = 'LAYER COMPACTION')
      or (Description = 'SYSTM COMPACTION')
      or (Description = 'PRECONSOL STRESS')
      or (Description = 'CHANGE IN PCSTRS')
      or (Description = 'EFFECTIVE STRESS')
      or (Description = 'CHANGE IN EFF-ST')
      or (Description = '      VOID RATIO')
      or (Description = '       THICKNESS')
      or (Description = 'CENTER ELEVATION');
  end;
begin
  Assert(AFile.Position = 0);
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(PERTIM, SizeOf(PERTIM));
  AFile.Read(TOTIM, SizeOf(TOTIM));
  AFile.Read(DESC, SizeOf(DESC));
  Description := DESC;
  if ValidDescription then
  begin
    result := mpSingle;
  end
  else
  begin
    result := mpDouble;
  end;
  AFile.Position := 0;
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(PERTIM_Double, SizeOf(PERTIM_Double));
  AFile.Read(TOTIM_Double, SizeOf(TOTIM_Double));
  AFile.Read(DESC, SizeOf(DESC));
  Description := DESC;
  case result of
    mpSingle: Assert(not ValidDescription);
    mpDouble: Assert(ValidDescription);
  end;
  AFile.Position := 0;
end;

function CheckBudgetPrecision(AFile: TFileStream; out HufFormat: boolean): TModflowPrecision;
var
  Description: string;
  FirstDescription: string;
  SecondDescription: string;
  procedure ReadDescription(var KPER: Integer; var KSTP: Integer);
  var
    DESC: TModflowDesc;
  begin
    AFile.Read(KSTP, SizeOf(KSTP));
    AFile.Read(KPER, SizeOf(KPER));
    AFile.Read(DESC, SizeOf(DESC));
    Description := DESC;
  end;
  function ReadSingleArray: boolean;
  var
    KSTP, KPER: Integer;
    NCOL, NROW, NLAY: Integer;
    AnArray: T3DTModflowArray;
    RowIndex: Integer;
    LayerIndex: Integer;
    ColIndex: Integer;
    PERTIM, TOTIM: TModflowFloat;
    ITYPE: integer;
    DELT: TModflowFloat;
    NVAL: Integer;
    Index: Integer;
    NLIST: Integer;
    NRC: Integer;
    Values: array of TModflowFloat;
    ICELL: Integer;
    ValIndex: Integer;
    AValue: TModflowFloat;
  begin
    result := True;
    try
      ReadDescription(KPER, KSTP);
      AFile.Read(NCOL, SizeOf(NCOL));
      AFile.Read(NROW, SizeOf(NROW));
      AFile.Read(NLAY, SizeOf(NLAY));
      if (KSTP < 1) or (KPER < 1) or (NCOL < 1) or (NROW < 1) then
      begin
        result := False;
        Exit;
      end;
      try
        SetLength(AnArray, Abs(NLAY), NROW, NCOL);
      except on E: ERangeError do
        begin
          result := False;
          Exit;
        end;
      end;
      for LayerIndex := 0 to Abs(NLAY) - 1 do
      begin
        for RowIndex := 0 to NROW - 1 do
        begin
          for ColIndex := 0 to NCOL - 1 do
          begin
            AnArray[LayerIndex, RowIndex, ColIndex] := 0;
          end;
        end;
      end;
      PERTIM := -1;
      TOTIM := -1;
      NVAL := -1;

      ITYPE := 0;
      if (NLAY < 0) or HufFormat then
      begin
        AFile.Read(ITYPE, SizeOf(ITYPE));
        AFile.Read(DELT, SizeOf(DELT));
        AFile.Read(PERTIM, SizeOf(PERTIM));
        AFile.Read(TOTIM, SizeOf(TOTIM));
        if (DELT < 0) or (PERTIM < 0) or (TOTIM < 0) then
        begin
          result := False;
          Exit;
        end;
        NVAL := 1;
    //C  Read data depending on ICODE.  ICODE 0,1, or 2 are the only allowed
    //C  values because the first budget terms must be from the internal
    //C  flow package (BCF,LPF, or HUF).
        if (ITYPE = 2) then
        begin
          AFile.Read(NLIST, SizeOf(NLIST));
          if NLIST < 0 then
          begin
            result := False;
            Exit;
          end;
        end;
      end;
      case ITYPE of
        0,1: // full 3D array
          begin
            for LayerIndex := 0 to Abs(NLAY) - 1 do
            begin
              for RowIndex := 0 to NROW - 1 do
              begin
                for ColIndex := 0 to NCOL - 1 do
                begin
                  AFile.Read(AValue, SizeOf(AValue));
                  AnArray[LayerIndex, RowIndex, ColIndex] := AValue;
                end;
              end;
            end;
          end;
        2:
          begin
            if NLIST > 0 then
            begin
              NRC := NROW*NCOL;
              SetLength(Values, NVAL);
              for Index := 0 to NLIST - 1 do
              begin
                AFile.Read(ICELL, SizeOf(ICELL));
                for ValIndex := 0 to NVAL - 1 do
                begin
                  AFile.Read(AValue, SizeOf(AValue));
                  Values[ValIndex] := AValue;
                end;
                LayerIndex :=  (ICELL-1) div NRC;
                RowIndex := ( (ICELL - LayerIndex*NRC)-1 ) div NCOL;
                ColIndex := ICELL - (LayerIndex)*NRC - (RowIndex)*NCOL-1;
                if ((ColIndex >= 0) AND (RowIndex >= 0) AND (LayerIndex >= 0)
                  AND (ColIndex < ncol) AND (RowIndex < NROW)
                  AND (LayerIndex < Abs(NLAY))) then
                begin
                  AnArray[LayerIndex, RowIndex, ColIndex] :=
                    AnArray[LayerIndex, RowIndex, ColIndex] + Values[0];
                end
                else
                begin
                  result := False;
                  Exit;
                end;
              end;
            end;
          end;
        else Assert(False);
      end;
    except
      result := false;
    end;
  end;
  function ReadDoubleArray: boolean;
  var
    KSTP, KPER: Integer;
    NCOL, NROW, NLAY: Integer;
    AnArray: T3DTModflowArray;
    RowIndex: Integer;
    LayerIndex: Integer;
    ColIndex: Integer;
    PERTIM, TOTIM: TModflowDouble;
    ITYPE: integer;
    DELT: TModflowDouble;
    NVAL: Integer;
    Index: Integer;
    NLIST: Integer;
    NRC: Integer;
    Values: array of TModflowDouble;
    ICELL: Integer;
    ValIndex: Integer;
//    FluxArray: TModflowSingleArray;
//    LayerIndicatorArray: array of array of integer;
    AValue: TModflowDouble;
  begin
    result := True;
    try
      ReadDescription(KPER, KSTP);
      AFile.Read(NCOL, SizeOf(NCOL));
      AFile.Read(NROW, SizeOf(NROW));
      AFile.Read(NLAY, SizeOf(NLAY));
      if (KSTP < 1) or (KPER < 1) or (NCOL < 1) or (NROW < 1) then
      begin
        result := False;
        Exit;
      end;
      try
        SetLength(AnArray, Abs(NLAY), NROW, NCOL);
      except on E: ERangeError do
        begin
          result := False;
          Exit;
        end;
      end;
      for LayerIndex := 0 to Abs(NLAY) - 1 do
      begin
        for RowIndex := 0 to NROW - 1 do
        begin
          for ColIndex := 0 to NCOL - 1 do
          begin
            AnArray[LayerIndex, RowIndex, ColIndex] := 0;
          end;
        end;
      end;
      PERTIM := -1;
      TOTIM := -1;
      NVAL := -1;

      ITYPE := 0;
      if (NLAY < 0) or HufFormat then
      begin
        AFile.Read(ITYPE, SizeOf(ITYPE));
        AFile.Read(DELT, SizeOf(DELT));
        AFile.Read(PERTIM, SizeOf(PERTIM));
        AFile.Read(TOTIM, SizeOf(TOTIM));
        if (DELT < 0) or (PERTIM < 0) or (TOTIM < 0) then
        begin
          result := False;
          Exit;
        end;
        NVAL := 1;
    //C  Read data depending on ICODE.  ICODE 0,1, or 2 are the only allowed
    //C  values because the first budget terms must be from the internal
    //C  flow package (BCF,LPF, or HUF).
        if (ITYPE = 2) then
        begin
          AFile.Read(NLIST, SizeOf(NLIST));
          if NLIST < 0 then
          begin
            result := False;
            Exit;
          end;
        end;
      end;
      case ITYPE of
        0,1: // full 3D array
          begin
            for LayerIndex := 0 to Abs(NLAY) - 1 do
            begin
              for RowIndex := 0 to NROW - 1 do
              begin
                for ColIndex := 0 to NCOL - 1 do
                begin
                  AFile.Read(AValue, SizeOf(AValue));
                  AnArray[LayerIndex, RowIndex, ColIndex] := AValue;
                end;
              end;
            end;
          end;
        2:
          begin
            if NLIST > 0 then
            begin
              NRC := NROW*NCOL;
              SetLength(Values, NVAL);
              for Index := 0 to NLIST - 1 do
              begin
                AFile.Read(ICELL, SizeOf(ICELL));
                for ValIndex := 0 to NVAL - 1 do
                begin
                  AFile.Read(AValue, SizeOf(AValue));
                  Values[ValIndex] := AValue;
                end;
                LayerIndex :=  (ICELL-1) div NRC;
                RowIndex := ( (ICELL - LayerIndex*NRC)-1 ) div NCOL;
                ColIndex := ICELL - (LayerIndex)*NRC - (RowIndex)*NCOL-1;
                if ((ColIndex >= 0) AND (RowIndex >= 0) AND (LayerIndex >= 0)
                  AND (ColIndex < ncol) AND (RowIndex < NROW)
                  AND (LayerIndex < Abs(NLAY))) then
                begin
                  AnArray[LayerIndex, RowIndex, ColIndex] :=
                    AnArray[LayerIndex, RowIndex, ColIndex] + Values[0];
                end
                else
                begin
                  result := False;
                  Exit;
                end;
              end;
            end
            else
            begin
              result := False;
              Exit;
            end;
          end;
        else Assert(False);
      end;
    except
      result := False;
    end;
  end;
begin
  HufFormat := False;
  result := mpSingle;
  Assert(AFile.Position = 0);
  if ReadDoubleArray then
  begin
    FirstDescription := Description;
    if (AFile.Position < AFile.Size) and ReadDoubleArray then
    begin
      SecondDescription := Description;
      if (FirstDescription = '         STORAGE')
        and (SecondDescription = '   CONSTANT HEAD') then
      begin
        result := mpDouble;
      end
      else if (FirstDescription = '   CONSTANT HEAD')
        and (SecondDescription = 'FLOW RIGHT FACE ') then
      begin
        result := mpDouble;
      end
      else if (FirstDescription = 'FLOW RIGHT FACE ')
        and (SecondDescription = 'FLOW FRONT FACE ') then
      begin
        Assert(result = mpDouble);
      end
      else if (FirstDescription = 'FLOW RIGHT FACE ')
        and (SecondDescription = 'FLOW LOWER FACE ') then
      begin
        Assert(result = mpDouble);
      end
      else if (FirstDescription = 'FLOW FRONT FACE ')
        and (SecondDescription = 'FLOW LOWER FACE ') then
      begin
        Assert(result = mpDouble);
      end
      else
      begin
        result := mpSingle;
      end;
    end;
  end;
  AFile.Position := 0;
  if ReadSingleArray then
  begin
    FirstDescription := Description;
    if (AFile.Position < AFile.Size) and ReadSingleArray then
    begin
      SecondDescription := Description;
      if (FirstDescription = '         STORAGE')
        and (SecondDescription = '   CONSTANT HEAD') then
      begin
        Assert(result = mpSingle);
      end
      else if (FirstDescription = '   CONSTANT HEAD')
        and (SecondDescription = 'FLOW RIGHT FACE ') then
      begin
        Assert(result = mpSingle);
      end
      else if (FirstDescription = 'FLOW RIGHT FACE ')
        and (SecondDescription = 'FLOW FRONT FACE ') then
      begin
        Assert(result = mpSingle);
      end
      else if (FirstDescription = 'FLOW RIGHT FACE ')
        and (SecondDescription = 'FLOW LOWER FACE ') then
      begin
        Assert(result = mpSingle);
      end
      else if (FirstDescription = 'FLOW FRONT FACE ')
        and (SecondDescription = 'FLOW LOWER FACE ') then
      begin
        Assert(result = mpSingle);
      end
      else
      begin
        Assert(result = mpDouble);
      end;
    end
    else
    begin
      if (result <> mpDouble) then
      begin
        AFile.Position := 0;
        HufFormat := True;
        result := mpSingle;
        if ReadDoubleArray then
        begin
          FirstDescription := Description;
          if (AFile.Position < AFile.Size) and ReadDoubleArray then
          begin
            SecondDescription := Description;
            if (FirstDescription = '         STORAGE')
              and (SecondDescription = '   CONSTANT HEAD') then
            begin
              result := mpDouble;
            end
            else if (FirstDescription = '   CONSTANT HEAD')
              and (SecondDescription = 'FLOW RIGHT FACE ') then
            begin
              Assert(result = mpDouble);
            end
            else if (FirstDescription = 'FLOW RIGHT FACE ')
              and (SecondDescription = 'FLOW FRONT FACE ') then
            begin
              Assert(result = mpDouble);
            end
            else if (FirstDescription = 'FLOW RIGHT FACE ')
              and (SecondDescription = 'FLOW LOWER FACE ') then
            begin
              Assert(result = mpDouble);
            end
            else if (FirstDescription = 'FLOW FRONT FACE ')
              and (SecondDescription = 'FLOW LOWER FACE ') then
            begin
              Assert(result = mpDouble);
            end
            else
            begin
              result := mpSingle;
            end;
          end;
        end;
        AFile.Position := 0;
        if ReadSingleArray then
        begin
          FirstDescription := Description;
          if (AFile.Position < AFile.Size) and ReadSingleArray then
          begin
            SecondDescription := Description;
            if (FirstDescription = '         STORAGE')
              and (SecondDescription = '   CONSTANT HEAD') then
            begin
              Assert(result = mpSingle);
            end
            else if (FirstDescription = '   CONSTANT HEAD')
              and (SecondDescription = 'FLOW RIGHT FACE ') then
            begin
              Assert(result = mpSingle);
            end
            else if (FirstDescription = 'FLOW RIGHT FACE ')
              and (SecondDescription = 'FLOW FRONT FACE ') then
            begin
              Assert(result = mpSingle);
            end
            else if (FirstDescription = 'FLOW RIGHT FACE ')
              and (SecondDescription = 'FLOW LOWER FACE ') then
            begin
              Assert(result = mpSingle);
            end
            else if (FirstDescription = 'FLOW FRONT FACE ')
              and (SecondDescription = 'FLOW LOWER FACE ') then
            begin
              Assert(result = mpSingle);
            end
            else
            begin
              Assert(result = mpDouble);
            end;
          end
          else
          begin
            Assert(result = mpDouble);
          end;
        end
        else
        begin
          Assert(result = mpDouble);
        end;
      end;
    end;
  end
  else
  begin
    Assert(result = mpDouble);
  end;
  AFile.Position := 0;
end;


procedure ReadSinglePrecisionModflowBinaryRealArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, ILAY: Integer; var AnArray: TModflowDoubleArray);
var
  ColIndex: Integer;
  RowIndex: Integer;
  AValue: TModflowFloat;
begin
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));

  AFile.Read(AValue, SizeOf(TModflowFloat));
  PERTIM := AValue;
  AFile.Read(AValue, SizeOf(TModflowFloat));
  TOTIM := AValue;
  AFile.Read(DESC, SizeOf(DESC));
  AFile.Read(NCOL, SizeOf(NCOL));
  AFile.Read(NROW, SizeOf(NROW));
  AFile.Read(ILAY, SizeOf(ILAY));
  SetLength(AnArray, NROW, NCOL);
  for RowIndex := 0 to NROW - 1 do
  begin
    for ColIndex := 0 to NCOL - 1 do
    begin
      AFile.Read(AValue, SizeOf(TModflowFloat));
      AnArray[RowIndex, ColIndex] := AValue;
    end;
  end;
end;

procedure ReadDoublePrecisionModflowBinaryRealArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, ILAY: Integer; var AnArray: TModflowDoubleArray);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(PERTIM, SizeOf(TModflowDouble));
  AFile.Read(TOTIM, SizeOf(TModflowDouble));
  AFile.Read(DESC, SizeOf(DESC));
  AFile.Read(NCOL, SizeOf(NCOL));
  AFile.Read(NROW, SizeOf(NROW));
  AFile.Read(ILAY, SizeOf(ILAY));
  SetLength(AnArray, NROW, NCOL);
  for RowIndex := 0 to NROW - 1 do
  begin
    for ColIndex := 0 to NCOL - 1 do
    begin
      AFile.Read(AnArray[RowIndex, ColIndex], SizeOf(TModflowDouble));
    end;
  end;
end;

procedure ReadModflowSinglePrecFluxArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, NLAY: Integer; var AnArray: T3DTModflowArray;
  HufFormat: boolean);
var
  ITYPE: integer;
  DELT: TModflowFloat;
  NVAL: Integer;
  Index: Integer;
  CTMP: TModflowDesc;
  NLIST: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  ColIndex: Integer;
  FluxArray: TModflowSingleArray;
  LayerIndicatorArray: array of array of integer;
  Values: array of TModflowFloat;
  ICELL: Integer;
  ValIndex: Integer;
  NRC: Integer;
  AValue: TModflowFloat;
begin
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(DESC, SizeOf(DESC));
  AFile.Read(NCOL, SizeOf(NCOL));
  AFile.Read(NROW, SizeOf(NROW));
  AFile.Read(NLAY, SizeOf(NLAY));
  SetLength(AnArray, Abs(NLAY), NROW, NCOL);
  for LayerIndex := 0 to Abs(NLAY) - 1 do
  begin
    for RowIndex := 0 to NROW - 1 do
    begin
      for ColIndex := 0 to NCOL - 1 do
      begin
        AnArray[LayerIndex, RowIndex, ColIndex] := 0;
      end;
    end;
  end;
  PERTIM := -1;
  TOTIM := -1;

  ITYPE := 0;
  if (NLAY < 0) or HufFormat then
  begin
    AFile.Read(ITYPE, SizeOf(ITYPE));
    AFile.Read(DELT, SizeOf(DELT));
    AFile.Read(AValue, SizeOf(AValue));
    PERTIM := AValue;
    AFile.Read(AValue, SizeOf(AValue));
    TOTIM := AValue;
    NVAL := 1;
    if ITYPE = 5 then
    begin
      AFile.Read(NVAL, SizeOf(NVAL));
      if NVAL > 1 then
      begin
        for Index := 2 to NVAL do
        begin
          AFile.Read(CTMP, SizeOf(CTMP));
        end;
      end;
    end;
    if (ITYPE = 2) or (ITYPE = 5) then
    begin
      AFile.Read(NLIST, SizeOf(NLIST));
    end;
  end;
  case ITYPE of
    0,1: // full 3D array
      begin
        for LayerIndex := 0 to Abs(NLAY) - 1 do
        begin
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AFile.Read(AValue, SizeOf(TModflowFloat));
              AnArray[LayerIndex, RowIndex, ColIndex] := AValue;
            end;
          end;
        end;
      end;
    2,5:
      begin
        if NLIST > 0 then
        begin
          NRC := NROW*NCOL;
          SetLength(Values, NVAL);
          for Index := 0 to NLIST - 1 do
          begin
            AFile.Read(ICELL, SizeOf(ICELL));
            for ValIndex := 0 to NVAL - 1 do
            begin
              AFile.Read(Values[ValIndex], SizeOf(TModflowFloat));
            end;
            LayerIndex :=  (ICELL-1) div NRC;
            RowIndex := ( (ICELL - LayerIndex*NRC)-1 ) div NCOL;
            ColIndex := ICELL - (LayerIndex)*NRC - (RowIndex)*NCOL-1;
            if ((ColIndex >= 0) AND (RowIndex >= 0) AND (LayerIndex >= 0)
              AND (ColIndex < ncol) AND (RowIndex < NROW)
              AND (LayerIndex < Abs(NLAY))) then
            begin
              AnArray[LayerIndex, RowIndex, ColIndex] :=
                AnArray[LayerIndex, RowIndex, ColIndex] + Values[0];
            end;
          end;
        end;
      end;
    3: // 1 layer array with layer indicator array
      begin
        SetLength(FluxArray, NROW, NCOL);
        SetLength(LayerIndicatorArray, NROW, NCOL);
        for RowIndex := 0 to NROW - 1 do
        begin
          for ColIndex := 0 to NCOL - 1 do
          begin
            AFile.Read(LayerIndicatorArray[RowIndex, ColIndex],
              SizeOf(integer));
          end;
        end;
        for RowIndex := 0 to NROW - 1 do
        begin
          for ColIndex := 0 to NCOL - 1 do
          begin
            AFile.Read(FluxArray[RowIndex, ColIndex],
              SizeOf(TModflowFloat));
          end;
        end;
        for RowIndex := 0 to NROW - 1 do
        begin
          for ColIndex := 0 to NCOL - 1 do
          begin
            AnArray[LayerIndicatorArray[RowIndex, ColIndex]-1,
              RowIndex, ColIndex] := FluxArray[RowIndex, ColIndex];
          end;
        end;
      end;
    4: // 1-layer array that defines layer 1.
      begin
        for LayerIndex := 1 to Abs(NLAY) - 1 do
        begin
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AnArray[LayerIndex, RowIndex, ColIndex] := 0;
            end;
          end;
        end;
        for RowIndex := 0 to NROW - 1 do
        begin
          for ColIndex := 0 to NCOL - 1 do
          begin
            AFile.Read(AValue, SizeOf(TModflowFloat));
            AnArray[0, RowIndex, ColIndex] := AValue;
          end;
        end;
      end;
    else Assert(False);
  end;
end;

procedure ReadModflowDoublePrecFluxArray(AFile: TFileStream;
  var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc;
  var NCOL, NROW, NLAY: Integer; var AnArray: T3DTModflowArray;
  HufFormat: boolean);
var
  ITYPE: integer;
  DELT: TModflowDouble;
  NVAL: Integer;
  Index: Integer;
  CTMP: TModflowDesc;
  NLIST: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  ColIndex: Integer;
  FluxArray: TModflowDoubleArray;
  LayerIndicatorArray: array of array of integer;
  Values: array of TModflowDouble;
  ICELL: Integer;
  ValIndex: Integer;
  NRC: Integer;
  AValue: TModflowDouble;
begin
  AFile.Read(KSTP, SizeOf(KSTP));
  AFile.Read(KPER, SizeOf(KPER));
  AFile.Read(DESC, SizeOf(DESC));
  AFile.Read(NCOL, SizeOf(NCOL));
  AFile.Read(NROW, SizeOf(NROW));
  AFile.Read(NLAY, SizeOf(NLAY));
  SetLength(AnArray, Abs(NLAY), NROW, NCOL);
  for LayerIndex := 0 to Abs(NLAY) - 1 do
  begin
    for RowIndex := 0 to NROW - 1 do
    begin
      for ColIndex := 0 to NCOL - 1 do
      begin
        AnArray[LayerIndex, RowIndex, ColIndex] := 0;
      end;
    end;
  end;
  PERTIM := -1;
  TOTIM := -1;

  ITYPE := 0;
  if (NLAY < 0) or HufFormat then
  begin
    AFile.Read(ITYPE, SizeOf(ITYPE));
    AFile.Read(DELT, SizeOf(DELT));
    AFile.Read(AValue, SizeOf(AValue));
    PERTIM := AValue;
    AFile.Read(AValue, SizeOf(AValue));
    TOTIM := AValue;
    NVAL := 1;
    if ITYPE = 5 then
    begin
      AFile.Read(NVAL, SizeOf(NVAL));
      if NVAL > 1 then
      begin
        for Index := 2 to NVAL do
        begin
          AFile.Read(CTMP, SizeOf(CTMP));
        end;
      end;
    end;
    if (ITYPE = 2) or (ITYPE = 5) then
    begin
      AFile.Read(NLIST, SizeOf(NLIST));
    end;
  end;
  case ITYPE of
    0,1: // full 3D array
      begin
        for LayerIndex := 0 to Abs(NLAY) - 1 do
        begin
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AFile.Read(AValue, SizeOf(TModflowDouble));
              AnArray[LayerIndex, RowIndex, ColIndex] := AValue;
            end;
          end;
        end;
      end;
    2,5:
      begin
        if NLIST > 0 then
        begin
          NRC := NROW*NCOL;
          SetLength(Values, NVAL);
          for Index := 0 to NLIST - 1 do
          begin
            AFile.Read(ICELL, SizeOf(ICELL));
            for ValIndex := 0 to NVAL - 1 do
            begin
              AFile.Read(Values[ValIndex], SizeOf(TModflowDouble));
            end;
            LayerIndex :=  (ICELL-1) div NRC;
            RowIndex := ( (ICELL - LayerIndex*NRC)-1 ) div NCOL;
            ColIndex := ICELL - (LayerIndex)*NRC - (RowIndex)*NCOL-1;
            if ((ColIndex >= 0) AND (RowIndex >= 0) AND (LayerIndex >= 0)
              AND (ColIndex < ncol) AND (RowIndex < NROW)
              AND (LayerIndex < Abs(NLAY))) then
            begin
              AnArray[LayerIndex, RowIndex, ColIndex] :=
                AnArray[LayerIndex, RowIndex, ColIndex] + Values[0];
            end;
          end;
        end;
      end;
    3: // 1 layer array with layer indicator array
      begin
        SetLength(FluxArray, NROW, NCOL);
        SetLength(LayerIndicatorArray, NROW, NCOL);
        for RowIndex := 0 to NROW - 1 do
        begin
          for ColIndex := 0 to NCOL - 1 do
          begin
            AFile.Read(LayerIndicatorArray[RowIndex, ColIndex],
              SizeOf(integer));
          end;
        end;
        for RowIndex := 0 to NROW - 1 do
        begin
          for ColIndex := 0 to NCOL - 1 do
          begin
            AFile.Read(FluxArray[RowIndex, ColIndex],
              SizeOf(TModflowDouble));
          end;
        end;
        for RowIndex := 0 to NROW - 1 do
        begin
          for ColIndex := 0 to NCOL - 1 do
          begin
            AnArray[LayerIndicatorArray[RowIndex, ColIndex]-1,
              RowIndex, ColIndex] := FluxArray[RowIndex, ColIndex];
          end;
        end;
      end;
    4: // 1-layer array that defines layer 1.
      begin
        for LayerIndex := 1 to Abs(NLAY) - 1 do
        begin
          for RowIndex := 0 to NROW - 1 do
          begin
            for ColIndex := 0 to NCOL - 1 do
            begin
              AnArray[LayerIndex, RowIndex, ColIndex] := 0;
            end;
          end;
        end;
        for RowIndex := 0 to NROW - 1 do
        begin
          for ColIndex := 0 to NCOL - 1 do
          begin
            AFile.Read(AValue, SizeOf(TModflowDouble));
            AnArray[0, RowIndex, ColIndex] := AValue;
          end;
        end;
      end;
    else Assert(False);
  end;
end;

procedure ReadModflowAsciiRealArray(F: TFileVariable; var KSTP, KPER: Integer;
  var PERTIM, TOTIM: TModflowDouble; var DESC: TModflowDesc2;
  var NCOL, NROW, ILAY: Integer; var AnArray: TModflowDoubleArray);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  Read(F.AFile, KSTP);
  Read(F.AFile, KPER);
  Read(F.AFile, PERTIM);
  Read(F.AFile, TOTIM);
  Read(F.AFile, DESC);
  Read(F.AFile, NCOL);
  Read(F.AFile, NROW);
  Read(F.AFile, ILAY);
  ReadLn(F.AFile);
  SetLength(AnArray, NROW, NCOL);
  for RowIndex := 0 to NROW - 1 do
  begin
    for ColIndex := 0 to NCOL - 1 do
    begin
      Read(F.AFile, AnArray[RowIndex, ColIndex]);
    end;
  end;
  ReadLn(F.AFile);
end;

end.
