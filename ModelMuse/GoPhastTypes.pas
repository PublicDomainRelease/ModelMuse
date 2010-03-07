{@abstract(@name is used to define types, constants, and small classes used
  in a variety of places in GoPhast.)}
unit GoPhastTypes;

interface

uses
  GR32, // defines TColor32.
  SysUtils, Types, Classes, FastGEO;

type
  PReal = ^Real;

  {@abstract(@name is a one-dimensional array of doubles.)
  }
  TOneDRealArray = array of Double;

  {@abstract(@name is a one-dimensional array of integers.)
  }
  TOneDIntegerArray = array of integer;

  {@abstract(@name is a two-dimensional array of doubles.)
  }
  TTwoDRealArray = array of TOneDRealArray;

  {@abstract(@name is a three-dimensional array of doubles.)
  }
  TThreeDRealArray = array of TTwoDRealArray;

  {@abstract(@name is a pointer to a TPoint2D.)
  }
  P2DRealPoint = TPoint2DPtr;

  {@abstract(@name is a one-dimensional array of T2DRealPoints.)
  }
  TRealPointArray = TPolygon2D;//array of TPoint2D;

  {@abstract(@name is a two-dimensional array of T2DRealPoints.)
  }
  T2DRealPointArray = array of TRealPointArray;

  {@abstract(@name is a two-dimensional array of booleans.)
  }
  T2DBoolArray = array of array of boolean;

  {@abstract(@name represents a 3D point with real-number coordinates.)
  }
  T3DRealPoint = record
    X: real;
    Y: real;
    Z: real;
  end;
  P3DRealPoint = ^T3DRealPoint;

  TRealArray = array of Real;

  // @name is a 1D array of @link(T3DRealPoint)s.
  T3DRealPointArray1 = array of T3DRealPoint;

  // @name is a 2D array of @link(T3DRealPoint)s.
  T3DRealPointArray2 = array of T3DRealPointArray1;

  // @name is a 3D array of @link(T3DRealPoint)s.
  T3DRealPointArray3 = array of array of array of T3DRealPoint;

  // @name is used to indicate which view of the model the cursor
  // is over.
  TCursorGrid = (cgNone, cgTop, cgFront, cgSide);

  // @name is used to describe the direction
  // @link(ScreenObjectUnit.TScreenObject)s are
  // viewed from.
  TViewDirection = (vdTop, vdFront, vdSide);

  // @name is used to specify the columns in the table on
  // @link(frmDataSetsUnits.TfrmDataSets).
  //
  // @value(dcName = name of the @link(DataSetUnit.TDataArray).)
  // @value(dcType = the type of data @link(DataSetUnit.TDataArray.DataType)
  //   (boolean, integer, real number, or string)
  //   stored by the @link(DataSetUnit.TDataArray).)
  // @value(dcOrientation = the @link(DataSetUnit.TDataArray.Orientation) of the
  //   @link(DataSetUnit.TDataArray).)
  // @value(dcEvaluatedAt = the @link(DataSetUnit.TDataArray.EvaluatedAt)
  //    of the @link(DataSetUnit.TDataArray).)
  // @value(dcUnits = the @link(DataSetUnit.TDataArray.Units) of the
  //   @link(DataSetUnit.TDataArray).)
  // @value(dcFormula = the @link(DataSetUnit.TDataArray.Formula) of the
  //    @link(DataSetUnit.TDataArray).)
  // @value(dcInterpolation = the @link(DataSetUnit.TDataArray.TwoDInterpolator)
  //    of the @link(DataSetUnit.TDataArray).)
  TDataColumns = (dcName, dcType, dcOrientation, dcEvaluatedAt,
    dcUnits, dcFormula, dcInterpolation);
  // @name specifies which values in the table on
  // @link(frmDataSetsUnits.TfrmDataSets) the user can edit.
  // only columns not included in @name can be edited.
  TDataLock = set of TDataColumns;

  // @name is used in TDataArray.@link(TDataArray.Orientation) to
  // indicate whether the @link(TDataArray) is a 2D or 3D data set and,
  // if it is 2D, which face of the grid it is associated with.
  //
  // @value(dsoTop 2D top face)
  // @value(dsoFront 2D front face)
  // @value(dsoSide 2D side face)
  // @value(dso3D 3D)
  TDataSetOrientation = (dsoTop, dsoFront, dsoSide, dso3D);

  TDataSetOrientations = set of TDataSetOrientation;

  // @name is used in specifying the number of elevations associated with
  // a @link(TScreenObject).
  TElevationCount = (ecZero, ecOne, ecTwo);

  // @name is used to specify whether a data set is evaluated at
  // element centers or at nodes.
  TEvaluatedAt = (eaBlocks, eaNodes);

  // @name represents the frequencies with which data can be printed
  // in PHAST.
  TFrequencyUnits = (fuDefault, fuSeconds, fuMinutes, fuHours, fuDays,
    fuYears, fuStep, fuEnd);

  // @name represents the time units recognized by PHAST.
  TTimeUnits = (tuSeconds, tuMinutes, tuHours, tuDays, tuYears);

  // @name represents the length units recognized by PHAST.
  TLengthUnits = (luInches, luFeet, luMiles, luMillimeters,
    luCentimeters, luMeters, luKilometers);

  // @name represents the 1/length units recognized by PHAST.
  TInverseLengthUnits = (iluInches, iluFeet, iluMiles, iluMillimeters,
    iluCentimeters, iluMeters, iluKilometers);

  // @name represents the volume units recognized by PHAST.
  TVolumeUnits = (vuGallons, vuInches3, vuFeet3, vuMiles3,
    vuLiters, vuMillimeters3, vuCentimeters3, vuMeters3, vuKilometers3);

  // @name represents the solvers used by PHAST.
  TPhastSolver = (psDirect, psIterative);

  //TInterpolationDirection determines whether "PHAST" style interpolation
  // is used or "PHAST" style mixtures.  If "PHAST" style interpolation
  // is used, it also determines the coordinate direction.
  // @value(pidX = Interpolate in the X direction.)
  // @value(pidY = Interpolate in the Y direction.)
  // @value(pidZ = Interpolate in the Z direction.)
  // @value(pidMix = Use "PHAST" style mixtures.)
  // See @LINK(TPhastInterpolationValues).
  TInterpolationDirection = (pidX, pidY, pidZ, pidMix);

  {@abstract(@name is a pointer to a @link(TInterpolationDirection).)
  @longcode(#
  PInterpolationDirection = ^TInterpolationDirection;
  #)
  }
  PInterpolationDirection = ^TInterpolationDirection;

  // @name represent the items whose print frequencies
  // can be specified in PHAST.
  TPrintFrequencyRows = (pfrName, pfrTime, pfrFlowRate, pfrComponents,
    pfrConductance, pfrFlowBalance, pfrChemPrint, pfrHDFChem, pfrHDFHeads,
    pfrHDFVelocity, pfrHeads, pfrProgress, pfrRestart, pfrVelocities, pfrWells, pfrXYZChem,
    pfrXYZComponents, pfrXYZHeads, pfrXYZVelocities, pfrXYZWells,
    pfrBoundaryConditions, pfrDefault);

  // @name represents the types of boundary
  // @value(btNone = no boundary condition)
  // @value(btSpecifiedHead = Specified head boundary condition)
  // @value(btFlux = Flux boundary condition)
  // @value(btLeaky = Leaky boundary condition)
  // @value(btRiver = River boundary condition)
  // @value(btWell = Well boundary condition)
  TBoundaryTypes = (btNone, btSpecifiedHead, btFlux, btLeaky, btRiver, btWell);
  TModflowBoundaryType = (mbtNone, mbtCHD);

  // @name specifies how elevations are specified in the well boundary
  // condition. See @link(TWellBoundary.WellElevationFormat).
  TWellElevationFormat = (wefElevation, wefDepth);

  // @name is used to indicate what type of model is active.
  // The type of model should never be set to msUndefined.
  TModelSelection = (msUndefined, msPhast, msModflow);

  //  @name is used to indicate how the spacing of layers within a unit
  // is specified.
  // @value(gmUniform) The layers are spaced uniformly.
  // @value(gmUp) The layers increase in thickness upward.
  // @value(gmDown) The layers increase in thickness downward.
  // @value(gmMiddle) The layers increase in thickness toward the middle
  // of the unit from both the top and bottom.
  // @value(gmEdge) The layers increase in thickness toward both the top
  // and bottom of the unit from the middle.
  // @value(gmCustom) The thickness of each layer is specified individually.
  TGrowthMethod = (gmUniform, gmUp, gmDown, gmMiddle, gmEdge, gmCustom);

  // @name indicates the locations at which a @link(TScreenObject) should
  // assign values to cells in a @link(TDataArray)
  // @value(alAll Assign values to all locations.)
  // @value(alFirstVertex Assign values to cells at the location of the first
  //   vertex in the @link(TScreenObject).)
  // @value(alLastVertex Assign values to cells at the location of the last
  //   vertex in the @link(TScreenObject).)
  TAssignmentLocation = (alAll, alFirstVertex, alLastVertex);

  TByteSet = set of byte;

  TIface = (iIndeterminant, iHorizontal, iInternal,
    iLeft, iRight, iFront, iBack, iBottom, iTop);

  TStatFlag = (stVariance, stStandardDev,
    stCoefVar, stWeight, stSquaredWeight);

  TObservationPurpose = (ofObserved, ofPredicted, ofInacative);

  // @abstract(@name invalidates the model when it is changed.)
  TPhastCollection = class(TCollection)
  protected
    FModel: TComponent;
  public
    procedure InvalidateModel;
    property Model: TComponent read FModel;
    // @name invalidates the model.
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification);
      override;
    constructor Create(ItemClass: TCollectionItemClass; Model: TComponent);
  end;

  TPhastCollectionItem = class(TCollectionItem)
  public
    procedure InvalidateModel;
  end;

  TPointArray = array of TPoint;

  TRealStorage = class(TPersistent)
  private
    FValue: real;
  protected
    procedure ReadValue(Reader: TReader);
    procedure WriteValue(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Value: real read FValue write FValue;
  end;

  TGoPhastPersistent = class(TPersistent)
  protected
    FModel: TObject;
    procedure InvalidateModel; virtual;
    procedure SetBooleanProperty(var AField: boolean; const NewValue: boolean);
    procedure SetIntegerProperty(var AField: integer; const NewValue: integer);
    procedure SetStringProperty(var AField: string; const NewValue: string);
  public
    Constructor Create(Model: TObject);
  end;

  function EvalAtToString(const Eval: TEvaluatedAt;
    const Model: TModelSelection; const Plural, TitleCase: boolean): string;

  function ValidName(const OriginalName: string): string;
  function RightCopy(const Source: string; LengthToCopy: integer): string;

resourcestring
  StrModelTop = 'Model_Top';

  {@name is used when writing the PHAST input file to insert a consistent
    number of blank spaces.

  @longcode(#
  BlankSpaces = '      ';
  #)
  }
  BlankSpaces = '      ';
  {@name is used as the default name for a new data set.

  @longcode(#
  rsNewDataSet = 'New data set';
  #)
  }
  rsNewDataSet = 'New data set';

  // @name is used to set captions for several radio buttons.
  // @Seealso(rsSetValueOfIntersected)
  // @Seealso(rsSetValueOf)
  // @Seealso(rsByInterpolation)
  // @Seealso(frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  //  frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  // @Seealso(frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions
  // frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions)
  // @Seealso(frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions
  // frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions)
  // @Seealso(frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions
  // frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions)
  rsSetValueOfEnclosed = 'Set values of enclosed ';
  // @name is used to set captions for several radio buttons.
  // @Seealso(rsSetValueOfEnclosed)
  // @Seealso(rsSetValueOf)
  // @Seealso(rsByInterpolation)
  // @Seealso(frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  //  frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  // @Seealso(frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions
  // frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions)
  // @Seealso(frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions
  // frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions)
  // @Seealso(frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions
  // frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions)
  rsSetValueOfIntersected = 'Set values of intersected ';
  // @name is used to set captions for several radio buttons.
  // @Seealso(rsSetValueOfEnclosed)
  // @Seealso(rsSetValueOfIntersected)
  // @Seealso(rsByInterpolation)
  // @Seealso(frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  //  frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  // @Seealso(frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions
  // frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions)
  // @Seealso(frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions
  // frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions)
  // @Seealso(frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions
  // frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions)
  rsSetValueOf = 'Set values of ';
  // @name is used to set captions for several radio buttons.
  // @Seealso(rsSetValueOfEnclosed)
  // @Seealso(rsSetValueOfIntersected)
  // @Seealso(rsSetValueOf)
  // @Seealso(frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  //  frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  // @Seealso(frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions
  // frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions)
  // @Seealso(frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions
  // frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions)
  // @Seealso(frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions
  // frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions)
  rsByInterpolation = ' by interpolation';

  // @name is the section name in the ini file that holds the
  // names of the most recently opened files.
  MRU_Section = 'MostRecentlyUsed';

  StrLowerLimit = 'Lower limit';
  StrUpperLimit = 'Upper limit';
  StrObjectIntersectLength = 'ObjectIntersectLength';
  StrObjectSectionIntersectLength = 'ObjectSectionIntersectLength';
  StrObjectIntersectArea = 'ObjectIntersectArea';
  StrObjectArea = 'ObjectArea';
  StrObjectLength = 'ObjectLength';
  StrStartingTime = 'Starting time';
  StrEndingTime = 'Ending time';

const
  // @name represents the characters used to define the end of a line.
  EndOfLine =
{$IFDEF MSWINDOWS}#13#10;
{$ENDIF}
{$IFDEF LINUX}#10;
{$ENDIF}

  // On Linux, @name is used to control the access permissions of files.
  // @name has no effect in Windows.
{$IFDEF MSWINDOWS}
  ReadWritePermissions = 0;
{$ELSE}
  ReadWritePermissions = S_IREAD or S_IWRITE or S_IRGRP or S_IWGRP or S_IROTH;
{$ENDIF}

  clTransparent32: TColor32 = 0;
  SelectEpsilon = 5;

implementation


{$IFNDEF Testing}
uses frmGoPhastUnit, PhastModelUnit;
{$ENDIF}

{ TPhastCollection }

constructor TPhastCollection.Create(ItemClass: TCollectionItemClass;
  Model: TComponent);
begin
  FModel := Model;
  inherited Create(ItemClass);
end;

procedure TPhastCollection.InvalidateModel;
begin
  if Model <> nil then
  begin
    (Model as TPhastModel).Invalidate;
  end;
end;

procedure TPhastCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;
{$IFNDEF Testing}
  InvalidateModel;
{$ENDIF}
end;

{ TRealStorage }

{ TRealStorage }

procedure TRealStorage.Assign(Source: TPersistent);
begin
  if Source is TRealStorage then
  begin
    Value := TRealStorage(Source).Value;
  end
  else
  begin
    inherited;
  end;
end;

procedure TRealStorage.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Value', ReadValue, WriteValue, Value = 0)
end;

procedure TRealStorage.ReadValue(Reader: TReader);
begin
  Value := Reader.ReadFloat;
end;

procedure TRealStorage.WriteValue(Writer: TWriter);
begin
  Writer.WriteFloat(Value);
end;

{ TGoPhastPersistent }

constructor TGoPhastPersistent.Create(Model: TObject);
begin
  inherited Create;
  Assert((Model = nil) or (Model is TPhastModel));
  FModel := Model;
end;

procedure TGoPhastPersistent.InvalidateModel;
begin
  if FModel <> nil then
  begin
    (FModel as TPhastModel).Invalidate;
  end;
end;

procedure TGoPhastPersistent.SetBooleanProperty(var AField: boolean;
  const NewValue: boolean);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

procedure TGoPhastPersistent.SetIntegerProperty(var AField: integer;
  const NewValue: integer);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

procedure TGoPhastPersistent.SetStringProperty(var AField: string;
  const NewValue: string);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

function EvalAtToString(const Eval: TEvaluatedAt; const Model: TModelSelection;
  const Plural, TitleCase: boolean): string;
begin
  result := '';
  case Model of
    msUndefined, msPhast:
      begin
        case Eval of
          eaBlocks:
            begin
              if TitleCase then
              begin
                result := 'Element';
              end
              else
              begin
                result := 'element';
              end;
            end;
          eaNodes:
            begin
              if TitleCase then
              begin
                result := 'Node';
              end
              else
              begin
                result := 'node';
              end;
            end;
          else
            Assert(False);
        end;
      end;
    msModflow:
      begin
        case Eval of
          eaBlocks:
            begin
              if TitleCase then
              begin
                result := 'Cell';
              end
              else
              begin
                result := 'cell';
              end;
            end;
          eaNodes:
            begin
              if TitleCase then
              begin
                result := 'Cell corner';
              end
              else
              begin
                result := 'cell corner';
              end;
            end;
          else
            Assert(False);
        end;
      end;
    else Assert(False);
  end;
  if Plural then
  begin
    result := result + 's';
  end;
end;

{ TPhastCollectionItem }

procedure TPhastCollectionItem.InvalidateModel;
begin
  (Collection as TPhastCollection).InvalidateModel;
end;

function ValidName(const OriginalName: string): string;
var
  Index: integer;
  AChar: Char;
begin
  result :=  Trim(OriginalName);
  for Index := 1 to Length(result) do
  begin
    AChar := result[Index];
    if Index = 1 then
    begin
      if not (AChar in ['_', 'a'..'z', 'A'..'Z']) then
      begin
        result[Index] := '_';
      end;
    end
    else
    begin
      if not (AChar in ['_', 'a'..'z', 'A'..'Z', '0'..'9']) then
      begin
        result[Index] := '_';
      end;
    end;
  end;
  if result = '' then
  begin
    result := '_';
  end
end;

function RightCopy(const Source: string; LengthToCopy: integer): string;
var
  Start: Integer;
begin
  Start := Length(Source) - LengthToCopy + 1;
  if Start < 1 then
  begin
    Start := 1;
  end;
  result := Copy(Source, Start, LengthToCopy);
end;

end.

