// @name defines a frame that is used to define the flux observation data
// for a particular @link(TScreenObject).
unit frameFluxObsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, RbwDataGrid4, StdCtrls, FluxObservationUnit;

type
  // @name is a frame that is used to define the flux observation data
  // for a particular @link(TScreenObject).
  TframeFluxObs = class(TFrame)
    // @name holds the flux observation data.
    // The checkbox in column 1 indicates whether the @link(TScreenObject)
    // being edited is part of the indicated flux observation.
    // The text is column 2 is the Factor formula.
    rdgObservationGroups: TRbwDataGrid4;
    // @name is used to indicate the type of flux observation.
    lblFluxObservations: TLabel;
    btnAddOrRemoveFluxObservations: TButton;
  private
    { Private declarations }
  public
    {@name sets the captions in @link(rdgObservationGroups).}
    procedure InitializeControls;
    // @name returns the number of @link(TScreenObject)s in ListOfScreenObjects
    // that are included in one or more @link(TFluxObservationGroup) in
    // Observations.
    //
    // @name fills @link(rdgObservationGroups) with the data related to
    // the observations.
    function GetData(ListOfScreenObjects: TList;
      Observations: TFluxObservationGroups): integer;
    // @name modifies Observations based on the values in
    // @link(rdgObservationGroups) and the @link(TScreenObject)s
    // in ListOfScreenObjects.
    // Initially, Observations should be identical to the
    // ones in @link(GetData).
    procedure SetData(ListOfScreenObjects: TList;
      Observations: TFluxObservationGroups; ScreenObjectsUsed: TCheckBoxState);
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses Math, ScreenObjectUnit;

{ TframeFluxObs }

function TframeFluxObs.GetData(ListOfScreenObjects: TList;
  Observations: TFluxObservationGroups): integer;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ObsevationIndex: Integer;
  Observation: TFluxObservationGroup;
  FirstObsForScreenObject: boolean;
  ScreenObjectUsed: boolean;
  ScreenObjectPosition: Integer;
  ObsFactor: TObservationFactor;
begin
  rdgObservationGroups.RowCount := Max(Observations.Count + 1,2);
  rdgObservationGroups.FixedCols := 1;
  rdgObservationGroups.FixedRows := 1;
  rdgObservationGroups.BeginUpdate;
  try
    for ObsevationIndex := 0 to Observations.Count - 1 do
    begin
      Observation := Observations[ObsevationIndex];
      rdgObservationGroups.Cells[0,ObsevationIndex+1] :=
        IntToStr(ObsevationIndex+1);
      rdgObservationGroups.Cells[1,ObsevationIndex+1] :=
        Observation.ObservationName;
      for ScreenObjectIndex := 0 to ListOfScreenObjects.Count - 1 do
      begin
        ScreenObject := ListOfScreenObjects[ScreenObjectIndex];
        ScreenObjectPosition :=
          Observation.ObservationFactors.IndexOfScreenObject(ScreenObject);
        ScreenObjectUsed := ScreenObjectPosition >= 0;
        if ScreenObjectIndex = 0 then
        begin
          rdgObservationGroups.Checked[1,ObsevationIndex+1] := ScreenObjectUsed;
          if ScreenObjectUsed then
          begin
            ObsFactor := Observation.ObservationFactors[ScreenObjectPosition];
            rdgObservationGroups.Cells[2,ObsevationIndex+1] := ObsFactor.Factor;
          end
          else
          begin
            rdgObservationGroups.Cells[2,ObsevationIndex+1] := '';
          end;
        end
        else
        begin
          if rdgObservationGroups.State[1,ObsevationIndex+1] <> cbGrayed then
          begin
            if rdgObservationGroups.Checked[1,ObsevationIndex+1]
              <> ScreenObjectUsed then
            begin
              if ScreenObjectUsed then
              begin
                ObsFactor :=
                  Observation.ObservationFactors[ScreenObjectPosition];
                rdgObservationGroups.Cells[2,ObsevationIndex+1] :=
                  ObsFactor.Factor;
              end;
              rdgObservationGroups.State[1,ObsevationIndex+1] := cbGrayed
            end;
          end
          else if ScreenObjectUsed then
          begin
            ObsFactor := Observation.ObservationFactors[ScreenObjectPosition];
            if rdgObservationGroups.Cells[2,ObsevationIndex+1]
              <> ObsFactor.Factor then
            begin
              rdgObservationGroups.Cells[2,ObsevationIndex+1] := '';
            end;
          end;
        end;
      end;
    end;
  finally
    rdgObservationGroups.EndUpdate;
  end;

  result := 0;
  for ScreenObjectIndex := 0 to ListOfScreenObjects.Count - 1 do
  begin
    ScreenObject := ListOfScreenObjects[ScreenObjectIndex];
    FirstObsForScreenObject := True;
    for ObsevationIndex := 0 to Observations.Count - 1 do
    begin
      Observation := Observations[ObsevationIndex];
      if Observation.ObservationFactors.
        IndexOfScreenObject(ScreenObject) >= 0 then
      begin
        if FirstObsForScreenObject then
        begin
          FirstObsForScreenObject := False;
          Inc(result);
        end;
      end;
    end;
  end;
end;

procedure TframeFluxObs.InitializeControls;
begin
  inherited;
  rdgObservationGroups.Cells[0,0] := 'N';
  rdgObservationGroups.Cells[1,0] := 'Observation group';
  rdgObservationGroups.Cells[2,0] := 'Factor';
end;

procedure TframeFluxObs.SetData(ListOfScreenObjects: TList;
  Observations: TFluxObservationGroups; ScreenObjectsUsed: TCheckBoxState);
var
  ObsevationIndex: Integer;
  Observation: TFluxObservationGroup;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ObjectPosition: Integer;
  ObsFactor: TObservationFactor;
  ObsState: TCheckBoxState;
begin
  Assert(rdgObservationGroups.RowCount = Observations.Count+1);
  for ObsevationIndex := 0 to Observations.Count - 1 do
  begin
    Observation:= Observations[ObsevationIndex];
    Assert(Observation.ObservationName =
      rdgObservationGroups.Cells[1,ObsevationIndex+1]);

    ObsState := rdgObservationGroups.CheckState[1, ObsevationIndex+1];
    for ScreenObjectIndex := 0 to ListOfScreenObjects.Count - 1 do
    begin
      ScreenObject := ListOfScreenObjects[ScreenObjectIndex];
      ObjectPosition := Observation.ObservationFactors.
        IndexOfScreenObject(ScreenObject);
      if (ScreenObjectsUsed = cbUnchecked)
        or (ObsState = cbUnchecked) then
      begin
        // remove
        if ObjectPosition >= 0 then
        begin
          Observation.ObservationFactors.Delete(ObjectPosition);
          ObjectPosition := -1;
        end;
      end
      else if (ScreenObjectsUsed = cbChecked)
        and (ObsState = cbChecked) then
      begin
        // add
        if ObjectPosition < 0 then
        begin
          ObjectPosition := Observation.AddObject(ScreenObject)
        end;
      end;
      if (ObjectPosition >= 0)
        and (rdgObservationGroups.Cells[2, ObsevationIndex+1] <> '') then
      begin
        ObsFactor := Observation.ObservationFactors[ObjectPosition];
        ObsFactor.Factor := rdgObservationGroups.Cells[2, ObsevationIndex+1];
      end;
    end;
  end;
end;

end.
