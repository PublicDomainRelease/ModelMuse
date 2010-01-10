unit ErrorMessages;

interface

uses Classes;

var
  ErrorValues: TStringList;
  WarningValues: TStringList;

implementation

Procedure AssignErrorStrings;
begin
//  MaxError = 113;
//  ErrorValues: array[0..MaxError] of string =
    ErrorValues.Add('INVALID VALUE FOR IFREQ PARAMETER:');
    ErrorValues.Add('ERROR IN GMG INPUT');
    ErrorValues.Add('ALLOCATION ERROR IN SUBROUTINE GMG1ALG');
    ErrorValues.Add('GMG ASSEMBLY ERROR IN SUBROUTINE GMG1AP');
    ErrorValues.Add('DIS file must be specified for MODFLOW to run');
    ErrorValues.Add('STOP EXECUTION');
    ErrorValues.Add('THERE MUST BE AT LEAST ONE TIME STEP IN EVERY STRESS PERIOD');
    ErrorValues.Add('PERLEN MUST NOT BE 0.0 FOR TRANSIENT STRESS PERIODS');
    ErrorValues.Add('TSMULT MUST BE GREATER THAN 0.0');
    ErrorValues.Add('PERLEN CANNOT BE LESS THAN 0.0 FOR ANY STRESS PERIOD');
    ErrorValues.Add('ERROR READING OUTPUT CONTROL INPUT DATA:');
    ErrorValues.Add('CANNOT OPEN');
    ErrorValues.Add('FIRST ENTRY IN NAME FILE MUST BE "LIST".');
    ErrorValues.Add('ILLEGAL FILE TYPE IN NAME FILE:');
    ErrorValues.Add('NAME FILE IS EMPTY.');
    ErrorValues.Add('BAS PACKAGE FILE HAS NOT BEEN OPENED.');
    ErrorValues.Add('ERROR OPENING FILE');
    ErrorValues.Add('ARRAY OPERAND HAS NOT BEEN PREVIOUSLY DEFINED:');
    ErrorValues.Add('NPVAL IN PARAMETER INPUT FILE MUST BE');
    ErrorValues.Add('BUT THE MAXIMUM NUMBER OF PARAMETERS IS');
    ErrorValues.Add('ERROR FOUND IN PARAMETER INPUT FILE.  SEARCH ABOVE');
    ErrorValues.Add('ERROR ENCOUNTERED IN READING PARAMETER INPUT FILE');
    ErrorValues.Add('IS GREATER THAN MXACTC');
    ErrorValues.Add('NO-FLOW CELLS CANNOT BE CONVERTED TO SPECIFIED HEAD');
    ErrorValues.Add('IS GREATER THAN MXACTD');
    ErrorValues.Add('IS GREATER THAN MXADRT');
    ErrorValues.Add('is outside of the grid');
    ErrorValues.Add('Blank parameter name in the');
    ErrorValues.Add('Parameter type conflict:');
    ErrorValues.Add('Blank instance name');
    ErrorValues.Add('file specifies undefined instance');
    ErrorValues.Add('IS GREATER THAN THE MAXIMUM ALLOWED');
    ErrorValues.Add('file specifies an undefined parameter');
    ErrorValues.Add('Parameter type must be');
    ErrorValues.Add('ILLEGAL ET OPTION CODE');
    ErrorValues.Add('INVALID LAYER NUMBER IN IEVT FOR COLUMN');
    ErrorValues.Add('SIMULATION ABORTING');
    ErrorValues.Add('Aborting. Weights for Auxiliary variables cannot');
    ErrorValues.Add('ABORTING');
    ErrorValues.Add('*** ERROR');
    ErrorValues.Add('IS GREATER THAN MXACTB');
    ErrorValues.Add('INSTANCES ARE NOT SUPPORTED FOR HFB');
    ErrorValues.Add('ERROR DETECTED IN LOCATION DATA OF BARRIER NO.');
    ErrorValues.Add('LAYWT is not 0 and LTHUF is 0 for layer:');
    ErrorValues.Add('Invalid parameter type');
    ErrorValues.Add('Simulation is transient and no storage parameters are');
    ErrorValues.Add('Simulation is steady state and storage parameters are');
    ErrorValues.Add('Simulation is transient and has convertible');
    ErrorValues.Add('Simulation is steady state and SYTP parameter(s) are');
    ErrorValues.Add('STOP SGWF2HUF7VKA');
    ErrorValues.Add('CONSTANT-HEAD CELL WENT DRY');
    ErrorValues.Add('Sy not defined for cell at');
    ErrorValues.Add('***ERROR***');
    ErrorValues.Add('INTERBED STORAGE INAPPROPRIATE FOR A STEADY-STATE');
    ErrorValues.Add('GSFLOW-1.0 cannot simulate transport');
    ErrorValues.Add('--program stopping');
    ErrorValues.Add('MAXIMUM NUMBER OF GRID CELLS ADJACENT TO LAKES');
    ErrorValues.Add('LAK Package requires BCF or LPF');
    ErrorValues.Add('PROGRAM STOPPING');
    ErrorValues.Add('ERROR - NO AQUIFER UNDER LAKE CELL');
    ErrorValues.Add('***NLAKES too large for BUFF in Subroutine GWF2');
    ErrorValues.Add('LAYWET is not 0 and LAYTYP is 0 for layer');
    ErrorValues.Add('IS AN INVALID LAYAVG VALUE -- MUST BE 0, 1, or 2');
    ErrorValues.Add('Negative cell thickness at (layer,row,col)');
    ErrorValues.Add('SIMULATION ABORTED');
    ErrorValues.Add('Negative confining bed thickness below cell (Layer,row,col)');
    ErrorValues.Add('exceeds maximum of');
    ErrorValues.Add('IS GREATER THAN mxwel2');
    ErrorValues.Add('ERROR opening auxillary input file');
    ErrorValues.Add('ILLEGAL RECHARGE OPTION CODE');
    ErrorValues.Add('INVALID LAYER NUMBER IN IRCH');
    ErrorValues.Add('IS GREATER THAN MXACTR');
    ErrorValues.Add('SEGMENT MUST BE GREATER THAN 0 AND LESS THAN NSS');
    ErrorValues.Add('SEGMENTS MUST BE IN ORDER FROM 1 THROUGH NSS');
    ErrorValues.Add('REACHES MUST BE NUMBERED CONSECUTIVELY');
    ErrorValues.Add('RESIDUAL WATER CONTENT IS EQUAL OR GREATER THAN');
    ErrorValues.Add('INITIAL WATER CONTENT IS GREATER THAN SATURATED');
    ErrorValues.Add('PROGRAM TERMINATED');
    ErrorValues.Add('CANNOT SPECIFY MORE THAN NSS STREAM SEGMENTS');
    ErrorValues.Add('CODE STOPPING');
    ErrorValues.Add('SEGMENT NUMBER (NSEG) OUT OF RANGE:');
    ErrorValues.Add('Blank instance name');
    ErrorValues.Add('file specifies undefined instance');
    ErrorValues.Add('GREATER THAN MXACTS');
    ErrorValues.Add('SUBSIDENCE CANNOT BE USED');
    ErrorValues.Add('STOPPING');
    ErrorValues.Add('IMPROPER LAYER ASSIGNMENT');
    ErrorValues.Add('GREATER THAN MXACTW');
    ErrorValues.Add('Duplicate parameter name');
    ErrorValues.Add('The number of parameters has exceeded the maximum');
    ErrorValues.Add('CLUSTERS WERE SPECIFIED, BUT THERE IS SPACE FOR ONLY');
    ErrorValues.Add('Multiplier array has not been defined');
    ErrorValues.Add('There were no zone values specified in the cluster');
    ErrorValues.Add('Zone array has not been defined');
    ErrorValues.Add('NH LESS THAN OR EQUAL TO 0');
    ErrorValues.Add('ERROR:');
    ErrorValues.Add('An observation cannot be placed');
    ErrorValues.Add('NQTCH LESS THAN OR EQUAL TO 0');
    ErrorValues.Add('SEE ABOVE FOR ERROR MESSAGE');
    ErrorValues.Add('DRAIN PACKAGE OF GWF IS NOT OPEN');
    ErrorValues.Add('NUMBER OF OBSERVATIONS LESS THAN OR EQUAL TO 0');
    ErrorValues.Add('GHB PACKAGE OF GWF IS NOT OPEN');
    ErrorValues.Add('RIVER PACKAGE OF GWF IS NOT OPEN');
    ErrorValues.Add('EXCEEDED THE MAXIMUM NUMBER OF INSTANCES:');
    ErrorValues.Add('The above parameter must be defined prior to its use');
    ErrorValues.Add('Number of parameters exceeds MXPAR');
    ErrorValues.Add('EXCEEDED THE MAXIMUM NUMBER OF LIST ENTRIES:');
    ErrorValues.Add('EXCEEDED THE MAXIMUM NUMBER OF INSTANCES:');
    ErrorValues.Add('Parameter type must be:');
    ErrorValues.Add('ERROR CONVERTING');
    ErrorValues.Add('ERROR READING ARRAY CONTROL RECORD');
    ErrorValues.Add('INVALID INTERBLOCK T CODE:');
    ErrorValues.Add('INVALID LAYER TYPE:');
    ErrorValues.Add('LAYER TYPE 1 IS ONLY ALLOWED IN TOP LAYER');
    ErrorValues.Add('FAILED TO MEET SOLVER CONVERGENCE CRITERIA');
    ErrorValues.Add('nlakes dimension problem in lak7');
    ErrorValues.Add('MAXIMUM NUMBER OF GRID CELLS ADJACENT TO LAKES HAS BEEN EXCEEDED WITH CELL');
    ErrorValues.Add('LAK Package requires BCF, LPF, or HUF');
    ErrorValues.Add('THIS WILL CAUSE PROBLEMS IN COMPUTING LAKE STAGE USING THE NEWTON METHOD.');
    ErrorValues.Add('***NLAKES too large for BUFF in Subroutine GWF2LAK7SFR7RPS***  STOP EXECUTION');
    ErrorValues.Add('FAILED TO CONVERGE');

//  MaxWarning = 2;
//  WarningValues: array[0..MaxWarning] of string =
    WarningValues.Add('**WARNING**');
    WarningValues.Add('*** WARNING ***');
    WarningValues.Add('CELL CONVERSIONS FOR ITER');
    WarningValues.Add('****Units are undefined');
    WarningValues.Add('ELIMINATED BECAUSE ALL HYDRAULIC CONDUCTIVITIES TO NODE ARE 0');
    WarningValues.Add('WARNING-- COMPUTED STAGE OF ');
    WarningValues.Add('IF WETDRY FLAG NOT TURNED ON, VERTICAL LEAKANCES ARE NOT SAVED:');
    WarningValues.Add('THEREFORE, LAKE/AQUIFER CONDUCTANCES ARE BASED SOLELY ON LAKEBED SPECIFICATION');
    WarningValues.Add('NODE(S) ADJACENT TO LAKE IN CONFINED LAYER:');
    WarningValues.Add('LAKE/AQUIFER CONDUCTANCES BASED SOLELY ON LAKEBED SPECIFICATION');
    WarningValues.Add('NOTE: INFORMATION ABOUT CALCULATED LAKE/AQUIFER CONDUCTANCES WHEN USING BCF PACKAGE FOLLOWS:');
    WarningValues.Add('*** WARNING: IBOUND = ');
    WarningValues.Add('WARNING -- SUM OF INTERLAKE FLUXES ');
    WarningValues.Add(' WARNING****  OUTFLOWING STREAM SEGMENT');
    WarningValues.Add('Note: Solution may be sensitive to value of HWtol;');
    WarningValues.Add('adjust value if solution fails to converge');
    WarningValues.Add('deactivated this time step because Hnew<bottom elev. of cell');
    WarningValues.Add('deactivated this time step because IBOUND=0');
    WarningValues.Add('DEACTIVATED THIS STRESS PERIOD BECAUSE ALL NODES WERE DEACTIVATED');
    WarningValues.Add('***WARNING*** Specified-head condition should not exist in same cell as a multi-node well');
    WarningValues.Add('***WARNING*** CWC<0 in Well ');
    WarningValues.Add('***WARNING*** CWC<0 reset to CWC=0');
    WarningValues.Add('WARNING');

end;

initialization
  ErrorValues := TStringList.Create;
  WarningValues := TStringList.Create;
  AssignErrorStrings;

finalization
  ErrorValues.Free;
  WarningValues.Free;

end.