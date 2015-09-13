      MODULE OBSRIVMODULE
         INTEGER, SAVE, POINTER  ::NQRV,NQCRV,NQTRV,IURVOBSV
         INTEGER, SAVE, DIMENSION(:),   POINTER ::NQOBRV
         INTEGER, SAVE, DIMENSION(:),   POINTER ::NQCLRV
         INTEGER, SAVE, DIMENSION(:),   POINTER ::IOBTS
         REAL,    SAVE, DIMENSION(:),   POINTER ::FLWSIM
         REAL,    SAVE, DIMENSION(:),   POINTER ::FLWOBS
         REAL,    SAVE, DIMENSION(:),   POINTER ::TOFF
         REAL,    SAVE, DIMENSION(:),   POINTER ::OTIME
         REAL,    SAVE, DIMENSION(:,:), POINTER ::QCELL
         CHARACTER*12,SAVE,DIMENSION(:),POINTER ::OBSNAM
      TYPE OBSRIVTYPE
         INTEGER, POINTER  ::NQRV,NQCRV,NQTRV,IURVOBSV
         INTEGER,     DIMENSION(:),   POINTER ::NQOBRV
         INTEGER,     DIMENSION(:),   POINTER ::NQCLRV
         INTEGER,     DIMENSION(:),   POINTER ::IOBTS
         REAL,        DIMENSION(:),   POINTER ::FLWSIM
         REAL,        DIMENSION(:),   POINTER ::FLWOBS
         REAL,        DIMENSION(:),   POINTER ::TOFF
         REAL,        DIMENSION(:),   POINTER ::OTIME
         REAL,        DIMENSION(:,:), POINTER ::QCELL
         CHARACTER*12,DIMENSION(:),POINTER ::OBSNAM
      END TYPE
      TYPE(OBSRIVTYPE),  SAVE   ::OBSRIVDAT(10)
      END MODULE
C  NQRV -- number of cell groups
C  NQCRV -- total number of cells in all groups
C  NQTRV -- total number of observations -- sum of the number of times for each group
C  NQOBRV(NQRV) -- The number of observations in each observation group
C  NQCLRV(NQRV) -- The number of cells in each observation group
C  IOBTS(NQTRV) -- Observation time step
C  FLWSIM(NQTRV) -- Simulated value
C  FLWOBS(NQTRV) -- Observed value
C  TOFF(NQTRV) -- Fractional offset between time steps
C  OTIME(NQTRV) -- Observation time in model time units
C  QCELL(4,NQCRV) -- Location and proportion factor for each observation cell


      SUBROUTINE OBS2RIV7AR(IURVOB,IURV,IGRID)
C     ******************************************************************
C     ALLOCATE MEMORY AND READ FLOW OBSERVATIONS AT RIVER CELLS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:IOUT,NPER,NSTP,PERLEN,TSMULT,ISSFLG,
     1                  NCOL,NROW,NLAY,ITRSS
      USE OBSRIVMODULE
      CHARACTER*200 LINE
	INTEGER LAYER,  ROW,  COLUMN
C     ------------------------------------------------------------------
      ALLOCATE(NQRV,NQCRV,NQTRV,IURVOBSV)
C
      ZERO=0.0
      IERR=0
C  NT is the observation counter.
      NT=0
C  NC is the cell counter.
      NC=0
C
C     IDENTIFY PROCESS AND PACKAGE
      WRITE(IOUT,*) 'RVOB:'
!      WRITE(IOUT,7) IURVOB
!    7 FORMAT(/,' OBS2RIV7 -- OBSERVATION PROCESS (RIVER FLOW ',
!     &    'OBSERVATIONS)',/,' VERSION 2, 02/28/2006',/,
!     &    ' INPUT READ FROM UNIT ',I4)
C
Cx------Stop if GWFRIV is not active
      IF (IURV.EQ.0) THEN
        WRITE (IOUT,29 )
   29   FORMAT (/,' RIVER PACKAGE OF GWF IS NOT OPEN')
        CALL USTOP(' ')
        RETURN
      ENDIF
C
Cx------Read items 0 and 1.
      CALL URDCOM(IURVOB,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQRV,DUM,IOUT,IURVOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQCRV,DUM,IOUT,IURVOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQTRV,DUM,IOUT,IURVOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IURVOBSV,DUM,IOUT,IURVOB)
      WRITE (IOUT,*) 'NQRV, NQCRV, NQTRV, IURVOBSV:'
      WRITE (IOUT,*) NQRV, NQCRV, NQTRV, IURVOBSV
!      WRITE (IOUT,9) NQRV, NQCRV, NQTRV
!    9 FORMAT (/,
!     &     ' NUMBER OF FLOW-OBSERVATION RIVER-CELL GROUPS.....: ',I6,/,
!     &     '   NUMBER OF CELLS IN RIVER-CELL GROUPS...........: ',I6,/,
!     &     '   NUMBER OF RIVER-CELL FLOWS.....................: ',I6)
      IF(NQTRV.LE.0) THEN
         WRITE(IOUT,*) ' NUMBER OF OBSERVATIONS LESS THAN OR EQUAL TO 0'
         CALL USTOP(' ')
      END IF
!      IF(IURVOBSV.GT.0) THEN
!         WRITE(IOUT,21) IURVOBSV
!   21    FORMAT(1X,
!     1      'RIVER OBSERVATIONS WILL BE SAVED ON UNIT.........:',I7)
!      ELSE
!         WRITE(IOUT,22)
!   22    FORMAT(1X,'RIVER OBSERVATIONS WILL NOT BE SAVED IN A FILE')
!      END IF
C
Cx------Allocate memory
      ALLOCATE(NQOBRV(NQRV))
      ALLOCATE(NQCLRV(NQRV))
      ALLOCATE(IOBTS(NQTRV))
      ALLOCATE(FLWSIM(NQTRV))
      ALLOCATE(FLWOBS(NQTRV))
      ALLOCATE(TOFF(NQTRV))
      ALLOCATE(OTIME(NQTRV))
      ALLOCATE(QCELL(4,NQCRV))
      ALLOCATE(OBSNAM(NQTRV))
C
Cx------Initialize simulated equivalents
      DO 15 N=1,NQTRV
      FLWSIM(N)=ZERO
   15 CONTINUE
C
Cx------READ AND WRITE TIME-OFFSET MULTIPLIER FOR FLOW-OBSERVATION TIMES.
      READ(IURVOB,*) TOMULTRV
      WRITE (IOUT,*) 'TOMULTRV:'
      WRITE (IOUT,*) TOMULTRV
!      WRITE (IOUT,20) TOMULTRV
!   20 FORMAT (/,' OBSERVED RIVER-CELL FLOW DATA',/,' -- TIME OFFSETS',
!     &        ' ARE MULTIPLIED BY: ',G12.5)
C
Cx------LOOP THROUGH CELL GROUPS.
      DO 200 IQ = 1,NQRV
C
Cx------READ NUMBER OF OBSERVATINS AND NUMBER OF CELLS FOR ONE GROUP
Cx------(ITEM 3).
        READ (IURVOB,*) NQOBRV(IQ), NQCLRV(IQ)
        WRITE (IOUT,*) 'NQOBRV(IQ), NQCLRV(IQ):'
        WRITE (IOUT,*)  NQOBRV(IQ), NQCLRV(IQ) 
!        WRITE (IOUT,25) IQ, 'RIV', NQCLRV(IQ), NQOBRV(IQ)
!   25   FORMAT (/,'   GROUP NUMBER: ',I6,'   BOUNDARY TYPE: ',A,
!     &  '   NUMBER OF CELLS IN GROUP: ',I6,/,
!     &  '   NUMBER OF FLOW OBSERVATIONS: ',I6,//,
!     &  40X,'OBSERVED',/,
!     &  20X,'REFER.',13X,'RIVER FLOW',/,
!     &  7X,'OBSERVATION',2X,'STRESS',4X,'TIME',5X,'GAIN (-) OR',14X,/,
!     &  2X,'OBS#    NAME',6X,'PERIOD   OFFSET',5X,'LOSS (+)')
C
Cx------SET FLAG FOR SETTING ALL PORTION FACTORS TO 1
        IFCTFLG = 0
        IF (NQCLRV(IQ).LT.0) THEN
          IFCTFLG = 1
          NQCLRV(IQ) = -NQCLRV(IQ)
        ENDIF
C
C
Cx------READ THE OBSERVATION NAMES, TIMES, AND MEASURED VALUES FOR
Cx------ONE CELL GROUP (ITEM 4)
        NT1 = NT + 1
        NT2 = NT + NQOBRV(IQ)
        DO 30 J = NT1, NT2
          READ (IURVOB,*) OBSNAM(J), IREFSP, TOFFSET, FLWOBS(J)
          WRITE(IOUT,* ) 'OBSNAM(J),IREFSP,TOFFSET,FLWOBS(J):'
          WRITE(IOUT,* ) OBSNAM(J)
          WRITE(IOUT,* ) IREFSP,TOFFSET,FLWOBS(J)
!          WRITE(IOUT,27 ) J,OBSNAM(J),IREFSP,TOFFSET,FLWOBS(J)
!   27     FORMAT (I6,1X,A12,2X,I4,2X,G11.4,1X,G11.4)
!          CALL UOBSTI(OBSNAM(J),IOUT,ISSFLG,ITRSS,NPER,NSTP,IREFSP,
!     &                IOBTS(J),PERLEN,TOFF(J),TOFFSET,TOMULTRV,TSMULT,1,
!     &                OTIME(J))
   30   CONTINUE
C
Cx------READ LAYER, ROW, COLUMN, AND FACTOR (ITEM 5) FOR EACH CELL IN
Cx------THE CELL GROUP.
        NC1 = NC + 1
        NC2 = NC + NQCLRV(IQ)
!        WRITE (IOUT,54)
!   54   FORMAT (/,'       LAYER  ROW  COLUMN    FACTOR')
        DO 100 L = NC1, NC2
          READ (IURVOB,*) (QCELL(I,L),I=1,4)
          IF(IFCTFLG.EQ.1) QCELL(4,L) = 1.
          WRITE (IOUT,*) 'Layer Row Column Factor:'
	    LAYER = QCELL(1,L)
	    ROW = QCELL(2,L)
          COLUMN = QCELL(3,L)
          WRITE (IOUT,*) LAYER, ROW, COLUMN, QCELL(4,L)
!          WRITE (IOUT,55) (QCELL(I,L),I=1,4)
!   55     FORMAT (4X,F8.0,F6.0,F7.0,F9.2)
          I = QCELL(2,L)
          J = QCELL(3,L)
          IF (J.LE.0 .OR. J.GT.NCOL .OR. I.LE.0 .OR. I.GT.NROW) THEN
            WRITE (IOUT,59)
   59       FORMAT (/,' ROW OR COLUMN NUMBER INVALID',
     &          ' -- STOP EXECUTION (OBS2RIV7AR)',/)
            IERR = 1
          ENDIF
  100   CONTINUE
C
Cx------END OF INPUT FOR ONE CELL GROUP -- UPDATE COUNTERS.
        NC = NC2
        NT = NT2
  200 CONTINUE
C
C
      IF (IERR.GT.0) THEN
        WRITE(IOUT,620)
  620 FORMAT (/,1X,'ERROR: SEARCH ABOVE FOR ERROR MESSAGE(S)',/,
     &' -- STOP EXECUTION (OBS2RIV7)')
        CALL USTOP(' ')
      ENDIF
C
Cx------RETURN.
      CALL SOBS2RIV7PSV(IGRID)
      RETURN
      END
!      SUBROUTINE OBS2RIV7SE(IGRID)
C     ******************************************************************
C     CALCULATE SIMULATED EQUIVALENTS TO OBSERVED FLOWS FOR THE RIVER
C     PACKAGE
C     ******************************************************************
!      SUBROUTINE OBS2RIV7OT(IGRID)
C     ******************************************************************
C     WRITE ALL OBSERVATIONS TO LISTING FILE.
C     ******************************************************************
      SUBROUTINE OBS2RIV7DA(IGRID)
C  Deallocate OBSRIV memory
      USE OBSRIVMODULE
C
      CALL SOBS2RIV7PNT(IGRID)
      DEALLOCATE(NQRV)
      DEALLOCATE(NQCRV)
      DEALLOCATE(NQTRV)
      DEALLOCATE(IURVOBSV)
      DEALLOCATE(NQOBRV)
      DEALLOCATE(NQCLRV)
      DEALLOCATE(IOBTS)
      DEALLOCATE(FLWSIM)
      DEALLOCATE(FLWOBS)
      DEALLOCATE(TOFF)
      DEALLOCATE(OTIME)
      DEALLOCATE(QCELL)
      DEALLOCATE(OBSNAM)
C
      RETURN
      END
      SUBROUTINE SOBS2RIV7PNT(IGRID)
C  Change OBSRIV data to a different grid.
      USE OBSRIVMODULE
C
      NQRV=>OBSRIVDAT(IGRID)%NQRV
      NQCRV=>OBSRIVDAT(IGRID)%NQCRV
      NQTRV=>OBSRIVDAT(IGRID)%NQTRV
      IURVOBSV=>OBSRIVDAT(IGRID)%IURVOBSV
      NQOBRV=>OBSRIVDAT(IGRID)%NQOBRV
      NQCLRV=>OBSRIVDAT(IGRID)%NQCLRV
      IOBTS=>OBSRIVDAT(IGRID)%IOBTS
      FLWSIM=>OBSRIVDAT(IGRID)%FLWSIM
      FLWOBS=>OBSRIVDAT(IGRID)%FLWOBS
      TOFF=>OBSRIVDAT(IGRID)%TOFF
      OTIME=>OBSRIVDAT(IGRID)%OTIME
      QCELL=>OBSRIVDAT(IGRID)%QCELL
      OBSNAM=>OBSRIVDAT(IGRID)%OBSNAM
C
      RETURN
      END
      SUBROUTINE SOBS2RIV7PSV(IGRID)
C  Save OBSRIV data for a grid.
      USE OBSRIVMODULE
C
      OBSRIVDAT(IGRID)%NQRV=>NQRV
      OBSRIVDAT(IGRID)%NQCRV=>NQCRV
      OBSRIVDAT(IGRID)%NQTRV=>NQTRV
      OBSRIVDAT(IGRID)%IURVOBSV=>IURVOBSV
      OBSRIVDAT(IGRID)%NQOBRV=>NQOBRV
      OBSRIVDAT(IGRID)%NQCLRV=>NQCLRV
      OBSRIVDAT(IGRID)%IOBTS=>IOBTS
      OBSRIVDAT(IGRID)%FLWSIM=>FLWSIM
      OBSRIVDAT(IGRID)%FLWOBS=>FLWOBS
      OBSRIVDAT(IGRID)%TOFF=>TOFF
      OBSRIVDAT(IGRID)%OTIME=>OTIME
      OBSRIVDAT(IGRID)%QCELL=>QCELL
      OBSRIVDAT(IGRID)%OBSNAM=>OBSNAM
C
      RETURN
      END
