      MODULE OBSGHBMODULE
         INTEGER, SAVE, POINTER  ::NQGB,NQCGB,NQTGB,IUGBOBSV
         INTEGER, SAVE, DIMENSION(:),   POINTER ::NQOBGB
         INTEGER, SAVE, DIMENSION(:),   POINTER ::NQCLGB
         INTEGER, SAVE, DIMENSION(:),   POINTER ::IOBTS
         REAL,    SAVE, DIMENSION(:),   POINTER ::FLWSIM
         REAL,    SAVE, DIMENSION(:),   POINTER ::FLWOBS
         REAL,    SAVE, DIMENSION(:),   POINTER ::TOFF
         REAL,    SAVE, DIMENSION(:),   POINTER ::OTIME
         REAL,    SAVE, DIMENSION(:,:), POINTER ::QCELL
         CHARACTER*12,SAVE,DIMENSION(:),POINTER ::OBSNAM
      TYPE OBSGHBTYPE
         INTEGER, POINTER  ::NQGB,NQCGB,NQTGB,IUGBOBSV
         INTEGER,     DIMENSION(:),   POINTER ::NQOBGB
         INTEGER,     DIMENSION(:),   POINTER ::NQCLGB
         INTEGER,     DIMENSION(:),   POINTER ::IOBTS
         REAL,        DIMENSION(:),   POINTER ::FLWSIM
         REAL,        DIMENSION(:),   POINTER ::FLWOBS
         REAL,        DIMENSION(:),   POINTER ::TOFF
         REAL,        DIMENSION(:),   POINTER ::OTIME
         REAL,        DIMENSION(:,:), POINTER ::QCELL
         CHARACTER*12,DIMENSION(:),POINTER ::OBSNAM
      END TYPE
      TYPE(OBSGHBTYPE),  SAVE   ::OBSGHBDAT(10)
      END MODULE
C  NQGB -- number of cell groups
C  NQCGB -- total number of cells in all groups
C  NQTGB -- total number of observations -- sum of the number of times for each group
C  NQOBGB(NQGB) -- The number of observations in each observation group
C  NQCLGB(NQGB) -- The number of cells in each observation group
C  IOBTS(NQTGB) -- Observation time step
C  FLWSIM(NQTGB) -- Simulated value
C  FLWOBS(NQTGB) -- Observed value
C  TOFF(NQTGB) -- Fractional offset between time steps
C  OTIME(NQTGB) -- Observation time in model time units
C  QCELL(4,NQCGB) -- Location and proportion factor for each observation cell


      SUBROUTINE OBS2GHB7AR(IUGBOB,IUGB,IGRID)
C     ******************************************************************
C     ALLOCATE MEMORY AND READ FLOW OBSERVATIONS AT GHB CELLS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:IOUT,NPER,NSTP,PERLEN,TSMULT,ISSFLG,
     1                  NCOL,NROW,NLAY,ITRSS
      USE OBSGHBMODULE
      CHARACTER*200 LINE
	INTEGER LAYER,  ROW,  COLUMN
C     ------------------------------------------------------------------
      ALLOCATE(NQGB,NQCGB,NQTGB,IUGBOBSV)
C
      ZERO=0.0
      IERR=0
C  NT is the observation counter.
      NT=0
C  NC is the cell counter.
      NC=0
C
C     IDENTIFY PROCESS AND PACKAGE
      WRITE(IOUT,*) 'GBOB:'
!      WRITE(IOUT,7) IUGBOB
!    7 FORMAT(/,' OBS2GHB7 -- OBSERVATION PROCESS (GHB FLOW ',
!     &    'OBSERVATIONS)',/,' VERSION 2, 02/28/2006',/,
!     &    ' INPUT READ FROM UNIT ',I4)
C
Cx------Turn off observation package if GWFGHB is not active
      IF (IUGB.EQ.0) THEN
        WRITE (IOUT,29 )
   29   FORMAT (/,' GHB PACKAGE OF GWF IS NOT OPEN')
        CALL USTOP(' ')
      ENDIF
C
Cx------Read items 0 and 1.
      CALL URDCOM(IUGBOB,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQGB,DUM,IOUT,IUGBOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQCGB,DUM,IOUT,IUGBOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQTGB,DUM,IOUT,IUGBOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUGBOBSV,DUM,IOUT,IUGBOB)
      WRITE (IOUT,*) 'NQGB, NQCGB, NQTGB, IUGBOBSV:'
      WRITE (IOUT,*) NQGB, NQCGB, NQTGB, IUGBOBSV
!      WRITE (IOUT,9) NQGB, NQCGB, NQTGB
!    9 FORMAT (/,
!     &     ' NUMBER OF FLOW-OBSERVATION GHB-CELL GROUPS.....: ',I6,/,
!     &     '   NUMBER OF CELLS IN GHB-CELL GROUPS...........: ',I6,/,
!     &     '   NUMBER OF GHB-CELL FLOWS.....................: ',I6)
      IF(NQTGB.LE.0) THEN
         WRITE(IOUT,*) ' NUMBER OF OBSERVATIONS LESS THAN OR EQUAL TO 0'
         CALL USTOP(' ')
      END IF
 !     IF(IUGBOBSV.GT.0) THEN
 !        WRITE(IOUT,21) IUGBOBSV
 !  21    FORMAT(1X,
 !    1      'GHB OBSERVATIONS WILL BE SAVED ON UNIT.........:',I7)
 !     ELSE
 !        WRITE(IOUT,22)
 !  22    FORMAT(1X,'GHB OBSERVATIONS WILL NOT BE SAVED IN A FILE')
 !     END IF
C
Cx------Allocate memory
      ALLOCATE(NQOBGB(NQGB))
      ALLOCATE(NQCLGB(NQGB))
      ALLOCATE(IOBTS(NQTGB))
      ALLOCATE(FLWSIM(NQTGB))
      ALLOCATE(FLWOBS(NQTGB))
      ALLOCATE(TOFF(NQTGB))
      ALLOCATE(OTIME(NQTGB))
      ALLOCATE(QCELL(4,NQCGB))
      ALLOCATE(OBSNAM(NQTGB))
C
Cx------Initialize simulated equivalents
      DO 15 N=1,NQTGB
      FLWSIM(N)=ZERO
   15 CONTINUE
C
Cx------READ AND WRITE TIME-OFFSET MULTIPLIER FOR FLOW-OBSERVATION TIMES.
      READ(IUGBOB,*) TOMULTRV
      WRITE (IOUT,*) 'TOMULTGB:'
      WRITE (IOUT,*) TOMULTRV
!      WRITE (IOUT,20) TOMULTRV
!   20 FORMAT (/,' OBSERVED GHB-CELL FLOW DATA',/,' -- TIME OFFSETS',
!     &        ' ARE MULTIPLIED BY: ',G12.5)
C
Cx------LOOP THROUGH CELL GROUPS.
      DO 200 IQ = 1,NQGB
C
Cx------READ NUMBER OF OBSERVATINS AND NUMBER OF CELLS FOR ONE GROUP
Cx------(ITEM 3).
        READ (IUGBOB,*) NQOBGB(IQ), NQCLGB(IQ)
        WRITE (IOUT,*) 'NQOBGB(IQ), NQCLGB(IQ):'
        WRITE (IOUT,*)  NQOBGB(IQ), NQCLGB(IQ)
!        WRITE (IOUT,25) IQ, 'GHB', NQCLGB(IQ), NQOBGB(IQ)
!   25   FORMAT (/,'   GROUP NUMBER: ',I6,'   BOUNDARY TYPE: ',A,
!     &  '   NUMBER OF CELLS IN GROUP: ',I6,/,
!     &  '   NUMBER OF FLOW OBSERVATIONS: ',I6,//,
!     &  40X,'OBSERVED',/,
!     &  20X,'REFER.',13X,'GHB FLOW',/,
!     &  7X,'OBSERVATION',2X,'STRESS',4X,'TIME',5X,'GAIN (-) OR',14X,/,
!     &  2X,'OBS#    NAME',6X,'PERIOD   OFFSET',5X,'LOSS (+)')
C
Cx------SET FLAG FOR SETTING ALL PORTION FACTORS TO 1
        IFCTFLG = 0
        IF (NQCLGB(IQ).LT.0) THEN
          IFCTFLG = 1
          NQCLGB(IQ) = -NQCLGB(IQ)
        ENDIF
C
C
Cx------READ THE OBSERVATION NAMES, TIMES, AND MEASURED VALUES FOR
Cx------ONE CELL GROUP (ITEM 4)
        NT1 = NT + 1
        NT2 = NT + NQOBGB(IQ)
        DO 30 J = NT1, NT2
          READ (IUGBOB,*) OBSNAM(J), IREFSP, TOFFSET, FLWOBS(J)
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
        NC2 = NC + NQCLGB(IQ)
!        WRITE (IOUT,54)
!   54   FORMAT (/,'       LAYER  ROW  COLUMN    FACTOR')
        DO 100 L = NC1, NC2
          READ (IUGBOB,*) (QCELL(I,L),I=1,4)
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
     &          ' -- STOP EXECUTION (OBS2GHB7AR)',/)
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
     &' -- STOP EXECUTION (OBS2GHB7AR)')
        CALL USTOP(' ')
      ENDIF
C
Cx------RETURN.
      CALL SOBS2GHB7PSV(IGRID)
      RETURN
      END
!      SUBROUTINE OBS2GHB7SE(IGRID)
C     ******************************************************************
C     CALCULATE SIMULATED EQUIVALENTS TO OBSERVED FLOWS FOR THE GHB
C     PACKAGE
C     ******************************************************************
!      SUBROUTINE OBS2GHB7OT(IGRID)
C     ******************************************************************
C     WRITE ALL OBSERVATIONS TO LISTING FILE.
C     ******************************************************************
      SUBROUTINE OBS2GHB7DA(IGRID)
C  Deallocate OBSGHB memory
      USE OBSGHBMODULE
C
      CALL SOBS2GHB7PNT(IGRID)
      DEALLOCATE(NQGB)
      DEALLOCATE(NQCGB)
      DEALLOCATE(NQTGB)
      DEALLOCATE(IUGBOBSV)
      DEALLOCATE(NQOBGB)
      DEALLOCATE(NQCLGB)
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
      SUBROUTINE SOBS2GHB7PNT(IGRID)
C  Change OBSGHB data to a different grid.
      USE OBSGHBMODULE
C
      NQGB=>OBSGHBDAT(IGRID)%NQGB
      NQCGB=>OBSGHBDAT(IGRID)%NQCGB
      NQTGB=>OBSGHBDAT(IGRID)%NQTGB
      IUGBOBSV=>OBSGHBDAT(IGRID)%IUGBOBSV
      NQOBGB=>OBSGHBDAT(IGRID)%NQOBGB
      NQCLGB=>OBSGHBDAT(IGRID)%NQCLGB
      IOBTS=>OBSGHBDAT(IGRID)%IOBTS
      FLWSIM=>OBSGHBDAT(IGRID)%FLWSIM
      FLWOBS=>OBSGHBDAT(IGRID)%FLWOBS
      TOFF=>OBSGHBDAT(IGRID)%TOFF
      OTIME=>OBSGHBDAT(IGRID)%OTIME
      QCELL=>OBSGHBDAT(IGRID)%QCELL
      OBSNAM=>OBSGHBDAT(IGRID)%OBSNAM
C
      RETURN
      END
      SUBROUTINE SOBS2GHB7PSV(IGRID)
C  Save OBSGHB data for a grid.
      USE OBSGHBMODULE
C
      OBSGHBDAT(IGRID)%NQGB=>NQGB
      OBSGHBDAT(IGRID)%NQCGB=>NQCGB
      OBSGHBDAT(IGRID)%NQTGB=>NQTGB
      OBSGHBDAT(IGRID)%IUGBOBSV=>IUGBOBSV
      OBSGHBDAT(IGRID)%NQOBGB=>NQOBGB
      OBSGHBDAT(IGRID)%NQCLGB=>NQCLGB
      OBSGHBDAT(IGRID)%IOBTS=>IOBTS
      OBSGHBDAT(IGRID)%FLWSIM=>FLWSIM
      OBSGHBDAT(IGRID)%FLWOBS=>FLWOBS
      OBSGHBDAT(IGRID)%TOFF=>TOFF
      OBSGHBDAT(IGRID)%OTIME=>OTIME
      OBSGHBDAT(IGRID)%QCELL=>QCELL
      OBSGHBDAT(IGRID)%OBSNAM=>OBSNAM
C
      RETURN
      END
