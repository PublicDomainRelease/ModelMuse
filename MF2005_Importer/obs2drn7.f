      MODULE OBSDRNMODULE
         INTEGER, SAVE, POINTER  ::NQDR,NQCDR,NQTDR,IUDROBSV
         INTEGER, SAVE, DIMENSION(:),   POINTER ::NQOBDR
         INTEGER, SAVE, DIMENSION(:),   POINTER ::NQCLDR
         INTEGER, SAVE, DIMENSION(:),   POINTER ::IOBTS
         REAL,    SAVE, DIMENSION(:),   POINTER ::FLWSIM
         REAL,    SAVE, DIMENSION(:),   POINTER ::FLWOBS
         REAL,    SAVE, DIMENSION(:),   POINTER ::TOFF
         REAL,    SAVE, DIMENSION(:),   POINTER ::OTIME
         REAL,    SAVE, DIMENSION(:,:), POINTER ::QCELL
         CHARACTER*12,SAVE,DIMENSION(:),POINTER ::OBSNAM
      TYPE OBSDRNTYPE
         INTEGER, POINTER  ::NQDR,NQCDR,NQTDR,IUDROBSV
         INTEGER,     DIMENSION(:),   POINTER ::NQOBDR
         INTEGER,     DIMENSION(:),   POINTER ::NQCLDR
         INTEGER,     DIMENSION(:),   POINTER ::IOBTS
         REAL,        DIMENSION(:),   POINTER ::FLWSIM
         REAL,        DIMENSION(:),   POINTER ::FLWOBS
         REAL,        DIMENSION(:),   POINTER ::TOFF
         REAL,        DIMENSION(:),   POINTER ::OTIME
         REAL,        DIMENSION(:,:), POINTER ::QCELL
         CHARACTER*12,DIMENSION(:),POINTER ::OBSNAM
      END TYPE
      TYPE(OBSDRNTYPE),  SAVE   ::OBSDRNDAT(10)
      END MODULE
C  NQDR -- number of cell groups
C  NQCDR -- total number of cells in all groups
C  NQTDR -- total number of observations -- sum of the number of times for each group
C  NQOBDR(NQDR) -- The number of observations in each observation group
C  NQCLDR(NQDR) -- The number of cells in each observation group
C  IOBTS(NQTDR) -- Observation time step
C  FLWSIM(NQTDR) -- Simulated value
C  FLWOBS(NQTDR) -- Observed value
C  TOFF(NQTDR) -- Fractional offset between time steps
C  OTIME(NQTDR) -- 
C  QCELL(4,NQCDR) -- Location and proportion factor for each observation cell


      SUBROUTINE OBS2DRN7AR(IUDROB,IUDRN,IGRID)
C     ******************************************************************
C     ALLOCATE MEMORY AND READ FLOW OBSERVATIONS AT DRAIN CELLS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:IOUT,NPER,NSTP,PERLEN,TSMULT,ISSFLG,
     1                  NCOL,NROW,NLAY,ITRSS
      USE OBSDRNMODULE
      CHARACTER*200 LINE
	INTEGER LAYER,  ROW,  COLUMN
C     ------------------------------------------------------------------
C
      ALLOCATE(NQDR,NQCDR,NQTDR,IUDROBSV)
C
      ZERO=0.0
      IERR=0
C  NT is the observation counter.
      NT=0
C  NC is the cell counter.
      NC=0
C
C     IDENTIFY PROCESS AND PACKAGE
      WRITE(IOUT,*) 'DROB:'
!      WRITE(IOUT,7) IUDROB
!    7 FORMAT(/,' OBS2DRN7 -- OBSERVATION PROCESS (DRAIN FLOW ',
!     &    'OBSERVATIONS)',/,' VERSION 2, 02/28/2006',/,
!     &    ' INPUT READ FROM UNIT ',I4)
C
Cx------Stop if GWFDRN is not active
      IF (IUDRN.EQ.0) THEN
        WRITE (IOUT,29 )
   29   FORMAT (/,' DRAIN PACKAGE OF GWF IS NOT OPEN')
        CALL USTOP(' ')
      ENDIF
C
Cx------Read items 0 and 1.
      CALL URDCOM(IUDROB,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQDR,DUM,IOUT,IUDROB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQCDR,DUM,IOUT,IUDROB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQTDR,DUM,IOUT,IUDROB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUDROBSV,DUM,IOUT,IUDROB)
      WRITE (IOUT,*) 'NQDR, NQCDR, NQTDR, IUDROBSV:'
      WRITE (IOUT,*) NQDR, NQCDR, NQTDR, IUDROBSV
!      WRITE (IOUT,9) NQDR, NQCDR, NQTDR
!    9 FORMAT (/,
!     &     ' NUMBER OF FLOW-OBSERVATION DRAIN-CELL GROUPS.....: ',I6,/,
!     &     '   NUMBER OF CELLS IN DRAIN-CELL GROUPS...........: ',I6,/,
!     &     '   NUMBER OF DRAIN-CELL FLOWS.....................: ',I6)
      IF(NQTDR.LE.0) THEN
         WRITE(IOUT,*) ' NUMBER OF OBSERVATIONS LESS THAN OR EQUAL TO 0'
         CALL USTOP(' ')
      END IF
!      IF(IUDROBSV.GT.0) THEN
!         WRITE(IOUT,21) IUDROBSV
!   21    FORMAT(1X,
!     1      'DRAIN OBSERVATIONS WILL BE SAVED ON UNIT.........:',I7)
!      ELSE
!         WRITE(IOUT,22)
!   22    FORMAT(1X,'DRAIN OBSERVATIONS WILL NOT BE SAVED IN A FILE')
!      END IF
C
Cx------Allocate memory
      ALLOCATE(NQOBDR(NQDR))
      ALLOCATE(NQCLDR(NQDR))
      ALLOCATE(IOBTS(NQTDR))
      ALLOCATE(FLWSIM(NQTDR))
      ALLOCATE(FLWOBS(NQTDR))
      ALLOCATE(TOFF(NQTDR))
      ALLOCATE(OTIME(NQTDR))
      ALLOCATE(QCELL(4,NQCDR))
      ALLOCATE(OBSNAM(NQTDR))
C
Cx------Initialize simulated equivalents
      DO 15 N=1,NQTDR
      FLWSIM(N)=ZERO
   15 CONTINUE
C
Cx------READ AND WRITE TIME-OFFSET MULTIPLIER FOR FLOW-OBSERVATION TIMES.
      READ(IUDROB,*) TOMULTDR
      WRITE (IOUT,*) 'TOMULTDR:'
      WRITE (IOUT,*) TOMULTDR
!      WRITE (IOUT,20) TOMULTDR
!   20 FORMAT (/,' OBSERVED DRAIN-CELL FLOW DATA',/,' -- TIME OFFSETS',
!     &        ' ARE MULTIPLIED BY: ',G12.5)
C
Cx------LOOP THROUGH CELL GROUPS.
      DO 200 IQ = 1,NQDR
C
Cx------READ NUMBER OF OBSERVATINS AND NUMBER OF CELLS FOR ONE GROUP
Cx------(ITEM 3).
        READ (IUDROB,*) NQOBDR(IQ), NQCLDR(IQ)
        WRITE (IOUT,*) 'NQOBDR(IQ), NQCLDR(IQ):'
        WRITE (IOUT,*)  NQOBDR(IQ), NQCLDR(IQ)
!        WRITE (IOUT,25) IQ, 'DRN', NQCLDR(IQ), NQOBDR(IQ)
!   25   FORMAT (/,'   GROUP NUMBER: ',I6,'   BOUNDARY TYPE: ',A,
!     &  '   NUMBER OF CELLS IN GROUP: ',I6,/,
!     &  '   NUMBER OF FLOW OBSERVATIONS: ',I6,//,
!     &  40X,'OBSERVED',/,
!     &  20X,'REFER.',13X,'DRAIN FLOW',/,
!     &  7X,'OBSERVATION',2X,'STRESS',4X,'TIME',5X,'GAIN (-) OR',14X,/,
!     &  2X,'OBS#    NAME',6X,'PERIOD   OFFSET',5X,'LOSS (+)')
C
Cx------SET FLAG FOR SETTING ALL PORTION FACTORS TO 1
        IFCTFLG = 0
        IF (NQCLDR(IQ).LT.0) THEN
          IFCTFLG = 1
          NQCLDR(IQ) = -NQCLDR(IQ)
        ENDIF
C
C
Cx------READ THE OBSERVATION NAMES, TIMES, AND MEASURED VALUES FOR
Cx------ONE CELL GROUP (ITEM 4)
        NT1 = NT + 1
        NT2 = NT + NQOBDR(IQ)
        DO 30 J = NT1, NT2
          READ (IUDROB,*) OBSNAM(J), IREFSP, TOFFSET, FLWOBS(J)
          WRITE(IOUT,* ) 'OBSNAM(J),IREFSP,TOFFSET,FLWOBS(J):'
          WRITE(IOUT,* ) OBSNAM(J)
          WRITE(IOUT,* ) IREFSP,TOFFSET,FLWOBS(J)
!          WRITE(IOUT,27 ) J,OBSNAM(J),IREFSP,TOFFSET,FLWOBS(J)
!   27     FORMAT (I6,1X,A12,2X,I4,2X,G11.4,1X,G11.4)
!          CALL UOBSTI(OBSNAM(J),IOUT,ISSFLG,ITRSS,NPER,NSTP,IREFSP,
!     &                IOBTS(J),PERLEN,TOFF(J),TOFFSET,TOMULTDR,TSMULT,1,
!     &                OTIME(J))
   30   CONTINUE
C
Cx------READ LAYER, ROW, COLUMN, AND FACTOR (ITEM 5) FOR EACH CELL IN
Cx------THE CELL GROUP.
        NC1 = NC + 1
        NC2 = NC + NQCLDR(IQ)
!        WRITE (IOUT,54)
!   54   FORMAT (/,'       LAYER  ROW  COLUMN    FACTOR')
        DO 100 L = NC1, NC2
          READ (IUDROB,*) (QCELL(I,L),I=1,4)
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
     &          ' -- STOP EXECUTION (OBS2DRN7RP)',/)
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
     &' -- STOP EXECUTION (OBS2DRN7RP)')
        CALL USTOP(' ')
      ENDIF
C
Cx------RETURN.
      CALL SOBS2DRN7PSV(IGRID)
      RETURN
      END
!      SUBROUTINE OBS2DRN7SE(IGRID)
C     ******************************************************************
C     CALCULATE SIMULATED EQUIVALENTS TO OBSERVED FLOWS FOR THE DRAIN
C     PACKAGE
C     ******************************************************************
!      SUBROUTINE OBS2DRN7OT(IGRID)
C     ******************************************************************
C     WRITE ALL OBSERVATIONS TO LISTING FILE.
C     ******************************************************************
      SUBROUTINE OBS2DRN7DA(IGRID)
C  Deallocate OBSDRN memory
      USE OBSDRNMODULE
C
      CALL SOBS2DRN7PNT(IGRID)
      DEALLOCATE(NQDR)
      DEALLOCATE(NQCDR)
      DEALLOCATE(NQTDR)
      DEALLOCATE(IUDROBSV)
      DEALLOCATE(NQOBDR)
      DEALLOCATE(NQCLDR)
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
      SUBROUTINE SOBS2DRN7PNT(IGRID)
C  Change OBSDRN data to a different grid.
      USE OBSDRNMODULE
C
      NQDR=>OBSDRNDAT(IGRID)%NQDR
      NQCDR=>OBSDRNDAT(IGRID)%NQCDR
      NQTDR=>OBSDRNDAT(IGRID)%NQTDR
      IUDROBSV=>OBSDRNDAT(IGRID)%IUDROBSV
      NQOBDR=>OBSDRNDAT(IGRID)%NQOBDR
      NQCLDR=>OBSDRNDAT(IGRID)%NQCLDR
      IOBTS=>OBSDRNDAT(IGRID)%IOBTS
      FLWSIM=>OBSDRNDAT(IGRID)%FLWSIM
      FLWOBS=>OBSDRNDAT(IGRID)%FLWOBS
      TOFF=>OBSDRNDAT(IGRID)%TOFF
      OTIME=>OBSDRNDAT(IGRID)%OTIME
      QCELL=>OBSDRNDAT(IGRID)%QCELL
      OBSNAM=>OBSDRNDAT(IGRID)%OBSNAM
C
      RETURN
      END
      SUBROUTINE SOBS2DRN7PSV(IGRID)
C  Save OBSDRN data for a grid.
      USE OBSDRNMODULE
C
      OBSDRNDAT(IGRID)%NQDR=>NQDR
      OBSDRNDAT(IGRID)%NQCDR=>NQCDR
      OBSDRNDAT(IGRID)%NQTDR=>NQTDR
      OBSDRNDAT(IGRID)%IUDROBSV=>IUDROBSV
      OBSDRNDAT(IGRID)%NQOBDR=>NQOBDR
      OBSDRNDAT(IGRID)%NQCLDR=>NQCLDR
      OBSDRNDAT(IGRID)%IOBTS=>IOBTS
      OBSDRNDAT(IGRID)%FLWSIM=>FLWSIM
      OBSDRNDAT(IGRID)%FLWOBS=>FLWOBS
      OBSDRNDAT(IGRID)%TOFF=>TOFF
      OBSDRNDAT(IGRID)%OTIME=>OTIME
      OBSDRNDAT(IGRID)%QCELL=>QCELL
      OBSDRNDAT(IGRID)%OBSNAM=>OBSNAM
C
      RETURN
      END
