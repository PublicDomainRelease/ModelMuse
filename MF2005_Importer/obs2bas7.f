      MODULE OBSBASMODULE
         INTEGER, SAVE,POINTER ::ITS,NH,MAXM,MOBS,IUHOBSV,IDRY,JDRY
         REAL,    SAVE,POINTER ::HOBDRY
         INTEGER, SAVE, DIMENSION(:,:), POINTER ::NDER
         INTEGER, SAVE, DIMENSION(:,:), POINTER ::MLAY
         INTEGER, SAVE, DIMENSION(:),   POINTER ::IOFF
         INTEGER, SAVE, DIMENSION(:),   POINTER ::JOFF
         INTEGER, SAVE, DIMENSION(:),   POINTER ::IHOBWET
         REAL,    SAVE, DIMENSION(:),   POINTER ::H
         REAL,    SAVE, DIMENSION(:),   POINTER ::HOBS
         REAL,    SAVE, DIMENSION(:),   POINTER ::TOFF
         REAL,    SAVE, DIMENSION(:),   POINTER ::ROFF
         REAL,    SAVE, DIMENSION(:),   POINTER ::COFF
         REAL,    SAVE, DIMENSION(:),   POINTER ::OTIME
         REAL,    SAVE, DIMENSION(:,:), POINTER ::PR
         REAL,    SAVE, DIMENSION(:,:), POINTER ::RINT
         CHARACTER*12,SAVE,DIMENSION(:),POINTER ::OBSNAM
       TYPE OBSBASTYPE
         INTEGER,  POINTER    ::ITS,NH,MAXM,MOBS,IUHOBSV,IDRY,JDRY
         REAL,     POINTER    ::HOBDRY
         INTEGER,  DIMENSION(:,:), POINTER ::NDER
         INTEGER,  DIMENSION(:,:), POINTER ::MLAY
         INTEGER,  DIMENSION(:),   POINTER ::IOFF
         INTEGER,  DIMENSION(:),   POINTER ::JOFF
         INTEGER,  DIMENSION(:),   POINTER ::IHOBWET
         REAL,     DIMENSION(:),   POINTER ::H
         REAL,     DIMENSION(:),   POINTER ::HOBS
         REAL,     DIMENSION(:),   POINTER ::TOFF
         REAL,     DIMENSION(:),   POINTER ::ROFF
         REAL,     DIMENSION(:),   POINTER ::COFF
         REAL,     DIMENSION(:),   POINTER ::OTIME
         REAL,     DIMENSION(:,:), POINTER ::PR
         REAL,     DIMENSION(:,:), POINTER ::RINT
         CHARACTER*12,DIMENSION(:),POINTER ::OBSNAM
      END TYPE
      TYPE(OBSBASTYPE), SAVE :: OBSBASDAT(10)
      END MODULE OBSBASMODULE
C  NDER(1,n) -- Observation layer
C  NDER(2,n) -- Observation row
C  NDER(3,n) -- Observation column
C  NDER(4,n) -- Observation time step
C  NDER(5,n) -- Observation number for computing observation as a head change
C  MLAY(MAXM,MOBS) -- Layer numbers for multilayer observations
C  IOFF(NH) -- Row offset for neighboring cell for interpolation
C  JOFF(NH) -- Column offset for neighboring cell for interpolation
C  IHOBWET(NH) -- Flag for observation -- 1 for wet, and -1 for dry
C  H(NH) -- Simulated value
C  HOBS(NH) -- Observed value
C  TOFF(NH) -- Fractional offset between time steps
C  ROFF(NH) -- Fractional offset from center of cell in Y direction (between rows)
C  COFF(NH) -- Fractional offset from center of cell in X direction (between columns)
C  OTIME(NH) -- Observation time in model time units
C  PR(MAXM,MOBS) -- Fractional value for each layer of multilayer observations
C  RINT(4,NH) -- Interpolation coefficients for the 4 nodes surrounding observation
C  OBSNAM(NH) -- Observation name


      SUBROUTINE OBS2BAS7AR(IUHDOB,IGRID)
C     ******************************************************************
C     INITIALIZE AND READ VARIABLES FOR HEAD OBSERVATIONS
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY: NCOL,NROW,NLAY,DELR,DELC,
     1                  NPER,NSTP,PERLEN,TSMULT,ISSFLG,IOUT,ITRSS
      USE OBSBASMODULE
C
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C1------ALLOCATE AND INITIALIZE TIME STEP COUNTER FOR USE BY ANY
C1------OBSERVATION PACKAGE.
      ALLOCATE(ITS)
      ITS=0
      IF(IUHDOB.LE.0) GO TO 700
C
C2------ALLOCATE OTHER SCALARS IF HEAD OBSERVATIONS ARE BEING SPECIFIED.
      ALLOCATE(NH,MAXM,MOBS,IUHOBSV,IDRY,JDRY)
      ALLOCATE(HOBDRY)
C
C3------DEFINE CONSTANTS
      IDRY = 0
      JDRY = 0
      ZERO=0.0
      ONEN=-1.0
      ML=0
      IERR=0
C
C4------WRITE VERSION.
      WRITE (IOUT,*) 'HOB:'
!      WRITE (IOUT,14) IUHDOB
!   14 FORMAT (/,' OBS2BAS7 -- HEAD OBSERVATIONS, ',
!     &        'VERSION 2.0, 2/28/2006',/,' INPUT READ FROM UNIT ',I3)
C
C5------READ & PRINT ITEM 1 OF THE HOB INPUT FILE
      CALL URDCOM(IUHDOB,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NH,DUM,IOUT,IUHDOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MOBS,DUM,IOUT,IUHDOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXM,DUM,IOUT,IUHDOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUHOBSV,DUM,IOUT,IUHDOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,HOBDRY,IOUT,IUHDOB)
	WRITE(IOUT, *) 'NH, MOBS, MAXM, IUHOBSV, HOBDRY:'
	WRITE(IOUT, *) NH, MOBS, MAXM, IUHOBSV, HOBDRY 
      IF (MAXM.EQ.1) THEN
        WRITE (IOUT,17)
   17   FORMAT (/,' MAXM CAN NOT EQUAL 1 -- STOP EXECUTION')
        CALL USTOP(' ')
      ENDIF
!      WRITE (IOUT,19) NH, MOBS, MAXM
!   19 FORMAT (/,
!     &     ' NUMBER OF HEADS....................................:',I5,/,
!     &     '   NUMBER OF MULTILAYER HEADS.......................:',I5,/,
!     &     '   MAXIMUM NUMBER OF LAYERS FOR MULTILAYER HEADS....:',I5)
      IF(NH.LE.0) THEN
         WRITE(IOUT,*) ' NH LESS THAN OR EQUAL TO 0'
         CALL USTOP(' ')
      END IF
!      IF(IUHOBSV.GT.0) THEN
!         WRITE(IOUT,21) IUHOBSV
!   21    FORMAT(1X,
!     1      'HEAD OBSERVATIONS WILL BE SAVED ON UNIT............:',I5)
!      ELSE
!         WRITE(IOUT,22)
!   22    FORMAT(1X,'HEAD OBSERVATIONS WILL NOT BE SAVED IN A FILE')
!      END IF
!      WRITE(IOUT,23) HOBDRY
!   23 FORMAT(1X,'SIMULATED EQUIVALENT HEAD AT DRY CELLS WILL BE:',
!     1            1P,1G15.6)
C
C6------ALLOCATE ARRAY DATA.
      ALLOCATE(NDER(5,NH))
      ALLOCATE(IOFF(NH))
      ALLOCATE(JOFF(NH))
      ALLOCATE(IHOBWET(NH))
      ALLOCATE(OBSNAM(NH))
      ALLOCATE(H(NH))
      ALLOCATE(HOBS(NH))
      ALLOCATE(TOFF(NH))
      ALLOCATE(OTIME(NH))
      ALLOCATE(ROFF(NH))
      ALLOCATE(COFF(NH))
      ALLOCATE(RINT(4,NH))
      IF(MOBS.GT.0 .AND. MAXM.GT.1) THEN
        ALLOCATE(MLAY(MAXM,MOBS))
        ALLOCATE(PR(MAXM,MOBS))
      ELSE
        ALLOCATE(MLAY(1,1))
        ALLOCATE(PR(1,1))
      END IF
C
C7------INITIALIZE OTIME, SIMULATED EQUIVALENT HEAD, NDER(5,n), and IHOBWET.
      DO 50 N = 1, NH
        OTIME(N) = ONEN
        H(N) = ZERO
        NDER(5,N)=0
        IHOBWET(N)=1
   50 CONTINUE
C
C8------READ ITEM 2
      READ (IUHDOB,*) TOMULTH
      WRITE (IOUT,*) 'TOMULTH:'
      WRITE (IOUT,*) TOMULTH
C
C9------WRITE ITEM 2 AND TITLE FOR OBSERVATION TIMES.
!      WRITE (IOUT,530) TOMULTH
!  530 FORMAT (/,' OBSERVED HEAD DATA -- TIME OFFSETS ARE',
!     &' MULTIPLIED BY: ',G12.5,//,
!     &20X,'REFER.',/,
!     &7X,'OBSERVATION',2X,'STRESS',4X,'TIME',/,
!     &2X,'OBS#    NAME',6X,'PERIOD',3X,'OFFSET    OBSERVATION')
C
C10-----INITIALIZE N, WHICH IS THE COUNT OF THE NUMBER OF OBSERVATIONS
C10-----AS THEY ARE READ.
      N=0
C
C11-----READ NAME, LOCATION, TIME, AND OBSERVED VALUE (ITEM 3)
   60 N=N+1
      READ (IUHDOB,*) OBSNAM(N), (NDER(I,N),I=1,3), IREFSP, TOFFSET,
     &                 ROFF(N), COFF(N), HOBS(N)
	WRITE (IOUT,*) 
	1  'OBSNAM_D3:'
      WRITE (IOUT,*) OBSNAM(N)
	WRITE (IOUT,*) 
	1  'LAYER ROW COLUMN IREFSP TOFFSET ROFF COFF HOBS:'
      WRITE (IOUT,*) (NDER(I,N),I=1,3), IREFSP, TOFFSET,
     &                 ROFF(N), COFF(N), HOBS(N)
!      WRITE (IOUT,535) N, OBSNAM(N), IREFSP, TOFFSET, HOBS(N)
!  535 FORMAT (1X,I5,1X,A12,2X,I4,2X,G11.4,1X,G11.4)
C
C11A----FOR SINGLE-TIME OBSERVATION (IREFSP>0), CALL UOBSTI TO DETERMINE
C11A----WHEN OBSERVATION OCCURS.
!      IF (IREFSP.GE.0) THEN
!        CALL UOBSTI(OBSNAM(N),IOUT,ISSFLG,ITRSS,NPER,NSTP,IREFSP,
!     &                NDER(4,N),PERLEN,TOFF(N),TOFFSET,TOMULTH,TSMULT,
!     &                0,OTIME(N))
!      END IF
C
C12-----CHECK ROW AND COLUMN LOCATION.
      I = NDER(2,N)
      J = NDER(3,N)
      IF (J.LE.0 .OR. J.GT.NCOL .OR. I.LE.0 .OR. I.GT.NROW) THEN
        WRITE (IOUT,550) N
  550   FORMAT (' FOR OBS',I5,' ROW OR COLUMN NUMBER INVALID -- ',
     &        'STOP EXECUTION (OBS2BAS7HRP)',/)
        IERR = 1
      ENDIF
C
C13-----Check if multi-layer
      IF(NDER(1,N).GE.0) THEN
C
C13A----SINGLE LAYER -- CHECK FOR VALID LAYER.
        IF (NDER(1,N).LE.0 .OR. NDER(1,N).GT.NLAY) THEN
          WRITE (IOUT,265) NDER(1,N)
  265     FORMAT (' FOR OBS ',I5,' LAYER INVALID -- STOP EXECUTION',
     &        ' (OBS2BAS7AR)',/)
          IERR = 1
        END IF
      ELSE
C
C13B----MULTI-LAYER -- CHECK LIMITS AND READ THE LAYERS AND PROPORTIONS.
         NL=-NDER(1,N)
         ML = ML + 1
         IF(ML.GT.MOBS) THEN
           WRITE (IOUT,565)
  565 FORMAT (/,' NUMBER OF MULTILAYER OBSERVATIONS EXCEEDS MOBS -- ',
     &        'STOP EXECUTION (OBS2BAS7AR)',/)
           CALL USTOP(' ')
         END IF
         IF(NL.GT.MAXM) THEN
           WRITE(IOUT,620) NL
  620      FORMAT(/,1X,'ERROR: VALUE ENTERED FOR MAXM IN HOB FILE IS',
     &    ' SMALLER THAN THE MAXIMUM NUMBER',/,' OF LAYERS',
     &    ' IN A MULTILAYER HEAD OBSERVATION, WHICH IS ',
     &    I3,' -- INCREASE MAXM.')
           CALL USTOP(' ')
         END IF
         DO 268 M=1,MAXM
           MLAY(M,ML) = 0
  268    CONTINUE
         READ (IUHDOB,*) (MLAY(M,ML),PR(M,ML),M=1,NL)
         WRITE(IOUT,*) '(MLAY(M,ML),PR(M,ML),M=1,NL):'
         WRITE(IOUT,*) (MLAY(M,ML),PR(M,ML),M=1,NL)
!         WRITE(IOUT,540) (MLAY(M,ML),PR(M,ML),M=1,NL)
!  540  FORMAT (5X,'MULTIPLE LAYERS AND PROPORTIONS :',5(I5,',',F5.2,3X))
C
C
C13C----CHECK LAYER NUMBERS AND ADD PROPORTIONS FOR MULTILAYER
C13C----OBSERVATION WELLS.
        TPR=ZERO
        DO 270 K = 1,NL
          KK = MLAY(K,ML)
          TPR = TPR + PR(K,ML)
          IF (KK.LE.0 .OR. KK.GT.NLAY) THEN
            WRITE (IOUT,265) N
            IERR = 1
          ENDIF
  270   CONTINUE
C
C13D----CHECK SUM OF PROPORTIONS FOR MULTILAYER OBS WELLS
        IF (ABS(1.-TPR).GT..02) THEN
          WRITE (IOUT,560) N
  560 FORMAT (/,' FOR OBS',I5,' MULTILAYER PROPORTIONS DO NOT SUM ',
     &        'TO 1.0 -- STOP EXECUTION (OBS2BAS7AR)',/)
          IERR = 1
        ENDIF
      END IF
C
C14-----CALCULATE INTERPOLATION COEFFICIENTS FOR THE LOCATION.
!      CALL SOBS2BAS7HIA(N,ML)
C
C15-----CHECK FOR MULTI-TIME
      NT=-IREFSP
      IF(NT.GT.0) THEN
C
C15A----READ FLAG FOR USING TEMPORAL CHANGES IN HEAD (ITEM 5)
        READ (IUHDOB,*) ITT
        WRITE (IOUT,*) 'ITT:'
        WRITE (IOUT,*) ITT
!        WRITE (IOUT,515) ITT
!  515   FORMAT (2X,'TRANSIENT DATA AT THIS LOCATION, ITT =',I4)
        IF (ITT.NE.1 .AND. ITT.NE.2) THEN
          WRITE (IOUT,575) N
  575     FORMAT (' FOR OBS',I5,
     &     ' ITT MUST = 1 OR 2 -- STOP EXECUTION (OBS2BAS7HRP)',/)
          CALL USTOP(' ')
        ENDIF
C
C15B----LOOP THROUGH THE TIMES
        NBASE=N
        DO 200 J=1,NT
        IF(J.NE.1) THEN
C
C15B1---DUPLICATE THE LOCATION INFORMATION FOR THE OBSERVATIONS AT THE
C15B1---SAME LOCATION.
          N=N+1
          IF(N.GT.NH) THEN
            WRITE(IOUT,127)
  127       FORMAT(1X,/,1X,'ABORTING BECAUSE THERE ARE MORE HEAD',
     1                       ' OBSERVATIONS THAN SPECIFIED BY NH')
            CALL USTOP(' ')
          END IF
          DO 140 I = 1, 3
            NDER(I,N) = NDER(I,N-1)
  140     CONTINUE
          ROFF(N) = ROFF(N-1)
          COFF(N) = COFF(N-1)
          IOFF(N) = IOFF(N-1)
          JOFF(N) = JOFF(N-1)
          DO 150 I = 1, 4
            RINT(I,N) = RINT(I,N-1)
  150     CONTINUE
          IF (NDER(1,N-1).LT.0) THEN
            ML1 = ML
            ML = ML + 1
            DO 160 M = 1, MAXM
              PR(M,ML) = PR(M,ML1)
              MLAY(M,ML) = MLAY(M,ML1)
  160       CONTINUE
          ENDIF
        END IF 
C
C15B2---READ ONE MULTI-TIME OBSERVATION.
        READ (IUHDOB,*) OBSNAM(N), IREFSP, TOFFSET, HOBS(N)
        WRITE (IOUT,*) 'OBSNAM_D6:'
        WRITE (IOUT,*) OBSNAM(N)
        WRITE (IOUT,*) 'IREFSP, TOFFSET, HOBS(N):'
        WRITE (IOUT,*) IREFSP, TOFFSET, HOBS(N)
        IF(ITT.EQ.2 .AND. J.NE.1) THEN
           HOBS(N)=HOBS(N)-HOBS(NBASE)
           NDER(5,N)=NBASE
        END IF
C
C15B3---WRITE ONE MULTI-TIME OBSERVATION.
!        WRITE (IOUT,535) N, OBSNAM(N), IREFSP, TOFFSET,HOBS(N)
!        CALL UOBSTI(OBSNAM(N),IOUT,ISSFLG,ITRSS,NPER,NSTP,IREFSP,
!     &              NDER(4,N),PERLEN,TOFF(N),TOFFSET,TOMULTH,
!     &              TSMULT,0,OTIME(N))
!        IF(J.EQ.NT) WRITE (IOUT,570)
!  570   FORMAT (' ')
  200   CONTINUE
      END IF
C
C16-----READ ANOTHER OBSERVATION (ITEM 3) IF THERE ARE STILL MORE OBSERVATIONS.
      IF(N.LT.NH) GO TO 60
C
C17-----DONE READING HEAD OBSERVATIONS.
C17-----PRINT TABLE SHOWING LOCATION OF OBSERVATIONS.
!      WRITE (IOUT,590)
!  590 FORMAT (/,53X,'HEAD CHANGE',/,54X,'REFERENCE',/,
!     &8X,'OBSERVATION',19X,'ROW',5X,'COL    OBSERVATION',/,
!     &2X,'OBS#',5X,'NAME',7X,'LAY  ROW  COL  OFFSET  OFFSET',3X,
!     &'(IF > 0)')
!      DO 450 N = 1, NH
!        WRITE (IOUT,600) N, OBSNAM(N), (NDER(I,N),I=1,3), ROFF(N),
!     &                   COFF(N), NDER(5,N)
!  600 FORMAT (1X,I5,2X,A12,2X,I3,2(1X,I4),2(2X,F6.3),3X,I6)
!  450 CONTINUE
C
C18-----IF ERROR OCCURRED ABOVE, PRINT MESSAGE AND STOP.
      IF (IERR.GT.0) THEN
        WRITE(IOUT,610)
  610 FORMAT(/,1X,'ERROR: SEARCH ABOVE FOR ERROR MESSAGE(S)',/,1X,
     &'STOP EXECUTION -- (OBS2BAS7AR)')
        CALL USTOP(' ')
      ENDIF
C
C19-----RETURN.
  700 CALL SOBS2BAS7PSV(IUHDOB,IGRID)
      RETURN
      END
!      SUBROUTINE OBS2BAS7SE(IUHDOB,IGRID)
C     ******************************************************************
C     INTERPOLATE HEADS.  ACCOUNT FOR DRY CELLS, IF NEEDED.
C     ******************************************************************
!      SUBROUTINE OBS2BAS7OT(IUHDOB,IGRID)
C     ******************************************************************
C     WRITE HEAD OBSERVATIONS
C     ******************************************************************
!      SUBROUTINE SOBS2BAS7HIA(N,ML)
C     ******************************************************************
C     CALCULATE INTERPOLATION COEFFICIENTS FOR LOCATING OBSERVED HEADS
C     ASSUMING ALL CELLS ARE ACTIVE.
C     ******************************************************************
!      SUBROUTINE SOBS2BAS7HIB(NDER,COFF,ROFF,DELR,DELC,IBOUND,NCOL,NROW,
!     &                      NLAY,RINT,JOFF,IOFF,MLAY)
C     ******************************************************************
C     CALCULATE INTERPOLATION COEFFICIENTS FOR LOCATING OBSERVED HEADS
C     USING CURRENT IBOUND VALUES.
C     ******************************************************************
!      SUBROUTINE SOBS2BAS7HBF(COFF,DELC,DELR,I,I1,IBI,IBIJ,IBJ,IOFF,J,
!     &                      J1,JOFF,NCOL,NROW,RINT,ROFF)
C     ******************************************************************
C     CALCULATE BASIS FUNCTIONS FOR INTERPOLATING OBSERVED HEADS.
C     ******************************************************************
!      SUBROUTINE UOBSTI(ID,IOUT,ISSFLG,ITRSS,NPER,NSTP,IREFSP,NUMTS,
!     &                  PERLEN,TOFF1,TOFFSET,TOMULT,TSMULT,ITR1ST,
!     &                  OBSTIME)
C     ******************************************************************
C     ASSIGN OBSERVATION TIME STEP (NUMTS) AND TOFF GIVEN REFERENCE
C     STRESS PERIOD (IREFSP), OBSERVATION-TIME OFFSET (TOFFSET), AND
C     TIME-OFFSET MULTIPLIER (TOMULT)
C     ******************************************************************
!      SUBROUTINE UOBSSV(IUOBSSV,NOBS,H,HOBS,OBSNAM,LABEL)
C     ******************************************************************
C     SAVE OBSERVATIONS TO A DISK FILE
C     ******************************************************************
      SUBROUTINE OBS2BAS7DA(IUHDOB,IGRID)
C  Deallocate OBSBAS memory
      USE OBSBASMODULE
C
      CALL SOBS2BAS7PNT(IUHDOB,IGRID)
      DEALLOCATE(ITS)
      IF(IUHDOB.LE.0) RETURN
C
      DEALLOCATE(NH)
      DEALLOCATE(MAXM)
      DEALLOCATE(MOBS)
      DEALLOCATE(IUHOBSV)
      DEALLOCATE(IDRY)
      DEALLOCATE(JDRY)
      DEALLOCATE(HOBDRY)
      DEALLOCATE(NDER)
      DEALLOCATE(MLAY)
      DEALLOCATE(IOFF)
      DEALLOCATE(JOFF)
      DEALLOCATE(IHOBWET)
      DEALLOCATE(H)
      DEALLOCATE(HOBS)
      DEALLOCATE(TOFF)
      DEALLOCATE(ROFF)
      DEALLOCATE(COFF)
      DEALLOCATE(OTIME)
      DEALLOCATE(PR)
      DEALLOCATE(RINT)
      DEALLOCATE(OBSNAM)
C
      RETURN
      END
      SUBROUTINE SOBS2BAS7PNT(IUHDOB,IGRID)
C  Change OBSBAS data to a different grid.
      USE OBSBASMODULE
C
      ITS=>OBSBASDAT(IGRID)%ITS
      IF(IUHDOB.LE.0) RETURN
C
      NH=>OBSBASDAT(IGRID)%NH
      MAXM=>OBSBASDAT(IGRID)%MAXM
      MOBS=>OBSBASDAT(IGRID)%MOBS
      IUHOBSV=>OBSBASDAT(IGRID)%IUHOBSV
      IDRY=>OBSBASDAT(IGRID)%IDRY
      JDRY=>OBSBASDAT(IGRID)%JDRY
      HOBDRY=>OBSBASDAT(IGRID)%HOBDRY
      NDER=>OBSBASDAT(IGRID)%NDER
      MLAY=>OBSBASDAT(IGRID)%MLAY
      IOFF=>OBSBASDAT(IGRID)%IOFF
      JOFF=>OBSBASDAT(IGRID)%JOFF
      IHOBWET=>OBSBASDAT(IGRID)%IHOBWET
      H=>OBSBASDAT(IGRID)%H
      HOBS=>OBSBASDAT(IGRID)%HOBS
      TOFF=>OBSBASDAT(IGRID)%TOFF
      ROFF=>OBSBASDAT(IGRID)%ROFF
      COFF=>OBSBASDAT(IGRID)%COFF
      OTIME=>OBSBASDAT(IGRID)%OTIME
      PR=>OBSBASDAT(IGRID)%PR
      RINT=>OBSBASDAT(IGRID)%RINT
      OBSNAM=>OBSBASDAT(IGRID)%OBSNAM
C
      RETURN
      END
      SUBROUTINE SOBS2BAS7PSV(IUHDOB,IGRID)
C  Save OBSBAS data for a grid.
      USE OBSBASMODULE
C
C
      OBSBASDAT(IGRID)%ITS=>ITS
      IF(IUHDOB.LE.0) RETURN
C
      OBSBASDAT(IGRID)%NH=>NH
      OBSBASDAT(IGRID)%MAXM=>MAXM
      OBSBASDAT(IGRID)%MOBS=>MOBS
      OBSBASDAT(IGRID)%IUHOBSV=>IUHOBSV
      OBSBASDAT(IGRID)%IDRY=>IDRY
      OBSBASDAT(IGRID)%JDRY=>JDRY
      OBSBASDAT(IGRID)%HOBDRY=>HOBDRY
      OBSBASDAT(IGRID)%NDER=>NDER
      OBSBASDAT(IGRID)%MLAY=>MLAY
      OBSBASDAT(IGRID)%IOFF=>IOFF
      OBSBASDAT(IGRID)%JOFF=>JOFF
      OBSBASDAT(IGRID)%IHOBWET=>IHOBWET
      OBSBASDAT(IGRID)%H=>H
      OBSBASDAT(IGRID)%HOBS=>HOBS
      OBSBASDAT(IGRID)%TOFF=>TOFF
      OBSBASDAT(IGRID)%ROFF=>ROFF
      OBSBASDAT(IGRID)%COFF=>COFF
      OBSBASDAT(IGRID)%OTIME=>OTIME
      OBSBASDAT(IGRID)%PR=>PR
      OBSBASDAT(IGRID)%RINT=>RINT
      OBSBASDAT(IGRID)%OBSNAM=>OBSNAM
C
      RETURN
      END
