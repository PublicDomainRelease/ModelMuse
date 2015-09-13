MODULE PCGN
  ! ... LAST MODIFIED:  R.L. NAFF,  FEBURARY, 2010
  ! ... MODULE BASED ON DE45, PCG2 AND LMG1, BY 
  ! ... A.W. HARBAUGH, M.C. HILL AND S.W. MEH, RESPECTIVELY.
  ! ...
  ! ... THIS MODULE SERVES AN THE INTERFACE BETWEEN THE PCG SOLVER
  ! ... AND MODFLOW.  MODFLOW CALLS SUBROUTINES IN THIS MODULE, AND 
  ! ... THE MODULE CALLS APPROPRIATE SUBROUTINES IN THE PCG SOLVER.
  ! ...
  ! ... R.L. Naff 
  ! ...
  ! ... VERSION 2.0.3 October, 2011
  ! ... 
  ! ... 08/08: VERSION 2.0, FOR USE WITH MF2005
  ! ... 04/09: CORRECTED MINOR ERROR IN REPORTING NODAL LOCATION OF
  ! ...        OF MAXIMUN HEAD CHANGE.
  ! ... 02/10: CORRECTED MINOR ERROR ASSOCIATED WITH REPORTING OF 
  ! ...        NONCONVERGENCE (KITER=MO_ITER), SUBROUTINE SECONDARY_CLOSE.
  ! ... 10/11: REPLACED WHOLE-ARRAY OPERATION IN SUBROUTINE NONLINEAR WITH
  ! ...        BLAS-LIKE SUBROUTINE CALL; SEE SUBROUTINE UPDATE_HEAD.
  ! ... 
  USE GLOBAL, ONLY: IOUT, NCOL, NROW, NLAY, NODES
  ! ... MODULE GLOBAL DEFINED IN gwf2bas7.f
!  USE PCG_MAIN
  IMPLICIT NONE
  TYPE PCGNTYPE
     INTEGER, POINTER  :: FILL, MO_ITER, MI_ITER, PRGUNIT, PC_UNIT, &
          TS_UNIT, CNVG_A, DAMP_A
     REAL(KIND=8), POINTER :: SAV_DAMP, DRELAX, MAG_CLOSE, SAV_CLOSE, &
          C_RATE, D_RATE, RCLOSE, HCLOSE, LIMHDCHG, MIN_DAMP, MIN_CLOSE
     REAL(KIND=8), DIMENSION(:), POINTER :: DD, DX, DY, DZ, RES, HCH
  END TYPE PCGNTYPE
  TYPE(PCGNTYPE), DIMENSION(1:10), TARGET ::PCGNDAT
  TYPE(PCGNTYPE), POINTER ::PT
  ! ...
  INTEGER, SAVE :: MGRID=0, KITER, KSTP, KPER
  REAL :: HCLOSEPCGN
  REAL(KIND=8), PARAMETER :: ZERO=0.D0, ONE=1.D0, TWO=2.D0, &     
       THREE=3.D0, FOUR=4.D0, FIVE=5.D0, SEVEN=7.D0, EIGHT=8.D0, &
       TEN=1.D1, HUNDRED=1.D2, SQRTEN=3.16227766017, &     
       HALF=0.5D0, QUARTER=0.25D0, TENTH=0.1D0, HUNDRETH=.01D0
  REAL(KIND=8), PARAMETER :: MZ=TINY(ZERO), LARGE=HUGE(1.0_8)
  REAL(KIND=8), PARAMETER :: SMALL=EPSILON(1.0_8)*HUNDRED
  ! ... POINTERS
  INTEGER, DIMENSION(:), POINTER :: IBOUND
  REAL, DIMENSION(:), POINTER :: CR, CC, CV, HCOF, RHS
  REAL(KIND=8), DIMENSION(:), POINTER :: HNEW
  PRIVATE; PUBLIC :: PCGN2AR, PCGN2DA, HCLOSEPCGN
  ! ... HCLOSEPCGN NEEDED IN gwf2mnw7.f
CONTAINS

  SUBROUTINE PCGN2AR(IN,FREE_FMT,ITER_MO,IGRID) 
    ! ... 
    ! ... ***************************************************************
    ! ... PURPOSE: READ INFO FOR PCG SOLVER AND PICARD ALGORITHM.
    ! ... ***************************************************************
    ! ... 
    ! ... SPECIFICATIONS:
    ! ... ---------------------------------------------------------------
    ! ...    ARGUMENT LIST
    ! ... ---------------------------------------------------------------
    INTEGER, INTENT(IN) :: IN, FREE_FMT, IGRID
    INTEGER, INTENT(OUT) :: ITER_MO
    ! ... ---------------------------------------------------------------
    ! ...    LOCAL VARIABLES
    ! ... ---------------------------------------------------------------
    INTEGER :: ITER_MI, IPUNIT, IFILL, UNIT_PC, UNIT_TS, ADAMP, &
         ACNVG, MCNVG, L_COUNT, IOS, IERR
    REAL :: DAMP, DAMP_LB, CNVG_LB, RELAX, RATE_C, RATE_D, CHGLIMIT, &
         CLOSE_R, CLOSE_H
    CHARACTER(LEN=12) :: C_VAL1, C_VAL2, C_VAL3, C_VAL4
    CHARACTER(LEN=1) :: CHECK
    CHARACTER(LEN=256) :: CHAR_STRING
    CHARACTER(LEN=80), DIMENSION(1:4) :: DATA_STRING
    !$ INTEGER :: N_PROC
    !$ INTEGER, EXTERNAL :: OMP_GET_NUM_PROCS
    !$ LOGICAL, SAVE :: INIT=.TRUE.
    ! ... ===============================================================
    ! ... OPEN MP DYNAMIC THREAD ADJUSTMENT 
    ! ... OPEN MP COMPILER FLAG MUST BE SET TO ACTIVATE PARALLELISM
    ! ...
    !$ IF (INIT) THEN
    !$    N_PROC=OMP_GET_NUM_PROCS()
    !$    CALL OMP_SET_NUM_THREADS(N_PROC)
    !$    INIT=.FALSE.
    !$ ENDIF
    ! ...
    ! ... PRINT A MESSAGE IDENTIFYING PCGN PACKAGE
!    WRITE(IOUT,300)
!300 FORMAT(1X,/1X,'PCG7AR -- PCGN INTERFACE PACKAGE', &     
!         ', VERSION 2.0, 08/2008')
    WRITE(IOUT, *) 'PCGN:';
    ! ... 
    ! ... STRIP OUT COMMENT CARDS (#) AND WRITE DATA TO DATA_STRING ARRAY
    ! ... 
    L_COUNT=0
    DO
       READ(UNIT=IN,FMT='(A)',IOSTAT=IOS) CHAR_STRING
       IF (IOS<0) THEN ! EOF
          EXIT
       ELSEIF (IOS>0) THEN
          WRITE(IOUT,*) '*** BAD RECORD ENCOUNTERED, PCGN DATA INPUT ***'
          STOP
       ELSE
          IF (LEN_TRIM(CHAR_STRING)==0) CYCLE
          CHECK=CHAR_STRING
          IF (CHECK=='#') then
		    if (L_COUNT.eq.0) then
		      WRITE(IOUT,*) CHAR_STRING
			endif
		    CYCLE
		  ENDIF
          L_COUNT=L_COUNT+1
          DATA_STRING(L_COUNT)=TRIM(CHAR_STRING)
          ! ... READ MAXIMUM OF FOUR DATA RECORDS
          IF (L_COUNT==4) EXIT
       ENDIF
    ENDDO
    ! ... READ IFILL,MO_ITER,MI_ITER,CLOSE_R,CLOSE_H,RELAX,UNIT_PC,UNIT_TS,
    ! ...  ADAMP,DAMP,DAMP_LB,RATE_D,CHGLIMIT,ACNVG,CNVG_LB,MCNVG,RATE_C,IPUNIT
    IF(FREE_FMT.EQ.0) THEN
       READ (UNIT=DATA_STRING(1),FMT=400,IOSTAT=IOS) ITER_MO, ITER_MI, &
            CLOSE_R, CLOSE_H
       IF (IOS/=0) THEN
          WRITE (IOUT,800) 'TER_MO, ITER_MI, CLOSE_R, CLOSE_H', DATA_STRING(1)
          STOP
       ENDIF
	   WRITE(IOUT,*) 'ITER_MO, ITER_MI, CLOSE_R, CLOSE_H:'
	   WRITE(IOUT,*) ITER_MO, ITER_MI, CLOSE_R, CLOSE_H


400    FORMAT (2I10,2F10.0)
       READ (UNIT=DATA_STRING(2),FMT=405,IOSTAT=IOS) RELAX, IFILL, &
            UNIT_PC, UNIT_TS
       IF (IOS/=0) THEN
          WRITE (IOUT,800) 'RELAX, IFILL, UNIT_PC, UNIT_TS', DATA_STRING(2)
          STOP
       ENDIF
	   WRITE(IOUT,*) 'RELAX, IFILL, UNIT_PC, UNIT_TS:'
	   WRITE(IOUT,*) RELAX, IFILL, UNIT_PC, UNIT_TS
405    FORMAT (F10.0,3I10)
       IF (ITER_MO>1) THEN
          ! ... NONLINEAR DAMPING
          READ (UNIT=DATA_STRING(3),FMT=410,IOSTAT=IOS) ADAMP, DAMP, &
               DAMP_LB, RATE_D, CHGLIMIT
          IF (IOS/=0) THEN
             WRITE (IOUT,800) 'ADAMP, DAMP, DAMP_LB, RATE_D, CHGLIMIT', &
                  DATA_STRING(3)
             STOP
          ENDIF
		  WRITE(IOUT,*) 'ADAMP, DAMP, DAMP_LB, RATE_D, CHGLIMIT:'
		  WRITE(IOUT,*) ADAMP, DAMP, DAMP_LB, RATE_D, CHGLIMIT
410       FORMAT (I10,4F10.0)
          ! ... NONLINEAR CONVERGENCE
          READ (UNIT=DATA_STRING(4),FMT=415,IOSTAT=IOS) ACNVG, CNVG_LB, &
               MCNVG, RATE_C, IPUNIT
          IF (IOS/=0) THEN
             WRITE (IOUT,800) 'ACNVG, CNVG_LB, MCNVG, RATE_C, IPUNIT', &
                  DATA_STRING(4)
             STOP
          ENDIF
		  WRITE(IOUT,*) 'ACNVG, CNVG_LB, MCNVG, RATE_C, IPUNIT:'
		  WRITE(IOUT,*) ACNVG, CNVG_LB, MCNVG, RATE_C, IPUNIT
415       FORMAT (I10,F10.0,I10,F10.0,I10)
       ELSE
          ! ... LINEAR DEFAULT VALUES
          ADAMP=0;ACNVG=0;DAMP=ONE;DAMP_LB=ZERO;RATE_C=ZERO
          CNVG_LB=ZERO;MCNVG=1;RATE_D=ZERO;CHGLIMIT=ZERO;IPUNIT=0
       ENDIF
    ELSE
       READ (UNIT=DATA_STRING(1),FMT=*,IOSTAT=IOS) ITER_MO,ITER_MI, &
            CLOSE_R,CLOSE_H
       IF (IOS/=0) THEN
          WRITE (IOUT,800) 'TER_MO,ITER_MI,CLOSE_R,CLOSE_H', DATA_STRING(1)
          STOP
       ENDIF
	   WRITE(IOUT,*) 'ITER_MO, ITER_MI, CLOSE_R, CLOSE_H:'
	   WRITE(IOUT,*) ITER_MO, ITER_MI, CLOSE_R, CLOSE_H
       READ (UNIT=DATA_STRING(2),FMT=*,IOSTAT=IOS)  RELAX, IFILL, &
            UNIT_PC,UNIT_TS
       IF (IOS/=0) THEN
          WRITE (IOUT,800) 'RELAX, IFILL, UNIT_PC, UNIT_TS', DATA_STRING(2)
          STOP
       ENDIF
	   WRITE(IOUT,*) 'RELAX, IFILL, UNIT_PC, UNIT_TS:'
	   WRITE(IOUT,*) RELAX, IFILL, UNIT_PC, UNIT_TS
       IF (ITER_MO>1) THEN
          ! ... NONLINEAR DAMPING
          READ (UNIT=DATA_STRING(3),FMT=*,IOSTAT=IOS) ADAMP, DAMP, &
               DAMP_LB, RATE_D, CHGLIMIT
          IF (IOS/=0) THEN
             WRITE (IOUT,800) 'ADAMP, DAMP, DAMP_LB, RATE_D, CHGLIMIT', &
                  DATA_STRING(3)
             STOP
          ENDIF
		  WRITE(IOUT,*) 'ADAMP, DAMP, DAMP_LB, RATE_D, CHGLIMIT:'
		  WRITE(IOUT,*) ADAMP, DAMP, DAMP_LB, RATE_D, CHGLIMIT
          ! ... NONLINEAR CONVERGENCE
          READ (UNIT=DATA_STRING(4),FMT=*,IOSTAT=IOS) ACNVG, CNVG_LB, &
               MCNVG, RATE_C, IPUNIT
          IF (IOS/=0) THEN
             WRITE (IOUT,800) 'ACNVG, CNVG_LB, MCNVG, RATE_C, IPUNIT', &
                  DATA_STRING(4)
             STOP
          ENDIF
		  WRITE(IOUT,*) 'ACNVG, CNVG_LB, MCNVG, RATE_C, IPUNIT:'
		  WRITE(IOUT,*) ACNVG, CNVG_LB, MCNVG, RATE_C, IPUNIT
       ELSE
          ! ... LINEAR DEFAULT VALUES
          ADAMP=0;ACNVG=0;DAMP=ONE;DAMP_LB=ZERO;RATE_C=ZERO
          CNVG_LB=ZERO;MCNVG=1;RATE_D=ZERO;CHGLIMIT=ZERO;IPUNIT=0
       ENDIF
    ENDIF
    ! ... 
    ! ... PRINT TOTAL ARRAY STORAGE REQUIRED
!    IF (IFILL==0) THEN
!       WRITE (IOUT,450) 4*NODES
!    ELSE
!       WRITE (IOUT,450) 8*NODES
!    ENDIF
!450 FORMAT (1X,'TOTAL ARRAY STORAGE REQUIRED IS ',I7)
    ! ... 
    ! ... PRINT VALUES FOR INPUT VARIABLES
    ! ... 
    ! ... SOLVER PARAMETERS
!    WRITE (IOUT,500)
!500 FORMAT (1X,///,20X,'SOLUTION BY PCG', /, 8X,46('-'))
!    WRITE (IOUT,510) IFILL
!510 FORMAT (1X,7X,'FILL LEVEL OF UDU PRECONDITIONER =',I9)
!    WRITE (IOUT,515) RELAX
!515 FORMAT (1X,22X,'RELAXATION FACTOR =',G15.5)
!    WRITE (IOUT,520) CLOSE_R
!520 FORMAT (1X,7X,'RESIDUAL-BASED CLOSURE CRITERION =',G15.5)
!    WRITE (IOUT,525) CLOSE_H
!525 FORMAT (1X,9X,'MAXIMUM HEAD CLOSURE CRITERION =',G15.5)
!    WRITE (IOUT,530) ITER_MO
!530 FORMAT (1X,2X,'MAXIMUM NUMBER OF NONLINER ITERATIONS =',I9)
!    WRITE (IOUT,535) ITER_MI
!535 FORMAT (1X,7X,'MAXIMUM NUMBER OF PCG ITERATIONS =',I9)
    ! ... NONLINEAR PICARD PARAMETERS
!    IF (ITER_MO>1) THEN
!    ENDIF
    ! ...
    ! ... ALLOCATE POINTER SPACE
    ! ... 
    IERR=0; MGRID=MAX(IGRID,MGRID)
    DO
       ALLOCATE (PCGNDAT(IGRID)%FILL, PCGNDAT(IGRID)%MO_ITER, &
            PCGNDAT(IGRID)%MI_ITER, PCGNDAT(IGRID)%CNVG_A, &
            PCGNDAT(IGRID)%DAMP_A, PCGNDAT(IGRID)%PRGUNIT, &
            PCGNDAT(IGRID)%PC_UNIT, PCGNDAT(IGRID)%TS_UNIT, &
            PCGNDAT(IGRID)%SAV_DAMP, PCGNDAT(IGRID)%DRELAX, &
            PCGNDAT(IGRID)%MAG_CLOSE, PCGNDAT(IGRID)%SAV_CLOSE, &
            PCGNDAT(IGRID)%C_RATE, PCGNDAT(IGRID)%D_RATE, &
            PCGNDAT(IGRID)%RCLOSE, PCGNDAT(IGRID)%HCLOSE, &
            PCGNDAT(IGRID)%LIMHDCHG, PCGNDAT(IGRID)%MIN_DAMP, &
            PCGNDAT(IGRID)%MIN_CLOSE,STAT=IERR)
       IF (IERR/=0) EXIT
       ALLOCATE(PCGNDAT(IGRID)%DD(NODES),STAT=IERR)
       IF (IERR/=0) EXIT
       ALLOCATE(PCGNDAT(IGRID)%RES(NODES),STAT=IERR)
       IF (IERR/=0) EXIT
       IF (NCOL>1) THEN
          ALLOCATE(PCGNDAT(IGRID)%DX(NODES),STAT=IERR)
       ELSE
          ALLOCATE(PCGNDAT(IGRID)%DX(1),STAT=IERR)
       ENDIF
       IF (IERR/=0) EXIT
       IF (NROW>1) THEN
          ALLOCATE(PCGNDAT(IGRID)%DY(NODES),STAT=IERR)
       ELSE
          ALLOCATE(PCGNDAT(IGRID)%DY(1),STAT=IERR)
       ENDIF
       IF (IERR/=0) EXIT
       IF (NLAY>1) THEN
          ALLOCATE(PCGNDAT(IGRID)%DZ(NODES),STAT=IERR)
       ELSE
          ALLOCATE(PCGNDAT(IGRID)%DZ(1),STAT=IERR)
       ENDIF
       IF (IERR/=0) EXIT
       IF (ITER_MO>1) THEN
          ALLOCATE(PCGNDAT(IGRID)%HCH(NODES),STAT=IERR)
       ELSE
          ALLOCATE(PCGNDAT(IGRID)%HCH(1),STAT=IERR)
       ENDIF
       EXIT
    ENDDO
    IF (IERR/=0) THEN
       WRITE(IOUT,630) IERR
       WRITE(IOUT,640)
       CALL PCGN_DEALLOCATE(MGRID)
       STOP
    ENDIF
    PT=>PCGNDAT(IGRID)
    PT%SAV_CLOSE=TENTH
630 FORMAT (/,1X,' UNABLE TO ALLOCATE STORAGE REQUIRED FOR', & 
         ' PCG SOLVER, ERROR CODE: ',I6)
640 FORMAT (/,1X,' ERROR ENCOUNTERED IN SUBROUTINE PCGNRP, &
         &MODULE PCGN.')
    ! ... 
    ! ... INTERPRET AND SET INPUT PARAMETERS
    ! ...
    ! ... ASSIGN MAIN TRANSFER VARIABLES
    PT%MO_ITER=ITER_MO; PT%MI_ITER=ITER_MI; PT%RCLOSE=CLOSE_R
    PT%HCLOSE=CLOSE_H; PT%DRELAX=RELAX; PT%FILL=IFILL; 
    PT%PRGUNIT=IPUNIT;PT%PC_UNIT=UNIT_PC; PT%TS_UNIT=UNIT_TS
    HCLOSEPCGN=CLOSE_H
    ! ...
    ! ... ASSIGN PICARD ITERATION TRANSFER VARIABLES
    ! ...
    ! ... CONVERGENCE ADJUSTMENTS; SAV_CLOSE PRESET TO 0.1
    PT%CNVG_A=ACNVG; PT%MIN_CLOSE=CNVG_LB; PT%C_RATE=RATE_C
    PT%DAMP_A=ADAMP; PT%SAV_DAMP=DAMP; PT%MIN_DAMP=DAMP_LB; PT%D_RATE=RATE_D
    PT%MAG_CLOSE=PT%SAV_CLOSE; PT%LIMHDCHG=CHGLIMIT
    IF (ITER_MO==1) THEN
	  CONTINUE
!       WRITE (IOUT,700)
    ELSE
       SELECT CASE(PT%CNVG_A)
       CASE(0)
          ! ... NO ADJUSTMENT FOR PCG_CLOSE; DEFAULT VALUES USED
          PT%MIN_CLOSE=ZERO; PT%C_RATE=ZERO; MCNVG=0
       CASE(1)
          ! ... ADAPTIVE PCG CLOSURE BASED ON LINEARITY
          PT%C_RATE=ZERO; MCNVG=0
          IF (PT%MIN_CLOSE>=PT%SAV_CLOSE.OR.PT%MIN_CLOSE<=SMALL) THEN
             ! ... MIN_CLOSE SET TOO LARGE OR SMALL (NEGATIVE)
             PT%MIN_CLOSE=ZERO; PT%CNVG_A=0
             C_VAL1='ACNVG=0'; C_VAL2='CNVG_LB=0'; C_VAL3='MCNVG=0'
             C_VAL4='RATE_C=0'
!             WRITE (IOUT,810) TRIM(C_VAL1), TRIM(C_VAL2), TRIM(C_VAL3), &
!                  TRIM(C_VAL4)
          ENDIF
       CASE(2)
          ! ... ENHANCED PCG CLOSURE
          PT%MIN_CLOSE=ZERO
          IF (MCNVG<0.OR.(MCNVG>6.AND.PT%C_RATE>=ONE)) THEN
             ! ... MCNVG MUST BE > 0 FOR CNVG_A=2 OR CONFUSING INTENT
             PT%C_RATE=ZERO; PT%CNVG_A=0; MCNVG=0
             C_VAL1='ACNVG=0'; C_VAL2='CNVG_LB=0'; C_VAL3='MCNVG=0'
             C_VAL4='RATE_C=0'
!             WRITE (IOUT,810) TRIM(C_VAL1), TRIM(C_VAL2), TRIM(C_VAL3), &
!                  TRIM(C_VAL4)
          ELSE
             IF (MCNVG>6) THEN
                ! ... MCNVG SET TOO LARGE; RESET
                MCNVG=6
                C_VAL1='MCNVG=6'; C_VAL2=''; C_VAL3=''; C_VAL4=''
!                WRITE (IOUT,810) TRIM(C_VAL1)
             ENDIF
             ! ... SET MAG_CLOSE
             PT%MAG_CLOSE=PT%SAV_CLOSE**MCNVG
             IF (PT%C_RATE>=ONE) THEN
                ! ... C_RATE SET TOO LARGE; RESET
                PT%C_RATE=ZERO
                C_VAL1='CNVG_LB=0'; C_VAL2='RATE_C=0'; C_VAL3=''; C_VAL4=''
!                WRITE (IOUT,810) TRIM(C_VAL1), TRIM(C_VAL2)
             ENDIF
          ENDIF
       CASE DEFAULT
          PT%MIN_CLOSE=ZERO; PT%C_RATE=ZERO; PT%CNVG_A=0; MCNVG=0
          C_VAL1='ACNVG=0'; C_VAL2='CNVG_LB=0'; C_VAL3='MCNVG=0'
          C_VAL4='RATE_C=0'
!          WRITE (IOUT,810) TRIM(C_VAL1), TRIM(C_VAL2), TRIM(C_VAL3), &
!               TRIM(C_VAL4)
       END SELECT
       ! ... DAMPING ADJUSTMENTS
       ! ... STORE INPUT DAMPING VALUE FOR LATER USE
       IF (PT%SAV_DAMP<=ZERO.OR.PT%SAV_DAMP>ONE) THEN
          PT%SAV_DAMP=HALF; C_VAL1='DAMP=0.5'; C_VAL2=''; C_VAL3=''; C_VAL4=''
!          WRITE (IOUT,810) TRIM(C_VAL1), TRIM(C_VAL2), TRIM(C_VAL3), &
!               TRIM(C_VAL4)
       ENDIF
       SELECT CASE(PT%DAMP_A)
       CASE(0)
          ! ... NO ADJUSTMENT TO DAMP; DEFAULT VALUES USED
          PT%D_RATE=ZERO; PT%MIN_DAMP=ZERO; PT%LIMHDCHG=ZERO
       CASE(1,2)
          ! ... AUTO DAMPING OR ENHANCED DAMPING
          IF (PT%DAMP_A==1.AND.PT%LIMHDCHG<SMALL) PT%LIMHDCHG=LARGE
          IF (PT%MIN_DAMP<SMALL.OR.PT%MIN_DAMP>PT%SAV_DAMP.OR. &
               PT%D_RATE<SMALL.OR.PT%D_RATE>=ONE) THEN
             ! ... MIN_DAMP OR D_RATE OUT OF RANGE
             PT%D_RATE=ZERO; PT%DAMP_A=0; PT%MIN_DAMP=ZERO; PT%LIMHDCHG=ZERO
             C_VAL1='ADAMP=0'; C_VAL2='DAMP_LB=0'; C_VAL3='RATE_D=0'
             C_VAL4='CHGLIMIT=0'
!             WRITE (IOUT,810) TRIM(C_VAL1), TRIM(C_VAL2), TRIM(C_VAL3), &
!                  TRIM(C_VAL4)
          ENDIF
       CASE DEFAULT
          PT%D_RATE=ZERO; PT%DAMP_A=0; PT%MIN_DAMP=ZERO; PT%LIMHDCHG=ZERO
          C_VAL1='ADAMP=0'; C_VAL2='DAMP_LB=0'; C_VAL3='RATE_D=0'
          C_VAL4='CHGLIMIT=0'
!          WRITE (IOUT,810) TRIM(C_VAL1), TRIM(C_VAL2), TRIM(C_VAL3), &
!               TRIM(C_VAL4)
       END SELECT
       ! ... 
       ! ... PRINT STYLE, NONLINEAR SOLVE
       ! ... 
!       SELECT CASE(PT%CNVG_A)
!       CASE(0)
!          WRITE (IOUT,710)
!       CASE(1)
!          WRITE (IOUT,715)
!       CASE(2)
!          WRITE (IOUT,720)
!          IF (PT%C_RATE>SMALL) THEN
!             WRITE (IOUT,725)
!          ELSE
!             WRITE (IOUT,730)
!          ENDIF
!       END SELECT
       ! ... 
!       SELECT CASE(PT%DAMP_A)
!       CASE(0)
!          WRITE (IOUT,735)
!       CASE(1)
!          WRITE (IOUT,740)
!       CASE(2)
!          WRITE (IOUT,745)
!       END SELECT
    ENDIF
    NULLIFY(PT)
700 FORMAT (1X,10X,'LINEAR SOLUTION BY PCG ITERATION')
710 FORMAT (1X,10X,'PICARD: STANDARD RELATIVE CONVERGENCE, INNER ITERATION')
715 FORMAT (1X,10X,'PICARD: ADAPTIVE RELATIVE CONVERGENCE, &
         &INNER ITERATION')
720 FORMAT (1X,10X,'PICARD: ENHANCED RELATIVE CONVERGENCE, INNER ITERATION')
725 FORMAT (1X,10X,'PICARD: INITIAL ENHANCEMENT; INCREASING BY RATE_C')
730 FORMAT (1X,10X,'PICARD: CONSTANT ENHANCEMENT THROUGHOUT NONLINEAR SOLVE')
735 FORMAT (1X,10X,'PICARD: DAMPING HELD CONSTANT')
740 FORMAT (1X,10X,'PICARD: ADAPTIVE DAMPING')
745 FORMAT (1X,10X,'PICARD: INITIALLY ENHANCED DAMPING, INCREASING BY &
         &RATE_D')
    ! ...
800 FORMAT (1X,/,1X,10X,'***BAD DATA READ, SUBROUTINE PCGNRP***',/, & 
         1X,8X,'VARIABLES READ WERE: ',A40,/, &
         1X,8X,'DATA READ WERE: ', A50)
810 FORMAT (1X,/,1X,20X,'***WARNING!!!***',/, &
         1X,8X,'INCONSISTANT DATA HAS BEEN ENTERED',/, &
         1X,8X,'THE FOLLOWING VARIABLES HAVE BEEN RESET',/, &
         'TO DEFAULT VALUES: ', A12,', ', A12,', ', A12,', ', A12)
    ! ... 
  END SUBROUTINE PCGN2AR
  ! ... *****************************************************************

  ! ... *****************************************************************
!  SUBROUTINE PCGN2AP(HNEWAP,RHSAP,CRAP,CCAP,CVAP,HCOFAP,IBDAP, &  
!       KKITER,KKSTP,KKPER,ICNVG,HNOFLO,IGRID)
    ! ... 
    ! ... ***************************************************************
    ! ... PURPOSE: INTERFACE FOR PCG PACKAGE
    ! ...          CALLS SUBROUTINE PCG_INIT AND PCG
    ! ...          CONTAINS NON-LINEAR ITERATION ALGORTIHM
    ! ... ***************************************************************
  ! ... ***************************************************************

  ! ... ***************************************************************
!    SUBROUTINE LINEAR(MI_ITER,RCLOSE,MRKR,NITER,ICNVG,ERR_STAT)
      ! ... SOLVE LINEAR PROBLEM
  ! ... ***************************************************************

  ! ... ***************************************************************
!    SUBROUTINE NONLINEAR(MO_ITER,MI_ITER,CNVG_A,DAMP_A,IPUNIT,IGRID, &
!         RCLOSE,HCLOSE,SAV_DAMP,MAG_CLOSE,SAV_CLOSE,C_RATE,D_RATE, &
!         LIMHDCHG,MIN_DAMP,MIN_CLOSE,RTR,MRKR,NITER,ICNVG,ERR_STAT)
      ! ... SOLVE NONLINEAR PROBLEM BY PICARD ITERATION
      ! ... SET MI_ITER LOOSELY; INNER ITERATION CONSTRAINED BY
      ! ... SPECIFIED REDUCTION IN INITIAL L2 NORM (EPS_I).
  ! ... ***************************************************************

  ! ... ***************************************************************
!  SUBROUTINE CDS_RES(RTR, HNOFLO, IPUNIT, DD, DX, DY, DZ, RES)
    ! ...
    ! ... ******************************************************************
    ! ... PURPOSE: BUILD AND STORE MATRIX IN COMPRESSED DIAGONAL FORMAT; 
    ! ... FORM RESIDUAL VECTOR
    ! ... ******************************************************************
  ! ... ***************************************************************

  ! ... ***************************************************************
!  SUBROUTINE MAX_HCH(RTR, MXHCH, MHC_NODE, IB0_COUNT, L2HR)
    ! ...
    ! ... ******************************************************************
    ! ... PURPOSE: EXTRACT LARGEST ABSOLUTE HEAD CHANGE;
    ! ... FORM HEAD-CHANGE L2 NORM; PRINT MESSAGE
    ! ... ******************************************************************
  ! ... ***************************************************************

  ! ... ***************************************************************
!  SUBROUTINE SECONDARY_CLOSE(MXHCH,RCLOSE,HCLOSE,RTR,ICNVG,KITER,MO_ITER, &
!       IPUNIT)
    ! ...
    ! ... RLN:  MODIFIED 02/2010; ELIMINATED RETURNING ICNVG=-1 WHEN 
    ! ...         KITER=MO_ITER (OLD MF2000 SETTING).
    ! ... ACCEPTABLE VALUES FOR ICNVG WITH MF2005 ARE 0 (NO CONVERGENCE) 
    ! ... OR 1 (CONVERGENCE).
    ! ... ******************************************************************
    ! ... PURPOSE: SECONDARY SOLVER TERMINATION CHECK, NONLINEAR PROBLEM;
    ! ... CHECKS FOR CLOSURE AGAINST MAXIMUM HEAD CHANGE.
    ! ... RTR: SQUARE OF L2 NORM AT TERMINATION.
    ! ... ******************************************************************
  ! ... ***************************************************************

  ! ... ***************************************************************
!  SUBROUTINE DAMP_ADJ(DAMP_A,KITER,KSTP,MIN_DAMP,SAV_DAMP,EPS,MXHCH, &
!       D_RATE,CHGLIMIT,DAMP)
    ! ...
    ! ... RLN:  MODIFIED 02/2010; FOR CASE DAMP_A=1, VALUES OF RATE_D>0.1 
    ! ...         NOT PERMITTED -- RATE_D RESET TO 0.1
    ! ... ******************************************************************
    ! ... PURPOSE: ADJUST DAMPING PARAMETER IN RESPONSE TO PROGRESS IN
    ! ... CONVERGENCE OF NONLINEAR PROBLEM.
    ! ... ******************************************************************
  ! ... ***************************************************************

  ! ... ***************************************************************
!  SUBROUTINE PIVOT_CK(E_NODE)
    ! ...
    ! ... RLN:  MODIFIED 04/2009 FOR ERROR IN ROW-COLUMN INDEXING
    ! ... ******************************************************************
    ! ... PURPOSE: PRINT ZERO OR NEGATIVE PIVOT INFO.
    ! ... ******************************************************************
  ! ... ***************************************************************

  ! ... ***************************************************************
!  SUBROUTINE CK_DOMAIN
    ! ...
    ! ... ******************************************************************
    ! ... PURPOSE: CHECK DOMAIN FOR INTEGRITY
    ! ... ******************************************************************
  ! ... ***************************************************************

  ! ... ***************************************************************
!  SUBROUTINE RENUMBER (CK_D, NO_NODES,NEW_NO,OLD_NO)
  ! ... ***************************************************************

  ! ... ***************************************************************
  SUBROUTINE PCGN_DEALLOCATE(NGRID)
    INTEGER, INTENT(IN) :: NGRID
    INTEGER :: I
    DO I=1, NGRID
       CALL PCGN2DA(I)
    ENDDO
  END SUBROUTINE PCGN_DEALLOCATE
  ! ... ***************************************************************

  ! ... ***************************************************************
  SUBROUTINE PCGN2DA(IGRID)
    ! ... *************************************************************
    ! ... DEALLOCATE ALL ALLOCATIONS AT JOB TERMINATION
    ! ... *************************************************************
    INTEGER, INTENT(IN) :: IGRID
    IF (IGRID==1) THEN
!       CALL PCG_FIN
       NULLIFY(PT,CR,CC,CV,HCOF,RHS,HNEW,IBOUND)
    ENDIF
    IF (ASSOCIATED(PCGNDAT(IGRID)%FILL))     DEALLOCATE(PCGNDAT(IGRID)%FILL)
    IF (ASSOCIATED(PCGNDAT(IGRID)%MO_ITER))  DEALLOCATE(PCGNDAT(IGRID)%MO_ITER)
    IF (ASSOCIATED(PCGNDAT(IGRID)%MI_ITER))  DEALLOCATE(PCGNDAT(IGRID)%MI_ITER)
    IF (ASSOCIATED(PCGNDAT(IGRID)%CNVG_A))   DEALLOCATE(PCGNDAT(IGRID)%CNVG_A)
    IF (ASSOCIATED(PCGNDAT(IGRID)%DAMP_A))   DEALLOCATE(PCGNDAT(IGRID)%DAMP_A)
    IF (ASSOCIATED(PCGNDAT(IGRID)%PRGUNIT))  DEALLOCATE(PCGNDAT(IGRID)%PRGUNIT)
    IF (ASSOCIATED(PCGNDAT(IGRID)%PC_UNIT))  DEALLOCATE(PCGNDAT(IGRID)%PC_UNIT)
    IF (ASSOCIATED(PCGNDAT(IGRID)%TS_UNIT))  DEALLOCATE(PCGNDAT(IGRID)%TS_UNIT)
    IF (ASSOCIATED(PCGNDAT(IGRID)%SAV_DAMP)) &
         DEALLOCATE(PCGNDAT(IGRID)%SAV_DAMP)
    IF (ASSOCIATED(PCGNDAT(IGRID)%DRELAX ))  DEALLOCATE(PCGNDAT(IGRID)%DRELAX)
    IF (ASSOCIATED(PCGNDAT(IGRID)%MAG_CLOSE)) &
         DEALLOCATE(PCGNDAT(IGRID)%MAG_CLOSE)
    IF (ASSOCIATED(PCGNDAT(IGRID)%SAV_CLOSE)) &
         DEALLOCATE(PCGNDAT(IGRID)%SAV_CLOSE)
    IF (ASSOCIATED(PCGNDAT(IGRID)%C_RATE))   DEALLOCATE(PCGNDAT(IGRID)%C_RATE) 
    IF (ASSOCIATED(PCGNDAT(IGRID)%D_RATE))   DEALLOCATE(PCGNDAT(IGRID)%D_RATE)
    IF (ASSOCIATED(PCGNDAT(IGRID)%RCLOSE))   DEALLOCATE(PCGNDAT(IGRID)%RCLOSE) 
    IF (ASSOCIATED(PCGNDAT(IGRID)%HCLOSE))   DEALLOCATE(PCGNDAT(IGRID)%HCLOSE)
    IF (ASSOCIATED(PCGNDAT(IGRID)%LIMHDCHG)) &
         DEALLOCATE(PCGNDAT(IGRID)%LIMHDCHG) 
    IF (ASSOCIATED(PCGNDAT(IGRID)%MIN_DAMP)) DEALLOCATE(PCGNDAT(IGRID)%MIN_DAMP)
    IF (ASSOCIATED(PCGNDAT(IGRID)%MIN_CLOSE)) &
         DEALLOCATE(PCGNDAT(IGRID)%MIN_CLOSE)
    IF (ASSOCIATED(PCGNDAT(IGRID)%DD))       DEALLOCATE(PCGNDAT(IGRID)%DD)
    IF (ASSOCIATED(PCGNDAT(IGRID)%DX))       DEALLOCATE(PCGNDAT(IGRID)%DX)
    IF (ASSOCIATED(PCGNDAT(IGRID)%DY))       DEALLOCATE(PCGNDAT(IGRID)%DY)
    IF (ASSOCIATED(PCGNDAT(IGRID)%DZ))       DEALLOCATE(PCGNDAT(IGRID)%DZ)
    IF (ASSOCIATED(PCGNDAT(IGRID)%RES))      DEALLOCATE(PCGNDAT(IGRID)%RES)
    IF (ASSOCIATED(PCGNDAT(IGRID)%HCH))      DEALLOCATE(PCGNDAT(IGRID)%HCH)
    CLOSE(IOUT)
  END SUBROUTINE PCGN2DA
  ! ... ***************************************************************

  ! ... PROG SUBROUTINES: PRINT NONLINEAR PROGRESS TO CSV FILE
  ! ... MODIFIED FROM  MODULE MHC, AS CREATED BY E.R. BANTA
  ! ... VERSION 1.0 NOVEMBER 2007


!  SUBROUTINE PROG1AD(KPER,KSTP,IUNIT)
    !   Time-step advance

!  SUBROUTINE PROG1OT(KITER,IB0_COUNT,RATIO_L,L2HR,HEADCHGMAX,MHC_NODE, &
!       DAMPMHC,HLAST,HTHIS,IUNIT,NCOL,NROW)
    ! ... RLN:  MODIFIED 04/2009 FOR ERROR IN ROW-COLUMN INDEXING
    !   PRINT CONVERGENCE PROGRESS

!  SUBROUTINE PROG2OT(KITER,IB0_COUNT,L2HR,HEADCHGMAX,MHC_NODE, &
!       DAMPMHC,HLAST,HTHIS,IUNIT,NCOL,NROW)
    ! ... RLN:  MODIFIED 04/2009 FOR ERROR IN ROW-COLUMN INDEXING
    !   PRINT CONVERGENCE PROGRESS

!  SUBROUTINE UPDATE_HEAD(Y,X,R)
    ! ... MODEL: Y=Y+R*X
    ! ... Y, X: ARRAYS
    ! ... R: SCALAR

END MODULE PCGN


