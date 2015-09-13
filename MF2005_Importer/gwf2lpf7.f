      MODULE GWFLPFMODULE
        INTEGER, SAVE,   POINTER ::ILPFCB,IWDFLG,IWETIT,IHDWET
        INTEGER, SAVE,   POINTER ::ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC
        REAL,    SAVE,   POINTER ::WETFCT
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYTYP
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYAVG
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::CHANI
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYVKA
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYWET
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYSTRT
        INTEGER, SAVE,   POINTER, DIMENSION(:,:)   ::LAYFLG
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::VKA
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::VKCB
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::SC1
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::SC2
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::HANI
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::WETDRY
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::HK
      TYPE GWFLPFTYPE
        INTEGER, POINTER ::ILPFCB,IWDFLG,IWETIT,IHDWET
        INTEGER, POINTER ::ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC
        REAL, POINTER    ::WETFCT
        INTEGER,   POINTER, DIMENSION(:)     ::LAYTYP
        INTEGER,   POINTER, DIMENSION(:)     ::LAYAVG
        REAL,      POINTER, DIMENSION(:)     ::CHANI
        INTEGER,   POINTER, DIMENSION(:)     ::LAYVKA
        INTEGER,   POINTER, DIMENSION(:)     ::LAYWET
        INTEGER,   POINTER, DIMENSION(:)     ::LAYSTRT
        INTEGER,   POINTER, DIMENSION(:,:)   ::LAYFLG
        REAL,      POINTER, DIMENSION(:,:,:) ::VKA
        REAL,      POINTER, DIMENSION(:,:,:) ::VKCB
        REAL,      POINTER, DIMENSION(:,:,:) ::SC1
        REAL,      POINTER, DIMENSION(:,:,:) ::SC2
        REAL,      POINTER, DIMENSION(:,:,:) ::HANI
        REAL,      POINTER, DIMENSION(:,:,:) ::WETDRY
        REAL,      POINTER, DIMENSION(:,:,:) ::HK
      END TYPE
      TYPE(GWFLPFTYPE) GWFLPFDAT(10)
      END MODULE GWFLPFMODULE


      SUBROUTINE GWF2LPF7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE AND READ DATA FOR LAYER PROPERTY FLOW PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,ITRSS,LAYHDT,LAYHDS,LAYCBD,
     1                      NCNFBD,IBOUND,BUFF,BOTM,NBOTM,DELR,DELC,IOUT
      USE GWFBASMODULE,ONLY:HDRY
      USE GWFLPFMODULE,ONLY:ILPFCB,IWDFLG,IWETIT,IHDWET,
     1                      ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC,WETFCT,
     2                      LAYTYP,LAYAVG,CHANI,LAYVKA,LAYWET,LAYSTRT,
     3                      LAYFLG,VKA,VKCB,SC1,SC2,HANI,WETDRY,HK
C
      CHARACTER*14 LAYPRN(5),AVGNAM(3),TYPNAM(2),VKANAM(2),WETNAM(2),
     1            HANNAM
      DATA AVGNAM/'      HARMONIC','   LOGARITHMIC','     LOG-ARITH'/
      DATA TYPNAM/'      CONFINED','   CONVERTIBLE'/
      DATA VKANAM/'    VERTICAL K','    ANISOTROPY'/
      DATA WETNAM/'  NON-WETTABLE','      WETTABLE'/
      DATA HANNAM/'      VARIABLE'/
      CHARACTER*200 LINE
      CHARACTER*24 ANAME(9),STOTXT
      CHARACTER*4 PTYP
C
      DATA ANAME(1) /'   HYD. COND. ALONG ROWS'/
      DATA ANAME(2) /'  HORIZ. ANI. (COL./ROW)'/
      DATA ANAME(3) /'     VERTICAL HYD. COND.'/
      DATA ANAME(4) /' HORIZ. TO VERTICAL ANI.'/
      DATA ANAME(5) /'QUASI3D VERT. HYD. COND.'/
      DATA ANAME(6) /'        SPECIFIC STORAGE'/
      DATA ANAME(7) /'          SPECIFIC YIELD'/
      DATA ANAME(8) /'        WETDRY PARAMETER'/
      DATA ANAME(9) /'     STORAGE COEFFICIENT'/
C     ------------------------------------------------------------------
C1------Allocate scalar data.
      ALLOCATE(ILPFCB,IWDFLG,IWETIT,IHDWET)
      ALLOCATE(ISFAC,ICONCV,ITHFLG,NOCVCO,NOVFC)
      ALLOCATE(WETFCT)
      ZERO=0.
C
C2------IDENTIFY PACKAGE
      WRITE(IOUT, *) 'LPF:'
!      WRITE(IOUT,1) IN
!    1 FORMAT(1X,/1X,'LPF -- LAYER-PROPERTY FLOW PACKAGE, VERSION 7',
!     1', 5/2/2005',/,9X,'INPUT READ FROM UNIT ',I4)
C
C3------READ COMMENTS AND ITEM 1.
      CALL URDCOM(IN,IOUT,LINE)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ILPFCB,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HDRY,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPLPF,R,IOUT,IN)
	WRITE(IOUT, *) 'ILPFCB, HDRY, NPLPF:'
	WRITE(IOUT, *) ILPFCB, HDRY, NPLPF
C
C3A-----WRITE ITEM 1
C
C3B-----GET OPTIONS.
      ISFAC=0
      ICONCV=0
      ITHFLG=0
      NOCVCO=0
      NOVFC=0
      STOTXT=ANAME(6)
   20 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'STORAGECOEFFICIENT') THEN
         ISFAC=1
         STOTXT=ANAME(9)
!         WRITE(IOUT,21)
!   21    FORMAT(1X,'STORAGECOEFFICIENT OPTION:',/,
!     1     1X,'Read storage coefficient rather than specific storage')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'CONSTANTCV') THEN
         ICONCV=1
!         WRITE(IOUT,23)
!   23    FORMAT(1X,'CONSTANTCV OPTION:',/,1X,'Constant vertical',
!     1         ' conductance for convertible layers')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'THICKSTRT') THEN
         ITHFLG=1
!         WRITE(IOUT,25)
!   25    FORMAT(1X,'THICKSTRT OPTION:',/,1X,'Negative LAYTYP indicates',
!     1 ' confined layer with thickness computed from STRT-BOT')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOCVCORRECTION') THEN
         NOCVCO=1
!         WRITE(IOUT,27)
!   27    FORMAT(1X,'NOCVCORRECTION OPTION:',/,1X,
!     1    'Do not adjust vertical conductance when applying',
!     2              ' the vertical flow correction')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOVFC') THEN
         NOVFC=1
         NOCVCO=1
!         WRITE(IOUT,29)
!   29    FORMAT(1X,'NOVFC OPTION:',/,1X,
!     1    'Do not apply the vertical flow correction')
      END IF
      IF(LLOC.LT.200) GO TO 20
	WRITE(IOUT, *) 'ISFAC, ICONCV, ITHFLG, NOCVCO:'
	WRITE(IOUT, *) ISFAC, ICONCV, ITHFLG, NOCVCO
C
C4------ALLOCATE AND READ LAYTYP, LAYAVG, CHANI, LAYVKA, LAYWET, LAYSTRT.
      ALLOCATE(LAYTYP(NLAY))
      ALLOCATE(LAYAVG(NLAY))
      ALLOCATE(CHANI(NLAY))
      ALLOCATE(LAYVKA(NLAY))
      ALLOCATE(LAYWET(NLAY))
      ALLOCATE(LAYSTRT(NLAY))
      READ(IN,*) (LAYTYP(K),K=1,NLAY)
      READ(IN,*) (LAYAVG(K),K=1,NLAY)
      READ(IN,*) (CHANI(K),K=1,NLAY)
      READ(IN,*) (LAYVKA(K),K=1,NLAY)
      READ(IN,*) (LAYWET(K),K=1,NLAY)
C
C4A-----PRINT A TABLE OF VALUES FOR LAYTYP, LAYAVG, CHANI, LAYVKA, LAYWET.
!      WRITE(IOUT,47)
!   47 FORMAT(1X,/3X,'LAYER FLAGS:',/1X,
!     1 'LAYER       LAYTYP        LAYAVG         CHANI ',
!     2 '       LAYVKA        LAYWET',/1X,75('-'))
      DO 50 K=1,NLAY
      WRITE(IOUT,*) 
	1  'K,LAYTYP(K),LAYAVG(K),CHANI(K),LAYVKA(K),LAYWET(K):'
      WRITE(IOUT,*) K,LAYTYP(K),LAYAVG(K),CHANI(K),LAYVKA(K),LAYWET(K)
!   48 FORMAT(1X,I4,2I14,1PE14.3,2I14)
C
C4A1----SET GLOBAL HEAD-DEPENDENT TRANSMISSIVITY AND STORAGE FLAGS.
      IF (LAYTYP(K).NE.0) THEN
        LAYHDT(K)=1
        LAYHDS(K)=1
      ELSE
        LAYHDT(K)=0
        LAYHDS(K)=0
      ENDIF
   50 CONTINUE
C
C4A2----SET LAYSTRT AND RESET LAYTYP IF THICKSTRT OPTION IS ACTIVE.
      DO 60 K=1,NLAY
      LAYSTRT(K)=0
      IF(LAYTYP(K).LT.0 .AND. ITHFLG.NE.0) THEN
         LAYSTRT(K)=1
         LAYTYP(K)=0
         LAYHDT(K)=0
         LAYHDS(K)=0
!         WRITE(IOUT,57) K
!   57    FORMAT(1X,'Layer',I5,
!     1  ' is confined because LAYTYP<0 and THICKSTRT option is active')
      END IF
   60 CONTINUE
C
C4B-----BASED ON LAYTYP, LAYAVG, CHANI, LAYWET, COUNT THE NUMBER OF EACH
C4B-----TYPE OF 2-D ARRAY; CHECK VALUES FOR CONSISTENCY; AND SETUP
C4B-----POINTERS IN LAYTYP, CHANI, AND LAYWET FOR CONVENIENT ACCESS
C4B-----TO SC2, HANI, and WETDRY.  PRINT INTERPRETED VALUES OF FLAGS.
      NCNVRT=0
      NHANI=0
      NWETD=0
!      WRITE(IOUT,67)
!   67 FORMAT(1X,/3X,'INTERPRETATION OF LAYER FLAGS:',/1X,
!     1  '                       INTERBLOCK     HORIZONTAL',
!     2  '    DATA IN',/1X,
!     3  '        LAYER TYPE   TRANSMISSIVITY   ANISOTROPY',
!     4  '   ARRAY VKA   WETTABILITY',/1X,
!     5  'LAYER      (LAYTYP)      (LAYAVG)       (CHANI)',
!     6  '      (LAYVKA)      (LAYWET)',/1X,75('-'))
      DO 100 K=1,NLAY
      IF(LAYTYP(K).NE.0) THEN
         NCNVRT=NCNVRT+1
         LAYTYP(K)=NCNVRT
      END IF
      IF(CHANI(K).LE.ZERO) THEN
         NHANI=NHANI+1
         CHANI(K)=-NHANI
      END IF
      IF(LAYWET(K).NE.0) THEN
         IF(LAYTYP(K).EQ.0) THEN
            WRITE(IOUT,*)
     1          ' LAYWET is not 0 and LAYTYP is 0 for layer:',K
            WRITE(IOUT,*) ' LAYWET must be 0 if LAYTYP is 0'
            CALL USTOP(' ')
         ELSE
            NWETD=NWETD+1
            LAYWET(K)=NWETD
         END IF
      END IF
      IF(LAYAVG(K).LT.0 .OR. LAYAVG(K).GT.2) THEN
         WRITE(IOUT,74) LAYAVG(K)
   74    FORMAT(1X,I8,
     1    ' IS AN INVALID LAYAVG VALUE -- MUST BE 0, 1, or 2')
         CALL USTOP(' ')
      END IF
      LAYPRN(1)=TYPNAM(1)
      IF(LAYTYP(K).NE.0) LAYPRN(1)=TYPNAM(2)
      LAYPRN(2)=AVGNAM(LAYAVG(K)+1)
      IF(CHANI(K).LE.0) THEN
         LAYPRN(3)=HANNAM
      ELSE
!         WRITE(LAYPRN(3),'(1PE14.3)') CHANI(K)
      END IF
      LAYPRN(4)=VKANAM(1)
      IF(LAYVKA(K).NE.0) LAYPRN(4)=VKANAM(2)
      LAYPRN(5)=WETNAM(1)
      IF(LAYWET(K).NE.0) LAYPRN(5)=WETNAM(2)
!      WRITE(IOUT,78) K,(LAYPRN(I),I=1,5)
!   78 FORMAT(1X,I4,5A)
  100 CONTINUE
C
C4C-----PRINT WETTING INFORMATION.
      IF(NWETD.EQ.0) THEN
!         WRITE(IOUT,13)
!   13    FORMAT(1X,/,1X,'WETTING CAPABILITY IS NOT ACTIVE IN ANY LAYER')
         IWDFLG=0
      ELSE
!         WRITE(IOUT,12) NWETD
!   12    FORMAT(1X,/,1X,'WETTING CAPABILITY IS ACTIVE IN',I4,' LAYERS')
         IWDFLG=1
         READ(IN,*) WETFCT,IWETIT,IHDWET
         IF(IWETIT.LE.0) IWETIT=1
         WRITE(IOUT,*) 'WETFCT,IWETIT,IHDWET:'
         WRITE(IOUT,*) WETFCT,IWETIT,IHDWET
!         WRITE(IOUT,*) ' WETTING FACTOR=',WETFCT
!         WRITE(IOUT,*) ' WETTING ITERATION INTERVAL=',IWETIT
!         WRITE(IOUT,*) ' IHDWET=',IHDWET
      END IF
C
C5------ALLOCATE MEMORY FOR ARRAYS.
      ALLOCATE(LAYFLG(6,NLAY))
      ALLOCATE(HK(NCOL,NROW,NLAY))
      ALLOCATE(VKA(NCOL,NROW,NLAY))
      IF(NCNFBD.GT.0) THEN
         ALLOCATE(VKCB(NCOL,NROW,NCNFBD))
      ELSE
         ALLOCATE(VKCB(1,1,1))
      END IF
      IF(ITRSS.NE.0) THEN
         ALLOCATE(SC1(NCOL,NROW,NLAY))
      ELSE
         ALLOCATE(SC1(1,1,1))
      END IF
      IF(ITRSS.NE.0 .AND. NCNVRT.GT.0) THEN
         ALLOCATE(SC2(NCOL,NROW,NCNVRT))
      ELSE
         ALLOCATE(SC2(1,1,1))
      END IF
      IF(NHANI.GT.0) THEN
         ALLOCATE(HANI(NCOL,NROW,NHANI))
      ELSE
         ALLOCATE(HANI(1,1,1))
      END IF
      IF(NWETD.GT.0) THEN
         ALLOCATE(WETDRY(NCOL,NROW,NWETD))
      ELSE
         ALLOCATE(WETDRY(1,1,1))
      END IF
C
C6------READ PARAMETER DEFINITIONS
      NPHK=0
      NPVKCB=0
      NPVK=0
      NPVANI=0
      NPSS=0
      NPSY=0
      NPHANI=0
      IF(NPLPF.GT.0) THEN
!         WRITE(IOUT,115)
!  115    FORMAT(/,' PARAMETERS DEFINED IN THE LPF PACKAGE')
         DO 120 K=1,NPLPF
         CALL UPARARRRP(IN,IOUT,N,1,PTYP,1,0,-1)
C   Note that NPHK and the other NP variables in
C   this group are used only as flags, not counts
         IF(PTYP.EQ.'HK') THEN
            NPHK=1
         ELSE IF(PTYP.EQ.'HANI') THEN
C6A-----WHEN A HANI PARAMETER IS USED, THEN ALL HORIZONTAL ANISOTROPY
C6A-----MUST BE DEFINED USING PARAMETERS.  ENSURE THAT ALL CHANI <= 0
            DO 118 I = 1, NLAY
              IF (CHANI(I).GT.0.0) THEN
                WRITE(IOUT,117)
  117           FORMAT(/,
     &' ERROR: WHEN A HANI PARAMETER IS USED, CHANI FOR ALL LAYERS',/,
     &' MUST BE LESS THAN OR EQUAL TO 0.0 -- STOP EXECUTION',
     &' (GWF2LPF7AR)')
                CALL USTOP(' ')
              ENDIF
  118       CONTINUE
            NPHANI=1
         ELSE IF(PTYP.EQ.'VKCB') THEN
            NPVKCB=1
         ELSE IF(PTYP.EQ.'VK') THEN
            NPVK=1
!            CALL SGWF2LPF7CK(IOUT,N,'VK  ')
         ELSE IF(PTYP.EQ.'VANI') THEN
            NPVANI=1
!            CALL SGWF2LPF7CK(IOUT,N,'VANI')
         ELSE IF(PTYP.EQ.'SS') THEN
            NPSS=1
         ELSE IF(PTYP.EQ.'SY') THEN
            NPSY=1
         ELSE
            WRITE(IOUT,*) ' Invalid parameter type for LPF Package'
            CALL USTOP(' ')
         END IF
  120    CONTINUE
      END IF
C
C7------DEFINE DATA FOR EACH LAYER -- VIA READING OR NAMED PARAMETERS.
      DO 200 K=1,NLAY
      KK=K
C
C7A-----DEFINE HORIZONTAL HYDRAULIC CONDUCTIVITY (HK)
      IF(NPHK.EQ.0) THEN
         CALL U2DREL(HK(:,:,KK),ANAME(1),NROW,NCOL,KK,IN,IOUT)
      ELSE
         READ(IN,*) LAYFLG(1,K)
!         WRITE(IOUT,121) ANAME(1),K,LAYFLG(1,K)
!  121    FORMAT(1X,/1X,A,' FOR LAYER',I4,
!     1   ' WILL BE DEFINED BY PARAMETERS',/1X,'(PRINT FLAG=',I4,')')
!         CALL UPARARRSUB1(HK(:,:,KK),NCOL,NROW,KK,'HK',
!     1      IOUT,ANAME(1),LAYFLG(1,KK))
         CALL UPARARRCK(BUFF,IBOUND,IOUT,K,NCOL,NLAY,NROW,'HK  ')
      END IF
C
C7B-----READ HORIZONTAL ANISOTROPY IF CHANI IS NON-ZERO
      IF(CHANI(K).LE.ZERO) THEN
        KHANI=-CHANI(K)
        IF(NPHANI.EQ.0) THEN
           CALL U2DREL(HANI(:,:,KHANI),ANAME(2),NROW,NCOL,KK,IN,IOUT)
        ELSE
           READ(IN,*) LAYFLG(6,K)
!           WRITE(IOUT,121) ANAME(2),K,LAYFLG(6,K)
!           CALL UPARARRSUB1(HANI(:,:,KHANI),NCOL,NROW,KK,'HANI',
!     &      IOUT,ANAME(2),LAYFLG(6,KK))
           CALL UPARARRCK(BUFF,IBOUND,IOUT,K,NCOL,NLAY,NROW,'HANI')
        END IF
      END IF
C
C7C-----DEFINE VERTICAL HYDRAULIC CONDUCTIVITY OR HORIZONTAL TO VERTICAL
C7C-----ANISOTROPY (VKA).
      IANAME=3
      PTYP='VK'
      IF(LAYVKA(K).NE.0) THEN
         IANAME=4
         PTYP='VANI'
      END IF
      IF(NPVK.EQ.0 .AND. NPVANI.EQ.0) THEN
         CALL U2DREL(VKA(:,:,KK),ANAME(IANAME),NROW,NCOL,KK,IN,IOUT)
      ELSE
         READ(IN,*) LAYFLG(2,K)
!         WRITE(IOUT,121) ANAME(IANAME),K,LAYFLG(2,K)
!         CALL UPARARRSUB1(VKA(:,:,KK),NCOL,NROW,KK,PTYP,IOUT,
!     &                       ANAME(IANAME),LAYFLG(2,KK))
         CALL UPARARRCK(BUFF,IBOUND,IOUT,K,NCOL,NLAY,NROW,PTYP)
      END IF
C
C7D-----DEFINE SPECIFIC STORAGE OR STORAGE COEFFICIENT IN ARRAY SC1 IF TRANSIENT.
      IF(ITRSS.NE.0) THEN
         IF(NPSS.EQ.0) THEN
            CALL U2DREL(SC1(:,:,KK),STOTXT,NROW,NCOL,KK,IN,IOUT)
         ELSE
            READ(IN,*) LAYFLG(3,K)
!            WRITE(IOUT,121) STOTXT,K,LAYFLG(3,K)
!            CALL UPARARRSUB1(SC1(:,:,KK),NCOL,NROW,KK,'SS',
!     1           IOUT,STOTXT,LAYFLG(3,KK))
            CALL UPARARRCK(BUFF,IBOUND,IOUT,K,NCOL,NLAY,NROW,'SS  ')
         END IF
!         IF(ISFAC.EQ.0) THEN
!            CALL SGWF2LPF7SC(SC1(:,:,KK),KK,1)
!         ELSE
!            CALL SGWF2LPF7SC(SC1(:,:,KK),KK,0)
!         END IF
      END IF
C
C7E-----DEFINE SPECIFIC YIELD IN ARRAY SC2 IF TRANSIENT AND LAYER IS
C7E-----IS CONVERTIBLE.
      IF(LAYTYP(K).NE.0) THEN
         IF(ITRSS.NE.0) THEN
            IF(NPSY.EQ.0) THEN
               CALL U2DREL(SC2(:,:,LAYTYP(K)),ANAME(7),NROW,NCOL,KK,IN,
     1                 IOUT)
            ELSE
               READ(IN,*) LAYFLG(4,K)
!               WRITE(IOUT,121) ANAME(7),K,LAYFLG(4,K)
!               CALL UPARARRSUB1(SC2(:,:,LAYTYP(K)),NCOL,
!     1         NROW,KK,'SY',IOUT,ANAME(7),LAYFLG(4,KK))
               CALL UPARARRCK(BUFF,IBOUND,IOUT,K,NCOL,NLAY,NROW,'SY  ')
            END IF
!            CALL SGWF2LPF7SC(SC2(:,:,LAYTYP(K)),KK,0)
         END IF
      END IF
C
C7F-----READ CONFINING BED VERTICAL HYDRAULIC CONDUCTIVITY (VKCB)
      IF(LAYCBD(K).NE.0) THEN
         IF(NPVKCB.EQ.0) THEN
            CALL U2DREL(VKCB(:,:,LAYCBD(K)),ANAME(5),NROW,NCOL,KK,IN,
     1             IOUT)
         ELSE
            READ(IN,*) LAYFLG(5,K)
!            WRITE(IOUT,121) ANAME(5),K,LAYFLG(5,K)
!            CALL UPARARRSUB1(VKCB(:,:,LAYCBD(K)),NCOL,NROW,KK,
!     1         'VKCB',IOUT,ANAME(5),LAYFLG(5,KK))
            CALL UPARARRCK(BUFF,IBOUND,IOUT,K,NCOL,NLAY,NROW,'VKCB')
         END IF
      END IF
C
C7G-----READ WETDRY CODES IF WETTING CAPABILITY HAS BEEN INVOKED
C7G-----(LAYWET NOT 0).
      IF(LAYWET(K).NE.0) THEN
         CALL U2DREL(WETDRY(:,:,LAYWET(K)),ANAME(8),NROW,NCOL,KK,IN,
     1            IOUT)
      END IF
  200 CONTINUE
C
C8------PREPARE AND CHECK LPF DATA.
!      CALL SGWF2LPF7N()
C
C9------RETURN
      CALL GWF2LPF7PSV(IGRID)
      RETURN
      END
!      SUBROUTINE GWF2LPF7AD(KPER,IGRID)
C     ******************************************************************
C     SET HOLD TO BOTM WHENEVER A WETTABLE CELL IS DRY
C     ******************************************************************
!      SUBROUTINE GWF2LPF7FM(KITER,KSTP,KPER,IGRID)
C     ******************************************************************
C     ADD LEAKAGE CORRECTION AND STORAGE TO HCOF AND RHS, AND CALCULATE
C     CONDUCTANCE AS REQUIRED.
C     ******************************************************************
!      SUBROUTINE SGWF2LPF7N()
C     ******************************************************************
C     INITIALIZE AND CHECK LPF DATA
C     ******************************************************************
!      SUBROUTINE GWF2LPF7BDADJ(KSTP,KPER,IDIR,IBDRET,
!     1                      IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
C     ******************************************************************
C     COMPUTE FLOW BETWEEN ADJACENT CELLS IN A SUBREGION OF THE GRID
C     ******************************************************************
!      SUBROUTINE GWF2LPF7BDS(KSTP,KPER,IGRID)
C     ******************************************************************
C     COMPUTE STORAGE BUDGET FLOW TERM FOR LPF.
C     ******************************************************************
!      SUBROUTINE GWF2LPF7BDCH(KSTP,KPER,IGRID)
C     ******************************************************************
C     COMPUTE FLOW FROM CONSTANT-HEAD CELLS
C     ******************************************************************
!      SUBROUTINE SGWF2LPF7SC(SC,K,ISPST)
C     ******************************************************************
C     COMPUTE STORAGE CAPACITY
C     ******************************************************************
!      SUBROUTINE SGWF2LPF7HCOND(K,KITER,KSTP,KPER)
C     ******************************************************************
C     COMPUTE HORIZONTAL BRANCH CONDUCTANCE FOR ONE LAYER.
C     ******************************************************************
!      SUBROUTINE SGWF2LPF7WET(K,KITER,KSTP,KPER,IHDCNV,NCNVRT,
!     1                        ICNVRT,JCNVRT,ACNVRT)
C
C     ******************************************************************
C     CONVERT DRY CELLS TO WET.
C     ******************************************************************
!      SUBROUTINE SGWF2LPF7WDMSG(ICODE,NCNVRT,ICNVRT,JCNVRT,ACNVRT,
!     1             IHDCNV,IOUT,KITER,J,I,K,KSTP,KPER,NCOL,NROW)
C     ******************************************************************
C     PRINT MESSAGE WHEN CELLS CONVERT BETWEEN WET AND DRY.
C     ******************************************************************
!      SUBROUTINE SGWF2LPF7HHARM(K)
C     ******************************************************************
C     COMPUTE HORIZONTAL BRANCH CONDUCTANCE USING HARMONIC MEAN OF BLOCK
C     CONDUCTANCES (DISTANCE WEIGHTED HARMONIC MEAN OF TRANSMISSIVITY).
C     CELL THICKNESS IS IN CC UPON ENTRY.
C     ******************************************************************
!      SUBROUTINE SGWF2LPF7HLOG(K)
C     ******************************************************************
C-----COMPUTE HORIZONTAL CONDUCTANCE USING LOGARITHMIC MEAN
C-----TRANSMISSIVITY -- ACTIVATED BY LAYAVG=1
C-----CELL SATURATED THICKNESS IS IN CC.
C     ******************************************************************
!      SUBROUTINE SGWF2LPF7HUNCNF(K)
C     ******************************************************************
C-----COMPUTE HORIZONTAL CONDUCTANCE USING ARITHMETIC MEAN SATURATED
C-----THICKNESS AND LOGARITHMIC MEAN HYDRAULIC CONDUCTIVITY.
C-----CELL SATURATED THICKNESS IS IN CC.
C-----ACTIVATED BY LAYAVG=2
C     ******************************************************************
!      SUBROUTINE SGWF2LPF7VCOND(K)
C     ******************************************************************
C     COMPUTE VERTICAL BRANCH CONDUCTANCE BETWEEN A LAYER AND THE NEXT
C     LOWER LAYER FROM VERTICAL HYDRAULIC CONDUCTIVITY.
C     ******************************************************************
!      SUBROUTINE SGWF2LPF7CK(IOUT,NP,PTYP)
C     ******************************************************************
C     CHECK THAT JUST-DEFINED PARAMETER OF TYPE 'VK' OR 'VANI' IS USED
C     CONSISTENTLY WITH LAYVKA ENTRIES FOR LAYERS LISTED IN CLUSTERS FOR
C     THE PARAMETER
C     ******************************************************************
      SUBROUTINE GWF2LPF7DA(IGRID)
C  Deallocate LPF DATA
      USE GWFLPFMODULE
C
        DEALLOCATE(GWFLPFDAT(IGRID)%ILPFCB)
        DEALLOCATE(GWFLPFDAT(IGRID)%IWDFLG)
        DEALLOCATE(GWFLPFDAT(IGRID)%IWETIT)
        DEALLOCATE(GWFLPFDAT(IGRID)%IHDWET)
        DEALLOCATE(GWFLPFDAT(IGRID)%ISFAC)
        DEALLOCATE(GWFLPFDAT(IGRID)%ICONCV)
        DEALLOCATE(GWFLPFDAT(IGRID)%ITHFLG)
        DEALLOCATE(GWFLPFDAT(IGRID)%NOCVCO)
        DEALLOCATE(GWFLPFDAT(IGRID)%NOVFC)
        DEALLOCATE(GWFLPFDAT(IGRID)%WETFCT)
        DEALLOCATE(GWFLPFDAT(IGRID)%LAYTYP)
        DEALLOCATE(GWFLPFDAT(IGRID)%LAYAVG)
        DEALLOCATE(GWFLPFDAT(IGRID)%CHANI)
        DEALLOCATE(GWFLPFDAT(IGRID)%LAYVKA)
        DEALLOCATE(GWFLPFDAT(IGRID)%LAYWET)
        DEALLOCATE(GWFLPFDAT(IGRID)%LAYSTRT)
        DEALLOCATE(GWFLPFDAT(IGRID)%LAYFLG)
        DEALLOCATE(GWFLPFDAT(IGRID)%VKA)
        DEALLOCATE(GWFLPFDAT(IGRID)%VKCB)
        DEALLOCATE(GWFLPFDAT(IGRID)%SC1)
        DEALLOCATE(GWFLPFDAT(IGRID)%SC2)
        DEALLOCATE(GWFLPFDAT(IGRID)%HANI)
        DEALLOCATE(GWFLPFDAT(IGRID)%WETDRY)
        DEALLOCATE(GWFLPFDAT(IGRID)%HK)
C
      RETURN
      END
      SUBROUTINE SGWF2LPF7PNT(IGRID)
C  Point to LPF data for a grid.
      USE GWFLPFMODULE
C
        ILPFCB=>GWFLPFDAT(IGRID)%ILPFCB
        IWDFLG=>GWFLPFDAT(IGRID)%IWDFLG
        IWETIT=>GWFLPFDAT(IGRID)%IWETIT
        IHDWET=>GWFLPFDAT(IGRID)%IHDWET
        ISFAC=>GWFLPFDAT(IGRID)%ISFAC
        ICONCV=>GWFLPFDAT(IGRID)%ICONCV
        ITHFLG=>GWFLPFDAT(IGRID)%ITHFLG
        NOCVCO=>GWFLPFDAT(IGRID)%NOCVCO
        NOVFC=>GWFLPFDAT(IGRID)%NOVFC
        WETFCT=>GWFLPFDAT(IGRID)%WETFCT
        LAYTYP=>GWFLPFDAT(IGRID)%LAYTYP
        LAYAVG=>GWFLPFDAT(IGRID)%LAYAVG
        CHANI=>GWFLPFDAT(IGRID)%CHANI
        LAYVKA=>GWFLPFDAT(IGRID)%LAYVKA
        LAYWET=>GWFLPFDAT(IGRID)%LAYWET
        LAYSTRT=>GWFLPFDAT(IGRID)%LAYSTRT
        LAYFLG=>GWFLPFDAT(IGRID)%LAYFLG
        VKA=>GWFLPFDAT(IGRID)%VKA
        VKCB=>GWFLPFDAT(IGRID)%VKCB
        SC1=>GWFLPFDAT(IGRID)%SC1
        SC2=>GWFLPFDAT(IGRID)%SC2
        HANI=>GWFLPFDAT(IGRID)%HANI
        WETDRY=>GWFLPFDAT(IGRID)%WETDRY
        HK=>GWFLPFDAT(IGRID)%HK
C
      RETURN
      END
      SUBROUTINE GWF2LPF7PSV(IGRID)
C  Save LPF data for a grid.
      USE GWFLPFMODULE
C
        GWFLPFDAT(IGRID)%ILPFCB=>ILPFCB
        GWFLPFDAT(IGRID)%IWDFLG=>IWDFLG
        GWFLPFDAT(IGRID)%IWETIT=>IWETIT
        GWFLPFDAT(IGRID)%IHDWET=>IHDWET
        GWFLPFDAT(IGRID)%ISFAC=>ISFAC
        GWFLPFDAT(IGRID)%ICONCV=>ICONCV
        GWFLPFDAT(IGRID)%ITHFLG=>ITHFLG
        GWFLPFDAT(IGRID)%NOCVCO=>NOCVCO
        GWFLPFDAT(IGRID)%NOVFC=>NOVFC
        GWFLPFDAT(IGRID)%WETFCT=>WETFCT
        GWFLPFDAT(IGRID)%LAYTYP=>LAYTYP
        GWFLPFDAT(IGRID)%LAYAVG=>LAYAVG
        GWFLPFDAT(IGRID)%CHANI=>CHANI
        GWFLPFDAT(IGRID)%LAYVKA=>LAYVKA
        GWFLPFDAT(IGRID)%LAYWET=>LAYWET
        GWFLPFDAT(IGRID)%LAYSTRT=>LAYSTRT
        GWFLPFDAT(IGRID)%LAYFLG=>LAYFLG
        GWFLPFDAT(IGRID)%VKA=>VKA
        GWFLPFDAT(IGRID)%VKCB=>VKCB
        GWFLPFDAT(IGRID)%SC1=>SC1
        GWFLPFDAT(IGRID)%SC2=>SC2
        GWFLPFDAT(IGRID)%HANI=>HANI
        GWFLPFDAT(IGRID)%WETDRY=>WETDRY
        GWFLPFDAT(IGRID)%HK=>HK
C
      RETURN
      END
