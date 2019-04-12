      subroutine ZIRMA
     $(W,IW,N,K,NLTE,TNU,SN,YHZ,MUX,YY,KODE,MYX,TMUN,WVL,DL,DDL,INDL,
     $ LDL,WVNUM,WTAB,RES,KRES,ZHZ,TCI,KTI,KILROY,Z,TE,ISB1,DODIDH,
     $ SVDIDH,JSAV,LEGEND,VEC,NO,IJECT)
C
C     Rudolf Loeser, 2000 Jul 21
C---- Computes and prints a line profile.
C     (This is version 4 of ZIRMA.)
C     !DASH
      save
C     !DASH
      real*8 DDL, DL, RES, SN, TCI, TE, TMUN, TNU, VEC, W, WTAB, WVL,
     $       WVNUM, YHZ, YY, Z, ZHZ
      integer IDIDH, IJECT, IL, IN, INDL, IS, ISB1, ISNSAV, ITMU, IU,
     $        IW, IWS, IWSSAV, IZTM, JSAV, K, KLIN, KODE, KRES, KTI,
     $        LDL, MOX, MUX, MYX, N, NLTE, NO
      logical DODIDH, KILROY, LEGEND, SVDIDH
C     !COM
C---- LOOPER      as of 2006 May 05
      integer     NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      real*8      EMOO,WVLTRN
      logical     EXPAND,SPHERE,VXZERO,FLOBRD
      common      /LOOPER1/ NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      common      /LOOPER2/ EMOO,WVLTRN
      common      /LOOPER3/ EXPAND,SPHERE,VXZERO,FLOBRD
C
C     Emergent Profiles calculations control data (simplified version).
C
C     NVEL   : number of velocity tables
C     NVY    : current value of velocity-loop index, 1.le.NVY.le.NVEL
C              (i.e. index of current velocity set)
C     JVEL   : code describing current velocity set (i.e. KVEL(NVY) )
C              =     1 : VSB
C              =     2 : VXS
C              =     3 : VADD     (from AMDIFF and/or VELGRAD)
C              = 100+j : VXN(j)
C              = 200+j : VXN(j) + VADD
C              = 300+j : VAX(j)
C              = 400+j : VFB(j)
C              = 500+j : VFB(j) + VADD
C
C     LFBV   : number of viewing positions (front only, or back also)
C     LFB    : current value of views-loop index, 1 .le. LFB .le. LFBV
C              = 1 - front-face
C              = 2 - back-face
C
C     MF     : current value of look-angles-loop index, 1.le.MF.le.LF
C     MUK    : is .gt. 0 if line intensity profile must be printed
C              [when MUK > 0, then EMOO = EMU(MUK) ]
C     EMOO   : current value of look-angle
C     WVLTRN : wavelength (Angstroms) (i.e. at Delta-Lambda = 0).
C
C     VXZERO : tells whether the current velocity =0, or not
C     EXPAND : tells whether the procedures for expanding atmospheres
C              should be used (set up in SWEET)
C     SPHERE : tells whether this is a spherical, as opposed to
C              plane-parallel, atmosphere
C     FLOBRD : tells whether to use the flow-broadening procedure
C     .
C
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
      equivalence (LINKDS( 3),KLIN )
C     !DASH
C     !EJECT
      external BROOM, ZAMIDAR, MOVE1, MAGENTA, DALLAN, BLUE, MIZZAR,
     $         WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               YHZ(KM,LF), TMUN(KM), SN(N,KM), TCI(KM), RES(KM), Z(N),
      dimension YHZ(K ,*),  TMUN(*),  SN(*),    TCI(*),  RES(*),  Z(*),
C
C               TNU(N,KM), MYX(KM), YY(KM), MUX(KM), KODE(KM), ZHZ(KM),
     $          TNU(*),    MYX(*),  YY(*),  MUX(*),  KODE(*),  ZHZ(*),
C
C               DL(KM), WVNUM(KM), TE(N), WTAB(KM), DDL(LDLMX), VEC(N),
     $          DL(*),  WVNUM(*),  TE(*), WTAB(*),  DDL(*),     VEC(*),
C
C               INDL(LDLMX)
     $          INDL(*)
C
      dimension IN(6)
      equivalence
     $(IN( 1),ITMU  ),(IN( 2),IWS   ),(IN( 3),IWSSAV),(IN( 4),ISNSAV),
     $(IN( 5),IDIDH ),(IN( 6),IZTM  )
C
      call HI ('ZIRMA')
C     !BEG
C     (Get, and allocate, W allotment)
      call MIZZAR      (IN, IS, MOX, 'ZIRMA')
C
C---- Compute intensity (and more if MUK.gt.0)
      call BLUE        (NLTE, K, N, TNU, W(ITMU), TMUN, SN, W(IWS),
     $                  ZHZ, MUX, YY, KODE, WTAB, W(IWSSAV),
     $                  W(ISNSAV), Z, DODIDH, W(IDIDH), MYX, W(IZTM),
     $                  W)
C---- Save I/Hz for later use
      call MOVE1       (ZHZ, K, YHZ(1,MF))
C     !EJECT
      if(MUK.gt.0) then
C----   Print line profile.
        call ZAMIDAR   (NLTE, 0, JSAV, K, DL, WVNUM, WTAB, WVL, ZHZ,
     $                  YY, KODE, MUX, MYX, TCI, KTI, RES, KRES, KLIN,
     $                  ISB1, KILROY, NO, LDL, DDL, IJECT, W)
C
        if(.not.DODIDH) then
C----     Print depths-of-formation graph
          call BROOM     (Z, TE, N, WTAB, W(IZTM), K, NO)
          if(NLTE.eq.1) then
C----       Depths-of-formation analysis (optional)
            call MAGENTA (Z, TE, K, WTAB, DDL, LDL, INDL, ZHZ,
     $                    W(IWSSAV), W(ISNSAV), LEGEND, VEC, IW, NO)
          end if
        else
C----     Print/plot dI/dh
          call DALLAN    (NO, DODIDH, SVDIDH, W(IDIDH), MYX, MUX, YY,
     $                    WTAB, K, Z, TE, N, IU, IL, EMOO, LDL, INDL,
     $                    W, IW)
        end if
      end if
C
C     (Give back W allotment)
      call WGIVE       (W, 'ZIRMA')
C     !END
      call BYE ('ZIRMA')
C
      return
      end
