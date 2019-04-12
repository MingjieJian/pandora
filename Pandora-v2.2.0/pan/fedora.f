      subroutine FEDORA
     $(W,N,K,WVL,DL,DDL,LDL,WVNM,WTAB,TMUN,CNDT,YHZ,YHZA,JSAV,MUX,YY,
     $ MYX,KODE,NO,RES,KRES,TCA,KTA,KILROY,ISB1,IJECT)
C
C     Rudolf Loeser, 1977 Jan 28
C---- Prints and computes a line profile, for MU=1 only, that includes
C     the radiation from the illuminating star.
C     !DASH
      save
C     !DASH
      real*8 CNDT, DDL, DL, ONE, RES, TCA, TMUN, W, WTAB, WVL, WVNM,
     $       YHZ, YHZA, YY
      integer IJECT, ISB1, JSAV, K, KLIN, KODE, KRES, KSTAR, KTA, LDL,
     $        MUX, MYX, N, NLTE, NO
      logical KILROY
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
C     !EJECT
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
      equivalence (LINKDS( 3),KLIN )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIOMIRA, ZAMIDAR, HI, BYE
C
      dimension W(*)
C
C               YHZ(KM,L), YY(KM), TMUN(KM), CNDT(N), YHZA(KM), DL(KM),
      dimension YHZ(K ,*), YY(*),  TMUN(*),  CNDT(*), YHZA(*),  DL(*),
C
C               RES(KM), MUX(KM), WVNM(KM), TCA(KM), KODE(KM), MYX(KM),
     $          RES(*),  MUX(*),  WVNM(*),  TCA(*),  KODE(*),  MYX(*),
C
C               WTAB(KM), DDL(LDLMX)
     $          WTAB(*),  DDL(*)
C
      data NLTE,KSTAR /1, 1/
C     !EJECT
C
      call HI ('FEDORA')
C     !BEG
      if(EMOO.eq.ONE) then
C----   Compute modified line profile
        call DIOMIRA (K, TMUN, N, CNDT, YHZ(1,MF), YHZA)
C----   Print
        call ZAMIDAR (NLTE, KSTAR, JSAV, K, DL, WVNM, WTAB, WVL, YHZA,
     $                YY, KODE, MUX, MYX, TCA, KTA, RES, KRES, KLIN,
     $                ISB1, KILROY, NO, LDL, DDL, IJECT, W)
      end if
C     !END
      call BYE ('FEDORA')
C
      return
      end
