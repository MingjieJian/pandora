      subroutine LISA
     $(NO,NOG,DUMP,K,Z,GTN,COPTRN,STRN,BCTRN,DP,DW,XNE,MPROM,DDL,FDDL,
     $ CDL,FRR,SI,EPCS,EPSS,R1N,QNAME,V,VR,VEL,KVEL,TE,DI,SF,DF,TF,AS,
     $ AD,VEX,DL,EPCD,INT1S,TAU1S,INT1D,TAU1D,SFC,DFC,W,IW,SII,DII,EW,
     $ XHZ,FDL,YDL,ISB1,WVNUM,WTAB)
C
C     Rudolf Loeser, 1978 Jun 30
C---- Computes Line and Flux profiles for transition (IU,IL).
C     Logical unit numbers for printout:
C     NO  is for general eclipse output, and
C     NOG is for eclipse graphs.
C     (This is version 3 of LISA.)
C     !DASH
      save
C     !DASH
      real*8 AD, AS, BCTRN, CDL, COPTRN, DDL, DF, DFC, DI, DII, DL, DP,
     $       DW, EPCD, EPCS, EPSS, EW, FDDL, FDL, FRR, GTN, R1N, SF,
     $       SFC, SI, SII, STRN, TAU1D, TAU1S, TE, TF, V, VEL, VEX, VR,
     $       W, WTAB, WVNUM, XHZ, XNE, YDL, Z, dummy
      integer ICE, IL, INT1D, INT1S, ISB1, IU, IW, K, KVEL, KVSB, LDL,
     $        MPROM, MRR, N, NO, NOG
      logical DUMP, KILROY
      character QNAME*8
C     !COM
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
      equivalence (LINKDS( 4),ICE  )
      equivalence (LINKDS(12),LDL  )
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(15),MRR)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(37),KVSB )
C
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
C     !DASH
C     !EJECT
      external BELLONA, AVESTA, DOLGAN, LEAVES, TOTILA, ZINGEL, MUKTAR,
     $         SWEET, SHRUB, BLAZE, HI, BYE
C
      dimension W(*), IW(*)
C
C               FDDL(N), GTN(N), COPTRN(N,KM), STRN(N,KM), BCTRN(N,KM),
      dimension FDDL(*), GTN(*), COPTRN(*),    STRN(*),    BCTRN(*),
C
C               DW(N), DDL(LDLMX), CDL(LDLMX), Z(N), SI(N,KM), XHZ(KM),
     $          DW(*), DDL(*),     CDL(*),     Z(*), SI(*),    XHZ(*),
C
C               EPSS(N), VEL(N,NVEL), V(N), XNE(N), KVEL(NVEL), VEX(N),
     $          EPSS(*), VEL(*),      V(*), XNE(*), KVEL(*),    VEX(*),
C
C               DI(MRR,KM), SF(KM), DF(KM), WVNUM(KM), TF(KM), AD(MRR),
     $          DI(*),      SF(*),  DF(*),  WVNUM(*),  TF(*),  AD(*),
C
C               INT1S(N,KM), TAU1S(N,KM), INT1D(MRR,KM), DL(KM), AS(N),
     $          INT1S(*),    TAU1S(*),    INT1D(*),      DL(*),  AS(*),
C
C               SFC(N,KM), DFC(MRR,KM), SII(N,KM), DII(N,KM), EPCS(KM),
     $          SFC(*),    DFC(*),      SII(*),    DII(*),    EPCS(*),
C
C               FDL(KM), YDL(KM), EPCD(N), VR(N), TE(N), TAU1D(MRR,KM),
     $          FDL(*),  YDL(*),  EPCD(*), VR(*), TE(*), TAU1D(*),
C
C               DP(N,LDLMX), EW(KM), FRR(MRR), WTAB(KM)
     $          DP(*),       EW(*),  FRR(*),   WTAB(*)
C     !EJECT
C
      call HI ('LISA')
C     !BEG
      KILROY = .true.
C
C---- Compute flux integration weights.
      call BELLONA  (R1N, N, Z, AS, MRR, FRR, AD, W)
C
C---- Set up loop over expansion velocities.
      do 100 NVY = 1,NVEL
C
C----   Set up "expansion velocity"
        call SWEET  (N, VEL, KVEL, VEX)
C----   (Optional dump of LOOPER)
        call MUKTAR ('LISA')
C
C----   Compute Shell (SI) and Disk (DI) intensity profiles.
        call LEAVES (N, MRR, K, IU, IL, DUMP, R1N, Z, FRR, COPTRN,
     $               BCTRN, GTN, STRN, DP, DW, XNE, MPROM, DDL, FDDL,
     $               CDL, LDL, VEX, V, VR, TE, DL, SI, INT1S, TAU1S,
     $               DI, INT1D, TAU1D, W, IW)
C----   Compute integrated Shell ray intensities (SII) and Disk
C       ray intensities (DII)
        call AVESTA (W, N, MRR, K, WVLTRN, DL, SI, SII, DI, DII, XHZ,
     $               EW, FDL, YDL)
C----   Compute Shell integrated intensities (EPCS and EPSS).
        call SHRUB  (IU, IL, N,   K, WVLTRN, EPCS, 1, EPSS,  DL, SI, Z)
C----   Compute Disk integrated intensity (EPCD)
        call SHRUB  (IU, IL, MRR, K, WVLTRN, EPCD, 0, dummy, DL, DI,
     $               dummy)
C----   Compute Shell (SF), Disk (DF) and Total (TF) flux profiles.
        call BLAZE  (N, Z, R1N, MRR, FRR, K, SI, AS, SF, SFC, DI, AD,
     $               DF, DFC, TF)
C----   Print and save profiles.
        call DOLGAN (NO, IU, IL, R1N, Z, N, DL, WVLTRN, WVNUM, WTAB, K,
     $               FRR, MRR, SI, EPCS, EPSS, EPCD, DI, SF, DF, TF,
     $               ICE, QNAME, INT1S, TAU1S, SFC, INT1D, TAU1D, DFC,
     $               KILROY, SII, DII)
C----   Save checksums
        call ZINGEL (K, N, MRR, SI, DI, SF, DF, TF, NVY, 'L')
C----   Make plots
        call TOTILA (NOG, NVY, N, MRR, K, R1N, Z, FRR, DL, WTAB, SI,
     $               DI, SF, DF, TF, W, WVLTRN)
C
  100 continue
C     !END
      call BYE ('LISA')
C
      return
      end
