      subroutine GRAU
     $(NO,K,STRN,SSTRN,BCTRN,COPTRN,TCFLX,VEL,KVEL,FDDL,LDL,DDL,CDL,
     $ DP,DW,GTN,GTNS,DL,PROGLI,PGD,ISB1,V,VR,TE,Z,FRR,XNE,MPROM,W,
     $ IW,LUA,LUG,IJECT)
C
C     Rudolf Loeser, 1981 Sep 16
C---- Controls Flux profiles calculations, printing and plotting, for
C     transition (IU,IL), using spherical coordinates.
C     (Completely revised, 2000 Jun 29)
C     !DASH
      save
C     !DASH
      real*8 BCTRN, CDL, COPTRN, DDL, DL, DP, DW, FDDL, FRR, GTN, GTNS,
     $       PGD, PROGLI, SSTRN, STRN, TCFLX, TE, V, VEL, VR, W, XNE, Z,
     $       dummy
      integer ICF, ICN, ICQ, IFZ, IJECT, IL, IN, IQLID, IQLTE, IS, ISB1,
     $        IU, IVA, IW, IWTAB, IWVNUM, IZN, IZQ, K, KTF, KVEL, LDL,
     $        LUA, LUG, MOX, MPROM, N, NO
      logical DUMP, KILROY, LTE
C     !COM
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ( 57),IQLID)
      equivalence (IQQ( 33),IQLTE)
C     !EJECT
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
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
C     !DASH
C     !EJECT
      external BOZENIA, BEVEL, APRICOT, SWEET, BARAMA, MUKTAR, SNUGUD,
     $         KAREN, WGIVE, OOZE, HI, BYE
C
      dimension W(*), IW(*)
C
C               STRN(N,KM), SSTRN(N,KM), BCTRN(N,KM), GTNS(N), FDDL(N),
      dimension STRN(*),    SSTRN(*),    BCTRN(*),    GTNS(*), FDDL(*),
C
C               COPTRN(N,KM), DP(N,LDLMX), FRR(MRR), CDL(LDLMX), DW(N),
     $          COPTRN(*),    DP(*),       FRR(*),   CDL(*),     DW(*),
C
C               VEL(N,NVEL), DL(KM), VR(N), Z(N), XNE(N), V(N), GTN(N),
     $          VEL(*),      DL(*),  VR(*), Z(*), XNE(*), V(*), GTN(*),
C
C               DDL(LDLMX), KVEL(NVEL), PGD(3,LDLMX), TCFLX(KM,LFBV),
     $          DDL(*),     KVEL(*),    PGD(*),       TCFLX(*),
C
C               TE(N)
     $          TE(*)
C
      dimension IN(9)
      equivalence
     $(IN( 1),ICN   ),(IN( 2),ICQ   ),(IN( 3),IZN   ),(IN( 4),IZQ   ),
     $(IN( 5),ICF   ),(IN( 6),IVA   ),(IN( 7),IFZ   ),(IN( 8),IWVNUM),
     $(IN( 9),IWTAB )
C     !EJECT
C
      call HI ('GRAU')
C     !BEG
C     (Get, and allocate, W allotment)
      call KAREN     (IN, IS, MOX, 'GRAU')
C
      LFB    = 1
      LFBV   = 1
      LTE    = IQLTE.gt.0
      DUMP   = IQLID.gt.0
      SPHERE = .true.
      KILROY = .true.
C---- Set up tables of WVNUM and WTAB corresponding to DL
      call SNUGUD    (K, DL, WVLTRN, W(IWVNUM), W(IWTAB), 1)
C
C---- Set up loop over expansion velocities.
      do 100 NVY = 1,NVEL
C
C----   Set up "expansion velocity", W(IVA)
        call SWEET   (N, VEL, KVEL, W(IVA))
C----   (Dump if requested)
        call MUKTAR  ('GRAU')
C----   Produce profile analysis.
        call BARAMA  (LUA, DL, K, N, Z, DW, DP, W(IVA), XNE, MPROM,
     $                LDL, DDL, FDDL, CDL, W, IW)
C----   Get "true" continuum data for residual calculation
        call OOZE    (K, TCFLX, W(ICF), KTF)
C
C----   Compute intensity and flux, non-LTE and LTE
        call BEVEL   (N, K, IU, IL, NVY, DUMP, Z, FRR, COPTRN, BCTRN,
     $                DP, DW, XNE, MPROM, DDL, FDDL, CDL, LDL, W(IVA),
     $                V, VR, TE, DL, LTE, GTN, GTNS, STRN, SSTRN,
     $                W(ICN), W(ICQ), W(IZN), W(IZQ), W, IW)
C----   Compute and print flux/Hz
        call BOZENIA (NO, K, DL, 0, dummy, WVLTRN, W(IWVNUM),
     $                W(IWTAB), LTE, dummy, dummy, W(IZN), W(IZQ),
     $                W(ICN), W(ICQ), W(IFZ), W(ICF), KTF, KILROY,
     $                ISB1, LDL, DDL, IJECT, W)
C----   Plot absolute flux, non-LTE.
        call APRICOT (LUG, K, WVLTRN, DL, W(IWVNUM), W(IWTAB), W(IFZ),
     $                PROGLI, LDL, DDL, PGD, W)
C
  100 continue
C
C     (Give back W allotment)
      call WGIVE     (W, 'GRAU')
C     !END
      call BYE ('GRAU')
C
      return
      end
