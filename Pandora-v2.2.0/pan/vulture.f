      subroutine VULTURE
     $(X,IX,W,IW,XND,BDI,GTO,GTN,FXI,COP,DP,DW,MPROM,XNE,DDL,FDDL,CDL,
     $ VXS,WVL,TAU,TAUM,XKL,XKT,IPRNT)
C
C     Rudolf Loeser, 1980 Apr 09
C---- Computes TAU (= line-center TNU, mu=1),
C     mean-TAU (which has PHI=1), and
C     printed-TAU (which has U=0 in PHI).
C     IPRNT is the overall printout control switch.
C
C
C---- Note the special DL-table of length K1 = 1.
C     ===========================================
C
C     !DASH
      save
C     !DASH
      real*8 BDI, CDL, COP, DDL, DL, DP, DW, FDDL, FXI, GTN, GTO, TAU,
     $       TAUM, VXS, W, WVL, X, XKL, XKT, XND, XNE
      integer IFXO, IGTM, IGTP, IIMG, IIMGG, IIMGT, IMAX, IMIN, IN,
     $        IPHI, IPHM, IPHP, IPRNT, IQTAF, IS, ISW, ITAU, IW, IWS,
     $        IX, JJMIJ, JN, K1, LDL, LS, LSFP, LSTMP, LU, MO, MOX,
     $        MPROM, MUX, N
      logical EDGTO, EDINT, EDINTM, EDTAU, EDTAUM, EQUL, SMTH, lummy
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
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
      equivalence (IQQ( 26),IQTAF)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 43),LSTMP)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ( 16),JJMIJ)
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
      equivalence (LINKDS(12),LDL  )
      equivalence (LINKDS(14),LSFP )
C     !DASH
C     !EJECT
      external MEAT, CONMUL, DANAKIL, YMUIR, MINMAXD, RAPTURE, PHAEDRA,
     $         ZEUS, YMIR, MYNAH, DEODAR, IGIVE, ROADEO, WGIVE, LAVURE,
     $         ELSI, BLIP, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               DP(N,LDL), BDI(N,NL), CDL(LDL), XKL(N), TAU(N), VXS(N),
      dimension DP(*),     BDI(*),    CDL(*),   XKL(*), TAU(*), VXS(*),
C
C               XND(N,NL), DDL(LDL), FDDL(N), FXI(N), COP(N), TAUM(N),
     $          XND(*),    DDL(*),   FDDL(*), FXI(*), COP(*), TAUM(*),
C
C               DW(N), XNE(N), GTN(N), XKT(N), GTO(N)
     $          DW(*), XNE(*), GTN(*), XKT(*), GTO(*)
C
      dimension DL(1)
C
      dimension IN(7)
      equivalence
     $(IN( 1),IPHI  ),(IN( 2),ITAU  ),(IN( 3),IGTM  ),(IN( 4),IPHM  ),
     $(IN( 5),IFXO  ),(IN( 6),IGTP  ),(IN( 7),IPHP  )
C
      dimension JN(3)
      equivalence
     $(JN( 1),IIMG  ),(JN( 2),IIMGG ),(JN( 3),IIMGT )
C
      data DL(1),K1 /0.D0, 1/
C     !EJECT
C
      call HI ('VULTURE')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call MEAT    (IN, IS , MOX, 'VULTURE')
      call LAVURE  (JN, IWS, MUX, 'VULTURE')
C
      call ZEUS    (IPRNT, IQTAF, ISW)
      call ZEUS    (LSFP, ISW, ISW)
      call ZEUS    (MO, ISW, LU)
      call ZEUS    (LU, LSTMP, LS)
C---- Compute GTO ("original" GTN) and FXO ("original" FXI)
      call DANAKIL (X, W, IW, LS, N, XND, BDI, DW, GTO, W(IFXO), EQUL,
     $              SMTH)
C---- Edit to get final FXI
      call PHAEDRA (N, W(IFXO), FXI, lummy)
C---- Compute PHI (Note: mu=1 and DL(1) = 0.0)
      call YMUIR   (W, IW, WVL, DL, K1, DP, DW, XNE, VXS, N, DDL, FDDL,
     $              CDL, LDL, MPROM, W(IPHI))
C---- Edit to get final GTN
      call DEODAR  (N, GTO, GTN, W(IPHI), COP, EDGTO, IW(IIMGG))
      call ROADEO  (EDGTO, IX(JJMIJ))
C---- Compute TAU
      call MYNAH   (X, W, W(IPHI), COP, GTN, TAU, EDINT, EDTAU,
     $              IW(IIMGT), 0)
C---- Compute mean-TAU (using PHI = 1)
      call BLIP    (X, W, N, COP, GTO, TAUM, IW(IIMG), W(IPHM),
     $              W(IGTM), EDINTM, EDTAUM)
C---- Compute printed-TAU (using PHI computed with U = 0)
      call MINMAXD (CDL, 1, LDL, IMIN, IMAX)
      call YMIR    (W, IW, DP, DW, XNE, N, MPROM, W(IPHP))
      call CONMUL  (CDL(IMAX), W(IPHP), N)
      call DEODAR  (N, GTO, W(IGTP), W(IPHP), COP, lummy, IW(IIMG))
      call MYNAH   (X, W, W(IPHP), COP, W(IGTP), W(ITAU), EDINTM,
     $              EDTAUM, IW(IIMG), 1)
C---- Compute Line Opacity and Total Opacity
      call ELSI    (1, N, W(IPHI), COP, GTN, XKL, XKT)
C---- Print
      call RAPTURE (LU, X, WVL, COP, W(ITAU), TAUM, GTN, GTO, W(IPHI),
     $              XND, XKL, XKT, EDINT, IW(IIMGT), EDTAU, EDTAUM,
     $              EDGTO, IW(IIMGG), LDL, DDL(IMAX), CDL(IMAX), LSTMP,
     $              EQUL, SMTH)
C
C     (Give back W & IW allotments)
      call WGIVE   (W , 'VULTURE')
      call IGIVE   (IW, 'VULTURE')
C     !END
      call BYE ('VULTURE')
C
      return
      end
