      subroutine KASHMIR
     $(X,IX,W,IW,XLB1,XLB2,XCBL,HN1,POPK,KHED,KUP1,KUP2)
C
C     Rudolf Loeser, 1998 Jan 27
C---- "POST"-processing for the current transition, for TUBA.
C     (This is version 3 of KASHMIR.)
C     !DASH
      save
C     !DASH
      real*8 HN1, POPK, W, X, XCBL, XLB1, XLB2
      integer IBRSW, IPRO, IQLCP, IQLGT, IQLTE, IQPH2, IW, IX, KHED,
     $        KLIN, KUP1, KUP2
      logical SPCTRM
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
      equivalence (LINKDS( 8),IBRSW)
      equivalence (LINKDS( 3),KLIN )
      equivalence (LINKDS( 5),IPRO )
C     !EJECT
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
      equivalence (IQQ(313),IQLCP)
      equivalence (IQQ( 33),IQLTE)
      equivalence (IQQ(  5),IQPH2)
      equivalence (IQQ(  9),IQLGT)
C     !DASH
      external COOK, YARN, ALLOD, PLOVER, WAGRIN, DOLLAR, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               POPK(N,NPOPS), XLB1(Li1len), XLB2(Li2len), XCBL(Miklen),
      dimension POPK(*),       XLB1(*),      XLB2(*),      XCBL(*),
C
C               HN1(N)
     $          HN1(*)
C     !EJECT
C
      call HI ('KASHMIR')
C     !BEG
      SPCTRM = (IPRO.gt.0).and.(IQPH2.gt.0).and.(IQLGT.gt.0)
      if(SPCTRM) then
C
        if(KLIN.eq.2) then
C----     Passive line calculations
          call COOK  (X, IX, W, IW, XLB1, XCBL, HN1, POPK, KHED)
          KUP1 = 1
        end if
C
C----   Final broadening
        call YARN  (X, W, IW, XLB1, HN1, POPK, KHED)
        KUP1 = 1
C
        if(IQLTE.gt.0) then
C----     LTE Line Source Function
          call ALLOD (X, IX, W, IW, XLB1, KHED)
          KUP1 = 1
        end if
C
C----   Frequency-dependent Line Source Function
        call PLOVER  (X, W, IW, XLB1, XLB2, KHED)
        KUP2 = 1
C
      end if
C
C
      if(IQLCP.gt.0) then
C----   Scattering albedo analysis (LINECOMP)
        call WAGRIN  (X, W, XLB1, KHED)
      end if
C
C---- Save transition data in special file (no computing/printing)
      call DOLLAR    (X, W, XLB1)
C     !END
      call BYE ('KASHMIR')
C
      return
      end
