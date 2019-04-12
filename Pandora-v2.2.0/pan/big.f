      subroutine BIG
     $(X,IX,W,IW,BDIJ,BDIPR,BDIUW,BDI,IMG,BD0,BD1,BDN,BDD,BDR,BDE,WEIT)
C
C     Rudolf Loeser, 2002 Feb 13
C---- Computes departure coefficients, for GORSE.
C     (This is version 2 of BIG.)
C     !DASH
      save
C     !DASH
      real*8 BD0, BD1, BDD, BDE, BDI, BDIJ, BDIPR, BDIUW, BDN, BDR, W,
     $       WBD, WEIT, X, dummy
      integer IMG, IQBDC, IW, IX, KLOG, KMSS, MODE, N, NL, NNL
      character qummy*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
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
      equivalence (RZQ( 45),WBD  )
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
      equivalence (IQQ(245),IQBDC)
C     !DASH
C     !EJECT
      external WILY, WEITER, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C
C               BD0(N,NL), BD1(N,NL), BDN(1,NL), BDD(N,NL), BDIJ(N,NL),
      dimension BD0(*),    BD1(*),    BDN(*),    BDD(*),    BDIJ(*),
C
C               BDIPR(N,NL), BDIUW(N,NL), BDI(N,NL), BDR(N,NL), IMG(N),
     $          BDIPR(*),    BDIUW(*),    BDI(*),    BDR(*),    IMG(*),
C
C               WEIT(N), BDE(N,NL)
     $          WEIT(*), BDE(*)
C
      data KLOG,MODE,KMSS /1, 0, 0/
C
      call HI ('BIG')
C     !BEG
      call WILY   (X, IX, W, IW, BDIJ, IQBDC, BDIUW, IMG, BD0, BD1,
     $             BDN, BDD, BDR, BDE)
      NNL = N*NL
      call WEITER (BDI, BDIUW, BDIPR, dummy, WBD, NNL, KLOG, MODE,
     $             KMSS, qummy, WEIT)
C     !END
      call BYE ('BIG')
C
      return
      end
