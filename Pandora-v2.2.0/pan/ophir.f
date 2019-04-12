      subroutine OPHIR
     $(X,IX,W,IW,XPBL,CHKI,CHIJ,NO)
C
C     Rudolf Loeser, 1991 May 06
C---- Computes rates due to collisions with Hydrogen atoms.
C     NO is for basic RATEPRNT printout.
C     !DASH
      save
C     !DASH
      real*8 AMASS, CHIJ, CHKI, FTAB, W, X, XPBL, XTAB, dummy
      integer ICHSW, IEQN, IFC, IHN1, IKCHSW, IN, IQCHP, IS, IW, IWC,
     $        IWS, IX, JHACL, JHACUL, JJAIJ, JJLCH, JJP, JJTE, JJTEX,
     $        JJXCU, JJXNC, JJXNU, JN, KCHIJ, KCHKI, LU, MOX, MUX, N,
     $        NINT, NL, NO, NSL
      logical PRNT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 27),JJP  )
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 71),JJTEX)
      equivalence (IZOQ( 32),JJAIJ)
      equivalence (IZOQ(149),JJXNC)
      equivalence (IZOQ(260),JJXCU)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ( 17),JJLCH)
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
      equivalence (RZQ(  4),AMASS)
      equivalence (KZQ(122),ICHSW)
C     !EJECT
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(80),KCHKI)
      equivalence (LEST(71),KCHIJ)
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
      equivalence (IQQ(282),IQCHP)
C     !DASH
C     !EJECT
      external NUMINA, ZEUS, POPUTIL, DICEROS, FRANCIS, DAMALA, FORBAS,
     $         DERVENI, FANG, ZERO1, ZEROI, PEDGE, WGIVE, IGIVE,
     $         HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               CHKI(N,NSL), CHIJ(N,NL,NL), XPBL(Lenpbl)
      dimension CHKI(*),     CHIJ(*),       XPBL(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IHN1  ),(IN( 2),IWC   ),(IN( 3),IFC   ),(IN( 4),IEQN  )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IKCHSW)
C
      parameter (NINT=51)
      dimension XTAB(NINT), FTAB(NINT)
C
      call HI ('OPHIR')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call NUMINA  (IN, IS,  MOX, 'OPHIR')
      call PEDGE   (JN, IWS, MUX, 'OPHIR')
C
      call ZERO1   (CHKI, (N*NSL  ))
      call ZERO1   (CHIJ, (N*NL*NL))
      call ZEROI   (IW(IKCHSW), 1, (NL*NL))
      call ZEUS    (NO, IQCHP, LU)
      PRNT = LU.gt.0
C
C---- Get level-1 Hydrogen population
      call POPUTIL (XPBL, 1, 1, W(IHN1), 0, dummy, 0, dummy, 0, dummy)
C---- Compute eefective quantum number EQN and common term WC
      call DICEROS (N, NL, AMASS, X(JJTE), X(JJXNU), W(IEQN), W(IWC))
C
C---- Compute CHKI: use Kaulakys
      JHACL = 0
      call FRANCIS (N, NL, AMASS, X(JJTE), IX(JJLCH), W(IEQN),
     $              X(JJXNU), X(JJXCU), W(IHN1), JHACL, CHKI, W(IWC),
     $              W(IFC), NINT, XTAB, FTAB, PRNT)
C     (Print)
      call FANG    (N, NL, CHKI, JHACL, ICHSW, LU)
C
      KCHKI = 0
      if((JHACL.gt.0).and.(ICHSW.gt.0)) then
        KCHKI = 1
      end if
C     !EJECT
C---- Compute CHIJ: first use Kaulakys . . .
      JHACUL = 0
      call DAMALA  (N, NL, AMASS, X(JJTE), X(JJTEX), IX(JJLCH),
     $              W(IEQN), X(JJXNU), X(JJP), W(IHN1), JHACUL,
     $              IW(IKCHSW), CHIJ, W(IWC), W(IFC), NINT,
     $              XTAB, FTAB, PRNT)
C---- . . . then, use Drawin (based on LCH(1))
      call DERVENI (N, NL, IX(JJLCH), AMASS, X(JJTE), X(JJXNC),
     $              X(JJTEX), W(IEQN), X(JJXNU), X(JJP), W(IHN1),
     $              X(JJAIJ), JHACUL, IW(IKCHSW), CHIJ, W(IFC), PRNT)
C
C     (Print)
      call FORBAS  (N, NL, CHIJ, JHACUL, IW(IKCHSW), ICHSW, LU)
C
      KCHIJ = 0
      if((JHACUL.gt.0).and.(ICHSW.gt.0)) then
        KCHIJ = 1
      end if
C
C     (Give back W & IW allotments)
      call WGIVE   (W,  'OPHIR')
      call IGIVE   (IW, 'OPHIR')
C     !END
      call BYE ('OPHIR')
C
      return
      end
