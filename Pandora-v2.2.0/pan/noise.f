      subroutine NOISE
     $(X,IX)
C
C     Rudolf Loeser, 1980 Jan 09
C---- Computes MRX, for TUNNEL.
C     !DASH
      save
C     !DASH
      real*8 X
      integer IQUTR, IX, JJMRJ, JJRRN, JJWRA, JSTCN, KSHEL, MRX, NLEV,
     $        NOION, NSL
      logical DOIT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(40),NSL)
      equivalence (JZQ(30),MRX)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(244),JJWRA)
      equivalence (IZOQ( 69),JJRRN)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  1),JJMRJ)
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
      equivalence (KZQ( 35),JSTCN)
      equivalence (KZQ( 94),NOION)
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
      equivalence (LEST( 1),KSHEL)
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
      equivalence (IQQ( 53),IQUTR)
C     !DASH
      external  MANIAC, HI, BYE
      intrinsic min
C
      dimension X(*), IX(*)
C
      call HI ('NOISE')
C     !BEG
      DOIT = (NOION.le.0).and.(JSTCN.eq.0).and.(IQUTR.le.0)
C
      if(DOIT) then
        NLEV = NSL+min(KSHEL,1)
C
        call MANIAC (X, IX, NLEV, IX(JJMRJ), X(JJRRN), X(JJWRA), MRX)
      end if
C     !END
      call BYE ('NOISE')
C
      return
      end
