      subroutine ESION
     $(X)
C
C     Rudolf Loeser, 2005 Jul 08
C---- Computes KKX, for TUNNEL.
C
C---- The resulting value of KKX may be slightly too large
C     (which should not matter), if some unused wavelengths are
C     included in the count.
C     !DASH
      save
C     !DASH
      real*8 X, XNUK
      integer IQUWT, JJXK, JJXNU, JSTCN, KK, KKX, KOLEV, KXLYM, NCR,
     $        NOION
      logical DOIT
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 33),KOLEV)
      equivalence (KZQ(216),KXLYM)
      equivalence (KZQ( 35),JSTCN)
      equivalence (KZQ( 94),NOION)
      equivalence (RZQ(  9),XNUK )
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
      equivalence (IQQ( 77),IQUWT)
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(10),KK )
      equivalence (JZQ(32),NCR)
      equivalence (JZQ(12),KKX)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ(123),JJXK )
C     !DASH
      external CAMINA, HI, BYE
C
      dimension X(*)
C
      call HI ('ESION')
C     !BEG
      KKX = KK+NCR
C
      DOIT = (NOION.le.0).and.(JSTCN.eq.0).and.(KXLYM.gt.0).and.
     $       (IQUWT.gt.0)
      if(DOIT) then
        call CAMINA (X(JJXK), KK, KKX, KOLEV, XNUK, X(JJXNU))
      end if
C     !END
      call BYE ('ESION')
C
      return
      end
