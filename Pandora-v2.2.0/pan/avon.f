      subroutine AVON
     $(X,IX,W,IW,SPHERE,SOMEL,LEGEND,LYNC,WLYNC,XLYNC)
C
C     Rudolf Loeser, 1980 Jun 18
C---- Drives Line Emission calculations.
C     !DASH
      save
C     !DASH
      real*8 W, WLYNC, X, XLYNC
      integer IQLGT, IW, IX, LYNC, NOTA, NPROG
      logical LEGEND, SOMEL, SPHERE
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(27),NOTA )
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
      equivalence (IQQ(  9),IQLGT)
C     !DASH
C     !EJECT
      external LOGIN, LIGHT, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               WLYNC(Klynf), XLYNC(Klynf)
      dimension WLYNC(*),     XLYNC(*)
C
      data NPROG /23/
C
      call HI ('AVON')
C     !BEG
      SOMEL = .false.
      if((NOTA.gt.0).and.(IQLGT.gt.0)) then
        SOMEL = .true.
C
        call LOGIN  (NPROG)
        call LIGHT  (X, IX, W, IW, SPHERE, LEGEND, LYNC, WLYNC, XLYNC)
        call LOGOUT (NPROG)
      end if
C     !END
      call BYE ('AVON')
C
      return
      end
