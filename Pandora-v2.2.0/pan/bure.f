      subroutine BURE
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1975 May 21
C---- Drives H- calculation.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IQHMS, IW, IX, MHL, NPROG
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
      equivalence (LEST( 5),MHL  )
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
      equivalence (IQQ( 68),IQHMS)
C     !DASH
C     !EJECT
      external LOGIN, ISIS, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /9/
C
      call HI ('BURE')
C     !BEG
      if((IQHMS.gt.0).and.(MHL.ge.2)) then
        call LOGIN  (NPROG)
        call ISIS   (X, IX, W, IW)
        call LOGOUT (NPROG)
      end if
C     !END
      call BYE ('BURE')
C
      return
      end
