      subroutine DEE
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1975 May 21
C---- Drives Dust Temperature recalculation.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IQDT2, IQND2, IW, IX, NDT, NPROG
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(21),NDT)
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
      equivalence (IQQ(212),IQND2)
      equivalence (IQQ( 99),IQDT2)
C     !DASH
      external LOGIN, HOTROD, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /10/
C     !EJECT
C
      call HI ('DEE')
C     !BEG
      if((IQDT2.gt.0).and.(IQND2.gt.0).and.(NDT.gt.0)) then
        call LOGIN  (NPROG)
        call HOTROD (X,IX,W,IW)
        call LOGOUT (NPROG)
      end if
C     !END
      call BYE ('DEE')
C
      return
      end
