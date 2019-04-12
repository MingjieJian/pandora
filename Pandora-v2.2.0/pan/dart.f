      subroutine DART
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Aug 14
C---- Drives Continuum Data Blocks initialization.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IQJIN, IW, IX, NPROG
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
      equivalence (IQQ(240),IQJIN)
C     !DASH
      external LOGIN, WAVE, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /6/
C
      call HI ('DART')
C     !BEG
      if(IQJIN.le.0) then
        call LOGIN  (NPROG)
        call WAVE   (X,IX,W,IW)
        call LOGOUT (NPROG)
      end if
C     !END
      call BYE ('DART')
C
      return
      end
