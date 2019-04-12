      subroutine MEIG
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1983 Nov 04
C---- Drives special Spectrum Save file initialization.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IQCFX, IQCPU, IQEMI, IQLGT, IQPPU, IW, IX, NPROG
      logical CONTIN, PROFIL
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
      equivalence (IQQ(151),IQPPU)
      equivalence (IQQ(  9),IQLGT)
      equivalence (IQQ( 97),IQCPU)
      equivalence (IQQ( 55),IQEMI)
      equivalence (IQQ( 80),IQCFX)
C     !DASH
C     !EJECT
      external LOGIN, CETOS, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /30/
C
      call HI ('MEIG')
C     !BEG
      PROFIL = (IQPPU.gt.0).and.(IQLGT.gt.0)
      CONTIN = (IQCPU.gt.0).and.((IQEMI.gt.0).or.(IQCFX.gt.0))
C
      if(PROFIL.or.CONTIN) then
        call LOGIN  (NPROG)
        call CETOS  (X)
        call LOGOUT (NPROG)
      end if
C     !END
      call BYE ('MEIG')
C
      return
      end
