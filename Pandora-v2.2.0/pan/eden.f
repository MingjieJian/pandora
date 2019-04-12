      subroutine EDEN
     $(X,IX,W,IW,PLANE,SOMEC)
C
C     Rudolf Loeser, 1986 Dec 11
C---- Drives Continuum Summaries.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IQABS, IQEMS, IQTAS, IW, IX, NPROG
      logical CONT, OPT, PLANE, SOMEC, SUMM
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
      equivalence (IQQ( 59),IQABS)
      equivalence (IQQ(104),IQEMS)
      equivalence (IQQ(105),IQTAS)
C
C---- ICON        as of 1999 Mar 30
      integer     ICBNCH,MXICON,MXIADR
      parameter   (ICBNCH=10)
      parameter   (MXICON=50*ICBNCH)
      parameter   (MXIADR=1000)
C     (Remember to recompile all users when changing any parameter!)
      integer     ICADRS,NIADR,NICON
      real*8      SSBUFF
      logical     ICSTRT, ICFULL
      dimension   ICADRS(MXIADR),SSBUFF(MXICON+ICBNCH)
      common      /ICON1/ NICON,NIADR,ICADRS
      common      /ICON2/ SSBUFF
      common      /ICON3/ ICSTRT, ICFULL
C     Buffer, and record addresses, and control parameters,
C     for saving/restoring Spectrum Summary data.
C     .
C     !DASH
C     !EJECT
      external LOGIN, SPIDER, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /31/
C
      call HI ('EDEN')
C     !BEG
      CONT = SOMEC.and.PLANE
      OPT  = (IQABS.gt.0).or.(IQEMS.gt.0).or.(IQTAS.gt.0)
      SUMM = CONT.and.OPT.and.(.not.ICFULL)
C
      if(SUMM) then
        call LOGIN  (NPROG)
        call SPIDER (X, IX, W, IW)
        call LOGOUT (NPROG)
      end if
C     !END
      call BYE ('EDEN')
C
      return
      end
