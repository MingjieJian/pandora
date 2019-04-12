      subroutine SWALE
     $(X,IX,W,IW,PLANE,SOMEL,SOMEC)
C
C     Rudolf Loeser, 1986 Dec 11
C---- Drives Spectrum Summary.
C     (This is version 2 of SWALE.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IQSPC, IW, IX, NPROG
      logical CONT, DATA, PLANE, PROF, SOMEC, SOMEL, SPEC
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
      equivalence (IQQ( 96),IQSPC)
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
      external STALE, LOGIN, SNIT, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /25/
C
      call HI ('SWALE')
C     !BEG
      PROF = SOMEL.and.PLANE
      CONT = SOMEC.and.PLANE
      DATA = PROF.or.CONT
      SPEC = DATA.and.(.not.ICFULL).and.(IQSPC.gt.0)
C
      if(SPEC) then
        call STALE
        call LOGIN  (NPROG)
        call SNIT   (X,IX,W,IW)
        call LOGOUT (NPROG)
      end if
C     !END
      call BYE ('SWALE')
C
      return
      end
