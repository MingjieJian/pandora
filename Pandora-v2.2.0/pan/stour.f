      subroutine STOUR
     $(X,IX,W,IW,SPHERE,SOMEC,LEGEND,LYNC,WLYNC,XLYNC)
C
C     Rudolf Loeser, 1980 Jun 13
C---- Continuum Emission.
C     (This is version 2 of STOUR.)
C     !DASH
      save
C     !DASH
      real*8 W, WLYNC, X, XLYNC, dummy
      integer IJECT, IQCFX, IQCOE, IQECL, IQEMI, IW, IX, KLNC, LINK,
     $        LYNC, NPROG
      logical CONFLX, CONINT, ECSPEC, LEGEND, LINFLX, LININT, NRML,
     $        SOMEC, SPHERE
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
      equivalence (IQQ( 55),IQEMI)
      equivalence (IQQ( 80),IQCFX)
      equivalence (IQQ(  6),IQECL)
      equivalence (IQQ(305),IQCOE)
C     !DASH
C     !EJECT
      external LOGIN, SIAM, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               WLYNC(Klynf), XLYNC(Klynf)
      dimension WLYNC(*),     XLYNC(*)
C
      data NPROG /24/
C
      data LININT,LINFLX,LINK,KLNC /.false., .false., 0, 1/
C
      call HI ('STOUR')
C     !BEG
      SOMEC  = (IQEMI.gt.0).and.(.not.SPHERE)
      CONINT = SOMEC
      CONFLX = (IQCFX.gt.0)
      ECSPEC = (IQECL.gt.0).or.(IQCOE.gt.0)
      NRML   = CONINT.or.CONFLX.or.ECSPEC
C
      if(NRML) then
        call LOGIN  (NPROG)
        call SIAM   (X, IX, W, IW, SPHERE, CONINT, CONFLX, ECSPEC,
     $               LININT, LINFLX, dummy, LEGEND, LINK, KLNC, LYNC,
     $               WLYNC, XLYNC, IJECT)
        call LOGOUT (NPROG)
      end if
C     !END
      call BYE ('STOUR')
C
      return
      end
