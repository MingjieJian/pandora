      subroutine LARISA
     $(LU,Z,NW,WTAB,XINT,WSSAV,SNUSAV,LEGEND,IW)
C
C     Rudolf Loeser, 1986 Dec 11
C---- Drives Depths-of-formation analysis printing, for continua.
C     (This is version 2 of LARISA.)
C     !DASH
      save
C     !DASH
      real*8 SNUSAV, WSSAV, WTAB, XINT, Z
      integer IQORI, IW, KODE, LU, N, NW
      logical LEGEND
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (IQQ(101),IQORI)
C     !DASH
      external ABJECT, WEATHER, HI, BYE
C
      dimension IW(*)
C
C               XINT(Nmkuse), WSSAV(N,Nmkuse), SNUSAV(N,Nmkuse), Z(N),
      dimension XINT(*),      WSSAV(*),        SNUSAV(*),        Z(*),
C
C               WTAB(Nmkuse)
     $          WTAB(*)
C
      data KODE /1/
C     !EJECT
C
      call HI ('LARISA')
C     !BEG
      if((IQORI.gt.0).and.(LU.gt.0)) then
        call ABJECT    (LU)
C
        write (LU,100)
  100   format(' ','DEPTHS-OF-FORMATION analysis for the preceding ',
     $             'set of continuum intensities')
C
        call WEATHER (Z,NW,WTAB,XINT,WSSAV,SNUSAV,LEGEND,KODE,LU,IW)
C
      end if
C     !END
      call BYE ('LARISA')
C
      return
      end
