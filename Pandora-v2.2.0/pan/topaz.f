      subroutine TOPAZ
     $(XNE,KODE)
C
C     Rudolf Loeser, 1977 Jan 19
C---- Writes out XNE for iterative summary.
C     !DASH
      save
C     !DASH
      real*8 XNE
      integer IQINE, KODE, KOPA, N
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
      equivalence (IQQ(127),IQINE)
C     !DASH
      external BERWYN, HI, BYE
C
C               XNE(N)
      dimension XNE(*)
C
      data KOPA /8/
C
      call HI ('TOPAZ')
C     !BEG
      if((KODE.gt.0).and.(IQINE.gt.0)) then
        call BERWYN (KOPA,'Topaz','Ne',0,0,XNE,N,.true.)
      end if
C     !END
      call BYE ('TOPAZ')
C
      return
      end
