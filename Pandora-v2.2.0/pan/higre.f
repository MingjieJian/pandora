      subroutine HIGRE
     $(V,VT,N)
C
C     Rudolf Loeser, 1988 Jun 29
C---- Sets VT = V.
C     (This is version 2 of HIGRE.)
C     !DASH
      save
C     !DASH
      real*8 V, VT
      integer IQVTV, N
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
      equivalence (IQQ( 32),IQVTV)
C     !DASH
      external MOVE1, HI, BYE
C
C               V(N), VT(N)
      dimension V(*), VT(*)
C
      call HI ('HIGRE')
C     !BEG
      if(IQVTV.gt.0) then
        call MOVE1 (V,N,VT)
      end if
C     !END
      call BYE ('HIGRE')
C
      return
      end
