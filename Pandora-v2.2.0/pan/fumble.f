      subroutine FUMBLE
     $(N,KK,CNXP,XJIK)
C
C     Rudolf Loeser, 1980 Apr 15
C---- Adds incident radiation term to XJIK.
C     !DASH
      save
C     !DASH
      real*8 CNXP, XJIK
      integer IQINC, KK, N
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
      equivalence (IQQ( 51),IQINC)
C     !DASH
      external ARRADD, HI, BYE
C
C               CNXP(N,KKX), XJIK(N,KKX)
      dimension CNXP(*),     XJIK(*)
C
      call HI ('FUMBLE')
C     !BEG
      if(IQINC.gt.0) then
        call ARRADD (XJIK, CNXP, XJIK, (N*KK))
      end if
C     !END
      call BYE ('FUMBLE')
C
      return
      end
