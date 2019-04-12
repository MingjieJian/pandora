      subroutine FRIGATE
     $(TAU,DP,DW,RHO)
C
C     Rudolf Loeser, 1987 Jun 18
C---- Computes RHO by the Escape Probability approximation.
C     (This is version 2 of FRIGATE.)
C     !DASH
      save
C     !DASH
      real*8 DP, DW, RHO, TAU
      integer IQVES
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
      equivalence (IQQ(214),IQVES)
C     !DASH
      external EAST, WEST, HI, BYE
C
      call HI ('FRIGATE')
C     !BEG
      if(IQVES.le.0) then
        call EAST (TAU,RHO)
      else
        call WEST (TAU,DP,DW,RHO)
      end if
C     !END
      call BYE ('FRIGATE')
C
      return
      end
