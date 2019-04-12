      subroutine ESMERAL
     $(TDST)
C
C     Rudolf Loeser, 1987 Mar 13
C---- Saves input TDST for iterative summary.
C     (This is version 5 of ESMERAL.)
C     !DASH
      save
C     !DASH
      real*8 TDST
      integer IQND2
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
      equivalence (IQQ(212),IQND2)
C     !DASH
      external ACRAGAS, HI, BYE
C
C               TDST(N)
      dimension TDST(*)
C
      call HI ('ESMERAL')
C     !BEG
      if(IQND2.gt.0) then
        call ACRAGAS (TDST)
      end if
C     !END
      call BYE ('ESMERAL')
C
      return
      end
