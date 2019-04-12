      subroutine TORY
     $(LAST,MO,LUG,LUT,LUC)
C
C     Rudolf Loeser, 1998 Aug 07
C---- Sets up output unit numbers, for SWALLOW.
C     !DASH
      save
C     !DASH
      integer IQSEC, IQSEP, IQSET, LUC, LUG, LUT, MO
      logical LAST
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
      equivalence (IQQ( 75),IQSEP)
      equivalence (IQQ(103),IQSEC)
      equivalence (IQQ( 17),IQSET)
C     !DASH
      external ZEUS, HI, BYE
C
      call HI ('TORY')
C     !BEG
      call ZEUS   (MO,IQSEP,LUG)
C
      if(LAST) then
        call ZEUS (MO,IQSEC,LUC)
        call ZEUS (MO,IQSET,LUT)
      else
        LUC = 0
        LUT = 0
      end if
C     !END
      call BYE ('TORY')
C
      return
      end
