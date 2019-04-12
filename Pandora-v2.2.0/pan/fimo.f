      subroutine FIMO
     $(MO,LUT,LUG,LUC)
C
C     Rudolf Loeser, 1998 Aug 07
C---- Sets up output unit numbers, for SWALLOW.
C     !DASH
      save
C     !DASH
      integer IQMSP, LO, LUC, LUG, LUT, MO
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
      equivalence (IQQ(187),IQMSP)
C     !DASH
      external ZEUS, HI, BYE
C
      call HI ('FIMO')
C     !BEG
      call ZEUS (MO, IQMSP, LO)
C
      if(LUT.le.0) then
        LUT = LO
      end if
C
      if(LUG.le.0) then
        LUG = LO
      end if
C
      if(LUC.le.0) then
        LUC = LO
      end if
C     !END
      call BYE ('FIMO')
C
      return
      end
