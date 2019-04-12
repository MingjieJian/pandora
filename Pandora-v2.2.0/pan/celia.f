      subroutine CELIA
     $(KHED,LUT,LUL)
C
C     Rudolf Loeser, 1980 Aug 20
C---- Printout control for ALLOD.
C     (This is version 2 of CELIA.)
C     !DASH
      save
C     !DASH
      integer IQLTD, KHED, LUL, LUT, MO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
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
      equivalence (IQQ(  4),IQLTD)
C     !DASH
      external CLEF, LINER, HI, BYE
C     !EJECT
C
      call HI ('CELIA')
C     !BEG
      LUT = 0
      LUL = 0
      if(IQLTD.gt.0) then
        LUT = 1
        LUL = MO
        if(KHED.eq.0) then
          KHED = 1
          call CLEF (MO)
        end if
      end if
C
      if(LUL.gt.0) then
        call LINER  (3,LUL)
        write (LUL,100)
  100   format(' ','Data for L.T.E. Profile Calculation.')
      end if
C     !END
      call BYE ('CELIA')
C
      return
      end
