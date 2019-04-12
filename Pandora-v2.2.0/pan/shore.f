      subroutine SHORE
     $(LU,LUT,PRNT)
C
C     Rudolf Loeser, 1984 Apr 13
C---- Sets up logical unit numbers, for printout from DEANNA.
C     (KODE=3 for "true" continuum.)
C     (This is version 2 of SHORE.)
C     !DASH
      save
C     !DASH
      integer I, IQCSG, IQCSP, IQEMG, IQEMO, IQOPG, IQOPO, IQTCP, LU,
     $        LUT
      logical PRNT
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
      equivalence (IQQ( 12),IQOPO)
      equivalence (IQQ( 66),IQEMO)
      equivalence (IQQ( 45),IQCSP)
      equivalence (IQQ( 63),IQOPG)
      equivalence (IQQ( 92),IQEMG)
      equivalence (IQQ( 93),IQCSG)
      equivalence (IQQ(300),IQTCP)
C     !EJECT
C---- KONOUT      as of 2004 Jan 08
      integer     KONLUN,KONLUR,KONLUD,KONHED
      common      /KONOUT/ KONLUN,KONLUR,KONLUD,KONHED
C     Logical output unit numbers for Continuum Calculations.
C             *** Initialized in PARLOR. ***
C
C     KONLUN: potential output unit number
C     KONLUR: unit number for regular output
C
C     KONLUD: =1 if dump output is authorized
C     KONHED: =1 if wavelength header has already been printed
C     .
C     !DASH
      external ZEUS, HI, BYE
C
C               LUT(6)
      dimension LUT(*)
C
      call HI ('SHORE')
C     !BEG
C---- Regular printout unit
      LU = KONLUR
C
      PRNT = LU.gt.0
C
      if(PRNT) then
C----   Set sub-section print switches
        call ZEUS (LU, IQOPO, LUT(1))
        call ZEUS (LU, IQEMO, LUT(2))
        call ZEUS (LU, IQCSP, LUT(3))
        call ZEUS (LU, IQOPG, LUT(4))
        call ZEUS (LU, IQEMG, LUT(5))
        call ZEUS (LU, IQCSG, LUT(6))
C
        PRNT = .false.
        do 100 I = 1,6
          PRNT = LUT(I).gt.0
          if(PRNT) then
            goto 101
          end if
  100   continue
  101   continue
      end if
C     !END
      call BYE ('SHORE')
C
      return
      end
