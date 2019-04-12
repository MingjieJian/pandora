      subroutine BITTER
     $(XLM,DUMP)
C
C     Rudolf Loeser, 1983 Jan 27
C---- Sets up a CSF-Dump unit number and output header.
C     (This is version 3 of BITTER.)
C     !DASH
      save
C     !DASH
      real*8 XLM
      integer IQCSD
      logical DUMP
C     !COM
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
      equivalence (IQQ( 39),IQCSD)
C     !DASH
C     !EJECT
      external DACAPO, HI, BYE
C
      call HI ('BITTER')
C     !BEG
      DUMP = (IQCSD.gt.0).and.(KONLUD.gt.0)
      if(DUMP) then
        call DACAPO (XLM)
      end if
C     !END
      call BYE ('BITTER')
C
      return
      end
