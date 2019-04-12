      subroutine DACAPO
     $(XLM)
C
C     Rudolf Loeser, 2002 Sep 20
C---- Prints a header for Continuum Calculations output.
C     !DASH
      save
C     !DASH
      real*8 XLM, dummy
      integer IQACP
      logical TOOT
      character HEAD*12
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
      equivalence (IQQ(254),IQACP)
C
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
      external ABJECT, STARER, LINER, MARABOU, PRIAM, HI, BYE
C
      data TOOT /.false./
C     !EJECT
C
      call HI ('DACAPO')
C     !BEG
      if(TOOT) then
        write(*,100) XLM,KONLUR,KONHED
  100   format(' ','DACAPO',1PE20.12,', LUR =',I3,', HED =',I3)
      end if
C
      if(KONHED.eq.0) then
C
        if(IQACP.gt.0) then
          call ABJECT  (KONLUR)
          write (KONLUR,101) XLM
  101     format(' ','Continuum Calculation results for ',1PE15.8,
     $               ' Angstroms (abbreviated format, option ACSFPRNT)')
          call STARER  (KONLUR)
C
        else
          call MARABOU (XLM, dummy, HEAD)
          call PRIAM   (KONLUR, HEAD, 12)
        end if
C
        KONHED = 1
      end if
C     !END
      call BYE ('DACAPO')
C
      return
      end
