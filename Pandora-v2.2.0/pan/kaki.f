      subroutine KAKI
     $(XLM,CORE,ION,WLO,WVL,WHI,LDSW,YES,DUMP)
C
C     Rudolf Loeser, 2004 Jun 23
C---- Tells whether to compute a contributor at this wavelength, and
C     sets the debug switch.
C
C     (This is version 3 of KAKI.)
C     !DASH
      save
C     !DASH
      real*8 CORE, DELTA, WHI, WLO, WVL, XLM
      integer FLAG, LDSW
      logical DUMP, ION, YES
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
C     !DASH
      external WITHIN, COMPD, HI, BYE
C
      data DELTA /1.D-6/
C
      call HI ('KAKI')
C     !BEG
      DUMP = .false.
      call WITHIN (WLO, XLM, WHI, 0, YES)
      if(YES) then
        if(ION) then
          call COMPD (WVL, CORE, DELTA, FLAG)
          YES = FLAG.ne.0
        end if
        if(YES) then
          DUMP = (KONLUD.gt.0).and.(LDSW.ne.0)
        end if
      end if
C     !END
      call BYE ('KAKI')
C
      return
      end
