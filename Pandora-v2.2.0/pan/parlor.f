      subroutine PARLOR
     $(X,XCBL,KTRU,MODE,KHED,PPRNT)
C
C     Rudolf Loeser, 2002 Sep 20
C---- Sets up Continuum-Calculation output unit numbers and switches
C     in KONOUT.
C     !DASH
      save
C     !DASH
      real*8 X, XCBL, XLM
      integer KHED, KKLAMD, KTRU, MO, MODE
      logical PPRNT, PRNTB, PRNTD, TOOT
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
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(54),KKLAMD)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
C     !EJECT
      external VALENS, BUPPI, DUMPER, HI, BYE
C
      dimension X(*)
C
C               XCBL(Miklen)
      dimension XCBL(*)
C
      data TOOT /.false./
C
      call HI ('PARLOR')
C     !BEG
      XLM    = XCBL(KKLAMD)
      KONHED = KHED
C
      if(MODE.eq.1) then
        KONLUN = MO
      else
C----   Set up unit for ANY printout (i.e. NO or MO)
        call VALENS   (KONLUN)
      end if
C
C---- Determine whether XLM is a wavelength of a type for which
C     printout has been requested, or whether it is one of SCOW,
C     the special continuum output wavelengths
      KONLUR = 0
      call BUPPI      (X, XLM, KTRU, PRNTB)
      if(PRNTB) then
        KONLUR = KONLUN
      end if
C
C---- Output requested for this PRD wavelength
      if(PPRNT) then
        KONLUR = KONLUN
      end if
C
C---- Determine whether XLM is one of DWAVE, the specific continuum
C     dump output wavelength table, or equals one of the other
C     special wavelengths
      KONLUD = 0
      call DUMPER     (X, XLM, PRNTD)
      if(PRNTD.and.(KONLUN.gt.0)) then
        KONLUR = KONLUN
        KONLUD = 1
      end if
C
      if(TOOT) then
        write (*,100) XLM,KTRU,MODE,KHED,PRNTB,PRNTD,KONLUN,KONLUR,
     $                KONLUD,KONHED
  100   format(' ','PARLOR',1PE20.12,', KTRU =',I2,', MODE =',I2,
     $             ', KHED =',I2,', PB =',L2,', PD =',L2,';  LUN =',I3,
     $             ', LUR =',I3,', LUD =',I2,', HED =',I2)
      end if
C     !END
      call BYE ('PARLOR')
C
      return
      end
