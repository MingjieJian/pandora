      subroutine KIDDER
     $(XLM,YES,DUMP)
C
C     Rudolf Loeser, 1978 Feb 03
C           revised, 2002 Sep 18
C                    2003 Oct 07
C
C---- Tells whether H Lyman alpha, or higher H Ly lines, background
C     opacity contributions should be computed at this wavelength.
C
C---- Also, sets the debug switch.
C     (See also DIKKER.)
C     !DASH
      save
C     !DASH
      real*8 RYDBRG, XLM, XLMZ
      integer LYODS
      logical DUMP, YES
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
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 32),XLMZ )
      equivalence (KZQ( 89),LYODS)
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 4),RYDBRG)
C     !DASH
C     !EJECT
      external HI, BYE
C
      call HI ('KIDDER')
C     !BEG
      YES  = (XLM.gt.RYDBRG).and.(XLM.le.XLMZ)
      DUMP = (KONLUD.gt.0).and.(LYODS.ne.0)
C     !END
      call BYE ('KIDDER')
C
      return
      end
