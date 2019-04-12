      subroutine ESKER
     $(XLMTHR,XLM,MODE,GT)
C
C     Rudolf Loeser, 1994 Dec 29
C---- Compares an arbitrary value of wavelength, XLM (Angstroms),
C     with XLMTHR, an ion continuum threshhold wavelength.
C---- MODE = 1 for Hydrogen, = 0 for other.
C
C     Returns  GT = .true.  if XLM > XLMTHR (to tolerance WAVEDEL),
C                 = .false. otherwise;
C
C---- i.e., GT = .true. if XLM does   N O T   fall in this continuum.
C
C     (This is version 3 of ESKER.)
C     !DASH
      save
C     !DASH
      real*8 DELTA, SMLLDEL, XLM, XLMTHR
      integer JDNUK, JDXNU, KODE, MODE
      logical GT
C     !COM
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
      equivalence (MEST(17),JDNUK)
      equivalence (MEST(16),JDXNU)
C
C---- ISOLA       as of 1997 Nov 19
      real*8      WAVEDEL
      common      /ISOLA/ WAVEDEL
C     Two Continuum Wavelength values are "equal" if their
C     relative difference is less than WAVEDEL.
C     .
C     !DASH
      external COMPD, HI, BYE
C
      data SMLLDEL /1.D-15/
C
      call HI ('ESKER')
C     !BEG
      if((MODE.eq.1).and.((JDXNU.gt.0).or.(JDNUK.gt.0))) then
        DELTA = SMLLDEL
      else
        DELTA = WAVEDEL
      end if
C
      call COMPD (XLM,XLMTHR,DELTA,KODE)
      GT = KODE.eq.1
C     !END
      call BYE ('ESKER')
C
      return
      end
