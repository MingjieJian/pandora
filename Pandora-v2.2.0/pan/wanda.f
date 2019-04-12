      subroutine WANDA
     $(GIVEN,OTHER)
C
C     Rudolf Loeser, 1992 Sep 16
C---- Converts wavenumber to Angstroms, or vice versa.
C     (This is version 2 of WANDA.)
C     !DASH
      save
C     !DASH
      real*8 ANGPCM, GIVEN, OTHER
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 6),ANGPCM)
C     !DASH
      external DIVIDE, HI, BYE
C
      call HI ('WANDA')
C     !BEG
      call DIVIDE (ANGPCM,GIVEN,OTHER)
C     !END
      call BYE ('WANDA')
C
      return
      end
