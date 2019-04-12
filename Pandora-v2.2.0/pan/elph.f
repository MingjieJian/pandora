      subroutine ELPH
     $(XNE,TE,PEL)
C
C     Rudolf Loeser, 1981 Feb 02
C---- Computes electron pressure.
C     !DASH
      save
C     !DASH
      real*8 BOLZMN, PEL, TE, XNE
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 2),BOLZMN)
C     !DASH
      external HI, BYE
C
      call HI ('ELPH')
C     !BEG
      PEL = BOLZMN*XNE*TE
C     !END
      call BYE ('ELPH')
C
      return
      end
