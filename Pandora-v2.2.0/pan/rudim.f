      subroutine RUDIM
     $(HND,HEND,RH,RS)
C
C     Rudolf Loeser, 1998 Mar 18
C---- Computes coefficients for diffusion calculations.
C     !DASH
      save
C     !DASH
      real*8 HEMASS, HEND, HND, RH, RS, T, X
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 5),HEMASS)
C     !DASH
      external DIVIDE, HI, BYE
C
      call HI ('RUDIM')
C     !BEG
      X = HEND*HEMASS
      T = HND+X
      call DIVIDE (HND,T,RH)
      call DIVIDE (X  ,T,RS)
C     !END
      call BYE ('RUDIM')
C
      return
      end
