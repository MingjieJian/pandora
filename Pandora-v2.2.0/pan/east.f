      subroutine EAST
     $(TAU,RHO)
C
C     Rudolf Loeser, 1987 Jun 18
C---- Computes RHO by the Escape Probability approximation.
C     !DASH
      save
C     !DASH
      real*8 FOUR, HALF, ONE, RHO, ROOTPI, RTAUL, TAU, TAUL, TWO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 5),FOUR  )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 2),ROOTPI)
C     !DASH
      external HI, BYE
C
      call HI ('EAST')
C     !BEG
      if(TAU.le.ONE) then
        RHO = HALF
      else
        TAUL  = log(TAU)
        RTAUL = sqrt(TAUL)
        RHO   = ONE/(TWO+FOUR*ROOTPI*TAU*RTAUL)
      end if
C     !END
      call BYE ('EAST')
C
      return
      end
