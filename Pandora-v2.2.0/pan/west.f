      subroutine WEST
     $(TAU,DP,DW,RHO)
C
C     Rudolf Loeser, 1987 Jun 18
C---- Computes RHO by the Escape Probability approximation.
C     !DASH
      save
C     !DASH
      real*8 A, ARG, DP, DW, FOUR, G, OLD, ONE, RAT, RHO, ROOT, ROOTPI,
     $       RTAUL, TAU, TAUL, THIRD, TWO, ZERO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(13),THIRD )
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
C     !EJECT
      external  DIVIDE, HI, BYE
      intrinsic max
C
      call HI ('WEST')
C     !BEG
      TAUL  = log(TAU+ONE)
      RTAUL = sqrt(TAUL)
      OLD   = ONE/(TWO+FOUR*ROOTPI*TAU*RTAUL)
C
      call DIVIDE   (DP, DW, A)
      if(A.le.ZERO) then
        G = ZERO
      else
        call DIVIDE (ONE, (A*TAU), RAT)
        if(RAT.ge.ONE) then
          ARG = (A**2)/ROOTPI
        else
          ARG = A/(TAU*ROOTPI)
        end if
        ROOT = sqrt(ARG)
        G    = THIRD*ROOT
      end if
C
      RHO = max(OLD,G)
C     !END
      call BYE ('WEST')
C
      return
      end
