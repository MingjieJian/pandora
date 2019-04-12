      subroutine QUASH
     $(ELL,Z,H,THETA,WAS)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes WAS, an asymptotic term for the calculation of
C     Partition Functions.
C     (For reference, see listing of "DEPART".)
C     !DASH
      save
C     !DASH
      real*8 A, ARG, C, ELL, EM2, EMM, ENN, ESS, H, HALF, HUNDRED, OE,
     $       OH, ONE, R, RA, S, SEM, THETA, THIRD, TT, W1, W2, WAS, Z,
     $       ZERO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(13),THIRD )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  DIVIDE, HI, BYE
      intrinsic min
C
      data C,HUNDRED /3.1321D1, 1.D2/
C
      call HI ('QUASH')
C     !BEG
      A   = C*THETA*(Z**2)
      ARG = A*HUNDRED
      RA  = sqrt(ARG)
      EMM = RA-ELL
C
      if(EMM.lt.ONE) then
        N = 0
      else
        N = EMM+HALF
        ENN = N
        if((ELL+ENN).gt.H) then
          N = H-ELL
        end if
      end if
      ENN = N
C     !EJECT
      ESS = ELL+ENN
C
      W1 = ZERO
      if(N.ne.0) then
        SEM = ELL-ONE
        do 100 I = 1,(N+1)
          SEM = SEM+ONE
          EM2 = SEM**2
          call DIVIDE (A, EM2, ARG)
          ARG = min(ARG,HUNDRED)
          TT  = exp(ARG)
          W1  = W1+EM2*TT
  100   continue
      end if
C
      if((ESS+ONE).ge.H) then
        W2 = ZERO
      else
        R = THIRD*(H*(H+HALF)*(H+ONE)-ESS*(ESS+HALF)*(ESS+ONE))
        S = (H-ESS+ONE)
        call DIVIDE   (ONE, (ESS-HALF), OE)
        call DIVIDE   (ONE, (H  +HALF), OH)
        TT = HALF*(OE-OH)
        W2 = R+A*(S+A*TT)
      end if
C
      WAS = W1+W2
C     !END
      call BYE ('QUASH')
C
      return
      end
