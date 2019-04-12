      subroutine HEESUM
     $(X,DT,N,F1,F2)
C
C     Rudolf Loeser, 2004 Jul 28
C---- Computes the integrals f1(x) amd f2(x) for collisional ionization.
C     (This is version 2 of HEESUM.)
C     !DASH
      save
C     !DASH
      real*8 ARG, DT, E1, E1P, E2, E2P, F1, F2, HALF, ONE, T, TLG, X, Z,
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
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
      call HI ('HEESUM')
C     !BEG
      T  = ONE
      Z  = HALF*DT
      F1 = ZERO
      F2 = ZERO
      E1 = ONE
      E2 = ZERO
      do 100 I = 1,N
        E1P = E1
        E2P = E2
        T   = T+DT
        TLG = log(T)
        ARG = X*(ONE-T)
        E1  = exp(ARG)
        E1  = E1/T
        E2  = E1*TLG
C
        F1 = F1+(E1+E1P)*Z
        F2 = F2+(E2+E2P)*Z
  100 continue
C     !END
      call BYE ('HEESUM')
C
      return
      end
