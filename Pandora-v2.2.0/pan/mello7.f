      subroutine MELLO7
     $(Y,A,B,C,D,E,F,GAMMA,NT,DX,X,Z)
C
C     Rudolf Loeser, 2006 Aug 11
C---- Computes a value of GAMMA for MELILLA.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CQ, D, DX, E, E1FX, E2FX, E3FX, E4FX, F, FOUR, FX,
     $       GAMMA, ONE, THREE, TWO, X, X2, Y, YM, Z
      integer I, NT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 4),THREE )
      equivalence (DLIT( 5),FOUR  )
C     !DASH
      external HART, HI, BYE
C
C               X(NT), Z(NT)
      dimension X(*),  Z(*)
C
      call HI ('MELLO7')
C     !BEG
      X(1) = ONE-DX
      do 100 I = 2,NT
        X(I) = X(I-1)+DX
        X2   = X(I)**2
        FX   = -F*X(I)
        E1FX = exp(FX      )
        E2FX = exp(FX*TWO  )
        E3FX = exp(FX*THREE)
        E4FX = exp(FX*FOUR )
        YM   = exp(-Y*X(I))
        Z(I) = (A/X2+B*E1FX+C*E2FX+D*E3FX+E*E4FX)*YM
  100 continue
C
      call HART ((NT-1), X(2), Z(2), CQ)
C
      YM    = exp(Y)
      GAMMA = (Y*YM)*CQ
C     !END
      call BYE ('MELLO7')
C
      return
      end
