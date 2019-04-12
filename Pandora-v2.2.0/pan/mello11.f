      subroutine MELLO11
     $(Y,A,B,C,D,E,F,E1Y,GAMMA)
C
C     Rudolf Loeser, 2006 Aug 11
C---- Computes a value of GAMMA for MELILLA.
C     !DASH
      save
C     !DASH
      real*8 A, B, BT, C, CT, D, DT, E, E1F, E1Y, E2F, E3F, E4F, EMY,
     $       ET, F, FOUR, GAMMA, ONE, RAT, THREE, TWO, Y
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
      external EXPINT, DIVIDE, HI, BYE
C
      call HI ('MELLO11')
C     !BEG
      call EXPINT (1, Y, E1Y, EMY)
      call DIVIDE (E1Y, EMY, RAT)
C
      E1F = exp(-F      )
      E2F = exp(-F*TWO  )
      E3F = exp(-F*THREE)
      E4F = exp(-F*FOUR )
      BT  = (B*E1F)/(Y+F      )
      CT  = (C*E2F)/(Y+F*TWO  )
      DT  = (D*E3F)/(Y+F*THREE)
      ET  = (E*E4F)/(Y+F*FOUR )
C
      GAMMA = Y*( A*(ONE-RAT*Y) + (BT+CT+DT+ET) )
C     !END
      call BYE ('MELLO11')
C
      return
      end
