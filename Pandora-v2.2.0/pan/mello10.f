      subroutine MELLO10
     $(Y,A,B,C,D,E,E1Y,GAMMA)
C
C     Rudolf Loeser, 2006 Aug 11
C---- Computes a value of GAMMA for MELILLA.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, DO2, E, E1Y, EMY, GAMMA, ONE, RAT, TRM, TWO, Y
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external EXPINT, DIVIDE, HI, BYE
C
      call HI ('MELLO10')
C     !BEG
      call EXPINT (1, Y, E1Y, EMY)
      call DIVIDE (E1Y, EMY, RAT)
      DO2 = D/TWO
      TRM = B-C*Y+DO2*(Y**2)+E/Y
C
      GAMMA = Y*(A/Y+C+DO2*(ONE-Y)+RAT*TRM)
C     !END
      call BYE ('MELLO10')
C
      return
      end
