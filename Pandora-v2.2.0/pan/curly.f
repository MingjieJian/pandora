      subroutine CURLY
     $(X,E1E,E2E)
C
C     Rudolf Loeser, 2005 Oct 25
C---- Computes approximations for DULBA.
C     (This is version 2 of CURLY.)
C     !DASH
      save
C     !DASH
      real*8 E1E, E2E, EIGHT, FOUR, ONE, SIX, SIXTEEN, TWO, X, X26, XP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 5),FOUR  )
      equivalence (DLIT( 7),SIX   )
      equivalence (DLIT( 9),EIGHT )
C     !DASH
      external HI, BYE
C
      data SIXTEEN /1.6D1/
C
      call HI ('CURLY')
C     !BEG
      X26 = SIX*(X**2)
      XP  = X+ONE
      E1E = (ONE+ONE/(XP**2)+(ONE-TWO*X)/(XP**4)
     $      +(X26-EIGHT*X+ONE)/(XP**6))/XP
      XP  = X+TWO
      E2E = (ONE+TWO/(XP**2)+TWO*(TWO-TWO*X)/(XP**4)
     $      +TWO*(X26-SIXTEEN*X+FOUR)/(XP**6))/XP
C     !END
      call BYE ('CURLY')
C
      return
      end
