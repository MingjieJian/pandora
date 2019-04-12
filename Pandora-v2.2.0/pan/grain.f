      subroutine GRAIN
     $(A,B,C,R)
C
C     Rudolf Loeser, 2007 Jan 03
C---- Solves a quadratic equation, for CONK.
C     (This is version 4 of GRAIN.)
C     !DASH
      save
C     !DASH
      real*8 A, AC, AOB, B, B2, C, COB, CRIT, FOUR, ONE, R, TWO
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
C     !DASH
      external DIVIDE, HI, BYE
C
      data CRIT /1.D-3/
C
      call HI ('GRAIN')
C     !BEG
      B2 = B**2
      AC = A*C
      call DIVIDE   (AC, B2, AOB)
      if(AOB.gt.CRIT) then
        R = sqrt(B2+FOUR*AC)
        call DIVIDE ((R-B), (TWO*A), R)
      else
        call DIVIDE (C, B, COB)
        R = COB*(ONE-AOB*(ONE-TWO*AOB))
      end if
C     !END
      call BYE ('GRAIN')
C
      return
      end
