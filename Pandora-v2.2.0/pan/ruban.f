      subroutine RUBAN
     $(NPQ,P)
C
C     Rudolf Loeser, 2004 Dec 14
C---- Computes a default value of statistical weight for Hydrogen.
C     !DASH
      save
C     !DASH
      real*8 P, PQN, TWO
      integer NPQ
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external HI, BYE
C
      call HI ('RUBAN')
C     !BEG
      PQN = NPQ
      P   = TWO*(PQN**2)
C     !END
      call BYE ('RUBAN')
C
      return
      end
