      subroutine ALAPOD
     $(NE,F1,EP1,P)
C
C     Rudolf Loeser, 2001 Dec 28
C---- Computes matrix, for PALDAO.
C     !DASH
      save
C     !DASH
      real*8 EP1, F1, ONE, P, R
      integer I, J, NE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, UNITADD, HI, BYE
C
C               P(NE,NE), EP1(NE), F1(NE)
      dimension P(NE,*),  EP1(*),  F1(*)
C
      call HI ('ALAPOD')
C     !BEG
      do 101 I = 1,NE
        R = ONE+EP1(I)
        do 100 J = 1,NE
          call DIVIDE ((-P(I,J)),(R*F1(J)),P(I,J))
  100   continue
  101 continue
      call UNITADD    (1,NE,P,NE)
C     !END
      call BYE ('ALAPOD')
C
      return
      end
