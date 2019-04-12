      subroutine PELYX
     $(D,F1,P,NE)
C
C     Rudolf Loeser, 1989 Oct 19
C---- Converts existing P(Lambda-1) to P(Lambda), for DURIAN.
C     !DASH
      save
C     !DASH
      real*8 D, F1, ONE, P
      integer I, NE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
C               D(NE), F1(NE), P(NE,NE)
      dimension D(*),  F1(*),  P(NE,*)
C
      call HI ('PELYX')
C     !BEG
      do 100 I = 1,NE
        P(I,I) = P(I,I)+F1(I)*(ONE-D(I))
  100 continue
C     !END
      call BYE ('PELYX')
C
      return
      end
