      subroutine NATASHA
     $(TE,W1,W2,N)
C
C     Rudolf Loeser, 1972 Sep 07
C---- Computes intermediates, for TAMARA.
C     !DASH
      save
C     !DASH
      real*8 CON23, CON24, RT, TE, W1, W2
      integer I, N
C     !DASH
      external RIGEL, HI, BYE
C
C               TE(N), W1(N), W2(N)
      dimension TE(*), W1(*), W2(*)
C
      call HI ('NATASHA')
C     !BEG
      call RIGEL (23, CON23)
      call RIGEL (24, CON24)
      do 100 I = 1,N
        RT    =  sqrt(TE(I))
        W1(I) =  (RT**3)/CON23
        W2(I) = -CON24/TE(I)
  100 continue
C     !END
      call BYE ('NATASHA')
C
      return
      end
