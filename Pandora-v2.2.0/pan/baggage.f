      subroutine BAGGAGE
     $(TE,X,PFCO)
C
C     Rudolf Loeser, 1992 Dec 21
C---- Computes CO partition function.
C     (This is version 2 of BAGGAGE.)
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, DIV, E, F, G, PFCO, T, TE, X
C     !DASH
      external HI, BYE
C
      data A,B,C /4.51349D0, 1.84221D1, 5.00599D1/
      data D,E,F /1.02208D2, 1.28504D2, 8.78414D1/
      data G,DIV /2.48533D1, 1.D4/
C
      call HI ('BAGGAGE')
C     !BEG
      T = TE/DIV
      X = A+T*(B-T*(C-T*(D-T*(E-T*(F-T*G)))))
C
      PFCO = exp(X)
C     !END
      call BYE ('BAGGAGE')
C
      return
      end
