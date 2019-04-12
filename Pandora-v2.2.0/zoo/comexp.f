      subroutine COMEXP
     $(X,Y,ER,EI)
C     Rudolf Loeser, 1978 Dec 13
C---- Computes the complex exponential of the argument (X+i*Y),
C     returning the answer as (ER+i*EI).
C     !DASH
      save
C     !DASH
      real*4 EI, ER, EX, X, Y
C     !DASH
C
C     !BEG
      EX = exp(X)
      ER = cos(Y)*EX
      EI = sin(Y)*EX
C     !END
C
      return
      end
