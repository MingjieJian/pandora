      subroutine COMEXPD
     $(X,Y,ER,EI)
C     Rudolf Loeser, 1978 Dec 13
C---- Computes the complex exponential of the argument (X+i*Y),
C     returning the answer as (ER+i*EI).
C     !DASH
      save
C     !DASH
      real*8 CY, EI, ER, EX, SY, X, Y
C     !DASH
C
C     !BEG
      EX = exp(X)
      CY = cos(Y)
      SY = sin(Y)
      ER = EX*CY
      EI = EX*SY
C     !END
C
      return
      end
