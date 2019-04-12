      subroutine COMCOSD
     $(X,Y,CR,CI)
C
C     Rudolf Loeser, 1978 Dec 13
C---- Computes the complex cosine of the argument (X+i*Y),
C     returning the answer as (CR+i*CI).
C     !DASH
      save
C     !DASH
      real*8 CI, CR, CX, EM, EP, HALF, ONE, SX, X, Y
C     !DASH
      data HALF,ONE /5.D-1, 1.D0/
C
C     !BEG
      EP = exp(Y)
      CX = cos(X)
      SX = sin(X)
      EM = ONE/EP
      CR = HALF*CX*(EP+EM)
      CI =-HALF*SX*(EP-EM)
C     !END
C
      return
      end
