      subroutine COMCOS
     $(X,Y,CR,CI)
C     Rudolf Loeser, 1978 Dec 13
C---- Computes the complex cosine of the argument (X+i*Y),
C     returning the answer as (CR+i*CI).
C     !DASH
      save
C     !DASH
      real*4 CI, CR, EM, EP, HALF, X, Y
C     !DASH
      data HALF /0.5/
C
C     !BEG
      EP = exp(Y)
      EM = 1./EP
      CR = HALF*cos(X)*(EP+EM)
      CI =-HALF*sin(X)*(EP-EM)
C     !END
C
      return
      end
