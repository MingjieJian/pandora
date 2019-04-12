      subroutine BELOWL
     $(X,RES)
C     Rudolf Loeser, 1979 Apr 23
C---- RES will be the largest integral power of 10
C     which is not larger than X.
C     !DASH
      save
C     !DASH
      real*4 A, B, ONE, RES, TEN, X, ZERO
C     !DASH
      external  BELOW
      intrinsic abs, sign
C
      data ZERO,ONE,TEN /0.E0, 1.E0, 1.E1/
C
C     !BEG
      if(X.eq.ZERO) then
        B = ZERO
      else
        A = log10(abs(X))
        call BELOW (A,ONE,B)
        B = TEN**B
        B = sign(B,X)
      end if
      RES = B
C     !END
C
      return
      end
