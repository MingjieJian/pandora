      subroutine BELOWLD
     $(X,RES)
C     Rudolf Loeser, 1979 Apr 23
C---- Returns the largest integral power of 10
C     which is not larger than X.
C     !DASH
      save
C     !DASH
      real*8 A, B, ONE, RES, TEN, X, ZERO
C     !DASH
      external  BELOWD
      intrinsic abs, sign
C
      data ZERO,ONE,TEN /0.D0, 1.D0, 1.D1/
C
C     !BEG
      if(X.eq.ZERO) then
        B = ZERO
      else
C
        A = log10(abs(X))
        call BELOWD (A,ONE,B)
        B = TEN**B
        B = sign(B,X)
      end if
C
      RES = B
C     !END
C
      return
      end
