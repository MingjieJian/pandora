      subroutine ABOVELD
     $(X,RES)
C
C     Rudolf Loeser, 1979 Apr 23
C---- RES will be the smallest integral power of 10
C     which is not smaller than X.
C     !DASH
      save
C     !DASH
      real*8 A, B, ONE, RES, TEN, X, ZERO
C     !DASH
      external  ABOVED
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
        call ABOVED (A,ONE,B)
        B = TEN**B
        B = sign(B,X)
      end if
C
      RES = B
C     !END
C
      return
      end
