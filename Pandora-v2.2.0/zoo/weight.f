      subroutine WEIGHT
     $(A, B,W, KODE, C)
C
C     Rudolf Loeser, 2002 Feb 12
C
C---- Uses weight W in combining A and B to form C.
C
C                 W is the weight of B.
C
C     KODE=0 means: use linear combination;
C     KODE=1 means: use logarithmic combination if appropriate.
C
C---- See Gene Avrett's "(02 Fev 01) Standard weighting"
C     !DASH
      save
C     !DASH
      real*8 A, B, C, OMW, ONE, R, W, X, ZERO
      integer KODE
      logical OK
C     !DASH
      intrinsic abs, sign
C
      data ZERO,ONE /0.D0, 1.D0/
C
C     !BEG
      if(W.ge.ONE) then
        C = B
C
      else if(W.le.ZERO) then
        C = A
C
      else
        OMW = ONE-W
        R = OMW*A+W*B
        if(KODE.gt.0) then
          OK = (A.ne.ZERO).and.(B.ne.ZERO).and.
     $         (sign(ONE,A).eq.sign(ONE,B))
          if(OK) then
            X = (abs(A)**OMW)*(abs(B)**W)
            R = sign(X,B)
          end if
        end if
        C = R
C
      end if
C     !END
C
      return
      end
