      subroutine HURT
     $(N,X,F,R)
C
C     Rudolf Loeser, 2006 Dec 20
C---- Trapezoidal-rule integration of F(X), yielding the
C     running integrals in R.
C
C     The table of X values must be sorted into non-decreasing
C     order, and the number of values N must be greater than 1.
C
C     See also HART.
C     !DASH
      save
C     !DASH
      real*8 F, HALF, R, S, X, ZERO
      integer I, N
C     !DASH
C               X(N), F(N), R(N)
      dimension X(*), F(*), R(*)
C
      data ZERO, HALF /0.D0, 5.D-1/
C
C     !BEG
      R(1) = ZERO
      if(N.gt.1) then
        do 100 I = 2,N
          S    = HALF*(F(I)+F(I-1))*(X(I)-X(I-1))
          R(I) = R(I-1)+S
  100   continue
      end if
C     !END
C
      return
      end
