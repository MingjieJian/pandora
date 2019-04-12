      subroutine HART
     $(N,X,F,S)
C
C     Rudolf Loeser, 2005 Feb 10
C---- Trapezoidal-rule integration of F(X), yielding the whole
C     integral in S.
C
C     The table of X values must be sorted into non-decreasing
C     order, and the number of values N must be greater than 1.
C
C     See also HURT.
C     !DASH
      save
C     !DASH
      real*8 F, HALF, S, X, ZERO
      integer I, N
C     !DASH
C               X(N), F(N)
      dimension X(*), F(*)
C
      data ZERO, HALF /0.D0, 5.D-1/
C
C     !BEG
      S = ZERO
      if(N.gt.1) then
        do 100 I = 2,N
          S = S+HALF*(F(I)+F(I-1))*(X(I)-X(I-1))
  100   continue
      end if
C     !END
C
      return
      end
