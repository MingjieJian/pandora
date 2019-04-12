      subroutine HARTR
     $(N,X,F,S)
C
C     Rudolf Loeser, 2005 Dec 20
C---- Trapezoidal-rule integration of F(X).
C     The table of X values must be sorted into non-decreasing
C     order, and the number of values N must be greater than 1.
C
C---- Version of HART for real*4 operands.
C     !DASH
      save
C     !DASH
      real*4 F, HALF, S, X, ZERO
      integer I, N
C     !DASH
C               X(N), F(N)
      dimension X(*), F(*)
C
      data ZERO, HALF /0.E0, 5.E-1/
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
