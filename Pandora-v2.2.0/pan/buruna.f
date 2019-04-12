      subroutine BURUNA
     $(X,N,W)
C
C     Rudolf Loeser, 2002 Jan 08
C---- Computes a set of trapezoidal-rule integration weights,
C     for integrating a tabular function of X(i), 1 .le. i .ne. N.
C
C     BURUNA treats a Special Case: the integration starts at an
C     implicit zero (i.e. X(0) = 0 and f(0) = 0).
C
C     (Also, it is ASSUMED that the tabulated values of X are
C     in ascending order, and that N .gt. 2 .)
C     !DASH
      save
C     !DASH
      real*8 HALF, W, X
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
C     !DASH
      external HI, BYE
C
C               X(N), W(N)
      dimension X(*), W(*)
C
      call HI ('BURUNA')
C     !BEG
      W(1) = X(2)*HALF
      do 100 I = 2,(N-1)
        W(I) = (X(I+1)-X(I-1))*HALF
  100 continue
      W(N) = (X(N)-X(N-1))*HALF
C     !END
      call BYE ('BURUNA')
C
      return
      end
