      subroutine SDERIV2
     $(X,F,SD1,N,W1,W2,W3,IM)
C
C     Rudolf Loeser, 1997 Mar 10
C---- Computes the first derivative of F(X):
C     this routine is analogous to DERIV1, but F is smoothed first.
C     Also, cubic spline is not fitted to a constant tail (if any).
C     W1, W2, W3, and IM are working storage.
C     !DASH
      save
C     !DASH
      real*8 CRIT, F, SD1, W1, W2, W3, X
      integer IM, INDX, ITMX, KRET, M, N
      logical EDIT
C     !DASH
      external  SLATHER, ABORT, GUYOT, ZERO1, SPLINTER
C
      dimension X(N), F(N), SD1(N), W1(N), W2(N), W3(N), IM(N)
C
      data EDIT /.true./
      data CRIT, ITMX, INDX /1.D-3, 20, 0/
C
C     !BEG
      call SLATHER    (X, F, N, CRIT, ITMX, INDX, W1, W2, W3, IM, KRET)
      if(KRET.lt.0) then
        write (*,100) KRET
  100   format(' ','Error in SDERIV2: KRET =',I12)
        call ABORT
      end if
C
      call GUYOT      (F, N, EDIT, M)
      if(M.gt.2) then
        call SPLINTER (X, F, N, W1, W2, SD1)
      else
        M = 0
      end if
C
      if(M.lt.N) then
        call ZERO1    (SD1(M+1), (N-M))
      end if
C     !END
C
      return
      end
