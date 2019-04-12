      subroutine SDERIV1
     $(X,F,SD1,N,W1,W2,W3,IM)
C
C     Rudolf Loeser, 1988 Feb 24
C---- Computes the first derivative of F(X):
C     this routine is analogous to DERIV1,
C     except that F is smoothed first.
C     W1, W2, W3, and IM are working storage.
C     !DASH
      save
C     !DASH
      real*8 CRIT, F, SD1, W1, W2, W3, X
      integer IM, INDX, ITMX, KRET, N
C     !DASH
      external  SLATHER, ABORT, DERIV1
C
      dimension X(N), F(N), SD1(N), W1(N), W2(N), W3(N), IM(N)
C
      data ITMX,CRIT,INDX /20, 1.D-3, 0/
C
C     !BEG
      call SLATHER (X, F, N, CRIT, ITMX, INDX, W1, W2, W3, IM, KRET)
C
      if(KRET.lt.0) then
        write (*,100) KRET
  100   format(' ','Error in SDERIV1: KRET =',I12)
        call ABORT
      end if
C
      call DERIV1  (X, F, SD1, N)
C     !END
C
      return
      end
