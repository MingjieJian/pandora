      subroutine CEXPINT
     $(N,X,EI,EX)
C
C     Rudolf Loeser, 1981 Feb 27
C---- Computes the N-th Exponential Integral of X.
C     Based on a subroutine for the 1. Exponential Integral written by
C
C     J. W.  C o o l e y ,
C
C     Courant Institute of Mathematical Sciences, New York University.
C---- Input - X, independent variable,
C           - N, order desired (1 .le. N .le. 8).
C     Output - EI, the desired result,
C            - EX, exp(-X).
C---- NOTE-- returns EI(1,0)=0, not infinity!
C     (This is version 2 of CEXPINT.)
C     !DASH
      save
C     !DASH
      real*8 D, E1M, E1SAVE, EI, EX, EXSAVE, F245, F5, ONE, U, X, XSAVE,
     $       Z1, ZERO
      integer I, N
      logical NORMAL
C     !DASH
      external CXPNTX, CXPNT1, CXPNT2, CXPNT3, CXPNT4
C
      data F245, F5, Z1 /2.45D1, 5.D0, -1.D0/
      data XSAVE, EXSAVE, E1SAVE /3*0.D0/
      data ZERO, ONE /0.D0, 1.D0/
C     !EJECT
C
C     !BEG
C---- Check whether this is a special case.
      call CXPNTX       (X,N,EI,EX,NORMAL)
      if(NORMAL) then
C----   Do normal stuff.
        if(X.ne.XSAVE) then
          XSAVE = X
          U = -X
C----     Compute exp(-X).
          EXSAVE = exp(U)
C----     Compute E1.
          if(U.ge.F245) then
            call CXPNT4 (U,EXSAVE,E1M)
          else if(U.ge.F5) then
            call CXPNT3 (U,EXSAVE,E1M)
          else if(U.ge.Z1) then
            call CXPNT2 (U,E1M)
          else
            call CXPNT1 (X,U,EXSAVE,E1M)
          end if
          E1SAVE = -E1M
        end if
        EI = E1SAVE
        EX = EXSAVE
C----   Obtain higher orders recursively.
C       (This upward recursion rapidly loses significant
C        figures when N exceeds about 5.)
        if(N.gt.1) then
          D = ZERO
          do 100 I=2,N
            D  = D-ONE
            EI = (X*EI-EX)/D
  100     continue
        end if
      end if
C     !END
C
      return
      end
