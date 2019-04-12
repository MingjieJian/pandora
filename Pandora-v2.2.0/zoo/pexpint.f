      subroutine PEXPINT
     $(N,X,EI,EX,ERROR)
C     Rudolf Loeser, 1988 Nov 21
C---- Computes the N'th Exponential Integral of X,
C     for all values of X .ge. 0 and N .ge. 0; except
C     that N must be .gt. 1 when X .eq. 0.
C     Also returns EX = exp(-X).
C     Also returns ERROR = .true. if ITMAX was insufficient
C     for continued fraction or series.
C
C     Based on the code
C     of
C     Evaluating Continued Fractions and Computing Exponential Integrals
C     by
C     William H.  P r e s s  and Saul A.  T e u k o l s k y
C     in
C     Computers in Physics, Sep/Oct 1988, pp. 88-89.
C     !DASH
      save
C     !DASH
      real*8 EI, EX, FNM1, ONE, X, ZERO
      integer N, NM1
      logical ERROR
C     !DASH
      external ABORT, PXPNT1, PXPNT2
C
      data  ZERO,ONE /0.D0, 1.D0/
C
C     !BEG
      if((N.lt.0).or.(X.lt.ZERO).or.((X.eq.ZERO).and.(N.lt.2))) then
        write (*,100) N,X
  100   format(' ','PEXPINT:  N =',I12,', X =',1PE24.16,'; bad ',
     $             'arguments.')
        call ABORT
      end if
      ERROR = .false.
C
      NM1  = N-1
      FNM1 = NM1
      EX   = exp(-X)
      if(N.eq.0) then
        EI = EX/X
      else if(X.eq.ZERO) then
        EI = ONE/FNM1
      else if(X.gt.ONE) then
C----   Lentz's algorithm
        call PXPNT1 (NM1,X,EI,EX,ERROR)
      else
C----   Series expansion
        call PXPNT2 (NM1,X,EI,FNM1,ERROR)
      end if
C     !END
C
      return
      end
