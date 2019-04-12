      subroutine SPLINTER
     $(X,Y,N,Y2,U, Y1)
C     Rudolf Loeser, 1997 Jan 21
C---- This is an adaptation of SUBROUTINEs spline & splint
C
C     from:   NUMERICAL ALGORITHMS, in Fortran, 2nd edition,
C     by Press, Teukolsky, Vetterling, anf Flannery.
C
C---- Uses "natural" cubic splines to deliver a table of first
C     derivative, Y1(X), given the table Y(X).
C     (U and Y2 are working storage.)
C     !DASH
      save
C     !DASH
      real*8 HALF, ONE, P, SIG, SIX, THREE, TWO, U, X, XC, XL, XU, Y,
     $       Y1, Y2, YL, YU, ZERO
      integer I, K, M, N
C     !DASH
      external ABORT
C
      dimension X(N), Y(N), U(N), Y1(N), Y2(N)
C
      data ZERO,HALF,ONE,TWO,THREE,SIX /0.D0,.5D0,1.D0,2.D0,3.D0,6.D0/
C     !EJECT
C
C     !BEG
      M = N-1
C
      Y2(1) = ZERO
      U(1)  = ZERO
C
      do 11 I = 2,M
        YL  = Y(I  )-Y(I-1)
        YU  = Y(I+1)-Y(I  )
        XL  = X(I  )-X(I-1)
        XU  = X(I+1)-X(I  )
        XC  = X(I+1)-X(I-1)
        SIG = XL/XC
        P   = SIG*Y2(I-1)+TWO
        if(P.eq.ZERO) then
          write (*,10) I,M
   10     format(' ','SPLINTER:  I =',I6,' of',I6,'; divisor P = 0.')
          call ABORT
        end if
        Y2(I) = (SIG-ONE)/P
        U(I)  = (SIX*(YU/XU-YL/XL)/XC-SIG*U(I-1))/P
   11 continue
C
      U(N)  = ZERO
      Y2(N) = ZERO
C
      do 12 K = M,2,-1
        Y2(K) = Y2(K)*Y2(K+1)+U(K)
   12 continue
C
      do 13 I = 1,M
        YU    = Y(I+1)-Y(I)
        XU    = X(I+1)-X(I)
        Y1(I) = YU/XU-(TWO*Y2(I)+Y2(I+1))*(XU/SIX)
   13 continue
C
      YL    = Y(N)-Y(M)
      XL    = X(N)-X(M)
      Y1(N) = YL/XL+(Y2(M)+TWO*Y2(N))*(XL/SIX)
C     !END
C
      return
      end
