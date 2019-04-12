      subroutine QUADDER
     $(X,F,N,DEL,XST,FP)
C
C     Rudolf Loeser, 1997 Feb 06
C---- Computes F' from quadratics.
C
C     X, F, FP and DEL are tables, each of length N.
C
C     DEL is a table of X-intervals such that
C     DEL(i) = X(i) - X(i-1), 2 .le. i .le. N; DEL(1) = 0.
C     NOTE: all DEL(i), 2 .le. i .le. N, must be .gt. 0!
C
C     If upon input the parameter XST = .true., then the
C     proper values of DEL are assumed to exist;
C     if XST = .false., then values of DEL are computed by
C     QUADDER, and XST is set = .true.
C     !DASH
      save
C     !DASH
      real*8 DEL, F, FP, SB, SF, X, ZERO
      integer I, M, N
      logical XST
C     !DASH
      external ABORT
C
      dimension X(*), F(*), FP(*), DEL(*)
C
      data ZERO /0.D0/
C
C     !BEG
      M = N-1
C
      if(.not.XST) then
        DEL(1) = ZERO
        do 101 I = 2,N
          DEL(I) = X(I)-X(I-1)
          if(DEL(I).le.ZERO) then
            write (*,100) I,DEL(I)
  100       format(' ','Error in QUADDER: i =',I12,', DEL(i) =',1PE16.8)
            call ABORT
          end if
  101   continue
        XST = .true.
      end if
C
      FP(1) = (F(2)-F(1))/DEL(2)
      FP(N) = (F(N)-F(M))/DEL(N)
C
      do 102 I = 2,M
        SF    = F(I+1)-F(I)
        SB    = F(I)-F(I-1)
        FP(I) = ((DEL(I)**2)*SF      +(DEL(I+1)**2)*SB) /
     $          ((DEL(I)**2)*DEL(I+1)+(DEL(I+1)**2)*DEL(I))
  102 continue
C     !END
C
      return
      end
