      subroutine ROBERT
     $(KOLEV,N,KK,CP,XNU,RK,TRK,TREFF,TR,W)
C
C     Rudolf Loeser, 1992 Jan 15
C---- Computes and saves effective radiation temperature, for ROPE.
C     !DASH
      save
C     !DASH
      real*8 CP, RK, TR, TREFF, TRK, W, XNU
      integer KK, KOLEV, N
      logical DORK, KSHL
C     !DASH
      external KYNAN, MOVE1, HI, BYE
C
      dimension W(*)
C
C               CP(NSL+1), TR(N,NSL), RK(N,NSL), TRK(N,KKX), TREFF(N),
      dimension CP(*),     TR(N,*),   RK(*),     TRK(*),     TREFF(*),
C
C               XNU(NSL)
     $          XNU(*)
C
      data KSHL, DORK /.false., .true./
C
      call HI ('ROBERT')
C     !BEG
      call KYNAN (KOLEV, N, KK, CP, XNU, RK, TRK, KSHL, DORK, TREFF, W)
C
      call MOVE1 (TREFF, N, TR(1,KOLEV))
C     !END
      call BYE ('ROBERT')
C
      return
      end
