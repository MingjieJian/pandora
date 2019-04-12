      subroutine KYNAN
     $(JLEV,N,MTR,CP,XNU,RK,TR,KSHL,DORK,TREFF,W)
C
C     Rudolf Loeser, 1991 Mar 22
C---- Controls calculation of TREFF, "effective radiation temperature,"
C     for level JLEV.
C     (This is version 2 of KYNAN.)
C     !DASH
      save
C     !DASH
      real*8 CP, RK, TR, TREFF, W, XNU
      integer IN, IRKI, IRKR, IS, ITRI, JLEV, MOX, MTR, N
      logical DORK, KSHL
C     !DASH
      external ZERO1, ELIAS, POLLY, WGIVE, HI, BYE
C
      dimension W(*)
C
C               CP(NSL+1), XNU(NSL), RK(N), TR(N,MTR), TREFF(N)
      dimension CP(*),     XNU(*),   RK(*), TR(*),     TREFF(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),ITRI ),(IN( 2),IRKR ),(IN( 3),IRKI )
C
      call HI ('KYNAN')
C     !BEG
      call ZERO1   (TREFF, N)
      if(DORK.and.(.not.KSHL).and.(MTR.gt.1)) then
C       (Get, and allocate, W allotment)
        call ELIAS (IN, IS, MOX, 'KYNAN', MTR)
C
        call POLLY (JLEV, N, MTR, TR, CP, XNU, RK, TREFF, W(ITRI),
     $              W(IRKR), W(IRKI))
C
C       (Give back W allotment)
        call WGIVE (W, 'KYNAN')
      end if
C     !END
      call BYE ('KYNAN')
C
      return
      end
