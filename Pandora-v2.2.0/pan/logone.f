      subroutine LOGONE
     $(X,XPBL,N,XNE,HNKR,HND,ZHEL,ZME,ZRN,ITER)
C
C     Rudolf Loeser, 1976 Aug 31
C     Revised RL/SGK Apr  9 2014 
C---- Sets Electron Density = Proton Density, and other defaults,
C     for HELGA.
C     !DASH
      save
C     !DASH
      real*8 HND, HNKR, X, XNE, XPBL, ZHEL, ZME, ZRN
      integer ITER, N
C     !DASH
      external ZERO1, ARRMUL, ANGRY, FRAGA, HI, BYE
C
      dimension X(*)
C
C               XPBL(Lenpbl), XNE(N), HNKR(N), HND(N), ZME(N), ZHEL(N)
      dimension XPBL(*),      XNE(*), HNKR(*), HND(*), ZME(*), ZHEL(*),
C
C               ZRN(N)
     $          ZRN(*)
C
      call HI ('LOGONE')
C     !BEG
      call ARRMUL (HND,HNKR,XNE,N)
C
      call ZERO1  (ZME,N)
      call ANGRY  (X,ZME,ZHEL,ZRN)
      call FRAGA  (X,XPBL)
C
      ITER = 0
C     !END
      call BYE ('LOGONE')
C
      return
      end
