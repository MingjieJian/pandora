      subroutine BRISTLE
     $(X,W,N,HND,HNKR,ZHEL,ZME,ETA,ZRN,XNE,ZTRM)
C
C     Rudolf Loeser, 1978 Aug 21
C     Revised RL/SGK Apr  9 2014 
C---- Computes new XNE, for SATIN.
C     !DASH
      save
C     !DASH
      real*8 ETA, HND, HNKR, W, X, XNE, ZHEL, ZME, ZRN, ZTRM
      integer I, N
C     !DASH
      external PEARL, ANGRY, HI, BYE
C
      dimension X(*), W(*)
C
C               ETA(N,NMT), ZHEL(N), XNE(N), HND(N), HNKR(N), ZRN(N),
      dimension ETA(*),     ZHEL(*), XNE(*), HND(*), HNKR(*), ZRN(*),
C
C               ZME(N), ZTRM(N,NMT)
     $          ZME(*), ZTRM(N,*)
C
      call HI ('BRISTLE')
C     !BEG
C---- Compute ZME, metal electron ratio
      call PEARL (X,W,XNE,ZME,ETA,ZTRM)
C---- Compute ZRN, ions other than protons
      call ANGRY (X,ZME,ZHEL,ZRN)
C
C---- Compute XNE
      do 100 I = 1,N
        XNE(I) = HND(I)*HNKR(I)+ZRN(I)
  100 continue
C     !END
      call BYE ('BRISTLE')
C
      return
      end
