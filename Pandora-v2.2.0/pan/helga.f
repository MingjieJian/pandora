      subroutine HELGA
     $(X,W,XPBL,EIDIF,N,NMT,NO,XNC,XNE,HNK,ZME,HND,RZM,AEL,XNEPR,HNKR,
     $ ETA,ZRN,ITER,IQNES,ZHEL,DUMP,ZTRM,ZMER)
C
C     Rudolf Loeser, 1978 Aug 21
C     RL/SGK revised Apr  9 2014 
C---- Obtains new Electron Density, using the "direct expression,"
C     and some related quantities, for ASTRID.
C     Note: HNK is needed only if HNKR are all zero.
C     (This is version 2 of HELGA.)
C     !DASH
      save
C     !DASH
      real*8 AEL, EIDIF, ETA, HND, HNK, HNKR, RZM, W, X, XNC, XNE,
     $       XNEPR, XPBL, ZHEL, ZME, ZRN, ZTRM, ZMER
      integer IQNES, ITER, N, NMT, NO
      logical DUMP
C     !DASH
      external TIBISEE, LOGONE, SATIN, HI, BYE
C
      dimension X(*), W(*)
C
C               XNE(N), HNK(N), ZME(N), HND(N), RZM(N), XNC(N), AEL(N),
      dimension XNE(*), HNK(*), ZME(*), HND(*), RZM(*), XNC(*), AEL(*),
C
C               XPBL(Lenpbl), XNEPR(N), HNKR(N), ETA(N,NMT), ZHEL(N),
     $          XPBL(*),      XNEPR(*), HNKR(*), ETA(*),     ZHEL(*),
C
C               ZRN(N), ZTRM(N,NMT), ZMER(N)
     $          ZRN(*), ZTRM(N,*),   ZMER(*)
C
      call HI ('HELGA')
C     !BEG
C---- Compute HNKR (if needed)
      call TIBISEE   (HNK, HND, N, HNKR)
C
      if(IQNES.le.0) then
C----   Easy way out: electrons = protons
        call LOGONE  (X, XPBL, N, XNE, HNKR, HND, ZHEL, ZME, ZRN, ITER)
      else
C----   Use "direct expression" iteratively (and print)
        call SATIN   (X, W, XPBL, N,NMT, ITER, NO, XNE, XNC, XNEPR,
     $                ZME, HND, HNKR, RZM, AEL, ZHEL, ETA, ZRN, EIDIF,
     $                DUMP, ZTRM, ZMER)
      end if
C     !END
      call BYE ('HELGA')
C
      return
      end
