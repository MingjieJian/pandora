      subroutine SATIN
     $(X,W,XPBL,N,NMT,ITER,NO,XNE,XNC,XNEPR,ZME,HND,HNKR,RZM,AEL,ZHEL,
     $ ETA,ZRN,EIDIF,DUMP,ZTRM,ZMER)
C
C     Rudolf Loeser, 1978 Aug 21
C     RL/SGK revised Apr  9 2014  
C---- Computes XNE by the "direct expression," for HELGA.
C     !DASH
      save
C     !DASH
      real*8 AEL, EIDIF, ETA, HND, HNKR, RZM, W, X, XNC, XNE, XNEPR,
     $       XPBL, ZHEL, ZME, ZRN, dummy, ZTRM, ZMER
      integer I, ITER, LIMIT, N, NMT, NO, jummy
      logical DUMP, SAME
C     !DASH
      external MOVE1, BRISTLE, CONVERD, FRAGA, SILK, DRUM, HI, 
     $         ARRDIV,  BYE
C
      dimension X(*), W(*)
C
C               XNE(N), XNEPR(N), ZME(N), HNKR(N), ETA(N,NMT), ZHEL(N),
      dimension XNE(*), XNEPR(*), ZME(*), HNKR(*), ETA(*),     ZHEL(*),
C
C               XPBL(Lenpbl), RZM(N), AEL(N), HND(N), XNC(N), ZRN(N),
     $          XPBL(*),      RZM(*), AEL(*), HND(*), XNC(*), ZRN(*),
C
C               ZTRM(N,NMT), ZMER(N)
     $          ZTRM(N,*),   ZMER(*)
C
      data LIMIT /10/
C
      call HI ('SATIN')
C     !BEG
C---- Compute XNE iteratively
      do 100 I = 1,LIMIT
        ITER = I
C----   Save current XNE
        call MOVE1   (XNE, N, XNEPR)
C----   Get new XNE
        call BRISTLE (X, W, N, HND, HNKR, ZHEL, ZME, ETA, ZRN, XNE, 
     $                ZTRM)
C----   Check for convergence
        call CONVERD (XNEPR, 1, N, XNE, 1, N, EIDIF, dummy, jummy, SAME)
        if(DUMP) then
C         Debug printout
          call SILK  (N, NMT, I, EIDIF, SAME, HND, HNKR, ZHEL, RZM, AEL,
     $                ETA, ZME, XNEPR, XNE, ZRN)
        end if
        if(SAME) goto 101
  100 continue
  101 continue
C---- Compute XNC and ZMER
      call FRAGA     (X, XPBL)
      call ARRDIV    (ZME, HND, ZMER, N)
C---- Print
      call DRUM      (NO, ITER, LIMIT, EIDIF, N, RZM, ZMER, AEL, 
     $                ZHEL, HND, XNC, XNE)
C     !END
      call BYE ('SATIN')
C
      return
      end
