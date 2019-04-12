      subroutine SHERBET
     $(X,W,XPBL,N,NLH,XNE,HND,XNC,EIDIF,SA,RAB,HNI,RZM,AEL,BDI,ZME,ZRN,
     $ ZHEL,XNEPR,U,HNP,SAB,ETA,SUM,NO,ZTRM,ZMER)
C
C     Rudolf Loeser, 1978 Aug 18
C     RL/SGK revised Apr  9 2014  
C---- Computes new Electron number density, using
C     the "quadratic expression."
C     (This is version 3 of SHERBET.)
C     !DASH
      save
C     !DASH
      real*8 AEL, BDI, EIDIF, ETA, HND, HNI, HNP, RAB, RZM, SA, SAB,
     $       SUM, U, W, X, XNC, XNE, XNEPR, XPBL, ZHEL, ZME, ZRN, 
     $       ZTRM, ZMER, dummy
      integer I, ITER, LIMIT, N, NLH, NO, jummy
      logical SAME
C     !DASH
      external  BULB, MOVE1, ZLOTY, CONVERD, FRAGA, TYMPANI,
     $          ARRDIV, HI, BYE
C
      dimension X(*), W(*)
C
C               XNE(N), HND(N), HNI(N,NLH), U(N), ETA(N,NMT), XNEPR(N),
      dimension XNE(*), HND(*), HNI(*),     U(*), ETA(*),     XNEPR(*),
C
C               RZM(N), AEL(N), ZHEL(N), SA(N), RAB(N), ZRN(N), SUM(N),
     $          RZM(*), AEL(*), ZHEL(*), SA(*), RAB(*), ZRN(*), SUM(*),
C
C               XPBL(Lenpbl), HNP(N), SAB(N), ZME(N), XNC(N), 
     $          XPBL(*),      HNP(*), SAB(*), ZME(*), XNC(*), 
C
C               BDI(N,NL), ZTRM(N,NMT), ZMER(N)
     $          BDI(N,*),  ZTRM(N,*),   ZMER(*)
C
      data LIMIT /10/
C
      call HI ('SHERBET')
C     !BEG
C---- Compute intermediates
      call BULB      (N,NLH,SA,BDI,HNI,SAB,SUM)
C---- Compute XNE iteratively
      do 100 I = 1,LIMIT
        ITER = I
C----   Save current XNE
        call MOVE1   (XNE,N,XNEPR)
C----   Get new XNE, and other intermediates
        call ZLOTY   (X,W,N,XNE,HND,RAB,SAB,SUM,ZME,ETA,ZRN,U,HNP,ZHEL,
     $                ZTRM)
C----   Check for convergence
        call CONVERD (XNE,1,N,XNEPR,1,N,EIDIF,dummy,jummy,SAME)
        if(SAME) goto 101
  100 continue
  101 continue
C---- Compute XNC and ZMER
      call FRAGA     (X,XPBL)
      call ARRDIV    (ZME, HND, ZMER, N)
C---- Print
      call TYMPANI   (NO,N,NLH,ITER,LIMIT,EIDIF,RZM,ZMER,AEL,ZHEL,HND,
     $                XNC,XNE,SUM,BDI,HNP,SA)
C     !END
      call BYE ('SHERBET')
C
      return
      end
