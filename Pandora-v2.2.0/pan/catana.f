      subroutine CATANA
     $(X,W,IW,LU,LJ,N,K,GMA,DL,XJBR,PE,FE,EP,BS,GTN,S,PHI,XIK,XJNU,
     $ XK2,FRD,GRD,XRD,ZRD,YRD,FAB,FJN,FJJ,SLF,VXI,XQSF,FO,IMG)
C
C     Rudolf Loeser, 2005 Jan 14
C---- Computes PRD terms for CYRENE.
C     (This is version 3 of CATANA.)
C     !DASH
      save
C     !DASH
      real*8 BS, DL, EP, FAB, FE, FJJ, FJN, FO, FRD, GMA, GRD, GTN, PE,
     $       PHI, S, SLF, VXI, W, X, XIK, XJBR, XJNU, XK2, XQSF, XRD,
     $       YRD, ZRD
      integer IMG, IW, K, LJ, LU, N
C     !DASH
      external PANTHER, SLATE, MARCOS, TURNIP, JOUR, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               BS(N), IMG(N), XQSF(N,K), VXI(N,K), SLF(N,K), PHI(N,K),
      dimension BS(*), IMG(*), XQSF(*),   VXI(*),   SLF(*),   PHI(*),
C
C               XJBR(N), FE(N), GTN(N), XIK(N,K), GMA(N), DL(K), EP(N),
     $          XJBR(*), FE(*), GTN(*), XIK(*),   GMA(*), DL(*), EP(*),
C
C               GRD(N,K), XRD(N,K), YRD(N,K), XK2(N,K), FRD(N,K), S(N),
     $          GRD(*),   XRD(*),   YRD(*),   XK2(*),   FRD(*),   S(*),
C
C               FAB(N,K), FJN(N,K), FJJ(N,K), XJNU(N,K), PE(N), FO(N),
     $          FAB(*),   FJN(*),   FJJ(*),   XJNU(*),   PE(*), FO(*),
C
C               ZRD(N,K)
     $          ZRD(*)
C
      call HI ('CATANA')
C     !BEG
C---- VXI
      call PANTHER (N, K, GMA, EP, FAB, VXI)
C---- QSF
      call SLATE   (N, K, GMA, FJJ, FJN, FAB, SLF, EP, BS, XQSF, IMG,
     $              FO)
C---- CHL and DHL
      call MARCOS  (X, W, K, LU, N, GMA, XJBR, PE, FE, EP, XK2, FRD,
     $              GRD, XRD, ZRD, YRD)
C
C---- Print
      call TURNIP  (LU, LJ, IW, N, K, DL, XJNU, XIK, SLF, VXI, XQSF,
     $              XRD, ZRD, YRD, S, GTN, PHI)
C
C---- Checksums
      call JOUR    (VXI, XQSF, XRD, ZRD, YRD, N, K)
C     !END
      call BYE ('CATANA')
C
      return
      end
