      subroutine DELOS
     $(X,W,IW,LU,LJ,N,K,DL,XIK,GMA,EP,PE,FE,XJBR,XK2,FRD,GRD,XRD,ZRD,
     $ YRD,FAB,FJN,XJNU,FJR,RXI,CORE,GMMA,XC,XP,XR)
C
C     Rudolf Loeser, 2005 Jan 19
C---- Computes PRD terms for CRETE.
C     (This is version 3 of DELOS.)
C     !DASH
      save
C     !DASH
      real*8 CHL, CORE, DHL, DL, EP, FAB, FE, FJN, FJR, FRD, GMA, GMMA,
     $       GRD, PE, RXI, W, X, XC, XIK, XJBR, XJNU, XK2, XP, XR, XRD,
     $       YRD, ZRD
      integer IW, K, LJ, LU, N
C     !DASH
      external FARRAH, PERINTH, MARCOS, BATAK, NOUR, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               FAB(N,K), XJNU(N,K), EP(N), GMA(N), XRD(N,K), YRD(N,K),
      dimension FAB(*),   XJNU(*),   EP(*), GMA(*), XRD(*),   YRD(*),
C
C               XK2(N,K), FRD(N,K), GRD(N,K), FJN(N,K), XJBR(N), DL(K),
     $          XK2(*),   FRD(*),   GRD(*),   FJN(*),   XJBR(*), DL(*),
C
C               FJR(N,K), RXI(N,K), XIK(N,K), PE(N), FE(N), ZRD(N,K)
     $          FJR(*),   RXI(*),   XIK(*),   PE(*), FE(*), ZRD(*)
C
      call HI ('DELOS')
C     !BEG
C---- FJR
      call FARRAH  (N, K, FJN, XJNU, FAB, FJR)
C---- RXI
      call PERINTH (N, K, EP, GMA, FAB, FJR, RXI)
C---- CHL and DHL
      call MARCOS  (X, W, K, LU, N, GMA, XJBR, PE, FE, EP, XK2, FRD,
     $              GRD, XRD, ZRD, YRD)
C
C---- Print
      call BATAK   (LU, LJ, IW, N, K, DL, XJNU, XIK, FJR, RXI, FRD,
     $              GRD, XRD, ZRD, YRD, CORE, GMMA, XC, XP, XR)
C
C---- Checksums
      call NOUR    (XRD, ZRD, YRD, FJR, RXI, N, K)
C     !END
      call BYE ('DELOS')
C
      return
      end
