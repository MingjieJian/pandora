      subroutine CYRENE
     $(X,W,IW,N,KTRN,NDR,MPROM,XNU,XDR,DDR,XNE,CDW,DDL,FDDL,XNEST,CDL,
     $ DP,DW,TAUM,GMMA,CRD,XC,XP,XR,DRLIMI,EP,PE,FE,SLF,BS,GTN,S,CORE,
     $ XITRN,ATRN,XJNU,GMA,DL,SAP,PHI,FAB,FJN,FJJ,VXI,XQSF,XK1,XK2,
     $ DJB,FRD,GRD,XRD,ZRD,YRD,V,DV,AA,UU,PHC,XIK,GII,VG,WG,XJBR,GJN,
     $ FO,IMG)
C
C     Rudolf Loeser, 1985 Jul 17
C---- Controls PRD processing, for HUGH.
C     (This is version 2 of CYRENE.)
C     !DASH
      save
C     !DASH
      real*8 AA, ATRN, BS, CDL, CDW, CORE, CRD, DDL, DDR, DJB, DL, DP,
     $       DRLIMI, DV, DW, EP, FAB, FDDL, FE, FJJ, FJN, FO, FRD, GII,
     $       GJN, GMA, GMMA, GRD, GTN, PE, PHC, PHI, S, SAP, SLF, TAUM,
     $       TIN, UU, V, VG, VXI, W, WG, X, XC, XDR, XIK, XITRN, XJBR,
     $       XJNU, XK1, XK2, XNE, XNEST, XNU, XP, XQSF, XR, XRD, YRD,
     $       ZRD
      integer IMG, IW, KTRN, LJ, LT, LU, MPROM, N, NDR
C     !DASH
C     !EJECT
      external CATANA, ADLER, SECOND, ALCMAN, DEBRIS, MAXIM, OSCAR,
     $         BOOM, PONOBLA, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XJNU(N,KM), DDL(LDL), CDL(LDL), DP(N,LDL), FJJ(N,KTRN),
      dimension XJNU(*),    DDL(*),   CDL(*),   DP(*),     FJJ(*),
C
C               PHC(N,KTRN,LDL), VXI(N,KTRN), FAB(N,KTRN), FJN(N,KTRN),
     $          PHC(*),          VXI(*),      FAB(*),      FJN(*),
C
C               DDR(NDR), XQSF(N,KTRN), XIK(N,KTRN), IMG(N), DRLIMI(N),
     $          DDR(*),   XQSF(*),      XIK(*),      IMG(*), DRLIMI(*),
C
C               PHI(N,KTRN), XDR(NDR), FO(N), XK1(N,KTRN), XK2(N,KTRN),
     $          PHI(*),      XDR(*),   FO(*), XK1(*),      XK2(*),
C
C               DW(N), SLF(N,KM), XNE(N), GTN(N), XITRN(KM), AA(N,LDL),
     $          DW(*), SLF(*),    XNE(*), GTN(*), XITRN(*),  AA(*),
C
C               ATRN(KM), GMA(N), DL(KTRN), SAP(N,LDL), UU(N,KTRN,LDL),
     $          ATRN(*),  GMA(*), DL(*),    SAP(*),     UU(*),
C
C               GII(KTRN,KTRN), VG(KTRN), WG(KTRN), BS(N), FRD(N,KTRN),
     $          GII(*),         VG(*),    WG(*),    BS(*), FRD(*),
C
C               XRD(N,KTRN), YRD(N,KTRN), TAUM(N), EP(N), PE(N), FE(N),
     $          XRD(*),      YRD(*),      TAUM(*), EP(*), PE(*), FE(*),
C
C               DJB(N,KTRN), V(N), DV(N,LDL), GRD(N,KTRN), GJN(N,KTRN),
     $          DJB(*),      V(*), DV(*),     GRD(*),      GJN(*),
C
C               XJBR(N), FDDL(N), XNU(NSL), S(N), CRD(LDL), ZRD(N,KTRN)
     $          XJBR(*), FDDL(*), XNU(*),   S(*), CRD(*),   ZRD(*)
C     !EJECT
C
      call HI ('CYRENE')
C     !BEG
      call SECOND  (TIN)
C---- Set up output units (print header ?)
      call BOOM    (LU, LJ, LT)
      call OSCAR   (LT, 1, 1, CORE, GMMA, XC, XP, XR)
C---- Print line-related input data
      call PONOBLA (LU, N, PE, FE, EP, BS, GTN, S)
C---- Get DL and XIK
      call DEBRIS  (XITRN, KTRN, CDW, DL)
      call MAXIM   (DL, DW, XIK, N, KTRN)
C
C---- Compute and print PHI and integrals of JNU
      call ADLER   (X, W, IW, N, KTRN, NDR, MPROM, LU, DL, ATRN, XITRN,
     $              DDL, CDL, FDDL, CRD, DP, DW, V, XNE, XNEST, TAUM,
     $              GMMA, GMA, XNU, XDR, DDR, XC, XP, DRLIMI, XJNU,
     $              XJBR, PHI, PHC, SAP, GII, FAB, FJN, FJJ, GJN, DV,
     $              AA, UU, VG, WG, XK1, XK2, DJB)
C
C---- Compute and print terms for background calculation
      call CATANA  (X, W, IW, LU, LJ, N, KTRN, GMA, DL, XJBR, PE, FE,
     $              EP, BS, GTN, S, PHI, XIK, XJNU, XK2, FRD, GRD,
     $              XRD, ZRD, YRD, FAB, FJN, FJJ, SLF, VXI, XQSF, FO,
     $              IMG)
C
C---- Sign off
      call ALCMAN  (TIN, LT)
C     !END
      call BYE ('CYRENE')
C
      return
      end
