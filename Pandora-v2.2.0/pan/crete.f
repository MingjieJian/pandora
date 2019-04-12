      subroutine CRETE
     $(X,W,IW,N,KTRN,NDR,MPROM,XNU,XDR,DDR,XNE,CDW,DDL,FDDL,XNEST,CDL,
     $ DP,DW,TAUM,GMMA,CRD,XC,XP,XR,DRLIMI,EP,PE,FE,SLF,BS,S,CORE,
     $ XITRN,ATRN,XJNU,GMA,DL,SAP,PHI,FAB,FJN,FJJ,FJR,RXI,VXI,XQSF,
     $ SNU,XK1,XK2,DJB,FRD,GRD,XRD,ZRD,YRD,SLR,V,DV,AA,UU,PHC,XIK,GII,
     $ VG,WG,XJBR,GJN,FO,IMG)
C
C     Rudolf Loeser, 1985 Jul 18
C---- Controls PRD processing, for PENNY.
C     (This is version 2 of CRETE.)
C     !DASH
      save
C     !DASH
      real*8 AA, ATRN, BS, CDL, CDW, CORE, CRD, DDL, DDR, DJB, DL, DP,
     $       DRLIMI, DV, DW, EP, FAB, FDDL, FE, FJJ, FJN, FJR, FO, FRD,
     $       GII, GJN, GMA, GMMA, GRD, PE, PHC, PHI, RXI, S, SAP, SLF,
     $       SLR, SNU, TAUM, TIN, UU, V, VG, VXI, W, WG, X, XC, XDR,
     $       XIK, XITRN, XJBR, XJNU, XK1, XK2, XNE, XNEST, XNU, XP,
     $       XQSF, XR, XRD, YRD, ZRD
      integer IMG, IW, KTRN, LJ, LT, LU, MPROM, N, NDR
      logical DOIT
C     !DASH
C     !EJECT
      external DEBRIS, DELOS, CIMON, OSCAR, SECOND, BOOM, ADLER, SHEBA,
     $         ALCMAN, MAXIM, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               FJN(N,KTRN), GMA(N), PHI(N,KTRN), UU(N,KTRN,LDL), V(N),
      dimension FJN(*),      GMA(*), PHI(*),      UU(*),          V(*),
C
C               FJR(N,KTRN), RXI(N,KTRN), AA(N,LDL), DRLIMI(N), XNE(N),
     $          FJR(*),      RXI(*),      AA(*),     DRLIMI(*), XNE(*),
C
C               DP(N,LDL), DW(N), DDL(LDL), FDDL(N), EP(N), XJNU(N,KM),
     $          DP(*),     DW(*), DDL(*),   FDDL(*), EP(*), XJNU(*),
C
C               DJB(N,KTRN), XIK(N,KTRN), FJJ(N,KTRN), WG(KTRN), BS(N),
     $          DJB(*),      XIK(*),      FJJ(*),      WG(*),    BS(*),
C
C               XDR(NDR), DDR(NDR), DL(KTRN), FRD(N,KTRN), GRD(N,KTRN),
     $          XDR(*),   DDR(*),   DL(*),    FRD(*),      GRD(*),
C
C               TAUM(N), XK1(N,K), XK2(N,K), VG(K), PE(N), GJN(N,KTRN),
     $          TAUM(*), XK1(*),   XK2(*),   VG(*), PE(*), GJN(*),
C
C               GII(KTRN,KTRN), XRD(N,KTRN), YRD(N,KTRN), FO(N), FE(N),
     $          GII(*),         XRD(*),      YRD(*),      FO(*), FE(*),
C
C               SLF(N,KTRN), VXI(N,KTRN), XQSF(N,KTRN), XNU(NSL), S(N),
     $          SLF(*),      VXI(*),      XQSF(*),      XNU(*),   S(*),
C
C               CDL(LDL), XITRN(KM), ATRN(KM), SAP(N,LDL), FAB(N,KTRN),
     $          CDL(*),   XITRN(*),  ATRN(*),  SAP(*),     FAB(*),
C
C               PHC(N,KTRN,LDL), SNU(N,KTRN), SLR(N,KTRN), ZRD(N,KTRN),
     $          PHC(*),          SNU(*),      SLR(*),      ZRD(*),
C
C               XJBR(N), IMG(N), CRD(LDL), DV(N,LDL)
     $          XJBR(*), IMG(*), CRD(*),   DV(*)
C     !EJECT
C
      call HI ('CRETE')
C     !BEG
      call SECOND  (TIN)
C---- Set up output units (print header ?)
      call BOOM    (LU, LJ, LT)
      call OSCAR   (LU, 2, 1, CORE, GMMA, XC, XP, XR)
C---- Get DL and XIK
      call DEBRIS  (XITRN, KTRN, CDW, DL)
      call MAXIM   (DL, DW, XIK, N, KTRN)
C
C---- Compute and print PHI and integrals of JNU
      call ADLER   (X, W, IW, N, KTRN, NDR, MPROM, LU, DL, ATRN,
     $              XITRN, DDL, CDL, FDDL, CRD, DP, DW, V, XNE, XNEST,
     $              TAUM, GMMA, GMA, XNU, XDR, DDR, XC, XP, DRLIMI,
     $              XJNU, XJBR, PHI, PHC, SAP, GII, FAB, FJN, FJJ,
     $              GJN, DV, AA, UU, VG, WG, XK1, XK2, DJB)
C
C---- Compute and print terms for Line Source Function calculation
      call DELOS   (X, W, IW, LU, LJ, N, KTRN, DL, XIK, GMA, EP, PE,
     $              FE, XJBR, XK2, FRD, GRD, XRD, ZRD, YRD, FAB, FJN,
     $              XJNU, FJR, RXI, CORE, GMMA, XC, XP, XR)
C
      call SHEBA   (DOIT)
      if(DOIT) then
C----   Compute and print PRD-modified source functions
        call CIMON (N, KTRN, LU, DL, GMA, EP, BS, S, XJNU, FAB, FJN,
     $              FJJ, SLF, XRD, YRD, VXI, XQSF, SNU, SLR, FO, IMG)
      end if
C
C---- Sign off
      call ALCMAN  (TIN, LT)
C     !END
      call BYE ('CRETE')
C
      return
      end
