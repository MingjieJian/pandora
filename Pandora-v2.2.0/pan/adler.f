      subroutine ADLER
     $(X,W,IW,N,K,NDR,MPROM,LU,DL,A,XI,DDL,CDL,FDDL,CRD,DP,DW,V,XNE,
     $ XNEST,TAUM,GMMA,GMA,XNU,XDR,DDR,XC,XP,DRLIMI,XJNU,XJBR,PHI,PHC,
     $ SAP,GII,FAB,FJN,FJJ,GJN,DV,AA,UU,VG,WG,XK1,XK2,DJB)
C
C     Rudolf Loeser, 2005 Jan 14
C---- Computes and prints PHI and integrals of JNU, for PRD.
C     !DASH
      save
C     !DASH
      real*8 A, AA, CDL, CORE, CRD, DDL, DDR, DJB, DL, DP, DRLIMI, DV,
     $       DW, FAB, FDDL, FJJ, FJN, GII, GJN, GMA, GMMA, PHC, PHI,
     $       SAP, TAUM, UU, V, VG, W, WG, X, XC, XDR, XI, XJBR, XJNU,
     $       XK1, XK2, XNE, XNEST, XNU, XP
      integer IW, K, LDL, LU, MPROM, MUSE, N, NDR
C     !COM
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
      equivalence (LINKDS(12),LDL  )
C     !DASH
C     !EJECT
      external PROFILE, HEXAGON, NIMBLE, JAFFA, EUSAPIA, GAFFE, ZERO1,
     $         SAPON, FALKE, BOUR, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               DDL(LDL), CDL(LDL), DL(K), A(K), V(N), DW(N), FJJ(N,K),
      dimension DDL(*),   CDL(*),   DL(*), A(*), V(*), DW(*), FJJ(*),
C
C               SAP(N,LDL), AA(N,LDL), UU(N,K,LDL),  DP(N,LDL), XNE(N),
     $          SAP(*),     AA(*),     UU(*),        DP(*),     XNE(*),
C
C               PHC(N,K,LDL), XJNU(N,K), DV(N,LDL), TAUM(N), DRLIMI(N),
     $          PHC(*),       XJNU(*),   DV(*),     TAUM(*), DRLIMI(*),
C
C               XDR(NDR), DDR(NDR), FAB(N,K), FJN(N,K), FDDL(N), XI(K),
     $          XDR(*),   DDR(*),   FAB(*),   FJN(*),   FDDL(*), XI(*),
C
C               XK1(N,K), XK2(N,K), GII(K,K), VG(K), GJN(N,K),
     $          XK1(*),   XK2(*),   GII(*),   VG(*), GJN(*),
C
C               XJBR(N), CRD(LDL), WG(K), XNU(NSL), PHI(N,K), DJB(N,K),
     $          XJBR(*), CRD(*),   WG(*), XNU(*),   PHI(*),   DJB(*),
C
C               GMA(N)
     $          GMA(*)
C
      data CORE,MUSE /0.D0, 1/
C     !EJECT
C
      call HI ('ADLER')
C     !BEG
C---- PHI (with V=0)
      call ZERO1   (V, N)
      call NIMBLE  (XNEST, XNE, FDDL, N)
      call PROFILE (CORE, DDL, FDDL, CDL, LDL, DL, K, MPROM, MUSE,
     $              XNE, DP, DW, V, N, PHI, DV, AA, UU, PHC, W, IW)
C---- SAP
      call SAPON   (N, K, LDL, A, PHC, SAP)
C---- JBAR
      call EUSAPIA (K, A, LDL, CDL, N, SAP, XJNU, PHC, XJBR)
C---- GMA
      call HEXAGON (N, LDL, CRD, XNU, GMMA, DP, GMA)
C---- FAB, FJN, FJJ and XK1
      call JAFFA   (N, K, A, DL, DW, DP, TAUM, GMA, PHC, PHI, XJNU,
     $              SAP, NDR, XDR, DDR, XC, XP, DRLIMI, LDL, DDL,
     $              FDDL, CDL, FAB, FJN, FJJ, XK1, DJB)
C---- GJN and XK2
      call GAFFE   (N, K, A, DL, DW, DP, XJNU, DJB, LDL, DDL, FDDL,
     $              CDL, GII, VG, WG, GJN, XK2)
C---- Print
      call FALKE   (LU, N, K, DP, DW, SAP, GMA, DRLIMI, XI, DL, A,
     $              XJNU, XJBR, PHI, FAB, FJN, FJJ, GJN, XK1, XK2,
     $              DJB)
C---- Checksums
      call BOUR    (SAP, GMA, PHI, XJNU, XJBR, FAB, FJN, FJJ, GJN,
     $              DJB, XK1, XK2, N, K, LDL)
C     !END
      call BYE ('ADLER')
C
      return
      end
