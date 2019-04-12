      subroutine CANUTE
     $(X,W,IW,N,VXS,GTN,COP,DP,DW,XNE,MPROM,DDL,FDDL,CDL,LDL,XMU,WAVE,
     $ DL,LL,LG,IND,PHI,WN,WH,ILFLX,Y,MOVING,RKODE,IMG,DUMP)
C
C     Rudolf Loeser, 1983 Mar 01
C---- Gets PHI, WN, WH and RKODE, for BITTERN.
C     !DASH
      save
C     !DASH
      real*8 CDL, COP, DDL, DL, DP, DW, FDDL, GTN, PHI, RKODE, VXS, W,
     $       WAVE, WH, WN, X, XMU, XNE, Y
      integer IAA, ICKL, ICKP, IDV, IEMU, ILFLX, IMG, IN, IND, IOPAC,
     $        IOPAW, IPHC, IPHP, IS, ITAUW, ITMU, IUU, IVP, IW, IWRK,
     $        LDL, LG, LL, MOX, MPROM, N
      logical DUMP, MOVING
C     !DASH
      external KINCAR, CAHORS, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               VXS(N), CDL(LDL), DDL(LDL), DW(N), XMU(LG), WH(N,N,LG),
      dimension VXS(*), CDL(*),   DDL(*),   DW(*), XMU(*),  WH(*),
C
C               DP(N,LDL), PHI(N,N,LG), RKODE(LG), FDDL(N), WN(N,N,LG),
     $          DP(*),     PHI(*),      RKODE(*),  FDDL(*), WN(*),
C
C               DL(1), COP(N), IMG(N), XNE(N), GTN(N)
     $          DL(*), COP(*), IMG(*), XNE(*), GTN(*)
C
      dimension IN(14)
      equivalence
     $(IN( 1),IPHP  ),(IN( 2),IVP   ),(IN( 3),IDV   ),(IN( 4),IAA   ),
     $(IN( 5),IUU   ),(IN( 6),IEMU  ),(IN( 7),ITMU  ),(IN( 8),IOPAW ),
     $(IN( 9),ITAUW ),(IN(10),ICKL  ),(IN(11),IOPAC ),(IN(12),ICKP  ),
     $(IN(13),IPHC  ),(IN(14),IWRK  )
C
      call HI ('CANUTE')
C     !BEG
C     (Get, and allocate, W allotment)
      call KINCAR (IN, IS, MOX, 'CANUTE')
C
      call CAHORS (X, W, IW, N, VXS, GTN, COP, DP, DW, XNE, MPROM, DDL,
     $             FDDL, CDL, LDL, XMU, WAVE, DL, LL, LG, IND, PHI, Y,
     $             MOVING, WN, RKODE, WH, ILFLX, W(IPHP), W(IPHC),
     $             W(ICKP), W(IVP), W(IDV), W(IAA), W(IUU), W(IEMU),
     $             W(ICKL), W(IOPAC), W(ITMU), W(IOPAW), W(ITAUW), IMG,
     $             W(IWRK), DUMP)
C
C     (Give back W allotment)
      call WGIVE  (W, 'CANUTE')
C     !END
      call BYE ('CANUTE')
C
      return
      end
