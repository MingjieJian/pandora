      subroutine RAGAN
     $(N,VXS,GTN,COP,DP,DW,MPROM,XNE,DDL,FDDL,CDL,LDL,EMU,XRAY,WAVE,DL,
     $ LL,MRR,IND,TOPT,PHI,WN,WH,ILFLX,Y,MOVING,RKODE,IMG,W,IW,DUMP)
C
C     Rudolf Loeser, 1983 Mar 02
C---- Gets PHI, WN, WH and RKODE, for all disk rays, for ANGARAD.
C     !DASH
      save
C     !DASH
      real*8 CDL, COP, DDL, DL, DP, DW, EMU, FDDL, GTN, PHI, RKODE, VXS,
     $       W, WAVE, WH, WN, XNE, XRAY, Y
      integer IAA, ICKL, ICKP, IDV, ILFLX, IMG, IN, IND, IOPAC, IOPAW,
     $        IPHC, IPHP, IS, ITAUW, ITNU, IUU, IVP, IW, IWRK, LDL, LL,
     $        MOX, MPROM, MRR, N
      logical DUMP, MOVING, TOPT
C     !DASH
      external MALGO, PEREDUR, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               FDDL(N), VXS(N), GTN(N), COP(N), CDL(LDL), WH(N,N,MRR),
      dimension FDDL(*), VXS(*), GTN(*), COP(*), CDL(*),   WH(*),
C
C               DDL(LDL), EMU(N,MRR), XRAY(N,MRR), IMG(N), WN(N,N,MRR),
     $          DDL(*),   EMU(*),     XRAY(*),     IMG(*), WN(*),
C
C               PHI(N,N,MRR), RKODE(MRR), DP(N,LDL), DL(1), XNE(N),
     $          PHI(*),       RKODE(*),   DP(*),     DL(*), XNE(*),
C
C               DW(N)
     $          DW(*)
C
      dimension IN(13)
      equivalence
     $(IN( 1),IVP   ),(IN( 2),IDV   ),(IN( 3),IAA   ),(IN( 4),IUU   ),
     $(IN( 5),IPHP  ),(IN( 6),ITNU  ),(IN( 7),IOPAW ),(IN( 8),ITAUW ),
     $(IN( 9),ICKL  ),(IN(10),IOPAC ),(IN(11),ICKP  ),(IN(12),IPHC  ),
     $(IN(13),IWRK  )
C
      call HI ('RAGAN')
C     !BEG
C     (Get, and allocate, W allotment)
      call MALGO   (IN, IS, MOX, 'RAGAN')
C
      call PEREDUR (N, VXS, GTN, COP, DP, DW, MPROM, XNE, DDL, FDDL,
     $              CDL, LDL, EMU, XRAY, WAVE, DL, LL, MRR, IND, TOPT,
     $              PHI, WN, RKODE, W(IPHP), W(IVP), W(IDV), W(IAA),
     $              W(IUU), W(ICKL), W(IOPAC), W(ITNU), W(IOPAW),
     $              W(ITAUW), WH, ILFLX, Y, MOVING, W(IPHC), W(ICKP),
     $              IMG, W(IWRK), W, IW, DUMP)
C
C     (Give back W allotment)
      call WGIVE   (W, 'RAGAN')
C     !END
      call BYE ('RAGAN')
C
      return
      end
