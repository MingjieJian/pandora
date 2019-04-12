      subroutine GILDAS
     $(N,VXS,GTN,COP,DP,DW,MPROM,XNE,DDL,FDDL,CDL,LDL,XRAY,Z,R1N,WAVE,
     $ DL,LL,NSHL,NRPMX,CODSRW,IND,TOPT,PHI,WN,WH,ILFLX,Y,MOVING,
     $ RKODE,IMG,W,IW,DUMP)
C
C     Rudolf Loeser, 1983 Mar 02
C---- Gets PHI, WN, WH and RKODE, for all shell rays, for ANGARAD.
C     !DASH
      save
C     !DASH
      real*8 CDL, CODSRW, COP, DDL, DL, DP, DW, FDDL, GTN, PHI, R1N,
     $       RKODE, VXS, W, WAVE, WH, WN, XNE, XRAY, Y, Z
      integer IAA, ICKLR, ICKP, ICPX, IDPX, IDV, IDWX, IEMX, IFDLX,
     $        IGTX, ILFLX, IMG, IN, IND, IOPAR, IOPAW, IPHC, IPHIR,
     $        IPHX, IS, ITAUW, ITNU, IUU, IVP, IVXX, IW, IWHR, IWNR,
     $        IWRK, IXNEX, LDL, LL, MOX, MPROM, N, NRPMX, NSHL
      logical DUMP, MOVING, TOPT
C     !DASH
      external MEDLAR, PALADUR, FAKIR, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               NRM = 2*N + 5
C
C               DDL(LDL), CDL(LDL), GTN(N), COP(N), DP(N,LDL), FDDL(N),
      dimension DDL(*),   CDL(*),   GTN(*), COP(*), DP(*),     FDDL(*),
C
C               RKODE(NSHL), WN(N,N,NSHL), WH(N,N,NSHL), VXS(N), DL(1),
     $          RKODE(*),    WN(*),        WH(*),        VXS(*), DL(*),
C
C               CODSRW(NSHL), XRAY(NRM,NSHL), PHI(N,N,NSHL), IMG(N),
     $          CODSRW(*),    XRAY(*),        PHI(*),        IMG(*),
C
C               DW(N), XNE(N), Z(N)
     $          DW(*), XNE(*), Z(*)
C
      dimension IN(24)
      equivalence
     $(IN( 1),IOPAR ),(IN( 2),ICKLR ),(IN( 3),IPHIR ),(IN( 4),IDPX  ),
     $(IN( 5),IDWX  ),(IN( 6),IVXX  ),(IN( 7),ICPX  ),(IN( 8),IEMX  ),
     $(IN( 9),IGTX  ),(IN(10),IPHX  ),(IN(11),IVP   ),(IN(12),IDV   ),
     $(IN(13),IAA   ),(IN(14),IUU   ),(IN(15),ITNU  ),(IN(16),ITAUW ),
     $(IN(17),IOPAW ),(IN(18),IWNR  ),(IN(19),IWHR  ),(IN(20),IFDLX ),
     $(IN(21),IPHC  ),(IN(22),ICKP  ),(IN(23),IXNEX ),(IN(24),IWRK  )
C     !EJECT
C
      call HI ('GILDAS')
C     !BEG
C     (Get, and allocate, W allotment)
      call MEDLAR  (IN, IS, MOX, 'GILDAS')
C
      call PALADUR (N, VXS, GTN, COP, DP, DW, MPROM, XNE, DDL, FDDL,
     $              CDL, LDL, XRAY, Z, R1N, WAVE, DL, LL, NSHL, NRPMX,
     $              CODSRW, IND, TOPT, PHI, WN, WH, ILFLX, Y, MOVING,
     $              RKODE, W(IOPAR), W(ICKLR), W(IPHIR), W(IDPX),
     $              W(IDWX), W(IVXX), W(IXNEX), W(ICPX), W(IEMX),
     $              W(IGTX), W(IFDLX), W(IPHX), W(IVP), W(IDV), W(IAA),
     $              W(IUU), W(ITNU), W(ITAUW), W(IOPAW), W(IWNR),
     $              W(IWHR), W(IPHC), W(ICKP), IMG, W(IWRK), W, IW,
     $              DUMP)
      call FAKIR   (N, NSHL, WN, CODSRW, W, DUMP)
      if(ILFLX.gt.0) then
        call FAKIR (N, NSHL, WH, CODSRW, W, DUMP)
      end if
C
C     (Give back W allotment)
      call WGIVE   (W, 'GILDAS')
C     !END
      call BYE ('GILDAS')
C
      return
      end
