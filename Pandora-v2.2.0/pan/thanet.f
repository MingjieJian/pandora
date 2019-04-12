      subroutine THANET
     $(CSHL,NSHL,CDSK,MRR,N,XLM,FMULT,CKLSHL,CKLDSK,ZAXA,ZAYA,CBHS,
     $ SIGMAS,BHSNMS,CNXP,CSFCRIT,INDSHL,INDDSK,XJNU,SOURCE,ITS,LAG,
     $ OPACSHL,OPACDSK,WNSHL,WNDSK,W,IW,DUMP)
C
C     Rudolf Loeser, 1983 Feb 23
C---- Gets XJNU, SOURCE, ITS and LAG, for SAMOS.
C     !DASH
      save
C     !DASH
      real*8 BHSNMS, CBHS, CDSK, CKLDSK, CKLSHL, CNXP, CSFCRIT, CSHL,
     $       FMULT, OPACDSK, OPACSHL, SIGMAS, SOURCE, W, WNDSK, WNSHL,
     $       XJNU, XLM, ZAXA, ZAYA
      integer IJNO, IN, INDDSK, INDSHL, IRR, IS, IS1, IS2, ITS, IW,
     $        IXAD, IXAS, IXM, IYAD, IYAS, LAG, MOX, MRR, N, NSHL
      logical DUMP
C     !DASH
      external KINOC, VERONA, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               CNXP(N), SIGMAS(N), WNDSK(N,N,MRR), ZAXA(N), BHSNMS(N),
      dimension CNXP(*), SIGMAS(*), WNDSK(*),       ZAXA(*), BHSNMS(*),
C
C               OPACSHL(N,N,NSHL), SOURCE(N), WNSHL(N,N,NSHL), XJNU(N),
     $          OPACSHL(*),        SOURCE(*), WNSHL(*),        XJNU(*),
C
C               CKLSHL(N,N,NSHL), OPACDSK(N,N,MRR), CKLDSK(N,N,MRR),
     $          CKLSHL(*),        OPACDSK(*),       CKLDSK(*),
C
C               CSHL(N,NSHL), CBHS(N), CDSK(N,MRR), ZAYA(N)
     $          CSHL(*),      CBHS(*), CDSK(*),     ZAYA(*)
C
      dimension IN(9)
      equivalence
     $(IN( 1),IXAS  ),(IN( 2),IXAD  ),(IN( 3),IYAS  ),(IN( 4),IYAD  ),
     $(IN( 5),IXM   ),(IN( 6),IRR   ),(IN( 7),IJNO  ),(IN( 8),IS1   ),
     $(IN( 9),IS2   )
C
      call HI ('THANET')
C     !BEG
C     (Get, and allocate, W allotment)
      call KINOC  (IN, IS, MOX, 'THANET')
C
      call VERONA (CSHL, NSHL, CDSK, MRR, N, XLM, FMULT, CKLSHL,
     $             CKLDSK, ZAXA, ZAYA, CBHS, SIGMAS, BHSNMS, CNXP,
     $             CSFCRIT, INDSHL, INDDSK, XJNU, SOURCE, ITS, LAG,
     $             OPACSHL, OPACDSK, W(IXAS), W(IXAD), W(IYAS),
     $             W(IYAD), WNSHL, WNDSK, W(IXM), W(IRR), W(IJNO),
     $             W(IS1), W(IS2), W, IW, DUMP)
C
C     (Give back W allotment)
      call WGIVE  (W, 'THANET')
C     !END
      call BYE ('THANET')
C
      return
      end
