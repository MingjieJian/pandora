      subroutine ERITH
     $(N,LG,CMU,XLM,FMULT,CKL,ZAXA,ZAYA,BHS,SIGMAS,BHSNMS,CNXP,CSFCT,
     $ INDEX,XJNU,SOURCE,ITS,LAG,OPAC,WN,W,IW,DUMP)
C
C     Rudolf Loeser, 1983 Feb 18
C---- Gets XJNU, SOURCE, ITS and LAG, for SALAMIS.
C     !DASH
      save
C     !DASH
      real*8 BHS, BHSNMS, CKL, CMU, CNXP, CSFCT, FMULT, OPAC, SIGMAS,
     $       SOURCE, W, WN, XJNU, XLM, ZAXA, ZAYA
      integer ICI, IJNO, IN, INDEX, IRR, IS, IS1, IS2, ITS, IW, IXA,
     $        IXM, IYA, LAG, LG, MOX, N
      logical DUMP
C     !DASH
      external IVOR, CREMONA, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               CMU(LG), BHSNMS(N), XJNU(N), CNXP(N), ZAYA(N), ZAXA(N),
      dimension CMU(*),  BHSNMS(*), XJNU(*), CNXP(*), ZAYA(*), ZAXA(*),
C
C               SIGMAS(N), SOURCE(N), OPAC(N,N,LG), WN(N,N,LG), BHS(N),
     $          SIGMAS(*), SOURCE(*), OPAC(*),      WN(*),      BHS(*),
C
C               CKL(N,N,LG)
     $          CKL(*)
C
      dimension IN(8)
      equivalence
     $(IN( 1),IXA   ),(IN( 2),IYA   ),(IN( 3),ICI   ),(IN( 4),IXM   ),
     $(IN( 5),IRR   ),(IN( 6),IJNO  ),(IN( 7),IS1   ),(IN( 8),IS2   )
C
      call HI ('ERITH')
C     !BEG
C     (Get, and allocate, W allotment)
      call IVOR    (IN, IS, MOX, 'ERITH')
C
      call CREMONA (CMU, LG, N, XLM, FMULT, CKL, ZAXA, ZAYA, BHS,
     $              SIGMAS, BHSNMS, CNXP, CSFCT, INDEX, XJNU, SOURCE,
     $              ITS, LAG, OPAC, W(IXA), W(IYA), W(ICI), WN, W(IXM),
     $              W(IRR), W(IJNO), W(IS1), W(IS2), W, IW, DUMP)
C
C     (Give back W allotment)
      call WGIVE   (W, 'ERITH')
C     !END
      call BYE ('ERITH')
C
      return
      end
