      subroutine BERBER
     $(X,W,IW,XLM,DUMP,TAU,BHS,BHSNUM,SIGMA,CNXP,Z,OPAC,N,YDAMP,IQINC,
     $ CSFCRIT,XJNU,SOURCE,WN,WH,MOVING,ILFLX,ITS,LAG,IMG)
C
C     Rudolf Loeser, 1983 Jan 24
C---- Computes Continuum Jnu, directly, and S, (and weight matrices).
C     (Special version of "TUAREG", for P.R.D. Jnu calculation.)
C     !DASH
      save
C     !DASH
      real*8 BHS, BHSNUM, CNXP, CSFCRIT, OPAC, SIGMA, SOURCE, TAU, W,
     $       WH, WN, X, XJNU, XLM, YDAMP, Z
      integer ILFLX, IMG, IN, IQINC, IRR, IS, ITS, IW, IXA, IXJRO, IXM,
     $        IYA, LAG, MOX, N
      logical DUMP, MOVING
C     !DASH
      external HAUSSA, ARRDIV, CRANE, GELA, DELLA, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               TAU(N), BHS(N), BHSNUM(N), SIGMA(N), SOURCE(N), IMG(N),
      dimension TAU(*), BHS(*), BHSNUM(*), SIGMA(*), SOURCE(*), IMG(*),
C
C               CNXP(N), XJNU(N), WN(N,N), WH(N,N), Z(N), OPAC(N)
     $          CNXP(*), XJNU(*), WN(*),   WH(*),   Z(*), OPAC(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IXA   ),(IN( 2),IYA   ),(IN( 3),IXM   ),(IN( 4),IRR   ),
     $(IN( 5),IXJRO )
C
      call HI ('BERBER')
C     !BEG
C     (Get, and allocate, W allotment)
      call HAUSSA (IN, IS, MOX, 'BERBER')
C
C---- Compute auxiliary functions XA and YA
      call ARRDIV (SIGMA , OPAC, W(IXA), N)
      call ARRDIV (BHSNUM, OPAC, W(IYA), N)
C---- Compute Jnu
      call CRANE  (X, W, IW, DUMP, TAU, W(IXA), W(IYA), XJNU, N,
     $             W(IXM), WN, WH, MOVING, ILFLX, IQINC, IMG, CNXP,
     $             YDAMP, W(IRR), W(IXJRO), ITS, XLM, CSFCRIT, Z, OPAC)
C---- Check validity of Jnu
      call GELA   (XJNU, N, LAG)
C---- Get Source Function
      call DELLA  (N, SOURCE, BHS, W(IXA), W(IYA), XJNU, LAG)
C
C     (Give back W allotment)
      call WGIVE  (W, 'BERBER')
C     !END
      call BYE ('BERBER')
C
      return
      end
