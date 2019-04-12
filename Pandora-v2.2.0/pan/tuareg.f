      subroutine TUAREG
     $(X,W,IW,XLM,DUMP,TAU,BHS,BHSNUM,SIGMA,CNXP,Z,OPAC,II,NR,N,YDAMP,
     $ IQINC,CSFCRIT,XJNU,SOURCE,ITS,LAG,IMG)
C
C     Rudolf Loeser, 1981 Jul 22
C---- Computes Continuum Jnu, directly, and S.
C     (See also "BERBER".)
C     !DASH
      save
C     !DASH
      real*8 BHS, BHSNUM, CNXP, CSFCRIT, OPAC, SIGMA, SOURCE, TAU, W, X,
     $       XJNU, XLM, YDAMP, Z
      integer ICNXR, II, ILFLX, IMG, IN, IQINC, IRR, IS, ITAUR, ITS, IW,
     $        IWH, IWN, IXA, IXAR, IXJR, IXJRO, IXM, IYA, IYAR, IZR,
     $        LAG, MOX, N, NR
      logical DUMP, MOVING
C     !DASH
      external TIBBU, ARRDIV, VESTA, CRANE, LUXURY, GELA, DELLA, WGIVE,
     $         HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               TAU(N), SOURCE(N), SIGMA(N), BHS(N), BHSNUM(N), IMG(N),
      dimension TAU(*), SOURCE(*), SIGMA(*), BHS(*), BHSNUM(*), IMG(*),
C
C               CNXP(N), OPAC(N), XJNU(N), Z(N)
     $          CNXP(*), OPAC(*), XJNU(*), Z(*)
C
      dimension IN(13)
      equivalence
     $(IN( 1),ITAUR ),(IN( 2),IXAR  ),(IN( 3),IYAR  ),(IN( 4),IXA   ),
     $(IN( 5),ICNXR ),(IN( 6),IZR   ),(IN( 7),IXJRO ),(IN( 8),IRR   ),
     $(IN( 9),IWH   ),(IN(10),IYA   ),(IN(11),IXJR  ),(IN(12),IXM   ),
     $(IN(13),IWN   )
C
      data MOVING /.false./
      data ILFLX /0/
C     !EJECT
C
      call HI ('TUAREG')
C     !BEG
C     (Get, and allocate, W allotment)
      call TIBBU  (IN, IS, MOX, 'TUAREG')
C
C---- Compute auxiliary functions XA and YA
      call ARRDIV (SIGMA , OPAC, W(IXA), N)
      call ARRDIV (BHSNUM, OPAC, W(IYA), N)
C
C---- Set up reduced tables
      call VESTA  (TAU, W(IXA), W(IYA), CNXP, Z, N, II, W(ITAUR),
     $             W(IXAR), W(IYAR), W(ICNXR), W(IZR), NR)
C---- Compute reduced table of Jnu
      call CRANE  (X, W, IW, DUMP, W(ITAUR), W(IXAR), W(IYAR), W(IXJR),
     $             NR, W(IXM), W(IWN), W(IWH), MOVING, ILFLX, IQINC,
     $             IMG, W(ICNXR), YDAMP, W(IRR), W(IXJRO), ITS, XLM,
     $             CSFCRIT, W(IZR), OPAC)
C
C---- Get full Jnu set
      call LUXURY (II, N, W(IXJR), XJNU)
C---- Check validity of Jnu
      call GELA   (XJNU, N, LAG)
C---- Get Source Function
      call DELLA  (N, SOURCE, BHS, W(IXA), W(IYA), XJNU, LAG)
C
C     (Give back W allotment)
      call WGIVE  (W, 'TUAREG')
C     !END
      call BYE ('TUAREG')
C
      return
      end
