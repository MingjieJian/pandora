      subroutine IBORA
     $(X,W,IW,XLM,DUMP,TAU,BHS,SCAT,CNDT,CNXP,Z,OPAC,N,YDAMP,IQINC,
     $ CSFCRIT,XJNU,SOURCE,WN,WH,MOVING,ILFLX,ITS,LAG,IMG)
C
C     Rudolf Loeser, 1983 Feb 01
C---- Computes Continuum Jnu from S, and S, (and weight matrices).
C     (Special version of "PANKU", for P.R.D. Jnu calculation.)
C     (This is version 2 of IBORA.)
C     !DASH
      save
C     !DASH
      real*8 BHS, CNDT, CNXP, CSFCRIT, OPAC, SCAT, SOURCE, TAU, W, WH,
     $       WN, X, XJNU, XLM, YDAMP, Z
      integer IBB, ICQ, ILFLX, IMG, IN, IQINC, IS, ISFR, ITS, IW, IXM,
     $        LAG, MOX, N
      logical DUMP, MOVING
C     !DASH
      external LYNE, SASKIA, GELA, DALETH, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XJNU(N), CNXP(N), SCAT(N), CNDT(N), WH(N,N), SOURCE(N),
      dimension XJNU(*), CNXP(*), SCAT(*), CNDT(*), WH(*),   SOURCE(*),
C
C               TAU(N), BHS(N), IMG(N), WN(N,N), Z(N), OPAC(N)
     $          TAU(*), BHS(*), IMG(*), WN(*),   Z(*), OPAC(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),ICQ   ),(IN( 2),IBB   ),(IN( 3),ISFR  ),(IN( 4),IXM   )
C
      call HI ('IBORA')
C     !BEG
C     (Get, and allocate, W allotment)
      calL LYNE   (IN, IS, MOX, 'IBORA')
C
C---- Compute Jnu
      call SASKIA (X, W, IW, DUMP, TAU, SCAT, BHS, W(ISFR), XJNU, N,
     $             W(IXM), WN, WH, MOVING, ILFLX, CNDT, IQINC, IMG,
     $             CNXP, YDAMP, W(IBB), W(ICQ), ITS, XLM, CSFCRIT, Z,
     $             OPAC)
C---- Check validity of Jnu
      call GELA   (XJNU, N, LAG)
C---- Get Source Function
      call DALETH (N, SOURCE, BHS, SCAT, XJNU, LAG)
C
C     (Give back W allotment)
      call WGIVE  (W, 'IBORA')
C     !END
      call BYE ('IBORA')
C
      return
      end
