      subroutine PANKU
     $(X,W,IW,XLM,DUMP,TAU,BHS,SCAT,CNDT,CNXP,Z,OPAC,II,NR,N,YDAMP,
     $ IQINC,CSFCRIT,XJNU,SOURCE,ITS,LAG,IMG)
C
C     Rudolf Loeser, 1981 Jul 21
C---- Computes Jnu from S, and S, for REED.
C     (This is version 2 of PANKU.)
C     !DASH
      save
C     !DASH
      real*8 BHS, CNDT, CNXP, CSFCRIT, OPAC, SCAT, SOURCE, TAU, W, X,
     $       XJNU, XLM, YDAMP, Z
      integer IBB, IBHSR, ICNDR, ICNXR, ICQ, II, ILFLX, IMG, IN, IQINC,
     $        IS, ISCTR, ISFR, ITAUR, ITS, IW, IWH, IWN, IXJR, IXM, IZR,
     $        LAG, MOX, N, NR
      logical DUMP, MOVING
C     !DASH
      external JOEPYE, BETH, SASKIA, LUXURY, GELA, DALETH, WGIVE,
     $         HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               CNXP(N), XJNU(N), SCAT(N), CNDT(N), SOURCE(N), OPAC(N),
      dimension CNXP(*), XJNU(*), SCAT(*), CNDT(*), SOURCE(*), OPAC(*),
C
C               Z(N), IMG(N), TAU(N), BHS(N)
     $          Z(*), IMG(*), TAU(*), BHS(*)
C
      dimension IN(13)
      equivalence
     $(IN( 1),ITAUR ),(IN( 2),IBHSR ),(IN( 3),ISCTR ),(IN( 4),ICNDR ),
     $(IN( 5),ICNXR ),(IN( 6),IZR   ),(IN( 7),ICQ   ),(IN( 8),IBB   ),
     $(IN( 9),IWH   ),(IN(10),ISFR  ),(IN(11),IXJR  ),(IN(12),IXM   ),
     $(IN(13),IWN   )
C
      data MOVING /.false./
      data ILFLX  /0/
C     !EJECT
C
      call HI ('PANKU')
C     !BEG
C     (Get, and allocate, W allotment)
      call JOEPYE (IN, IS, MOX, 'PANKU')
C
C---- Set up reduced tables
      call BETH   (XLM, TAU, BHS, SCAT, CNDT, CNXP, Z, N, II, W(ITAUR),
     $             W(IBHSR), W(ISCTR), W(ICNDR), W(ICNXR), W(IZR), NR)
C---- Compute reduced table of Jnu
      call SASKIA (X, W, IW, DUMP, W(ITAUR), W(ISCTR), W(IBHSR),
     $             W(ISFR), W(IXJR), NR, W(IXM), W(IWN), W(IWH),
     $             MOVING, ILFLX, W(ICNDR), IQINC, IMG, W(ICNXR),
     $             YDAMP, W(IBB), W(ICQ), ITS, XLM, CSFCRIT, W(IZR),
     $             OPAC)
C---- Get full Jnu set
      call LUXURY (II, N, W(IXJR), XJNU)
C---- Check validity of Jnu
      call GELA   (XJNU, N, LAG)
C---- Get Source Function
      call DALETH (N, SOURCE, BHS, SCAT, XJNU, LAG)
C
C     (Give back W allotment)
      call WGIVE  (W, 'PANKU')
C     !END
      call BYE ('PANKU')
C
      return
      end
