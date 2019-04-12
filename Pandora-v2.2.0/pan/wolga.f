      subroutine WOLGA
     $(X,IX,W,IW,N,NL,NSL,BDIJ,RHOIJ,YBRIJ,CIJ,GMI,RKI,RLI,CKI,BDX,
     $ CQUI,CQSI,SQS,KDGV,GVL,ZION,METH,BD0,BD1,BDN,BDD,BDI)
C
C     Rudolf Loeser, 1988 Jan 05
C---- Computes a set of b values (departure coefficients).
C     (This is version 4 of WOLGA.)
C     !DASH
      save
C     !DASH
      real*8 BD0, BD1, BDD, BDI, BDIJ, BDN, BDX, CIJ, CKI, CQSI, CQUI,
     $       GMI, GVL, RHOIJ, RKI, RLI, SQS, W, X, YBRIJ, ZION
      integer IBR, IDEN, IN, INUM, IS, ISTM, IW, IWT, IX, KAMB, KDGV,
     $        KVLG, METH, MOX, N, NL, NNL, NSL
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(32),KAMB )
      equivalence (LEST(47),KVLG )
C     !DASH
      external KURIL, BOWMAN, ROTINGA, RHEIN, MULIAK, MOVE1, WGIVE,
     $         HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               CKI(N,NSL), BDI(N,NL), RKI(N,NSL), RLI(N,NSL), ZION(N),
      dimension CKI(*),     BDI(*),    RKI(*),     RLI(*),     ZION(*),
C
C               CQUI(N,NSL), BDIJ(N,NL), GVL(N,NL), GMI(N,NSL), SQS(N),
     $          CQUI(*),     BDIJ(*),    GVL(*),    GMI(*),     SQS(*),
C
C               BDX(N,NSL), BD0(N,NL), BD1(N,NL), BDD(N,NL), BDN(N,NL),
     $          BDX(*),     BD0(*),    BD1(*),    BDD(*),    BDN(*),
C
C               YBRIJ(N,NT), CIJ(N,NL,NL), RHOIJ(N,NT), CQSI(N,NSL)
     $          YBRIJ(*),    CIJ(*),       RHOIJ(*),    CQSI(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IWT   ),(IN( 2),ISTM  ),(IN( 3),IBR   ),(IN( 4),INUM  ),
     $(IN( 5),IDEN  )
C     !EJECT
C
      call HI ('WOLGA')
C     !BEG
C     (Get, and allocate, W allotment)
      call KURIL    (IN, IS, MOX, 'WOLGA')
C
      NNL = N*NL
C---- Use all level equations
      call RHEIN    (X, IX, W, IW, N, NL, BD1, RHOIJ, YBRIJ, CIJ, GMI,
     $               CQUI, CQSI, GVL)
C---- Use the continuum equation
      call ROTINGA  (N, NL, NSL, BDIJ, GMI, RKI, RLI, CKI, CQUI, BDX,
     $               GVL, KDGV, BD0, SQS, W(ISTM), W(IBR), W(INUM),
     $               W(IDEN))
C---- Choose set to be used
      if(METH.gt.0) then
        call MOVE1  (BD1, NNL, BDN)
      else
        call MOVE1  (BD0, NNL, BDN)
      end if
C
      if((KAMB.gt.0).or.(KVLG.gt.0)) then
C----   Also calculate directly from b1 and b-ratios
        call BOWMAN (X, W, N, NL, BDD, BDIJ)
C       Get weighted average, with dump printout (if needed)
        call MULIAK (BDN, BDD, BDI, N, NL, ZION, W(IWT))
      else
        call MOVE1  (BDN, NNL, BDI)
      end if
C
C     (Give back W allotment)
      call WGIVE    (W, 'WOLGA')
C     !END
      call BYE ('WOLGA')
C
      return
      end
