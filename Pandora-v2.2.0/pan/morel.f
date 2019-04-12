      subroutine MOREL
     $(X,IX,W,IW,N,NL,NSL,BDIJ,RHOIJ,YBRIJ,CIJ,GMI,RKI,RLI,CKI,BDI,
     $ BD0,BD1,BDN,BDD,METH,BDX,BDO,ITS,GVL,KDGV,CQUI,CQSI,SQS,ZION)
C
C     Rudolf Loeser, 1978 May 18
C---- Computes a BD-set for SPIKE, by iterating.
C     (This is version 2 of MOREL.)
C     !DASH
      save
C     !DASH
      real*8 BD0, BD1, BDD, BDI, BDIJ, BDN, BDO, BDX, CIJ, CKI, CONV,
     $       CQSI, CQUI, DIFMX, GMI, GVL, RHOIJ, RKI, RLI, SQS, W, X,
     $       YBRIJ, ZION
      integer IMX, IT, ITS, IW, IX, JMX, KDGV, LIMIT, METH, N, NL, NNL,
     $        NSL
      logical SAME
C     !DASH
      external ZERO1, PARASO, MOVE1, WOLGA, BRICK, RUNE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               CQUI(N,NSL), GVL(N,NL), RKI(N,NSL), RLI(N,NSL), SQS(N),
      dimension CQUI(*),     GVL(*),    RKI(*),     RLI(*),     SQS(*),
C
C               BDO(N,NL), BDI(N,NL), GMI(N,NSL), RHOIJ(N,NT), ZION(N),
     $          BDO(*),    BDI(*),    GMI(*),     RHOIJ(*),    ZION(*),
C
C               CKI(N,NSL), BD0(N,NL), BD1(N,NL), BDN(N,NL), BDD(N,NL),
     $          CKI(*),     BD0(*),    BD1(*),    BDN(*),    BDD(*),
C
C               CQSI(N,NSL), BDIJ(N,NL), CIJ(N,NL,NL), YBRIJ(N,NT),
     $          CQSI(*),     BDIJ(*),    CIJ(*),       YBRIJ(*),
C
C               BDX(N,NSL)
     $          BDX(*)
C
      data LIMIT,CONV /50, 1.D-4/
C     !EJECT
C
      call HI ('MOREL')
C     !BEG
      NNL = N*NL
C---- Initialize BDI
      call ZERO1    (BDI, NNL)
C
C---- Begin iterative loop
      do 100 IT = 1,LIMIT
C
        ITS = IT
C----   Get extended BDI set
        call PARASO (IT, N, NL, NSL, BDI, BDX)
C----   Compute new BDI set
        call MOVE1  (BDI, NNL, BDO)
        call WOLGA  (X, IX, W, IW, N, NL, NSL, BDIJ, RHOIJ, YBRIJ, CIJ,
     $               GMI, RKI, RLI, CKI, BDX, CQUI, CQSI, SQS, KDGV,
     $               GVL, ZION, METH, BD0, BD1, BDN, BDD, BDI)
C----   Check for convergence
        call BRICK  (N, NL, BDI, BDO, CONV, DIFMX, IMX, JMX, SAME)
        if(SAME) then
          goto 101
        end if
C
  100 continue
C---- Tell about lack of convergence
      call RUNE     (DIFMX, JMX, IMX)
C
  101 continue
C     !END
      call BYE ('MOREL')
C
      return
      end
