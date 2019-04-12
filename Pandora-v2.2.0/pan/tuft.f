      subroutine TUFT
     $(X,IX,W,IW,N,NL,NSL,BDIJ,RHOIJ,YBRIJ,CIJ,GMI,RKI,RLI,CKI,BDI,
     $ BD0,BD1,BDN,BDD,METH,BDX,ITS,GVL,KDGV,CQUI,CQSI,SQS,ZION)
C
C     Rudolf Loeser, 1978 May 18
C---- Computes a BD-set for SPIKE, without iterating.
C     !DASH
      save
C     !DASH
      real*8 BD0, BD1, BDD, BDI, BDIJ, BDN, BDX, CIJ, CKI, CQSI, CQUI,
     $       GMI, GVL, RHOIJ, RKI, RLI, SQS, W, X, YBRIJ, ZION
      integer ITS, IW, IX, J, KDGV, METH, N, NL, NSL
C     !DASH
      external ONE1, WOLGA, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               BD0(N,NL), BDIJ(N,NL), GMI(N,NSL), RKI(N,NSL), ZION(N),
      dimension BD0(N,*),  BDIJ(*),    GMI(*),     RKI(*),     ZION(*),
C
C               CQUI(N,NSL), BDI(N,NL), GVL(N,NL), YBRIJ(N,NT), SQS(N),
     $          CQUI(*),     BDI(*),    GVL(*),    YBRIJ(*),    SQS(*),
C
C               CIJ(N,NL**2), RLI(N,NSL), RHOIJ(N,NT), CQSI(N,NSL),
     $          CIJ(*),       RLI(*),     RHOIJ(*),    CQSI(*),
C
C               CKI(N,NSL), BDX(N,NSL), BD1(N,NL), BDN(N,NL), BDD(N,NL)
     $          CKI(*),     BDX(N,*),   BD1(*),    BDN(*),    BDD(*)
C
      call HI ('TUFT')
C     !BEG
      do 100 J = (NL+1),NSL
        call ONE1 (BDX(1,J), N)
  100 continue
C
      call WOLGA (X, IX, W, IW, N, NL, NSL, BDIJ, RHOIJ, YBRIJ, CIJ,
     $            GMI, RKI, RLI, CKI, BDX, CQUI, CQSI, SQS, KDGV, GVL,
     $            ZION, METH, BD0, BD1, BDN, BDD, BDI)
C
      ITS = 0
C     !END
      call BYE ('TUFT')
C
      return
      end
