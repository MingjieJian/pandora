      subroutine SPIKE
     $(X,IX,W,IW,N,NL,NSL,BDIJ,RHOIJ,YBRIJ,CIJ,GMI,RKI,RLI,CKI,BDI,
     $ METH,BD0,BD1,BDN,BDD,BDX,BDO,JBFSW,GVL,KDGV,CQUI,CQSI,SQS,ZION)
C
C     Rudolf Loeser, 1978 May 18
C---- Computes BD-sets, taking Supplementary Levels into account.
C     !DASH
      save
C     !DASH
      real*8 BD0, BD1, BDD, BDI, BDIJ, BDN, BDO, BDX, CIJ, CKI, CQSI,
     $       CQUI, GMI, GVL, RHOIJ, RKI, RLI, SQS, W, X, YBRIJ, ZION
      integer IQPBD, ITS, IW, IX, JBFSW, KDGV, LU, METH, N, NL, NO, NSL
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(130),IQPBD)
C     !DASH
C     !EJECT
      external ZEUS, TUFT, MOREL, HALT, INKY, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               BDIJ(N,NL), BD0(N,NL), RLI(N,NSL), CKI(N,NSL), ZION(N),
      dimension BDIJ(*),    BD0(*),    RLI(*),     CKI(*),     ZION(*),
C
C               CIJ(N,NL**2), BDI(N,NL), GMI(N,NSL), SQS(N), BD1(N,NL),
     $          CIJ(*),       BDI(*),    GMI(*),     SQS(*), BD1(*),
C
C               BDO(N,NL), GVL(N,NL), BDX(N,NSL), BDN(N,NL), BDD(N,NL),
     $          BDO(*),    GVL(*),    BDX(*),     BDN(*),    BDD(*),
C
C               CQUI(N,NSL), CQSI(N,NSL), YBRIJ(N,NT), RHOIJ(N,NT),
     $          CQUI(*),     CQSI(*),     YBRIJ(*),    RHOIJ(*),
C
C               RKI(N,NSL)
     $          RKI(*)
C
      call HI ('SPIKE')
C     !BEG
      if(JBFSW.eq.1) then
C----   Don't iterate
        call TUFT  (X, IX, W, IW, N, NL, NSL, BDIJ, RHOIJ, YBRIJ, CIJ,
     $              GMI, RKI, RLI, CKI, BDI, BD0, BD1, BDN, BDD, METH,
     $              BDX, ITS, GVL, KDGV, CQUI, CQSI, SQS, ZION)
      else if(JBFSW.eq.2) then
C----   Iterate
        call MOREL (X, IX, W, IW, N, NL, NSL, BDIJ, RHOIJ, YBRIJ, CIJ,
     $              GMI, RKI, RLI, CKI, BDI, BD0, BD1, BDN, BDD, METH,
     $              BDX, BDO, ITS, GVL, KDGV, CQUI, CQSI, SQS, ZION)
      else
        write (MSSLIN(1),100) JBFSW
  100   format('JBFSW =',I12,', which is neither 1 nor 2.')
        call HALT  ('SPIKE', 1)
      end if
C
      call ZEUS    (NO, IQPBD, LU)
      call INKY    (LU, N, NL, NSL, BDI, BDX, BDIJ, ITS, JBFSW)
C     !END
      call BYE ('SPIKE')
C
      return
      end
