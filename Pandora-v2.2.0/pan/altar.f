      subroutine ALTAR
     $(N,NL,KOLEV,EP1,EP2,CK,RS,BDI,XND,RHOIJ,YBRIJ,GMI,CIJ,SUMS,SUMU,
     $ ARHO,CINC,FLM,CQLI,CQL,X,IX,GVL,KDGV)
C
C     Rudolf Loeser, 1976 Sep 01
C---- Computes EP1 and EP2 by the "new" method.
C     (This is version 3 of ALTAR.)
C     !DASH
      save
C     !DASH
      real*8 ARHO, BDI, CIJ, CINC, CK, CQL, CQLI, EP1, EP2, FLM, GMI,
     $       GVL, RHOIJ, RS, SUMS, SUMU, X, XND, YBRIJ
      integer IX, KDGV, KOLEV, M, N, NL
      logical DUMP, KILROY
C     !DASH
      external BRAZIL, CHANT, QUERY, LIVID, VICTIM, ENTRAIL, MASHED,
     $         FLAME, HI, BYE
C
      dimension X(*), IX(*)
C
C               EP1(N), EP2(N), CK(N,NL), RS(N), ARHO(N,NL), XND(N,NL),
      dimension EP1(*), EP2(*), CK(*),    RS(*), ARHO(*),    XND(*),
C
C               RHOIJ(N,NT), FLM(N,NL), CIJ(N,NL,NL), SUMS(N), SUMU(N),
     $          RHOIJ(*),    FLM(*),    CIJ(*),       SUMS(*), SUMU(*),
C
C               CINC(N,NL), CQLI(N,NL), CQL(NL), GVL(N,NL), GMI(N,NSL),
     $          CINC(*),    CQLI(*),    CQL(*),  GVL(*),    GMI(*),
C
C               YBRIJ(N,NT), BDI(N,NL)
     $          YBRIJ(*),    BDI(*)
C
      call HI ('ALTAR')
C     !BEG
      M = KOLEV
C
      call BRAZIL   (DUMP, KILROY, M, 'Complex-L', KDGV, 'ALTAR')
      call CHANT    (N, NL, M, RHOIJ, YBRIJ, GMI, X, IX, ARHO, CINC,
     $               FLM)
      call QUERY    (N, NL, M, FLM, CIJ, CQLI, CQL)
      call LIVID    (N, NL, M, BDI, XND, CQL, CQLI, ARHO, CINC, FLM,
     $               CIJ, DUMP)
      call VICTIM   (N, NL, M, CQL, BDI, CIJ, FLM, ARHO, CINC, XND,
     $               SUMS, SUMU)
      call ENTRAIL  (N, M, CK, RS, SUMS, SUMU, EP1, EP2, GVL, KDGV)
      call FLAME    (N, M, CK, RS, SUMS, EP1, SUMU, EP2, KDGV, GVL,
     $               DUMP)
C
      if(.not.KILROY) then
        call MASHED ('ALTAR')
      end if
C     !END
      call BYE ('ALTAR')
C
      return
      end
