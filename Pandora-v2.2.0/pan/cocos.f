      subroutine COCOS
     $(N,NL,KOLEV,RHOIJ,YBRIJ,XND,GMI,RLI,CKI,RS,EP1,EP2,ARHO,CL1TA,
     $ C1LTA,CIJ,PKL,PL1,PLK,P1L,RKI,CQI,BDI,MQT,CQTAIL,TAUK,XLMT,
     $ CQSI,NSL,SPKL,EPCBR,PS1,AL,GVL,KDGV,X,IX)
C
C     Rudolf Loeser, 1976 Nov 26
C---- Computes Lyman epsilons by the "CHAIN-like" method.
C
C     Works only for KOLEV=1.
C
C     !DASH
      save
C     !DASH
      real*8 AL, ARHO, BDI, C1LTA, CIJ, CKI, CL1TA, CQI, CQSI, CQTAIL,
     $       EP1, EP2, EPCBR, GMI, GVL, P1L, PKL, PL1, PLK, PS1, RHOIJ,
     $       RKI, RLI, RS, SPKL, TAUK, X, XLMT, XND, YBRIJ
      integer IX, KDGV, KOLEV, M, MQT, N, NL, NSL
      logical DUMP, KILROY
C     !DASH
      external DUNCAN, BRAZIL, ENDIVE, PECAN, ALMOND, PIE, MASHED,
     $         HI, BYE
C
      dimension X(*), IX(*)
C
C               RHOIJ(N,NT), XND(N,NL), GMI(N,NSL), RLI(N,NSL), PKL(N),
      dimension RHOIJ(*),    XND(*),    GMI(*),     RLI(*),     PKL(*),
C
C               CKI(N,NSL), RS(N), EP1(N), EP2(N), ARHO(N,NL), TAUK(N),
     $          CKI(*),     RS(*), EP1(*), EP2(*), ARHO(*),    TAUK(*),
C
C               CL1TA(N,NL), C1LTA(N,NL), CIJ(N,NL,NL), PL1(N), PLK(N),
     $          CL1TA(*),    C1LTA(*),    CIJ(*),       PL1(*), PLK(*),
C
C               P1L(N), GVL(N,NL), CQI(N), CQTAIL(MQT), PS1(N), AL(NL),
     $          P1L(*), GVL(*),     CQI(*), CQTAIL(*),   PS1(*), AL(*),
C
C               BDI(N,NL), CQSI(N,NSL), YBRIJ(N,NT), RKI(N,NSL),
     $          BDI(*),    CQSI(*),     YBRIJ(*),    RKI(*),
C
C               SPKL(N)
     $          SPKL(*)
C     !EJECT
C
      call HI ('COCOS')
C     !BEG
      call DUNCAN   (KOLEV, 'Chain', M)
C
C---- Set up optional dump
      call BRAZIL   (DUMP, KILROY, M, 'Chain', KDGV, 'COCOS')
C---- Compute first-level intermediates, for each level
      call ENDIVE   (N, NL, M, RHOIJ, YBRIJ, X, IX, ARHO, CL1TA, C1LTA)
C---- Compute and print second-level intermediates (levels sums)
      call PECAN    (N, NL, M, PKL, PL1, PLK, P1L, GMI, CKI, XND, ARHO,
     $               CIJ,CL1TA, C1LTA, RKI, CQSI, GVL, KDGV, DUMP)
C---- Provide for supplementary levels
      call ALMOND   (N, NL, NSL, M, RLI, RKI, GMI, SPKL, EPCBR, XND,
     $               ARHO, PS1, DUMP)
C---- Compute and print epsilons
      call PIE      (N, NL, M, EP1, EP2, RS, CKI, PKL, PL1, PLK, P1L,
     $               CQI, BDI, MQT, CQTAIL, TAUK, XLMT, EPCBR, SPKL,
     $               PS1, AL, KDGV, GVL, DUMP)
C
      if(.not.KILROY) then
        call MASHED ('COCOS')
      end if
C     !END
      call BYE ('COCOS')
C
      return
      end
