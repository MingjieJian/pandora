      subroutine OBELISK
     $(X,W,IW,KAMB,N,MN1,NL,DUMP,ITER,KINOUT,Z,XN1,XNK,G1,DIDG1,HEND,
     $ ALFA,BETA,HE2K,HE1,KBNDS,MSSPR,DEE,DELTA,RND,PLK,SPKL,PALBET,
     $ PBETAL,PBETGM,PGMBET,KDAMP,SMGMMA)
C
C     Rudolf Loeser, 1998 Jun 29
C---- Computes populations, all ions simultaneously, for CARAMBA.
C     !DASH
      save
C     !DASH
      real*8 ALFA, ALFB, BETA, BETB, CRIT, DEE, DELTA, EDIT, G1, HE1,
     $       HE2K, HEND, PALBET, PBETAL, PBETGM, PGMBET, PLK, RND, SPKL,
     $       W, X, XN1, XNK, Z
      integer IAA, IAB, IALBE, IANORM, IARAT, IBA, IBB, IBETA, IBNORM,
     $        IBRAT, ICA, ICB, ICHK, ICNORM, IDA, IDB, IDEL, IDNORM,
     $        IEA, IEB, IENORM, IFA, IFB, IFD, IFO, IGA, IGB, IGFIN,
     $        IGNED, IGNEW, IGNSM, IGOLD, IH, IIMG, IMNORM, IN, IP, IR,
     $        IRES, IS, ISS, ITER, IVEC, IW, IWS, IX, IXO, JAB, JBA,
     $        JBG, JGB, JN, KAMB, KBINN, KBNDS, KBOUT, KDAMP, KINOUT, M,
     $        MN1, MOX, MSSPR, MUX, N, NL, NUM
      logical DABN, DIDG1, DUMP, PRNTM, SMGMMA
C     !DASH
C     !EJECT
      external QUOIN, BEGGAR, KLISBOE, MILFORD, ARRDIV, ARRMUL, LUDLOW,
     $         SKILO, BOISE, BLOKE, SOBLE, BOSKE, ZERO1, LISKO, IMMAKE,
     $         WGIVE, IGIVE, MOVE1, BILK, BIKE, MALT, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               PLK(N,NL), PALBET(N), PBETAL(N), PBETGM(N), DELTA(7,N),
      dimension PLK(*),    PALBET(*), PBETAL(*), PBETGM(*), DELTA(*),
C
C               PGMBET(N), SPKL(N), HEND(N), HE2K(N), ALFA(N), BETA(N),
     $          PGMBET(*), SPKL(*), HEND(*), HE2K(*), ALFA(*), BETA(*),
C
C               DEE(4,5,N), XN1(N), XNK(N), HE1(N), G1(N), RND(N,NL),
     $          DEE(*),     XN1(*), XNK(*), HE1(*), G1(*), RND(*),
C
C               Z(N)
     $          Z(*)
C
      dimension IN(41)
      equivalence
     $(IN( 1),IX    ),(IN( 2),IXO   ),(IN( 3),IDEL  ),(IN( 4),IFA   ),
     $(IN( 5),IFB   ),(IN( 6),IGA   ),(IN( 7),IGB   ),(IN( 8),IAA   ),
     $(IN( 9),IBA   ),(IN(10),ICA   ),(IN(11),IDA   ),(IN(12),IEA   ),
     $(IN(13),IAB   ),(IN(14),IBB   ),(IN(15),ICB   ),(IN(16),IDB   ),
     $(IN(17),IEB   ),(IN(18),ICHK  ),(IN(19),IRES  ),(IN(20),ISS   ),
     $(IN(21),IBETA ),(IN(22),IH    ),(IN(23),IP    ),(IN(24),IR    ),
     $(IN(25),IFD   ),(IN(26),IALBE ),(IN(27),IGOLD ),(IN(28),IGFIN ),
     $(IN(29),IGNSM ),(IN(30),IGNEW ),(IN(31),IGNED ),(IN(32),IVEC  ),
     $(IN(33),IARAT ),(IN(34),IBRAT ),(IN(35),IANORM),(IN(36),IBNORM),
     $(IN(37),ICNORM),(IN(38),IDNORM),(IN(39),IMNORM),(IN(40),IENORM),
     $(IN(41),IFO   )
C
      dimension JN( 1)
      equivalence
     $(JN( 1),IIMG  )
C
      data CRIT, EDIT /1.D-7, 1.D-1/
C     !EJECT
C
      call HI ('OBELISK')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call QUOIN     (IN, IS,  MOX, 'OBELISK')
      call IMMAKE    (JN, IWS, MUX, 'OBELISK')
C
      if(DUMP) then
        PRNTM = MSSPR.eq.1
C----   Print marker
        call KLISBOE (1)
      end if
C---- Get rates
      call BOISE     (KAMB, N, NL, RND, PLK, SPKL, PALBET, JAB, PBETAL,
     $                JBA, PBETGM, JBG, PGMBET, JGB)
C---- Set up boundary conditions
      call LISKO     (MN1, KINOUT, KBNDS, PALBET, PBETAL, PBETGM,
     $                PGMBET, KBINN, KBOUT, ALFB, BETB)
C---- Compute DEL = d
      call BEGGAR    (MN1, Z, W(IDEL), CRIT, 'OBELISK')
C---- Get f, g, A - E, for both ions
      call ZERO1     (W(IR), N)
      call MILFORD   (N, HEND, BETA, HE2K, HE1, DEE, DELTA, W(IFA),
     $                W(IGA))
      call ARRDIV    (W(IFA), W(IDEL), W(IFD), N)
      call MALT      (MN1, W(IDEL), W(IFD), W(IGA), W(IR), KBINN,
     $                KBOUT, W(IAA), W(IBA), W(ICA), W(IDA), W(IEA))
      call LUDLOW    (N, HEND, HE2K, HE1, DEE, DELTA, W(IFB), W(IGB))
      call ARRDIV    (W(IFB), W(IDEL), W(IFD), N)
      call MALT      (MN1, W(IDEL), W(IFD), W(IGB), W(IR), KBINN,
     $                KBOUT, W(IAB), W(IBB), W(ICB), W(IDB), W(IEB))
C---- Get h and p
      call SKILO     (N, HEND, PALBET, PBETAL, PBETGM, PGMBET, W(IH))
      call ARRMUL    (HEND, PGMBET, W(IP), N)
C---- Set up matrix.....
      NUM = MN1-KBINN-KBOUT
      M   = 2*NUM
      call BLOKE     (MN1, NUM, M, W(IAA), W(IBA), W(ICA), W(IDA),
     $                W(IEA), W(IAB), W(IBB), W(ICB), W(IDB), W(IEB),
     $                W(IH), W(IP), KBINN, KBOUT, ALFB, BETB, W(IX),
     $                W(ISS))
      call MOVE1     (W(IX), (M**2), W(IXO))
C---- ......and solve (with Check)
      call BILK      (W(IX), W(ISS), W(IRES), W(ICHK), M, W(IXO),
     $                W, IW)
C---- Get N1-new, NK-new, and G1
      call BIKE      (KAMB, MN1, HEND, W(IRES), KBINN, KBOUT, ALFB,
     $                BETB, ALFA, W(IBETA), EDIT, XN1, XNK, W(IALBE),
     $                W(IGOLD), W(IGNEW), W(IGFIN), W(IGNSM), W(IGNED),
     $                Z, DUMP, KDAMP, SMGMMA, W(IVEC), W(IARAT),
     $                W(IBRAT), W(IANORM), W(IBNORM), W(ICNORM),
     $                W(IDNORM), W(IENORM), IW(IIMG), W(IFO),
     $                W(IMNORM), DABN, W, IW)
      call SOBLE     (KAMB, N, G1, DIDG1, XN1, XNK, W(IARAT), W(IBRAT),
     $                W(IVEC), PALBET, PBETAL, PBETGM, PGMBET)
C     !EJECT
      if(DUMP) then
C----   Print
        call BOSKE   (MN1, M, ITER, KAMB, Z, W(IDEL), W(IFA), W(IGA),
     $                W(IAA), W(IBA), W(ICA), W(IDA), W(IEA), W(IFB),
     $                W(IGB), W(IAB), W(IBB), W(ICB), W(IDB), W(IEB),
     $                PALBET, JAB, PBETAL, JBA, PBETGM, JBG, PGMBET,
     $                JGB, W(IH), W(ICHK), W(IP), ALFA, W(IBETA), HEND,
     $                KBINN, KBOUT, W(ISS), W(IRES), W(IXO), PRNTM, G1,
     $                W(IANORM), W(IBNORM), W(ICNORM), W(IDNORM),
     $                W(IENORM), W(IMNORM), DABN, XNK, XN1)
C----   Print marker
        call KLISBOE (2)
      end if
C
C     (Give back W & IW allotments)
      call WGIVE   (W , 'OBELISK')
      call IGIVE   (IW, 'OBELISK')
C     !END
      call BYE ('OBELISK')
C
      return
      end
