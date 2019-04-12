      subroutine TULIP
     $(X,IX,W,IW,LUH,LUD,LUF,LUS,LUQ,LUC,YBAR,ALF,BDIJ,RHOIJ,BATA,
     $ RHOS,RHOJ,RBDS,RBDJ,RHOW,RHOO,CEK,CIJ,WEIGHT,ORHO,RBDR,AIJ,S,
     $ TAUL,RHWL,NEDL,DROL,IFSL,ILSL,WSML,KIJ,TRA,T,PTAU,WW,WT,SSTAR,
     $ RHOX,RBDU,RBDQ,CHI,ASTAR,AW,SA,EP,BS,BA,BB,BC,XVAL,WEIT,IMG,
     $ CAU1,ABDR,ARHO)
C
C     Rudolf Loeser, 1968 Jul 03
C---- Computes and prints new values of RHOIJ and BDIJ; also
C     consistency CHECKS and RHO Weights.
C     (This is version 2 of TULIP.)
C     !DASH
      save
C     !DASH
      real*8 ABDR, AIJ, ALF, ARHO, ASTAR, AW, BA, BATA, BB, BC, BDIJ,
     $       BS, CAU1, CEK, CHI, CHLIM, CHOP, CIJ, CRIT, CWJ, CWR, CWU,
     $       DROL, EP, FUDGE, ORHO, PTAU, RBDJ, RBDQ, RBDR, RBDS, RBDU,
     $       RHOIJ, RHOJ, RHOO, RHOS, RHOW, RHOX, RHWL, S, SA, SMP,
     $       SSTAR, T, TAUL, TRA, W, WEIGHT, WEIT, WMN, WMX, WSML, WT,
     $       WW, X, XINCH, XVAL, YBAR, ZERO
      integer IFSL, IFUDGE, ILI, ILSL, IMG, ITER, ITMX, ITUS, IW, IX,
     $        KIJ, KNW, KODE, KTRAS, LUC, LUD, LUF, LUH, LUQ, LUS, N,
     $        NEDL, NIL, NL, NLM, NNL, NNT, NT
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 10),CWR  )
      equivalence (RZQ( 11),CHOP )
      equivalence (RZQ( 96),CHLIM)
      equivalence (KZQ( 13),ILI  )
      equivalence (KZQ( 14),NIL  )
      equivalence (RZQ( 54),XINCH)
      equivalence (RZQ(149),CWJ  )
      equivalence (RZQ( 55),WMX  )
      equivalence (RZQ( 43),WMN  )
      equivalence (RZQ( 56),SMP  )
C     !EJECT
C---- TULLY       as of 2004 Mar 17
      integer     MBD,MRHO
      logical     KRHOJ,KRHOW,KRBDS,KRBDJ,KRBDR,KRBDQ
C
      common      /TULLY1/ MBD,MRHO
      common      /TULLY2/ KRHOJ,KRHOW,KRBDS,KRBDJ,KRBDR,KRBDQ
C     Intermediates for TULIP: RHO & RBD calculation.
C     .
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(38),KTRAS)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external CROCUS, CLOVER, CHEQUE, TANSIE, JONQUIL, RESEDA, BANQUE,
     $         ANEMONE, CREMA, LOGRES, PETUNIA, CRYSTAL, MILAN, ZERO1,
     $         ASTER, PAVIA, VETCH, LYCRA, RASTY, ONE1, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               SSTAR(N,NT), ALF(NT), BDIJ(N,NL), DROL(NT), RHOW(N,NT),
      dimension SSTAR(*),    ALF(*),  BDIJ(*),    DROL(*),  RHOW(*),
C
C               RHOIJ(N,NT), BATA(N,NT), RHOS(N,NT), RBDS(N,NT), WW(N),
     $          RHOIJ(*),    BATA(*),    RHOS(*),    RBDS(*),    WW(*),
C
C               RBDJ(N,NL), RBDR(N,NL), RHOO(N,NT), IFSL(NT), NEDL(NT),
     $          RBDJ(*),    RBDR(*),    RHOO(*),    IFSL(*),  NEDL(*),
C
C               CIJ(N,NL**2), WEIGHT(N,NT), YBAR(N,NT), T(N), ILSL(NT),
     $          CIJ(*),       WEIGHT(*),    YBAR(*),    T(*), ILSL(*),
C
C               WSML(NT), CEK(N,NL-2,2), BA(N), TRA(N), XVAL(N), BB(N),
     $          WSML(*),  CEK(*),        BA(*), TRA(*), XVAL(*), BB(*),
C
C               RHOJ(N,NT), BC(N), RHOX(N,NT), WT(2*NIL+1), AIJ(NL,NL),
     $          RHOJ(*),    BC(*), RHOX(*),    WT(*),       AIJ(*),
C
C               TAUL(N,NT), RHWL(N,NT), KIJ(NL,NL), ORHO(N,NT), IMG(N),
     $          TAUL(*),    RHWL(*),    KIJ(*),     ORHO(*),    IMG(*),
C
C               AW(N,NT), CAU1(N,NL), RBDQ(N,NL), ASTAR(N,NT), PTAU(N),
     $          AW(*),    CAU1(*),    RBDQ(*),    ASTAR(*),    PTAU(*),
C
C               CHI(N,NT), WEIT(N), RBDU(N,NL), ABDR(N,3), ARHO(N,3),
     $          CHI(*),    WEIT(*), RBDU(*),    ABDR(*),   ARHO(*),
C
C               SA(N,NT), S(N,NT), EP(N,NT), BS(N,NT)
     $          SA(*),    S(*),    EP(*),    BS(*)
C
      data CRIT,ITMX /1.D-4, 20/
      data KODE,ITER,IFUDGE /1, 0, 1/
C
      call HI ('TULIP')
C     !BEG
      NLM = NL-2
      NNL = N*NL
      NNT = N*NT
C
C---- Initialize
      call ZERO1 (RHOJ, NNT)
      call ZERO1 (RHOW, NNT)
      call ONE1  (RBDS, NNT)
      call ONE1  (RBDQ, NNL)
      call ONE1  (RBDJ, NNL)
      call ONE1  (RBDR, NNL)
C     !EJECT
C---- Get CHI (a Rho-type quantity), and SA and A*.
      call LYCRA     (N, NT, YBAR, EP, BS, AW, CHI)
      call RASTY     (X, AW, SA, ASTAR)
C
      if(KRBDS) then
C----   Get B-ratios from PERSEUS (i.e. from S).
        call PETUNIA (N, NT, ALF, BATA, RBDS, S)
      end if
      if(KRBDQ) then
C----   Get B-ratios from CHI.
        call CRYSTAL (X, IX, W, IW, N, NL, CHI, ASTAR, SA, RBDU,
     $                RBDQ, IMG)
      end if
      if(KRBDJ) then
C----   Compute B-ratios from Jbar.
        call TANSIE  (X, IX, W, IW, YBAR, RBDJ, IMG)
      end if
      if(KRHOJ) then
C----   Compute Rho's from Jbar iteratively, and S* (update MRHO ? ).
        call MILAN   (X, IX, W, IW, N, NL, NT, MRHO, CRIT, ITMX, ALF,
     $                BATA, RBDR, S, SSTAR, RHOJ, YBAR, RHOS, CWJ,
     $                RHOX, ITUS, CWU, KNW, IMG)
      end if
      if(KRHOW) then
C----   Get combination Rho's.
        call RESEDA  (N, NT, CWR, CHOP, CHLIM, ILI, NIL, TAUL, TRA,
     $                KTRAS, RHOJ, RHOS, RHOW, T, PTAU, WW, WT, WEIT)
      end if
C
C---- Compute weights, and select and weight Rho-s to be used.
      call ANEMONE   (N, NL, NT, MRHO, XINCH, WMX, WMN, SMP, AIJ,
     $                CIJ, WEIGHT, RHWL, ORHO, RHOO, RHOS, RHOJ, RHOW,
     $                WEIT, RHOIJ, IFSL, ILSL, WSML, NEDL, DROL, XVAL,
     $                CAU1, W, IW)
C
      if(KRBDR) then
C----   Now compute B-ratios from final Rho.
        call VETCH   (X, IX, W, IW, RHOIJ, RHOO, YBAR, RBDR, IMG,
     $                IFUDGE, KODE, FUDGE, ITER)
      end if
C---- Select B-ratios to be used.
      call CLOVER    (MBD, RBDJ, RBDR, RBDQ, BDIJ)
C
C---- Compute final consistency CHECKS.
      call CHEQUE    (BDIJ, RBDS, N, NL, NLM, KIJ, CEK, BA, BB, BC)
C     !EJECT
C---- Print everything.
      call ASTER   (LUD, FUDGE, KTRAS, CRIT, ITMX, ITUS, CWU, KNW)
      call CROCUS  (LUD, LUQ, N, NT, YBAR, RBDS, RBDJ, RBDQ, BDIJ,
     $              RHOIJ, RHOJ, RHOS, RHOW, WEIGHT, RBDR, S, RBDU,
     $              CHI, ASTAR, AW, SA, TRA, KTRAS, ABDR, ARHO)
      call JONQUIL (RBDJ, RBDR, RBDQ, RBDS, BDIJ, SSTAR, N, NL, NT,
     $              KIJ, LUF, W)
      call PAVIA   (LUS, N, NL, RHOIJ, BDIJ, KIJ)
      call BANQUE  (CEK, N, NLM, LUC)
C
C---- Save CHECKs and CHIs for iterative summary, and enter Checksums.
      call LOGRES  (CEK, N, NLM, CHI, NT)
      call CREMA   (RBDS, RBDJ, RBDR, RBDQ, RHOS, RHOJ, RHOW, SSTAR,
     $              CHI)
C     !END
      call BYE ('TULIP')
C
      return
      end
