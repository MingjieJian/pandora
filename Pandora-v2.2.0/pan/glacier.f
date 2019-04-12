      subroutine GLACIER
     $(X,IX,W,IW,KKU,RHO,YBR,RL,RK,CK,GM,BDI,CQS,XND,CQTAIL,AL,TE,EP1,
     $ EP2,XKX,AKX,GKX,CP,CIJ,GVL,RS,TK,U,EMUX,F1,IMG)
C
C     Rudolf Loeser, 1975 Sep 26
C---- Computes Lyman Epsilons.
C     !DASH
      save
C     !DASH
      real*8 AKX, AL, BDI, CIJ, CK, CP, CQS, CQTAIL, EMUX, EP1, EP2, F1,
     $       GKX, GM, GVL, RHO, RK, RL, RS, TE, TK, U, W, WEP, X, XKX,
     $       XND, YBR
      integer IE1, IE2, IENW, IES, IFO, IMG, IN, IS, IW, IWEIT, IX, KKU,
     $        MOX, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 40),WEP  )
C     !DASH
C     !EJECT
      external JEANNIE, BERG, MOVE1, CASSIA, WEAPON, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               RHO(N,NT), RL(N,NSL), RK(N,NSL), CIJ(N,NL,NL), CP(NSL),
      dimension RHO(*),    RL(*),     RK(*),     CIJ(*),       CP(*),
C
C               YBR(N,NT), BDI(N,NL), CQS(N,NSL), XND(N,NL), CK(N,NSL),
     $          YBR(*),    BDI(*),    CQS(*),     XND(*),    CK(*),
C
C               F1(N), GVL(N,NL), EP1(N), GM(N,NSL), CQTAIL(MQT), U(N),
     $          F1(*), GVL(*),    EP1(*), GM(*),     CQTAIL(*),   U(*),
C
C               TK(N), XKX(KKX), AKX(KKX), EP2(N), AL(NL), EMUX(N,KKX),
     $          TK(*), XKX(*),   AKX(*),   EP2(*), AL(*),  EMUX(*),
C
C               RS(N), TE(N), GKX(KKX), IMG(N)
     $          RS(*), TE(*), GKX(*),   IMG(*)
C
      dimension IN(6)
      equivalence
     $(IN( 1),IES   ),(IN( 2),IE1   ),(IN( 3),IE2   ),(IN( 4),IENW  ),
     $(IN( 5),IWEIT ),(IN( 6),IFO   )
C
      call HI ('GLACIER')
C     !BEG
C     (Get, and allocate, W allotment)
      call JEANNIE (IN, IS, MOX, 'GLACIER')
C
C---- Compute auxiliary functions U, EMUX, F1 and RS
      call BERG    (N, KKU, U, TE, XKX, AKX, GKX, CP, EMUX, F1, RS)
C---- Save old Epsilons
      call MOVE1   (EP1, N, W(IE1))
      call MOVE1   (EP2, N, W(IE2))
C---- Compute new Epsilons
      call CASSIA  (X, IX, W, IW, RHO, YBR, RL, RK, CK, CIJ, GM, BDI,
     $              CQS, XND, CQTAIL, AL, GVL, EP1, EP2, RS, TK)
C---- Edit new Epsilons
      call WEAPON  (N, WEP, EP1, EP2, BDI, TK, W(IE1), W(IE2), W(IES),
     $              IMG, W(IENW), W(IWEIT), W(IFO))
C
C     (Give back W allotment)
      call WGIVE   (W,'GLACIER')
C     !END
      call BYE ('GLACIER')
C
      return
      end
