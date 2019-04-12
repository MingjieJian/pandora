      subroutine ROPE
     $(X,IX,W,IW,KKU,XKX,AKX,GKX,RK,RL,CK,SA,HND,XNE,CQU,CQS,SQS,BDI,
     $ OR1,CP,R1W,YK,TE,GM,XNU,RLC,RKC,AL,EP1,EP2,B1,XJBN,GVL,PIJ,TR,
     $ IETA,F1,S,EMUX,SLY,XJIKB,XLB,SP,CNXP,RKO,WR1,DNRT,RLO,DNRTC,
     $ PIS,V,RS,RP,CHECKL,CHECKR,TRK,TREFF,WNSAV,IXUSE,IMG,XJIKA,
     $ IQRK,IQRL)
C
C     Rudolf Loeser, 1980 Jul 15
C---- Does final Lyman calculations.
C     (This is version 3 of ROPE.)
C     !DASH
      save
C     !DASH
      real*8 AKX, AL, B1, BDI, CHECKL, CHECKR, CK, CNXP, CP, CQS, CQU,
     $       DNRT, DNRTC, EMUX, EP1, EP2, F1, GKX, GM, GVL, HND, OR1,
     $       PIJ, PIS, R1W, RK, RKC, RKO, RL, RLC, RLO, RP, RS, S, SA,
     $       SLY, SMP, SP, SQS, TE, TR, TREFF, TRK, V, W, WMN, WMX,
     $       WNSAV, WR1, X, XINCH, XJBN, XJIKA, XJIKB, XKX, XLB, XNE,
     $       XNU, XNUK, YK
      integer IB1EP, IB1NW, IETA, IFO, IMG, IN, IQRK, IQRL, IRKA, IRKB,
     $        IS, ISPKL, ITERM, IVEC, IW, IWEIT, IX, IXPBL, IXUSE, KKU,
     $        KOLEV, MOX, N, NCR, NL, NSL, jummy
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(32),NCR)
      equivalence (JZQ(40),NSL)
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
      equivalence (RZQ(  9),XNUK )
      equivalence (RZQ( 56),SMP  )
      equivalence (KZQ( 33),KOLEV)
      equivalence (RZQ( 55),WMX  )
      equivalence (RZQ( 43),WMN  )
      equivalence (RZQ( 54),XINCH)
C     !DASH
C     !EJECT
      external LUMP, SETPIJ, FLOTE, JUMBLE, RAKE, RALE, BRENDA, LOWELL,
     $         QUIT, CUP, TALE, PALE, ROBERT, POPIO, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               IQRK(NSL), CHECKL(N), CHECKR(N), RLO(N), TE(N), SQS(N),
      dimension IQRK(*),   CHECKL(*), CHECKR(*), RLO(*), TE(*), SQS(*),
C
C               RP(N), XKX(KKX), AKX(KKX), GKX(KKX), RK(N,NSL), XNE(N),
     $          RP(*), XKX(*),   AKX(*),   GKX(*),   RK(*),     XNE(*),
C
C               CK(N,NSL), CQU(N,NSL), CQS(N,NSL), B1(N), XJIKB(N,KKX),
     $          CK(*),     CQU(*),     CQS(*),     B1(*), XJIKB(*),
C
C               RLC(N,NL), RKC(N,NL), AL(NL), IMG(N), SA(N), IQRL(NSL),
     $          RLC(*),    RKC(*),    AL(*),  IMG(*), SA(*), IQRL(*),
C
C               OR1(N), RS(N), F1(N), SLY(N,KKX), PIJ(N,NL,NL), RKO(N),
     $          OR1(*), RS(*), F1(*), SLY(*),     PIJ(*),       RKO(*),
C
C               XLB(N,KKX), SP(N,KKX), CNXP(N,KKX), TREFF(N), XNU(NSL),
     $          XLB(*),     SP(*),     CNXP(*),     TREFF(*), XNU(*),
C
C               WR1(N), DNRT(N), PIS(N,NL), IXUSE(KKX), EP1(N), EP2(N),
     $          WR1(*), DNRT(*), PIS(*),    IXUSE(*),   EP1(*), EP2(*),
C
C               WNSAV(N,N,KKX), XJIKA(N,KKX), V(N,KKX), R1W(N), HND(N),
     $          WNSAV(*),       XJIKA(*),     V(*),     R1W(*), HND(*),
C
C               CP(NSL+1), TR(N,NSL), GVL(N,NL), DNRTC(N), XJBN(N,KKX),
     $          CP(*),     TR(*),     GVL(*),    DNRTC(*), XJBN(*),
C
C               BDI(N,NL), TRK(N,KKX), YK(KKX), GM(N,NSL), EMUX(N,KKX),
     $          BDI(*),    TRK(*),     YK(*),   GM(*),     EMUX(*),
C
C               RL(N,NSL), S(N)
     $          RL(*),     S(*)
C
      dimension IN(10)
      equivalence
     $(IN( 1),ITERM ),(IN( 2),IVEC  ),(IN( 3),ISPKL ),(IN( 4),IRKA  ),
     $(IN( 5),IRKB  ),(IN( 6),IB1EP ),(IN( 7),IXPBL ),(IN( 8),IWEIT ),
     $(IN( 9),IB1NW ),(IN(10),IFO   )
C     !EJECT
C
      call HI ('ROPE')
C     !BEG
C     (Get, and allocate, W allotment)
      call LUMP   (IN, IS, MOX, 'ROPE')
C     (Initialize populations buffer)
      call POPIO  ('INIT', jummy, W(IXPBL))
C
C---- Compute B1
      call CUP    (F1, S, N, B1, BDI, NL, W(IB1NW), W(IWEIT))
C---- Compute SLY
      call FLOTE  (N, B1, KKU, XKX, XNUK, EMUX, SLY)
C---- Compute XJIKB
      call JUMBLE (WNSAV, IXUSE, IETA, N, KKU, XJIKB, SLY, XLB, SP,
     $             CNXP, XJBN)
C---- Compute RK
      call RAKE   (N, KKU, AKX, XKX, GKX, XJIKB, RK, NL, RKO, OR1,
     $             CP, WR1, R1W, XINCH, WMN, WMX, SMP, KOLEV, DNRT,
     $             NCR, XJIKA, W(IRKA), W(IRKB), CHECKR, W(IWEIT),
     $             IMG, W(IFO))
C---- Compute RL
      call RALE   (N, NL, KKU, RL, CP, AKX, GKX, XKX, EMUX, XJIKB,
     $             RLO, KOLEV, YK, XNU)
C---- Compute additional terms for RK and/or RL
      call BRENDA (X, IX, W, IW, W(IXPBL), RK, IQRK, RL, IQRL)
C---- Compute cooling rates data
      call LOWELL (N, NL, KKU, CP, AKX, GKX, XKX, EMUX, XJIKB, XNU,
     $             RLC, RKC, DNRTC, NCR)
C---- Compute effective radiation temperature
      call ROBERT (KOLEV, N, KKU, CP, XNU, RK, TRK, TREFF, TR, W)
C---- Compute radiation terms CQU, CQS and SUM
      call QUIT   (N, NL, NSL, KOLEV, KOLEV, GM, RK, RL, CK, TE, SA,
     $             HND, XNE, CQU, CQS, SQS, W(IVEC))
C---- Compute PIJ
      call SETPIJ (0, AL, RK, RL, CK, CQS, SQS, W(ISPKL), W(ITERM),
     $             PIS, GM, GVL, PIJ)
C---- Save RK for iterative summaries
      call TALE   (N, NL, RK)
C---- Compute RP and CHECKL, for display only
      call PALE   (N, KKU, CP, AKX, GKX, XKX, V, XJIKB, B1, W(IB1EP),
     $             EP1, EP2, RS, RP, CHECKL, KOLEV)
C
C     (Give back W allotment)
      call WGIVE  (W, 'ROPE')
C     !END
      call BYE ('ROPE')
C
      return
      end
