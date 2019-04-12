      subroutine SETTUP
     $(X,IX,W,IW,N,NSL,NL,LU,LUP,LUG,LUS,LUD,LUM,XNU,XNUC,P,CP,TE,SA,
     $ TEX,TR,XNE,HND,IQRK,IQRL,GMI,RKI,CKI,RLI,CQUI,CQSI,SQS,TER,NTE,
     $ CII,CEI,SLT,SPKL,PIS,AL,CIJ,PIJ,FCJ,FCK,Z,XND,XNK,VM,ZT,H2N,
     $ GVL,GVI,CKIAD,CIJAD,RHEAB,XNC,CHKI,CHIJ,TIJ,DIJ,CMCI,CACI,CMCE,
     $ CACE,DRCT,FCE,AIJ,AATIJ,KIJ,NPQ,LRQ,NLE,XPBL,IMG,SIJ)
C
C     Rudolf Loeser, 1969 Dec 10
C---- Computes transition rates.
C     (This is version 2 of SETTUP.)
C     !DASH
      save
C     !DASH
      real*8 AATIJ, AIJ, AL, CACE, CACI, CEI, CHIJ, CHKI, CII, CIJ,
     $       CIJAD, CKI, CKIAD, CMCE, CMCI, CP, CQSI, CQUI, DIJ, DRCT,
     $       FCE, FCJ, FCK, GMI, GVI, GVL, H2N, HND, P, PIJ, PIS, RHEAB,
     $       RKI, RLI, SA, SIJ, SLT, SPKL, SQS, TE, TER, TEX, TIJ, TR,
     $       VM, W, X, XNC, XND, XNE, XNK, XNU, XNUC, XPBL, Z, ZT
      integer IMG, IQRK, IQRL, IW, IX, KIJ, LRQ, LU, LUD, LUG, LUM, LUP,
     $        LUS, N, NL, NLE, NPQ, NSL, NTE
C     !DASH
C     !EJECT
      external GAMBIT, CIRCUS, FROLIC, TURBOT, EXPOUND, SETCIJ, SETPIJ,
     $         SETRIJ, QUETZAL, GOONEY, SICCUR, DROPION, OPHIR, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XNU(NSL), P(NSL), CP(NSL+1), RLI(N,NSL), SA(N), TEX(N),
      dimension XNU(*),   P(*),   CP(*),     RLI(*),     SA(*), TEX(*),
C
C               TE(N), XNE(N), CHKI(N,NSL), IQRL(NSL), SPKL(N), SQS(N),
     $          TE(*), XNE(*), CHKI(*),     IQRL(*),   SPKL(*),
C
C               GMI(N,NSL), RKI(N,NSL), CKI(N,NSL), RHEAB(N), TR(N,NL),
     $          GMI(*),     RKI(*),     CKI(*),     RHEAB(*), TR(*),
C
C               CQUI(N,NSL), CHIJ(N,NL,NL), CIJ(N,NL,NL), CII(NSL,NTE),
     $          CQUI(*),     CHIJ(*),       CIJ(*),       CII(*),
C
C               CEI(NTE,MUL), HND(N), XND(N,NL), Z(N), H2N(N), DRCT(N),
     $          CEI(*),       HND(*), XND(*),    Z(*), H2N(*), DRCT(*),
C
C               SLT(N), PIS(N,NL), GVL(N,NL), TER(NTE), XNK(N), XNC(N),
     $          SLT(*), PIS(*),    GVL(*),    TER(*),   XNK(*), XNC(*),
C
C               FCJ(N,NL), CKIAD(N,NSL), GVI(N), FCK(NL), ZT(N), AL(N),
     $          FCJ(*),    CKIAD(*),     GVI(*), FCK(*),  ZT(*), AL(*),
C
C               CIJAD(N,NL,NL), CQSI(N,NSL), IQRK(NSL), IMG(N), SQS(N),
     $          CIJAD(*),       CQSI(*),     IQRK(*),   IMG(*), SQS(*),
C
C               PIJ(N,NL,NL), TIJ(N,NL,NL), DIJ(N,NL,NL), XPBL(Lenpbl),
     $          PIJ(*),       TIJ(*),       DIJ(*),       XPBL(*),
C
C               CACI(NSL), CMCI(NSL), CMCE(MUL), CACE(MUL), KIJ(NL,NL),
     $          CACI(*),   CMCI(*),   CMCE(*),   CACE(*),   KIJ(*),
C
C               AATIJ(NL,NL), SIJ(N,NL,NL), NPQ(NSL), XNUC(NSL), VM(N),
     $          AATIJ(*),     SIJ(*),       NPQ(*),   XNUC(*),   VM(*),
C
C               FCE(N,NT), NLE(NSL), LRQ(NSL), AIJ(NL,NL)
     $          FCE(*),    NLE(*),   LRQ(*),   AIJ(*)
C     !EJECT
C
C     Printout LUNs:  LUP - RATEPRNT
C                     LUG - RATEGRAF
C                     LUS - RATESUMM
C                     LUD - AMBPRNT, VLGPRNT
C                     LUM - IRATE (= "minimal")
C
C
      call HI ('SETTUP')
C     !BEG
C---- Compute rates due to collisions with Hydrogen atoms
      call OPHIR   (X, IX, W, IW, XPBL, CHKI, CHIJ, LUP)
C---- Compute rates due to fast electrons
      call GOONEY  (X, W, XPBL, FCJ, FCK, LUP)
C---- Compute GAMMA and exhibit it
      call GAMBIT  (N, NSL, P, TE, XNU, GMI, LUP)
C---- Compute CK
      call CIRCUS  (X, IX, N, NSL, NTE, TER, CII, TE, XNU, XNUC, NPQ,
     $              LRQ, NLE, XNE, XNC, CMCI, CACI, CKIAD, CKI, FCK,
     $              CHKI, LUP)
C---- Compute RK and RL, and exhibit them (also CK)
      call FROLIC  (X, IX, W, IW, XPBL, RKI, IQRK, RLI, IQRL, LUP,
     $              LUG, LUS)
C---- Compute QS, QU and SQS, and exhibit them
      call QUETZAL (N, NL, NSL, GMI, RKI, RLI, CKI, TE, SA, HND, XNE,
     $              CQUI, CQSI, SQS, DRCT, LUP)
C---- Compute and exhibit collisional transition rates
      call SETCIJ  (X, IX, LUP, P, TEX, XNE, XNC, XNU, XNUC, TER, CEI,
     $              CMCE, CACE, AIJ, AATIJ, NPQ, LRQ, CIJAD, CIJ, FCJ,
     $              CHIJ, FCE, KIJ, SIJ)
C---- Compute and exhibit ambipolar diffusion, velocity gradient terms,
C     and Helium abundance variation
      call TURBOT  (X, IX, W, IW, XPBL, Z, XND, XNK, VM, ZT, HND, TE,
     $              XNE, H2N, RHEAB, SA, RKI, CKI, CQSI, GVL, GVI, IMG,
     $              LUD)
C---- Compute and exhibit bound-free-bound transition rates
      call SETPIJ  (LUP, AL, RKI, RLI, CKI, CQSI, SQS, SPKL, SLT, PIS,
     $              GMI, GVL, PIJ)
C---- Optional diffusion analysis of PIJ
      call SICCUR  (LU, AL, RKI, RLI, CKI, CQSI, SQS, SPKL, SLT, PIS,
     $              GMI, PIJ, CIJ, TIJ, DIJ)
C---- Exhibit RIJ=CIJ+PIJ
      call SETRIJ  (LUP, PIJ, CIJ)
C---- Optional "minimal" printout
      call DROPION (W, LUM, CQUI, CQSI, GMI, RKI, RLI, CKI, CIJ, PIJ)
C---- Checksums
      call EXPOUND (N, NL, NSL, GMI, CKI, RKI, RLI, CQUI, CQSI, PIJ,
     $              CIJ, CHKI, CHIJ)
C     !END
      call BYE ('SETTUP')
C
      return
      end
