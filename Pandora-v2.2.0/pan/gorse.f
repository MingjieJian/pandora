      subroutine GORSE
     $(X,IX,W,IW,N,NL,NSL,LUE,LUN,LUD,LUF,LUP,BDI,XND,XNK,XNE,BDIJ,
     $ BDIPR,XNDPR,XNKPR,BDIUW,BDIW,XNKUW,XNDUW,XNDW,XNDE,XNKS,XNDS,
     $ RK,RN,HND,H2N,FIONL,FLVSL,BATAL,XPBL,IKS,INS,IBS,IMG,ABDEL,FRN,
     $ FION,FLVS,WEIT,BD0,BD1,BDN,BDD,BDR,BDE,BDA,XNKW,SET)
C
C     Rudolf Loeser, 1975 Jul 30
C---- Deals with Number Densities.
C     Meaning of suffixes: PR=previous; UW=unweighted;
C                          W=weighted(i.e. unedited); S=LTE.
C
C---- In the comments below, steps flagged with "?" are optional,
C     or will be done if otherwise needed.
C
C     Output units: LUN - basic N and B values
C                   LUF - supplementary (final, weighted) values
C                   LUE - electron density
C                   LUD - detailed trace at depth # IBNVIEW
C                   LUP - graphs (plots)
C
C     (This is version 2 of GORSE.)
C     !DASH
      save
C     !DASH
      real*8 ABDEL, BATAL, BD0, BD1, BDA, BDD, BDE, BDI, BDIJ, BDIPR,
     $       BDIUW, BDIW, BDN, BDR, FION, FIONL, FLVS, FLVSL, FRN, H2N,
     $       HND, RK, RN, SET, W, WEIT, X, XND, XNDE, XNDPR, XNDS,
     $       XNDUW, XNDW, XNE, XNK, XNKPR, XNKS, XNKUW, XNKW, XPBL
      integer IBS, IKS, IMG, INS, IW, IX, KFCE, LUD, LUE, LUF, LUN, LUP,
     $        N, NL, NSL
      logical DONE, EDITED, NBAD, OFCE
C     !DASH
C     !EJECT
      external DHARMA, AVAR, FRIDAY, POPUP, OGBO, AMBER, NOCTAR, FAKER,
     $         RAIN, FOOT, RAVI, SOUR, HYDRANT, DARDAN, BALIAN, DERMOT,
     $         CYMBAL, CRATON, GUY, BIG, MYTU, CHALCIS, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XPBL(Lenpop), IKS(N), BATAL(N,NL), XND(N,NL), RN(N,NL),
      dimension XPBL(*),      IKS(*), BATAL(*),    XND(*),    RN(*),
C
C               FIONL(N), XNE(N), BDIJ(N,NL), BDIPR(N,NL), XNDPR(N,NL),
     $          FIONL(*), XNE(*), BDIJ(*),    BDIPR(*),    XNDPR(*),
C
C               XNKPR(N), BDIUW(N,NL), XNKUW(N), XNDUW(N,NL), ABDEL(N),
     $          XNKPR(*), BDIUW(*),    XNKUW(*), XNDUW(*),    ABDEL(*),
C
C               XNDS(N,NL), FRN(N), RK(N), FION(N), FLVS(N), BDI(N,NL),
     $          XNDS(*),    FRN(*), RK(*), FION(*), FLVS(*), BDI(*),
C
C               INS(N,NL), IBS(N,NL), IMG(N), FLVSL(*), HND(N), H2N(N),
     $          INS(*),    IBS(*),    IMG(*), FLVSL(*), HND(*), H2N(*),
C
C               XNK(N), XNDE(N,NL), WEIT(N,NL), XNDW(N,NL), BDIW(N,NL),
     $          XNK(*), XNDE(*),    WEIT(*),    XNDW(*),    BDIW(*),
C
C               SET(N,MUL), BD0(N,NL), BD1(N,NL), BDN(N,NL), BDD(N,NL),
     $          SET(*),     BD0(*),    BD1(*),    BDN(*),    BDD(*),
C
C               BDR(N,NL), BDE(N,NL), BDA(N,NL), XNKW(N), XNKS(N)
     $          BDR(*),    BDE(*),    BDA(*),    XNKW(*), XNKS(*)
C
      data KFCE /1/
C     !EJECT
C
      call HI ('GORSE')
C     !BEG
C---- Save previous results
      call NOCTAR  (N, NL, XNK, XNKPR, XND, XNDPR, BDI, BDIPR)
C---- ? Set up default B-ratios
      call DHARMA  (X, IX, W, IW, IMG)
C---- Compute new Departure Coefficients, Bs, from B-ratios
      call BIG     (X, IX, W, IW, BDIJ, BDIPR, BDIUW, BDIW, IMG, BD0,
     $              BD1, BDN, BDD, BDR, BDE, WEIT)
C---- ? Edit Departure Coefficients to prevent negative CSF
      call AVAR    (N, NL, 1, NL, BDIW, BDA, BATAL, 1)
C---- ? Compute and print new Electron density (JYDRO=1 only)
      call FRIDAY  (X, W, BDA, XNE, LUE)
C---- Compute new Number Densities using new (edited?) Bs
      call GUY     (X, W, BDIW, XNKPR, XNKUW, XNKW, XNKS, XNDPR, XNDUW,
     $              XNDW, XNDS, ABDEL, WEIT, IMG)
C---- Examine Number Densities (? and edit them to prevent LSF < 0)
      call RAVI    (X, IX, W, IW, XNDW, XNDE, EDITED, KFCE, OFCE)
C---- Renormalize Number Densities (? and edit Departure Coefficients)
      call DERMOT  (X, ABDEL, XNKW, XNDW, XNDE, BDA, XNK, XND, BDI,
     $              FRN, FION, IMG)
C---- Recompute stimulated emission terms using new Bs
      call CHALCIS (X, IX, W, IW)
C---- Test Number Densities, and set signals
      call BALIAN  (N, NL, XNK, XND, NBAD, LUF)
C---- Compute Number Density ratios
      call RAIN    (N, NL, XNKS, XNKUW, RK, XNDS, XNDUW, RN)
C---- Compute ionization fractions
      call FOOT    (X, XNKUW, XNDUW, HND, ABDEL, FION, FLVS, FIONL,
     $              FLVSL)
C---- ? Print (unweighted values!)
      call CYMBAL  (LUN, N, NL, NSL, XNKUW, XNKS, RK, XNDUW, XNDS, RN,
     $              BDIUW, FION, FLVS, FIONL, FLVSL, IKS, INS, IBS,
     $              NBAD, EDITED, OFCE)
C---- ? Plot Number Densities
      call CRATON  (X, W, IW, XNK, XND, LUP)
C---- ? Supplemental/Error printout of final, weighted values
      call FAKER   (N, NL, XNDW, XNDE, EDITED, XNK, XND, FRN, BDIW,
     $              BDI, BDIJ, NBAD, SET, LUF)
C     !EJECT
C---- ? Calculation-Trace printout
      call MYTU    (LUD, XNKPR, XNDPR, BDIPR, BDIJ, BD1, BD0, BDN, BDD,
     $              BDR, BDE, BDIUW, BDIW, BDA, XNE, XNKUW, XNKW, XNK,
     $              XNDUW, XNDW, XNDE, XND, BDI)
C---- ? Update Populations (? and print), (? and update NC)
      call POPUP   (X, W, ABDEL, XPBL)
C---- ? Compute Molecular Hydrogen
      call AMBER   (X, W, XPBL, H2N, DONE, LUN)
C---- ? Compute and print QOUT (for K-shell)
      call HYDRANT (X, W, BDI)
C---- ? Computer upper-level charge exchange terms
      call OGBO    (X, IX, XNK, XND, N, NL)
C---- ? Save for iterative summary
      call DARDAN  (XND, XNK, BDI, XNE)
C---- ? Save debug checksums
      call SOUR    (XNK, XND, BDI, XNE, H2N, DONE)
C     !END
      call BYE ('GORSE')
C
      return
      end
