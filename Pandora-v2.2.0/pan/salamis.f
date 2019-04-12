      subroutine SALAMIS
     $(X,W,IW,XLB1,XLB2,XLM,DUMP,XCBL,XJNU,SOURCE,ITS,LAG,Y,MOVING,
     $ ILFLX,MPROM,N,LG,IMG)
C
C     Rudolf Loeser, 1982 Feb 02
C---- Controls computation of XJNU, SOURCE, ITS and LAG.
C     Also, saves matrices for line source function computation.
C     !DASH
      save
C     !DASH
      real*8 CSFCT, SOURCE, W, X, XCBL, XJNU, XLB1, XLB2, XLM, Y
      integer ICKL, ICOP, ILFLX, IMG, IMUCD, IN, IOPAC, IPHI, IS, ITS,
     $        IW, IWH, IWN, IXOBL, JJCMU, JJVXS, JJXMU, JJXNE, KKBHS,
     $        KKBNMS, KKCAPP, KKCNXP, KKDL, KKLTIT, KKMULT, KKSIGS,
     $        KKZAXA, KKZAYA, LAG, LDL, LG, MOX, MPROM, N
      logical DUMP, MOVING
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ(130),JJXMU)
      equivalence (IZOQ(131),JJCMU)
      equivalence (IZOQ(  9),JJXNE)
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
      equivalence (KZQ( 41),IMUCD)
      equivalence (RZQ( 69),CSFCT)
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK( 2),KKMULT)
      equivalence (KKK(45),KKSIGS)
      equivalence (KKK(29),KKCAPP)
      equivalence (KKK(46),KKDL  )
      equivalence (KKK( 1),KKLTIT)
      equivalence (KKK(58),KKZAXA)
      equivalence (KKK(59),KKZAYA)
      equivalence (KKK(10),KKBHS )
      equivalence (KKK(16),KKBNMS)
      equivalence (KKK(11),KKCNXP)
C     !DASH
C     !EJECT
      external HIMILCO, CONAN, BANGOR, ABALLAC, ERITH, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XCBL(Miklen), XJNU(N), IMG(N), XLB1(Li1len), SOURCE(N),
      dimension XCBL(*),      XJNU(*), IMG(*), XLB1(*),      SOURCE(*),
C
C               XLB2(Li2len)
     $          XLB2(*)
C
      dimension IN(7)
      equivalence
     $(IN( 1),IPHI  ),(IN( 2),ICKL  ),(IN( 3),IOPAC ),(IN( 4),IWN   ),
     $(IN( 5),ICOP  ),(IN( 6),IXOBL ),(IN( 7),IWH   )
C
      call HI ('SALAMIS')
C     !BEG
C     (Get, and allocate, W allotment)
      call HIMILCO (IN, IS, MOX, 'SALAMIS')
C
C---- Compute background component, COP, of total opacity
      call CONAN   (N, XCBL(KKMULT), XCBL(KKSIGS), XCBL(KKCAPP),
     $              W(ICOP))
C---- Compute PHI, CKL, OPAC and weight matrices, for all rays
      call BANGOR  (X, W, IW, XLB1, N, LG, X(JJVXS), W(ICOP), IMUCD,
     $              XLM, XCBL(KKDL), X(JJXNE), MPROM, X(JJXMU),
     $              W(IPHI), W(ICKL), W(IOPAC), W(IWN), W(IWH), Y,
     $              MOVING, ILFLX, IMG, DUMP)
C---- Save PHI and matrices, for Line Source Function calculation
      call ABALLAC (N, LG, W(IWN), W(IWH), ILFLX, W(IPHI), W(IXOBL),
     $              XCBL(KKLTIT))
C---- Compute XJNU, SOURCE, ITS and LAG
      call ERITH   (N, LG, X(JJCMU), XLM, XCBL(KKMULT), W(ICKL),
     $              XCBL(KKZAXA), XCBL(KKZAYA), XCBL(KKBHS),
     $              XCBL(KKSIGS), XCBL(KKBNMS), XCBL(KKCNXP), CSFCT,
     $              IMUCD, XJNU, SOURCE, ITS, LAG, W(IOPAC), W(IWN),
     $              W, IW, DUMP)
C
C     (Give back W allotment)
      call WGIVE   (W, 'SALAMIS')
C     !END
      call BYE ('SALAMIS')
C
      return
      end
