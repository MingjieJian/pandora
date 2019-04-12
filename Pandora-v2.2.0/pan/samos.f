      subroutine SAMOS
     $(X,W,IW,XLB1,XLB2,XLM,DUMP,XCBL,XJNU,SOURCE,ITS,LAG,Y,MOVING,
     $ ILFLX,MPROM,N,MRR,IMG)
C
C     Rudolf Loeser, 1982 Feb 05
C---- Controls computation of XJNU, SOURCE, ITS and LAG.
C     Also, saves matrices for line source function calculation.
C     !DASH
      save
C     !DASH
      real*8 CSFCT, R1N, SOURCE, W, X, XCBL, XJNU, XLB1, XLB2, XLM, Y
      integer ICOP, IDRCD, IKLDK, IKLSH, ILFLX, IMG, IN, IOPDK, IOPSH,
     $        IPHDK, IPHSH, IS, ISRCD, ITS, IW, IWHDK, IWHSH, IWNDK,
     $        IWNSH, IXOBL, JJCDK, JJCSH, JJKSR, JJMDK, JJVXS, JJXDK,
     $        JJXNE, JJXSH, JJZ, KKBHS, KKBNMS, KKCAPP, KKCNXP, KKDL,
     $        KKLTIT, KKMULT, KKSIGS, KKZAXA, KKZAYA, LAG, LDL, MOX,
     $        MPROM, MRR, N, NRPMX, NSHL, NSIZE
      logical DUMP, MOVING
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(153),JJKSR)
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ( 40),JJXSH)
      equivalence (IZOQ(132),JJXDK)
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(133),JJMDK)
      equivalence (IZOQ(109),JJCSH)
      equivalence (IZOQ(134),JJCDK)
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
      equivalence (KZQ( 53),ISRCD)
      equivalence (KZQ( 54),IDRCD)
      equivalence (RZQ( 23),R1N  )
      equivalence (RZQ( 69),CSFCT)
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
      equivalence (LEST( 4),NSHL )
      equivalence (LEST( 8),NRPMX)
C     !EJECT
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
      external TELYS, CONAN, ZERO1, TOTNES, HORSA, THANET, WGIVE,
     $         HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XCBL(Miklen), SOURCE(N), XJNU(N), XLB1(Li1len), IMG(N),
      dimension XCBL(*),      SOURCE(*), XJNU(*), XLB1(*),      IMG(*),
C
C               XLB2(Li2len)
     $          XLB2(*)
C
      dimension IN(12)
      equivalence
     $(IN( 1),IPHSH ),(IN( 2),IPHDK ),(IN( 3),IKLSH ),(IN( 4),IKLDK ),
     $(IN( 5),IOPSH ),(IN( 6),IOPDK ),(IN( 7),IWNSH ),(IN( 8),IWNDK ),
     $(IN( 9),ICOP  ),(IN(10),IXOBL ),(IN(11),IWHSH ),(IN(12),IWHDK )
C     !EJECT
C
      call HI ('SAMOS')
C     !BEG
C     (Get, and allocate, W allotment)
      call TELYS   (IN, IS, MOX, 'SAMOS')
C
C---- Compute background component, COP, of total opacity
      call CONAN   (N, XCBL(KKMULT), XCBL(KKSIGS), XCBL(KKCAPP),
     $              W(ICOP))
C
C---- Compute PHI, CKL, OPAC, WN and WH, for all rays
      NSIZE = N*N*NSHL
      call ZERO1   (W(IPHSH), NSIZE)
      call ZERO1   (W(IKLSH), NSIZE)
      call ZERO1   (W(IOPSH), NSIZE)
      call ZERO1   (W(IWNSH), NSIZE)
      if(ILFLX.gt.0) then
        call ZERO1 (W(IWHSH), NSIZE)
      end if
      call TOTNES  (X, W, IW, XLB1, N, NSHL, NRPMX, X(JJKSR), MRR,
     $              W(IPHSH), W(IPHDK), W(IKLSH), W(IKLDK), W(IOPSH),
     $              W(IOPDK), W(IWNSH), W(IWNDK), X(JJVXS), W(ICOP),
     $              XLM, XCBL(KKDL), X(JJXNE), XCBL(KKMULT), ISRCD,
     $              IDRCD, X(JJXSH), X(JJXDK), X(JJZ), R1N, X(JJMDK),
     $              W(IWHSH), W(IWHDK), ILFLX, Y, MOVING, MPROM, IMG,
     $              DUMP)
C---- Save matrices, for use in line source function calculation
      call HORSA   (N, MRR, W(IWNSH), W(IWNDK), W(IPHSH), W(IPHDK),
     $              W(IXOBL), XCBL(KKLTIT), W(IWHSH), W(IWHDK), ILFLX)
C
C---- Compute XJNU, SOURCE, ITS and LAG
      call THANET  (X(JJCSH), NSHL, X(JJCDK), MRR, N, XLM,
     $              XCBL(KKMULT), W(IKLSH), W(IKLDK), XCBL(KKZAXA),
     $              XCBL(KKZAYA), XCBL(KKBHS), XCBL(KKSIGS),
     $              XCBL(KKBNMS), XCBL(KKCNXP), CSFCT, ISRCD, IDRCD,
     $              XJNU, SOURCE, ITS, LAG, W(IOPSH), W(IOPDK),
     $              W(IWNSH), W(IWNDK), W, IW, DUMP)
C
C     (Give back W allotment)
      call WGIVE   (W, 'SAMOS')
C     !END
      call BYE ('SAMOS')
C
      return
      end
