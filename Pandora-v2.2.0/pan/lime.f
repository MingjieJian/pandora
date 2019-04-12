      subroutine LIME
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Jul 15
C---- Controls the "Lyman" calculations.
C     (This is version 5 of LIME.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IADRS, IAKX, ICHEK, ICHKR, ICNX, ID, IDNRT, IDNTC, IEL,
     $        IEMUX, IERT, IETA, IF1, IGKX, IGP, IIMG, ILB, ILF, ILP,
     $        IN, IOPAC, IPIS, IQRK, IQRL, IRKO, IRLO, IRNDT, IRP, IRS,
     $        IS, ISL, ISLY, ISP, ISS, ITK, ITNU, ITNUL, ITRF, ITRK, IU,
     $        IUSE, IV, IW, IWNSV, IWR1, IWS, IX, IXCBL, IXJIK, IXJKA,
     $        IXKX, IZL, JJAL, JJBDI, JJBDL, JJCIJ, JJCK, JJCP, JJEP1,
     $        JJEP2, JJEQT, JJFIN, JJGK, JJGM, JJGVL, JJHND, JJICR,
     $        JJJBN, JJLCR, JJOR1, JJPIJ, JJQS, JJQU, JJR1W, JJRHO,
     $        JJRK, JJRKQ, JJRL, JJRLQ, JJSA, JJSQS, JJTE, JJTR, JJXIN,
     $        JJXK, JJXND, JJXNE, JJXNU, JJYBR, JJYK, JJZ, JN, KASE,
     $        KKU, MOX, MUX
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ(116),JJLCR)
      equivalence (IZOQ(123),JJXK )
      equivalence (IZOQ(112),JJGK )
      equivalence (IZOQ( 45),JJRHO)
      equivalence (IZOQ( 13),JJYBR)
      equivalence (IZOQ(144),JJPIJ)
      equivalence (IZOQ(175),JJGVL)
      equivalence (IZOQ( 18),JJRL )
      equivalence (IZOQ( 17),JJRK )
      equivalence (IZOQ( 31),JJCK )
      equivalence (IZOQ( 16),JJGM )
      equivalence (IZOQ( 44),JJBDI)
      equivalence (IZOQ( 20),JJQS )
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ(  1),JJEQT)
      equivalence (IZOQ( 34),JJAL )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(105),JJEP1)
      equivalence (IZOQ(  2),JJEP2)
      equivalence (IZOQ(235),JJSA )
      equivalence (IZOQ( 28),JJCP )
      equivalence (IZOQ(145),JJCIJ)
      equivalence (IZOQ( 14),JJXIN)
      equivalence (IZOQ( 15),JJFIN)
      equivalence (IZOQ(117),JJICR)
      equivalence (IZOQ( 19),JJQU )
      equivalence (IZOQ(110),JJOR1)
      equivalence (IZOQ(108),JJR1W)
      equivalence (IZOQ( 72),JJYK )
      equivalence (IZOQ( 67),JJRLQ)
      equivalence (IZOQ( 66),JJRKQ)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ(125),JJBDL)
      equivalence (IZOQ(174),JJJBN)
      equivalence (IZOQ(  8),JJTR )
      equivalence (IZOQ(223),JJSQS)
C     !DASH
      external KNOT, BLUNT, MOOLA, GLACIER, HAWSER, TAURUS, ROPE, HOUR,
     $         LOCRI, HELL, SLIDE, WGIVE, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(41)
      equivalence
     $(IN( 1),ITNU  ),(IN( 2),IU    ),(IN( 3),IGP   ),(IN( 4),ILB   ),
     $(IN( 5),ISP   ),(IN( 6),IF1   ),(IN( 7),IRNDT ),(IN( 8),ITK   ),
     $(IN( 9),IV    ),(IN(10),IOPAC ),(IN(11),ISS   ),(IN(12),IRS   ),
     $(IN(13),ILF   ),(IN(14),IERT  ),(IN(15),ILP   ),(IN(16),ID    ),
     $(IN(17),IDNRT ),(IN(18),ITNUL ),(IN(19),IDNTC ),(IN(20),ICNX  ),
     $(IN(21),ISLY  ),(IN(22),IXJIK ),(IN(23),IPIS  ),(IN(24),IRKO  ),
     $(IN(25),IRLO  ),(IN(26),IRP   ),(IN(27),ICHEK ),(IN(28),IWR1  ),
     $(IN(29),ICHKR ),(IN(30),IXJKA ),(IN(31),ITRK  ),(IN(32),ITRF  ),
     $(IN(33),IWNSV ),(IN(34),IXCBL ),(IN(35),ISL   ),(IN(36),IEL   ),
     $(IN(37),IZL   ),(IN(38),IXKX  ),(IN(39),IGKX  ),(IN(40),IAKX  ),
     $(IN(41),IEMUX )
C
      dimension JN( 5)
      equivalence
     $(JN( 1),IUSE  ),(JN( 2),IIMG  ),(JN( 3),IQRK  ),(JN( 4),IQRL  ),
     $(JN( 5),IADRS )
C
      call HI ('LIME')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call LOCRI   (IN, IS,  MOX, 'LIME')
      call MOOLA   (JN, IWS, MUX, 'LIME')
C     !EJECT
C---- Produce printout header
C
      call BLUNT
C
C---- Set up continuum integration data
C
      call SLIDE   (X, W, IW, X(JJXK), X(JJGK), W(IXKX), W(IGKX),
     $              W(IAKX), IW(IADRS), KKU)
C
C---- Retrieve and manipulate Continuum Data
C
      call HELL    (X, W, KKU, X(JJZ), X(JJXNU), X(JJLCR), W(IXKX),
     $              W(IGKX), IW(IADRS), W(ITNU), W(IOPAC), W(ITK),
     $              W(ITNUL), W(ILB), W(ISP), W(ITRK), W(IXJKA),
     $              W(IXCBL), IW(IIMG))
C
C---- Calculate Epsilons, and auxiliary functions
C
      call GLACIER (X, IX, W, IW, KKU, X(JJRHO), X(JJYBR), X(JJRL),
     $              X(JJRK), X(JJCK), X(JJGM), X(JJBDI), X(JJQS),
     $              X(JJXND), X(JJEQT), X(JJAL), X(JJTE), X(JJEP1),
     $              X(JJEP2), W(IXKX), W(IAKX), W(IGKX), X(JJCP),
     $              X(JJCIJ), X(JJGVL), W(IRS), W(ITK), W(IU),
     $              W(IEMUX), W(IF1), IW(IIMG))
C
C---- Compute other auxiliary functions, and IETA and KASE
C
      call KNOT    (W, KKU, W(IXKX), W(IGKX), W(IAKX), X(JJBDI),
     $              X(JJXIN), X(JJFIN), X(JJZ), X(JJLCR), X(JJICR),
     $              X(JJXNU), X(JJEP1), X(JJEP2), X(JJCP), W(IU),
     $              W(IEMUX), W(IV), W(ITNU), W(IF1), W(ITNUL),
     $              W(IRNDT), W(IDNRT), W(IDNTC), W(ICNX), W(ILB),
     $              W(ISP), W(ILP), W(ID), W(IERT), W(ITK),
     $              IETA, KASE)
C
C---- Calculate source function, and auxiliary term LF
C
      call TAURUS  (X, W, IW, KKU, W(IXKX), W(IAKX), W(IGKX),
     $              X(JJEP1), X(JJEP2), X(JJZ), W(ITNU), W(IOPAC),
     $              W(IEMUX), W(IV), W(ILB), W(ID), W(IF1), W(ILP),
     $              W(IRNDT), W(ISP), W(ILF), W(ITK), W(ISS),
     $              W(IERT), IW(IIMG), W(IWNSV), IW(IUSE), IETA,
     $              KASE)
C     !EJECT
C---- Calculate new Rates, and final auxiliary functions
C
      call ROPE    (X, IX, W, IW, KKU, W(IXKX), W(IAKX), W(IGKX),
     $              X(JJRK), X(JJRL), X(JJCK), X(JJSA), X(JJHND),
     $              X(JJXNE), X(JJQU), X(JJQS), X(JJSQS), X(JJBDI),
     $              X(JJOR1), X(JJCP), X(JJR1W), X(JJYK), X(JJTE),
     $              X(JJGM), X(JJXNU), X(JJRLQ), X(JJRKQ), X(JJAL),
     $              X(JJEP1), X(JJEP2), X(JJBDL), X(JJJBN), X(JJGVL),
     $              X(JJPIJ), X(JJTR), IETA, W(IF1), W(ISS), W(IEMUX),
     $              W(ISLY), W(IXJIK), W(ILB), W(ISP), W(ICNX),
     $              W(IRKO), W(IWR1), W(IDNRT), W(IRLO), W(IDNTC),
     $              W(IPIS), W(IV), W(IRS), W(IRP), W(ICHEK),
     $              W(ICHKR), W(ITRK), W(ITRF), W(IWNSV), IW(IUSE),
     $              IW(IIMG), W(IXJKA), IW(IQRK), IW(IQRL))
C
C---- Save debug checksums
C
      call HOUR    (W(ISS), X(JJEP1), X(JJEP2), W(ITK))
C
C---- Print
C
      call HAWSER  (KKU, X(JJEP1), X(JJEP2), X(JJRK), X(JJRL),
     $              X(JJRKQ), X(JJRLQ), X(JJQU), X(JJQS), X(JJBDL),
     $              X(JJPIJ), IETA, KASE, 1, W(IU), W(IF1), W(ISP),
     $              W(ID), W(ILF), W(ILP), W(IRNDT), W(ITK), W(IERT),
     $              W(ISS), W(ISLY), W(IXJIK), W(IRKO), W(IRLO),
     $              W(IRP), W(IRS), W(IWR1), W(IDNRT), W(IDNTC),
     $              W(IPIS), W(ITRF), W(ISL), W(IEL), W(IZL))
C
C     (Give back W & IW allotments)
      call WGIVE   (W,  'LIME')
      call IGIVE   (IW, 'LIME')
C     !END
      call BYE ('LIME')
C
      return
      end
