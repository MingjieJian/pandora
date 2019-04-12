      subroutine NADINE
     $(X,IX,W,IW,XLB1,XLB2,XLB3,LZA,ZAUX,XKPCR,INPAIR,NLPAIR,WAVC,KSW)
C
C     Rudolf Loeser, 1984 Feb 28
C---- Reads Batch 2 of input --- general data,
C     does post-read defaults, and
C     initializes Line Intensity Data blocks.
C     (This is version 3 of NADINE.)
C     !DASH
      save
C     !DASH
      real*8 W, WAVC, WMNO, WMXO, X, XKPCR, XLB1, XLB2, XLB3, ZAUX
      integer ICDL, ICOP, ICRD, ICRS, ICSK, ICVW, IDDL, IDPM, IDRO,
     $        IDWN, IGMA, IN, INPAIR, IPGL, IRHW, IRHWO, IRKWO, IS,
     $        ISTNE, IW, IWS, IWSM, IX, IXC, IXIB, IXIR, IXIS, IXLAM,
     $        IXP, IXR, IY, JIFS, JILS, JISB1, JISB2, JIST, JKST, JN,
     $        JNED, KSW, LZA, MOX, MUX, NLPAIR
C     !DASH
      external VANILLA, JET, SAUCE, ELIDYR, ZERO1, ZEROI, WGIVE, IGIVE,
     $         BUSTER, CINAMON, GUITAR, SARD, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XLB1(Li1len), XLB2(Li2len), ZAUX(LZM,NZM), WAVC(KWC),
      dimension XLB1(*),      XLB2(*),      ZAUX(*),       WAVC(*),
C
C               XLB3(Li3len), KSW(Nopac), INPAIR(2,NT), NLPAIR(2,NL),
     $          XLB3(*),      KSW(*),     INPAIR(*),    NLPAIR(*),
C
C               XKPCR(NT), LZA(50)
     $          XKPCR(*),  LZA(*)
C
      dimension IN(25)
      equivalence
     $(IN( 1),IY    ),(IN( 2),ISTNE ),(IN( 3),ICRD  ),(IN( 4),ICVW  ),
     $(IN( 5),ICSK  ),(IN( 6),ICRS  ),(IN( 7),ICOP  ),(IN( 8),IRHW  ),
     $(IN( 9),ICDL  ),(IN(10),IDDL  ),(IN(11),IDWN  ),(IN(12),IWSM  ),
     $(IN(13),IDRO  ),(IN(14),IXC   ),(IN(15),IXP   ),(IN(16),IGMA  ),
     $(IN(17),IXR   ),(IN(18),IPGL  ),(IN(19),IXIB  ),(IN(20),IXIR  ),
     $(IN(21),IXIS  ),(IN(22),IXLAM ),(IN(23),IRHWO ),(IN(24),IRKWO ),
     $(IN(25),IDPM  )
C
      dimension JN(7)
      equivalence
     $(JN( 1),JIFS  ),(JN( 2),JILS  ),(JN( 3),JNED  ),(JN( 4),JISB1 ),
     $(JN( 5),JISB2 ),(JN( 6),JIST  ),(JN( 7),JKST  )
C
      data WMNO,WMXO /-2.D0, -2.D0/
C     !EJECT
C
      call HI ('NADINE')
C     !BEG
C---- (Get, and allocate, W & IW allotments . . .
      call JET     (IN, IS,  MOX, 'NADINE')
      call SARD    (JN, IWS, MUX, 'NADINE')
C     . . . and initialize them)
      call ZERO1   (W (IS),     (MOX-IS +1))
      call ZEROI   (IW(IWS), 1, (MUX-IWS+1))
      call GUITAR  (W(IY), W(IDRO), W(IGMA), W(IXC), W(IXP), W(IXR),
     $              W(ICRS), W(ICDL), W(ICRD), W(ICVW), W(ICSK),
     $              W(IDPM), IW(JNED), IW(JISB1), IW(JISB2), W(IRHW))
C
C---- Read `General Input'
      call VANILLA (X, IX, W, LZA, ZAUX, XKPCR, INPAIR, KSW, WMNO,
     $              WMXO, W(IRKWO), W(IY), W(ICRD), W(ICVW), W(ICSK),
     $              W(ICRS), W(ICOP), W(IRHW), W(IRHWO), W(ICDL),
     $              W(IDDL), W(IDWN), W(IWSM), W(IDRO), W(IXC),
     $              W(IXP), W(IXR), W(IGMA), W(IPGL), W(IXIB),
     $              W(IXIR), W(IXIS), W(IDPM), IW(JIFS), IW(JILS),
     $              IW(JNED), IW(JISB1), IW(JISB2), IW(JIST),
     $              IW(JKST))
C
C---- Get data needed for background contributor lines
C     (Also check on NDWM, NDW, and set up NMLR)
      call CINAMON (X, IX, W, IW)
C
C---- Post-read defaults for `General Input'
      call SAUCE   (X, IX, W, IW, XKPCR, WAVC, NLPAIR, W(ICOP),
     $              W(IRHW), W(IRHWO), W(IXLAM), W(IY), W(IDDL),
     $              W(ICDL), W(IDWN), W(ICRD), W(ICVW), W(ICSK),
     $              W(ICRS), W(ISTNE), IW(JKST), IW(JIST), IW(JISB1),
     $              WMNO, WMXO, W(IRKWO))
C
C---- Check data for background contributor lines
      call BUSTER  (X, IX, W, IW, W(IDDL), W(ICDL), W(ICRD), W(ICVW),
     $              W(ICSK), W(ICRS))
C
C---- Initialize and write Line Intensity Data blocks
      call ELIDYR  (X, XLB1, XLB2, XLB3, W(IY), W(ICRD), W(ICVW),
     $              W(ICSK), W(ICRS), W(ICOP), W(IRHW), W(ICDL),
     $              W(IDDL), W(IDWN), W(IWSM), W(IDRO), W(IXC),
     $              W(IXP), W(IXR), W(IGMA), W(IPGL), W(IXIB),
     $              W(IXIR), W(IXIS), W(IXLAM), W(ISTNE), W(IDPM),
     $              IW(JIFS), IW(JILS), IW(JNED), IW(JISB1),
     $              IW(JISB2), IW(JIST), IW(JKST))
C
C---- (Give back W & IW allotments)
      call WGIVE   (W,  'NADINE')
      call IGIVE   (IW, 'NADINE')
C     !END
      call BYE ('NADINE')
C
      return
      end
