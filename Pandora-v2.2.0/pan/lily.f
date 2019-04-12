      subroutine LILY
     $(X,IX,W,IW,XLB1)
C
C     Rudolf Loeser, 1980 May 02
C---- Controls TULIP.
C     (This is version 2 of LILY.)
C     !DASH
      save
C     !DASH
      real*8 W, X, XLB1
      integer IABDR, IARHO, IBA, IBB, IBC, IBJ, IBQ, IBR, IBRAS, IBSL,
     $        IBU, ICAU1, IDROL, IEPL, IFSL, IIMG, ILSL, IN, IORHO,
     $        IPTAU, IRHWL, IRJ, IRO, IRP, IRW, IRX, IS, ISTAR, IT,
     $        ITAUL, IW, IWATE, IWEIT, IWS, IWSML, IWT, IWW, IX, IXVAL,
     $        JJAIJ, JJALF, JJAW, JJBAT, JJBIJ, JJCEK, JJCIJ, JJKIJ,
     $        JJQHI, JJQSA, JJQST, JJRHO, JJS, JJTE, JJTRA, JJXNU,
     $        JJYBR, JJZ, JN, LUC, LUD, LUF, LUH, LUQ, LUS, MO, MOX,
     $        MUX, N, NEDL, NT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 5),NT )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 13),JJYBR)
      equivalence (IZOQ( 25),JJALF)
      equivalence (IZOQ( 23),JJBIJ)
      equivalence (IZOQ( 45),JJRHO)
      equivalence (IZOQ( 39),JJBAT)
      equivalence (IZOQ( 36),JJCEK)
      equivalence (IZOQ(145),JJCIJ)
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 32),JJAIJ)
      equivalence (IZOQ(173),JJTRA)
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ(241),JJS  )
      equivalence (IZOQ(242),JJAW )
      equivalence (IZOQ(245),JJQHI)
      equivalence (IZOQ(246),JJQSA)
      equivalence (IZOQ(247),JJQST)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  2),JJKIJ)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external GENTIAN, TULIP, BHUTAN, VERVAIN, ZENGI, WGIVE, BUILT,
     $         MUM, TURIN, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XLB1(Lizlen)
      dimension XLB1(*)
C
      dimension IN(31)
      equivalence
     $(IN( 1),IDROL ),(IN( 2),IRJ   ),(IN( 3),IRP   ),(IN( 4),IT    ),
     $(IN( 5),IBRAS ),(IN( 6),IBJ   ),(IN( 7),IRX   ),(IN( 8),IWSML ),
     $(IN( 9),IPTAU ),(IN(10),IWATE ),(IN(11),IBR   ),(IN(12),IRW   ),
     $(IN(13),IORHO ),(IN(14),IRO   ),(IN(15),IABDR ),(IN(16),IWW   ),
     $(IN(17),IRHWL ),(IN(18),ITAUL ),(IN(19),IWEIT ),(IN(20),IWT   ),
     $(IN(21),IEPL  ),(IN(22),ISTAR ),(IN(23),IBA   ),(IN(24),IBB   ),
     $(IN(25),IBC   ),(IN(26),IXVAL ),(IN(27),ICAU1 ),(IN(28),IBQ   ),
     $(IN(29),IARHO ),(IN(30),IBU   ),(IN(31),IBSL  )
C
      dimension JN(4)
      equivalence
     $(JN( 1),IFSL  ),(JN( 2),ILSL  ),(JN( 3),NEDL  ),(JN( 4),IIMG  )
C     !EJECT
C
      call HI ('LILY')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call MUM     (IN, IS , MOX, 'LILY')
      call ZENGI   (JN, IWS, MUX, 'LILY')
C
C---- Set up output units, and print title
      call TURIN   (MO,LUH,LUD,LUF,LUS,LUQ,LUC)
C
C---- Get data from Line Intensity data blocks
      call GENTIAN (N, NT, XLB1, X(JJYBR), W(IRP), X(JJS), X(JJAW),
     $              W(IEPL), W(IBSL), W(ITAUL), W(IRHWL), IW(NEDL),
     $              W(IDROL), W(IORHO), IW(IFSL), IW(ILSL), W(IWSML))
C
C---- Set up control switches for TULIP
      call BUILT   (IX, X(JJYBR))
C
C---- Get final "b-ratios" and Rhos
      call TULIP   (X, IX, W, IW, LUH, LUD, LUF, LUS, LUQ, LUC,
     $              X(JJYBR), X(JJALF), X(JJBIJ), X(JJRHO), X(JJBAT),
     $              W(IRP), W(IRJ), W(IBRAS), W(IBJ), W(IRW), W(IRO),
     $              X(JJCEK), X(JJCIJ), W(IWATE), W(IORHO), W(IBR),
     $              X(JJAIJ), X(JJS), W(ITAUL), W(IRHWL), IW(NEDL),
     $              W(IDROL), IW(IFSL), IW(ILSL), W(IWSML), IX(JJKIJ),
     $              X(JJTRA), W(IT), W(IPTAU), W(IWW), W(IWT),
     $              W(ISTAR), W(IRX), W(IBU), W(IBQ), X(JJQHI),
     $              X(JJQST), X(JJAW), X(JJQSA), W(IEPL), W(IBSL),
     $              W(IBA), W(IBB), W(IBC), W(IXVAL), W(IWEIT),
     $              IW(IIMG), W(ICAU1), W(IABDR), W(IARHO))
C
C---- Write "iterative studies" file, if needed
      call BHUTAN  (N, X(JJZ), X(JJTE), X(JJXNU), W(IEPL), W(ITAUL),
     $              X(JJS), X(JJRHO), X(JJYBR), W(IRHWL))
C
C---- Update data in Line Intensity data blocks
      call VERVAIN (N, NT, XLB1, X(JJS), X(JJAW), X(JJYBR), X(JJRHO),
     $              X(JJQHI), W(IRHWL), W(IRO))
C
C---- (Give back W & IW allotments)
      call WGIVE   (W , 'LILY')
      call IGIVE   (IW, 'LILY')
C     !END
      call BYE ('LILY')
C
      return
      end
