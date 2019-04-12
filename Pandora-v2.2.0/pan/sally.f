      subroutine SALLY
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1970 Feb 10
C---- Supervises SETTUP: the rates calculation.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer ICHK, IDIJ, IDRCT, IFCK, IGMJ, IIMG, IN, IPIS, IQRK, IQRL,
     $        IS, ISQCA, ISQRA, ITRM, IW, IWS, IX, IXPBL, JJAAT, JJACE,
     $        JJACI, JJAIJ, JJAL, JJCEI, JJCIA, JJCII, JJCIJ, JJCK,
     $        JJCKA, JJCP, JJFCE, JJFCJ, JJGM, JJGVI, JJGVL, JJH2N,
     $        JJHEA, JJHIJ, JJHND, JJKIJ, JJLRQ, JJMCE, JJMCI, JJNK,
     $        JJNLE, JJNPQ, JJP, JJPIJ, JJQS, JJQU, JJRK, JJRL, JJSA,
     $        JJSIJ, JJSQS, JJTE, JJTER, JJTEX, JJTIJ, JJTR, JJVM,
     $        JJXCU, JJXNC, JJXND, JJXNE, JJXNU, JJZ, JJZT, JN, LU, LUD,
     $        LUG, LUM, LUP, LUS, LUX, MOX, MUX, N, NL, NSL, NTE, jummy
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
      equivalence (JZQ(20),NTE)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ( 27),JJP  )
      equivalence (IZOQ( 28),JJCP )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(235),JJSA )
      equivalence (IZOQ( 71),JJTEX)
      equivalence (IZOQ(  8),JJTR )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 16),JJGM )
      equivalence (IZOQ( 17),JJRK )
      equivalence (IZOQ( 31),JJCK )
      equivalence (IZOQ( 18),JJRL )
      equivalence (IZOQ( 19),JJQU )
      equivalence (IZOQ( 20),JJQS )
      equivalence (IZOQ( 79),JJTER)
      equivalence (IZOQ(166),JJMCI)
      equivalence (IZOQ( 29),JJCII)
      equivalence (IZOQ( 30),JJCEI)
      equivalence (IZOQ( 34),JJAL )
      equivalence (IZOQ(145),JJCIJ)
      equivalence (IZOQ(144),JJPIJ)
      equivalence (IZOQ(163),JJTIJ)
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ( 50),JJNK )
      equivalence (IZOQ(181),JJVM )
      equivalence (IZOQ(178),JJZT )
      equivalence (IZOQ(140),JJH2N)
      equivalence (IZOQ(175),JJGVL)
      equivalence (IZOQ(176),JJGVI)
      equivalence (IZOQ(208),JJCKA)
      equivalence (IZOQ(207),JJCIA)
      equivalence (IZOQ(219),JJHEA)
      equivalence (IZOQ(149),JJXNC)
      equivalence (IZOQ(223),JJSQS)
      equivalence (IZOQ(106),JJMCE)
      equivalence (IZOQ(107),JJACE)
      equivalence (IZOQ(248),JJFCE)
      equivalence (IZOQ(250),JJHIJ)
      equivalence (IZOQ(251),JJFCJ)
      equivalence (IZOQ(252),JJSIJ)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(204),JJACI)
      equivalence (IZOQ(262),JJAAT)
      equivalence (IZOQ(260),JJXCU)
      equivalence (IZOQ( 32),JJAIJ)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  2),JJKIJ)
      equivalence (JZOQ(  9),JJNPQ)
      equivalence (JZOQ( 10),JJLRQ)
      equivalence (JZOQ( 12),JJNLE)
C     !DASH
C     !EJECT
      external POPIO, ELPENOR, BAYUK, ULTAMI, CHARM, LATAKIA, SETTUP,
     $         LARA, IGIVE, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(10)
      equivalence
     $(IN( 1),ICHK  ),(IN( 2),ISQCA ),(IN( 3),IFCK  ),(IN( 4),IPIS  ),
     $(IN( 5),IGMJ  ),(IN( 6),ISQRA ),(IN( 7),IDRCT ),(IN( 8),ITRM  ),
     $(IN( 9),IXPBL ),(IN(10),IDIJ  )
C
      dimension JN(3)
      equivalence
     $(JN( 1),IQRK  ),(JN( 2),IQRL  ),(JN( 3),IIMG  )
C
      call HI ('SALLY')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call LARA    (IN, IS,  MOX, 'SALLY')
      call LATAKIA (JN, IWS, MUX, 'SALLY')
C     (Initialize populations buffer)
      call POPIO   ('INIT', jummy, W(IXPBL))
C---- Set up switch-arrays IQRK and IQRL
C     (such that IQRK(J)=1 means: RKI(J) must be computed, while
C     IQRK(J)=0 means: RKI(J) should not be recomputed;
C     and IQRL similarly for RLI.)
      call BAYUK   (X, IX, NSL, IW(IQRK), IW(IQRL))
C---- Set up LUNs for various printouts (based on NO or MO); print
C     header, and explanations
      call ELPENOR (LU, LUP, LUG, LUS, LUD, LUM, LUX)
C---- Compute Rates
      call SETTUP  (X, IX, W, IW, N, NSL, NL, LU, LUP, LUG, LUS, LUD,
     $              LUM, X(JJXNU), X(JJXCU), X(JJP), X(JJCP), X(JJTE),
     $              X(JJSA), X(JJTEX), X(JJTR), X(JJXNE), X(JJHND),
     $              IW(IQRK), IW(IQRL), X(JJGM), X(JJRK), X(JJCK),
     $              X(JJRL), X(JJQU), X(JJQS), X(JJSQS), X(JJTER),
     $              NTE, X(JJCII), X(JJCEI), W(ITRM), W(IGMJ),
     $              W(IPIS),X(JJAL), X(JJCIJ), X(JJPIJ), X(JJFCJ),
     $              W(IFCK), X(JJZ), X(JJXND), X(JJNK), X(JJVM),
     $              X(JJZT), X(JJH2N), X(JJGVL), X(JJGVI), X(JJCKA),
     $              X(JJCIA), X(JJHEA), X(JJXNC), W(ICHK), X(JJHIJ),
     $              X(JJTIJ), W(IDIJ) , X(JJMCI), X(JJACI), X(JJMCE),
     $              X(JJACE), W(IDRCT), X(JJFCE), X(JJAIJ), X(JJAAT),
     $              IX(JJKIJ), IX(JJNPQ), IX(JJLRQ), IX(JJNLE),
     $              X(IXPBL), IW(IIMG), X(JJSIJ))
C---- Exhibit dielectric recombination
      call CHARM   (N, NL, NSL, LU, X(JJGM), X(JJRL), X(JJCK), X(JJTE),
     $              X(JJSA), X(JJHND), X(JJXNE), W(ISQCA), W(ISQRA))
C---- Print trailer
      call ULTAMI  (LUX, 'RATES')
C     (Give back W & IW allotments)
      call WGIVE   (W,  'SALLY')
      call IGIVE   (IW, 'SALLY')
C     !END
      call BYE ('SALLY')
C
      return
      end
