      subroutine ATOM
     $(X,IX,W,IW,XLB1,KDDR,KDRX,KNZGM,IZERO,ARR,BRR,IRR,QAR,LINE,LU)
C
C     Rudolf Loeser, 1980 Dec 28
C---- Prints the parameters of the atomic model.
C     (This is version 2 of ATOM.)
C     !DASH
      save
C     !DASH
      real*8 ABD, AMASS, ARR, BRR, PMSK, PW, W, WNUK, X, XLB1, XNUK
      integer IBOC, ICHSW, ICKH, ICRD, ICRS, ICSK, ICVW, IDPC, IDPM,
     $        IFDB, IFLX, IGMA, IHSSW, IJL, IJU, IKIJ, IKL, IKU, ILDL,
     $        ILIJ, ILSFP, ILSFT, IN, IOLL, IOLN, IOML, IPCE, IPRD,
     $        IPXC, IPXP, IPXR, IQCEF, IQLSG, IQLSP, IQRKE, IRR, IS,
     $        ISBI, ISEM, IUIR, IW, IWAV, IWS, IWVN, IX, IYCO, IYLI,
     $        IZERO, JJAAT, JJACE, JJACI, JJAEW, JJAIJ, JJAL, JJCEI,
     $        JJCII, JJCP, JJLCH, JJLCX, JJLRQ, JJMCE, JJMCI, JJNLE,
     $        JJNPQ, JJOSF, JJP, JJRKX, JJTER, JJWCU, JJWNU, JJXCU,
     $        JJXNU, JJYK, JN, KDDR, KDRX, KNZGM, KSHEL, KTR, KXNUC,
     $        LSFGC, LU, MOX, MTR, MUX, NL, NSL, NTE
      logical HBC, LM1, LM3
      character LINE*120, QAR*10, QATOM*8, QELSM*8, QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(20),NTE)
      equivalence (JZQ(40),NSL)
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ(220),JJWNU)
      equivalence (IZOQ( 27),JJP  )
      equivalence (IZOQ( 28),JJCP )
      equivalence (IZOQ( 29),JJCII)
      equivalence (IZOQ( 72),JJYK )
      equivalence (IZOQ( 79),JJTER)
      equivalence (IZOQ( 34),JJAL )
      equivalence (IZOQ( 30),JJCEI)
      equivalence (IZOQ(224),JJOSF)
      equivalence (IZOQ(166),JJMCI)
      equivalence (IZOQ(204),JJACI)
      equivalence (IZOQ(106),JJMCE)
      equivalence (IZOQ(107),JJACE)
      equivalence (IZOQ(243),JJAEW)
      equivalence (IZOQ(  6),JJRKX)
      equivalence (IZOQ( 32),JJAIJ)
      equivalence (IZOQ(260),JJXCU)
      equivalence (IZOQ(261),JJWCU)
      equivalence (IZOQ(262),JJAAT)
C     !EJECT
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  9),JJNPQ)
      equivalence (JZOQ( 10),JJLRQ)
      equivalence (JZOQ( 11),JJLCX)
      equivalence (JZOQ( 12),JJNLE)
      equivalence (JZOQ( 17),JJLCH)
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
      equivalence (RZQ(  4),AMASS)
      equivalence (RZQ(  6),ABD  )
      equivalence (QZQ(  1),QNAME)
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ(122),ICHSW)
      equivalence (RZQ(  9),XNUK )
      equivalence (RZQ(119),WNUK )
      equivalence (QZQ(  4),QATOM)
      equivalence (KZQ(108),LSFGC)
      equivalence (KZQ(123),IHSSW)
      equivalence (RZQ(124),PMSK )
      equivalence (RZQ(  7),PW   )
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(287),IQLSP)
      equivalence (IQQ(250),IQLSG)
      equivalence (IQQ(328),IQCEF)
      equivalence (IQQ(334),IQRKE)
C     !EJECT
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 1),KSHEL)
      equivalence (LEST(82),KXNUC)
C     !DASH
      external INNER, ZERO1, GECKO, PRIAM, SKINK, MELFI, LIZARD, EERIE,
     $         NARAVO, OWEN, ZEROI, ALI, TOAD, WGIVE, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XLB1(Li1len), ARR(8), BRR(8), IRR(8), QAR(16)
      dimension XLB1(*),      ARR(*), BRR(*), IRR(*), QAR(*)
C
      dimension IN(29)
      equivalence
     $(IN (1),IBOC ),(IN( 2),ICKH ),(IN( 3),IWVN ),(IN( 4),IWAV ),
     $(IN( 5),ICRD ),(IN( 6),ICVW ),(IN( 7),ICSK ),(IN( 8),ICRS ),
     $(IN( 9),IYLI ),(IN(10),IYCO ),(IN(11),ISEM ),(IN(12),IOLN ),
     $(IN(13),IPRD ),(IN(14),IDPC ),(IN(15),ILSFT),(IN(16),IGMA ),
     $(IN(17),IPCE ),(IN(18),IOML ),(IN(19),IUIR ),(IN(20),ISBI ),
     $(IN(21),IFLX ),(IN(22),ILDL ),(IN(23),ILSFP),(IN(24),IFDB ),
     $(IN(25),IOLL ),(IN(26),IDPM ),(IN(27),IPXC ),(IN(28),IPXP ),
     $(IN(29),IPXR )
C
      dimension JN(6)
      equivalence
     $(JN( 1),IJU  ),(JN( 2),IJL  ),(JN( 3),IKU  ),(JN( 4),IKL  ),
     $(JN( 5),IKIJ ),(JN( 6),ILIJ )
C
      call HI ('ATOM')
C     !BEG
      if(LU.gt.0) then
C       (Get, and allocate, W & IW allotments)
        call INNER (IN, IS,  MOX, 'ATOM')
        call MELFI (JN, IWS, MUX, 'ATOM')
C
C----   KEY STEP: set all temporary data arrays = 0!
        call ZERO1 (W(IS),      (MOX-IS +1))
        call ZEROI (IW(IWS), 1, (MUX-IWS+1))
C     !EJECT
C----   Get transition data.
        call GECKO  (X, IX, XLB1, W(ICRD), W(ICVW), W(ICSK), W(ICRS),
     $               W(IYLI), W(ISEM), W(IOLN), W(IPRD), W(IDPC),
     $               W(ILSFT), W(IGMA), W(IYCO), W(IOML), MTR,
     $               IW(IJU), IW(IJL), KTR, IW(IKU), IW(IKL), KDDR,
     $               KDRX, KNZGM, W(IWAV), W(IUIR), IW(IKIJ),
     $               W(IFLX), W(ILDL), W(ILSFP), W(IFDB), W(IOLL),
     $               W(ISBI), W(IWVN), W(ICKH), W(IBOC), IW(ILIJ),
     $               W(IDPM), W(IPXC), W(IPXP), W(IPXR), W(IPCE))
C----   Write heading.
        call PRIAM  (LU, 'ATOM', 4)
C----   Write miscellaneous data.
        call SKINK  (AMASS, ABD, QNAME, QELSM, QATOM, LINE, IZERO, LU)
C----   Write level data.
        call OWEN   (LU)
        call NARAVO (LU,NL,NSL)
        call EERIE  (IX(JJLRQ), NSL, LM1, LM3)
        call TOAD   (XNUK, X(JJXNU), X(JJP), X(JJCP), X(JJCII),
     $               X(JJMCI), X(JJACI), X(JJYK), NTE, X(JJTER),
     $               X(JJAL), NSL, NL, KSHEL, IX(JJNPQ), IX(JJLRQ),
     $               IX(JJNLE), IX(JJLCX), WNUK, X(JJWNU), X(JJAEW),
     $               X(JJRKX), IX(JJLCH), ICHSW, IQRKE, X(JJXCU),
     $               X(JJWCU), KXNUC, LM1, LM3, ARR, BRR, IRR, QAR,
     $               LINE, IZERO, LU)
C----   Print "Run-to-Pop" indices.
        call ALI    (LU)
C----   Write transition data.
        call OWEN   (LU)
        HBC = (QELSM.eq.'H  ').and.(IHSSW.eq.1)
        call LIZARD (NL, NSL, X(JJAIJ), X(JJAAT), X(JJCEI), NTE,
     $               X(JJTER), IW(IKIJ), PMSK, PW, IW(IJU), IW(IJL),
     $               MTR, IW(IKU), IW(IKL), KTR, W(ICRD), W(ICVW),
     $               W(ICSK), W(ICRS), W(IYLI), W(IYCO), W(ISEM),
     $               W(IOLN), W(IPRD), W(IDPC), W(IGMA), W(IOML),
     $               W(IOLL), W(IWAV), W(IUIR), W(IFLX), W(ILSFT),
     $               W(ILDL), W(ILSFP), W(IFDB), W(ISBI), W(IWVN),
     $               IQLSP, IQLSG, LSFGC, HBC, W(ICKH), X(JJOSF),
     $               W(IBOC), W(IDPM), W(IPXC), W(IPXP), W(IPXR),
     $               IW(ILIJ), X(JJMCE), X(JJACE), W(IPCE), IX(JJLCH),
     $               IQCEF, ICHSW, ARR, BRR, QAR, LINE, IZERO, LU)
C
C       (Give back W & IW allotments)
        call WGIVE  (W,  'ATOM')
        call IGIVE  (IW, 'ATOM')
      end if
C     !END
      call BYE ('ATOM')
C
      return
      end
