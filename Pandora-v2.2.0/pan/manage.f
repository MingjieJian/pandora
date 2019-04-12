      subroutine MANAGE
     $(IN,IS,MUX)
C
C     Rudolf Loeser, 28 Mar 68
C---- Allocates the floating point general data block.
C     !DASH
      save
C     !DASH
      integer IN, INC, INCS, INK, IQCXU, IQJLY, IQSFS, IS, JJBN, JM,
     $        JXNCS, KB, KBX, KEK, KF, KK, KM, KNC, KNCL, KNW, KR, KS,
     $        KSHL, KWA, KWC, L, LDU, LF, LFL, LG, LLY, LOM, LZM, M,
     $        MHM, MLR, MLS, MMM, MMMP, MMR, MQT, MRR, MRS, MRX, MSHL,
     $        MUL, MUX, N, NAB, NCB, NCL, NCP, NCQ, NCR, NDAH, NDR,
     $        NDSK, NDT, NDV, NFH, NFL, NGM, NKA, NL, NL2, NLB, NLM,
     $        NLN, NLQ, NNL2, NRM, NSHL, NSL, NSLP, NSW, NT, NTE, NVF,
     $        NVH, NVX, NWS, NWV, NXC1, NXC2, NXC3, NXC4, NXF, NZE, NZM
      character QELSM*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
C     .
      equivalence
     $(JZQ( 1),N  ),(JZQ( 2),NL ),(JZQ( 3),M  ),(JZQ( 4),KF ),
     $(JZQ( 5),NT ),(JZQ( 6),NSW),(JZQ( 7),L  ),(JZQ( 8),NLB),
     $(JZQ( 9),NFB),(JZQ(10),KK ),(JZQ(11),KBX),(JZQ(12),KKX),
     $(JZQ(13),LZM),(JZQ(14),NZM),(JZQ(15),MRR),(JZQ(16),NFL),
     $(JZQ(17),NWV),(JZQ(18),MMR),(JZQ(19),LF ),(JZQ(20),NTE),
     $(JZQ(21),NDT),(JZQ(22),MHM),(JZQ(23),NWS),(JZQ(24),JM ),
     $(JZQ(25),KNW),(JZQ(26),LDU),(JZQ(27),LLY),(JZQ(28),MLR),
     $(JZQ(29),MRS),(JZQ(30),MRX),(JZQ(31),NFH),(JZQ(32),NCR),
     $(JZQ(33),MLS),(JZQ(34),LG ),(JZQ(35),INK),(JZQ(36),KS ),
     $(JZQ(37),KR ),(JZQ(38),KB ),(JZQ(39),MQT),(JZQ(40),NSL),
     $(JZQ(41),NDR),(JZQ(42),NVX),(JZQ(43),NDV),(JZQ(44),NCP),
     $(JZQ(45),NAB),(JZQ(46),KWC),(JZQ(47),NVF),(JZQ(48),NXF),
     $(JZQ(49),KM ),(JZQ(50),NKA),(JZQ(51),NCL),(JZQ(52),NLN),
     $(JZQ(53),NCQ),(JZQ(54),NVH),(JZQ(55),NCB),(JZQ(56),NZE),
     $(JZQ(57),KWA),(JZQ(58),NGM)
C     .
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(189),JXNCS)
      equivalence (QZQ(  2),QELSM)
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
      equivalence (LEST( 4),NSHL )
C
C---- ELEMENT     as of 1998 Aug 17
      integer     NELX
      parameter   (NELX=50)
C     (Remember to recompile all users when changing NELX)
      real*8      ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      integer     LATNO,LDEFR,NMT,NMTMAX
      logical     LATEM
      character   ELSYM*3, ELSUB*3
      dimension   ELSYM(NELX),ELSUB(NELX),ELABD(NELX),ELCHI(NELX),
     $            ELLU1(NELX),ELLU2(NELX),ELABL(NELX),ELDEF(NELX),
     $            LATNO(NELX),LDEFR(NELX),LATEM(NELX)
C
      common      /ELEMNT0/ NMT,NMTMAX
      common      /ELEMNT1/ ELSYM,ELSUB
      common      /ELEMNT2/ ELABD,ELCHI,ELLU1,ELLU2,ELABL,ELDEF
      common      /ELEMNT3/ LATNO,LDEFR
      common      /ELEMNT4/ LATEM
C
C     Element data tables:
C             ELSYM - element symbol;
C             ELSUB - (Scratch storage for I.D. symbols);
C             ELABD - abundance (w.r.t. Hydrogen);
C             ELCHI - Chi, i.e. ionization potential;
C             ELLU1 - U-I partition function;
C             ELLU2 - U-II partition function;
C             ELABL - logarithmic abundance;
C             ELDEF - defaults values of logarithmic abundance;
C             LATNO - atomic number; and
C             LDEFR - default values sources codes.
C             LATEM - "metal" designator
C     .
C     !EJECT
C---- XINGU       as of 1999 Sep 21
      real*8      AXED,BXED,RCHX,DELCHX
      character   NAMXED*3
      integer     NXI,NPQLM,NPQMX
      parameter   (NXI=10)
C     (Remember to change all users when changing NXI)
      parameter   (NPQLM=15)
C     (Maximum permitted value of principal quantum number n)
C     (NPQLM must not exceed LIMDAT(1) [in popdata.inc], the
C     number of levels in the Hydrogen population ion model.)
      dimension   AXED(NXI), BXED(NXI), NAMXED(NXI)
      dimension   RCHX(NPQLM,NPQLM), DELCHX(NPQLM,NPQLM)
      common      /XINGU1/ AXED,BXED,RCHX,DELCHX
      common      /XINGU2/ NAMXED
      common      /XINGU3/ NPQMX
C---- Charge Exchange data tables
C     .
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
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ(215),IQJLY)
      equivalence (IQQ(331),IQCXU)
C     !EJECT
C---- POPDATA     as of 2007 Jan 12
      integer     NPI
      parameter   (NPI=14)
C     (Remember to recompile all users when changing NPI.)
      real*8      POPMSS
      integer     LZOQ,MRTP,NPOPS,MAXPOPL,LENPBL,MRTPA,MRTPM,LIMPOP,
     $            LENT,NAMKNT,LENPOP,ICKSM,IUPOP,IBLAD,IPSWICH,KAPNO
      character   NAMES*10,TNAMES*8,POPSYM*3,KLABPI*8,NLABPI*8,BLABPI*8
      dimension   LZOQ(5), MRTP(50),
     $            LIMPOP(NPI), NAMKNT(NPI), LENPOP(NPI), IBLAD(NPI),
     $            ICKSM(NPI),  IUPOP(NPI),  NAMES(NPI),  IPSWICH(NPI),
     $            POPSYM(NPI), KAPNO(NPI),  POPMSS(NPI), TNAMES(NPI),
     $            KLABPI(NPI), NLABPI(NPI), BLABPI(NPI)
C
      common      /POPS01/ NPOPS,MAXPOPL,LENT,LENPBL,MRTPM,MRTPA,ICKSM
      common      /POPS02/ POPMSS
      common      /POPS03/ LZOQ
      common      /POPS04/ MRTP
      common      /POPS05/ LENPOP
      common      /POPS06/ LIMPOP
      common      /POPS07/ NAMES
      common      /POPS08/ TNAMES
      common      /POPS09/ NAMKNT
      common      /POPS10/ IUPOP
      common      /POPS11/ IBLAD
      common      /POPS12/ IPSWICH
      common      /POPS13/ POPSYM
      common      /POPS14/ KAPNO
      common      /POPS15/ KLABPI
      common      /POPS16/ NLABPI
      common      /POPS17/ BLABPI
C
C     Population Data Blocks parameters and data.
C     !DASH
C     !EJECT
      external  HI, BYE
      intrinsic max
C
      dimension IN(*)
C
      call HI ('MANAGE')
C     !BEG
      MMM  = MRS+NSL
      LOM  = MLS+NL
      NL2  = NL*NL
      INC  = N*NL
      INCS = N*NSL
      NLM  = NL-1
      MUL  = (NL*NLM)/2
      KNC  = N*MUL
      KNCL = NT*N
      NLQ  = max(MAXPOPL,NL)
      NNL2 = INC*NL
      MMMP = MMM+1
      NSLP = NSL+1
      LFL  = max(LF,L)
      if(NL.le.0) then
        KEK = 0
      else
        KEK = (NL-2)*N
      end if
      if((JXNCS.gt.0).and.(QELSM(:3).eq.'H  ')) then
        NDAH = KNC
      else
        NDAH = 0
      end if
      if(IQSFS.le.0) then
        KSHL = 0
        MSHL = 0
        NDSK = 0
      else
        NRM  = 2*N+5
        KSHL = NSHL*NRM
        MSHL = NSHL*N
        NDSK = MRR*N
      end if
      if(IQJLY.le.0) then
        JJBN = 0
      else
        JJBN = N*KK
      end if
      if(IQCXU.le.0) then
        NXC3 = 0
      else
        NXC3 = INC
      end if
      NXC2 = N*NPQLM
      NXC1 = NXC2*NXI
      NXC4 = NPQLM*NPQLM
C     !EJECT
      IN(  1) = IS
C
      IN(  2) = IN(  1)+MQT
      IN(  3) = IN(  2)+N
      IN(  4) = IN(  3)+NT
      IN(  5) = IN(  4)+LFL
      IN(  6) = IN(  5)+N
      IN(  7) = IN(  6)+NSL
      IN(  8) = IN(  7)+N
      IN(  9) = IN(  8)+INCS
      IN( 10) = IN(  9)+N
      IN( 11) = IN( 10)+NFH
C
      IN( 12) = IN( 11)+N
      IN( 13) = IN( 12)+N
      IN( 14) = IN( 13)+KNCL
      IN( 15) = IN( 14)+INK
      IN( 16) = IN( 15)+INK
      IN( 17) = IN( 16)+INCS
      IN( 18) = IN( 17)+INCS
      IN( 19) = IN( 18)+INCS
      IN( 20) = IN( 19)+INCS
      IN( 21) = IN( 20)+INCS
C
      IN( 22) = IN( 21)+NFH
      IN( 23) = IN( 22)+M
      IN( 24) = IN( 23)+INC
      IN( 25) = IN( 24)+NT*MUL
      IN( 26) = IN( 25)+MUL
      IN( 27) = IN( 26)+NSL
      IN( 28) = IN( 27)+NSL
      IN( 29) = IN( 28)+NSLP
      IN( 30) = IN( 29)+NSL*NTE
      IN( 31) = IN( 30)+MUL*NTE
C
      IN( 32) = IN( 31)+INCS
      IN( 33) = IN( 32)+NL2
      IN( 34) = IN( 33)+N
      IN( 35) = IN( 34)+NL
      IN( 36) = IN( 35)+N
      IN( 37) = IN( 36)+KEK
      IN( 38) = IN( 37)+N
      IN( 39) = IN( 38)+N
      IN( 40) = IN( 39)+KNC
      IN( 41) = IN( 40)+KSHL
C
      IN( 42) = IN( 41)+NVH
      IN( 43) = IN( 42)+N
      IN( 44) = IN( 43)+KNC
      IN( 45) = IN( 44)+INC
      IN( 46) = IN( 45)+KNCL
      IN( 47) = IN( 46)+NDR
      IN( 48) = IN( 47)+NDR
      IN( 49) = IN( 48)+N
      IN( 50) = IN( 49)+N
      IN( 51) = IN( 50)+N
C
      IN( 52) = IN( 51)+N
      IN( 53) = IN( 52)+N
      IN( 54) = IN( 53)+N
      IN( 55) = IN( 54)+N
      IN( 56) = IN( 55)+N
      IN( 57) = IN( 56)+N
      IN( 58) = IN( 57)+N
      IN( 59) = IN( 58)+N
      IN( 60) = IN( 59)+INC
      IN( 61) = IN( 60)+MSHL
C
      IN( 62) = IN( 61)+KK
      IN( 63) = IN( 62)+N
      IN( 64) = IN( 63)+N
      IN( 65) = IN( 64)+KNW
      IN( 66) = IN( 65)+NWV
      IN( 67) = IN( 66)+INCS
      IN( 68) = IN( 67)+INCS
      IN( 69) = IN( 68)+N
      IN( 70) = IN( 69)+MMMP
      IN( 71) = IN( 70)+MMMP
C
      IN( 72) = IN( 71)+N
      IN( 73) = IN( 72)+NSL
      IN( 74) = IN( 73)+N
      IN( 75) = IN( 74)+LOM
      IN( 76) = IN( 75)+N
      IN( 77) = IN( 76)+NT
      IN( 78) = IN( 77)+MMMP
      IN( 79) = IN( 78)+NWV
      IN( 80) = IN( 79)+NTE
      IN( 81) = IN( 80)+N
C
      IN( 82) = IN( 81)+KNCL
      IN( 83) = IN( 82)+N
      IN( 84) = IN( 83)+N
      IN( 85) = IN( 84)+MHM
      IN( 86) = IN( 85)+MHM
      IN( 87) = IN( 86)+N
      IN( 88) = IN( 87)+MHM
      IN( 89) = IN( 88)+N
      IN( 90) = IN( 89)+NDT
      IN( 91) = IN( 90)+NDT
C
      IN( 92) = IN( 91)+NDT
      IN( 93) = IN( 92)+NDT
      IN( 94) = IN( 93)+JM
      IN( 95) = IN( 94)+JM
      IN( 96) = IN( 95)+M**2
      IN( 97) = IN( 96)+LDU
      IN( 98) = IN( 97)+LDU
      IN( 99) = IN( 98)+LDU
      IN(100) = IN( 99)+LDU
      IN(101) = IN(100)+LLY
C
      IN(102) = IN(101)+LLY
      IN(103) = IN(102)+LOM
      IN(104) = IN(103)+LOM
      IN(105) = IN(104)+N
      IN(106) = IN(105)+N
      IN(107) = IN(106)+MUL
      IN(108) = IN(107)+MUL
      IN(109) = IN(108)+N
      IN(110) = IN(109)+MSHL
      IN(111) = IN(110)+N
C
      IN(112) = IN(111)+N
      IN(113) = IN(112)+KK
      IN(114) = IN(113)+N
      IN(115) = IN(114)+N
      IN(116) = IN(115)+LFL
      IN(117) = IN(116)+NCR
      IN(118) = IN(117)+NCR
      IN(119) = IN(118)+NCR
      IN(120) = IN(119)+N
      IN(121) = IN(120)+NVX
C
      IN(122) = IN(121)+KF
      IN(123) = IN(122)+KF
      IN(124) = IN(123)+KK
      IN(125) = IN(124)+N
      IN(126) = IN(125)+N
      IN(127) = IN(126)+N
      IN(128) = IN(127)+KNW
      IN(129) = IN(128)+N*KNW
      IN(130) = IN(129)+N
      IN(131) = IN(130)+LG
C
      IN(132) = IN(131)+LG
      IN(133) = IN(132)+NDSK
      IN(134) = IN(133)+NDSK
      IN(135) = IN(134)+NDSK
      IN(136) = IN(135)+MRR
      IN(137) = IN(136)+N
      IN(138) = IN(137)+N*NLQ
      IN(139) = IN(138)+N
      IN(140) = IN(139)+N
      IN(141) = IN(140)+N
C
      IN(142) = IN(141)+N
      IN(143) = IN(142)+N*NMT
      IN(144) = IN(143)+NMT
      IN(145) = IN(144)+NNL2
      IN(146) = IN(145)+NNL2
      IN(147) = IN(146)+KS
      IN(148) = IN(147)+KS
      IN(149) = IN(148)+KR
      IN(150) = IN(149)+N
      IN(151) = IN(150)+KB
C
      IN(152) = IN(151)+N
      IN(153) = IN(152)+NDV
      IN(154) = IN(153)+NSHL
      IN(155) = IN(154)+N
      IN(156) = IN(155)+N
      IN(157) = IN(156)+N
      IN(158) = IN(157)+NCP
      IN(159) = IN(158)+NCP*N
      IN(160) = IN(159)+NCP
      IN(161) = IN(160)+NAB
C
      IN(162) = IN(161)+NAB
      IN(163) = IN(162)+NAB
      IN(164) = IN(163)+NNL2
      IN(165) = IN(164)+N*NVX
      IN(166) = IN(165)+N
      IN(167) = IN(166)+NSL
      IN(168) = IN(167)+LG
      IN(169) = IN(168)+NDSK
      IN(170) = IN(169)+MSHL
      IN(171) = IN(170)+NKA
C
      IN(172) = IN(171)+NKA
      IN(173) = IN(172)+N
      IN(174) = IN(173)+N
      IN(175) = IN(174)+JJBN
      IN(176) = IN(175)+INC
      IN(177) = IN(176)+N
      IN(178) = IN(177)+N
      IN(179) = IN(178)+N
      IN(180) = IN(179)+N
      IN(181) = IN(180)+N
C
      IN(182) = IN(181)+N
      IN(183) = IN(182)+NT
      IN(184) = IN(183)+NCL
      IN(185) = IN(184)+N
      IN(186) = IN(185)+NWS
      IN(187) = IN(186)+NVX
      IN(188) = IN(187)+N
      IN(189) = IN(188)+N
      IN(190) = IN(189)+N
      IN(191) = IN(190)+N
C
      IN(192) = IN(191)+N**2
      IN(193) = IN(192)+N
      IN(194) = IN(193)+N
      IN(195) = IN(194)+N
      IN(196) = IN(195)+N
      IN(197) = IN(196)+N
      IN(198) = IN(197)+N
      IN(199) = IN(198)+N
      IN(200) = IN(199)+N
      IN(201) = IN(200)+N
C
      IN(202) = IN(201)+N
      IN(203) = IN(202)+N
      IN(204) = IN(203)+N
      IN(205) = IN(204)+NSL
      IN(206) = IN(205)+N
      IN(207) = IN(206)+NVH
      IN(208) = IN(207)+NNL2
      IN(209) = IN(208)+INCS
      IN(210) = IN(209)+N
      IN(211) = IN(210)+N
C
      IN(212) = IN(211)+N
      IN(213) = IN(212)+NXC1
      IN(214) = IN(213)+NXC1
      IN(215) = IN(214)+NXC2
      IN(216) = IN(215)+NXC2
      IN(217) = IN(216)+NXC3
      IN(218) = IN(217)+NXC3
      IN(219) = IN(218)+NXC4
      IN(220) = IN(219)+N
      IN(221) = IN(220)+NSL
C
      IN(222) = IN(221)+NCB
      IN(223) = IN(222)+NCB
      IN(224) = IN(223)+N
      IN(225) = IN(224)+MUL
      IN(226) = IN(225)+N
      IN(227) = IN(226)+NZE
      IN(228) = IN(227)+KWA
      IN(229) = IN(228)+KWA*N
      IN(230) = IN(229)+LFL
      IN(231) = IN(230)+NGM
C
      IN(232) = IN(231)+N
      IN(233) = IN(232)+NCQ
      IN(234) = IN(233)+NCQ
      IN(235) = IN(234)+N
      IN(236) = IN(235)+N
      IN(237) = IN(236)+N
      IN(238) = IN(237)+N
      IN(239) = IN(238)+N
      IN(240) = IN(239)+NSW
      IN(241) = IN(240)+NDAH
C
      IN(242) = IN(241)+KNCL
      IN(243) = IN(242)+KNCL
      IN(244) = IN(243)+NSL
      IN(245) = IN(244)+MMMP
      IN(246) = IN(245)+KNCL
      IN(247) = IN(246)+KNCL
      IN(248) = IN(247)+KNCL
      IN(249) = IN(248)+KNCL
      IN(250) = IN(249)+NT
      IN(251) = IN(250)+NNL2
C
      IN(252) = IN(251)+INC
      IN(253) = IN(252)+NNL2
      IN(254) = IN(253)+KNC
      IN(255) = IN(254)+KBX
      IN(256) = IN(255)+NFL
      IN(257) = IN(256)+NFL
      IN(258) = IN(257)+NFL
      IN(259) = IN(258)+NFL
      IN(260) = IN(259)+NLN
      IN(261) = IN(260)+NSL
C
      IN(262) = IN(261)+NSL
      IN(263) = IN(262)+NL2
      IN(264) = IN(263)+NGM
      IN(265) = IN(264)+NT
      IN(266) = IN(265)+N
      IN(267) = IN(266)+N
      IN(268) = IN(267)+N
      IN(269) = IN(268)+N
      MUX     = IN(269)+N
C     !END
      call BYE ('MANAGE')
C
      return
      end
