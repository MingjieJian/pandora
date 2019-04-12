      subroutine DABBLE
     $(X,IX,W,IW,NO)
C
C     Rudolf Loeser, 1969 Apr 07
C---- Prints the input.
C     !DASH
      save
C     !DASH
      real*8 ARR, ASMCR, BRR, CCHX, CDZ, CGR, CHLIM, CHOP, CLOGG, COMU,
     $       CPRSS, CSDW, CTCO, CTMX, CVSB, CVXF, CVXM, CVXS, CVZ, CWJ,
     $       CWR, DDT, DLU, EPTAU, FABD, FBVMX, FMCDL, FMVLM, FRCDL,
     $       FSTKM, HSBFQ, HSBM, HSBMN, HSBMX, OPF, PMSK, PRTLM, PZERO,
     $       R1N, REFLM, RFMAS, SHCOC, SHCOP, SMP, SRCO, TDUST, TLTR,
     $       TX, W, WAVMN, WAVMX, WBD, WFB, WMN, WMX, WNJNK, WPOP, WTD,
     $       WTPZ, X, XCOMX, XINCH, XKDST, XLCOD, XLMCR, XLMH, XLMZ,
     $       XMCOA, YCOL, YFLUX, YH, ZNDW, ZRCO
      integer IBNVW, ICHSW, ICXDP, IDGMZ, IFALL, IHSDD, IHSDP, IHSKM,
     $        IHSLT, IHSSM, IHSSP, IHSSW, ILI, IN, INK, IOMX, IPZER,
     $        IQADA, IQAMP, IQATP, IQCXD, IQGDS, IQHMO, IQHMS, IQHSV,
     $        IQINC, IQINP, IQLNU, IQPGA, IQPPF, IQPZT, IQRSQ, IQSRJ,
     $        IQSTW, IQUVP, IRR, IRTIS, IS, ISUB, IVNH, IVOIT, IW,
     $        IWEIT, IX, IXASM, IXLB1, J304I, JATMO, JATOM, JBD, JBDNC,
     $        JDMCE, JDMCI, JEDIT, JH1, JH2, JHLSK, JJ304, JJABD, JJABK,
     $        JJADT, JJAEL, JJAHM, JJALD, JJBHM, JJBHZ, JJBNL, JJBNU,
     $        JJBNY, JJBXI, JJCHI, JJCIA, JJCKA, JJCOL, JJCQA, JJCQT,
     $        JJCVX, JJDDR, JJDFD, JJDGM, JJDTE, JJDWV, JJEP1, JJEP2,
     $        JJEPD, JJEQT, JJFIN, JJFIW, JJFMV, JJFNA, JJFNB, JJFNH,
     $        JJFRR, JJFRS, JJGK, JJGMZ, JJH2N, JJHEA, JJHJ, JJHND,
     $        JJHNF, JJHNV, JJIBE, JJICR, JJISV, JJLCR, JJLDR, JJLDT,
     $        JJLHM, JJLMD, JJLMM, JJLRJ, JJLXX, JJMLC, JJMRJ, JJMSI,
     $        JJMSR, JJMU, JJMUF, JJPAB, JJPBA, JJPBG, JJPF, JJPGB,
     $        JJQIN, JJR1W, JJRAB, JJRBL, JJRK, JJRKC, JJRKH, JJRL,
     $        JJRLH, JJRML, JJRRC, JJRRN, JJRZM, JJSCW, JJSWV, JJTDN,
     $        JJTE, JJTEX, JJTKI, JJTKR, JJTR, JJV, JJVBM, JJVM, JJVNH,
     $        JJVR, JJVSB, JJVT, JJVXN, JJVXS, JJWAV, JJWEI, JJWLA,
     $        JJWLB, JJWRA, JJWTP, JJXCA, JJXCB, JJXDR, JJXIN, JJXK,
     $        JJXNC, JJXNE, JJXNU, JJXRK, JJXRL, JJYCR, JJYDT, JJYHM,
     $        JJYKR, JJYLM, JJYRA, JJYWA, JJZ, JJZBK, JJZGM, JJZME,
     $        JJZT, JM, JNEDP, JRHO, JSTCN, KAMB, KBX, KDDR, KDRX,
     $        KMASN, KNZGM, KPRSW, KSHEL, KTKIN, KVLG, KWA, KWC, LCOW,
     $        LDLMX, LDU, LGGIN, LLY, LYMIT, M304, MBREC, MCXK, MDTR1,
     $        MDTR2, MFMV, MHM, MOX, MS, MSEDG, MSEDW, MSFGR, MSFQM,
     $        MSFQR, MSFRT, MTHEI, N, NAB, NBS, NCB, NCL, NCP, NCQ, NCR,
     $        NDR, NDT, NDV, NDW, NDWM, NECLP, NERM, NFL, NGM, NH2CS,
     $        NIASM, NIL, NKA, NL, NLY, NO, NOION, NQLYM, NS, NSL, NSW,
     $        NVH, NVSB, NVX, NWS, NWV
      logical ION
      character LINE*120, QAR*10, QELSM*8
C     !COM
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(11),KBX)
      equivalence (JZQ(41),NDR)
      equivalence (JZQ(42),NVX)
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(40),NSL)
      equivalence (JZQ(26),LDU)
      equivalence (JZQ(21),NDT)
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(17),NWV)
      equivalence (JZQ(24),JM )
      equivalence (JZQ(35),INK)
      equivalence (JZQ(32),NCR)
      equivalence (JZQ(45),NAB)
      equivalence (JZQ(46),KWC)
      equivalence (JZQ(44),NCP)
      equivalence (JZQ(57),KWA)
      equivalence (JZQ(22),MHM)
      equivalence (JZQ(50),NKA)
      equivalence (JZQ(51),NCL)
      equivalence (JZQ(23),NWS)
      equivalence (JZQ(54),NVH)
      equivalence (JZQ(55),NCB)
      equivalence (JZQ(53),NCQ)
      equivalence (JZQ( 6),NSW)
      equivalence (JZQ(43),NDV)
      equivalence (JZQ(27),LLY)
      equivalence (JZQ(16),NFL)
      equivalence (JZQ(58),NGM)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ( 53),JJRAB)
      equivalence (IZOQ(189),JJRBL)
      equivalence (IZOQ( 12),JJV  )
      equivalence (IZOQ( 49),JJVT )
      equivalence (IZOQ( 52),JJRZM)
      equivalence (IZOQ(  8),JJTR )
      equivalence (IZOQ(183),JJCOL)
      equivalence (IZOQ( 80),JJAEL)
      equivalence (IZOQ( 86),JJBHM)
      equivalence (IZOQ(  5),JJ304)
      equivalence (IZOQ( 33),JJMSI)
      equivalence (IZOQ( 38),JJMSR)
      equivalence (IZOQ( 71),JJTEX)
      equivalence (IZOQ( 75),JJTKI)
      equivalence (IZOQ( 82),JJQIN)
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ(136),JJHJ )
      equivalence (IZOQ(138),JJFRS)
      equivalence (IZOQ(140),JJH2N)
      equivalence (IZOQ(141),JJPF )
      equivalence (IZOQ(156),JJRML)
      equivalence (IZOQ(165),JJVR )
      equivalence (IZOQ( 96),JJLMD)
      equivalence (IZOQ( 97),JJDFD)
      equivalence (IZOQ( 98),JJALD)
      equivalence (IZOQ( 99),JJEPD)
      equivalence (IZOQ( 90),JJLDT)
      equivalence (IZOQ( 89),JJYDT)
      equivalence (IZOQ( 91),JJADT)
      equivalence (IZOQ( 92),JJABD)
      equivalence (IZOQ( 62),JJTDN)
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ( 69),JJRRN)
      equivalence (IZOQ( 70),JJRRC)
      equivalence (IZOQ( 77),JJYRA)
      equivalence (IZOQ( 74),JJRKC)
      equivalence (IZOQ(102),JJTKR)
      equivalence (IZOQ(103),JJYKR)
      equivalence (IZOQ( 65),JJWAV)
      equivalence (IZOQ( 78),JJYWA)
      equivalence (IZOQ( 17),JJRK )
      equivalence (IZOQ( 18),JJRL )
      equivalence (IZOQ(105),JJEP1)
      equivalence (IZOQ(  2),JJEP2)
      equivalence (IZOQ(108),JJR1W)
      equivalence (IZOQ(  1),JJEQT)
      equivalence (IZOQ(123),JJXK )
      equivalence (IZOQ( 61),JJYLM)
      equivalence (IZOQ(112),JJGK )
      equivalence (IZOQ( 93),JJLMM)
      equivalence (IZOQ( 94),JJMLC)
      equivalence (IZOQ( 14),JJXIN)
      equivalence (IZOQ( 15),JJFIN)
      equivalence (IZOQ(116),JJLCR)
      equivalence (IZOQ(117),JJICR)
      equivalence (IZOQ(118),JJYCR)
      equivalence (IZOQ(100),JJLXX)
      equivalence (IZOQ(101),JJLDR)
      equivalence (IZOQ(160),JJBNL)
      equivalence (IZOQ(161),JJBNU)
      equivalence (IZOQ(162),JJBNY)
      equivalence (IZOQ( 46),JJXDR)
      equivalence (IZOQ(152),JJDWV)
      equivalence (IZOQ(244),JJWRA)
      equivalence (IZOQ(254),JJBXI)
      equivalence (IZOQ( 47),JJDDR)
      equivalence (IZOQ( 21),JJFNH)
      equivalence (IZOQ(234),JJDTE)
      equivalence (IZOQ(143),JJCHI)
      equivalence (IZOQ( 24),JJWEI)
      equivalence (IZOQ(135),JJFRR)
      equivalence (IZOQ( 84),JJLHM)
      equivalence (IZOQ( 85),JJAHM)
      equivalence (IZOQ( 87),JJYHM)
      equivalence (IZOQ(170),JJZBK)
      equivalence (IZOQ(171),JJABK)
      equivalence (IZOQ(  4),JJMU )
      equivalence (IZOQ(115),JJMUF)
      equivalence (IZOQ(172),JJVSB)
      equivalence (IZOQ(181),JJVM )
      equivalence (IZOQ(196),JJVBM)
      equivalence (IZOQ(185),JJSWV)
      equivalence (IZOQ(206),JJHNV)
      equivalence (IZOQ( 41),JJVNH)
      equivalence (IZOQ(208),JJCKA)
      equivalence (IZOQ(207),JJCIA)
      equivalence (IZOQ(212),JJRKH)
      equivalence (IZOQ(213),JJRLH)
      equivalence (IZOQ(214),JJXRK)
      equivalence (IZOQ(215),JJXRL)
      equivalence (IZOQ(219),JJHEA)
      equivalence (IZOQ(221),JJXCA)
      equivalence (IZOQ(222),JJXCB)
      equivalence (IZOQ(164),JJVXN)
      equivalence (IZOQ(178),JJZT )
      equivalence (IZOQ(225),JJFMV)
      equivalence (IZOQ(229),JJFIW)
      equivalence (IZOQ(149),JJXNC)
      equivalence (IZOQ( 48),JJZME)
      equivalence (IZOQ( 10),JJHNF)
      equivalence (IZOQ(232),JJCQT)
      equivalence (IZOQ(233),JJCQA)
      equivalence (IZOQ(186),JJCVX)
      equivalence (IZOQ( 73),JJPAB)
      equivalence (IZOQ(104),JJPBA)
      equivalence (IZOQ(113),JJPBG)
      equivalence (IZOQ(114),JJPGB)
      equivalence (IZOQ(239),JJSCW)
      equivalence (IZOQ(120),JJWTP)
      equivalence (IZOQ(255),JJWLA)
      equivalence (IZOQ(256),JJFNA)
      equivalence (IZOQ(257),JJWLB)
      equivalence (IZOQ(258),JJFNB)
      equivalence (IZOQ(230),JJZGM)
      equivalence (IZOQ(263),JJGMZ)
      equivalence (IZOQ(151),JJDGM)
      equivalence (IZOQ(266),JJBHZ)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  3),JJLRJ)
      equivalence (JZOQ(  1),JJMRJ)
      equivalence (JZOQ(  7),JJIBE)
      equivalence (JZOQ( 13),JJISV)
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
      equivalence (KZQ(  2),MS   )
      equivalence (KZQ(  3),NS   )
      equivalence (KZQ(  8),IOMX )
      equivalence (KZQ(  4),ISUB )
      equivalence (KZQ( 19),LYMIT)
      equivalence (KZQ( 20),IHSLT)
      equivalence (KZQ( 35),JSTCN)
      equivalence (KZQ( 94),NOION)
      equivalence (KZQ(128),NDWM )
      equivalence (RZQ( 28),XKDST)
      equivalence (KZQ( 30),MDTR1)
      equivalence (RZQ( 17),DLU  )
      equivalence (RZQ(124),PMSK )
      equivalence (KZQ( 51),JH1  )
      equivalence (RZQ( 23),R1N  )
      equivalence (RZQ( 29),TDUST)
      equivalence (RZQ( 32),XLMZ )
      equivalence (KZQ( 52),JH2  )
      equivalence (RZQ( 16),CGR  )
      equivalence (RZQ( 89),CLOGG)
      equivalence (RZQ( 15),YH   )
      equivalence (RZQ( 61),FABD )
      equivalence (RZQ( 53),DDT  )
      equivalence (RZQ( 18),TX   )
      equivalence (RZQ( 72),PZERO)
      equivalence (KZQ( 85),IRTIS)
      equivalence (KZQ( 82),IPZER)
      equivalence (RZQ( 50),WTD  )
      equivalence (RZQ( 30),YFLUX)
      equivalence (RZQ( 51),TLTR )
      equivalence (KZQ( 12),JRHO )
      equivalence (KZQ(197),IBNVW)
      equivalence (RZQ( 98),XLCOD)
      equivalence (RZQ( 97),YCOL )
      equivalence (KZQ(  1),NDW  )
      equivalence (RZQ(107),ZNDW )
      equivalence (RZQ( 52),XCOMX)
      equivalence (KZQ(105),MTHEI)
      equivalence (RZQ(105),XMCOA)
      equivalence (RZQ(108),WNJNK)
      equivalence (RZQ( 99),CCHX )
      equivalence (KZQ(116),ICXDP)
      equivalence (KZQ( 95),NERM )
      equivalence (RZQ(118),EPTAU)
      equivalence (RZQ( 25),WPOP )
      equivalence (RZQ( 45),WBD  )
      equivalence (RZQ( 10),CWR  )
      equivalence (RZQ( 11),CHOP )
      equivalence (RZQ(111),ASMCR)
      equivalence (RZQ( 54),XINCH)
      equivalence (RZQ( 43),WMN  )
      equivalence (RZQ( 55),WMX  )
      equivalence (RZQ( 56),SMP  )
      equivalence (RZQ( 96),CHLIM)
      equivalence (KZQ(122),ICHSW)
      equivalence (KZQ(123),IHSSW)
      equivalence (KZQ(125),IHSDD)
      equivalence (KZQ(126),IHSKM)
      equivalence (KZQ(127),IHSSM)
      equivalence (RZQ(120),HSBM )
      equivalence (RZQ(123),HSBFQ)
      equivalence (RZQ(121),HSBMN)
      equivalence (RZQ(122),HSBMX)
      equivalence (KZQ(194),JEDIT)
      equivalence (RZQ(130),FSTKM)
      equivalence (RZQ(131),FRCDL)
      equivalence (RZQ(132),FMCDL)
      equivalence (KZQ(136),IHSSP)
      equivalence (RZQ(135),WAVMN)
      equivalence (RZQ(136),WAVMX)
      equivalence (KZQ(139),NECLP)
      equivalence (RZQ(139),COMU )
      equivalence (RZQ(157),CTCO )
      equivalence (KZQ(146),JDMCI)
      equivalence (KZQ(147),JDMCE)
      equivalence (RZQ(149),CWJ  )
      equivalence (RZQ(114),CVSB )
      equivalence (RZQ(104),CVXS )
      equivalence (RZQ(137),CVZ  )
      equivalence (RZQ(138),CDZ  )
      equivalence (KZQ( 67),NIASM)
      equivalence (RZQ(158),CTMX )
      equivalence (RZQ(159),SHCOP)
      equivalence (RZQ(161),SHCOC)
      equivalence (RZQ(160),ZRCO )
      equivalence (KZQ(182),JATMO)
      equivalence (KZQ(181),JATOM)
      equivalence (KZQ( 40),JBDNC)
      equivalence (RZQ(165),XLMCR)
      equivalence (KZQ(184),NLY  )
      equivalence (KZQ(186),IWEIT)
      equivalence (KZQ(187),IFALL)
      equivalence (KZQ(188),NQLYM)
      equivalence (RZQ(169),XLMH )
      equivalence (KZQ(190),JHLSK)
      equivalence (KZQ(195),MBREC)
      equivalence (RZQ( 65),CVXM )
      equivalence (RZQ( 67),CVXF )
      equivalence (RZQ( 68),WFB  )
      equivalence (RZQ(110),FBVMX)
      equivalence (KZQ(221),MSEDG)
      equivalence (KZQ(222),MSEDW)
      equivalence (KZQ(199),IXASM)
      equivalence (KZQ(193),JNEDP)
      equivalence (RZQ(147),FMVLM)
      equivalence (RZQ(141),SRCO )
      equivalence (RZQ(133),CSDW )
      equivalence (KZQ( 13),ILI  )
      equivalence (KZQ( 14),NIL  )
      equivalence (KZQ( 10),JBD  )
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ(124),IHSDP)
      equivalence (KZQ(134),NBS  )
      equivalence (KZQ( 38),IVOIT)
      equivalence (KZQ( 42),M304 )
      equivalence (RZQ( 86),CPRSS)
      equivalence (RZQ( 91),REFLM)
      equivalence (KZQ( 31),MDTR2)
      equivalence (RZQ( 41),OPF  )
      equivalence (RZQ( 47),PRTLM)
      equivalence (RZQ( 60),RFMAS)
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
      equivalence (LEST(13),J304I)
      equivalence (LEST(30),KMASN)
      equivalence (LEST(11),KTKIN)
      equivalence (LEST( 1),KSHEL)
      equivalence (LEST( 9),NH2CS)
      equivalence (LEST( 6),KPRSW)
      equivalence (LEST(12),LGGIN)
      equivalence (LEST(26),KNZGM)
      equivalence (LEST(16),MSFQR)
      equivalence (LEST(18),MSFQM)
      equivalence (LEST(20),MSFRT)
      equivalence (LEST(21),MSFGR)
      equivalence (LEST(44),LCOW )
      equivalence (LEST(32),KAMB )
      equivalence (LEST(47),KVLG )
      equivalence (LEST(42),IVNH )
      equivalence (LEST(33),LDLMX)
      equivalence (LEST(54),MFMV )
      equivalence (REST( 1),WTPZ )
      equivalence (LEST(64),IDGMZ)
      equivalence (LEST(36),NVSB )
      equivalence (LEST(55),MCXK )
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
      equivalence (IQQ(246),IQSRJ)
      equivalence (IQQ(165),IQUVP)
      equivalence (IQQ( 51),IQINC)
      equivalence (IQQ( 62),IQRSQ)
      equivalence (IQQ( 60),IQGDS)
      equivalence (IQQ(270),IQHSV)
      equivalence (IQQ(277),IQCXD)
      equivalence (IQQ(229),IQPGA)
      equivalence (IQQ(292),IQPZT)
      equivalence (IQQ(304),IQSTW)
      equivalence (IQQ( 68),IQHMS)
      equivalence (IQQ(183),IQHMO)
      equivalence (IQQ(164),IQPPF)
      equivalence (IQQ(247),IQAMP)
      equivalence (IQQ(248),IQATP)
      equivalence (IQQ(249),IQINP)
      equivalence (IQQ(260),IQADA)
      equivalence (IQQ(340),IQLNU)
C     !DASH
C     !EJECT
      external MODEL, HIERON, DILBERT, HEPZIBA, ARNOLD, EREBUS, AVOCET,
     $         TELEUT, LINTEL, AMALFI, RUPERT, ANNA, ATOM, BWANA, LOUD,
     $         DELIA, MASULA, KOHL, WGIVE, FLOOR, ALBION, RODEO, PATCH,
     $         EYAK, GUNDI, DUBRAS, ABAKAN, GNOME, GOFFAR, POPE, PEACH,
     $         MARGY, IMBROS, GUDEA, GARYM, GELLA, CELLA, TELLA, YODEL,
     $         LUTH, WON, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IXLB1)
C
      dimension ARR(8), BRR(8), IRR(8), QAR(16)
C
      call HI ('DABBLE')
C     !BEG
      if(NO.gt.0) then
C       (Get W allotment)
        call LOUD       (IN, IS, MOX, 'DABBLE')
C
        if(IQAMP.gt.0) then
          call MODEL    (N, X(JJZ), X(JJTE), X(JJXNE), X(JJHND),
     $                   X(JJV), X(JJVT), X(JJDGM), X(JJRZM),
     $                   X(JJAEL), X(JJBHM), X(JJ304), J304I, KMASN,
     $                   X(JJMSI), X(JJMSR), RFMAS, X(JJTEX), KTKIN,
     $                   X(JJTKI), X(JJVXS), X(JJFRS), X(JJH2N),
     $                   NH2CS, X(JJRML), X(JJVR), REFLM, X(JJVSB),
     $                   NVSB, X(JJVM), KVLG, KAMB, X(JJHEA), MFMV,
     $                   X(JJFMV), X(JJZME), X(JJXNC), CGR, YH,
     $                   X(JJBHZ), R1N, KPRSW, CPRSS, CLOGG, LGGIN,
     $                   PZERO, JATMO, ARR, LINE, NO)
          call HEPZIBA  (NO, LDU, XKDST, TDUST, X(JJLMD), X(JJDFD),
     $                   X(JJALD), X(JJEPD), DDT, MDTR1, MDTR2, NDT,
     $                   X(JJLDT), X(JJYDT), X(JJADT), X(JJABD), N,
     $                   X(JJZ), X(JJTDN), WTD, YFLUX, TLTR)
          call GNOME    (NO, X(JJHNV), X(JJVNH), NVH, IVNH, X(JJZGM),
     $                   X(JJGMZ), NGM, IDGMZ)
          call EREBUS   (NO, N, X(JJZ), X(JJTE), X(JJXNE), X(JJHND),
     $                   X(JJZT), X(JJDTE), R1N, IQPZT, W)
        else
          call CELLA    (NO)
        end if
C     !EJECT
        ION = ((JSTCN.le.0).and.(NOION.le.0))
        if(ION) then
          call MARGY      (X)
C
          if(IQATP.gt.0) then
            call ATOM     (X, IX, W, IW, W(IXLB1), KDDR, KDRX, KNZGM,
     $                     JATOM, ARR, BRR, IRR, QAR, LINE, NO)
            call GUDEA    (X(JJZ), ZNDW, NDW, NO)
            call AVOCET   (W(IXLB1), QELSM, NO)
            call YODEL    (N, NSL, X(JJZ), X(JJTE), X(JJTR), X(JJRAB),
     $                     KSHEL, X(JJQIN), JH1, JH2, X(JJHJ),
     $                     X(JJFRS), X(JJPF), X(JJRBL), JATMO, LINE,
     $                     NO)
          else
            call TELLA    (NO)
          end if
C
          call HIERON     (X, IX)
        end if
C
        call POPE         (X(JJCHI), FABD, PRTLM, IQPPF, IQUVP, LINE,
     $                     NO)
        call AMALFI       (FABD, NO)
C
        call GELLA        (IQINP, NO)
C
        if(IQINP.gt.0) then
          call LUTH       (ISUB, IOMX, LYMIT, IHSLT, ION, WPOP, WBD,
     $                     NBS, IWEIT, JEDIT, MBREC, JNEDP, IBNVW, NO)
          call GOFFAR     (IVOIT, MTHEI, NO)
          call KOHL       (IPZER, WNJNK, NERM, EPTAU, IQPGA, IQSRJ,
     $                     IOMX, MS, NS, NL, ION, ASMCR, NIASM, IXASM,
     $                     MSEDG, MSEDW, NO)
          if(IQADA.le.0) then
            if(ION) then
              call DELIA  (NO, JRHO, JBD, JBDNC, CWR, CHOP, CHLIM,
     $                     ILI, NIL, XINCH, WMN, WMX, SMP, MBREC, CWJ)
              call PEACH  (IX(JJMRJ), X(JJWRA), X(JJRRN), X(JJRRC),
     $                     X(JJYRA), ICHSW, JDMCI, JDMCE, LINE, NO)
              call BWANA  (NL, IX(JJLRJ), NO, X(JJRKC), X(JJTKR),
     $                     X(JJYKR), J304I, M304)
              call TELEUT (NO, X(JJRK), X(JJRL), X(JJCIA), X(JJCKA),
     $                     N, NL, NSL)
              call LINTEL (X(JJEP1), X(JJEP2), X(JJR1W), X(JJEQT),
     $                     X(JJXNU), X(JJXK), X(JJYLM), X(JJGK), NO)
              call PATCH  (NO, X(JJVM), X(JJVBM), X(JJPAB), X(JJPBA),
     $                     X(JJPBG), X(JJPGB))
              call MASULA (NO, N, CCHX, ICXDP, MCXK, IQCXD, X(JJRKH),
     $                     X(JJRLH), X(JJXRK), X(JJXRL))
            end if
C     !EJECT
            call GUNDI    (NO, MHM, X(JJLHM), X(JJAHM), X(JJYHM),
     $                     IQHMS, IQHMO)
            call ABAKAN   (NO, NWV, X(JJWAV), X(JJYWA), WAVMN, WAVMX,
     $                     NWS, X(JJSWV), NECLP, NSW, X(JJSCW), JM,
     $                     X(JJLMM), X(JJMLC), NDV, X(JJDWV), KBX,
     $                     X(JJBXI), NDWM)
            call DILBERT  (NO, NVX, CVSB, CVXS, CVXM, CVXF, WFB, WTPZ,
     $                     FBVMX, X(JJCVX), X(JJWTP), CVZ, CDZ, FMVLM)
            call RUPERT   (NO, INK, X(JJXIN), X(JJFIN), IQINC, IQRSQ,
     $                     TX, DLU, IQGDS, NCR, X(JJLCR), X(JJICR),
     $                     X(JJYCR), OPF, IRTIS)
            call ARNOLD   (NO, LLY, X(JJLXX), X(JJLDR), XLMZ, XLMH,
     $                     XLMCR, NLY, IFALL, NQLYM, JHLSK, PMSK, NFL,
     $                     X(JJWLA), X(JJFNA), X(JJWLB), X(JJFNB),
     $                     IQLNU)
            call IMBROS   (NO, QELSM, IHSSW, IHSKM, IHSSM, IHSDP,
     $                     IHSDD, MS, NS, N, HSBM, HSBFQ, HSBMX,
     $                     HSBMN, IHSSP, CSDW, FSTKM, FRCDL, FMCDL,
     $                     IQSTW, LDLMX)
            call RODEO    (NO, X(JJBNL), X(JJBNU), X(JJBNY),
     $                     IX(JJIBE), NAB, KWC, NCP, NKA, X(JJZBK),
     $                     X(JJABK), KWA, NCQ, X(JJCQT), X(JJCQA))
            call EYAK     (NO, X(JJCOL), NCL, X(JJXCA), X(JJXCB), NCB,
     $                     YCOL, XLCOD, X(JJTE), X(JJV), NDW, XCOMX,
     $                     XMCOA, LCOW, NECLP, COMU, SRCO, CTCO, CTMX,
     $                     SHCOP, SHCOC, ZRCO)
            if(ION) then
              call DUBRAS (NDR, X(JJXDR), X(JJDDR), KDDR, KDRX, KNZGM,
     $                     NO)
              call WON    (NL, X(JJWEI), NO)
            end if
            call ANNA     (NO, X(JJMU), X(JJMUF), X(JJFIW), X(JJFRR),
     $                     IX(JJISV), X(JJVXN), X(JJHND), X(JJHNF),
     $                     X(JJFNH))
            call ALBION   (NO, MSFQR, MSFQM, MSFRT, MSFGR, LCOW)
            call FLOOR    (NO)
          end if
        end if
C
C       (Give back W allotment)
        call WGIVE        (W, 'DABBLE')
      end if
C     !END
      call BYE ('DABBLE')
C
      return
      end
