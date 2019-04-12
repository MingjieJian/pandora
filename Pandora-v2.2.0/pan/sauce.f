      subroutine SAUCE
     $(X,IX,W,IW,XKPCR,WAVC,NLPAIR,COP,RHW,RHWO,XLAM,Y,DDL,CDL,DWN,CRD,
     $ CVW,CSK,CRS,STNE,KST,IST,ISB1,WMNO,WMXO,RKWO)
C     Rudolf Loeser, 1980 May 21
C     Revised RL/SGK Apr 10 2014 
C---- Post-read defaults for the second batch of input data:
C     expand shorthand input notations, set defaults and switches,
C     and make explicit other things implied by input as read.
C     (This is version 2 of SAUCE.)
C     !DASH
      save
C     !DASH
      real*8 ABD, ADMAS, ADS, CDL, CDZ, CFH, CFHE, CGR, CLOGG, COP,
     $       CPRSS, CQM, CRD, CRS, CSK, CTCO, CTMX, CVSB, CVW, CVXS,
     $       CVZ, DDL, DWN, FABD, FMCDL, FMVLM, FRCDL, FSTKM, PMSK,
     $       R1GD, R1N, REFLM, RFAC, RFMAS, RFXNC, RHW, RHWO, RKWO,
     $       RWKSI, SCTA, SCTS, SHCOC, SHCOP, STNE, VSMLL, W, WAVC,
     $       WAVMN, WAVMX, WBD, WMN, WMNO, WMX, WMXO, WNUK, WPOP, X,
     $       XKPCR, XLAM, XLMH, XLMXC, XLMZ, XNUK, Y, YH, YL, YPRE,
     $       ZRCO
      integer IBNVW, ICXDP, IDGMZ, IHEAB, IHNDL, IHNVL, IHSSP, IHSSW,
     $        IIMG, IN, INDRN, INK, IONST, IPNT, IPZER, IQBDC, IQCPS,
     $        IQCXU, IQDT2, IQEBI, IQEXA, IQFIN, IQGDS, IQHEA, IQHSE,
     $        IQIFF, IQINC, IQLYM, IQND2, IQREF, IQSFO, IQSFS, IQUET,
     $        IQUTR, IQUVP, IRFNC, IS, ISB1, IST, ISTRK, IVEC, IVET,
     $        IVNH, IVOIT, IW, IWS, IX, IXPBL, J304I, J304S, JDDL,
     $        JEDIT, JH1, JH2, JIBR, JJ304, JJAAT, JJABD, JJABK, JJACE,
     $        JJACI, JJADT, JJAEL, JJAEW, JJAHM, JJAIJ, JJAW, JJBHM,
     $        JJBHZ, JJBNL, JJBNU, JJBNY, JJBXI, JJCEI, JJCEQ, JJCHI,
     $        JJCHN, JJCII, JJCP, JJCVX, JJDGM, JJDTE, JJFCE, JJFCT,
     $        JJFIN, JJFMV, JJFRS, JJGK, JJGMZ, JJHEA, JJHJ, JJHND,
     $        JJHNV, JJICO, JJIKW, JJKIJ, JJLCX, JJLDR, JJLDT, JJLHM,
     $        JJLIJ, JJLRQ, JJMCE, JJMCI, JJMRJ, JJMSI, JJMSR, JJNCO,
     $        JJNE0, JJNK, JJNLE, JJNPQ, JJOHN, JJOSF, JJP, JJPF, JJPMG,
     $        JJPNF, JJQHI, JJR1W, JJRAB, JJRBL, JJRHO, JJRK, JJRKH,
     $        JJRKS, JJRL, JJRLH, JJRLS, JJRML, JJRNI, JJRRC, JJRRN,
     $        JJRZM, JJSA, JJSCW, JJTCO, JJTDN, JJTE, JJTER, JJTEX,
     $        JJTKI, JJTR, JJV, JJVM, JJVNH, JJVSB, JJVT, JJVXI, JJVXS,
     $        JJWAV, JJWCU, JJWEI, JJWNU, JJWRA, JJWVA, JJWVC, JJXCA,
     $        JJXCB, JJXCU, JJXIB, JJXIF, JJXIN, JJXIR, JJXIS, JJXK,
     $        JJXNC, JJXND, JJXNE, JJXNU, JJXRK, JJXRL, JJYBR, JJYCO,
     $        JJYCR, JJYDT, JJYHM, JJYKR, JJYLM, JJYRA, JJYWA, JJYWC,
     $        JJZ, JJZBK, JJZGM, JJZIN, JJZME, JJZRN, JJZT, JN, JPOP,
     $        JSSV, JSTCN, KAMB, KB, KBNDS, KBTMX, KBX, KDFGA, KDFGB,
     $        KDFGS, KF, KK, KM, KMASN, KOLEV, KPRSW, KR, KRTMX, KS,
     $        KSHEL, KST, KSTMX, KSTRK, KTECH, KTKIN, KVLG, KVSB, KZXST,
     $        LCOW, LDLMU, LDLMX, LFLX, LGGIN, LHHSE, LLY, MCXK, METEP,
     $        MFMV, MHM, MLS, MLSFP, MN1, MNG1, MOX, MRR, MRS, MS, MUX,
     $        N, N1MET, NAB, NCB, NCINM, NCOSW, NCQ, NCR, NDT, NGM,
     $        NH2CS, NHTSW, NKA, NL, NLB, NLFDB, NLPAIR, NO, NOION,
     $        NONC, NOTA, NOTX, NPT, NS, NSL, NSW, NT, NTE, NTK, NVH,
     $        NVOIT, NVSB, NVSBP, NVX, NWV
      logical DOFMV, NOCEIJ, NOCII
      character QELSM*8, QIONM*8, QNAME*8
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(40),NSL)
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
      equivalence (JZQ(35),INK)
      equivalence (JZQ( 4),KF )
      equivalence (JZQ(49),KM )
      equivalence (JZQ(36),KS )
      equivalence (JZQ(37),KR )
      equivalence (JZQ(38),KB )
      equivalence (JZQ(10),KK )
      equivalence (JZQ(33),MLS)
      equivalence (JZQ(29),MRS)
      equivalence (JZQ(45),NAB)
      equivalence (JZQ(21),NDT)
      equivalence (JZQ(32),NCR)
      equivalence (JZQ(17),NWV)
      equivalence (JZQ(50),NKA)
      equivalence (JZQ(54),NVH)
      equivalence (JZQ(20),NTE)
      equivalence (JZQ(55),NCB)
      equivalence (JZQ(42),NVX)
      equivalence (JZQ(53),NCQ)
      equivalence (JZQ( 6),NSW)
      equivalence (JZQ(11),KBX)
      equivalence (JZQ(15),MRR)
      equivalence (JZQ(22),MHM)
      equivalence (JZQ( 8),NLB)
      equivalence (JZQ(27),LLY)
      equivalence (JZQ(58),NGM)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  8),JJTR )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(178),JJZT )
      equivalence (IZOQ( 12),JJV  )
      equivalence (IZOQ( 49),JJVT )
      equivalence (IZOQ( 53),JJRAB)
      equivalence (IZOQ(189),JJRBL)
      equivalence (IZOQ( 62),JJTDN)
      equivalence (IZOQ( 52),JJRZM)
      equivalence (IZOQ( 86),JJBHM)
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ(141),JJPF )
      equivalence (IZOQ(143),JJCHI)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ( 28),JJCP )
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ( 69),JJRRN)
      equivalence (IZOQ( 70),JJRRC)
      equivalence (IZOQ(  5),JJ304)
      equivalence (IZOQ( 32),JJAIJ)
      equivalence (IZOQ(111),JJNE0)
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 35),JJZIN)
      equivalence (IZOQ( 33),JJMSI)
      equivalence (IZOQ( 38),JJMSR)
      equivalence (IZOQ( 71),JJTEX)
      equivalence (IZOQ( 14),JJXIN)
      equivalence (IZOQ( 15),JJFIN)
      equivalence (IZOQ(121),JJXIF)
      equivalence (IZOQ(146),JJXIS)
      equivalence (IZOQ(148),JJXIR)
      equivalence (IZOQ(150),JJXIB)
      equivalence (IZOQ(123),JJXK )
      equivalence (IZOQ(112),JJGK )
      equivalence (IZOQ(136),JJHJ )
      equivalence (IZOQ(138),JJFRS)
      equivalence (IZOQ(139),JJCEQ)
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ(202),JJVXI)
      equivalence (IZOQ(156),JJRML)
      equivalence (IZOQ(161),JJBNU)
      equivalence (IZOQ(162),JJBNY)
      equivalence (IZOQ(157),JJWVC)
      equivalence (IZOQ(227),JJWVA)
      equivalence (IZOQ(159),JJYWC)
      equivalence (IZOQ( 84),JJLHM)
      equivalence (IZOQ( 85),JJAHM)
      equivalence (IZOQ( 87),JJYHM)
      equivalence (IZOQ( 76),JJYCO)
      equivalence (IZOQ( 78),JJYWA)
      equivalence (IZOQ( 61),JJYLM)
      equivalence (IZOQ( 77),JJYRA)
      equivalence (IZOQ(118),JJYCR)
      equivalence (IZOQ(103),JJYKR)
      equivalence (IZOQ( 89),JJYDT)
      equivalence (IZOQ(170),JJZBK)
      equivalence (IZOQ(171),JJABK)
      equivalence (IZOQ( 17),JJRK )
      equivalence (IZOQ( 18),JJRL )
      equivalence (IZOQ( 50),JJNK )
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ(181),JJVM )
      equivalence (IZOQ(172),JJVSB)
      equivalence (IZOQ(206),JJHNV)
      equivalence (IZOQ( 79),JJTER)
      equivalence (IZOQ( 29),JJCII)
      equivalence (IZOQ( 30),JJCEI)
      equivalence (IZOQ(212),JJRKH)
      equivalence (IZOQ(213),JJRLH)
      equivalence (IZOQ(214),JJXRK)
      equivalence (IZOQ( 75),JJTKI)
      equivalence (IZOQ( 41),JJVNH)
      equivalence (IZOQ(215),JJXRL)
      equivalence (IZOQ(219),JJHEA)
      equivalence (IZOQ(221),JJXCA)
      equivalence (IZOQ(222),JJXCB)
      equivalence (IZOQ( 27),JJP  )
      equivalence (IZOQ(224),JJOSF)
      equivalence (IZOQ( 65),JJWAV)
      equivalence (IZOQ(225),JJFMV)
      equivalence (IZOQ(186),JJCVX)
      equivalence (IZOQ(149),JJXNC)
      equivalence (IZOQ( 48),JJZME)
      equivalence (IZOQ( 80),JJAEL)
      equivalence (IZOQ(101),JJLDR)
      equivalence (IZOQ(231),JJICO)
      equivalence (IZOQ( 68),JJPNF)
      equivalence (IZOQ(166),JJMCI)
      equivalence (IZOQ(204),JJACI)
      equivalence (IZOQ(211),JJRNI)
      equivalence (IZOQ(234),JJDTE)
      equivalence (IZOQ(235),JJSA )
      equivalence (IZOQ( 90),JJLDT)
      equivalence (IZOQ( 91),JJADT)
      equivalence (IZOQ( 92),JJABD)
      equivalence (IZOQ(236),JJZRN)
      equivalence (IZOQ(237),JJTCO)
      equivalence (IZOQ(238),JJFCT)
      equivalence (IZOQ( 24),JJWEI)
      equivalence (IZOQ(108),JJR1W)
      equivalence (IZOQ(106),JJMCE)
      equivalence (IZOQ(107),JJACE)
      equivalence (IZOQ(239),JJSCW)
      equivalence (IZOQ(243),JJAEW)
      equivalence (IZOQ(244),JJWRA)
      equivalence (IZOQ(254),JJBXI)
      equivalence (IZOQ(248),JJFCE)
      equivalence (IZOQ(184),JJNCO)
      equivalence (IZOQ(220),JJWNU)
      equivalence (IZOQ(160),JJBNL)
      equivalence (IZOQ(260),JJXCU)
      equivalence (IZOQ(261),JJWCU)
      equivalence (IZOQ(262),JJAAT)
      equivalence (IZOQ(151),JJDGM)
      equivalence (IZOQ(230),JJZGM)
      equivalence (IZOQ(263),JJGMZ)
      equivalence (IZOQ( 45),JJRHO)
      equivalence (IZOQ( 13),JJYBR)
      equivalence (IZOQ(242),JJAW )
      equivalence (IZOQ(245),JJQHI)
      equivalence (IZOQ(265),JJPMG)
      equivalence (IZOQ(266),JJBHZ)
      equivalence (IZOQ(268),JJCHN)
      equivalence (IZOQ(269),JJOHN)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  2),JJKIJ)
      equivalence (JZOQ(  8),JJLIJ)
      equivalence (JZOQ(  6),JJIKW)
      equivalence (JZOQ(  1),JJMRJ)
      equivalence (JZOQ(  4),JJRKS)
      equivalence (JZOQ(  5),JJRLS)
      equivalence (JZOQ(  9),JJNPQ)
      equivalence (JZOQ( 10),JJLRQ)
      equivalence (JZOQ( 11),JJLCX)
      equivalence (JZOQ( 12),JJNLE)
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
      equivalence (LEST(12),LGGIN)
      equivalence (LEST(25),NOTX )
      equivalence (LEST( 7),NPT  )
      equivalence (LEST(29),NONC )
      equivalence (LEST(35),NLFDB)
      equivalence (LEST(14),NTK  )
      equivalence (LEST(22),JPOP )
      equivalence (LEST( 9),NH2CS)
      equivalence (LEST(23),J304S)
      equivalence (LEST(30),KMASN)
      equivalence (LEST(11),KTKIN)
      equivalence (REST( 2),R1GD )
      equivalence (LEST(27),NOTA )
      equivalence (LEST( 1),KSHEL)
      equivalence (LEST( 6),KPRSW)
      equivalence (LEST(15),LFLX )
      equivalence (LEST(53),LDLMU)
      equivalence (LEST(60),KBTMX)
      equivalence (LEST(61),KRTMX)
      equivalence (LEST(62),KSTMX)
      equivalence (LEST(36),NVSB )
      equivalence (LEST(50),NVSBP)
      equivalence (LEST(43),MLSFP)
      equivalence (LEST(44),LCOW )
      equivalence (LEST(47),KVLG )
      equivalence (LEST(37),KVSB )
      equivalence (LEST(42),IVNH )
      equivalence (LEST(51),JIBR )
      equivalence (LEST(55),MCXK )
      equivalence (LEST(52),JDDL )
      equivalence (LEST(54),MFMV )
      equivalence (REST( 5),RFXNC)
      equivalence (LEST(65),KTECH)
      equivalence (LEST(68),KZXST)
      equivalence (LEST(79),NCINM)
      equivalence (QEST( 1),QIONM)
      equivalence (LEST(13),J304I)
      equivalence (LEST(33),LDLMX)
      equivalence (LEST(32),KAMB )
      equivalence (LEST(64),IDGMZ)
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
      equivalence (RZQ( 16),CGR  )
      equivalence (RZQ( 89),CLOGG)
      equivalence (RZQ(101),CQM  )
      equivalence (KZQ(  2),MS   )
      equivalence (KZQ(  3),NS   )
      equivalence (KZQ( 44),NCOSW)
      equivalence (KZQ( 55),NHTSW)
      equivalence (RZQ( 61),FABD )
      equivalence (QZQ(  1),QNAME)
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ( 51),JH1  )
      equivalence (KZQ( 52),JH2  )
      equivalence (RZQ(  6),ABD  )
      equivalence (KZQ( 56),IONST)
      equivalence (RZQ( 15),YH   )
      equivalence (RZQ(  9),XNUK )
      equivalence (RZQ(  3),RWKSI)
      equivalence (RZQ( 19),YL   )
      equivalence (RZQ( 25),WPOP )
      equivalence (RZQ( 45),WBD  )
      equivalence (KZQ( 35),JSTCN)
      equivalence (RZQ( 60),RFMAS)
      equivalence (RZQ( 23),R1N  )
      equivalence (KZQ( 33),KOLEV)
      equivalence (RZQ( 85),VSMLL)
      equivalence (KZQ( 82),IPZER)
      equivalence (RZQ( 86),CPRSS)
      equivalence (RZQ( 20),YPRE )
      equivalence (KZQ( 94),NOION)
      equivalence (KZQ(101),MN1  )
      equivalence (KZQ(110),MNG1 )
      equivalence (RZQ(104),CVXS )
      equivalence (RZQ(152),CFHE )
      equivalence (RZQ(114),CVSB )
      equivalence (KZQ(116),ICXDP)
      equivalence (KZQ( 83),IHEAB)
      equivalence (RZQ(  1),ADS  )
      equivalence (RZQ( 95),ADMAS)
      equivalence (KZQ(123),IHSSW)
      equivalence (KZQ( 38),IVOIT)
      equivalence (RZQ(124),PMSK )
      equivalence (RZQ(131),FRCDL)
      equivalence (RZQ(130),FSTKM)
      equivalence (RZQ(132),FMCDL)
      equivalence (RZQ(135),WAVMN)
      equivalence (RZQ( 55),WMX  )
      equivalence (RZQ( 43),WMN  )
      equivalence (KZQ(136),IHSSP)
      equivalence (RZQ(136),WAVMX)
      equivalence (RZQ(137),CVZ  )
      equivalence (RZQ(138),CDZ  )
      equivalence (RZQ(147),FMVLM)
      equivalence (KZQ(145),IRFNC)
      equivalence (RZQ( 91),REFLM)
      equivalence (RZQ( 32),XLMZ )
      equivalence (KZQ(129),LHHSE)
      equivalence (KZQ(154),KDFGS)
      equivalence (KZQ(155),KDFGA)
      equivalence (KZQ(156),KDFGB)
      equivalence (KZQ( 91),N1MET)
      equivalence (KZQ(173),INDRN)
      equivalence (KZQ(170),KBNDS)
      equivalence (RZQ(157),CTCO )
      equivalence (RZQ(158),CTMX )
      equivalence (RZQ(159),SHCOP)
      equivalence (RZQ(161),SHCOC)
      equivalence (RZQ(160),ZRCO )
      equivalence (RZQ( 42),RFAC )
      equivalence (RZQ(162),XLMXC)
      equivalence (RZQ(169),XLMH )
      equivalence (RZQ(172),SCTA )
      equivalence (RZQ(173),SCTS )
      equivalence (KZQ(194),JEDIT)
      equivalence (KZQ(197),IBNVW)
      equivalence (RZQ(119),WNUK )
      equivalence (KZQ( 25),METEP)
      equivalence (RZQ(113),CFH  )
      equivalence (KZQ( 39),NVOIT)
      equivalence (KZQ(133),ISTRK)
      equivalence (KZQ(192),JSSV )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
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
      equivalence (IQQ(165),IQUVP)
      equivalence (IQQ( 53),IQUTR)
      equivalence (IQQ( 51),IQINC)
      equivalence (IQQ(141),IQIFF)
      equivalence (IQQ( 49),IQFIN)
      equivalence (IQQ( 50),IQREF)
      equivalence (IQQ( 13),IQLYM)
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ( 60),IQGDS)
      equivalence (IQQ( 16),IQHSE)
      equivalence (IQQ(184),IQCPS)
      equivalence (IQQ(169),IQEXA)
      equivalence (IQQ( 60),IQGDS)
      equivalence (IQQ(208),IQEBI)
      equivalence (IQQ(245),IQBDC)
      equivalence (IQQ(281),IQHEA)
      equivalence (IQQ(331),IQCXU)
      equivalence (IQQ(318),IQUET)
      equivalence (IQQ( 99),IQDT2)
      equivalence (IQQ(212),IQND2)
      equivalence (IQQ(  8),IQSFO)
C     !DASH
C     !EJECT
      external    HADRA, HADRE, HADRI, HADRO, HADRU, HADRY,
     $            HAGRA, HAGRE, HAGRI, HAGRO, HAGRU, HAGRY,
     $            HAMRA, HAMRE, HAMRI, HAMRO, HAMRU, HAMRY,
     $            HEDRA, HEDRE, HEDRI, HEDRO, HEDRU, HEDRY,
     $            HEGRA, HEGRE, HEGRI, HEGRO, HEGRU, HEGRY,
     $            HEMRA, HEMRE, HEMRI, HEMRO, HEMRU, HEMRY,
     $            HIDRA, HIDRE, HIDRI, HIDRO, HIDRU, HIDRY,
     $            HIGRA, HIGRE, HIGRI, HIGRO, HIGRU, HIGRY,
     $            HIMRA, HIMRE,
     $            HODRA, HODRE, HODRI, HODRO, HODRU, HODRY,
     $            HOGRA, HOGRE, HOGRI, HOGRO, HOGRU, HOGRY,
     $            HUDRA, HUDRE, HUDRI, HUDRO, HUDRU, HUDRY,
     $            HUGRA, HUGRE, HUGRI, HUGRO, HUGRU, HUGRY,
     $            HYDRA, HYDRE, HYDRI, HYDRO, HYDRU, HYDRY,
     $            HYGRA, HYGRE, HYGRI, HYGRO, HYGRU, HYGRY,
     $            JOPLIN, KALPA, WGIVE, IGIVE, HI, BYE
C
      dimension   X(*), IX(*), W(*), IW(*)
C
C                 WAVC(KWC), XKPCR(NT), DWN(LDLMX,NT), XLAM(NT), Y(NT),
      dimension   WAVC(*),   XKPCR(*),  DWN(*),        XLAM(*),  Y(*),
C
C                 NLPAIR(2,NL), DDL(LDLMX,NT), CDL(LDLMX,NT), STNE(NT),
     $            NLPAIR(*),    DDL(*),        CDL(*),        STNE(*),
C
C                 CRD(LDLMX,NT), CVW(LDLMX,NT), CSK(LDLMX,NT), CRS(NT),
     $            CRD(*),        CVW(*),        CSK(*),        CRS(*),
C
C                 COP(N,NT), RHW(N,NT), ISB1(NT), RHWO(N,NT), RKWO(N),
     $            COP(*),    RHW(*),    ISB1(*),  RHWO(*),    RKWO(*),
C
C                 KST(NT), IST(NT)
     $            KST(*),  IST(*)
C
      dimension   IN(5)
      equivalence
     $(IN( 1),IHNVL ),(IN( 2),IHNDL ),(IN( 3),IVET  ),(IN( 4),IVEC  ),
     $(IN( 5),IXPBL )
C
      dimension   JN(2)
      equivalence
     $(JN( 1),IPNT  ),(JN( 2),IIMG  )
C
      call HI ('SAUCE')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call JOPLIN (IN, IS , MOX, 'SAUCE')
      call KALPA  (JN, IWS, MUX, 'SAUCE')
C
C---- Z-Recalculation control
      call HADRY (X(JJZ), X(JJZIN), X(JJMSI), X(JJTKI), N, QNAME, KZXST,
     $            KMASN, KTKIN, RFMAS, X(JJMSR), REFLM)
C
C---- Check input Z values
C     (MUST not precede HADRY)
      call HIMRE (KZXST, X(JJZ), N, NO)
C
C---- Check LHHSE
C     (MUST not precede HADRY)
      call HIMRA (IQHSE, KTKIN, N, LHHSE)
C
C---- Check compatibility of processing options
C     (MUST come here, before other stuff)
      call HADRI (N, NL, IQINC, IQIFF, IQFIN, IQREF, IQGDS, IQEBI)
C
C---- Massage abundance table values
      call HADRA (FABD)
C
C---- Edit metal abundances and ionization potentials
C     (eliminating data for elements whose abundance now =0)
C     (MUST precede HUDRI)
      call HEDRI
C
C---- Play with Helium data
C     (MUST not precede HADRA)
      call HEDRO (YH, CFH, CFHE)
C
C---- Set up gravity values
C     (MUST precede HEMRE)
      call HAGRO (CGR, CLOGG, LGGIN)
C
C---- Set up default values of V
C     (MUST precede HIGRE)
      call HOGRU (X(JJHNV), X(JJVNH), NVH, X(JJHND), X(JJV), N,
     $            W(IHNVL), W(IHNDL), IVNH)
C
C---- Set VT table
C     (MUST precede HEMRE)
      call HIGRE (X(JJV), X(JJVT), N)
C
C---- Set up defaults values of DGM
C     (MUST precede HEMRE)
      call HYDRE (X(JJZGM), X(JJGMZ), NGM, X(JJZ), X(JJDGM), N,
     $            IDGMZ)
C
C---- Set up pressures
C     (MUST precede HEMRE)
      call HEMRY (N, X(JJBHZ), X(JJPMG))
C
C---- Check and edit TE
      call HYDRU (N, X(JJZ), KZXST, X(JJTE), W(IVEC), W(IVET), IQUET,
     $            KTECH, JSSV, SCTA, SCTS, W, IW, NO)
C
C---- Default HND and XNE if needed when input mass is given
C     (MUST not precede HADRY)
      call HEMRE (KMASN, N, YH, X(JJMSI), X(JJTE), X(JJHND), X(JJXNE),
     $            W(IVEC), W(IVET))
C
C---- Compute temperature gradients
C     (MUST be preceded by HYDRU)
      call HYGRA (N, X(JJTE), X(JJZ), KZXST, X(JJZT), X(JJDTE), W(IVEC),
     $            W, IW)
C
C---- Check XI, XK tables
      call HOGRA (X(JJXIS), KS, X(JJXIR), KR, X(JJXIB), KB, X(JJBXI),
     $            KBX, X(JJXK), KK)
C
C---- Set up "DIVIDE" controls
      call HAGRE (VSMLL, IPZER)
C
C---- Default RAB, RZM, and BDHM
      call HYDRI (X(JJRAB), X(JJRBL), X(JJRZM), X(JJBHM), N, QELSM)
C
C---- Check defaults for Helium abundance variation
      call HUGRO (X(JJHEA), IQSFS, IQHEA, IHEAB, N)
C
C---- Set up table of ionization potentials
      call HUDRI (X(JJCHI), IQUVP)
C
C---- Set up TCO, "carbon monoxide temperature"
C     (MUST precede HODRE)
      call HAMRU (X(JJZ), KZXST, X(JJTE), X(JJTCO), X(JJFCT), N, CTCO,
     $            CTMX, SHCOP, SHCOC, ZRCO, NO)
C
C---- Molecular number densities: CON, CHN and OHN
      call HODRE (N, X(JJTE), X(JJTCO), X(JJHND), X(JJNCO), X(JJCHN),
     $            X(JJOHN), NO)
C
C---- Fill-in CO-lines opacity data tables
      call HEGRO (X(JJXCA), X(JJXCB), NCB, LCOW)
C
C---- Set up CO number density to be used
C     (MUST not precede HODRE)
      call HEGRE (N, X(JJZ), X(JJTE), X(JJTCO), X(JJHND), X(JJICO),
     $            X(JJNCO), NO)
C
C---- Default XNE0 table
      call HODRI (X(JJXNE), X(JJNE0), N)
C
C---- Default charged particle density, XNC, and
C     reference index, IRFNC
      call HYGRY (X, W(IXPBL), N, NTE, IRFNC, X(JJTER), X(JJTE),
     $            X(JJRZM), X(JJZME), X(JJAEL), X(JJZRN),
     $            X(JJXNC), W(IVEC))
C
C---- Default TEX table
      call HODRI (X(JJTE), X(JJTEX), N)
C
C---- Sort XINK table
      call HODRO (INK, X(JJXIN), X(JJFIN), IW(IPNT), W(IVEC))
C
C---- Sort SCOW table
      call HEMRA (NSW, X(JJSCW), IW(IPNT))
C
C---- Default R1GD value
      call HODRU (X(JJZ), KZXST, R1N, N, R1GD)
C
C---- Check adequacy of FRR table
      call HIDRE (MRR, IQSFS)
C
C---- Compute FRS
      call HIDRU (N, R1N, X(JJZ), KZXST, X(JJFRS), IQSFS, IQSFO)
C
C---- Set up H2 abundance calculation parameters
      call HIDRI (X(JJTE), X(JJCEQ), N, QNAME, NHTSW, NH2CS)
C
C---- Edit absorber switches
C     (MUST not precede HIDRI, HODRE, or HEGRO)
      call HEDRE
C
C---- Compute mass loss rate
      call HEDRU (X, X(JJFRS), X(JJHND), X(JJVXS), W(IVEC), X(JJRML))
C
C---- Set up data for Composite Line opacity
      call HEDRY (X(JJBNL), X(JJBNU), X(JJBNY), X(JJWVC), X(JJYWC),
     $            IX(JJIKW), WAVC)
C
C---- Set up data for "Averaged" line opacity
      call HYGRI (X(JJWVA))
C
C---- Set up Line Opacities albedo parameters
      call HUDRU (NKA, X(JJZBK), N, X(JJZ), KZXST, X(JJABK), CQM, NCQ)
C
C---- Make sure H- wavelengths are in ascending sorted order
      call HAGRA (X(JJLHM), X(JJAHM), X(JJYHM), MHM, IW(IPNT), W(IVEC))
C
C---- Set constant-pressure NH-adjustment switch
      call HAGRI (IQHSE, IQCPS, CPRSS, QNAME, QELSM, KPRSW)
C
C---- Set up angular diameter data
      call HOGRI (ADS, ADMAS)
C
C---- Check, and massage, type-2 dust data
      call HAMRO (IQDT2, IQND2, NDT, X(JJLDT), X(JJYDT), X(JJADT),
     $            X(JJABD), IW(IPNT), W(IVEC))
C
C---- Initial "SENNA" checksums, for "background" contributors
      call HIGRY (N, X(JJTE), X(JJXNE), X(JJHND), X(JJBHM), X(JJTDN))
C
C---- Augment WAVES (if desired) and sort
C     (MUST not precede HEDRE)
      call HUGRE (X, NWV, X(JJWAV), X(JJYWA), WAVMN, WAVMX, IW(IPNT),
     $            W(IVEC))
C
C---- Edit Source Function method control switches
C     for consistency, and
C     set up Source Function methods summaries
C     Part I  --  see also HAGRU
      call HUGRU (X(JJYWA), NWV, X(JJYHM), MHM, X(JJYDT), NDT,
     $            X(JJBNY), NAB)
C
C---- Check Lyman-alpha control parameters
      call HAMRA (X(JJLDR), LLY, XLMZ, XLMH, XLMXC)
C
C---- Determine whether FMV must be computed (if not, then it must be
C     set to its default values =1).
C     (MUST precede HUGRY.)
      call HYGRU (CVXS, CVSB, NVX, X(JJCVX), DOFMV)
C
C---- Compute FMV - fluid velocity multiplier
C     (MUST precede HEGRA, HIGRI, HIGRU, and HOGRO.)
      call HUGRY (DOFMV, X(JJFMV), X(JJHND), X(JJZ), KZXST, N, CDZ,
     $            CVZ, FMVLM, MFMV)
C
      if((NOION.eq.0).and.(JSTCN.eq.0))then
C
C----   The calls to HIDRY, HEMRO, HAGRY/HOGRY, HUDRO and HYGRE
C       must occur in that order.
C
C----   Default element symbol
        call HIDRY (QNAME, QELSM, QIONM, NCINM)
C
C----   Set up optically-thin-limit if needed
C       (MUST precede HAGRY and HOGRY)
        call HEMRO (IX(JJKIJ), NL)
C
C----   Default ABD value
        call HIDRO (ABD, QELSM, QNAME)
C
C----   Normalize ND,NK, and make sure FCE is OK
C       (MUST succeed HYDRI, HIDRY and HIDRO)
        call HAMRI (N, NL, NT, INDRN, X, IX, W, IW, X(JJHND), X(JJRAB),
     $              W(IVEC), X(JJNK), X(JJXND), X(JJRNI), W(IVEC),
     $              X(JJFCE), IW(IIMG), NO)
C
C----   Default atomic model parameters: Hydrogen or other
        if(QELSM(1:3).eq.'H  ') then
          call HAGRY (N, NL, NSL, NT, NTE, X(JJTER), IHSSP, ISTRK,
     $                FRCDL, FMCDL, FSTKM, LDLMX, JDDL, IHSSW, PMSK,
     $                X(JJXNE), X(JJXNC), X(JJTE), X(JJV), X(JJMCI),
     $                X(JJACI), X(JJMCE), X(JJACE), IX(JJNPQ),
     $                IX(JJLRQ), IX(JJNLE), XNUK, X(JJXNU), X(JJXCU),
     $                WNUK, X(JJWNU), X(JJWCU), X(JJAEW), X(JJCP),
     $                X(JJP), X(JJCII), IX(JJKIJ), IX(JJLIJ),
     $                X(JJAIJ), X(JJAAT), X(JJCEI), RFXNC, IRFNC,
     $                XLAM, CRD, CVW, CSK, CRS, DWN, CDL, DDL, STNE,
     $                IST, KST)
        else
          call HOGRY (N, NL, NSL, NT, NTE, X(JJTER), X(JJMCI),
     $                X(JJACI), X(JJMCE), X(JJACE), IX(JJNPQ),
     $                IX(JJLRQ), IX(JJNLE), NLPAIR, XNUK, X(JJXNU),
     $                X(JJXCU), WNUK, X(JJWNU), X(JJWCU), X(JJAEW),
     $                X(JJCP), X(JJP), X(JJCII), IX(JJKIJ),
     $                IX(JJLIJ), X(JJAIJ), X(JJAAT), X(JJCEI), XLAM,
     $                DDL, DWN, CRD, CRS, CVW, CSK)
        end if
C
C----   Adjust collision rates
        call HAMRY (NL, NSL, NTE, RFAC, X(JJCII), X(JJCEI))
C
C----   Print CI and CE comparisons
C       (MUST NOT precede HAGRY, HOGRY, or HAMRY)
        call HAMRE (X, IX, X(JJTE), X(JJXNC), NO)
C
C----   Set up oscillator strengths
        call HYGRO (NL, X(JJAIJ), X(JJP), X(JJXNU), X(JJOSF))
C
C----   Assure line processing mode switches compatibility, and
C       return ISB1MX, the maximum value of the first Sobolev
C       solution limit index
C
C       A L S O :   massages input
C
C       (MUST precede HAGRU and HEMRU)
        call HUDRO (X(JJYCO), IQEXA, NVX, LDLMU, KBTMX, KRTMX, KSTMX,
     $              KSTRK, N, NT, WMN, WMX, WMNO, WMXO, Y, RHW, RHWO,
     $              KST, ISB1)
C
C----   Check things for VAMOS
        call HEMRI (NT, NL, NSL)
C
C----   Fiddle with input when OPTHINL = on
        call HEMRU (N, NT, X(JJRHO), X(JJYBR), X(JJQHI), X(JJAW))
C
C----   Obtain RRNU (frequ. units) from WRAT (angstroms), and set up
C       continuum edge wavelengths in WRAT
C       (MUST precede HUGRA)
        call HUDRY (NSL, X(JJXNU), X(JJXCU), IX(JJMRJ), X(JJRRN),
     $              X(JJWRA), RWKSI)
C
C----   Set up default RRCP values
        call HUGRA (QELSM, NSL, IX(JJMRJ), X(JJWRA), X(JJXNU),
     $              X(JJXCU), X(JJRRC))
C
C----   Abort if KOLEV is bad
C       (MUST precede HIGRA)
        call HEGRU (QELSM, IQLYM, KOLEV)
C
C----   Set default Lyman XK table (and set GK = 0 ?)
C       A L S O :   fudges RRNU if necessary and, in any event, fudges
C                   the edge values of RRNU
C       (MUST precede HAGRU, HOGRA and HADRE)
        call HIGRA (KK, X(JJXK), X(JJGK), KOLEV, IX(JJMRJ), X(JJRRN),
     $              NSL, N, X(JJRK), X(JJRL), X(JJXNU), X(JJXCU),
     $              X(JJWRA))
C
C----   Edit Source Function method control switches
C       for consistency, and
C       set up Source Function methods summaries
C       Part II  --  see also HUGRU
        call HAGRU (YL, YPRE, X(JJYCO), NT, X(JJYLM), KK, X(JJYCR),
     $              NCR, X(JJYKR), MLS, X(JJYRA), (MRS+NSL))
C
C----   Compute summary switches
        call HUDRA (NT, ISB1, NPT, NOTA, NOTX, NONC, LFLX, KM, NTK,
     $              NLFDB, NVSB, NVSBP, MLSFP, JIBR)
C
C----   Set Voigt function control
        call HYGRE (QELSM, KSTRK, IVOIT, NVOIT)
C
C----   Default TR values
        call HYDRA (X(JJTR), X(JJTE), N, NSL)
C
C----   Set up and check data for diffusion calculations
C       (MUST not precede HADRY)
        call HEGRA (X(JJNK), X(JJXND), NL, NT, N, MN1, MNG1, KAMB,
     $              KVLG, KTKIN, KMASN, KDFGS, KDFGA, KDFGB,
     $              X(JJPNF), N1MET, MFMV, KBNDS, X(JJWEI))
C
C----   Continuous Opacity at line wavelengths, and input switches
        call HYDRY (N, NT, XKPCR, MS, NS, COP)
C
C----   Set up partition function
        call HUDRE (N, X(JJTE), X(JJXNE), X(JJPF))
C
C----   Set up Saha-Boltzmann term
C       (MUST be preceded by HUDRE and HYDRU)
        call HODRA (N, X(JJTE), X(JJPF), X(JJSA))
C
C----   Check and adjust Lyman EP1, EP2 control parameters, and
C       RK-weights
        call HEGRI (METEP, KOLEV, NL, N, X(JJR1W), RKWO)
C
C----   Set KSHEL value
C       (MUST be preceded by HOGRY)
        call HODRY (X(JJCP), NSL, IQUTR, KSHEL)
C
C----   Defaults of parameters for BD & ND calculations
        call HADRO (WPOP, WBD, JEDIT, IBNVW)
C
C----   Set J304I (input) and J304S (save) switches
        call HADRU (X(JJ304), N, X(JJXNU), NL, NT, J304I, J304S)
C
C----   Set up XIF table (& XIB and/or XIR if needed)
        call HEDRA (X(JJXIF), KF, X(JJXIS), KS, X(JJXIR) ,KR,
     $              X(JJXIB), KB)
C
C----   Delete Rates-related input in non-LYMAN runs
        call HOGRE (N, NSL, IQLYM, IX(JJRKS), X(JJRK), IX(JJRLS),
     $              X(JJRL))
C
C----   Lyman GK
        call HADRE (IQLYM, KK, X(JJXK), X(JJGK), X(JJXNU), X(JJXCU),
     $              KOLEV, W(IVEC))
C
C----   Photoionization rates multiplier
        call HIDRA (X(JJZ), KZXST, X(JJHJ), N, JH1, JH2, R1N, IQSFS,
     $              IQGDS, IQUTR)
C
C----   Compute NLB, B-editing index
        call HYDRO (JPOP, NL, NLB)
C
C----   Check the "Run-to-Pop" indices
        call HEGRY
C
C----   Check b-option w.r.t. supplementary levels
        call HIGRO (NL, NSL, IQBDC)
C
C----   Set up charge exchange parameters
C       (MUST be preceded by HUGRE)
        call HUGRI (IQCXU, QELSM, MCXK, N, X(JJTE), ICXDP, NL,
     $              IX(JJNPQ), IX(JJLRQ), IX(JJLCX), X(JJRKH),
     $              X(JJRLH), X(JJXRK), X(JJXRL))
      end if
C
C     (The order of the calls to HIGRI, HIGRU and HOGRO matters,
C      so does the fact that they occur after all the parameters they
C      use as input have been set up.)
C
C---- Be sure of VM - mass-motion velocity
C     (MUST be preceded by HEDRO)
      call HIGRI (X(JJVM), X, N, W(IVEC), W(IVET), KVLG)
C
C---- Be sure of VXS - Source function expansion velocity
      call HIGRU (X(JJVXS), X(JJVXI), X(JJVM), X, N, CVXS, W(IVEC),
     $            IQEXA)
C
C---- Be sure of VSB - Sobolev velocity
      call HOGRO (X(JJVSB), X(JJVM), X(JJVXS), X, N, CVSB, CVXS,
     $            W(IVEC), NVSB, KVSB)
C
C---- (Give back W & IW allotments)
      call WGIVE (W , 'SAUCE')
      call IGIVE (IW, 'SAUCE')
C     !END
      call BYE ('SAUCE')
C
      return
      end
