      subroutine MOCHA
     $(QNAME,J,X,LZA,ZAUX,W)
C
C     Rudolf Loeser, 1968 Apr 19
C---- Reads floating vectors.
C     !DASH
      save
C     !DASH
      real*8 W, X, ZAUX
      integer INK, J, JJ304, JJABD, JJABK, JJACI, JJADT, JJAEL, JJAHM,
     $        JJAL, JJALD, JJBHM, JJBHZ, JJBNY, JJBXI, JJCOL, JJCP,
     $        JJCQA, JJCQT, JJDDR, JJDFD, JJDGM, JJDWV, JJEP1, JJEP2,
     $        JJEPD, JJEQT, JJFIN, JJFKR, JJFNA, JJFNB, JJFRR, JJGK,
     $        JJGMZ, JJHEA, JJHND, JJHNV, JJICO, JJICR, JJLCR, JJLDR,
     $        JJLDT, JJLHM, JJLMD, JJLMM, JJLXX, JJMCI, JJMLC, JJMSR,
     $        JJNK, JJP, JJPAB, JJPBA, JJPBG, JJPGB, JJQIN, JJRAB,
     $        JJRBL, JJRKX, JJRZM, JJSCW, JJSWV, JJTDN, JJTE, JJTER,
     $        JJTEX, JJTKI, JJTR, JJTS, JJV, JJVBM, JJVM, JJVNH, JJVR,
     $        JJVSB, JJVT, JJVXI, JJVXS, JJWAV, JJWCU, JJWNU, JJXCA,
     $        JJXCB, JJXCU, JJXDR, JJXIB, JJXIN, JJXIR, JJXIS, JJXK,
     $        JJXMU, JJXNC, JJXNE, JJXNU, JJYCR, JJYDT, JJYHM, JJYK,
     $        JJYLM, JJYWA, JJZ, JJZBK, JJZGM, JJZME, JM, KB, KBX, KK,
     $        KNW, KR, KS, LDU, LG, LLY, LZA, M, MHM, MQT, MRR, N, NAB,
     $        NCB, NCL, NCQ, NCR, NDR, NDT, NDV, NFL, NGM, NKA, NL, NSL,
     $        NSW, NTE, NVH, NWS, NWV
      character QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 3),M  )
      equivalence (JZQ(36),KS )
      equivalence (JZQ(40),NSL)
      equivalence (JZQ(20),NTE)
      equivalence (JZQ(15),MRR)
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(21),NDT)
      equivalence (JZQ(37),KR )
      equivalence (JZQ(10),KK )
      equivalence (JZQ(38),KB )
      equivalence (JZQ(53),NCQ)
      equivalence (JZQ(17),NWV)
      equivalence (JZQ(43),NDV)
      equivalence (JZQ(26),LDU)
      equivalence (JZQ(27),LLY)
      equivalence (JZQ(35),INK)
      equivalence (JZQ(32),NCR)
      equivalence (JZQ(39),MQT)
      equivalence (JZQ(11),KBX)
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(41),NDR)
      equivalence (JZQ(25),KNW)
      equivalence (JZQ(34),LG )
      equivalence (JZQ(45),NAB)
      equivalence (JZQ(50),NKA)
      equivalence (JZQ(51),NCL)
      equivalence (JZQ(23),NWS)
      equivalence (JZQ(54),NVH)
      equivalence (JZQ(55),NCB)
      equivalence (JZQ( 6),NSW)
      equivalence (JZQ(24),JM )
      equivalence (JZQ(16),NFL)
      equivalence (JZQ(22),MHM)
      equivalence (JZQ(58),NGM)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 22),JJTS )
      equivalence (IZOQ(146),JJXIS)
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ(220),JJWNU)
      equivalence (IZOQ( 27),JJP  )
      equivalence (IZOQ( 28),JJCP )
      equivalence (IZOQ( 79),JJTER)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(  8),JJTR )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ( 12),JJV  )
      equivalence (IZOQ(135),JJFRR)
      equivalence (IZOQ( 91),JJADT)
      equivalence (IZOQ(148),JJXIR)
      equivalence (IZOQ(  5),JJ304)
      equivalence (IZOQ(150),JJXIB)
      equivalence (IZOQ(105),JJEP1)
      equivalence (IZOQ(204),JJACI)
      equivalence (IZOQ( 52),JJRZM)
      equivalence (IZOQ( 49),JJVT )
      equivalence (IZOQ( 53),JJRAB)
      equivalence (IZOQ( 65),JJWAV)
      equivalence (IZOQ( 62),JJTDN)
      equivalence (IZOQ(172),JJVSB)
      equivalence (IZOQ( 78),JJYWA)
      equivalence (IZOQ( 80),JJAEL)
      equivalence (IZOQ( 89),JJYDT)
      equivalence (IZOQ( 84),JJLHM)
      equivalence (IZOQ( 85),JJAHM)
      equivalence (IZOQ( 86),JJBHM)
      equivalence (IZOQ( 87),JJYHM)
      equivalence (IZOQ( 90),JJLDT)
      equivalence (IZOQ( 93),JJLMM)
      equivalence (IZOQ( 94),JJMLC)
      equivalence (IZOQ(152),JJDWV)
      equivalence (IZOQ( 96),JJLMD)
      equivalence (IZOQ( 97),JJDFD)
      equivalence (IZOQ( 98),JJALD)
      equivalence (IZOQ( 99),JJEPD)
      equivalence (IZOQ(100),JJLXX)
      equivalence (IZOQ(101),JJLDR)
      equivalence (IZOQ( 92),JJABD)
      equivalence (IZOQ( 14),JJXIN)
      equivalence (IZOQ( 15),JJFIN)
      equivalence (IZOQ( 61),JJYLM)
      equivalence (IZOQ(112),JJGK )
      equivalence (IZOQ(116),JJLCR)
      equivalence (IZOQ(117),JJICR)
      equivalence (IZOQ(118),JJYCR)
      equivalence (IZOQ( 50),JJNK )
      equivalence (IZOQ(  1),JJEQT)
      equivalence (IZOQ(  2),JJEP2)
      equivalence (IZOQ(166),JJMCI)
      equivalence (IZOQ(239),JJSCW)
      equivalence (IZOQ(254),JJBXI)
      equivalence (IZOQ( 34),JJAL )
      equivalence (IZOQ( 38),JJMSR)
      equivalence (IZOQ( 46),JJXDR)
      equivalence (IZOQ( 47),JJDDR)
      equivalence (IZOQ( 64),JJFKR)
      equivalence (IZOQ( 71),JJTEX)
      equivalence (IZOQ( 75),JJTKI)
      equivalence (IZOQ( 82),JJQIN)
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ(202),JJVXI)
      equivalence (IZOQ(130),JJXMU)
      equivalence (IZOQ(162),JJBNY)
      equivalence (IZOQ(170),JJZBK)
      equivalence (IZOQ(171),JJABK)
      equivalence (IZOQ(165),JJVR )
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(183),JJCOL)
      equivalence (IZOQ(185),JJSWV)
      equivalence (IZOQ(181),JJVM )
      equivalence (IZOQ( 41),JJVNH)
      equivalence (IZOQ(206),JJHNV)
      equivalence (IZOQ(219),JJHEA)
      equivalence (IZOQ(221),JJXCA)
      equivalence (IZOQ(222),JJXCB)
      equivalence (IZOQ( 48),JJZME)
      equivalence (IZOQ(231),JJICO)
      equivalence (IZOQ(232),JJCQT)
      equivalence (IZOQ(233),JJCQA)
      equivalence (IZOQ(196),JJVBM)
      equivalence (IZOQ( 73),JJPAB)
      equivalence (IZOQ(104),JJPBA)
      equivalence (IZOQ(113),JJPBG)
      equivalence (IZOQ(114),JJPGB)
      equivalence (IZOQ(149),JJXNC)
      equivalence (IZOQ(189),JJRBL)
      equivalence (IZOQ(123),JJXK )
      equivalence (IZOQ( 72),JJYK )
      equivalence (IZOQ(  6),JJRKX)
      equivalence (IZOQ(256),JJFNA)
      equivalence (IZOQ(258),JJFNB)
      equivalence (IZOQ(260),JJXCU)
      equivalence (IZOQ(261),JJWCU)
      equivalence (IZOQ(151),JJDGM)
      equivalence (IZOQ(230),JJZGM)
      equivalence (IZOQ(263),JJGMZ)
      equivalence (IZOQ(266),JJBHZ)
C     !DASH
C     !EJECT
      external BASIL, CREAM, HI, BYE
C
      dimension X(*), W(*)
C
C               LZA(50), ZAUX(LZM,NZM)
      dimension LZA(*),  ZAUX(*)
C
      call HI ('MOCHA')
C     !BEG
      goto (
C
C       TS       XI       NU       P        CP       TER      TE
     $  101,     102,     103,     104,     105,     106,     107,
C
C       TR       XNE      HND      V        FRR      Z        ADT
     $  108,     109,     110,     111,     112,     113,     114,
C
C       XISYM    XIRED    HE304    XK       XIBLU    ZALBK    EP1
     $  115,     116,     117,     118,     119,     120,     121,
C
C       BXI      RZM      VT       RABD     WAVES    TDUST    YK
     $  122,     123,     124,     125,     126,     127,     128,
C
C       DGM      YWAVE    AEL      YLDT     LHM      AHM      BDHM
     $  129,     130,     131,     132,     133,     134,     135,
C
C       YHM      LDT      XLM      XMLC     DWAVE    XLMDUST  DFDUST
     $  136,     137,     138,     139,     140,     141,     142,
C
C       ALBDUST  EPDUST   LMXX     LMDR     ALBK     VSB      ALBDT
     $  143,     144,     145,     146,     147,     148,     149,
C
C       XINK     FINK     YLYM     VBMB     PALBET   GK       LCR
     $  150,     151,     152,     153,     154,     155,     156,
C
C       ICR      YCR      NK       QTAIL    EP2      WNUC     ZGM
     $  157,     158,     159,     160,     161,     162,     163,
C
C       DGMZ     XCOL     AL       ZMASS    XDR      DDR      FKUR
     $  164,     165,     166,     167,     168,     169,     170,
C
C       TEX      TAUKIN   QIN               VXS      XMU      BANDY
     $  171,     172,     173,     174,     175,     176,     177,
C
C       DELWAVE  VR       PBETAL   PBETGM   VM       VNH      HNDV
     $  178,     179,     180,     181,     182,     183,     184,
C
C       RHEAB    WNU      LCOA     LCOB     PGMBET   ZME      CONI
     $  185,     186,     187,     188,     189,     190,     191,
C
C       CQT      CQA      XNC      RABDL    CMCI     CACI     SCOW
     $  192,     193,     194,     195,     196,     197,     198,
C
C       RKMULT   FNRMLA   FNRMLB   NUC      BHORIZ
     $  199,     200,     201,     202,     203
C
     $ ), J
C
  101 continue
        call BASIL   (X(JJTS), M, QNAME)
        goto 400
  102 continue
        call BASIL   (X(JJXIS), KS, QNAME)
        goto 400
  103 continue
        call BASIL   (X(JJXNU), NSL, QNAME)
        goto 400
  104 continue
        call BASIL   (X(JJP), NSL, QNAME)
        goto 400
  105 continue
        call BASIL   (X(JJCP), (NSL+1), QNAME)
        goto 400
  106 continue
        call BASIL   (X(JJTER), NTE, QNAME)
        goto 400
  107 continue
        call CREAM   (X(JJTE), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  108 continue
        call CREAM   (X(JJTR), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  109 continue
        call CREAM   (X(JJXNE), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  110 continue
        call CREAM   (X(JJHND), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  111 continue
        call CREAM   (X(JJV), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  112 continue
        call BASIL   (X(JJFRR), MRR, QNAME)
        goto 400
  113 continue
        call BASIL   (X(JJZ), N, QNAME)
        goto 400
  114 continue
        call BASIL   (X(JJADT), NDT, QNAME)
        goto 400
  115 continue
        call BASIL   (X(JJXIS), KS, QNAME)
        goto 400
  116 continue
        call BASIL   (X(JJXIR), KR, QNAME)
        goto 400
  117 continue
        call CREAM   (X(JJ304), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  118 continue
        call BASIL   (X(JJXK), KK, QNAME)
        goto 400
  119 continue
        call BASIL   (X(JJXIB), KB, QNAME)
        goto 400
  120 continue
        call BASIL   (X(JJZBK), NKA, QNAME)
        goto 400
  121 continue
        call CREAM   (X(JJEP1), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  122 continue
        call BASIL   (X(JJBXI), KBX, QNAME)
        goto 400
  123 continue
        call CREAM   (X(JJRZM), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  124 continue
        call CREAM   (X(JJVT), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  125 continue
        call CREAM   (X(JJRAB), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  126 continue
        call BASIL   (X(JJWAV), NWV, QNAME)
        goto 400
  127 continue
        call CREAM   (X(JJTDN), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  128 continue
        call BASIL   (X(JJYK), NSL, QNAME)
        goto 400
  129 continue
        call CREAM   (X(JJDGM), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  130 continue
        call BASIL   (X(JJYWA), NWV, QNAME)
        goto 400
  131 continue
        call CREAM   (X(JJAEL), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  132 continue
        call BASIL   (X(JJYDT), NDT, QNAME)
        goto 400
  133 continue
        call BASIL   (X(JJLHM), MHM, QNAME)
        goto 400
  134 continue
        call BASIL   (X(JJAHM), MHM, QNAME)
        goto 400
  135 continue
        call CREAM   (X(JJBHM), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  136 continue
        call BASIL   (X(JJYHM), MHM, QNAME)
        goto 400
  137 continue
        call BASIL   (X(JJLDT), NDT, QNAME)
        goto 400
  138 continue
        call BASIL   (X(JJLMM), JM, QNAME)
        goto 400
  139 continue
        call BASIL   (X(JJMLC), JM, QNAME)
        goto 400
  140 continue
        call BASIL   (X(JJDWV), NDV, QNAME)
        goto 400
  141 continue
        call BASIL   (X(JJLMD), LDU, QNAME)
        goto 400
  142 continue
        call BASIL   (X(JJDFD), LDU, QNAME)
        goto 400
  143 continue
        call BASIL   (X(JJALD), LDU, QNAME)
        goto 400
  144 continue
        call BASIL   (X(JJEPD), LDU, QNAME)
        goto 400
  145 continue
        call BASIL   (X(JJLXX), LLY, QNAME)
        goto 400
  146 continue
        call BASIL   (X(JJLDR), LLY, QNAME)
        goto 400
  147 continue
        call BASIL   (X(JJABK), NKA, QNAME)
        goto 400
  148 continue
        call CREAM   (X(JJVSB), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  149 continue
        call BASIL   (X(JJABD), NDT, QNAME)
        goto 400
  150 continue
        call BASIL   (X(JJXIN), INK, QNAME)
        goto 400
  151 continue
        call BASIL   (X(JJFIN), INK, QNAME)
        goto 400
  152 continue
        call BASIL   (X(JJYLM), KK, QNAME)
        goto 400
  153 continue
        call CREAM   (X(JJVBM), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  154 continue
        call CREAM   (X(JJPAB), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  155 continue
        call BASIL   (X(JJGK), KK, QNAME)
        goto 400
  156 continue
        call BASIL   (X(JJLCR), NCR, QNAME)
        goto 400
  157 continue
        call BASIL   (X(JJICR), NCR, QNAME)
        goto 400
  158 continue
        call BASIL   (X(JJYCR), NCR, QNAME)
        goto 400
  159 continue
        call CREAM   (X(JJNK), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  160 continue
        call BASIL   (X(JJEQT), MQT, QNAME)
        goto 400
  161 continue
        call CREAM   (X(JJEP2), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  162 continue
        call BASIL   (X(JJWCU), NSL, QNAME)
        goto 400
  163 continue
        call BASIL   (X(JJZGM), NGM, QNAME)
        goto 400
  164 continue
        call BASIL   (X(JJGMZ), NGM, QNAME)
        goto 400
  165 continue
        call BASIL   (X(JJCOL), NCL, QNAME)
        goto 400
  166 continue
        call BASIL   (X(JJAL), NL, QNAME)
        goto 400
  167 continue
        call BASIL   (X(JJMSR), N, QNAME)
        goto 400
  168 continue
        call BASIL   (X(JJXDR), NDR, QNAME)
        goto 400
  169 continue
        call BASIL   (X(JJDDR), NDR, QNAME)
        goto 400
  170 continue
        call BASIL   (X(JJFKR), KNW, QNAME)
        goto 400
  171 continue
        call CREAM   (X(JJTEX), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  172 continue
        call BASIL   (X(JJTKI), N, QNAME)
        goto 400
  173 continue
        call CREAM   (X(JJQIN), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  174 continue
        goto 400
  175 continue
        call CREAM   (X(JJVXI), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  176 continue
        call BASIL   (X(JJXMU), LG, QNAME)
        goto 400
  177 continue
        call BASIL   (X(JJBNY), NAB, QNAME)
        goto 400
  178 continue
        call BASIL   (X(JJSWV), NWS, QNAME)
        goto 400
  179 continue
        call CREAM   (X(JJVR), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  180 continue
        call CREAM   (X(JJPBA), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  181 continue
        call CREAM   (X(JJPBG), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  182 continue
        call CREAM   (X(JJVM), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  183 continue
        call BASIL   (X(JJVNH), NVH, QNAME)
        goto 400
  184 continue
        call BASIL   (X(JJHNV), NVH, QNAME)
        goto 400
  185 continue
        call CREAM   (X(JJHEA), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  186 continue
        call BASIL   (X(JJWNU), NSL, QNAME)
        goto 400
  187 continue
        call BASIL   (X(JJXCA), NCB, QNAME)
        goto 400
  188 continue
        call BASIL   (X(JJXCB), NCB, QNAME)
        goto 400
  189 continue
        call CREAM   (X(JJPGB), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  190 continue
        call CREAM   (X(JJZME), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  191 continue
        call CREAM   (X(JJICO), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  192 continue
        call BASIL   (X(JJCQT), NCQ, QNAME)
        goto 400
  193 continue
        call BASIL   (X(JJCQA), NCQ, QNAME)
        goto 400
  194 continue
        call CREAM   (X(JJXNC), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  195 continue
        call CREAM   (X(JJRBL), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
        goto 400
  196 continue
        call BASIL   (X(JJMCI), NSL, QNAME)
        goto 400
  197 continue
        call BASIL   (X(JJACI), NSL, QNAME)
        goto 400
  198 continue
        call BASIL   (X(JJSCW), NSW, QNAME)
        goto 400
  199 continue
        call BASIL   (X(JJRKX), NSL, QNAME)
        goto 400
  200 continue
        call BASIL   (X(JJFNA), NFL, QNAME)
        goto 400
  201 continue
        call BASIL   (X(JJFNB), NFL, QNAME)
        goto 400
  202 continue
        call BASIL   (X(JJXCU), NSL, QNAME)
        goto 400
  203 continue
        call CREAM   (X(JJBHZ), QNAME, 0, 0, LZA, ZAUX, X(JJZ), W)
  400 continue
C     !END
      call BYE ('MOCHA')
C
      return
      end
