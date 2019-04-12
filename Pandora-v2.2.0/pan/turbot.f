      subroutine TURBOT
     $(X,IX,W,IW,XPBL,Z,XND,XNK,VM,ZT,HND,TE,XNE,H2N,RHEAB,SA,RKI,CKI,
     $ CQSI,GVL,GVI,IMG,LUD)
C
C     Rudolf Loeser, 1987 Oct 05
C---- Supervises calculations of ambipolar diffusion and velocity
C     gradient terms,
C     for statistical equilibrium and related calculations.
C     May also recompute one or more of XNK, XND, RHEAB, and VM.
C     !DASH
      save
C     !DASH
      real*8 CKI, CQSI, GVI, GVL, H2N, HND, RHEAB, RKI, SA, TE, VM, W,
     $       X, XND, XNE, XNK, XPBL, Z, ZT
      integer IALFA, IBETA, IBETR, IDZB, IDZZ, IG1, IGHI, IGHL, IGRF,
     $        IGVCK, IGVL1, IGVO, IGX1M, IGXI, IGXL, IH1, IHE1, IHE21,
     $        IHE2K, IHEDF, IHEK, IHEND, IHK, IKZAS, IMG, IN, INDL,
     $        INKL, IPKL, IPLK, IRGVL, IRHAB, IRND, IRNDU, IS, ISHE,
     $        ISHE2, ISN1V, ISNKV, ISPKL, IVEC, IVM, IVV1, IVV2, IW,
     $        IWS, IX, IXION, IZETA, IZXG, JJDIO, JJDLV, JJFMV, JJKZA,
     $        JJKZU, JJPAB, JJPBA, JJPBG, JJPGB, JJPNF, JJRAB, JJV1,
     $        JJV2, JJV3, JJVAD, JJVAM, JJVBM, JJVCM, JJVDM, JJVEL,
     $        JJVHA, JJVPR, JJVXI, JJVXS, JJZ1, JJZ2, JJZ3, JJZI, JJZIO,
     $        JN, KAMB, KDFA, KDGV, KDZIN, KINOUT, KVLG, LUD, MN1, MOX,
     $        MUX, N, N1NUP, NL
      logical HE2SIM, NSOK
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
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
      equivalence (KZQ(101),MN1  )
      equivalence (KZQ( 47),IHEDF)
      equivalence (KZQ(171),N1NUP)
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
      equivalence (LEST(32),KAMB )
      equivalence (LEST(47),KVLG )
      equivalence (LEST(39),KDGV )
      equivalence (LEST(49),KDFA )
      equivalence (LEST(76),KDZIN)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(180),JJVAM)
      equivalence (IZOQ(187),JJVPR)
      equivalence (IZOQ(188),JJVHA)
      equivalence (IZOQ(192),JJVEL)
      equivalence (IZOQ(193),JJV1 )
      equivalence (IZOQ(194),JJV2 )
      equivalence (IZOQ(195),JJV3 )
      equivalence (IZOQ(196),JJVBM)
      equivalence (IZOQ(197),JJVCM)
      equivalence (IZOQ(198),JJVDM)
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ(202),JJVXI)
      equivalence (IZOQ(177),JJVAD)
      equivalence (IZOQ(179),JJZI )
      equivalence (IZOQ(203),JJZIO)
      equivalence (IZOQ(199),JJZ1 )
      equivalence (IZOQ(200),JJZ2 )
      equivalence (IZOQ(201),JJZ3 )
      equivalence (IZOQ(225),JJFMV)
      equivalence (IZOQ( 53),JJRAB)
      equivalence (IZOQ( 68),JJPNF)
      equivalence (IZOQ( 73),JJPAB)
      equivalence (IZOQ(104),JJPBA)
      equivalence (IZOQ(113),JJPBG)
      equivalence (IZOQ(114),JJPGB)
      equivalence (IZOQ(209),JJDIO)
      equivalence (IZOQ(210),JJDLV)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ( 14),JJKZA)
      equivalence (JZOQ( 15),JJKZU)
C     !EJECT
      external TARPON, PIDDOCK, BARNET, HARLECH, ROTBUR, BOGACH, WGIVE,
     $         ANIAN, UBOM, BRUTTO, ULTAMI, NAZIR, KOILS, ZERO1, IGIVE,
     $         KUNTUR, PILLAGE, HOOPOE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               GVI(N), XND(N,NL), XNK(N), VM(N), ZT(N), HND(N), TE(N),
      dimension GVI(*), XND(*),    XNK(*), VM(*), ZT(*), HND(*), TE(*),
C
C               RKI(N,NSL), H2N(N), GVL(N,NL), SA(N), RHEAB(N), IMG(N),
     $          RKI(*),     H2N(*), GVL(*),    SA(*), RHEAB(*), IMG(*),
C
C               CQSI(N,NSL), CKI(N,NSL), Z(N), XNE(N), XPBL(Lenpbl)
     $          CQSI(*),     CKI(*),     Z(*), XNE(*), XPBL(*)
C
      dimension IN(42)
      equivalence
     $(IN( 1),IGHL  ),(IN( 2),IGHI  ),(IN( 3),IGXL  ),(IN( 4),IGXI  ),
     $(IN( 5),IVEC  ),(IN( 6),IDZB  ),(IN( 7),IDZZ  ),(IN( 8),IG1   ),
     $(IN( 9),IBETA ),(IN(10),IHK   ),(IN(11),IH1   ),(IN(12),IHEK  ),
     $(IN(13),IHE1  ),(IN(14),IHE2K ),(IN(15),IHE21 ),(IN(16),IGVL1 ),
     $(IN(17),IGVCK ),(IN(18),ISHE  ),(IN(19),IZETA ),(IN(20),IRND  ),
     $(IN(21),IXION ),(IN(22),ISNKV ),(IN(23),IALFA ),(IN(24),IGX1M ),
     $(IN(25),IGRF  ),(IN(26),ISN1V ),(IN(27),IRNDU ),(IN(28),IPLK  ),
     $(IN(29),IPKL  ),(IN(30),ISPKL ),(IN(31),IHEND ),(IN(32),IVM   ),
     $(IN(33),IRHAB ),(IN(34),IZXG  ),(IN(35),IRGVL ),(IN(36),INKL  ),
     $(IN(37),INDL  ),(IN(38),IVV1  ),(IN(39),IVV2  ),(IN(40),ISHE2 ),
     $(IN(41),IBETR ),(IN(42),IGVO  )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IKZAS )
C     !EJECT
C
      call HI ('TURBOT')
C     !BEG
C---- Initialize
      KDGV = 0
      KDFA = 0
      call ZERO1 (GVL     , (N*NL))
      call ZERO1 (GVI     , N     )
      call ZERO1 (X(JJVAM), N     )
      call ZERO1 (X(JJVCM), N     )
      call ZERO1 (X(JJVDM), N     )
      call ZERO1 (X(JJVPR), N     )
      call ZERO1 (X(JJVHA), N     )
      call ZERO1 (X(JJVEL), N     )
      call ZERO1 (X(JJV1 ), N     )
      call ZERO1 (X(JJV2 ), N     )
      call ZERO1 (X(JJV3 ), N     )
      call ZERO1 (X(JJZI ), N     )
      call ZERO1 (X(JJZ1 ), N     )
      call ZERO1 (X(JJZ2 ), N     )
      call ZERO1 (X(JJZ3 ), N     )
      call ZERO1 (X(JJZIO), N     )
      call ZERO1 (X(JJVAD), N     )
C
      if(((KAMB.gt.0).or.(KVLG.gt.0)).and.(KDZIN.eq.0)) then
C       (Get, and allocate, W & IW allotments)
        call PIDDOCK (IN, IS,  MOX, 'TURBOT')
        call PILLAGE (JN, IWS, MUX, 'TURBOT')
C
C----   Output heading
        call NAZIR   (LUD, KAMB, KVLG)
C----   Set up "local ND and NK," and populations data
        call UBOM    (KAMB, KVLG, XNK, XND, W(INKL), W(INDL), IHEDF,
     $                XPBL, N, NL, HND, W(IH1), W(IHK), W(IHEND), RHEAB,
     $                W(IHE1), W(IHEK), W(IBETA), W(IHE21), W(IHE2K),
     $                W(ISHE), W(ISHE2), X(JJPNF), W(IVEC), LUD)
C----   Validate populations data; skip if bad
        call HOOPOE  (N, NL, IHEDF, W(INKL), W(INDL), W(IH1), W(IHK),
     $                W(IHE1), W(IHEK), W(IHE21), W(IHE2K), NSOK)
        if(.not.NSOK) then
          goto 100
        end if
C
C----   Set up HE2SIM switch
        call ROTBUR  (KAMB, KVLG, HE2SIM)
C----   Initialize internal VM-table, and FCV
        call ANIAN   (X, W, N, RHEAB, X(JJVBM), W(IVM))
C----   Determine flow regime
        call KOILS   (W(IVM), MN1, KINOUT)
C----   Set up RHAB
        call HARLECH (N, X(JJRAB), W(IRHAB))
C     !EJECT
C----   Compute
        call TARPON  (X, W, IW, KAMB, KVLG, W(INDL), W(INKL), Z,
     $                W(IVM), ZT, HND, TE, XNE, H2N, RHEAB, SA, RKI,
     $                CKI, CQSI, GVL, GVI, KINOUT, HE2SIM, IMG, LUD,
     $                W(ISHE), W(IHK), W(IH1), W(IHEK), W(IHE1),
     $                W(IHE2K), W(IHE21), X(JJVAM), X(JJVBM), X(JJVCM),
     $                X(JJVDM), X(JJVPR), X(JJVHA), X(JJVEL), X(JJV1),
     $                X(JJV2), X(JJV3), X(JJZI), X(JJZ1), X(JJZ2),
     $                X(JJZ3), X(JJZIO), X(JJFMV), X(JJRAB), W(IXION),
     $                W(IGHL), W(IGHI), W(IGXL), W(IGXI), W(IG1),
     $                W(IGVL1), W(IGVCK), W(IRND), W(IALFA), W(IGX1M),
     $                W(IZETA), W(IDZZ), W(IDZB), W(IBETA), W(IGRF),
     $                W(ISN1V), W(ISNKV), W(IRNDU), W(IPLK), W(IPKL),
     $                W(ISPKL), W(IHEND), W(IZXG), X(JJPNF), X(JJPAB),
     $                X(JJPBA), X(JJPBG), X(JJPGB), W(IRHAB), W(IVEC),
     $                W(IRGVL), XPBL, X(JJDIO), X(JJDLV), W(ISHE2),
     $                W(IBETR), W(IGVO), IX(JJKZA), IW(IKZAS),
     $                IX(JJKZU))
C----   Move "local ND and NK" into populations-of-the-run, and
C       update Population Data blocks and checksums ---(if needed)
        call BOGACH  (N1NUP, KAMB, KVLG, HE2SIM, N, NL, XNK, XND,
     $                W(INKL), W(INDL), W(IHEK), W(IHE1), XPBL, LUD)
C----   Reset KDGV and KDFA, according to the values of GVL and GVI
        call BRUTTO  (KDGV, KDFA, N, NL, GVI, GVL)
C----   Update VM, compute VADD, and update VXS --- as needed
        call BARNET  (N, VM, W(IVM))
        call KUNTUR  (KAMB, KVLG, N, VM, X(JJVHA), X(JJV1), X(JJV2),
     $                X(JJVXI), X(JJVXS), W(IVV1), W(IVV2), X(JJVAD))
C----   Output trailer
        call ULTAMI  (LUD, 'DIFFUSION')
C
  100   continue
C       (Give back W & IW allotments)
        call WGIVE   (W,  'TURBOT')
        call IGIVE   (IW, 'TURBOT')
      end if
C     !END
      call BYE ('TURBOT')
C
      return
      end
