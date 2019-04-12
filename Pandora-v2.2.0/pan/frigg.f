      subroutine FRIGG
     $(X,W,IW,PB,CB,XLYB,KABS,KEMIT,KODE,KTRU)
C
C     Rudolf Loeser, 1986 Jul 12
C---- Calculates Continuum Absorption and Emission, for
C     current Continuum Data Block.
C
C     KODE = 3 for current line (prd, FDB)
C     KODE = 2 for Lyman
C     KODE = 1 for everything else
C
C     PB   is the buffer for the Population Data Block;
C     CB   is the buffer for the Continuum Data Block;
C     XLYB is the H Ly lines data block.
C     !DASH
      save
C     !DASH
      real*8 CB, CORE, EMU, PB, W, X, XLM, XLP, XLYB
      integer IALO, ICABS, ICEMI, IIFLAG, IIH, IISWA, IISWE, IJOP, IN,
     $        IOREM, IORES, IS, ISLO, ISREM, ISRES, IVEX, IW, IWS,
     $        IXFRQ, JJABD, JJADT, JJALD, JJALK, JJARA, JJARC, JJARK,
     $        JJBHM, JJBNL, JJBNU, JJCHN, JJCQA, JJCQT, JJDFD, JJEPD,
     $        JJH1, JJH2N, JJHND, JJLDR, JJLDT, JJLMD, JJLXX, JJNCO,
     $        JJOHN, JJTDN, JJTE, JJV, JJVXS, JJWVA, JJWVC, JJWVK,
     $        JJXNC, JJXNE, JJXNU, JN, KABS, KEMIT, KISLV, KJKONT,
     $        KJOPAC, KKALBD, KKB, KKBHS, KKBHS1, KKBHSD, KKBHSN,
     $        KKBHSR, KKBNMS, KKBULT, KKCAPP, KKCAPR, KKCB, KKCKSM,
     $        KKCLO, KKCO, KKISLV, KKLAMD, KKLAMP, KKLTIT, KKMULT,
     $        KKRESN, KKS1, KKSCAT, KKSIGM, KKSIGS, KKSR, KKT1, KKTR,
     $        KODE, KRESN, KTRU, MOX, MUX, N, jummy
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 12),JJV  )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(140),JJH2N)
      equivalence (IZOQ(184),JJNCO)
      equivalence (IZOQ( 86),JJBHM)
      equivalence (IZOQ(100),JJLXX)
      equivalence (IZOQ( 62),JJTDN)
      equivalence (IZOQ( 96),JJLMD)
      equivalence (IZOQ( 99),JJEPD)
      equivalence (IZOQ( 97),JJDFD)
      equivalence (IZOQ( 98),JJALD)
      equivalence (IZOQ( 90),JJLDT)
      equivalence (IZOQ( 91),JJADT)
      equivalence (IZOQ( 92),JJABD)
      equivalence (IZOQ(127),JJWVK)
      equivalence (IZOQ( 42),JJALK)
      equivalence (IZOQ(128),JJARK)
      equivalence (IZOQ(157),JJWVC)
      equivalence (IZOQ(158),JJARC)
      equivalence (IZOQ(227),JJWVA)
      equivalence (IZOQ(228),JJARA)
      equivalence (IZOQ(160),JJBNL)
      equivalence (IZOQ(161),JJBNU)
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ(232),JJCQT)
      equivalence (IZOQ(233),JJCQA)
      equivalence (IZOQ(149),JJXNC)
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ(124),JJH1 )
      equivalence (IZOQ(101),JJLDR)
      equivalence (IZOQ(268),JJCHN)
      equivalence (IZOQ(269),JJOHN)
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(32),KKRESN)
      equivalence (KKK(33),KKISLV)
      equivalence (KKK( 2),KKMULT)
      equivalence (KKK(21),KKCB  )
      equivalence (KKK(19),KKCO  )
      equivalence (KKK( 1),KKLTIT)
      equivalence (KKK(47),KKCLO )
      equivalence (KKK(45),KKSIGS)
      equivalence (KKK(28),KKCAPR)
      equivalence (KKK(29),KKCAPP)
      equivalence (KKK(30),KKSIGM)
      equivalence (KKK( 7),KKSCAT)
      equivalence (KKK(22),KKT1  )
      equivalence (KKK(23),KKTR  )
      equivalence (KKK(24),KKS1  )
      equivalence (KKK(25),KKSR  )
      equivalence (KKK(10),KKBHS )
      equivalence (KKK( 9),KKBHSD)
      equivalence (KKK( 8),KKBHSN)
      equivalence (KKK(26),KKBHS1)
      equivalence (KKK(27),KKBHSR)
      equivalence (KKK(16),KKBNMS)
      equivalence (KKK( 5),KKCKSM)
      equivalence (KKK(54),KKLAMD)
      equivalence (KKK(55),KKLAMP)
      equivalence (KKK(31),KKALBD)
      equivalence (KKK(39),KKBULT)
      equivalence (KKK(15),KKB   )
C     !EJECT
C---- OPACITY     as of 2007 Jan 12
C     Paraphernalia for background absorption/emission contributors.
C
C     (Must recompile BARE, BRACE, FORAGER & SHARI when changing NABS!)
      parameter   (NABS=45)
C
      integer     NABS,NOPAC,KOPAC,TOPAC,LOPAC
      character   CNAME*24,SYMID*1,SHNAM*6
      dimension   KOPAC(NABS),LOPAC(NABS),SYMID(NABS)
      dimension   CNAME(NABS),SHNAM(NABS)
C
      common      /OPAC1/ NOPAC
      common      /OPAC2/ KOPAC
      common      /OPAC3/ LOPAC
      common      /OPAC4/ CNAME
      common      /OPAC5/ SYMID
      common      /OPAC6/ SHNAM
C
C     NOPAC = number of contributors
C     KOPAC = contributor status switch: 0 = don't use, 1 = use
C     CNAME = name (description) of contributor
C             NOTE: This sequence of names establishes the indices or
C                   ordinals by which the contributors are also known.
C     SHNAM = "short" name of contributor
C     LOPAC = "printout order" list of contributor ordinals
C     SYMID = scratch space for symbolic identifiers
C     .
C     !DASH
      external  CONTDI, JUNEBUG, SHELF, GODWIT, CHEVRE, FULGOR, MERIDA,
     $          MIIKO, CONRAD, GROAN, MOVE1, WGIVE, IGIVE, HI, BYE
      intrinsic abs
C
      dimension X(*), W(*), IW(*)
C
C               PB(Lenpbl), CB(Miklen), XLYB(Lenlyb)
      dimension PB(*),      CB(*),      XLYB(*)
C
      dimension IN(11)
      equivalence
     $(IN( 1),IIH   ),(IN( 2),ISREM ),(IN( 3),ISRES ),(IN( 4),ISLO  ),
     $(IN( 5),IORES ),(IN( 6),IOREM ),(IN( 7),IXFRQ ),(IN( 8),IVEX  ),
     $(IN( 9),IALO  ),(IN(10),ICABS ),(IN(11),ICEMI )
C
      dimension JN(3)
      equivalence
     $(JN( 1),IISWA ),(JN( 2),IISWE ),(JN( 3),IJOP  )
C     !EJECT
C
      call HI ('FRIGG')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call MIIKO   (IN, IS , MOX, 'FRIGG')
      call MERIDA  (JN, IWS, MUX, 'FRIGG')
C
      call CONRAD  (KTRU, jummy, jummy, KJOPAC, jummy, jummy, KJKONT)
      call CONTDI  (CB(KJKONT), 1, NOPAC, IW(IJOP) , 1, NOPAC)
      call CHEVRE  (N, EMU, W(IVEX))
      call JUNEBUG (CB(KKLTIT), X(JJXNU), CORE)
C
      XLM = abs(CB(KKLAMD))
      XLP = abs(CB(KKLAMP))
C
      call FULGOR  (XLM, CB(KKCKSM))
C
      call GROAN   (X, XLM)
C
      KISLV = CB(KKISLV)
      KRESN = CB(KKRESN)
C
      call GODWIT  (W, IW, KABS, KEMIT, KODE, IIFLAG, KRESN, KISLV,
     $              N, NOPAC, XLM, XLP, CORE, CB(KKMULT), CB(KKBULT),
     $              CB(KKCB), CB(KKCO), CB(KJOPAC), CB(KKCAPR),
     $              CB(KKCAPP), CB(KKSIGM), CB(KKSCAT), CB(KKT1),
     $              CB(KKTR), CB(KKB), CB(KKS1), CB(KKSR), CB(KKBHS),
     $              CB(KKBHSD), CB(KKBHSN), CB(KKBHS1), CB(KKBHSR),
     $              CB(KKSIGS), CB(KKBNMS), CB(KKCKSM), CB(KKALBD),
     $              CB(KKCLO), X(JJTE), X(JJV), X(JJVXS), X(JJXNE),
     $              X(JJHND), X(JJH2N), X(JJBHM), X(JJNCO), X(JJCHN),
     $              X(JJOHN), X(JJLXX), X(JJLDR), X(JJTDN), X(JJLMD),
     $              X(JJEPD), X(JJDFD), X(JJALD), X(JJLDT), X(JJADT),
     $              X(JJABD), X(JJWVK), X(JJALK), X(JJARK), X(JJWVC),
     $              X(JJARC), X(JJWVA), X(JJARA), X(JJBNL), X(JJBNU),
     $              X(JJCQT), X(JJCQA), X(JJXNC), PB, IW(IISWA),
     $              IW(IISWE), W(IIH), W(IORES), W(IOREM), W(ISRES),
     $              W(ISREM), W(ISLO), W(IALO), W(IXFRQ), XLYB,
     $              IW(IJOP), EMU, W(IVEX), X(JJH1), W(ICABS),
     $              W(ICEMI))
C
      call SHELF   (CB, KTRU, IIFLAG, NOPAC, IW(IISWA), IW(IISWE))
C
C     (Give back W & IW allotments)
      call WGIVE   (W , 'FRIGG')
      call IGIVE   (IW, 'FRIGG')
C     !END
      call BYE ('FRIGG')
C
      return
      end
