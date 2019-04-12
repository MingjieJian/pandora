      subroutine FLOSS
     $(X,IX)
C
C     Rudolf Loeser, 1980 May 21
C---- Sets up second stage defaults
C     !DASH
      save
C     !DASH
      real*8 ONE, TEN, X, YCOL, YL, YRATS
      integer IQCXU, IQLSP, IQLYM, IQOTL, IX, JJABK, JJBDI, JJBNY,
     $        JJCEI, JJCII, JJFCE, JJFKR, JJFNA, JJFNB, JJKIJ, JJMCE,
     $        JJMCI, JJMRJ, JJOLL, JJOML, JJPCE, JJR1W, JJRKS, JJRLS,
     $        JJRRC, JJWLA, JJWLB, JJWRA, JJWSL, JJYCO, JJYCR, JJYDT,
     $        JJYHM, JJYKR, JJYLM, JJYRA, JJYWA, JJZBK, KK, KNW, MHM,
     $        MLS, MUL, N, NAB, NCR, NDT, NFL, NKA, NL, NLN, NOION, NSL,
     $        NT, NTE, NWV
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
      equivalence (JZQ(50),NKA)
      equivalence (JZQ( 5),NT )
      equivalence (JZQ(17),NWV)
      equivalence (JZQ(22),MHM)
      equivalence (JZQ(10),KK )
      equivalence (JZQ(32),NCR)
      equivalence (JZQ(33),MLS)
      equivalence (JZQ(21),NDT)
      equivalence (JZQ(45),NAB)
      equivalence (JZQ(52),NLN)
      equivalence (JZQ(16),NFL)
      equivalence (JZQ(25),KNW)
      equivalence (JZQ(20),NTE)
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
      equivalence (RZQ( 19),YL   )
      equivalence (RZQ( 97),YCOL )
      equivalence (KZQ( 94),NOION)
      equivalence (RZQ(168),YRATS)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 76),JJYCO)
      equivalence (IZOQ( 78),JJYWA)
      equivalence (IZOQ( 87),JJYHM)
      equivalence (IZOQ( 61),JJYLM)
      equivalence (IZOQ(118),JJYCR)
      equivalence (IZOQ(103),JJYKR)
      equivalence (IZOQ(106),JJMCE)
      equivalence (IZOQ(108),JJR1W)
      equivalence (IZOQ( 89),JJYDT)
      equivalence (IZOQ(162),JJBNY)
      equivalence (IZOQ(  3),JJOML)
      equivalence (IZOQ(182),JJOLL)
      equivalence (IZOQ( 64),JJFKR)
      equivalence (IZOQ(170),JJZBK)
      equivalence (IZOQ(171),JJABK)
      equivalence (IZOQ( 70),JJRRC)
      equivalence (IZOQ( 77),JJYRA)
      equivalence (IZOQ( 44),JJBDI)
      equivalence (IZOQ(166),JJMCI)
      equivalence (IZOQ(244),JJWRA)
      equivalence (IZOQ(248),JJFCE)
      equivalence (IZOQ(249),JJPCE)
      equivalence (IZOQ(255),JJWLA)
      equivalence (IZOQ(257),JJWLB)
      equivalence (IZOQ(256),JJFNA)
      equivalence (IZOQ(258),JJFNB)
      equivalence (IZOQ(259),JJWSL)
      equivalence (IZOQ( 29),JJCII)
      equivalence (IZOQ( 30),JJCEI)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  1),JJMRJ)
      equivalence (JZOQ(  4),JJRKS)
      equivalence (JZOQ(  5),JJRLS)
      equivalence (JZOQ(  2),JJKIJ)
C
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
C---- LIMBO       as of 2006 Jul 11
      integer     MXLEV,MAXTR,LIMTR
      parameter   (MXLEV=50)
      parameter   (MAXTR=(MXLEV*(MXLEV-1))/2)
      integer     LINIU ,LINIL ,LINKLN,LINPRD,LINPRO,LINMSE,
     $            LINMSF,LINDAM,LININK,LINTPS,LININR,LINFLX,
     $            LINLDL,LININT,LINPRN,LINFDB,LINOML,LINSBG,
     $            LINKBT,LINKRT,LINKST,LINKM
C     (Remember to recompile ADAM when changing MXLEV.)
      dimension   LINIU (MAXTR), LINIL (MAXTR), LINKLN(MAXTR),
     $            LINPRD(MAXTR), LINPRO(MAXTR), LINMSE(MAXTR),
     $            LINMSF(MAXTR), LINDAM(MAXTR), LININK(MAXTR),
     $            LINTPS(MAXTR), LININR(MAXTR), LINFLX(MAXTR),
     $            LINLDL(MAXTR), LININT(MAXTR), LINPRN(MAXTR),
     $            LINFDB(MAXTR), LINOML(MAXTR), LINSBG(MAXTR),
     $            LINKBT(MAXTR), LINKRT(MAXTR), LINKST(MAXTR),
     $            LINKM (MAXTR)
C
C     Line Source Function Calculation control parameters.
C
      common      /LIMBO10/ LIMTR
C     LIMTR  -    = MAXTR
      common      /LIMBO11/ LINIU
C     LINIU  -    UPPER level index of transition or line
      common      /LIMBO12/ LINIL
C     LINIL  -    LOWER level index of transition or line
      common      /LIMBO13/ LINKLN
C     LINKLN -    line type code:
C                 =0 for NO transition,
C                 =1 for RADIATIVE transition,
C                 =2 for PASSIVE transition,
C                 =3 for OPTICALLY-THIN transition,
C                 =4 for TWO-PHOTON transition,
C                 =5 for OPTICALLY-THICK transition
      common      /LIMBO14/ LINPRD
C     LINPRD -    partial redistribution calculation control:
C                 =0 if PRD calculation IS NOT required,
C                 =1 if Kneer-Heasley PRD (option PRDMETH=off)
C                 =2 if Hubeny-Lites PRD (option PRDMETH=on)
      common      /LIMBO15/ LINPRO
C     LINPRO -    emergent profiles calculation control:
C                 =0 if NO profiles are required,
C                 =1 if only "REGULAR" profile is required,
C                 =2 if only "ECLIPSE" profile is required,
C                 =3 if BOTH types of profile are required
      common      /LIMBO16/ LINMSE
C     LINMSE -    statistical equilibrium method selector:
C                 =0 for NOVA,
C                 =1 for Complex/UPPER,
C                 =2 for Complex/LOWER,
C                 =3 for CHAIN,
C                 =4 for VAMOS
      common      /LIMBO17/ LINMSF
C     LINMSF -    line source function method selector:
C                 =0 for RT (Ray tracing),
C                 =1 for QR, DIRECT (Quadratic representation),
C                 =2 for QR, MAPPED,
C                 =3 for GR, (General ray tracing)
      common      /LIMBO18/ LINDAM
C     LINDAM -    damping parameter components selector
      common      /LIMBO19/ LININK
C     LININK -    input opacity signal:
C                 =1 if there ARE input values of continuous opacity,
C                 =0 if there ARE NOT input values of continuous opacity
      common      /LIMBO20/ LINTPS
C     LINTPS -    source function solution method selector:
C                 =0 for FULL solution (involving matrix inversion),
C                 =1 for DIRECT solution (involving level propulations),
C                 =2 for ESCAPE PROBABILITY approximation
      common      /LIMBO21/ LININR
C     LININR -    Rho recomputation control:
C                 =0 if iteratively-recalculated Rho should be used,
C                 =1 if only input values of Rho should be used
      common      /LIMBO22/ LINFLX
C     LINFLX -    Line Flux calculation control:
C                 =1 if it IS required,
C                 =0 if it IS NOT required
      common      /LIMBO23/ LINLDL
C     LINLDL -    number of component lines:
C                 =1 if this is a SINGLE line,
C                 >1 if this is a BLENDED line
      common      /LIMBO24/ LININT
C     LININT -    Source Function frequency integration range:
C                 =0 if it is a HALF profile,
C                 =1 if it is a WHOLE profile
      common      /LIMBO25/ LINPRN
C     LINPRN -    Source Function calculation printout switch:
C                 =0 for NO printout nor graph,
C                 =1 for REGULAR Source Function printout and graph
      common      /LIMBO26/ LINFDB
C     LINFDB -    Source Function calculation background data switch:
C                 =0 for CONSTANT background
C                 (line core values used for every wavelength),
C                 =1 for FREQUENCY-DEPENDENT background
C                 (values are calculated for every wavelength)
      common      /LIMBO27/ LINOML
C     LINOML -    "Line Background Opacity" control
C                 (i.e. Composite, Statistical, or Averaged Opacity):
C                 =0 if this opacity should be suppressed at the
C                    wavelengths of this transition
C                 =1 if this opacity should NOT be suppressed
      common      /LIMBO28/ LINSBG
C     LINSBG -    Blended Line profile plot mode switch
C                 =0 for plotting as blend only,
C                 =1 for additonal separate plots of each component
      common      /LIMBO29/ LINKBT
C     LINKBT -    length of input table XIBLUT
      common      /LIMBO30/ LINKRT
C     LINKRT -    length of input table XIREDT
      common      /LIMBO31/ LINKST
C     LINKST -    length of input table XISYMT
      common      /LIMBO32/ LINKM
C     LINKM  -    length of actual tables XI and DL
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
      equivalence (IQQ(331),IQCXU)
      equivalence (IQQ(287),IQLSP)
      equivalence (IQQ( 13),IQLYM)
      equivalence (IQQ(344),IQOTL)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT(11),TEN   )
C     !DASH
C     !EJECT
      external DEFAULT, HIMBERT, ONE1, DIAZ, SETI, ANTON, GUMIEL, SETD,
     $         BAHIA, ATLAS, HI, BYE
C
      dimension X(*), IX(*)
C
      call HI ('FLOSS')
C     !BEG
      call DEFAULT  (2, X)
      call DEFAULT  (3, X)
C---- Initialize ZALBK for Kurucz opacities albedo
      call HIMBERT  (NKA, X(JJZBK), X(JJABK))
C---- Set up data for simulated background H Ly lines normalization
      call BAHIA    (NLN, X(JJWSL), NFL, X(JJWLA), X(JJFNA),
     $               X(JJWLB), X(JJFNB))
C---- Initialize source function method control switches
      call DIAZ     (YL, YCOL, YRATS, X(JJYCO), NT, X(JJYWA), NWV,
     $               X(JJYHM), MHM, X(JJYLM), KK, X(JJYCR), NCR,
     $               X(JJYKR), MLS, X(JJYDT), NDT, X(JJBNY), NAB)
C---- Initialize Kurucz multipliers
      call ONE1     (X(JJFKR), KNW)
C---- Initialize CHI values (ionization potential) in ELEMENT table
      call ANTON
C     !EJECT
      if(NOION.le.0) then
        MUL = NL*(NL-1)/2
C----   Initialize line background opacity multipliers
        call ONE1    (X(JJOML), NT)
C----   Initialize line opacity multipliers
        call ONE1    (X(JJOLL), NT)
C----   Initialize rates integrations switches
        call SETI    (IX(JJRKS), 1, NSL, 1)
        call SETI    (IX(JJRLS), 1, NSL, 1)
C----   Initialize collision coefficients
        call SETD    (X(JJCII), 1, (NTE*NSL), (-ONE))
        call SETD    (X(JJCEI), 1, (NTE*MUL), (-ONE))
C----   Initialize collision rates multipliers
        call ONE1    (X(JJMCI), NSL)
        call ONE1    (X(JJMCE), MUL)
C----   Initialize CE-enhancement multiplicative increments
        call SETD    (X(JJPCE), 1, NT, TEN)
C----   Initialize departure coefficients
        call ONE1    (X(JJBDI), (N*NL))
C----   Initialize CE-enhancement
        call ONE1    (X(JJFCE), (N*NT))
C----   Initialize rates integration data
        call GUMIEL  (NSL, IX(JJMRJ), X(JJWRA), X(JJRRC), X(JJYRA))
        if(IQCXU.gt.0) then
C----     Initialize charge exchange parameters
          call ONE1  (RCHX, (NPQLM**2))
        end if
        if(IQLSP.gt.0) then
C----     Initialize LSFPRINT
          call SETI  (LINPRN, 1, MAXTR, 2)
        end if
        if(IQOTL.gt.0) then
C----     Optically-thin-limit: set all transitions "thin"
          call SETI  (LINKLN, 1, MAXTR, 3)
          call ATLAS (IX(JJKIJ), NL)
        end if
        if(IQLYM.gt.0) then
C----     Initialize RK-weights
          call ONE1  (X(JJR1W), N)
        end if
      end if
C     !END
      call BYE ('FLOSS')
C
      return
      end
