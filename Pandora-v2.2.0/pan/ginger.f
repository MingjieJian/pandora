      subroutine GINGER
     $(QNAME,KIND,X,LZA,ZAUX,W,XKPR,Y,CRD,CVW,CSK,CRS,COP,RHW,CDL,DDL,
     $ DWN,WSM,DRO,XC,XP,XR,GMA,PGL,XIB,XIR,XIS,DPM,IFS,ILS,NED,ISB1,
     $ ISB2,IST,KST)
C
C     Rudolf Loeser, 1976 May 12
C---- Reads Line Intensity data block data, and LIMBO data.
C     (This is version 2 of GINGER.)
C     !DASH
      save
C     !DASH
      real*8 CDL, COP, CRD, CRS, CSK, CVW, DDL, DPM, DRO, DWN, GMA, PGL,
     $       RHW, W, WSM, X, XC, XIB, XIR, XIS, XKPR, XP, XR, Y, ZAUX,
     $       ZERO, dummy
      integer IFS, IL, ILS, INDI, ISB, ISB1, ISB2, IST, IU, IUL, JJZ,
     $        KBTMX, KIND, KRTMX, KST, KSTMX, LDLMX, LHOS, LZA, N, NED,
     $        NT, jummy
      character QNAME*8, qummy*8
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
      equivalence (IZOQ( 37),JJZ  )
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
      equivalence (LEST(33),LDLMX)
      equivalence (LEST(60),KBTMX)
      equivalence (LEST(61),KRTMX)
      equivalence (LEST(62),KSTMX)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !EJECT
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
C     !DASH
C     !EJECT
      external  ARMILLA, MUSTARD, CILENTO, RACHEL, JELL, JILL, JUNIPER,
     $          CREAM, BASIL, MINT, INTRANS, HI, BYE
      intrinsic min, max
C
      dimension X(*), W(*)
C
C               LZA(50), ZAUX(LZM,NZM), XKPR(NT), CRD(LDLMX,NT), Y(NT),
      dimension LZA(*),  ZAUX(*),       XKPR(*),  CRD(LDLMX,*),  Y(*),
C
C               CVW(LDLMX,NT), CSK(LDLMX,NT), CDL(LDLMX,NT), COP(N,NT),
     $          CVW(LDLMX,*),  CSK(LDLMX,*),  CDL(LDLMX,*),  COP(N,*),
C
C               DDL(LDLMX,NT), DWN(LDLMX,NT), XIB(KBTMX,NT), RHW(N,NT),
     $          DDL(LDLMX,*),  DWN(LDLMX,*),  XIB(KBTMX,*),  RHW(N,*),
C
C               XIR(KRTMX,NT), XIS(KSTMX,NT), XC(NT), XP(NT), ISB1(NT),
     $          XIR(KRTMX,*),  XIS(KSTMX,*),  XC(*),  XP(*),  ISB1(*),
C
C               ISB2(NT), CRS(NT), WSM(NT), DRO(NT), GMA(NT), DPM(NT),
     $          ISB2(*),  CRS(*),  WSM(*),  DRO(*),  GMA(*),  DPM(*),
C
C               PGL(NT), IFS(NT), ILS(NT), NED(NT), IST(NT), KST(NT),
     $          PGL(*),  IFS(*),  ILS(*),  NED(*),  IST(*),  KST(*),
C
C               XR(NT)
     $          XR(*)
C
      dimension ISB(2)
C     !EJECT
C
      call HI ('GINGER')
C     !BEG
C---- Read transition indices
      call MINT    (QNAME, IU)
      call MINT    (QNAME, IL)
C---- Get index for LIMBO tables
      call RACHEL  (IU, IL, INDI)
C---- Get index for intermediate tables
      call INTRANS (IU, IL, 'GINGER', IUL)
C
      goto (
C
C       KPCR     ECLI     METSE    PROF     SCH      CRD      CVW
     $  101,     102,     103,     104,     105,     106,     107,
C
C       CSK      CRS      YLINE             KPC      RHOWT    BLCSW
     $  108,     109,     110,     111,     112,     113,     114,
C
C       LSFTYP   SMOOTH   DRHO     NED      XP       XC       GMMA
     $  115,     116,     117,     118,     119,     120,     121,
C
C                INRHO    LFLUX    LDL      DDL      CDL      LSFPRINT
     $  122,     123,     124,     125,     126,     127,     128,
C
C       LSFFDB   SOBOLEV  PROGLI   SGRAF    DWN      KBT      KRT
     $  129,     130,     131,     132,     133,     134,     135,
C
C       KST      XIBLUT   XIREDT   XISYMT   STARKI   CSTARK   LSFBOC
     $  136,     137,     138,     139,     140,     141,     142,
C
C       DPMULT   XR
     $  143,     144
C
     $  ), KIND
  101 continue
C----   Read input opacity ratio (radiative transitions)
        call JUNIPER (1, 'NT', 1, IU, IL, XKPR, dummy, 1, QNAME, jummy,
     $                dummy, dummy, dummy)
        goto 400
  102 continue
C----   Read line eclipse profile switch
        call JELL    (QNAME, INDI)
        goto 400
  103 continue
C----   Read statistical equilibrium method switch
        call MUSTARD (QNAME, dummy, LINMSE(INDI), qummy, 1, 3)
        goto 400
  104 continue
C----   Read line profile switch
        call JELL    (QNAME, INDI)
        goto 400
  105 continue
C----   Read non-coherent scattering switch for profiles
        call MUSTARD (QNAME, dummy, LINPRD(INDI), qummy, 1, 3)
        goto 400
  106 continue
C----   Read radiative halfwidth
        call BASIL   (CRD(1,IUL), LDLMX, QNAME)
        goto 400
  107 continue
C----   Read van der Waals half width
        call BASIL   (CVW(1,IUL), LDLMX, QNAME)
        goto 400
  108 continue
C----   Read Stark half width
        call BASIL   (CSK(1,IUL), LDLMX, QNAME)
        goto 400
  109 continue
C----   Read resonance half width
        call MUSTARD (QNAME,CRS(IUL),jummy,qummy,1,5)
        goto 400
  110 continue
C----   Read line source function method parameter, and
C       set source function method switch accordingly
        call MUSTARD (QNAME, Y(IUL), jummy, qummy, 1, 5)
        call JILL    (Y(IUL), INDI)
        goto 400
  111 continue
C----   (unsused)
        goto 400
  112 continue
C----   Read input opacity, and set switch accordingly
        call CREAM   (COP(1,IUL), QNAME, IU, IL, LZA, ZAUX, X(JJZ), W)
        LININK(INDI) = 1
        goto 400
  113 continue
C----   Read RHO weights
        call CREAM   (RHW(1,IUL), QNAME, IU, IL, LZA, ZAUX, X(JJZ), W)
        goto 400
  114 continue
C----   Read line broadening components switch
        call MUSTARD (QNAME, dummy, LINDAM(INDI), qummy, 1, 3)
        goto 400
  115 continue
C----   Read Line Source Function solution type switch
        call MUSTARD (QNAME, dummy, LINTPS(INDI), qummy, 1, 3)
        goto 400
  116 continue
C----   Read RHO-smoothing parameter
        call ARMILLA (WSM(IUL),IFS(IUL),ILS(IUL))
        goto 400
  117 continue
C----   Read RHO-editing weight
        call MUSTARD (QNAME, DRO(IUL), jummy, qummy, 1, 5)
        goto 400
  118 continue
C----   Read RHO-editing limit
        call MUSTARD (QNAME, dummy, NED(IUL), qummy, 1, 3)
        goto 400
  119 continue
C----   Read PRD parameter
        call MUSTARD (QNAME, XP(IUL), jummy, qummy, 1, 5)
        goto 400
  120 continue
C----   Read PRD parameter
        call MUSTARD (QNAME, XC(IUL), jummy, qummy, 1, 5)
        goto 400
  121 continue
C----   Read PRD parameter
        call MUSTARD (QNAME, GMA(IUL), jummy, qummy, 1, 5)
        goto 400
  122 continue
C----
        goto 400
  123 continue
C----   Read input-RHO switch
        call MUSTARD (QNAME, dummy, LININR(INDI), qummy, 1, 3)
        goto 400
  124 continue
C----   Read Line Flux calculation control switch
        call MUSTARD (QNAME, dummy, LINFLX(INDI), qummy, 1, 3)
        goto 400
  125 continue
C----   Read count of component lines
        call MUSTARD (QNAME, dummy, LINLDL(INDI), qummy, 1, 3)
        goto 400
  126 continue
C----   Read component lines offset (Angstroms)
        call BASIL   (DDL(1,IUL), LDLMX, QNAME)
        goto 400
  127 continue
C----   Read component lines weight
        call BASIL   (CDL(1,IUL), LDLMX, QNAME)
        goto 400
  128 continue
C----   Read Source Function printout switch
        call MUSTARD (QNAME, dummy, LINPRN(INDI), qummy, 1, 3)
        goto 400
  129 continue
C----   Read Source Function background switch
        call MUSTARD (QNAME, dummy, LINFDB(INDI), qummy, 1, 3)
        goto 400
  130 continue
C----   Read Sobolev solution limit indices
        call CILENTO (ISB, 2, QNAME)
        ISB1(IUL) = ISB(1)
        ISB2(IUL) = ISB(2)
        goto 400
  131 continue
C----   Read profile graph limit
        call MUSTARD (QNAME, PGL(IUL), jummy, qummy, 1, 5)
        goto 400
  132 continue
C----   Read blended line profile plot switch
        call MUSTARD (QNAME, dummy, LINSBG(INDI), qummy, 1, 3)
        goto 400
  133 continue
C----   Read component lines offset (wavenumber)
        call BASIL   (DWN(1,IUL), LDLMX, QNAME)
        goto 400
  134 continue
C----   Read length of XIBLUT
        call MUSTARD (QNAME, dummy, LINKBT(INDI), qummy, 1, 3)
        goto 400
  135 continue
C----   Read length of XIREDT
        call MUSTARD (QNAME,dummy,LINKRT(INDI),qummy,1,3)
        goto 400
  136 continue
C----   Read length of XISYMT
        call MUSTARD (QNAME, dummy, LINKST(INDI), qummy, 1, 3)
        goto 400
  137 continue
C----   Read blue-side frequency grid
        call BASIL   (XIB(1,IUL), LINKBT(INDI), QNAME)
        goto 400
  138 continue
C----   Read red-side frequency grid
        call BASIL   (XIR(1,IUL), LINKRT(INDI), QNAME)
        goto 400
  139 continue
C----   Read symmetric frequency grid
        call BASIL   (XIS(1,IUL), LINKST(INDI), QNAME)
        goto 400
  140 continue
C----   Read Hydrogen lines Stark splitting NE-index
        call MUSTARD (QNAME, dummy, IST(IUL), qummy, 1, 3)
        goto 400
  141 continue
C----   Read Hydrogen convolved-Stark-profile switch
        call MUSTARD (QNAME, dummy, KST(IUL), qummy, 1, 3)
        goto 400
  142 continue
C----   Read LSF Background Line Opacities control
        call MUSTARD (QNAME, dummy, LHOS, qummy, 1, 3)
        LINOML(INDI) = max(min(LHOS,1),0)
        goto 400
  143 continue
C----   Read damping multiplier
        call MUSTARD (QNAME, DPM(IUL), jummy, qummy, 1, 5)
        goto 400
  144 continue
C----   Read PRD parameter
        call MUSTARD (QNAME, XR(IUL), jummy, qummy, 1, 5)
        goto 400
  400 continue
C     !END
      call BYE ('GINGER')
C
      return
      end
