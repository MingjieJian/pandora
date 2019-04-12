      subroutine LOTTY
     $(AMASS,A,P,XNU,CRD,CVW,CSK,DDL,CDL,KODE)
C
C     Rudolf Loeser, 2007 Jan 18
C---- Checks the O-III data in WARGO. Returns with KODE=1 if they agree,
C     with KODE=0 if not.
C
C---- As of Jan 2007, there is the complication that level 8 of the
C     population ion = level 13 of o3l13.atm.
C     !DASH
      save
C     !DASH
      real*8 A, AMASS, CDL, CRD, CSK, CVW, DDL, DELTA, P, XNU
      integer I, IL, INDI, IU, IUL, J, K, K1, K2, K3, K4, K5, KODE, LDL,
     $        LDLMX, LUEO, NL
      logical OK
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
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
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- WARGO       as of 2007 Jan 18
      parameter   (MX3L=2, LX3L=6)
      integer     MX3L, LX3L, IUX3, ILX3, LDLX3
      real*8      X3MAS, X3SKE, X3WVL, X3WLO, X3WHI, X3NUU, X3NUL, X3AUL
      real*8      X3PU,  X3PL,  X3DDL, X3CDL, X3CRD, X3CVW, X3CSK
      dimension   X3WVL(MX3L), X3WLO(MX3L), X3WHI(MX3L), X3NUU(MX3L),
     $            X3NUL(MX3L), X3PU(MX3L),  X3PL(MX3L),  X3AUL(MX3L),
     $            IUX3(MX3L),  ILX3(MX3L),  LDLX3(MX3L)
      dimension   X3DDL(LX3L,MX3L), X3CDL(LX3L,MX3L),
     $            X3CRD(LX3L,MX3L), X3CVW(LX3L,MX3L), X3CSK(LX3L,MX3L)
      common      /WARGO0/ X3MAS,X3SKE
      common      /WARGO1/ X3WVL,X3WLO,X3WHI
      common      /WARGO2/ X3NUU,X3NUL,X3PU,X3PL
      common      /WARGO3/ X3AUL,X3DDL,X3CDL,X3CRD,X3CVW,X3CSK
      common      /WARGO4/ IUX3,ILX3,LDLX3
C     Data for Oxygen-III lines in the background.
C     .
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
C     .
C     NPOPS   = total number of population data blocks = NPI;
C     MAXPOPL = maximum value of LIMPOP;
C     LENT    = sum of LIMPOP;
C     LENPBL  = length of a Population Data Block;
C     LZOQ    = Population Data Blocks components index, (POPN and BD
C               are arrays of the form [N,LIMP]);
C     LENPOP  = number of non-LTE population levels in a set;
C     LIMPOP  = total number of population levels in a set;
C     NAMES   = population ion names;
C     NAMKNT  = number of characters in NAMES;
C     TNAMES  = truncated population ion names;
C     IUPOP   = populations update switches;
C     IBLAD   = file address of Population Block records;
C     IPSWICH = copy of population data print option setting;
C     POPMSS  = ion mass;
C     POPSYM  = element symbol of population ion, as in ELEMENT table;
C     KAPNO   = absorber (Kappa) number of population ion;
C     ICKSM   = index of "SENNA" checksum;
C     MRTP    = "ion-of-run"-to-"population-ion-model" level indices
C               (length must be .ge. MAXPOPL). [MRTP(I)=J means:
C               level J of the "ion-of-run" correseponds to level I of
C               the built-in "population-ion-model"];
C     KLABPI, NLABPI, BLABPI = names of population in data tables, as
C                              used in input statements.
C     .
      equivalence
     $(LZOQ( 1),LLNPOP),(LZOQ( 2),LLIUP ),(LZOQ( 3),LLPOPK),
     $(LZOQ( 4),LLPOPN),(LZOQ( 5),LLBD  )
      equivalence
     $(LENPOP( 1),NLH ),(LENPOP( 2),NLC ),(LENPOP( 3),NLS ),
     $(LENPOP( 4),NLZ ),(LENPOP( 5),NZ2 ),(LENPOP( 6),NAL ),
     $(LENPOP( 7),NMG ),(LENPOP( 8),NFE ),(LENPOP( 9),NNA ),
     $(LENPOP(10),NCA ),(LENPOP(11),NLO ),(LENPOP(12),NLU ),
     $(LENPOP(13),NO2 ),(LENPOP(14),NO3 )
      equivalence
     $(IUPOP( 1),JYDRO),(IUPOP( 2),JARBO),(IUPOP( 3),JILIC),
     $(IUPOP( 4),JELIU),(IUPOP( 5),JELI2),(IUPOP( 6),JLUMI),
     $(IUPOP( 7),JAGNE),(IUPOP( 8),JIRON),(IUPOP( 9),JODIU),
     $(IUPOP(10),JALCI),(IUPOP(11),JOXYG),(IUPOP(12),JULPH),
     $(IUPOP(13),JOXY2),(IUPOP(14),JOXY3)
C     .
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
      external  INTRANS, COMPD, MESHED, MASHED, SHOUT, LINER, NOVEMBR,
     $          SLIM, SLIME, HI, BYE
      intrinsic abs
C
C               CRD(LDLMX,NT), CVW(LDLMX,NT), CSK(LDLMX,NT), A(NL,NL),
      dimension CRD(LDLMX,*),  CVW(LDLMX,*),  CSK(LDLMX,*),  A(NL,*),
C
C               DDL(LDLMX,NT), CDL(LDLMX,NT), XNU(NSL), P(NSL)
     $          DDL(LDLMX,*),  CDL(LDLMX,*),  XNU(*),   P(*)
C
      data DELTA /1.D-5/
C
      call HI ('LOTTY')
C     !BEG
      call SLIME     (MX3L, IUX3, ILX3, 'OXYGEN3', KODE, OK)
      if(.not.OK) then
        goto 109
      end if
C
C
      call COMPD     (AMASS,    X3MAS,    DELTA, K )
C
      do 100 I = 1,MX3L
        IU = IUX3(I)
        if(IU.eq.8) then
          IU = MRTP(8)
        end if
        IL = ILX3(I)
C
        call COMPD   (A(IU,IL), X3AUL(I), DELTA, K1)
        call COMPD   (P(IU),    X3PU(I),  DELTA, K2)
        call COMPD   (P(IL),    X3PL(I),  DELTA, K3)
        call COMPD   (XNU(IU),  X3NUU(I), DELTA, K4)
        call COMPD   (XNU(IL),  X3NUL(I), DELTA, K5)
        K = K+abs(K1)+abs(K2)+abs(K3)+abs(K4)+abs(K5)
C
        call INTRANS (IU, IL, 'LOTTY', IUL)
        call NOVEMBR (DDL(1,IUL), LDLX3(I), X3DDL(1,I), DELTA, K1)
        call NOVEMBR (CDL(1,IUL), LDLX3(I), X3CDL(1,I), DELTA, K2)
        call NOVEMBR (CRD(1,IUL), LDLX3(I), X3CRD(1,I), DELTA, K3)
        call NOVEMBR (CVW(1,IUL), LDLX3(I), X3CVW(1,I), DELTA, K4)
        call NOVEMBR (CSK(1,IUL), LDLX3(I), X3CSK(1,I), DELTA, K5)
        K = K+K1+K2+K3+K4+K5
  100 continue
C
      if(K.eq.0) then
        KODE = 1
      else
        KODE = 0
      end if
C     !EJECT
      if(KODE.ne.1) then
        call MESHED    ('LOTTY', 3)
        call SHOUT     (LUEO, KODE, 'OXYGEN3')
C
        I = 0
        call LINER     (1, LUEO)
        write (LUEO,101)
  101   format(' ',33X,'mass')
        write (LUEO,102) I,I,I,AMASS
  102   format(' ',I5,' (',I2,',',I2,')  this run',1P5E14.6)
        write (LUEO,103)   X3MAS
  103   format(' ',13X,               '  built-in',1P5E14.6)
C
        do 108 I = 1,MX3L
          IU = IUX3(I)
          if(IU.eq.8) then
            IU = MRTP(8)
          end if
          IL = ILX3(I)
          call INTRANS (IU, IL, 'LOTTY', IUL)
          call LINER   (1, LUEO)
          write (LUEO,104)
  104     format(' ',33X,'P(u)',10X,'P(l)',9X,'NU(u)',9X,'NU(l)',
     $               13X,'A')
          write (LUEO,102) I,IU,IL,P(IU),  P(IL),  XNU(IU), XNU(IL),
     $                             A(IU,IL)
          write (LUEO,103)         X3PU(I),X3PL(I),X3NUU(I),X3NUL(I),
     $                             X3AUL(I)
C
          call SLIM    (IU, IL, INDI, OK)
          if(OK) then
            LDL = LINLDL(INDI)
            call LINER (1, LUEO)
            write (LUEO,105)
  105       format(' ',34X,'DDL',11X,'CDL',11X,'CRD',11X,'CVW',
     $                 11X,'CSK')
            do 106 J = 1,LDL
              write (LUEO,102) I,IU,IL,DDL(J,IUL),CDL(J,IUL),CRD(J,IUL),
     $                                 CVW(J,IUL),CSK(J,IUL)
  106       continue
          end if
C
          LDL = LDLX3(I)
          do 107 J = 1,LDL
            write (LUEO,103)           X3DDL(J,I),X3CDL(J,I),X3CRD(J,I),
     $                                 X3CVW(J,I),X3CSK(J,I)
  107     continue
C
  108   continue
        call MASHED    ('LOTTY')
      end if
C
  109 continue
C     !END
      call BYE ('LOTTY')
C
      return
      end
