      subroutine POTTY
     $(AMASS,A,P,XNU,CRD,CVW,CSK,DDL,CDL,KODE)
C
C     Rudolf Loeser, 2007 Jan 18
C---- Checks the O-II data in WURGO. Returns with KODE=1 if they agree,
C     with KODE=0 if not.
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
C---- WURGO       as of 2007 Jan 25
      parameter   (MX2L=1, LX2L=3)
      integer     MX2L, LX2L, IUX2, ILX2, LDLX2
      real*8      X2MAS, X2SKE, X2WVL, X2WLO, X2WHI, X2NUU, X2NUL, X2AUL
      real*8      X2PU,  X2PL,  X2DDL, X2CDL, X2CRD, X2CVW, X2CSK
      dimension   X2WVL(MX2L), X2WLO(MX2L), X2WHI(MX2L), X2NUU(MX2L),
     $            X2NUL(MX2L), X2PU(MX2L),  X2PL(MX2L),  X2AUL(MX2L),
     $            IUX2(MX2L),  ILX2(MX2L),  LDLX2(MX2L)
      dimension   X2DDL(LX2L,MX2L), X2CDL(LX2L,MX2L),
     $            X2CRD(LX2L,MX2L), X2CVW(LX2L,MX2L), X2CSK(LX2L,MX2L)
      common      /WURGO0/ X2MAS,X2SKE
      common      /WURGO1/ X2WVL,X2WLO,X2WHI
      common      /WURGO2/ X2NUU,X2NUL,X2PU,X2PL
      common      /WURGO3/ X2AUL,X2DDL,X2CDL,X2CRD,X2CVW,X2CSK
      common      /WURGO4/ IUX2,ILX2,LDLX2
C     Data for Oxygen-II lines in the background.
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
      call HI ('POTTY')
C     !BEG
      call SLIME     (MX2L, IUX2, ILX2, 'OXYGEN2', KODE, OK)
      if(.not.OK) then
        goto 109
      end if
C
C
      call COMPD     (AMASS,    X2MAS,    DELTA, K )
C
      do 100 I = 1,MX2L
        IU = IUX2(I)
        IL = ILX2(I)
C
        call COMPD   (A(IU,IL), X2AUL(I), DELTA, K1)
        call COMPD   (P(IU),    X2PU(I),  DELTA, K2)
        call COMPD   (P(IL),    X2PL(I),  DELTA, K3)
        call COMPD   (XNU(IU),  X2NUU(I), DELTA, K4)
        call COMPD   (XNU(IL),  X2NUL(I), DELTA, K5)
        K = K+abs(K1)+abs(K2)+abs(K3)+abs(K4)+abs(K5)
C
        call INTRANS (IU, IL, 'POTTY', IUL)
        call NOVEMBR (DDL(1,IUL), LDLX2(I), X2DDL(1,I), DELTA, K1)
        call NOVEMBR (CDL(1,IUL), LDLX2(I), X2CDL(1,I), DELTA, K2)
        call NOVEMBR (CRD(1,IUL), LDLX2(I), X2CRD(1,I), DELTA, K3)
        call NOVEMBR (CVW(1,IUL), LDLX2(I), X2CVW(1,I), DELTA, K4)
        call NOVEMBR (CSK(1,IUL), LDLX2(I), X2CSK(1,I), DELTA, K5)
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
        call MESHED    ('POTTY', 3)
        call SHOUT     (LUEO, KODE, 'OXYGEN2')
C
        I = 0
        call LINER     (1, LUEO)
        write (LUEO,101)
  101   format(' ',33X,'mass')
        write (LUEO,102) I,I,I,AMASS
  102   format(' ',I5,' (',I2,',',I2,')  this run',1P5E14.6)
        write (LUEO,103)   X2MAS
  103   format(' ',13X,               '  built-in',1P5E14.6)
C
        do 108 I = 1,MX2L
          IU = IUX2(I)
          IL = ILX2(I)
          call INTRANS (IU, IL, 'POTTY', IUL)
          call LINER   (1, LUEO)
          write (LUEO,104)
  104     format(' ',33X,'P(u)',10X,'P(l)',9X,'NU(u)',9X,'NU(l)',
     $               13X,'A')
          write (LUEO,102) I,IU,IL,P(IU),  P(IL),  XNU(IU), XNU(IL),
     $                             A(IU,IL)
          write (LUEO,103)         X2PU(I),X2PL(I),X2NUU(I),X2NUL(I),
     $                             X2AUL(I)
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
          LDL = LDLX2(I)
          do 107 J = 1,LDL
            write (LUEO,103)           X2DDL(J,I),X2CDL(J,I),X2CRD(J,I),
     $                                 X2CVW(J,I),X2CSK(J,I)
  107     continue
C
  108   continue
        call MASHED    ('POTTY')
      end if
C
  109 continue
C     !END
      call BYE ('POTTY')
C
      return
      end
