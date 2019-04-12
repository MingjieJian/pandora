      subroutine HUDRO
     $(YCO,IQEXA,NVX,LDLMU,KBTMX,KRTMX,KSTMX,KSTRK,N,NT,WMN,WMX,WMNO,
     $ WMXO,Y,RHW,RHWO,KST,ISB1)
C
C     Rudolf Loeser, 1984 Feb 16
C---- Assures that line source function processing switches
C     are appropriate and compatible.
C
C---- Also, massages input !
C
C     (This is version 4 of HUDRO.)
C     !DASH
      save
C     !DASH
      real*8 RHW, RHWO, THREE, WMN, WMNO, WMX, WMXO, Y, YCO, ZERO
      integer I, IL, IN, IQEXA, IQOTL, ISB1, IU, KBTMAX, KBTMX, KLIN,
     $        KOUNT, KOUNTF, KOUNTS, KRTMAX, KRTMX, KST, KSTMAX, KSTMX,
     $        KSTRK, LDLMU, LDLMX, N, NOION, NT, NVX
      logical KILROY, METH, SOBO, STOP, WHOLE
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 94),NOION)
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 4),THREE )
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
C     !EJECT
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
      equivalence (IQQ(344),IQOTL)
C     !DASH
      external  MALLARD, JILL, BULBUL, ABORT, RUBY, HAIDA, ETIAS, ENYO,
     $          BUTELUA, INTRANS, EXVIN, ESTEEM, MASHED, MANA, HI, BYE
      intrinsic max
C
C               YCO(NT), RHW(N,NT), ISB1(NT), KST(NT), Y(NT), RHWO(N,NT)
      dimension YCO(*),  RHW(N,*),  ISB1(*),  KST(*),  Y(*),  RHWO(N,*)
C
      data KILROY,STOP /.true., .false./
C
      call HI ('HUDRO')
C     !BEG
      if((NT.gt.0).and.(NOION.le.0)) then
        KOUNT  = 0
        KOUNTS = 0
        KOUNTF = 0
        LDLMU  = 1
        KBTMAX = 0
        KRTMAX = 0
        KSTMAX = 0
        KSTRK  = 0
        WHOLE  = (IQEXA.gt.0).or.(NVX.gt.0)
C
C----   Check RHO-weights adjustment parameters
        call BULBUL (WMN, WMX, WMNO, WMXO)
C     !EJECT
        do 100 I = 1,NT
          IU = LINIU(I)
          IL = LINIL(I)
          call INTRANS  (IU, IL, 'HUDRO', IN)
          SOBO = ISB1(IN).gt.1
          if(IQOTL.gt.0) then
C----       Set up for optically-thin-limit
            LINKLN(I) = 3
          end if
C----     Make sure appropriate line source function method is used
          call RUBY     (Y(IN), KOUNT, KOUNTS, KOUNTF)
          call JILL     (Y(IN), I)
C----     Make sure METSE has been specified (statistical equilibrium)
          call ESTEEM   (IU, IL, LINMSE(I))
C----     Make sure PRD is properly set
          call MANA     (IU, IL, LINPRD(I), LINLDL(I))
C
          if(LINFLX(I).eq.1) then
            if(LINMSF(I).ne.3) then
C----         Make sure that GR is used for weight matrices . . .
              Y(IN)     = -THREE
              LINMSF(I) = 3
            end if
            if(LINPRD(I).ne.0) then
C----         . . . and that GR is used for background calculation also
              if(YCO(IN).ne.-THREE) then
                YCO(IN) = -THREE
              end if
            end if
          end if
C
          if(KST(IN).gt.0) then
            KSTRK = KSTRK+1
          end if
C
          KLIN = LINKLN(I)
C
          if(KLIN.eq.1) then
C----       This is a RADIATIVE transition: check on table size limits
            KBTMAX = max(KBTMAX,LINKBT(I))
            call ENYO (LINKBT(I), KBTMX, IU, IL, 'KBT', 'HUDRO', STOP)
            KRTMAX = max(KRTMAX,LINKRT(I))
            call ENYO (LINKRT(I), KRTMX, IU, IL, 'KRT', 'HUDRO', STOP)
            KSTMAX = max(KSTMAX,LINKST(I))
            call ENYO (LINKST(I), KSTMX, IU, IL, 'KST', 'HUDRO', STOP)
C
            LDLMU = max(LDLMU,LINLDL(I))
          else if((KLIN.eq.2).or.(KLIN.eq.3).or.(KLIN.eq.4).or.
     $            (KLIN.eq.5)) then
C----       This is a PASSIVE, THIN, 2-PHOTON, or THICK transition,
C           therefore no PRD and no FDB
            LINPRD(I) = 0
            LINFDB(I) = 0
          end if
C     !EJECT
          if(LINPRD(I).ne.0) then
C----       Change switch, to use frequency-dependent background
C           for PRD
            LINFDB(I) = 1
          end if
C
          METH = (LINLDL(I).gt.1).or.(LINFDB(I).gt.0)
          if(METH.or.WHOLE.or.SOBO) then
C----       Change switch, to integrate over whole profile
            LININT(I) = 1
          end if
C
C----     Check compatibility of BDOPT and LSF method
          call EXVIN  (LINTPS(I), STOP)
C
C----     Use full-profile & frequency-dependent-background for
C         raditaive transitions with computed profiles
          if((LINPRO(I).gt.0).and.(LINKLN(I).eq.1)) then
            LININT(I) = 1
            LINFDB(I) = 1
          end if
  100   continue
        call HAIDA    (KOUNT, KOUNTS, KOUNTF, 'YLINE', KILROY, 'HUDRO')
        if(.not.KILROY) then
          call MASHED ('HUDRO')
        end if
C----   Check, and perhaps modify, input RHO weights
        call BUTELUA  (N, NT, RHW, RHWO, WMN, WMX)
C
C----   Double-check counter limits, and give good advice as needed
        call ETIAS    (KBTMAX, KBTMX, 'KBT', STOP)
        call ETIAS    (KRTMAX, KRTMX, 'KRT', STOP)
        call ETIAS    (KSTMAX, KSTMX, 'KST', STOP)
        if(STOP) then
          call ABORT
        end if
C
C----   Print current state of LIMBO
        call MALLARD  ('HUDRO')
C
      end if
C     !END
      call BYE ('HUDRO')
C
      return
      end
