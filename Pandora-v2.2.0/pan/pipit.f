      subroutine PIPIT
C
C     Rudolf Loeser, 1985 Dec 30
C---- Sets up default data in LIMBO, and
C     sets up the name table in THULE, for all transitions.
C     !DASH
      save
C     !DASH
      integer IL, IU, IUL, NL, NOION, NT
      logical OK
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
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
      equivalence (KZQ( 94),NOION)
C
C---- THULE       as of 1999 Dec 07
      integer     MAXLV,MXTRA
      parameter   (MAXLV=50)
      parameter   (MXTRA=(MAXLV*(MAXLV-1))/2)
C     (Remember to recompile ADAM when changing MAXLV.)
      integer     LMTRA,NUMTRN,NUMBLK,LINNAM,LI1ADR,LI2ADR,LI3ADR
      dimension   LI1ADR(MXTRA),LI2ADR(MXTRA),LI3ADR(MXTRA)
      dimension   LINNAM(MXTRA)
      common      /THULE0/ LMTRA,NUMTRN,NUMBLK
      common      /THULE1/ LINNAM
      common      /THULE2/ LI1ADR
      common      /THULE3/ LI2ADR
      common      /THULE4/ LI3ADR
C
C     Indices and Names of the Line Intensity Data Blocks.
C     LMTRA  -    = MAXLV
C     NUMTRN -    number of transitions (i.e. AIJ .ne. 0), which is
C                 the number of Line Intensity Data Blocks allocated;
C     NUMBLK -    number of Line Intensity Data Blocks in actual use
C                 (i.e. number of radiative and passive transitions);
C     LINNAM -    block name, = 100*IU+IL;
C     LI1ADR -    block address in scratch memory, part 1;
C     LI2ADR -    block address in scratch memory, part 2;
C     LI3ADR -    block address in scratch memory, part 3.
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
C     !EJECT
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external INDXNT, MALLARD, HALT, HI, BYE
C
      call HI ('PIPIT')
C     !BEG
      NUMBLK = 0
C     (An actual value of NUMBLK is computed by HUDRA - 10/5/90)
      NUMTRN = 0
      if(NOION.le.0) then
C
        do 101 IU = 2,NL
          do 100 IL = 1,(IU-1)
            call INDXNT (IU, IL, OK, IUL)
            if(OK) then
              NUMTRN = NUMTRN+1
C
              LINIU (NUMTRN) = IU
              LINIL (NUMTRN) = IL
              LINKLN(NUMTRN) = 1
C
              LINNAM(NUMTRN) = 100*IU+IL
C
              LI1ADR(NUMTRN) = 0
              LI2ADR(NUMTRN) = 0
              LI3ADR(NUMTRN) = 0
C
            end if
  100     continue
  101   continue
C
        call MALLARD    ('PIPIT')
C
        if(NUMTRN.ne.NT) then
          write (MSSLIN(1),102) NT,NUMTRN
  102     format('NT =',I12,'; NUMTRN =',I12,'; they should be equal.')
          call HALT     ('PIPIT', 1)
        end if
      end if
C     !END
      call BYE ('PIPIT')
C
      return
      end
