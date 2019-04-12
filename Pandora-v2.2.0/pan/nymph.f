      subroutine NYMPH
     $(IN,MUX)
C
C     Rudolf Loeser, 24 Feb 70
C---- Allocates integer scratch storage for TANGO.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX
C     !COM
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
C
C---- NIVELON     as of 1994 May 31
      integer     KMXBND,KMXWAV
      common      /NIVELON/ KMXBND,KMXWAV
C     Limits for tables of Composite Lines Opacity data.
C     .
C
C---- INDEX       as of 1990 Nov 26
      integer     MILTI,LIMIT,NULSIG,INC
      parameter   (MILTI=50)
C     (Remember to recompile ADAM and GRUB when changing MILTI.)
      dimension   INC(MILTI,MILTI)
      common      /INDEX/ LIMIT,NULSIG,INC
C     Data and controls for the index-mapping routines.
C     .
C     !DASH
      external HI, BYE
C
      dimension IN(*)
C
      data IS /1/
C
      call HI ('NYMPH')
C     !BEG
      IN( 1) = IS
      IN( 2) = IN( 1)+(2*LIMTR)
      IN( 3) = IN( 2)+(2*LIMIT)
      IN( 4) = IN( 3)+50
      IN( 5) = IN( 4)+KMXBND
      IN( 6) = IN( 5)+(LIMIT+1)
      MUX    = IN( 6)+LIMIT
C     !END
      call BYE ('NYMPH')
C
      return
      end
