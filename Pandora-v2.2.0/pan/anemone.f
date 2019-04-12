      subroutine ANEMONE
     $(N,NL,NT,MRHO,XINCH,WMX,WMN,SMP,AIJ,CIJ,WEIGHT,RHWL,ORHO,RHOO,
     $ RHOS,RHOJ,RHOW,WEIT,RHOIJ,IFSL,ILSL,WSML,NEDL,DROL,XVAL,CAU1,
     $ W,IW)
C
C     Rudolf Loeser, 1980 May 04
C---- Controls selection, weighting and smoothing
C     of the new Rho's, for TULIP.
C     (This is version 4 of ANEMONE.)
C     !DASH
      save
C     !DASH
      real*8 AIJ, CAU1, CIJ, DROL, ORHO, RHOIJ, RHOJ, RHOO, RHOS, RHOW,
     $       RHWL, SMP, W, WEIGHT, WEIT, WMN, WMX, WSML, XINCH, XVAL
      integer IFSL, IL, ILSL, INDX, IQRSM, IU, IUL, IW, J, KLIN, MRHO,
     $        N, NEDL, NL, NT, jummy
      logical JILROY, KILROY, lummy
      character LABEL*100, TYPE*3
C     !COM
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
      equivalence (LINKDS( 3),KLIN )
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
      equivalence (IQQ(147),IQRSM)
C
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
      external SMOOTH, ONE1, INTRANS, SPIREA, AZALEA, SOPHRON, INDARRD,
     $         PET, MOVE1, HI, BYE
C
      dimension W(*), IW(*)
C
C               IFSL(NT), NEDL(NT), RHOS(N,NT), RHOO(N,NT), ORHO(N,NT),
      dimension IFSL(*),  NEDL(*),  RHOS(N,*),  RHOO(N,*),  ORHO(N,*),
C
C               RHOJ(N,NT), RHOW(N,NT), ILSL(NT), AIJ(NL,NL), WSML(NT),
     $          RHOJ(N,*),  RHOW(N,*),  ILSL(*),  AIJ(*),     WSML(*),
C
C               WEIT(N), DROL(NT), CIJ(N,NL**2), WEIGHT(N,NT), XVAL(N),
     $          WEIT(*), DROL(*),  CIJ(N,*),     WEIGHT(N,*),  XVAL(*),
C
C               RHWL(N,NT), RHOIJ(N,NT), CAU1(N,NL)
     $          RHWL(N,*),  RHOIJ(N,*),  CAU1(*)
C
      data TYPE,INDX /'lin', 0/
C     !EJECT
C
      call HI ('ANEMONE')
C     !BEG
      JILROY = .true.
      KILROY = .true.
C---- Save current Rho's
      call MOVE1         (RHOIJ, (N*NT), RHOO)
C---- Loop over all transitions
      do 100 J = 1,NT
        call PET         (J)
        if(KLIN.eq.1) then
          call INTRANS   (IU,IL,'ANEMONE-1',IUL)
          if(LININR(J).eq.1) then
            call ONE1    (WEIGHT(1,IUL), N)
          else
C----       Establish weights to be used (save for iterative summary)
            call SPIREA  (N, MRHO, XINCH, WMX, WMN, SMP, WEIGHT(1,IUL),
     $                    RHWL(1,IUL), ORHO(1,IUL), RHOO(1,IUL),
     $                    RHOS(1,IUL), RHOJ(1,IUL), RHOW(1,IUL),
     $                    IU, IL, KILROY)
C----       Select, smoothe and weight one of the new Rho sets
            call AZALEA  (N, MRHO, IFSL(IUL), ILSL(IUL), WSML(IUL),
     $                    WEIGHT(1,IUL), RHOO(1,IUL), RHOS(1,IUL),
     $                    RHOJ(1,IUL), RHOW(1,IUL), WEIT, RHOIJ(1,IUL),
     $                    IU,IL)
C----       Edit final Rho's (uses XVAL for scratch)
            call SOPHRON (IU, IL, NL, N, RHOIJ(1,IUL), AIJ, NEDL(IUL),
     $                    DROL(IUL), CIJ, CAU1, XVAL, JILROY)
          end if
        end if
  100 continue
C
      if(IQRSM.gt.0) then
C----   Sequential smoothing of final Rho
        call INDARRD     (XVAL, 1, 1, N)
        do 102 J = 1,NT
          call PET       (J)
          if((KLIN.eq.1).and.(LININR(J).ne.1)) then
            write (LABEL,101) IU,IL
  101       format('Final Rho for transition (',I2,'/',I2,')')
            call INTRANS (IU, IL, 'ANEMONE-2', IUL)
            call SMOOTH  (XVAL, RHOIJ(1,IUL), N, TYPE, LABEL, INDX, W,
     $                    IW, jummy, lummy)
          end if
  102   continue
      end if
C     !END
      call BYE ('ANEMONE')
C
      return
      end
