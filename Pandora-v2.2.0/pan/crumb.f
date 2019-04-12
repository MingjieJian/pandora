      subroutine CRUMB
     $(X,W,IW)
C
C     Rudolf Loeser, 1992 Oct 15
C---- Initializes transition-related data for GRENADE.
C     (This is version 2 of CRUMB.)
C     !DASH
      save
C     !DASH
      real*8 W, X, dummy
      integer I, IAFU, IASU, IDWS, IN, IQDPW, IQIWA, IS, IVM, IW, IWAM,
     $        IXIFU, IXISU, IXLB1, IZERO, JJYAF, K, KLIN, KODE, KXMAX,
     $        LUG, LUW, MOX, NO, NT
      logical GOODA, GOODT, KILROY
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 5),NT )
C
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
      equivalence (LINKDS( 3),KLIN )
C     !EJECT
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(264),JJYAF)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C
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
      equivalence (IQQ( 25),IQDPW)
      equivalence (IQQ(285),IQIWA)
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
      external  KRUMB, ZEUS, PET, LIDGET, GALIUM, LIDPUT, GIRON, WRACK,
     $          DITTANY, DEWDROP, JETSAM, HALT, WGIVE, MALLARD, CITAUR,
     $          HI, BYE
      intrinsic max
C
      dimension X(*), W(*), IW(*)
C
      dimension IN(9)
      equivalence
     $(IN( 1),IXLB1),(IN( 2),IZERO),(IN( 3),IVM  ),(IN( 4),IDWS ),
     $(IN( 5),IWAM ),(IN( 6),IAFU ),(IN( 7),IXISU),(IN( 8),IASU ),
     $(IN( 9),IXIFU)
C
      call HI ('CRUMB')
C     !BEG
C     (Get, and allocate, W allotment)
      call KRUMB       (IN, IS, MOX, 'CRUMB')
C
      call ZEUS        (NO, IQDPW, LUW)
      call ZEUS        (NO, IQIWA, LUG)
      KILROY = .true.
      KXMAX  = 0
C
C---- Print header
      call CITAUR (LUG)
C
      GOODA = .true.
      GOODT = .true.
C
      do 100 I = 1,NT
        call PET       (I)
        if((KLIN.eq.1).or.(KLIN.eq.2)) then
C         (Do these things only for radiative and passive transitions)
C----     Read Line Intensity Data Block
          call LIDGET  (W(IXLB1), 1, dummy, 0, dummy, 0, I)
C----     Set up doppler width, DW, and reference doppler width, NDW
          call WRACK   (X, W(IXLB1), W(IZERO), W(IVM), KILROY)
C----     Save for printing
          call JETSAM  (LUW, I, W(IXLB1), W(IDWS))
C----     Set up frequency table, XI, integration weights, A, and
C         Delta-Lambda table, DL; and print
          call GALIUM  (X, W, IW, W(IXLB1), W(IXISU), W(IASU),
     $                  W(IXIFU), W(IAFU), X(JJYAF), GOODA, GOODT,
     $                  KODE, K, KXMAX, LUG)
C----     Write updated Line Intensity Data Block
          call LIDPUT  (W(IXLB1), 1, dummy, 0, dummy, 0, I)
C----     Update LINKM (= KTRN) and KXMAX
          LINKM(I) = K
          KXMAX    = max(KXMAX, K)
C----     Check for absorption edges in the line background
          call DEWDROP (W(IXLB1), K, KODE, LUG, W(IWAM), IW)
        end if
  100 continue
C     !EJECT
C---- Update KM (and also LDLMX, if needed), and print message
      call GIRON   (KXMAX)
C---- (Dump line data ?)
      call MALLARD ('CRUMB')
C---- Print Doppler Widths
      call DITTANY (LUW, X, W(IDWS), W(IVM))
C
      if(.not.GOODA) then
        write (MSSLIN(1),101)
  101   format('Bad integration weights.')
        call HALT  ('CRUMB', 1)
      end if
      if(.not.GOODT) then
        write (MSSLIN(1),102)
  102   format('Table limits trouble.')
        call HALT  ('CRUMB', 1)
      end if
C
C     (Give back W allotment)
      call WGIVE   (W, 'CRUMB')
C     !END
      call BYE ('CRUMB')
C
      return
      end
