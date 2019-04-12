      subroutine HOGRY
     $(N,NL,NSL,NT,NTE,TER,CMCI,CACI,CMCE,CACE,NPQ,LRQ,NLE,NLPAIR,
     $ XNUK,XNU,XNUC,WNUK,WNU,WNUC,AEW,CP,P,CII,KIJ,LIJ,AIJ,AATIJ,
     $ CEIJ,XLAM,DDL,DWN,CRD,CRS,CVW,CSK)
C
C     Rudolf Loeser, 1992 Mar 26
C---- Sets up default values of atomic model parameters
C     for ions other than Hydrogen.
C     (This is version 2 of HOGRY.)
C     !DASH
      save
C     !DASH
      real*8 AATIJ, AEW, AIJ, CACE, CACI, CEIJ, CII, CMCE, CMCI, CP,
     $       CRD, CRS, CSK, CVW, DDL, DWN, ONE, P, TER, WNU, WNUC, WNUK,
     $       XII, XLAM, XNU, XNUC, XNUK, ZERO, dummy
      integer I, IL, IQOTL, IT, IU, IUL, J, KDEF, KIJ, KLIN, KXNUC, LDL,
     $        LDLMX, LIJ, LRQ, N, NL, NLE, NLPAIR, NPQ, NSL, NT, NTE
C     !COM
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
      equivalence (LEST(82),KXNUC)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !EJECT
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
      equivalence (LINKDS(12),LDL  )
      equivalence (LINKDS( 3),KLIN )
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
      equivalence (IQQ(344),IQOTL)
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
      external  GUAN, ILYA, BRUN, INDXUL, NUGA, RACHEL, RORQUAL, KEDGE,
     $          DATINI, SLIVER, INTRANS, PRIMROS, WINTRY, WHAPS, BOWER,
     $          PET, BLOOM, GANE, NOAM, DONEGAL, HI, BYE
      intrinsic max
C
C               NPQ(NSL), LRQ(NSL), NLPAIR(2,NL), CII(NTE,NSL), P(NSL),
      dimension NPQ(*),   LRQ(*),   NLPAIR(2,*),  CII(*),       P(*),
C
C               LIJ(MUL), WNU(NSL), CACI(NSL), DWN(LDLMX,NT), XLAM(NT),
     $          LIJ(*),   WNU(*),   CACI(*),   DWN(LDLMX,*),  XLAM(*),
C
C               CRD(LDLMX,NT), CVW(LDLMX,NT), CSK(LDLMX,NT), CACE(MUL),
     $          CRD(LDLMX,*),  CVW(LDLMX,*),  CSK(LDLMX,*),  CACE(*),
C
C               CEIJ(NTE,MUL), CMCE(MUL), CRS(NT), AEW(NSL), CP(NSL+1),
     $          CEIJ(*),       CMCE(*),   CRS(*),  AEW(*),   CP(*),
C
C               XNU(NSL), TER(NTE), NLE(NSL), CMCI(NSL), DDL(LDLMX,NT),
     $          XNU(*),   TER(*),   NLE(*),   CMCI(*),   DDL(LDLMX,*),
C
C               AIJ(NL,NL), KIJ(NL,NL), XNUC(NSL), AATIJ(NL,NL),
     $          AIJ(*),     KIJ(NL,*),  XNUC(*),   AATIJ(*),
C
C               WNUC(NSL)
     $          WNUC(*)
C
      call HI ('HOGRY')
C     !BEG
C
C     L e v e l s   (non-H)
C
C---- Set uo principal and rotational quantum numbers
      call BLOOM  (NSL, NPQ, LRQ, NLPAIR)
C---- Check NLE
      call GANE   (NLE, NSL)
C---- Frequencies (if necessary, from input wavenumber values)
      call ILYA   (XNUK, WNUK, NSL, XNU, WNU)
C---- WNUK and WNU (wavenumber equivalents of XNUK and XNU)
      call GUAN   (NSL, XNUK, XNU, WNUK, WNU)
C---- Auxiliary continua
      call NOAM   (NSL, KXNUC, XNUK, WNUK, XNUC, WNUC)
C---- Level edges (nm)
      call NUGA   (NSL, XNUC, XNU, AEW)
C---- CP (photoionization cross sections)
      call WINTRY (NSL, XNUK, XNU, CP)
C---- CII (collisional ionization coefficients)
      call BRUN   (TER, CMCI, CACI, dummy, XNU, XNUC, NPQ, LRQ, NLE,
     $             CII)
C     !EJECT
C
C     T r a n s i t i o n s   (non-H)
C
C---- Parameters kept in LIMBO, and KIJ
C
      if(IQOTL.gt.0) then
        KDEF = 3
      else
        KDEF = 1
      end if
      do 100 J = 1,NT
        call PET       (J)
        call INDXUL    (IU, IL, IUL)
        if(KIJ(IU,IL).le.0) then
C----     KIJ (transition code)
          call RACHEL  (IU, IL, IT)
          LINKLN(IT) = KDEF
          KIJ(IU,IL) = KDEF
        end if
        if(LIJ(IUL).le.ZERO) then
C----     LIJ (single rate switch)
          LIJ(IUL) = 1
        end if
  100 continue
C     !EJECT
C---- Parameters kept in General Data block
C
C---- AATIJ (A(u,l)'s for all transitions)
      call DONEGAL     (NL, AIJ, AATIJ, P, XNU)
C---- CEIJ (collisional excitation coefficients)
      call RORQUAL     (TER, CMCE, CACE, dummy, XNU, XNUC, P, AIJ,
     $                  AATIJ, NPQ, LRQ, CEIJ)
C
C---- Parameters destined for the Line Intensity Data block
C
      do 101 J = 1,NT
        call PET       (J)
        call INTRANS   (IU, IL, 'HOGRY', IUL)
C----   Line core wavelength
        call SLIVER    (XNU, IU, IL, XLAM(IUL))
C
        if((KLIN.eq.1).or.(KLIN.eq.2)) then
C----     Check blended line components
          call DATINI  (IU, IL, XNU, LDL, DDL(1,IUL), DWN(1,IUL))
C----     CRD (radiative broadening parameter)
          call PRIMROS (IU, IL, NL, CRD(1,IUL), LDL, AIJ, XNU)
C----     CVW (van der Waals broadening parameter)
          call BOWER   (IU, IL, NL, CVW(1,IUL), LDL, XNU)
C----     CSK (Stark broadening parameter)
          call KEDGE   (IU, IL, NL, CSK(1,IUL), LDL, XNU)
C----     Check CRS (resonance broadening parameter)
          CRS(IUL) = max(CRS(IUL), ZERO)
        end if
C
  101 continue
C
C---- Supplemental printout
      call WHAPS       (NSL, XNU, XNUK, WNU, WNUK, XNUC, WNUC)
C     !END
      call BYE ('HOGRY')
C
      return
      end
