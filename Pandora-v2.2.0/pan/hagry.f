      subroutine HAGRY
     $(N,NL,NSL,NT,NTE,TER,IHSSP,ISTRK,FRCDL,FMCDL,FSTKM,LDLMX,JDDL,
     $ IHSSW,PMSK,XNE,XNC,TE,V,CMCI,CACI,CMCE,CACE,NPQ,LRQ,NLE,XNUK,
     $ XNU,XNUC,WNUK,WNU,WNUC,AEW,CP,P,CII,KIJ,LIJ,AIJ,AATIJ,CEIJ,
     $ RFXNC,IRFNC,XLAM,CRD,CVW,CSK,CRS,DWN,CDL,DDL,STNE,IST,KST)
C
C     Rudolf Loeser, 1992 Mar 26
C---- Sets up default values of atomic model parameters for Hydrogen.
C     (This is version 3 of HAGRY.)
C     !DASH
      save
C     !DASH
      real*8 AATIJ, AEW, AIJ, CACE, CACI, CDL, CEFEQ, CEIJ, CII, CMCE,
     $       CMCI, CP, CRD, CRS, CSK, CVW, DDL, DWN, FMCDL, FRCDL,
     $       FSTKM, ONE, P, PMSK, RFXNC, STNE, TE, TER, V, WNU, WNUC,
     $       WNUK, XII, XLAM, XNC, XNE, XNU, XNUC, XNUK, ZERO
      integer I, IHSSP, IHSSW, IL, IQHBR, IQOTL, IRFNC, IST, ISTRK, IT,
     $        IU, IUL, J, JDAIJ, JDDL, JDKNT, KDEF, KHSSW, KIJ, KLIN,
     $        KST, KXNUC, LDL, LDLMX, LIJ, LRQ, N, NL, NLE, NPQ, NSL,
     $        NT, NTE
      logical AGOOD, TGOOD
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
      equivalence (LINKDS(12),LDL  )
      equivalence (LINKDS( 3),KLIN )
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
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
      equivalence (MEST( 9),JDAIJ)
      equivalence (MEST(21),JDKNT)
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
      equivalence (IQQ(273),IQHBR)
      equivalence (IQQ(344),IQOTL)
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
      equivalence (LEST(82),KXNUC)
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external GANE, GUAN, BRAUN, BROWN, BRUN, INTRANS, INDXUL, RACHEL,
     $         KERBAU, NOVARA, SLIVER, FOSTUR, PRIMROS, KALAMAS, BLOND,
     $         LANGUR, FAHAKA, DONEGAL, RORQUAL, HALT, PET, NUGA, IVAN,
     $         WHAPS, NOAM, HI, BYE
C
C               P(NSL), LRQ(NSL), XNU(NSL), WNU(NSL), CP(NSL+1), TE(N),
      dimension P(*),   LRQ(*),   XNU(*),   WNU(*),   CP(*),     TE(*),
C
C               NPQ(NSL), CII(NTE,NSL), AIJ(NL,NL), KST(NT), CMCI(NSL),
     $          NPQ(*),   CII(*),       AIJ(NL,*),  KST(*),  CMCI(*),
C
C               XNC(N), XNE(N), CRD(LDLMX,NT), CVW(LDLMX,NT), XLAM(NT),
     $          XNC(*), XNE(*), CRD(LDLMX,*),  CVW(LDLMX,*),  XLAM(*),
C
C               CSK(LDLMX,NT), DWN(LDLMX,NT), CDL(LDLMX,NT), CACE(MUL),
     $          CSK(LDLMX,*),  DWN(LDLMX,*),  CDL(LDLMX,*),  CACE(*),
C
C               KIJ(NL,NL), NLE(NSL), IST(NT), LIJ(MUL), 
     $          KIJ(NL,*),  NLE(*),   IST(*),  LIJ(*),   
C
C               CRS(NT), STNE(NT), CMCE(MUL), AEW(NSL), V(N), TER(NTE),
     $          CRS(*),  STNE(*),  CMCE(*),   AEW(*),   V(*), TER(*),
C
C               CEIJ(NTE,MUL), CACI(NSL), DDL(LDLMX,NT), AATIJ(NL,NL),
     $          CEIJ(*),       CACI(*),   DDL(LDLMX,*),  AATIJ(*),
C
C               WNUC(NSL), XNUC(NSL)
     $          WNUC(*),   XNUC(*)
C     !EJECT
C
      call HI ('HAGRY')
C     !BEG
C
C     L e v e l s   (H)
C
C---- Set up principal and rotational quantum numbers
      call BLOND (NSL, NPQ, LRQ)
C---- Check NLE
      call GANE  (NLE, NSL)
C---- Frequencies
      call IVAN  (XNUK, XNU, NPQ, NSL)
C---- WNUK and WNU (wavenumber-equivalents of XNUK and XNU)
      call GUAN  (NSL, XNUK, XNU, WNUK, WNU)
C---- Auxiliary continua
      call NOAM  (NSL, KXNUC, XNUK, WNUK, XNUC, WNUC)
C---- Level edges (nm)
      call NUGA  (NSL, XNUC, XNU, AEW)
C---- P (statistical weights)
      call BRAUN (NSL, NPQ, P)
C---- CP (photoionization cross-sections)
      call BROWN (NSL, NPQ, CP)
C---- CII (collisional ionization coefficients)
      RFXNC = XNC(IRFNC)
      call BRUN  (TER, CMCI, CACI, RFXNC, XNU, XNUC, NPQ, LRQ, NLE,
     $            CII)
C     !EJECT
C
C     T r a n s i t i o n s   (H)
C
C---- Parameters kept in LIMBO, and KIJ
C
      if(IQOTL.gt.0) then
        KDEF = 3
      else
        KDEF = 1
      end if
      do 100 J = 1,NT
        call PET      (J)
        call INDXUL   (IU, IL, IUL)
        call RACHEL   (IU, IL, IT)
        if(AIJ(IU,IL).eq.ZERO) then
C----     A (Einstein A coefficient) and KIJ (transition code)
          call KERBAU (IU, IL, ZERO, ZERO, AIJ(IU,IL))
          JDAIJ = JDAIJ+1
          if(KIJ(IU,IL).le.0) then
            KIJ(IU,IL) = KDEF
            LINKLN(IT) = KDEF
          end if
        else
          JDKNT = JDKNT+1
        end if
        if(KIJ(IU,IL).eq.0) then
C----     KIJ (transition code)
          KIJ(IU,IL) = KDEF
          LINKLN(IT) = KDEF
        end if
        if(LIJ(IUL).le.0) then
C----     LIJ (single rate switch)
          LIJ(IUL) = 1
        end if
        if((IQHBR.gt.0).and.(IL.gt.4)) then
C----     Set up ion broadening
          LINDAM(IT) = LINDAM(IT)+16
        end if
  100 continue
C
C
C---- Parameters kept in General Data block
C
C---- AATIJ (A(u,l)'s for all transitions)
      call DONEGAL    (NL, AIJ, AATIJ, P, XNU)
C---- CEIJ (collisional excitation coefficients)
      call RORQUAL    (TER, CMCE, CACE, RFXNC, XNU, XNUC, P, AIJ,
     $                 AATIJ, NPQ, LRQ, CEIJ)
C     !EJECT
C---- Parameters destined for the Line Intensity Data block   (H)
C
      AGOOD = .true.
      do 101 J = 1,NT
        call PET       (J)
        call INTRANS   (IU, IL, 'HAGRY', IUL)
C----   Line core wavelength
        call SLIVER    (XNU, IU, IL, XLAM(IUL))
C
        if((KLIN.eq.1).or.(KLIN.eq.2)) then
C----     Compute and/or check blended line components
          call FOSTUR  (N, LDL, LINLDL(J), JDDL, IHSSP, XNU, XNE, TE,
     $                  V, ISTRK, FRCDL, FMCDL, FSTKM, IU, IL, LDLMX,
     $                  IST(IUL), STNE(IUL), DWN(1,IUL), CDL(1,IUL),
     $                  DDL(1,IUL), TGOOD)
          if(.not.TGOOD) then
            AGOOD = .false.
          end if
C----     CRD (radiative broadening parameter)
          call PRIMROS (IU, IL, NL, CRD(1,IUL), LDL, AIJ, XNU)
C----     CVW (van der Waals broadening parameter)
          call KALAMAS (IU, IL, NL, CVW(1,IUL), LDL, XLAM(IUL))
C----     CSK (Stark broadening paraphernalia)
          call LANGUR  (KST(IUL), IHSSW, KHSSW)
          call NOVARA  (IU, IL, NL, CSK(1,IUL), LDL, KHSSW, PMSK)
C----     CRS (resonance broadening parameter)
          call FAHAKA  (IU, IL, NL, CRS(IUL), AIJ, XNU)
        end if
C
  101 continue
C
      if(.not.AGOOD) then
        write (MSSLIN(1),102)
  102   format('Trouble with Stark splitting components (input ',
     $         'parameter IHSSP, option HSTSUMM).')
        call HALT      ('HAGRY', 1)
      end if
C
C---- Supplemental printout
      call WHAPS       (NSL, XNU, XNUK, WNU, WNUK, XNUC, WNUC)
C     !END
      call BYE ('HAGRY')
C
      return
      end
