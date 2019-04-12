      subroutine BUFFALO
     $(XLB1,NT,JU,JL,CRD,CVW,CSK,CRS,YLI,SEM,OLN,PRD,DPC,SFT,GMA,
     $ KDDR,KDRX,KNZGM,UIR,FLX,DLL,SFP,FDB,SBI,WAV,WVN,CKH,BOC,DPM,
     $ PXC,PXP,PXR)
C
C     Rudolf Loeser, 1978 Sep 13
C---- Get Line Transition data from Line Intensity Data Blocks
C     and from arrays in LIMBO, converting all switches
C     to floating point form, for GECKO.
C     (This is version 2 of BUFFALO.)
C     !DASH
      save
C     !DASH
      real*8 BOC, CKH, CRD, CRS, CSK, CVW, DLL, DPC, DPM, FDB, FLX, GMA,
     $       OLN, PRD, PXC, PXP, PXR, SBI, SEM, SFP, SFT, UIR, WAV, WVN,
     $       XLB1, YLI, ZERO, dummy
      integer I, IL, ISB1, ISB2, IT, IU, JL, JU, KDDR, KDRX, KNZGM,
     $        LUEO, MMCRD, MMCRS, MMCSK, MMCVW, MMDPM, MMGMA, MMLAM,
     $        MMSB1, MMSB2, MMSTK, MMXC, MMXP, MMXR, MMY, NT
      logical OK
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
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML( 7),MMCRD)
      equivalence (MML( 8),MMCVW)
      equivalence (MML( 9),MMCSK)
      equivalence (MML(10),MMCRS)
      equivalence (MML(44),MMGMA)
      equivalence (MML(42),MMXC )
      equivalence (MML( 5),MMY  )
      equivalence (MML(49),MMSB1)
      equivalence (MML(50),MMSB2)
      equivalence (MML( 2),MMLAM)
      equivalence (MML(62),MMSTK)
      equivalence (MML( 6),MMDPM)
      equivalence (MML(43),MMXP )
      equivalence (MML(63),MMXR )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external LIDGET, INDXUL, JOGI, WANDA, MESHED, ABORT, HI, BYE
C
C               CRD(MUL), CVW(MUL), CSK(MUL), CRS(MUL), PXC(MUL),
      dimension CRD(*),   CVW(*),   CSK(*),   CRS(*),   PXC(*),
C
C               YLI(MUL), SEM(MUL), OLN(MUL), PRD(MUL), DPC(MUL),
     $          YLI(*),   SEM(*),   OLN(*),   PRD(*),   DPC(*),
C
C               UIR(MUL), FLX(MUL), DLL(MUL), SFP(MUL), FDB(MUL),
     $          UIR(*),   FLX(*),   DLL(*),   SFP(*),   FDB(*),
C
C               WAV(MUL), SBI(MUL), SFT(MUL), GMA(MUL), CKH(MUL),
     $          WAV(*),   SBI(*),   SFT(*),   GMA(*),   CKH(*),
C
C               DPM(MUL), JU (MUL), JL (MUL), WVN(MUL), BOC(MUL),
     $          DPM(*),   JU (*),   JL (*),   WVN(*),   BOC(*),
C
C               PXP(MUL), PXR(MUL), XLB1(Li1len)
     $          PXP(*),   PXR(*),   XLB1(*)
C
      call HI ('BUFFALO')
C     !BEG
      KDDR  = 0
      KDRX  = 0
      KNZGM = 0
C---- Loop over all Line Intensity Data blocks (Note: PIPIT has set up
C     a data block for every INPAIR transition)
      do 101 I = 1,NT
        IU = LINIU(I)
        IL = LINIL(I)
C----   Get index of corresponding data array slot (cf. JACKAL)
        call INDXUL   (IU, IL, IT)
C----   Make sure that transition index pairs agree
        OK = (IU.eq.JU(IT)).and.(IL.eq.JL(IT))
        if(.not.OK) then
          call MESHED ('BUFFALO', 1)
          write (LUEO,100) I,IT,IU,IL,JU(IT),JL(IT)
  100     format(' ','Index mixup; I =',I12,', IT =',I12/
     $           ' ','IU =',I12,', IL =',I12,'; JU =',I12,', JL =',I12)
          call ABORT
        end if
C     !EJECT
C----   Retrieve the corresponding Line Intensity Data block, and make
C       sure it is the right one
        call LIDGET (XLB1, 1, dummy, 0, dummy, 0, I)
C----   Check PRD data
        call JOGI   (LINPRD(I), XLB1(MMXC), KDDR, KDRX, XLB1(MMGMA),
     $               KNZGM)
C----   Set up Sobolev data
        ISB1    = XLB1(MMSB1)
        ISB2    = XLB1(MMSB2)
        SBI(IT) = 10000*ISB1+ISB2
C----   Pull out data from Line Intensity Data block
        CRD(IT) = XLB1(MMCRD)
        CVW(IT) = XLB1(MMCVW)
        CSK(IT) = XLB1(MMCSK)
        CRS(IT) = XLB1(MMCRS)
        DPM(IT) = XLB1(MMDPM)
        YLI(IT) = XLB1(MMY  )
        GMA(IT) = XLB1(MMGMA)
        PXC(IT) = XLB1(MMXC )
        PXP(IT) = XLB1(MMXP )
        PXR(IT) = XLB1(MMXR )
        WAV(IT) = XLB1(MMLAM)
        CKH(IT) = XLB1(MMSTK)
C       (compute wavenumber)
        call WANDA  (WAV(IT), WVN(IT))
C----   Pull out data from LIMBO
        PRD(IT) = LINPRD(I)
        DPC(IT) = LINDAM(I)
        SFT(IT) = LINTPS(I)
        UIR(IT) = LININR(I)
        SEM(IT) = LINMSE(I)
        OLN(IT) = LINPRO(I)
        FLX(IT) = LINFLX(I)
        DLL(IT) = LINLDL(I)
        SFP(IT) = LINPRN(I)
        FDB(IT) = LINFDB(I)
        BOC(IT) = LINOML(I)
  101 continue
C     !END
      call BYE ('BUFFALO')
C
      return
      end
