      subroutine LIZARD
     $(NL,NSL,AIJ,AATIJ,CEIJ,NTE,TER,KIJ,PMSK,PW,JU,JL,MTR,KU,KL,KTR,
     $ CRD,CVW,CSK,CRS,YLI,YCO,SEM,OLN,PRD,DPC,GMA,OML,OLL,WAV,UIR,
     $ FLX,SFT,DLL,SFP,FDB,SBI,WVN,IQLSP,IQLSG,LSFGC,HBC,CKH,OSF,BOC,
     $ DPM,PXC,PXP,PXR,LIJ,CMCE,CACE,PCE,LCH,IQCEF,ICHSW,ARR,BRR,QAR,
     $ LINE,IZZ,NO)
C
C     Rudolf Loeser, 1980 Dec 28
C---- Prints transition input, for ATOM.
C     Put IZZ = 0 to print floating zero as blank;
C             = 1                           "0".
C     (This is version 3 of LIZARD.)
C     !DASH
      save
C     !DASH
      real*8 AATIJ, AIJ, ARR, BOC, BRR, CACE, CEIJ, CKH, CMCE, CRD, CRS,
     $       CSK, CVW, DLL, DPC, DPM, FDB, FLX, GMA, OLL, OLN, OML, OSF,
     $       PCE, PMSK, PRD, PW, PXC, PXP, PXR, SBI, SEM, SFP, SFT, TER,
     $       UIR, WAV, WVN, YCO, YLI, dummy
      integer IB, ICHSW, IE, IQCEF, IQLSG, IQLSP, IZZ, J, JL, JU, KIJ,
     $        KL, KOUNT, KTR, KU, LCH, LIJ, LSFGC, MTR, MUL, NL, NO,
     $        NSL, NTE, jummy
      logical HBC, KSTARK, ZH, lummy
      character BLANK*1, LINE*120, QAR*10, QLL*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  TURTLE, ROLAND, FROG, IGUANA, JOLL, CLICK, FRUG, ROALD,
     $          TRACK, TRICK, TRUCK, ARSOOF, LINER, VARUS, OWEN, MARAY,
     $          TRECK, GRINDLE, CLOOCK, NARVA, RONALD, NALDOR, NAUGHTD,
     $          GATOR, GAVIAL, CRICK, NAUGHTI, CLUCK, OPIS, BRIG, FREG,
     $          RANDALL, FROCK, CLACK, HI, BYE
C
      intrinsic min
C
C               The size of all these arrays is MUL
C
      dimension LIJ(*), SBI(*), OLL(*), KIJ(*), JU(*) , JL(*) ,
     $          CRD(*), CVW(*), CSK(*), CRS(*), YLI(*), YCO(*),
     $          SEM(*), OLN(*), PRD(*), DPC(*), GMA(*), PCE(*),
     $          OML(*), WAV(*), UIR(*), FLX(*), SFT(*), DLL(*),
     $          SFP(*), FDB(*), WVN(*), KU(*) , KL(*) , CKH(*),
     $          OSF(*), BOC(*), DPM(*), PXC(*), PXP(*), PXR(*)
C
C               CEIJ(NTE,MUL), AIJ(NL,NL), CMCE(MUL), AATIJ(NL,NL),
      dimension CEIJ(*),       AIJ(*),     CMCE(*),   AATIJ(*),
C
C               TER(NTE), LCH(NSL), CACE(MUL)
     $          TER(*),   LCH(*),   CACE(*)
C
      dimension ARR(8), BRR(8), QAR(16), QLL(16)
C     !EJECT
C
      call HI ('LIZARD')
C     !BEG
      MUL = NL*(NL-1)/2
      IE  = 0
  100 continue
        IB = IE+1
        IE = min((IE+7),MTR)
        call TURTLE   (JU, JL, IB, IE, KIJ, QAR, LINE, NO)
C
        call GAVIAL   (DLL, JU, JL, IB, IE, KIJ, ARR, KOUNT)
        call CLACK    (ARR, QLL, KOUNT)
        call FRUG     ('Line components',
     $                 QLL, KOUNT, LINE, NO)
C
        call ROLAND   (WAV, JU, JL, IB, IE, NL, AIJ, ARR, KOUNT)
        call FROG     ('Wavelength (nano-m)',
     $                 ARR, KOUNT, LINE, IZZ, NO)
C
        call ROLAND   (WVN, JU, JL, IB, IE, NL, AIJ, ARR, KOUNT)
        call FROCK    ('Wavenumber (/cm)',
     $                 ARR, KOUNT, LINE, IZZ, NO)
C
        call NALDOR   (AIJ, AATIJ, JU, JL, IB, IE, NL, ARR, KOUNT)
        call BRIG     ('Einstein A Value',
     $                 ARR, KOUNT, LINE, IZZ, NO)
C
        call ROLAND   (OSF, JU, JL, IB, IE, NL, AIJ, ARR, KOUNT)
        call FROG     ('Oscillator strength',
     $                 ARR, KOUNT, LINE, IZZ, NO)
C
        if(NTE.gt.1) then
          call LINER  (1, NO)
        end if
        do 101 J = 1,NTE
          call IGUANA (CEIJ, JU, JL, IB, IE, TER, J, NTE, ARR, KOUNT)
          if(J.eq.1) then
            call FROG ('CE: Collisional Excitation Coefficient',
     $                 ARR, KOUNT, LINE, IZZ, NO)
          else if(J.eq.2) then
            call FROG ('(as function of temperature)',
     $                 ARR, KOUNT, LINE, IZZ, NO)
          else
            call FROG (BLANK,
     $                 ARR, KOUNT, LINE, IZZ, NO)
          end if
  101   continue
C     !EJECT
        call IGUANA   (CMCE, JU, JL, IB, IE, dummy, 1, 1, ARR, KOUNT)
        call FREG     ('MCE: CE multiplier',
     $                 ARR, KOUNT, LINE, NO, lummy)
        call IGUANA   (CACE, JU, JL, IB, IE, dummy, 1, 1, ARR, KOUNT)
        call FROG     ('ACE: CE addend',
     $                 ARR, KOUNT, LINE, IZZ, NO)
        if(NTE.gt.1) then
          call LINER  (1, NO)
        end if
C
        call GAVIAL   (CRD, JU, JL, IB, IE, KIJ, ARR, KOUNT)
        call RANDALL  (ARR, QLL, QAR, KOUNT, IZZ)
        call FRUG     ('Radiative half width (A)',
     $                 QAR, KOUNT, LINE, NO)
C
        call GAVIAL   (CVW, JU, JL, IB, IE, KIJ, ARR, KOUNT)
        call RANDALL  (ARR, QLL, QAR, KOUNT, IZZ)
        call FRUG     ('van der Waals half width (A)',
     $                 QAR, KOUNT, LINE, NO)
C
        call GAVIAL   (CSK, JU, JL, IB, IE, KIJ, ARR, KOUNT)
        call GAVIAL   (CKH, JU, JL, IB, IE, KIJ, BRR, jummy)
        call RONALD   (ARR, BRR, QLL, QAR, KOUNT, HBC, IZZ)
        call FRUG     ('Stark half width (A)',
     $                 QAR, KOUNT, LINE, NO)
C
        call GAVIAL   (CRS, JU, JL, IB, IE, KIJ, ARR, KOUNT)
        call FROG     ('Resonance half width (A)',
     $                 ARR, KOUNT, LINE, IZZ, NO)
C
        call GAVIAL   (DPC, JU, JL, IB, IE, KIJ, ARR, KOUNT)
        call TRACK    (ARR, QAR, KOUNT)
        call FRUG     ('Damping component selector',
     $                 QAR, KOUNT, LINE, NO)
C
        call GAVIAL   (PRD, JU, JL, IB, IE, KIJ, ARR, KOUNT)
        call TRICK    (ARR, QAR, KOUNT)
        call FRUG     ('Frequency Redistribution',
     $                 QAR, KOUNT, LINE, NO)
C
        call GAVIAL   (GMA, JU, JL, IB, IE, KIJ, ARR, KOUNT)
        call FROG     ('GMMA for P.R.D.',
     $                 ARR, KOUNT, LINE, IZZ, NO)
C
        call GAVIAL   (PXC, JU, JL, IB, IE, KIJ, ARR, KOUNT)
        call FROG     ('XC (for DR) for P.R.D.',
     $                 ARR, KOUNT, LINE, IZZ, NO)
C
        call GAVIAL   (PXP, JU, JL, IB, IE, KIJ, ARR, KOUNT)
        call FROG     ('XP (for DR) for P.R.D.',
     $                 ARR, KOUNT, LINE, IZZ, NO)
C
        call GAVIAL   (PXR, JU, JL, IB, IE, KIJ, ARR, KOUNT)
        call FROG     ('XR (for DR) for P.R.D.',
     $                 ARR, KOUNT, LINE, IZZ, NO)
C
      if(IE.lt.MTR) goto 100
C
      call LINER      (2, NO)
      call NAUGHTD    (CSK, 1, MUL, KSTARK)
      if(.not.KSTARK) then
        call VARUS    ('Exponent for Stark Broadening',
     $                 PW, LINE, IZZ, NO, 0)
        call LINER    (1, NO)
      end if
      call VARUS      ('Multiplier for default Stark half width',
     $                 PMSK, LINE, IZZ, NO, 0)
C
C---- Write legend
      call OWEN       (NO)
      call NAUGHTI    (LCH, 1, NSL, ZH)
      call MARAY      (NO, IZZ, ZH)
      call OWEN       (NO)
C     !EJECT
      call LINER      (5, NO)
      write(NO,102)
  102 format(' ','Line Source Function calculation ',
     $           'control parameters.')
      IE = 0
  103 continue
        IB = IE+1
        IE = min((IE+7),KTR)
        call TURTLE   (KU, KL, IB, IE, KIJ, QAR, LINE, NO)
C
        call GATOR    (FDB, KU, KL, IB, IE, KIJ, ARR, KOUNT)
        call CLUCK    (ARR, QAR, KOUNT)
        call FRUG     ('Source Function background-opacity (1)',
     $                 QAR, KOUNT, LINE, NO)
C
        call GAVIAL   (BOC, KU, KL, IB, IE, KIJ, ARR, KOUNT)
        call GRINDLE  (ARR, QAR, KOUNT)
        call FRUG     ('Include Line-background-opacity (2)',
     $                 QAR, KOUNT, LINE, NO)
C
        call GAVIAL   (OML, KU, KL, IB, IE, KIJ, ARR, KOUNT)
        call FROG     ('Line-background-opacity multiplier (2)',
     $                 ARR, KOUNT, LINE, IZZ, NO)
C
        call GAVIAL   (OLL, KU, KL, IB, IE, KIJ, ARR, KOUNT)
        call FROG     ('Line opacity multiplier (3)',
     $                 ARR, KOUNT, LINE, IZZ, NO)
C
        call GAVIAL   (DPM, KU, KL, IB, IE, KIJ, ARR, KOUNT)
        call FROG     ('Damping multiplier',
     $                 ARR, KOUNT, LINE, IZZ, NO)
C
        call GAVIAL   (PCE, KU, KL, IB, IE, KIJ, ARR, KOUNT)
        call FROG     ('CE-multiplier increment (4)',
     $                 ARR, KOUNT, LINE, IZZ, NO)
C
        call GAVIAL   (YCO, KU, KL, IB, IE, KIJ, ARR, KOUNT)
        call CLICK    (ARR, QAR, KOUNT)
        call FRUG     ('Background source function method',
     $                 QAR, KOUNT, LINE, NO)
C
        call GATOR    (SFT, KU, KL, IB, IE, KIJ, ARR, KOUNT)
        call JOLL     (ARR, QAR, KOUNT)
        call FRUG     ('Line Source Function solution type (5)',
     $                 QAR, KOUNT, LINE, NO)
C
        call ARSOOF   (LIJ, KU, KL, IB, IE, KIJ, ARR, KOUNT)
        call CLOOCK   (ARR, QAR, KOUNT)
        call FRUG     ('Statistical Equilibrium "rates" (6)',
     $                 QAR, KOUNT, LINE, NO)
C
        call GATOR    (SBI, KU, KL, IB, IE, KIJ, BRR, KOUNT)
        call ROALD    (ARR, BRR, QAR, KOUNT)
        call FRUG     ('"Sobolev" indices',
     $                 QAR, KOUNT, LINE, NO)
C
        call GATOR    (SEM, KU, KL, IB, IE, KIJ, ARR, KOUNT)
        call CRICK    (ARR, QAR, KOUNT)
        call FRUG     ('Statistical Equilibrium method (7)',
     $                 QAR, KOUNT, LINE, NO)
C
        call GATOR    (YLI, KU, KL, IB, IE, KIJ, ARR, KOUNT)
        call CLICK    (ARR, QAR, KOUNT)
        call FRUG     ('Line source function method',
     $                 QAR, KOUNT, LINE, NO)
C
        call GAVIAL   (SFP, KU, KL, IB, IE, KIJ, ARR, KOUNT)
        call OPIS     (ARR, QAR, KOUNT)
        call FRUG     ('"LSFPRINT" (8)',
     $                 QAR, KOUNT, LINE, NO)
C
        call GATOR    (UIR, KU, KL, IB, IE, KIJ, ARR, KOUNT)
        call TRECK    (ARR, QAR, KOUNT)
        call FRUG     ('Use only input-Rho',
     $                 QAR, KOUNT, LINE, NO)
C
        call GAVIAL   (FLX, KU, KL, IB, IE, KIJ, ARR, KOUNT)
        call GRINDLE  (ARR, QAR, KOUNT)
        call FRUG     ('Show Radiative Force Calculation',
     $                 QAR, KOUNT, LINE, NO)
C
        call GAVIAL   (OLN, KU, KL, IB, IE, KIJ, ARR, KOUNT)
        call TRUCK    (ARR, 1, QAR, KOUNT)
        call FRUG     ('Show computed line profile',
     $                 QAR, KOUNT, LINE, NO)
C
        call TRUCK    (ARR, 2, QAR, KOUNT)
        call FRUG     ('Show eclipse line profile',
     $                 QAR, KOUNT, LINE, NO)
C
      if(IE.lt.KTR) goto 103
C
C---- Write legend
      call OWEN       (NO)
      call NARVA      (NO, IQLSP, IQLSG, IQCEF, LSFGC, IZZ)
      call OWEN       (NO)
C     !END
      call BYE ('LIZARD')
C
      return
      end
