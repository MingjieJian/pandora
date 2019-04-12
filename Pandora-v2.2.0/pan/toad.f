      subroutine TOAD
     $(XNUK,XNU,P,CP,CII,CMCI,CACI,YK,NTE,TER,AL,NSL,NL,KSHEL,NPQ,LRQ,
     $ NLQ,LCX,WNUK,WNU,AEW,RKMLT,LCH,ICHSW,IQRKE,XNUC,WNUC,KXNUC,LM1,
     $ LM3,ARR,BRR,IRR,QAR,LINE,IZZ,NO)
C
C     Rudolf Loeser, 1980 Dec 28
C---- Prints level input, for ATOM.
C     Put IZZ = 0 to print floating zero as blank;
C             = 1                           "0".
C     (This is version 4 of TOAD.)
C     !DASH
      save
C     !DASH
      real*8 AEW, AL, ARR, BRR, CACI, CII, CMCI, CP, P, RKMLT, TER, WNU,
     $       WNUC, WNUK, XNU, XNUC, XNUK, YK
      integer IB, ICHSW, IE, IQRKE, IRR, IZZ, J, KSHEL, KXNUC, LCH, LCX,
     $        LRQ, M, NL, NLQ, NO, NPQ, NSL, NTE
      logical LM1, LM3, ZH, ZL, ZN, ZX, ZZ, lummy
      character LINE*120, QAR*10
C     !DASH
      external  LINER, DOUGLAS, SATOR, FROG, ROTAS, CRAB, JATOR, VARUS,
     $          FRAG, FRUG, FREG, NAUGHTI, AREPO, OWEN, MORAY, FROCK,
     $          CRIB, HI, BYE
      intrinsic min
C
C               LCX(NL), CP(NSL), XNU(NSL), CMCI(NSL), AL(NSL), P(NSL),
      dimension LCX(*),  CP(*),   XNU(*),   CMCI(*),   AL(*),   P(*),
C
C               WNU(NSL), NLQ(NSL), CII(NTE,NSL), RKMLT(NSL), TER(NTE),
     $          WNU(*),   NLQ(*),   CII(*),       RKMLT(*),   TER(*),
C
C               LCH(NSL),  XNUC(NSL), WNUC(NSL), AEW(NSL), CACI(NSL),
     $          LCH(*),    XNUC(*),   WNUC(*),   AEW(*),   CACI(*),
C
C               YK(NSL), NPQ(NL), LRQ(NL)
     $          YK(*),   NPQ(*),  LRQ(*)
C
      dimension ARR(8), BRR(8), IRR(8), QAR(16)
C     !EJECT
C
      call HI ('TOAD')
C     !BEG
      call NAUGHTI    (LCH, 1, NSL, ZH)
C
      call NAUGHTI    (NPQ, 1, NSL, ZN)
      call NAUGHTI    (LRQ, 1, NSL, ZL)
      call NAUGHTI    (LCX, 1, NSL, ZX)
      ZZ = (.not.ZN).or.(.not.ZL).or.(.not.ZX)
C
      IE = 0
  100 continue
        IB = IE+1
        IE = min(IE+7,NSL)
        call DOUGLAS  (NO, NL, IB, IE, QAR, LINE)
        if(ZZ) then
          call JATOR  (NPQ, IB, IE, IRR, M)
          call FRAG   ('Principal quantum number n',
     $                 IRR, M, LINE, NO)
          call JATOR  (LRQ, IB, IE, IRR, M)
          call FRAG   ('Rotational quantum number l',
     $                 IRR, M, LINE, NO)
          call JATOR  (NLQ, IB, IE, IRR, M)
          call FRAG   ('Number of "nl" electrons',
     $                 IRR, M, LINE, NO)
          call CRIB   (LCX, NPQ, LRQ, IB, IE, QAR, BRR, M)
          call FRUG   ('Charge exchange level',
     $                 QAR, M, LINE, NO)
          call FROG   ('Charge exchange parameter',
     $                 BRR, M, LINE, IZZ, NO)
          call LINER  (1, NO)
        end if
        call SATOR    (XNU, IB, IE, ARR, M)
        call FROG     ('Frequency Interval (peta-Hz)',
     $                 ARR, M, LINE, IZZ, NO)
        call SATOR    (WNU, IB, IE, ARR, M)
        call FROCK    ('Wavenumber (/cm)',
     $                 ARR, M, LINE, IZZ, NO)
        if(KXNUC.gt.0) then
          call SATOR  (XNUC, IB, IE, ARR, M)
          call AREPO  (ARR, M, XNUK)
          call FROG   ('auxiliary continuum',
     $                 ARR, M, LINE, IZZ, NO)
          call SATOR  (WNUC, IB, IE, ARR, M)
          call AREPO  (ARR, M, WNUK)
          call FROCK  (' ',
     $                 ARR, M, LINE, IZZ, NO)
        end if
        call SATOR    (AEW, IB, IE, ARR, M)
        call FROG     ('Threshhold wavelength (nm)',
     $                 ARR, M, LINE, IZZ, NO)
        call LINER    (1, NO)
C     !EJECT
        call SATOR    (P, IB, IE, ARR, M)
        call FROG     ('Statistical Weight',
     $                 ARR, M, LINE, IZZ, NO)
        call SATOR    (CP, IB, IE, ARR, M)
        call FROG     ('CP: Photoioniz. cross-section (cm**2)',
     $                 ARR, M, LINE, IZZ, NO)
        call SATOR    (RKMLT, IB, IE, ARR, M)
        call FROG     ('RKMULT: RK enhancement factors',
     $                 ARR, M, LINE, IZZ, NO)
        do 101 J = 1,NTE
          call ROTAS  (CII, IB, IE, NSL, TER, J, NTE, ARR, M)
          if(J.eq.1) then
            call FROG ('CI: Collisional ioniz. coefficient',
     $                 ARR, M, LINE, IZZ, NO)
          else if(J.eq.2) then
            call FROG ('(as function of temperature)',
     $                 ARR, M, LINE, IZZ, NO)
          else
            call FROG (' ',
     $                 ARR, M, LINE, IZZ, NO)
          end if
  101   continue
        call SATOR    (CMCI, IB, IE, ARR, M)
        call FREG     ('MCI: CI multiplier',
     $                 ARR, M, LINE, NO, lummy)
        call SATOR    (CACI, IB, IE, ARR, M)
        call FROG     ('ACI: CI addend',
     $                 ARR, M, LINE, IZZ, NO)
        call JATOR    (LCH, IB, IE, IRR, M)
        call FRAG     ('LCH: collisions-with-hydrogen code',
     $                 IRR, M, LINE, NO)
        call SATOR    (YK, IB, IE, ARR, M)
        call FROG     ('Hydrogen Recombination parameter',
     $                 ARR, M, LINE, IZZ, NO)
        if(NSL.gt.NL) then
          call CRAB   (AL, IB, NL, NSL, ARR, M)
          call FROG   ('Supplementary Recombination fraction',
     $                 ARR, M, LINE, IZZ, NO)
        end if
        if(IE.lt.NSL) goto 100
      continue
C     !EJECT
      call LINER      (1, NO)
      call VARUS      ('Continuum Frequency Interval (peta-Hz)',
     $                 XNUK, LINE, IZZ, NO, 0)
      call VARUS      ('Continuum wavenumber (/cm)',
     $                 WNUK, LINE, IZZ, NO, 1)
      if(KSHEL.gt.0) then
        call LINER    (1, NO)
        call VARUS    ('K-Shell photoioniz. cross-section',
     $                 CP(NSL+1), LINE, IZZ, NO, 0)
      end if
C
      call OWEN       (NO)
      call MORAY      (NO, LM1, LM3, IZZ, IQRKE, ZH)
      call OWEN       (NO)
C     !END
      call BYE ('TOAD')
C
      return
      end
