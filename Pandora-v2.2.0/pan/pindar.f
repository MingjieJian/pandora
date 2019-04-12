      subroutine PINDAR
     $(IMX,IDM,JMX,WAVE,DL,EMU,VXS,DP,DW,XNE,MPROM,DDL,FDDL,CDL,LDL,
     $ GTN,VP,DV,AA,UU,PHP,PHC,CKP,PHI,CKL,W,IW,DUMP)
C
C     Rudolf Loeser, 1982 Jan 27
C---- Sets up arrays of Absorption Profile and Opacity along a ray.
C     CKL and PHI are each arrays of the form (IMX,JMX).
C     !DASH
      save
C     !DASH
      real*8 AA, CDL, CKL, CKP, DDL, DL, DP, DV, DW, EMU, FDDL, GTN,
     $       PHC, PHI, PHP, UU, VP, VXS, W, WAVE, XNE
      integer I, IDM, IMX, IW, JMX, K1, LDL, MPROM, MUSE
      logical DUMP
C     !DASH
      external DERBY, PROFILE, MOVED, ARIADNE, ARRMUL, HI, BYE
C
      dimension W(*), IW(*)
C
C               GTN(JMX), CKP(JMX), PHP(JMX), UU(JMX,LDL), AA(JMX,LDL),
      dimension GTN(*),   CKP(*),   PHP(*),   UU(*),       AA(*),
C
C               PHI(IDM,JMX), DW(JMX), VXS(JMX), PHC(JMX,LDL), VP(JMX),
     $          PHI(IDM,*),   DW(*),   VXS(*),   PHC(*),       VP(*),
C
C               DP(JMX,LDL), CDL(LDL), DV(JMX,LDL), DDL(LDL), EMU(JMX),
     $          DP(*),       CDL(*),   DV(*),       DDL(*),   EMU(*),
C
C               XNE(JMX), DL(1), CKL(IDM,JMX), FDDL(JMX),
     $          XNE(*),   DL(*), CKL(IDM,*),   FDDL(*)
C
      data K1,MUSE /1, 1/
C
      call HI ('PINDAR')
C     !BEG
      do 100 I = 1,IMX
        call DERBY     (VXS, EMU, I, JMX, VP)
        call PROFILE   (WAVE, DDL, FDDL, CDL, LDL, DL, K1, MPROM, MUSE,
     $                  XNE, DP, DW, VP, JMX, PHP, DV, AA, UU, PHC,
     $                  W, IW)
        call ARRMUL    (GTN, PHP, CKP, JMX)
        call MOVED     (PHP, 1, JMX, PHI(I,1), IDM, JMX)
        call MOVED     (CKP, 1, JMX, CKL(I,1), IDM, JMX)
        if(DUMP) then
          call ARIADNE (I, JMX, EMU, VXS, DP, DW, GTN, VP, DV, PHP, CKP)
        end if
  100 continue
C     !END
      call BYE ('PINDAR')
C
      return
      end
