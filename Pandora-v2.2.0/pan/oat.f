      subroutine OAT
     $(PHI,ELL,PROD,M,YL,YU)
C
C     Rudolf Loeser, 1991 Dec 23
C---- Computes ordinate limits for EMORY.
C     (This is version 4 of OAT.)
C     !DASH
      save
C     !DASH
      real*4 ELL, PHI, PROD, YL, YU, ZERO, ZL, ZU
      integer M, MNE, MNP, MNV, MXE, MXP, MXV
C     !DASH
      external  MNMXR, ABOVEL, BELOWL, HI, BYE
      intrinsic min, max
C
C               PHI(M), ELL(M), PROD(M)
      dimension PHI(*), ELL(*), PROD(*)
C
      data      ZERO /0.E0/
C
      call HI ('OAT')
C     !BEG
      call MNMXR  (PHI ,1,M,ZERO,MNV,MXV)
      call MNMXR  (ELL ,1,M,ZERO,MNE,MXE)
      call MNMXR  (PROD,1,M,ZERO,MNP,MXP)
      ZU = max(PHI(MXV),ELL(MXE),PROD(MXP))
      ZL = min(PHI(MNV),ELL(MNE),PROD(MNP))
      call ABOVEL (ZU,YU)
      call BELOWL (ZL,YL)
      YU = log10(YU)
      YL = log10(YL)
C     !END
      call BYE ('OAT')
C
      return
      end
