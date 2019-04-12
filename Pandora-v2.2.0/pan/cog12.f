      subroutine COG12
     $(K,KD,M,F)
C
C     Rudolf Loeser, 1992 Sep 11
C---- Computes the function F, for the Carbon-12 isotope, for COG.
C     !DASH
      save
C     !DASH
      real*8 CG, F, ONE, PK, PM, XK, XM, ZERO
      integer I, J, K, KD, M
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  HI, BYE
C
      dimension CG(7,4,2), PK(4)
C
      data CG / 1.0000D+00,  1.9027D-04,  7.0112D-06, -2.1784D-08,
     $         -8.2485D-12, -2.0811D-13, -8.3958D-17,
     $          0.0000D+00, -5.0303D-05, -2.0387D-07, -7.3577D-10,
     $         -1.9716D-12,  1.5967D-15, -1.1501D-17,
     $          0.0000D+00, -3.2773D-07, -1.1501D-09, -1.6314D-12,
     $          7.6882D-16, -9.0972D-16, -1.8261D-18,
     $          0.0000D+00, -6.6337D-09, -1.3006D-10, -4.1434D-14,
     $         -5.3066D-15,  3.5135D-17, -4.4716D-20,
C
     $          1.0000D+00,  5.1206D-03,  3.4079D-05,  7.8730D-08,
     $          3.3385D-10,  4.1807D-13,  3.0415D-15,
     $         -3.8840D-10, -9.6144D-05, -5.7521D-07, -2.9083D-09,
     $         -9.5081D-12, -1.0206D-13,  7.3446D-18,
     $         -4.4289D-08,  7.4737D-07,  4.8188D-09, -1.9733D-11,
     $         -3.8067D-13,  1.4492D-14, -5.2711D-18,
     $          2.7195D-09, -2.2463D-08,  1.0095D-10,  2.0817D-12,
     $          5.2529D-14, -8.0869D-16,  6.2062D-19/
C     !EJECT
C
      call HI ('COG12')
C     !BEG
      XK    = K
      PK(1) = ONE
      PK(2) = XK
      PK(3) = XK*PK(2)
      PK(4) = XK*PK(3)
C
      F  = ZERO
      XM = M
      PM = ONE
C
      do 101 I = 1,7
C
        do 100 J  =1,4
          F = F+CG(I,J,KD)*PM*PK(J)
  100   continue
C
        PM = PM*XM
  101 continue
C     !END
      call BYE ('COG12')
C
      return
      end
