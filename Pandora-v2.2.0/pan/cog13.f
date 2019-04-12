      subroutine COG13
     $(K,KD,M,F)
C
C     Rudolf Loeser, 1992 Nov 02
C---- Computes the function F, for the Carbon-13 isotope, for COG.
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
      external HI, BYE
C
      dimension CG(7,4,2), PK(4)
C
      data CG / 1.0000D+00,  1.8715D-04,  6.7064D-06, -2.0339D-08,
     $         -7.3902D-12, -1.8781D-13, -7.7479D-17,
     $         -4.9068D-07, -4.8069D-05, -1.8880D-07, -6.7442D-10,
     $         -2.3291D-12,  2.4804D-15,  5.5058D-17,
     $          1.5268D-07, -3.0956D-07, -1.6120D-09,  2.5921D-12,
     $          1.8270D-13, -2.1397D-15, -2.2671D-17,
     $         -1.1655D-08, -5.6640D-09, -7.1160D-11, -4.8054D-13,
     $         -1.9002D-14,  1.7465D-16,  1.6896D-18,
C
     $          1.0000D+00,  5.0098D-03,  3.2593D-05,  7.3604D-08,
     $          3.1228D-10,  4.0897D-13,  1.7903D-15,
     $          1.2090D-06, -9.1628D-05, -5.4225D-07, -3.0024D-09,
     $         -1.3573D-11,  5.1298D-15,  4.3201D-16,
     $         -3.9219D-07,  6.2305D-07,  4.4973D-09,  7.0560D-11,
     $          1.3694D-12, -1.1954D-14, -1.5877D-16,
     $          2.9332D-08, -1.5719D-08,  1.1486D-10, -3.6948D-12,
     $         -9.1088D-14,  8.3345D-16,  1.3086D-17/
C     !EJECT
C
      call HI ('COG13')
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
      do 100 I = 1,7
C
        do 100 J = 1,4
          F = F+CG(I,J,KD)*PM*PK(J)
  100   continue
C
        PM = PM*XM
  101 continue
C     !END
      call BYE ('COG13')
C
      return
      end
