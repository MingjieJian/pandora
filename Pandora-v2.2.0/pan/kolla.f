      subroutine KOLLA
     $(L,G0,G1,G2)
C
C     Rudolf Loeser, 1990 Oct 09
C---- Computes g0, g1, and g2 for Hydrogen.
C
C     Johnson, L.C. 1972, ApJ, 174, 227-236.
C
C     !DASH
      save
C     !DASH
      real*8 C01, C02, C03, C11, C12, C13, C21, C22, C23, E2, EL, G0,
     $       G01, G02, G1, G11, G12, G2, G21, G22, ONE
      integer L
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
      data G01,G02     / 1.133D0,   1.0785D0/
      data G11,G12     /-4.059D-1, -2.319D-1/
      data G21,G22     / 7.014D-2,  2.947D-2/
      data C01,C02,C03 / 9.935D-1,  2.328D-1, -1.296D-1/
      data C11,C12,C13 / 6.282D-1, -5.598D-1,  5.299D-1/
      data C21,C22,C23 / 3.887D-1, -1.181D0,   1.47D0/
C
      call HI ('KOLLA')
C     !BEG
      if(L.le.1) then
        G0 = G01
        G1 = G11
        G2 = G21
C
      else if(L.eq.2) then
        G0 = G02
        G1 = G12
        G2 = G22
      else
C
        EL =  L
        E2 =  L**2
        G0 =  (C01+C02/EL+C03/E2)
        G1 = -(C11+C12/EL+C13/E2)/EL
        G2 =  (C21+C22/EL+C23/E2)/E2
      end if
C     !END
      call BYE ('KOLLA')
C
      return
      end
