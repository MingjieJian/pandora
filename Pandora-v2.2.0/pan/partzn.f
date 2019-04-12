      subroutine PARTZN
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Zinc,
C     such that Partition Function Q = G0 + SIGMA.
C     (ION=1 for the neutral atom.)
C     Also returns the Excitation Potential X.
C     THETA is the reciprocal temperature, and PE is the
C     electron pressure in dynes/cm**2.
C     Returns G0=-1 if data for "ION" is unavailable.
C     (For reference, see listing of "DEPART".)
C     !DASH
      save
C     !DASH
      real*8 ALF1, ALF2, ALF3, CHI1, CHI2, CHI3, CHI4, ELL1, ELL2, ELL3,
     $       ELL4, ERROR, G0, G1, G2, G3, G4, GAM1, GAM2, GAM3, GPR1,
     $       GPR2, GPR3, GPR4, H, PE, SIGMA, THETA, X, Z, ZERO, dmmy
      integer ION, LION, M1, M2, M3, M4, NS1, NS2, NS3, NS4
C     !DASH
      external HOOP, DEPART, HI, BYE
C
      dimension CHI1(3), ELL1(3), GPR1(3), M1(3), ALF1(6), GAM1(6)
      dimension CHI2(3), ELL2(3), GPR2(3), M2(3), ALF2(8), GAM2(8)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(3), GAM3(3)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1)
C
      data      LION /4/
C----  ION = 1 ----------------------------------------------
      data      G1   /1.D0/
      data      NS1  /3/
      data      CHI1 /9.391D0, 1.7503D1, 1.7166D1/
      data      ELL1 /8.D0, 5.D0, 5.D0/
      data      GPR1 /4.D0, 8.D0, 1.2D1/
      data      M1   /2, 2, 2/
      data      ALF1 /1.5988D1, 4.840042D2,
     $                1.85863D1, 1.234134D2,
     $                3.D0, 1.89D2/
      data      GAM1 /4.546D0, 8.84D0,
     $                1.0247D1, 1.662D1,
     $                1.1175D1, 1.6321D1/
C----  ION = 2 ----------------------------------------------
      data      G2   /2.D0/
      data      NS2  /3/
      data      CHI2 /1.7959D1, 2.7757D1, 2.831D1/
      data      ELL2 /8.D0, 5.D0, 5.D0/
      data      GPR2 /2.D0, 3.D1, 1.D1/
      data      M2   /3, 3, 2/
      data      ALF2 /6.1902D0, 3.89317D1, 2.04878D2,
     $                1.02588D1, 8.93771D1, 3.70364D2,
     $                3.D1, 1.28D2/
      data      GAM2 /6.113D0, 1.2964D1, 1.6444D1,
     $                7.926D0, 1.3633D1, 2.4353D1,
     $                1.6286D1, 2.491D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /1.D0/
      data      NS3  /1/
      data      CHI3 /3.9701D1/
      data      ELL3 /5.D0/
      data      GPR3 /2.D1/
      data      M3   /3/
      data      ALF3 /2.46904D1, 1.067491D2, 4.395586D2/
      data      GAM3 /1.0291D1, 2.0689D1, 3.2077D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /1.D1/
      data      NS4  /1/
      data      CHI4 /6.5074D1/
      data      ELL4 /2.8D0/
      data      GPR4 /4.2D1/
      data      M4   /0/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTZN')
C     !BEG
      if((ION.lt.1).or.(ION.gt.LION)) then
        G0 = ERROR
        SIGMA = ZERO
      else
        Z = ION
        call HOOP     (Z,THETA,PE,H)
        goto (101,102,103,104), ION
C
  101   continue
          call DEPART (G1,NS1,CHI1,ELL1,GPR1,M1,ALF1,GAM1,Z,THETA,H,
     $                 X,G0,SIGMA)
          goto 100
  102   continue
          call DEPART (G2,NS2,CHI2,ELL2,GPR2,M2,ALF2,GAM2,Z,THETA,H,
     $                 X,G0,SIGMA)
          goto 100
  103   continue
          call DEPART (G3,NS3,CHI3,ELL3,GPR3,M3,ALF3,GAM3,Z,THETA,H,
     $                 X,G0,SIGMA)
          goto 100
  104   continue
          call DEPART (G4,NS4,CHI4,ELL4,GPR4,M4,dmmy,dmmy,Z,THETA,H,
     $                 X,G0,SIGMA)
  100   continue
      end if
C     !END
      call BYE ('PARTZN')
C
      return
      end
