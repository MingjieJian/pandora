      subroutine PARTCA
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Calcium,
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
      real*8 ALF1, ALF2, ALF3, ALF4, CHI1, CHI2, CHI3, CHI4, ELL1, ELL2,
     $       ELL3, ELL4, ERROR, G0, G1, G2, G3, G4, GAM1, GAM2, GAM3,
     $       GAM4, GPR1, GPR2, GPR3, GPR4, H, PE, SIGMA, THETA, X, Z,
     $       ZERO
      integer ION, LION, M1, M2, M3, M4, NS1, NS2, NS3, NS4
C     !DASH
      external HOOP, DEPART, HI, BYE
C
      dimension CHI1(2), ELL1(2), GPR1(2), M1(2), ALF1(5), GAM1(5)
      dimension CHI2(1), ELL2(1), GPR2(1), M2(1), ALF2(3), GAM2(3)
      dimension CHI3(2), ELL3(2), GPR3(2), M3(2), ALF3(4), GAM3(4)
      dimension CHI4(2), ELL4(2), GPR4(2), M4(2), ALF4(5), GAM4(5)
C
      data      LION /4/
C----  ION = 1 ----------------------------------------------
      data      G1   /1.D0/
      data      NS1  /2/
      data      CHI1 /6.111D0, 7.808D0/
      data      ELL1 /5.9D0, 6.D0/
      data      GPR1 /4.D0, 2.D1/
      data      M1   /3, 2/
      data      ALF1 /1.82366D1, 2.75012D1, 1.492617D2,
     $                9.45242D1, 7.054711D2/
      data      GAM1 /2.05D0, 3.349D0, 5.321D0,
     $                4.873D0, 7.017D0/
C----  ION = 2 ----------------------------------------------
      data      G2   /2.D0/
      data      NS2  /1/
      data      CHI2 /1.1868D1/
      data      ELL2 /7.D0/
      data      GPR2 /2.D0/
      data      M2   /3/
      data      ALF2 /1.18706D1, 1.4071D1, 1.060547D2/
      data      GAM2 /1.769D0, 5.109D0, 9.524D0/
C----  ION = 3 ----------------------------------------------
      data      G3   /1.D0/
      data      NS3  /2/
      data      CHI3 /5.1207D1, 5.1596D1/
      data      ELL3 /5.D0, 4.9D0/
      data      GPR3 /8.D0, 4.D0/
      data      M3   /2, 2/
      data      ALF3 /5.72414D1, 1.107567D2,
     $                2.98121D1, 5.41874D1/
      data      GAM3 /2.7271D1, 4.1561D1,
     $                2.9172D1, 4.214D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /4.D0/
      data      NS4  /2/
      data      CHI4 /6.7181D1, 6.9536D1/
      data      ELL4 /5.D0, 4.3D0/
      data      GPR4 /1.8D1, 1.D1/
      data      M4   /3, 2/
      data      ALF4 /2.0184D0, 9.75784D1, 2.823939D2,
     $                2.091871D2, 2.528129D2/
      data      GAM4 /3.94D-1, 2.893D1, 5.2618D1,
     $                3.8593D1, 4.9646D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTCA')
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
          call DEPART (G4,NS4,CHI4,ELL4,GPR4,M4,ALF4,GAM4,Z,THETA,H,
     $                 X,G0,SIGMA)
  100   continue
      end if
C     !END
      call BYE ('PARTCA')
C
      return
      end
