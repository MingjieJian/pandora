      subroutine PARTAR
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Argon,
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
      real*8 ALF1, ALF2, ALF3, ALF4, ALF5, CHI1, CHI2, CHI3, CHI4, CHI5,
     $       ELL1, ELL2, ELL3, ELL4, ELL5, ERROR, G0, G1, G2, G3, G4,
     $       G5, GAM1, GAM2, GAM3, GAM4, GAM5, GPR1, GPR2, GPR3, GPR4,
     $       GPR5, H, PE, SIGMA, THETA, X, Z, ZERO
      integer ION, LION, M1, M2, M3, M4, M5, NS1, NS2, NS3, NS4, NS5
C     !DASH
      external HOOP, DEPART, HI, BYE
C
      dimension CHI1(2), ELL1(2), GPR1(2), M1(2), ALF1(4), GAM1(4)
      dimension CHI2(2), ELL2(2), GPR2(2), M2(2), ALF2(5), GAM2(5)
      dimension CHI3(3), ELL3(3), GPR3(3), M3(3), ALF3(8), GAM3(8)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1), ALF4(4), GAM4(4)
      dimension CHI5(1), ELL5(1), GPR5(1), M5(1), ALF5(4), GAM5(4)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /1.D0/
      data      NS1  /2/
      data      CHI1 /1.5755D1, 1.5933D1/
      data      ELL1 /6.D0, 6.D0/
      data      GPR1 /8.D0, 4.D0/
      data      M1   /2, 2/
      data      ALF1 /4.36623D1, 3.243375D2,
     $                2.08298D1, 1.631701D2/
      data      GAM1 /1.2638D1, 1.4958D1,
     $                1.2833D1, 1.5139D1/
C----  ION = 2 ----------------------------------------------
      data      G2   /4.D0/
      data      NS2  /2/
      data      CHI2 /2.7619D1, 2.9355D1/
      data      ELL2 /5.1D0, 5.D0/
      data      GPR2 /1.8D1, 1.D1/
      data      M2   /3, 2/
      data      ALF2 /2.0026D0, 1.374515D2, 2.585445D2,
     $                6.28129D1, 1.491867D2/
      data      GAM2 /1.78D-1, 1.7522D1, 2.3584D1,
     $                2.0464D1, 2.515D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /5.D0/
      data      NS3  /3/
      data      CHI3 /4.0899D1, 4.2407D1, 4.5234D1/
      data      ELL3 /5.D0, 5.D0, 5.D0/
      data      GPR3 /8.D0, 2.D1, 1.2D1/
      data      M3   /4, 2, 2/
      data      ALF3 /4.0495D0, 1.44466D1, 4.68234D1, 1.246651D2,
     $                1.519828D2, 2.680157D2,
     $                1.011302D2, 1.508691D2/
      data      GAM3 /1.51D-1, 1.561D0, 1.7399D1, 3.0871D1,
     $                2.4684D1, 3.3978D1,
     $                2.7091D1, 3.6481D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /4.D0/
      data      NS4  /1/
      data      CHI4 /5.9793D1/
      data      ELL4 /5.D0/
      data      GPR4 /1.8D1/
      data      M4   /4/
      data      ALF4 /1.33718D1, 8.6528D0, 6.04614D1, 2.855072D2/
      data      GAM4 /2.81D0, 8.877D0, 2.4351D1, 4.4489D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /1.D0/
      data      NS5  /1/
      data      CHI5 /7.5002D1/
      data      ELL5 /4.D0/
      data      GPR5 /1.2D1/
      data      M5   /4/
      data      ALF5 /6.7655D0, 4.7684D0, 1.28631D1, 5.4526D1/
      data      GAM5 /1.44D-1, 1.16D0, 1.021D1, 2.7178D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTAR')
C     !BEG
      if((ION.lt.1).or.(ION.gt.LION)) then
        G0 = ERROR
        SIGMA = ZERO
      else
        Z = ION
        call HOOP     (Z,THETA,PE,H)
        goto (101,102,103,104,105), ION
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
          goto 100
  105   continue
          call DEPART (G5,NS5,CHI5,ELL5,GPR5,M5,ALF5,GAM5,Z,THETA,H,
     $                 X,G0,SIGMA)
  100   continue
      end if
C     !END
      call BYE ('PARTAR')
C
      return
      end
