      subroutine PARTFE
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Iron,
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
      dimension CHI1(3), ELL1(3), GPR1(3), M1(3), ALF1(11), GAM1(11)
      dimension CHI2(2), ELL2(2), GPR2(2), M2(2), ALF2(8), GAM2(8)
      dimension CHI3(2), ELL3(2), GPR3(2), M3(2), ALF3(8), GAM3(8)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1), ALF4(4), GAM4(4)
      dimension CHI5(1), ELL5(1), GPR5(1), M5(1), ALF5(4), GAM5(4)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /9.D0/
      data      NS1  /3/
      data      CHI1 /7.896D0, 8.195D0, 8.927D0/
      data      ELL1 /5.D0, 5.D0, 5.D0/
      data      GPR1 /6.D1, 5.6D1, 4.D1/
      data      M1   /4, 4, 3/
      data      ALF1 /1.44102D1, 2.705D0, 4.216612D2, 9.401484D2,
     $                3.62187D1, 2.28883D1, 2.395997D2, 8.252919D2,
     $                1.100242D2, 9.92304D2, 6.406715D2/
      data      GAM1 /6.6D-2, 3.39D-1, 2.897D0, 6.585D0,
     $                9.23D-1, 1.679D0, 4.62D0, 7.053D0,
     $                4.249D0, 5.875D0, 7.781D0/
C----  ION = 2 ----------------------------------------------
      data      G2   /1.D1/
      data      NS2  /2/
      data      CHI2 /1.6178D1, 1.8662D1/
      data      ELL2 /5.D0, 5.D0/
      data      GPR2 /5.D1, 1.8D1/
      data      M2   /5, 3/
      data      ALF2 /1.70494D1, 3.23783D1, 3.43184D1, 4.209626D2,
     $                           1.0672064D3,
     $                1.540059D2, 4.621117D2, 3.298618D2/
      data      GAM2 /6.2D-2, 2.83D-1, 1.504D0, 5.43D0,
     $                        1.121D1,
     $                2.792D0, 7.627D0, 1.3623D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /9.D0/
      data      NS3  /2/
      data      CHI3 /3.064D1, 3.4607D1/
      data      ELL3 /6.D0, 5.D0/
      data      GPR3 /1.2D1, 7.2D1/
      data      M3   /4, 4/
      data      ALF3 /1.57906D1, 4.71168D1, 2.799292D2, 6.921005D2,
     $                9.10206D1, 2.063082D2, 7.069927D2, 8.366689D2/
      data      GAM3 /7.7D-2, 3.723D0, 1.2137D1, 2.37D1,
     $                2.688D0, 7.595D0, 1.5444D1, 2.5587D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /6.D0/
      data      NS4  /1/
      data      CHI4 /5.6001D1/
      data      ELL4 /3.6D0/
      data      GPR4 /5.D1/
      data      M4   /4/
      data      ALF4 /4.0079D1, 2.76965D1, 2.82243D1, 1.80001D1/
      data      GAM4 /3.982D0, 4.677D0, 6.453D0, 2.3561D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /1.D0/
      data      NS5  /1/
      data      CHI5 /7.9001D1/
      data      ELL5 /3.8D0/
      data      GPR5 /5.6D1/
      data      M5   /4/
      data      ALF5 /2.40899D1, 8.9634D1, 5.15756D1, 2.41698D2/
      data      GAM5 /1.02D-1, 3.354D0, 2.2954D1, 3.3796D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTFE')
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
      call BYE ('PARTFE')
C
      return
      end
