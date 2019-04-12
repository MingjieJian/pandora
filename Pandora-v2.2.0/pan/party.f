      subroutine PARTY
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Yttrium,
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
      real*8 ALF1, ALF2, ALF3, ALF5, CHI1, CHI2, CHI3, CHI4, CHI5, ELL1,
     $       ELL2, ELL3, ELL4, ELL5, ERROR, G0, G1, G2, G3, G4, G5,
     $       GAM1, GAM2, GAM3, GAM5, GPR1, GPR2, GPR3, GPR4, GPR5, H,
     $       PE, SIGMA, THETA, X, Z, ZERO, dmmy
      integer ION, LION, M1, M2, M3, M4, M5, NS1, NS2, NS3, NS4, NS5
C     !DASH
      external  HOOP, DEPART, HI, BYE
C
      dimension CHI1(1), ELL1(1), GPR1(1), M1(1), ALF1(4), GAM1(4)
      dimension CHI2(2), ELL2(2), GPR2(2), M2(2), ALF2(8), GAM2(8)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(4), GAM3(4)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1)
      dimension CHI5(1), ELL5(1), GPR5(1), M5(1), ALF5(3), GAM5(3)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /4.D0/
      data      NS1  /1/
      data      CHI1 /6.377D0/
      data      ELL1 /6.D0/
      data      GPR1 /2.D0/
      data      M1   /4/
      data      ALF1 /6.031D0, 1.020874D2, 2.306864D2, 3.491883D2/
      data      GAM1 /6.6D-2, 1.62D0, 3.139D0, 5.015D0/
C----  ION = 2 ----------------------------------------------
      data      G2   /1.D0/
      data      NS2  /2/
      data      CHI2 /1.2233D1, 1.3158D1/
      data      ELL2 /6.D0, 6.D0/
      data      GPR2 /2.D1, 4.D0/
      data      M2   /5, 3/
      data      ALF2 /1.33733D1, 1.02739D1, 4.35888D1, 8.80471D1,
     $                           3.977009D2,
     $                9.2791D0, 2.5574D1, 7.01467D1/
      data      GAM2 /1.33D-1, 4.47D-1, 1.509D0, 4.379D0,
     $                         9.715D0,
     $                2.999D0, 7.394D0, 1.0873D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /4.D0/
      data      NS3  /1/
      data      CHI3 /2.0514D1/
      data      ELL3 /4.9D0/
      data      GPR3 /2.D0/
      data      M3   /4/
      data      ALF3 /6.0039D0, 2.0213D0, 6.6309D0, 2.53433D1/
      data      GAM3 /9.D-2, 9.36D-1, 5.454D0, 1.1911D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /1.D0/
      data      NS4  /1/
      data      CHI4 /7.6105D1/
      data      ELL4 /2.7D0/
      data      GPR4 /1.2D1/
      data      M4   /0/
C----  ION = 5 ----------------------------------------------
      data      G5   /4.D0/
      data      NS5  /1/
      data      CHI5 /7.6849D1/
      data      ELL5 /4.D0/
      data      GPR5 /1.8D1/
      data      M5   /3/
      data      ALF5 /2.0055D0, 3.5232D1, 9.67617D1/
      data      GAM5 /1.5D0, 2.592D1, 3.6347D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTY')
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
          call DEPART (G4,NS4,CHI4,ELL4,GPR4,M4,dmmy,dmmy,Z,THETA,H,
     $                 X,G0,SIGMA)
          goto 100
  105   continue
          call DEPART (G5,NS5,CHI5,ELL5,GPR5,M5,ALF5,GAM5,Z,THETA,H,
     $                 X,G0,SIGMA)
  100   continue
      end if
C     !END
      call BYE ('PARTY')
C
      return
      end
