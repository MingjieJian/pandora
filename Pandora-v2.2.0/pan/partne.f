      subroutine PARTNE
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Neon,
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
      external  HOOP, DEPART, HI, BYE
C
      dimension CHI1(2), ELL1(2), GPR1(2), M1(2), ALF1(4), GAM1(4)
      dimension CHI2(2), ELL2(2), GPR2(2), M2(2), ALF2(5), GAM2(5)
      dimension CHI3(3), ELL3(3), GPR3(3), M3(3), ALF3(8), GAM3(8)
      dimension CHI4(2), ELL4(2), GPR4(2), M4(2), ALF4(6), GAM4(6)
      dimension CHI5(1), ELL5(1), GPR5(1), M5(1), ALF5(4), GAM5(4)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /1.D0/
      data      NS1  /2/
      data      CHI1 /2.1559D1, 2.1656D1/
      data      ELL1 /6.D0, 6.D0/
      data      GPR1 /8.D0, 4.D0/
      data      M1   /2, 2/
      data      ALF1 /3.4508D1, 3.654919D2,
     $                1.65768D1, 1.834231D2/
      data      GAM1 /1.7796D1, 2.073D1,
     $                1.7879D1, 2.0855D1/
C----  ION = 2 ----------------------------------------------
      data      G2   /4.D0/
      data      NS2  /2/
      data      CHI2 /4.1071D1, 4.4274D1/
      data      ELL2 /5.D0, 4.D0/
      data      GPR2 /1.8D1, 1.D1/
      data      M2   /3, 2/
      data      ALF2 /2.0007D0, 8.95607D1, 3.804381D2,
     $                2.64473D1, 6.35527D1/
      data      GAM2 /9.7D-2, 2.9878D1, 3.7221D1,
     $                3.1913D1, 3.7551D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /5.D0/
      data      NS3  /3/
      data      CHI3 /6.3729D1, 6.8806D1, 7.1434D1/
      data      ELL3 /3.9D0, 4.D0, 4.D0/
      data      GPR3 /8.D0, 2.D1, 1.2D1/
      data      M3   /4, 2, 2/
      data      ALF3 /4.0342D0, 5.6162D0, 1.15176D1, 7.28273D1,
     $                4.85684D1, 1.314315D2,
     $                3.1171D1, 7.6829D1/
      data      GAM3 /9.2D-2, 3.424D0, 2.4806D1, 4.6616D1,
     $                4.5643D1, 5.4147D1,
     $                4.8359D1, 5.742D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /4.D0/
      data      NS4  /2/
      data      CHI4 /9.7162D1, 1.00917D2/
      data      ELL4 /5.D0, 5.D0/
      data      GPR4 /1.8D1, 1.D1/
      data      M4   /4, 2/
      data      ALF4 /1.40482D1, 1.33077D1, 5.27897D1, 4.678487D2,
     $                5.42196D1, 1.9578D2/
      data      GAM4 /5.453D0, 1.856D1, 4.6583D1, 8.0101D1,
     $                7.0337D1, 8.5789D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /1.D0/
      data      NS5  /1/
      data      CHI5 /1.26423D2/
      data      ELL5 /4.D0/
      data      GPR5 /1.2D1/
      data      M5   /4/
      data      ALF5 /8.3813D0, 7.9725D0, 3.93316D1, 1.542828D2/
      data      GAM5 /1.35D-1, 5.497D0, 2.6121D1, 8.6665D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTNE')
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
      call BYE ('PARTNE')
C
      return
      end
