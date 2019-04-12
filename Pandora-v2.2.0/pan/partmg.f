      subroutine PARTMG
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Magnesium,
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
      dimension CHI1(1), ELL1(1), GPR1(1), M1(1), ALF1(3), GAM1(3)
      dimension CHI2(1), ELL2(1), GPR2(1), M2(1), ALF2(3), GAM2(3)
      dimension CHI3(2), ELL3(2), GPR3(2), M3(2), ALF3(4), GAM3(4)
      dimension CHI4(2), ELL4(2), GPR4(2), M4(2), ALF4(5), GAM4(5)
      dimension CHI5(3), ELL5(3), GPR5(3), M5(3), ALF5(8), GAM5(8)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /1.D0/
      data      NS1  /1/
      data      CHI1 /7.644D0/
      data      ELL1 /7.D0/
      data      GPR1 /4.D0/
      data      M1   /3/
      data      ALF1 /1.07445D1, 2.915057D2, 5.37488D1/
      data      GAM1 /2.805D0, 6.777D0, 9.254D0/
C----  ION = 2 ----------------------------------------------
      data      G2   /2.D0/
      data      NS2  /1/
      data      CHI2 /1.5031D1/
      data      ELL2 /7.D0/
      data      GPR2 /2.D0/
      data      M2   /3/
      data      ALF2 /6.227D0, 3.11291D1, 1.326438D2/
      data      GAM2 /4.459D0, 9.789D0, 1.3137D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /1.D0/
      data      NS3  /2/
      data      CHI3 /8.0117D1, 8.0393D1/
      data      ELL3 /5.D0, 5.D0/
      data      GPR3 /8.D0, 4.D0/
      data      M3   /2, 2/
      data      ALF3 /4.04379D1, 1.595618D2,
     $                2.03845D1, 7.96154D1/
      data      GAM3 /5.7413D1, 7.1252D1,
     $                5.801D1, 7.166D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /4.D0/
      data      NS4  /2/
      data      CHI4 /1.09294D2, 1.13799D2/
      data      ELL4 /5.D0, 5.D0/
      data      GPR4 /1.8D1, 1.D1/
      data      M4   /3, 2/
      data      ALF4 /2.0007D0, 1.068977D2, 3.43101D2,
     $                1.01326D1, 2.378581D2/
      data      GAM4 /2.76D-1, 7.444D1, 9.4447D1,
     $                5.4472D1, 9.5858D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /5.D0/
      data      NS5  /3/
      data      CHI5 /1.41231D2, 1.47944D2, 1.51493D2/
      data      ELL5 /5.D0, 5.D0, 5.D0/
      data      GPR5 /8.D0, 2.D1, 1.2D1/
      data      M5   /4, 2, 2/
      data      ALF5 /4.1096D0, 6.1538D0, 2.23636D1, 2.753339D2,
     $                1.170375D2, 3.829616D2,
     $                7.1423D1, 2.285765D2/
      data      GAM5 /2.51D-1, 5.37D0, 5.1461D1, 1.21819D2,
     $                1.01026D2, 1.24652D2,
     $                1.04636D2, 1.28118D2/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTMG')
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
      call BYE ('PARTMG')
C
      return
      end
