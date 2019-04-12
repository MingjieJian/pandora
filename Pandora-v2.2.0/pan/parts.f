      subroutine PARTS
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Sulfur,
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
      dimension CHI1(3), ELL1(3), GPR1(3), M1(3), ALF1(8), GAM1(8)
      dimension CHI2(2), ELL2(2), GPR2(2), M2(2), ALF2(6), GAM2(6)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(5), GAM3(5)
      dimension CHI4(2), ELL4(2), GPR4(2), M4(2), ALF4(6), GAM4(6)
      dimension CHI5(2), ELL5(2), GPR5(2), M5(2), ALF5(4), GAM5(4)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /5.D0/
      data      NS1  /3/
      data      CHI1 /1.0357D1, 1.22D1, 1.3401D1/
      data      ELL1 /6.D0, 5.D0, 5.D0/
      data      GPR1 /8.D0, 2.D1, 1.2D1/
      data      M1   /4, 2, 2/
      data      ALF1 /3.9615D0, 5.078D0, 1.50944D1, 3.628588D2,
     $                5.15995D1, 2.684002D2,
     $                1.2D1, 2.76D2/
      data      GAM1 /5.3D-2, 1.121D0, 5.812D0, 9.425D0,
     $                8.936D0, 1.1277D1,
     $                9.6D0, 1.2551D1/
C----  ION = 2 ----------------------------------------------
      data      G2   /4.D0/
      data      NS2  /2/
      data      CHI2 /2.3405D1, 2.4807D1/
      data      ELL2 /5.D0, 5.D0/
      data      GPR2 /1.8D1, 1.D1/
      data      M2   /4, 2/
      data      ALF2 /1.14377D1, 5.5126D0, 1.410009D2, 2.540478D2,
     $                3.30518D1, 1.269479D2/
      data      GAM2 /1.892D0, 3.646D0, 1.355D1, 1.9376D1,
     $                1.6253D1, 2.1062D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /1.D0/
      data      NS3  /1/
      data      CHI3 /3.5047D1/
      data      ELL3 /3.5D0/
      data      GPR3 /1.2D1/
      data      M3   /5/
      data      ALF3 /4.0707D0, 4.0637D0, 5.7245D0, 1.446376D2,
     $                          1.064909D2/
      data      GAM3 /4.3D-2, 1.23D-1, 1.59D0, 1.3712D1,
     $                        2.205D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /2.D0/
      data      NS4  /2/
      data      CHI4 /4.7292D1, 5.7681D1/
      data      ELL4 /5.D0, 1.44D1/
      data      GPR4 /2.D0, 1.8D1/
      data      M4   /4, 2/
      data      ALF4 /4.0011D0, 1.92813D1, 2.7599D1, 3.51179D1,
     $                9.47454D1, 2.832486D2/
      data      GAM4 /1.18D-1, 9.545D0, 1.8179D1, 3.1441D1,
     $                3.0664D1, 5.615D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /1.D0/
      data      NS5  /2/
      data      CHI5 /7.2474D1, 8.5701D1/
      data      ELL5 /5.D0, 4.D0/
      data      GPR5 /4.D0, 1.2D1/
      data      M5   /3, 1/
      data      ALF5 /1.05474D1, 2.87137D1, 6.57378D1,
     $                2.4D1/
      data      GAM5 /1.0704D1, 2.7075D1, 5.0599D1,
     $                4.3034D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTS')
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
      call BYE ('PARTS')
C
      return
      end
