      subroutine PARTCL
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Chlorine,
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
      dimension CHI1(2), ELL1(2), GPR1(2), M1(2), ALF1(5), GAM1(5)
      dimension CHI2(3), ELL2(3), GPR2(3), M2(3), ALF2(9), GAM2(9)
      dimension CHI3(2), ELL3(2), GPR3(2), M3(2), ALF3(6), GAM3(6)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1), ALF4(4), GAM4(4)
      dimension CHI5(1), ELL5(1), GPR5(1), M5(1), ALF5(4), GAM5(4)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /4.D0/
      data      NS1  /2/
      data      CHI1 /1.3014D1, 1.4458D1/
      data      ELL1 /6.D0, 5.D0/
      data      GPR1 /1.8D1, 1.D1/
      data      M1   /3, 2/
      data      ALF1 /2.0007D0, 6.25048D1, 6.694942D2,
     $                2.90259D1, 1.30974D2/
      data      GAM1 /1.1D-1, 9.919D0, 1.228D1,
     $                1.1017D1, 1.3532D1/
C----  ION = 2 ----------------------------------------------
      data      G2   /5.D0/
      data      NS2  /3/
      data      CHI2 /2.3798D1, 2.6041D1, 2.7501D1/
      data      ELL2 /5.D0, 5.D0, 5.D0/
      data      GPR2 /8.D0, 2.D1, 1.2D1/
      data      M2   /5, 2, 2/
      data      ALF2 /3.9064D0, 3.993D-1, 5.357D0, 6.03424D1,
     $                          1.199913D2,
     $                1.381567D2, 2.788418D2,
     $                1.023681D2, 1.586311D2/
      data      GAM2 /9.2D-2, 5.81D-1, 1.62D0, 1.3121D1,
     $                        1.9787D1,
     $                1.6365D1, 2.1988D1,
     $                1.8065D1, 2.3594D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /4.D0/
      data      NS3  /2/
      data      CHI3 /3.9904D1, 4.161D1/
      data      ELL3 /5.D0, 5.D0/
      data      GPR3 /1.8D1, 1.D1/
      data      M3   /4, 2/
      data      ALF3 /1.26089D1, 5.9527D0, 1.105635D2, 2.628715D2,
     $                6.92035D1, 1.00796D2/
      data      GAM3 /2.358D0, 5.708D0, 1.9084D1, 3.0683D1,
     $                2.488D1, 3.32229D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /1.D0/
      data      NS4  /1/
      data      CHI4 /5.345D1/
      data      ELL4 /5.D0/
      data      GPR4 /1.2D1/
      data      M4   /4/
      data      ALF4 /7.3458D0, 5.6638D0, 4.41256D1, 2.027846D2/
      data      GAM4 /1.02D-1, 1.391D0, 1.4709D1, 3.6968D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /2.D0/
      data      NS5  /1/
      data      CHI5 /6.7801D1/
      data      ELL5 /5.2D0/
      data      GPR5 /2.D0/
      data      M5   /4/
      data      ALF5 /4.0037D0, 2.18663D1, 4.05363D1, 5.75919D1/
      data      GAM5 /1.85D-1, 1.1783D1, 2.5653D1, 4.4698D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTCL')
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
      call BYE ('PARTCL')
C
      return
      end
