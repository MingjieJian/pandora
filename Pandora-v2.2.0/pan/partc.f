      subroutine PARTC
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Carbon,
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
      dimension CHI1(1), ELL1(1), GPR1(1), M1(1), ALF1(4), GAM1(4)
      dimension CHI2(2), ELL2(2), GPR2(2), M2(2), ALF2(6), GAM2(6)
      dimension CHI3(2), ELL3(2), GPR3(2), M3(2), ALF3(6), GAM3(6)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1), ALF4(3), GAM4(3)
      dimension CHI5(1), ELL5(1), GPR5(1), M5(1), ALF5(2), GAM5(2)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /1.D0/
      data      NS1  /1/
      data      CHI1 /1.1256D1/
      data      ELL1 /6.D0/
      data      GPR1 /1.2D1/
      data      M1   /4/
      data      ALF1 /8.0158D0, 5.8833D0, 3.37521D1, 5.953432D2/
      data      GAM1 /4.D-3, 1.359D0, 6.454D0, 1.0376D1/
C----  ION = 2 ----------------------------------------------
      data      G2   /2.D0/
      data      NS2  /2/
      data      CHI2 /2.4376D1, 3.0868D1/
      data      ELL2 /6.D0, 5.D0/
      data      GPR2 /2.D0, 1.8D1/
      data      M2   /3, 3/
      data      ALF2 /4.0003D0, 1.70841D1, 8.29154D1,
     $                1.59808D1, 4.82044D1, 4.358093D2/
      data      GAM2 /8.D-3, 1.6546D1, 2.1614D1,
     $                5.688D0, 1.5801D1, 2.6269D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /1.D0/
      data      NS3  /2/
      data      CHI3 /4.7871D1, 5.5873D1/
      data      ELL3 /6.1D0, 5.D0/
      data      GPR3 /4.D0, 1.2D1/
      data      M3   /3, 3/
      data      ALF3 /1.00281D1, 1.57574D1, 1.862109D2,
     $                1.54127D1, 5.59559D1, 2.436311D2/
      data      GAM3 /6.691D0, 2.5034D1, 4.0975D1,
     $                1.7604D1, 3.618D1, 4.7133D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /2.D0/
      data      NS4  /1/
      data      CHI4 /6.4476D1/
      data      ELL4 /6.D0/
      data      GPR4 /2.D0/
      data      M4   /3/
      data      ALF4 /6.0057D0, 2.35757D1, 7.64185D1/
      data      GAM4 /8.005D0, 4.0804D1, 5.4492D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /1.D0/
      data      NS5  /1/
      data      CHI5 /3.91986D2/
      data      ELL5 /4.D0/
      data      GPR5 /4.D0/
      data      M5   /2/
      data      ALF5 /1.57995D1, 3.62005D1/
      data      GAM5 /3.03772D2, 3.54208D2/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTC')
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
      call BYE ('PARTC')
C
      return
      end
