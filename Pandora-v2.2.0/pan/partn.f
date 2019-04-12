      subroutine PARTN
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Nitrogen,
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
      dimension CHI1(2), ELL1(2), GPR1(2), M1(2), ALF1(6), GAM1(6)
      dimension CHI2(2), ELL2(2), GPR2(2), M2(2), ALF2(7), GAM2(7)
      dimension CHI3(3), ELL3(3), GPR3(3), M3(3), ALF3(9), GAM3(9)
      dimension CHI4(2), ELL4(2), GPR4(2), M4(2), ALF4(5), GAM4(5)
      dimension CHI5(1), ELL5(1), GPR5(1), M5(1), ALF5(3), GAM5(3)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /4.D0/
      data      NS1  /2/
      data      CHI1 /1.4529D1, 1.6428D1/
      data      ELL1 /6.1D0, 4.D0/
      data      GPR1 /1.8D1, 1.D1/
      data      M1   /3, 3/
      data      ALF1 /1.40499D1, 3.08008D1, 8.831443D2,
     $                1.D1, 1.6D1, 6.4D1/
      data      GAM1 /2.554D0, 9.169D0, 1.3651D1,
     $                1.2353D1, 1.3784D1, 1.4874D1/
C----  ION = 2 ----------------------------------------------
      data      G2   /1.D0/
      data      NS2  /2/
      data      CHI2 /2.9593D1, 3.6693D1/
      data      ELL2 /5.D0, 3.9D0/
      data      GPR2 /1.2D1, 2.4D1/
      data      M2   /4, 3/
      data      ALF2 /8.0462D0, 6.2669D0, 1.78696D1, 2.828084D2,
     $                7.3751D0, 3.3139D1, 2.154829D2/
      data      GAM2 /1.4D-2, 2.131D0, 1.5745D1, 2.4949D1,
     $                6.376D0, 1.4246D1, 2.9465D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /2.D0/
      data      NS3  /3/
      data      CHI3 /4.7426D1, 5.5765D1, 6.3626D1/
      data      ELL3 /6.D0, 5.D0, 4.D0/
      data      GPR3 /2.D0, 1.8D1, 6.D0/
      data      M3   /3, 4, 2/
      data      ALF3 /4.0003D0, 1.93533D1, 8.06462D1,
     $                1.30998D1, 1.96425D1, 9.43035D1, 3.709539D2,
     $                1.6D1, 3.8D1/
      data      GAM3 /2.2D-2, 3.1259D1, 4.1428D1,
     $                7.212D0, 1.5228D1, 3.4387D1, 4.6708D1,
     $                4.6475D1, 4.9468D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /1.D0/
      data      NS4  /2/
      data      CHI4 /7.745D1, 8.7445D1/
      data      ELL4 /6.D0, 6.3D0/
      data      GPR4 /4.D0, 1.2D1/
      data      M4   /3, 2/
      data      ALF4 /1.03289D1, 1.45021D1, 1.871624D2,
     $                1.081615D2, 1.918383D2/
      data      GAM4 /8.693D0, 3.765D1, 6.5479D1,
     $                6.1155D1, 7.9196D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /2.D0/
      data      NS5  /1/
      data      CHI5 /9.7863D1/
      data      ELL5 /6.D0/
      data      GPR5 /2.D0/
      data      M5   /3/
      data      ALF5 /6.0044D0, 2.35612D1, 7.64344D1/
      data      GAM5 /9.999D0, 6.0991D1, 8.2262D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTN')
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
      call BYE ('PARTN')
C
      return
      end
