      subroutine PARTV
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Vanadium,
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
      dimension CHI1(2), ELL1(2), GPR1(2), M1(2), ALF1(8), GAM1(8)
      dimension CHI2(3), ELL2(3), GPR2(3), M2(3), ALF2(13), GAM2(13)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(4), GAM3(4)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1), ALF4(4), GAM4(4)
      dimension CHI5(1), ELL5(1), GPR5(1), M5(1), ALF5(4), GAM5(4)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /4.D0/
      data      NS1  /2/
      data      CHI1 /6.738D0, 7.101D0/
      data      ELL1 /5.D0, 5.D0/
      data      GPR1 /5.D1, 7.D1/
      data      M1   /5, 3/
      data      ALF1 /1.52627D1, 2.39869D1, 5.13053D1, 5.703384D2,
     $                           1.6509417D3,
     $                1.622829D2, 2.988303D2, 9.088852D2/
      data      GAM1 /2.6D-2, 1.45D-1, 7.18D-1, 2.586D0,
     $                        5.458D0,
     $                2.171D0, 4.153D0, 6.097D0/
C----  ION = 2 ----------------------------------------------
      data      G2   /1.D0/
      data      NS2  /3/
      data      CHI2 /1.4205D1, 1.567D1, 1.6277D1/
      data      ELL2 /5.D0, 5.D0, 5.D0/
      data      GPR2 /5.6D1, 7.2D1, 6.4D1/
      data      M2   /5, 4, 4/
      data      ALF2 /2.36736D1, 3.71624D1, 8.68011D1, 3.00744D2,
     $                           8.64588D2,
     $                5.78961D1, 7.94605D1, 2.149007D2, 8.647425D2,
     $                6.18508D1, 6.40845D1, 1.928298D2, 7.182349D2/
      data      GAM2 /9.D-3, 3.66D-1, 1.504D0, 5.924D0,
     $                       1.0126D1,
     $                1.796D0, 2.353D0, 6.068D0, 1.2269D1,
     $                2.56D0, 3.674D0, 6.593D0, 1.288D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /4.D0/
      data      NS3  /1/
      data      CHI3 /2.9748D1/
      data      ELL3 /5.D0/
      data      GPR3 /4.2D1/
      data      M3   /4/
      data      ALF3 /2.38116D1, 6.82495D1, 1.350613D2, 5.367632D2/
      data      GAM3 /4.5D-2, 1.684D0, 8.162D0, 2.1262D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /5.D0/
      data      NS4  /1/
      data      CHI4 /4.8464D1/
      data      ELL4 /5.D0/
      data      GPR4 /2.D1/
      data      M4   /4/
      data      ALF4 /1.59543D1, 2.25542D1, 7.14921D1, 2.489544D2/
      data      GAM4 /6.5D-2, 1.746D0, 1.5158D1, 3.3141D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /4.D0/
      data      NS5  /1/
      data      CHI5 /6.5198D1/
      data      ELL5 /1.12D1/
      data      GPR5 /2.D0/
      data      M5   /4/
      data      ALF5 /6.0006D0, 5.8785D0, 5.05077D1, 9.76129D1/
      data      GAM5 /7.7D-2, 2.1229D1, 4.4134D1, 6.0203D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTV')
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
      call BYE ('PARTV')
C
      return
      end
