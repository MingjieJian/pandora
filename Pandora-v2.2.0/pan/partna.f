      subroutine PARTNA
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Sodium,
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
      dimension CHI1(1), ELL1(1), GPR1(1), M1(1), ALF1(2), GAM1(2)
      dimension CHI2(2), ELL2(2), GPR2(2), M2(2), ALF2(4), GAM2(4)
      dimension CHI3(2), ELL3(2), GPR3(2), M3(2), ALF3(5), GAM3(5)
      dimension CHI4(3), ELL4(3), GPR4(3), M4(3), ALF4(8), GAM4(8)
      dimension CHI5(5), ELL5(5), GPR5(5), M5(5), ALF5(13), GAM5(13)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /2.D0/
      data      NS1  /1/
      data      CHI1 /5.138D0/
      data      ELL1 /7.D0/
      data      GPR1 /2.D0/
      data      M1   /2/
      data      ALF1 /1.16348D1, 1.583593D2/
      data      GAM1 /2.4D0, 4.552D0/
C----  ION = 2 ----------------------------------------------
      data      G2   /1.D0/
      data      NS2  /2/
      data      CHI2 /4.729D1, 4.7459D1/
      data      ELL2 /4.D0, 4.D0/
      data      GPR2 /8.D0, 4.D0/
      data      M2   /2, 2/
      data      ALF2 /2.10453D1, 5.09546D1,
     $                1.01389D1, 2.58611D1/
      data      GAM2 /3.4367D1, 4.0566D1,
     $                3.4676D1, 4.0764D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /4.D0/
      data      NS3  /2/
      data      CHI3 /7.1647D1, 7.5504D1/
      data      ELL3 /4.D0, 4.D0/
      data      GPR3 /1.8D1, 1.D1/
      data      M3   /3, 2/
      data      ALF3 /2.0019D0, 3.80569D1, 1.379398D2,
     $                2.83106D1, 6.16893D1/
      data      GAM3 /1.7D-1, 4.4554D1, 5.7142D1,
     $                5.1689D1, 6.0576D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /5.D0/
      data      NS4  /3/
      data      CHI4 /9.888D1, 1.04778D2, 1.07864D2/
      data      ELL4 /5.D0, 5.D0, 5.D0/
      data      GPR4 /8.D0, 2.D1, 1.2D1/
      data      M4   /4, 2, 2/
      data      ALF4 /4.0334D0, 5.856D0, 1.81786D1, 2.089142D2,
     $                9.36895D1, 4.063095D2,
     $                6.04276D1, 2.395719D2/
      data      GAM4 /1.52D-1, 4.26D0, 3.6635D1, 8.3254D1,
     $                7.2561D1, 8.9475D1,
     $                7.5839D1, 9.2582D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /4.D0/
      data      NS5  /5/
      data      CHI5 /1.38597D2, 1.4298D2, 1.47803D2, 1.51427D2,
     $                1.63906D2/
      data      ELL5 /5.D0, 5.D0, 4.D0, 4.D0, 4.D0/
      data      GPR5 /1.8D1, 1.D1, 2.D0, 1.D1, 3.D1/
      data      M5   /4, 2, 2, 3, 2/
      data      ALF5 /1.03174D1, 5.6967D0, 1.210726D2, 3.409131D2,
     $                6.37918D1, 1.862078D2,
     $                2.D0, 1.6D1,
     $                1.79266D1, 1.87036D1, 9.13687D1,
     $                3.68524D1, 2.331475D2/
      data      GAM5 /5.93D0, 9.127D0, 9.4219D1, 1.1569D2,
     $                9.7783D1, 1.211D1,
     $                9.2769D1, 1.084D2,
     $                2.8372D1, 4.9002D1, 1.11473D2,
     $                1.10226D2, 1.2569D2/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTNA')
C     !BEG
      if((ION.lt.1).or.(ION.gt.LION)) then
        G0 = ERROR
        SIGMA = ZERO
      else
        Z = ION
        call HOOP     (Z,THETA,PE,H)
        goto (101,102,103,104,105), ION
  101     continue
          call DEPART (G1,NS1,CHI1,ELL1,GPR1,M1,ALF1,GAM1,Z,THETA,H,
     $                 X,G0,SIGMA)
          goto 100
  102     continue
          call DEPART (G2,NS2,CHI2,ELL2,GPR2,M2,ALF2,GAM2,Z,THETA,H,
     $                 X,G0,SIGMA)
          goto 100
  103     continue
          call DEPART (G3,NS3,CHI3,ELL3,GPR3,M3,ALF3,GAM3,Z,THETA,H,
     $                 X,G0,SIGMA)
          goto 100
  104     continue
          call DEPART (G4,NS4,CHI4,ELL4,GPR4,M4,ALF4,GAM4,Z,THETA,H,
     $                 X,G0,SIGMA)
          goto 100
  105     continue
          call DEPART (G5,NS5,CHI5,ELL5,GPR5,M5,ALF5,GAM5,Z,THETA,H,
     $                 X,G0,SIGMA)
  100   continue
      end if
C     !END
      call BYE ('PARTNA')
C
      return
      end
