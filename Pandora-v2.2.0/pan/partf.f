      subroutine PARTF
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Fluorine,
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
      dimension CHI1(2), ELL1(2), GPR1(2), M1(2), ALF1(6), GAM1(6)
      dimension CHI2(3), ELL2(3), GPR2(3), M2(3), ALF2(8), GAM2(8)
      dimension CHI3(4), ELL3(4), GPR3(4), M3(4), ALF3(11), GAM3(11)
      dimension CHI4(3), ELL4(3), GPR4(3), M4(3), ALF4(9), GAM4(9)
      dimension CHI5(5), ELL5(5), GPR5(5), M5(5), ALF5(12), GAM5(12)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /4.D0/
      data      NS1  /2/
      data      CHI1 /1.7418D1, 2.0009D1/
      data      ELL1 /4.D0, 4.D0/
      data      GPR1 /1.8D1, 1.D1/
      data      M1   /3, 3/
      data      ALF1 /2.0001D0, 3.99012D1, 1.220986D2,
     $                1.D1, 3.D1, 5.D1/
      data      GAM1 /5.D-2, 1.3317D1, 1.5692D1,
     $                1.5361D1, 1.7128D1, 1.8498D1/
C----  ION = 2 ----------------------------------------------
      data      G2   /5.D0/
      data      NS2  /3/
      data      CHI2 /3.4977D1, 3.9204D1, 4.1368D1/
      data      ELL2 /5.D0, 4.D0, 4.D0/
      data      GPR2 /8.D0, 2.D1, 1.2D1/
      data      M2   /4, 2, 2/
      data      ALF2 /4.0199D0, 5.5741D0, 2.21839D1, 1.902179D2,
     $                5.30383D1, 1.269616D2,
     $                3.16894D1, 7.53105D1/
      data      GAM2 /4.8D-2, 2.735D0, 2.0079D1, 3.0277D1,
     $                2.7548D1, 3.2532D1,
     $                3.0391D1, 3.4707D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /4.D0/
      data      NS3  /4/
      data      CHI3 /6.2646D1, 6.5774D1, 6.9282D1, 7.1882D1/
      data      ELL3 /5.D0, 4.D0, 3.9D0, 4.D0/
      data      GPR3 /1.8D1, 1.D1, 2.D0, 1.D1/
      data      M3   /4, 2, 3, 2/
      data      ALF3 /1.35014D1, 7.9936D0, 5.57981D1, 2.987039D2,
     $                2.62496D1, 6.37503D1,
     $                2.D0, 6.D0, 1.D1,
     $                2.8715D1, 7.1285D1/
      data      GAM3 /4.479D0, 1.2072D1, 3.1662D1, 5.1432D1,
     $                4.4283D1, 5.0964D1,
     $                4.6193D1, 5.0436D1, 5.488D1,
     $                5.0816D1, 5.7479D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /1.D0/
      data      NS4  /3/
      data      CHI4 /8.7139D1, 9.7852D1, 1.06089D2/
      data      ELL4 /5.D0, 5.D0, 4.D0/
      data      GPR4 /1.2D1, 2.4D1, 2.D1/
      data      M4   /5, 2, 2/
      data      ALF4 /8.0153D0, 6.1931D0, 2.17287D1, 4.8778D1,
     $                          2.782782D2,
     $                1.78556D2, 4.214435D2,
     $                5.17632D1, 9.52368D1/
      data      GAM4 /5.8D-2, 3.434D0, 1.4892D1, 3.7472D1,
     $                        6.9883D1,
     $                6.781D1, 8.3105D1,
     $                7.2435D1, 7.9747D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /2.D0/
      data      NS5  /5/
      data      CHI5 /1.14214D2, 1.26256D2, 1.37373D2, 1.45435D2,
     $                           1.4825D2/
      data      ELL5 /5.D0, 5.D0, 4.D0, 4.D0,
     $                      4.D0/
      data      GPR5 /2.D0, 1.8D1, 6.D0, 1.8D1,
     $                       1.D1/
      data      M5   /4, 2, 2, 2, 2/
      data      ALF5 /4.0266D0, 1.75284D1, 2.74353D1, 4.90023D1,
     $                1.431411D2, 3.068586D2,
     $                1.49231D1, 3.90769D1,
     $                5.61826D1, 7.58174D1,
     $                2.24437D1, 4.95563D1/
      data      GAM5 /9.8D-2, 1.1913D1, 3.124D1, 8.6166D1,
     $                8.3085D1, 1.03846D2,
     $                9.0117D1, 9.703D1,
     $                9.9602D1, 1.067D2,
     $                1.0199D2, 1.08179D2/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTF')
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
      call BYE ('PARTF')
C
      return
      end
