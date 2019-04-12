      subroutine PARTCR
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Chromium,
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
      real*8 ALF1, ALF2, ALF3, ALF4, ALF5, ALF6, CHI1, CHI2, CHI3, CHI4,
     $       CHI5, CHI6, ELL1, ELL2, ELL3, ELL4, ELL5, ELL6, ERROR, G0,
     $       G1, G2, G3, G4, G5, G6, GAM1, GAM2, GAM3, GAM4, GAM5, GAM6,
     $       GPR1, GPR2, GPR3, GPR4, GPR5, GPR6, H, PE, SIGMA, THETA, X,
     $       Z, ZERO
      integer ION, LION, M1, M2, M3, M4, M5, M6, NS1, NS2, NS3, NS4,
     $        NS5, NS6
C     !DASH
      external HOOP, DEPART, HI, BYE
C
      dimension CHI1(3), ELL1(3), GPR1(3), M1(3), ALF1(9), GAM1(9)
      dimension CHI2(2), ELL2(2), GPR2(2), M2(2), ALF2(8), GAM2(8)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(4), GAM3(4)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1), ALF4(4), GAM4(4)
      dimension CHI5(1), ELL5(1), GPR5(1), M5(1), ALF5(4), GAM5(4)
      dimension CHI6(1), ELL6(1), GPR6(1), M6(1), ALF6(3), GAM6(3)
C
      data      LION /6/
C----  ION = 1 ----------------------------------------------
      data      G1   /7.D0/
      data      NS1  /3/
      data      CHI1 /6.763D0, 8.285D0, 9.221D0/
      data      ELL1 /5.D0, 5.D0, 5.D0/
      data      GPR1 /1.2D1, 6.D1, 4.D1/
      data      M1   /3, 3, 3/
      data      ALF1 /3.01842D1, 7.92847D1, 1.495293D2,
     $                2.153696D2, 1.191974D2, 7.414321D2,
     $                1.849946D2, 1.3525038D3, 7.844937D2/
      data      GAM1 /9.93D-1, 3.07D0, 5.673D0,
     $                3.339D0, 4.801D0, 7.198D0,
     $                2.829D0, 4.99D0, 7.643D0/
C----  ION = 2 ----------------------------------------------
      data      G2   /6.D0/
      data      NS2  /2/
      data      CHI2 /1.6493D1, 1.8662D1/
      data      ELL2 /5.D0, 5.D0/
      data      GPR2 /5.D1, 1.8D1/
      data      M2   /4, 4/
      data      ALF2 /4.66191D1, 1.601361D2, 4.880449D2, 6.571928D2,
     $                4.71742D1, 2.670275D2, 4.411324D2, 1.50665D2/
      data      GAM2 /1.645D0, 3.727D0, 7.181D0, 1.2299D1,
     $                2.902D0, 4.273D0, 8.569D0, 1.4912D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /1.D0/
      data      NS3  /1/
      data      CHI3 /3.095D1/
      data      ELL3 /5.D0/
      data      GPR3 /5.6D1/
      data      M3   /4/
      data      ALF3 /2.43768D1, 1.228359D2, 2.855092D2, 7.945092D2/
      data      GAM3 /4.7D-2, 2.556D0, 9.411D0, 2.1198D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /4.D0/
      data      NS4  /1/
      data      CHI4 /4.958D1/
      data      ELL4 /5.D0/
      data      GPR4 /4.2D1/
      data      M4   /4/
      data      ALF4 /2.42296D1, 7.50258D1, 1.729452D2, 5.436511D2/
      data      GAM4 /7.8D-2, 2.242D0, 1.5638D1, 3.2725D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /5.D0/
      data      NS5  /1/
      data      CHI5 /7.3093D1/
      data      ELL5 /5.2D0/
      data      GPR5 /2.D1/
      data      M5   /4/
      data      ALF5 /1.59819D1, 1.768D1, 9.52003D1, 2.250947D2/
      data      GAM5 /1.03D-1, 2.146D0, 2.6153D1, 4.9381D1/
C----  ION = 6 ----------------------------------------------
      data      G6   /4.D0/
      data      NS6  /1/
      data      CHI6 /9.0595D1/
      data      ELL6 /4.9D0/
      data      GPR6 /2.D0/
      data      M6   /3/
      data      ALF6 /6.0008D0, 6.8734D0, 1.51253D1/
      data      GAM6 /1.19D-1, 3.2711D1, 5.8117D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTCR')
C     !BEG
      if((ION.lt.1).or.(ION.gt.LION)) then
        G0 = ERROR
        SIGMA = ZERO
      else
        Z = ION
        call HOOP     (Z,THETA,PE,H)
        goto (101,102,103,104,105,106), ION
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
          goto 100
  106   continue
          call DEPART (G6,NS6,CHI6,ELL6,GPR6,M6,ALF6,GAM6,Z,THETA,H,
     $                 X,G0,SIGMA)
  100   continue
      end if
C     !END
      call BYE ('PARTCR')
C
      return
      end
