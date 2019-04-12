      subroutine PARTNI
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Nickel,
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
      real*8 ALF1, ALF2, ALF3, CHI1, CHI2, CHI3, CHI4, ELL1, ELL2, ELL3,
     $       ELL4, ERROR, G0, G1, G2, G3, G4, GAM1, GAM2, GAM3, GPR1,
     $       GPR2, GPR3, GPR4, H, PE, SIGMA, THETA, X, Z, ZERO, dmmy
      integer ION, LION, M1, M2, M3, M4, NS1, NS2, NS3, NS4
C     !DASH
      external  HOOP, DEPART, HI, BYE
C
      dimension CHI1(2), ELL1(2), GPR1(2), M1(2), ALF1(9), GAM1(9)
      dimension CHI2(2), ELL2(2), GPR2(2), M2(2), ALF2(8), GAM2(8)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(5), GAM3(5)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1)
C
      data      LION /4/
C----  ION = 1 ----------------------------------------------
      data      G1   /9.D0/
      data      NS1  /2/
      data      CHI1 /7.633D0, 8.793D0/
      data      ELL1 /5.4D0, 5.D0/
      data      GPR1 /2.D1, 5.6D1/
      data      M1   /6, 3/
      data      ALF1 /7.1268D0, 1.24486D1, 1.19953D1, 1.00546D1,
     $                          1.141658D2, 3.912064D2,
     $                2.63908D1, 2.138081D2, 9.387927D2/
      data      GAM1 /2.6D-2, 1.37D-1, 3.15D-1, 1.778D0,
     $                        4.029D0, 6.621D0,
     $                2.249D0, 4.042D0, 7.621D0/
C----  ION = 2 ----------------------------------------------
      data      G2   /6.D0/
      data      NS2  /2/
      data      CHI2 /1.8147D1, 2.0233D1/
      data      ELL2 /9.D0, 5.D0/
      data      GPR2 /4.2D1, 1.8D1/
      data      M2   /5, 3/
      data      ALF2 /4.1421D0, 3.73781D1, 2.59712D1, 3.333397D2,
     $                          3.111633D2,
     $                3.31031D1, 1.841854D2, 1.367072D2/
      data      GAM2 /1.91D-1, 1.235D0, 3.358D0, 8.429D0,
     $                         1.7096D1,
     $                3.472D0, 9.065D0, 1.6556D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /9.D0/
      data      NS3  /1/
      data      CHI3 /3.5161D1/
      data      ELL3 /5.D0/
      data      GPR3 /5.6D1/
      data      M3   /5/
      data      ALF3 /1.11915D1, 5.4174D0, 5.36793D1, 4.606781D2,
     $                           3.800056D2/
      data      GAM3 /1.94D-1, 1.305D0, 5.813D0, 1.4172D1,
     $                         2.6169D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /2.8D1/
      data      NS4  /1/
      data      CHI4 /5.6025D1/
      data      ELL4 /3.D0/
      data      GPR4 /5.D1/
      data      M4   /0/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTNI')
C     !BEG
      if((ION.lt.1).or.(ION.gt.LION)) then
        G0 = ERROR
        SIGMA = ZERO
      else
        Z = ION
        call HOOP     (Z,THETA,PE,H)
        goto (101,102,103,104), ION
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
          call DEPART (G4,NS4,CHI4,ELL4,GPR4,M4,dmmy,dmmy,Z,THETA,H,
     $                 X,G0,SIGMA)
  100   continue
      end if
C     !END
      call BYE ('PARTNI')
C
      return
      end
