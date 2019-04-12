      subroutine PARTAL
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Aluminum,
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
      dimension CHI2(2), ELL2(2), GPR2(2), M2(2), ALF2(5), GAM2(5)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(3), GAM3(3)
      dimension CHI4(2), ELL4(2), GPR4(2), M4(2), ALF4(4), GAM4(4)
      dimension CHI5(1), ELL5(1), GPR5(1), M5(1), ALF5(4), GAM5(4)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /2.D0/
      data      NS1  /2/
      data      CHI1 /5.984D0, 1.0634D1/
      data      ELL1 /7.D0, 4.D0/
      data      GPR1 /2.D0, 1.8D1/
      data      M1   /3, 2/
      data      ALF1 /4.0009D0, 1.17804D1, 1.422179D2,
     $                1.36585D1, 9.63371D1/
      data      GAM1 /1.4D-2, 3.841D0, 5.42D0,
     $                3.727D0, 8.833D0/
C----  ION = 2 ----------------------------------------------
      data      G2   /1.D0/
      data      NS2  /2/
      data      CHI2 /1.8823D1, 2.5496D1/
      data      ELL2 /7.D0, 4.D0/
      data      GPR2 /4.D0, 1.2D1/
      data      M2   /3, 2/
      data      ALF2 /1.00807D1, 4.95843D1, 2.853343D2,
     $                1.46872D1, 5.93122D1/
      data      GAM2 /4.749D0, 1.1902D1, 1.6719D1,
     $                1.131D1, 1.8268D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /2.D0/
      data      NS3  /1/
      data      CHI3 /2.8441D1/
      data      ELL3 /7.D0/
      data      GPR3 /2.D0/
      data      M3   /3/
      data      ALF3 /6.3277D0, 2.95086D1, 1.341634D2/
      data      GAM3 /6.751D0, 1.6681D1, 2.4151D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /1.D0/
      data      NS4  /2/
      data      CHI4 /1.19957D2, 1.20383D2/
      data      ELL4 /5.D0, 5.D0/
      data      GPR4 /8.D0, 4.D0/
      data      M4   /2, 2/
      data      ALF4 /4.63164D1, 1.536833D2,
     $                2.29896D1, 7.70103D1/
      data      GAM4 /8.3551D1, 1.04787D2,
     $                8.4293D1, 1.05171D2/
C----  ION = 5 ----------------------------------------------
      data      G5   /4.D0/
      data      NS5  /1/
      data      CHI5 /1.53772D2/
      data      ELL5 /5.D0/
      data      GPR5 /1.8D1/
      data      M5   /4/
      data      ALF5 /2.0002D0, 2.3599D0, 1.512144D2, 3.924253D2/
      data      GAM5 /4.26D-1, 4.6012D1, 1.07928D2, 1.31729D2/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTAL')
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
      call BYE ('PARTAL')
C
      return
      end
