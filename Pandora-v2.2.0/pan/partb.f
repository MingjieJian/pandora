      subroutine PARTB
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Boron,
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
      dimension CHI1(1), ELL1(1), GPR1(1), M1(1), ALF1(3), GAM1(3)
      dimension CHI2(2), ELL2(2), GPR2(2), M2(2), ALF2(5), GAM2(5)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(3), GAM3(3)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1), ALF4(2), GAM4(2)
      dimension CHI5(1), ELL5(1), GPR5(1), M5(1), ALF5(2), GAM5(2)
C
      data      LION /5/
C----  ION = 1  ---------------------------------------------
      data      G1   /2.D0/
      data      NS1  /1/
      data      CHI1 /8.296D0/
      data      ELL1 /9.D0/
      data      GPR1 /2.D0/
      data      M1   /3/
      data      ALF1 /4.0086D0, 1.96741D1, 4.02311D2/
      data      GAM1 /2.D-3, 3.971D0, 7.882D0/
C----  ION = 2  ---------------------------------------------
      data      G2   /1.D0/
      data      NS2  /2/
      data      CHI2 /2.5149D1, 3.1146D1/
      data      ELL2 /6.D0, 4.D0/
      data      GPR2 /4.D0, 1.2D1/
      data      M2   /3, 2/
      data      ALF2 /9.7257D0, 3.09262D1, 1.863466D2,
     $                4.41629D1, 6.08371D1/
      data      GAM2 /4.72D0, 1.3477D1, 2.2103D1,
     $                2.3056D1, 2.4734D1/
C----  ION = 3  ---------------------------------------------
      data      G3   /2.D0/
      data      NS3  /1/
      data      CHI3 /3.792D1/
      data      ELL3 /6.D0/
      data      GPR3 /2.D0/
      data      M3   /3/
      data      ALF3 /6.0084D0, 2.35767D1, 7.64149D1/
      data      GAM3 /6.D0, 2.454D1, 3.23D1/
C----  ION = 4  ---------------------------------------------
      data      G4   /1.D0/
      data      NS4  /1/
      data      CHI4 /2.59298D2/
      data      ELL4 /5.D0/
      data      GPR4 /4.D0/
      data      M4   /2/
      data      ALF4 /1.9553D1, 9.6447D1/
      data      GAM4 /2.04371D2, 2.43028D2/
C----  ION = 5  ---------------------------------------------
      data      G5   /2.D0/
      data      NS5  /1/
      data      CHI5 /3.40127D2/
      data      ELL5 /6.D0/
      data      GPR5 /2.D0/
      data      M5   /2/
      data      ALF5 /8.D0, 1.D2/
      data      GAM5 /2.55201D2, 3.19715D2/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTB')
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
      call BYE ('PARTB')
C
      return
      end
