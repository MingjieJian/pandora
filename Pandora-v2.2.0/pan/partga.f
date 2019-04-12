      subroutine PARTGA
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Gallium,
C     such that Partition Function Q = G0 + SIGMA.
C     (ION=1 for the neutral atom.)
C     Also returns the Excitation Potemtial X.
C     THETA is the reciprocal temperature, and PE is the
C     electron pressure in dynes/cm**2.
C     Returns G0=-1 if data for "ION" is unavailable.
C     (For reference, see listing of "DEPART".)
C     !DASH
      save
C     !DASH
      real*8 ALF1, ALF2, ALF3, ALF4, CHI1, CHI2, CHI3, CHI4, ELL1, ELL2,
     $       ELL3, ELL4, ERROR, G0, G1, G2, G3, G4, GAM1, GAM2, GAM3,
     $       GAM4, GPR1, GPR2, GPR3, GPR4, H, PE, SIGMA, THETA, X, Z,
     $       ZERO
      integer ION, LION, M1, M2, M3, M4, NS1, NS2, NS3, NS4
C     !DASH
      external HOOP, DEPART, HI, BYE
C
      dimension CHI1(2), ELL1(2), GPR1(2), M1(2), ALF1(6), GAM1(6)
      dimension CHI2(1), ELL2(1), GPR2(1), M2(1), ALF2(3), GAM2(3)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(3), GAM3(3)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1), ALF4(3), GAM4(3)
C
      data      LION /4/
C----  ION = 1 ----------------------------------------------
      data      G1   /2.D0/
      data      NS1  /2/
      data      CHI1 /5.997D0, 1.1982D1/
      data      ELL1 /1.D1, 5.D0/
      data      GPR1 /2.D0, 1.8D1/
      data      M1   /4, 2/
      data      ALF1 /4.0002D0, 2.0966D0, 3.49646D1, 4.969385D2,
     $                1.64913D1, 2.295019D2/
      data      GAM1 /1.02D-1, 1.854D0, 4.531D0, 5.793D0,
     $                5.126D0, 1.1014D1/
C----  ION = 2 ----------------------------------------------
      data      G2   /1.D0/
      data      NS2  /1/
      data      CHI2 /2.0509D1/
      data      ELL2 /7.D0/
      data      GPR2 /4.D0/
      data      M2   /3/
      data      ALF2 /1.01998D1, 5.8605D1, 2.441949D2/
      data      GAM2 /6.102D0, 1.3989D1, 1.8438D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /2.D0/
      data      NS3  /1/
      data      CHI3 /3.0702D1/
      data      ELL3 /6.D0/
      data      GPR3 /2.D0/
      data      M3   /3/
      data      ALF3 /6.0724D0, 2.30304D1, 5.08971D1/
      data      GAM3 /8.239D0, 1.8899D1, 2.5142D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /1.D0/
      data      NS4  /1/
      data      CHI4 /6.4157D1/
      data      ELL4 /5.D0/
      data      GPR4 /2.D1/
      data      M4   /3/
      data      ALF4 /1.99887D1, 5.99442D1, 2.400671D2/
      data      GAM4 /1.8801D1, 2.8656D1, 5.056D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTGA')
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
          call DEPART (G4,NS4,CHI4,ELL4,GPR4,M4,ALF4,GAM4,Z,THETA,H,
     $                 X,G0,SIGMA)
  100   continue
      end if
C     !END
      call BYE ('PARTGA')
C
      return
      end
