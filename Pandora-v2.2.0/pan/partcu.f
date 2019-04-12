      subroutine PARTCU
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Copper,
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
      external HOOP, DEPART, HI, BYE
C
      dimension CHI1(3), ELL1(3), GPR1(3), M1(3), ALF1(8), GAM1(8)
      dimension CHI2(2), ELL2(2), GPR2(2), M2(2), ALF2(7), GAM2(7)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(4), GAM3(4)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1)
C
      data      LION /4/
C----  ION = 1 ----------------------------------------------
      data      G1   /2.D0/
      data      NS1  /3/
      data      CHI1 /7.724D0, 1.0532D1, 1.098D1/
      data      ELL1 /8.D0, 6.D0, 5.D0/
      data      GPR1 /2.D0, 3.D1, 1.D1/
      data      M1   /2, 3, 3/
      data      ALF1 /1.10549D1, 2.389423D2,
     $                1.03077D1, 1.26299D2, 1.0733876D3,
     $                3.D1, 5.D1, 6.D1/
      data      GAM1 /4.212D0, 7.227D0,
     $                1.493D0, 5.859D0, 9.709D0,
     $                7.081D0, 9.362D0, 1.013D1/
C----  ION = 2 ----------------------------------------------
      data      G2   /1.D0/
      data      NS2  /2/
      data      CHI2 /2.0286D1, 2.7985D1/
      data      ELL2 /7.D0, 5.D0/
      data      GPR2 /2.D1, 5.6D1/
      data      M2   /4, 3/
      data      ALF2 /1.92984D1, 5.05974D1, 2.402021D2, 1.2169016D3,
     $                4.83048D1, 5.832011D2, 3.204931D2/
      data      GAM2 /2.865D0, 8.26D0, 1.4431D1, 1.8292D1,
     $                9.65D0, 1.464D1, 2.432D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /6.D0/
      data      NS3  /1/
      data      CHI3 /3.6826D1/
      data      ELL3 /5.D0/
      data      GPR3 /4.2D1/
      data      M3   /4/
      data      ALF3 /4.0155D0, 7.03264D1, 3.131213D2, 5.365331D2/
      data      GAM3 /3.37D-1, 8.52D0, 1.6925D1, 2.8342D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /2.1D1/
      data      NS4  /1/
      data      CHI4 /6.1975D1/
      data      ELL4 /2.9D0/
      data      GPR4 /5.6D1/
      data      M4   /0/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTCU')
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
      call BYE ('PARTCU')
C
      return
      end
