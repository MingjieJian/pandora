      subroutine PARTKR
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Krypton,
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
      real*8 ALF1, ALF2, ALF3, CHI1, CHI2, CHI3, ELL1, ELL2, ELL3,
     $       ERROR, G0, G1, G2, G3, GAM1, GAM2, GAM3, GPR1, GPR2, GPR3,
     $       H, PE, SIGMA, THETA, X, Z, ZERO
      integer ION, LION, M1, M2, M3, NS1, NS2, NS3
C     !DASH
      external HOOP, DEPART, HI, BYE
C
      dimension CHI1(2), ELL1(2), GPR1(2), M1(2), ALF1(4), GAM1(4)
      dimension CHI2(3), ELL2(3), GPR2(3), M2(3), ALF2(7), GAM2(7)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(4), GAM3(4)
C
      data      LION /3/
C----  ION = 1 ----------------------------------------------
      data      G1   /1.D0/
      data      NS1  /2/
      data      CHI1 /1.3996D1, 1.4661D1/
      data      ELL1 /1.D1, 1.D1/
      data      GPR1 /8.D0, 4.D0/
      data      M1   /2, 2/
      data      ALF1 /8.11676D1, 2.0548318D3,
     $                3.94009D1, 9.685989D2/
      data      GAM1 /1.1409D1, 1.369D1,
     $                1.2064D1, 1.4369D1/
C----  ION = 2 ----------------------------------------------
      data      G2   /4.D0/
      data      NS2  /3/
      data      CHI2 /2.4565D1, 2.638D1, 2.8665D1/
      data      ELL2 /8.D0, 7.D0, 7.D0/
      data      GPR2 /1.8D1, 1.D1, 2.D0/
      data      M2   /3, 2, 2/
      data      ALF2 /2.0215D0, 2.434886D2, 2.0384851D3,
     $                1.392137D2, 5.90784D2,
     $                1.97365D1, 1.262633D2/
      data      GAM2 /6.75D-1, 1.6518D1, 2.294D1,
     $                1.8636D1, 2.4293D1,
     $                2.0502D1, 2.6753D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /5.D0/
      data      NS3  /1/
      data      CHI3 /3.994D1/
      data      ELL3 /5.5D0/
      data      GPR3 /8.D0/
      data      M3   /4/
      data      ALF3 /4.5228D0, 5.4786D0, 2.969358D2, 5.690524D2/
      data      GAM3 /6.17D-1, 2.202D0, 1.9856D1, 2.9183D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTKR')
C     !BEG
      if((ION.lt.1).or.(ION.gt.LION)) then
        G0 = ERROR
        SIGMA = ZERO
      else
        Z = ION
        call HOOP     (Z,THETA,PE,H)
        goto (101,102,103), ION
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
  100   continue
      end if
C     !END
      call BYE ('PARTKR')
C
      return
      end
