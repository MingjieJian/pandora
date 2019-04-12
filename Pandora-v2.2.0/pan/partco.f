      subroutine PARTCO
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Cobalt,
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
      dimension CHI1(4), ELL1(4), GPR1(4), M1(4), ALF1(14), GAM1(14)
      dimension CHI2(2), ELL2(2), GPR2(2), M2(2), ALF2(9), GAM2(9)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(5), GAM3(5)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1)
C
      data      LION /4/
C----  ION = 1 ----------------------------------------------
      data      G1   /1.D1/
      data      NS1  /4/
      data      CHI1 /7.863D0, 8.378D0, 9.16D0, 9.519D0/
      data      ELL1 /5.D0, 5.D0, 5.D0, 5.D0/
      data      GPR1 /4.2D1, 7.D1, 4.2D1, 1.8D1/
      data      M1   /5, 3, 3, 3/
      data      ALF1 /1.1912D1, 2.04424D1, 2.83863D1, 1.325038D2,
     $                          6.007461D2,
     $                3.33092D1, 2.374331D2, 9.772502D2,
     $                5.55396D1, 3.188169D2, 6.196366D2,
     $                3.269D1, 8.38694D1, 1.074378D2/
      data      GAM1 /1.12D-1, 3.41D-1, 8.09D-1, 3.808D0,
     $                         6.723D0,
     $                2.057D0, 3.484D0, 7.21D0,
     $                2.405D0, 5.133D0, 8.097D0,
     $                2.084D0, 5.291D0, 8.426D0/
C----  ION = 2 ----------------------------------------------
      data      G2   /9.D0/
      data      NS2  /2/
      data      CHI2 /1.7052D1, 1.8958D1/
      data      ELL2 /5.D0, 5.D0/
      data      GPR2 /5.6D1, 2.4D1/
      data      M2   /5, 4/
      data      ALF2 /1.12593D1, 3.82239D1, 2.29964D1, 2.613486D2,
     $                           6.371485D2,
     $                2.30233D1, 4.16599D1, 2.64646D2, 1.816699D2/
      data      GAM2 /1.35D-1, 5.17D-1, 1.606D0, 6.772D0,
     $                         1.2622D1,
     $                2.512D0, 4.348D0, 8.253D0, 1.5377D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /1.D1/
      data      NS3  /1/
      data      CHI3 /3.3491D1/
      data      ELL3 /5.D0/
      data      GPR3 /5.D1/
      data      M3   /5/
      data      ALF3 /1.60356D1, 7.8633D0, 7.03158D1, 4.233512D2,
     $                           7.423553D2/
      data      GAM3 /1.32D-1, 8.63D-1, 3.086D0, 1.1789D1,
     $                         2.3263D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /2.D1/
      data      NS4  /1/
      data      CHI4 /5.3001D1/
      data      ELL4 /3.D0/
      data      GPR4 /1.2D1/
      data      M4   /0/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTCO')
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
      call BYE ('PARTCO')
C
      return
      end
