      subroutine PARTBE
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Beryllium,
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
      real*8 ALF1, ALF2, ALF3, ALF4, CHI1, CHI2, CHI3, CHI4, CONST,
     $       ELL1, ELL2, ELL3, ELL4, ERROR, G0, G1, G2, G3, G4, G5,
     $       GAM1, GAM2, GAM3, GAM4, GPR1, GPR2, GPR3, GPR4, H, PE,
     $       SIGMA, THETA, X, Z, ZERO
      integer ION, LION, M1, M2, M3, M4, NS1, NS2, NS3, NS4
C     !DASH
      external HOOP, DEPART, HI, BYE
C
      dimension CHI1(2), ELL1(2), GPR1(2), M1(2), ALF1(5), GAM1(5)
      dimension CHI2(1), ELL2(1), GPR2(1), M2(1), ALF2(3), GAM2(3)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(2), GAM3(2)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1), ALF4(2), GAM4(2)
C
      data      LION /5/
C----  ION = 1  ---------------------------------------------
      data      G1   /1.D0/
      data      NS1  /2/
      data      CHI1 /9.32D0, 1.3278D1/
      data      ELL1 /6.D0, 4.D0/
      data      GPR1 /4.D0, 1.2D1/
      data      M1   /3, 2/
      data      ALF1 /9.1849D0, 3.29263D1, 1.838887D2,
     $                1.99563D1, 8.80437D1/
      data      GAM1 /2.735D0, 6.774D0, 8.569D0,
     $                1.075D1, 1.1672D1/
C----  ION = 2  ---------------------------------------------
      data      G2   /2.D0/
      data      NS2  /1/
      data      CHI2 /1.8206D1/
      data      ELL2 /8.D0/
      data      GPR2 /2.D0/
      data      M2   /3/
      data      ALF2 /6.0478D0, 3.59723D1, 2.339798D2/
      data      GAM2 /3.967D0, 1.2758D1, 1.6692D1/
C----  ION = 3  ---------------------------------------------
      data      G3   /1.D0/
      data      NS3  /1/
      data      CHI3 /1.5385D2/
      data      ELL3 /6.D0/
      data      GPR3 /4.D0/
      data      M3   /2/
      data      ALF3 /2.31331D1, 1.928669D2/
      data      GAM3 /1.23719D2, 1.47167D2/
C----  ION = 4  ---------------------------------------------
      data      G4   /2.D0/
      data      NS4  /1/
      data      CHI4 /2.17657D2/
      data      ELL4 /8.D0/
      data      GPR4 /2.D0/
      data      M4   /2/
      data      ALF4 /1.61651D1, 2.618348D2/
      data      GAM4 /1.71066D2, 2.10558D2/
C----  ION = 5  ---------------------------------------------
      data      G5   /1.D0/
C------------------------------------------------------------
      data      ZERO, ERROR, CONST /0.D0, -1.D0, 1.D3/
C     !EJECT
C
      call HI ('PARTBE')
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
          G0 = G5
          SIGMA = ZERO
          X = CONST
  100   continue
      end if
C     !END
      call BYE ('PARTBE')
C
      return
      end
