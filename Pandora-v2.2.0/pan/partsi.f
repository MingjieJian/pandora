      subroutine PARTSI
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Silicon,
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
      external  HOOP, DEPART, HI, BYE
C
      dimension CHI1(1), ELL1(1), GPR1(1), M1(1), ALF1(5), GAM1(5)
      dimension CHI2(2), ELL2(2), GPR2(2), M2(2), ALF2(8), GAM2(8)
      dimension CHI3(2), ELL3(2), GPR3(2), M3(2), ALF3(7), GAM3(7)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1), ALF4(3), GAM4(3)
      dimension CHI5(2), ELL5(2), GPR5(2), M5(2), ALF5(4), GAM5(4)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /1.D0/
      data      NS1  /1/
      data      CHI1 /8.149D0/
      data      ELL1 /6.1D0/
      data      GPR1 /1.2D1/
      data      M1   /5/
      data      ALF1 /7.9658D0, 4.6762D0, 1.3512D0, 1.232267D2,
     $                          4.437797D2/
      data      GAM1 /2.D-2, 7.52D-1, 1.614D0, 5.831D0,
     $                       7.431D0/
C----  ION = 2 ----------------------------------------------
      data      G2   /2.D0/
      data      NS2  /2/
      data      CHI2 /1.6339D1, 2.2894D1/
      data      ELL2 /5.9D0, 5.D0/
      data      GPR2 /2.D0, 1.8D1/
      data      M2   /4, 4/
      data      ALF2 /4.D0, 7.4186D0, 2.41754D1, 6.0406D1,
     $                1.44695D1, 1.19721D1, 2.65062D1, 2.690521D2/
      data      GAM2 /3.6D-2, 8.795D0, 1.1208D1, 1.3835D1,
     $                5.418D0, 7.825D0, 1.444D1, 1.9412D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /1.D0/
      data      NS3  /2/
      data      CHI3 /3.3459D1, 4.2333D1/
      data      ELL3 /5.D0, 5.D0/
      data      GPR3 /4.D0, 1.2D1/
      data      M3   /4, 3/
      data      ALF3 /9.1793D0, 4.8766D0, 2.91442D1, 5.27998D1,
     $                1.32674D1, 3.60147D1, 1.80691D2/
      data      GAM3 /6.572D0, 1.1449D1, 1.8424D1, 2.5457D1,
     $                1.5682D1, 2.701D1, 3.4599D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /2.D0/
      data      NS4  /1/
      data      CHI4 /4.513D1/
      data      ELL4 /7.D0/
      data      GPR4 /2.D0/
      data      M4   /3/
      data      ALF4 /6.4839D0, 2.76851D1, 1.358301D2/
      data      GAM4 /9.042D0, 2.4101D1, 3.7445D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /1.D0/
      data      NS5  /2/
      data      CHI5 /1.66725D2, 1.67357D2/
      data      ELL5 /4.9D0, 4.9D0/
      data      GPR5 /8.D0, 4.D0/
      data      M5   /2, 2/
      data      ALF5 /3.69821D1, 1.630176D2,
     $                1.91161D1, 8.08837D1/
      data      GAM5 /1.15608D2, 1.42257D2,
     $                1.18377D2, 1.43084D2/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTSI')
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
      call BYE ('PARTSI')
C
      return
      end
