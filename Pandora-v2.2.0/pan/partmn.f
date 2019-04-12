      subroutine PARTMN
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Manganese,
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
      external  HOOP, DEPART, HI, BYE
C
      dimension CHI1(3), ELL1(3), GPR1(3), M1(3), ALF1(8), GAM1(8)
      dimension CHI2(2), ELL2(2), GPR2(2), M2(2), ALF2(8), GAM2(8)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(4), GAM3(4)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1), ALF4(4), GAM4(4)
      dimension CHI5(1), ELL5(1), GPR5(1), M5(1), ALF5(4), GAM5(4)
      dimension CHI6(1), ELL6(1), GPR6(1), M6(1), ALF6(4), GAM6(4)
C
      data      LION /6/
C----  ION = 1 ----------------------------------------------
      data      G1   /6.D0/
      data      NS1  /3/
      data      CHI1 /7.432D0, 8.606D0, 9.24D0/
      data      ELL1 /6.D0, 5.D0, 6.D0/
      data      GPR1 /1.4D1, 1.D1, 5.D1/
      data      M1   /3, 2, 3/
      data      ALF1 /5.39107D1, 8.13931D1, 5.466945D2,
     $                1.441893D2, 4.078029D2,
     $                4.56177D1, 2.984423D2, 2.4109335D3/
      data      GAM1 /2.527D0, 4.204D0, 6.602D0,
     $                4.155D0, 7.321D0,
     $                2.285D0, 5.631D0, 8.448D0/
C----  ION = 2 ----------------------------------------------
      data      G2   /7.D0/
      data      NS2  /2/
      data      CHI2 /1.5636D1, 1.8963D1/
      data      ELL2 /7.D0, 5.D0/
      data      GPR2 /1.2D1, 7.2D1/
      data      M2   /4, 4/
      data      ALF2 /2.26382D1, 9.38419D1, 1.839367D2, 9.075765D2,
     $                1.370409D2, 1.686783D2, 3.290287D2, 7.732513D2/
      data      GAM2 /1.496D0, 3.839D0, 7.751D0, 1.3484D1,
     $                3.681D0, 6.054D0, 9.934D0, 1.4936D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /6.D0/
      data      NS3  /1/
      data      CHI3 /3.369D1/
      data      ELL3 /5.D0/
      data      GPR3 /5.D1/
      data      M3   /4/
      data      ALF3 /7.01925D1, 7.23372D1, 2.139512D2, 5.395165D2/
      data      GAM3 /3.531D0, 6.967D0, 1.5222D1, 2.5069D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /1.D0/
      data      NS4  /1/
      data      CHI4 /5.3001D1/
      data      ELL4 /5.D0/
      data      GPR4 /5.6D0/
      data      M4   /4/
      data      ALF4 /2.42373D1, 9.35415D1, 4.566167D2, 5.065484D2/
      data      GAM4 /7.1D-2, 2.896D0, 2.0725D1, 3.7383D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /4.D0/
      data      NS5  /1/
      data      CHI5 /7.6006D1/
      data      ELL5 /5.D0/
      data      GPR5 /4.2D1/
      data      M5   /4/
      data      ALF5 /2.47687D1, 6.69896D1, 2.641853D2, 4.840161D2/
      data      GAM5 /1.26D-1, 2.66D0, 2.8528D1, 5.3413D1/
C----  ION = 6 ----------------------------------------------
      data      G6   /5.D0/
      data      NS6  /1/
      data      CHI6 /1.09002D2/
      data      ELL6 /3.7D0/
      data      GPR6 /2.D1/
      data      M6   /4/
      data      ALF6 /1.23142D1, 4.305D0, 2.22424D1, 6.01198D1/
      data      GAM6 /1.17D-1, 3.5D-1, 2.539D0, 4.0301D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTMN')
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
      call BYE ('PARTMN')
C
      return
      end
