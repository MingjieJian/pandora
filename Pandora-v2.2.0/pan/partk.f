      subroutine PARTK
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Potassium,
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
      dimension CHI3(3), ELL3(3), GPR3(3), M3(3), ALF3(8), GAM3(8)
      dimension CHI4(3), ELL4(3), GPR4(3), M4(3), ALF4(8), GAM4(8)
      dimension CHI5(2), ELL5(2), GPR5(2), M5(2), ALF5(6), GAM5(6)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /2.D0/
      data      NS1  /1/
      data      CHI1 /4.339D0/
      data      ELL1 /7.D0/
      data      GPR1 /2.D0/
      data      M1   /3/
      data      ALF1 /1.29782D1, 1.486673D2, 6.3493D0/
      data      GAM1 /1.871D0, 3.713D0, 1.8172D1/
C----  ION = 2 ----------------------------------------------
      data      G2   /1.D0/
      data      NS2  /2/
      data      CHI2 /3.181D1, 3.2079D1/
      data      ELL2 /5.D0, 5.D0/
      data      GPR2 /8.D0, 4.D0/
      data      M2   /2, 3/
      data      ALF2 /6.63444D1, 1.016553D2,
     $                4.0001D0, 1.34465D1, 4.65534D1/
      data      GAM2 /2.1185D1, 2.7705D1,
     $                2.059D0, 2.3709D1, 2.8542D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /4.D0/
      data      NS3  /3/
      data      CHI3 /4.5738D1, 4.7768D1, 5.0515D1/
      data      ELL3 /6.D0, 6.D0, 5.D0/
      data      GPR3 /1.8D1, 1.D1, 2.D0/
      data      M3   /3, 2, 3/
      data      ALF3 /2.0171D0, 1.164767D2, 7.134965D2,
     $                6.35907D1, 3.964079D2,
     $                2.D0, 1.D1, 3.D1/
      data      GAM3 /2.73D-1, 2.6709D1, 3.964D1,
     $                3.122D1, 4.1865D1,
     $                2.9955D1, 3.7557D1, 4.2862D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /5.D0/
      data      NS4  /3/
      data      CHI4 /6.0897D1, 6.389D1, 6.5849D1/
      data      ELL4 /6.D0, 5.D0, 5.D0/
      data      GPR4 /8.D0, 2.D1, 1.2D1/
      data      M4   /4, 2, 2/
      data      ALF4 /4.0702D0, 5.7791D0, 5.26795D1, 3.274539D2,
     $                6.28604D1, 3.571331D2,
     $                5.59337D1, 1.960646D2/
      data      GAM4 /2.28D-1, 2.274D0, 2.1703D1, 5.0191D1,
     $                3.2145D1, 4.9262D1,
     $                3.4155D1, 5.1718D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /4.D0/
      data      NS5  /2/
      data      CHI5 /8.2799D1, 8.515D1/
      data      ELL5 /3.6D0, 4.D0/
      data      GPR5 /1.8D1, 1.D1/
      data      M5   /4, 2/
      data      ALF5 /1.09275D1, 5.5398D0, 4.32761D1, 7.6256D1,
     $                4.2D1, 1.8D1/
      data      GAM5 /3.043D0, 5.479D0, 2.0547D1, 3.068D1,
     $                3.6275D1, 4.7345D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTK')
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
      call BYE ('PARTK')
C
      return
      end
