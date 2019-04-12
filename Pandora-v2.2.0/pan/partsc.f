      subroutine PARTSC
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Scandium,
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
      dimension CHI1(3), ELL1(3), GPR1(3), M1(3), ALF1(10), GAM1(10)
      dimension CHI2(1), ELL2(1), GPR2(1), M2(1), ALF2(5), GAM2(5)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(4), GAM3(4)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1), ALF4(2), GAM4(2)
      dimension CHI5(1), ELL5(1), GPR5(1), M5(1), ALF5(3), GAM5(3)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /4.D0/
      data      NS1  /3/
      data      CHI1 /6.538D0, 7.147D0, 8.042D0/
      data      ELL1 /4.9D0, 4.9D0, 5.D0/
      data      GPR1 /3.D1, 4.2D1, 1.8D1/
      data      M1   /4, 3, 3/
      data      ALF1 /6.0014D0, 8.31958D1, 6.73666D1, 3.294354D2,
     $                4.40793D1, 1.699969D2, 5.339195D2,
     $                3.41642D1, 1.248475D2, 2.289879D2/
      data      GAM1 /2.1D-2, 2.056D0, 3.551D0, 5.465D0,
     $                1.535D0, 3.797D0, 6.203D0,
     $                2.389D0, 4.858D0, 7.141D0/
C----  ION = 2 ----------------------------------------------
      data      G2   /3.D0/
      data      NS2  /1/
      data      CHI2 /1.2891D1/
      data      ELL2 /5.D0/
      data      GPR2 /2.D1/
      data      M2   /5/
      data      ALF2 /1.19979D1, 1.6928D1, 2.84778D1, 8.20418D1,
     $                           2.34536D2/
      data      GAM2 /1.1D-2, 4.3D-1, 1.156D0, 3.711D0,
     $                        8.863D0/
C----  ION = 3 ----------------------------------------------
      data      G3   /4.D0/
      data      NS3  /1/
      data      CHI3 /2.4752D1/
      data      ELL3 /6.D0/
      data      GPR3 /2.D0/
      data      M3   /4/
      data      ALF3 /6.0042D0, 2.7101D0, 1.39801D1, 6.53039D1/
      data      GAM3 /2.5D-2, 3.499D0, 1.0463D1, 1.8606D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /1.D0/
      data      NS4  /1/
      data      CHI4 /7.409D1/
      data      ELL4 /4.6D0/
      data      GPR4 /1.2D1/
      data      M4   /2/
      data      ALF4 /1.2D1, 1.2D1/
      data      GAM4 /4.1779D1, 5.7217D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /4.D0/
      data      NS5  /1/
      data      CHI5 /9.1847D1/
      data      ELL5 /3.8D0/
      data      GPR5 /1.8D1/
      data      M5   /3/
      data      ALF5 /2.0051D0, 2.9621D0, 2.90306D1/
      data      GAM5 /5.39D-1, 2.4442D1, 5.1079D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTSC')
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
      call BYE ('PARTSC')
C
      return
      end
