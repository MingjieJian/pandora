      subroutine PARTNB
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Niobium,
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
      dimension CHI1(1), ELL1(1), GPR1(1), M1(1), ALF1(5), GAM1(5)
      dimension CHI2(1), ELL2(1), GPR2(1), M2(1), ALF2(5), GAM2(5)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(4), GAM3(4)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1), ALF4(5), GAM4(5)
      dimension CHI5(1), ELL5(1), GPR5(1), M5(1), ALF5(4), GAM5(4)
      dimension CHI6(1), ELL6(1), GPR6(1), M6(1), ALF6(2), GAM6(2)
C
      data      LION /6/
C----  ION = 1 ----------------------------------------------
      data      G1   /2.D0/
      data      NS1  /1/
      data      CHI1 /6.881D0/
      data      ELL1 /6.4D0/
      data      GPR1 /5.D1/
      data      M1   /5/
      data      ALF1 /1.00963D1, 3.77286D1, 1.363014D2, 7.814263D2,
     $                           1.6963149D3/
      data      GAM1 /3.D-2, 1.5D-1, 1.006D0, 2.634D0,
     $                       4.916D0/
C----  ION = 2 ----------------------------------------------
      data      G2   /1.D0/
      data      NS2  /1/
      data      CHI2 /1.4316D1/
      data      ELL2 /3.8D0/
      data      GPR2 /5.6D1/
      data      M2   /5/
      data      ALF2 /7.8059D0, 2.78039D1, 1.077304D2, 2.992145D2,
     $                          7.453281D2/
      data      GAM2 /3.2D-2, 1.71D-1, 8.58D-1, 2.305D0,
     $                        5.995D0/
C----  ION = 3 ----------------------------------------------
      data      G3   /4.D0/
      data      NS3  /1/
      data      CHI3 /2.5038D1/
      data      ELL3 /3.7D0/
      data      GPR3 /4.2D1/
      data      M3   /4/
      data      ALF3 /7.707D0, 1.61724D1, 2.7163D1, 8.49473D1/
      data      GAM3 /6.9D-2, 2.06D-1, 3.198D0, 8.348D0/
C----  ION = 4 ----------------------------------------------
      data      G4   /5.D0/
      data      NS4  /1/
      data      CHI4 /3.8251D1/
      data      ELL4 /6.D0/
      data      GPR4 /2.D1/
      data      M4   /5/
      data      ALF4 /1.33087D1, 1.9138D1, 1.20353D1, 9.471D1,
     $                           4.127579D2/
      data      GAM4 /1.68D-1, 5.37D-1, 2.454D0, 1.1863D1,
     $                         2.8177D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /4.D0/
      data      NS5  /1/
      data      CHI5 /4.958D1/
      data      ELL5 /6.D0/
      data      GPR5 /2.D0/
      data      M5   /4/
      data      ALF5 /6.001D0, 3.3714D0, 2.03125D1, 4.03147D1/
      data      GAM5 /2.32D-1, 1.0404D1, 2.2652D1, 3.4952D1/
C----  ION = 6 ----------------------------------------------
      data      G6   /1.D0/
      data      NS6  /1/
      data      CHI6 /1.02848D2/
      data      ELL6 /5.D0/
      data      GPR6 /1.2D1/
      data      M6   /2/
      data      ALF6 /1.54391D1, 2.85593D1/
      data      GAM6 /4.139D1, 6.9575D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTNB')
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
      call BYE ('PARTNB')
C
      return
      end
