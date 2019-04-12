      subroutine PARTBA
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Barium,
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
      real*8 ALF1, ALF2, ALF4, CHI1, CHI2, CHI3, CHI4, ELL1, ELL2, ELL3,
     $       ELL4, ERROR, G0, G1, G2, G3, G4, GAM1, GAM2, GAM4, GPR1,
     $       GPR2, GPR3, GPR4, H, PE, SIGMA, THETA, X, Z, ZERO, dmmy
      integer ION, LION, M1, M2, M3, M4, NS1, NS2, NS3, NS4
C     !DASH
      external HOOP, DEPART, HI, BYE
C
      dimension CHI1(2), ELL1(2), GPR1(2), M1(2), ALF1(6), GAM1(6)
      dimension CHI2(1), ELL2(1), GPR2(1), M2(1), ALF2(4), GAM2(4)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1), ALF4(3), GAM4(3)
C
      data      LION /4/
C----  ION = 1 ----------------------------------------------
      data      G1   /1.D0/
      data      NS1  /2/
      data      CHI1 /5.21D0, 5.874D0/
      data      ELL1 /7.D0, 7.D0/
      data      GPR1 /4.D0, 2.D1/
      data      M1   /3, 3/
      data      ALF1 /2.46641D1, 1.8848D1, 2.084858D2,
     $                6.9369D1, 2.050097D2, 4.436212D2/
      data      GAM1 /1.236D0, 2.641D0, 4.639D0,
     $                2.939D0, 4.529D0, 5.461D0/
C----  ION = 2 ----------------------------------------------
      data      G2   /2.D0/
      data      NS2  /1/
      data      CHI2 /1.0001D1/
      data      ELL2 /1.7D1/
      data      GPR2 /2.D0/
      data      M2   /4/
      data      ALF2 /9.8234D0, 6.0628D0, 8.33893D1, 2.707233D2/
      data      GAM2 /6.54D-1, 2.523D0, 6.575D0, 9.413D0/
C----  ION = 3 ----------------------------------------------
      data      G3   /1.D0/
      data      NS3  /1/
      data      CHI3 /3.7001D1/
      data      ELL3 /2.8D0/
      data      GPR3 /1.2D1/
      data      M3   /0/
C----  ION = 4 ----------------------------------------------
      data      G4   /4.D0/
      data      NS4  /1/
      data      CHI4 /6.1975D1/
      data      ELL4 /3.4D0/
      data      GPR4 /1.2D1/
      data      M4   /3/
      data      ALF4 /2.0004D0, 6.64811D1, 4.55185D1/
      data      GAM4 /2.21D0, 1.9594D1, 2.4341D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTBA')
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
          call DEPART (G3,NS3,CHI3,ELL3,GPR3,M3,dmmy,dmmy,Z,THETA,H,
     $                 X,G0,SIGMA)
          goto 100
  104   continue
          call DEPART (G4,NS4,CHI4,ELL4,GPR4,M4,ALF4,GAM4,Z,THETA,H,
     $                 X,G0,SIGMA)
  100   continue
      end if
C     !END
      call BYE ('PARTBA')
C
      return
      end
