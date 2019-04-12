      subroutine PARTP
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Phophorus,
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
      dimension CHI1(2), ELL1(2), GPR1(2), M1(2), ALF1(5), GAM1(5)
      dimension CHI2(1), ELL2(1), GPR2(1), M2(1), ALF2(4), GAM2(4)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(4), GAM3(4)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1), ALF4(3), GAM4(3)
      dimension CHI5(1), ELL5(1), GPR5(1), M5(1), ALF5(3), GAM5(3)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /4.D0/
      data      NS1  /2/
      data      CHI1 /1.0474D1, 1.1585D1/
      data      ELL1 /5.D0, 5.D0/
      data      GPR1 /1.8D1, 1.D1/
      data      M1   /3, 2/
      data      ALF1 /1.35211D1, 2.2213D1, 3.532583D2,
     $                1.D1, 1.5D2/
      data      GAM1 /1.514D0, 5.575D0, 9.247D0,
     $                8.076D0, 1.0735D1/
C----  ION = 2 ----------------------------------------------
      data      G2   /1.D0/
      data      NS2  /1/
      data      CHI2 /1.972D1/
      data      ELL2 /5.D0/
      data      GPR2 /1.2D1/
      data      M2   /4/
      data      ALF2 /8.0241D0, 5.8085D0, 5.17542D1, 2.524002D2/
      data      GAM2 /4.3D-2, 1.212D0, 8.545D0, 1.5525D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /2.D0/
      data      NS3  /1/
      data      CHI3 /3.0156D1/
      data      ELL3 /7.D0/
      data      GPR3 /2.D0/
      data      M3   /4/
      data      ALF3 /4.0021D0, 2.07985D1, 6.24194D1, 2.007786D2/
      data      GAM3 /7.4D-2, 7.674D0, 1.6639D1, 2.5118D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /1.D0/
      data      NS4  /1/
      data      CHI4 /5.1354D1/
      data      ELL4 /8.6D0/
      data      GPR4 /4.D0/
      data      M4   /3/
      data      ALF4 /1.17414D1, 6.35124D1, 1.79742D2/
      data      GAM4 /8.992D0, 2.4473D1, 4.0704D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /2.D0/
      data      NS5  /1/
      data      CHI5 /6.5007D1/
      data      ELL5 /8.D0/
      data      GPR5 /2.D0/
      data      M5   /3/
      data      ALF5 /6.8835D0, 3.27777D1, 2.283366D2/
      data      GAM5 /1.1464D1, 3.3732D1, 5.5455D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTP')
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
      call BYE ('PARTP')
C
      return
      end
