      subroutine PARTO
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Oxygen,
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
      dimension CHI1(3), ELL1(3), GPR1(3), M1(3), ALF1(8), GAM1(8)
      dimension CHI2(4), ELL2(4), GPR2(4), M2(4), ALF2(12), GAM2(12)
      dimension CHI3(3), ELL3(3), GPR3(3), M3(3), ALF3(10), GAM3(10)
      dimension CHI4(5), ELL4(5), GPR4(5), M4(5), ALF4(13), GAM4(13)
      dimension CHI5(2), ELL5(2), GPR5(2), M5(2), ALF5(6), GAM5(6)
C
      data      LION /5/
C----  ION = 1 ----------------------------------------------
      data      G1   /5.D0/
      data      NS1  /3/
      data      CHI1 /1.3614D1, 1.6938D1, 1.863D1/
      data      ELL1 /8.D0, 6.D0, 3.4D0/
      data      GPR1 /8.D0, 2.D1, 1.2D1/
      data      M1   /4, 2, 2/
      data      ALF1 /4.0029D0, 5.3656D0, 3.62853D1, 1.0443447D3,
     $                1.310217D2, 8.689779D2,
     $                1.48533D1, 9.31466D1/
      data      GAM1 /2.2D-2, 2.019D0, 9.812D0, 1.3087D1,
     $                1.3804D1, 1.6061D1,
     $                1.4293D1, 1.6114D1/
C----  ION = 2 ----------------------------------------------
      data      G2   /4.D0/
      data      NS2  /4/
      data      CHI2 /3.5108D1, 3.7621D1, 4.0461D1, 4.2584D1/
      data      ELL2 /6.D0, 5.D0, 3.9D0, 3.9D0/
      data      GPR2 /1.8D1, 1.D1, 2.D0, 1.D1/
      data      M2   /4, 2, 3, 3/
      data      ALF2 /1.27843D1, 5.6828D0, 9.80919D1, 8.294396D2,
     $                5.09878D1, 1.99012D2,
     $                2.D0, 6.D0, 1.D1,
     $                1.D1, 3.D1, 5.D1/
      data      GAM2 /3.472D0, 7.437D0, 2.2579D1, 3.2035D1,
     $                2.7774D1, 3.3678D1,
     $                2.8118D1, 3.1019D1, 3.4204D1,
     $                3.0892D1, 3.3189D1, 3.6181D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /1.D0/
      data      NS3  /3/
      data      CHI3 /5.4868D1, 6.3733D1, 7.0556D1/
      data      ELL3 /6.D0, 4.9D0, 4.D0/
      data      GPR3 /1.2D1, 2.4D1, 2.D1/
      data      M3   /4, 4, 2/
      data      ALF3 /8.0703D0, 5.7144D0, 8.41156D1, 5.290927D2,
     $                5.6609D0, 2.89355D1, 1.11362D2, 4.940413D2,
     $                4.55249D1, 1.344751D2/
      data      GAM3 /3.2D-2, 2.76D0, 3.5328D1, 4.8277D1,
     $                7.662D0, 1.6786D1, 4.2657D1, 5.4522D1,
     $                5.0204D1, 5.6044D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /2.D0/
      data      NS4  /5/
      data      CHI4 /7.7394D1, 8.7609D1, 9.7077D1, 1.03911D2,
     $                          1.06116D2/
      data      ELL4 /5.9D0, 5.D0, 4.9D0, 4.D0,
     $                       4.D0/
      data      GPR4 /2.D0, 1.8D1, 6.D0, 1.8D1,
     $                      1.D1/
      data      M4   /3, 4, 2, 2, 2/
      data      ALF4 /4.0003D0, 2.12937D1, 7.87058D1,
     $                1.28293D1, 1.6273D1, 1.236578D2, 3.272396D2,
     $                4.87883D1, 1.022117D2,
     $                2.0006D1, 1.619903D2,
     $                2.84184D1, 6.15816D1/
      data      GAM4 /4.8D-2, 5.0089D1, 6.6604D1,
     $                8.954D0, 1.8031D1, 5.7755D1, 7.2594D1,
     $                6.8388D1, 8.2397D1,
     $                3.196D1, 7.6876D1,
     $                7.5686D1, 8.0388D1/
C----  ION = 5 ----------------------------------------------
      data      G5   /1.D0/
      data      NS5  /2/
      data      CHI5 /1.13873D2, 1.25863D2/
      data      ELL5 /6.D0, 6.D0/
      data      GPR5 /4.D0, 1.2D1/
      data      M5   /3, 3/
      data      ALF5 /1.05563D1, 1.3295D1, 1.88139D2,
     $                1.4656D1, 1.294922D2, 4.708512D2/
      data      GAM5 /1.0747D1, 5.2323D1, 9.4976D1,
     $                2.7405D1, 8.635D1, 1.09917D2/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTO')
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
      call BYE ('PARTO')
C
      return
      end
