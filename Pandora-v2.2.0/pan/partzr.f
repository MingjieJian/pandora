      subroutine PARTZR
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Zirconium,
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
      real*8 ALF1, ALF2, ALF3, ALF4, CHI1, CHI2, CHI3, CHI4, ELL1, ELL2,
     $       ELL3, ELL4, ERROR, G0, G1, G2, G3, G4, GAM1, GAM2, GAM3,
     $       GAM4, GPR1, GPR2, GPR3, GPR4, H, PE, SIGMA, THETA, X, Z,
     $       ZERO
      integer ION, LION, M1, M2, M3, M4, NS1, NS2, NS3, NS4
C     !DASH
      external HOOP, DEPART, HI, BYE
C
      dimension CHI1(1), ELL1(1), GPR1(1), M1(1), ALF1(5), GAM1(5)
      dimension CHI2(2), ELL2(2), GPR2(2), M2(2), ALF2(8), GAM2(8)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(5), GAM3(5)
      dimension CHI4(1), ELL4(1), GPR4(1), M4(1), ALF4(4), GAM4(4)
C
      data      LION /4/
C----  ION = 1 ----------------------------------------------
      data      G1   /5.D0/
      data      NS1  /1/
      data      CHI1 /6.835D0/
      data      ELL1 /6.D0/
      data      GPR1 /5.6D1/
      data      M1   /5/
      data      ALF1 /1.16004D1, 2.45594D1, 1.11533D2, 5.708658D2,
     $                           1.1854027D3/
      data      GAM1 /8.4D-2, 4.01D-1, 1.17D0, 2.864D0,
     $                        5.852D0/
C----  ION = 2 ----------------------------------------------
      data      G2   /4.D0/
      data      NS2  /2/
      data      CHI2 /1.4028D1, 1.5398D1/
      data      ELL2 /6.D0, 6.D0/
      data      GPR2 /4.2D1, 1.8D1/
      data      M2   /4, 4/
      data      ALF2 /1.77876D1, 5.36729D1, 1.485349D2, 8.756878D2,
     $                2.68482D1, 1.013551D2, 1.552734D2, 3.164934D2/
      data      GAM2 /6.8D-2, 4.48D-1, 3.2D0, 1.1169D1,
     $                6.87D-1, 1.728D0, 5.735D0, 1.1943D1/
C----  ION = 3 ----------------------------------------------
      data      G3   /5.D0/
      data      NS3  /1/
      data      CHI3 /2.298D1/
      data      ELL3 /6.D0/
      data      GPR3 /2.D1/
      data      M3   /5/
      data      ALF3 /1.17757D1, 9.511D0, 3.47245D1, 1.009139D2,
     $                           4.770311D2/
      data      GAM3 /1.02D-1, 4.01D-1, 1.655D0, 7.737D0,
     $                         1.7096D1/
C----  ION = 4 ----------------------------------------------
      data      G4   /4.D0/
      data      NS4  /1/
      data      CHI4 /3.433D1/
      data      ELL4 /5.7D0/
      data      GPR4 /2.D0/
      data      M4   /4/
      data      ALF4 /6.0011D0, 2.3714D0, 8.3077D0, 2.53194D1/
      data      GAM4 /1.55D-1, 4.962D0, 1.1822D1, 2.0015D1/
C------------------------------------------------------------
      data      ZERO, ERROR /0.D0, -1.D0/
C     !EJECT
C
      call HI ('PARTZR')
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
          call DEPART (G4,NS4,CHI4,ELL4,GPR4,M4,ALF4,GAM4,Z,THETA,H,
     $                 X,G0,SIGMA)
  100   continue
      end if
C     !END
      call BYE ('PARTZR')
C
      return
      end
