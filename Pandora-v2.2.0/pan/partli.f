      subroutine PARTLI
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Lithium,
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
      real*8 ALF1, ALF2, ALF3, CHI1, CHI2, CHI3, CONST, ELL1, ELL2,
     $       ELL3, ERROR, G0, G1, G2, G3, G4, GAM1, GAM2, GAM3, GPR1,
     $       GPR2, GPR3, H, PE, SIGMA, THETA, X, Z, ZERO
      integer ION, LION, M1, M2, M3, NS1, NS2, NS3
C     !DASH
      external HOOP, DEPART, HI, BYE
C
      dimension CHI1(1), ELL1(1), GPR1(1), M1(1), ALF1(2), GAM1(2)
      dimension CHI2(1), ELL2(1), GPR2(1), M2(1), ALF2(2), GAM2(2)
      dimension CHI3(1), ELL3(1), GPR3(1), M3(1), ALF3(2), GAM3(2)
C
      data      LION /4/
C----  ION = 1  ---------------------------------------------
      data      G1   /2.D0/
      data      NS1  /1/
      data      CHI1 /5.39D0/
      data      ELL1 /6.D0/
      data      GPR1 /2.D0/
      data      M1   /2/
      data      ALF1 /8.4915D0, 9.75015D1/
      data      GAM1 /2.022D0, 4.604D0/
C----  ION = 2  ---------------------------------------------
      data      G2   /1.D0/
      data      NS2  /1/
      data      CHI2 /7.5619D1/
      data      ELL2 /6.D0/
      data      GPR2 /4.D0/
      data      M2   /2/
      data      ALF2 /2.33299D1, 1.926701D2/
      data      GAM2 /6.2032D1, 7.2624D1/
C----  ION = 3  ---------------------------------------------
      data      G3   /2.D0/
      data      NS3  /1/
      data      CHI3 /1.2242D2/
      data      ELL3 /8.D0/
      data      GPR3 /2.D0/
      data      M3   /2/
      data      ALF3 /1.60047D1, 2.479952D2/
      data      GAM3 /9.6149D1, 1.18333D2/
C----  ION = 4  ---------------------------------------------
      data      G4   /1.D0/
C------------------------------------------------------------
      data      ZERO, ERROR, CONST /0.D0, -1.D0, 1.D3/
C     !EJECT
C
      call HI ('PARTLI')
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
          G0 = G4
          SIGMA = ZERO
          X = CONST
  100   continue
      end if
C     !END
      call BYE ('PARTLI')
C
      return
      end
