      subroutine PARTHE
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Helium,
C     such that the Partition Function Q = G0+SIGMA.
C     (ION=1 for the neutral atom.)
C     Also returns the Ionization Potential X.
C     THETA is the reciprocal temperature, and PE is the
C     electron pressure in dynes/cm**2.
C     Returns G0=-1 if data for "ION" is unavailable.
C     (For reference, see listing of "DEPART".)
C     !DASH
      save
C     !DASH
      real*8 ALF1, ALF2, CHI1, CHI2, CONST, ELL1, ELL2, ERROR, G0, G1,
     $       G2, G3, GAM1, GAM2, GPR1, GPR2, H, HALF, PE, SIGMA, THETA,
     $       X, Z, ZERO
      integer ION, LION, M1, M2, NS1, NS2
C     !DASH
      external HOOP, ALTA, DEPART, HI, BYE
C
      dimension CHI1(1), ELL1(1), GPR1(1), M1(1), ALF1(2), GAM1(2)
      dimension CHI2(1), ELL2(1), GPR2(1), M2(1), ALF2(2), GAM2(2)
C
      data      LION /3/
C----  ION = 1  ---------------------------------------------
      data      G1   /1.D0/
      data      NS1  /1/
      data      CHI1 /2.458D1/
      data      ELL1 /8.D0/
      data      GPR1 /4.D0/
      data      M1   /2/
      data      ALF1 /2.81703D1, 5.278296D2/
      data      GAM1 /2.117D1, 2.4125D1/
C----  ION = 2  ---------------------------------------------
      data      G2   /2.D0/
      data      NS2  /1/
      data      CHI2 /5.4403D1/
      data      ELL2 /1.2D1/
      data      GPR2 /2.D0/
      data      M2   /2/
      data      ALF2 /2.22809D1, 9.877189D2/
      data      GAM2 /4.3708D1, 5.3542D1/
C----  ION = 3  ---------------------------------------------
      data      G3   /1.D0/
C------------------------------------------------------------
      data      ERROR, CONST /-1.D0, 1.D3/
      data      ZERO, HALF   /0.D0, 0.5D0/
C     !EJECT
C
      call HI ('PARTHE')
C     !BEG
      if((ION.lt.1).or.(ION.gt.LION)) then
        G0 = ERROR
        SIGMA = ZERO
      else
        Z = ION
        call HOOP       (Z,THETA,PE,H)
        goto (101,102,103), ION
C
  101   continue
          call DEPART   (G1,NS1,CHI1,ELL1,GPR1,M1,ALF1,GAM1,Z,THETA,
     $                   H,X,G0,SIGMA)
          goto 100
  102   continue
          if(H.le.(ELL2(1)+HALF)) then
            call ALTA   (G2,THETA,CHI2(1),H,G0,SIGMA)
            X = CHI2(1)
          else
            call DEPART (G2,NS2,CHI2,ELL2,GPR2,M2,ALF2,GAM2,Z,THETA,
     $                   H,X,G0,SIGMA)
          end if
          goto 100
  103   continue
          G0 = G3
          SIGMA = ZERO
          X = CONST
  100   continue
      end if
C     !END
      call BYE ('PARTHE')
C
      return
      end
