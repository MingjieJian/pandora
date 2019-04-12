      subroutine PARTH
     $(ION,THETA,PE,X,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- Computes G0 and SIGMA for the
C     IONth stage of ionization of Hydrogen,
C     such that the Partition Function Q = G0+SIGMA.
C     (ION=1 for the neutral atom.)
C     Also returns the Excitation Potential X.
C     THETA is the reciprocal temperature, and PE is the
C     electron pressure in dynes/cm**2.
C     Returns G0=-1 if data for "ION" is unavailable.
C     (For reference, see listing of "DEPART".)
C     !DASH
      save
C     !DASH
      real*8 ALF1, CHI1, CONST, ELL1, ERROR, G0, G1, G2, GAM1, GPR1, H,
     $       HALF, PE, SIGMA, THETA, X, Z, ZERO
      integer ION, LION, M1, NS1
C     !DASH
      external HOOP, ALTA, DEPART, HI, BYE
C
      dimension CHI1(1), ELL1(1), GPR1(1), M1(1), ALF1(2), GAM1(2)
C
      data LION /2/
C
C----  ION = 1  ---------------------------------------------
      data G1   /2.D0/
      data NS1  /1/
      data CHI1 /1.3595D1/
      data ELL1 /1.1D1/
      data GPR1 /2.D0/
      data M1   /2/
      data ALF1 /2.04976D1,  7.475023D2/
      data GAM1 /1.0853D1, 1.3342D1/
C
C----  ION = 2  ---------------------------------------------
      data G2   /1.D0/
C------------------------------------------------------------
      data ERROR, CONST /-1.D0, 1.D3/
      data ZERO, HALF   /0.D0, 0.5D0/
C     !EJECT
C
      call HI ('PARTH')
C     !BEG
      if((ION.lt.1).or.(ION.gt.LION)) then
        G0 = ERROR
        SIGMA = ZERO
C
      else
C
        Z = ION
        call HOOP       (Z,THETA,PE,H)
        goto (101,102), ION
C
  101   continue
          if(H.le.(ELL1(1)+HALF)) then
            call ALTA   (G1,THETA,CHI1(1),H,G0,SIGMA)
            X = CHI1(1)
          else
            call DEPART (G1,NS1,CHI1,ELL1,GPR1,M1,ALF1,GAM1,Z,THETA,
     $                   H,X,G0,SIGMA)
          end if
          goto 100
C
  102   continue
          X = CONST
          G0 = G2
          SIGMA = ZERO
C
  100   continue
C
      end if
C     !END
      call BYE ('PARTH')
C
      return
      end
