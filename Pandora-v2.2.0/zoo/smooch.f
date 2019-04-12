      subroutine SMOOCH
     $(P1,P2,P3,P4,P5,P3P)
C     Rudolf Loeser, 1985 Apr 02
C---- Smoothes P3, producing P3P.
C     !DASH
      save
C     !DASH
      real*8 A12, A24, A45, B12, B24, B45, D, FIVE, FOUR, ONE, P1, P2,
     $       P3, P3P, P4, P5, PY, PZ, THREE, TWO, X, Y3, Z3, ZERO
C     !DASH
      intrinsic abs,sign
C
      data ZERO,ONE,TWO    /0.D0, 1.D0, 2.D0/
      data THREE,FOUR,FIVE /3.D0, 4.D0, 5.D0/
C
C     !BEG
      P3P = P3
C---- Coefficients for equation of line L24: P2 to P4
      A24 = (P4-P2)/TWO
      B24 = (FOUR*P2-TWO*P4)/TWO
C---- Intersection of X=3 and L24
      Y3  = THREE*A24+B24
C---- Coefficients for equation of line L12: P1 to P2
      A12 = P2-P1
      B12 = TWO*P1-P2
C---- Coefficients for equation of line L45: P4 to P5
      A45 = P5-P4
      B45 = FIVE*P4-FOUR*P5
C     !EJECT
C---- Do L12 and L45 intersect within the interval (X=2,X=4)?
      D = A45-A12
      if(D.ne.ZERO) then
        X = (B12-B45)/D
        if((X.gt.TWO).and.(X.lt.FOUR)) then
C----     Yes - find other relevant point
          if(X.lt.THREE) then
            Z3 = THREE*A45+B45
          else
            Z3 = THREE*A12+B12
          end if
C----     Does P3 lie outside the triangle?
C         (i.e. does P3 not fall between Y3 and Z3)
          PY = P3-Y3
          PZ = P3-Z3
          if((sign(ONE,PY)+sign(ONE,PZ)).ne.ZERO) then
C----       Yes - choose appropriate point
            if(abs(PY).lt.abs(PZ)) then
              P3P = Y3
            else
              P3P = Z3
            end if
          end if
        else
C----     No - use Y3
          P3P = Y3
        end if
      else
C----   Special case: L12 and L45 have the same slope
        P3P = (P2+P4)/TWO
      end if
C     !END
C
      return
      end
