      subroutine BESSEL
     $(X,BK0,BK1)
C
C     Rudolf Loeser, 1991 Jan 02
C---- Computes the modified Bessel functions K0(x) and K1(x),
C     for positive real x.
C
C     (Returns values = -2 for x out-of-range.)
C
C     Based on the routines in
C     P r e s s   e t   a l .
C     "Numerical Recipes" (1986), pp. 176-179;
C
C     these routines use polynomial approximations given by
C     A b r a m o w i t z   &   S t e g u n
C     "Handbook of Mathematical Functions" (1964).
C     !DASH
      save
C     !DASH
      real*8 A1, A2, A3, A4, A5, A6, A7, B1, B2, B3, B4, B5, B6, B7,
     $       BI0, BI1, BK0, BK1, C1, C2, C3, C4, C5, C6, C7, CRITI,
     $       CRITK, D1, D2, D3, D4, D5, D6, D7, E1, E2, E3, E4, E5, E6,
     $       E7, F1, F2, F3, F4, F5, F6, F7, R, X, XE, XR, Y, YL, Z,
     $       ZERO
C     !DASH
      data     A1,A2,A3,A4,A5,A6,A7 /
     $         1.00000000D+0,  3.51562290D+0,  3.08994240D+0,
     $         1.20674920D+0,  2.65973200D-1,  3.60768000D-2,
     $         4.58130000D-3 /
      data     B1,B2,B3,B4,B5,B6,B7 /
     $         5.00000000D-1,  8.78905940D-1,  5.14988690D-1,
     $         1.50849340D-1,  2.65873300D-2,  3.01532000D-3,
     $         3.24110000D-4 /
      data     C1,C2,C3,C4,C5,C6,C7 /
     $        -5.77215660D-1,  4.22784200D-1,  2.30697560D-1,
     $         3.48859000D-2,  2.62698000D-3,  1.07500000D-4,
     $         7.40000000D-6 /
      data     D1,D2,D3,D4,D5,D6,D7 /
     $         1.00000000D+0,  1.54431440D-1, -6.72785790D-1,
     $        -1.81568970D-1, -1.91940200D-2, -1.10404000D-3,
     $        -4.68600000D-5 /
      data     E1,E2,E3,E4,E5,E6,E7 /
     $         1.25331414D+0, -7.83235800D-2,  2.18956800D-2,
     $        -1.06244600D-2,  5.87872000D-3, -2.51540000D-3,
     $         5.32080000D-4 /
      data     F1,F2,F3,F4,F5,F6,F7 /
     $         1.25331414D+0,  2.34986190D-1, -3.65562000D-2,
     $         1.50426800D-2, -7.80353000D-3,  3.25614000D-3,
     $        -6.82450000D-4 /
      data     CRITI, CRITK /3.75D0, 2.D0/
      data     ZERO /0.D0/
C     !EJECT
C
C     !BEG
      if(X.le.ZERO) then
        BK0 = -CRITK
        BK1 = -CRITK
      else if(X.le.CRITK) then
C
        Z = (X/CRITI)**2
C----   Compute I0(x) and I1(x), x .le. 3.75
        BI0 =   (A1+Z*(A2+Z*(A3+Z*(A4+Z*(A5+Z*(A6+Z*A7))))))
        BI1 = X*(B1+Z*(B2+Z*(B3+Z*(B4+Z*(B5+Z*(B6+Z*B7))))))
C
        Y  = (X/CRITK)
        YL = log(Y)
        Y  = Y**2
C----   Compute K0(x) and K1(x), x .le. 2
        BK0 = (-YL*BI0)+(C1+Y*(C2+Y*(C3+Y*(C4+Y*(C5+Y*(C6+Y*C7))))))
        BK1 = ( YL*BI1)+(D1+Y*(D2+Y*(D3+Y*(D4+Y*(D5+Y*(D6+Y*D7))))))/X
      else
C
        XE = exp(-X)
        XR = sqrt(X)
        R  = (XE/XR)
        Y  = (CRITK/X)
C----   Compute K0(x) and K1(x), x .gt. 2
        BK0 = R*(E1+Y*(E2+Y*(E3+Y*(E4+Y*(E5+Y*(E6+Y*E7))))))
        BK1 = R*(F1+Y*(F2+Y*(F3+Y*(F4+Y*(F5+Y*(F6+Y*F7))))))
      end if
C     !END
C
      return
      end
