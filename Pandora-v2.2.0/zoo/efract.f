      subroutine EFRACT
     $(X,Y,VAL)
C     Rudolf Loeser, 1990 Dec 05
C---- Part of DRAYSON: computes Voigt function in Region II.
C     !DASH
      save
C     !DASH
      real*4 CRIT, ELEVEN, NBY2, ROOTPI, T1, T2, TWO, U, UU, VAL, VV, X,
     $       XN, Y, YN
      integer I, J, MN, MX
C     !DASH
      intrinsic min
C
      dimension XN(15),YN(15),NBY2(19)
C
      data XN / 1.E1, 9.E0, 8.E0, 8.E0, 7.E0, 6.E0, 5.E0,
     $          4.E0, 3.E0, 3.E0, 3.E0, 3.E0, 3.E0, 3.E0,
     $          3.E0 /
      data YN / 6.E-1, 6.E-1, 6.E-1, 5.E-1, 4.E-1, 4.E-1,
     $          3.E-1, 3.E-1, 3.E-1, 3.E-1, 1.E+0, 9.E-1,
     $          8.E-1, 7.E-1, 7.E-1 /
      data NBY2 / 9.5E0, 9.E0, 8.5E0, 8.E0, 7.5E0, 7.E0,
     $            6.5E0, 6.E0, 5.5E0, 5.E0, 4.5E0, 4.E0,
     $            3.5E0, 3.E0, 2.5E0, 2.E0, 1.5E0, 1.E0,
     $            5.E-1 /
      data TWO, ELEVEN / 2.E0, 1.1E1 /
      data ROOTPI / 1.7724539E0 /
      data CRIT / 1.45E0 /
      data T1, T2 / 1.85E0, 4.6E-1 /
C
C     !BEG
C---- Compute number of terms needed in continued fraction
      if(Y.ge.CRIT) then
        I = TWO*Y
      else
        I = ELEVEN*Y
      end if
      J  = TWO*X+T1
      MX = XN(J)*YN(I)+T2
      MN = min(16,(21-2*MX))
C---- Evaluate continued fraction
      UU = Y
      VV = X
      do 100 J = MN,19
        U  = NBY2(J)/(UU**2+VV**2)
        UU = Y+U*UU
        VV = X-U*VV
  100 continue
C
      VAL = (UU/(UU**2+VV**2))/ROOTPI
C     !END
C
      return
      end
