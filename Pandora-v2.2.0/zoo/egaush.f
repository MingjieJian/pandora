      subroutine EGAUSH
     $(X,Y,KODE,VAL)
C     Rudolf Loeser, 1990 Dec 05
C---- Part of DRAYSON: computes Voigt function in Region III.
C     !DASH
      save
C     !DASH
      real*4 C, ELEVEN, HH, U, UU, V, VAL, VV, X, XX, Y, Y2
      integer KODE
C     !DASH
      dimension XX(3),HH(3)
C
      data XX / 5.246476E-1, 1.650680E+0, 7.071068E-1 /
      data HH / 2.562121E-1, 2.588268E-2, 2.820948E-1 /
      data C / 6.875E-1 /
      data ELEVEN / 1.1E1 /
C
C     !BEG
      Y2 = Y**2
      if((Y.lt.(ELEVEN-C*X)).or.(KODE.eq.1)) then
C----   Region IIIA: 4-point Gauss-Hermite quadrature
        U  = X-XX(1)
        V  = X+XX(1)
        UU = X-XX(2)
        VV = X+XX(2)
C
        VAL = Y*(HH(1)/(Y2+ U**2)+HH(1)/(Y2+ V**2)+
     $           HH(2)/(Y2+UU**2)+HH(2)/(Y2+VV**2))
      else
C----   Region IIIB: 2-point Gauss-Hermite quadrature
        U = X-XX(3)
        V = X+XX(3)
C
        VAL = Y*(HH(3)/(Y2+ U**2)+HH(3)/(Y2+ V**2))
      end if
C     !END
C
      return
      end
