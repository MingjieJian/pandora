c
c     this is a modern version, using IEEE_ARITHMETIC
c     ifort or pgfortran will compile this right, NOT pgf77 (== strict f77)
c
c     Nov  4 2013 - SGK
c
      logical function is_inf_r8(value)
      USE, INTRINSIC :: IEEE_ARITHMETIC
      real*8 value
      is_inf_r8 = .not. ieee_is_finite( VALUE )
      end

      logical function is_nan_r8(value)
      USE, INTRINSIC :: IEEE_ARITHMETIC
      real*8 value
      is_nan_r8 = ieee_is_nan( VALUE )
      end
