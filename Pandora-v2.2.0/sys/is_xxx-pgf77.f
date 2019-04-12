      logical function is_inf_r8(value)
      real*8 value
      integer k
C     -SGK- isinff() is NOT documented in PGI's manuals ;-(
      k = isinff( VALUE )
      is_inf_r8 = (k .eq. 1)
      end

      logical function is_nan_r8(value)
      real*8 value
      integer k
C     -SGK- isnanf() is NOT documented in PGI's manuals ;-(
      k = isnanf( VALUE )
      is_nan_r8 = (k .eq. 1)
      end
