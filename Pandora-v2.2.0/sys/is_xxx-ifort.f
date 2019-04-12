      logical function is_inf_r8(value)
      real*8 value
      include 'for_fpclass.for'
      ires = fp_class(value)
      is_inf_r8  = .FALSE.
      if (ires.eq.FOR_K_FP_POS_INF .or.
     &    ires.eq.FOR_K_FP_NEG_INF) is_inf_r8 = .TRUE.
      end

      logical function is_nan_r8(value)
      real*8 value
      include 'for_fpclass.for'
      ires = fp_class(value)
      is_nan_r8 = .FALSE.
      if (ires.eq.FOR_K_FP_SNAN .or.
     &    ires.eq.FOR_K_FP_QNAN) is_nan_r8 = .TRUE.
      end
