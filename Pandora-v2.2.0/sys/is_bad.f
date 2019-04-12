      subroutine IS_BAD
     $(VALUE,YES)

C
C     Rudolf Loeser, 2003 Feb 24
C---- Returns YES = .true. if VALUE = Inf or NaN.     
C     !DASH
C     Oct 31 2013 - SGK added is_XXX_r8 wrappers
      
      save
      
C     !DASH
     
      real*8 VALUE
      logical YES
      logical  is_nan_r8, is_inf_r8
      external is_nan_r8, is_inf_r8

C     !DASH
      
      YES = is_nan_r8( VALUE )
      if(.not. YES) then
        YES = is_inf_r8( VALUE )
      end if

C     !END
C
      return
      end
