      subroutine SHIM
     $(I,J,NO)
C     Rudolf Loeser, 1980 Aug 22
C---- If "I" is an exact multiple of "J",
C     then SHIM will write a blank line
C     in the file attached to unit "NO".
C     !DASH
      save
C     !DASH
      integer I, J, NO
C     !DASH
      external  LINER
      intrinsic mod
C
C     !BEG
      if(NO.gt.0) then
        if(mod(I,J).eq.0) then
          call LINER (1,NO)
        end if
      end if
C     !END
C
      return
      end
