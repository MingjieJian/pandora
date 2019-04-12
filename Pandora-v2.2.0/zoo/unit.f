      subroutine UNIT
     $(I,J,DELTA)
C     Rudolf Loeser, 1981 Feb 23
C---- Returns the (I,J)'th element of a Unit matrix.
C     !DASH
      save
C     !DASH
      real*8 DELTA, ONE, ZERO
      integer I, J
C     !DASH
      data ZERO, ONE /0.D0, 1.D0/
C
C     !BEG
      if(I.eq.J) then
        DELTA = ONE
      else
        DELTA = ZERO
      end if
C     !END
C
      return
      end
