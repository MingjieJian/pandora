      subroutine COMPI
     $(I,J,INT,FLAG)
C     Rudolf Loeser, 1979 Mar 30
C---- See remarks in "COMPARE."
C     !DASH
      save
C     !DASH
      integer FLAG, I, INT, J, JIF, LESS, LIF, MORE, SAME, ZERO
C     !DASH
      intrinsic abs
C
      data ZERO /0/
      data LESS, SAME, MORE /-1, 0, +1/
C
C     !BEG
      JIF = I-J
      if(JIF.eq.ZERO) then
        FLAG = SAME
      else if(INT.gt.ZERO) then
        LIF = abs(JIF)-INT
        if(LIF.le.ZERO) then
          FLAG = SAME
        else if(JIF.lt.ZERO) then
          FLAG = LESS
        else
          FLAG = MORE
        end if
      else if(JIF.lt.ZERO) then
        FLAG = LESS
      else
        FLAG = MORE
      end if
C     !END
C
      return
      end
