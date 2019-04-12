      subroutine COMPR
     $(A,B,DELTA,FLAG)
C     Rudolf Loeser, 1979 Mar 30
C---- See remarks in "COMPARE."
C     !DASH
      save
C     !DASH
      real*4 A, B, DELTA, DIF, HALF, RIF, ZERO
      integer FLAG, LESS, MORE, SAME
C     !DASH
      intrinsic abs
      data ZERO, HALF /0.E+0, 5.E-1/
      data LESS, SAME, MORE /-1, 0, +1/
C
C     !BEG
      DIF = A-B
      if(DIF.eq.ZERO) then
        FLAG = SAME
      else if(DELTA.gt.ZERO) then
        RIF = abs(DIF)-HALF*DELTA*(abs(A)+abs(B))
        if(RIF.le.ZERO) then
          FLAG = SAME
        else if(DIF.lt.ZERO) then
          FLAG = LESS
        else
          FLAG = MORE
        end if
      else if(DIF.lt.ZERO) then
        FLAG = LESS
      else
        FLAG = MORE
      end if
C     !END
C
      return
      end
