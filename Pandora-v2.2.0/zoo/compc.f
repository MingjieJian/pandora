      subroutine COMPC
     $(P,Q,FLAG)
C     Rudolf Loeser, 1979 Mar 30
C---- See remarks in "COMPARE."
C     !DASH
      save
C     !DASH
      integer AFTER, BEFORE, FLAG, SAME
      character P*(*), Q*(*)
C     !DASH
      data BEFORE, SAME, AFTER /-1, 0, +1/
C
C     !BEG
      if(P.eq.Q) then
        FLAG = SAME
      else if(P.gt.Q) then
        FLAG = AFTER
      else
        FLAG = BEFORE
      end if
C     !END
C
      return
      end
