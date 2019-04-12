      subroutine SECOND
     $(TIME)
C     Rudolf Loeser, 1996 Jun 03
C---- Returns the current process elapsed time,
C     in seconds.
C     !DASH
      save
C     !DASH
      real*8 TIME
      real*4 T, TARRAY
C     !DASH
      external etime
C
      dimension TARRAY(2)
C
C     !BEG
      T    = etime (TARRAY)
      TIME = TARRAY(1)
C     !END
C
      return
      end
