      subroutine LRATIO
     $(A,B,RATL)
C
C     Rudolf Loeser, 1984 Jan 10
C---- Computes log(A/B).
C     !DASH
      save
C     !DASH
      real*8 A, B, R, RATL, ZERO
C     !DASH
      external DIVVY
C
      data ZERO /0.D0/
C
C     !BEG
      call DIVVY    (A,B,R)
      if(R.gt.ZERO) then
        RATL = log10(R)
      else
        RATL = ZERO
      end if
C     !END
C
      return
      end
