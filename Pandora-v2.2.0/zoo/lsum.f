      subroutine LSUM
     $(D,SUM)
C     Rudolf Loeser, 1981 Jul 27
C---- Computes the series expansion
C     SUM = D**0/2 - D**1/3 + D**2/4 - D**3/5 + ...
C     !DASH
      save
C     !DASH
      real*8 D, HALF, ONE, POW, SEQ, STOP, SUM, TRM, TWO
C     !DASH
      intrinsic abs
C
      data STOP /1.D-20/
      data HALF,ONE,TWO /.5D0, 1.D0, 2.D0/
C
C     !BEG
      SUM = HALF
      SEQ = TWO
      POW = ONE
  100 continue
        SEQ = SEQ+ONE
        POW =  -D*POW
        TRM = POW/SEQ
        SUM = SUM+TRM
      if(abs(TRM).gt.STOP) goto 100
C     !END
C
      return
      end
