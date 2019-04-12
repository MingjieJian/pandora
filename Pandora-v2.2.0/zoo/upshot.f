      subroutine UPSHOT
     $(SCALE,FAC,BIG,BOUND)
C     Rudolf Loeser, 1983 Dec 21
C---- Updates scale factors, for "FISCAL".
C     !DASH
      save
C     !DASH
      real*8 BGL, BIG, BNL, BOUND, FAC, FCL, RUL, SCALE, SCL, TEN, ZERO
C     !DASH
      intrinsic abs
      data ZERO,TEN /0.D0, 1.D1/
C
C     !BEG
      BNL = log(abs(BOUND))
      BGL = log(abs(BIG))
      FCL = log(FAC)
      SCL = log(SCALE)
      if((SCL+FCL).gt.BNL) then
        SCALE = abs(BOUND)/TEN
      else
        SCALE = SCALE*FAC
        SCL = SCL+FCL
        RUL = SCL+(BGL-BNL)
        if(RUL.gt.ZERO) then
          SCALE = SCALE/(exp(RUL))
        end if
      end if
C     !END
C
      return
      end
