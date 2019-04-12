      subroutine MARBU
     $(XN1O,XN1N,DELTA)
C
C     Rudolf Loeser, 1998 Mar 31
C---- Sets up DELTA for printing, for RUMBA.
C     !DASH
      save
C     !DASH
      real*8 DELTA, DLIM, XN1N, XN1O
C     !DASH
      external  RELDIFF, HI, BYE
      intrinsic abs, sign
C
      data DLIM /9.99999999/
C
      call HI ('MARBU')
C     !BEG
      call RELDIFF (XN1O,XN1N,DELTA)
      if(abs(DELTA).gt.DLIM) then
        DELTA = sign(DLIM,DELTA)
      end if
C     !END
      call BYE ('MARBU')
C
      return
      end
