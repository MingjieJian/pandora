      subroutine RAKNTS
     $(FILDAT,XNRW,XNBW,XNRR,XNBR)
C     Rudolf Loeser, 1987 Aug 06
C---- Returns current values of I/O counts; see remarks in RAFAEL.
C     !DASH
      save
C     !DASH
      real*8 FILDAT, XNBR, XNBW, XNRR, XNRW
C     !DASH
      dimension FILDAT(11)
C
C     !BEG
      XNRW = FILDAT( 6)+FILDAT(8)
      XNBW = FILDAT( 7)+FILDAT(9)
      XNRR = FILDAT(10)
      XNBR = FILDAT(11)
C     !END
C
      return
      end
