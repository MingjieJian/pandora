      subroutine BELVOIR
     $(NO,LINE,LEN,ZXL,ZXH)
C
C     Rudolf Loeser, 1994 Jul 05
C---- Prints an abscissa label for a PRPLOT plot.
C     (See subroutine KPRINT.)
C     !DASH
      save
C     !DASH
      real*8 ZXH, ZXL
      integer JHB, LEN, NFH, NO, jummy
      character LINE*(*), QXH*16, QXL*16
C     !DASH
      external ENCODED, HI, BYE
C
      call HI ('BELVOIR')
C     !BEG
      if(NO.gt.0) then
        call ENCODED (ZXL,QXL,16,6,1,jummy)
        call ENCODED (ZXH,QXH,16,6,1,NFH  )
C
        LINE = QXL
        JHB  = 17-NFH
        LINE((LEN+1)-NFH:) = QXH(JHB:)
C
        write (NO,100) LINE(:LEN)
  100   format(' ',10X,A)
      end if
C     !END
      call BYE ('BELVOIR')
C
      return
      end
