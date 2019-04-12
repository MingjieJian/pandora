      subroutine MARES
     $(NO,IC)
C
C     Rudolf Loeser, 2003 Apr 23
C---- Prints a message for FRAME.
C     !DASH
      save
C     !DASH
      integer IC, NO
C     !DASH
      external ABJECT, LINER, HI, BYE
C
      call HI ('MARES')
C     !BEG
      if(NO.gt.0) then
        call ABJECT (NO)
        call LINER  (5,NO)
        write (NO,100) IC
  100   format(' ','Graph of CHECK ',I2,' is not interesting.')
      end if
C     !END
      call BYE ('MARES')
C
      return
      end
