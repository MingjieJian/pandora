      subroutine ALCMAN
     $(TIN,NO)
C
C     Rudolf Loeser, 1985 Aug 20
C---- Prints PRD signoff message.
C     (This is version 2 of ALCMAN.)
C     !DASH
      save
C     !DASH
      real*8 TIME, TIN, TOUT
      integer NO
C     !DASH
      external SECOND, LINER, HI, BYE
C
      call HI ('ALCMAN')
C     !BEG
      if(NO.gt.0) then
        call SECOND (TOUT)
        TIME = TOUT-TIN
C
        call LINER  (2,NO)
        write (NO,100) TIME
  100   format(' ','Time used to compute PRD terms:',F8.1,' sec.')
      end if
C     !END
      call BYE ('ALCMAN')
C
      return
      end
