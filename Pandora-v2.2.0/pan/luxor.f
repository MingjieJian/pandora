      subroutine LUXOR
     $(NO)
C
C     Rudolf Loeser, 1979 Oct 30
C---- Prints heading, for OSIRIS.
C     !DASH
      save
C     !DASH
      integer NO
C     !DASH
      external PRIAM, LINER, HI, BYE
C
      call HI ('LUXOR')
C     !BEG
      if(NO.gt.0) then
        call PRIAM (NO, 'H MINUS', 7)
        call LINER (3, NO)
C
        write (NO,100)
  100   format(' ','H- departure coefficient calculation')
      end if
C     !END
      call BYE ('LUXOR')
C
      return
      end
