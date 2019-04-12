      subroutine LILA
     $(NO,JBFSW,ITS)
C
C     Rudolf Loeser, 1984 Feb 13
C---- Prints heading, for INKY.
C     (This is version 3 of LILA.)
C     !DASH
      save
C     !DASH
      integer ITS, JBFSW, NO
C     !DASH
      external ABJECT, HI, BYE
C
      call HI ('LILA')
C     !BEG
      if(NO.gt.0) then
        call ABJECT (NO)
        if(JBFSW.eq.1) then
          write (NO,100)
  100     format(' ','Details of BD calculation')
        else if(JBFSW.eq.2) then
          write (NO,101) ITS
  101     format(' ','Details of iterative BD calculation',77X,I4,
     $               ' iterations')
        else
          write (NO,102) JBFSW
  102     format(' ','ERROR: LILA cannot select proper heading ',
     $               'since JBFSW =',I10)
        end if
      end if
C     !END
      call BYE ('LILA')
C
      return
      end
