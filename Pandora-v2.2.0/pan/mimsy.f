      subroutine MIMSY
     $(NO,MODE)
C
C     Rudolf Loeser, 1984 Feb 14
C---- Prints a heading.
C     (This is version 3 of MIMSY.)
C     !DASH
      save
C     !DASH
      integer MODE, NO
C     !DASH
      external LINER, HI, BYE
C
      call HI ('MIMSY')
C     !BEG
      if(NO.gt.0) then
        call LINER (1,NO)
        if(MODE.eq.1) then
          write (NO,100)
  100     format(' ','Rate (ergs/cm**3/sec)')
        else if(MODE.eq.2) then
          write (NO,101)
  101     format(' ','Integrated rate (ergs/cm**2/sec)')
        else
          write (NO,102) MODE
  102     format(' ','Huh?  MODE =',I10)
        end if
        call LINER (1,NO)
      end if
C     !END
      call BYE ('MIMSY')
C
      return
      end
