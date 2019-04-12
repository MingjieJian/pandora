      subroutine LACK
     $(UNIT,ERROUT)
C
C     Rudolf Loeser, 1987 Jul 15
C---- 'Close' a permanent file.
C     In case of error, writes a message on ERROUT and stops.
C     !DASH
      save
C     !DASH
      integer ERROUT, IOSTAT, UNIT
C     !DASH
      external IOFAULT
C
C     !BEG
      close (unit=UNIT, status='KEEP', iostat=IOSTAT)
C
      if(IOSTAT.ne.0) then
        call IOFAULT (ERROUT, 'LACK', 'Close/Keep', UNIT, IOSTAT)
        stop
      end if
C     !END
C
      return
      end
