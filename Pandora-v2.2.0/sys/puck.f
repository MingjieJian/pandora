      subroutine PUCK
     $(UNIT,ERROUT)
C
C     Rudolf Loeser, 1987 Jul 15
C---- 'Open' an existing unformatted file, for reading.
C     In case of error, writes message on ERROUT and stops.
C     !DASH
      save
C     !DASH
      integer ERROUT, IOSTAT, UNIT
C     !DASH
      external IOFAULT
C
C     !BEG
      open (unit=UNIT, status='OLD', form='UNFORMATTED', READONLY,
     $                 iostat=IOSTAT)
C
      if(IOSTAT.ne.0) then
        call IOFAULT (ERROUT, 'PUCK', 'Open/unformatted for read',
     $                UNIT, IOSTAT)
        stop
      end if
C     !END
C
      return
      end
