      subroutine DUCK
     $(UNIT,ERROUT)
C
C     Rudolf Loeser, 1987 Jul 15
C---- 'Open' a new formatted file, for writing.
C     In case of error, writes a message on ERROUT and stops.
C     !DASH
      save
C     !DASH
      integer ERROUT, IOSTAT, UNIT
C     !DASH
      external IOFAULT
C
C     !BEG
      open (unit=UNIT, form='FORMATTED', status='UNKNOWN',
     $                 iostat=IOSTAT)
C
      if(IOSTAT.ne.0) then
        call IOFAULT (ERROUT, 'DUCK', 'Open/formatted for write, list',
     $                UNIT, IOSTAT)
        stop
      end if
C     !END
C
      return
      end
