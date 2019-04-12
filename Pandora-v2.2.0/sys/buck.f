      subroutine BUCK
     $(UNIT,ERROUT)
C
C     Rudolf Loeser, 1987 Jul 15
C---- 'Open' a new unformatted file, for writing.
C     In case of error, writes message on ERROUT and stops.
C     !DASH
      save
C     !DASH
      integer ERROUT, IOSTAT, UNIT
C     !DASH
      external IOFAULT
C
C     !BEG
      open (unit=UNIT, form='UNFORMATTED', status='UNKNOWN',
     $                 iostat=IOSTAT)
C
      if(IOSTAT.ne.0) then
        call IOFAULT (ERROUT, 'BUCK', 'Open/unformatted for write',
     $                UNIT, IOSTAT)
        stop
      end if
C     !END
C
      return
      end
