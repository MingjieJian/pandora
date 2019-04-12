      subroutine LUCK
     $(UNIT,ERROUT)
C
C     Rudolf Loeser, 1987 Jul 15
C---- 'Open' an existing formatted file, for reading.
C     In case of error, writes a message on ERROUT and stops.
C     !DASH
      save
C     !DASH
      integer ERROUT, IOSTAT, UNIT
C     !DASH
      external IOFAULT
C
C     !BEG
      open (unit=UNIT, status='OLD', form='FORMATTED', READONLY,
     $                 iostat=IOSTAT)
C
      if(IOSTAT.ne.0) then
        call IOFAULT (ERROUT, 'LUCK', 'Open/formatted for read',
     $                UNIT, IOSTAT)
        stop
      end if
C     !END
C
      return
      end
