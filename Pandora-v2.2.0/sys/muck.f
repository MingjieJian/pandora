      subroutine MUCK
     $(UNIT,NAME,ERROUT)
C
C     Rudolf Loeser, 1988 Apr 25
C---- 'Open' an existing formatted file, for reading,
C     with SPECIFIED file name.
C     In case of error, writes message on ERROUT and stops.
C     !DASH
      save
C     !DASH
      integer ERROUT, IOSTAT, UNIT
      character NAME*(*)
C     !DASH
      external IOFAULT
C
C     !BEG
      open (unit=UNIT, file=NAME, status='OLD', READONLY,
     $                 form='FORMATTED', iostat=IOSTAT)
C
      if(IOSTAT.ne.0) then
        call IOFAULT (ERROUT, 'MUCK', 'Open/formatted for read',
     $                UNIT, IOSTAT)
        stop
      end if
C     !END
C
      return
      end
