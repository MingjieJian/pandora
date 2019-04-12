      subroutine TUCK
     $(UNIT,ERROUT)
C
C     Rudolf Loeser, 1987 Jul 15
C---- 'Open' a new formatted file, for writing,
C     WITH Fortran carriage control.
C
C     The presumption is that TUCK is used only for opening the
C     printout file.
C
C     In case of error, writes message on ERROUT and stops.
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
        call IOFAULT (ERROUT, 'TUCK',
     $                'Open/formatted for write, Fortran',
     $                UNIT, IOSTAT)
        stop
      end if
C     !END
C
      return
      end
