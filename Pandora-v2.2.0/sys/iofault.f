      subroutine IOFAULT
     $(ERROUT,CALLER,ACTION,UNIT,IOSTAT)
C
C     Rudolf Loeser, 1987 Jul 16
C---- Writes an I/O error message on unit ERROUT, else types it.
C     !DASH
      save
C     !DASH
      integer ERROUT, IOSTAT, UNIT
      character ACTION*(*), CALLER*(*)
C
C     !BEG
      if(ERROUT.gt.0) then
        write (ERROUT,100) CALLER,ACTION,UNIT,IOSTAT
  100   format(' ',////
     $         ' ','Error in ',A,' : trying to ',A,' on unit',I4,
     $             '; IOSTAT =',I10////)
      else
        write(*,100) CALLER,ACTION,UNIT,IOSTAT
      end if
C     !END
C
      return
      end
