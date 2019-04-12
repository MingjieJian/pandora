      subroutine ARREST
     $(LU,MESSAGE)
C     Rudolf Loeser, 1986 Jun 23
C---- Prints a one-line error message, and aborts the run.
C     !DASH
      save
C     !DASH
      integer LU
      character BLANK*1, LINE*127, MESSAGE*(*)
C     !DASH
      external LINER, ABORT
C
      data BLANK /' '/
C
C     !BEG
      if(LU.gt.0) then
        LINE = MESSAGE//BLANK
C
        call LINER (5,LU)
        write (LU,100) LINE
  100   format(' ',A127)
C
        call ABORT
      end if
C     !END
C
      return
      end
