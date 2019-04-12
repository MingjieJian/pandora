      subroutine HICCUP
     $(LU)
C     Rudolf Loeser, 1986 Jun 24
C---- Prints a condolence.
C     !DASH
      save
C     !DASH
      integer LU
C     !DASH
      external LINER
C
C     !BEG
      if(LU.gt.0) then
        call LINER (2,LU)
        write (LU,100)
  100   format(' ','I think this can only arise in the event of an ',
     $             'internal inconsistency: either the program is ',
     $             'wrong, or the System hiccupped.'//
     $         ' ','If the latter is suspected, try repeating the run.')
      end if
C     !END
C
      return
      end
