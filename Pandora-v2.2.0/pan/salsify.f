      subroutine SALSIFY
     $(SAME,RESET,DUMP,ITER,LIMIT,MORE)
C
C     Rudolf Loeser, 2002 Feb 13
C---- Arranges for more, or for "just one more", N-1 iteration.
C     (This is version 2 of SALSIFY.)
C     !DASH
      save
C     !DASH
      integer ITER, LIMIT
      logical DUMP, MORE, RESET, SAME
C     !DASH
      external HI, BYE
C
      call HI ('SALSIFY')
C     !BEG
      if(SAME) then
C----   The calculation has converged
        MORE = .false.
C
C       Hoever, it may be necessary to do "just one more" iteration so
C       that detailed printout can be produced
        if(DUMP.and.(ITER.lt.LIMIT)) then
C----     No printout has yet been provided; so, just once,
C         rig the case to do "just one more"
          if(.not.RESET) then
            RESET = .true.
            LIMIT = ITER+1
            MORE  = .true.
          end if
        end if
C
      else
C----   The calculation has not converged; do more (if the iteration
C       limit has not been reached)
        MORE = ITER.lt.LIMIT
      end if
C     !END
      call BYE ('SALSIFY')
C
      return
      end
