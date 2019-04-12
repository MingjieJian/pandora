      subroutine ABORT
C
C     Rudolf Loeser, 1996 Jul 22
C---- Stops the run in case of error;
C     prints the caller stack if it exists.
C     !DASH
      save
C     !DASH
      integer LUEO
C     !COM
C---- SWABORT     as of 1996 Nov 27
      logical     SWDELA, SWPEND
      common      /SWABORT/ SWDELA, SWPEND
C     Control switches for subroutine ABORT:
C     SWDELA - delay an abort;
C     SWPEND - a delayed abort is pending.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external FLEBB, LINER
C
      call HI ('ABORT')
C     !BEG
      call FLEBB
C
      if(SWDELA) then
        SWPEND = .true.
        call LINER (2, LUEO)
        write (LUEO,100)
  100   format(' ','Delayed ABORT in effect: confusion may ensue.')
        write (*,100)
        call LINER (2, LUEO)
      else
C
        call LINER (2, LUEO)
        write (LUEO,101)
  101   format(' ','*****  ABORT  stopped this run.')
C
        stop 'ABORT: run stopped because of an error.'
C
      end if
C     !END
      call BYE ('ABORT')
C
      end
