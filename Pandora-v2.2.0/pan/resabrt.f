      subroutine RESABRT
C
C     Rudolf Loeser, 1996 Nov 27
C---- Sets switch for immediate ABORTs.
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
      external ABORT, MESHED, MASHED
C
C     !BEG
      SWDELA = .false.
      write (*,100)
  100 format(' ','"Delayed-ABORT" now deactivated.')
C
      if(SWPEND) then
        call ABORT
      else
C
        call MESHED ('RESABRT', 3)
        write (LUEO,100)
        call MASHED ('RESABRT')
C
      end if
C     !END
C
      return
      end
