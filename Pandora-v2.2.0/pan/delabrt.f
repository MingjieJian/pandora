      subroutine DELABRT
C
C     Rudolf Loeser, 1996 Nov 27
C---- Sets switch to delay ABORTs.
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
      external MESHED, MASHED
C
C     !BEG
      SWDELA = .true.
C
      call MESHED ('DELABRT', 3)
      write (LUEO,100)
  100 format(' ','"Delayed-ABORT" now activated.')
      call MASHED ('DELABRT')
C
      write (*,100)
C     !END
C
      return
      end
