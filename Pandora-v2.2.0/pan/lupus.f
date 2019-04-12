      subroutine LUPUS
     $(TDUST)
C
C     Rudolf Loeser, 1989 Nov 13
C---- Sets up Continuum Recalculation control, and debug checksum,
C     for QUIVER.
C     (This is version 3 of LUPUS.)
C     !DASH
      save
C     !DASH
      real*8 TDUST
      integer IOVER, N
      character TIT*12
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
C     !DASH
      external WENDY, CHECKER, HI, BYE
C
C               TDUST(N)
      dimension TDUST(*)
C
      call HI ('LUPUS')
C     !BEG
      call WENDY   (TDUST, 1, N, 6, 'LUPUS')
C
      write (TIT,100) IOVER
  100 format(', IOVER =',I3)
      call CHECKER (TDUST, 1, N, 'Dust Temperature'//TIT)
C     !END
      call BYE ('LUPUS')
C
      return
      end
