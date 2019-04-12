      subroutine FELIS
     $(N,RM,RD,BDHM)
C
C     Rudolf Loeser, 1989 Nov 13
C---- Sets up Continuum Recalculation controls, and debug checksums,
C     for OSIRIS.
C     (This is version 3 of FELIS.)
C     !DASH
      save
C     !DASH
      real*8 BDHM, RD, RM
      integer IOVER, N
      character TIT*12
C     !COM
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
C               RM(N), RD(N), BDHM(N)
      dimension RM(*), RD(*), BDHM(*)
C
      call HI ('FELIS')
C     !BEG
C---- Continuum Recalculation control
      call WENDY   (BDHM, 1, N, 5, 'FELIS')
C
C---- Checksums
      write (TIT,100) IOVER
  100 format(', IOVER =',I3)
C
      call CHECKER (RM,   1, N, 'H- RM'//TIT)
      call CHECKER (RD,   1, N, 'H- RD'//TIT)
      call CHECKER (BDHM, 1, N, 'H- departure coefficient'//TIT)
C     !END
      call BYE ('FELIS')
C
      return
      end
