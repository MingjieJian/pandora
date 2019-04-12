      subroutine HOUR
     $(S,EP1,EP2,TAUK)
C
C     Rudolf Loeser, 1984 Nov 20
C---- Saves debug checksums, for LIME.
C     (This is version 2 of HOUR.)
C     !DASH
      save
C     !DASH
      real*8 EP1, EP2, S, TAUK
      integer IOVER, ITHSL, LITER, N
      character TIT*40
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
      equivalence (LEST(24),LITER)
      equivalence (LEST(19),ITHSL)
C     !DASH
      external CHECKER, HI, BYE
C
C               S(N), EP1(N), EP2(N), TAUK(N)
      dimension S(*), EP1(*), EP2(*), TAUK(*)
C
      call HI ('HOUR')
C     !BEG
      write (TIT,100) IOVER,LITER,ITHSL
  100 format(6X,' Lyman: IOVER=',I2,', LITER=',I1,', ITHSL=',I1)
C
      TIT(1:6) = '     S'
      call CHECKER (S,    1, N, TIT)
      TIT(1:6) = '   EP1'
      call CHECKER (EP1,  1, N, TIT)
      TIT(1:6) = '   EP2'
      call CHECKER (EP2,  1, N, TIT)
      TIT(1:6) = '  TAUK'
      call CHECKER (TAUK, 1, N, TIT)
C     !END
      call BYE ('HOUR')
C
      return
      end
