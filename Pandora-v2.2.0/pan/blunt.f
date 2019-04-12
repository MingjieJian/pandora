      subroutine BLUNT
C
C     Rudolf Loeser, 1974 Dec 18
C---- Writes the heading for Lyman output.
C     !DASH
      save
C     !DASH
      integer KOLEV, LITER, MO
      character TITLE*12
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 33),KOLEV)
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
      equivalence (LEST(24),LITER)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external PRIAM, HI, BYE
C
      call HI ('BLUNT')
C     !BEG
      if((MO.gt.0).and.(LITER.le.1)) then
        write (TITLE,100) KOLEV
  100   format('LEVEL',I2,' TO K')
C
        call PRIAM (MO,TITLE,12)
C
        write (MO,101)
  101   format(' ','(Options ALLY, ALYMPRNT)')
      end if
C     !END
      call BYE ('BLUNT')
C
      return
      end
