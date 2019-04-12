      subroutine MANBUK
C
C     Rudolf Loeser, 1987 Dec 03
C---- Initializes scratch I/O processing.
C     !DASH
      save
C     !DASH
      integer ISCRS, KIMS, LUMA
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
      equivalence (KZQ( 88),ISCRS)
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
      equivalence (LEST(41),KIMS )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(19),LUMA )
C     !DASH
      external MEMOIR, MEFULL, VISAYAS, HI, BYE
C
      call HI ('MANBUK')
C     !BEG
C---- Enable "in-memory" scratch I/O
      call MEMOIR
      KIMS = 1
C
      if(ISCRS.ne.0) then
C----   Disable "in-memory" scratch I/O
        KIMS = 0
        call MEFULL
C----   Enable random access scratch disk file
        call VISAYAS (LUMA)
      end if
C     !END
      call BYE ('MANBUK')
C
      return
      end
