      subroutine MISS
     $(X)
C
C     Rudolf Loeser, 2002 Jul 12
C---- Prints Lyman iteration heading.
C     (Also, is place-holder for other potential stuff.)
C     !DASH
      save
C     !DASH
      real*8 X
      integer IHSLT, IOMX, IOVER, ITHSL, LITER, LYMIT
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
      equivalence (KZQ(  8),IOMX )
      equivalence (KZQ( 20),IHSLT)
      equivalence (KZQ( 19),LYMIT)
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
      equivalence (LEST(19),ITHSL)
      equivalence (LEST(24),LITER)
C     !DASH
      external LYMHEAD, HI, BYE
C
      dimension X(*)
C
      call HI ('MISS')
C     !BEG
C---- Print headers
      call LYMHEAD (IOVER,IOMX,ITHSL,IHSLT,LITER,LYMIT)
C     !END
      call BYE ('MISS')
C
      return
      end
