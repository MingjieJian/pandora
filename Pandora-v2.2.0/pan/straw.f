      subroutine STRAW
     $(X)
C
C     Rudolf Loeser, 2004 Aug 11
C---- Checks on NDWM, NDW and NMLR, for CINAMON.
C     (This is version 2 of STRAW.)
C     !DASH
      save
C     !DASH
      real*8 X, ZNDW
      integer JJZ, KZXST, N, NDW, NDWM, NMLR
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(  1),NDW  )
      equivalence (RZQ(107),ZNDW )
      equivalence (KZQ(191),NMLR )
      equivalence (KZQ(128),NDWM )
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
      equivalence (LEST(68),KZXST)
C     !DASH
C     !EJECT
      external SHIRT, HI, BYE
C
      dimension X(*)
C
      call HI ('STRAW')
C     !BEG
      call SHIRT (NDW, ZNDW, X(JJZ), KZXST, N, NMLR)
C
      if((NDWM.lt.1).or.(NDWM.gt.N)) then
        NDWM = NDW
      end if
C     !END
      call BYE ('STRAW')
C
      return
      end
