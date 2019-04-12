      subroutine NAR
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Aug 14
C---- Read (and interpolate ?) PRD Jnu restart values
C     (This is version 2 of NAR.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, JNUNC, NONC, NPROG
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
      equivalence (KZQ( 34),JNUNC)
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
      equivalence (LEST(29),NONC )
C     !DASH
      external LOGIN, WISSANT, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /26/
C
      call HI ('NAR')
C     !BEG
      if((JNUNC.gt.0).and.(NONC.gt.0)) then
        call LOGIN   (NPROG)
        call WISSANT (X, W)
        call LOGOUT  (NPROG)
      end if
C     !END
      call BYE ('NAR')
C
      return
      end
