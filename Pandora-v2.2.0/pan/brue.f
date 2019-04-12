      subroutine BRUE
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1983 Jul 21
C---- Controls the HSL-iteration processing of an overall iteration:
C     Lyman, Number Density, HSE, Gas data, etc.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IHSLT, ITHSL, IW, IX, MO
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
      equivalence (KZQ( 20),IHSLT)
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
      equivalence (LEST(19),ITHSL)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external CYLON, DISS, ROTHER, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('BRUE')
C     !BEG
      do 100 ITHSL = 1,IHSLT
        call CYLON  (MO)
        call DISS   (X)
        call ROTHER (X,IX,W,IW)
  100 continue
      ITHSL = 0
C     !END
      call BYE ('BRUE')
C
      return
      end
