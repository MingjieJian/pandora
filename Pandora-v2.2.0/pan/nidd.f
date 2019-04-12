      subroutine NIDD
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Jan 31
C---- "Lyman" calculation.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, KODE, KTRU, LITER, LYMIT, MODE
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
      equivalence (LEST(24),LITER)
C     !DASH
      external MISS, COLNE, BRAY, OUSE, TWEED, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data KODE,MODE,KTRU /2, 0, 0/
C
      call HI ('NIDD')
C     !BEG
      do 100 LITER = 1,LYMIT
        call MISS  (X)
C----   Number densities
        call COLNE (1, 0, 0, X, IX, W, IW)
C----   TAUK processing, if needed
        call BRAY  (X, IX, W, IW)
C----   Continuum Calculations for Lyman integration frequencies
        call OUSE  (KODE, MODE, KTRU, 1, 1, 1, 1, X, IX, W, IW)
C----   Lyman Calculations
        call TWEED (X, IX, W, IW)
  100 continue
      LITER = 0
C     !END
      call BYE ('NIDD')
C
      return
      end
