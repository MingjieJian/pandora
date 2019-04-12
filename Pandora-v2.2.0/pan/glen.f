      subroutine GLEN
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Jan 31
C---- Performs "Continuum-only" runs.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IOVER, IW, IX, KODE, KTRU, MO, MODE, NO
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
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
      equivalence (LUNITS( 5),NO   )
C     !DASH
      external DEFILE, BRAY, OUSE, THURSO, RIBBLE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data KODE,MODE,KTRU /1, 0, 0/
C
      call HI ('GLEN')
C     !BEG
      IOVER = 0
      MO    = NO
C
C---- Delete unneeded files
      call DEFILE
C
C---- Calculate PREF (and Z)
      call BRAY   (X, IX, W, IW)
C---- Continuum calculations
      call OUSE   (KODE, MODE, KTRU, 1, 1, 1, 1, X, IX, W, IW)
C---- Wavelengths summary
      call THURSO (X, IX, W, IW)
C---- Spectrum calculations
      call RIBBLE (X, IX, W, IW)
C     !END
      call BYE ('GLEN')
C
      return
      end
