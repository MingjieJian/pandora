      subroutine YARTY
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Jul 31
C---- Controls "INIT": PANDORA's input and initialization processing.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX
C     !DASH
      external TRENT, TERN, SEVERN, DOVEY, DART, NAR, HI, BYE
C
C               X(*), IX(*), W(*), IW(*)
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('YARTY')
C     !BEG
C
C---- Set input defaults, read and print input data,
C     initialize various data blocks and data files
      call TRENT  (X, IX, W, IW)
C
C---- Populations initializations
      call SEVERN (X, IX, W, IW)
C
C---- Various precalculations
      call TERN   (X, IX, W, IW)
C
C---- Kurucz opacities initialization
      call DOVEY  (X, IX, W, IW)
C
C---- Continuum Data file
      call DART   (X, IX, W, IW)
C
C---- Read restart PRD Jnu
      call NAR    (X, IX, W, IW)
C
C     !END
      call BYE ('YARTY')
C
      return
      end
