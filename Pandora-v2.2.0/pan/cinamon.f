      subroutine CINAMON
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 2004 Apr 20
C---- Plays with data needed for background contributor lines.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX
C     !DASH
      external STRAW, VENTURE, SPOTTY, SHODDY, SCOTTY, KNOTTY, NATTY,
     $         HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('CINAMON')
C     !BEG
C---- Make sure NDWM, NDW and NMLR are ok
      call STRAW   (X)
C---- For Hydrogen Lyman lines
      call VENTURE (X, IX, W, IW)
C---- For Oxygen-I
      call SPOTTY  (X, IX, W, IW)
C---- For Oxygen-II
      call KNOTTY  (X, IX, W, IW)
C---- For Oxygen-III
      call NATTY   (X, IX, W, IW)
C---- For Helium-I
      call SHODDY  (X, IX, W, IW)
C---- For Helium-II
      call SCOTTY  (X, IX, W, IW)
C     !END
      call BYE ('CINAMON')
C
      return
      end
