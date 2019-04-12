      subroutine CLYDE
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Jul 29
C---- Controls Ne Updating for a nonHSE NLTE Ion run.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, NPROG
C     !DASH
      external LOGIN, URSULA, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /16/
C
      call HI ('CLYDE')
C     !BEG
      call LOGIN  (NPROG)
      call URSULA (X, IX, W, IW)
      call LOGOUT (NPROG)
C     !END
      call BYE ('CLYDE')
C
      return
      end
