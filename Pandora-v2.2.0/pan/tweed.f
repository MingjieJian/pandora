      subroutine TWEED
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Jul 15
C---- Lyman rates calculation.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, NPROG
C     !DASH
      external LOGIN, LIME, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /14/
C
      call HI ('TWEED')
C     !BEG
      call LOGIN  (NPROG)
      call LIME   (X,IX,W,IW)
      call LOGOUT (NPROG)
C     !END
      call BYE ('TWEED')
C
      return
      end
