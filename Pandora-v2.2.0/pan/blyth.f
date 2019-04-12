      subroutine BLYTH
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Jun 25
C---- Drives final net rates calculation.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, NPROG
C     !DASH
      external LOGIN, ARTHUR, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /21/
C
      call HI ('BLYTH')
C     !BEG
      call LOGIN  (NPROG)
      call ARTHUR (X,IX,W,IW)
      call LOGOUT (NPROG)
C     !END
      call BYE ('BLYTH')
C
      return
      end
