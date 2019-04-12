      subroutine ORRIN
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Aug 15
C---- Damping Parameters for Radiative Transitions.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, NPROG
C     !DASH
      external LOGIN, FLUE, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /11/
C
      call HI ('ORRIN')
C     !BEG
      call LOGIN  (NPROG)
      call FLUE   (X,IX,W,IW)
      call LOGOUT (NPROG)
C     !END
      call BYE ('ORRIN')
C
      return
      end
