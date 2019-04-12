      subroutine SEVERN
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1975 May 21
C---- Drives Population Initialization.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, NPROG
C     !DASH
      external LOGIN, RUBBLE, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /4/
C
      call HI ('SEVERN')
C     !BEG
      call LOGIN  (NPROG)
      call RUBBLE (X, W, IW)
      call LOGOUT (NPROG)
C     !END
      call BYE ('SEVERN')
C
      return
      end
