      subroutine TERN
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1975 May 21
C---- Drives precalculations and remaining file initializations.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, NPROG
C     !DASH
      external LOGIN, GRENADE, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /5/
C
      call HI ('TERN')
C     !BEG
      call LOGIN   (NPROG)
      call GRENADE (X,IX,W,IW)
      call LOGOUT  (NPROG)
C     !END
      call BYE ('TERN')
C
      return
      end
