      subroutine HULL
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Aug 21
C---- Drives Line "POST"-processing.
C     (This is version 3 of HULL.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX, NPROG
C     !DASH
      external LOGIN, TRIDENS, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /19/
C
      call HI ('HULL')
C     !BEG
      call LOGIN   (NPROG)
      call TRIDENS (X, IX, W, IW)
      call LOGOUT  (NPROG)
C     !END
      call BYE ('HULL')
C
      return
      end
