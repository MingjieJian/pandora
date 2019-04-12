      subroutine BLEND
     $(X,W,IW,DUMP)
C
C     Rudolf Loeser, 1980 Sep 24
C---- Recomputes PREF, for THOMAS.
C     (This is version 4 oF BLEND.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IN, IS, IW, IXCBL, MOX
      logical DUMP
C     !DASH
      external LUCIFER, BRAVO, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IXCBL )
C
      call HI ('BLEND')
C     !BEG
C     (Get W allotment)
      call LUCIFER (IN, IS, MOX, 'BLEND')
C
      call BRAVO   (X, W, IW, W(IXCBL), DUMP)
C
C     (Give back W allotment)
      call WGIVE   (W, 'BLEND')
C     !END
      call BYE ('BLEND')
C
      return
      end
