      subroutine WALTZ
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 May 10
C---- Drives MINUET, to do subiterations processing.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IN, IS, IW, IX, IXCBL, IXLB1, IXLB2, MOX
C     !DASH
      external JUMP, MINUET, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IXLB1 ),(IN( 2),IXLB2 ),(IN( 3),IXCBL )
C
      call HI ('WALTZ')
C     !BEG
C     (Get, and allocate, W allotment)
      call JUMP   (IN,IS,MOX,'WALTZ')
C
      call MINUET (X,IX,W,IW,W(IXLB1),W(IXLB2),W(IXCBL))
C
C     (Give back W allotment)
      call WGIVE  (W,'WALTZ')
C     !END
      call BYE ('WALTZ')
C
      return
      end
