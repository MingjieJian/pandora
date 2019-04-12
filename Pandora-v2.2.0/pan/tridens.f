      subroutine TRIDENS
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Aug 21
C---- Drives Transitions post-Processing.
C     (This is version 2 of TRIDENS.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IN, IS, IW, IX, IXCBL, IXLB1, IXLB2, MOX
C     !DASH
      external JINN, TUBA, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IXLB1 ),(IN( 2),IXLB2 ),(IN( 3),IXCBL )
C
      call HI ('TRIDENS')
C     !BEG
C     (Get, and allocate, W allotment)
      call JINN  (IN, IS, MOX, 'TRIDENS')
C
C---- Compute and print
      call TUBA  (X, IX, W, IW, W(IXLB1), W(IXLB2), W(IXCBL))
C
C     (Give back W allotment)
      call WGIVE (W, 'TRIDENS')
C     !END
      call BYE ('TRIDENS')
C
      return
      end
