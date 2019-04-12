      subroutine INGER
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 Aug 12
C---- Controls initialization of Bs, NDs and NKs.
C     (This is version 2 of INGER.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX
      logical BRNT, PRNT
C     !DASH
      external ARETE, ARION, EDERN, CHALCIS, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('INGER')
C     !BEG
C---- Compute missing Bs
      call ARETE   (X, IX, W, IW, BRNT)
C
C---- Initialize SET - stimulated emission terms (depend on Bs)
      call CHALCIS (X, IX, W, IW)
C
C---- Compute missing Number Densities
      call ARION   (X, IX, W, IW, PRNT)
C
C---- Print
      call EDERN   (X, BRNT, PRNT)
C     !END
      call BYE ('INGER')
C
      return
      end
