      subroutine GOAT
     $(X,IX,W,IW,XPBL,RKI,IQRK,RLI,IQRL,LU)
C
C     Rudolf Loeser, 1991 Feb 15
C---- Supervises calculation of the additional terms that affect the
C     basic values of the rates RKI and/or RLI.
C     (This is version 2 of GOAT.)
C     !DASH
      save
C     !DASH
      real*8 RKI, RLI, W, X, XPBL
      integer IQRK, IQRL, IW, IX, LU
C     !DASH
      external ANCHOR, KIX, BURRO, COST, MEADOW, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               RKI(N,NSL), RLI(N,NSL), XPBL(Lenpbl), IQRK(NSL),
      dimension RKI(*),     RLI(*),     XPBL(*),      IQRK(*),
C
C               IQRL(NSL)
     $          IQRL(*)
C
      call HI ('GOAT')
C     !BEG
C---- Additional photoionization
      call ANCHOR (X, IX, W, RKI, IQRK, LU)
C---- K-shell ionization
      call KIX    (X, RKI, IQRK, LU)
C---- Additional recombination
      call BURRO  (X, RLI, IQRL, LU)
C---- Artificial enhancement of RK1
      call COST   (X, RKI, RLI, LU)
C---- Charge exchange
      call MEADOW (X, IX, W, IW, RKI, IQRK, RLI, IQRL, XPBL, LU)
C     !END
      call BYE ('GOAT')
C
      return
      end
