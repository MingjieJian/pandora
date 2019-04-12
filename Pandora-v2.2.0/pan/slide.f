      subroutine SLIDE
     $(X,W,IW,XK,GK,XKX,GKX,AKX,IADRS,KKU)
C
C     Rudolf Loeser, 2005 Jul 11
C---- Augments basic tables for Lyman continuum integrations, if
C     required.
C     !DASH
      save
C     !DASH
      real*8 AKX, GK, GKX, TIN, W, X, XK, XKX
      integer IADRS, IW, KKU
C     !DASH
      external GLIDE, AKITU, PULPIT, SECOND, LINER, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XK(KK), GK(KK), XKX(KKX), GKX(KKX), AKX(KKX), IADRS(KKX)
      dimension XK(*),  GK(*),  XKX(*),   GKX(*),   AKX(*),   IADRS(*)
C
      call HI ('SLIDE')
C     !BEG
      call SECOND  (TIN)
C
C---- Make (?  augmented) tables: XKX, GKX, and IADRS
      call GLIDE  (X, W, IW, XK, GK, XKX, GKX, IADRS, KKU)
C
C---- Compute integration weights (  ABORT  if calculation failed)
      call AKITU  (KKU, XKX, AKX, W, IW)
C---- (Print ?)
      call PULPIT (KKU, XKX, AKX, GKX, TIN)
C     !END
      call BYE ('SLIDE')
C
      return
      end
