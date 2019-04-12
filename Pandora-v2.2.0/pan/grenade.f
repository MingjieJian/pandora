      subroutine GRENADE
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1975 May 13
C---- Drives precalculations, and remaining file initializations.
C     (This is version 2 of GRENADE.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX
      logical CONONLY, INPONLY, REGULAR
C     !DASH
      external ANUNIT, BUST, NUBA, CRUMB, GANDER, RUIN, RICON, QUIXOTE,
     $         MIAS, AHMED, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('GRENADE')
C     !BEG
C     Compute processing mode switches, needed below
      call RICON     (REGULAR, CONONLY, INPONLY)
C
C---- Look angle integration weights
      call ANUNIT    (X)
C---- ( ? Z-from-mass)
      call AHMED     (X, W, IW)
C---- Z-dependent spherical geometry intermediates
      call NUBA      (X, W)
      if(REGULAR.or.CONONLY) then
C----   Geometrical dilution terms
        call GANDER  (X)
      end if
      if(REGULAR) then
C----   Stimulated Emission factors
        call RUIN    (X, IX)
      end if
      if(REGULAR.or.INPONLY) then
C----   Upper-Level Charge Exchange terms
        call MIAS    (X, IX, W, IW)
C----   Line Source Function intermediates
        call BUST    (X, W, IW)
C----   Transition-related quantities in Line Intensity Data Blocks:
C       Doppler Width, Frequency table, Frequency Integration Weights,
C       and Delta-Lambda
        call CRUMB   (X, W, IW)
      end if
      if(REGULAR) then
C----   Initialize Line Source Function data blocks
        call QUIXOTE (W)
      end if
C     !END
      call BYE ('GRENADE')
C
      return
      end
