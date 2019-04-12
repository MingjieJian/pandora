      subroutine WAVE
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1995 Apr 07
C---- Controls the sequence of Continuum Blocks initializations, and
C     computes various wavelengths range counters.
C     (This is version 3 of WAVE.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer ICBL, IN, IS, ISLTIT, ISWAVE, IW, IX, MOX, NSH
C     !DASH
      external LUCY, HOTEL, RAGWORT, BUGBANE, DATURA, COWBANE, HEALALL,
     $         REDLEG, DUGOUT, ORPINE, COMFREY, BELISAR, TUNNEL, WGIVE,
     $         TOE, TONE, HOVEL, ZOLTAN, REPION, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),ICBL  ),(IN( 2),ISWAVE),(IN( 3),ISLTIT)
C     !EJECT
C
      call HI ('WAVE')
C     !BEG
C     (Get W allotment)
      call LUCY    (IN, IS, MOX, 'WAVE')
C
C---- Initialize index, etc.
      call BELISAR (NSH)
C
C     First, shareable blocks
C
C---- Rates integrations
      call HOTEL   (X, IX, W, IW, W(ICBL), W(ISWAVE), W(ISLTIT), NSH)
C---- H-
      call REDLEG  (X, IX, W, IW, W(ICBL), W(ISWAVE), W(ISLTIT), NSH)
C---- Dust
      call COWBANE (X, IX, W, IW, W(ICBL), W(ISWAVE), W(ISLTIT), NSH)
C---- Composite Line Opacity
      call RAGWORT (X, IX, W, IW, W(ICBL), W(ISWAVE), W(ISLTIT), NSH)
C---- CO
      call DUGOUT  (X, IX, W, IW, W(ICBL), W(ISWAVE), W(ISLTIT), NSH)
C---- Additional photoionization
      call DATURA  (X, IX, W, IW, W(ICBL), W(ISWAVE), W(ISLTIT), NSH)
C---- Additional wavelengths
      call BUGBANE (X, IX, W, IW, W(ICBL), W(ISWAVE), W(ISLTIT), NSH)
C
C---- Update supplementary headers in shared blocks
      call REPION  (       W, IW, W(ICBL), W(ISWAVE), W(ISLTIT), NSH)
C
C     Next, unique blocks
C
C---- Line cores
      call ORPINE  (X, IX, W, IW, W(ICBL))
C---- Frequency-dependent background transitions
      call TOE     (X, IX, W, IW, W(ICBL))
C---- Lyman continuum
      call COMFREY (X, IX, W, IW, W(ICBL))
C---- Incident coronal radiation (Lyman)
      call HEALALL (X, IX, W, IW, W(ICBL))
C---- Standard rates integrations wavelengths
      call HOVEL   (X, IX, W, IW, W(ICBL))
C
C---- Sort (and print ?) index
      call TONE    (W, IW)
      call ZOLTAN
C---- Set up various wavelengths range counters
      call TUNNEL  (X, IX)
C
C     (Give back W allotment)
      call WGIVE   (W, 'WAVE')
C     !END
      call BYE ('WAVE')
C
      return
      end
