      subroutine TUNNEL
     $(X,IX)
C
C     Rudolf Loeser, 2002 Aug 20
C---- Sets uo various continuum-wavelength-range counters.
C     !DASH
      save
C     !DASH
      real*8 X
      integer IX
C     !DASH
      external NOISE, ANATASE, LINNET, TULA, ESION, LABOR, HI, BYE
C
      dimension X(*), IX(*)
C
      call HI ('TUNNEL')
C     !BEG
C---- For rates integeration
      call NOISE   (X, IX)
C---- For H- calculation
      call ANATASE (X)
C---- For X-ray cooling
      call LINNET
C---- For CO lines cooling
      call TULA    (X)
C---- For Lyman calculation
      call ESION   (X)
C---- For H Ly lines normalization factors
      call LABOR
C     !END
      call BYE ('TUNNEL')
C
      return
      end
