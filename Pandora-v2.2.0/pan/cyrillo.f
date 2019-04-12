      subroutine CYRILLO
     $(Z,GMASIN,GMASS,GMASSE,N,WZM,GML,GMIL,ZOLD,ZNEW,ZED,WEIT,FZLIM,
     $ DZMSS,ZQ)
C
C     Rudolf Loeser, 1978 Sep 07
C---- Computes a new Z-scale.
C     !DASH
      save
C     !DASH
      real*8 DZMSS, FZLIM, GMASIN, GMASS, GMASSE, GMIL, GML, WEIT, WZM,
     $       Z, ZED, ZERO, ZNEW, ZOLD, ZQ
      integer I, J, N
      logical lummy
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external MOVE1, IMPROVE, LOGO, DERE, GLOSS, PLAUTUS, HI, BYE
C
C               GML(N), GMASSE(N), GMIL(N), GMASIN(N), ZED(N), WEIT(N),
      dimension GML(*), GMASSE(*), GMIL(*), GMASIN(*), ZED(*), WEIT(*),
C
C               ZOLD(N), GMASS(N), ZNEW(N), Z(N)
     $          ZOLD(*), GMASS(*), ZNEW(*), Z(*)
C
      call HI ('CYRILLO')
C     !BEG
C---- Save current Z
      call MOVE1   (Z, N, ZOLD)
C---- Set up edited mass
      call MOVE1   (GMASS, N, GMASSE)
      call IMPROVE (GMASSE, Z, N, 'CYRILLO', J, lummy)
C---- Compute logs
      call LOGO    (GMASSE, N, 0, ZERO, GML )
      call LOGO    (GMASIN, N, 0, ZERO, GMIL)
C---- Interpolate to new Z-scale
      call DERE    (GML, 1, ZOLD, 1, N, GMIL, 1, ZNEW, 1, N, 2)
C---- Edit out excessive alterations
      call PLAUTUS (N, FZLIM, DZMSS, ZOLD, ZNEW, ZED, ZQ)
C---- Apply weighting to get new Z
      call GLOSS   (ZOLD, ZED, Z, N, WZM, WEIT)
C     !END
      call BYE ('CYRILLO')
C
      return
      end
