      subroutine FINK
     $(N,Z,OPAC,S,DX,FX,GX,TX,FRR,DUMP,IR,EINT,LABEL,W)
C
C     Rudolf Loeser, 1981 Aug 30
C---- Computes a value of Continuous Emergent Disk Intensity,
C     in spherical coordinates.
C
C     See also WINK.
C     !DASH
      save
C     !DASH
      real*8 DX, EINT, FRR, FX, GX, OPAC, S, TX, W, Z
      integer IMAX, IR, N
      logical DUMP
      character LABEL*(*)
C     !DASH
      external CLARUS, MOVE1, STENTOR, NESTOR, HI, BYE
C
      dimension W(*)
C
C               Z(N), TX(NRPMX), DX(NRPMX), FX(NRPMX), GX(NRPMX), S(N),
      dimension Z(*), TX(*),     DX(*),     FX(*),     GX(*),     S(*),
C
C               OPAC(N)
     $          OPAC(*)
C
      call HI ('FINK')
C     !BEG
C---- Get DX (distance along ray)
      call CLARUS  (N, Z, FRR, DX, W)
C
C---- Set up GX (monochromatic source function)
      call MOVE1   (S,    N, GX)
C---- Set up FX (opacity)
      call MOVE1   (OPAC, N, FX)
      call STENTOR (FX, N, LABEL)
C
C---- Compute EINT (intensity) and TX (optical depth)
      call NESTOR  ((N-1), DX, FX, GX, TX, EINT, IMAX, DUMP, IR, 2,
     $              LABEL)
C     !END
      call BYE ('FINK')
C
      return
      end
