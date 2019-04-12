      subroutine WINK
     $(N,Z,OPAC,S,DX,FX,GX,TX,ZN,R1N,DUMP,IZ,EINT,LABEL)
C
C     Rudolf Loeser, 1981 Aug 30
C---- Computes a value of Continuous Emergent Shell Intensity,
C     in spherical coordinates.
C     (This is version 3 of WINK.)
C
C     See also FINK.
C     !DASH
      save
C     !DASH
      real*8 DX, EINT, FX, GX, OPAC, R1N, S, TX, Z, ZN
      integer IMAX, IZ, N, NRP
      logical DUMP
      character LABEL*(*)
C     !DASH
      external CYNTHIA, BARBARA, STENTOR, NESTOR, HI, BYE
C
C               Z(N), TX(NRPMX), DX(NRPMX), FX(NRPMX), GX(NRPMX), S(N),
      dimension Z(*), TX(*),     DX(*),     FX(*),     GX(*),     S(*),
C
C               OPAC(N)
     $          OPAC(*)
C
      call HI ('WINK')
C     !BEG
C---- NRP is the number of points along an extended ray
      NRP = 2*N+5
C
C---- Get DX (distance along extended ray)
      call CYNTHIA (N, Z, DX, ZN, R1N)
C---- Extend GX (monochromatic source function)
      call BARBARA (S, N, GX)
C---- Extend & edit FX (opacity)
      call BARBARA (OPAC, N, FX)
      call STENTOR (FX, NRP, LABEL)
C
C---- Compute EINT (intensity) and TX (optical depth)
      call NESTOR  ((NRP-1), DX, FX, GX, TX, EINT, IMAX, DUMP, IZ, 1,
     $              LABEL)
C     !END
      call BYE ('WINK')
C
      return
      end
