      subroutine MEGARA
     $(XLM,E,C0,C1,C2)
C
C     Rudolf Loeser, 1985 Aug 09
C---- Computes photon energy, and retrieves coefficients, for REFLECT.
C     Data from Morrison and McCammon (1983), ApJ 270, p.119.
C     (This is version 2 of MEGARA.)
C     !DASH
      save
C     !DASH
      real*8 C0, C0T, C1, C1T, C2, C2T, CON22, E, ET, XLM, ZERO
      integer K, LOOK, NOTE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external RIGEL, DIVIDE, LOOKSD, HI, BYE
C
      dimension ET(14), C0T(14), C1T(14), C2T(14)
C
      data ET /    .1D0,       .284D0,     .4D0,       .532D0,
     $      .707D0,     .867D0,    1.303D0,    1.84D0,     2.471D0,
     $     3.21D0,     4.038D0,    7.111D0,    8.331D0,   10.D0/
      data C0T / -6.25D0,    17.3D0,     10.6D0,     34.7D0,
     $   248.1D0,     59.6D0,     80.5D0,    141.9D0,    281.9D0,
     $   291.4D0,    373.1D0,    568.2D0,    640.4D0,      0.D0/
      data C1T / 62.5D0,     18.8D0,     66.8D0,    145.8D0,
     $  -380.6D0,    169.3D0,    146.8D0,    104.7D0,     18.7D0,
     $    18.7D0,     -2.4D0,     30.9D0,     25.7D0,      0.D0/
      data C2T /  0.D0,       4.3D0,    -51.4D0,    -61.1D0,
     $   294.D0,     -47.7D0,    -31.5D0,    -17.D0,       0.D0,
     $     0.D0,        .75D0,     0.D0,       0.D0,       0.D0/
C
      call HI ('MEGARA')
C     !BEG
      call RIGEL  (32, CON22)
      call DIVIDE (CON22, XLM, E)
C
      call LOOKSD (ET, 14, ZERO, E, K, NOTE, LOOK)
      if(.not.(LOOK.eq.1)) then
        K = 14
      end if
C
      C0 = C0T(K)
      C1 = C1T(K)
      C2 = C2T(K)
C     !END
      call BYE ('MEGARA')
C
      return
      end
