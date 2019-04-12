      subroutine SARACEN
     $(N,Z,XI,XII)
C
C     Rudolf Loeser, 1981 Aug 21
C---- Computes an Integrated Intensity.
C     (This is version 2 of SARACEN.)
C     !DASH
      save
C     !DASH
      real*8 HALF, XI, XII, Z, ZERO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
      external CATRIN, HI, BYE
C
C               Z(N), XI(N), XII(N)
      dimension Z(*), XI(*), XII(*)
C
      call HI ('SARACEN')
C     !BEG
      XII(1) = ZERO
      do 100 I = 2,N
        XII(I) = XII(I-1)+HALF*(XI(I)+XI(I-1))*(Z(I)-Z(I-1))
  100 continue
      call CATRIN (XII,N)
C     !END
      call BYE ('SARACEN')
C
      return
      end
