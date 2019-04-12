      subroutine ANADYR
     $(TAU,I,N,D)
C
C     Rudolf Loeser, 1982 Aug 17
C---- Sets up delta-TAU, D, for row I for "GR" weight
C     matrix calculation.
C     !DASH
      save
C     !DASH
      real*8 D, TAU, ZERO
      integer I, J, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  HI, BYE
      intrinsic max
C
C               TAU(N,N), D(N)
      dimension TAU(N,*), D(*)
C
      call HI ('ANADYR')
C     !BEG
      do 100 J = 1,(N-1)
        D(J) = max((TAU(I,J+1)-TAU(I,J)),ZERO)
  100 continue
      D(N) = ZERO
C     !END
      call BYE ('ANADYR')
C
      return
      end
