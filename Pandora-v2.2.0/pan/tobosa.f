      subroutine TOBOSA
     $(N,A,XI,F,FLUX)
C
C     Rudolf Loeser, 1978 Jun 30
C---- Computes Flux, given Intensities (XI) and Integration Weights (A).
C     !DASH
      save
C     !DASH
      real*8 A, CON, F, FLUX, XI, ZERO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external RIGEL, HI, BYE
C
C               A(N), XI(N), F(N)
      dimension A(*), XI(*), F(*)
C
      call HI ('TOBOSA')
C     !BEG
      call RIGEL (35,CON)
      FLUX = ZERO
      do 100 I = 1,N
        F(I) = CON*XI(I)*A(I)
        FLUX = FLUX+F(I)
  100 continue
C     !END
      call BYE ('TOBOSA')
C
      return
      end
