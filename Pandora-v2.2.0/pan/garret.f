      subroutine GARRET
     $(N,Z,NZE,IND,BMWAC,DELTA,FME,FNRML,A)
C
C     Rudolf Loeser, 1993 Jul 01
C---- Computes Gaussians, and their integrals, for depth smearing
C     in GRIFFIN.
C     !DASH
      save
C     !DASH
      real*8 A, ARG, BMWAC, CON69, DELTA, FME, FNRML, ONE, OOD, Z, ZERO
      integer I, IND, J, N, NZE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external RIGEL, DIVIDE, BUSH, HI, BYE
C
C               Z(N), IND(NZE), FME(N,NZE), FNRML(NZE), A(N)
      dimension Z(*), IND(*),   FME(N,*),   FNRML(*),   A(*)
C
C
      call HI ('GARRET')
C     !BEG
      if(BMWAC.gt.ZERO) then
        call RIGEL  (69, CON69)
        DELTA = CON69*BMWAC
        call DIVIDE (ONE, DELTA, OOD)
C
        do 101 J = 1,NZE
C
          do 100 I = 1,N
            ARG = -(((Z(I)-Z(IND(J)))*OOD)**2)
            FME(I,J) = exp(ARG)
  100     continue
C
          call BUSH (Z, 1, FME(1,J), 1, A, 1, N)
          FNRML(J) = A(N)
C
  101   continue
      end if
C     !END
      call BYE ('GARRET')
C
      return
      end
