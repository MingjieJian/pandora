      subroutine MAGGOT
     $(N,Z,VSB,R1N,DV,RV,SPHERE)
C
C     Rudolf Loeser, 1986 Jul 30
C---- Computes DV and RV
C     for "Sobolev" escape probability calculation.
C     !DASH
      save
C     !DASH
      real*8 DV, DZ, R1N, RV, VSB, Z, ZERO
      integer I, N
      logical SPHERE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external DERIV1, DIVIDE, NEGATE, BOUNDLO, ZERO1, HI, BYE
C
C               Z(N), VSB(N), DV(N), RV(N)
      dimension Z(*), VSB(*), DV(*), RV(*)
C
      call HI ('MAGGOT')
C     !BEG
      call DERIV1     (Z,VSB,DV,N)
      call NEGATE     (DV,N)
      call BOUNDLO    (N,DV,ZERO)
C
      if(SPHERE) then
        DZ = Z(N)+R1N
        do 100 I = 1,N
          call DIVIDE (VSB(I),(DZ-Z(I)),RV(I))
  100   continue
      else
        call ZERO1    (RV,N)
      end if
C     !END
      call BYE ('MAGGOT')
C
      return
      end
