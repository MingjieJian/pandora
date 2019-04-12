      subroutine TANG
     $(XMU,LG,TAU,N,TMU)
C
C     Rudolf Loeser, 1981 May 07
C---- Computes a set of TMU values.
C     !DASH
      save
C     !DASH
      real*8 ONE, OOMU, TAU, TMU, XMU
      integer J, LG, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, MOVE1, CONMUL, HI, BYE
C
C               XMU(LG), TAU(N), TMU(N,LG)
      dimension XMU(*),  TAU(*), TMU(N,*)
C
      call HI ('TANG')
C     !BEG
      do 100 J = 1,LG
        call DIVIDE (ONE,XMU(J),OOMU)
        call MOVE1  (TAU,N,TMU(1,J))
        call CONMUL (OOMU,TMU(1,J),N)
  100 continue
C     !END
      call BYE ('TANG')
C
      return
      end
