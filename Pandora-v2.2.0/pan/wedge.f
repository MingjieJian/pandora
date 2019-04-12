      subroutine WEDGE
     $(N,NL,NSL,GM,RL,RK,CK,BD,STM)
C
C     Rudolf Loeser, 1978 May 18
C---- Computes sum over Supplementary Levels, for
C     "b from b-ratios" calculation.
C     !DASH
      save
C     !DASH
      real*8 BD, CK, GM, ONE, RK, RL, STM, SUM, ZERO
      integer I, J, N, NL, NP, NSL
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
      external  ZERO1, HI, BYE
C
C               GM(N,NSL), RL(N,NSL), RK(N,NSL), CK(N,NSL), BD(N,NSL),
      dimension GM(N,*),   RL(N,*),   RK(N,*),   CK(N,*),   BD(N,*),
C
C               STM(N)
     $          STM(*)
C
      call HI ('WEDGE')
C     !BEG
      call ZERO1 (STM,N)
C
      NP = NL+1
      if(NSL.ge.NP) then
        do 101 I = 1,N
          SUM = ZERO
          do 100 J = NP,NSL
            SUM = SUM+GM(I,J)*(RL(I,J)-RK(I,J)*BD(I,J)
     $               -CK(I,J)*(BD(I,J)-ONE))
  100     continue
          STM(I) = SUM
  101   continue
      end if
C     !END
      call BYE ('WEDGE')
C
      return
      end
