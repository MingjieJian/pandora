      subroutine CLOGO
     $(N,NL,ALPHA,VLEV,VCNT,VL,VK)
C
C     Rudolf Loeser, 1998 Jul 23
C---- Sets up velocities, for COLUGO.
C     !DASH
      save
C     !DASH
      real*8 ALF, ALPHA, OMA, ONE, VCNT, VK, VL, VLEV
      integer I, J, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external MOVE1, NEGATE, TRINCA, HI, BYE
C
C               ALPHA(NL), VL(N,NL), VK(N), VLEV(N), VCNT(N)
      dimension ALPHA(*),  VL(N,*),  VK(*), VLEV(*), VCNT(*)
C
      call HI ('CLOGO')
C     !BEG
      call MOVE1    (VCNT,N,VK)
      call NEGATE   (VK,N)
      call TRINCA   (VK,N)
C
      do 101 J = 1,NL
        ALF = ALPHA(J)
        OMA = ONE-ALF
        do 100 I = 1,N
          VL(I,J) = ALF*VLEV(I)+OMA*VCNT(I)
  100   continue
        call TRINCA (VL(1,J),N)
  101 continue
C     !END
      call BYE ('CLOGO')
C
      return
      end
