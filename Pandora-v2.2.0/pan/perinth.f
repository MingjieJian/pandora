      subroutine PERINTH
     $(N,K,EP,GMA,FAB,FJR,RXI)
C
C     Rudolf Loeser, 1978 Apr 23
C---- Computes RXI, a PRD term.
C     (This is version 3 of PERINTH.)
C     !DASH
      save
C     !DASH
      real*8 EP, FAB, FJR, GMA, ONE, R, RXI
      integer I, J, K, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, HI, BYE
C
C               GMA(N), EP(N), FAB(N,K), FJR(N,K), RXI(N,K)
      dimension GMA(*), EP(*), FAB(N,*), FJR(N,*), RXI(N,*)
C
      call HI ('PERINTH')
C     !BEG
      do 101 I = 1,N
        call DIVIDE (GMA(I), (ONE+EP(I)), R)
C
        do 100 J = 1,K
          RXI(I,J) = R*FAB(I,J)*FJR(I,J)
  100   continue
  101 continue
C     !END
      call BYE ('PERINTH')
C
      return
      end
