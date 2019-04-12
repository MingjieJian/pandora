      subroutine PANTHER
     $(N,K,GMA,EP,FAB,VXI)
C
C     Rudolf Loeser, 1978 Apr 23
C---- Computes VXI, a PRD term.
C     (This is version 2 of PANTHER.)
C     !DASH
      save
C     !DASH
      real*8 EP, FAB, GMA, ONE, RAT, VXI
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
C               GMA(N), EP(N), FAB(N,K), VXI(N,K)
      dimension GMA(*), EP(*), FAB(N,*), VXI(N,*)
C
      call HI ('PANTHER')
C     !BEG
      do 101 I = 1,N
        call DIVIDE (GMA(I),(ONE+EP(I)),RAT)
        do 100 J = 1,K
          VXI(I,J) = RAT*FAB(I,J)
  100   continue
  101 continue
C     !END
      call BYE ('PANTHER')
C
      return
      end
