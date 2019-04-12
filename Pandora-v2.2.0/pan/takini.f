      subroutine TAKINI
     $(N3,N,W3,WNZ)
C
C     Rudolf Loeser, 1981 Oct 28
C---- Matrix manipulation for NIDABA.
C     W3(N,N+3)  ->  WNZ(N,N).
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CWARTR, D, E, EIGHTH, F, HALF, ONE, W3, WNZ
      integer I, J, N, N3
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(18),EIGHTH)
      equivalence (DLIT(14),CWARTR)
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external MOVE1, HI, BYE
C
C               W3(N,N3), WNZ(N,N)
      dimension W3(N,*),  WNZ(N,*)
C
      call HI ('TAKINI')
C     !BEG
      do 100 J = 1,(N-2)
        call MOVE1 (W3(1,J),N,WNZ(1,J))
  100 continue
C
      A = HALF
      B = CWARTR
      C = EIGHTH
      D = ONE-HALF
      E = ONE-CWARTR
      F = ONE-EIGHTH
C
      do 101 I = 1,N
        WNZ(I,N-1) = W3(I,N3-4)+A*W3(I,N3-3)+B*W3(I,N3-2)+C*W3(I,N3-1)
        WNZ(I,N  ) = W3(I,N3  )+D*W3(I,N3-3)+E*W3(I,N3-2)+F*W3(I,N3-1)
  101 continue
C     !END
      call BYE ('TAKINI')
C
      return
      end
