      subroutine COMPACT
     $(A,M,MDIM,N,B)
 
C
C     Rudolf Loeser, 2002 Dec 17
C---- Copies A, an M x N array with column-dimension MDIM > M,
C       into B, an M x N array with column-dimension M.
C     !DASH
      save
C     !DASH
      real*8 A, B
      integer J, M, MDIM, N
C     !DASH
      external MOVE1, HI, BYE
C
      dimension A(MDIM,N), B(M,N)
C
C     !BEG
      do 100 J = 1,N
        call MOVE1 (A(1,J),M,B(1,J))
  100 continue
C     !END
C
      return
      end
