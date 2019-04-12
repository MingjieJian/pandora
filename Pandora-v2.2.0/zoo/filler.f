      subroutine FILLER
     $(A,M,MDIM,N)
C     Rudolf Loeser, 1992 Aug 17
C---- Copies the first column of an array into all
C     other columns of that array.
C     !DASH
      save
C     !DASH
      real*8 A
      integer J, M, MDIM, N
C     !DASH
      external  MOVE1
C
C               A(M   ,N)
      dimension A(MDIM,N)
C
C     !BEG
      do 100 J = 2,N
        call MOVE1 (A(1,1),M,A(1,J))
  100 continue
C     !END
C
      return
      end
