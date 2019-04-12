      subroutine TRNSPS
     $(A,M,N,T)
C     Rudolf Loeser, 1982 May 17
C---- Transposes A(M,N) to T(N,M).
C     !DASH
      save
C     !DASH
      real*8 A, T
      integer J, M, N
C     !DASH
      external MOVED
C
C               A(M,N)  T(N,M)
      dimension A(M,*), T(N,*)
C
C     !BEG
      do 100 J = 1,M
        call MOVED (A(J,1),M,N, T(1,J),1,N)
  100 continue
C     !END
C
      return
      end
