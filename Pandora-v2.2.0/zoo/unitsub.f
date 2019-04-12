      subroutine UNITSUB
     $(I,J,A,N)
C     Rudolf Loeser, 1984 Mar 20
C---- Subtracts a Unit matrix from the square matrix A(N,N) -
C     but only from columns I through J inclusive.
C     !DASH
      save
C     !DASH
      real*8 A, ONE
      integer I, J, K, N
C     !DASH
      dimension A(N,N)
C
      data ONE /1.D0/
C
C     !BEG
      if(J.ge.I) then
        do 100 K = I,J
          A(K,K) = A(K,K)-ONE
  100   continue
      end if
C     !END
C
      return
      end
