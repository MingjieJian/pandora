      subroutine SETI
     $(A,INC,N,X)
C     Rudolf Loeser, 1979 Apr 18
C---- Sets every INC'th element of the array A
C     for a total of N elements, equal to X.
C     !DASH
      save
C     !DASH
      integer A, I, INC, J, N, X
C     !DASH
      dimension A(*)
C
C     !BEG
      if(N.gt.0) then
        J = 1-INC
        do 100 I = 1,N
          J = J+INC
          A(J) = X
  100   continue
      end if
C     !END
C
      return
      end
