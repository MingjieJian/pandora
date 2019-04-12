      subroutine NEGATE
     $(A,N)
C     Rudolf Loeser, 1989 Oct 05
C---- Changes the sign of every element of array A.
C     A contains N elements.
C     !DASH
      save
C     !DASH
      real*8 A
      integer I, N
C     !DASH
      dimension A(*)
C
C     !BEG
      if(N.gt.0) then
        do 100 I = 1,N
          A(I) = -A(I)
  100   continue
      end if
C     !END
C
      return
      end
