      subroutine MULSUM
     $(A,B,C,N)
C     Rudolf Loeser, 1992 Dec 16
C---- Increments an array of sums of products, such that
C     C = C + A*B.
C     A, B, and C all are arrays of length N.
C     !DASH
      save
C     !DASH
      real*8 A, B, C
      integer I, N
C     !DASH
      dimension A(*), B(*), C(*)
C
C     !BEG
      if(N.gt.0) then
        do 100 I = 1,N
          C(I) = C(I)+A(I)*B(I)
  100   continue
      end if
C     !END
C
      return
      end
