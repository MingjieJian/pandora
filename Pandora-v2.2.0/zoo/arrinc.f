      subroutine ARRINC
     $(A,F,B,N)
C     Rudolf Loeser, 1991 Dec 16
C---- Increments an array of sums, such that B = B + F*A.
C     A and B each contain N elements; F is a constant.
C     !DASH
      save
C     !DASH
      real*8 A, B, F
      integer I, N
C     !DASH
      dimension A(N), B(N)
C
C     !BEG
      if(N.gt.0) then
        do 100 I = 1,N
          B(I) = B(I)+F*A(I)
  100   continue
      end if
C     !END
C
      return
      end
