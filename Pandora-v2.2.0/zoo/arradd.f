      subroutine ARRADD
     $(A,B,C,N)
C     Rudolf Loeser, 1984 Mar 20
C---- Adds the two arrays A and B, term-by-term, to make C.
C     A, B and C each contain N elements.
C     !DASH
      save
C     !DASH
      real*8 A, B, C
      integer I, N
C     !DASH
      dimension A(N), B(N), C(N)
C
C     !BEG
      if(N.gt.0) then
        do 100 I = 1,N
          C(I) = A(I)+B(I)
  100   continue
      end if
C     !END
C
      return
      end
