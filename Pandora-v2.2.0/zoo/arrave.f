      subroutine ARRAVE
     $(A,B,C,N)
C     Rudolf Loeser, 1988 Feb 11
C---- Averages the two arrays A and B, term-by-term, to make C.
C     A, B and C each contain N elements.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, HALF
      integer I, N
C     !DASH
      dimension A(N), B(N), C(N)
C
      data HALF /5.D-1/
C
C     !BEG
      if(N.gt.0) then
        do 100 I = 1,N
          C(I) = HALF*(A(I)+B(I))
  100   continue
      end if
C     !END
C
      return
      end
