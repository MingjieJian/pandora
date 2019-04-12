      subroutine DIVSUM
     $(A,B,C,N)
C
C     Rudolf Loeser, 1992 Dec 16
C---- Increments an array of sums of quotients, such that
C     C = C + A/B.
C     A, B, and C each contain N elements.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, DIV
      integer I, N
C     !DASH
      external DIVVY
C
      dimension A(N), B(N), C(N)
C
C     !BEG
      if(N.gt.0) then
        do 100 I = 1,N
          call DIVVY (A(I),B(I),DIV)
          C(I) = C(I)+DIV
  100   continue
      end if
C     !END
C
      return
      end
