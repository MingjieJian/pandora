      subroutine RECIPRO
     $(A,N)
C     Rudolf Loeser, 1989 Oct 05
C---- Replaces every element of array A by its reciprocal.
C     Note: 1/0 = 0!
C     A contains N elements.
C     !DASH
      save
C     !DASH
      real*8 A, ONE, ZERO
      integer I, N
C     !DASH
      dimension A(*)
C
      data ZERO, ONE /0.D0, 1.D0/
C
C     !BEG
      if(N.gt.0) then
        do 100 I = 1,N
          if(A(I).ne.ZERO) then
            A(I) = ONE/A(I)
          end if
  100   continue
      end if
C     !END
C
      return
      end
