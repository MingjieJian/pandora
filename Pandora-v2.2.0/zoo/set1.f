      subroutine SET1
     $(A,N,X)
C     Rudolf Loeser, 1986 Oct 03
C---- Special version of SETD - q.v.
C     !DASH
      save
C     !DASH
      real*8 A, X, XX
      integer I, N
C     !DASH
      dimension A(*)
C
C     !BEG
      if(N.gt.0) then
        XX = X
        do 100 I = 1,N
          A(I) = XX
  100   continue
      end if
C     !END
C
      return
      end
